# 阶段 B 全球 GPS 双频内部产品估计结果

## 结论

已在 `codex/zhang-full-rank-ppp-rtk` 工作树中完成全球 GPS L1C/L2W 内部钟差–相位偏差产品的估计链，并实际完成 2019-199 00:00–06:00、300 s 间隔的 74 站网络运行。

本次结果通过了结构与输出完整性验收，但没有通过整数产品验收。因此，当前交付物是 **FLOAT 内部钟差–双频相位偏差估计及完整协方差**，不是可供用户端 PPP-AR 使用的整数有效 phase OSB。CSV 中虽然同时写有 `FLOAT` 和 `FIXED` 行，但全部 8760 行的 `integer_valid=0`、`integer_component_id=UNRESOLVED`；任何消费者都必须按这两个字段门控，不能仅凭 `solution=FIXED` 使用。

机器可读审计结果见 `Docs/zhangGlobalInternalProductAudit.json`。

## 实施范围

- 系统与信号：GPS，L1C/L2W，码和相位联合，电离层浮点估计。
- 网络：97 个候选 RINEX 中 96 个通过 C1C/L1C/C2W/L2W 和 95% 完整率硬门禁；74 个估计站、2 个备份站、20 个预留验证站。
- 估计站与验证站交集为空。基础配置中的原 39 站观测列表在运行时被移除，避免 Ginan 合并 YAML 列表后重新载入验证站。
- 外部产品：WUM 2019-198/199/200 15 min SP3、WUM 2019-199 30 s CLK、IERS `finals.data.iau2000.txt`、`igs20.atx`、IGS 2019-195 七日 CRD SINEX；未加载外部 BSX。
- 时间段：2019-07-18 00:00:00 至 06:00:00，共 73 个 300 s 历元。
- 不在本次范围：多系统、第三频点、SSR/Bias-SINEX 发布、轨道估计和预留站 PPP-AR 科学验收。

## 本次实现

1. `scripts/select_zhang_global_network.py`
   - 审计 RINEX 头和历元完整率；
   - 固定预留 20 个独立验证站；
   - 应用区域配额、锚站优先级和 15°×15° 网格最多 2 站；
   - 生成估计、备份、验证列表和输入 YAML。

2. `scripts/run_zhang_global_internal_products.sh`
   - 提供 `smoke`、`throughput`、`full` 三种运行模式；
   - 启动前检查估计/验证站交集；
   - 生成不含原观测列表的运行时基础 YAML；
   - 保证正式网络只打开选定的 74 个 RINEX。

3. 内部产品输出
   - 主 CSV 扩展为 23 列，包含钟差、L1C/L2W 相位状态、标准差、钟–相协方差、整数有效性、整数分量、datum ID、不连续计数及解区间；
   - 新增完整产品向量协方差 CSV，按每个历元/解输出 90×90 协方差的上三角；
   - `FIXED` 行仅在整数 datum 完整时才置 `integer_valid=1`。

4. 稀疏图修正
   - 参考图现在与 PPP 测量生成使用相同的活动信号、失效标志、截止高度角以及 LLI/GF/MW/SCDIA/retrack/单频周跳剔除规则；
   - 修复了第二历元 MAW1–G19 被测量层剔除但仍被选作树边、导致 L1C/L2W 各一维秩亏的问题；
   - 图基在边失效时执行整数保持的 `tree_exchange`。

5. 诊断解耦
   - 严格 smoke 启用纯观测矩阵 Jacobi SVD；
   - 六小时正式段关闭该 O(n³) SVD，但继续输出图基、AR 和内部产品诊断；
   - 关闭 SVD 不再静默关闭 Zhang AR 摘要。

## 验证结果

### 构建和单元测试

- PEA 增量构建成功。
- `zhang_full_rank_tests`：17/17 通过。
- `phase_clock_osb_tests`：4/4 通过。

### 严格全秩 smoke

| 时间 | 观测行 | 活动列 | 秩 | 零度 |
|---|---:|---:|---:|---:|
| 00:00 | 2028 | 1696 | 1696 | 0 |
| 00:05 | 2480 | 2035 | 2035 | 0 |

第二历元发生 `tree_exchange`，修复前的 2 维秩亏不再出现。严格 smoke 总耗时约 5 min 20 s。这里的全秩证据只覆盖两个历元；六小时正式段没有逐历元执行 SVD。

### 六小时正式估计

- 73/73 历元完成，耗时 4 min 46.67 s。
- 图事件 59 次：初始化 1、叶节点扩展 1、树交换 47、重新初始化 10。
- 图中树边数 102–103，基本圈数 387–561。
- 发生 10 次 `replacement edge has no prior state` 引起的 phase datum discontinuity。

### 整数固定

- 每历元候选整数 810–1084。
- 10 个历元产生新固定，共 285 个；首次为 00:15，末次为 05:20。
- held integer rank 最大 147，06:00 时为 10。
- ADOP 范围 0.0322–0.4722 cycles。
- 由于固定秩在树重构/重新初始化后不能覆盖完整活动整数 datum，产品门禁始终未打开：`integer_valid_rows=0`。

这说明局部整数反馈链工作，但当前“全网完整整数 datum”没有在六小时弧段内稳定建立。把成功率或 ratio 阈值调低不会解决 datum 不完整问题，只会增加错误固定风险。

### 产品与协方差

- 主产品：73 历元 × 2 种解 × 30 颗卫星 × 2 个信号 = 8760 行，23 列。
- 每个历元/解的产品向量维数为 90：30 个卫星钟差 + 30×2 个相位状态。
- 每组协方差上三角 4095 行，共 146 组、597870 行；全部完整。
- 不连续计数和 datum version 最大均为 25。

实际输出位于：

- `/home/rx/GINAN/inputData/outputs/zhang_global_2019199_products/zhang_internal_products.csv`
- `/home/rx/GINAN/inputData/outputs/zhang_global_2019199_products/zhang_internal_product_covariance.csv`
- `/home/rx/GINAN/inputData/outputs/zhang_global_2019199/Network-zhang_global_2019199_product-201919900.TRACE`
- `/home/rx/GINAN/inputData/run_logs/zhang_global_2019199_full.log`

对应 SHA-256：

| 文件 | SHA-256 |
|---|---|
| `zhang_internal_products.csv` | `1ad96613f7717d9a7a0dff4df9313ab517ca3f1486c7c2481c0b195af868fbdf` |
| `zhang_internal_product_covariance.csv` | `dea9e9e0b09c293b4a2400b971314d17f304b64a9203aacc658a7c6c9322b517` |
| `Network-zhang_global_2019199_product-201919900.TRACE` | `ab22bbb0473666f17fe3c17a16b672f098d350bc69f5ee1cadb83e37ffd857b0` |
| `zhang_global_2019199_full.log` | `d23da01737bbcba635608adda171e03b14c090f338d9ebb73e0306dac39f3594` |

## 可复现命令

```bash
cd /mnt/c/Users/rx/Documents/GINAN/ginan

./scripts/run_zhang_global_internal_products.sh smoke
./scripts/run_zhang_global_internal_products.sh full

python3 scripts/audit_zhang_internal_products.py \
  --estimation-sites exampleConfigs/zhang_global_2019199/network_estimation.txt \
  --validation-sites exampleConfigs/zhang_global_2019199/network_validation.txt \
  --trace /home/rx/GINAN/inputData/outputs/zhang_global_2019199/Network-zhang_global_2019199_product-201919900.TRACE \
  --log /home/rx/GINAN/inputData/run_logs/zhang_global_2019199_full.log \
  --rank-trace /home/rx/GINAN/inputData/outputs/zhang_global_2019199_smoke/Network-zhang_global_2019199_smoke-201919900.TRACE \
  --rank-log /home/rx/GINAN/inputData/run_logs/zhang_global_2019199_smoke.log \
  --products /home/rx/GINAN/inputData/outputs/zhang_global_2019199_products/zhang_internal_products.csv \
  --covariance /home/rx/GINAN/inputData/outputs/zhang_global_2019199_products/zhang_internal_product_covariance.csv
```

## 下一步门禁

在启动 20 个预留站的独立 PPP-AR 验证前，至少需要完成：

1. 把整数有效性从当前全网 all-or-none 门禁改为按卫星/连通整数子图发布；
2. 对 `replacement edge has no prior state` 实现跨图基的可追溯整数 datum 延续，减少 10 次重新初始化；
3. 延长弧段并要求多个连续历元的 component-level integer validity；
4. 只有通过上述门禁后，才运行预留站冷启动、重启和换参考星 PPP-AR 验收。
