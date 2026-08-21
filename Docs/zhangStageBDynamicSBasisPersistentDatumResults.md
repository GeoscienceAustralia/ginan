# 阶段 B：动态估计 S-basis 与持久卫星产品整数 datum 分离结果

## 1. 实现范围

本轮在既有 E5 直接 `G_sat` WL/条件 L1 固定基础上完成了最小分离改造：

- 新增 `IntegerPotentialUnionFind`，以精确整数势差维护卫星 phase segment 组件；
- 新增 `ZhangSatelliteDatumManager`，独立维护卫星组件、整数对齐量 `alpha`、phase segment、datum version 和 discontinuity counter；
- 新增 `ZhangProductTargetBuilder`，将产品目标构造从动态图控制器的所有权中分离；
- 新增 `ProductConstraintPromotion`，仅在精确整数行格包含具名单位目标时恢复对应整数值；
- 将 E5 固定的 L1 目标和 `L2=L1-WL` 关系提升到持久卫星账本；
- 将物理 held HNF 中具有非零整数证明的卫星关系提升到同一账本；提升后关系不再依赖源物理弧段是否继续存在；
- 精确换树改为按信号、按卫星组件批处理。组件公共非整数平移作为 gauge 保留，只有星间相对平移需要满足整数条件；
- 接收机弧段或局部动态坐标重初始化不增加产品 datum version，也不删除已经提升的卫星关系；
- 只有显式卫星 phase discontinuity 才建立新 phase segment，并增加产品 version/counter；
- 产品 CSV 将原 `integer_valid` 拆分为：
  - `integer_structure_valid`；
  - `integer_datum_continuous`；
  - `integer_precision_valid`；
  - 最终 `integer_valid` 为三者逻辑与。

零目标行虽然形式上属于空整数格，但不提供新的固定信息，因此不会被提升为“已证明精度”的卫星关系。

## 2. 验证配置与产物

- 分支：`codex/zhang-full-rank-ppp-rtk`
- 配置：`exampleConfigs/zhang_global_2019199_e6.yaml`
- 网络：74 个估计站，GPS L1C/L2W，2019-07-18 00:00–06:00，300 s
- 产品记录：8760 行，73 个历元
- 运行时间：约 4 分 50 秒（非 smoke 全速配置）
- 结果汇总：`Docs/zhangPppArResults/e6_persistent_satellite_datum_summary.json`
- 可复现分析：`scripts/analyse_zhang_persistent_datum.py`

WSL 原始产物：

- `/home/rx/GINAN/inputData/outputs/zhang_global_2019199_e6/zhang_internal_products.csv`
- `/home/rx/GINAN/inputData/outputs/zhang_global_2019199_e6/Network-zhang_global_2019199_e6_persistent_satellite_datum-201919900.TRACE`
- `/home/rx/GINAN/inputData/run_logs/zhang_global_2019199_full_e6.log`

## 3. 六小时结果

### 3.1 有效性与版本

| 指标 | 结果 |
|---|---:|
| `integer_structure_valid=1` | 8760 / 8760 |
| `integer_datum_continuous=1` | 192 / 8760 |
| `integer_precision_valid=1` | 460 / 8760 |
| `integer_valid=1` | 96 / 8760 |
| 产品 datum version | 全部为 0 |
| 产品 discontinuity counter | 全部为 0 |
| 非零整数平移记录 | 460 |
| 非零平移值 | -7、-3、4、9、150 周 |

旧物理 product-tree 支撑映射仍会随站端 QC/缺测变化，其内部诊断计数不能再解释为产品 datum version。真正写入产品的持久卫星 datum version 在本试验中没有变化。

### 3.2 提升关系

02:15 首次固定 3 个 WL 组合和 3 个条件 L1 组合。精确单位目标恢复得到：

- L1C：`G01 -> G03 = 9`，`G01 -> G22 = -3`；
- L2W：`G01 -> G03 = 0`，`G01 -> G22 = 4`。

随后从非零物理 HNF 证明中提升了 G12–G25 关系：

- L1C：整数势差 150 周；
- L2W：整数势差 -7 周。

分析共读取 58 次已接受的提升/重复证明记录，无不一致整数桥。重复证明不会增加 datum version。

### 3.3 持久组件

| 信号 | 组件 | 最大卫星数 | 连续历元 | 连续时间 |
|---|---|---:|---:|---:|
| L1C | G01/G03/G22 组件 | 3 | 12 | 55 min |
| L2W | G01/G03/G22 组件 | 3 | 12 | 55 min |
| L1C | G12/G25 组件 | 2 | 9 | 40 min |
| L2W | G12/G25 组件 | 2 | 9 | 40 min |

组件公共 fractional gauge 修正把主双频组件的连续时间从第一版的 25 min 延长到 55 min。02:45 后 G03 的当前动态对齐无法继续得到严格整数证明，因此主组件的当前可输出部分降为 2 星；账本中的历史关系没有因接收机物理弧退休而删除。

## 4. 验收判断

| 文档目标 | 判断 | 证据 |
|---|---|---|
| 66 次产品 version 变化只保留真实卫星不连续 | 条件通过（产品层） | 8760 行产品 version/counter 均为 0；本时段无显式卫星 phase discontinuity，但上游自动卫星 discontinuity 分类器尚未完成 |
| 精确 tree exchange 不增加产品 version | 通过 | 组件级换树测试和六小时 version 序列均为 0 |
| 接收机弧段周跳不删除已提升关系 | 通过 | 源弧退休单元测试；持久关系跨多个物理支撑事件保留 |
| 持久双频组件连续至少 30 min | 通过 | 3 星主组件 55 min；2 星次组件 40 min |
| 单个组件包含 6–8 星 | **未通过** | 最大组件仍为 3 星；另有独立 2 星组件，缺少已固定的跨组件整数桥 |
| 产品目标非零覆盖 rank > 0 | 通过 | 02:15 具名目标 exact rank 为 2，并产生实际提升 |
| 出现实测非零整数平移 | 通过 | -7、-3、4、9、150 周 |
| 换参考卫星后整数关系不变 | 单元级通过 | 势差反对称/传递和 held-out user reference exchange 测试通过；尚无 20 用户站场景验收 |

## 5. 仍然存在的阻塞

本轮不能进入 24 小时和 20 用户站正式验证，因为“单个 6–8 星双频组件”仍未形成。当前结果不是 74 站覆盖不足，而是固定关系图缺少将 G01/G03/G22 与 G12/G25 两个产品组件连接起来的可靠双频整数桥。

此外，`recordZhangSatellitePhaseDiscontinuity` 已实现 phase segment/version 更新语义并有单元测试，但实际观测预处理目前只提供接收机—卫星物理弧事件，尚未自动判定“多站一致的卫星端 phase discontinuity”。因此本时段 version 为 0 是正确的观测结果，不等于自动卫星故障分类已经完成。

下一步应直接针对组件间候选桥构造低维 WL/条件 L1 目标，并在固定后调用同一提升接口。不能通过把两个未连通组件强行赋予相同 component ID 来满足验收；那会制造没有整数证明的 datum。

## 6. 回归测试

- Zhang full-rank：新增源弧退休、支撑路径切换、脱离子树、错误整数桥、动态换树、公共 fractional gauge、参考星交换和显式卫星 discontinuity 等测试；
- phase-clock/OSB 原有 4 个测试保留；
- `pea`、`zhang_full_rank_tests`、`phase_clock_osb_tests` 均完成构建。
