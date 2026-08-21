# 阶段 B E12–E13：持久关系局部隔离与产品完整性门控结果

日期：2026-08-04
分支：`codex/zhang-full-rank-ppp-rtk`

## 1. 本轮修改

### E12：持久 held 块局部隔离

1. 为每条 held 约束保留其卫星—信号产品支撑集；
2. held 整块不相容时选择联合 NIS 兼容子集，而不是中止整个 fixed 分支；
3. 对被剔除关系只隔离当前产品对齐，保留已经确认的持久关系森林；
4. 未受影响子集继续条件化；
5. 被隔离关系仍需连续 5 个历元重新确认后才允许恢复对齐。

### E13：数值回退与产品完整性门控

E12 六小时试验发现两类新失效后，增加：

1. held 子集条件化发生协方差数值失效时，逐步移除最差行并重新条件化，直至得到数值可接受的联合子集；
2. `maximum_pppar_correction_sigma_m = 0.5 m`：产品改正数标准差超过门限时不得进入整数精度有效状态；
3. `maximum_product_residual_step_m = 0.5 m`：去除同信号历元公共模后的逐星跳变超过门限时，PPP 和 PPP-AR 均失败关闭；
4. 新获得或重新获得整数精度后重新执行稳定期，不沿用旧的连续性状态。

这些门限只决定是否发布，不改变 FLOAT 权威网络滤波状态，也没有放宽 NIS 或缩短 5 历元确认条件。

## 2. 实验对照

| 实验 | 时段 | FIXED 中止 | PPP-AR 可用行 | 关键结果 |
|---|---:|---:|---:|---|
| E11-v3 | 00:00–02:10 | 21 | 1128 | 首次形成产品图，但 held 冲突导致整历元回滚 |
| E12 短窗 | 00:00–02:10 | 0 | 1171 | 局部隔离消除短窗整历元回滚 |
| E12 6 h | 00:00–06:00 | 8 | 1639 | 02:19–02:26 协方差数值失效；05:09 错发 42.58 m 残差跳变产品 |
| E13 定向 | 00:00–02:30 | 0 | 1169 | 37 次数值回退成功；仅 1 条整数标志产品因连续性超限被拒发 |
| E13 6 h | 00:00–06:00 | 0 | 1316 | 米级异常全部拒发，但可用整数产品覆盖仍不足 |

## 3. E12 六小时失败原因

### 3.1 held 联合子集仍可能破坏协方差

02:19–02:26 共 8 个历元出现 `CONDITIONED_STATE_NUMERIC_FAILURE`。这不是 held 联合 NIS 拒绝，而是条件化后的协方差出现负对角元或闭合误差，说明“统计兼容的行集”不等于“数值上可以安全施加的行集”。

E13 在这些历元触发 37 次 `ZHANG_HELD_BLOCK_NUMERIC_FALLBACK`，每次移除一个造成数值问题的行后重试，最终 8 个历元全部成功，fixed 分支中止数由 8 降为 0。

### 3.2 `integer_valid` 曾漏掉产品协方差条件

E12 在 05:09 的 G01/L1C 产品中：

- 改正数标准差：70.536 m；
- 原始相邻历元变化：57.45 m；
- 去公共模残差跳变：42.58 m；
- 旧实现仍标记为 `pppar_usable=1`。

这证明原 P3 只验证了整数关系、当前对齐和 fixed 分支，没有验证实际输出的钟差—相位偏差组合协方差与卫星相关跳变，因而不能作为产品完整性判据。

## 4. E13 六小时结果

### 4.1 数值安全与失败关闭

| 指标 | FLOAT | FIXED |
|---|---:|---:|
| 总行数 | 21660 | 21660 |
| numeric valid | 21660 | 21660 |
| branch valid | 21660 | 21660 |
| continuity / PPP usable | 19814 | 19846 |
| integer valid | 0 | 1326 |
| PPP-AR usable | 0 | 1316 |

- 361 个历元全部完成；
- fixed 事务中止：0；
- FLOAT 与 FIXED 均无数值发散；
- E12 的 05:09 G01/L1C 异常仍存在，但 E13 将其标记为 `integer_precision_valid=0`、`continuity_valid=0`、`pppar_usable=0`；
- 10 条具有 `integer_valid=1` 但跳变超限的产品被连续性门控拒发；
- 1316 条实际发布的 PPP-AR 产品改正数标准差最大为 0.4856 m；
- 实际发布 PPP-AR 产品的最大去公共模历元跳变为 0.4595 m。

因此，E13 解决的是“异常产品不得发布”，并没有消除产生异常的网络估计源。

### 4.2 可用性仍不合格

- PPP-AR 至少有一条可用产品的历元：206/361；
- 最长连续 PPP-AR 时段：111 个历元，即 110 min；
- 最大双频有效卫星数：8；
- 最长有效双频分量：108 个历元，即 107 min；
- 04:25 后基本失去双频整数产品，05:41–05:43 仅短暂恢复少量单信号产品；
- 普通 FLOAT/FIXED PPP 产品分别有 1846/1814 行因去公共模跳变超过 0.5 m 被拒发。

这表明当前主要矛盾已经从“错误产品被发布”转为“产品图和精度状态在后半段衰退”。增加用户站或直接跑 PPP-AR 不能修复该问题。

## 5. 门禁结论

### 已通过

1. 74 站、361 历元网络滤波完整运行；
2. held 局部冲突不再导致 fixed 分支中止；
3. 产品精度和连续性门控能够拦截已知米级异常；
4. 所有实际发布 PPP-AR 产品满足本轮 0.5 m 精度门限和连续性门限；
5. 两个单元测试套件均通过。

### 未通过

1. 双频整数产品不能覆盖完整 6 小时；
2. PPP 产品存在大量卫星相关不连续；
3. 04:25 后持久产品图退化为单卫星或无有效双频分量；
4. 仍未产生足够连续的产品供 20 个独立站做公平的 PPP/PPP-AR 对照。

因此，20 站用户验证继续受门禁阻塞。下一轮应针对产品图衰退和普通 PPP 产品跳变做事件级归因，至少区分：观测弧结束/重启、动态 S-basis 切换、持久对齐失效、产品协方差膨胀和网络共同参数变化。只有在 6 小时窗口内恢复持续双频覆盖后，才进入独立重启和坐标完整性验证。

## 6. 可复核产物

- E13 6 h 配置：`exampleConfigs/zhang_global_2019199_e13_integrity_6h.yaml`
- FLOAT 审计：`Docs/zhangPppArResults/e13_integrity_6h_float_numeric.json`
- FIXED 审计：`Docs/zhangPppArResults/e13_integrity_6h_fixed_numeric.json`
- 持久 datum 审计：`Docs/zhangPppArResults/e13_integrity_6h_persistent_datum.json`
- WSL 产品：`/home/rx/GINAN/inputData/outputs/zhang_global_2019199_e13_integrity_6h/zhang_internal_products.csv`
- WSL TRACE：`/home/rx/GINAN/inputData/outputs/zhang_global_2019199_e13_integrity_6h/Network-zhang_global_2019199_e13_integrity_6h-201919900.TRACE`
- WSL 日志：`/home/rx/GINAN/inputData/run_logs/zhang_global_2019199_full_e13_integrity_6h.log`
