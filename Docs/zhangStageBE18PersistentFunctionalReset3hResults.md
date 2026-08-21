# E18 持久 canonical functional 与物理弧重置三小时结果

## 结论

三小时加长实验揭示并修复了一个只有在 02:19 才出现的生命周期错误。原实现把不可逆的局部相位坐标重初始化继续当作精确 S-basis 变换；当该投影删除持久物理 functional 所依赖的状态方向后，捕获链在 02:19 进入 fail-closed，并导致后续 42 个测量历元和 23 个结构事件级联拒绝。

修复后，纯 S-basis 交换仍必须通过逆转置 functional 传输；只有局部相位重初始化且持久 functional 明确不可表示时，才把事件分类为真实物理弧边界，关闭旧原始因子窗口并从当前最终接受测量重新锚定。第二次三小时运行完整处理 181 个测量历元，最终捕获状态有效，反馈始终为 0。

该结果只通过了结构与生命周期门禁，没有通过整数可靠性、噪声尺度、四树同观测 replay 或独立预测门禁，因此不能进入固定反馈、24 小时产品或 20 站 PPP/PPP-AR 验证。

## 修复前后对照

| 指标 | 修复前 | 修复后 |
|---|---:|---:|
| 测量事件日志 | 181 | 181 |
| 最终接受测量数 | 139 | 当前窗口 34；跨重置段合计 181 个历元均接受 |
| 精确结构事件日志 | 51 | 51 |
| GPS 树交换 | 24 | 24 次接受，0 次重置，0 次拒绝 |
| 局部重初始化 | 27 | 24 次精确接受，3 次物理弧重置，0 次拒绝 |
| 首个级联故障 | 02:19 `RAW_SQUARE_ROOT_PRIOR_MISMATCH` | 无 |
| 最终捕获状态 | `REJECTED` | `ACCEPTED`，`failure_reason=NONE` |
| 最终平方根边界相对误差 | 均值 5.24e-9，协方差 3.27e-6 后链断裂 | 均值 3.81e-11，协方差 1.93e-10 |
| 硬目标拒绝 | 216 | 0 |
| 静默 canonical 替换 | 0 | 0 |

修复后的三个物理弧边界分别位于 02:19、02:20 和 02:27。这三次事件均为局部相位坐标重初始化，且旧 functional 不能由新状态精确表示，因此候选连续性不适用。其余 48 次结构事件全部通过精确传输。24 次纯树交换的事件相邻 posterior 候选跳变为 0；24 次可传输局部重初始化中出现的 12 个事件相邻候选跳变混入了同历元测量更新，不能归因于 S 变换。确定性单元测试和运行时 `recordCoordinateTransform` 的 1e-10 functional 可表示性门禁才是纯变换不变量的权威判据。

## 窗口行为

物理弧重置前四个 30 历元增量窗口均保持 6 个 canonical 目标、信息秩 6、商空间秩 4和绝对秩 2：

| 时刻 | 残差自由度 | 残差平方和 |
|---|---:|---:|
| 00:29 | 34 | 7.98218 |
| 00:59 | 94 | 18.4638 |
| 01:29 | 154 | 26.7265 |
| 01:59 | 214 | 40.6173 |

旧实现 00:59 后曾出现百万级残差爆炸；本次未复现。02:19、02:20、02:27 的真实物理弧边界会合法清空旧窗口。02:27 至 03:00 之间不足 30 个连续历元，所以不能把重置前后的因子强行拼接。02:56 的增量 separator 仍保存 6 个物理坐标、商空间秩 4，但当前状态中六个 quotient functional 均不可表示，因此原始平方根目标边际正确返回 `NO_CURRENT_RAW_SQUARE_ROOT_TARGETS`。这是 fail-closed，不是数据缺失。

## 残差自由度与相关性

保留目标残差使用 `PREFIT_INNOVATION` 域。154 个有效块全部数值有效，0 个无效块；声明自由度 243 与白化残差个数 243 完全一致，投影掉的 gauge 秩累计为 118。

原残差平方和为 36.0255，自由度为 243。Ljung–Box 10 阶检验得到 p=2.087e-5，主要相关位于二阶（ACF=0.3315），因此独立白噪声假设被拒绝。BIC 选择 AR(2)：

\[
e_t = 0.00306 e_{t-1} + 0.33435 e_{t-2} + \varepsilon_t.
\]

AR(2) 后创新的 Ljung–Box p=0.672，相关性门禁通过；相关性修正自由度为 238。不过创新方差仅 0.1345，卡方 CDF 约为 4.63e-61，说明测量/目标协方差整体偏保守，尺度门禁仍失败。后续应估计一个独立于 AR 系数的稳健尺度因子，不能通过增加历元掩盖该问题。

## 整数与产品门禁

- 持久产品 datum 仍为按 L1C/L2W 和卫星关系分别版本化的 V0；本时段无产品 datum 版本变化。
- canonical 关系集合始终为 `G01->G02;G01->G03;G01->G05`，29 个历元出现可替代关系但全部被忽略，没有静默替换。
- 978 条目标记录中，386 条接受；592 条均为 `PERSISTENT_QUOTIENT_FUNCTIONAL_NOT_TRANSPORTABLE` 的合法 held quotient，硬拒绝为 0。
- 最小绝对目标错误概率约 0.0482，远高于验收阈值 1e-3。
- 重置后 02:56 的增量联合候选成功率仅 9.72e-5，ratio=1；不能固定。
- `RAW_SQUARE_ROOT_WL_L1_UNIMODULAR` 仍因 `UNPAIRED_K1_K2_INTEGER_RELATION` 被拒绝，说明当前可用的 L1C/L2W 目标并未形成用户域要求的成对 WL 整数关系。

因此 `feedback=0` 保持不变，`downstream_feedback_authorized=false`。

## 后续顺序

1. 在目标构造层形成同一卫星对、同一物理弧版本的 L1C/L2W 成对 functional，消除 `UNPAIRED_K1_K2_INTEGER_RELATION`。
2. 对 AR(2) 后创新估计稳健尺度，并用留出历元验证，不在训练窗口内自洽调参。
3. 完成四种树策略的同一最终接受原始因子 replay；纯树交换需逐事件满足均值、协方差和候选不变量。
4. 完成留出历元和留出接收机预测。只有 perr<=1e-3、独立预测一致且白化尺度合理后，才进入固定反馈。
5. 固定反馈通过后，才运行 24 小时产品和 20 个独立站 PPP/PPP-AR；验收同时检查坐标完整性、重启试验和 float 对照，不能只看固定率。

## 证据文件

- 结构化结果：`Docs/zhangPppArResults/e18_persistent_functional_reset_3h.json`
- 修复前结构化结果：`Docs/zhangPppArResults/e18_persistent_functional_3h.json`
- 运行配置：`exampleConfigs/zhang_global_2019199_e18_persistent_functional_reset_3h.yaml`
- 运行日志：`/home/rx/GINAN/inputData/run_logs/zhang_global_2019199_full_e18_persistent_functional_reset_3h.log`
- TRACE：`/home/rx/GINAN/inputData/outputs/zhang_global_2019199_e18_persistent_functional_reset_3h/Network-zhang_global_2019199_e18_persistent_functional_reset_3h-201919900.TRACE`
