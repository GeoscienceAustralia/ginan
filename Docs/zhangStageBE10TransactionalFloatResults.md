# 阶段 B E10：纯 float 主滤波器、事务式整数分支与数值门控实验

## 1. 方案审查结论

原方案的总体方向正确，但不能直接完整实施。静态状态变换

\[
[C_s, B^\phi_{s,j}] \longrightarrow [C_s, Q_{s,j}=C_s-B^\phi_{s,j}]
\]

是可逆的；然而在当前动态模型中，卫星钟采用一阶高斯—马尔可夫过程，而相位偏差没有相同的过程噪声。变换后的动态方程应包含由卫星钟噪声诱导的交叉过程噪声：

\[
\operatorname{Cov}(C_s,Q_{s,j})=q_C,
\qquad
\operatorname{Cov}(Q_{s,1},Q_{s,2})=q_C.
\]

当前 `KFState` 只支持对角过程噪声映射，不能正确表达上述相关性。因此，本次没有贸然切换到直接 `C/Q` 状态，而是先实施以下可独立验证的因果修复：

1. 权威网络滤波器始终保持纯 float，历史 held 整数和新整数均不得回写；
2. 每历元从 float 状态复制临时 fixed 分支；
3. 使用等式条件化和 NIS 检验取代近零方差伪观测；
4. 产品增加数值、分支、连续性、PPP 可用性和 PPP-AR 可用性标志；
5. 用户端对不可用的 Zhang 产品失败关闭，不再退回到混合定义的常规钟产品。

## 2. 实施内容

- 增加 `transactional_integer_fixing` 和 `held_constraint_nis_alpha` 配置项；
- fixed 分支克隆当前图运行时，但不污染权威 float 状态；
- 对整数等式约束执行秩感知协方差条件化、闭合检查和卡方 NIS 门控；
- 对半正定整数目标协方差先选择正条件方差子集，再执行整数搜索；
- 将事务失败显式写入产品状态，禁止失败分支进入用户端；
- 新增 E10 配置 `exampleConfigs/zhang_global_2019199_e10_transactional.yaml`；
- 新增机器可读统计：
  - `Docs/zhangPppArResults/e10_transactional_float_numeric.json`
  - `Docs/zhangPppArResults/e10_transactional_fixed_numeric.json`

## 3. 构建与回归测试

- `pea` 构建通过；
- `zhang_full_rank_tests`：43/43 通过；
- `phase_clock_osb_tests`：4/4 通过；
- E10 YAML 解析和运行脚本语法检查通过。

## 4. E10 短时实验

### 4.1 设置

- 日期：2019-07-18；
- 时间：00:00–02:10 GPST，共 131 个 60 s 历元；
- 网络：74 个估计站；
- 产品：GPS L1C/L2W，60 颗“卫星—信号”产品记录/历元；
- 总记录：FLOAT 7860 行，FIXED 7860 行。

从 00:00 开始而非只截取 01:58–02:10，是为了让持久整数台账、弧段和图拓扑具有与异常时段一致的历史。

### 4.2 FLOAT 产品

| 指标 | 结果 |
|---|---:|
| `numeric_valid` | 7860/7860 |
| `branch_valid` | 7860/7860 |
| `continuity_valid` | 7860/7860 |
| `ppp_usable` | 7860/7860 |
| `pppar_usable` | 0/7860 |
| 最大相邻历元改正数变化 | 26.2878 m |
| 大于 1000 m 的跳变 | 0 |

E9 在 02:04 附近曾出现约 \(2.21\times10^5\) m 的中位数异常、约 \(1.49\times10^7\) m 的最大异常，并继续发散到约 \(10^{39}\) 量级。E10 的纯 float 主滤波器在完整窗口内没有再出现这种爆炸。这证明旧异常的直接原因是 held/new 整数约束污染了后续权威滤波状态，而不是 74 站不足或历元太少。

产品改正数本身约为 229 km，主要包含钟差 datum 和接收机钟可吸收的公共部分，不能只凭绝对值判定产品错误；本次使用相邻历元变化、非公共变化和数值有限性来判断稳定性。

### 4.3 FIXED 分支

| 指标 | 结果 |
|---|---:|
| `numeric_valid` | 7860/7860 |
| `continuity_valid` | 7860/7860 |
| `branch_valid` / `ppp_usable` | 3480/7860 |
| `FIXED_TRANSACTION_ABORTED` | 4380/7860 |
| `pppar_usable` | 0/7860 |
| 最大相邻历元改正数变化 | 26.2245 m |
| 大于 1000 m 的跳变 | 0 |

秩感知整数搜索消除了半正定目标协方差导致的 LD 分解警告，并成功找到更多整数候选：

- `NEW_INTEGER APPLIED`：69 次；
- `NEW_INTEGER REJECTED`：63 次，全部由 NIS 拒绝；
- `PERSISTENT_HELD APPLIED`：98 次；
- `PERSISTENT_HELD REJECTED`：10 次，全部由 NIS 拒绝。

02:04 的诊断最能说明问题：

1. 拓扑变化后，held 约束由 10 行降为 8 行；NIS=15.9352，小于阈值 42.7009，成功应用；
2. 第一阶段新整数约束 NIS=15.0794，小于阈值 46.863，成功应用；
3. 第二阶段新整数约束 NIS=72.7308，大于阈值 46.863，被正确拒绝；
4. 目前的“整历元原子事务”随后回滚整个 fixed 分支，连第一阶段已经通过的结果也被丢弃。

因此，当前 fixed 可用率低不是数值秩问题，而是不同整数层之间存在真实的统计不相容，加上事务边界过大。

## 5. 验收判断

### 已通过

- 权威 float 主滤波器不再被整数约束污染；
- 等式条件化不会通过近零方差伪观测向后续历元注入病态信息；
- 数值异常和事务失败能够失败关闭；
- 旧 E9 的灾难性产品跳变已经消失；
- 半正定整数目标的搜索数值问题已经修复。

### 未通过

- 所有记录的 `pppar_usable` 仍为 0；
- fixed 分支有 73 个历元整体回滚；
- 当前产品整数有效性仍以过于粗糙的全局 held rank 判定，不能表达“宽巷已提交、单信号窄巷未提交”；
- 尚不具备开展 6 小时及 20 个独立站 PPP-AR 科学验收的条件。

在 `pppar_usable=0` 的情况下继续跑 20 站 PPP-AR，只会重复证明没有可用 AR 产品，不会提供新的根因信息。因此本次实验止于网络端因果验证，没有把固定失败误包装成用户端验收。

## 6. 下一步修复方案

下一轮应实施分层、部分可提交事务，而不是放松 NIS 阈值：

1. held 分支作为本历元 fixed 基线；
2. 宽巷约束作为独立原子子事务，成功后立即保留该层结果；
3. L1/窄巷约束在宽巷分支副本上执行；若失败，只回滚该层；
4. 持久台账只提升已提交子事务对应的整数关系；
5. 对临界 held 冲突按行或按连通块隔离，连续 3–5 个兼容历元后再恢复；
6. 产品按信号和目标秩分别记录 `wide_lane_valid`、`narrow_lane_valid` 与 `pppar_usable`，不得再用全状态 ambiguity 数量判断整数有效；
7. fixed 可用率和信号级整数闭合通过后，再运行 6 小时网络实验以及 20 站 float/PPP-AR/重启对照。

直接 `C/Q` 状态改造应等到滤波器支持交叉过程噪声后再做，否则静态满秩正确但动态统计模型错误。

## 7. 结果位置

- 当前完整输出：`/home/rx/GINAN/inputData/outputs/zhang_global_2019199_e10_transactional`
- 主 TRACE：`/home/rx/GINAN/inputData/outputs/zhang_global_2019199_e10_transactional/Network-zhang_global_2019199_e10_transactional_fixed_branch-201919900.TRACE`
- 产品 CSV：`/home/rx/GINAN/inputData/outputs/zhang_global_2019199_e10_transactional/zhang_internal_products.csv`
- 运行日志：`/home/rx/GINAN/inputData/run_logs/zhang_global_2019199_full_e10_transactional.log`
