# E18 增量固定滞后与通用整数目标修改记录

日期：2026-08-06

## 1. 本轮修正的统计定义

四类残差现在是不同类型，分别保存各自的自由度：

1. `PREFIT_INNOVATION`：`v=y-H*x_minus`，协方差
   `S=H*P_minus*H^T+R`，自由度为 `rank(S)`；
2. `BATCH_ORTHOGONAL`：白化批量系统 `A*x=b` 的左零空间残差，
   自由度为 `rows(A)-rank(A)`；
3. `HELD_OUT_PREDICTION`：未参与估计的数据形成的预测残差，协方差为
   `R_hold+H_hold*P_train*H_hold^T`；
4. `TARGET_TO_INTEGER`：目标均值与给定整数候选之差，先去掉 quotient
   方向，再按投影后的协方差白化。

分析脚本不再把目标信息秩当作卡方自由度。旧 trace 如果没有明确的
`retained_block_residual_dof`，不会再计算卡方 CDF。

若 `D` 的列张成未解析 quotient/gauge 方向，则使用

`U=ker(D^T)`，

并对白化量

`U^T(mu-z)`，`U^T*Q*U`

进行统计检验。实际 E18 capture 会按系统、信号组合和 topology key 记录
共享 gauge identity，并输出 residual domain、residual dof 和 projected gauge
rank。

## 2. 真正增量的平方根核心

新增 `ZhangIncrementalFixedLagSquareRoot`。该类不保存历史测量矩阵：

1. 初始边界高斯转换为平方根信息因子；
2. 新历元只追加转移和本历元最终接受的测量因子；
3. 本历元局部 nuisance 作为临时前置列，调用返回前即投影消去；
4. 活动 separator 因子经列主元 QR 压缩为有界平方根先验；
5. 活动历元超过 lag 时，最旧 separator 只边缘化一次；
6. 精确 S-basis 变化使用 `x_new=T*x_old+b` 的代数替换，不作为随机约束；
7. 保存规模上限为 `separator_dimension * lag`，与总历史历元数无关。

2、5、10 历元人工系统结果如下：

| 历元 | 增量与 dense batch 均值误差 | 协方差相对误差 | 存储因子 |
|---:|---:|---:|---:|
| 2 | `1.39e-16` | `2.02e-16` | `4 x 4` |
| 5 | `1.67e-16` | `1.22e-16` | `6 x 6` |
| 10 | `1.00e-16` | `1.33e-16` | `6 x 6` |

上述结果同时与顺序 Kalman 后验满足 `1e-10` 门限。另有独立测试证明：

- epoch-local nuisance 在方法返回前已消去；
- 仿射精确 S-basis 变化后的均值和协方差与直接变换一致；
- lag=3 时从 5 历元到 10 历元，存储矩阵仍为 `6 x 6`。

真实滤波另新增 `ZhangIncrementalRawSquareRoot`。它保存
`x=a+Bz, Rz=d` 的仿射随机支持，因此能精确表示含零方差确定性状态的
半正定协方差，不通过添加小对角噪声伪造满秩。最终接受的 H/R、状态转移
F/Q 和精确 S 变换均在该边界上递推；离开边界的历史因子不保留。专门的
半正定先验和半正定过程噪声测试均通过。

## 3. 通用 primitive 整数目标

固定滞后数学核心不再定义 WL 专用状态。基础整数目标写成

`k=[k1,k2,...]^T`，`z=Z^T*k`。

新增能力包括：

- 对 `Z` 做满列秩、最大子式 gcd、primitive 和 unimodular 审计；
- 直接联合 `I^T*k`；
- 完整 `WL/L1` unimodular 变换；
- 接收 LAMBDA 输出的任意 unimodular decorrelation 变换；
- 对给定最优和次优候选统一报告距离、比值和 bootstrap 联合成功率；
- 按可靠性选取 PAR 子集；
- 报告 quotient/absolute rank、产品关系图秩、通过目标数、可恢复卫星数
  和环闭合误差。

非 primitive 变换（例如对某个基础整数乘 2）会被拒绝，不能作为完整
整数坐标交给 LAMBDA。

## 4. 当前验证结果

`zhang_full_rank_tests` 共 72 项，全部通过。人工残差示例明确输出：

- prefit dof：2；
- batch orthogonal dof：3；
- held-out dof：2；
- quotient 投影后的 target-to-integer dof：1。

联合整数诊断人工结果：

- quotient-valid rank：3；
- absolute-valid rank：2；
- 产品关系图秩：2；
- 最优候选距离：0.365535；
- 次优候选距离：89.5938；
- 距离比：245.103；
- 通过目标数：3；
- bootstrap 联合成功率：0.999994889；
- 可恢复卫星：3；
- 环闭合误差：0。

真实 74 站路径已经改为保存 K1/K2 primitive base targets。最新 9 分钟
原始因子平方根 shadow 结果为：

- 连续接受 9 个最终量测块、16 个转移和 2 次精确 S 变换，无 capture
  reject；边界维数最大为 `2075 x 2075`，不随历史行数增长；
- 原始量测行累计 21824，物理 base target 信息秩 6，未解析整数 datum
  秩 2，商空间秩 4，绝对秩 0；
- 物理目标均值和方差与当前滤波状态直接投影在 trace 精度内一致；
- 原始平方根 direct joint 最优/次优距离为 `3.85284/5.01198`，比值
  `1.30085`，bootstrap 联合成功率 `0.524198`；
- LAMBDA 未通过，`P(success)>=0.999` 的 PAR 子集为空；
- 原始创新正交量为 `2370.15/21824 dof`，卡方 CDF 数值下溢为 0，说明
  当前随机模型显著偏保守或自由度仍需按可估 nuisance 进一步校准；
- 9 分钟耗时 3 分 23 秒，峰值内存约 3.65 GB。当前稠密 QR 不能直接
  扩展到三小时，必须先做稀疏/分块 Schur 优化。

旧 target-increment separator 在同一末历元给出均值
`[-176.675,-23.0386,9.19843,0.287656,-39.4427,41.5311]`，而原始因子
路径给出 `[-178.825,-25.6445,9.47277,0.55892,-41.5919,38.9254]`；其
direct joint 最优候选也从原始路径的 `[185,136,22,63]` 变为
`[184,136,21,63]`。因此旧 separator 只保留为诊断，不能再作为整数产品
判定依据。

direct joint、完整 WL/L1 unimodular 坐标和 LAMBDA decorrelated 坐标给出
相同的最优/次优 ILS 距离，验证了坐标变换不改变整数格搜索结论。标量
`Perr` 只保留为坐标相关诊断，不再作为验收门禁。

## 5. 物理连续性与 datum promotion 修复

真实 replay 发现并修正了两个不能靠增加历元解决的状态问题：

1. phase segment 不变且 `physical_identity_resets=0` 时，源弧表示变化曾
   错误地产生新 separator key。现在纯 S-basis/支撑路径变化沿用原物理
   key；同一物理弧版本或 phase segment 真变化仍重置。
2. 一个目标获得绝对 integer datum 时，共享 gauge 从未锚定变为已锚定。
   这不是冲突，而是合法秩提升。separator 会将同组连续目标提升为 absolute，
   并按整数 offset 差执行精确仿射平移，不能只改布尔元数据。

上述行为均有单元测试：坐标变化保持 key、真实弧版本变化重置、共享 gauge
promotion、整数 offset 平移，以及后续未解析元数据不降级已锚定 datum。

## 6. 尚未完成的边界

当前仍是 `feedback=0` 的 shadow。进入产品反馈前还必须完成：

1. 将当前约 2000 维稠密边界改为稀疏平方根/分块 Schur，只保留产品目标
   separator 和必要跨历元状态，先通过 2/5/10 真实历元回归；
2. 四树同观测 replay，比较物理均值、完整协方差和整数候选；
3. held-out epoch 与 held-out receiver 独立预测；
4. 用可估秩定义原始因子残差自由度，并完成白化 ACF/Ljung-Box 与卡方
   门禁；
5. 通过后才允许三小时 shadow、固定反馈、24 小时产品和 20 站
   PPP/PPP-AR。

35 分钟实测已证明仅增加历元不足以放行：

- target separator：`6 x 6`，absolute/quotient/information rank 均为 6；
- datum promotion offset：`[0,0,-9,0,0,0]`，整数平移后不再产生大残差；
- direct、WL/L1 和 LAMBDA decorrelated 的最优/次优距离均为
  `17.3452/25.1365`；
- bootstrap 联合成功率 `0.99571`，LAMBDA validation 未通过，可靠 PAR
  子集为空；
- batch orthogonal residual 为 `77.163/204 dof`，卡方 CDF
  `1.88e-17`，明显不服从声明的白化模型。

因此三小时实验目前被统计门禁阻止。下一实现必须在状态级平方根因子图中
保留跨历元相关，再消去钟差、接收机相位、电离层等 nuisance；不能继续
把目标边缘后验的信息差当成彼此独立的历元似然。

原始 H/R、F/Q 平方根路径的 35 分钟复现实验进一步得到：

- 35 个量测块、68 个转移、6 次精确 S 变换全部连续接受；最大边界为
  `2144 x 2144`，没有历史行泄漏；
- 完整状态最大均值/协方差相对差为 `5.24e-9/3.27e-6`，但最终 6 个物理
  目标的均值和方差与主滤波直接投影在 trace 精度内一致；
- datum promotion 后 information/quotient/absolute rank 均为 6，产品关系
  图秩为 3，可恢复 4 颗卫星；
- direct、WL/L1、LAMBDA-decorrelated 的 ILS 距离均为
  `16.004/27.7992`，比值 `1.73701`；最佳整数候选在 raw 与 shortcut
  路径之间相同；
- LAMBDA ratio 检验通过，但 bootstrap 联合成功率只有 `0.996517`，仍低于
  `0.999` 门禁，可靠 PAR 子集为空；因此不能反馈；
- 原始创新统计为 `11400.7/86172 dof`，卡方 CDF 为 0；当前把全部量测行
  当自由度的模型明显不成立，必须按消去后的独立残差秩重新定义；
- 耗时 14 分 41 秒，峰值内存 3.75 GB。内存基本有界，但稠密 QR 的时间
  复杂度不允许四树三小时直接 replay。

这说明此前阻塞中“datum transport 无效”的部分已经排除；当前阻塞是
`P(success)` 仍不足、残差自由度/随机模型不闭合，以及实现性能。单纯继续
增加历元可能提高成功率，但在解决统计定义和稀疏化之前不能作为合格证据。

## 7. 文献审查与实施约束

1. Khodabandeh 与 Teunissen 的整数可估理论证明：网络生成树可作为
   S-basis，但可固定的用户整数函数仍是网络与用户联合模型中的双差型
   整数可估函数。因此本实现必须先投影 quotient，再执行 LAMBDA，不能把
   单个未锚定 OSB 当绝对整数。
2. Demmel 等的平方根滑窗边缘化证明：直接 QR 边缘化与 Hessian Schur
   补代数等价，并能自然处理秩亏 Jacobian。后续应保留 Jacobian/平方根，
   不构造法方程，也不对病态协方差普通 Cholesky。
3. Kaess 等的 iSAM2 说明增量效率来自稀疏因子分解、局部重排序和只更新
   受影响 clique；当前每历元重做 2000 维稠密 QR 不满足该条件。
4. 2026 年 Platz 的 PPP-RTK 统一模型工作明确把 LC 预消元和更小观测空间
   作为全球网络产品生成的计算优势，同时要求与 UDUC 用户模型解析等价。
   后续 nuisance 分块必须从完整用户方程推导，不能凭状态名任意删除。
5. CODE 和 all-frequency OSB 产品研究都强调卫星钟 datum、code/phase OSB
   datum 与用户观测模型的一致性，并使用独立于估计网的站点评估。20 站
   PPP/PPP-AR 仍是最终不可替代的门禁。

对应的下一实现顺序为：按状态转移和用户泛函构造可证明的 local/persistent
分块；对 local nuisance 做稀疏 QR；保留 persistent clique 与 6 维产品目标
separator；通过 2/5/10 历元与当前稠密 raw 路径的目标均值、协方差和整数
候选回归；然后才运行四树同观测 replay、三小时 shadow 和独立预测。

主要参考：

- Khodabandeh, A., Teunissen, P.J.G. (2019), *Integer estimability in GNSS
  networks*, Journal of Geodesy, https://doi.org/10.1007/s00190-019-01282-6
- Demmel, N. et al. (2021), *Square Root Marginalization for Sliding-Window
  Bundle Adjustment*, ICCV, https://arxiv.org/abs/2109.02182
- Kaess, M. et al. (2012), *iSAM2: Incremental Smoothing and Mapping Using the
  Bayes Tree*, IJRR, https://www.cs.cmu.edu/~kaess/pub/Kaess12ijrr.html
- Schaer, S. et al. (2021), *The CODE ambiguity-fixed clock and phase bias
  analysis products*, Journal of Geodesy,
  https://doi.org/10.1007/s00190-021-01521-9
- Geng, J. et al. (2022), *GNSS observable-specific phase biases for
  all-frequency PPP ambiguity resolution*, Journal of Geodesy,
  https://doi.org/10.1007/s00190-022-01602-3
- Platz, H.D. (2026), *Unifying linear combination and undifferenced uncombined
  observation modeling in PPP-RTK*, TU Darmstadt,
  https://doi.org/10.26083/tuda-8023

## 8. 三小时原始平方根 shadow 实验

在保持 `feedback=0` 的条件下，将同一 74 站、60 s 处理扩展至
2019-07-18 00:00--03:00。配置按闭区间处理 181 个历元。运行正常退出，
共捕获 181 个最终接受量测块、360 个状态转移、51 次精确 S 变换；观测到
24 次 GPS tree exchange 和 27 次 local reinitialisation，capture reject 为 0。
完整状态 replay 的最大均值/协方差相对差仍为
`5.24e-9/3.27e-6`，说明原始因子捕获链本身在三小时内连续。

每 30 个接受量测块重新评估一次同一套产品目标。原始平方根 direct joint
诊断如下：

| 窗口 | quotient rank | absolute rank | 未解 gauge | ratio | 联合成功率 | LAMBDA | 最佳候选 |
|---:|---:|---:|---:|---:|---:|---:|---|
| 30 min | 6 | 6 | 0 | 2.06286 | 0.991492 | 通过 | `[-176,-22,0,0,-40,41]` |
| 60 min | 6 | 6 | 0 | 1.38731 | 0.871147 | 不通过 | `[0,0,-67,4,-95,65]` |
| 90 min | 5 | 3 | 1 | 2.58737 | 0.949407 | 不通过 | `[-178,0,-138,92,18]` |
| 120 min | 6 | 6 | 0 | 13.7084 | 1.000000 | 通过 | `[-178,-92,0,0,-199,-107]` |
| 150 min | 5 | 3 | 1 | 1.00019 | 0.0252976 | 不通过 | `[-93,-16,-108,222,-21]` |
| 180 min | 4 | 0 | 2 | 1.21197 | 0.403111 | 不通过 | `[120,-25,11,-29]` |

不能把 120 分钟的 `P(success)=1` 解释为成功。它与前后窗口的整数候选不
一致，而且六个检查点的可靠 PAR 子集全部为空。90 分钟三个 L2W 目标的
产品 datum 未解析，150 分钟则变成三个 L1C 目标未解析；180 分钟两个频率
的绝对 datum 均未解析，绝对有效秩降为 0。这说明新增历元并未持续增强同一
物理整数向量，而是跨越了频率 datum 重新对齐、树交换和局部重初始化事件。
当前 target projection 只能在单个检查点内形成商空间整数格，尚未证明能把
不同检查点严格输运到同一持久整数坐标系。

统计门禁也未通过。最终原始因子正交残差为
`67051.1/446584 dof`，卡方 CDF 数值下溢为 0；保留目标白化残差为
`108.739/701 dof`。其 Ljung--Box `Q(10)=26.852`，上尾
`p=0.00275`，拒绝白噪声假设，且 lag-4 ACF 为 0.1817。因此当前自由度、
随机模型或相关性消元至少有一项仍未闭合。

运行墙钟为 1 h 28 min 44 s，峰值 RSS 为 4.05 GB，无 swap。边界最大为
`2284 x 2284`，内存仍基本有界；但普通历元耗时由前段约 22--26 s 增至
后段约 35--41 s，四树三小时 replay 在当前稠密实现下成本过高。

本实验否定“只要把历元增加到三小时即可固定”的假设。下一步不应继续
盲目加长到 6 h 或 24 h，而应先实现：

1. 为 K1/L1C、K2/L2W 分别建立持久、版本化的产品 datum 节点；只有真实
   datum 事件才创建新版本，纯 S-basis 交换只执行精确整数仿射输运；
2. 将每个窗口的目标行、均值、协方差和整数候选统一输运到固定 canonical
   satellite-pair/frequency 坐标，输运矩阵必须为整数单模或明确标记不可
   比较，禁止静默换目标；
3. datum 暂时不可绝对观时只保留 quotient 关系，不得用后续绝对锚点回填
   先前不同版本，也不得将 L1/L2 的 gauge 交替当作同一整数向量；
4. 以 51 次 S 变换、24 次树交换和 27 次局部重初始化为事件清单，逐事件
   审计候选跳变；先通过同一物理弧跨事件候选严格不变测试；
5. 按消元后残差信息秩重新定义自由度，并对剩余相关性建模；卡方与
   Ljung--Box 门禁通过后，再进行四树同观测 replay。

结构化结果保存在 `Docs/zhangPppArResults/e18_raw_square_root_3h.json`。
固定反馈、24 小时产品和 20 站 PPP/PPP-AR 仍不授权。
