# Stage B 产品 datum 支撑归因与直接产品目标实验

## 1. 实验目的

本轮针对“74 个站是否过少”以及上一轮 66 次产品 datum version 变化，执行两组独立实验：

- E4：保持 E3 估计策略不变，只增加产品树换边归因、物理替代路径、卫星关系支撑数、bridge 和边连通度诊断；
- E5：保持产品门关闭，改为直接对精确 `G_sat` 产品目标执行 WL→条件 L1 LAMBDA/PAR。

两组实验均使用 74 个估计站、2019-07-18 00:00–06:00、300 s 间隔。20 个独立验证站未进入估计，也未启动用户端 PPP-AR。

## 2. 实现修正

### 2.1 arc version 纳入产品 datum 身份

产品树身份不再只比较接收机—卫星拓扑边。若同一拓扑边的 `arcVersion` 变化，该边也视为新的物理整数坐标，并参与 datum 连续性判定和事件输出。

### 2.2 E4 支撑诊断

每次产品树变化输出：

- 换出的旧物理边与加入的新物理边；
- 事件原因与受影响信号；
- 新旧 arc version；
- 物理边删除后的边不相交替代路径数；
- 物理边是否为 bridge；
- 卫星关系的共同接收机支撑数；
- 卫星产品图 bridge 数和以支撑数为容量的边连通度；
- 产品树物理边最小、平均支撑数及 bridge 数。

这里的“替代路径”是整数关联结构上可构造的物理路径，不等于该路径切换整数已经被 held lattice 固定。后者仍需产品目标整数覆盖证明。

### 2.3 E5 直接产品目标

E5 不再先固定普通网络循环后寻找产品方向，而是构造：

`b_WL = (G_sat,L1 - G_sat,L2) k`，

先固定 `b_WL`，反馈至完整滤波状态与协方差，再固定条件 `b_L1 = G_sat,L1 k`。只使用所有非零循环列均存在 L1/L2 AR 状态的目标行，并从中选择精确独立的原始卫星目标行；缺失状态不会被补造。

## 3. E4：66 次 datum break 的归因

E4 与 E3 的固定事件、精确 HNF 插入和 held lattice 序列逐条一致，证明新增诊断没有改变估计。

### 3.1 互斥主因

对每个 datum version break 按“确认周跳 > 站点/QC 剔除 > 临时观测损失”的优先级归为一个主因：

| 主因 | datum break 数 | 比例 |
|---|---:|---:|
| `CONFIRMED_CYCLE_SLIP` | 29 | 43.9% |
| `STATION_QC_REMOVAL` | 34 | 51.5% |
| `TEMPORARY_OBSERVATION_LOSS` | 3 | 4.5% |
| `PRODUCT_EDGE_NO_ALTERNATIVE_SUPPORT` | 0 | 0% |
| 合计 | 66 | 100% |

`STATION_QC_REMOVAL` 包括已有 `obs.exclude` 和高度角门限剔除，不应解释为 34 次整站永久退出。

### 3.2 产品树物理边支撑

共有 191 条旧产品树物理边被换出：

- 190 条存在至少 3 条替代路径，即总支撑数为 4–13；
- 仅 1 条无替代路径：01:25 的 `SUTM:G19`，原因是临时观测损失；
- 支撑数集中在 7–10：151/191 条，占 79.1%；
- 全部 66 次 datum break 中，没有一次以“无替代支撑”为主因。

旧边总支撑数分布：

| 支撑数 | 换出边数 |
|---:|---:|
| 1 | 1 |
| 4 | 2 |
| 5 | 4 |
| 6 | 8 |
| 7 | 25 |
| 8 | 46 |
| 9 | 44 |
| 10 | 36 |
| 11 | 17 |
| 12 | 5 |
| 13 | 3 |

### 3.3 上层卫星关系图

68 条冗余度记录均显示：

- 30 颗 GPS 卫星位于同一卫星关系组件；
- 卫星关系图 bridge 最大值为 0；
- 以共同接收机支撑数为容量的边连通度最小为 4；
- 单条卫星关系支撑数范围为 1–30；
- 初始化时产品树有 1 条低支撑物理边，随后绝大多数产品树边具有多路径支撑。

因此，74 站不是本次 datum 高频变化的主要瓶颈。网络通常已经提供替代物理路径，失败发生在产品层：旧实现将产品 datum 绑定到具体物理树边，不能利用替代路径保持同一卫星产品关系。

换边最频繁的接收机为 ALIC 11、BAKE 10、AGGO 9、ANKR 8、USN7 8；最频繁涉及的卫星为 G05 34、G02 27、G01 25、G03 21。该统计适合后续站点/区域质量诊断，但不足以支持直接增加到 120 站。

## 4. E5：直接产品目标 LAMBDA

### 4.1 固定结果

仅在 02:15 出现一次成功固定：

| 阶段 | 可估目标秩 | 固定数 |
|---|---:|---:|
| 产品 WL | 22 | 3 |
| WL 条件下产品 L1 | 22 | 3 |

全程产品 WL 的完整目标秩和可映射目标秩最大均达到 29。说明 74 站可以形成满维产品目标，也可以把目标正确送入 LAMBDA/PAR。

### 4.2 非零产品覆盖

直接目标固定后：

- L1C、L2W、WL 的非零覆盖秩均由 0 提升到 2；
- 非零覆盖出现在 02:15、02:20、02:25 三个历元；
- 产生 78 条非零整数平移记录，整数值范围为 -9 至 157 cycles；
- 这是首次在实网 6 小时实验中证明 held lattice 与非零 `G_sat` 产品方向存在交。

### 4.3 覆盖失效

02:30 产品树更换 5 条物理边并将 datum version 更新至 26。换出的旧边支撑数分别为 9、8、7、10、10，全部不是 bridge，但产品覆盖秩立即从 2 降为 0。

与此同时精确 held lattice 并未清零：

| 历元 | held HNF 秩 | 产品覆盖秩 |
|---|---:|---:|
| 02:15 | 6 | 2 |
| 02:20 | 6 | 2 |
| 02:25 | 6 | 2 |
| 02:30 | 4 | 0 |
| 03:00 | 2 | 0 |
| 03:15 | 0 | 0 |

这证明两个失败层可以严格分开：直接目标 LAMBDA 已解决“固定方向不覆盖产品目标”的问题，但产品 datum 仍不能在已有替代路径之间执行可证明的整数换基。

## 5. 确定性与安全门

E5 使用同一输入完整复跑两次，下列记录逐条一致：

| 记录 | 条数 | SHA-256 |
|---|---:|---|
| `ZHANG_PRODUCT_TARGET_AR_RESULT` | 146 | `abbfc889c8f9455ab6a8008ee42067ac9ec8dc7b8a7354272f6994b99d841d45` |
| `ZHANG_HELD_LATTICE_EVENT` | 11 | `f56400c22fdc02ffb73eec5b311b3d0458dde0b0a6d9335f07c9640d4b3f8d06` |
| `ZHANG_SATELLITE_INTEGER_LATTICE` | 146 | `0baac51a22738049f12933975c7cd2eb79adff3afb7b0cd61362f1e082890146` |
| `ZHANG_SATELLITE_INTEGER_EDGE` | 2115 | `fb3e2fdfe54291a40a0d26d14cd362f2182114269a915b8e4333b22ca9cebc8d` |
| `ZHANG_PRODUCT_DATUM_EDGE_EVENT` | 193 | `2dcf67592ba834e1670f970f60ae416f224dcf60f606cbddcdb937cd4d3aaa8e` |

30 个 Zhang/精确格测试与 4 个 phase-clock/OSB 测试通过。E4/E5 产品 CSV 均为 8760 行，`integer_valid` 全部为 0；没有放行产品或启动独立用户验证。

## 6. 结论与下一步

实验支持以下结论：

1. 74 站已提供满维瞬时产品目标及大量物理替代路径；总体站数不是当前第一瓶颈。
2. 普通网络循环固定方向确实不适合产品目标；直接 `G_sat` WL→条件 L1 固定首次得到非零产品覆盖。
3. 当前最主要的剩余问题是产品 datum 仍绑定具体物理边。替代路径虽然存在，但切换所需循环整数没有被产品关系层持续管理和认证。

下一步应建立持久的卫星产品关系树。每条卫星产品边保存多个物理支撑路径；支撑切换时，只有当两条路径之差属于 held integer lattice 并能计算实际整数平移量时，才保持 datum version，否则明确断开该局部产品组件。完成该机制后，仍应先用 74 站、6 小时、300 秒回归；当前不进入 60 秒、24 小时、120 站消融或 20 站 PPP-AR。

## 7. 产物

- E4 TRACE：`/home/rx/GINAN/inputData/outputs/zhang_global_2019199_e4/Network-zhang_global_2019199_e4_product_datum_attribution-201919900.TRACE`
- E4 汇总：`/home/rx/GINAN/inputData/outputs/zhang_global_2019199_e4/integer_audit_attribution_6h.json`
- E5 TRACE：`/home/rx/GINAN/inputData/outputs/zhang_global_2019199_e5/Network-zhang_global_2019199_e5_direct_product_target-201919900.TRACE`
- E5 汇总：`/home/rx/GINAN/inputData/outputs/zhang_global_2019199_e5/integer_audit_direct_product_6h.json`
- 第一次 E5 TRACE 与汇总：`/home/rx/GINAN/inputData/outputs/zhang_global_2019199_e5_first/`
