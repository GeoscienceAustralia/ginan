# 阶段 B E11：分层具名产品整数固定实验结果

日期：2026-08-03
分支：`codex/zhang-full-rank-ppp-rtk`

## 1. 实验目的与控制变量

E11 使用 74 个估计站、2019-07-18 00:00–02:10、60 s 间隔，共 131 个历元。实验保持 E10 的纯 FLOAT 权威滤波器、一次性 FIXED 副分支、NIS 显著性水平 `1e-6`、产品关系连续 5 历元确认和最大确认间隔 120 s 不变，只依次改变以下环节：

1. 将 held、WL 和第一信号固定拆成嵌套子事务；
2. 以具名卫星 WL/L1 目标进行 PAR，而不是直接把不透明 LAMBDA 组合写入产品台账；
3. 逐卫星、逐信号判断产品整数有效性；
4. 同历元 fixed 行先作为临时关系使用，只有相同物理弧版本和整数值连续出现 5 个历元后，才允许进入持久 held 格。

## 2. 三轮因果对照

| 版本 | 关键差异 | FIXED 可用行 | PPP-AR 可用行 | fixed 中止 | 结论 |
|---|---|---:|---:|---:|---|
| E11-v1 | 分层子事务 + 具名 ROUND；新固定整集联合 NIS | 7860/7860 | 0 | 0 | 事务稳定，但多数具名候选因整集 NIS 被拒，无法形成产品图 |
| E11-v2 | 对具名目标执行联合 NIS 兼容 PAR；新 fixed 行立即写入 held | 5880/7860 | 0 | 33 历元 | 同历元候选增加，但未经时间确认就持久化，污染下一历元 held 基线 |
| E11-v3 | 新 fixed 行连续 5 历元确认后才进入 held | 6600/7860 | 1128/7860 | 21 历元 | 首次形成可用产品整数图；剩余问题收敛到持久 held 块的局部冲突隔离 |

v3 相对 v2 增加 720 条可用 FIXED 产品行，并第一次得到 1128 条 `pppar_usable=1` 产品行。这个变化没有通过放宽 NIS 或减少确认历元获得，因此可以归因于“临时固定与持久格准入分离”。

## 3. E11-v3 完整统计

### 3.1 数值与分支完整性

| 分支 | 总行数 | numeric valid | continuity valid | branch/PPP usable | PPP-AR usable |
|---|---:|---:|---:|---:|---:|
| FLOAT | 7860 | 7860 | 7860 | 7860 | 0 |
| FIXED | 7860 | 7860 | 7860 | 6600 | 1128 |

- FLOAT 最大相邻历元变化：26.2878 m；
- FIXED 最大相邻历元变化：26.2930 m；
- 两个分支均无大于 1000 m 的跳变；
- 131 个历元全部完成，运行时长 7 min 20.40 s；
- 21 个 fixed 中止历元全部由持久 held 基线联合 NIS 拒绝触发，FLOAT 不受影响。

### 3.2 具名卫星产品图

- 产品关系提升事件 118 次：82 次等待确认、26 次接受、10 次当前对齐隔离；
- 形成 19 条唯一拓扑关系；
- 最大双频有效卫星分量为 10 颗；
- 最长双频产品分量持续 105 个历元（104 min）；
- 最长连续整数有效双频分量持续 61 个历元（60 min）；
- G01/G18 的 L1C、L2W 均连续整数有效 61 个历元；
- G23 出现 10 次冲突隔离，是本窗口内唯一被隔离的卫星。

全部 15720 条 FLOAT/FIXED 产品记录均满足结构有效性。1128 条 FIXED 产品同时满足持久关系、当前对齐、整数精度、连续性和 fixed 分支有效性，因此被标记为 PPP-AR 可用。

### 3.3 持久格行为

- held 准入等待确认 114 行次；
- held 准入接受 25 行次；
- 持久 held 条件化成功 83 个历元；
- 持久 held 联合 NIS 拒绝 21 个历元；
- WL 子事务提交 51 次；
- 第一信号子事务提交 36 次。

01:28 的典型拒绝中，持久格有 20 行，联合 NIS 为 78.9547，高于自由度对应阈值 65.4207。失败后权威 FLOAT 仍正常，后续图事件消除失效弧后，held 秩在 01:52–01:53 恢复到 18–19，整数有效产品在 02:08 重新出现。

## 4. 当前结论

E11 证明原先的失败不是 74 个站不足、全球站一起组网或有效历元过少造成的。相反，74 站和 60 s 间隔已经能够在 26 min 后建立首批确认的具名双频卫星关系。此前不能形成产品的直接原因是：

1. 整数固定事务边界过粗；
2. 固定的整数对象与产品台账对象不一致；
3. 临时同历元 fixed 行未经时间确认就进入持久格；
4. 持久格发生局部冲突时仍以整块联合事务处理。

前三项已经得到实现和实验验证。第四项现在是扩大实验前的主要阻塞。

## 5. 下一步门禁

暂不运行 6 小时网络和 20 个用户站 PPP/PPP-AR。先实现 P4：

1. 对 rejected held 块计算秩揭示白化残差；
2. 以物理弧关系/卫星连通块定位冲突子块；
3. 只将冲突子块置为 `REACQUISITION_PENDING`；
4. 未受影响子块继续联合条件化；
5. 被隔离子块仍需连续 5 历元重新确认才能恢复；
6. 产品有效性必须与实际成功施加的持久子块对应，不能因全局产品图已有关系而误标为可用。

P4 不能简单实现成“逐行独立通过就全部施加”。必须保留子块内部联合 NIS 和全族误警控制，否则会以提高可用率为代价引入隐蔽错误固定。

## 6. 可复核产物

- 配置：`exampleConfigs/zhang_global_2019199_e11_layered_named.yaml`
- FLOAT 数值审计：`Docs/zhangPppArResults/e11_layered_named_float_numeric.json`
- FIXED 数值审计：`Docs/zhangPppArResults/e11_layered_named_fixed_numeric.json`
- 持久 datum 审计：`Docs/zhangPppArResults/e11_layered_named_persistent_datum.json`
- WSL 产品：`/home/rx/GINAN/inputData/outputs/zhang_global_2019199_e11_layered_named/zhang_internal_products.csv`
- WSL TRACE：`/home/rx/GINAN/inputData/outputs/zhang_global_2019199_e11_layered_named/Network-zhang_global_2019199_e11_layered_named-201919900.TRACE`
- WSL 日志：`/home/rx/GINAN/inputData/run_logs/zhang_global_2019199_full_e11_layered_named.log`
