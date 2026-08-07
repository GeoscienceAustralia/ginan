# 阶段 B E14：确定性具名目标重连实验结果

日期：2026-08-04
分支：`codex/zhang-full-rank-ppp-rtk`

## 1. 实验问题

E13 六小时试验中，PPP-AR 产品在 04:25 后基本消失。E14 首先对以下事件逐历元诊断：

1. 具名 WL 目标数量、随机秩和固定结果；
2. 拓扑重连候选的 WL/L1 浮点值、周内小数和方差；
3. 动态 product datum 版本切换；
4. 持久关系、当前对齐和产品精度状态；
5. 实际发布产品的精度与连续性门控。

## 2. E14-A 诊断结论

### 2.1 不是目标空间消失

- 六小时内主具名 WL 固定成功 80 个历元；
- 最后一次主具名 WL 固定发生在 02:26；
- 02:27–06:00 连续 214 个历元主具名 WL 固定数为 0；
- 失锁后每历元仍有约 25–27 个可映射具名目标；
- 六小时 product datum 内部版本切换 146 次。

因此，后半段失败不是没有卫星目标，也不是满秩映射退化为零秩，而是候选不能通过当前固定/重连路径。

### 2.2 发现确定性目标被错误丢弃

02:27–02:49 的 G07、G16 等 `CURRENT_RELINK` 候选具有：

- WL 方差约为 0–10⁻¹² cycle²；
- 距最近整数约 10⁻¹² cycle；
- 相同关系已存在于持久卫星整数森林。

这些目标不是待搜索的新随机整数，而是 held 整数格已经精确确定的具名关系。然而 `rankAwareGnssAr()` 只把正方差主子集送入 ROUND，将零方差行作为 `SKIPPED_DETERMINISTIC` 丢弃。剩余高方差候选又不能满足 ROUND 的整数错误概率门限，导致重连返回 0。

02:50 以后情况不同：最佳重连候选的典型 WL 方差约为 0.07–0.10 cycle²，周内小数约为 0.26–0.32 cycle，单历元整数错误概率约为 0.18–0.24。它们不能通过原 0.999 成功率门限是正确行为，不能靠降低阈值修复。

## 3. E14-B 实现

新增确定性重连恢复逻辑：

1. 仅对 `CURRENT_RELINK` 生效；
2. 只接纳边际方差不大于数值秩容差的目标；
3. 目标浮点值与最近整数的闭合误差必须不大于 10⁻⁸ cycle；
4. 目标必须对应已经存在的持久卫星关系；
5. 确定性目标只作为当前产品坐标重新捕获证据，不重复施加伪观测；
6. 原有连续 5 历元确认、NIS、冲突隔离和稳定期保持不变；
7. 未知的 `COMPONENT_BRIDGE` 仍必须经过原 ROUND/NIS，不允许使用确定性重连捷径。

同时增加拓扑候选池、浮点值、周内小数、方差和确定性恢复数量的 TRACE 诊断。

## 4. 三小时定向对照

| 指标 | E14-A 原逻辑 | E14-B 确定性重连 | 变化 |
|---|---:|---:|---:|
| PPP-AR 可用行 | 1222 | 1436 | +214（+17.5%） |
| PPP-AR 覆盖历元 | 145 | 145 | 0 |
| 最大有效双频卫星数 | 8 | 9 | +1 |
| 拓扑 `PROMOTED` 事件 | 0 | 2 | +2 |
| fixed 事务中止 | 0 | 0 | 0 |

最终源码在将确定性证据与随机伪观测施加完全分离后重新运行，结果仍为 1436 条 PPP-AR、145 个覆盖历元、2 次拓扑提升和 0 次中止。

## 5. 六小时结果

| 指标 | E13 | E14 | 变化 |
|---|---:|---:|---:|
| PPP-AR 可用行 | 1316 | 1530 | +214（+16.3%） |
| integer valid 行 | 1326 | 1546 | +220 |
| datum continuous 行 | 3650 | 3826 | +176 |
| precision valid 行 | 1326 | 1693 | +367 |
| PPP-AR 覆盖历元 | 206/361 | 206/361 | 0 |
| 最大有效双频卫星数 | 8 | 9 | +1 |
| 最长双频组件 | 126 min | 152 min | +26 min |
| 最长有效双频组件 | 107 min | 110 min | +3 min |
| fixed 事务中止 | 0 | 0 | 0 |

安全指标没有退化：

- 实际发布 PPP-AR 产品最大改正数标准差：0.4856 m；
- 实际发布 PPP-AR 产品最大去公共模跳变：0.4595 m；
- 05:09 的 42.58 m 异常仍被拒发；
- 两个单元测试套件均重新编译并通过。

FLOAT 的 `ppp_usable` 相比 E13 少 7 行。产品历史键已经包含 FLOAT/FIXED 解类型，并非两分支历史串扰；差异来自 E14 重连后两类输出共同采用的持久产品 datum 整数重表达，权威 FLOAT 滤波状态没有接受 fixed 反馈。

## 6. 门禁结论

E14 修复有效，但只解决了“已经被整数格精确确定的关系仍无法重连”这一类漏用。它增加了同一可用历元内的卫星数和产品行数，却没有增加 PPP-AR 覆盖历元，也没有跨过 04:25 后的长期失锁。

因此：

1. E14 的确定性重连实现应保留；
2. 六小时安全性继续通过；
3. 六小时连续可用性仍未通过；
4. 20 个独立验证站继续受门禁阻塞。

下一阻塞已经收敛到 02:50 后的非确定性 `CURRENT_RELINK`：其正式方差和周内小数都不足以支持单历元整数判定。下一阶段不能假设连续历元独立后简单平均，也不能降低 0.999 成功率门限；应从多接收机物理支撑中构造具有正确协方差的批量/递归重连观测，或减少导致非整数当前坐标丢失的动态 S-basis 事件。

## 7. 可复核产物

- E14-A 配置：`exampleConfigs/zhang_global_2019199_e14_diagnostic_3h.yaml`
- E14-B 3 h 配置：`exampleConfigs/zhang_global_2019199_e14_deterministic_relink_3h.yaml`
- E14-C 6 h 配置：`exampleConfigs/zhang_global_2019199_e14_deterministic_relink_6h.yaml`
- 3 h FLOAT/FIXED/datum 审计：`Docs/zhangPppArResults/e14_deterministic_relink_3h_*.json`
- 6 h FLOAT/FIXED/datum 审计：`Docs/zhangPppArResults/e14_deterministic_relink_6h_*.json`
- WSL 6 h 产品：`/home/rx/GINAN/inputData/outputs/zhang_global_2019199_e14_deterministic_relink_6h/zhang_internal_products.csv`
- WSL 6 h TRACE：`/home/rx/GINAN/inputData/outputs/zhang_global_2019199_e14_deterministic_relink_6h/Network-zhang_global_2019199_e14_deterministic_relink_6h-201919900.TRACE`
- WSL 6 h 日志：`/home/rx/GINAN/inputData/run_logs/zhang_global_2019199_full_e14_deterministic_relink_6h.log`
