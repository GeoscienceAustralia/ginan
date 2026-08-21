# E18 持久 canonical 产品 datum 三小时 shadow 结果

## 1. 试验边界

- 数据时段：2019-07-18 00:00:00–03:00:00，历元间隔 60 s；
- 最终接受测量块：181；
- 模式：`feedback=0`，所有整数候选仅用于诊断；
- 配置：`exampleConfigs/zhang_global_2019199_e18_persistent_canonical_3h.yaml`；
- 运行耗时 1:32:01，峰值内存 4,024,756 kB，无 swap，退出码 0。

## 2. 已通过的结构门禁

1. 原始因子捕获完整：181 个测量事件、360 个状态转移、51 个精确坐标变换均被接受，拒绝数为 0。
2. 51 个精确变换与旧三小时基线一致，其中 24 次树交换、27 次局部相位坐标重初始化，未出现其他未分类事件。
3. canonical 产品坐标全程只有一个 ID：
   `GPS:CANONICAL:G01->G02:G01->G03:G01->G05`。
4. 29 个历元出现 G06/G07/G08/G09/G10 等额外合法候选，但均被显式记录为 `ignored_substitutes`，没有替换固定目标。
5. L1C、L2W 的六个 relation-specific datum 均保持 V0；51 个结构事件均未造成 product datum 换版，物理 identity reset 数为 0。
6. 原始因子 replay 的先验均值相对误差为 0、先验协方差误差为 `3.98e-19`；平方根边界均值和协方差相对误差上限分别为 `5.24e-9` 和 `3.27e-6`。

## 3. 未通过的门禁

### 3.1 固定 functional 没有真正持久保留

181 个历元中，仅 152 个历元形成全部六个 L1C/L2W canonical 目标；4 个历元只形成四个目标，25 个历元只形成两个目标。典型区段为 01:00–01:08、01:13–01:20 和 02:44–02:51。

这不是 canonical ID 被替换，而是当前实现仍从当历元 `namedTargetRows` 重构 functional。当 dual-frequency 当前坐标不能直接给出固定关系时，该 relation 没有被输运到当前状态，随后 `retainOnly` 又把其历史列从 shortcut separator 中删除。因此“固定目标身份”已经实现，但“固定目标信息块跨 S-basis/暂失保持”尚未实现。

### 3.2 shortcut separator 出现确定性坐标不连续

| 窗口终点 | 目标数 | quotient rank | absolute rank | shortcut dof | shortcut 残差平方和 |
|---|---:|---:|---:|---:|---:|
| 00:29 | 6 | 4 | 2 | 174 | 73.253 |
| 00:59 | 4 | 2 | 2 | 348 | 1.11981e6 |
| 01:29 | 6 | 4 | 2 | 452 | 1.13324e6 |
| 01:59 | 6 | 4 | 2 | 632 | 6.03790e6 |
| 02:29 | 6 | 4 | 2 | 799 | 8.14845e6 |
| 02:59 | 6 | 4 | 0 | 936 | 9.14919e6 |

异常从固定 relation 首次暂失的 00:59 窗口开始，并随历史累计增长。它不能由增加历元、调整噪声或放宽整数检验修复；根因是目标列退休/重建时没有保证同一 canonical functional 的精确逆转置坐标输运和整数 offset 连续性。

### 3.3 残差独立性可建模，但尺度仍错误

retained-target `PREFIT_INNOVATION` 残差的声明自由度与样本数均为 628；原始序列 Ljung–Box `p=0.00280`，拒绝白噪声。AR(4) 后创新 Ljung–Box `p=0.99793`，剩余线性相关已被消除，自由度按 `624-4-1=619` 计算。

但是 AR(4) 创新平方和仅 88.251，创新方差为 0.14257，对 619 个相关修正自由度仍严重欠离散。这说明测量/目标协方差尺度过保守，或同一数据经过条件化后又被当作独立预测残差使用。AR 模型只能修正相关性，不能修正尺度语义。

### 3.4 整数门禁未通过

最终原始因子 quotient 只有 4 维，absolute rank 为 0。直接联合候选的 bootstrapped success rate 为 0.403111，第二/第一距离比为 1.21197，LAMBDA validation 未通过。绝对目标中最小单目标误固定概率也只有约 0.0482，远高于 `1e-3` 门限。

因此当前结果不授权固定反馈，不授权 24 小时产品，也不授权进入 20 站 PPP/PPP-AR 验证。

## 4. 下一步修复顺序

1. 在 factor-capture 层保存每个 `(signal, canonical relation, datum version)` 的 functional 行、整数 offset、物理弧版本和 absolute/quotient 元数据。
2. 每次精确状态变换 `x_new = T x_old` 后求解 `T^T l_new = l_old`；用秩揭示 QR/SVD 验证残差在 `1e-10` 内。不可表示时 fail-closed，并区分真实状态消失与纯 S-basis 交换。
3. relation 当历元不可直接重构时，不删除历史列；只停止加入新似然。仅 phase segment、physical arc version 或 product datum version 真正变化时退休旧列并建立新版本。
4. 对 offset 强制检查 `d_new-d_old` 的整数一致性；非整数平移直接拒绝，不允许继续累计。
5. 先重跑 70 分钟区段覆盖 00:55–01:20，要求六个 functional 全程保留且 shortcut 残差不跳变；通过后再重跑三小时 51 事件审计。

## 5. 可追溯输出

- 结构化结果：`Docs/zhangPppArResults/e18_persistent_canonical_3h.json`；
- 产品 SHA-256：`37bdb18e8f148077f57d8162c1f4a8994946d232fa233969c1cb684a9f0bc8ee`；
- 协方差 SHA-256：`fdb269050848122908c8cbf59071d22b7b4c177cad3d7bd58048b69316cf2c18`。
