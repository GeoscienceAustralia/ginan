# Stage A：外部匹配 Clock/Code OSB/Phase OSB 的独立用户 PPP-AR 验证

## 1. 验证目标与边界

本阶段不使用六站网络服务端产品，也不加入区域电离层或对流层改正。目标是用成熟且相互匹配的精密钟差、code OSB 和 phase OSB，独立验证 Ginan 用户端的：

- 外部 OSB 读取、单位和符号；
- clock reference observables 与用户观测信号之间的 datum 关系；
- 接收机端模糊度 datum 消除；
- 原始频率和宽巷模糊度的小数残差；
- LAMBDA/PAR 固定链路；
- 三个真实用户站的 PPP-AR 性能。

因此，本阶段的判据不是秒级 PPP-RTK 收敛。若使用成熟外部产品仍无法形成可靠整数聚类，则问题位于用户模型或产品 datum 转换，不能归因于六站服务端。

## 2. 数据与产品

### 2.1 外部产品

使用同一 IGS Repro3 组合系列的产品：

- `IGS2R03FIN_20191980000_01D_05M_ORB.SP3`
- `IGS2R03FIN_20191990000_01D_05M_ORB.SP3`
- `IGS2R03FIN_20192000000_01D_05M_ORB.SP3`
- `IGS2R03FIN_20191990000_01D_30S_CLK.CLK`
- `IGS2R03FIN_20191990000_01D_01D_OSB.BIA`

Bias-SINEX 声明 GPS 卫星钟参考观测为 `C1W/C2W`。用户采用 GPS `C1C/L1C + C2W/L2W`，因为 MATE 的 RINEX 中不存在 `C1W/L1W`。因此 `C1W → C1C` 的 code/clock datum 转换是当前必须继续核查的关键环节。

### 2.2 用户站

选取三个完全独立运行的用户：

| 空间类别 | 用户站 |
|---|---|
| 原网络内部 | MATE |
| 原网络边缘 | DYNG |
| 原网络外部 | NICO |

这些类别仅用于比较空间位置，不表示用户运行依赖原六站服务端。三个用户滤波器均不共享服务端接收机状态、模糊度、协方差或生成树状态。

### 2.3 配置

- 主配置：`exampleConfigs/external_osb_pppar_europe_user.yaml`
- MATE：`exampleConfigs/external_osb_user_MATE.yaml`
- DYNG：`exampleConfigs/external_osb_user_DYNG.yaml`
- NICO：`exampleConfigs/external_osb_user_NICO.yaml`
- MATE 重初始化：`exampleConfigs/external_osb_user_MATE_restart.yaml`
- MATE 浮点对照：`exampleConfigs/external_osb_user_MATE_restart_float.yaml`

主配置不启用 Zhang 网络控制器、phase-clock/OSB 服务端控制器或区域大气改正。

## 3. 接收机端整数 datum 修正

### 3.1 发现的问题

原 Ginan LAMBDA 路径直接把用户非差模糊度送入整数搜索。使用外部卫星 phase OSB 后，卫星端相位 datum 已被消除，但每个接收机、星座和信号仍保留一个接收机端公共相位 datum。因而单个非差模糊度并非全部可直接固定的整数。

直接固定这些非差状态曾产生表面上约 99% 的固定率，但 MATE 03:00 重初始化试验出现明显错误固定：

| 解 | E/N/U RMS (m) | H95 (m) |
|---|---:|---:|
| 错误的非差固定解 | 0.260 / 0.109 / 0.282 | 0.573 |
| 浮点对照 | 0.024 / 0.042 / 0.130 | 0.025 |

因此，该高固定率是错误的，不能作为 PPP-AR 成功证据。

### 3.2 实现的修正

在 `src/cpp/pea/ppp_ambres.cpp` 中实现接收机端整数变换：

1. 按 `(receiver, constellation, signal)` 分组；
2. 选择组内方差最小的卫星模糊度作为 pivot，卫星编号用于确定性平局处理；
3. 构造卫星单差整数：

\[
\Delta N_{u,j}^{s,s_0}
=
N_{u,j}^{s}-N_{u,j}^{s_0};
\]

4. 对变换后的模糊度及协方差执行 LAMBDA：

\[
\hat{\boldsymbol a}_{SD}=D\hat{\boldsymbol a},\qquad
Q_{SD}=DQD^T;
\]

5. 将固定约束组合回原状态坐标：

\[
Z_{\rm original}=Z_{\rm integer}D.
\]

该逻辑由配置项 `receiver_amb_pivot: true` 启用。运行日志输出每个系统和信号的参考卫星及整数数量。

## 4. OSB 读取、单位和符号审计

### 4.1 单位

`biasSINEXread.cpp` 将 Bias-SINEX 的纳秒值转换为米：

\[
B_{\rm m}=B_{\rm ns}\frac{c}{10^9}.
\]

对 64 个实际进入 MATE 观测改正链的 OSB 项进行逐项匹配，最大绝对差为：

\[
4.57\times10^{-5}\ {\rm m},
\]

差异来自 trace 输出的小数位截断。

### 4.2 符号

观测改正使用：

\[
\Delta O=-B_{\rm BIA,m}.
\]

以 G05 为例：

| 信号 | BIA (ns) | 预期观测改正 (m) | Trace (m) |
|---|---:|---:|---:|
| C1C | -6.002 | +1.79935 | +1.7994 |
| L1C | -0.097 | +0.02908 | +0.0291 |
| C2W | -8.009 | +2.40104 | +2.4010 |
| L2W | +0.570 | -0.17088 | -0.1709 |

另进行了 OSB 符号反转对照。当前符号下 MATE 的 L1/L2/WL 单差小数残差中位数分别约为 `0.188/0.169/0.084 cycle`；反号后恶化为 `0.218/0.319/0.311 cycle`。因此简单反转 OSB 符号是错误的。

### 4.3 参考卫星单差

对 MATE 分别固定使用 G13 和 G15 作为单差参考：

| 参考星 | 可用历元 | L1 中位数 | L2 中位数 | WL 中位数 | WL \(\le0.15\) |
|---|---:|---:|---:|---:|---:|
| G13 | 532 | 0.26 | 0.25 | 0.10 | 68% |
| G15 | 670 | 0.27 | 0.26 | 0.07 | 74% |

非整数偏移在不同参考星下仍然存在，说明问题不是某一参考卫星造成的。

## 5. 三用户真实运行结果

三个用户均处理 721 个历元，PEA 退出码为 0。修正接收机端整数 datum 后：

- LD 分解错误：0；
- AR postfit 最大迭代告警：0；
- 可靠固定事件：0；
- ambiguity fix rate：0%。

最后一小时的单差模糊度小数残差和浮点定位结果如下。

| 用户 | \(|e_{L1}|\) 中位数 (cycle) | \(|e_{L2}|\) 中位数 (cycle) | \(|e_{WL}|\) 中位数 (cycle) | WL \(\le0.15\) | E/N/U RMS (m) | H95 / U95 (m) |
|---|---:|---:|---:|---:|---:|---:|
| MATE | 0.240 | 0.241 | 0.092 | 79.4% | 0.027 / 0.033 / 0.075 | 0.023 / 0.027 |
| DYNG | 0.220 | 0.233 | 0.032 | 96.5% | 0.039 / 0.009 / 0.050 | 0.081 / 0.078 |
| NICO | 0.224 | 0.225 | 0.058 | 85.7% | 0.033 / 0.029 / 0.102 | 0.110 / 0.123 |

连续 10 历元满足水平/垂向阈值的首次时间：

| 用户 | H≤0.10 m、U≤0.20 m | H≤0.05 m、U≤0.10 m |
|---|---:|---:|
| MATE | 390 s | 990 s |
| DYNG | 450 s | 1080 s |
| NICO | 1350 s | 1710 s |

宽巷具有一定整数聚类，尤其是 DYNG；但 L1、L2 原始频率单差模糊度仍稳定偏离整数约 0.22–0.24 周。当前协方差和小数残差不支持可靠 LAMBDA 固定。

## 6. 科学结论

阶段 A 的软件链路验证已经完成，但 PPP-AR 科学验收未通过。

已经排除：

- Bias-SINEX 纳秒到米的单位转换错误；
- 当前 OSB 应用符号错误；
- 单一参考卫星选择造成的偏移；
- 直接将非差模糊度作为整数造成的假高固定率；
- LAMBDA 数值崩溃或 AR postfit 迭代失败。

当前证据指向：

\[
\boxed{
\text{用户端 clock/code/phase datum 转换或非组合用户参数基准仍不闭合}
}
\]

特别是外部 CLK/BIA 以 `C1W/C2W` 为钟参考，而实际用户观测采用 `C1C/C2W`。虽然 Bias-SINEX 提供 all-frequency OSB，但仍需显式验证 `C1W → C1C` 的 code datum 转换是否与 Ginan 的 receiver clock、code-bias 和 ionosphere 参数化一致。

因此，不能把当前 0% 固定率归因于六站服务端，也不能继续用此前错误非差固定产生的约 99% 固定率。现阶段不应进入标准 OSB、STEC 格网或 SSR 播发。

## 7. 下一步

按以下顺序继续定位：

1. 显式推导并实现 `C1W/C2W` 卫星钟参考到 `C1C/C2W` 用户观测的 code-datum 变换，逐卫星验证组合改正闭合；
2. 审计非差非组合用户端的 receiver code-bias、receiver clock 与链路 ionosphere S-basis，确认两条基准码偏差的处理没有把非整数项转移到模糊度；
3. 对相同 IGS2 CLK/BIA 数据建立一个已知正确的 IGS PPP-AR 对照路径，优先采用无电离层 PPP-AR 或独立参考实现；
4. 只有当 L1/L2 单差模糊度小数残差显著聚集到整数附近并出现可靠固定后，才重新评估 ambiguity fix rate、wrong-fix rate、TTFF 和重初始化恢复时间；
5. 阶段 A 通过后，再使用相同独立用户验证六站内部固定解产品。

## 8. 复现与结果文件

- 批量运行：`scripts/run_external_osb_pppar_users.sh`
- 指标分析：`scripts/analyse_external_osb_pppar.py`
- OSB 链路审计：`scripts/audit_external_osb_chain.py`
- 数值结果：`Docs/zhangPppArResults/external_stageA3_*.json`
- OSB 审计：`Docs/zhangPppArResults/external_stageA2_osb_audit.json`

