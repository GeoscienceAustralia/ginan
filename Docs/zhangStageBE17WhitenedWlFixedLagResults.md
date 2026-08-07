# Stage B E17 whitened WL fixed-lag results

## Decision

E17 does not pass the gate for estimator feedback.  It verifies that the
physical-arc version layer can detect unsafe coordinate changes, but it also
shows that a scalar prior/posterior likelihood ratio is not yet a calibrated,
independent WL observation and that `G*k` alone cannot be transported across a
dynamic S-basis exchange.

No six-hour feedback run, 24-hour product run, or 20-station user PPP/PPP-AR
validation is authorised from E17.

## Implemented estimator boundary

For each current-relink WL target, E17 forms the scalar innovation-equivalent
likelihood from the float prior and posterior marginals,

\[
J_k=P_{k,+}^{-1}-P_{k,-}^{-1},\qquad
h_k=P_{k,+}^{-1}a_{k,+}-P_{k,-}^{-1}a_{k,-},
\]

and records the effective observation `h_k/J_k` with variance `1/J_k`.
Observations are retained for at most 1,800 s and 60 samples.  A leave-one-out
prediction residual is evaluated before inserting each sample.  All E17 paths
remain shadow-only (`feedback=0`).

The persistent identity has two layers:

1. L1/L2 satellite phase segments identify explicit satellite-product phase
   discontinuities.
2. Exact physical receiver-satellite ambiguity arcs include the observable,
   edge, and arc version.  A shared physical edge with a changed version is an
   immediate hard reset.

## E17-A: attempted S-basis transport

The first run retained a window when the phase segments were unchanged and no
shared physical arc version conflicted.  This was insufficient.  A named
satellite product is `z_T + G*k`; `G*k` alone changes under a tree exchange.
The missing `z_T` term caused integer-coordinate jumps as large as hundreds of
cycles even though all checked physical arc versions remained continuous.

The three-hour run completed 181 epochs in 10 min 20 s:

| Metric | E17-A |
|---|---:|
| accepted observations | 378 |
| prediction-gate rejections | 143 |
| prediction-gate fraction | 27.45% |
| apparent basis transports | 10 |
| physical arc version conflicts | 0 |
| maximum window observations | 30 |
| minimum WL `perr` | 0.01142 |
| reliable WL rows (`perr <= 1e-3`) | 0 |
| whitened residual lag-1 correlation | 0.1810 |
| whitened residual RMS | 5.059 |
| maximum reduced chi-square | 4,469.88 |
| feedback violations | 0 |

The extreme prediction residuals and chi-square values invalidate the attempted
transport.  The low lag-1 correlation alone is not a pass because the residual
scale is grossly inconsistent.

## E17-B: fail-closed physical-coordinate windows

E17-B does not claim an unavailable exact transport.  It retains observations
only while the product datum version and the complete versioned physical row
remain unchanged.  A product datum change or an S-basis physical-coordinate
change resets the window before the new observation is inserted.

The three-hour run completed 181 epochs in 10 min 02 s:

| Metric | E17-B |
|---|---:|
| accepted observations | 521 |
| rejected observations | 0 |
| product-datum resets | 212 |
| S-basis physical-coordinate resets | 4 |
| physical arc version conflicts | 0 |
| satellite phase-segment resets | 0 |
| maximum window observations | 7 |
| minimum WL `perr` | 0.17094 |
| reliable WL rows (`perr <= 1e-3`) | 0 |
| whitened residual lag-1 correlation | 0.1393 |
| whitened residual RMS | 0.2355 |
| median reduced chi-square | 0.0281 |
| feedback violations | 0 |

The fail-closed policy removes the false cross-coordinate outliers, but the
window never exceeds seven observations because the product target changes too
frequently.  The residual lag-1 statistic meets the provisional absolute 0.2
limit, while the RMS and reduced chi-square are far below their calibrated
values near one.  Therefore these likelihood ratios cannot be treated as
independent unit-information observations.

## Non-intrusion and product regression

E17-B exactly reproduces the E15/E16 three-hour product baseline:

| Metric | Baseline | E17-B |
|---|---:|---:|
| product rows | 21,720 | 21,720 |
| epochs | 181 | 181 |
| structure-valid rows | 21,720 | 21,720 |
| datum-continuous rows | 3,456 | 3,456 |
| precision-valid rows | 1,715 | 1,715 |
| integer-valid rows | 1,604 | 1,604 |
| promotion records | 139 | 139 |
| maximum valid dual-component satellites | 10 | 10 |
| longest valid dual component | 110 min | 110 min |

## Scientific interpretation

E17 separates two failure mechanisms that E16 could not distinguish:

1. Physical identity is not the same as stochastic-coordinate transport.
   Arc versions and phase segments can prove that a physical arc did not slip,
   but `G*k` still changes coordinates when the S-basis changes.  Exact
   transport requires the stochastic `z_T + G*k` target, not an integer shift
   inferred from the nearest previous hypothesis.
2. The scalar marginal likelihood ratios are not calibrated independent epoch
   observations.  Their very small residual RMS and reduced chi-square show
   that nuisance-state reuse and cross-epoch covariance remain present even
   after fail-closed coordinate segmentation.

Increasing the number of processed epochs is not the next repair.  With 212
datum resets in only three hours, a longer run primarily creates more short
segments; it does not lengthen the longest valid fixed-lag window or correct
the covariance model.

## Required E18 experiment

The next estimator must be built on the invariant stochastic target and the
actual fixed-lag measurement covariance:

1. Define each dual-frequency target as the full-state `z_T + G*k` linear form,
   including satellite phase states and cycle ambiguities with exact units and
   signs.
2. Retain the accepted raw measurement innovations, their design rows, and the
   complete within-lag covariance.  Eliminate nuisance states with a Schur
   complement and whiten the resulting WL block with Cholesky/LDLT.
3. Key the batch by L1/L2 satellite phase segments and versioned physical arcs.
   Exact S-basis transforms must transform the full target and covariance;
   physical arc-version changes remain hard resets.
4. Require simultaneously: WL `perr <= 1e-3`, calibrated normalized residual
   scale, acceptable lag autocorrelation, independent hold-out prediction,
   zero feedback leakage, and exact product regression.

Only after those gates pass in a three-hour shadow should controlled feedback
or longer processing be attempted.

## Artifacts

- Configurations:
  `exampleConfigs/zhang_global_2019199_e17_whitened_wl_3h.yaml` and
  `exampleConfigs/zhang_global_2019199_e17_physical_window_3h.yaml`
- Analyzer: `scripts/analyse_zhang_e17.py`
- Audits: `Docs/zhangPppArResults/e17_*_3h*.json`
