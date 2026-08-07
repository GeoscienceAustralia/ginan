# Stage B E19: LAMBDA reliability and information-loss diagnostics

## Scope and safety boundary

E19 replaces marginal-coordinate ambiguity diagnostics with the quantities
used by the operational LAMBDA reduction.  It also compares the raw-factor
square-root marginal against the epoch-local target separator on exactly
matching physical coordinates.  Both paths remain shadow diagnostics:
`feedback=0`, and the target-increment separator is explicitly labelled
`DIAGNOSTIC_ONLY`.

This stage does not rescale the filter covariance and does not lower the
`0.999` joint reliability threshold.

## Reliability implementation

For each integer target block the trace now records:

- the operational integer transform `lambda_Z`, with `z = Z^T a`;
- `reduced_covariance = Z^T Q Z`;
- the post-reduction conditional variances `D` and their one-dimensional
  conditional success rates;
- joint bootstrapped success, ADOP and fixed-failure-rate validation;
- the best and second candidates in original and reduced coordinates;
- exact integer/unimodular and candidate back-transform audits;
- covariance-transform, determinant and bootstrap-implementation consistency
  errors.

The old `passed_target_count` field and its marginal-variance calculation have
been removed from the source and result parser.  The shadow joint gate is now

```
joint_reliability_pass =
    lambda_validation_pass && joint_bootstrapped_success_rate >= 0.999
```

PAR subset selection also calls the operational LAMBDA reduction for every
candidate subset; it no longer ranks subsets with an unreduced LDLT of the
original coordinate order.

The RTKLIB normal-CDF approximation and the independent `std::erf` audit differ
at the order of `1e-7`.  A strict `1e-7` comparison rejected one otherwise
finite four-dimensional result at `1.00333e-7`; this is numerical agreement
between two probability implementations, not ambiguity reliability.  The
implementation cross-check therefore uses `5e-7` and reports a dedicated
`OPERATIONAL_LAMBDA_BOOTSTRAP_AUDIT_MISMATCH` failure.  This does not relax the
scientific `0.999` joint reliability threshold or the FFRT gate.

## Tests

`zhang_full_rank_tests` includes a correlated two-coordinate reduction test.
It accepts the true conditional `D`, verifies ADOP, the unimodular transform
and candidate round trip, and rejects use of the reduced covariance diagonal
as a substitute for conditional variances.

Build and unit-test evidence:

```
cmake --build . --target zhang_full_rank_tests pea -j6
ctest -R zhang_full_rank_tests --output-on-failure
```

Result: `1/1` test passed; `pea` built successfully.

## 35-minute real-data result

The first complete 30-measurement block at 2019-07-18 00:29:00 produced:

| path | dimensions | conditional success rates | joint bootstrap | FFRT | joint gate | ADOP |
|---|---:|---|---:|---:|---:|---:|
| raw square-root WL/L1 | 2 | `1;1` | `1.000000` | pass | pass | `0.0605841` |
| incremental target WL/L1 | 4 | `0.999667;0.999369;0.999616;0.999604` | `0.998258` | pass | reject | `0.141864` |

Thus all four incremental conditional directions individually exceed 0.999,
while their product does not.  A per-coordinate count would give the wrong
joint decision.  The raw candidate round trip and covariance transform close
at approximately `1e-14` and `1e-16`, respectively.

## Raw-factor versus target-separator information

The short final-binary replay compares the common G01-G03 L1C/L2W physical
coordinates at 00:09:00:

| metric | result |
|---|---:|
| common targets | 2 |
| raw information rank | 2 |
| incremental information rank | 2 |
| incremental/raw covariance trace | `1.70774` |
| incremental/raw information trace | `0.226161` |
| covariance relative difference | `0.707307` |
| information relative difference | `0.773899` |

The two paths are therefore not related by a harmless coordinate relabelling.
On the same physical coordinates, the target-only separator retains only about
23% of the raw square-root information trace.  This is direct evidence of
information loss in the epoch-local target shortcut, separate from the global
stochastic scale problem.

## Persistent raw-target result

The explicit persistent target model has now been inserted in the raw
multi-epoch graph.  A target is added as an exact affine functional of the
current full state, propagated with zero process noise, and constrained to the
new state representation after each accepted epoch.  A pure S-basis change is
applied as an exact coordinate transform; a changed physical arc version is
rejected and requires a window reset.  No small pseudo-variance is used.

The 12-minute final-binary replay completed 13 measurement epochs and three
local exact coordinate reinitialisations with no capture rejection.  At
00:09:00 it retained all six paired L1C/L2W targets:

| metric | result |
|---|---:|
| persistent targets | 6 |
| exact target constraints applied | 38 |
| unresolved frequency datum rank | 2 |
| integer quotient rank | 4 |
| absolute datum rank | 0 |
| raw orthogonal residual dof | 24,304 |
| raw orthogonal squared norm | 2,697.5 |
| squared norm / dof | 0.1110 |
| runtime | 7 min 34 s |

The corresponding four-dimensional quotient covariance has eigenvalues
approximately

```
9.41e-5, 2.06e-2, 1.462, 21.423 cycles^2
```

and condition number `2.28e5`.  This is a directional observability problem,
not one scalar variance error.  The G02-G03 and G02-G05 directions are almost
collinear within each frequency (correlations about `0.998` and `0.9999` after
quotient construction).  Joint fixing of all four directions has bootstrap
success `0.219959` and ratio `1.0401`, so it must be rejected.  Operational PAR
selects the two G02-G05 L1C/L2W directions, for which bootstrap success is
`0.999995` and the candidate-distance ratio is `20.60`.

The result proves three separate points:

1. the prior missing-rank failure was caused by reconstructing only the latest
   target pair, not by a fundamental absence of a four-dimensional quotient;
2. the epoch-local separator loses information, because its covariance and
   information still disagree with the raw model on identical coordinates;
3. a global covariance multiplier is unsafe, because weak and strong quotient
   eigen-directions differ by more than five orders of magnitude.

## Covariance-scale diagnostics and remaining boundary

The raw orthogonal statistic (`2697.5/24304 = 0.1110`) and the earlier retained
target statistic (`6.2248/24 = 0.2594`) are both much smaller than one.  They do
not, however, justify multiplying every target covariance by either number:
the two residual constructions have different nuisance projections, and the
target covariance is strongly anisotropic.

The capture path now accumulates the Kalman quality-control prefit ratios by
measurement type, constellation and observable.  These ratios are already
normalised by the marginal diagonal of `H P H' + R`.  They are explicitly
labelled `MARGINAL_PREFIT_RATIO_NOT_JOINT_CHI_SQUARE`; correlated rows are not
counted as independent chi-square degrees of freedom.  Cumulative snapshots
allow the analysis script to use an earlier segment for scale training and a
later increment for holdout checking.  This remains `feedback=0`.

Before any stochastic rescaling or integer feedback, a longer replay must:

- estimate separate code/phase and signal-family scales on the training part;
- verify those scales on later epochs and held-out receivers;
- retain the full quotient covariance, including cross-frequency and
  shared-reference correlations;
- require operational PAR bootstrap success and FFRT, not marginal variances;
- replace the current dense duplicated raw boundary with sparse QR/Schur
  compression before the three-hour four-tree replay.

## Remaining boundary

The persistent functional is now present and valid in a short real-data
window.  The remaining blockers are grouped stochastic calibration with a
genuine train/holdout split, sparse resource scaling, four-tree same-observation
invariance, and independent prediction.  Integer feedback remains prohibited.
