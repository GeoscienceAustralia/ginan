# Stage B E15 candidate-pool and S-basis ablation results

## Decision

E15 does not satisfy the gate for a 24-hour network run or independent-user
PPP/PPP-AR validation.  The retained baseline for E16 is:

- all `CURRENT_RELINK` candidates pass through the existing ROUND/NIS gate;
- `max_topology_targets` caps only unknown `COMPONENT_BRIDGE` relations;
- `deterministic_relink_variance_tolerance_cycles2 = 1e-10`;
- `persistent_edge_grace_epochs = 10`;
- `prefer_persistent_core_edges = false`.

The next experiment must accumulate new epoch information, initially in shadow
mode.  It must not average successive posterior ambiguity estimates, because
those estimates share the same propagated state and are not independent
observations.

## E15-A: complete candidate-pool audit

The diagnostic-only six-hour run reproduced the E14 product metrics exactly,
so the added traces were non-intrusive.  Across 279 candidate epochs the full
pool contained 2,128 entries (mean 7.627, maximum 9).  The previous policy
selected at most three entries and omitted 140 deterministic current-relink
entries over 53 epochs.  Posterior candidate series were visibly correlated;
the aggregate lag-one correlation was about 0.548, and one G16 segment reached
about 0.959.  Increasing the epoch count therefore does not provide the same
amount of independent information as the raw epoch count suggests.

## E15-B: current-relink pool policy

Allowing every current relink into ROUND/NIS reduced omitted reliable
candidates from 140 to zero.  The product gain was small: integer-valid rows
increased from 1,546 to 1,548 and promoted relations from 2 to 4, while the
longest valid interval remained 110 minutes.  This policy is logically correct
and is retained, but it is not the dominant coverage fix.

## E15-C: deterministic numerical-rank tolerance

The six-hour explicit-tolerance run produced:

| Metric | E14 | E15 tolerance |
|---|---:|---:|
| datum-continuous rows | 3,826 | 4,176 |
| precision-valid rows | 1,693 | 1,891 |
| integer-valid rows | 1,546 | 1,705 |
| promoted dual relations | 2 | 8 |
| PPP-AR rows | 1,530 | 1,687 |
| PPP-AR epochs | 206 | 207 |
| longest valid interval | 110 min | 110 min |
| maximum published sigma | 0.485613 m | 0.485613 m |
| maximum published residual step | 0.4595 m | 0.475908 m |

The 05:09 discontinuity remained correctly blocked: the common-mode-removed
step was 42.5831 m and both PPP and PPP-AR usability were false.  After 02:50,
the stochastic candidates retained variances of roughly 0.07--0.10 cycle
squared and were not converted into deterministic relinks by the tolerance.
The tolerance therefore fixes a numerical classification problem without
weakening the stochastic integer gate.

It still does not solve continuity.  The same relations are repeatedly
quarantined and reacquired at five-epoch intervals, showing that the estimator
lacks persistent new-evidence accumulation rather than candidate availability.

## E15-D: S-basis ablations

Increasing the persistent-edge grace from 10 to 30 epochs extended the longest
contiguous PPP-AR epoch run from 111 to 130, but reduced covered epochs from 207
to 150.  Restricting the basis to persistent-core edges reduced datum mapping
changes but also reduced support, yielding 180 PPP-AR epochs and no improvement
in the 110-minute product-valid interval.  Both variants fail the requirement
to retain at least 90% of baseline usable coverage and are rejected.

## Acceptance result

- Numerical safety: pass.  Product sigma and residual-step gates remain active,
  and the injected/observed 42.58 m discontinuity remains blocked.
- Candidate-pool completeness: pass after the policy change.
- Coverage and continuity: fail.  Only one additional PPP-AR epoch is gained in
  the best six-hour run, and the longest valid product interval is unchanged.
- Promotion stability: fail.  Repeated quarantine/relink cycles remain.

Accordingly, the 24-hour and 20-user-station experiments are deferred until an
E16 shadow experiment demonstrates that prior-to-posterior information
increments are positive, accumulate consistently on a fixed physical satellite
relation, and predict reliable integer decisions without feeding those
decisions back into the filter.

## Reproducible artifacts

- Configurations: `exampleConfigs/zhang_global_2019199_e15_*.yaml`
- Analyzer: `scripts/analyse_zhang_e15.py`
- Audits: `Docs/zhangPppArResults/e15_*_{candidate_pool,persistent_datum,float_numeric,fixed_numeric}.json`
