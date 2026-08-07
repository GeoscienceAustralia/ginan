# Stage B E16 multi-epoch information-shadow results

## Decision

E16 confirms that the current blocker is wide-lane evidence stability, not the
number of network stations and not the conditional first-frequency ambiguity.
The experiment fails the gate for estimator feedback, so no six-hour
closed-loop, 24-hour product, or 20-station user validation is authorized from
this branch of the experiment.

Merely increasing the number of processed epochs is not a sufficient repair.
The current relation hypotheses change before enough reliable WL information
can accumulate.

## E16-A: scalar prior-to-posterior information increments

The float prior was captured after state transition and immediately before the
Kalman measurement update.  Only scalar physical satellite-relation marginals
were retained, avoiding another copy of the full network covariance.  At AR
time, the authoritative float posterior was used; fixed/held constraints on the
disposable integer branch did not enter the shadow statistics.

For a scalar target, each epoch contributed

\[
J_k = P_{k,+}^{-1}-P_{k,-}^{-1},\qquad
h_k = P_{k,+}^{-1}a_{k,+}-P_{k,-}^{-1}a_{k,-}.
\]

The three-hour run completed 181 epochs in 9 min 51 s.  Results were:

- 1,042 accepted scalar increments and zero rejected increments;
- 18 stage-specific relations;
- zero feedback violations;
- maximum configured window of 20 epochs;
- 155 integer-hypothesis resets and 8 gap resets;
- zero rows meeting `perr <= 1e-3`;
- minimum scalar WL error probability 0.01050;
- minimum scalar L1 error probability 0.6147 in an optimistic uncapped
  same-hypothesis reconstruction.

The scalar L1 result is not the correct layered decision statistic because it
does not condition L1 on a fixed WL.  It therefore triggered E16-B rather than
being used to reject multi-epoch estimation directly.

## E16-B: joint [WL, L1] information increments

E16-B retained the full 2-by-2 prior and posterior covariance of each physical
satellite relation and accumulated

\[
\mathbf J_k=\mathbf P_{k,+}^{-1}-\mathbf P_{k,-}^{-1},\qquad
\mathbf h_k=\mathbf P_{k,+}^{-1}\mathbf a_{k,+}
-\mathbf P_{k,-}^{-1}\mathbf a_{k,-}.
\]

WL was tested first.  L1 was then evaluated from the conditional distribution
given the selected WL integer.  The three-hour run completed 181 epochs in
9 min 38 s and produced:

| Metric | Result |
|---|---:|
| accepted joint increments | 521 |
| rejected joint increments | 0 |
| feedback violations | 0 |
| maximum configured window | 20 epochs |
| integer-hypothesis resets | 100 |
| gap resets | 4 |
| conditional-L1 reliable rows (`perr <= 1e-3`) | 242 |
| WL reliable rows (`perr <= 1e-3`) | 0 |
| joint layered reliable rows | 0 |
| minimum configured-window WL `perr` | 0.01242 |
| minimum conditional-L1 `perr` | 4.96e-62 |

The optimistic offline reconstruction removed the 20-epoch storage cap but
still reset whenever the live posterior integer pair changed or the epoch gap
exceeded 120 s.  Its longest consistent streak was 21 epochs, its best WL
`perr` was 0.01139, and it produced zero reliable layered decisions.  Thus
raising the window cap alone cannot change the decision.

The very small conditional-L1 probabilities must not be used without a reliable
WL.  Conditioning on the wrong WL branch can make L1 appear extremely precise
around the wrong integer.

## Non-intrusion and product regression

Both shadow runs set `feedback=0`.  E16-B reproduced the E15 three-hour baseline
exactly for all checked product metrics:

| Metric | E15 baseline | E16-B |
|---|---:|---:|
| product rows | 21,720 | 21,720 |
| epochs | 181 | 181 |
| datum-continuous rows | 3,456 | 3,456 |
| precision-valid rows | 1,715 | 1,715 |
| integer-valid rows | 1,604 | 1,604 |
| promotion records | 139 | 139 |
| maximum valid dual-component satellites | 10 | 10 |
| longest valid dual component | 110 min | 110 min |

Topology result counts were also identical: 88 `NO_TOPOLOGY_TARGET`, 52
`NO_NAMED_DUAL_FIX`, 8 `PROMOTED`, and 32 `WL_NOT_FIXED`.

## Acceptance result

- Prior/posterior extraction: pass; all increments were numerically valid.
- Shadow isolation: pass; zero feedback violations and exact E15 product
  regression.
- Conditional L1 information: pass after WL conditioning.
- WL reliability: fail; zero rows reached the configured integer-error gate.
- Integer-hypothesis persistence: fail; frequent resets limit a consistent
  relation to at most 21 epochs in the joint experiment.
- Authorization for feedback/longer runs/user validation: fail.

## Required next experiment

The next change should target WL observability and hypothesis stability rather
than simply extend processing duration:

1. Form a measurement-domain WL likelihood from independent epoch innovations
   (or a rigorously whitened fixed-lag batch), including its nuisance-state
   elimination and cross-epoch covariance.
2. Add a cycle-slip/arc-version-consistent relation key so evidence is retained
   across harmless S-basis exchanges but reset on a true physical arc break.
3. Require a stable WL integer candidate, ratio/success validation, innovation
   consistency, and an independent hold-out prediction before using the
   conditional L1 result.
4. Repeat a three-hour shadow run.  Only if WL and conditional L1 both satisfy
   `perr <= 1e-3` without integrity regressions should a six-hour feedback
   experiment be started.

The scalar and joint information sums remain optimistic diagnostics because
they do not reconstruct cross-epoch nuisance correlations.  They are suitable
for rejecting the current feedback proposal, not for authorizing a fix.

## Artifacts

- Configurations:
  `exampleConfigs/zhang_global_2019199_e16_information_shadow_3h.yaml` and
  `exampleConfigs/zhang_global_2019199_e16_joint_shadow_3h.yaml`
- Analyzers: `scripts/analyse_zhang_e16.py` and
  `scripts/analyse_zhang_e16_joint.py`
- Audits: `Docs/zhangPppArResults/e16_*_3h*.json`
