# Stage B E18 integer-gauge quotient review and execution plan

Date: 2026-08-06

## 1. Correction to the previous diagnosis

The nine-epoch smoke run established only that no persistent dual-frequency
satellite relation had yet been promoted.  It did not establish that the
three-hour model has zero integer rank.  Historical E15 results show the first
dual-frequency persistent component at about 00:26 and the first precision-
valid component at about 00:28.  Therefore the short smoke is too short to
decide whether later epochs produce a usable persistent product datum.

The previous label `UNCONSTRAINED_INTEGER_DATUM_GAUGE` also combined two
different questions:

1. whether a linear function of the physical ambiguities is integer estimable;
2. whether the arbitrary integer origin of the delivered satellite product has
   already been transported into a persistent product convention.

For the rank-defect mixed-integer model

\[
E(y)=Az+Bb,
\]

the first question is decided by the nuisance-free row space
`range(A^T B_perp)` and by admissibility of the integer transformation.  It is
not decided by assigning finite Gaussian priors to every nuisance parameter.
The second question is a datum convention.  If

\[
a=z_T+G_T(k_1-k_2),\qquad z_T\in\mathbb Z,
\]

then the absolute candidate depends on `z_T`, but the fractional residual,
variance and nearest-integer error probability do not:

\[
\operatorname{frac}(a)=\operatorname{frac}(G_T(k_1-k_2)).
\]

An unresolved `z_T` must therefore block continuous absolute OSB publication,
but it must not discard the modulo-integer likelihood used to test whether the
underlying integer-estimable function is precise enough.

## 2. Literature review

Khodabandeh and Teunissen's integer-estimability theorem requires an estimable
integer function to lie in the nuisance-eliminated ambiguity row space and to
be part of an admissible integer transformation.  An integer-valued row alone
is insufficient.  Their GNSS-network analysis also shows that the estimable
ambiguities are network-cycle functions and that PPP-RTK user fixing is a
special case of the combined network-plus-user model.

Odijk et al. use S-system theory to separate estimable parameters from the
chosen S-basis in undifferenced, uncombined network and user models.  This
supports treating a product datum as an explicit convention rather than as an
extra stochastic observation.

The 2026 recursive-batch PPP-RTK study supports accumulation across epochs
when the ambiguity function is already well-defined and constant.  It does
not provide a mechanism by which more epochs remove a structural rank defect.
Thus more data may reduce the variance of `Gk` and permit a reliable integer
decision, but cannot identify an arbitrary integer gauge.

Sparse square-root information elimination remains appropriate for the
multi-epoch factor window.  QR-based methods are also required when process
noise or exact constraints are singular; a dense ordinary Cholesky inverse is
not an acceptable substitute.

Primary references:

- A. Khodabandeh and P. J. G. Teunissen, *Integer estimability in GNSS
  networks*, Journal of Geodesy, 2019,
  https://doi.org/10.1007/s00190-019-01282-6.
- D. Odijk et al., *On the estimability of parameters in undifferenced,
  uncombined GNSS network and PPP-RTK user models by means of S-system
  theory*, Journal of Geodesy, 2016,
  https://doi.org/10.1007/s00190-015-0854-9.
- P. J. G. Teunissen et al., *PPP-RTK theory for varying transmitter
  frequencies with satellite and terrestrial positioning applications*,
  Journal of Geodesy, 2022,
  https://doi.org/10.1007/s00190-022-01665-2.
- J. Jeong et al., *Recursive Batch Positioning Algorithm for Efficient
  Integer Ambiguity Resolution in PPP-RTK*, JPNT, 2026,
  https://doi.org/10.11003/JPNT.2026.15.2.145.
- M. L. Psiaki, *Square-root information filtering and fixed-interval
  smoothing with singularities*, Automatica, 1999,
  https://doi.org/10.1016/S0005-1098(99)00027-8.

## 3. Defects in the current deterministic evidence

The test named `raw_multi_epoch_window_eliminates_complete_phase_nuisance`
uses a proper finite prior for satellite clock, receiver clock, receiver phase
biases, ionosphere and both ambiguities.  Agreement with a covariance-form
Kalman update proves numerical equivalence only.  It does not prove classical
estimability from the observations.  In its single-receiver four-observation
design, `N1-N2` is not in the row space after the two independent receiver
phase-bias columns are included.

The existing four-tree test keeps all physical arc ambiguities as independent
Gaussian states and applies invertible coordinate changes.  The operational
Zhang model instead absorbs tree-edge ambiguities into nuisance states.  The
test proves invariance of a full physical Gaussian, but not invariance of the
rank-reduced S-basis model.  It must be supplemented with a reduced-coordinate
and integer-gauge test.

## 4. State classification

Every requested target shall report these logically separate flags:

1. `integer_estimable`: the exact current-chord row is integral, primitive and
   belongs to the retained integer coordinate lattice;
2. `quotient_valid`: the raw-factor window has a finite marginal for the target
   modulo an integer translation;
3. `absolute_datum_valid`: the persistent product manager supplies an exact,
   causal integer offset for the current physical arcs and phase segments.

The allowed states are:

| State | Integer reliability shadow | Absolute OSB output |
|---|---:|---:|
| not integer estimable | no | no |
| integer estimable modulo Z | yes | no |
| datum transport pending | yes | no |
| persistent datum aligned | yes | yes, subject to precision/integrity gates |

## 5. Implementation order

### A. Deterministic gates

1. Add a row-space estimability test showing that the isolated single-receiver
   `N1-N2` row is rejected without a receiver phase-bias datum.
2. Add a connected bipartite-graph test showing that a fundamental-cycle row
   is nuisance-orthogonal, primitive and unchanged modulo integer gauge under
   four legal S-bases.
3. Preserve the existing covariance-equivalence test but rename its claim so
   it cannot be read as an estimability proof.

### B. Quotient target capture

1. Retain `G_T(k1-k2)` when the exact persistent offset is not yet available.
2. Report its canonical fractional mean, covariance and unresolved gauge rank.
3. When a causal persistent relation becomes available, add the exact integer
   offset and set `absolute_datum_valid=true` without changing the fractional
   likelihood.
4. Never feed a rounded candidate or held integer constraint into the float
   factor window.

### C. Experiments

1. Build and run all Zhang unit tests.
2. Run a 35-minute, 60-second shadow with `wait_next_epoch=3600`, `feedback=0`
   and the same 74-station input.  Acceptance requires all 35 epochs, no capture
   rejection, nonzero quotient target rank, and either a causal transition to
   persistent alignment or an explicit finding that no relation was fixed.
3. Compare the time of first aligned target with the E15 expectation around
   00:26--00:28.  A materially different result must be explained from trace
   events before extending the run.
4. Only after the 35-minute gate, run the three-hour single-policy shadow.
5. Only after the three-hour numerical/whitening gate, replay the identical
   accepted factors under four legal tree policies and then perform held-out
   epoch and receiver prediction.

## 6. Stop rules

- More epochs are not used to repair a failed exact-rank or primitive-lattice
  test.
- A modulo-integer result is never labelled as a continuous absolute product.
- A persistent relation produced from the current epoch is not reused as an
  independent validation factor for that same epoch.
- Feedback, 24-hour products and 20-station PPP/PPP-AR remain disabled until
  quotient reliability, absolute datum transport, whitening and independent
  prediction pass together.

## 7. Execution record and decision (2026-08-06)

### 7.1 Deterministic and short-window results

- All 60 Zhang unit tests pass.  The suite now includes the negative
  single-receiver estimability case, the primitive nuisance-orthogonal network
  cycle case, Gaussian-posterior covariance equivalence and four legal-tree
  deterministic replay.
- The five-epoch raw-window diagnostic produced one quotient-valid block, but
  its best integer error probability was `0.13386395571435927`; five minutes is
  therefore insufficient for the requested `perr <= 1e-3` gate.
- In the 35-minute run, all 35 final measurement blocks were captured without
  rejection: 86,172 rows, 465,164 design nonzeros, 115,102 covariance nonzeros,
  68 transitions and six exact S transforms.  Replay prior mean and covariance
  relative errors were both zero.
- The persistent absolute target first became available at 00:28.  Of 102
  complete target records, 96 were quotient-only and six were absolute-datum
  aligned.  The minimum single-target absolute integer error probability was
  `2.943036176490388e-4`.  This corrects the earlier nine-minute inference:
  additional epochs do matter for variance reduction and causal datum
  promotion in this data set.

### 7.2 Statistical gate

The retained one-epoch target blocks have rank 102 and squared whitened norm
13.0324.  Under a correctly scaled 102-degree-of-freedom chi-square model the
lower-tail probability is `3.56e-28`, so the covariance is severely
over-dispersed relative to the residuals.  The Ljung--Box CDF at ten lags is
0.318 and does not by itself reject whiteness, but absence of detectable ACF
does not repair the covariance-scale failure.  This block is also not the
requested raw multi-epoch marginal and cannot be used as a substitute for its
gate.

### 7.3 Numerical implementation gate

The first 35-minute full-window attempt formed normal equations after sparse
QR and was stopped near 6.8 GB RSS.  The covariance propagation was then
changed to use the QR square-root information factor directly:

`A P = Q R`, `R^T Y = (L P)^T`, `cov(Lu) = Y^T Y`.

The revised formula passes the exact Gaussian-posterior covariance unit test,
but the real window still failed the resource gate.  The first 34 epochs took
about 1.5 minutes in total; the epoch-35 all-window marginal had not completed
after 23 minutes 33 seconds.  Observed memory reached about 7.3 GB RSS and
about 4.8 GB swap, with 14.6 GB virtual size.  It was terminated deliberately
at the resource boundary.  No raw-window marginal was emitted.

This result identifies the remaining computational defect: it is not merely
the explicit normal equation, but fill and duplicated epoch-local variables in
the one-shot all-history SparseQR.  Extending this implementation to three
hours would be unjustified.

### 7.4 Revised implementation sequence

1. Replace all-history recomputation with an incremental square-root fixed-lag
   smoother.  At each epoch, eliminate receiver clock, receiver phase,
   ionosphere and other epoch-local nuisance immediately; retain only a bounded
   separator containing the physical WL datum functionals, their exact arc
   versions and the states needed by the next epoch.
2. When an epoch leaves the lag, marginalise it once by QR and carry forward a
   square-root prior on the separator.  Do not materialise a covariance matrix
   or duplicate all historical state columns.
3. Add a dense toy equivalence test comparing incremental fixed-lag output with
   the existing batch posterior for every lag boundary, including tree exchange
   and physical arc reset.
4. Rerun 5-, 15- and 35-minute resource/precision gates.  Acceptance requires
   bounded memory versus window duration, exact agreement with batch results on
   small cases, and a completed raw marginal.
5. Only then run three-hour shadow, four-tree same-observation replay, whitening
   and held-out prediction.  Feedback, 24-hour products and 20-station
   PPP/PPP-AR remain prohibited until those gates pass.
