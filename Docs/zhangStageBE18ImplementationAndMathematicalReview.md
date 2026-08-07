# Stage B E18 implementation plan and mathematical review

## 1. Decision from E17

E18 is necessary and is feasible, but it must not extend the E17 scalar
likelihood-ratio accumulator.  E17-B produced 212 product-datum resets in 181
epochs, a maximum window length of seven, minimum WL `perr=0.17094`, and a
median reduced chi-square of 0.0281.  More epochs would only produce more short
segments.  The failure is a coordinate and covariance-model failure, not a
sample-count failure.

The implementation order is therefore:

1. prove deterministic user-target invariance;
2. define every estimated quantity from the user observation equation;
3. record the accepted float factors and state transitions;
4. form a square-root fixed-lag marginal without integer feedback;
5. whiten and validate the physical WL block;
6. run deterministic S-basis ablations on an identical observation replay;
7. enable controlled feedback only after the three-hour shadow gates pass.

## 2. Correct target and coordinate law

Let the same physical stochastic state be represented in two Zhang full-rank
coordinates by the exact affine change

\[
x_j=A_{ji}x_i+b_{ji},\qquad P_j=A_{ji}P_iA_{ji}^{T}.
\]

A scalar user target has the affine representation

\[
p=\ell_i^Tx_i+d_i=\ell_j^Tx_j+d_j.
\]

For an invertible equal-dimensional transform, invariance requires

\[
\ell_i=A_{ji}^{T}\ell_j,\qquad
d_i=\ell_j^Tb_{ji}+d_j.
\]

It follows immediately that

\[
\ell_i^TP_i\ell_i=\ell_j^TP_j\ell_j.
\]

Both the linear row and affine offset are mandatory.  Testing `G*k` alone is
invalid because the dynamic-tree node-potential term changes at a tree
exchange.  The test tolerance is applied to the physical scalar target,

\[
|p_i-p_j|<10^{-10}\ \text{cycle},
\]

and to its variance using a protected relative denominator,

\[
\frac{|\sigma_i^2-\sigma_j^2|}
{\max(\sigma_i^2,\sigma_j^2,\sigma_{floor}^2)}<10^{-10}.
\]

For a dimension-changing satellite join or leaf exit, an invertible complete
state transform does not exist.  Equality is required only for targets whose
support lies in the common physical subspace.  New-satellite targets start
after the join; removed-leaf targets end at the exit.

## 3. User-domain product definition

The current writer emits, in metres,

\[
c_{s,j}^{\phi}=C_s-B_{s,j}^{\phi},
\]

where the persistent integer alignment is part of the emitted phase bias.
For satellite pair \(s,r\), define a generic dual-frequency target

\[
a_{sr,WL}=\alpha_1(c_{s,1}^{\phi}-c_{r,1}^{\phi})
          +\alpha_2(c_{s,2}^{\phi}-c_{r,2}^{\phi}).
\]

The coefficients must carry explicit units.  If a cycle-domain difference is
required, the caller supplies the frequency-dependent inverse wavelengths
with the sign dictated by the actual user ambiguity equation.  E18 must not
silently assume that every pair of coefficients named "WL" has integer units.

The target is projected to every current basis as

\[
a_{sr,WL}=\ell_{sr,WL,T}^{T}x_T+d_T,
\]

and the implementation retains both \(\ell_T\) and \(d_T\), together with
\(\ell_T^TP_T\ell_T\).

## 4. Fixed-lag estimator

For accepted float observations in epoch \(k\), retain the actual factor

\[
r_k-H_k\delta x_k,\qquad R_k,
\]

plus transition/process factors

\[
\delta x_{k+1}-F_k\delta x_k,\qquad Q_k.
\]

Receiver clocks, troposphere, ionosphere, receiver phase biases and other
epoch-local nuisance states are eliminated with sparse QR or square-root
information elimination.  A dense global covariance or explicit inverse is
forbidden.  Ordering places epoch-local nuisance states first and the retained
physical WL target coordinates last.

Process covariance is generally positive semidefinite rather than positive
definite.  Positive process-noise directions are whitened as stochastic
factors; zero-noise directions are exact state substitutions/constraints and
must not be represented by an arbitrary very large weight.  The transition API
therefore exposes both the actual state-transition matrix and the actual
process covariance used by the filter.

No hard integer factor is inserted during float shadow estimation.  Integer
hypotheses are evaluated after obtaining the float physical-target marginal.
Only a later controlled-feedback experiment may add a selected integer factor.
This avoids using the desired integer answer to manufacture its own small
`perr`.

If an equivalent residual covariance is materialised for a small retained
target block, use pivoted LDLT or rank-revealing QR.  With

\[
S=LDL^T,
\]

the whitened residual/design are

\[
\widetilde v=D^{-1/2}L^{-1}v,\qquad
\widetilde H=D^{-1/2}L^{-1}H.
\]

Negative pivots beyond numerical tolerance, non-finite values, or an
unexpected numerical rank are hard failures, not values to clip silently.

## 5. Identity and reset policy

The window identity is the user target plus L1/L2 satellite phase segments and
the versions of every physical receiver-satellite arc in its support.

* A legal exact S-basis exchange transforms the target row, state and
  square-root factor; it does not reset the window.
* A receiver or satellite reference change is treated identically when it is
  an exact coordinate-only event.
* A real phase-segment or physical-arc-version change resets only affected
  targets.
* A satellite join starts new supported targets without resetting unrelated
  targets.
* A leaf exit retires affected targets without resetting targets supported by
  the surviving subgraph.
* A product datum metadata event that has an exact affine target transport is
  not a physical reset.  An event lacking such a transport fails closed.

## 6. Deterministic gates before data replay

Artificial states and positive-definite covariances are replayed through at
least three legal bases.  Tests cover:

1. ordinary edge exchange;
2. receiver-root change;
3. satellite-reference change;
4. satellite join on the common target subspace;
5. leaf exit on the common target subspace;
6. unchanged physical arc across a basis event;
7. changed physical arc version and phase segment;
8. independent L1/L2 backbones and their common user WL target.

Mean and variance tolerances are both `1e-10`; zero-variance comparisons use an
absolute floor.  A pure coordinate change must return `CONTINUE_TRANSFORMED`.
A true version/segment change must return `RESET_PHYSICAL_IDENTITY`.

## 7. Data experiments and acceptance order

### E18-A: deterministic unit gate

Build and run all Zhang unit tests.  No real-data run is allowed if any target
or variance invariant fails.

### E18-B: factor-capture regression

On the existing three-hour data, record accepted raw prefit factors,
measurement covariance, transitions, process factors, phase segments, physical
arc versions and exact S-basis transforms.  Keep `feedback=0`.  With factor
capture enabled and estimation disabled, product output must be byte/numerical
equivalent to the E17-B baseline.

### E18-C: square-root window shadow

Run the sparse fixed-lag elimination and retain only the physical WL target
block.  Check finite rank, non-negative retained variances, chi-square scale,
multi-lag ACF and Ljung--Box statistics.  Do not use a single lag-1 threshold as
a substitute for whiteness.

### E18-D: S-basis ablation

Replay the identical accepted observation set and QC decisions with:

* frequent minimum-tree switching;
* persistent-core tree;
* deterministic seeded random legal trees;
* E15 baseline strategy.

For every common target/epoch, compare mean, covariance, integer candidate,
`perr`, window age and reset classification.  Target and covariance equality
are primary gates; a similar aggregate fixing rate is insufficient.

### E18-E: independent prediction

Use held-out epochs and held-out receivers.  A held-out receiver must not
contribute any factor to the network window.  Report normalized prediction
residuals and coordinate integrity, not only integer success.

### E18-F: three-hour shadow decision

The run passes only if all of the following hold simultaneously:

* target mean/variance invariant within tolerance;
* physical identity events classified correctly;
* no negative retained variance, non-finite result, or unexplained rank loss;
* whitened residual sum lies in the configured chi-square confidence interval;
* Ljung--Box is not rejected at the declared significance level, or the
  remaining correlation is explicitly modelled and the test repeated;
* held-out receiver and epoch prediction pass;
* WL `perr <= 1e-3` with stable candidates and independent prediction;
* `feedback=0` and exact product regression.

Only then may E18-G evaluate conditional L1, controlled feedback, a six-hour
run, a 24-hour run, and the 20 independent PPP/PPP-AR users in that order.

## 8. Feasibility and principal risk

The approach is mathematically feasible because the physical target is a
linear functional and exact coordinate changes preserve its Gaussian marginal.
Sparse square-root elimination avoids the tens-of-thousands-dimensional dense
covariance implied by 74 stations and multiple epochs.

The principal implementation risk is architectural: the current E17 hook sees
only prior/posterior marginals after nuisance information has already been
reused.  E18 must capture the original accepted measurement and transition
factors at the Kalman assembly boundary.  If those factors cannot be replayed
identically, the estimator has not implemented E18 even if its final residuals
look favourable.

## 9. Implementation checkpoint (2026-08-04)

Completed before starting a real-data shadow:

* added an explicit affine user-domain phase/WL target, including persistent
  alignment offsets, exact coordinate transport, and protected variance
  comparison;
* changed the internal product writer to evaluate `C_s-B^phi_s,j` through that
  target definition while retaining the same algebraic product;
* added deterministic tests for three affine bases, ordinary tree exchange,
  receiver-root change, satellite-reference re-expression, satellite join,
  leaf exit, independent dual-frequency backbone changes, and physical
  identity reset classification;
* added sparse-QR nuisance elimination, retained-block rank checks and
  rank-revealing covariance whitening;
* exposed the filter's state-transition matrix and actual process covariance
  through optional, non-mutating outputs for the upcoming factor recorder.

The `zhang_full_rank_tests` executable now passes 53/53 cases.  The production
`pea` target also compiles.  E18-B is not yet declared complete: original-factor
capture must still preserve chronological key maps and exact S-basis transform
events before the three-hour replay is scientifically valid.

The three-hour E17-B configuration was then replayed as a strict non-intrusion
regression.  It completed 181 epochs with 21,721 CSV lines (header included).
After preserving the legacy floating-point evaluation order in the product
writer, both artifacts were byte-identical to the pre-E18 baseline:

* internal products SHA-256:
  `37bdb18e8f148077f57d8162c1f4a8994946d232fa233969c1cb684a9f0bc8ee`;
* covariance SHA-256:
  `fdb269050848122908c8cbf59071d22b7b4c177cad3d7bd58048b69316cf2c18`.

This passes the non-intrusion gate only.  It does not supply E18 whiteness,
integer-candidate or independent-prediction evidence.

## 10. Raw-factor and user-target capture checkpoint (2026-08-04)

The authoritative network filter now exposes three read-only callbacks at the
actual accepted-factor boundary: the final post-QC measurement factor, the
state transition with the process covariance actually used, and every exact
state-coordinate transform.  The recorder stores the measurement in absolute
linearised form `H*x = V + H*x_minus`, preserves ordered state keys, and fails
closed on a broken key chain, a non-finite factor, a replay mismatch, or an
event limit that would require an unimplemented marginal anchor.

The three-hour E18-B capture before adding target blocks contained 181 accepted
measurement events, 360 transitions and 51 exact coordinate transforms.  The
transforms comprised 24 GPS tree exchanges and 27 local phase-coordinate
reinitialisations.  The maximum replay prior error was zero for the mean and
`3.98271e-19` for the covariance.  No event was rejected and `feedback=0`.
The products and covariance retained the E17 SHA-256 values quoted above.

The physical user target is now recorded as the explicit affine correction

\[
a_{sr}=\frac{c^\phi_{s,1}-c^\phi_{r,1}}{\lambda_1}
       -\frac{c^\phi_{s,2}-c^\phi_{r,2}}{\lambda_2},
\]

together with both satellite phase segments and the complete versioned
physical-arc support.  The recorder verifies its mean and variance against the
factor replay at `1e-10`.  A pure support/coordinate change is retained when
all known arc versions are continuous; a phase-segment change or a repeated
physical arc with a new version is a physical reset.

This correction target must not yet be rounded.  Its cycle-labelled units do
not by themselves make it an integer: the common satellite clock contributes
with coefficient `1/lambda_1 - 1/lambda_2`.  The integer `perr` gate must be
applied only after the raw factor block has eliminated the clock, receiver
phase, ionosphere and other nuisance directions and has exposed the retained
integer datum functional dictated by the complete user observation equation.
Treating the correction target itself as `N1-N2` would repeat the target-space
error diagnosed before E18.

For each epoch, the implementation also forms the rank-revealing retained
target information increment

\[
J=P_{a,+}^{-1}-P_{a,-}^{-1},\qquad
h=P_{a,+}^{-1}\mu_{a,+}-P_{a,-}^{-1}\mu_{a,-},
\]

which is the small Schur-equivalent likelihood block after nuisance
elimination.  Positive information directions are whitened; a materially
negative direction, non-positive target marginal or zero information rank is
reported as an invalid block.  This one-epoch retained block is a prerequisite
for, but not a substitute for, the multi-epoch fixed-lag factor replay and the
receiver/epoch exclusion tests.

### 10.1 Three-hour retained-block result

The final run preserved all 181 measurement events, 360 transition factors and
51 exact transforms, with no capture rejection.  It recorded 521 accepted
physical user targets over 93 epochs.  Every one-epoch retained block was
finite and full rank for its selected targets (aggregate rank 521); no negative
information direction or target replay mismatch occurred.  Product regression
remained byte-identical to E17.

The stochastic gate nevertheless failed decisively.  The aggregate whitened
sum of squares was only `0.123216669238` for 521 retained directions, instead
of being on the scale of its degrees of freedom.  The residual ACF included
`rho_4=-0.185161` and `rho_6=0.193594`.  The ten-lag Ljung--Box statistic was
`Q=44.5281`, whose upper-tail probability is approximately `2.65e-6`.

This is not evidence of exceptional precision.  It means that the correction
target block is vastly over-dispersed relative to its realised innovations and
still temporally correlated.  The most likely structural reason is now
explicit: the recorded `c` combination is a user correction functional, not
the final integer datum functional after eliminating the common clock and the
remaining user/network nuisance directions.  E18-C therefore remains open.
Four-tree final comparison, held-out replay, integer `perr`, feedback, 24-hour
products and 20-station PPP/PPP-AR are not authorised by this result.
