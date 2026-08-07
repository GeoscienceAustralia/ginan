# Stage B E18 integer-datum raw-window implementation and replay

## 1. Correction to the retained quantity

The former E18 hook retained the cycle-scaled user phase correction

\[
\frac{(C_s-B^\phi_{s,1})-(C_r-B^\phi_{r,1})}{\lambda_1}
-\frac{(C_s-B^\phi_{s,2})-(C_r-B^\phi_{r,2})}{\lambda_2}.
\]

This is not an integer because the satellite-clock coefficient does not cancel.
The revised hook accepts only the affine integer datum

\[
a_{sr,WL}=z_T+G_T(k_1-k_2),
\]

where `G_T` is the exact current-tree chord row and `z_T` is the integer
translation maintained by the persistent satellite datum manager.  The code
rejects a target when the two signals do not both have an exact continuous
alignment; an unaligned relink candidate is not silently promoted to a
persistent datum.

## 2. Complete observation equation used by the gate

The deterministic test uses the signs implemented in `ppp_obs.cpp`:

\[
\Phi_{rj}=\rho+C_r-C_s+B^\phi_{rj}-\alpha_j I_r^s
            +\lambda_jN_{rj}^s+\epsilon_{\Phi,j},
\]

\[
P_{rj}=\rho+C_r-C_s+\alpha_jI_r^s+\epsilon_{P,j}.
\]

The retained row is `N1-N2`.  Satellite clock, receiver clock, receiver phase
biases and ionosphere remain in the factor rows and are eliminated by the
window; they are not fixed to their simulated truth.

## 3. Raw-factor window

For a boundary Gaussian and process model, the implementation writes

\[
x_k=\bar x_k+B_ku,
\]

where each positive boundary/process covariance direction contributes an
independent standard-normal component of `u`.  Zero process-noise directions
contribute no component and are therefore exact substitutions.  An exact
S-basis transform updates `B` and `x_bar` directly.  Every accepted observation
is stored and replayed as

\[
R_k^{-1/2}H_kB_ku=R_k^{-1/2}(b_k-H_k\bar x_k).
\]

The assembled matrix is sparse.  Sparse QR solves the raw factor system and a
sparse symmetric information solve projects its covariance to the small
retained block.  No hard integer factor and no feedback are present.

## 4. Deterministic results

All 58 Zhang unit tests pass.  The complete-equation two-epoch marginal agreed
with an independent covariance-form Kalman control to `0` in mean and
`6.54e-13 cycle^2` in variance.  Four distinct legal trees replaying the same
two accepted observation blocks agreed to `2.22e-16 cycle` in target mean and
`9.99e-16 cycle^2` in target variance.  The four strategies are deterministic baseline,
persistent-preferred, an alternative legal preferred tree and a seeded random
legal tree.

## 5. Real-data boundary

The initial 60-second overlay accidentally inherited
`wait_next_epoch=60.05 s`.  A 74-station epoch took about `102 s`, so Ginan
explicitly logged `Excessive time elapsed, skipping epoch`.  This was a replay
scheduling error, not a lack of RINEX data: the 74 files are 30-second data.
The smoke overlay now keeps `epoch_interval=60 s` and sets
`wait_next_epoch=3600 s`.  Disabling the unrelated per-epoch dense
pure-observation rank/SVD audit reduced the complete nine-epoch runtime to
about 40 seconds, with no dropped epoch.

The corrected run captured nine accepted measurement factors, sixteen state
transitions and two exact local phase-coordinate transforms.  It retained
21,824 measurement rows and 117,818 design nonzeros.  Chronological replay
matched every Kalman prior exactly in mean and covariance and no factor event
was rejected.

The run explicitly requested three named product datum functionals per epoch.
All 27 requests were rejected with
`UNCONSTRAINED_INTEGER_DATUM_GAUGE`; the raw window reported requested target
count 3 and target rank 0 at every epoch.  This is the mathematically correct
result for this short interval: `PRODUCT_TARGET_WL_L1` produced zero fixed WL
relations, so no observation or persistent integer constraint identifies the
tree-potential term `z_T`.  Replacing the missing term by zero would estimate
`Gk`, not the requested persistent product datum.

A three-hour four-policy replay and whiteness/perr gates therefore remain
blocked.  The next implementation step is to carry `z_T` as an explicit
latent integer-gauge variable and attach only exact persistent-lattice
relations to it.  Until that augmented block has nonzero rank, feedback and
user PPP-AR remain disabled.
