# Zhang full-rank stage-one validation

The stage-one implementation was validated directly in Ginan without a Python
prototype.

## C++ checks

`zhang_full_rank_tests` verifies:

1. omission of ambiguity states in the reference receiver row and reference
   satellite column;
2. the raw dual-frequency `48 x 74` design has rank 45 and the expected
   29-dimensional null space;
3. all 29 analytic IF, GF, clock and phase/ambiguity null-space directions
   satisfy `A_raw V = 0`, and the raw-to-full-rank map annihilates the same
   directions;
4. the raw generalized-inverse solution and the direct full-rank solution
   produce the same 45 estimable parameters, fitted observations and
   residuals;
5. full column rank of the `48 x 45` reparameterized design containing
   troposphere, receiver/satellite clocks, link ionosphere,
   receiver/satellite phase biases and double-difference ambiguities;
6. exact observation closure after changing the reference receiver and
   satellite;
7. exact state, full covariance, predicted-observation, innovation and
   innovation-covariance closure under a receiver/satellite datum change.

The raw design has numerical nullity 29, while the reparameterized design has
numerical rank 45 and nullity zero. The six Zhang test cases and the four
existing phase-clock/OSB regression tests pass.

## European network run

The 2019-07-18 test used FFMJ, GANP, MARS, MATE, NOT1, PADO and ZIM2, GPS
L1C/L2W, fixed station coordinates, external precise orbits and seven epochs
from 00:00 to 00:30 UTC at 300-second spacing.

With ZIM2/G05 as receiver/satellite datum:

- no ZIM2 receiver-clock or receiver-phase-bias state was created;
- no ZIM2-row or G05-column ambiguity state was created;
- no baseline code-bias state was created;
- retained ambiguity states were labelled `Zhang DD`;
- code postfit RMS was 0.227819 m;
- phase postfit RMS was 0.000838 m.

A second independent run used GANP/G07:

- code postfit RMS was 0.227834 m;
- phase postfit RMS was 0.000840 m;
- the phase-residual difference between the runs had 0.000032 m RMS.

The independent Kalman runs did not transform their initial state covariance
between datums. Their code residuals therefore differ by 0.039345 m RMS. Exact
datum invariance was first established for the static observation equations.

The runtime implementation now applies

\[
\mathbf{x}'=\mathbf{T}\mathbf{x},\qquad
\mathbf{P}'=\mathbf{T}\mathbf{P}\mathbf{T}^{T}
\]

to the state, covariance, state correction and RTS transition matrix. It has
been exercised with two satellite-reference changes and one
receiver-reference pressure test on the European network. The formal unit test
also verifies, for the same observations on both sides of a switch,

\[
\mathbf{H}'\mathbf{x}'=\mathbf{H}\mathbf{x},
\]

\[
\mathbf{v}'=\mathbf{v},
\]

and

\[
\mathbf{H}'\mathbf{P}'\mathbf{H}'^{T}+\mathbf{R}
=
\mathbf{H}\mathbf{P}\mathbf{H}^{T}+\mathbf{R}.
\]

## Pure-observation real-data rank

The runtime diagnostic constructs a separate matrix from `CODE_MEAS` and
`PHAS_MEAS` entries only. It does not include pseudo-observations, initial
state constraints or process-noise equations. Zero columns are removed, the
remaining columns are normalized, and numerical rank is computed with the
threshold `1e-10 * sigma_max`.

The unrestricted seven-station input exposed a real boundary that the Kalman
run had hidden: the first three epochs had nullity 4 and the remaining epochs
had nullity 2. The null vectors consisted of the G08/G09 satellite phase-bias
states and their incident ambiguity states. ZIM2 did not observe those
satellites, so the configured ZIM2 receiver row was not a spanning-tree edge
for them.

The formal stage-one rank configuration therefore restricts the test to the
fixed ZIM2/G05 common-view star by excluding G08 and G09. All seven epochs are
then full column rank:

| Epoch (UTC) | Rows | Active columns | Rank | Nullity | Smallest normalized singular value | Condition number |
|---|---:|---:|---:|---:|---:|---:|
| 00:00 | 132 | 117 | 117 | 0 | 0.006678 | 332.50 |
| 00:05 | 140 | 123 | 123 | 0 | 0.009331 | 238.05 |
| 00:10 | 140 | 123 | 123 | 0 | 0.010094 | 220.34 |
| 00:15 | 140 | 123 | 123 | 0 | 0.010932 | 203.57 |
| 00:20 | 140 | 123 | 123 | 0 | 0.011814 | 188.38 |
| 00:25 | 140 | 123 | 123 | 0 | 0.012500 | 177.95 |
| 00:30 | 140 | 123 | 123 | 0 | 0.012541 | 177.15 |

This is a full-rank validation of the fixed-star stage-one model, not a claim
that the current state definition supports an arbitrary incomplete bipartite
observation graph. Such a graph requires a general spanning-tree ambiguity
S-basis rather than unconditional deletion of one receiver row and one
satellite column.

This stage does not validate ambiguity fixing, ionosphere-weighted regional
constraints, clock/OSB product generation or user PPP-RTK performance.
