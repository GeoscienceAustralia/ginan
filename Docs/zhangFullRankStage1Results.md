# Zhang full-rank stage-one validation

The stage-one implementation was validated directly in Ginan without a Python
prototype.

## C++ checks

`zhang_full_rank_tests` verifies:

1. omission of ambiguity states in the reference receiver row and reference
   satellite column;
2. full column rank of a dual-frequency design matrix containing troposphere,
   receiver/satellite clocks, link ionosphere, receiver/satellite phase biases
   and double-difference ambiguities;
3. exact observation closure after changing the reference receiver and
   satellite.

The test matrix has 48 rows, 45 columns and numerical rank 45. All three Zhang
tests and the four existing phase-clock/OSB regression tests passed.

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
datum invariance has been established for the static observation equations,
but online state/covariance S-transforms remain future work.

This stage does not validate ambiguity fixing, ionosphere-weighted regional
constraints, clock/OSB product generation or user PPP-RTK performance.
