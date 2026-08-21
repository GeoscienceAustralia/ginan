# Zhang full-rank stage-two results

## Scope

Stage two implements and validates the GPS L1C/L2W common-backbone version of:

- a general receiver-satellite bipartite graph;
- a deterministic spanning-tree ambiguity S-basis;
- integer fundamental-cycle ambiguity states on non-tree edges;
- exact state and full-covariance transforms between represented trees;
- explicit phase-datum reinitialisation when a new replacement edge has no old state;
- LAMBDA/PAR cycle fixing and full-state fixed-solution feedback;
- same-datum internal satellite clock/phase correction records.

The internal correction is not a standard phase OSB and is not interoperable
with an external clock or Bias-SINEX product without a later datum transform.

## Formal acceptance tests

`test_ZhangFullRank.cpp` contains 17 tests. The stage-two graph tests cover:

1. full rank for random connected sparse bipartite graphs;
2. observation, innovation, innovation-covariance, and integer-lattice closure
   between two different spanning trees;
3. a tree-edge failure and exact basis exchange;
4. disconnected-graph detection and root-component isolation;
5. conditional integer feedback to the complete state and covariance;
6. synthetic leave-one-out recovery of user integer differences with internal
   clock/phase products.

Five additional product-continuity tests cover exact integer branch changes,
fractional transforms, explicit reinitialisation, and held-out user reference
exchange with full-covariance closure. They also verify that loss of complete
held integer rank invalidates the product exactly once.

The Zhang test executable passes 17/17 cases. The pre-existing phase-clock/OSB
regression executable passes 4/4 cases.

## European seven-station run

Configuration:

- receivers: FFMJ, GANP, MARS, MATE, NOT1, PADO, and ZIM2;
- root receiver: ZIM2;
- date: 2019-07-18;
- interval: 00:00--06:00 GPST, 300 s sampling;
- observations: GPS L1C/L2W code and phase intersection;
- orbit: fixed WUM precise orbit;
- ambiguity resolution: LAMBDA/PAR, ratio threshold 3, success threshold 0.99;
- input overlays:
  `zhang_full_rank_europe_stage1.yaml` and
  `zhang_full_rank_europe_stage2.yaml`.

The PEA run completed 73 epochs with exit code 0.

### Graph and numerical-rank results

- initial graph: 14 nodes, 39 edges, 13 tree edges, 26 cycles;
- exact tree exchanges: 16;
- leaf extensions: 4;
- explicit phase-datum reinitialisations: 9;
- pure-observation rank checks: 73/73 full column rank;
- observation rows: 140--244;
- active columns: 124--205;
- minimum normalized singular value: 0.004118--0.016740;
- normalized condition number: 131.23--530.68.

The rank diagnostic uses only `CODE_MEAS` and `PHAS_MEAS`. It excludes initial
constraints, process equations, and pseudo-observations.

The nine reinitialisations are not exact datum-preserving transforms. They
occur when an active replacement tree edge has no prior cycle state. The
implementation removes the old phase/cycle coordinate block, initializes the
new one from subsequent observations, and emits an explicit discontinuity
record. Downstream real-time products must propagate this event through a
phase discontinuity counter.

### Integer fixing and internal products

- AR diagnostics: 73/73 epochs;
- epochs with newly accepted integers: 8;
- newly fixed fundamental-cycle integers: 71 in total;
- fixed-update internal product records: 144.

Accepted integers are returned through `applyUCAmbiguities()`. Consequently,
the fixed update propagates through the full covariance to satellite and
receiver clocks, satellite and receiver phase states, ionosphere, troposphere,
and all retained cross-covariances.

The internal product records contain:

- the post-feedback satellite clock;
- the post-feedback internal satellite phase state;
- their same-datum phase-observation correction;
- the formal correction standard deviation;
- phase continuity metadata and a conservative integer-valid flag.

Partial network fixing does not authorize all satellite phase products.  The
integer-valid flag is true only when the held integer rank covers the complete
candidate cycle dimension.  Real independent user results and the resulting
scientific rejection are documented in
`Docs/zhangPppArLeaveOneOutResults.md`.

## Validation boundary

These results validate the graph parameterisation, integer-cycle construction,
tree-coordinate management, integer feedback, and internal product
construction. They do not establish:

- a standard code or phase OSB datum;
- Bias-SINEX or SSR interoperability;
- regional ionosphere-weighted PPP-RTK performance;
- multi-frequency, multi-constellation, or GLONASS FDMA support.

The real internal-product adapter and leave-one-station experiment are now
implemented.  The experiment completed but did not establish a complete
network integer datum, so it correctly produced no authorized user fixes.  A
usable PPP-AR product remains unvalidated.
