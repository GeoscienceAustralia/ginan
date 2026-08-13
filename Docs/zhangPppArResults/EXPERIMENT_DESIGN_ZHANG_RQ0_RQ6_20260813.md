# Zhang product-lattice quotient experiment plan (R-Q0 to R-Q6)

## Scope and invariants

All stages are shadow experiments. Product feedback, temporal transition, BESD,
and user-side PPP-AR remain disabled until the algebraic acceptance gates below
pass. A numerically zero covariance does not by itself certify an integer.

For each target product lattice `L_T`, the implementation reports:

- `target_rank`: rank of `L_T`;
- `held_rank`: rank of the exact intersection `L_T intersect L_held`;
- `quotient_rank`: rank of the primitive quotient `L_T / L_H`;
- `quotient_covariance_rank`: numerical covariance rank on quotient coordinates;
- `newly_fixed_rank`: rank newly certified by quotient IAR/PAR;
- `combined_certified_rank`: exact HNF rank of held and newly fixed rows.

A component is certified only when the exact union equals the target lattice.
Arithmetic rank addition is diagnostic only and never authorizes certification.

## Experiment sequence

### R-Q0: exact held quotient audit

Use the frozen epoch 2024-07-17 00:15:30. Construct the exact target/held
intersection in integer row coordinates, verify primitivity and exact quotient
closure, then require `quotient_covariance_rank == quotient_rank`. Fail closed as
`UNTRACKED_DETERMINISTIC_RELATION` or
`DETERMINISTIC_INTEGER_INCONSISTENCY` where applicable.

### R-Q1: quotient IAR and exact union

Run LAMBDA/PAR only on quotient coordinates. Map newly fixed quotient rows back
to named and physical coordinates, combine them with held rows using exact HNF,
and emit all six rank fields. Preserve valid partial certified rows even when a
whole component is not yet certified.

### R-Q2: actual certified product graph

Test exact membership of every satellite-pair row in the combined certified
lattice. Build graph edges only from exact membership; marginal pair variance or
`Perr` cannot create certified edges. Contract each resulting certified
component before downstream bridge search.

### R-Q3: component gauge solver

Use the actual certified graph from R-Q2. Aggregate all correlated cross edges
between a singleton and a certified component, or between two certified
components, and solve the component gauge by correlated GLS/ILS. Do not require
the legacy candidate graph to certify an entire component first.

### R-Q4: integer-constrained product-gain frontier

For quotient ranks near 4 and 7, enumerate admissible primitive integer rows
with coefficient bounds `abs(a_i) <= 2` and then `<= 3`. Apply exact lattice
admissibility, `Perr`, and joint NIS gates. Report the reliable product-gain
frontier separately from the relaxed real-subspace upper bound. The comparison
field is `RELAXED_REAL_UPPER_BOUND_GAP`, not integer-search efficiency.

### R-Q5: conditioning-only mixed mode

Keep the mixed lattice private and shadow-only. Use it only for covariance
conditioning, never as a certificate. Re-run R-Q0 through R-Q4 and compare exact
held ranks, quotient ranks, certified graph edges, reliability, and gain.

### R-Q6: multi-epoch stability

Only after R-Q0 through R-Q5 pass their algebraic gates, run 00:10:00 through
00:16:00. Check provenance transitions among `CURRENT_FLOAT`,
`ZERO_VARIANCE_NUMERICAL`, `EXACT_HELD_CONSEQUENCE`,
`EXACT_CERTIFIED_DERIVED`, `TEMPORAL_RECERTIFIED`, and `BESD_CERTIFIED`.
Temporal gauge, BESD, and user PPP-AR remain out of scope until this stage is
stable.

## Current runnable artifact

`zhang_global_2024199_180_rq0_rq2_quotient_frozen_001530_20260813.yaml` and its
run/analyser scripts implement the first frozen R-Q0/R-Q1/R-Q2 evidence pass.
R-Q3 source instrumentation is included but requires a fresh PEA build before a
new run. R-Q4 to R-Q6 remain gated follow-up experiments and must not be reported
as completed by the frozen R-Q0/R-Q2 run.
