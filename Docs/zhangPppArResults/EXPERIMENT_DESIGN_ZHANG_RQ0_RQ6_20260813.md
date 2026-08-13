# Zhang product-lattice quotient experiment plan (R-Q0 to R-Q6)

## Scope and invariants

All stages are shadow experiments. Product feedback, temporal transition, BESD,
and user-side PPP-AR remain disabled until the algebraic acceptance gates below
pass. A numerically zero covariance does not by itself certify an integer.

For each target product lattice `L_T`, the implementation reports:

- `target_rank`: rank of `L_T`;
- `persistent_held_intersection_rank`: rank contributed by the historical held
  ledger;
- `current_certified_increment_rank`: additional rank contributed by exact,
  statistically accepted current-epoch rows that already conditioned the
  disposable covariance but have not yet passed persistent promotion;
- `held_rank`: rank of the exact intersection with the union of those two
  certified lattices;
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

The same-epoch conditioning lattice is passed to this audit explicitly and is
never inserted early into the persistent held ledger. This removes a concrete
ordering bug: `LAYERED_WIDE_LANE` conditions the covariance before product
audit, while persistent admission occurs only after the enclosing AR transaction
commits. Omitting those exact rows produced unexplained covariance nullities of
one and five in the first frozen run.

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

The implementation exhaustively enumerates primitive rank-one rows inside each
coefficient bound. A rank-one point is therefore an exact optimum within that
finite domain. Higher-rank points are labelled either
`EXACT_WITHIN_COEFFICIENT_BOUND` when enumeration remains complete, or
`RELIABLE_BEAM_LOWER_BOUND` after candidate/beam truncation. A lower bound must
not be reported as the global integer optimum `G_k^Z`.

### R-Q5: conditioning-only mixed mode

Keep the mixed lattice private and shadow-only. Use it only for covariance
conditioning, never as a certificate. Re-run R-Q0 through R-Q4 and compare exact
held ranks, quotient ranks, certified graph edges, reliability, and gain.

The private scenario is emitted as `audit_scenario=PRIVATE_CONDITIONING_ONLY`;
the authoritative input remains `audit_scenario=BASELINE`. Exact pair
consequences are removed from the accepted mixed fixed lattice first, so only
the residual primitive quotient is applied to the private posterior.

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
R-Q3 component bridging, the R-Q4 bounded integer frontier, and the R-Q5 private
conditioning comparison require a fresh PEA rerun. The source, including the
same-epoch certified-lattice provenance repair, builds and its 176 formal tests
pass; that is not real-data validation. R-Q6 has a separate
00:10--00:16 configuration and fail-closed multi-epoch analyser. None of these
later source additions may be claimed as experimentally validated by an older
binary.
