# Zhang full-rank PPP-RTK network model

`processing_options:gnss_general:zhang_full_rank` is an opt-in implementation
of the code-plus-phase, ionosphere-float, CDMA full-rank model and its
general sparse-graph ambiguity basis.

For the two configured baseline observables, Ginan interprets its existing
states as:

- `REC_CLOCK` and `SAT_CLOCK`: the code-IF-redefined receiver and satellite
  clocks, with the configured reference receiver clock selected as S-basis;
- `IONO_STEC`: the code-GF-redefined link ionosphere;
- receiver and satellite `PHASE_BIAS`: the final estimable phase-bias
  combinations;
- `AMBIGUITY`: only the receiver/satellite interior is retained, and these
  states are the integer double-difference ambiguities.

Ginan writes both receiver and satellite phase-bias design coefficients as
`+1`. Zhang's paper writes the satellite term as `-\widetilde{\delta}_j^s`.
Consequently, Ginan's satellite `PHASE_BIAS` state is the sign-reversed Zhang
satellite phase-bias parameter; the observation correction is unchanged.

The implementation makes the S-basis structural: it does not add weak datum
pseudo-observations. The following states are absent from the filter:

1. receiver and satellite baseline code biases;
2. the configured reference receiver clock;
3. the configured reference receiver phase biases;
4. ambiguities belonging to the reference receiver row;
5. ambiguities belonging to the reference satellite column.

The reference receiver and satellite can either remain fixed over a common-view
arc or be changed online with an exact S-transform.  For old references
receiver \(a\), satellite \(A\), and new references receiver \(b\), satellite
\(B\), the Ginan-sign state transform is:

\[
\begin{aligned}
C'_r &= C_r-C_b, &
S'_s &= S_s-C_b,\\
U'_{r,j} &= U_{r,j}-U_{b,j}
 +\lambda_j(D_{rB,j}-D_{bB,j}),\\
V'_{s,j} &= V_{s,j}+U_{b,j}+\lambda_jD_{bs,j},\\
D'_{rs,j} &= D_{rs,j}-D_{bs,j}-D_{rB,j}+D_{bB,j}.
\end{aligned}
\]

Here \(C\) and \(S\) are receiver and satellite clock states, \(U\) and
\(V\) are receiver and satellite phase-bias states, and \(D\) is the Zhang
double-difference ambiguity.  A state on the old reference receiver row or
old reference satellite column is implicitly zero.  Clock-rate states, when
present, use the same receiver-datum transformation as the clocks.

The complete filter state and covariance are transformed with

\[
\mathbf{x}'=\mathbf{T}\mathbf{x},\qquad
\mathbf{P}'=\mathbf{T}\mathbf{P}\mathbf{T}^{T}.
\]

Thus all cross-covariances are retained and the switch adds no stochastic
information.  The implementation rejects a switch if a required source state
is absent or if active pseudo states make an exact full-state transform
unsafe.

Example:

```yaml
processing_options:
  gnss_general:
    sys_options:
      gps:
        process: true
        code_priorities: [L1C, L2W]

    zhang_full_rank:
      enable: true
      output_diagnostics: true
      sys_options:
        gps:
          baseline_observables: [L1C, L2W]
          reference_receiver: ZIM2
          reference_satellite: G05
          auto_reference_switch: true
          reference_outage_epochs: 2
          reference_receiver_candidates: [ZIM2, MATE, PADO]
          reference_satellite_candidates: [G05, G12, G24]
```

`reference_outage_epochs` is the number of consecutive unavailable epochs
before replacement.  Candidate lists are priority ordered.  If no candidate
is usable, the receiver with the largest baseline-observable satellite count
and the common satellite with the largest network elevation sum are selected.
The replacement satellite must be visible with both baseline code and phase
observables at every active reference station; otherwise the switch is
deferred rather than applying an incomplete transform.

## Pure-observation rank diagnostic

With `output_diagnostics: true`, PEA reports one
`ZHANG_PURE_OBS_RANK` record per epoch. The diagnostic uses only
`CODE_MEAS` and `PHAS_MEAS` rows. Pseudo-observations, initial-state
constraints and process-noise equations are excluded. Columns that are
identically zero in the current epoch are removed, the active columns are
normalized, and singular values below `1e-10 * sigma_max` are treated as
numerically zero. If the matrix is deficient, `ZHANG_PURE_OBS_NULL_VECTOR`
records identify the dominant state components of every null direction.

The fixed receiver-row/satellite-column S-basis is full rank only when those
star edges span the active receiver-satellite graph: the reference receiver
must observe every modelled satellite, and every modelled receiver must
observe the reference satellite on both baseline observables. Measurements
outside that common-view star must be excluded for the fixed-star stage-one
model.

The reproducible European rank check is provided by
`exampleConfigs/zhang_full_rank_europe_formal_rank.yaml`. Its seven epochs are
full column rank after G08 and G09, which are not observed by the reference
receiver ZIM2 in this arc, are excluded. Detailed numerical results are in
`Docs/zhangFullRankStage1Results.md`.

## General sparse-graph spanning-tree basis

Stage two enables a general ambiguity S-basis with:

```yaml
processing_options:
  gnss_general:
    zhang_full_rank:
      sys_options:
        gps:
          baseline_observables: [L1C, L2W]
          reference_receiver: ZIM2
          use_spanning_tree: true
```

For each constellation, the controller constructs the bipartite graph whose
receiver-satellite edges have valid code and phase observations on both
configured baseline frequencies. Only the connected component containing the
configured root receiver is modelled. Other components are reported and their
baseline observations are not inserted into the common-datum filter.

The controller selects a deterministic maximum-quality spanning tree. Valid
edges from the previous tree are preferred, then root-receiver edges, then
higher-elevation edges. This keeps the ambiguity basis stable without requiring
one receiver to observe every satellite or one satellite to be observed by
every receiver.

For one frequency, write an edge phase constant as

\[
q_{rs}=U_r+V_s+\lambda N_{rs}.
\]

The root receiver phase state and all tree-edge ambiguities are S-bases.
Tree-edge integer terms are absorbed into the estimable node phase states.
Every non-tree edge \(e\) closes one unique fundamental cycle with the tree and
owns one integer state

\[
D_e=N_e-\sum_{a\in\operatorname{path}_{\mathcal T}(e)}
\operatorname{dir}(a)N_a.
\]

The fixed-star double difference is the four-edge special case. A general
sparse graph may produce longer even cycles, but every coefficient remains
\(0,\pm1\), so the retained state is still an integer combination of the raw
ambiguities.

If an existing tree edge fails and an already-estimated non-tree edge can
replace it, the controller reconstructs every represented edge phase constant,
decomposes it in the new tree coordinates, and applies the resulting exact
linear transform to the complete state and covariance. A newly observed edge
has no old-state expression. If no exact replacement is available for
`reference_outage_epochs`, the controller explicitly reinitializes the
constellation's phase and cycle coordinates in the new tree, logs
`phase_datum_discontinuity=true`, and leaves clocks, atmosphere, troposphere,
and unrelated states intact. It never invents the missing edge state or adds a
datum pseudo-observation. A new leaf receiver or satellite can extend the tree
without changing existing coordinates.

The implementation uses the L1C/L2W intersection as the stage-two integer
backbone. This ensures that both frequencies use the same tree and fundamental
cycles. Frequency-specific auxiliary edges and independent per-frequency trees
remain a later multi-frequency extension; the intersection is an engineering
choice for the first dual-frequency implementation, not a theoretical
requirement of the Zhang model.

## Integer fixing, feedback, and internal fixed products

Non-tree `AMBIGUITY` states are tagged `Zhang cycle` and are passed, with their
full covariance, to Ginan's existing LAMBDA/PAR implementation. Accepted
integer constraints are fed back with `applyUCAmbiguities()`, so satellite
clocks, phase states, ionosphere, troposphere, and all cross-covariances receive
the conditional fixed-solution update.

With diagnostics enabled, PEA emits:

- `ZHANG_AR_SUMMARY`: candidate/fixed counts, ADOP, and float-cycle fractional
  residual statistics;
- `ZHANG_FIXED_PRODUCT`: the ambiguity-fixed satellite clock, internal Zhang
  satellite phase state, their combined phase-observation correction, and its
  formal standard deviation.

These records are internal same-datum research products. They are not standard
Bias-SINEX phase OSBs and must not be combined with clocks or biases generated
under another datum. The runnable stage-two overlay is
`exampleConfigs/zhang_full_rank_europe_stage2.yaml`; formal and European
numerical results are recorded in `Docs/zhangFullRankStage2Results.md`.

## Independent internal-product PPP-AR validation

The `zhang_pppar` controller can serialize FLOAT and post-feedback FIXED
satellite clock/phase corrections to a CSV product stream and apply them in a
separate held-out user PEA process.  The user process imports no network
receiver state, ambiguity, covariance, or spanning-tree coordinate.

Product continuity is managed per constellation, satellite, and observable.
Exact integer branch changes preserve validity; fractional transforms and
unrepresentable tree changes increment discontinuity metadata and invalidate
the affected user ambiguity datum.  User ambiguity-reference changes apply an
exact state and full-covariance single-difference transform.

`integer_valid` is deliberately conservative.  A FIXED record becomes valid
only when the held integer rank covers the full active candidate cycle space.
Partial fixing can update the network solution but cannot silently advertise a
complete constellation-wide integer datum.

The six-hour European leave-one-out experiment completed for an inside, edge,
and outside user.  The network never reached the complete integer-datum gate,
so all three users correctly performed zero ambiguity fixes.  The pipeline is
implemented, but scientific PPP-AR acceptance has not passed.  Details,
metrics, and reproducible commands are in
`Docs/zhangPppArLeaveOneOutResults.md`.

This controller is mutually exclusive with `phase_clock_osb`. It also requires
uncombined processing and two known, distinct carrier frequencies.
