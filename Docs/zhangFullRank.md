# Zhang full-rank PPP-RTK stage-one model

`processing_options:gnss_general:zhang_full_rank` is an opt-in implementation
of the code-plus-phase, ionosphere-float, CDMA full-rank model used for the
first validation stage.

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

This controller is mutually exclusive with `phase_clock_osb`. It also requires
uncombined processing and two known, distinct carrier frequencies.
