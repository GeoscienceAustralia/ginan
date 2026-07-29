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

The first stage requires a fixed reference receiver and satellite over a
common-view arc. Changing either reference without applying an S-transform
changes the parameter datum and is outside this first implementation.

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
```

This controller is mutually exclusive with `phase_clock_osb`. It also requires
uncombined processing and two known, distinct carrier frequencies.
