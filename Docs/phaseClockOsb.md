# Datum-consistent phase-clock/OSB processing

Ginan's `phase_clock_osb` controller is an opt-in path for generating a
baseline-frequency ambiguity-fixed satellite clock together with compatible
code and phase observable-specific biases (OSBs). It preserves the legacy
behaviour when disabled.

The first supported production scope is deliberately narrow:

- GPS L1/L2;
- fixed external precise orbit;
- fixed reference-station coordinates;
- offline, undifferenced and uncombined network processing;
- C1W/C2W code datum;
- L1W/L2W ambiguity resolution;
- one configured receiver as the receiver/satellite phase-bias S-basis.

It does not by itself implement L5/all-frequency estimation, regional
ionosphere interpolation or real-time SSR broadcasting.

## Configuration

The controller is configured under `processing_options:gnss_general`:

```yaml
processing_options:
  gnss_general:
    sys_options:
      gps:
        process: true
        ambiguity_resolution: true
        code_priorities: [L1W, L2W]

    phase_clock_osb:
      enable: true
      enforce_code_datum: true
      constrain_reference_receiver_phase: true
      baseline_only_ambiguity_resolution: true
      output_diagnostics: true
      code_datum_sigma: 1.0e-6
      phase_datum_sigma: 1.0e-6
      datum_identifier: GPS_C1W_C2W_IF
      solution_id: GINAN_PHASE_CLOCK_OSB
      discontinuity_counter: 0
      sys_options:
        gps:
          baseline_code_observables: [L1W, L2W]
          baseline_phase_observables: [L1W, L2W]
          phase_reference_receiver: USN7
```

Ginan uses `L1W`/`L2W` internally for a signal identifier shared by code and
phase measurements. Bias-SINEX output writes these as C1W/C2W for code OSBs and
L1W/L2W for phase OSBs.

The configured reference receiver must match the actual receiver ID stored in
the filter; an option alias such as `PIVOT` is not sufficient unless that is
also the filter's receiver ID. The constraint defines an S-basis; it does not
claim that the receiver's physical hardware delay is zero.

## Datum equations

For baseline wavelengths \(\lambda_1,\lambda_2\), the controller calculates

\[
\alpha =
\frac{\lambda_2^2}{\lambda_2^2-\lambda_1^2},\qquad
\beta =
\frac{\lambda_1^2}{\lambda_2^2-\lambda_1^2},
\]

so that \(\alpha-\beta=1\). Satellite and receiver baseline code-bias states
are constrained by

\[
\alpha D_1-\beta D_2=0.
\]

This is different from the legacy `clock_codes` implementation, which fixes
each listed code bias independently to zero. The new constraint preserves the
observable DCB \(D_1-D_2\) while assigning its common OSB zero point to the
same ionosphere-free datum as the satellite clock.

For each baseline phase signal, the reference receiver constraint is

\[
B_{L,j}^{r_0}=0,\qquad j\in\{1,2\}.
\]

For each baseline signal, the controller then builds a deterministic spanning
tree over the receiver--satellite ambiguity graph. Starting from \(r_0\), it
constrains one integer ambiguity whenever that edge first reaches a new
satellite or receiver. These `NETWORK_PIVOT` pseudo-observations propagate the
phase S-basis across the connected network; disconnected receivers or
satellites are reported in the trace and are not silently treated as defined.

Only ambiguities on `baseline_phase_observables` are passed to Ginan's
configured integer estimator. Accepted integer constraints are applied through
the existing Kalman pseudo-observation update. Because satellite clocks and
phase-bias states retain their cross-covariance with the ambiguities, this
update produces the ambiguity-fixed clock/bias state rather than merely
printing rounded ambiguities.

## Recommended staged runs

1. Float baseline network: estimate GPS L1/L2 satellite clocks, receiver and
   satellite code/phase biases, STEC and continuous-arc ambiguities using fixed
   orbit and station coordinates.
2. Baseline AR and phase clock: enable ambiguity resolution and the controller.
   Only L1W/L2W ambiguities define the ambiguity-fixed satellite clock.
3. Additional frequencies: in a later implementation, hold or tightly
   constrain the stage-2 clock and estimate L5 phase OSBs. L5 ambiguities must
   not define the baseline clock.

The present controller implements the datum and baseline-selection mechanisms
required by stages 1-2. Stage 3 still requires a separate run/control block.

## Diagnostics

With `output_diagnostics: true`, the network trace contains:

- `CODE_DATUM_CLOSURE`: \(\alpha D_1-\beta D_2\), in metres;
- `CLOCK_BIAS_CLOSURE`:
  \(-c\,\delta t^s+\alpha D_1-\beta D_2\), compared before and after the
  ambiguity-fixing update at the same epoch. The minus sign follows Ginan's
  satellite-clock design coefficient in `ppp_obs.cpp`; `ar_delta_m` is the
  closure statistic;
- `AMBIGUITY_CLOSURE scope=NETWORK_FLOAT`: RMS fractional-cycle residual and
  the fractions within 0.15 and 0.25 cycles for baseline network ambiguities
  entering AR;
- `FREQUENCY_CLOSURE`: baseline phase OSBs transformed to wide-/narrow-lane
  length biases and reconstructed back to the single-frequency OSBs.

Bias-SINEX headers also include the satellite-clock reference observables,
baseline phase observables, phase reference receiver, datum identifier,
solution ID and configured offline discontinuity counter.

`FREQUENCY_CLOSURE` is an internal algebra/units check. It is not a substitute
for comparing the OSB-derived wide-/narrow-lane biases with a separately
estimated FCB/UPD product; that comparison remains part of independent product
validation.

Likewise, the in-process ambiguity statistic is not the required independent
user closure test. The latter must be computed from an excluded station after
applying the exported clock, code OSBs and phase OSBs together.

## Product consistency

The clock file and Bias-SINEX file from a controlled run are one product set.
Users must not combine the phase OSBs with a clock generated before ambiguity
fixing or with code OSBs in another datum. External validation should apply the
clock, code OSBs and phase OSBs together at independent stations and report
wide-lane/narrow-lane fractional residuals, fixed rate, false-fix controls,
coordinate accuracy and convergence time.
