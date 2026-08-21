# Independent leave-one-station PPP-AR validation

## Scope

This experiment validates whether the internal Zhang full-rank network
products restore held-out user ambiguities to an integer space before any
standard code/phase OSB, regional atmosphere model, SSR encoding, or NTRIP
transport is introduced.

The service solution and each user solution are separate PEA processes.  A
user reads only the CSV product stream and does not share network receiver
states, ambiguities, covariance, spanning-tree coordinates, or filter memory.

The implemented internal product contains, for every satellite and baseline
phase observable:

- the satellite clock state;
- the internal satellite phase state;
- their combined phase-observation correction;
- the complete clock/phase variance and covariance needed for the correction
  variance;
- phase discontinuity counter, integer and fractional shift, datum version,
  valid-from epoch, product IOD, reset reason, and integer-valid flag.

These are same-datum research products.  They are not Bias-SINEX OSBs and
cannot be combined with an unrelated clock or bias product.

## Phase continuity semantics

`ZhangPhaseContinuityState` maintains continuity per
`(constellation, satellite, observable)`.

1. An exact integer S-transform updates the integer branch without invalidating
   the integer datum.
2. An exact fractional S-transform increments the discontinuity counter,
   datum version, and IOD, invalidates the integer datum, and starts the
   configured stabilization interval.
3. A transformation that cannot be represented from existing states performs
   an explicit reinitialisation, resets branch shifts, increments continuity
   metadata, and keeps the integer-valid flag false until a complete integer
   datum is established again.

The held-out user maintains an independent ambiguity reference for every
signal.  A reference exchange applies an exact state and full-covariance
single-difference transform.  A product datum change resets only the affected
signal block or satellite ambiguity; no network graph state is imported.

## Integer-validity gate

Partial network ambiguity fixing does not prove that every satellite phase
product is integer calibrated.  Therefore a FIXED product is marked
`integer_valid=true` only when the held integer rank equals the complete
candidate cycle dimension at that epoch.  A few newly fixed cycles may improve
the network state, but they do not authorize user ambiguity fixing.

This conservative gate replaced an unsafe preliminary rule that marked all
satellite products valid whenever `newly_fixed > 0`.  The preliminary rule
created apparent 30--34% user fixing rates even though raw user ambiguities did
not cluster at integers.  Those rates are rejected and are not acceptance
results.

## Reproducible configuration

- date: 2019-07-18;
- interval: 00:00--06:00 GPST;
- sampling: 30 s, 721 epochs;
- observables: GPS L1C/L2W code and phase;
- service receivers: FFMJ, GANP, MARS, NOT1, PADO, ZIM2;
- held-out users: MATE (inside), DYNG (edge), NICO (outside);
- orbit: fixed WUM precise orbit;
- service ambiguity resolution: LAMBDA/PAR with fix-and-hold;
- user atmosphere: independently estimated STEC and troposphere; no regional
  atmospheric correction.

Configurations:

- `exampleConfigs/zhang_pppar_europe_network.yaml`;
- `exampleConfigs/zhang_pppar_europe_user.yaml`;
- `exampleConfigs/zhang_pppar_user_MATE.yaml`;
- `exampleConfigs/zhang_pppar_user_DYNG.yaml`;
- `exampleConfigs/zhang_pppar_user_NICO.yaml`;
- `exampleConfigs/zhang_pppar_product_float.yaml`.

The full strict run is reproduced by
`scripts/run_zhang_pppar_leave_one_out.sh`.  The common user configuration
must be loaded before the station overlay so every user writes to a distinct
output directory.

## Network result

The six-station service run completed normally:

- 721 processed epochs;
- 25,260 CSV records;
- 12,630 FLOAT and 12,630 FIXED records;
- zero FIXED records passed the complete-integer-datum gate.

The network did accept partial integer subsets at several epochs, but the held
integer rank never reached the full active cycle dimension.  Consequently all
FIXED satellite products correctly retained `integer_valid=false`.

## Independent user results

`FIXED_STRICT` means that the user applied the network post-feedback internal
clock/phase correction, but ambiguity resolution was permitted only when the
strict product integer-valid flag was true.

| User | Class | Product | AR attempts | TTFF | Raw integer-like rate | E RMS (m) | N RMS (m) | U RMS (m) | Horizontal P95 (m) | |U| P95 (m) |
|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|
| MATE | inside | FLOAT | 0 | -- | 0.0 | 0.0774 | 0.0590 | 0.3735 | 0.1880 | 0.9490 |
| MATE | inside | FIXED_STRICT | 0 | -- | 0.0 | 0.0775 | 0.0587 | 0.3726 | 0.1878 | 0.9470 |
| DYNG | edge | FLOAT | 0 | -- | 0.0 | 0.0608 | 0.1376 | 0.3888 | 0.1396 | 0.5717 |
| DYNG | edge | FIXED_STRICT | 0 | -- | 0.0 | 0.0608 | 0.1370 | 0.3878 | 0.1395 | 0.5716 |
| NICO | outside | FLOAT | 0 | -- | 0.0 | 0.3200 | 0.2159 | 0.5838 | 0.6988 | 1.1113 |
| NICO | outside | FIXED_STRICT | 0 | -- | 0.0 | 0.3199 | 0.2152 | 0.5834 | 0.6986 | 1.1113 |

Continuity events were propagated and handled:

- MATE: 10 product datum changes and 18 user reference changes;
- DYNG: 10 product datum changes and 16 user reference changes;
- NICO: 7 product datum changes and 18 user reference changes.

No wrong-fix rate or recovery-after-fix time can be reported because the
strict gate authorized no user fix.  This is not a missing statistic; it is
the scientifically correct outcome for this product set.

Machine-readable results are stored in `Docs/zhangPppArResults`.

## Acceptance decision

The software workflow passes:

- independent product serialization and exact-epoch ingestion;
- independent user filtering without state/covariance/tree sharing;
- combined clock/phase correction application;
- user ambiguity-reference state and covariance transformation;
- phase continuity counter and datum-reset propagation;
- FLOAT versus post-feedback FIXED product comparison;
- inside, edge, and outside user execution over a six-hour real-data arc.

The scientific PPP-AR acceptance criterion fails:

- no epoch established a complete network integer datum;
- all strict user ambiguity fix rates are zero;
- raw held-out user ambiguity fractional parts are not integer clustered.

Therefore the current implementation is an end-to-end PPP-AR validation
framework, but the current European product is not yet a usable integer
PPP-AR product.  Standard OSB, STEC-grid, SSR, and NTRIP work must remain
blocked.

The next technical task is not to weaken the gate.  It is to derive and track
which satellite phase differences are covered by the accepted cycle
constraints, then publish a per-satellite/per-signal integer-valid subgraph.
Only users whose satellite and reference belong to the same calibrated
integer component may enter LAMBDA/PAR.  This allows valid partial products
without falsely promoting the entire constellation.
