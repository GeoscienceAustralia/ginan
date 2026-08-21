# Stage B E20: internal Hou-style OSB-like PPP-RTK route

## Decision

The internal service no longer requires a conventional absolute satellite
integer datum before emitting a PPP-AR-capable phase product.  The operational
integer object is the full-rank network fundamental-cycle lattice.  The
satellite phase product is the directly estimated Hou-style coordinate in the
service datum.

`SATELLITE_TARGET_DATUM` remains available as a legacy/experimental mode, but
`HOU_OSB_LIKE` rejects `PRODUCT_TARGET_WL_L1` and opaque `JOINT` ambiguity
strategies at configuration time.  The permitted routes are
`INDEPENDENT_SIGNAL` and `LAYERED_WL_L1`.

## Product coordinate

For internal dynamic-tree coordinates `x_T`, the emitted phase correction is

```text
q_s,j(T) = C_s(T) - B_s,j(T) - lambda_j * alpha_s,j(T).
```

For an exact affine S-basis exchange

```text
x_T2 = A_T2,T1 x_T1 + b_T2,T1,
```

the complete phase-coordinate displacement, including both its integer and
fractional cycle parts, is accumulated in `alpha_s,j`.  Therefore a pure tree
exchange satisfies

```text
q_s,j(T2) = q_s,j(T1)
```

without changing the product discontinuity counter, datum version, IOD, or
stabilisation state.  Only a real satellite phase discontinuity or a physical
phase reinitialisation is allowed to create a new user product datum.

The user receiver retains one phase/ambiguity reference per constellation and
signal.  Consequently, the common unobservable phase datum is absorbed at the
user receiver; no original undifferenced satellite ambiguity, absolute
satellite integer, or satellite-only HNF anchor is required.

## Integer route and fail-closed gate

For GPS L1C/L2W, the network branch performs:

```text
full network float state
  -> common physical-arc WL cycle lattice
  -> LAMBDA/PAR at success probability >= 0.999
  -> transactional WL conditioning
  -> L1 cycle lattice in the WL-conditioned covariance
  -> transactional L1 conditioning and NIS check
  -> direct fixed phase-state product
```

A FIXED product is PPP-AR eligible only when all of the following hold:

1. the graph integer structure is valid;
2. both WL and WL-conditioned L1 stages commit in the same epoch;
3. the fixed branch passes transactional covariance and constraint-NIS checks;
4. the correction covariance is finite and its sigma does not exceed the
   configured 0.5 m gate;
5. the product has passed the configured two-epoch stabilisation interval;
6. the common-mode-removed temporal step does not exceed 0.5 m.

If the L1 subtransaction rolls back, that epoch is immediately PPP-only even
when WL remains fixed.  `persistent_relation_known` deliberately remains zero
in Hou mode and is not an integer-validity gate.

## Deterministic verification

Build target: `zhang_full_rank_tests` and `pea`.

Result:

```text
zhang_full_rank_tests: 1/1 passed
```

The added invariant test applies a -2.25-cycle fractional tree-coordinate
translation.  The compensated Hou product value is invariant within 1e-12,
while counter, datum version, IOD, and integer-valid state remain unchanged.

## Same-observation experiments

### E20 initial 12-minute smoke

- 13 epochs, 60 s interval, 74 network stations.
- About 514 correctly paired L1C/L2W WL cycle candidates per epoch.
- No WL subset reached the 0.999 success gate.
- No PPP-AR product was released.

This was a float-convergence limitation, not evidence for restoring an
absolute satellite integer datum.

### E20 one-hour diagnostic before the direct Hou gate

- First WL and L1 cycle fixes appeared at 00:20.
- WL and L1 both committed from 00:20 through 00:24.
- At 00:25 and later, WL continued to commit but L1 was rolled back by the
  constraint-NIS gate.
- The legacy satellite HNF coverage remained zero despite hundreds of held
  network-cycle rows.  This confirms that exact satellite-only lattice
  coverage is a different and unnecessarily stronger objective for this
  internal Hou product.

### E20b/E20c direct Hou product acceptance

The final 30-minute E20c replay used the same raw observations and unchanged
AR thresholds:

```text
epochs                                      31
CSV product records                       3720
PPP-AR usable records                      180
PPP-AR usable epochs                         3  (00:22, 00:23, 00:24)
records per eligible epoch                  60  (30 GPS satellites x 2 signals)
persistent_relation_known among all rows     0
early PPP-AR release before 00:22             0
PPP-AR release from 00:25 onward              0
forbidden Hou product-target AR entries       0
fixed-branch transaction aborts                0
```

The two-epoch difference between the first complete cycle fix at 00:20 and
the first released product at 00:22 is the configured stabilisation interval.

The one-hour E20b replay also contained a dynamic-tree event at 00:48.  Before
the Hou affine-coordinate fix, the same event produced artificial steps of
about 4.25 m on L1C and 8.17 m on L2W for G05/G28.  After the fix, the steps
were 0.14--0.23 m, passed the common-mode-removed continuity gate, and retained
counter = 0 and datum version = 0.

## Current acceptance status

The architecture and fail-closed routing are accepted for continued shadow
testing.  The service product is not yet accepted for feedback or user PPP-AR
validation because full dual-frequency network fixing remained available for
only three consecutive product epochs.  The next blocker is the persistent L1
constraint-NIS rejection after 00:25, not absolute satellite rank.

Next work must diagnose the L1 conditional constraint residual/covariance,
then repeat a three-hour shadow.  Only after stable WL+L1 availability,
tree-invariance replay, covariance whitening, and held-out prediction gates
pass should the route proceed to fixed feedback and the 20 independent user
stations.
