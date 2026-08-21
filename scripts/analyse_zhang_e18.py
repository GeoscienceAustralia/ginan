#!/usr/bin/env python3
"""Audit E18 factor capture and retained-target whitening traces.

This analyzer deliberately does not label innovation tests as held-out replay.
Receiver/epoch exclusion requires a separate factor replay with those factors
removed before estimation.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import re
from pathlib import Path


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def scalar_fields(line: str) -> dict[str, str]:
    fields = dict(re.findall(r"([A-Za-z0-9_]+)=([^ ]+)", line))
    timestamp = re.search(r"(?:^| )time=(.*?)(?= [A-Za-z0-9_]+=)", line)
    if timestamp:
        fields["time"] = timestamp.group(1)
    return fields


def window_summary(line: str, residual_prefix: str) -> dict[str, object]:
    fields = scalar_fields(line)
    return {
        "time": fields.get("time"),
        "requested_targets": int(fields.get("requested_targets", "0")),
        "information_rank": int(fields.get("information_rank", "0")),
        "unresolved_gauge_rank": int(fields.get("unresolved_gauge_rank", "0")),
        "quotient_valid_rank": int(fields.get("quotient_valid_rank", "0")),
        "absolute_valid_rank": int(fields.get("absolute_valid_rank", "0")),
        "orthogonal_residual_dof": int(
            fields.get(f"{residual_prefix}orthogonal_residual_dof", "0")
        ),
        "orthogonal_residual_squared_norm": float(
            fields.get(f"{residual_prefix}orthogonal_residual_squared_norm", "nan")
        ),
    }


def autocorrelation(values: list[float], lag: int) -> float:
    if lag <= 0 or lag >= len(values):
        return math.nan
    mean = sum(values) / len(values)
    denominator = sum((value - mean) ** 2 for value in values)
    if denominator == 0:
        return math.nan
    numerator = sum(
        (values[index] - mean) * (values[index - lag] - mean)
        for index in range(lag, len(values))
    )
    return numerator / denominator


def ljung_box(values: list[float], maximum_lag: int) -> dict[str, object]:
    maximum_lag = min(maximum_lag, max(0, len(values) - 1))
    acf = {
        str(lag): autocorrelation(values, lag)
        for lag in range(1, maximum_lag + 1)
    }
    statistic = 0.0
    for lag, correlation in ((int(key), value) for key, value in acf.items()):
        if math.isfinite(correlation):
            statistic += correlation * correlation / (len(values) - lag)
    statistic *= len(values) * (len(values) + 2)
    cdf = chi2_cdf(statistic, maximum_lag) if maximum_lag else None
    return {
        "lags": maximum_lag,
        "acf": acf,
        "q": statistic,
        "cdf": cdf,
        "p_value": None if cdf is None else 1.0 - cdf,
    }


def fit_ar_residual_model(values: list[float], maximum_order: int = 10) -> dict[str, object]:
    """Fit a small shadow-only AR model and report its innovations.

    The original target residuals remain authoritative.  This model prevents
    correlated samples from being advertised as independent chi-square dof;
    it is not fed back into the estimator.
    """
    if len(values) < 8:
        return {"valid": False, "reason": "INSUFFICIENT_RESIDUALS"}
    try:
        import numpy as np  # type: ignore
    except Exception:
        return {"valid": False, "reason": "NUMPY_UNAVAILABLE"}
    centred = np.asarray(values, dtype=float) - float(np.mean(values))
    candidates: list[tuple[bool, float, int, object, object]] = []
    for order in range(0, min(maximum_order, len(values) // 4) + 1):
        if order == 0:
            innovations = centred.copy()
            coefficients = np.empty(0)
        else:
            design = np.column_stack(
                [centred[order - lag - 1 : -lag - 1] for lag in range(order)]
            )
            response = centred[order:]
            coefficients, _, _, _ = np.linalg.lstsq(design, response, rcond=None)
            innovations = response - design @ coefficients
        squared_norm = float(innovations @ innovations)
        sample_count = int(innovations.size)
        if sample_count <= order + 1 or squared_norm <= 0:
            continue
        bic = sample_count * math.log(squared_norm / sample_count) \
            + order * math.log(sample_count)
        whiteness = ljung_box([float(value) for value in innovations], 10)
        p_value = whiteness["p_value"]
        candidates.append((
            p_value is not None and float(p_value) >= 0.05,
            bic, order, coefficients, innovations,
        ))
    if not candidates:
        return {"valid": False, "reason": "AR_FIT_FAILED"}
    whitened_candidates = [candidate for candidate in candidates if candidate[0]]
    selected = min(
        whitened_candidates or candidates,
        key=lambda candidate: candidate[1],
    )
    whiteness_achieved, bic, order, coefficients, innovations = selected
    innovation_values = [float(value) for value in innovations]
    adjusted_dof = max(0, len(innovation_values) - order - 1)
    squared_norm = sum(value * value for value in innovation_values)
    innovation_ljung_box = ljung_box(innovation_values, 10)
    return {
        "valid": True,
        "whiteness_achieved": whiteness_achieved,
        "order": order,
        "coefficients": [float(value) for value in coefficients],
        "bic": bic,
        "correlation_adjusted_dof": adjusted_dof,
        "innovation_count": len(innovation_values),
        "innovation_squared_norm": squared_norm,
        "innovation_variance": (
            squared_norm / adjusted_dof if adjusted_dof else None
        ),
        "innovation_chi_square_cdf": (
            chi2_cdf(squared_norm, adjusted_dof) if adjusted_dof else None
        ),
        "innovation_ljung_box": innovation_ljung_box,
    }


def chi2_cdf(value: float, degrees_of_freedom: int) -> float | None:
    try:
        from scipy.stats import chi2  # type: ignore

        return float(chi2.cdf(value, degrees_of_freedom))
    except Exception:
        if value < 0 or degrees_of_freedom <= 0:
            return None
        # Regularized lower incomplete gamma P(k/2, x/2).  This fallback
        # keeps the audit reproducible when scipy is unavailable.
        shape = 0.5 * degrees_of_freedom
        argument = 0.5 * value
        epsilon = 1e-14
        tiny = 1e-300
        if argument == 0:
            return 0.0
        if argument < shape + 1:
            term = 1.0 / shape
            total = term
            current = shape
            for _ in range(10000):
                current += 1.0
                term *= argument / current
                total += term
                if abs(term) <= abs(total) * epsilon:
                    break
            return min(
                1.0,
                max(
                    0.0,
                    total
                    * math.exp(-argument + shape * math.log(argument)
                               - math.lgamma(shape)),
                ),
            )
        b = argument + 1.0 - shape
        c = 1.0 / tiny
        d = 1.0 / max(tiny, b)
        fraction = d
        for index in range(1, 10001):
            coefficient = -index * (index - shape)
            b += 2.0
            d = coefficient * d + b
            if abs(d) < tiny:
                d = tiny
            c = b + coefficient / c
            if abs(c) < tiny:
                c = tiny
            d = 1.0 / d
            delta = d * c
            fraction *= delta
            if abs(delta - 1.0) <= epsilon:
                break
        upper = math.exp(
            -argument + shape * math.log(argument) - math.lgamma(shape)
        ) * fraction
        return min(1.0, max(0.0, 1.0 - upper))


def nearest_integer_error_probability(fractional_mean: float, variance: float) -> float:
    if variance < 0 or not math.isfinite(variance):
        return math.nan
    if variance == 0:
        return 0.0 if abs(fractional_mean) <= 0.5 else 1.0
    sigma = math.sqrt(variance)
    root_two = math.sqrt(2.0)

    def normal_cdf(value: float) -> float:
        return 0.5 * (1.0 + math.erf(value / root_two))

    success = normal_cdf((0.5 - fractional_mean) / sigma) - normal_cdf(
        (-0.5 - fractional_mean) / sigma
    )
    return min(1.0, max(0.0, 1.0 - success))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--log", type=Path, required=True)
    parser.add_argument("--trace", type=Path, required=True)
    parser.add_argument("--products", type=Path, required=True)
    parser.add_argument("--covariance", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    arguments = parser.parse_args()

    log_lines = arguments.log.read_text(errors="replace").splitlines()
    capture_lines = [
        line for line in log_lines if "ZHANG_E18_FACTOR_CAPTURE" in line
    ]
    measurement_lines = [
        line for line in capture_lines if "event=MEASUREMENT" in line
    ]
    transform_lines = [
        line for line in capture_lines
        if "event=EXACT_COORDINATE_TRANSFORM" in line
    ]
    rejected_capture = [
        line for line in capture_lines
        if "status=RESET" not in line
        and (
            "status=REJECTED" in line
            or re.search(r"failure_reason=(?!NONE)", line)
        )
    ]

    latest_target_by_epoch: dict[str, dict[str, str]] = {}
    target_lines = []
    canonical_target_snapshots: dict[str, dict[str, dict[str, str]]] = {}
    raw_square_root_window_lines = []
    persistent_raw_target_window_lines = []
    target_increment_window_lines = []
    legacy_raw_window_lines = []
    integer_diagnostic_lines = []
    target_information_comparison_lines = []
    canonical_target_set_lines = []
    innovation_scale_group_lines = []
    with arguments.trace.open(errors="replace") as stream:
        for line in stream:
            if "ZHANG_E18_CANONICAL_TARGET_SET" in line:
                canonical_target_set_lines.append(line)
            if "ZHANG_E18_RAW_SQUARE_ROOT_WINDOW" in line:
                raw_square_root_window_lines.append(line)
            elif "ZHANG_E19_PERSISTENT_RAW_TARGET_WINDOW" in line:
                persistent_raw_target_window_lines.append(line)
            elif "ZHANG_E18_INCREMENTAL_INTEGER_WINDOW" in line:
                target_increment_window_lines.append(line)
            elif "ZHANG_E18_RAW_INTEGER_DATUM_WINDOW" in line:
                legacy_raw_window_lines.append(line)
            if "ZHANG_E18_INTEGER_DIAGNOSTIC" in line:
                integer_diagnostic_lines.append(line)
            if "ZHANG_E19_TARGET_INFORMATION_COMPARISON" in line:
                target_information_comparison_lines.append(line)
            if "ZHANG_E19_INNOVATION_SCALE_GROUP" in line:
                innovation_scale_group_lines.append(line)
            if (
                "ZHANG_E18_INTEGER_DATUM_TARGET" not in line
                and "ZHANG_E18_PHYSICAL_WL_TARGET" not in line
            ):
                continue
            target_lines.append(line)
            match = re.search(r"time=(.*?) system=", line)
            if not match:
                continue
            epoch = match.group(1)
            fields = scalar_fields(line)
            if fields.get("status") == "REJECTED":
                continue
            anchor = fields.get("anchor", "")
            satellite = fields.get("satellite", "")
            ordered_pair = "->".join(sorted((anchor, satellite)))
            canonical_identity = fields.get(
                "canonical_coordinate_id",
                ":".join(
                    (
                        fields.get("system", "UNKNOWN"),
                        fields.get("target_family", "UNKNOWN"),
                        ordered_pair,
                    )
                ),
            )
            canonical_target_snapshots.setdefault(epoch, {})[
                canonical_identity
            ] = fields
            previous = latest_target_by_epoch.get(epoch)
            if previous is None or int(fields.get("retained_block_targets", "0")) >= int(
                previous.get("retained_block_targets", "0")
            ):
                latest_target_by_epoch[epoch] = fields

    residuals: list[float] = []
    valid_blocks = 0
    invalid_blocks = 0
    information_rank = 0
    residual_dof = 0
    projected_gauge_rank = 0
    residual_domains: set[str] = set()
    missing_residual_definition_blocks = 0
    squared_norm = 0.0
    invalid_reasons: dict[str, int] = {}
    for fields in latest_target_by_epoch.values():
        if "retained_block_valid" not in fields:
            continue
        if fields.get("retained_block_valid") != "1":
            invalid_blocks += 1
            reason = fields.get("retained_block_reason", "UNKNOWN")
            invalid_reasons[reason] = invalid_reasons.get(reason, 0) + 1
            continue
        valid_blocks += 1
        information_rank += int(fields["retained_block_rank"])
        if "retained_block_residual_dof" not in fields:
            missing_residual_definition_blocks += 1
        else:
            residual_dof += int(fields["retained_block_residual_dof"])
            projected_gauge_rank += int(
                fields.get("retained_block_projected_gauge_rank", "0")
            )
            residual_domains.add(
                fields.get("retained_block_residual_domain", "UNDECLARED")
            )
        squared_norm += float(fields["retained_block_whitened_squared_norm"])
        encoded = fields.get("retained_block_whitened_residuals", "NONE")
        if encoded != "NONE":
            residuals.extend(float(value) for value in encoded.split(";") if value)

    residual_ljung_box = ljung_box(residuals, 10)
    residual_ar_model = fit_ar_residual_model(residuals)
    whitened_cdf = (
        chi2_cdf(squared_norm, residual_dof)
        if residual_dof and not missing_residual_definition_blocks
        else None
    )

    final_measurement = scalar_fields(measurement_lines[-1]) if measurement_lines else {}

    ordered_epochs = sorted(canonical_target_snapshots)
    previous_snapshots: dict[str, dict[str, str]] = {}
    transitions_by_epoch: dict[str, list[dict[str, object]]] = {}
    for epoch in ordered_epochs:
        transitions = []
        for identity, current in canonical_target_snapshots[epoch].items():
            previous = previous_snapshots.get(identity)
            if previous is not None and "mean" in previous and "mean" in current:
                previous_mean = float(previous["mean"])
                current_mean = float(current["mean"])
                previous_candidate = round(previous_mean)
                current_candidate = round(current_mean)
                transitions.append(
                    {
                        "canonical_coordinate_id": identity,
                        "previous_mean": previous_mean,
                        "current_mean": current_mean,
                        "previous_candidate": previous_candidate,
                        "current_candidate": current_candidate,
                        "candidate_jump": current_candidate - previous_candidate,
                        "fractional_change": (
                            current_mean - current_candidate
                            - (previous_mean - previous_candidate)
                        ),
                        "previous_product_datum_id": previous.get(
                            "product_datum_id", "UNDECLARED"
                        ),
                        "current_product_datum_id": current.get(
                            "product_datum_id", "UNDECLARED"
                        ),
                        "product_datum_changed": previous.get(
                            "product_datum_id", "UNDECLARED"
                        )
                        != current.get("product_datum_id", "UNDECLARED"),
                        "previous_absolute_valid": previous.get(
                            "absolute_datum_valid", "0"
                        ),
                        "current_absolute_valid": current.get(
                            "absolute_datum_valid", "0"
                        ),
                    }
                )
            previous_snapshots[identity] = current
        transitions_by_epoch[epoch] = transitions

    structural_event_audit = []
    for sequence, line in enumerate(transform_lines, start=1):
        match = re.search(r"time=(.*?) event=EXACT_COORDINATE_TRANSFORM", line)
        epoch = match.group(1) if match else "UNKNOWN"
        if "tree exchange" in line:
            action = "TREE_EXCHANGE"
        elif "local phase-coordinate reinitialisation" in line:
            action = "LOCAL_REINITIALISATION"
        else:
            action = "OTHER_EXACT_TRANSFORM"
        status = scalar_fields(line).get("status", "UNKNOWN")
        physical_arc_reset = status == "RESET"
        # Candidate continuity is intentionally undefined across a true physical
        # arc/version boundary.  Only exact S-coordinate changes may be audited as
        # candidate-preserving events.
        transitions = (
            [] if physical_arc_reset else transitions_by_epoch.get(epoch, [])
        )
        structural_event_audit.append(
            {
                "sequence": sequence,
                "time": epoch,
                "action": action,
                "status": status,
                "physical_arc_reset": physical_arc_reset,
                "candidate_comparison": (
                    "NOT_APPLICABLE_PHYSICAL_ARC_RESET"
                    if physical_arc_reset
                    else "EVENT_ADJACENT_POSTERIOR"
                ),
                "target_transitions": transitions,
                "candidate_jump_count": sum(
                    int(transition["candidate_jump"] != 0)
                    for transition in transitions
                ),
                "product_datum_change_count": sum(
                    int(bool(transition["product_datum_changed"]))
                    for transition in transitions
                ),
                "maximum_absolute_fractional_change": max(
                    (
                        abs(float(transition["fractional_change"]))
                        for transition in transitions
                    ),
                    default=None,
                ),
            }
        )
    absolute_target_fields = [
        scalar_fields(line)
        for line in target_lines
        if "status=ACCEPTED_ABSOLUTE_DATUM" in line
    ]
    held_quotient_lines = [
        line for line in target_lines
        if "status=REJECTED" in line
        and "reason=PERSISTENT_QUOTIENT_FUNCTIONAL_NOT_TRANSPORTABLE" in line
    ]
    rejected_target_lines = [
        line for line in target_lines if "status=REJECTED" in line
    ]
    absolute_target_times = [
        match.group(1)
        for line in target_lines
        if "status=ACCEPTED_ABSOLUTE_DATUM" in line
        if (match := re.search(r"time=(.*?) system=", line))
    ]
    absolute_target_perr = [
        nearest_integer_error_probability(
            float(fields["mean"]) - round(float(fields["mean"])),
            float(fields["variance"]),
        )
        for fields in absolute_target_fields
        if "mean" in fields and "variance" in fields
    ]
    canonical_target_sets = [
        scalar_fields(line) for line in canonical_target_set_lines
    ]
    canonical_set_ids = sorted(
        {
            fields.get("canonical_set_id", "UNDECLARED")
            for fields in canonical_target_sets
        }
    )

    # The accepted H/R, F/Q, exact-S square-root boundary is authoritative.
    # Legacy and target-increment windows remain diagnostic-only.
    raw_window_lines = raw_square_root_window_lines or legacy_raw_window_lines
    quotient_blocks = []
    absolute_blocks = []
    quotient_perr: list[float] = []
    for line in raw_window_lines:
        fields = scalar_fields(line)
        if fields.get("quotient_valid") == "1":
            quotient_blocks.append(fields)
            encoded_mean = fields.get("target_fractional_mean", "NONE")
            encoded_variance = fields.get("target_variance_diagonal", "NONE")
            if encoded_mean != "NONE" and encoded_variance != "NONE":
                means = [float(value) for value in encoded_mean.split(";")]
                variances = [float(value) for value in encoded_variance.split(";")]
                quotient_perr.extend(
                    nearest_integer_error_probability(mean, variance)
                    for mean, variance in zip(means, variances)
                )
        if fields.get("absolute_datum_valid") == "1":
            absolute_blocks.append(fields)
    final_integer_window = (
        scalar_fields(raw_window_lines[-1]) if raw_window_lines else {}
    )
    incremental_orthogonal_dof = int(
        final_integer_window.get(
            "batch_orthogonal_residual_dof",
            final_integer_window.get("orthogonal_residual_dof", "0"),
        )
    )
    incremental_orthogonal_norm = float(
        final_integer_window.get(
            "batch_orthogonal_residual_squared_norm",
            final_integer_window.get("orthogonal_residual_squared_norm", "nan"),
        )
    )
    incremental_orthogonal_cdf = (
        chi2_cdf(incremental_orthogonal_norm, incremental_orthogonal_dof)
        if incremental_orthogonal_dof
        and math.isfinite(incremental_orthogonal_norm)
        else None
    )
    diagnostics_by_strategy: dict[str, list[dict[str, str]]] = {}
    for line in integer_diagnostic_lines:
        fields = scalar_fields(line)
        strategy = fields.get("strategy", "UNKNOWN")
        diagnostics_by_strategy.setdefault(strategy, []).append(fields)
    integer_diagnostics = {}
    for strategy, blocks in diagnostics_by_strategy.items():
        valid = [block for block in blocks if block.get("valid") == "1"]
        integer_diagnostics[strategy] = {
            "trace_lines": len(blocks),
            "valid_lines": len(valid),
            "rejected_lines": len(blocks) - len(valid),
            "maximum_quotient_valid_rank": max(
                (int(block.get("quotient_valid_rank", "0")) for block in valid),
                default=0,
            ),
            "maximum_absolute_valid_rank": max(
                (int(block.get("absolute_valid_rank", "0")) for block in valid),
                default=0,
            ),
            "maximum_product_relation_graph_rank": max(
                (
                    int(block.get("product_relation_graph_rank", "0"))
                    for block in valid
                ),
                default=0,
            ),
            "maximum_conditional_direction_pass_count": max(
                (
                    int(block.get("conditional_direction_pass_count", "0"))
                    for block in valid
                ),
                default=0,
            ),
            "maximum_recoverable_satellite_count": max(
                (
                    int(block.get("recoverable_satellite_count", "0"))
                    for block in valid
                ),
                default=0,
            ),
            "maximum_joint_bootstrapped_success_rate": max(
                (
                    float(block["joint_bootstrapped_success_rate"])
                    for block in valid
                    if "joint_bootstrapped_success_rate" in block
                    and math.isfinite(float(block["joint_bootstrapped_success_rate"]))
                ),
                default=None,
            ),
            "lambda_validation_passes": sum(
                block.get("lambda_validation_pass") == "1" for block in valid
            ),
            "joint_reliability_passes": sum(
                block.get("joint_reliability_pass") == "1" for block in valid
            ),
            "final": blocks[-1],
        }
    target_information_comparisons = [
        scalar_fields(line) for line in target_information_comparison_lines
    ]
    valid_target_information_comparisons = [
        block for block in target_information_comparisons
        if block.get("valid") == "1"
    ]
    innovation_scale_snapshots: dict[str, list[dict[str, str]]] = {}
    for line in innovation_scale_group_lines:
        snapshot = scalar_fields(line)
        innovation_scale_snapshots.setdefault(
            snapshot.get("group", "UNKNOWN"), []
        ).append(snapshot)

    def innovation_scale_split(
        snapshots: list[dict[str, str]],
    ) -> dict | None:
        if len(snapshots) < 2:
            return None
        training = snapshots[-2]
        final = snapshots[-1]
        holdout_blocks = int(final.get("blocks", "0")) \
            - int(training.get("blocks", "0"))
        holdout_samples = int(final.get("marginal_samples", "0")) \
            - int(training.get("marginal_samples", "0"))
        holdout_sum = float(final.get(
            "marginal_standardised_squared_sum", "nan"
        )) - float(training.get(
            "marginal_standardised_squared_sum", "nan"
        ))
        return {
            "training": training,
            "holdout_increment": {
                "blocks": holdout_blocks,
                "marginal_samples": holdout_samples,
                "marginal_standardised_squared_sum": holdout_sum,
                "predictive_covariance_scale_mle": (
                    holdout_sum / holdout_samples
                    if holdout_samples > 0 else None
                ),
            },
        }
    result = {
        "factor_capture": {
            "measurement_events": len(measurement_lines),
            "exact_transform_events": len(transform_lines),
            "gps_tree_exchanges": sum(
                "label=Zhang graph GPS tree exchange" in line
                for line in transform_lines
            ),
            "local_reinitialisations": sum(
                "local phase-coordinate reinitialisation" in line
                for line in transform_lines
            ),
            "rejected_events": len(rejected_capture),
            "final_summary": final_measurement,
        },
        "physical_targets": {
            "trace_lines": len(target_lines),
            "epochs": len(latest_target_by_epoch),
            "accepted": sum("status=ACCEPTED" in line for line in target_lines),
            "accepted_integer_quotient": sum(
                "status=ACCEPTED_INTEGER_QUOTIENT" in line for line in target_lines
            ),
            "accepted_absolute_datum": sum(
                "status=ACCEPTED_ABSOLUTE_DATUM" in line for line in target_lines
            ),
            "first_absolute_datum_time": (
                absolute_target_times[0] if absolute_target_times else None
            ),
            "minimum_absolute_target_perr": (
                min(absolute_target_perr) if absolute_target_perr else None
            ),
            "rejected": sum("status=REJECTED" in line for line in target_lines),
            "held_persistent_quotient": len(held_quotient_lines),
            "hard_rejected": len(rejected_target_lines) - len(held_quotient_lines),
            "final_identity_resets": (
                int(scalar_fields(target_lines[-1]).get("physical_identity_resets", "0"))
                if target_lines else 0
            ),
            "final_coordinate_continuations": (
                int(scalar_fields(target_lines[-1]).get("coordinate_continuations", "0"))
                if target_lines else 0
            ),
            "maximum_replay_mean_relative_error": (
                float(scalar_fields(target_lines[-1])[
                    "target_mean_replay_relative_error"
                ])
                if target_lines
                and "target_mean_replay_relative_error"
                    in scalar_fields(target_lines[-1])
                else None
            ),
            "maximum_replay_variance_relative_error": (
                float(scalar_fields(target_lines[-1])[
                    "target_variance_replay_relative_error"
                ])
                if target_lines
                and "target_variance_replay_relative_error"
                    in scalar_fields(target_lines[-1])
                else None
            ),
        },
        "target_information_comparison": {
            "trace_lines": len(target_information_comparisons),
            "valid_lines": len(valid_target_information_comparisons),
            "maximum_common_target_count": max(
                (
                    int(block.get("common_target_count", "0"))
                    for block in valid_target_information_comparisons
                ),
                default=0,
            ),
            "final": (
                target_information_comparisons[-1]
                if target_information_comparisons else {}
            ),
        },
        "canonical_target_set": {
            "trace_lines": len(canonical_target_sets),
            "unique_set_ids": canonical_set_ids,
            "set_change_count": max(0, len(canonical_set_ids) - 1),
            "epochs_with_missing_relations": sum(
                fields.get("missing", "NONE") != "NONE"
                for fields in canonical_target_sets
            ),
            "epochs_with_ignored_substitutes": sum(
                fields.get("ignored_substitutes", "NONE") != "NONE"
                for fields in canonical_target_sets
            ),
            "silent_substitution_rejections": sum(
                int(fields.get("silent_substitution_rejected", "0"))
                for fields in canonical_target_sets
            ),
            "final": canonical_target_sets[-1] if canonical_target_sets else {},
        },
        "structural_event_candidate_audit": {
            "comparison_domain": "EVENT_ADJACENT_POSTERIOR",
            "includes_same_epoch_measurement_update": True,
            "events": structural_event_audit,
            "event_count": len(structural_event_audit),
            "tree_exchange_count": sum(
                event["action"] == "TREE_EXCHANGE"
                for event in structural_event_audit
            ),
            "local_reinitialisation_count": sum(
                event["action"] == "LOCAL_REINITIALISATION"
                for event in structural_event_audit
            ),
            "other_exact_transform_count": sum(
                event["action"] == "OTHER_EXACT_TRANSFORM"
                for event in structural_event_audit
            ),
            "events_without_target_transition": sum(
                not event["target_transitions"] for event in structural_event_audit
            ),
            "events_with_candidate_jump": sum(
                int(event["candidate_jump_count"]) > 0
                for event in structural_event_audit
            ),
            "events_with_product_datum_change": sum(
                int(event["product_datum_change_count"]) > 0
                for event in structural_event_audit
            ),
        },
        "retained_target_whitening": {
            "residual_domains": sorted(residual_domains),
            "valid_epoch_blocks": valid_blocks,
            "invalid_epoch_blocks": invalid_blocks,
            "invalid_reasons": invalid_reasons,
            "information_rank": information_rank,
            "residual_dof": residual_dof,
            "projected_gauge_rank": projected_gauge_rank,
            "missing_residual_definition_blocks": (
                missing_residual_definition_blocks
            ),
            "squared_norm": squared_norm,
            "chi_square_cdf": whitened_cdf,
            "residual_count": len(residuals),
            "declared_dof_matches_residual_count": residual_dof == len(residuals),
            "acf": residual_ljung_box["acf"],
            "ljung_box_lags": residual_ljung_box["lags"],
            "ljung_box_q": residual_ljung_box["q"],
            "ljung_box_cdf": residual_ljung_box["cdf"],
            "ljung_box_p_value": residual_ljung_box["p_value"],
            "ar_residual_model": residual_ar_model,
        },
        "raw_square_root_integer_datum_window": {
            "windows": [
                window_summary(line, "batch_") for line in raw_window_lines
            ],
            "trace_lines": len(raw_window_lines),
            "accepted": sum("status=ACCEPTED" in line for line in raw_window_lines),
            "rejected": sum("status=REJECTED" in line for line in raw_window_lines),
            "quotient_valid_blocks": len(quotient_blocks),
            "absolute_datum_valid_blocks": len(absolute_blocks),
            "first_absolute_datum_time": (
                absolute_blocks[0].get("time") if absolute_blocks else None
            ),
            "minimum_scalar_perr_diagnostic_only": (
                min(value for value in quotient_perr if math.isfinite(value))
                if any(math.isfinite(value) for value in quotient_perr)
                else None
            ),
            "maximum_quotient_valid_rank": max(
                (int(block.get("quotient_valid_rank", "0")) for block in quotient_blocks),
                default=0,
            ),
            "maximum_absolute_valid_rank": max(
                (int(block.get("absolute_valid_rank", "0")) for block in quotient_blocks),
                default=0,
            ),
            "maximum_information_rank": max(
                (int(block.get("information_rank", "0")) for block in quotient_blocks),
                default=0,
            ),
            "final_orthogonal_residual_dof": incremental_orthogonal_dof,
            "final_orthogonal_residual_squared_norm": incremental_orthogonal_norm,
            "final_orthogonal_chi_square_cdf": incremental_orthogonal_cdf,
            "final": final_integer_window,
        },
        "persistent_raw_target_window": {
            "authoritative_candidate": True,
            "feedback": 0,
            "windows": [
                window_summary(line, "batch_")
                for line in persistent_raw_target_window_lines
            ],
            "trace_lines": len(persistent_raw_target_window_lines),
            "accepted": sum(
                "status=ACCEPTED" in line
                for line in persistent_raw_target_window_lines
            ),
            "rejected": sum(
                "status=REJECTED" in line
                for line in persistent_raw_target_window_lines
            ),
            "maximum_target_count": max(
                (
                    int(scalar_fields(line).get("requested_targets", "0"))
                    for line in persistent_raw_target_window_lines
                ),
                default=0,
            ),
            "maximum_exact_constraints_applied": max(
                (
                    int(
                        scalar_fields(line).get(
                            "exact_constraints_applied", "0"
                        )
                    )
                    for line in persistent_raw_target_window_lines
                ),
                default=0,
            ),
            "final": (
                scalar_fields(persistent_raw_target_window_lines[-1])
                if persistent_raw_target_window_lines else {}
            ),
        },
        "innovation_scale_groups": {
            "statistic": "MARGINAL_PREFIT_RATIO_NOT_JOINT_CHI_SQUARE",
            "feedback": 0,
            "chi_square_gate_authorized": False,
            "trace_lines": len(innovation_scale_group_lines),
            "groups": {
                group: {
                    "snapshots": snapshots,
                    "final": snapshots[-1],
                    "training_holdout": innovation_scale_split(snapshots),
                }
                for group, snapshots in sorted(innovation_scale_snapshots.items())
            },
        },
        "target_increment_shortcut_diagnostic": {
            "authoritative": False,
            "windows": [
                window_summary(line, "")
                for line in target_increment_window_lines
            ],
            "trace_lines": len(target_increment_window_lines),
            "accepted": sum(
                "status=ACCEPTED" in line
                for line in target_increment_window_lines
            ),
            "rejected": sum(
                "status=REJECTED" in line
                for line in target_increment_window_lines
            ),
            "final": (
                scalar_fields(target_increment_window_lines[-1])
                if target_increment_window_lines else {}
            ),
        },
        "joint_integer_diagnostics": integer_diagnostics,
        "product_regression": {
            "products_sha256": sha256(arguments.products),
            "covariance_sha256": sha256(arguments.covariance),
        },
        "gate_boundary": {
            "feedback": 0,
            "held_out_epoch_replay_completed": False,
            "held_out_receiver_replay_completed": False,
            "four_tree_same_observation_replay_completed": False,
            "four_tree_deterministic_factor_gate_completed": True,
            "raw_factor_nuisance_elimination_completed": bool(
                raw_square_root_window_lines
                and all(
                    scalar_fields(line).get("valid") == "1"
                    for line in raw_square_root_window_lines
                )
            ),
            "persistent_raw_target_shadow_completed": bool(
                persistent_raw_target_window_lines
                and all(
                    scalar_fields(line).get("valid") == "1"
                    for line in persistent_raw_target_window_lines
                )
            ),
            "joint_integer_candidate_gate_completed": bool(
                diagnostics_by_strategy.get("RAW_SQUARE_ROOT_DIRECT_JOINT")
            ),
            "integer_perr_gate_completed": False,
            "downstream_feedback_authorized": False,
        },
    }
    arguments.output.parent.mkdir(parents=True, exist_ok=True)
    arguments.output.write_text(
        json.dumps(result, ensure_ascii=False, indent=2) + "\n"
    )
    print(json.dumps(result, ensure_ascii=False, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
