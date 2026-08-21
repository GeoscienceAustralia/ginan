#!/usr/bin/env python3
"""Audit E17 whitened WL fixed-lag shadow observations."""

from __future__ import annotations

import argparse
import json
import math
import re
from collections import Counter, defaultdict
from pathlib import Path


PAIR = re.compile(r"([A-Za-z_][A-Za-z0-9_]*)=([^\s]+)")


def fields(line: str) -> dict[str, str]:
    return dict(PAIR.findall(line))


def finite(item: dict[str, str], name: str) -> float | None:
    try:
        value = float(item[name])
    except (KeyError, ValueError):
        return None
    return value if math.isfinite(value) else None


def correlation(pairs: list[tuple[float, float]]) -> float | None:
    if len(pairs) < 3:
        return None
    left = [item[0] for item in pairs]
    right = [item[1] for item in pairs]
    mean_left = sum(left) / len(left)
    mean_right = sum(right) / len(right)
    covariance = sum(
        (x - mean_left) * (y - mean_right) for x, y in pairs
    )
    scale_left = sum((x - mean_left) ** 2 for x in left)
    scale_right = sum((y - mean_right) ** 2 for y in right)
    denominator = math.sqrt(scale_left * scale_right)
    return covariance / denominator if denominator > 0 else None


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("trace", type=Path)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--reliable-perr", type=float, default=1e-3)
    parser.add_argument("--maximum-absolute-lag1", type=float, default=0.2)
    args = parser.parse_args()

    accepted: list[dict[str, str]] = []
    rejection_reasons: Counter[str] = Counter()
    shadows: list[dict[str, str]] = []
    feedback_violations = 0

    with args.trace.open(encoding="utf-8", errors="replace") as stream:
        for line in stream:
            if "ZHANG_WL_WHITENED_OBSERVATION" in line:
                item = fields(line)
                feedback_violations += item.get("feedback") != "0"
                if item.get("status") == "ACCEPTED":
                    accepted.append(item)
                else:
                    rejection_reasons[item.get("reason", "UNKNOWN")] += 1
            elif "ZHANG_WL_FIXED_LAG_SHADOW" in line:
                item = fields(line)
                feedback_violations += item.get("feedback") != "0"
                shadows.append(item)

    residuals_by_relation: dict[str, list[float]] = defaultdict(list)
    relation_sequences: Counter[str] = Counter()
    for item in accepted:
        base_key = ":".join(
            item.get(name, "")
            for name in ("system", "anchor", "satellite")
        )
        if item.get("window_reset") == "1":
            relation_sequences[base_key] += 1
        residual = finite(item, "whitened_prediction_residual")
        if residual is None:
            continue
        key = f"{base_key}:sequence={relation_sequences[base_key]}"
        residuals_by_relation[key].append(residual)
    lag1_pairs = [
        (values[index - 1], values[index])
        for values in residuals_by_relation.values()
        for index in range(1, len(values))
    ]
    residuals = [value for values in residuals_by_relation.values() for value in values]
    lag1 = correlation(lag1_pairs)

    perrs = [
        value for item in shadows
        if (value := finite(item, "perr")) is not None
    ]
    reduced_chi_square = [
        value for item in shadows
        if (value := finite(item, "reduced_chi_square")) is not None
        and int(item.get("degrees_of_freedom", 0)) > 0
    ]
    resets = Counter(
        item.get("reset_reason", "UNKNOWN")
        for item in shadows if item.get("reset") == "1"
    )
    basis_transports = sum(
        item.get("basis_transport") == "1" for item in accepted
    )
    basis_switches = sum(
        item.get("basis_switch") == "1" for item in accepted
    )
    arc_version_conflicts = sum(
        item.get("arc_version_conflict") == "1" for item in accepted
    )
    prediction_rejections = rejection_reasons.get("PREDICTION_GATE", 0)
    prediction_total = len(accepted) + prediction_rejections
    reliable_rows = sum(value <= args.reliable_perr for value in perrs)
    residual_mean = sum(residuals) / len(residuals) if residuals else None
    residual_rms = (
        math.sqrt(sum(value * value for value in residuals) / len(residuals))
        if residuals else None
    )
    sorted_chi = sorted(reduced_chi_square)

    result = {
        "trace": str(args.trace),
        "observation_domain": {
            "source": "KALMAN_INNOVATION_LIKELIHOOD_RATIO",
            "accepted": len(accepted),
            "rejected": sum(rejection_reasons.values()),
            "rejection_reasons": dict(rejection_reasons),
            "prediction_gate_fraction": (
                prediction_rejections / prediction_total if prediction_total else 0
            ),
            "relations_with_whitened_residuals": len(residuals_by_relation),
            "whitened_residual_count": len(residuals),
            "whitened_residual_mean": residual_mean,
            "whitened_residual_rms": residual_rms,
            "lag1_pair_count": len(lag1_pairs),
            "lag1_correlation": lag1,
        },
        "physical_identity": {
            "basis_transports": basis_transports,
            "basis_switches": basis_switches,
            "arc_version_conflicts": arc_version_conflicts,
            "resets": dict(resets),
            "satellite_phase_segment_resets": resets.get(
                "SATELLITE_PHASE_SEGMENT_CHANGED", 0
            ),
        },
        "fixed_lag": {
            "rows": len(shadows),
            "maximum_observations": max(
                (int(item.get("observations", 0)) for item in shadows), default=0
            ),
            "minimum_perr": min(perrs, default=None),
            "reliable_perr_threshold": args.reliable_perr,
            "reliable_rows": reliable_rows,
            "median_reduced_chi_square": (
                sorted_chi[len(sorted_chi) // 2] if sorted_chi else None
            ),
            "maximum_reduced_chi_square": max(sorted_chi, default=None),
        },
        "feedback_violations": feedback_violations,
        "shadow_acceptance": {
            "reliable_wl_observed": reliable_rows > 0,
            "lag1_within_limit": (
                lag1 is not None and abs(lag1) <= args.maximum_absolute_lag1
            ),
            "maximum_absolute_lag1": args.maximum_absolute_lag1,
            "whitened_residual_scale_calibrated": (
                residual_rms is not None and 0.8 <= residual_rms <= 1.2
            ),
            "accepted_whitened_residual_rms_range": [0.8, 1.2],
            "physical_arc_versions_consistent_across_basis_transport": (
                arc_version_conflicts == 0
            ),
            "physical_arc_version_conflicts_absent": arc_version_conflicts == 0,
            "feedback_isolation": feedback_violations == 0,
        },
        "interpretation_boundary": (
            "The effective observations are exact scalar Kalman innovation "
            "likelihood ratios after nuisance elimination. Empirical lag-1 "
            "testing is required before treating epochs as independent."
        ),
    }
    rendered = json.dumps(result, ensure_ascii=False, indent=2)
    print(rendered)
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(rendered + "\n", encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
