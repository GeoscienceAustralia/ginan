#!/usr/bin/env python3
"""Summarise E15 topology-candidate selection and temporal dependence.

The temporal statistics intentionally describe consecutive candidate posterior
values.  They are a guard against treating epochs as independent; they are not
a substitute for measurement-domain innovation diagnostics.
"""

from __future__ import annotations

import argparse
import json
import math
import re
from collections import Counter, defaultdict
from datetime import datetime
from pathlib import Path
from statistics import mean


PREFIX = "ZHANG_TOPOLOGY_CANDIDATE_POOL_ENTRY "
TIME_RE = re.compile(r"time=(\S+\s+\S+)")
FIELD_RE = re.compile(r"([A-Za-z0-9_]+)=([^\s]+)")


def round_error_probability(offset: float, variance: float) -> float:
    """Match GNSSambres.cpp round_perr()."""
    if variance < 1e-20:
        return 0.0
    exponent = -0.25 / variance
    wrong = 0.0
    for integer in range(1, 10):
        wrong += math.exp((integer + 2 * offset) * integer * exponent)
        wrong += math.exp((integer - 2 * offset) * integer * exponent)
    return wrong / (wrong + 1.0)


def correlation(pairs: list[tuple[float, float]]) -> float | None:
    if len(pairs) < 3:
        return None
    left = [item[0] for item in pairs]
    right = [item[1] for item in pairs]
    left_mean = mean(left)
    right_mean = mean(right)
    numerator = sum(
        (x - left_mean) * (y - right_mean) for x, y in pairs
    )
    left_power = sum((x - left_mean) ** 2 for x in left)
    right_power = sum((y - right_mean) ** 2 for y in right)
    denominator = math.sqrt(left_power * right_power)
    if denominator == 0:
        return None
    return numerator / denominator


def parse_trace(path: Path) -> list[dict[str, object]]:
    entries: list[dict[str, object]] = []
    with path.open("r", encoding="utf-8", errors="replace") as source:
        for line in source:
            marker = line.find(PREFIX)
            if marker < 0:
                continue
            record = line[marker + len(PREFIX):].strip()
            time_match = TIME_RE.search(record)
            if not time_match:
                continue
            fields = dict(FIELD_RE.findall(record))
            try:
                entries.append(
                    {
                        "time": datetime.fromisoformat(time_match.group(1)),
                        "system": fields["system"],
                        "datum_version": int(fields["product_datum_version"]),
                        "raw_rank": int(fields["raw_rank"]),
                        "independent_rank": int(fields["independent_rank"]),
                        "topology_unique": fields["topology_unique"] == "1",
                        "selected": fields["selected"] == "1",
                        "type": fields["type"],
                        "topology_key": fields["topology_key"],
                        "anchor": fields["anchor"],
                        "satellite": fields["satellite"],
                        "component_gain": int(fields["component_gain"]),
                        "wl_variance": float(fields["wl_variance"]),
                        "conditional_l1_variance": float(
                            fields["conditional_l1_variance"]
                        ),
                        "wl_float": float(fields["wl_float"]),
                        "wl_fractional": float(fields["wl_fractional"]),
                        "l1_float": float(fields["l1_float"]),
                        "l1_fractional": float(fields["l1_fractional"]),
                        "physical_support": int(fields["physical_support"]),
                        "score": float(fields["score"]),
                    }
                )
            except (KeyError, ValueError):
                continue
    return entries


def summarise(
    entries: list[dict[str, object]],
    epoch_interval: int,
    overall_success: float,
    ratio_threshold: float,
    maximum_targets: int,
) -> dict[str, object]:
    per_candidate_error = 1 - overall_success ** (1 / maximum_targets)
    fractional_limit = 1 / (ratio_threshold + 1)
    epochs: dict[datetime, list[dict[str, object]]] = defaultdict(list)
    series: dict[tuple[str, str, str, str], list[dict[str, object]]] = (
        defaultdict(list)
    )
    datum_by_epoch: dict[datetime, int] = {}
    type_counts: Counter[str] = Counter()
    reliable_unselected: list[dict[str, object]] = []
    reliable_unselected_epochs: set[datetime] = set()
    reliable_unselected_relations: set[tuple[str, str, str, str]] = set()
    deterministic_unselected = 0

    for entry in entries:
        epoch = entry["time"]
        assert isinstance(epoch, datetime)
        epochs[epoch].append(entry)
        datum_by_epoch[epoch] = int(entry["datum_version"])
        type_counts[str(entry["type"])] += 1
        if bool(entry["topology_unique"]):
            key = (
                str(entry["type"]),
                str(entry["topology_key"]),
                str(entry["anchor"]),
                str(entry["satellite"]),
            )
            series[key].append(entry)
        offset = float(entry["wl_fractional"])
        variance = float(entry["wl_variance"])
        perr = round_error_probability(offset, variance)
        if (
            bool(entry["topology_unique"])
            and not bool(entry["selected"])
            and abs(offset) < fractional_limit
            and perr < per_candidate_error
        ):
            reliable_unselected_epochs.add(epoch)
            reliable_unselected_relations.add(
                (
                    str(entry["type"]),
                    str(entry["topology_key"]),
                    str(entry["anchor"]),
                    str(entry["satellite"]),
                )
            )
            if variance <= 1e-12 and abs(offset) <= 1e-8:
                deterministic_unselected += 1
            reliable_unselected.append(
                {
                    "time": epoch.isoformat(sep=" "),
                    "type": entry["type"],
                    "topology_key": entry["topology_key"],
                    "anchor": entry["anchor"],
                    "satellite": entry["satellite"],
                    "independent_rank": entry["independent_rank"],
                    "wl_fractional": offset,
                    "wl_variance": variance,
                    "round_error_probability": perr,
                }
            )

    sorted_epochs = sorted(datum_by_epoch)
    datum_changes = sum(
        datum_by_epoch[current] != datum_by_epoch[previous]
        for previous, current in zip(sorted_epochs, sorted_epochs[1:])
    )
    pool_sizes = [len(value) for value in epochs.values()]
    independent_sizes = [
        sum(bool(item["topology_unique"]) for item in value)
        for value in epochs.values()
    ]
    selected_sizes = [
        sum(bool(item["selected"]) for item in value)
        for value in epochs.values()
    ]

    temporal: list[dict[str, object]] = []
    all_pairs: list[tuple[float, float]] = []
    for key, values in series.items():
        values.sort(key=lambda item: item["time"])
        pairs: list[tuple[float, float]] = []
        for previous, current in zip(values, values[1:]):
            delta = (
                current["time"] - previous["time"]
            ).total_seconds()
            if delta == epoch_interval:
                pairs.append(
                    (
                        float(previous["wl_fractional"]),
                        float(current["wl_fractional"]),
                    )
                )
        rho = correlation(pairs)
        all_pairs.extend(pairs)
        if rho is None or len(pairs) < 5:
            continue
        sample_count = len(pairs) + 1
        effective = sample_count * (1 - rho) / (1 + rho)
        effective = max(1.0, min(float(sample_count), effective))
        temporal.append(
            {
                "candidate": ":".join(key),
                "consecutive_samples": sample_count,
                "lag1_fractional_correlation": rho,
                "ar1_effective_sample_size": effective,
            }
        )

    temporal.sort(
        key=lambda item: int(item["consecutive_samples"]), reverse=True
    )
    aggregate_rho = correlation(all_pairs)

    return {
        "trace_entries": len(entries),
        "epochs_with_candidate_pool": len(epochs),
        "first_epoch": sorted_epochs[0].isoformat(sep=" ") if sorted_epochs else None,
        "last_epoch": sorted_epochs[-1].isoformat(sep=" ") if sorted_epochs else None,
        "datum_version_changes_within_candidate_epochs": datum_changes,
        "candidate_type_counts": dict(type_counts),
        "pool_size": {
            "minimum": min(pool_sizes, default=0),
            "maximum": max(pool_sizes, default=0),
            "mean": mean(pool_sizes) if pool_sizes else 0,
        },
        "independent_pool_size": {
            "minimum": min(independent_sizes, default=0),
            "maximum": max(independent_sizes, default=0),
            "mean": mean(independent_sizes) if independent_sizes else 0,
        },
        "selected_pool_size": {
            "minimum": min(selected_sizes, default=0),
            "maximum": max(selected_sizes, default=0),
            "mean": mean(selected_sizes) if selected_sizes else 0,
        },
        "round_gate": {
            "overall_success": overall_success,
            "maximum_targets": maximum_targets,
            "per_candidate_error_limit": per_candidate_error,
            "fractional_limit": fractional_limit,
        },
        "reliable_unique_candidates_omitted_by_top3_count": len(
            reliable_unselected
        ),
        "epochs_with_reliable_candidate_omitted_by_top3": len(
            reliable_unselected_epochs
        ),
        "reliable_relations_omitted_by_top3": [
            ":".join(item) for item in sorted(reliable_unselected_relations)
        ],
        "deterministic_candidate_entries_omitted_by_top3": (
            deterministic_unselected
        ),
        "reliable_unique_candidates_omitted_by_top3_examples": (
            reliable_unselected[:50]
        ),
        "posterior_temporal_dependence": {
            "warning": (
                "Candidate posteriors reuse filter information; these "
                "statistics diagnose dependence and must not be used as "
                "measurement-domain covariance."
            ),
            "aggregate_lag1_fractional_correlation": aggregate_rho,
            "longest_candidate_series": temporal[:50],
        },
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("trace", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("--epoch-interval", type=int, default=60)
    parser.add_argument("--overall-success", type=float, default=0.999)
    parser.add_argument("--ratio-threshold", type=float, default=3.0)
    parser.add_argument("--maximum-targets", type=int, default=3)
    args = parser.parse_args()

    summary = summarise(
        parse_trace(args.trace),
        args.epoch_interval,
        args.overall_success,
        args.ratio_threshold,
        args.maximum_targets,
    )
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(
        json.dumps(summary, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    print(json.dumps(summary, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()
