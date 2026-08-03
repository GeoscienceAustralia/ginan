#!/usr/bin/env python3
"""Audit the persistent Zhang satellite-product datum CSV and promotions."""

from __future__ import annotations

import argparse
import csv
import json
import re
from collections import Counter, defaultdict
from pathlib import Path


PROMOTION = re.compile(
    r"ZHANG_PRODUCT_RELATION_PROMOTION .*?time=(?P<time>\S+\s+\S+) "
    r"system=(?P<system>\S+) observable=(?P<observable>\S+) "
    r"satellite_a=(?P<a>\S+) satellite_b=(?P<b>\S+) "
    r"integer_difference=(?P<integer>-?\d+) status=(?P<status>\S+) "
    r"(?:event_type=(?P<event_type>\S+) "
    r"old_component_size_a=(?P<old_size_a>\d+) "
    r"old_component_size_b=(?P<old_size_b>\d+) "
    r"new_component_size=(?P<new_size>\d+) "
    r"(?:confirmation_count=(?P<confirmation_count>\d+) "
    r"confirmation_required=(?P<confirmation_required>\d+) "
    r"quarantined_satellite=(?P<quarantined_satellite>\S+) )?"
    r")?"
    r"provenance=(?P<provenance>\S+)"
)

TOPOLOGY_RESULT = re.compile(
    r"ZHANG_TOPOLOGY_TARGET_RESULT .*?time=(?P<time>\S+\s+\S+) "
    r"system=(?P<system>\S+) .*?status=(?P<status>\S+)"
)


def truth(row: dict[str, str], field: str) -> bool:
    return row.get(field, "0") == "1"


def analyse(products: Path, logs: list[Path] | None) -> dict[str, object]:
    with products.open(newline="", encoding="utf-8") as stream:
        rows = list(csv.DictReader(stream))
    fixed = [row for row in rows if row["solution"] == "FIXED"]
    epochs = sorted({int(row["gpst_seconds"]) for row in rows})

    fixed_by_epoch: dict[int, list[dict[str, str]]] = defaultdict(list)
    for row in fixed:
        fixed_by_epoch[int(row["gpst_seconds"])].append(row)

    component_series: list[dict[str, object]] = []
    for epoch in epochs:
        epoch_rows = fixed_by_epoch[epoch]
        components: dict[str, set[str]] = defaultdict(set)
        for row in epoch_rows:
            if truth(row, "integer_datum_continuous"):
                components[row["integer_component_id"]].add(row["satellite"])
        signal_components: dict[str, dict[str, set[str]]] = defaultdict(
            lambda: defaultdict(set)
        )
        valid_signal_components: dict[str, dict[str, set[str]]] = defaultdict(
            lambda: defaultdict(set)
        )
        for row in epoch_rows:
            if truth(row, "integer_datum_continuous"):
                signal_components[row["observable"]][
                    row["integer_component_id"]
                ].add(row["satellite"])
            if truth(row, "integer_valid"):
                valid_signal_components[row["observable"]][
                    row["integer_component_id"]
                ].add(row["satellite"])
        dual_components: dict[str, set[str]] = {}
        observables = sorted(signal_components)
        if len(observables) >= 2:
            first, second = observables[:2]
            for first_id, first_satellites in signal_components[first].items():
                for second_id, second_satellites in signal_components[second].items():
                    common = first_satellites & second_satellites
                    if len(common) >= 2:
                        dual_components[f"{first_id}|{second_id}"] = common
        valid_dual_components: dict[str, set[str]] = {}
        valid_observables = sorted(valid_signal_components)
        if len(valid_observables) >= 2:
            first, second = valid_observables[:2]
            for first_id, first_satellites in valid_signal_components[first].items():
                for second_id, second_satellites in valid_signal_components[second].items():
                    common = first_satellites & second_satellites
                    if len(common) >= 2:
                        valid_dual_components[
                            f"{first_id}|{second_id}"
                        ] = common
        component_series.append(
            {
                "epoch": epoch,
                "valid_satellite_signals": sum(
                    truth(row, "integer_valid") for row in epoch_rows
                ),
                "continuous_satellite_signals": sum(
                    truth(row, "integer_datum_continuous") for row in epoch_rows
                ),
                "component_sizes": {
                    component: len(satellites)
                    for component, satellites in sorted(components.items())
                },
                "dual_component_sizes": {
                    component: len(satellites)
                    for component, satellites in sorted(dual_components.items())
                },
                "valid_dual_component_sizes": {
                    component: len(satellites)
                    for component, satellites in sorted(
                        valid_dual_components.items()
                    )
                },
            }
        )

    longest: dict[str, dict[str, object]] = {}
    active: dict[str, list[int]] = defaultdict(list)
    for item in component_series:
        for component, size in item["component_sizes"].items():
            if size >= 2:
                active[component].append(int(item["epoch"]))
    interval = min(
        (right - left for left, right in zip(epochs, epochs[1:])),
        default=0,
    )
    for component, component_epochs in active.items():
        best: list[int] = []
        current: list[int] = []
        for epoch in component_epochs:
            if current and interval and epoch - current[-1] != interval:
                if len(current) > len(best):
                    best = current
                current = []
            current.append(epoch)
        if len(current) > len(best):
            best = current
        longest[component] = {
            "epochs": len(best),
            "start": best[0] if best else None,
            "end": best[-1] if best else None,
            "elapsed_minutes": (
                (best[-1] - best[0]) / 60 if len(best) >= 2 else 0
            ),
            "maximum_satellites": max(
                (
                    item["component_sizes"].get(component, 0)
                    for item in component_series
                ),
                default=0,
            ),
        }

    def longest_series(
        active_epochs: dict[str, list[int]],
        maximum_size: dict[str, int] | None = None,
    ) -> dict[str, dict[str, object]]:
        result: dict[str, dict[str, object]] = {}
        for key, key_epochs in active_epochs.items():
            best: list[int] = []
            current: list[int] = []
            for epoch in key_epochs:
                if current and interval and epoch - current[-1] != interval:
                    if len(current) > len(best):
                        best = current
                    current = []
                current.append(epoch)
            if len(current) > len(best):
                best = current
            result[key] = {
                "epochs": len(best),
                "start": best[0] if best else None,
                "end": best[-1] if best else None,
                "elapsed_minutes": (
                    (best[-1] - best[0]) / 60 if len(best) >= 2 else 0
                ),
            }
            if maximum_size is not None:
                result[key]["maximum_satellites"] = maximum_size.get(key, 0)
        return result

    dual_active: dict[str, list[int]] = defaultdict(list)
    dual_maximum: dict[str, int] = defaultdict(int)
    valid_dual_active: dict[str, list[int]] = defaultdict(list)
    valid_dual_maximum: dict[str, int] = defaultdict(int)
    for item in component_series:
        for component, size in item["dual_component_sizes"].items():
            dual_active[component].append(int(item["epoch"]))
            dual_maximum[component] = max(dual_maximum[component], size)
        for component, size in item["valid_dual_component_sizes"].items():
            valid_dual_active[component].append(int(item["epoch"]))
            valid_dual_maximum[component] = max(
                valid_dual_maximum[component], size
            )

    valid_satellite_signal_epochs: dict[str, list[int]] = defaultdict(list)
    for row in fixed:
        if truth(row, "integer_valid"):
            valid_satellite_signal_epochs[
                f'{row["observable"]}:{row["satellite"]}'
            ].append(int(row["gpst_seconds"]))

    promotions: list[dict[str, str]] = []
    topology_results: list[dict[str, str]] = []
    for log in logs or []:
        if not log.exists():
            continue
        for raw in log.read_text(encoding="utf-8", errors="replace").splitlines():
            match = PROMOTION.search(raw)
            if match:
                promotions.append(match.groupdict())
            topology_match = TOPOLOGY_RESULT.search(raw)
            if topology_match:
                topology_results.append(topology_match.groupdict())

    unique_relations = {
        (
            item["observable"],
            *sorted((item["a"], item["b"])),
        )
        for item in promotions
        if item["status"] == "ACCEPTED"
        and (
            not item.get("event_type")
            or item["event_type"] in {
                "NEW_COMPONENT_EDGE", "COMPONENT_MERGE"
            }
        )
    }

    versions = Counter(int(row["datum_version"]) for row in rows)
    counters = Counter(int(row["discontinuity_counter"]) for row in rows)
    shifts = Counter(int(row["integer_shift_cycles"]) for row in rows)
    alignment_states = Counter(
        row.get("current_alignment_state", "LEGACY_UNAVAILABLE")
        for row in rows
    )
    quarantined_satellites = Counter(
        item.get("quarantined_satellite")
        for item in promotions
        if item.get("event_type") == "CURRENT_ALIGNMENT_QUARANTINED"
        and item.get("quarantined_satellite")
        and item.get("quarantined_satellite") != "NONE"
    )
    return {
        "products": str(products),
        "rows": len(rows),
        "epochs": len(epochs),
        "structure_valid_rows": sum(
            truth(row, "integer_structure_valid") for row in rows
        ),
        "datum_continuous_rows": sum(
            truth(row, "integer_datum_continuous") for row in rows
        ),
        "precision_valid_rows": sum(
            truth(row, "integer_precision_valid") for row in rows
        ),
        "integer_valid_rows": sum(truth(row, "integer_valid") for row in rows),
        "datum_versions": dict(sorted(versions.items())),
        "discontinuity_counters": dict(sorted(counters.items())),
        "integer_shifts": dict(sorted(shifts.items())),
        "persistent_relation_known_rows": sum(
            truth(row, "persistent_relation_known") for row in rows
        ),
        "current_alignment_states": dict(sorted(alignment_states.items())),
        "nonzero_shift_rows": sum(shift != 0 for shift in map(
            lambda row: int(row["integer_shift_cycles"]), rows
        )),
        "promotion_count": len(promotions),
        "promotion_status": dict(Counter(item["status"] for item in promotions)),
        "promotion_event_type": dict(
            Counter(item.get("event_type") or "LEGACY_UNCLASSIFIED"
                    for item in promotions)
        ),
        "quarantined_satellite_events": dict(
            sorted(quarantined_satellites.items())
        ),
        "unique_quarantined_satellite_count": len(quarantined_satellites),
        "unique_topology_relation_count": len(unique_relations),
        "unique_topology_relations": [list(item) for item in sorted(unique_relations)],
        "promotion_provenance": dict(
            Counter(item["provenance"] for item in promotions)
        ),
        "topology_result_status": dict(
            Counter(item["status"] for item in topology_results)
        ),
        "maximum_dual_component_satellites": max(
            (
                max(item["dual_component_sizes"].values(), default=0)
                for item in component_series
            ),
            default=0,
        ),
        "maximum_valid_dual_component_satellites": max(
            (
                max(item["valid_dual_component_sizes"].values(), default=0)
                for item in component_series
            ),
            default=0,
        ),
        "longest_dual_components": longest_series(
            dual_active, dual_maximum
        ),
        "longest_valid_dual_components": longest_series(
            valid_dual_active, valid_dual_maximum
        ),
        "longest_integer_valid_by_satellite_signal": longest_series(
            valid_satellite_signal_epochs
        ),
        "longest_components": longest,
        "component_series": component_series,
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("products", type=Path)
    parser.add_argument(
        "--log", type=Path, action="append",
        help="repeat for the console log and TRACE file",
    )
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    result = analyse(args.products, args.log)
    text = json.dumps(result, indent=2, ensure_ascii=False)
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(text + "\n", encoding="utf-8")
    print(text)


if __name__ == "__main__":
    main()
