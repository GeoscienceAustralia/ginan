#!/usr/bin/env python3
"""Summarise Zhang canonical-integer, per-signal rank and event diagnostics."""

from __future__ import annotations

import argparse
import json
import math
import re
from collections import defaultdict
from pathlib import Path


KEY_VALUE = re.compile(r"([A-Za-z0-9_]+)=([^\s]+)")
TIME = re.compile(r"time=(\d{4}-\d{2}-\d{2})\s+(\d{2}:\d{2}:\d{2})")
RECORDS = {
    "ZHANG_AR_SUMMARY",
    "ZHANG_SIGNAL_HELD_RANK",
    "ZHANG_WIDE_LANE_HELD_RANK",
    "ZHANG_CANONICAL_INTEGER_AUDIT",
    "ZHANG_GRAPH_INTEGER_EVENT",
    "ZHANG_SATELLITE_INTEGER_LATTICE",
    "ZHANG_SATELLITE_PRODUCT_LATTICE",
    "ZHANG_SATELLITE_INTEGER_EDGE",
    "ZHANG_PRODUCT_DATUM_EVENT",
    "ZHANG_PRODUCT_DATUM_EDGE_EVENT",
    "ZHANG_PRODUCT_GRAPH_REDUNDANCY",
    "ZHANG_PRODUCT_TARGET_AR_RESULT",
    "ZHANG_SIGNAL_AR_RESULT",
    "ZHANG_HELD_LATTICE_EVENT",
    "ZHANG_HELD_LATTICE_NORMALISE",
    "ZHANG_HELD_LATTICE_STATUS",
    "ZHANG_LAYERED_AR_RESULT",
}


def parse(path: Path) -> dict[str, list[dict[str, str]]]:
    records: dict[str, list[dict[str, str]]] = defaultdict(list)
    with path.open(encoding="utf-8", errors="replace") as stream:
        for raw in stream:
            line = raw.strip()
            kind = line.split(maxsplit=1)[0] if line else ""
            if kind not in RECORDS:
                continue
            line = TIME.sub(r"time=\1T\2", line)
            records[kind].append(dict(KEY_VALUE.findall(line)))
    return records


def integer(record: dict[str, str] | None, field: str) -> int | None:
    if record is None or field not in record:
        return None
    value = record[field]
    if value in {
        "NOT_EVALUATED",
        "NOT_TRACKED",
        "NONE",
        "DEFERRED_TO_HELD_LATTICE_EVENT",
    }:
        return None
    return int(value)


def rank_summary(series: list[dict[str, object]]) -> dict[str, object]:
    ranks = [int(item["rank"]) for item in series]
    return {
        "epochs": len(series),
        "maximum": max(ranks, default=0),
        "ending": ranks[-1] if ranks else 0,
        "nonzero_epochs": sum(rank > 0 for rank in ranks),
    }


def analyse(path: Path, label: str) -> dict[str, object]:
    records = parse(path)
    total_by_time = {
        item["time"]: int(item["held_integer_rank"])
        for item in records["ZHANG_AR_SUMMARY"]
    }
    signal_by_time: dict[str, dict[str, int]] = defaultdict(dict)
    for item in records["ZHANG_SIGNAL_HELD_RANK"]:
        signal_by_time[item["time"]][item["observable"]] = int(
            item["held_integer_rank"]
        )
    wide_lane_by_time = {
        item["time"]: int(item["held_integer_rank"])
        for item in records["ZHANG_WIDE_LANE_HELD_RANK"]
    }
    graph_events = {
        (item["time"], item["event_type"]): item
        for item in records["ZHANG_GRAPH_INTEGER_EVENT"]
    }
    held_events = {
        (item["time"], item.get("event_id", "")): item
        for item in records["ZHANG_HELD_LATTICE_EVENT"]
    }
    exact_held_by_time = {
        item["time"]: int(item["exact_held_rank"])
        for item in records["ZHANG_HELD_LATTICE_STATUS"]
    }
    # A new fix is inserted after the pre-fix status at the same epoch.
    exact_held_by_time.update(
        {
            item["time"]: int(item["hnf_rows"])
            for item in records["ZHANG_HELD_LATTICE_NORMALISE"]
        }
    )

    times = sorted(set(total_by_time) | set(signal_by_time) | set(wide_lane_by_time))
    time_series = [
        {
            "time": time,
            "total": total_by_time.get(time),
            "signals": signal_by_time.get(time, {}),
            "wide_lane": wide_lane_by_time.get(time),
        }
        for time in times
    ]

    signal_series: dict[str, list[dict[str, object]]] = defaultdict(list)
    for time, values in signal_by_time.items():
        for observable, rank in values.items():
            signal_series[observable].append({"time": time, "rank": rank})
    total_series = [
        {"time": time, "rank": rank} for time, rank in sorted(total_by_time.items())
    ]
    wide_lane_series = [
        {"time": time, "rank": rank}
        for time, rank in sorted(wide_lane_by_time.items())
    ]

    # Canonical audits are emitted once per signal.  Collapse those duplicates
    # into a single graph event before calculating the post-event rank loss.
    event_keys: set[tuple[str, str, str, str]] = set()
    events: list[dict[str, object]] = []
    time_index = {time: index for index, time in enumerate(times)}
    for audit in records["ZHANG_CANONICAL_INTEGER_AUDIT"]:
        key = (
            audit["time"],
            audit["action"],
            audit.get("component_id", ""),
            audit.get("datum_version", ""),
        )
        if key in event_keys:
            continue
        event_keys.add(key)
        time = audit["time"]
        total = total_by_time.get(time)
        signals = signal_by_time.get(time, {})
        wide_lane = wide_lane_by_time.get(time)
        index = time_index.get(time, 0)
        previous_time = times[index - 1] if index > 0 else None
        previous_total = total_by_time.get(previous_time) if previous_time else None
        previous_signals = signal_by_time.get(previous_time, {}) if previous_time else {}
        previous_wide_lane = (
            wide_lane_by_time.get(previous_time) if previous_time else None
        )
        graph_event = graph_events.get((time, audit["action"]), {})
        held_event = held_events.get(
            (time, graph_event.get("event_id", "")), {}
        )
        events.append(
            {
                "time": time,
                "action": audit["action"],
                "valid": integer(audit, "valid"),
                "failure_reason": audit.get("reason"),
                "failure_detail": audit.get("detail"),
                "component_id": audit.get("component_id"),
                "datum_version": integer(audit, "datum_version"),
                "tree_datum_integers": integer(audit, "tree_datum_integers"),
                "cycle_integers": integer(audit, "cycle_integers"),
                "total_rank_before": previous_total,
                "total_rank": total,
                "total_rank_loss": (
                    previous_total - total
                    if previous_total is not None and total is not None
                    else None
                ),
                "signal_ranks": signals,
                "signal_ranks_before": previous_signals,
                "signal_rank_losses": {
                    observable: previous_signals[observable] - rank
                    for observable, rank in signals.items()
                    if observable in previous_signals
                },
                "wide_lane_rank": wide_lane,
                "wide_lane_rank_before": previous_wide_lane,
                "wide_lane_rank_loss": (
                    previous_wide_lane - wide_lane
                    if previous_wide_lane is not None and wide_lane is not None
                    else None
                ),
                "exact_epoch_transition": integer(audit, "exact_epoch_transition"),
                "affected_tree_edges": graph_event.get("affected_tree_edges"),
                "replacement_edges": graph_event.get("replacement_edges"),
                "local_reset_nodes": graph_event.get("local_reset_nodes"),
                "local_reset_satellites": graph_event.get("local_reset_satellites"),
                "removed_integer_columns": integer(
                    graph_event, "removed_integer_columns"
                ),
                "held_rows_touched": integer(held_event, "held_rows_touched"),
                "held_rows_removed": integer(held_event, "held_rows_removed"),
                "delete_touched_rows_rank": integer(
                    held_event, "delete_touched_rows_rank"
                ),
                "exact_surviving_lattice_rank": integer(
                    held_event, "exact_surviving_lattice_rank"
                ),
                "surviving_integer_nullity": integer(
                    held_event, "surviving_integer_nullity"
                ),
                "removed_arc_count": integer(held_event, "removed_arc_count"),
                "removed_arc_ids": held_event.get("removed_arc_ids"),
                "exact_held_rank_before": integer(
                    held_event, "held_rank_before"
                ),
                "exact_held_rank_after": integer(
                    held_event, "held_rank_after"
                ),
                "exact_unimodular_transform_available": integer(
                    graph_event, "exact_unimodular_transform_available"
                ),
                "held_lattice_storage": graph_event.get("held_lattice_storage"),
            }
        )

    lattice_records = records["ZHANG_SATELLITE_INTEGER_LATTICE"]
    valid_counts = [integer(item, "valid_satellite_count") or 0 for item in lattice_records]
    required_by_signal: dict[str, int] = {}
    coverage_by_signal: dict[str, list[dict[str, str]]] = defaultdict(list)
    for item in lattice_records:
        required = integer(item, "satellite_integer_rank_required")
        if required is not None:
            required_by_signal[item["observable"]] = required
        coverage_by_signal[item["observable"]].append(item)

    product_records = records["ZHANG_SATELLITE_PRODUCT_LATTICE"]
    product_by_target: dict[str, list[dict[str, str]]] = defaultdict(list)
    for item in product_records:
        product_by_target[item["target"]].append(item)
    datum_events = records["ZHANG_PRODUCT_DATUM_EVENT"]
    datum_edge_events = records["ZHANG_PRODUCT_DATUM_EDGE_EVENT"]
    redundancy_records = records["ZHANG_PRODUCT_GRAPH_REDUNDANCY"]
    integer_edges = records["ZHANG_SATELLITE_INTEGER_EDGE"]
    attribution_counts: dict[str, int] = defaultdict(int)
    attribution_epochs: dict[str, set[str]] = defaultdict(set)
    attribution_receivers: dict[str, int] = defaultdict(int)
    attribution_satellites: dict[str, int] = defaultdict(int)
    for item in datum_edge_events:
        reason = item.get("event_reason", "UNCLASSIFIED")
        attribution_counts[reason] += 1
        attribution_epochs[reason].add(item.get("time", "UNKNOWN"))
        attribution_receivers[item.get("receiver", "NONE")] += 1
        attribution_satellites[item.get("satellite", "NONE")] += 1
    edge_events_by_time: dict[str, list[dict[str, str]]] = defaultdict(list)
    for item in datum_edge_events:
        edge_events_by_time[item.get("time", "UNKNOWN")].append(item)
    reason_priority = [
        "CONFIRMED_CYCLE_SLIP",
        "STATION_QC_REMOVAL",
        "TEMPORARY_OBSERVATION_LOSS",
        "PRODUCT_EDGE_NO_ALTERNATIVE_SUPPORT",
        "COMPONENT_SPLIT",
        "COMPONENT_MERGE",
        "TREE_REOPTIMIZATION",
    ]
    exclusive_event_counts: dict[str, int] = defaultdict(int)
    for items in edge_events_by_time.values():
        if not any(integer(item, "datum_version_changed") == 1 for item in items):
            continue
        reasons = {item.get("event_reason", "UNCLASSIFIED") for item in items}
        selected = next(
            (reason for reason in reason_priority if reason in reasons),
            sorted(reasons)[0],
        )
        exclusive_event_counts[selected] += 1
    old_support_histogram: dict[int, int] = defaultdict(int)
    for item in datum_edge_events:
        if item.get("old_product_tree_edge") == "NONE":
            continue
        support = integer(item, "old_support_count")
        if support is not None:
            old_support_histogram[support] += 1

    wide_lane_records = records["ZHANG_WIDE_LANE_HELD_RANK"]
    signal_ar_fixed: dict[str, int] = defaultdict(int)
    for item in records["ZHANG_SIGNAL_AR_RESULT"]:
        signal_ar_fixed[item["observable"]] += int(item["fixed"])
    layered_fixed: dict[str, int] = defaultdict(int)
    layered_nonzero_epochs: dict[str, list[dict[str, object]]] = defaultdict(list)
    for item in records["ZHANG_LAYERED_AR_RESULT"]:
        fixed = int(item["fixed"])
        stage = item["stage"]
        layered_fixed[stage] += fixed
        if fixed:
            layered_nonzero_epochs[stage].append(
                {"time": item["time"], "fixed": fixed}
            )

    product_target_fixed: dict[str, int] = defaultdict(int)
    product_target_nonzero: dict[str, list[dict[str, object]]] = defaultdict(list)
    product_target_records: dict[str, list[dict[str, str]]] = defaultdict(list)
    for item in records["ZHANG_PRODUCT_TARGET_AR_RESULT"]:
        stage = item.get("stage", "UNKNOWN")
        fixed = integer(item, "fixed") or 0
        product_target_fixed[stage] += fixed
        product_target_records[stage].append(item)
        if fixed:
            product_target_nonzero[stage].append(
                {"time": item.get("time"), "fixed": fixed}
            )

    return {
        "label": label,
        "trace": str(path),
        "rank_metric": "POSTERIOR_COVARIANCE_THRESHOLD_NOT_INTEGER_LATTICE",
        "rank_summary": {
            "total": rank_summary(total_series),
            "signals": {
                observable: rank_summary(sorted(series, key=lambda item: str(item["time"])))
                for observable, series in sorted(signal_series.items())
            },
            "wide_lane": rank_summary(wide_lane_series),
        },
        "canonical_events": events,
        "canonical_component_count": len(
            {
                item.get("component_id")
                for item in records["ZHANG_CANONICAL_INTEGER_AUDIT"]
                if item.get("component_id")
            }
        ),
        "canonical_invalid_event_count": sum(
            integer(item, "valid") == 0
            for item in records["ZHANG_CANONICAL_INTEGER_AUDIT"]
        ),
        "common_arc_audit": {
            "records": len(wide_lane_records),
            "mapping": sorted(
                {item.get("mapping", "UNSPECIFIED") for item in wide_lane_records}
            ),
            "mismatch_records": sum(
                integer(item, "signal_1_only") not in {None, 0}
                or integer(item, "signal_2_only") not in {None, 0}
                for item in wide_lane_records
            ),
        },
        "independent_signal_ar_fixed_total": dict(sorted(signal_ar_fixed.items())),
        "layered_ar": {
            "fixed_total_by_stage": dict(sorted(layered_fixed.items())),
            "nonzero_epochs_by_stage": dict(sorted(layered_nonzero_epochs.items())),
        },
        "product_target_ar": {
            "fixed_total_by_stage": dict(sorted(product_target_fixed.items())),
            "nonzero_epochs_by_stage": dict(sorted(product_target_nonzero.items())),
            "by_stage": {
                stage: {
                    "records": len(series),
                    "maximum_candidates": max(
                        (integer(item, "candidates") or 0 for item in series),
                        default=0,
                    ),
                    "maximum_full_target_exact_rank": max(
                        (
                            integer(item, "full_target_exact_rank") or 0
                            for item in series
                        ),
                        default=0,
                    ),
                    "maximum_mappable_target_exact_rank": max(
                        (
                            integer(item, "mappable_target_exact_rank") or 0
                            for item in series
                        ),
                        default=0,
                    ),
                }
                for stage, series in sorted(product_target_records.items())
            },
        },
        "exact_held_lattice": {
            "epochs": len(exact_held_by_time),
            "maximum_rank": max(exact_held_by_time.values(), default=0),
            "ending_rank": (
                exact_held_by_time[sorted(exact_held_by_time)[-1]]
                if exact_held_by_time
                else 0
            ),
            "consistent": all(
                integer(item, "consistent") != 0
                for item in records["ZHANG_HELD_LATTICE_STATUS"]
                + records["ZHANG_HELD_LATTICE_NORMALISE"]
            ),
            "time_series": [
                {"time": time, "rank": rank}
                for time, rank in sorted(exact_held_by_time.items())
            ],
        },
        "satellite_lattice": {
            "records": len(lattice_records),
            "evaluated_records": sum(
                item.get("integer_lattice_containment") != "NOT_EVALUATED"
                for item in lattice_records
            ),
            "not_evaluated_records": sum(
                item.get("integer_lattice_containment") == "NOT_EVALUATED"
                for item in lattice_records
            ),
            "required_rank_by_signal": required_by_signal,
            "maximum_valid_satellite_count": max(valid_counts, default=0),
            "ending_valid_satellite_count": valid_counts[-1] if valid_counts else 0,
            "diagnostic_component_observed": any(count >= 2 for count in valid_counts),
            "nonzero_product_target_coverage_observed": any(
                (integer(item, "satellite_integer_rank_covered") or 0) > 0
                for item in lattice_records
            ),
            "production_gate_opened": any(
                item.get("gate") == "OPEN" for item in lattice_records
            ),
            "by_signal": {
                observable: {
                    "maximum_product_target_exact_rank": max(
                        (
                            integer(item, "product_target_exact_rank") or 0
                            for item in series
                        ),
                        default=0,
                    ),
                    "maximum_covered_rank": max(
                        (
                            integer(item, "satellite_integer_rank_covered") or 0
                            for item in series
                        ),
                        default=0,
                    ),
                    "maximum_component": max(
                        (integer(item, "largest_component") or 0 for item in series),
                        default=0,
                    ),
                    "ending_component": (
                        integer(series[-1], "largest_component") or 0
                        if series
                        else 0
                    ),
                    "nonzero_covered_epochs": sum(
                        (integer(item, "satellite_integer_rank_covered") or 0) > 0
                        for item in series
                    ),
                    "first_nonzero_covered_epoch": next(
                        (
                            item.get("time")
                            for item in series
                            if (integer(item, "satellite_integer_rank_covered") or 0) > 0
                        ),
                        None,
                    ),
                    "last_nonzero_covered_epoch": next(
                        (
                            item.get("time")
                            for item in reversed(series)
                            if (integer(item, "satellite_integer_rank_covered") or 0) > 0
                        ),
                        None,
                    ),
                }
                for observable, series in sorted(coverage_by_signal.items())
            },
            "wide_lane": {
                target: {
                    "records": len(series),
                    "maximum_product_target_exact_rank": max(
                        (
                            integer(item, "product_target_exact_rank") or 0
                            for item in series
                        ),
                        default=0,
                    ),
                    "maximum_covered_rank": max(
                        (
                            integer(item, "covered_satellite_rank") or 0
                            for item in series
                        ),
                        default=0,
                    ),
                    "maximum_component": max(
                        (integer(item, "largest_component") or 0 for item in series),
                        default=0,
                    ),
                    "nonzero_covered_epochs": sum(
                        (integer(item, "covered_satellite_rank") or 0) > 0
                        for item in series
                    ),
                }
                for target, series in sorted(product_by_target.items())
            },
            "integer_edge_count": len(integer_edges),
            "nonzero_integer_shift_count": sum(
                integer(item, "integer_shift") not in {None, 0}
                for item in integer_edges
            ),
        },
        "product_datum": {
            "events": len(datum_events),
            "version_changes": sum(
                index > 0
                and item.get("datum_version") != datum_events[index - 1].get(
                    "datum_version"
                )
                for index, item in enumerate(datum_events)
            ),
            "continuity_preserved_events": sum(
                integer(item, "continuity_preserved") == 1
                for item in datum_events
            ),
            "ending_version": (
                integer(datum_events[-1], "datum_version")
                if datum_events
                else None
            ),
            "edge_change_records": len(datum_edge_events),
            "edge_change_count_by_reason": dict(sorted(attribution_counts.items())),
            "event_epoch_count_by_reason": {
                reason: len(times)
                for reason, times in sorted(attribution_epochs.items())
            },
            "exclusive_version_break_count_by_primary_reason": dict(
                sorted(exclusive_event_counts.items())
            ),
            "bridge_before_edge_changes": sum(
                integer(item, "bridge_before") == 1
                for item in datum_edge_events
            ),
            "edge_changes_without_alternative_path": sum(
                (integer(item, "old_alternative_exact_paths") or 0) == 0
                for item in datum_edge_events
                if item.get("old_product_tree_edge") != "NONE"
            ),
            "minimum_old_support_count": min(
                (
                    integer(item, "old_support_count") or 0
                    for item in datum_edge_events
                    if item.get("old_product_tree_edge") != "NONE"
                ),
                default=0,
            ),
            "maximum_old_support_count": max(
                (
                    integer(item, "old_support_count") or 0
                    for item in datum_edge_events
                    if item.get("old_product_tree_edge") != "NONE"
                ),
                default=0,
            ),
            "old_support_count_histogram": {
                str(support): count
                for support, count in sorted(old_support_histogram.items())
            },
            "bridge_edge_changes": [
                {
                    "time": item.get("time"),
                    "edge": item.get("old_product_tree_edge"),
                    "reason": item.get("event_reason"),
                    "receiver": item.get("receiver"),
                    "satellite": item.get("satellite"),
                }
                for item in datum_edge_events
                if integer(item, "bridge_before") == 1
            ],
            "most_frequent_receivers": sorted(
                attribution_receivers.items(), key=lambda item: (-item[1], item[0])
            )[:10],
            "most_frequent_satellites": sorted(
                attribution_satellites.items(), key=lambda item: (-item[1], item[0])
            )[:10],
            "satellite_support_graph": {
                "records": len(redundancy_records),
                "minimum_relation_support": min(
                    (
                        integer(item, "min_support_count") or 0
                        for item in redundancy_records
                    ),
                    default=0,
                ),
                "maximum_relation_support": max(
                    (
                        integer(item, "max_support_count") or 0
                        for item in redundancy_records
                    ),
                    default=0,
                ),
                "minimum_edge_connectivity": min(
                    (
                        integer(item, "edge_connectivity") or 0
                        for item in redundancy_records
                    ),
                    default=0,
                ),
                "maximum_bridge_count": max(
                    (integer(item, "bridge_count") or 0 for item in redundancy_records),
                    default=0,
                ),
                "minimum_product_tree_support": min(
                    (
                        integer(item, "product_tree_min_support") or 0
                        for item in redundancy_records
                    ),
                    default=0,
                ),
                "maximum_product_tree_bridge_count": max(
                    (
                        integer(item, "product_tree_bridge_count") or 0
                        for item in redundancy_records
                    ),
                    default=0,
                ),
            },
        },
        "time_series": time_series,
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("trace", type=Path)
    parser.add_argument("--label", required=True)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    result = analyse(args.trace, args.label)
    text = json.dumps(result, indent=2, ensure_ascii=False, allow_nan=False)
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(text + "\n", encoding="utf-8")
    print(text)


if __name__ == "__main__":
    main()
