#!/usr/bin/env python3
"""Audit the one-hour HYBRID_PRODUCT_WL_L1 evidence run.

The report deliberately distinguishes three ranks:

* current single-frequency conditioning rank;
* the physical-row rank retained by the product ledger;
* the graph rank of named edges certified in both WL and L1 coordinates.

Only the last quantity is evidence for a dual-frequency product certificate.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import math
import re
from collections import Counter, defaultdict
from pathlib import Path


FIELD = re.compile(r"([A-Za-z0-9_]+)=([^\s]+)")
TRACE_TIME = re.compile(
    r"\btime=(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}(?:\.\d+)?)"
)


def fields(line: str) -> dict[str, str]:
    result = dict(FIELD.findall(line))
    match = TRACE_TIME.search(line)
    if match:
        result["time"] = match.group(1)
    return result


def integer(row: dict[str, str], key: str, default: int = 0) -> int:
    try:
        return int(float(row.get(key, str(default))))
    except (TypeError, ValueError):
        return default


def number(row: dict[str, str], key: str) -> float:
    try:
        return float(row.get(key, "nan"))
    except (TypeError, ValueError):
        return math.nan


def canonical_edge(row: dict[str, str]) -> tuple[str, str, int] | None:
    first = row.get("first", "")
    second = row.get("second", "")
    if not first or not second or first == second:
        return None
    value = integer(row, "integer")
    if first < second:
        return first, second, value
    return second, first, -value


def graph_rank(edges: set[tuple[str, str]]) -> int:
    parent: dict[str, str] = {}

    def root(node: str) -> str:
        parent.setdefault(node, node)
        while parent[node] != node:
            parent[node] = parent[parent[node]]
            node = parent[node]
        return node

    rank = 0
    for first, second in sorted(edges):
        left = root(first)
        right = root(second)
        if left == right:
            continue
        parent[right] = left
        rank += 1
    return rank


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(8 * 1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("output_dir", type=Path)
    parser.add_argument("--json", type=Path)
    parser.add_argument("--run-log", type=Path)
    parser.add_argument("--expected-epoch-count", type=int, default=121)
    parser.add_argument(
        "--expected-last-gpst-seconds", type=int, default=1405213200
    )
    args = parser.parse_args()

    traces = sorted(args.output_dir.glob("*.TRACE"))
    product_path = args.output_dir / "zhang_internal_products.csv"
    covariance_path = (
        args.output_dir / "zhang_internal_product_covariance.csv"
    )
    if len(traces) != 1 or not product_path.exists():
        raise SystemExit("expected one TRACE and zhang_internal_products.csv")

    stages: list[dict[str, str]] = []
    blocks: list[dict[str, str]] = []
    branches: list[dict[str, str]] = []
    l1_alternatives: list[dict[str, str]] = []
    pair_guided_l1: list[dict[str, str]] = []
    named_pair_forest_searches: list[dict[str, str]] = []
    current_certified_pairs: list[dict[str, str]] = []
    ledger_admissions: list[dict[str, str]] = []
    ledger_admission_rejects: list[dict[str, str]] = []
    ledger_pair_conflicts: list[dict[str, str]] = []
    ledger_segment_rejects: list[dict[str, str]] = []
    ledger_presearches: list[dict[str, str]] = []
    ledger_transports: list[dict[str, str]] = []
    post_ledger_graphs: list[dict[str, str]] = []
    ledger_updates: list[dict[str, str]] = []
    ledger_pairs_by_time: dict[str, list[dict[str, str]]] = defaultdict(list)
    effect_rows: list[dict[str, str]] = []
    authority_rows: list[dict[str, str]] = []
    relation_rows: list[dict[str, str]] = []
    functional_suspends: list[dict[str, str]] = []
    warning_counts: Counter[str] = Counter()

    with traces[0].open(encoding="utf-8", errors="replace") as stream:
        for raw in stream:
            line = raw.rstrip("\n")
            if line.startswith("ZHANG_PRODUCT_LATTICE_STAGE "):
                stages.append(fields(line))
            elif line.startswith("ZHANG_PRODUCT_LATTICE_BLOCK "):
                blocks.append(fields(line))
            elif line.startswith("ZHANG_PRODUCT_FIXED_BRANCH "):
                branches.append(fields(line))
            elif line.startswith("ZHANG_PRODUCT_LATTICE_L1_ALTERNATIVE "):
                l1_alternatives.append(fields(line))
            elif line.startswith("ZHANG_PRODUCT_LATTICE_PAIR_GUIDED_L1 "):
                pair_guided_l1.append(fields(line))
            elif line.startswith("ZHANG_PRODUCT_NAMED_PAIR_FOREST_SEARCH "):
                named_pair_forest_searches.append(fields(line))
            elif line.startswith("ZHANG_PRODUCT_LATTICE_CERTIFIED_PAIR "):
                current_certified_pairs.append(fields(line))
            elif line.startswith("ZHANG_PRODUCT_INTEGER_LEDGER_ADMISSION "):
                ledger_admissions.append(fields(line))
            elif line.startswith(
                "ZHANG_PRODUCT_INTEGER_LEDGER_ADMISSION_REJECT "
            ):
                ledger_admission_rejects.append(fields(line))
            elif line.startswith("ZHANG_PRODUCT_INTEGER_LEDGER_PAIR_CONFLICT "):
                ledger_pair_conflicts.append(fields(line))
            elif line.startswith("ZHANG_PRODUCT_INTEGER_LEDGER_SEGMENT_REJECT "):
                ledger_segment_rejects.append(fields(line))
            elif line.startswith("ZHANG_PRODUCT_INTEGER_LEDGER_PRESEARCH "):
                ledger_presearches.append(fields(line))
            elif line.startswith("ZHANG_PRODUCT_LATTICE_POST_LEDGER_GRAPH "):
                post_ledger_graphs.append(fields(line))
            elif line.startswith("ZHANG_PRODUCT_INTEGER_LEDGER_TRANSPORT "):
                ledger_transports.append(fields(line))
            elif line.startswith("ZHANG_PRODUCT_INTEGER_LEDGER "):
                ledger_updates.append(fields(line))
            elif line.startswith("ZHANG_PRODUCT_INTEGER_LEDGER_PAIR "):
                row = fields(line)
                ledger_pairs_by_time[row.get("time", "MISSING")].append(row)
            elif line.startswith("ZHANG_PRODUCT_FIXED_EFFECT "):
                effect_rows.append(fields(line))
            elif line.startswith("ZHANG_FLOAT_AUTHORITY_CLOSURE "):
                authority_rows.append(fields(line))
            elif line.startswith("ZHANG_PRODUCT_RELATION_IAR_SHADOW "):
                relation_rows.append(fields(line))
            elif line.startswith("ZHANG_HOU_PRODUCT_FUNCTIONAL_SUSPEND "):
                functional_suspends.append(fields(line))
            lower = line.lower()
            for term in (
                "fatal",
                "segmentation fault",
                "bad_alloc",
                "out of memory",
            ):
                if term in lower:
                    warning_counts[term] += 1

    input_warnings: dict[str, object] = {
        "run_log_present": bool(args.run_log and args.run_log.exists()),
        "spp_failure_count": 0,
        "spp_failure_by_station": {},
        "secondary_code_fallback_count": 0,
        "secondary_code_fallback_by_station": {},
        "apriori_spp_offset_count": 0,
        "maximum_apriori_spp_offset_m": math.nan,
        "maximum_apriori_spp_offset_station": "",
    }
    if args.run_log and args.run_log.exists():
        spp_by_station: Counter[str] = Counter()
        fallback_by_station: Counter[str] = Counter()
        apriori_offsets: list[tuple[float, str]] = []
        spp_pattern = re.compile(r"SPP failed for (\S+) at ")
        fallback_pattern = re.compile(
            r"secondary frequency for \S+ at (\S+), falling back"
        )
        apriori_pattern = re.compile(
            r"Apriori for (\S+) is ([0-9.eE+-]+)m from SPP estimate"
        )
        with args.run_log.open(encoding="utf-8", errors="replace") as stream:
            for line in stream:
                match = spp_pattern.search(line)
                if match:
                    spp_by_station[match.group(1)] += 1
                match = fallback_pattern.search(line)
                if match:
                    fallback_by_station[match.group(1)] += 1
                match = apriori_pattern.search(line)
                if match:
                    apriori_offsets.append((float(match.group(2)), match.group(1)))
        input_warnings = {
            "run_log_present": True,
            "spp_failure_count": sum(spp_by_station.values()),
            "spp_failure_by_station": dict(spp_by_station),
            "secondary_code_fallback_count": sum(fallback_by_station.values()),
            "secondary_code_fallback_by_station": dict(fallback_by_station),
            "apriori_spp_offset_count": len(apriori_offsets),
            "maximum_apriori_spp_offset_m": (
                max(apriori_offsets)[0] if apriori_offsets else math.nan
            ),
            "maximum_apriori_spp_offset_station": (
                max(apriori_offsets)[1] if apriori_offsets else ""
            ),
        }

    latest_pair_time = max(ledger_pairs_by_time, default="")
    latest_pair_rows = ledger_pairs_by_time.get(latest_pair_time, [])
    coordinate_edges: dict[str, dict[tuple[str, str], int]] = defaultdict(dict)
    coordinate_values: dict[
        str, dict[tuple[str, str], set[int]]
    ] = defaultdict(lambda: defaultdict(set))
    coordinate_conflicts: Counter[str] = Counter()
    for row in latest_pair_rows:
        edge = canonical_edge(row)
        if edge is None:
            continue
        first, second, value = edge
        coordinate = row.get("coordinate", "UNKNOWN")
        key = (first, second)
        coordinate_values[coordinate][key].add(value)
        previous = coordinate_edges[coordinate].get(key)
        if previous is not None and previous != value:
            coordinate_conflicts[coordinate] += 1
        coordinate_edges[coordinate][key] = value

    wl_edges = set(coordinate_edges.get("WL", {}))
    l1_edges = set(coordinate_edges.get("L1", {}))
    paired_edges = wl_edges & l1_edges

    generation_pair_edges: dict[
        tuple[str, str], set[tuple[str, str]]
    ] = defaultdict(set)
    current_pair_value_errors = 0
    for row in current_certified_pairs:
        first = row.get("first", "")
        second = row.get("second", "")
        generation = row.get("backend_generation", "MISSING")
        segment = row.get("phase_segment_fingerprint", "MISSING")
        if not first or not second or first == second:
            current_pair_value_errors += 1
            continue
        wl = integer(row, "wl_integer")
        l1 = integer(row, "l1_integer")
        l2 = integer(row, "l2_integer")
        if l2 != l1 - wl:
            current_pair_value_errors += 1
        edge = (first, second) if first < second else (second, first)
        generation_pair_edges[(generation, segment)].add(edge)

    ledger_generation_missing = sum(
        "backend_generation" not in row
        or "phase_segment_fingerprint" not in row
        for rows in ledger_pairs_by_time.values()
        for row in rows
    )

    product_epochs: set[int] = set()
    solution_counts: Counter[str] = Counter()
    integer_valid: Counter[str] = Counter()
    pppar_usable: Counter[str] = Counter()
    invalid_reasons: Counter[str] = Counter()
    last_epoch = 0
    with product_path.open(newline="", encoding="utf-8") as stream:
        for row in csv.DictReader(stream):
            epoch = int(row["gpst_seconds"])
            product_epochs.add(epoch)
            last_epoch = max(last_epoch, epoch)
            solution = row.get("solution", "MISSING")
            solution_counts[solution] += 1
            integer_valid[solution] += int(row.get("integer_valid", "0"))
            pppar_usable[solution] += int(row.get("pppar_usable", "0"))
            reason = row.get("invalid_reason", "")
            if reason:
                invalid_reasons[reason] += 1

    reliable_blocks = [row for row in blocks if integer(row, "reliable") == 1]
    rejected_branches = [row for row in branches if row.get("status") == "REJECTED"]
    rejection_streaks: list[int] = []
    current_rejection_streak = 0
    for row in branches:
        if row.get("status") == "REJECTED":
            current_rejection_streak += 1
        else:
            if current_rejection_streak:
                rejection_streaks.append(current_rejection_streak)
            current_rejection_streak = 0
    latest_rejection_streak = current_rejection_streak
    if current_rejection_streak:
        rejection_streaks.append(current_rejection_streak)
    tree_suspends = [
        row
        for row in functional_suspends
        if row.get("event_cause") == "TREE_REOPTIMIZATION"
    ]
    tree_suspend_epochs = Counter(row.get("time", "MISSING") for row in tree_suspends)
    rejected_epochs = {row.get("time") for row in rejected_branches}
    coincident_tree_rejections = sorted(
        epoch for epoch in rejected_epochs if tree_suspend_epochs.get(epoch, 0) > 0
    )
    combined_nis_ratios = [
        number(row, "combined_joint_nis")
        / number(row, "combined_joint_nis_threshold")
        for row in branches
        if math.isfinite(number(row, "combined_joint_nis"))
        and math.isfinite(number(row, "combined_joint_nis_threshold"))
        and number(row, "combined_joint_nis_threshold") > 0
    ]

    result = {
        "experiment": args.output_dir.name,
        "trace": str(traces[0]),
        "artifacts": {
            path.name: {
                "bytes": path.stat().st_size,
                "sha256": sha256_file(path),
            }
            for path in (traces[0], product_path, covariance_path)
            if path.exists()
        },
        "completion": {
            "product_epoch_count": len(product_epochs),
            "expected_epoch_count": args.expected_epoch_count,
            "last_gpst_seconds": last_epoch,
            "expected_last_gpst_seconds": args.expected_last_gpst_seconds,
            "natural_target_reached": (
                len(product_epochs) == args.expected_epoch_count
                and last_epoch == args.expected_last_gpst_seconds
            ),
            "covariance_csv_present": covariance_path.exists(),
        },
        "direct_product_lattice": {
            "block_records": len(blocks),
            "reliable_blocks": len(reliable_blocks),
            "maximum_wl_rank": max(
                (integer(row, "wl_rank") for row in blocks), default=0
            ),
            "maximum_conditional_l1_rank": max(
                (integer(row, "conditional_l1_rank") for row in blocks),
                default=0,
            ),
            "maximum_conditioning_rank": max(
                (integer(row, "conditioning_rank") for row in blocks),
                default=0,
            ),
            "maximum_certified_pair_rank": max(
                (integer(row, "certified_pair_rank") for row in blocks),
                default=0,
            ),
            "epochs_with_certified_pairs": [
                row.get("time")
                for row in blocks
                if integer(row, "certified_pair_rank") > 0
            ],
            "stage_status_counts": dict(
                Counter(row.get("status", "MISSING") for row in stages)
            ),
            "stage_selection_source_counts": dict(
                Counter(row.get("selection_source", "LEGACY_UNTRACED") for row in stages)
            ),
            "maximum_stage_certified_pair_rank": max(
                (integer(row, "certified_pair_rank") for row in stages),
                default=0,
            ),
            "selected_l1_source_counts": dict(
                Counter(row.get("l1_source", "LEGACY_UNTRACED") for row in blocks)
            ),
            "l1_alternative_records": len(l1_alternatives),
            "l1_alternative_source_counts": dict(
                Counter(row.get("source", "MISSING") for row in l1_alternatives)
            ),
            "pair_guided_l1_records": len(pair_guided_l1),
            "pair_guided_l1_reliable_records": sum(
                integer(row, "reliable") for row in pair_guided_l1
            ),
            "maximum_pair_guided_forest_rank": max(
                (integer(row, "wl_pair_forest_rank") for row in pair_guided_l1),
                default=0,
            ),
            "maximum_pair_guided_fixed_rank": max(
                (integer(row, "guided_fixed_rank") for row in pair_guided_l1),
                default=0,
            ),
            "named_pair_forest_search_records": len(named_pair_forest_searches),
            "named_pair_forest_reliable_records": sum(
                integer(row, "reliable") for row in named_pair_forest_searches
            ),
            "maximum_named_pair_forest_candidate_edges": max(
                (
                    integer(row, "candidate_edges")
                    or integer(row, "initial_rank")
                    for row in named_pair_forest_searches
                ),
                default=0,
            ),
            "maximum_named_pair_forest_generated_rank": max(
                (
                    integer(row, "maximum_generated_rank")
                    or integer(row, "initial_rank")
                    for row in named_pair_forest_searches
                ),
                default=0,
            ),
            "maximum_named_pair_forest_selected_rank": max(
                (integer(row, "selected_rank") for row in named_pair_forest_searches),
                default=0,
            ),
            "named_pair_forest_total_evaluated_branches": sum(
                integer(row, "evaluated_branches")
                for row in named_pair_forest_searches
            ),
            "precondition_exact_pair_records": len(current_certified_pairs),
            "precondition_pair_value_errors": current_pair_value_errors,
            "precondition_generation_graph_ranks": {
                f"generation={generation}|segment={segment}": graph_rank(edges)
                for (generation, segment), edges in sorted(
                    generation_pair_edges.items()
                )
            },
            "post_ledger_graph_records": len(post_ledger_graphs),
            "maximum_post_ledger_certified_pair_rank": max(
                (
                    integer(row, "certified_pair_rank")
                    for row in post_ledger_graphs
                ),
                default=0,
            ),
            "maximum_post_ledger_applied_conditioning_rank": max(
                (
                    integer(row, "applied_conditioning_rank")
                    for row in post_ledger_graphs
                ),
                default=0,
            ),
            "post_ledger_product_certified_records": sum(
                integer(row, "certified_for_product")
                for row in post_ledger_graphs
            ),
            "ledger_presearch_records": len(ledger_presearches),
            "ledger_presearch_applied_records": sum(
                integer(row, "applied") for row in ledger_presearches
            ),
            "maximum_ledger_presearch_exact_rank": max(
                (integer(row, "exact_rank") for row in ledger_presearches),
                default=0,
            ),
            "maximum_ledger_presearch_selected_rank": max(
                (integer(row, "selected_rank") for row in ledger_presearches),
                default=0,
            ),
            "ledger_presearch_status_counts": dict(
                Counter(row.get("status", "MISSING") for row in ledger_presearches)
            ),
            "post_ledger_status_counts": dict(
                Counter(row.get("status", "MISSING") for row in post_ledger_graphs)
            ),
            "relation_records": len(relation_rows),
            "latest_full_target_rank": (
                integer(relation_rows[-1], "full_target_rank")
                if relation_rows
                else 0
            ),
            "latest_mappable_target_rank": (
                integer(relation_rows[-1], "mappable_target_rank")
                if relation_rows
                else 0
            ),
            "latest_unmappable_target_rank": (
                integer(relation_rows[-1], "unmappable_target_rank")
                if relation_rows
                else 0
            ),
            "minimum_mappable_target_rank": min(
                (integer(row, "mappable_target_rank") for row in relation_rows),
                default=0,
            ),
            "maximum_mappable_target_rank": max(
                (integer(row, "mappable_target_rank") for row in relation_rows),
                default=0,
            ),
            "temporal_recovery_required_records": sum(
                integer(row, "temporal_recovery_required")
                for row in relation_rows
            ),
            "certified_for_product_records": sum(
                integer(row, "certified_for_product") for row in relation_rows
            ),
        },
        "private_branch": {
            "records": len(branches),
            "conditioned": sum(row.get("status") == "CONDITIONED" for row in branches),
            "rejected": len(rejected_branches),
            "rejection_reasons": dict(
                Counter(row.get("reason", "MISSING") for row in rejected_branches)
            ),
            "maximum_consecutive_rejections": max(rejection_streaks, default=0),
            "latest_consecutive_rejections": latest_rejection_streak,
            "maximum_combined_constraint_rank": max(
                (integer(row, "constraint_rank") for row in branches), default=0
            ),
            "maximum_ledger_projected_rank": max(
                (integer(row, "ledger_projected_rank") for row in branches),
                default=0,
            ),
            "maximum_ledger_selected_rank": max(
                (integer(row, "ledger_selected_rank") for row in branches),
                default=0,
            ),
            "total_ledger_rows_rejected_by_admission": sum(
                integer(row, "ledger_rejected_rows") for row in branches
            ),
            "maximum_ledger_selected_pair_rows": max(
                (integer(row, "ledger_selected_pair_rows") for row in branches),
                default=0,
            ),
            "maximum_combined_joint_nis_ratio": (
                max(combined_nis_ratios) if combined_nis_ratios else None
            ),
        },
        "ledger": {
            "update_records": len(ledger_updates),
            "maximum_active_rank": max(
                (integer(row, "active_rank_after") for row in ledger_updates),
                default=0,
            ),
            "latest_active_rank": (
                integer(ledger_updates[-1], "active_rank_after")
                if ledger_updates
                else 0
            ),
            "total_conflicting_rows": sum(
                integer(row, "conflicting_rows") for row in ledger_updates
            ),
            "latest_named_snapshot_time": latest_pair_time,
            "latest_named_snapshot_rows": len(latest_pair_rows),
            "named_edge_counts": {
                coordinate: len(edges)
                for coordinate, edges in sorted(coordinate_edges.items())
            },
            "named_graph_ranks": {
                coordinate: graph_rank(set(edges))
                for coordinate, edges in sorted(coordinate_edges.items())
            },
            "named_value_conflicts": dict(coordinate_conflicts),
            "named_value_conflict_details": {
                coordinate: [
                    {
                        "edge": list(edge),
                        "integers": sorted(values),
                    }
                    for edge, values in sorted(edges.items())
                    if len(values) > 1
                ]
                for coordinate, edges in sorted(coordinate_values.items())
                if any(len(values) > 1 for values in edges.values())
            },
            "paired_wl_l1_edge_count": len(paired_edges),
            "paired_wl_l1_graph_rank": graph_rank(paired_edges),
            "paired_wl_l1_edges": [list(edge) for edge in sorted(paired_edges)],
            "admission_records": len(ledger_admissions),
            "admission_reject_records": len(ledger_admission_rejects),
            "mandatory_current_pair_conflict_reject_records": len(
                ledger_pair_conflicts
            ),
            "mandatory_current_pair_conflict_details": ledger_pair_conflicts,
            "row_local_segment_reject_records": len(ledger_segment_rejects),
            "row_local_segment_reject_details": ledger_segment_rejects,
            "presearch_records": len(ledger_presearches),
            "presearch_applied_records": sum(
                integer(row, "applied") for row in ledger_presearches
            ),
            "presearch_current_segment_reject_rows": sum(
                integer(row, "segment_rejected_rows")
                for row in ledger_presearches
            ),
            "presearch_cross_generation_rows": sum(
                integer(row, "cross_generation_rows")
                for row in ledger_presearches
            ),
            "presearch_status_counts": dict(
                Counter(row.get("status", "MISSING") for row in ledger_presearches)
            ),
            "exact_physical_reprojection_records": len(ledger_transports),
            "exact_physical_reprojection_status_counts": dict(
                Counter(row.get("status", "MISSING") for row in ledger_transports)
            ),
            "exact_physical_reprojection_generation_pairs": dict(
                Counter(
                    "{}->{}".format(
                        row.get("from_backend_generation", "MISSING"),
                        row.get("to_backend_generation", "MISSING"),
                    )
                    for row in ledger_transports
                )
            ),
            "maximum_admission_selected_rank": max(
                (integer(row, "selected_rank") for row in ledger_admissions),
                default=0,
            ),
            "maximum_admission_rejected_rows": max(
                (integer(row, "rejected_rows") for row in ledger_admissions),
                default=0,
            ),
            "pair_rows_missing_generation_or_segment": ledger_generation_missing,
        },
        "tree_and_temporal": {
            "functional_suspend_records": len(functional_suspends),
            "tree_reoptimization_suspend_records": len(tree_suspends),
            "tree_reoptimization_suspend_epochs": dict(tree_suspend_epochs),
            "private_rejection_epochs_with_tree_reoptimization": (
                coincident_tree_rejections
            ),
        },
        "effect_and_authority": {
            "effect_records": len(effect_rows),
            "float_authority_checks": len(authority_rows),
            "float_authority_failures": sum(
                row.get("status") != "PASS" for row in authority_rows
            ),
        },
        "products": {
            "solution_counts": dict(solution_counts),
            "integer_valid_counts": dict(integer_valid),
            "pppar_usable_counts": dict(pppar_usable),
            "invalid_reason_counts": dict(invalid_reasons),
        },
        "input_warnings": input_warnings,
        "fatal_terms": dict(warning_counts),
        "interpretation_gates": {
            # TRACE pair snapshots currently do not include the physical arc and
            # backend-generation key.  Their graph rank is therefore a raw
            # upper bound only.  The strict offline Oracle builder must prove
            # generation compatibility and exact rank before readiness can be
            # asserted.
            "raw_dual_frequency_rank22": graph_rank(paired_edges) >= 22,
            "generation_metadata_complete": bool(current_certified_pairs)
            and current_pair_value_errors == 0
            and ledger_generation_missing == 0,
            "generation_compatibility_proven": False,
            "requires_strict_oracle_builder_verification": True,
            "full_oracle_ready": False,
        },
    }

    output = json.dumps(result, indent=2, ensure_ascii=False, allow_nan=True)
    print(output)
    if args.json:
        args.json.write_text(output + "\n", encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
