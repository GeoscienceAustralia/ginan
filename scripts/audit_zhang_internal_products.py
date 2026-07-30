#!/usr/bin/env python3
"""Audit a Zhang GPS clock/phase internal-product run.

The audit deliberately separates structural validity (network isolation, full
column rank, CSV schema, and complete covariance) from integer validity.  A run
may pass the former while still producing FLOAT-only products.
"""

from __future__ import annotations

import argparse
import csv
import json
import re
from collections import Counter, defaultdict
from pathlib import Path


RANK_RE = re.compile(
    r"ZHANG_PURE_OBS_RANK time=(?P<time>.*?) "
    r"rows=(?P<rows>\d+) active_cols=(?P<cols>\d+) "
    r"rank=(?P<rank>\d+) nullity=(?P<nullity>\d+).*?"
    r"full_column_rank=(?P<full>true|false)"
)
GRAPH_RE = re.compile(
    r"ZHANG_GRAPH_BASIS sys=(?P<system>\S+) action=(?P<action>\S+) "
    r"(?:(?:nodes=(?P<nodes>\d+) edges=(?P<edges>\d+))|"
    r"(?:modelled_edges=(?P<modelled_edges>\d+))) "
    r"tree_edges=(?P<tree_edges>\d+) cycles=(?P<cycles>\d+)"
)
AR_RE = re.compile(
    r"ZHANG_AR_SUMMARY time=(?P<time>.*?) "
    r"candidates=(?P<candidates>\d+) newly_fixed=(?P<newly_fixed>\d+) "
    r"held_integer_rank=(?P<held_rank>\d+).*?"
    r"adop_cycles=(?P<adop>[-+0-9.eE]+)"
)

REQUIRED_PRODUCT_COLUMNS = {
    "gpst_seconds",
    "solution",
    "satellite",
    "observable",
    "clock_m",
    "clock_sigma_m",
    "phase_m",
    "phase_sigma_m",
    "clock_phase_covariance_m2",
    "integer_valid",
    "integer_component_id",
    "integer_datum_id",
    "discontinuity_counter",
    "solution_interval_start_gpst_seconds",
    "solution_interval_end_gpst_seconds",
}


def read_sites(path: Path) -> set[str]:
    return {
        line.strip().upper()
        for line in path.read_text(encoding="utf-8").splitlines()
        if line.strip() and not line.lstrip().startswith("#")
    }


def parse_diagnostics(
    trace: Path, log: Path
) -> tuple[list[dict], list[dict], list[dict], Counter[str], int]:
    combined = trace.read_text(encoding="utf-8", errors="replace")
    combined += "\n" + log.read_text(encoding="utf-8", errors="replace")

    ranks = []
    for match in RANK_RE.finditer(combined):
        item = match.groupdict()
        ranks.append(
            {
                "time": item["time"],
                "rows": int(item["rows"]),
                "active_columns": int(item["cols"]),
                "rank": int(item["rank"]),
                "nullity": int(item["nullity"]),
                "full_column_rank": item["full"] == "true",
            }
        )

    graphs = []
    for match in GRAPH_RE.finditer(combined):
        item = match.groupdict()
        graphs.append(
            {
                key: int(value) if value and key not in {"system", "action"} else value
                for key, value in item.items()
                if value is not None
            }
        )

    ar = []
    for match in AR_RE.finditer(combined):
        item = match.groupdict()
        ar.append(
            {
                "time": item["time"],
                "candidates": int(item["candidates"]),
                "newly_fixed": int(item["newly_fixed"]),
                "held_integer_rank": int(item["held_rank"]),
                "adop_cycles": float(item["adop"]),
            }
        )

    graph_actions = Counter(
        re.findall(r"ZHANG_GRAPH_BASIS[^\n]* action=(\S+)", combined)
    )
    datum_discontinuities = combined.count("phase_datum_discontinuity=true")

    # Rank and AR records may be echoed to both TRACE and console log.
    ranks = list({json.dumps(item, sort_keys=True): item for item in ranks}.values())
    ar = list({json.dumps(item, sort_keys=True): item for item in ar}.values())
    return ranks, graphs, ar, graph_actions, datum_discontinuities


def parse_products(path: Path) -> dict:
    group_counts: Counter[tuple[str, str]] = Counter()
    satellites: defaultdict[tuple[str, str], set[str]] = defaultdict(set)
    observables: set[str] = set()
    integer_valid = 0
    unresolved = 0
    invalid_intervals = 0
    discontinuity_counters: list[int] = []
    datum_versions: list[int] = []
    reset_reasons: Counter[str] = Counter()

    with path.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        columns = set(reader.fieldnames or [])
        missing = sorted(REQUIRED_PRODUCT_COLUMNS - columns)
        rows = 0
        for row in reader:
            rows += 1
            group = (row["gpst_seconds"], row["solution"])
            group_counts[group] += 1
            satellites[group].add(row["satellite"])
            observables.add(row["observable"])
            integer_valid += row["integer_valid"].strip().lower() in {"1", "true"}
            unresolved += row["integer_component_id"] == "UNRESOLVED"
            invalid_intervals += (
                float(row["solution_interval_end_gpst_seconds"])
                < float(row["solution_interval_start_gpst_seconds"])
            )
            discontinuity_counters.append(int(row["discontinuity_counter"]))
            datum_versions.append(int(row["datum_version"]))
            reset_reasons[row["reset_reason"]] += 1

    return {
        "rows": rows,
        "columns": len(columns),
        "missing_required_columns": missing,
        "epochs": len({group[0] for group in group_counts}),
        "epoch_solution_groups": len(group_counts),
        "solutions": sorted({group[1] for group in group_counts}),
        "observables": sorted(observables),
        "rows_per_epoch_solution_min": min(group_counts.values(), default=0),
        "rows_per_epoch_solution_max": max(group_counts.values(), default=0),
        "satellites_per_epoch_solution_min": min(
            (len(values) for values in satellites.values()), default=0
        ),
        "satellites_per_epoch_solution_max": max(
            (len(values) for values in satellites.values()), default=0
        ),
        "integer_valid_rows": integer_valid,
        "unresolved_integer_component_rows": unresolved,
        "invalid_solution_intervals": invalid_intervals,
        "discontinuity_counter_min": min(discontinuity_counters, default=0),
        "discontinuity_counter_max": max(discontinuity_counters, default=0),
        "datum_version_min": min(datum_versions, default=0),
        "datum_version_max": max(datum_versions, default=0),
        "reset_reason_counts": dict(reset_reasons),
    }


def component(row: dict, prefix: str) -> tuple[str, str, str]:
    return (
        row[f"{prefix}_satellite"],
        row[f"{prefix}_parameter"],
        row[f"{prefix}_observable"],
    )


def parse_covariance(path: Path) -> dict:
    counts: Counter[tuple[str, str]] = Counter()
    components: defaultdict[tuple[str, str], set[tuple[str, str, str]]] = defaultdict(set)

    with path.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        rows = 0
        for row in reader:
            rows += 1
            group = (row["gpst_seconds"], row["solution"])
            counts[group] += 1
            components[group].add(component(row, "row"))
            components[group].add(component(row, "column"))

    complete = {}
    for group, count in sorted(counts.items()):
        dimension = len(components[group])
        expected = dimension * (dimension + 1) // 2
        complete[f"{group[0]}:{group[1]}"] = {
            "dimension": dimension,
            "rows": count,
            "expected_upper_triangle_rows": expected,
            "complete": count == expected,
        }

    return {
        "rows": rows,
        "groups": len(complete),
        "dimensions": sorted({item["dimension"] for item in complete.values()}),
        "rows_per_group_min": min(
            (item["rows"] for item in complete.values()), default=0
        ),
        "rows_per_group_max": max(
            (item["rows"] for item in complete.values()), default=0
        ),
        "incomplete_groups": [
            group for group, item in complete.items() if not item["complete"]
        ],
        "all_upper_triangles_complete": all(item["complete"] for item in complete.values()),
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--estimation-sites", type=Path, required=True)
    parser.add_argument("--validation-sites", type=Path, required=True)
    parser.add_argument("--trace", type=Path, required=True)
    parser.add_argument("--log", type=Path, required=True)
    parser.add_argument(
        "--rank-trace",
        type=Path,
        help="Optional strict-smoke TRACE used only for rank diagnostics",
    )
    parser.add_argument(
        "--rank-log",
        type=Path,
        help="Optional strict-smoke console log used only for rank diagnostics",
    )
    parser.add_argument("--products", type=Path, required=True)
    parser.add_argument("--covariance", type=Path, required=True)
    args = parser.parse_args()

    estimation = read_sites(args.estimation_sites)
    validation = read_sites(args.validation_sites)
    ranks, graphs, ar, graph_actions, datum_discontinuities = parse_diagnostics(
        args.trace, args.log
    )
    rank_scope = "product run"
    if args.rank_trace or args.rank_log:
        if not (args.rank_trace and args.rank_log):
            parser.error("--rank-trace and --rank-log must be supplied together")
        ranks, _, _, _, _ = parse_diagnostics(args.rank_trace, args.rank_log)
        rank_scope = "separate strict smoke run"
    products = parse_products(args.products)
    covariance = parse_covariance(args.covariance)

    structural_pass = (
        bool(ranks)
        and all(item["full_column_rank"] for item in ranks)
        and not (estimation & validation)
        and not products["missing_required_columns"]
        and products["invalid_solution_intervals"] == 0
        and covariance["all_upper_triangles_complete"]
    )
    integer_pass = (
        bool(ar)
        and any(item["held_integer_rank"] > 0 for item in ar)
        and products["integer_valid_rows"] > 0
    )

    report = {
        "verdict": {
            "structural_product_pass": structural_pass,
            "integer_product_pass": integer_pass,
            "scientific_status": (
                "integer-valid internal phase product"
                if integer_pass
                else "FLOAT internal clock/phase estimate only"
            ),
        },
        "network": {
            "estimation_sites": len(estimation),
            "validation_sites": len(validation),
            "intersection": sorted(estimation & validation),
        },
        "rank_diagnostics": ranks,
        "rank_diagnostic_scope": rank_scope,
        "graph_diagnostics": {
            "events": sum(graph_actions.values()),
            "action_counts": dict(graph_actions),
            "phase_datum_discontinuities": datum_discontinuities,
            "tree_edges_min": min(
                (item["tree_edges"] for item in graphs), default=0
            ),
            "tree_edges_max": max(
                (item["tree_edges"] for item in graphs), default=0
            ),
            "cycles_min": min((item["cycles"] for item in graphs), default=0),
            "cycles_max": max((item["cycles"] for item in graphs), default=0),
        },
        "ambiguity_resolution": {
            "epochs": len(ar),
            "candidates_min": min((item["candidates"] for item in ar), default=0),
            "candidates_max": max((item["candidates"] for item in ar), default=0),
            "newly_fixed_total": sum(item["newly_fixed"] for item in ar),
            "epochs_with_new_fixes": sum(
                item["newly_fixed"] > 0 for item in ar
            ),
            "first_epoch_with_new_fixes": next(
                (item for item in ar if item["newly_fixed"] > 0), None
            ),
            "last_epoch_with_new_fixes": next(
                (item for item in reversed(ar) if item["newly_fixed"] > 0), None
            ),
            "held_integer_rank_max": max(
                (item["held_integer_rank"] for item in ar), default=0
            ),
            "adop_cycles_min": min((item["adop_cycles"] for item in ar), default=0),
            "adop_cycles_max": max((item["adop_cycles"] for item in ar), default=0),
            "last_epoch": ar[-1] if ar else None,
        },
        "products": products,
        "covariance": covariance,
    }
    print(json.dumps(report, indent=2, sort_keys=True))
    return 0 if structural_pass else 2


if __name__ == "__main__":
    raise SystemExit(main())
