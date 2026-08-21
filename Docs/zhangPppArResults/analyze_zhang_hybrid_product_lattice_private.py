#!/usr/bin/env python3
"""Audit the 00:15:30 HYBRID_PRODUCT_WL_L1 private replay.

This analyzer deliberately separates the product-feedback algebra gate from
the stronger dual-frequency product-certificate gate.  A covariance gain by
itself never authorizes PPP-AR products.
"""

from __future__ import annotations

import argparse
import csv
import json
import math
import re
from collections import Counter
from pathlib import Path


FIELD = re.compile(r"([A-Za-z0-9_]+)=([^\s]+)")
TRACE_TIME = re.compile(r"\btime=(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}(?:\.\d+)?)")


def fields(line: str) -> dict[str, str]:
    result = dict(FIELD.findall(line))
    match = TRACE_TIME.search(line)
    if match:
        result["time"] = match.group(1)
    return result


def integer(row: dict[str, str], key: str, default: int = 0) -> int:
    try:
        return int(float(row.get(key, str(default))))
    except ValueError:
        return default


def number(row: dict[str, str], key: str) -> float:
    try:
        return float(row.get(key, "nan"))
    except ValueError:
        return math.nan


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("output_dir", type=Path)
    parser.add_argument("--json", type=Path)
    args = parser.parse_args()
    traces = sorted(args.output_dir.glob("*.TRACE"))
    product_path = args.output_dir / "zhang_internal_products.csv"
    if len(traces) != 1 or not product_path.exists():
        raise SystemExit("expected one TRACE and zhang_internal_products.csv")

    trace_text = traces[0].read_text(encoding="utf-8", errors="replace")
    stage_rows: list[dict[str, str]] = []
    block_rows: list[dict[str, str]] = []
    branch_rows: list[dict[str, str]] = []
    effect_rows: list[dict[str, str]] = []
    authority_rows: list[dict[str, str]] = []
    ledger_rows: list[dict[str, str]] = []
    for line in trace_text.splitlines():
        if line.startswith("ZHANG_PRODUCT_LATTICE_STAGE "):
            stage_rows.append(fields(line))
        elif line.startswith("ZHANG_PRODUCT_LATTICE_BLOCK "):
            block_rows.append(fields(line))
        elif line.startswith("ZHANG_PRODUCT_FIXED_BRANCH "):
            branch_rows.append(fields(line))
        elif line.startswith("ZHANG_PRODUCT_FIXED_EFFECT "):
            effect_rows.append(fields(line))
        elif line.startswith("ZHANG_FLOAT_AUTHORITY_CLOSURE "):
            authority_rows.append(fields(line))
        elif line.startswith("ZHANG_PRODUCT_INTEGER_LEDGER "):
            ledger_rows.append(fields(line))

    epochs: set[int] = set()
    solution_counts: Counter[str] = Counter()
    integer_valid: Counter[str] = Counter()
    pppar_usable: Counter[str] = Counter()
    last_epoch = 0
    with product_path.open(newline="", encoding="utf-8") as stream:
        for row in csv.DictReader(stream):
            epoch = int(row["gpst_seconds"])
            epochs.add(epoch)
            last_epoch = max(last_epoch, epoch)
            solution = row["solution"]
            solution_counts[solution] += 1
            integer_valid[solution] += int(row["integer_valid"])
            pppar_usable[solution] += int(row["pppar_usable"])

    conditioned = [row for row in branch_rows if row.get("status") == "CONDITIONED"]
    reliable_blocks = [row for row in block_rows if integer(row, "reliable") == 1]
    overall_effects = [
        row for row in effect_rows if row.get("component") == "ALL_MAPPABLE"
    ]
    effect_passes = []
    for row in overall_effects:
        wl = number(row, "pair_trace_wl")
        product = number(row, "pair_trace_product_fixed")
        noncommon = number(row, "noncommon_mean_update_norm")
        if (
            integer(row, "product_precision_valid") == 1
            and math.isfinite(wl)
            and math.isfinite(product)
            and product < wl
            and math.isfinite(noncommon)
            and noncommon > 1e-10
        ):
            effect_passes.append(row)
    effect_observables = {row.get("observable") for row in effect_passes}
    effects_by_epoch: dict[str, set[str]] = {}
    for row in effect_passes:
        epoch = row.get("time")
        observable = row.get("observable")
        if epoch and observable:
            effects_by_epoch.setdefault(epoch, set()).add(observable)
    dual_effect_epochs = sorted(
        epoch
        for epoch, observables in effects_by_epoch.items()
        if {"L1C", "L2W"}.issubset(observables)
    )
    dual_effect_epoch = bool(dual_effect_epochs)
    pair_certified_blocks = [
        row for row in reliable_blocks if integer(row, "certified_pair_rank") > 0
    ]
    formal_rows = solution_counts.get("PRODUCT_FIXED", 0)
    result = {
        "experiment": args.output_dir.name,
        "trace": str(traces[0]),
        "completion": {
            "product_epoch_count": len(epochs),
            "expected_epoch_count": 32,
            "last_gpst_seconds": last_epoch,
            "expected_last_gpst_seconds": 1405210530,
            "natural_target_reached": len(epochs) == 32
            and last_epoch == 1405210530,
        },
        "product_lattice": {
            "stage_records": len(stage_rows),
            "reliable_stage_records": sum(integer(row, "reliable") for row in stage_rows),
            "maximum_wl_exact_rank": max(
                (integer(row, "exact_fixed_rank") for row in stage_rows
                 if row.get("stage") == "WL"),
                default=0,
            ),
            "maximum_conditional_l1_exact_rank": max(
                (integer(row, "exact_fixed_rank") for row in stage_rows
                 if row.get("stage") == "L1_GIVEN_WL"),
                default=0,
            ),
            "block_records": len(block_rows),
            "reliable_blocks": len(reliable_blocks),
            "maximum_conditioning_rank": max(
                (integer(row, "conditioning_rank") for row in block_rows), default=0
            ),
            "maximum_certified_pair_rank": max(
                (integer(row, "certified_pair_rank") for row in block_rows), default=0
            ),
            "stage_status_counts": dict(Counter(
                row.get("status", "MISSING") for row in stage_rows
            )),
            "minimum_reliable_failure_probability_bound": min(
                (number(row, "failure_probability_bound") for row in stage_rows
                 if integer(row, "reliable") == 1),
                default=math.nan,
            ),
            "maximum_reliable_failure_probability_budget": max(
                (number(row, "failure_probability_budget") for row in stage_rows
                 if integer(row, "reliable") == 1),
                default=math.nan,
            ),
        },
        "private_branch": {
            "records": len(branch_rows),
            "conditioned_records": len(conditioned),
            "maximum_constraint_rank": max(
                (integer(row, "constraint_rank") for row in branch_rows), default=0
            ),
            "float_authority_checks": len(authority_rows),
            "float_authority_failures": sum(
                row.get("status") != "PASS" for row in authority_rows
            ),
        },
        "effect_gate": {
            "records": len(effect_rows),
            "overall_records": len(overall_effects),
            "passing_overall_records": len(effect_passes),
            "passing_observables": sorted(effect_observables - {None}),
            "dual_frequency_effect_seen": dual_effect_epoch,
            "dual_frequency_effect_epochs": dual_effect_epochs,
            "maximum_wl_to_product_gain": max(
                (number(row, "wl_to_product_gain") for row in overall_effects),
                default=math.nan,
            ),
            "maximum_noncommon_mean_update_norm": max(
                (number(row, "noncommon_mean_update_norm")
                 for row in overall_effects),
                default=math.nan,
            ),
        },
        "products": {
            "solution_counts": dict(solution_counts),
            "integer_valid_counts": dict(integer_valid),
            "pppar_usable_counts": dict(pppar_usable),
            "product_fixed_rows": formal_rows,
        },
        "ledger": {
            "records": len(ledger_rows),
            "maximum_active_rank": max(
                (integer(row, "active_rank_after") for row in ledger_rows), default=0
            ),
        },
        "gates": {
            "algebra_gate_pass": bool(conditioned)
            and dual_effect_epoch
            and not any(row.get("status") != "PASS" for row in authority_rows),
            "product_certificate_gate_pass": bool(pair_certified_blocks)
            and pppar_usable.get("PRODUCT_FIXED", 0) > 0,
        },
        "fatal_terms": {
            term: trace_text.lower().count(term)
            for term in ("fatal", "segmentation fault", "bad_alloc", "out of memory")
        },
    }
    output = json.dumps(result, indent=2, ensure_ascii=False, allow_nan=True)
    print(output)
    if args.json:
        args.json.write_text(output + "\n", encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
