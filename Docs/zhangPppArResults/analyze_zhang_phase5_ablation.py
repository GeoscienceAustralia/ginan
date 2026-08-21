#!/usr/bin/env python3
"""Summarise the Phase-5 Direct/Ledger/ComponentGauge ablation.

The script intentionally uses only certified dual-frequency graph facts:
``r_dual`` is the latest certified pair-forest rank, ``K`` is the number of
remaining components reported by the component-gauge solver (zero means that
no dual graph was available), and ``J_pair_PRODUCT_FIXED`` is read from the
actual PRODUCT_FIXED effect trace.  It never substitutes network WL rank.
"""

from __future__ import annotations

import argparse
import csv
import json
import re
from collections import defaultdict
from datetime import datetime, timezone
from pathlib import Path


CASES = {
    "A_direct": "zhang_phase5_A_direct_2024199_180_001500_20260817_bounded",
    "B_ledger": "zhang_phase5_B_ledger_2024199_180_001500_20260817_bounded",
    "C_component": "zhang_phase5_C_component_2024199_180_001500_20260817_bounded",
    "D_ledger_component": "zhang_phase5_D_ledger_component_2024199_180_001500_20260817_bounded",
}


def fields(line: str) -> dict[str, str]:
    return dict(re.findall(r"([A-Za-z][A-Za-z0-9_]*)=([^\s]+)", line))


def as_int(value: str | None, default: int = 0) -> int:
    try:
        return int(value) if value is not None else default
    except ValueError:
        return default


def read_trace(trace: Path) -> dict[str, dict[str, int]]:
    by_time: dict[str, dict[str, int]] = defaultdict(
        lambda: {"r_dual": 0, "K_dual": 0, "J_pair_PRODUCT_FIXED": 0,
                 "max_closure_iteration": -1}
    )
    with trace.open("r", errors="replace") as stream:
        for line in stream:
            if " time=" not in line and not line.startswith("time="):
                continue
            values = fields(line)
            # The trace time contains a space; tokenising key=value pairs
            # alone would retain only the date.  Use the anchored field.
            match = re.search(r"(?:^|\s)time=(\d{4}-\d\d-\d\d\s+\d\d:\d\d:\d\d)", line)
            time = match.group(1) if match else None
            if time is None:
                continue
            row = by_time[time]
            if "ZHANG_PRODUCT_PRIVATE_CLOSURE_ITERATION" in line:
                iteration = as_int(values.get("iteration"), -1)
                # Later accepted iterations define the closure result.  A
                # rejected iteration is diagnostic only and must not lower r.
                if iteration >= row["max_closure_iteration"]:
                    row["max_closure_iteration"] = iteration
                    row["r_dual"] = max(row["r_dual"],
                                        as_int(values.get("dual_rank")))
                    row["K_dual"] = as_int(values.get("components_after"))
            elif "ZHANG_PRODUCT_LATTICE_POST_LEDGER_GRAPH" in line:
                row["r_dual"] = max(row["r_dual"],
                                    as_int(values.get("certified_pair_rank")))
            elif "ZHANG_PRODUCT_FIXED_EFFECT" in line:
                row["J_pair_PRODUCT_FIXED"] = max(
                    row["J_pair_PRODUCT_FIXED"],
                    as_int(values.get("pair_certificate_rank")))
    return dict(sorted(by_time.items()))


def component_sizes(product_csv: Path) -> dict[str, int]:
    # Count unique satellites in each actual dual-frequency PRODUCT_FIXED AR
    # component.  FLOAT-only satellites deliberately do not enter this KPI.
    satellites: dict[tuple[str, str], set[str]] = defaultdict(set)
    with product_csv.open(newline="", errors="replace") as stream:
        for row in csv.DictReader(stream):
            if row.get("solution") != "PRODUCT_FIXED":
                continue
            if row.get("pppar_usable") not in {"1", "true", "True"}:
                continue
            component = row.get("integer_component_id", "NONE")
            satellite = row.get("satellite", "")
            gpst = row.get("gpst_seconds", "")
            if component != "NONE" and satellite and gpst:
                try:
                    time = datetime.fromtimestamp(float(gpst), tz=timezone.utc).strftime(
                        "%Y-%m-%d %H:%M:%S")
                except (TypeError, ValueError, OverflowError):
                    continue
                satellites[(time, component)].add(satellite)
    answer: dict[str, int] = defaultdict(int)
    for (time, _), members in satellites.items():
        answer[time] = max(answer[time], len(members))
    return dict(answer)


def analyse_case(outputs: Path, description: str) -> dict[str, object]:
    traces = sorted(outputs.glob("*.TRACE"))
    if len(traces) != 1:
        raise RuntimeError(f"{description}: expected one TRACE, found {len(traces)}")
    trace_rows = read_trace(traces[0])
    csv_rows = component_sizes(outputs / "zhang_internal_products.csv")
    timeline = []
    for time, values in trace_rows.items():
        values = dict(values)
        values["max_abs_Ci"] = csv_rows.get(time, 0)
        timeline.append({"time": time, **values})
    return {
        "description": description,
        "trace": str(traces[0]),
        "epochs": timeline,
        "peak": {
            key: max((row[key] for row in timeline), default=0)
            for key in ("r_dual", "K_dual", "max_abs_Ci", "J_pair_PRODUCT_FIXED")
        },
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("outputs_root", type=Path,
                        help="GINAN inputData/outputs directory")
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    result = {name: analyse_case(args.outputs_root / desc, desc)
              for name, desc in CASES.items()}
    args.output.write_text(json.dumps(result, indent=2) + "\n")


if __name__ == "__main__":
    main()
