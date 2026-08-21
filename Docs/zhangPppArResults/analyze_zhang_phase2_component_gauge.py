#!/usr/bin/env python3
"""Audit the Phase-2 component-gauge SHADOW contract from a PEA TRACE.

The pass condition deliberately uses only strict integer certificates:
for an epoch with both a direct Product-IAR result and a component-gauge
result, r_combined = r_direct + exact_new_product_rank must exceed r_direct.
Neither network-WL rank nor conditioning-only ledger rank is accepted as a
substitute.  Missing same-epoch evidence is reported as INCONCLUSIVE.
"""

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path


FIELDS = re.compile(r"(?P<key>[A-Za-z0-9_]+)=(?P<value>[^\s]+)")
TIME = re.compile(r"\btime=(?P<time>\d{4}-\d{2}-\d{2}\s+\d{2}:\d{2}:\d{2})")


def fields(line: str) -> dict[str, str]:
    result = {match.group("key"): match.group("value") for match in FIELDS.finditer(line)}
    if match := TIME.search(line):
        result["time"] = match.group("time")
    return result


def integer(row: dict[str, str], key: str, default: int = 0) -> int:
    try:
        return int(row.get(key, default))
    except ValueError:
        return default


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("trace", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()

    direct_by_time: dict[str, dict[str, str]] = {}
    gauge_rows: list[dict[str, str]] = []
    schedulers: list[dict[str, str]] = []
    runtime_epochs: list[str] = []

    with args.trace.open("r", errors="replace") as trace:
        for line in trace:
            if "ZHANG_AR_RUNTIME_CONFIG time=" in line:
                row = fields(line)
                if "time" in row:
                    runtime_epochs.append(row["time"])
            elif "ZHANG_PRODUCT_RELATION_IAR_SHADOW" in line:
                row = fields(line)
                if "time" in row:
                    direct_by_time[row["time"]] = row
            elif "ZHANG_PRODUCT_COMPONENT_GAUGE_SHADOW" in line:
                gauge_rows.append(fields(line))
            elif "ZHANG_PRODUCT_CLOSURE_SCHEDULER" in line:
                schedulers.append(fields(line))

    epochs: list[dict[str, object]] = []
    pass_epochs = 0
    for gauge in gauge_rows:
        time = gauge.get("time", "UNKNOWN")
        direct = direct_by_time.get(time)
        direct_rank = integer(direct or {}, "certified_joint_integer_rank")
        new_rank = integer(gauge, "exact_new_product_rank")
        combined_rank = direct_rank + new_rank
        comparable = direct is not None
        passes = comparable and combined_rank > direct_rank
        pass_epochs += int(passes)
        epochs.append({
            "time": time,
            "components_before": integer(gauge, "components_before"),
            "gauge_target_rank": integer(gauge, "gauge_target_rank"),
            "measurement_rank": integer(gauge, "measurement_rank"),
            "estimable_gauge_rank": integer(gauge, "estimable_gauge_rank"),
            "wl_gauge_fixed_rank": integer(gauge, "wl_fixed_rank"),
            "l1_gauge_fixed_rank": integer(gauge, "l1_fixed_rank"),
            "exact_new_product_rank": new_rank,
            "components_after_shadow": integer(gauge, "components_after_shadow", -1),
            "r_direct": direct_rank if comparable else None,
            "r_combined": combined_rank if comparable else None,
            "strict_rank_gain_proven": passes,
            "status": gauge.get("status", "MISSING"),
        })

    natural_target = "2024-07-17"
    completed = any(epoch.endswith("00:15:30") and epoch.startswith(natural_target)
                    for epoch in runtime_epochs)
    result = {
        "trace": str(args.trace),
        "runtime_epoch_count": len(runtime_epochs),
        "last_runtime_epoch": runtime_epochs[-1] if runtime_epochs else None,
        "natural_completion_001530": completed,
        "closure_scheduler_rows": len(schedulers),
        "closure_scheduler_run_rows": sum(integer(row, "run") == 1 for row in schedulers),
        "direct_product_rows": len(direct_by_time),
        "component_gauge_rows": len(gauge_rows),
        "strict_rank_gain_epochs": pass_epochs,
        "phase2_verdict": (
            "PASS_R_COMBINED_GT_R_DIRECT" if pass_epochs else
            "FAIL_NO_STRICT_COMPONENT_RANK_GAIN" if gauge_rows else
            "INCONCLUSIVE_NO_COMPONENT_GAUGE_TRACE"
        ),
        "epochs": epochs,
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
