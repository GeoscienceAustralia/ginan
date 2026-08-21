#!/usr/bin/env python3
"""Verify the frozen-posterior Phase-4 private closure iteration contract."""

from __future__ import annotations

import argparse
import json
import re
from collections import defaultdict
from pathlib import Path


FIELDS = re.compile(r"(?P<key>[A-Za-z0-9_]+)=(?P<value>[^\s]+)")
TIME = re.compile(r"\btime=(?P<time>\d{4}-\d{2}-\d{2}\s+\d{2}:\d{2}:\d{2})")


def fields(line: str) -> dict[str, str]:
    row = {m.group("key"): m.group("value") for m in FIELDS.finditer(line)}
    if match := TIME.search(line):
        row["time"] = match.group("time")
    return row


def integer(row: dict[str, str], key: str) -> int:
    try:
        return int(row[key])
    except (KeyError, ValueError):
        return 0


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("trace", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()

    by_epoch: dict[str, dict[int, dict[str, str]]] = defaultdict(dict)
    with args.trace.open(errors="replace") as trace:
        for line in trace:
            if "ZHANG_PRODUCT_PRIVATE_CLOSURE_ITERATION time=" not in line:
                continue
            row = fields(line)
            if "time" in row and "iteration" in row:
                by_epoch[row["time"]][integer(row, "iteration")] = row

    epochs = []
    violations = []
    for time, iterations in sorted(by_epoch.items()):
        ordered = [iterations[key] for key in sorted(iterations)]
        ranks = [integer(row, "dual_rank") for row in ordered]
        components = [integer(row, "components_after") for row in ordered]
        monotonic = all(after >= before for before, after in zip(ranks, ranks[1:]))
        if not monotonic:
            violations.append(time)
        epochs.append({
            "time": time,
            "iterations": [integer(row, "iteration") for row in ordered],
            "dual_rank": ranks,
            "components_after": components,
            "monotonic": monotonic,
        })

    result = {
        "trace": str(args.trace),
        "epochs_with_iteration_trace": len(epochs),
        "monotonic_rank_violations": violations,
        "phase4_verdict": (
            "PASS_FROZEN_PRIVATE_CLOSURE_MONOTONIC"
            if epochs and not violations
            else "FAIL_NONMONOTONIC_DUAL_RANK"
            if violations else "INCONCLUSIVE_NO_ITERATION_TRACE"
        ),
        "epochs": epochs,
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
