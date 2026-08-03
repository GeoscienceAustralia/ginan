#!/usr/bin/env python3
"""Audit epoch-to-epoch numerical continuity of Zhang internal products."""

from __future__ import annotations

import argparse
import csv
import json
import math
from collections import Counter, defaultdict
from pathlib import Path


THRESHOLDS_M = (1.0, 10.0, 1_000.0, 1e6, 1e12, 1e30)


def event(row: dict[str, str], step_m: float, epoch: int) -> dict[str, object]:
    return {
        "gpst_seconds": epoch,
        "satellite": row["satellite"],
        "observable": row["observable"],
        "step_m": step_m,
        "integer_valid": row["integer_valid"] == "1",
        "datum_version": int(row["datum_version"]),
        "discontinuity_counter": int(row["discontinuity_counter"]),
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("products", type=Path)
    parser.add_argument("--solution", default="FIXED")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    by_key: dict[tuple[str, str], list[dict[str, str]]] = defaultdict(list)
    with args.products.open(newline="", encoding="utf-8") as stream:
        for row in csv.DictReader(stream):
            if row["solution"] == args.solution:
                by_key[(row["satellite"], row["observable"])].append(row)

    counts = Counter()
    valid_counts = Counter()
    first: dict[float, dict[str, object]] = {}
    first_valid: dict[float, dict[str, object]] = {}
    maximum: dict[str, object] | None = None
    maximum_valid: dict[str, object] | None = None
    absolute_maximum: dict[str, object] | None = None
    epochs: set[int] = set()

    for rows in by_key.values():
        rows.sort(key=lambda row: int(row["gpst_seconds"]))
        for row in rows:
            epoch = int(row["gpst_seconds"])
            epochs.add(epoch)
            correction = abs(float(row["correction_m"]))
            current_absolute = {
                "gpst_seconds": epoch,
                "satellite": row["satellite"],
                "observable": row["observable"],
                "absolute_correction_m": correction,
                "integer_valid": row["integer_valid"] == "1",
            }
            if (absolute_maximum is None or
                    correction > absolute_maximum["absolute_correction_m"]):
                absolute_maximum = current_absolute

        for previous, current in zip(rows, rows[1:]):
            previous_epoch = int(previous["gpst_seconds"])
            current_epoch = int(current["gpst_seconds"])
            if current_epoch - previous_epoch <= 0:
                continue
            previous_value = float(previous["correction_m"])
            current_value = float(current["correction_m"])
            step = abs(current_value - previous_value)
            if not math.isfinite(step):
                step = math.inf
            current_event = event(current, step, current_epoch)
            both_valid = (
                previous["integer_valid"] == "1" and
                current["integer_valid"] == "1"
            )
            if maximum is None or step > maximum["step_m"]:
                maximum = current_event
            if both_valid and (
                    maximum_valid is None or step > maximum_valid["step_m"]):
                maximum_valid = current_event
            for threshold in THRESHOLDS_M:
                if step > threshold:
                    counts[threshold] += 1
                    if threshold not in first or current_epoch < first[threshold]["gpst_seconds"]:
                        first[threshold] = current_event
                    if both_valid:
                        valid_counts[threshold] += 1
                        if (threshold not in first_valid or
                                current_epoch < first_valid[threshold]["gpst_seconds"]):
                            first_valid[threshold] = current_event

    start = min(epochs) if epochs else None
    result = {
        "products": str(args.products),
        "solution": args.solution,
        "epoch_count": len(epochs),
        "start_gpst_seconds": start,
        "maximum_absolute_correction": absolute_maximum,
        "maximum_epoch_step": maximum,
        "maximum_integer_valid_epoch_step": maximum_valid,
        "thresholds": {
            str(threshold): {
                "step_count": counts[threshold],
                "integer_valid_step_count": valid_counts[threshold],
                "first": first.get(threshold),
                "first_integer_valid": first_valid.get(threshold),
            }
            for threshold in THRESHOLDS_M
        },
    }
    text = json.dumps(result, indent=2, ensure_ascii=False, allow_nan=False)
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(text + "\n", encoding="utf-8")
    print(text)


if __name__ == "__main__":
    main()
