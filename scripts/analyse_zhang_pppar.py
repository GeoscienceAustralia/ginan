#!/usr/bin/env python3
"""Summarise independent Zhang internal-product PPP-AR user traces.

Wrong-fix is reported as an internal-consistency proxy: an accepted integer is
wrong when it differs from the modal integer later observed for the same
receiver/satellite/signal/reference/product-datum segment.  A genuinely
external ambiguity truth is not available in an undifferenced user run.
"""

from __future__ import annotations

import argparse
import json
import math
import re
import statistics
from collections import Counter, defaultdict
from datetime import datetime
from pathlib import Path


KEY_VALUE = re.compile(r"([A-Za-z0-9_]+)=([^\s]+)")
TIME_FORMAT = "%Y-%m-%dT%H:%M:%S"


def parse_record(line: str) -> tuple[str, dict[str, str]] | None:
    if not line.startswith("ZHANG_USER_"):
        return None
    line = re.sub(
        r"time=(\d{4}-\d{2}-\d{2})\s+(\d{2}:\d{2}:\d{2})",
        r"time=\1T\2",
        line,
    )
    record_type = line.split(maxsplit=1)[0]
    values = dict(KEY_VALUE.findall(line))
    return record_type, values


def percentile(values: list[float], probability: float) -> float:
    if not values:
        return math.nan
    ordered = sorted(values)
    index = probability * (len(ordered) - 1)
    low = math.floor(index)
    high = math.ceil(index)
    if low == high:
        return ordered[low]
    return ordered[low] + (ordered[high] - ordered[low]) * (index - low)


def first_sustained_convergence(
    diagnostics: list[dict[str, str]],
    horizontal_threshold: float,
    vertical_threshold: float,
    count: int = 3,
) -> float | None:
    if not diagnostics:
        return None
    start = datetime.fromisoformat(diagnostics[0]["time"])
    run = 0
    run_start: datetime | None = None
    for record in diagnostics:
        east = float(record["east_error_m"])
        north = float(record["north_error_m"])
        up = float(record["up_error_m"])
        converged = (
            math.hypot(east, north) <= horizontal_threshold
            and abs(up) <= vertical_threshold
        )
        if converged:
            if run == 0:
                run_start = datetime.fromisoformat(record["time"])
            run += 1
            if run >= count and run_start is not None:
                return (run_start - start).total_seconds()
        else:
            run = 0
            run_start = None
    return None


def analyse(trace_path: Path, label: str, category: str) -> dict[str, object]:
    records: dict[str, list[dict[str, str]]] = defaultdict(list)
    with trace_path.open(encoding="utf-8", errors="replace") as stream:
        for line in stream:
            parsed = parse_record(line.strip())
            if parsed:
                record_type, values = parsed
                records[record_type].append(values)

    diagnostics = records["ZHANG_USER_DIAGNOSTIC"]
    ar_records = records["ZHANG_USER_AR_SUMMARY"]
    ambiguities = records["ZHANG_USER_AMBIGUITY"]
    references = records["ZHANG_USER_REFERENCE"]
    if not diagnostics:
        raise RuntimeError(f"No ZHANG_USER_DIAGNOSTIC records in {trace_path}")

    start = datetime.fromisoformat(diagnostics[0]["time"])
    fix_events = [
        record for record in ar_records if int(record["newly_fixed"]) > 0
    ]
    first_fix = (
        (datetime.fromisoformat(fix_events[0]["time"]) - start).total_seconds()
        if fix_events
        else None
    )

    ambiguity_by_epoch: dict[str, list[dict[str, str]]] = defaultdict(list)
    segments: dict[tuple[str, ...], list[dict[str, str]]] = defaultdict(list)
    for record in ambiguities:
        ambiguity_by_epoch[record["time"]].append(record)
        segment = (
            record["satellite"],
            record["observable"],
            record["reference"],
            record["product_counter"],
            record["datum_version"],
        )
        segments[segment].append(record)

    integer_like_total = 0
    integer_valid_total = 0
    for _, epoch_records in ambiguity_by_epoch.items():
        for record in epoch_records:
            if int(record["integer_valid"]) == 0:
                continue
            integer_valid_total += 1
            if float(record["fractional_cycle"]) < 1e-6:
                integer_like_total += 1

    segment_modes: dict[tuple[str, ...], int] = {}
    for segment, segment_records in segments.items():
        integers = [
            int(record["rounded_cycles"])
            for record in segment_records
            if float(record["fractional_cycle"]) < 1e-6
        ]
        if integers:
            segment_modes[segment] = Counter(integers).most_common(1)[0][0]

    event_times = {record["time"] for record in fix_events}
    accepted = 0
    inconsistent = 0
    for event_time in event_times:
        for record in ambiguity_by_epoch.get(event_time, []):
            if (
                int(record["integer_valid"]) == 0
                or float(record["fractional_cycle"]) >= 1e-6
            ):
                continue
            segment = (
                record["satellite"],
                record["observable"],
                record["reference"],
                record["product_counter"],
                record["datum_version"],
            )
            if segment not in segment_modes:
                continue
            accepted += 1
            inconsistent += (
                int(record["rounded_cycles"]) != segment_modes[segment]
            )

    datum_changes: list[datetime] = []
    previous_datum: tuple[int, int] | None = None
    for record in diagnostics:
        datum = (int(record["product_counter"]), int(record["datum_version"]))
        if previous_datum is not None and datum != previous_datum:
            datum_changes.append(datetime.fromisoformat(record["time"]))
        previous_datum = datum
    fix_times = [datetime.fromisoformat(record["time"]) for record in fix_events]
    recoveries = []
    for reset_time in datum_changes:
        later = next((time for time in fix_times if time >= reset_time), None)
        if later is not None:
            recoveries.append((later - reset_time).total_seconds())

    enu_all = [
        (
            float(record["east_error_m"]),
            float(record["north_error_m"]),
            float(record["up_error_m"]),
        )
        for record in diagnostics
    ]
    if first_fix is None:
        enu_fixed: list[tuple[float, float, float]] = []
    else:
        enu_fixed = [
            error
            for record, error in zip(diagnostics, enu_all)
            if (
                datetime.fromisoformat(record["time"]) - start
            ).total_seconds()
            >= first_fix
        ]

    def component_stats(
        errors: list[tuple[float, float, float]]
    ) -> dict[str, float | None]:
        if not errors:
            return {
                "east_rms_m": None,
                "north_rms_m": None,
                "up_rms_m": None,
                "horizontal_p95_m": None,
                "up_abs_p95_m": None,
            }
        east = [value[0] for value in errors]
        north = [value[1] for value in errors]
        up = [value[2] for value in errors]
        return {
            "east_rms_m": math.sqrt(statistics.fmean(x * x for x in east)),
            "north_rms_m": math.sqrt(statistics.fmean(x * x for x in north)),
            "up_rms_m": math.sqrt(statistics.fmean(x * x for x in up)),
            "horizontal_p95_m": percentile(
                [math.hypot(e, n) for e, n in zip(east, north)], 0.95
            ),
            "up_abs_p95_m": percentile([abs(x) for x in up], 0.95),
        }

    candidates_at_fix_events = sum(
        int(record["candidates"]) for record in fix_events
    )
    held_ranks = [
        int(record.get("held_integer_rank", 0)) for record in ar_records
    ]
    maximum_held_rank = max(held_ranks, default=0)
    held_after_ttff = [
        int(record.get("held_integer_rank", 0))
        for record in ar_records
        if first_fix is not None
        and (
            datetime.fromisoformat(record["time"]) - start
        ).total_seconds()
        >= first_fix
    ]
    event_position_wrong = 0
    diagnostic_by_time = {record["time"]: record for record in diagnostics}
    for event in fix_events:
        diagnostic = diagnostic_by_time.get(event["time"])
        if diagnostic is None:
            continue
        horizontal = math.hypot(
            float(diagnostic["east_error_m"]),
            float(diagnostic["north_error_m"]),
        )
        vertical = abs(float(diagnostic["up_error_m"]))
        event_position_wrong += horizontal > 0.5 or vertical > 1.0

    return {
        "label": label,
        "category": category,
        "trace": str(trace_path),
        "epochs": len(diagnostics),
        "ar_attempt_epochs": len(ar_records),
        "newly_fixed_total": sum(
            int(record["newly_fixed"]) for record in ar_records
        ),
        "time_to_first_fix_s": first_fix,
        "ambiguity_fix_rate": (
            sum(int(record["newly_fixed"]) for record in fix_events)
            / candidates_at_fix_events
            if candidates_at_fix_events
            else 0.0
        ),
        "maximum_held_integer_rank": maximum_held_rank,
        "fixed_hold_rate_after_ttff": (
            sum(rank > 0 for rank in held_after_ttff) / len(held_after_ttff)
            if held_after_ttff
            else 0.0
        ),
        "raw_ambiguity_integer_like_rate": (
            integer_like_total / integer_valid_total
            if integer_valid_total
            else 0.0
        ),
        "wrong_fix_position_proxy_count": event_position_wrong,
        "wrong_fix_position_proxy_denominator": len(fix_events),
        "wrong_fix_position_proxy_rate": (
            event_position_wrong / len(fix_events) if fix_events else None
        ),
        "wrong_fix_modal_proxy_count": inconsistent,
        "wrong_fix_modal_proxy_denominator": accepted,
        "wrong_fix_modal_proxy_rate": (
            inconsistent / accepted if accepted else None
        ),
        "datum_change_count": len(datum_changes),
        "median_recovery_after_datum_change_s": (
            statistics.median(recoveries) if recoveries else None
        ),
        "reference_change_count": len(references),
        "enu_all": component_stats(enu_all),
        "enu_after_first_fix": component_stats(enu_fixed),
        "convergence_enu_10cm_20cm_s": first_sustained_convergence(
            diagnostics, 0.10, 0.20
        ),
        "convergence_enu_5cm_10cm_s": first_sustained_convergence(
            diagnostics, 0.05, 0.10
        ),
        "median_float_fractional_cycle": statistics.median(
            float(record["median_fractional_cycle"]) for record in diagnostics
        ),
        "p90_float_fractional_cycle": percentile(
            [float(record["p90_fractional_cycle"]) for record in diagnostics],
            0.90,
        ),
    }


def json_safe(value: object) -> object:
    """Replace non-finite diagnostics from empty/initial states with null."""
    if isinstance(value, float) and not math.isfinite(value):
        return None
    if isinstance(value, dict):
        return {key: json_safe(item) for key, item in value.items()}
    if isinstance(value, list):
        return [json_safe(item) for item in value]
    return value


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("trace", type=Path)
    parser.add_argument("--label", required=True)
    parser.add_argument(
        "--category", required=True,
        help="free-form station class or geographic region",
    )
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    result = json_safe(analyse(args.trace, args.label, args.category))
    text = json.dumps(result, indent=2, ensure_ascii=False, allow_nan=False)
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(text + "\n", encoding="utf-8")
    print(text)


if __name__ == "__main__":
    main()
