#!/usr/bin/env python3
"""Analyse an independent Ginan PPP-AR run using external clock/OSB products.

The trace contains both the forward float state (STATES/PPP) and the temporary
per-epoch ambiguity-resolved state (STATES/AR).  This script deliberately
reports datum-invariant satellite single differences and wide-lane single
differences in addition to raw undifferenced ambiguity fractions.
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


STATE_RE = re.compile(
    r"^\*\s+-?\d+\s+"
    r"(?P<date>\d{4}-\d{2}-\d{2})\s+(?P<time>\d{2}:\d{2}:\d{2}\.\d+)\s+"
    r"AMBIGUITY\s+(?P<sat>[A-Z]\d{2})\s+(?P<rec>\S+)\s+"
    r"(?P<signal>L\d\w)\s+(?P<value>[+-]?\d+(?:\.\d+)?)"
)
FIX_TIME_RE = re.compile(
    r"fixAndHoldAmbiguities:\s+"
    r"(?P<date>\d{4}-\d{2}-\d{2})\s+(?P<time>\d{2}:\d{2}:\d{2}\.\d+)"
)
FIX_RE = re.compile(
    r"(?P<resolved>\d+)\s+out of\s+(?P<candidates>\d+)"
    r"\s+ambiguities resolved, applying"
)


def fraction(value: float) -> float:
    return abs(value - round(value))


def percentile(values: list[float], probability: float) -> float | None:
    if not values:
        return None
    values = sorted(values)
    index = probability * (len(values) - 1)
    low = math.floor(index)
    high = math.ceil(index)
    if low == high:
        return values[low]
    return values[low] + (values[high] - values[low]) * (index - low)


def fraction_stats(values: list[float]) -> dict[str, float | int | None]:
    return {
        "count": len(values),
        "median_abs_cycle": statistics.median(values) if values else None,
        "p90_abs_cycle": percentile(values, 0.90),
        "within_0.15_rate": (
            sum(value <= 0.15 for value in values) / len(values)
            if values
            else None
        ),
        "within_0.25_rate": (
            sum(value <= 0.25 for value in values) / len(values)
            if values
            else None
        ),
        "within_0.01_rate": (
            sum(value <= 0.01 for value in values) / len(values)
            if values
            else None
        ),
    }


def parse_trace(
    trace: Path,
) -> tuple[
    dict[str, dict[datetime, dict[tuple[str, str], float]]],
    list[dict[str, int | datetime]],
    int,
]:
    states: dict[
        str, dict[datetime, dict[tuple[str, str], float]]
    ] = {
        "PPP": defaultdict(dict),
        "AR": defaultdict(dict),
    }
    section: str | None = None
    fix_time: datetime | None = None
    fix_attempt_times: set[datetime] = set()
    fixes_by_epoch: dict[datetime, dict[str, int | datetime]] = {}
    with trace.open(encoding="utf-8", errors="replace") as stream:
        for line in stream:
            stripped = line.strip()
            if stripped == "+STATES/PPP":
                section = "PPP"
                continue
            if stripped == "-STATES/PPP":
                section = None
                continue
            if stripped == "+STATES/AR":
                section = "AR"
                continue
            if stripped == "-STATES/AR":
                section = None
                continue
            time_match = FIX_TIME_RE.search(line)
            if time_match:
                fix_time = datetime.fromisoformat(
                    f"{time_match['date']}T{time_match['time']}"
                )
                fix_attempt_times.add(fix_time)
            fix_match = FIX_RE.search(line)
            if fix_match and fix_time is not None:
                record = {
                    "time": fix_time,
                    "resolved": int(fix_match["resolved"]),
                    "candidates": int(fix_match["candidates"]),
                }
                previous = fixes_by_epoch.get(fix_time)
                if (
                    previous is None
                    or int(record["resolved"]) > int(previous["resolved"])
                ):
                    fixes_by_epoch[fix_time] = record
            if section is None:
                continue
            state_match = STATE_RE.match(line)
            if not state_match:
                continue
            epoch = datetime.fromisoformat(
                f"{state_match['date']}T{state_match['time']}"
            )
            key = (state_match["sat"], state_match["signal"])
            states[section][epoch][key] = float(state_match["value"])
    return (
        states,
        [fixes_by_epoch[epoch] for epoch in sorted(fixes_by_epoch)],
        len(fix_attempt_times),
    )


def choose_reference(
    epochs: dict[datetime, dict[tuple[str, str], float]],
    signals: tuple[str, str],
) -> str | None:
    counts: Counter[str] = Counter()
    for ambiguities in epochs.values():
        satellites = {
            sat for sat, signal in ambiguities if signal == signals[0]
        } & {
            sat for sat, signal in ambiguities if signal == signals[1]
        }
        counts.update(satellites)
    if not counts:
        return None
    return sorted(counts, key=lambda sat: (-counts[sat], sat))[0]


def ambiguity_diagnostics(
    epochs: dict[datetime, dict[tuple[str, str], float]],
    signals: tuple[str, str],
    reference: str | None,
) -> tuple[dict[str, object], dict[tuple[str, str], list[int]]]:
    raw: dict[str, list[float]] = {signal: [] for signal in signals}
    sd: dict[str, list[float]] = {signal: [] for signal in signals}
    raw_wl: list[float] = []
    sd_wl: list[float] = []
    integer_series: dict[tuple[str, str], list[int]] = defaultdict(list)
    usable_reference_epochs = 0

    for ambiguities in epochs.values():
        for (sat, signal), value in ambiguities.items():
            if signal in raw:
                raw[signal].append(fraction(value))
        paired = {
            sat
            for sat, signal in ambiguities
            if signal == signals[0]
            and (sat, signals[1]) in ambiguities
        }
        for sat in paired:
            raw_wl.append(
                fraction(
                    ambiguities[(sat, signals[0])]
                    - ambiguities[(sat, signals[1])]
                )
            )
        if (
            reference is None
            or (reference, signals[0]) not in ambiguities
            or (reference, signals[1]) not in ambiguities
        ):
            continue
        usable_reference_epochs += 1
        for sat in paired:
            if sat == reference:
                continue
            sd_values = {}
            for signal in signals:
                value = (
                    ambiguities[(sat, signal)]
                    - ambiguities[(reference, signal)]
                )
                sd[signal].append(fraction(value))
                sd_values[signal] = value
                if fraction(value) <= 0.01:
                    integer_series[(sat, signal)].append(round(value))
            wide_lane = sd_values[signals[0]] - sd_values[signals[1]]
            sd_wl.append(fraction(wide_lane))
            if fraction(wide_lane) <= 0.01:
                integer_series[(sat, "WL")].append(round(wide_lane))

    result: dict[str, object] = {
        "reference_satellite": reference,
        "reference_usable_epochs": usable_reference_epochs,
        "raw_original_frequency": {
            signal: fraction_stats(raw[signal]) for signal in signals
        },
        "raw_wide_lane": fraction_stats(raw_wl),
        "satellite_single_difference": {
            signal: fraction_stats(sd[signal]) for signal in signals
        },
        "single_difference_wide_lane": fraction_stats(sd_wl),
    }
    return result, integer_series


def read_pos_errors(
    pos: Path,
) -> list[tuple[datetime, float, float, float]]:
    errors_by_epoch: dict[datetime, tuple[datetime, float, float, float]] = {}
    with pos.open(encoding="utf-8", errors="replace") as stream:
        for line in stream:
            fields = line.split()
            if len(fields) < 17 or not fields[0][:4].isdigit():
                continue
            try:
                epoch = datetime.fromisoformat(fields[0])
                north, east, up = map(float, fields[14:17])
            except (ValueError, IndexError):
                continue
            errors_by_epoch[epoch] = (epoch, east, north, up)
    return [errors_by_epoch[epoch] for epoch in sorted(errors_by_epoch)]


def coordinate_stats(
    all_errors: list[tuple[datetime, float, float, float]],
    first_fix: datetime | None,
) -> dict[str, object]:
    def stats(
        errors: list[tuple[datetime, float, float, float]]
    ) -> dict[str, float | int | None]:
        if not errors:
            return {
                "epochs": 0,
                "east_rms_m": None,
                "north_rms_m": None,
                "up_rms_m": None,
                "horizontal_p95_m": None,
                "up_abs_p95_m": None,
            }
        east = [row[1] for row in errors]
        north = [row[2] for row in errors]
        up = [row[3] for row in errors]
        return {
            "epochs": len(errors),
            "east_rms_m": math.sqrt(statistics.fmean(x * x for x in east)),
            "north_rms_m": math.sqrt(statistics.fmean(x * x for x in north)),
            "up_rms_m": math.sqrt(statistics.fmean(x * x for x in up)),
            "horizontal_p95_m": percentile(
                [math.hypot(e, n) for e, n in zip(east, north)], 0.95
            ),
            "up_abs_p95_m": percentile([abs(x) for x in up], 0.95),
        }

    after_fix = (
        [row for row in all_errors if row[0] >= first_fix]
        if first_fix is not None
        else []
    )
    return {"all": stats(all_errors), "after_first_fix": stats(after_fix)}


def convergence_time(
    errors: list[tuple[datetime, float, float, float]],
    horizontal_threshold: float,
    vertical_threshold: float,
    sustained_epochs: int = 10,
) -> float | None:
    if not errors:
        return None
    run = 0
    run_start: datetime | None = None
    for epoch, east, north, up in errors:
        if (
            math.hypot(east, north) <= horizontal_threshold
            and abs(up) <= vertical_threshold
        ):
            if run == 0:
                run_start = epoch
            run += 1
            if run >= sustained_epochs and run_start is not None:
                return (run_start - errors[0][0]).total_seconds()
        else:
            run = 0
            run_start = None
    return None


def modal_inconsistency(
    integer_series: dict[tuple[str, str], list[int]]
) -> tuple[int, int]:
    inconsistent = 0
    total = 0
    for values in integer_series.values():
        if len(values) < 3:
            continue
        mode = Counter(values).most_common(1)[0][0]
        inconsistent += sum(value != mode for value in values)
        total += len(values)
    return inconsistent, total


def analyse(
    trace: Path,
    pos: Path,
    station: str,
    category: str,
    signals: tuple[str, str],
    requested_reference: str | None = None,
) -> dict[str, object]:
    states, fixes, ar_attempts = parse_trace(trace)
    all_epochs = sorted(states["PPP"])
    first_epoch = all_epochs[0] if all_epochs else None
    first_fix = fixes[0]["time"] if fixes else None
    reference = requested_reference or choose_reference(
        states["PPP"], signals
    )
    fixed_epochs = {record["time"] for record in fixes}
    float_diag, _ = ambiguity_diagnostics(
        states["PPP"], signals, reference
    )
    last_float_epoch = max(states["PPP"], default=None)
    float_last_hour = (
        {
            epoch: ambiguities
            for epoch, ambiguities in states["PPP"].items()
            if (last_float_epoch - epoch).total_seconds() <= 3600
        }
        if last_float_epoch is not None
        else {}
    )
    last_hour_reference = requested_reference or choose_reference(
        float_last_hour, signals
    )
    float_last_hour_diag, _ = ambiguity_diagnostics(
        float_last_hour, signals, last_hour_reference
    )
    fixed_diag, fixed_integer_series = ambiguity_diagnostics(
        {
            epoch: states["AR"][epoch]
            for epoch in states["AR"]
            if epoch in fixed_epochs
        },
        signals,
        reference,
    )
    inconsistent, integer_total = modal_inconsistency(
        fixed_integer_series
    )
    attempt_epochs = ar_attempts
    resolved_total = sum(int(record["resolved"]) for record in fixes)
    candidates_total = sum(int(record["candidates"]) for record in fixes)
    pos_errors = read_pos_errors(pos)
    pos_by_epoch = {row[0]: row for row in pos_errors}
    accepted_position_checks = [
        pos_by_epoch[record["time"]]
        for record in fixes
        if record["time"] in pos_by_epoch
    ]
    position_proxy_wrong = sum(
        math.hypot(row[1], row[2]) > 0.5 or abs(row[3]) > 1.0
        for row in accepted_position_checks
    )
    return {
        "station": station,
        "category": category,
        "trace": str(trace),
        "pos": str(pos),
        "signals": list(signals),
        "float_epochs": len(states["PPP"]),
        "ar_attempt_epochs": attempt_epochs,
        "ar_accepted_epochs": len(fixes),
        "ambiguity_epoch_fix_rate": (
            len(fixes) / attempt_epochs if attempt_epochs else 0.0
        ),
        "resolved_transform_rate": (
            resolved_total / candidates_total if candidates_total else 0.0
        ),
        "resolved_total": resolved_total,
        "candidates_total_at_accepted_epochs": candidates_total,
        "time_to_first_fix_s": (
            (first_fix - first_epoch).total_seconds()
            if first_fix is not None and first_epoch is not None
            else None
        ),
        "wrong_fix_modal_proxy_count": inconsistent,
        "wrong_fix_modal_proxy_denominator": integer_total,
        "wrong_fix_modal_proxy_rate": (
            inconsistent / integer_total if integer_total else None
        ),
        "wrong_fix_position_proxy_count": position_proxy_wrong,
        "wrong_fix_position_proxy_denominator": len(
            accepted_position_checks
        ),
        "wrong_fix_position_proxy_rate": (
            position_proxy_wrong / len(accepted_position_checks)
            if accepted_position_checks
            else None
        ),
        "float_ambiguities": float_diag,
        "float_ambiguities_last_hour": float_last_hour_diag,
        "ar_ambiguities": fixed_diag,
        "coordinates": coordinate_stats(pos_errors, first_fix),
        "convergence_enu_10cm_20cm_s": convergence_time(
            pos_errors, 0.10, 0.20
        ),
        "convergence_enu_5cm_10cm_s": convergence_time(
            pos_errors, 0.05, 0.10
        ),
        "interpretation": {
            "wrong_fix_note": (
                "Modal inconsistency is an internal proxy, not external "
                "ambiguity truth and is inflated by unlabelled cycle-slip "
                "arc resets. The position proxy flags accepted epochs with "
                "horizontal error >0.5 m or vertical error >1.0 m; repeated "
                "datasets are still required for a scientific wrong-fix rate."
            ),
            "raw_ambiguity_note": (
                "Raw undifferenced ambiguity fractions are datum-dependent. "
                "Satellite single differences and their wide lanes cancel "
                "the receiver ambiguity datum."
            ),
        },
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("trace", type=Path)
    parser.add_argument("pos", type=Path)
    parser.add_argument("--station", required=True)
    parser.add_argument(
        "--category",
        choices=("inside", "edge", "outside"),
        required=True,
    )
    parser.add_argument("--signals", nargs=2, default=("L1C", "L2W"))
    parser.add_argument("--reference")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    result = analyse(
        args.trace,
        args.pos,
        args.station,
        args.category,
        tuple(args.signals),
        args.reference,
    )
    text = json.dumps(result, indent=2, ensure_ascii=False, allow_nan=False)
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(text + "\n", encoding="utf-8")
    print(text)


if __name__ == "__main__":
    main()
