#!/usr/bin/env python3
"""Aggregate the four independent E9 user cases for 20 held-out stations."""

from __future__ import annotations

import argparse
import csv
import json
import math
import re
import statistics
from collections import Counter
from pathlib import Path


DIAGNOSTIC = re.compile(
    r"ZHANG_USER_DIAGNOSTIC .*?time=(?P<date>\d{4}-\d{2}-\d{2}) "
    r"(?P<clock>\d{2}:\d{2}:\d{2}).*?"
    r"east_error_m=(?P<east>[-+0-9.eE]+) "
    r"north_error_m=(?P<north>[-+0-9.eE]+) "
    r"up_error_m=(?P<up>[-+0-9.eE]+)"
)
MODES = ("full_ar", "full_float", "restart_ar", "restart_float")


def rms(values: list[float]) -> float | None:
    if not values:
        return None
    return math.sqrt(statistics.fmean(value * value for value in values))


def diagnostics(path: Path) -> dict[str, tuple[float, float, float]]:
    result: dict[str, tuple[float, float, float]] = {}
    for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
        match = DIAGNOSTIC.search(line)
        if not match:
            continue
        result[f'{match["date"]}T{match["clock"]}'] = (
            float(match["east"]),
            float(match["north"]),
            float(match["up"]),
        )
    return result


def coordinate_delta(
    first: dict[str, tuple[float, float, float]],
    second: dict[str, tuple[float, float, float]],
) -> dict[str, float | int | None]:
    common = sorted(first.keys() & second.keys())
    deltas = [
        tuple(a - b for a, b in zip(first[epoch], second[epoch]))
        for epoch in common
    ]
    return {
        "matched_epochs": len(common),
        "east_rms_m": rms([value[0] for value in deltas]),
        "north_rms_m": rms([value[1] for value in deltas]),
        "up_rms_m": rms([value[2] for value in deltas]),
        "horizontal_rms_m": rms(
            [math.hypot(value[0], value[1]) for value in deltas]
        ),
    }


def aggregate_component(
    records: list[dict[str, object]], mode: str, component: str
) -> dict[str, float | None]:
    fields = ("east_rms_m", "north_rms_m", "up_rms_m", "horizontal_p95_m", "up_abs_p95_m")
    result: dict[str, float | None] = {}
    for field in fields:
        field_values = [
            float(record[mode][component][field])
            for record in records
            if record.get(mode) is not None
            if record[mode][component][field] is not None
        ]
        result[f"median_{field}"] = (
            statistics.median(field_values) if field_values else None
        )
    return result


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--manifest", type=Path, required=True)
    parser.add_argument("--result-root", type=Path, required=True)
    parser.add_argument("--trace-root", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()

    with args.manifest.open(newline="", encoding="utf-8") as stream:
        manifest = list(csv.DictReader(stream))

    stations: list[dict[str, object]] = []
    failed_cases: list[dict[str, str]] = []
    for item in manifest:
        station = item["station"]
        station_result: dict[str, object] = {
            "station": station,
            "region": item["region"],
        }
        traces: dict[str, dict[str, tuple[float, float, float]]] = {}
        for mode in MODES:
            result_path = args.result_root / f"{station}_{mode}.json"
            if not result_path.exists():
                station_result[mode] = None
                failed_cases.append({"station": station, "mode": mode})
                continue
            result = json.loads(result_path.read_text(encoding="utf-8"))
            station_result[mode] = result
            description = f"zhang_e9_{station}_{mode}"
            trace_path = (
                args.trace_root / description /
                f"Network-{description}-201919900.TRACE"
            )
            traces[mode] = diagnostics(trace_path)

        comparisons = (
            ("restart_ar_vs_full_ar", "restart_ar", "full_ar"),
            ("restart_float_vs_full_float", "restart_float", "full_float"),
            ("full_ar_vs_full_float", "full_ar", "full_float"),
        )
        for label, first, second in comparisons:
            station_result[label] = (
                coordinate_delta(traces[first], traces[second])
                if first in traces and second in traces
                else None
            )
        stations.append(station_result)

    fixed_stations = [
        record for record in stations
        if record["full_ar"] is not None
        and record["full_ar"]["newly_fixed_total"] > 0
    ]
    restart_fixed_stations = [
        record for record in stations
        if record["restart_ar"] is not None
        and record["restart_ar"]["newly_fixed_total"] > 0
    ]
    summary = {
        "station_count": len(stations),
        "region_counts": dict(Counter(item["region"] for item in manifest)),
        "modes": list(MODES),
        "completed_case_count": len(stations) * len(MODES) - len(failed_cases),
        "failed_cases": failed_cases,
        "full_ar_fixed_station_count": len(fixed_stations),
        "restart_ar_fixed_station_count": len(restart_fixed_stations),
        "full_ar_total_newly_fixed": sum(
            record["full_ar"]["newly_fixed_total"] for record in stations
            if record["full_ar"] is not None
        ),
        "restart_ar_total_newly_fixed": sum(
            record["restart_ar"]["newly_fixed_total"] for record in stations
            if record["restart_ar"] is not None
        ),
        "full_ar_wrong_fix_position_proxy_count": sum(
            record["full_ar"]["wrong_fix_position_proxy_count"]
            for record in stations
            if record["full_ar"] is not None
        ),
        "restart_ar_wrong_fix_position_proxy_count": sum(
            record["restart_ar"]["wrong_fix_position_proxy_count"]
            for record in stations
            if record["restart_ar"] is not None
        ),
        "full_ar_coordinate_medians": aggregate_component(
            stations, "full_ar", "enu_all"
        ),
        "full_float_coordinate_medians": aggregate_component(
            stations, "full_float", "enu_all"
        ),
        "restart_ar_coordinate_medians": aggregate_component(
            stations, "restart_ar", "enu_all"
        ),
        "restart_float_coordinate_medians": aggregate_component(
            stations, "restart_float", "enu_all"
        ),
        "stations": stations,
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    text = json.dumps(summary, indent=2, ensure_ascii=False, allow_nan=False)
    args.output.write_text(text + "\n", encoding="utf-8")
    print(text)


if __name__ == "__main__":
    main()
