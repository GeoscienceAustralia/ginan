#!/usr/bin/env python3
"""Audit and select a globally balanced Zhang phase-product network.

The selector is intentionally tied to the first GPS L1C/L2W prototype:

* require C1C/L1C and C2W/L2W in the RINEX header;
* require at least 95 percent of the nominal daily epochs;
* reserve an independent validation set before selecting estimation stations;
* prefer the documented frame/coverage anchors;
* limit dense 15 by 15 degree cells to two estimation stations;
* fill regional quotas before using remaining globally ranked stations.

It writes plain station lists and a machine-readable audit.  It does not claim
that header/epoch checks replace Ginan's observation-, slip-, metadata-, or
graph-level quality control during estimation.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import json
import math
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Iterable


REQUIRED_OBSERVABLES = {"C1C", "L1C", "C2W", "L2W"}

# Fixed before product estimation so validation stations cannot leak into the
# service solution.  The list spans all seven regions represented in the
# 2019-199 data set.
VALIDATION_PRIORITY = [
    "MARS", "DYNG", "NICO",
    "BREW", "JPLM",
    "BOGT", "GLPS", "POAL",
    "DJIG", "MBAR", "SEY2",
    "GMSD", "USUD",
    "CEDU", "GAMB", "MRO1", "TONG",
    "CAS1", "DUMG", "PALM",
]

# Available representatives of the anchors recommended in the supplied
# station-selection plan, followed by strong substitutes in weak regions.
ANCHOR_PRIORITY = [
    "MATE", "STJ3", "TRO1", "MAS1", "REYK", "ZIM2",
    "FAIR", "YEL2", "KOKV", "USN7", "BAKE",
    "AREQ", "CRO1", "FALK", "RGDG", "KOUG",
    "CPVG", "NKLG", "DGAR", "KERG", "VACS", "SUTM",
    "IISC", "ULAB", "BAKO", "JFNG", "LHAZ",
    "DARW", "HOB2", "KIRI", "MAJU", "POHN", "LAUT", "NNOR",
    "DAV1", "MAW1", "OHI3",
]

REGION_TARGETS = {
    "europe_north_atlantic": 12,
    "north_america_arctic": 9,
    "south_america_caribbean": 11,
    "africa_middle_east_indian": 11,
    "central_east_south_asia": 10,
    "australia_pacific": 14,
    "antarctic_southern_ocean": 7,
}


@dataclass(frozen=True)
class StationAudit:
    station: str
    filename: str
    epoch_count: int
    completeness: float
    gps_observables: tuple[str, ...]
    missing_observables: tuple[str, ...]
    latitude_deg: float
    longitude_deg: float
    region: str
    usable: bool
    rejection_reason: str


def ecef_to_geocentric(x: float, y: float, z: float) -> tuple[float, float]:
    longitude = math.degrees(math.atan2(y, x))
    latitude = math.degrees(math.atan2(z, math.hypot(x, y)))
    return latitude, longitude


def classify_region(latitude: float, longitude: float) -> str:
    if latitude <= -55:
        return "antarctic_southern_ocean"
    if -30 <= longitude <= 65 and latitude >= 25:
        return "europe_north_atlantic"
    if -170 <= longitude < -30 and latitude >= 25:
        return "north_america_arctic"
    if -120 <= longitude < -30 and latitude < 25:
        return "south_america_caribbean"
    if 65 < longitude <= 150 and latitude >= -15:
        return "central_east_south_asia"
    if -30 <= longitude <= 90 and latitude < 25:
        return "africa_middle_east_indian"
    return "australia_pacific"


def parse_header(path: Path) -> tuple[set[str], tuple[float, float, float] | None]:
    gps_observables: list[str] = []
    expected_gps_observables: int | None = None
    position = None

    with path.open("rt", errors="replace") as stream:
        for line in stream:
            label = line[60:80].strip() if len(line) >= 60 else ""
            if label == "APPROX POSITION XYZ":
                try:
                    position = tuple(float(value) for value in line[:60].split()[:3])
                except (TypeError, ValueError):
                    position = None

            if label == "SYS / # / OBS TYPES":
                system = line[0:1]
                if system == "G":
                    try:
                        expected_gps_observables = int(line[3:6])
                    except ValueError:
                        expected_gps_observables = None
                    gps_observables.extend(line[7:60].split())
                elif (
                    system == " "
                    and expected_gps_observables is not None
                    and len(gps_observables) < expected_gps_observables
                ):
                    gps_observables.extend(line[7:60].split())

            if "END OF HEADER" in line:
                break

    if expected_gps_observables is not None:
        gps_observables = gps_observables[:expected_gps_observables]
    return set(gps_observables), position


def count_rinex3_epochs(path: Path) -> int:
    count = 0
    with path.open("rb") as stream:
        for line in stream:
            count += line.startswith(b">")
    return count


def audit_station(
    path: Path,
    expected_epochs: int,
    minimum_completeness: float,
) -> StationAudit:
    observables, position = parse_header(path)
    epoch_count = count_rinex3_epochs(path)
    completeness = min(1.0, epoch_count / expected_epochs)
    missing = tuple(sorted(REQUIRED_OBSERVABLES - observables))

    latitude = float("nan")
    longitude = float("nan")
    if position is not None and len(position) == 3:
        latitude, longitude = ecef_to_geocentric(*position)

    reasons = []
    if missing:
        reasons.append("missing:" + ",".join(missing))
    if completeness < minimum_completeness:
        reasons.append(f"completeness:{completeness:.4f}")
    if not math.isfinite(latitude) or not math.isfinite(longitude):
        reasons.append("missing_approx_position")

    return StationAudit(
        station=path.name[:4].upper(),
        filename=path.name,
        epoch_count=epoch_count,
        completeness=completeness,
        gps_observables=tuple(sorted(observables)),
        missing_observables=missing,
        latitude_deg=latitude,
        longitude_deg=longitude,
        region=(
            classify_region(latitude, longitude)
            if math.isfinite(latitude) and math.isfinite(longitude)
            else "unknown"
        ),
        usable=not reasons,
        rejection_reason=";".join(reasons),
    )


def cell_key(station: StationAudit) -> tuple[int, int]:
    return (
        math.floor((station.latitude_deg + 90) / 15),
        math.floor((station.longitude_deg + 180) / 15),
    )


def choose_priority(
    names: Iterable[str],
    usable: dict[str, StationAudit],
    selected: list[str],
    excluded: set[str],
    cell_counts: dict[tuple[int, int], int],
    target: int,
    max_per_cell: int,
) -> None:
    for name in names:
        if len(selected) >= target or name in selected or name in excluded:
            continue
        station = usable.get(name)
        if station is None:
            continue
        cell = cell_key(station)
        if cell_counts.get(cell, 0) >= max_per_cell:
            continue
        selected.append(name)
        cell_counts[cell] = cell_counts.get(cell, 0) + 1


def select_network(
    audits: list[StationAudit],
    estimation_count: int,
    validation_count: int,
    max_per_cell: int,
) -> tuple[list[str], list[str], list[str]]:
    usable = {station.station: station for station in audits if station.usable}

    validation = [
        station
        for station in VALIDATION_PRIORITY
        if station in usable
    ][:validation_count]
    if len(validation) < validation_count:
        remaining = sorted(
            (station for station in usable if station not in validation),
            key=lambda name: (
                sum(
                    1
                    for chosen in validation
                    if usable[chosen].region == usable[name].region
                ),
                -usable[name].completeness,
                name,
            ),
        )
        validation.extend(remaining[:validation_count - len(validation)])

    excluded = set(validation)
    estimation: list[str] = []
    cell_counts: dict[tuple[int, int], int] = {}
    choose_priority(
        ANCHOR_PRIORITY,
        usable,
        estimation,
        excluded,
        cell_counts,
        estimation_count,
        max_per_cell,
    )

    for region, quota in REGION_TARGETS.items():
        need = max(
            0,
            min(quota, estimation_count)
            - sum(usable[name].region == region for name in estimation),
        )
        candidates = sorted(
            (
                station.station
                for station in audits
                if station.usable
                and station.region == region
                and station.station not in excluded
                and station.station not in estimation
            ),
            key=lambda name: (-usable[name].completeness, name),
        )
        choose_priority(
            candidates[:need * 3],
            usable,
            estimation,
            excluded,
            cell_counts,
            min(estimation_count, len(estimation) + need),
            max_per_cell,
        )

    remaining = sorted(
        (
            station.station
            for station in audits
            if station.usable
            and station.station not in excluded
            and station.station not in estimation
        ),
        key=lambda name: (
            sum(usable[item].region == usable[name].region for item in estimation),
            -usable[name].completeness,
            name,
        ),
    )
    choose_priority(
        remaining,
        usable,
        estimation,
        excluded,
        cell_counts,
        estimation_count,
        max_per_cell,
    )

    backup = sorted(
        station
        for station in usable
        if station not in estimation and station not in validation
    )
    return estimation, backup, validation


def write_station_list(path: Path, stations: list[str]) -> None:
    path.write_text("\n".join(stations) + "\n", encoding="utf-8", newline="\n")


def write_input_overlay(
    path: Path,
    stations: list[str],
    audits: dict[str, StationAudit],
) -> None:
    lines = [
        "# Generated by scripts/select_zhang_global_network.py.",
        "# Validation and backup stations are deliberately absent.",
        "inputs:",
        "  gnss_observations:",
        "    gnss_observations_root: ../data/",
        "    rnx_inputs:",
    ]
    lines.extend(
        f"      - {audits[station].filename}"
        for station in stations
    )
    lines.extend(
        [
            "",
            "receiver_options:",
            "  global:",
            "    exclude: true",
        ]
    )
    lines.extend(f"  {station}: {{ exclude: false }}" for station in stations)
    path.write_text(
        "\n".join(lines) + "\n",
        encoding="utf-8",
        newline="\n",
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("rinex_directory", type=Path)
    parser.add_argument("--pattern", default="*_R_20191990000_01D_30S_MO.rnx")
    parser.add_argument("--expected-epochs", type=int, default=2880)
    parser.add_argument("--minimum-completeness", type=float, default=0.95)
    parser.add_argument("--estimation-count", type=int, default=76)
    parser.add_argument("--validation-count", type=int, default=20)
    parser.add_argument("--max-per-cell", type=int, default=2)
    parser.add_argument("--output-directory", type=Path, required=True)
    args = parser.parse_args()

    paths = sorted(args.rinex_directory.glob(args.pattern))
    if not paths:
        raise SystemExit(f"No RINEX files matched {args.pattern!r}")

    with concurrent.futures.ThreadPoolExecutor(max_workers=12) as executor:
        audits = list(
            executor.map(
                lambda path: audit_station(
                    path,
                    args.expected_epochs,
                    args.minimum_completeness,
                ),
                paths,
            )
        )

    estimation, backup, validation = select_network(
        audits,
        args.estimation_count,
        args.validation_count,
        args.max_per_cell,
    )

    args.output_directory.mkdir(parents=True, exist_ok=True)
    write_station_list(
        args.output_directory / "network_estimation.txt",
        estimation,
    )
    write_station_list(
        args.output_directory / "network_backup.txt",
        backup,
    )
    write_station_list(
        args.output_directory / "network_validation.txt",
        validation,
    )

    audit_by_name = {station.station: station for station in audits}
    write_input_overlay(
        args.output_directory / "zhang_global_2019199_inputs.yaml",
        estimation,
        audit_by_name,
    )
    region_counts = {
        region: sum(audit_by_name[name].region == region for name in estimation)
        for region in REGION_TARGETS
    }
    report = {
        "scope": "GPS L1C/L2W first global Zhang internal-product prototype",
        "rinex_directory": str(args.rinex_directory),
        "hard_gates": {
            "required_observables": sorted(REQUIRED_OBSERVABLES),
            "minimum_completeness": args.minimum_completeness,
            "expected_epochs": args.expected_epochs,
        },
        "selection": {
            "estimation": estimation,
            "backup": backup,
            "validation": validation,
            "region_counts": region_counts,
        },
        "counts": {
            "input": len(audits),
            "usable": sum(station.usable for station in audits),
            "rejected": sum(not station.usable for station in audits),
            "estimation": len(estimation),
            "backup": len(backup),
            "validation": len(validation),
        },
        "limitations": [
            "RINEX header and epoch completeness do not replace per-epoch signal quality checks.",
            "Receiver, antenna, firmware, SINEX interval, PSD, and ANTEX gates remain estimator inputs.",
            "Graph coverage and cycle rank are checked by the Zhang estimator at run time.",
        ],
        "stations": [asdict(station) for station in audits],
    }
    (args.output_directory / "network_selection_audit.json").write_text(
        json.dumps(report, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
        newline="\n",
    )

    print(json.dumps(report["counts"], sort_keys=True))
    print(json.dumps(region_counts, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
