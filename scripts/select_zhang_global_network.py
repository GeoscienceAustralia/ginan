#!/usr/bin/env python3
"""Audit and select a globally balanced Zhang phase-product network.

The selector is intentionally tied to the first GPS L1C/L2W prototype:

* require C1C/L1C and C2W/L2W in the RINEX header;
* require at least 95 percent of the nominal daily epochs;
* reserve an independent validation set before selecting estimation stations;
* prefer the documented frame/coverage anchors;
* enforce a configurable cap in dense 15 by 15 degree cells;
* fill regional quotas before using remaining globally ranked stations.

It writes plain station lists and a machine-readable audit.  It does not claim
that header/epoch checks replace Ginan's observation-, slip-, metadata-, or
graph-level quality control during estimation.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import csv
import json
import math
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Iterable


REQUIRED_OBSERVABLES = {"C1C", "L1C", "C2W", "L2W"}

# Fixed before product estimation so validation stations cannot leak into the
# service solution.  Available members give continuity with the 2019
# experiment; missing slots are filled from the audited 2024 population before
# any service receiver is selected.
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
    sinex_coordinate_available: bool
    antenna_type: str
    antenna_calibration_available: bool
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


def parse_header(
    path: Path,
) -> tuple[set[str], tuple[float, float, float] | None, str]:
    gps_observables: list[str] = []
    expected_gps_observables: int | None = None
    position = None
    antenna_type = ""

    with path.open("rt", errors="replace") as stream:
        for line in stream:
            label = line[60:80].strip() if len(line) >= 60 else ""
            if label == "APPROX POSITION XYZ":
                try:
                    position = tuple(float(value) for value in line[:60].split()[:3])
                except (TypeError, ValueError):
                    position = None
            elif label == "ANT # / TYPE":
                antenna_type = line[20:40].rstrip()

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
    return set(gps_observables), position, antenna_type


def count_rinex3_epochs(path: Path) -> int:
    count = 0
    with path.open("rb") as stream:
        for line in stream:
            count += line.startswith(b">")
    return count


def parse_sinex_coordinate_stations(path: Path) -> set[str]:
    """Return sites having a complete STAX/STAY/STAZ estimate triplet."""
    components: dict[str, set[str]] = {}
    in_estimate = False
    with path.open("rt", errors="replace") as stream:
        for line in stream:
            if line.startswith("+SOLUTION/ESTIMATE"):
                in_estimate = True
                continue
            if line.startswith("-SOLUTION/ESTIMATE"):
                break
            if not in_estimate or not line or line[0] in "*%":
                continue
            fields = line.split()
            if len(fields) < 3 or fields[1] not in {"STAX", "STAY", "STAZ"}:
                continue
            components.setdefault(fields[2].upper(), set()).add(fields[1])
    return {
        station
        for station, available in components.items()
        if {"STAX", "STAY", "STAZ"}.issubset(available)
    }


def parse_antex_receiver_types(path: Path) -> set[str]:
    antenna_types = set()
    with path.open("rt", errors="replace") as stream:
        for line in stream:
            if len(line) >= 80 and line[60:80].strip() == "TYPE / SERIAL NO":
                antenna_type = line[:20].rstrip()
                if antenna_type and not antenna_type.startswith("BLOCK "):
                    antenna_types.add(antenna_type)
    return antenna_types


def antenna_calibration_available(
    antenna_type: str,
    calibrated_types: set[str] | None,
) -> bool:
    if calibrated_types is None:
        return True
    if not antenna_type:
        return False
    if antenna_type in calibrated_types:
        return True
    # Ginan intentionally permits a calibrated NONE-radome fallback for an
    # otherwise identical 16-character antenna model.
    return f"{antenna_type[:16]}NONE".rstrip() in calibrated_types


def audit_station(
    path: Path,
    expected_epochs: int,
    minimum_completeness: float,
    sinex_coordinate_stations: set[str] | None,
    calibrated_antenna_types: set[str] | None,
) -> StationAudit:
    observables, position, antenna_type = parse_header(path)
    epoch_count = count_rinex3_epochs(path)
    completeness = min(1.0, epoch_count / expected_epochs)
    missing = tuple(sorted(REQUIRED_OBSERVABLES - observables))

    latitude = float("nan")
    longitude = float("nan")
    if position is not None and len(position) == 3:
        latitude, longitude = ecef_to_geocentric(*position)

    station = path.name[:4].upper()
    sinex_coordinate_available = (
        sinex_coordinate_stations is None
        or station in sinex_coordinate_stations
    )
    calibration_available = antenna_calibration_available(
        antenna_type,
        calibrated_antenna_types,
    )
    reasons = []
    if missing:
        reasons.append("missing:" + ",".join(missing))
    if completeness < minimum_completeness:
        reasons.append(f"completeness:{completeness:.4f}")
    if not math.isfinite(latitude) or not math.isfinite(longitude):
        reasons.append("missing_approx_position")
    if not sinex_coordinate_available:
        reasons.append("sinex_coordinate_missing")
    if not calibration_available:
        reasons.append("antenna_calibration_missing")

    return StationAudit(
        station=station,
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
        sinex_coordinate_available=sinex_coordinate_available,
        antenna_type=antenna_type,
        antenna_calibration_available=calibration_available,
        usable=not reasons,
        rejection_reason=";".join(reasons),
    )


def cell_key(station: StationAudit) -> tuple[int, int]:
    return (
        math.floor((station.latitude_deg + 90) / 15),
        math.floor((station.longitude_deg + 180) / 15),
    )


def great_circle_km(left: StationAudit, right: StationAudit) -> float:
    lat1 = math.radians(left.latitude_deg)
    lat2 = math.radians(right.latitude_deg)
    dlat = lat2 - lat1
    dlon = math.radians(right.longitude_deg - left.longitude_deg)
    value = (
        math.sin(dlat / 2) ** 2
        + math.cos(lat1) * math.cos(lat2) * math.sin(dlon / 2) ** 2
    )
    return 6371.0088 * 2 * math.asin(min(1.0, math.sqrt(value)))


def network_geometry(
    names: list[str],
    audits: dict[str, StationAudit],
) -> dict:
    nearest = []
    for name in names:
        distances = [
            great_circle_km(audits[name], audits[other])
            for other in names if other != name
        ]
        if distances:
            nearest.append(min(distances))
    nearest.sort()
    percentile = lambda probability: nearest[
        min(len(nearest) - 1, round(probability * (len(nearest) - 1)))
    ] if nearest else None
    latitude_bands: dict[str, int] = {}
    longitude_sectors: dict[str, int] = {}
    for name in names:
        station = audits[name]
        lat_lower = int(math.floor((station.latitude_deg + 90) / 30) * 30 - 90)
        lon_lower = int(math.floor((station.longitude_deg + 180) / 30) * 30 - 180)
        latitude_bands[f"{lat_lower:+03d}:{lat_lower + 30:+03d}"] = (
            latitude_bands.get(f"{lat_lower:+03d}:{lat_lower + 30:+03d}", 0) + 1
        )
        longitude_sectors[f"{lon_lower:+04d}:{lon_lower + 30:+04d}"] = (
            longitude_sectors.get(f"{lon_lower:+04d}:{lon_lower + 30:+04d}", 0) + 1
        )
    return {
        "nearest_neighbor_km": {
            "minimum": nearest[0] if nearest else None,
            "median": percentile(0.5),
            "p95": percentile(0.95),
        },
        "latitude_band_counts": dict(sorted(latitude_bands.items())),
        "longitude_sector_counts": dict(sorted(longitude_sectors.items())),
    }


def choose_priority(
    names: Iterable[str],
    usable: dict[str, StationAudit],
    selected: list[str],
    excluded: set[str],
    cell_counts: dict[tuple[int, int], int],
    target: int,
    max_per_cell: int,
    minimum_separation_km: float,
) -> None:
    for name in names:
        if len(selected) >= target or name in selected or name in excluded:
            continue
        station = usable.get(name)
        if station is None:
            continue
        if minimum_separation_km > 0 and any(
            other in usable
            and great_circle_km(station, usable[other]) < minimum_separation_km
            for other in (*selected, *excluded)
        ):
            continue
        cell = cell_key(station)
        if cell_counts.get(cell, 0) >= max_per_cell:
            continue
        selected.append(name)
        cell_counts[cell] = cell_counts.get(cell, 0) + 1


def choose_maximin(
    names: Iterable[str],
    usable: dict[str, StationAudit],
    selected: list[str],
    excluded: set[str],
    cell_counts: dict[tuple[int, int], int],
    target: int,
    max_per_cell: int,
    minimum_separation_km: float,
) -> None:
    """Greedily fill the network by maximum distance from accepted sites."""
    remaining = set(names)
    while len(selected) < target:
        accepted = (*selected, *excluded)
        ranked: list[tuple[float, float, str]] = []
        for name in remaining:
            station = usable.get(name)
            if station is None or name in selected or name in excluded:
                continue
            if cell_counts.get(cell_key(station), 0) >= max_per_cell:
                continue
            nearest = min(
                (
                    great_circle_km(station, usable[other])
                    for other in accepted
                    if other in usable
                ),
                default=float("inf"),
            )
            if nearest < minimum_separation_km:
                continue
            ranked.append((nearest, station.completeness, name))
        if not ranked:
            return
        _, _, chosen = max(ranked)
        selected.append(chosen)
        cell = cell_key(usable[chosen])
        cell_counts[cell] = cell_counts.get(cell, 0) + 1
        remaining.remove(chosen)


def select_network(
    audits: list[StationAudit],
    estimation_count: int,
    validation_count: int,
    max_per_cell: int,
    minimum_separation_km: float,
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
        minimum_separation_km,
    )

    quota_total = sum(REGION_TARGETS.values())
    for region, base_quota in REGION_TARGETS.items():
        quota = round(estimation_count * base_quota / quota_total)
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
        choose_maximin(
            candidates,
            usable,
            estimation,
            excluded,
            cell_counts,
            min(estimation_count, len(estimation) + need),
            max_per_cell,
            minimum_separation_km,
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
    choose_maximin(
        remaining,
        usable,
        estimation,
        excluded,
        cell_counts,
        estimation_count,
        max_per_cell,
        minimum_separation_km,
    )

    backup = sorted(
        station
        for station in usable
        if station not in estimation and station not in validation
    )
    return estimation, backup, validation


def write_station_list(path: Path, stations: list[str]) -> None:
    path.write_text("\n".join(stations) + "\n", encoding="utf-8", newline="\n")


def write_station_manifest(
    path: Path,
    stations: list[str],
    audits: dict[str, StationAudit],
) -> None:
    with path.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.writer(stream, lineterminator="\n")
        writer.writerow(("station", "filename", "region", "latitude", "longitude"))
        for name in stations:
            station = audits[name]
            writer.writerow((
                name,
                station.filename,
                station.region,
                f"{station.latitude_deg:.6f}",
                f"{station.longitude_deg:.6f}",
            ))


def write_input_overlay(
    path: Path,
    stations: list[str],
    audits: dict[str, StationAudit],
    observations_root: str,
    role: str = "estimation",
) -> None:
    lines = [
        "# Generated by scripts/select_zhang_global_network.py.",
        (
            "# Validation and backup stations are deliberately absent."
            if role == "estimation"
            else "# Estimation and backup stations are deliberately absent."
        ),
        "inputs:",
        "  gnss_observations:",
        f"    gnss_observations_root: {observations_root}",
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
    parser.add_argument("--minimum-separation-km", type=float, default=0)
    parser.add_argument(
        "--sinex-coordinate-file",
        type=Path,
        help=(
            "Require every selected station to have a complete STAX/STAY/STAZ "
            "triplet in this SINEX SOLUTION/ESTIMATE block"
        ),
    )
    parser.add_argument(
        "--antex-file",
        type=Path,
        help=(
            "Require an exact or NONE-radome receiver antenna calibration "
            "in this ANTEX file"
        ),
    )
    parser.add_argument("--output-directory", type=Path, required=True)
    parser.add_argument(
        "--input-overlay-name",
        default="zhang_global_2019199_inputs.yaml",
    )
    parser.add_argument(
        "--validation-overlay-name",
        default="",
        help=(
            "Optional YAML filename containing only the independent validation "
            "RINEX inputs and receiver enables"
        ),
    )
    parser.add_argument("--observations-root", default="../data/")
    parser.add_argument(
        "--scope",
        default="GPS L1C/L2W first global Zhang internal-product prototype",
    )
    args = parser.parse_args()

    paths = sorted(args.rinex_directory.glob(args.pattern))
    if not paths:
        raise SystemExit(f"No RINEX files matched {args.pattern!r}")

    sinex_coordinate_stations = None
    if args.sinex_coordinate_file is not None:
        if not args.sinex_coordinate_file.is_file():
            raise SystemExit(
                f"SINEX coordinate file not found: {args.sinex_coordinate_file}"
            )
        sinex_coordinate_stations = parse_sinex_coordinate_stations(
            args.sinex_coordinate_file
        )
        if not sinex_coordinate_stations:
            raise SystemExit(
                "No complete STAX/STAY/STAZ coordinate triplets were found in "
                f"{args.sinex_coordinate_file}"
            )

    calibrated_antenna_types = None
    if args.antex_file is not None:
        if not args.antex_file.is_file():
            raise SystemExit(f"ANTEX file not found: {args.antex_file}")
        calibrated_antenna_types = parse_antex_receiver_types(args.antex_file)
        if not calibrated_antenna_types:
            raise SystemExit(
                f"No receiver antenna calibrations found in {args.antex_file}"
            )

    with concurrent.futures.ThreadPoolExecutor(max_workers=12) as executor:
        audits = list(
            executor.map(
                lambda path: audit_station(
                    path,
                    args.expected_epochs,
                    args.minimum_completeness,
                    sinex_coordinate_stations,
                    calibrated_antenna_types,
                ),
                paths,
            )
        )

    estimation, backup, validation = select_network(
        audits,
        args.estimation_count,
        args.validation_count,
        args.max_per_cell,
        args.minimum_separation_km,
    )
    if len(estimation) != args.estimation_count:
        raise SystemExit(
            f"Only {len(estimation)} estimation stations passed the spatial "
            f"selection; {args.estimation_count} were required"
        )
    if len(validation) != args.validation_count:
        raise SystemExit(
            f"Only {len(validation)} validation stations passed; "
            f"{args.validation_count} were required"
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
    write_station_manifest(
        args.output_directory / "network_estimation_manifest.csv",
        estimation,
        audit_by_name,
    )
    write_station_manifest(
        args.output_directory / "validation_manifest.csv",
        validation,
        audit_by_name,
    )
    write_input_overlay(
        args.output_directory / args.input_overlay_name,
        estimation,
        audit_by_name,
        args.observations_root,
    )
    if args.validation_overlay_name:
        write_input_overlay(
            args.output_directory / args.validation_overlay_name,
            validation,
            audit_by_name,
            args.observations_root,
            role="validation",
        )
    region_counts = {
        region: sum(audit_by_name[name].region == region for name in estimation)
        for region in REGION_TARGETS
    }
    report = {
        "scope": args.scope,
        "rinex_directory": str(args.rinex_directory),
        "hard_gates": {
            "required_observables": sorted(REQUIRED_OBSERVABLES),
            "minimum_completeness": args.minimum_completeness,
            "expected_epochs": args.expected_epochs,
            "minimum_separation_km": args.minimum_separation_km,
            "sinex_coordinate_file": (
                str(args.sinex_coordinate_file)
                if args.sinex_coordinate_file is not None
                else None
            ),
            "antex_file": (
                str(args.antex_file) if args.antex_file is not None else None
            ),
        },
        "selection": {
            "estimation": estimation,
            "backup": backup,
            "validation": validation,
            "region_counts": region_counts,
            "geometry": network_geometry(estimation, audit_by_name),
            "validation_geometry": network_geometry(validation, audit_by_name),
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
