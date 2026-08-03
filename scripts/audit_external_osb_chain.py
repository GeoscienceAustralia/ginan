#!/usr/bin/env python3
"""Numerically audit BIA units and the correction applied in a receiver trace."""

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path


CLIGHT_M_PER_NS = 0.299792458
COMPONENT_RE = re.compile(
    r"(?P<kind>PHAS_MEAS|CODE_MEAS)\s+"
    r"(?P<sat>G\d{2})\s+\S+\s+(?P<signal>[CL]\d\w)\s+"
    r"SAT_(?P<bias>PHASE|CODE)_BIAS\s+"
    r"(?P<applied>[+-]?\d+(?:\.\d+)?)"
)


def bias_key(satellite: str, signal: str) -> tuple[str, str, str]:
    return (
        satellite,
        "CODE" if signal.startswith("C") else "PHASE",
        signal[1:],
    )


def read_bia(
    path: Path, signals: set[str]
) -> dict[tuple[str, str, str], tuple[str, float]]:
    values = {}
    with path.open(encoding="utf-8", errors="replace") as stream:
        for line in stream:
            fields = line.split()
            if (
                len(fields) < 8
                or fields[0] != "OSB"
                or not fields[2].startswith("G")
                or fields[3] not in signals
            ):
                continue
            if fields[6] != "ns":
                raise RuntimeError(
                    f"Expected ns for {fields[2]} {fields[3]}, got {fields[6]}"
                )
            values[bias_key(fields[2], fields[3])] = (
                fields[3],
                float(fields[7]),
            )
    return values


def read_components(
    path: Path, signals: set[str]
) -> dict[tuple[str, str, str], tuple[str, float]]:
    values = {}
    with path.open(encoding="utf-8", errors="replace") as stream:
        for line in stream:
            match = COMPONENT_RE.search(line)
            if not match or match["signal"] not in signals:
                continue
            key = (
                match["sat"],
                match["bias"],
                match["signal"][1:],
            )
            values.setdefault(
                key, (match["signal"], float(match["applied"]))
            )
    return values


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("bia", type=Path)
    parser.add_argument("receiver_trace", type=Path)
    parser.add_argument(
        "--signals", nargs="+", default=("C1C", "C2W", "L1C", "L2W")
    )
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    signals = set(args.signals)
    bia = read_bia(args.bia, signals)
    applied = read_components(args.receiver_trace, signals)
    comparisons = []
    for key in sorted(bia.keys() & applied.keys()):
        bia_signal, bia_value = bia[key]
        trace_signal, trace_value = applied[key]
        expected = -bia_value * CLIGHT_M_PER_NS
        comparisons.append(
            {
                "satellite": key[0],
                "bias_type": key[1],
                "bia_signal": bia_signal,
                "trace_signal": trace_signal,
                "bia_ns": bia_value,
                "expected_observation_correction_m": expected,
                "trace_observation_correction_m": trace_value,
                "difference_m": trace_value - expected,
            }
        )
    max_abs = max(
        (abs(item["difference_m"]) for item in comparisons), default=None
    )
    result = {
        "bia": str(args.bia),
        "receiver_trace": str(args.receiver_trace),
        "unit": "BIA ns converted with c/1e9 = 0.299792458 m/ns",
        "sign": (
            "Ginan observation correction equals negative of the BIA OSB "
            "value after conversion to metres"
        ),
        "bia_entries": len(bia),
        "trace_entries": len(applied),
        "matched_entries": len(comparisons),
        "maximum_abs_difference_m": max_abs,
        "comparisons": comparisons,
    }
    text = json.dumps(result, indent=2, ensure_ascii=False)
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(text + "\n", encoding="utf-8")
    print(text)


if __name__ == "__main__":
    main()
