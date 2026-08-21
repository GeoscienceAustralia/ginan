#!/usr/bin/env python3
"""Audit Phase-3 PRIVATE feedback without treating FLOAT changes as success."""

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path


FIELDS = re.compile(r"(?P<key>[A-Za-z0-9_]+)=(?P<value>[^\s]+)")
TIME = re.compile(r"\btime=(?P<time>\d{4}-\d{2}-\d{2}\s+\d{2}:\d{2}:\d{2})")


def fields(line: str) -> dict[str, str]:
    result = {item.group("key"): item.group("value") for item in FIELDS.finditer(line)}
    if match := TIME.search(line):
        result["time"] = match.group("time")
    return result


def number(row: dict[str, str], key: str) -> float | None:
    try:
        return float(row[key])
    except (KeyError, ValueError):
        return None


def integer(row: dict[str, str], key: str) -> int:
    try:
        return int(row.get(key, "0"))
    except ValueError:
        return 0


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("trace", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()

    float_closures: list[dict[str, str]] = []
    fixed_branches: list[dict[str, str]] = []
    component_blocks: list[dict[str, str]] = []
    epochs: list[str] = []
    with args.trace.open("r", errors="replace") as trace:
        for line in trace:
            if "ZHANG_AR_RUNTIME_CONFIG time=" in line:
                row = fields(line)
                if "time" in row:
                    epochs.append(row["time"])
            elif "ZHANG_FLOAT_AUTHORITY_CLOSURE time=" in line:
                float_closures.append(fields(line))
            elif "ZHANG_PRODUCT_FIXED_BRANCH time=" in line:
                fixed_branches.append(fields(line))
            elif "ZHANG_PRODUCT_COMPONENT_GAUGE_BLOCK time=" in line:
                component_blocks.append(fields(line))

    float_pass = bool(float_closures) and all(
        row.get("core_bitwise_equal") == "1"
        and number(row, "float_state_maximum_difference") == 0
        and number(row, "float_covariance_maximum_difference") == 0
        for row in float_closures
    )
    # The runtime emits one block record per independently solved component
    # and one PRODUCT_FIXED_BRANCH record per attempted final conditioning.
    # Do not rely on the obsolete, never-emitted *_EFFECT / *_PRIVATE tags.
    strict_rank_gain = any(integer(row, "new_dual_rank") > 0 for row in component_blocks)
    strict_pair_effects = [
        row for row in fixed_branches
        if row.get("status") == "CONDITIONED"
        and integer(row, "pair_certificate_rank") > 0
    ]
    # A product pair-trace effect is optional evidence: a component merge can
    # add an exact dual certificate while producing negligible new covariance
    # reduction because earlier mixed rows already conditioned the posterior.
    precision_gain = []
    result = {
        "trace": str(args.trace),
        "runtime_epoch_count": len(epochs),
        "last_runtime_epoch": epochs[-1] if epochs else None,
        "float_authority_closure_rows": len(float_closures),
        "float_authority_unchanged": float_pass,
        "private_component_gauge_rows": len(component_blocks),
        "strict_rank_gain": strict_rank_gain,
        "strict_pair_product_effect_rows": len(strict_pair_effects),
        "precision_gain_rows": len(precision_gain),
        "conditioned_product_fixed_rows": len(strict_pair_effects),
        "phase3_verdict": (
            "PASS_PRIVATE_WITH_STRICT_PRODUCT_GAIN" if float_pass and
            (strict_rank_gain or precision_gain) else
            "FAIL_FLOAT_AUTHORITY_CHANGED" if float_closures and not float_pass else
            "FAIL_NO_STRICT_PRODUCT_GAIN" if float_pass else
            "INCONCLUSIVE_NO_FLOAT_AUTHORITY_TRACE"
        ),
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
