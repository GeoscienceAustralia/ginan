#!/usr/bin/env python3
"""Fail-closed parser for the frozen R-Q0/R-Q1/R-Q2 TRACE evidence."""

from __future__ import annotations

import argparse
import json
import math
import re
from pathlib import Path


def fields(line: str) -> dict[str, str]:
    return dict(re.findall(r"([A-Za-z0-9_]+)=([^\s]+)", line))


def integer(item: dict[str, str], key: str) -> int:
    return int(item[key])


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("trace", type=Path)
    parser.add_argument("--epoch", default="2024-07-17 00:15:30")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    quotient: list[dict[str, str]] = []
    pair_sources: dict[str, int] = {}
    certified_graph: dict[str, str] | None = None
    current_epoch_lattices: list[dict[str, str]] = []
    with args.trace.open(errors="replace") as stream:
        for raw in stream:
            if args.epoch not in raw:
                continue
            if "ZHANG_PRODUCT_COMPONENT_QUOTIENT_IAR" in raw:
                quotient.append(fields(raw))
            elif "ZHANG_CURRENT_EPOCH_CERTIFIED_LATTICE" in raw:
                current_epoch_lattices.append(fields(raw))
            elif "ZHANG_PRODUCT_RELATION_PAIR_FLOAT" in raw:
                source = fields(raw).get("evidence_source", "MISSING")
                pair_sources[source] = pair_sources.get(source, 0) + 1
            elif "ZHANG_ACTUAL_CERTIFIED_PRODUCT_GRAPH_SUMMARY" in raw:
                certified_graph = fields(raw)

    errors: list[str] = []
    for item in quotient:
        target = integer(item, "target_rank")
        held = integer(item, "held_rank")
        unresolved = integer(item, "quotient_rank")
        covariance = integer(item, "quotient_covariance_rank")
        newly_fixed = integer(item, "newly_fixed_rank")
        combined = integer(item, "combined_certified_rank")
        persistent = integer(item, "persistent_held_intersection_rank")
        current = integer(item, "current_certified_increment_rank")
        if target != held + unresolved:
            errors.append(f"component {item.get('component')}: target != held + quotient")
        if item.get("status") not in {
            "UNTRACKED_DETERMINISTIC_RELATION",
            "DETERMINISTIC_INTEGER_INCONSISTENCY",
        } and covariance != unresolved:
            errors.append(f"component {item.get('component')}: quotient covariance rank mismatch")
        if combined < held or combined > target:
            errors.append(f"component {item.get('component')}: invalid combined certified rank")
        if held != persistent + current:
            errors.append(
                f"component {item.get('component')}: held/certified provenance rank mismatch"
            )
        if newly_fixed > unresolved:
            errors.append(f"component {item.get('component')}: fixed rank exceeds quotient")
        if item.get("certified") == "1" and combined != target:
            errors.append(f"component {item.get('component')}: false full certificate")

    if not quotient:
        errors.append("no quotient audit records at requested epoch")
    if "EXACT_DERIVED_OR_HELD" in pair_sources:
        errors.append("obsolete variance-derived provenance remains")
    if pair_sources.get("ZERO_VARIANCE_NUMERICAL", 0) and not (
        pair_sources.get("EXACT_HELD_CONSEQUENCE", 0)
        or pair_sources.get("CURRENT_FLOAT", 0)
    ):
        errors.append("pair provenance classification is degenerate")
    if certified_graph is None:
        errors.append("missing actual certified product graph summary")
    if not current_epoch_lattices:
        errors.append("missing current-epoch certified lattice provenance")
    elif any(item.get("valid") != "1" for item in current_epoch_lattices):
        errors.append("invalid current-epoch certified physical lattice")

    report = {
        "epoch": args.epoch,
        "trace": str(args.trace),
        "quotient_components": quotient,
        "pair_evidence_sources": pair_sources,
        "actual_certified_graph": certified_graph,
        "current_epoch_certified_lattices": current_epoch_lattices,
        "errors": errors,
        "valid": not errors,
    }
    text = json.dumps(report, ensure_ascii=False, indent=2)
    if args.output:
        args.output.write_text(text + "\n", encoding="utf-8")
    print(text)
    return 0 if not errors else 1


if __name__ == "__main__":
    raise SystemExit(main())
