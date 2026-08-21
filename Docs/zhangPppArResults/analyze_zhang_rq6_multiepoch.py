#!/usr/bin/env python3
"""Fail-closed R-Q6 quotient/component/provenance stability audit."""

from __future__ import annotations

import argparse
import datetime as dt
import json
import re
from collections import Counter, defaultdict
from pathlib import Path


FIELD = re.compile(r"([A-Za-z0-9_]+)=([^\s]+)")
EPOCH = re.compile(r"time=(2024-07-17 00:(?:1[0-6]):(?:00|30))")


def parse_fields(line: str) -> dict[str, str]:
    return dict(FIELD.findall(line))


def expected_epochs() -> list[str]:
    start = dt.datetime(2024, 7, 17, 0, 10)
    return [(start + dt.timedelta(seconds=30 * i)).strftime("%Y-%m-%d %H:%M:%S")
            for i in range(13)]


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("trace", type=Path)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    quotient: dict[str, list[dict[str, str]]] = defaultdict(list)
    graphs: dict[str, list[dict[str, str]]] = defaultdict(list)
    provenance: dict[str, Counter[str]] = defaultdict(Counter)
    frontiers: dict[str, list[dict[str, str]]] = defaultdict(list)
    forbidden = Counter()
    with args.trace.open(errors="replace") as stream:
        for line in stream:
            match = EPOCH.search(line)
            if not match:
                continue
            epoch = match.group(1)
            item = parse_fields(line)
            if "ZHANG_PRODUCT_COMPONENT_QUOTIENT_IAR" in line:
                quotient[epoch].append(item)
            elif "ZHANG_ACTUAL_CERTIFIED_PRODUCT_GRAPH_SUMMARY" in line:
                graphs[epoch].append(item)
            elif "ZHANG_PRODUCT_RELATION_PAIR_FLOAT" in line:
                provenance[epoch][item.get("evidence_source", "MISSING")] += 1
            elif "ZHANG_INTEGER_PRODUCT_GAIN_FRONTIER_POINT" in line:
                frontiers[epoch].append(item)
            if "feedback=1" in line or "ar_authorized=1" in line:
                forbidden[epoch] += 1

    errors: list[str] = []
    rank_signatures: dict[str, list[tuple[int, ...]]] = {}
    for epoch in expected_epochs():
        records = quotient.get(epoch, [])
        if not records:
            errors.append(f"{epoch}: missing quotient records")
        if not graphs.get(epoch):
            errors.append(f"{epoch}: missing certified graph summary")
        if not provenance.get(epoch):
            errors.append(f"{epoch}: missing provenance records")
        signatures = []
        for item in records:
            required = ("target_rank", "held_rank", "quotient_rank",
                        "quotient_covariance_rank", "newly_fixed_rank",
                        "combined_certified_rank")
            if any(key not in item for key in required):
                errors.append(f"{epoch}: incomplete six-rank record")
                continue
            values = tuple(int(item[key]) for key in required)
            target, held, unresolved, covariance, newly, combined = values
            signatures.append(values)
            if target != held + unresolved:
                errors.append(f"{epoch}: target != held + quotient")
            if item.get("status") not in {
                "UNTRACKED_DETERMINISTIC_RELATION",
                "DETERMINISTIC_INTEGER_INCONSISTENCY",
            } and covariance != unresolved:
                errors.append(f"{epoch}: quotient covariance rank mismatch")
            if newly > unresolved or not held <= combined <= target:
                errors.append(f"{epoch}: invalid fixed/combined rank")
        rank_signatures[epoch] = signatures
        if provenance[epoch].get("EXACT_DERIVED_OR_HELD"):
            errors.append(f"{epoch}: obsolete provenance label")
    if forbidden:
        errors.append("authoritative feedback/AR authorization occurred")

    report = {
        "trace": str(args.trace),
        "expected_epochs": expected_epochs(),
        "quotient_rank_signatures": rank_signatures,
        "provenance": {epoch: dict(counts) for epoch, counts in provenance.items()},
        "certified_graphs": graphs,
        "integer_gain_frontier_points": frontiers,
        "forbidden_feedback": dict(forbidden),
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
