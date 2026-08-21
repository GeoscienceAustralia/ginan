#!/usr/bin/env python3
"""Audit cross-generation transport and dual-product KPIs from a TRACE.

The audit deliberately separates network WL rank from the dual product graph:
``r_dual`` and ``J_pair_PRODUCT_FIXED`` come from certified pair ranks, while
component sizes come from PRODUCT_FIXED effect/component records.  Missing
events are reported as missing evidence, never interpreted as success.
"""

from __future__ import annotations

import argparse
import json
import re
from collections import defaultdict
from pathlib import Path


TIME_RE = re.compile(r"(?:^|\s)time=(\d{4}-\d\d-\d\d\s+\d\d:\d\d:\d\d)")
TOKEN_RE = re.compile(r"([A-Za-z][A-Za-z0-9_]*)=([^\s]+)")


def parse_line(line: str) -> tuple[str | None, dict[str, str]]:
    match = TIME_RE.search(line)
    return (match.group(1) if match else None,
            dict(TOKEN_RE.findall(line)))


def integer(values: dict[str, str], name: str, default: int = 0) -> int:
    try:
        return int(values.get(name, default))
    except (TypeError, ValueError):
        return default


def read_trace(path: Path) -> dict[str, object]:
    events: dict[str, list[dict[str, object]]] = defaultdict(list)
    timeline: dict[str, dict[str, object]] = defaultdict(
        lambda: {
            "r_dual": 0,
            "K_dual": 0,
            "largest_dual_component_size": 0,
            "dual_certified_satellite_count": 0,
            "J_pair_PRODUCT_FIXED": 0,
            "rank_sources": {"DIRECT": 0, "LEDGER": 0,
                              "COMPONENT": 0, "BESD": 0},
        }
    )
    with path.open("r", errors="replace") as stream:
        for line in stream:
            time, values = parse_line(line)
            if time is None:
                continue
            kind_match = re.search(r"(ZHANG_[A-Z0-9_]+)", line)
            if not kind_match:
                continue
            kind = kind_match.group(1)
            record = {"time": time, "kind": kind, **values}
            events[kind].append(record)
            row = timeline[time]
            if kind == "ZHANG_PRODUCT_FIXED_EFFECT":
                rank = integer(values, "pair_certificate_rank")
                row["r_dual"] = max(row["r_dual"], rank)
                row["J_pair_PRODUCT_FIXED"] = max(
                    row["J_pair_PRODUCT_FIXED"], rank)
                component_rank = integer(values, "component_rank")
                satellites = integer(values, "satellites")
                row["largest_dual_component_size"] = max(
                    row["largest_dual_component_size"],
                    max(component_rank + 1, satellites if component_rank else 0))
                if component_rank > 0 and satellites > 0:
                    row["dual_certified_satellite_count"] = max(
                        row["dual_certified_satellite_count"], satellites)
                if values.get("persistent_relation_known") in {"1", "true", "True"}:
                    row["rank_sources"]["LEDGER"] += rank
                else:
                    row["rank_sources"]["DIRECT"] += rank
            elif kind == "ZHANG_PRODUCT_INTEGER_LEDGER":
                row["r_dual"] = max(row["r_dual"],
                                    integer(values, "active_rank_after"))
                row["rank_sources"]["LEDGER"] = max(
                    row["rank_sources"]["LEDGER"],
                    integer(values, "active_rank_after"))
            elif kind == "ZHANG_HYBRID_PAIR_COMPONENT_SUMMARY":
                row["K_dual"] = max(row["K_dual"],
                                     integer(values, "usable_components"))
                row["dual_certified_satellite_count"] = max(
                    row["dual_certified_satellite_count"],
                    integer(values, "usable_satellites"))
                row["largest_dual_component_size"] = max(
                    row["largest_dual_component_size"],
                    integer(values, "usable_satellites"))

    ordered = []
    for time in sorted(timeline):
        row = dict(timeline[time])
        row["time"] = time
        ordered.append(row)

    transport = []
    for key in ("ZHANG_PRODUCT_INTEGER_LEDGER_TRANSPORT",
                "ZHANG_PRODUCT_INTEGER_LEDGER_SEGMENT_REJECT",
                "ZHANG_HYBRID_TREE_INVARIANCE",
                "ZHANG_HYBRID_REAL_GAUGE_TRANSACTION",
                "ZHANG_TEMPORAL_PRODUCT_TRANSITION_SUMMARY",
                "ZHANG_TEMPORAL_COMPONENT_GAUGE_CANCELLATION",
                "ZHANG_TEMPORAL_COMPONENT_JOINT_NIS"):
        transport.extend(events.get(key, []))

    tree_records = events.get("ZHANG_HYBRID_TREE_INVARIANCE", [])
    invariant_records = [r for r in tree_records
                         if r.get("invariant") in {"1", "true", "True"}
                         or r.get("status") in {"INVARIANT", "COMMITTED"}]
    rejected_records = [r for r in transport
                        if str(r.get("status", "")).upper() in
                        {"REJECTED", "UNAVAILABLE", "SUSPENDED", "FAILED"}
                        or "REJECT" in str(r.get("reason", "")).upper()]
    return {
        "trace": str(path),
        "timeline": ordered,
        "phase6": {
            "transport_event_count": len(transport),
            "tree_invariance_record_count": len(tree_records),
            "invariant_record_count": len(invariant_records),
            "rejected_or_suspended_record_count": len(rejected_records),
            "evidence_status": (
                "CERTIFIED_INVARIANCE_OBSERVED" if invariant_records
                else "NO_MACHINE_INVARIANCE_RECORD"),
            "events": transport,
        },
        "phase7": {
            "peak_r_dual": max((r["r_dual"] for r in ordered), default=0),
            "peak_K_dual": max((r["K_dual"] for r in ordered), default=0),
            "peak_largest_dual_component_size": max(
                (r["largest_dual_component_size"] for r in ordered), default=0),
            "peak_dual_certified_satellite_count": max(
                (r["dual_certified_satellite_count"] for r in ordered), default=0),
            "peak_J_pair_PRODUCT_FIXED": max(
                (r["J_pair_PRODUCT_FIXED"] for r in ordered), default=0),
            "partial_component_allowed": True,
            "rank_source_semantics": {
                "DIRECT": "current certified dual pair rows",
                "LEDGER": "confirmed product integer ledger rows",
                "COMPONENT": "component-gauge ledger is audited separately",
                "BESD": "targeted BESD route is audited separately",
            },
        },
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("trace", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    args.output.write_text(json.dumps(read_trace(args.trace), indent=2) + "\n")


if __name__ == "__main__":
    main()
