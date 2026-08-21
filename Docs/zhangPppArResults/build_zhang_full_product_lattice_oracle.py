#!/usr/bin/env python3
"""Build a full Zhang product-lattice oracle only from exact pair evidence.

The input TRACE must contain generation-tagged
``ZHANG_PRODUCT_INTEGER_LEDGER_PAIR`` snapshots or exact pre-conditioning
``ZHANG_PRODUCT_LATTICE_CERTIFIED_PAIR`` records.  This program never rounds
float products and never combines backend generations or phase segments.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
from collections import defaultdict, deque
from pathlib import Path


FIELD = re.compile(r"([A-Za-z0-9_]+)=([^\s]+)")
TRACE_TIME = re.compile(
    r"\btime=(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}(?:\.\d+)?)"
)
SEGMENT_TOKEN = re.compile(r"([A-Z]\d{2})\|([A-Z0-9]+)\|SEG(\d+);")


def fields(line: str) -> dict[str, str]:
    row = dict(FIELD.findall(line))
    match = TRACE_TIME.search(line)
    if match:
        row["time"] = match.group(1)
    return row


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def segment_identity(value: str) -> tuple[str, object] | None:
    """Parse a row-local signal-segment map, preserving legacy opaque IDs."""
    matches = list(SEGMENT_TOKEN.finditer(value))
    if matches and "".join(match.group(0) for match in matches) == value:
        signals: dict[tuple[str, str], int] = {}
        for match in matches:
            key = (match.group(1), match.group(2))
            segment = int(match.group(3))
            if key in signals and signals[key] != segment:
                return None
            signals[key] = segment
        return "ROW_LOCAL", signals
    if value:
        return "OPAQUE", value
    return None


def segment_identities_compatible(
    left: tuple[str, object], right: tuple[str, object]
) -> bool:
    if left[0] != right[0]:
        return False
    if left[0] == "OPAQUE":
        return left[1] == right[1]
    left_signals = left[1]
    right_signals = right[1]
    return all(
        key not in right_signals or right_signals[key] == segment
        for key, segment in left_signals.items()
    )


def merge_segment_identities(
    identities: list[tuple[str, object]],
) -> tuple[str, object] | None:
    if not identities:
        return None
    merged = identities[0]
    for identity in identities[1:]:
        if not segment_identities_compatible(merged, identity):
            return None
        if merged[0] == "ROW_LOCAL":
            signals = dict(merged[1])
            signals.update(identity[1])
            merged = "ROW_LOCAL", signals
    return merged


def canonical_segment_identity(identity: tuple[str, object]) -> str:
    if identity[0] == "OPAQUE":
        return str(identity[1])
    return "".join(
        f"{satellite}|{observable}|SEG{segment};"
        for (satellite, observable), segment in sorted(identity[1].items())
    )


def canonical_edge(first: str, second: str, value: int) -> tuple[str, str, int]:
    if first <= second:
        return first, second, value
    return second, first, -value


def graph_from_rows(rows: list[dict[str, str]], coordinate: str) -> dict:
    values: dict[tuple[str, str], set[int]] = defaultdict(set)
    confirmations: dict[tuple[str, str], int] = defaultdict(int)
    sources: dict[tuple[str, str], set[str]] = defaultdict(set)
    for row in rows:
        if row.get("coordinate") != coordinate:
            continue
        first = row.get("first", "")
        second = row.get("second", "")
        if not first or not second or first == "INVALID" or second == "INVALID":
            continue
        try:
            value = int(row["integer"])
            confirmation = int(row.get("confirmations", "0"))
        except (KeyError, ValueError):
            continue
        left, right, canonical_value = canonical_edge(first, second, value)
        values[(left, right)].add(canonical_value)
        confirmations[(left, right)] = max(confirmations[(left, right)], confirmation)
        sources[(left, right)].add(row.get("source", "UNKNOWN"))

    edge_conflicts = {
        f"{left}-{right}": sorted(edge_values)
        for (left, right), edge_values in values.items()
        if len(edge_values) != 1
    }
    adjacency: dict[str, list[tuple[str, int]]] = defaultdict(list)
    accepted_edges = []
    for (left, right), edge_values in sorted(values.items()):
        if len(edge_values) != 1:
            continue
        value = next(iter(edge_values))  # left - right
        adjacency[left].append((right, value))
        adjacency[right].append((left, -value))
        accepted_edges.append(
            {
                "first": left,
                "second": right,
                "integer_first_minus_second": value,
                "confirmations": confirmations[(left, right)],
                "sources": sorted(sources[(left, right)]),
            }
        )

    potentials: dict[str, int] = {}
    components: list[list[str]] = []
    cycle_conflicts = []
    for start in sorted(adjacency):
        if start in potentials:
            continue
        potentials[start] = 0
        component = []
        queue = deque([start])
        while queue:
            first = queue.popleft()
            component.append(first)
            for second, first_minus_second in adjacency[first]:
                expected_second = potentials[first] - first_minus_second
                if second not in potentials:
                    potentials[second] = expected_second
                    queue.append(second)
                elif potentials[second] != expected_second:
                    cycle_conflicts.append(
                        {
                            "first": first,
                            "second": second,
                            "edge_integer": first_minus_second,
                            "implied_integer": potentials[first] - potentials[second],
                        }
                    )
        components.append(sorted(component))
    components.sort(key=lambda component: (-len(component), component))
    return {
        "coordinate": coordinate,
        "edges": accepted_edges,
        "edge_count": len(accepted_edges),
        "edge_conflicts": edge_conflicts,
        "cycle_conflicts": cycle_conflicts,
        "components": components,
        "maximum_rank": max((len(component) - 1 for component in components), default=0),
        "potentials": potentials,
        "adjacency": adjacency,
    }


def connected(nodes: set[str], adjacency: dict[str, list[tuple[str, int]]]) -> bool:
    if not nodes:
        return False
    reached = {min(nodes)}
    queue = deque(reached)
    while queue:
        node = queue.popleft()
        for neighbour, _ in adjacency.get(node, []):
            if neighbour in nodes and neighbour not in reached:
                reached.add(neighbour)
                queue.append(neighbour)
    return reached == nodes


def dual_connected_components(wl: dict, l1: dict) -> list[list[str]]:
    candidates = []
    for wl_component in wl["components"]:
        wl_nodes = set(wl_component)
        for l1_component in l1["components"]:
            nodes = wl_nodes.intersection(l1_component)
            if (
                len(nodes) > 1
                and connected(nodes, wl["adjacency"])
                and connected(nodes, l1["adjacency"])
            ):
                candidates.append(sorted(nodes))
    # WL and L1 components are partitions, so their non-empty intersections
    # are disjoint.  Product rank is the sum of the independent component
    # ranks, not merely the rank of the largest component.
    candidates.sort(key=lambda nodes: (-len(nodes), nodes))
    return candidates


def relative_potentials(graph: dict, nodes: list[str], reference: str) -> dict[str, int]:
    potential = {reference: 0}
    queue = deque([reference])
    node_set = set(nodes)
    while queue:
        first = queue.popleft()
        for second, first_minus_second in graph["adjacency"].get(first, []):
            if second not in node_set:
                continue
            expected = potential[first] - first_minus_second
            if second not in potential:
                potential[second] = expected
                queue.append(second)
            elif potential[second] != expected:
                raise ValueError(f"cycle conflict in {graph['coordinate']}")
    if set(potential) != node_set:
        raise ValueError(f"disconnected {graph['coordinate']} oracle component")
    return {satellite: potential[satellite] for satellite in sorted(nodes)}


def public_graph(graph: dict) -> dict:
    return {key: value for key, value in graph.items() if key not in {"adjacency", "potentials"}}


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("trace", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--expected-rank", type=int, default=22)
    parser.add_argument("--reference", default="G02")
    parser.add_argument(
        "--epoch",
        help="exact frozen epoch (YYYY-MM-DD HH:MM:SS); never mix epochs",
    )
    parser.add_argument("--binary", type=Path)
    parser.add_argument("--config", type=Path)
    args = parser.parse_args()

    text = args.trace.read_text(encoding="utf-8", errors="replace")
    pair_rows = []
    current_pair_rows = []
    ledger_rows = []
    for line in text.splitlines():
        if line.startswith("ZHANG_PRODUCT_INTEGER_LEDGER_PAIR "):
            pair_rows.append(fields(line))
        elif line.startswith("ZHANG_PRODUCT_LATTICE_CERTIFIED_PAIR "):
            current_pair_rows.append(fields(line))
        elif line.startswith("ZHANG_PRODUCT_INTEGER_LEDGER "):
            ledger_rows.append(fields(line))
    metadata_missing_rows = sum(
        not row.get("backend_generation")
        or not row.get("phase_segment_fingerprint")
        for row in pair_rows + current_pair_rows
    )

    # Current exact pair records carry WL and L1 together.  Expand them into
    # the same coordinate-row schema as the ledger without changing integers.
    expanded_current_rows = []
    current_pair_value_errors = 0
    for row in current_pair_rows:
        try:
            wl_integer = int(row["wl_integer"])
            l1_integer = int(row["l1_integer"])
            l2_integer = int(row["l2_integer"])
        except (KeyError, ValueError):
            current_pair_value_errors += 1
            continue
        if l2_integer != l1_integer - wl_integer:
            current_pair_value_errors += 1
            continue
        for coordinate, value in (("WL", wl_integer), ("L1", l1_integer)):
            expanded = dict(row)
            expanded["coordinate"] = coordinate
            expanded["integer"] = str(value)
            expanded["confirmations"] = "1"
            expanded["source"] = "CURRENT_EXACT_PRODUCT_LATTICE"
            expanded_current_rows.append(expanded)

    def grouped_snapshots(rows: list[dict[str, str]]) -> list[dict]:
        grouped: dict[tuple[str, str], list[dict[str, str]]] = defaultdict(list)
        for row in rows:
            time = row.get("time", "")
            generation = row.get("backend_generation", "")
            segment = row.get("phase_segment_fingerprint", "")
            identity = segment_identity(segment)
            if not time or not generation or identity is None:
                continue
            if args.epoch and time != args.epoch:
                continue
            row["_segment_identity"] = identity
            grouped[(time, generation)].append(row)
        snapshots = []
        for (time, generation), coordinate_rows in sorted(grouped.items()):
            # Current exact rows are mandatory evidence.  Try every row as a
            # deterministic seed and greedily add all mutually compatible rows;
            # this retains alternative conflict-free ledger branches without
            # ever mixing a shared signal at different physical segments.
            ordered = sorted(
                coordinate_rows,
                key=lambda row: (
                    row.get("evidence_source") !=
                    "CURRENT_PRECONDITION_CERTIFICATE",
                    row.get("coordinate", ""),
                    row.get("first", ""),
                    row.get("second", ""),
                    row.get("integer", ""),
                ),
            )
            compatible_groups: dict[tuple[int, ...], list[dict[str, str]]] = {}
            mandatory_indices = [
                index
                for index, row in enumerate(ordered)
                if row.get("evidence_source") ==
                "CURRENT_PRECONDITION_CERTIFICATE"
            ]
            mandatory_identities = [
                ordered[index]["_segment_identity"] for index in mandatory_indices
            ]
            if mandatory_indices and merge_segment_identities(
                mandatory_identities
            ) is None:
                # Mutually inconsistent current evidence makes this complete
                # coordinate group unusable; never let a historical-only branch
                # bypass a current physical segment conflict.
                continue
            optional_indices = [
                index for index in range(len(ordered))
                if index not in mandatory_indices
            ]
            seeds = [-1] + optional_indices
            for seed in seeds:
                selected_indices = list(mandatory_indices)
                selected_identities = list(mandatory_identities)
                if seed >= 0:
                    trial = selected_identities + [
                        ordered[seed]["_segment_identity"]
                    ]
                    if merge_segment_identities(trial) is None:
                        continue
                    selected_indices.append(seed)
                    selected_identities.append(ordered[seed]["_segment_identity"])
                for index, row in enumerate(ordered):
                    if index in selected_indices:
                        continue
                    trial = selected_identities + [row["_segment_identity"]]
                    if merge_segment_identities(trial) is None:
                        continue
                    selected_indices.append(index)
                    selected_identities.append(row["_segment_identity"])
                selected_indices.sort()
                compatible_groups.setdefault(
                    tuple(selected_indices),
                    [ordered[index] for index in selected_indices],
                )

            for group_rows in compatible_groups.values():
                merged_identity = merge_segment_identities(
                    [row["_segment_identity"] for row in group_rows]
                )
                if merged_identity is None:
                    continue
                segment = canonical_segment_identity(merged_identity)
                evidence_sources = sorted(
                    {row.get("evidence_source", "UNKNOWN") for row in group_rows}
                )
                wl_graph = graph_from_rows(group_rows, "WL")
                l1_graph = graph_from_rows(group_rows, "L1")
                components = dual_connected_components(wl_graph, l1_graph)
                nodes = sorted(
                    {node for component in components for node in component}
                )
                conflicts = (
                    len(wl_graph["edge_conflicts"])
                    + len(l1_graph["edge_conflicts"])
                    + len(wl_graph["cycle_conflicts"])
                    + len(l1_graph["cycle_conflicts"])
                )
                snapshots.append(
                    {
                        "time": time,
                        "backend_generation": generation,
                        "phase_segment_fingerprint": segment,
                        "segment_identity_mode": merged_identity[0],
                        "evidence_sources": evidence_sources,
                        "rows": len(group_rows),
                        "wl": wl_graph,
                        "l1": l1_graph,
                        "dual_components": components,
                        "dual_nodes": nodes,
                        "dual_rank": sum(
                            len(component) - 1 for component in components
                        ),
                        "graph_conflicts": conflicts,
                    }
                )
        return snapshots

    # Evidence source is provenance, not part of the integer-coordinate
    # identity.  Exact current certificates and an admitted ledger snapshot
    # may therefore be united only when epoch/backend generation match and
    # their row-local signal-segment maps are mutually compatible.  Exact
    # equality was correct for the legacy global fingerprint, but would split
    # every row once fingerprints became local to physicalExpansion.
    for row in pair_rows:
        row["evidence_source"] = "LEDGER_SNAPSHOT"
    for row in expanded_current_rows:
        row["evidence_source"] = "CURRENT_PRECONDITION_CERTIFICATE"
    snapshots = grouped_snapshots(pair_rows + expanded_current_rows)
    snapshots.sort(
        key=lambda item: (
            item["dual_rank"],
            -item["graph_conflicts"],
            item["time"],
            "LEDGER_SNAPSHOT" in item["evidence_sources"],
        ),
        reverse=True,
    )
    selected = snapshots[0] if snapshots else None
    latest_time = selected["time"] if selected else None
    latest_rows = selected["rows"] if selected else 0
    wl = selected["wl"] if selected else graph_from_rows([], "WL")
    l1 = selected["l1"] if selected else graph_from_rows([], "L1")
    dual_components = selected["dual_components"] if selected else []
    dual_nodes = selected["dual_nodes"] if selected else []
    physical_conflicts = sum(
        int(row.get("conflicting_rows", "0"))
        for row in ledger_rows
        if row.get("status") == "UPDATED"
    )
    graph_conflict_count = selected["graph_conflicts"] if selected else 0
    dual_rank = sum(len(component) - 1 for component in dual_components)
    ready = (
        selected is not None
        and dual_rank >= args.expected_rank
        and graph_conflict_count == 0
        and physical_conflicts == 0
        and metadata_missing_rows == 0
        and current_pair_value_errors == 0
    )
    oracle = None
    if ready and dual_components:
        oracle_components = []
        for component in dual_components:
            reference = args.reference if args.reference in component else min(component)
            wl_potential = relative_potentials(wl, component, reference)
            l1_potential = relative_potentials(l1, component, reference)
            oracle_components.append(
                {
                    "reference_satellite": reference,
                    "satellites": component,
                    "rank": len(component) - 1,
                    "relations": [
                        {
                            "satellite": satellite,
                            "reference": reference,
                            "wl_satellite_minus_reference": wl_potential[satellite],
                            "l1_satellite_minus_reference": l1_potential[satellite],
                            "l2_satellite_minus_reference": (
                                l1_potential[satellite] - wl_potential[satellite]
                            ),
                        }
                        for satellite in component
                        if satellite != reference
                    ],
                }
            )
        oracle = {
            "schema": "ZHANG_FULL_PRODUCT_LATTICE_ORACLE_V1",
            "system": "GPS",
            "satellites": dual_nodes,
            "dual_frequency_rank": dual_rank,
            "components": oracle_components,
        }

    provenance = {"trace_sha256": sha256(args.trace)}
    for label, path in (("binary", args.binary), ("config", args.config)):
        if path:
            provenance[f"{label}_path"] = str(path)
            provenance[f"{label}_sha256"] = sha256(path)
    result = {
        "status": "FULL_ORACLE_READY" if ready else "INSUFFICIENT_OR_CONFLICTING_EVIDENCE",
        "hard_gate_passed": ready,
        "expected_rank": args.expected_rank,
        "requested_epoch": args.epoch,
        "selected_pair_snapshot_time": latest_time,
        "selected_backend_generation": (
            selected["backend_generation"] if selected else None
        ),
        "selected_phase_segment_fingerprint": (
            selected["phase_segment_fingerprint"] if selected else None
        ),
        "selected_segment_identity_mode": (
            selected["segment_identity_mode"] if selected else None
        ),
        "selected_evidence_sources": (
            selected["evidence_sources"] if selected else []
        ),
        "pair_snapshot_group_count": len(snapshots),
        "selected_pair_rows": latest_rows,
        "rows_missing_generation_or_segment": metadata_missing_rows,
        "current_pair_value_errors": current_pair_value_errors,
        "physical_ledger_conflicting_rows": physical_conflicts,
        "graph_conflict_count": graph_conflict_count,
        "wl_graph": public_graph(wl),
        "l1_graph": public_graph(l1),
        "dual_connected_components": dual_components,
        "dual_connected_satellites": dual_nodes,
        "dual_frequency_rank": dual_rank,
        "snapshot_group_audit": [
            {
                "time": item["time"],
                "backend_generation": item["backend_generation"],
                "phase_segment_fingerprint": item[
                    "phase_segment_fingerprint"
                ],
                "segment_identity_mode": item["segment_identity_mode"],
                "evidence_sources": item["evidence_sources"],
                "rows": item["rows"],
                "dual_connected_components": item["dual_components"],
                "dual_frequency_rank": item["dual_rank"],
                "graph_conflict_count": item["graph_conflicts"],
            }
            for item in snapshots
        ],
        "oracle": oracle,
        "provenance": provenance,
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, indent=2, ensure_ascii=False) + "\n")
    print(json.dumps(result, indent=2, ensure_ascii=False))
    return 0 if ready else 2


if __name__ == "__main__":
    raise SystemExit(main())
