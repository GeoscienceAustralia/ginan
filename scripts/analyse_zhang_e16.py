#!/usr/bin/env python3
"""Audit E16 prior/posterior information increments and shadow windows."""

from __future__ import annotations

import argparse
import json
import math
import re
from collections import Counter, defaultdict
from datetime import datetime
from pathlib import Path


PAIR = re.compile(r"([A-Za-z_]+)=([^\s]+)")


def fields(line: str) -> dict[str, str]:
    return dict(PAIR.findall(line))


def as_float(value: str | None) -> float | None:
    try:
        result = float(value) if value is not None else None
    except ValueError:
        return None
    return result if result is not None and math.isfinite(result) else None


def integer_round(value: float) -> int:
    return math.floor(value + 0.5) if value >= 0 else math.ceil(value - 0.5)


def round_perr(dx: float, variance: float) -> float:
    if variance < 1e-20:
        return 0.0
    factor = -0.25 / variance
    probability = 0.0
    for integer in range(1, 10):
        probability += math.exp((integer + 2 * dx) * integer * factor)
        probability += math.exp((integer - 2 * dx) * integer * factor)
    return probability / (probability + 1)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("trace", type=Path)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--reliable-perr", type=float, default=1e-3)
    args = parser.parse_args()

    accepted = []
    rejected = Counter()
    shadows = []
    feedback_violations = 0
    relation_hypotheses: dict[str, set[int]] = defaultdict(set)

    with args.trace.open(encoding="utf-8", errors="replace") as stream:
        for line in stream:
            if "ZHANG_RELINK_INFORMATION_INCREMENT" in line:
                item = fields(line)
                timestamp = re.search(
                    r"time=(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2})", line
                )
                if timestamp:
                    item["_timestamp"] = timestamp.group(1)
                feedback_violations += item.get("feedback") != "0"
                if item.get("status") == "ACCEPTED":
                    accepted.append(item)
                else:
                    rejected[item.get("reason", "UNKNOWN")] += 1
            elif "ZHANG_RELINK_SHADOW" in line:
                item = fields(line)
                feedback_violations += item.get("feedback") != "0"
                shadows.append(item)
                key = ":".join(
                    item.get(name, "")
                    for name in ("system", "stage", "anchor", "satellite")
                )
                try:
                    relation_hypotheses[key].add(int(item["integer_hypothesis"]))
                except (KeyError, ValueError):
                    pass

    information = [
        value for item in accepted
        if (value := as_float(item.get("information"))) is not None
    ]
    perrs = [
        value for item in shadows
        if (value := as_float(item.get("perr"))) is not None
    ]
    epochs = [int(item.get("epochs", 0)) for item in shadows]
    resets = Counter(
        item.get("reset_reason", "UNKNOWN")
        for item in shadows if item.get("reset") == "1"
    )
    reliable = [item for item in shadows
                if (as_float(item.get("perr")) or math.inf) <= args.reliable_perr]
    stable_relations = sum(len(values) == 1 for values in relation_hypotheses.values())

    # Reconstruct unlimited same-hypothesis streaks offline.  This is an
    # optimistic diagnostic only: summing scalar increments still omits
    # cross-epoch nuisance correlations, but it answers whether merely raising
    # the configured 20-epoch cap could possibly meet the integer gate.
    unlimited = {}
    for item in accepted:
        key = ":".join(
            item.get(name, "")
            for name in ("system", "stage", "anchor", "satellite")
        )
        information_value = as_float(item.get("information"))
        natural_value = as_float(item.get("natural"))
        posterior_mean = as_float(item.get("posterior_mean"))
        if None in (information_value, natural_value, posterior_mean):
            continue
        timestamp = datetime.fromisoformat(item["_timestamp"])
        hypothesis = integer_round(posterior_mean)
        state = unlimited.setdefault(key, {
            "last_time": None,
            "hypothesis": hypothesis,
            "epochs": 0,
            "information": 0.0,
            "natural": 0.0,
            "max_streak_epochs": 0,
            "minimum_perr": None,
            "reliable_rows": 0,
            "streaks": 1,
        })
        gap = ((timestamp - state["last_time"]).total_seconds()
               if state["last_time"] is not None else 0)
        if hypothesis != state["hypothesis"] or gap < 0 or gap > 120:
            state["hypothesis"] = hypothesis
            state["epochs"] = 0
            state["information"] = 0.0
            state["natural"] = 0.0
            state["streaks"] += 1
        state["last_time"] = timestamp
        state["epochs"] += 1
        state["information"] += information_value
        state["natural"] += natural_value
        mean = state["natural"] / state["information"]
        variance = 1 / state["information"]
        perr = round_perr(mean - integer_round(mean), variance)
        state["max_streak_epochs"] = max(
            state["max_streak_epochs"], state["epochs"]
        )
        state["minimum_perr"] = (
            perr if state["minimum_perr"] is None
            else min(state["minimum_perr"], perr)
        )
        state["reliable_rows"] += perr <= args.reliable_perr

    unlimited_summary = {
        key: {
            name: value for name, value in state.items()
            if name not in {"last_time", "hypothesis", "epochs",
                            "information", "natural"}
        }
        for key, state in unlimited.items()
    }
    stage_minimum_perr = {}
    for stage in ("WL", "L1"):
        values = [state["minimum_perr"] for key, state in unlimited.items()
                  if f":{stage}:" in key and state["minimum_perr"] is not None]
        stage_minimum_perr[stage] = min(values, default=None)

    result = {
        "trace": str(args.trace),
        "information_increments": {
            "accepted": len(accepted),
            "rejected": sum(rejected.values()),
            "acceptance_fraction": (
                len(accepted) / (len(accepted) + sum(rejected.values()))
                if accepted or rejected else 0
            ),
            "rejection_reasons": dict(rejected),
            "minimum": min(information, default=None),
            "median": (
                sorted(information)[len(information) // 2]
                if information else None
            ),
            "maximum": max(information, default=None),
        },
        "shadow": {
            "rows": len(shadows),
            "relations": len(relation_hypotheses),
            "stable_integer_hypothesis_relations": stable_relations,
            "maximum_epochs": max(epochs, default=0),
            "reliable_perr_threshold": args.reliable_perr,
            "reliable_rows": len(reliable),
            "minimum_perr": min(perrs, default=None),
            "resets": dict(resets),
        },
        "unlimited_same_hypothesis_streaks": {
            "warning": (
                "Optimistic offline sum; cross-epoch nuisance correlations are "
                "not reconstructed."
            ),
            "maximum_streak_epochs": max(
                (state["max_streak_epochs"] for state in unlimited.values()),
                default=0,
            ),
            "minimum_perr_by_stage": stage_minimum_perr,
            "reliable_rows": sum(
                state["reliable_rows"] for state in unlimited.values()
            ),
            "relations": unlimited_summary,
        },
        "feedback_violations": feedback_violations,
        "interpretation_boundary": (
            "Scalar prior/posterior marginal differences are effective epoch "
            "likelihoods. Cross-epoch nuisance correlations are not reconstructed; "
            "the audit is not authorization for estimator feedback."
        ),
    }
    rendered = json.dumps(result, indent=2, ensure_ascii=False)
    print(rendered)
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(rendered + "\n", encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
