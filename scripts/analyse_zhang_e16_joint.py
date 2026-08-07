#!/usr/bin/env python3
"""Audit E16-B joint [WL,L1] information and layered shadow decisions."""

from __future__ import annotations

import argparse
import json
import math
import re
from collections import Counter, defaultdict
from pathlib import Path


PAIR = re.compile(r"([A-Za-z_][A-Za-z0-9_]*)=([^\s]+)")


def fields(line: str) -> dict[str, str]:
    return dict(PAIR.findall(line))


def number(item: dict[str, str], name: str) -> float:
    return float(item[name])


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


def inverse_2x2(a: float, b: float, d: float) -> tuple[float, float, float] | None:
    determinant = a * d - b * b
    if not math.isfinite(determinant) or determinant <= 0:
        return None
    return d / determinant, -b / determinant, a / determinant


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("trace", type=Path)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--reliable-perr", type=float, default=1e-3)
    args = parser.parse_args()

    accepted = 0
    rejected = Counter()
    pending: dict[tuple[str, str, str], dict[str, str]] = {}
    configured_rows = []
    feedback_violations = 0
    unlimited = defaultdict(lambda: {
        "j00": 0.0, "j01": 0.0, "j11": 0.0, "h0": 0.0, "h1": 0.0,
        "epochs": 0, "max_epochs": 0, "minimum_wl_perr": None,
        "minimum_conditional_l1_perr": None, "reliable_layered_rows": 0,
        "streaks": 1,
    })

    with args.trace.open(encoding="utf-8", errors="replace") as stream:
        for line in stream:
            if "ZHANG_RELINK_JOINT_INFORMATION" in line:
                item = fields(line)
                feedback_violations += item.get("feedback") != "0"
                if item.get("status") != "ACCEPTED":
                    rejected[item.get("reason", "UNKNOWN")] += 1
                    continue
                accepted += 1
                key = (item.get("topology_key", ""), item.get("anchor", ""),
                       item.get("satellite", ""))
                pending[key] = item
            elif "ZHANG_RELINK_JOINT_SHADOW" in line:
                item = fields(line)
                feedback_violations += item.get("feedback") != "0"
                key_tuple = (item.get("topology_key", ""), item.get("anchor", ""),
                             item.get("satellite", ""))
                increment = pending.pop(key_tuple, None)
                if increment is None:
                    continue
                configured_rows.append(item)
                key = ":".join(key_tuple)
                state = unlimited[key]
                if item.get("reset") == "1":
                    for name in ("j00", "j01", "j11", "h0", "h1"):
                        state[name] = 0.0
                    state["epochs"] = 0
                    state["streaks"] += 1
                for name in ("j00", "j01", "j11", "h0", "h1"):
                    state[name] += number(increment, name)
                state["epochs"] += 1
                state["max_epochs"] = max(state["max_epochs"], state["epochs"])
                covariance = inverse_2x2(state["j00"], state["j01"], state["j11"])
                if covariance is None:
                    continue
                c00, c01, c11 = covariance
                mean0 = c00 * state["h0"] + c01 * state["h1"]
                mean1 = c01 * state["h0"] + c11 * state["h1"]
                wl_integer = integer_round(mean0)
                wl_perr = round_perr(mean0 - wl_integer, c00)
                conditional_mean = mean1 + c01 / c00 * (wl_integer - mean0)
                conditional_variance = max(0.0, c11 - c01 * c01 / c00)
                conditional_perr = round_perr(
                    conditional_mean - integer_round(conditional_mean),
                    conditional_variance,
                )
                for name, value in (
                    ("minimum_wl_perr", wl_perr),
                    ("minimum_conditional_l1_perr", conditional_perr),
                ):
                    state[name] = value if state[name] is None else min(state[name], value)
                state["reliable_layered_rows"] += (
                    wl_perr <= args.reliable_perr and
                    conditional_perr <= args.reliable_perr
                )

    wl_perrs = [number(item, "wl_perr") for item in configured_rows]
    l1_perrs = [number(item, "conditional_l1_perr") for item in configured_rows]
    resets = Counter(
        item.get("reset_reason", "UNKNOWN") for item in configured_rows
        if item.get("reset") == "1"
    )
    result = {
        "trace": str(args.trace),
        "joint_information": {
            "accepted": accepted,
            "rejected": sum(rejected.values()),
            "rejection_reasons": dict(rejected),
        },
        "configured_window": {
            "rows": len(configured_rows),
            "maximum_epochs": max(
                (int(item.get("epochs", 0)) for item in configured_rows),
                default=0,
            ),
            "minimum_wl_perr": min(wl_perrs, default=None),
            "minimum_conditional_l1_perr": min(l1_perrs, default=None),
            "reliable_wl_rows": sum(x <= args.reliable_perr for x in wl_perrs),
            "reliable_conditional_l1_rows": sum(
                x <= args.reliable_perr for x in l1_perrs
            ),
            "reliable_layered_rows": sum(
                wl <= args.reliable_perr and l1 <= args.reliable_perr
                for wl, l1 in zip(wl_perrs, l1_perrs)
            ),
            "resets": dict(resets),
        },
        "unlimited_same_hypothesis_streaks": {
            "warning": (
                "Optimistic offline sum; cross-epoch nuisance correlations are "
                "not reconstructed."
            ),
            "maximum_epochs": max(
                (state["max_epochs"] for state in unlimited.values()), default=0
            ),
            "minimum_wl_perr": min(
                (state["minimum_wl_perr"] for state in unlimited.values()
                 if state["minimum_wl_perr"] is not None), default=None
            ),
            "minimum_conditional_l1_perr": min(
                (state["minimum_conditional_l1_perr"] for state in unlimited.values()
                 if state["minimum_conditional_l1_perr"] is not None), default=None
            ),
            "reliable_layered_rows": sum(
                state["reliable_layered_rows"] for state in unlimited.values()
            ),
            "relations": {
                key: {name: value for name, value in state.items()
                      if name not in {"j00", "j01", "j11", "h0", "h1", "epochs"}}
                for key, state in unlimited.items()
            },
        },
        "reliable_perr_threshold": args.reliable_perr,
        "feedback_violations": feedback_violations,
    }
    rendered = json.dumps(result, ensure_ascii=False, indent=2)
    print(rendered)
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(rendered + "\n", encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
