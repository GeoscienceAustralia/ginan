#!/usr/bin/env python3
"""Exact small-graph truth cases for Zhang integer-coordinate experiments.

This script is deliberately independent of the C++ implementation.  It uses
SymPy integer matrices/HNF/SNF and emits machine-readable results for the five
minimum cases specified in the Stage-B improvement-2 design note.
"""

from __future__ import annotations

import json

try:
    from sympy import Matrix, ZZ
    from sympy.matrices.normalforms import smith_normal_form
except ImportError as exc:  # pragma: no cover - environment diagnostic
    raise SystemExit(
        "SymPy is required; install python3-sympy or set PYTHONPATH to an extracted package"
    ) from exc


def in_row_lattice(rows: Matrix, target: Matrix) -> bool:
    """Test target membership from exact rank and Smith determinantal divisors."""

    augmented = rows.col_join(target)

    def signature(matrix: Matrix) -> tuple[int, int]:
        rank = matrix.rank()
        smith = smith_normal_form(matrix, domain=ZZ)
        divisor = 1
        for index in range(min(smith.rows, smith.cols)):
            if smith[index, index] != 0:
                divisor *= abs(int(smith[index, index]))
        return rank, divisor

    return signature(rows) == signature(augmented)


def classify_dual_signal(rows: Matrix, dimension: int) -> dict[str, bool]:
    l1 = True
    l2 = True
    wide_lane = True
    for index in range(dimension):
        target_l1 = Matrix([[int(i == index) for i in range(2 * dimension)]])
        target_l2 = Matrix(
            [[int(i == dimension + index) for i in range(2 * dimension)]]
        )
        target_wl = target_l1 - target_l2
        l1 &= in_row_lattice(rows, target_l1)
        l2 &= in_row_lattice(rows, target_l2)
        wide_lane &= in_row_lattice(rows, target_wl)
    return {"l1": l1, "l2": l2, "wide_lane": wide_lane}


def primitive_integer_vector(vector: Matrix) -> Matrix:
    """Scale a one-dimensional rational null vector to a primitive Z vector."""

    from math import gcd, lcm

    denominator = 1
    for value in vector:
        denominator = lcm(denominator, int(value.q))
    integers = [int(value * denominator) for value in vector]
    divisor = 0
    for value in integers:
        divisor = gcd(divisor, abs(value))
    if divisor:
        integers = [value // divisor for value in integers]
    if next((value for value in integers if value), 1) < 0:
        integers = [-value for value in integers]
    return Matrix(integers)


def main() -> None:
    # Case 1: K2,2.  Canonical coordinates are [d1,d2,d3,k].
    canonical_to_arc = Matrix(
        [
            [1, 0, 0, 0],
            [0, 1, 0, 0],
            [0, 0, 1, 0],
            [-1, 1, 1, 1],
        ]
    )
    fundamental_cycle = Matrix([[1, -1, -1, 1]])
    datum_satellite_sd = Matrix([[-1, 1, 0]])
    fix_quotient = Matrix([[0]])

    # Case 2: exact transition to tree [R0-G1,R1-G1,R1-G2].
    transition = Matrix(
        [
            [1, 0, 0, 0],
            [0, 0, 1, 0],
            [-1, 1, 1, 1],
            [0, 0, 0, -1],
        ]
    )

    # Cases 4 and 5: WL-only, then WL plus an L1 unit constraint.
    wide_lane_only = Matrix([[1, -1]])
    wide_lane_plus_l1 = Matrix([[1, -1], [1, 0]])
    smith_even = smith_normal_form(Matrix([[2]]), domain=ZZ)

    # Exact surviving-lattice truth case.  Every row touches removed arc n3,
    # but the primitive integer left-kernel combination r1-r2 preserves
    # n1-n2=-2.  This is the information lost by DELETE_TOUCHED_ROWS.
    held = Matrix([[1, 0, 1], [0, 1, 1], [1, 1, 2]])
    held_values = Matrix([5, 7, 12])
    removed_block = held[:, [2]]
    kernel = primitive_integer_vector(removed_block.T.nullspace()[0])
    surviving_row = (kernel.T * held[:, [0, 1]])
    surviving_value = int((kernel.T * held_values)[0])

    results = {
        "k22": {
            "canonical_to_arc_det": int(canonical_to_arc.det()),
            "cycle_closure": [
                int(value) for value in (fundamental_cycle * canonical_to_arc)
            ],
            "datum_satellite_single_difference": [
                [int(value) for value in datum_satellite_sd.row(row)]
                for row in range(datum_satellite_sd.rows)
            ],
            "fix_quotient": [
                [int(value) for value in fix_quotient.row(row)]
                for row in range(fix_quotient.rows)
            ],
        },
        "tree_exchange": {
            "determinant": int(transition.det()),
            "unimodular": abs(int(transition.det())) == 1,
        },
        "local_subtree_break": {
            "root_component_satellites": ["G01"],
            "detached_satellites": ["G02", "G03"],
        },
        "wide_lane_only": classify_dual_signal(wide_lane_only, 1),
        "wide_lane_plus_l1": classify_dual_signal(wide_lane_plus_l1, 1),
        "unsaturated_unit_lattice": {
            "smith_invariants": [int(smith_even[0, 0])],
            "contains_unit": in_row_lattice(Matrix([[2]]), Matrix([[1]])),
        },
        "exact_surviving_lattice": {
            "integer_left_kernel": [int(value) for value in kernel],
            "delete_touched_rows_rank": 0,
            "exact_surviving_rank": int(surviving_row.rank()),
            "surviving_row": [int(value) for value in surviving_row],
            "surviving_value": surviving_value,
        },
    }

    assert results["k22"]["canonical_to_arc_det"] == 1
    assert results["k22"]["cycle_closure"] == [0, 0, 0, 1]
    assert results["k22"]["fix_quotient"] == [[0]]
    assert results["tree_exchange"]["unimodular"]
    assert results["wide_lane_only"] == {
        "l1": False,
        "l2": False,
        "wide_lane": True,
    }
    assert results["wide_lane_plus_l1"] == {
        "l1": True,
        "l2": True,
        "wide_lane": True,
    }
    assert results["unsaturated_unit_lattice"] == {
        "smith_invariants": [2],
        "contains_unit": False,
    }
    assert results["exact_surviving_lattice"] == {
        "integer_left_kernel": [1, -1, 0],
        "delete_touched_rows_rank": 0,
        "exact_surviving_rank": 1,
        "surviving_row": [1, -1],
        "surviving_value": -2,
    }
    print(json.dumps(results, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
