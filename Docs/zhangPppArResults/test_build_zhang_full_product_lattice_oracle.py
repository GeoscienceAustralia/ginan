from __future__ import annotations

import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


SCRIPT = Path(__file__).with_name("build_zhang_full_product_lattice_oracle.py")


def run_builder(tmp_path: Path, lines: list[str], expected_rank: int = 2):
    trace = tmp_path / "input.TRACE"
    output = tmp_path / "oracle.json"
    trace.write_text("\n".join(lines) + "\n", encoding="utf-8")
    completed = subprocess.run(
        [
            sys.executable,
            str(SCRIPT),
            str(trace),
            "--output",
            str(output),
            "--expected-rank",
            str(expected_rank),
            "--epoch",
            "2024-07-17 00:15:30",
        ],
        capture_output=True,
        text=True,
        check=False,
    )
    return completed, json.loads(output.read_text(encoding="utf-8"))


def ledger_pair(
    coordinate: str,
    first: str,
    second: str,
    integer: int,
    generation: int,
    segment: str = "SEGMENT_A",
):
    return (
        "ZHANG_PRODUCT_INTEGER_LEDGER_PAIR "
        "time=2024-07-17 00:15:30 system=GPS "
        f"coordinate={coordinate} first={first} second={second} "
        f"integer={integer} confirmations=2 backend_generation={generation} "
        f"phase_segment_fingerprint={segment} source=DERIVED_PAIR"
    )


def current_pair(
    first: str,
    second: str,
    wl: int,
    l1: int,
    generation: int,
    segment: str = "SEGMENT_A",
):
    return (
        "ZHANG_PRODUCT_LATTICE_CERTIFIED_PAIR "
        "time=2024-07-17 00:15:30 system=GPS "
        f"first={first} second={second} wl_integer={wl} l1_integer={l1} "
        f"l2_integer={l1 - wl} backend_generation={generation} "
        f"phase_segment_fingerprint={segment} exact_pair_membership=1"
    )


class OracleBuilderTests(unittest.TestCase):
    def run_case(self, lines: list[str], expected_rank: int = 2):
        with tempfile.TemporaryDirectory() as directory:
            return run_builder(Path(directory), lines, expected_rank)

    def test_oracle_accepts_one_complete_generation(self):
        lines = [
            ledger_pair("WL", "G02", "G03", 5, 7),
            ledger_pair("L1", "G02", "G03", 11, 7),
            ledger_pair("WL", "G03", "G04", -2, 7),
            ledger_pair("L1", "G03", "G04", 4, 7),
        ]
        completed, result = self.run_case(lines)
        self.assertEqual(completed.returncode, 0)
        self.assertTrue(result["hard_gate_passed"])
        self.assertEqual(result["dual_frequency_rank"], 2)
        self.assertEqual(result["selected_backend_generation"], "7")
        self.assertEqual(result["oracle"]["dual_frequency_rank"], 2)

    def test_oracle_rank_sums_disjoint_dual_components(self):
        lines = [
            ledger_pair("WL", "G02", "G03", 5, 7),
            ledger_pair("L1", "G02", "G03", 11, 7),
            ledger_pair("WL", "G10", "G11", -2, 7),
            ledger_pair("L1", "G10", "G11", 4, 7),
        ]
        completed, result = self.run_case(lines)
        self.assertEqual(completed.returncode, 0)
        self.assertEqual(result["dual_frequency_rank"], 2)
        self.assertEqual(len(result["dual_connected_components"]), 2)
        self.assertEqual(len(result["oracle"]["components"]), 2)

    def test_oracle_never_combines_backend_generations(self):
        lines = [
            ledger_pair("WL", "G02", "G03", 5, 7),
            ledger_pair("L1", "G02", "G03", 11, 7),
            ledger_pair("WL", "G03", "G04", -2, 8),
            ledger_pair("L1", "G03", "G04", 4, 8),
        ]
        completed, result = self.run_case(lines)
        self.assertEqual(completed.returncode, 2)
        self.assertFalse(result["hard_gate_passed"])
        self.assertEqual(result["dual_frequency_rank"], 1)
        self.assertEqual(result["pair_snapshot_group_count"], 2)

    def test_oracle_combines_exact_sources_with_same_coordinate_identity(self):
        lines = [
            ledger_pair("WL", "G02", "G03", 5, 7),
            ledger_pair("L1", "G02", "G03", 11, 7),
            current_pair("G03", "G04", -2, 4, 7),
        ]
        completed, result = self.run_case(lines)
        self.assertEqual(completed.returncode, 0)
        self.assertTrue(result["hard_gate_passed"])
        self.assertEqual(result["dual_frequency_rank"], 2)
        self.assertEqual(result["pair_snapshot_group_count"], 1)
        self.assertEqual(
            result["selected_evidence_sources"],
            ["CURRENT_PRECONDITION_CERTIFICATE", "LEDGER_SNAPSHOT"],
        )

    def test_oracle_rejects_inconsistent_current_l2_identity(self):
        lines = [
            "ZHANG_PRODUCT_LATTICE_CERTIFIED_PAIR "
            "time=2024-07-17 00:15:30 system=GPS first=G02 second=G03 "
            "wl_integer=5 l1_integer=11 l2_integer=99 backend_generation=7 "
            "phase_segment_fingerprint=SEGMENT_A exact_pair_membership=1"
        ]
        completed, result = self.run_case(lines, expected_rank=1)
        self.assertEqual(completed.returncode, 2)
        self.assertFalse(result["hard_gate_passed"])
        self.assertEqual(result["current_pair_value_errors"], 1)

    def test_oracle_combines_compatible_row_local_segments(self):
        first_segment = (
            "G02|L1C|SEG0;G02|L2W|SEG0;"
            "G03|L1C|SEG0;G03|L2W|SEG0;"
        )
        second_segment = (
            "G03|L1C|SEG0;G03|L2W|SEG0;"
            "G04|L1C|SEG0;G04|L2W|SEG0;"
        )
        lines = [
            ledger_pair("WL", "G02", "G03", 5, 7, first_segment),
            ledger_pair("L1", "G02", "G03", 11, 7, first_segment),
            current_pair("G03", "G04", -2, 4, 7, second_segment),
        ]
        completed, result = self.run_case(lines)
        self.assertEqual(completed.returncode, 0)
        self.assertTrue(result["hard_gate_passed"])
        self.assertEqual(result["dual_frequency_rank"], 2)
        self.assertEqual(result["pair_snapshot_group_count"], 1)
        self.assertIn("G02|L1C|SEG0;", result["selected_phase_segment_fingerprint"])
        self.assertIn("G04|L2W|SEG0;", result["selected_phase_segment_fingerprint"])

    def test_oracle_never_combines_conflicting_row_local_segments(self):
        first_segment = (
            "G02|L1C|SEG0;G02|L2W|SEG0;"
            "G03|L1C|SEG0;G03|L2W|SEG0;"
        )
        conflicting_segment = (
            "G03|L1C|SEG1;G03|L2W|SEG0;"
            "G04|L1C|SEG0;G04|L2W|SEG0;"
        )
        lines = [
            ledger_pair("WL", "G02", "G03", 5, 7, first_segment),
            ledger_pair("L1", "G02", "G03", 11, 7, first_segment),
            current_pair("G03", "G04", -2, 4, 7, conflicting_segment),
        ]
        completed, result = self.run_case(lines)
        self.assertEqual(completed.returncode, 2)
        self.assertFalse(result["hard_gate_passed"])
        self.assertEqual(result["dual_frequency_rank"], 1)
        self.assertEqual(result["pair_snapshot_group_count"], 1)
        self.assertEqual(
            result["selected_evidence_sources"],
            ["CURRENT_PRECONDITION_CERTIFICATE"],
        )


if __name__ == "__main__":
    unittest.main()
