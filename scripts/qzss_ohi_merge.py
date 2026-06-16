#!/usr/bin/env python3
"""Merge QZSS operational history information (OHI) files into a single SINEX file.

Specifically merges SATELLITE/ATTITUDE MODE blocks into the SATELLITE/ATTITUDE_MODE
SINEX format used by Ginan.

Individual OHI files are available at:
    https://qzss.go.jp/en/technical/qzssinfo/index.html

SVN assignments follow igs_satellite_metadata.snx. Note that IGS does not assign J006;
the constellation goes J001-J005 then J007.

Usage:
    python qzss_ohi_merge.py <input_dir> <output_file>

    input_dir   - directory containing JAXA OHI files (ohi-qzs*.txt or ohi-qzs*.txt.gz)
    output_file - path for the output SINEX file (e.g. qzss_yaw_modes.snx)

Example:
    python qzss_ohi_merge.py inputData/products/tables/qzss_ohi qzss_yaw_modes.snx
"""

import argparse
import gzip
import sys
from pathlib import Path


# Mapping from JAXA OHI filename to IGS SVN identifier.
# Follows igs_satellite_metadata.snx — note IGS skips J006.
OHI_FILE_MAP = [
    ("ohi-qzs1.txt",  "J001"),  # QZS-1  (MICHIBIKI-1, launched 2010-09-11)
    ("ohi-qzs2.txt",  "J002"),  # QZS-2I (MICHIBIKI-2, launched 2017-06-01)
    ("ohi-qzs3.txt",  "J003"),  # QZS-2G (MICHIBIKI-3, launched 2017-08-19)
    ("ohi-qzs4.txt",  "J004"),  # QZS-2I (MICHIBIKI-4, launched 2017-10-09)
    ("ohi-qzs1r.txt", "J005"),  # QZS-2A / QZS-1R     (launched 2021-10-26)
    ("ohi-qzs6.txt",  "J007"),  # QZS-3G / QZS-6      (launched 2025-02-02)
]


def open_ohi(path):
    """Open an OHI file, transparently handling plain text and gzip formats."""
    if path.suffix == ".gz":
        return gzip.open(path, "rt")
    return open(path, "r")


def find_ohi_file(input_dir, filename):
    """Locate an OHI file in input_dir, accepting plain (.txt) or gzip (.txt.gz) variants."""
    plain   = input_dir / filename
    gzipped = input_dir / (filename + ".gz")
    if plain.exists():
        return plain
    if gzipped.exists():
        return gzipped
    return None


def format_attitude_line(sat_id, line):
    """Reformat a single OHI CSV line into SINEX SATELLITE/ATTITUDE_MODE format."""
    if "#+SATELLITE/ATTITUDE MODE" in line:
        return "+SATELLITE/ATTITUDE_MODE"
    if "#-SATELLITE/ATTITUDE MODE" in line:
        return "-SATELLITE/ATTITUDE_MODE\n"
    if "#DATE TIME START(UTC),END(UTC),ATTITUDE MODE" in line:
        return "*SVN_ DATE_TIME_START(UTC) END(UTC)___________ ATTITUDE_MODE"

    fields = line.split(",")
    widths = [20, 19, 9]
    formatted = " " + sat_id + " "
    for i, field in enumerate(fields):
        formatted += ("" if i == 0 else " ") + field[:widths[i]].ljust(widths[i])
    return formatted


def extract_attitude_block(path, sat_id):
    """Extract and reformat the SATELLITE/ATTITUDE MODE block from an OHI file.

    Returns a list of formatted lines, or an empty list if no attitude block is found.
    """
    lines = []
    in_block = False
    with open_ohi(path) as f:
        for raw in f:
            line = raw.strip()
            if "#+SATELLITE/ATTITUDE MODE" in line:
                in_block = True
            if in_block:
                lines.append(format_attitude_line(sat_id, line))
            if "#-SATELLITE/" in line:
                in_block = False
    return lines


def merge(input_dir, output_path):
    input_dir   = Path(input_dir)
    output_path = Path(output_path)

    found   = []
    missing = []
    for filename, sat_id in OHI_FILE_MAP:
        path = find_ohi_file(input_dir, filename)
        if path:
            found.append((path, sat_id))
        else:
            missing.append((filename, sat_id))

    for filename, sat_id in missing:
        print(f"Warning: OHI file not found for {sat_id} ({filename}[.gz]) — skipping.", file=sys.stderr)

    if not found:
        print("Error: No OHI files found in the input directory.", file=sys.stderr)
        sys.exit(1)

    processed = []
    with open(output_path, "w") as out:
        out.write("%=SNX\n")
        out.write("*" + "-" * 79 + "\n")
        out.write("*This file was created from the following OHI files using 'scripts/qzss_ohi_merge.py':\n")
        for path, sat_id in found:
            out.write(f"*  '{path}' ({sat_id})\n")
        out.write("*" + "-" * 79 + "\n\n")

        for path, sat_id in found:
            block = extract_attitude_block(path, sat_id)
            if not block:
                print(f"Warning: No SATELLITE/ATTITUDE MODE section in {path.name} ({sat_id}) — skipping.", file=sys.stderr)
                continue
            for line in block:
                out.write(line + "\n")
            processed.append(sat_id)

        out.write("%ENDSNX\n")

    print(f"Written to '{output_path}' ({len(processed)} satellites: {', '.join(processed)}).")


def main():
    parser = argparse.ArgumentParser(
        description="Merge QZSS OHI files into a Ginan SINEX SATELLITE/ATTITUDE_MODE file.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument(
        "input_dir",
        help="Directory containing JAXA OHI files (ohi-qzs*.txt or ohi-qzs*.txt.gz)",
    )
    parser.add_argument(
        "output_file",
        help="Output SINEX file path (e.g. qzss_yaw_modes.snx)",
    )
    args = parser.parse_args()
    merge(args.input_dir, args.output_file)


if __name__ == "__main__":
    main()
