#!/usr/bin/env bash
set -euo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
data_root="${ZHANG_DATA_ROOT:-/home/rx/GINAN/inputData}"
pea="${ZHANG_PEA:-$repo/bin/pea}"
station_count="${ZHANG_STATION_COUNT:-180}"
suite="$repo/exampleConfigs/zhang_global_2024199_${station_count}"
base="$suite/zhang_global_2024199_${station_count}_base.yaml"
inputs="$suite/zhang_global_2024199_${station_count}_inputs.yaml"
product="$suite/overlays/zhang_global_2024199_${station_count}_product.yaml"

if [[ $# -lt 1 ]]; then
    echo "Usage: $0 <migrated-overlay.yaml> [additional-overlay.yaml ...]" >&2
    exit 2
fi
overlays=()
for overlay in "$@"; do
    if [[ "$overlay" != /* ]]; then
        overlay="$suite/overlays/$overlay"
    fi
    overlays+=("$overlay")
done
for required in "$pea" "$base" "$inputs" "$product" \
                "${overlays[@]}" "$suite/network_selection_audit.json"; do
    if [[ ! -s "$required" ]]; then
        echo "Missing 2024/${station_count}-station experiment input: $required" >&2
        exit 3
    fi
done

python3 - "$suite/network_selection_audit.json" "$station_count" <<'PY'
import json, sys
audit = json.load(open(sys.argv[1]))
counts = audit["counts"]
expected = int(sys.argv[2])
if expected < 150:
    raise SystemExit(f"Configured station count {expected} is below the 150-station gate")
if counts["estimation"] != expected or counts["validation"] != 20:
    raise SystemExit(
        f"Network gate failed: estimation={counts['estimation']} "
        f"validation={counts['validation']}")
PY

last_overlay="${overlays[$((${#overlays[@]} - 1))]}"
description="${ZHANG_CONFIG_DESCRIPTION:-$(basename "$last_overlay" .yaml)}"
output="$data_root/outputs/$description"
if [[ -e "$output" ]]; then
    echo "Refusing to overwrite experiment output: $output" >&2
    exit 4
fi

cd "$data_root"
exec "$pea" -q -y "$base" "$inputs" "$product" "${overlays[@]}" -d "$description"
