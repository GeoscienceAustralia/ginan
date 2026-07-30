#!/usr/bin/env bash
set -euo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
data_root="${ZHANG_DATA_ROOT:-/home/rx/GINAN/inputData}"
base_config="${ZHANG_BASE_CONFIG:-/mnt/c/Users/rx/Documents/GINAN/osb_wum_fixed_orbit.yaml}"
pea="${ZHANG_PEA:-$repo/bin/pea}"
mode="${1:-smoke}"

selection_dir="$repo/exampleConfigs/zhang_global_2019199"
runtime_dir="$data_root/outputs/zhang_global_2019199_runtime"
runtime_base="$runtime_dir/base_products_only.yaml"
mkdir -p "$runtime_dir" "$data_root/run_logs"

if [[ "$mode" != "smoke" && "$mode" != "throughput" && "$mode" != "full" ]]; then
    echo "Usage: $0 [smoke|throughput|full]" >&2
    exit 2
fi

overlap="$(
    comm -12 \
        <(sort "$selection_dir/network_estimation.txt") \
        <(sort "$selection_dir/network_validation.txt")
)"
if [[ -n "$overlap" ]]; then
    echo "Validation leakage detected: $overlap" >&2
    exit 3
fi

# Ginan concatenates file lists from multiple YAML files.  Remove the base
# network's observation list before adding the selected 74-station list;
# otherwise duplicate streams and held-out validation stations would still be
# opened even though receiver exclusion later prevents validation leakage.
python3 - "$base_config" "$runtime_base" <<'PY'
from pathlib import Path
import sys
import yaml

source = Path(sys.argv[1])
target = Path(sys.argv[2])
config = yaml.safe_load(source.read_text(encoding="utf-8"))
inputs = config.setdefault("inputs", {})
inputs.pop("gnss_observations", None)
target.write_text(
    yaml.safe_dump(config, sort_keys=False),
    encoding="utf-8",
)
PY

configs=(
    "$runtime_base"
    "$selection_dir/zhang_global_2019199_inputs.yaml"
    "$repo/exampleConfigs/zhang_global_2019199_product.yaml"
)
if [[ "$mode" == "smoke" ]]; then
    configs+=("$repo/exampleConfigs/zhang_global_2019199_smoke.yaml")
elif [[ "$mode" == "throughput" ]]; then
    configs+=("$repo/exampleConfigs/zhang_global_2019199_throughput.yaml")
fi

cd "$data_root"
"$pea" -y "${configs[@]}" \
    > "$data_root/run_logs/zhang_global_2019199_${mode}.log" 2>&1
