#!/usr/bin/env bash
set -euo pipefail

repo=/mnt/c/Users/rx/Documents/GINAN/ginan
input=/home/rx/GINAN/inputData
base=/home/rx/GINAN/exampleConfigs/osb_wum_fixed_orbit.yaml

cd "$input"

"$repo/scripts/run_zhang_pppar_network.sh"

run_user()
{
    local station=$1
    local category=$2

    "$repo/bin/pea" \
        -y "$base" \
           "$repo/exampleConfigs/zhang_pppar_europe_user.yaml" \
           "$repo/exampleConfigs/zhang_pppar_user_${station}.yaml"

    python3 "$repo/scripts/analyse_zhang_pppar.py" \
        "$input/outputs/zhang_pppar_user_${station}/Network-zhang_pppar_user_${station}-201919900.TRACE" \
        --label "${station}_FIXED_STRICT" \
        --category "$category" \
        --output "$repo/Docs/zhangPppArResults/${station}_FIXED_STRICT.json"
}

run_user MATE inside
run_user DYNG edge
run_user NICO outside
