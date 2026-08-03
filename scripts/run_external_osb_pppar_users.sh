#!/usr/bin/env bash
set -euo pipefail

repo=/mnt/c/Users/rx/Documents/GINAN/ginan
input=/home/rx/GINAN/inputData
base="$repo/exampleConfigs/ppp_example.yaml"
validation="$repo/exampleConfigs/external_osb_pppar_europe_user.yaml"

cd "$input"

for station in MATE DYNG NICO
do
    "$repo/bin/pea" \
        -y "$base" \
           "$validation" \
           "$repo/exampleConfigs/external_osb_user_${station}.yaml"
done
