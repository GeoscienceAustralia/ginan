#!/usr/bin/env bash
set -euo pipefail

repo=/mnt/c/Users/rx/Documents/GINAN/ginan
input=/home/rx/GINAN/inputData
base=/home/rx/GINAN/exampleConfigs/osb_wum_fixed_orbit.yaml
common="$repo/exampleConfigs/zhang_global_2019199_user_validation.yaml"
float_overlay="$repo/exampleConfigs/zhang_global_2019199_user_float.yaml"
manifest="$repo/exampleConfigs/zhang_global_2019199/validation_manifest.csv"
result_root="$repo/Docs/zhangPppArResults/e9_user_validation"
output_root="$input/outputs"
jobs=${ZHANG_USER_JOBS:-1}
case_timeout=${ZHANG_USER_TIMEOUT_SECONDS:-180}

# Multiple long user filters can otherwise enter a BLAS/OpenMP wait in the
# dense linear-algebra path. Process-level parallelism is controlled by jobs.
linear_algebra_threads=${ZHANG_USER_LINEAR_ALGEBRA_THREADS:-1}
export OMP_NUM_THREADS=$linear_algebra_threads
export OPENBLAS_NUM_THREADS=$linear_algebra_threads
export MKL_NUM_THREADS=$linear_algebra_threads

mkdir -p "$result_root" "$output_root"
cd "$input"

run_case()
{
    local station=$1
    local filename=$2
    local region=$3
    local mode=$4
    local start=$5
    local ar=$6
    local description="zhang_e9_${station}_${mode}"
    local output="$output_root/$description"
    local console="$output/run.log"
    local completion_marker="$output/.pea_complete"
    local failure_marker="$output/.pea_failed"
    local trace="$output/Network-${description}-201919900.TRACE"
    local result="$result_root/${station}_${mode}.json"
    local -a configs=(-y "$base" "$common")

    if [[ "$ar" == "float" ]]; then
        configs+=("$float_overlay")
    fi

    if [[ -f "$result" ]]; then
        return
    fi

    mkdir -p "$output"
    if [[ ! -f "$completion_marker" ]]; then
        local status=0
        timeout --signal=TERM --kill-after=10s "${case_timeout}s" \
        "$repo/bin/pea" -q \
            "${configs[@]}" \
            -d "$description" \
            -r "$input/data/$filename" \
            --start_epoch "$start" \
            --end_epoch "2019-07-18 06:00:00" \
            >"$console" 2>&1 || status=$?
        if (( status != 0 )); then
            printf 'exit_status=%d\n' "$status" >"$failure_marker"
            return
        fi
        touch "$completion_marker"
        rm -f "$failure_marker"
    fi

    python3 "$repo/scripts/analyse_zhang_pppar.py" \
        "$trace" \
        --label "${station}_${mode}" \
        --category "$region" \
        --output "$result" \
        >"$output/analysis.json"
}

wait_for_slot()
{
    while (( $(jobs -pr | wc -l) >= jobs )); do
        wait -n
    done
}

while IFS=, read -r station filename region _latitude _longitude; do
    wait_for_slot
    run_case "$station" "$filename" "$region" full_ar "2019-07-18 00:00:00" ar &
    wait_for_slot
    run_case "$station" "$filename" "$region" full_float "2019-07-18 00:00:00" float &
    wait_for_slot
    run_case "$station" "$filename" "$region" restart_ar "2019-07-18 03:00:00" ar &
    wait_for_slot
    run_case "$station" "$filename" "$region" restart_float "2019-07-18 03:00:00" float &
done < <(tail -n +2 "$manifest")
wait

python3 "$repo/scripts/summarise_zhang_global_user_validation.py" \
    --manifest "$manifest" \
    --result-root "$result_root" \
    --trace-root "$output_root" \
    --output "$repo/Docs/zhangPppArResults/e9_user_validation_summary.json"
