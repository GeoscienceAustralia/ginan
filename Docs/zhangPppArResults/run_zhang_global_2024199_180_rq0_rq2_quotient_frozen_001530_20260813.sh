#!/usr/bin/env bash
set -euo pipefail

export ZHANG_DATA_ROOT=/home/rx/GINAN/inputData
export ZHANG_STATION_COUNT=180
export ZHANG_CONFIG_DESCRIPTION=zhang_global_2024199_180_rq0_rq2_quotient_frozen_001530_20260813
export ZHANG_PEA=/mnt/c/Users/rx/Documents/GINAN/ginan/bin/pea

exec /mnt/c/Users/rx/Documents/GINAN/ginan/scripts/run_zhang_2024_experiment.sh \
  zhang_global_2024199_180_e29_180network_product_1h.yaml \
  zhang_global_2024199_180_e29_service_30s_1h.yaml \
  /mnt/c/Users/rx/Documents/GINAN/ginan/Docs/zhangPppArResults/zhang_global_2024199_180_rq0_rq2_quotient_frozen_001530_20260813.yaml
