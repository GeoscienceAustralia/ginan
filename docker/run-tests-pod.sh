#!/bin/bash

set -euo pipefail

# activate conda environment
eval "$(/root/.miniconda3/bin/conda shell.bash hook)"
conda activate gn37

# download example tests
cd /ginan
python scripts/download_examples.py

# run example tests
cd /ginan/examples

TEST_NUM=$1

case $TEST_NUM in
  1)
    pod -y ex21_pod_fit_gps.yaml
    ;;
  2)
    pod -y ex22_pod_fit_gnss.yaml
    ;;
  3)
    pod -y ex23_pod_prd_gps.yaml
    ;;
  4)
    pod -y ex24_pod_ic_gps.yaml
    ;;
  5)
    pod -y ex25_pod_fit_gps.yaml
    ;;
  6)
    pod -y ex26_pod_fit_meo.yaml
    ;;
esac

