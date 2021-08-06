#!/bin/bash

set -euo pipefail

# activate conda environment
eval "$(/root/.miniconda3/bin/conda shell.bash hook)"
conda activate gn37

# download example tests
cd /ginan
python scripts/download_examples.py

# run example tests
TEST_NUM=$1

cd /ginan/examples

case $TEST_NUM in
  1)
    pea --config ex11_pea_pp_user_gps.yaml
    ;;
  2)
    pea --config ex12_pea_pp_user_gnss.yaml
    ;;
  3)
    pea --config ex13_pea_pp_user_gps_sf.yaml
    ;;
  4)
    pea --config ex14_pea_pp_user_gnss_ar.yaml
    ;;
  5)
    # realtime: needs auscors user/pass in config: skipped
    ## pea --config ex15_pea_rt_user_gnss_ar.yaml
    ;;
  6)
    pea --config ex16_pea_pp_ionosphere.yaml
    ;;
  7)
    pea --config ex17_pea_pp_netw_gnss_ar.yaml
    ;;
  8)
    # TODO not working:
    # pea --config ex18_pea_rt_netw_gnss_ar.yaml
    ;;
esac
