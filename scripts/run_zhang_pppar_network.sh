#!/usr/bin/env bash
set -euo pipefail

cd /home/rx/GINAN/inputData

/mnt/c/Users/rx/Documents/GINAN/ginan/bin/pea \
  -y /home/rx/GINAN/exampleConfigs/osb_wum_fixed_orbit.yaml \
     /mnt/c/Users/rx/Documents/GINAN/ginan/exampleConfigs/zhang_full_rank_europe_stage1.yaml \
     /mnt/c/Users/rx/Documents/GINAN/ginan/exampleConfigs/zhang_full_rank_europe_stage2.yaml \
     /mnt/c/Users/rx/Documents/GINAN/ginan/exampleConfigs/zhang_pppar_europe_network.yaml
