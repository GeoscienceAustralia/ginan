#!/usr/bin/env bash
set -euo pipefail
cd /home/rx/GINAN/inputData
/mnt/c/Users/rx/Documents/GINAN/ginan/bin/pea -q \
  -y /mnt/c/Users/rx/Documents/GINAN/ginan/exampleConfigs/zhang_global_2024199_180/zhang_global_2024199_180_base.yaml \
     /mnt/c/Users/rx/Documents/GINAN/ginan/exampleConfigs/zhang_global_2024199_180/zhang_global_2024199_180_inputs.yaml \
     /mnt/c/Users/rx/Documents/GINAN/ginan/exampleConfigs/zhang_global_2024199_180/overlays/zhang_global_2024199_180_product.yaml \
     /mnt/c/Users/rx/Documents/GINAN/ginan/exampleConfigs/zhang_global_2024199_180/overlays/zhang_global_2024199_180_e29_180network_product_1h.yaml \
     /mnt/c/Users/rx/Documents/GINAN/ginan/exampleConfigs/zhang_global_2024199_180/overlays/zhang_global_2024199_180_e29_service_30s_1h.yaml \
     /mnt/c/Users/rx/Documents/GINAN/ginan/Docs/zhangPppArResults/zhang_global_2024199_180_rq6_multiepoch_001000_001600_20260813.yaml \
  -d zhang_global_2024199_180_rq6_multiepoch_001000_001600_20260813
