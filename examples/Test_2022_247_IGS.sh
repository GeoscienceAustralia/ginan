#!/bin/sh
set -e
clear

echo "Orbit fitting"
../bin/pod -y ex31_pod_fit_gps_VD_2022_247_IGS.yaml

# echo "Orbit correction"
# ../bin/pea --config ex31_pea_pp_netw_gnss_orb_ar_VD_2022_247_IGS.yaml

echo "Orbit integration"
../bin/pod -y ex31_pod_ic_gps_VD_2022_247_IGS.yaml

# echo "Clock & bias estimation"
# ../bin/pea --config ex31_pea_pp_netw_gnss_clk_ar_VD_2019_199.yaml

echo "POD/PCD complete."
