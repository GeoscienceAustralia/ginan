#!/bin/sh
set -e

# echo "Orbit fitting"
# ../bin/pod -y ex31_pod_fit_gps_VD_2022_185.yaml

# echo "Orbit correction"
# ../bin/pea --config ex31_pea_pp_netw_gnss_orb_ar_VD_2022_185.yaml


echo "Orbit integration & prediction"
 ../bin/pod -y ex31_pod_ic_gps_VD_2022_185.yaml


# echo "Orbit prediction"
#  ../bin/pod -y ex31_pod_prd_gps_VD_2022_185.yaml

# echo "Clock & bias estimation"
# #../bin/pea --config ex31_pea_pp_netw_gnss_clk_ar_VD_2022_185.yaml
# ../bin/pea --config ex31_pea_rt_netw_gnss_clk_ar_VD_2022_185.yaml

echo "POD/PCD complete."

