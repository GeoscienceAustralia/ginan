#!/bin/sh
set -e
clear 
# Setup environment path
#------------------------
export STD_FILE=/data/ginan/scripts/ION
export CLK_FILE=/data/ginan/scripts/ION
export PYTHON_FILES=/data/ginan/scripts

echo "//**********************Orbit comparison (Integration)*************************//"
# python3 $PYTHON_FILES/sp3_compare.py $STD_FILE/COD0MGXFIN_20222230000_01D_05M_ORB.SP3 $STD_FILE/GBM0MGXRAP_20222230000_01D_05M_ORB_GE.SP3 --ylims -50 50 -hlm ECF 
python3 $PYTHON_FILES/sp3_compare.py $STD_FILE/COD0MGXFIN_20213520000_01D_05M_ORB.SP3 $STD_FILE/cnt21886_GEC.sp3 --ylims -10 10 -hlm ECF 
python3 /data/ginan/scripts/sp3_compare.py /data/ginan/scripts/ION/COD0MGXFIN_20213520000_01D_05M_ORB.SP3 /data/ginan/scripts/ION/cnt21886_G.sp3 --ylims -20 20 -hlm ECF 
# echo "//***************************Clock comparison**********************************//"
# python3 $PYTHON_FILES/compareClock.py --standard $STD_FILE/COD0MGXFIN_20222230000_01D_30S_CLK.CLK --test $STD_FILE/GBM0MGXRAP_20222230000_01D_30S_CLK.CLK

echo "//----------------------Completed POD/PCD comparison.--------------------------//"
