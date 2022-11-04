#!/bin/sh
set -e
# clear 
# Setup environment path
#------------------------
export STD_FILE=/data/ginan/examples/products
export ORB_FILE=/mnt/e/2022_223/pod_ic
#export ORB_FILE_PRD=/mnt/e/2022_223/pod_prd
export CLK_FILE=/mnt/e/2022_223
#export CLK_FILE=/mnt/e/GINAN_BACKUP_DATA/ORB_CLK_05M/2022_185_NOR1_REAL-TIME/2022_223/
export PYTHON_FILES=/data/ginan/scripts

echo "//*****************************Orbit comparison (Integration)********************************//"
python3 $PYTHON_FILES/sp3_compare.py $STD_FILE/COD0MGXFIN_20222230000_01D_05M_ORB.SP3 $ORB_FILE/*.sp3 --ylims -50 50 -hlm ECF 
 

#  echo "//*****************************Orbit comparison (Prediction) ********************************//"
#  python3 $PYTHON_FILES/sp3_compare.py $STD_FILE/igs22172.sp3 $ORB_FILE_PRD/*.sp3 --ylims -50 50 -hlm ECF 


 echo "//********************3D-RMS of POD Orbit determination & prediction ************************//"
 python3 $PYTHON_FILES/rms_bar_plot.py -i $ORB_FILE/*_rtn.out -c G 
 #python3 $PYTHON_FILES/rms_bar_plot.py -i $ORB_FILE_PRD/*_rtn.out -c G 

# echo "//***************************Clock comparison**********************************//"
# python3 $PYTHON_FILES/compareClock.py --standard $STD_FILE/igs22171.clk_30s --test $CLK_FILE/pea/2022_1852022-07-04_0000.clk
# python3 $PYTHON_FILES/compareClock.py --standard $STD_FILE/igs22171.clk_30s --test $CLK_FILE/pea_clk/2022_1852022-07-04_0000.clk
#python3 $PYTHON_FILES/compareClock.py --standard $STD_FILE/igs22171.clk_30s --test $CLK_FILE/pea_clk/2022_1852022-07-04_0000.clk
#python3 $PYTHON_FILES/compareClock.py --standard $STD_FILE/igs22171.clk_30s --test $CLK_FILE/pea_clk_fw_orbit/2022_1852022-07-04_0000.clk_smoothed

#python3 $PYTHON_FILES/compareClock.py --standard $STD_FILE/igs20624.clk_30s --test $STD_FILE/jpl20624.clk
#python3 $PYTHON_FILES/compareClock.py --standard $STD_FILE/igs22171.clk_30s --test $STD_FILE/cnt22171.clk #jpl22171.clk #
echo "//----------------------Completed POD/PCD comparison.--------------------------//"
