#!/bin/env bash

# This script is used to run precise orbit determination (POD) using SP3 files.
#
# Usage: "nohup ./run_brd.sh args[0] args[1] args[2] args[3] args[4]> log.txt &"
#
# Variables:
#          args[0]: GPSYEAR
#          args[1]: Start DOY
#          args[2]: End DOY
#          args[3]: Path of executable files
#          args[4]: Path of data campaign
#
# Authors: Tzupang Tseng
#
# Date: 26-03-2020
#
# Modifcations: 24-04-2020 TT: add command line options to run this script dynamically
#


# Setup command line arguments
#-----------------------------
args=("$@")
echo "GPSYEAR->"                     ${args[0]}
echo "Start DOY->"                   ${args[1]}
echo "End DOY->"                     ${args[2]}
echo "Path of executable files->"    ${args[3]}
echo "Path of data campaign->"       ${args[4]}

GPSYEAR=${args[0]}
STDOY=${args[1]}
EDDOY=${args[2]}
POD=${args[3]}
P=${args[4]}


# Time system conversion
$POD/timesystem -i "$STDOY" -y "$GPSYEAR" > time.tmp
ARRAY=$(grep 'GPSWEEKDAY' time.tmp)
read -r STR STWEEK DAY <<<$(echo $ARRAY) ; echo "$STR|$STWEEK|$DAY"

$POD/timesystem -i "$EDDOY" -y "$GPSYEAR" > time.tmp
ARRAY=$(grep 'GPSWEEKDAY' time.tmp)
read -r STR EDWEEK DAY <<<$(echo $ARRAY) ; echo "$STR|$EDWEEK|$DAY"
#exit


# Download GNSS satellite SP3 file
#---------------------------------
echo "====Download GNSS orbit files===="
echo

for  (( GPSWEEK=$STWEEK; GPSWEEK<=$EDWEEK; GPSWEEK++ ))
do
wget -N ftp://cddis.gsfc.nasa.gov/gnss/products/$GPSWEEK/igs*.sp3.Z
done

echo "====Data download completion===="
echo

# Decompress *.Z files
#---------------------
gzip -d *.Z


for  (( GPSWEEK=$STWEEK; GPSWEEK<=$EDWEEK; GPSWEEK++ )) ####### for multi-GPSWEEK processing
do
# Set up GPSWEEK DAY for data process
#------------------------------------
for GPSDAY in "$GPSWEEK"0 "$GPSWEEK"1 "$GPSWEEK"2 "$GPSWEEK"3 "$GPSWEEK"4 "$GPSWEEK"5 "$GPSWEEK"6
do
# Start time
echo "====Processing GPS Week day $GPSDAY===="
echo
echo "====Start time: GNSS orbit determination====" 
date 

 
# Clean up 
echo
echo "====Initialize the clean-up===="
rm $P/POD_TEMP"$GPSDAY".txt
rm $P/ORBIT_TMP"$GPSDAY".txt
rm $P/*_srp*
rm $P/EQM0_*
rm $P/VEQ0_*

# Start to run POD
#-----------------
echo
echo "====Precise orbit determination and prediction====";

#echo $POD/bin/pod
$POD/pod -m 1 -q 1 -k 1 -s igs"$GPSDAY".sp3 -o igs"$GPSDAY".sp3 > gnss_rms_fit"$GPSDAY".txt


# Prepare a summary report for POD
#---------------------------------
echo "Summary report for POD" > POD_TEMP"$GPSDAY".txt
echo "======================" >> POD_TEMP"$GPSDAY".txt
echo                          >> POD_TEMP"$GPSDAY".txt
echo "Day of year and beta angle (deg)" >> POD_TEMP"$GPSDAY".txt
echo "================================" >> POD_TEMP"$GPSDAY".txt
echo                                    >> POD_TEMP"$GPSDAY".txt
grep 'day of year' $P/gnss_rms_fit"$GPSDAY".txt >> POD_TEMP"$GPSDAY".txt
echo                                    >> POD_TEMP"$GPSDAY".txt

echo "================================" >> POD_TEMP"$GPSDAY".txt
echo "Orbit fitting in Radial (R), Along-track (T) and Cross-track (N) in meter" >> POD_TEMP"$GPSDAY".txt
echo "=========================================================================" >> POD_TEMP"$GPSDAY".txt
grep 'RMS-RTN ICRF FIT' $P/gnss_rms_fit"$GPSDAY".txt >> POD_TEMP"$GPSDAY".txt
echo                                                 >> POD_TEMP"$GPSDAY".txt

echo "Solar radiation pressure" >> POD_TEMP"$GPSDAY".txt
echo "========================" >> POD_TEMP"$GPSDAY".txt

ECOM1="ECOM1"
ECOM2="ECOM2"
SBOXW="SBOXW"

SRP=$(echo "$line" | grep -o ECOM1 ECOM1_srp*G01.in)
if [ "$ECOM1" == "$SRP" ] ;then
echo "****ECOM1 SRP model is applied****"
echo "****NO ECOM2 and SBOXW****"

echo "ECOM1 is activated for SRP modeling" >> POD_TEMP"$GPSDAY".txt
echo "D0          Y0          B0          DC          DS          YC          YS          BC          BS" >> POD_TEMP"$GPSDAY".txt
echo "==================================================================================================" >> POD_TEMP"$GPSDAY".txt

rm ECOM1_srp.in
grep ECOM1 ECOM1_srp*.in >> POD_TEMP"$GPSDAY".txt 
echo                                                                                                      >> POD_TEMP"$GPSDAY".txt
fi


SRP=$(echo "$line" | grep -o ECOM2 ECOM2_srp*G01.in)
if [ "$ECOM2" == "$SRP" ] ;then
echo "****ECOM2 SRP model is applied****"
echo "****NO ECOM1 and SBOXW****"

echo "ECOM2 is activated for SRP modeling" >> POD_TEMP"$GPSDAY".txt
echo "D0          Y0          B0          D2C          D2S          D4C          D4S          BC          BS" >> POD_TEMP"$GPSDAY".txt
echo "======================================================================================================" >> POD_TEMP"$GPSDAY".txt

rm ECOM2_srp.in
grep ECOM2 ECOM2_srp*.in >> POD_TEMP"$GPSDAY".txt
echo                                                                                                          >> POD_TEMP"$GPSDAY".txt
fi


SRP=$(echo "$line" | grep -o SBOXW SBOXW_srp*G01.in)
if [ "$SBOXW" == "$SRP" ] ;then
echo "****SBOXW SRP model is applied****"
echo "****NO ECOM1 and ECOM2****"

echo "SBOXW is activated for SRP modeling" >> POD_TEMP"$GPSDAY".txt
echo "DX          DZ          DSP          Y0          B0          BC          BS" >> POD_TEMP"$GPSDAY".txt
echo "===========================================================================" >> POD_TEMP"$GPSDAY".txt

rm SBOXW_srp.in
grep SBOXW SBOXW_srp*.in >> POD_TEMP"$GPSDAY".txt
echo                                                                               >> POD_TEMP"$GPSDAY".txt
fi

echo "Orbit residuals" >> POD_TEMP"$GPSDAY".txt
echo "===============" >> POD_TEMP"$GPSDAY".txt
echo "GPS:     PRN = PRN"       >> POD_TEMP"$GPSDAY".txt
echo "GLONASS: PRN = PRN + 100" >> POD_TEMP"$GPSDAY".txt
echo "GALILEO: PRN = PRN + 200" >> POD_TEMP"$GPSDAY".txt
echo "BDS:     PRN = PRN + 300" >> POD_TEMP"$GPSDAY".txt
echo "QZSS:    PRN = PRN + 400" >> POD_TEMP"$GPSDAY".txt
echo
echo "MJD(day)       PRN        BLOCKTYPE        lambda        beta(deg)        del_u(deg)        yaw(deg)        ANGX(deg)       ANGY(deg)        ANGZ(deg)        dR(m)        dT(m)        dN(m)        FR(m^2/s)        FT(m^2/s)        FN(m^2/s)" >> POD_TEMP"$GPSDAY".txt
echo "================================================================================================================================================================================================================================================" >> POD_TEMP"$GPSDAY".txt
cat gag"$GPSDAY"_igs"$GPSDAY"_orbdiff_rtn.out >> POD_TEMP"$GPSDAY".txt

cp POD_TEMP"$GPSDAY".txt POD_SUMMARY"$GPSDAY".txt

# Final Clean up
echo
echo "====Finalize the clean-up===="
rm $P/gnss_rms_fit*.txt
rm $P/POD_TEMP"$GPSDAY".txt
rm $P/ORBIT_TMP"$GPSDAY".txt
rm $P/*_srp*
rm $P/EQM0*
rm $P/VEQ0*
rm $P/*.out
rm $P/emp_cpr.*
rm $P/emp_bias.*


# Check output file
#------------------
echo
echo "====Please check POD summary report on $GPSDAY===="
echo
echo "====Satellite orbit fitting completion===="


# End of process
echo
echo "End of process:GNSS orbit determination" 
date
#
done
done    #### for multi-GPSWEEK processing
