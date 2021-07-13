#!/bin/env bash

# This script is used as an interface between POD and PEA.
#
# Usage: "nohup ./run_podpea.sh > log.txt &"
#
# Authors: Tzupang Tseng
#
# Date: 04-03-2020


# Setup environment path
#------------------------
export POD=/data/acs/pod
export PEA=/data/acs/pea
export P=/data/test
#echo $P $POD


# Download GNSS satellite SP3 file
#---------------------------------
echo "====Download GNSS orbit files===="
echo

for (( GPSWEEK=2062; GPSWEEK<=2062; GPSWEEK++ ))
do
wget -N ftp://cddis.gsfc.nasa.gov/gnss/products/$GPSWEEK/igs*.sp3.Z
wget -N ftp://cddis.gsfc.nasa.gov/gnss/products/$GPSWEEK/igs*.erp.Z
wget -N ftp://cddis.gsfc.nasa.gov/gnss/products/$GPSWEEK/igs*.snx.Z
done

echo "====Data download completion===="
echo
GPSWEEK=$(( $GPSWEEK - 1 ))
# Decompress *.Z files
#---------------------
gzip -d *.Z


# Set up GPSWEEK DAY for data process
#------------------------------------
for GPSDAY in 20624 #"GPSWEEK"0 "GPSWEEK"1 "GPSWEEK"2 "GPSWEEK"3 "GPSWEEK"4 "GPSWEEK"5 "GPSWEEK"6 
do
# Start time
echo "====Processing GPS Week day $GPSDAY===="
echo
echo "====Start time: GNSS orbit and clock determination====" 
date 

 
# Clean up 
rm $P/POD_TEMP"$GPSDAY".txt
rm $P/ORBIT_TMP"$GPSDAY".txt
rm $P/*_srp*
rm $P/VEQ0*
rm $P/EQM0*


# Start to run POD
#-----------------
echo
echo "====Precise orbit determination and prediction====";

#echo $POD/bin/pod
$POD/bin/pod -m 1 -q 1 -k 1 -w 0 -s igs"$GPSDAY".sp3 -o igs"$GPSDAY".sp3 > gnss_rms_fit"$GPSDAY".txt


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
grep 'RMS-RTN ICRF CMP' $P/gnss_rms_fit"$GPSDAY".txt >> POD_TEMP"$GPSDAY".txt
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
# Check output file
#------------------
echo
echo "====Please check POD summary report on $GPSDAY===="
echo
echo "====Satellite orbit fitting completion===="



# Start to run PEA (Parameter Estimation Algorithm)
#--------------------------------------------------
echo
echo "====Parameter estimation procedure===="
echo

# Replace variables in PEA configuration
#--------------------------------------
#GPSNEXT=$(( $GPSDAY + 1 ))
#echo $GPSNEXT
#sed -i -- "s/gag"$GPSDAY"_orbits/gag"$GPSNEXT"_orbits/g" $PEA/config/EX03.yaml

echo "====Download station RINEX files===="
echo
#DOY=363
#for YEAR4 in 2019
#YEAR2=$(( $YEAR4 - 2000 ))
#do
##(Should we specify the station name for processing?)
#wget -N ftp://cddis.gsfc.nasa.gov/gnss/pub/gps/data/daily/$GPSYEAR4/$DOY/$YEAR2o/*.$YEAR2o.Z
#done

#gzip -d *.Z
#mv *.19o $PEA/example/EX03/data/


echo "====RINEX download completion===="


echo
echo "====Prepare files for running PEA===="
echo
echo "====Prepare GNSS orbit and earth rotation parameters===="
cp igs"$GPSDAY".sp3 $PEA/example/EX03/products/
cp igs19P"$GPSWEEK".erp   $PEA/example/EX03/products/
# replace the sting in the current onfiguration file (we may creat a temproary configuration file that contain certain strings ready for the replacement.)
echo 


echo "====Prepare orbit partials from POD===="
echo
cp gag"$GPSDAY"_orbits_partials.out $PEA/example/EX03/products/orb_partials/


echo "====Prepare navigation data and satellite DCB file===="
#brdm1990.19p (To be set up)
#CAS0MGXRAP_20191990000_01D_01D_DCB.BSX (To be set up)
echo


echo "====Prepare station SINEX file===="
cp igs19P"$GPSWEEK".snx $PEA/example/EX03/products/
sed -i -- "s/gfm/igs/g" $PEA/config/EX03.yaml

$PEA/cpp/build/pea --config $PEA/config/EX03.yaml > PEA_temp.txt


echo
echo "====Parameter estimation completion===="


# Generate SP3 file
#------------------
echo
echo "====Orbit integration procedure===="
echo
cp /data/acs/output/orb_pea.out $P/


# Chnage the POD option to the integration only
$POD/bin/pod -m 4 -q 2 -k 1 -w 0 -o igs"$GPSDAY".sp3 > orb_est_acc"$GPSDAY".txt

echo 


echo "==========================================================================" >> ORBIT_TMP"$GPSDAY".txt
echo "Orbit accuracy in Radial (R), Along-track (T) and Cross-track (N) in meter" >> ORBIT_TMP"$GPSDAY".txt
echo "==========================================================================" >> ORBIT_TMP"$GPSDAY".txt

grep 'RMS-RTN ICRF CMP' $P/orb_est_acc"$GPSDAY".txt >> ORBIT_TMP"$GPSDAY".txt

cp ORBIT_TMP"$GPSDAY".txt ORBIT_ACC"$GPSDAY".txt
echo "====Please check orbit accuracy report on $GPSDAY===="
done


# End of process
echo
echo "End of process:GNSS orbit and clock determination" 
date
#

