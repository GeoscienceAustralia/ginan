#!/bin/bash

set -euo pipefail

# download example tests
source /ginan/docker/tags # PEA's tag as $PEA
source /ginan/docker/run-aux.sh

/ginan/scripts/download_example_input_data.py -d -p # tag is ignored for products and data tarballs

# run example tests
TEST_NUM=$1

# start mongo DB
#/bin/systemctl start /usr/bin/mongod
#@/bin/systemctl status /usr/bin/mongod
mkdir /ginan/db
/usr/bin/mongod --dbpath /ginan/db --bind_ip 127.0.0.1 &
sleep 5

cd /ginan/pipelineTests

ATOL=1E-4

# in all cases there is a pea.out file but no sensible way to compare it
case $TEST_NUM in
  11)
    pea --config ex11_pea_pp_user_gps.yaml -v | tee pea11.out
    DIR="ex11"
    results2s3 $DIR
    ../scripts/download_example_input_data.py $DIR --tag $TAG --push
    ../scripts/download_example_input_data.py $DIR --tag $PEA
    runAllAndDiffOnFailure diffex $DIR/{*snx,!(*Network*).TRACE*}
    ;;
  12)
    pea --config ex12_pea_pp_user_gnss.yaml | tee pea12.out
    DIR="ex12"
    results2s3 $DIR
    ../scripts/download_example_input_data.py $DIR --tag $TAG --push
    ../scripts/download_example_input_data.py $DIR --tag $PEA
    runAllAndDiffOnFailure diffex $DIR/{*snx,!(*Network*).TRACE}
    ;;
  13)
    pea --config ex13_pea_pp_user_gps_sf.yaml | tee pea13.out
    DIR="ex13"
    results2s3 $DIR
    ../scripts/download_example_input_data.py $DIR --tag $TAG --push
    ../scripts/download_example_input_data.py $DIR --tag $PEA
    runAllAndDiffOnFailure diffex $DIR/{*snx,!(*Network*).TRACE}
    ;;
  14)
    pea --config ex14_pea_pp_user_gnss_ar.yaml | tee pea14.out # ex14 run 5
    DIR="ex14"
    results2s3 $DIR
    ../scripts/download_example_input_data.py $DIR --tag $TAG --push
    ../scripts/download_example_input_data.py $DIR --tag $PEA
    runAllAndDiffOnFailure diffex $DIR/{*snx,!(*Network*).TRACE}
    ;;
  15)
    # TODO: broken recordings maybe
    #pea --config ex15_pea_rt_user_gnss_ar.yaml | tee pea15.out
    #DIR="ex15"
    #results2s3 $DIR
    #../scripts/download_example_input_data.py $DIR --tag $TAG --push
    #../scripts/download_example_input_data.py $DIR --tag $PEA
    #runAllAndDiffOnFailure diffex $DIR/{*snx,!(*Network*).TRACE}
    ;;
  16)
    pea --config ex16_pea_pp_ionosphere.yaml | tee pea16.out
    DIR="ex16"
    results2s3 $DIR
    ../scripts/download_example_input_data.py $DIR --tag $TAG --push
    ../scripts/download_example_input_data.py $DIR --tag $PEA
    runAllAndDiffOnFailure diffex $DIR/{*.INX*,*Network*.TRACE,*stec}
    ;;
  17)
    pea --config ex17_pea_pp_netw_gnss_ar.yaml | tee pea17.out
    DIR="ex17"
    results2s3 $DIR
    ../scripts/download_example_input_data.py $DIR --tag $TAG --push
    ../scripts/download_example_input_data.py $DIR --tag $PEA
    runAllAndDiffOnFailure diffex $DIR/{*snx,*Network*.TRACE}
    ;;
  41)
    pea --config ex41_gin2_pp_user.yaml -v | tee pea41.out
    DIR="ex41"
    results2s3 $DIR
    ../scripts/download_example_input_data.py $DIR --tag $TAG --push
    ../scripts/download_example_input_data.py $DIR --tag $PEA
    runAllAndDiffOnFailure diffex $DIR/{*.TRACE,*.TRACE}
    ;;
  2)
    pea --config ex02_fit_sp3_pseudoobs.yaml -v | tee pea02.out
    DIR="ex02"
    results2s3 $DIR
    ../scripts/download_example_input_data.py $DIR --tag $TAG --push
    ../scripts/download_example_input_data.py $DIR --tag $PEA
    runAllAndDiffOnFailure diffex $DIR/{*.TRACE,*.TRACE}
    ;;
esac
