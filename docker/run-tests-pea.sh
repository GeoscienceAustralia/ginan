#!/bin/bash

set -euo

# download example tests
source /ginan/docker/tags # PEA's tag as $PEA
source /ginan/docker/run-aux.sh

# /ginan/scripts/download_example_input_data.py -d -p # tag is ignored for products and data tarballs

# run example tests
TEST_NUM=$1

mkdir /ginan/db
/usr/bin/mongod --dbpath /ginan/db --bind_ip 127.0.0.1 &
sleep 5

cd /ginan/pipelineTests

ATOL=1E-4

case $TEST_NUM in
  41)
    pea --config a.yaml -v | tee pea41.out
    DIR="ex41" ../scripts/run_and_check_tests.sh
               ../scripts/run_and_check_tests.sh
#     results2s3 $DIR
#     ../scripts/download_example_input_data.py $DIR --tag $TAG --push
#     ../scripts/download_example_input_data.py $DIR --tag $PEA
#     runAllAndDiffOnFailure diffex $DIR/{*.TRACE,*.TRACE}
    ;;
    
#   "41")
#     mkdir ex41
#     pea --config ex41_gin2_pp_user.yaml -v -n 10 | tee pea.out
# #     DIR="ex41"
# #     results2s3 $DIR
#     ;;
    
  "02")
    mkdir ex02
    pea --config ex02_fit_sp3_pseudoobs.yaml -v -n 10 | tee pea.out
#     DIR="ex02"
#     results2s3 $DIR
    ;;
    
  "16")
    mkdir ex16
    pea --config ex16_pea_pp_ionosphere.yaml -n 10 | tee pea.out
#     DIR="ex16"
#     results2s3 $DIR
    ;;
esac
