#!/bin/bash

set -euo pipefail

# activate conda environment
eval "$(/root/.miniconda3/bin/conda shell.bash hook)"
conda activate gn37

# download example tests
source /ginan/docker/tags # PEA's tag as $PEA
source /ginan/docker/run-aux.sh

/ginan/scripts/download_examples.py -d -p # tag is ignored for products and data tarballs

# run example tests
TEST_NUM=$1

# start mongo DB
#/bin/systemctl start /usr/bin/mongod
#@/bin/systemctl status /usr/bin/mongod
mkdir /ginan/db
/usr/bin/mongod --dbpath /ginan/db --bind_ip 127.0.0.1 &
sleep 5

cd /ginan/examples

ATOL=1E-4

# in all cases there is a pea.out file but no sensible way to compare it
case $TEST_NUM in
  1)
    pea --config ex11_pea_pp_user_gps.yaml -V | tee pea11.out
    DIR="ex11"
    results2s3 $DIR
    ../scripts/download_examples.py --dirs $DIR --tag $TAG --push
    ../scripts/download_examples.py --dirs $DIR --tag $PEA
    diffex $DIR/{*snx,!(*Network*).TRACE*}
    ;;
  2)
    pea --config ex12_pea_pp_user_gnss.yaml | tee pea12.out
    DIR="ex12"
    results2s3 $DIR
    ../scripts/download_examples.py --dirs $DIR --tag $TAG --push
    ../scripts/download_examples.py --dirs $DIR --tag $PEA
    diffex $DIR/{*snx,!(*Network*).TRACE*}
    ;;
  3)
    pea --config ex13_pea_pp_user_gps_sf.yaml | tee pea13.out
    DIR="ex13"
    results2s3 $DIR
    ../scripts/download_examples.py --dirs $DIR --tag $TAG --push
    ../scripts/download_examples.py --dirs $DIR --tag $PEA
    diffex $DIR/{*snx,!(*Network*).TRACE*}
    ;;
  4)
    pea --config ex14_pea_pp_user_gnss_ar.yaml | tee pea14.out # ex14 run 5
    DIR="ex14"
    results2s3 $DIR
    ../scripts/download_examples.py --dirs $DIR --tag $TAG --push
    ../scripts/download_examples.py --dirs $DIR --tag $PEA
    diffex $DIR/{*snx,!(*Network*).TRACE*}
    ;;
  5)
    pea --config ex15_pea_rt_user_gnss_ar.yaml | tee pea15.out
    DIR="ex15"
    results2s3 $DIR
    ../scripts/download_examples.py --dirs $DIR --tag $TAG --push
    ../scripts/download_examples.py --dirs $DIR --tag $PEA
    diffex $DIR/{*snx,!(*Network*).TRACE*}
    ;;
  6)
    pea --config ex16_pea_pp_ionosphere.yaml | tee pea16.out
    DIR="ex16"
    results2s3 $DIR
    ../scripts/download_examples.py --dirs $DIR --tag $TAG --push
    ../scripts/download_examples.py --dirs $DIR --tag $PEA
    diffex $DIR/{*.*I,*Network*.TRACE,*stec}
    ;;
  7)
    pea --config ex17_pea_pp_netw_gnss_ar.yaml | tee pea17.out
    DIR="ex17"
    results2s3 $DIR
    ../scripts/download_examples.py --dirs $DIR --tag $TAG --push
    ../scripts/download_examples.py --dirs $DIR --tag $PEA
    diffex $DIR/{*snx,*Network*.TRACE}
    ;;
  8)
    # TODO not working:
    # pea --config ex18_pea_rt_netw_gnss_ar.yaml
    ;;
esac

