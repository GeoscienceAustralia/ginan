#!/bin/bash

set -euo pipefail

# download example tests
source /ginan/docker/tags
source /ginan/docker/run-aux.sh

# start mongo DB
#/bin/systemctl start /usr/bin/mongod
#@/bin/systemctl status /usr/bin/mongod
mkdir /ginan/db
/usr/bin/mongod --dbpath /ginan/db --bind_ip 127.0.0.1 &
sleep 5

/ginan/scripts/download_example_input_data.py -d -p # tag is ignored for products and data tarballs

# run example tests
TEST_NUM=$1

cd /ginan/pipelineTests

ATOL=1E-4

case $TEST_NUM in
  1)
    ../bin/pod -y ex31_pod_fit_gps.yaml | tee pod.out
    mv pod.out ex31/pod_fit
    ../bin/pea -y ex31_pea_pp_netw_gnss_orb_ar.yaml | tee pea.out
    mv pea.out ex31/pea
    pod -y ex31_pod_ic_gps.yaml | tee pod.out
    mv pod.out ex31/pod_ic
    DIR="ex31"
    results2s3 $DIR
    ../scripts/download_example_input_data.py --push $DIR --tag $TAG
    ../scripts/download_example_input_data.py $DIR --tag $PEAPOD
    runAllAndDiffOnFailure diffex $DIR/pod_fit/{*.sp3,pod.out}
    runAllAndDiffOnFailure diffex $DIR/pea/{*etwork*.TRACE,*.snx,*.clk*} # todo: add *.erp
    runAllAndDiffOnFailure diffex $DIR/pod_ic/{*.sp3,pod.out}
    ;;
esac
