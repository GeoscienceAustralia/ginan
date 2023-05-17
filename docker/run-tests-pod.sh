#!/bin/bash

set -euo pipefail

# download example tests
source /ginan/docker/tags
source /ginan/docker/run-aux.sh

/ginan/scripts/download_examples.py -d -p # tag is ignored for products and data tarballs

# run example tests
TEST_NUM=$1

cd /ginan/examples

ATOL=1E-4

case $TEST_NUM in
  1)
    pod -y ex21_pod_fit_gps.yaml | tee pod21.out
    mv pod21.out ex21/pod.out
    DIR="ex21"
    results2s3 $DIR
    ../scripts/download_examples.py --push $DIR --tag $TAG
    ../scripts/download_examples.py $DIR --tag $POD
    diffex $DIR/{*.sp3,pod.out}
    ;;
  2g) # ex22 GPS
    pod -y ex22_pod_fit_gps.yaml | tee pod22g.out
    mv pod22g.out ex22g/pod.out
    DIR="ex22g"
    results2s3 $DIR
    ../scripts/download_examples.py --push $DIR --tag $TAG
    ../scripts/download_examples.py $DIR --tag $POD
    diffex $DIR/{*.sp3,pod.out}
    ;;
  2r) # ex22 GLONASS
    pod -y ex22_pod_fit_glo.yaml | tee pod22r.out
    mv pod22r.out ex22r/pod.out
    DIR="ex22r"
    results2s3 $DIR
    ../scripts/download_examples.py --push $DIR --tag $TAG
    ../scripts/download_examples.py $DIR --tag $POD
    diffex $DIR/{*.sp3,pod.out}
    ;;
  2e) # ex22 GALILEO
    pod -y ex22_pod_fit_gal.yaml | tee pod22e.out
    mv pod22e.out ex22e/pod.out
    DIR="ex22e"
    results2s3 $DIR
    ../scripts/download_examples.py --push $DIR --tag $TAG
    ../scripts/download_examples.py $DIR --tag $POD
    diffex $DIR/{*.sp3,pod.out}
    ;;
  2c) # ex22 BEIDOU
    pod -y ex22_pod_fit_bds.yaml | tee pod22c.out
    mv pod22c.out ex22c/pod.out
    DIR="ex22c"
    results2s3 $DIR
    ../scripts/download_examples.py --push $DIR --tag $TAG
    ../scripts/download_examples.py $DIR --tag $POD
    diffex $DIR/{*.sp3,pod.out}
    ;;
  2j) # ex22 QZSS
    pod -y ex22_pod_fit_qzss.yaml | tee pod22j.out
    mv pod22j.out ex22j/pod.out
    DIR="ex22j"
    results2s3 $DIR
    ../scripts/download_examples.py --push $DIR --tag $TAG
    ../scripts/download_examples.py $DIR --tag $POD
    diffex $DIR/{*.sp3,pod.out}
    ;;
  3)
    pod -y ex23_pod_prd_gps.yaml | tee pod23.out
    mv pod23.out ex23/pod.out
    DIR="ex23"
    results2s3 $DIR
    ../scripts/download_examples.py --push $DIR --tag $TAG
    ../scripts/download_examples.py $DIR --tag $POD
    diffex $DIR/{*.sp3,pod.out}
    ;;
  4)
    pod -y ex24_pod_ic_gps.yaml | tee pod24.out
    mv pod24.out ex24/pod.out
    DIR="ex24"
    results2s3 $DIR
    ../scripts/download_examples.py --push $DIR --tag $TAG
    ../scripts/download_examples.py $DIR --tag $POD
    diffex $DIR/{*.sp3,pod.out}
    ;;
  5)
    pod -y ex25_pod_fit_gps.yaml | tee pod25.out
    mv pod25.out ex25/pod.out
    DIR="ex25"
    results2s3 $DIR
    ../scripts/download_examples.py --push $DIR --tag $TAG
    ../scripts/download_examples.py $DIR --tag $POD
    diffex $DIR/{*.sp3,pod.out}
    ;;
  6)
    pod -y ex26_pod_fit_meo.yaml | tee pod26.out
    mv pod26.out ex26/pod.out
    DIR="ex26"
    results2s3 $DIR
    ../scripts/download_examples.py --push $DIR --tag $TAG
    ../scripts/download_examples.py $DIR --tag $POD
    diffex $DIR/{*.sp3,pod.out}
    ;;
esac

