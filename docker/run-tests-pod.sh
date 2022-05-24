#!/bin/bash

set -euo pipefail

# activate conda environment
eval "$(/root/.miniconda3/bin/conda shell.bash hook)"
conda activate gn37

# download example tests
cd /ginan
OLDTAG=70c9099
python scripts/download_examples.py -d -p # tag is ignored for products and data tarballs

# run example tests
cd /ginan/examples

TEST_NUM=$1

case $TEST_NUM in
  1)
    pod -y ex21_pod_fit_gps.yaml | tee pod21.out
    mv pod21.out ex21/pod.out
    tar cfz run-results-ex21.tar.gz ex21
    aws s3 cp run-results-ex21.tar.gz s3://ginan-pipeline-results/$TAG/
    python ../scripts/download_examples.py --push --dirs ex21 --tag $TAG
    python ../scripts/download_examples.py --dirs ex21 --tag $OLDTAG
    python ../scripts/diffutil.py -i ex21/pod.out -o solutions/ex21/pod.out -t pod -a 1E-4 --passthrough
    python ../scripts/diffutil.py -i ex21/gag19424.sp3 -o products/igs19424.sp3 -t sp3 -a 1E-1
    ;;
  2g) # ex22 GPS
    pod -y ex22_pod_fit_gps.yaml | tee pod22g.out
    mv pod22g.out ex22g/pod.out
    tar cfz run-results-ex22g.tar.gz ex22g
    aws s3 cp run-results-ex22g.tar.gz s3://ginan-pipeline-results/$TAG/
    python ../scripts/download_examples.py --push --dirs ex22g --tag $TAG
    python ../scripts/download_examples.py --dirs ex22g --tag $OLDTAG
    python ../scripts/diffutil.py -i ex22g/pod.out -o solutions/ex22g/pod.out -t pod -a 1E-4 --passthrough
    python ../scripts/diffutil.py -i ex22g/gag20624.sp3 -o products/COD0MGXFIN_20191990000_01D_05M_ORB.SP3 -t sp3 -a 1E-1
    ;;
  2r) # ex22 GLONASS
    pod -y ex22_pod_fit_glo.yaml | tee pod22r.out
    mv pod22r.out ex22r/pod.out
    tar cfz run-results-ex22r.tar.gz ex22r
    aws s3 cp run-results-ex22r.tar.gz s3://ginan-pipeline-results/$TAG/
    python ../scripts/download_examples.py --push --dirs ex22r --tag $TAG
    python ../scripts/download_examples.py --dirs ex22r --tag $OLDTAG
    python ../scripts/diffutil.py -i ex22r/pod.out -o solutions/ex22r/pod.out -t pod -a 1E-4 --passthrough
    python ../scripts/diffutil.py -i ex22r/gag20624.sp3 -o products/COD0MGXFIN_20191990000_01D_05M_ORB.SP3 -t sp3 -a 1E-1
    ;;
  2e) # ex22 GALILEO
    pod -y ex22_pod_fit_gal.yaml | tee pod22e.out
    mv pod22e.out ex22e/pod.out
    tar cfz run-results-ex22e.tar.gz ex22e
    aws s3 cp run-results-ex22e.tar.gz s3://ginan-pipeline-results/$TAG/
    python ../scripts/download_examples.py --push --dirs ex22e --tag $TAG
    python ../scripts/download_examples.py --dirs ex22e --tag $OLDTAG
    python ../scripts/diffutil.py -i ex22e/pod.out -o solutions/ex22e/pod.out -t pod -a 1E-4 --passthrough
    python ../scripts/diffutil.py -i ex22e/gag20624.sp3 -o products/COD0MGXFIN_20191990000_01D_05M_ORB.SP3 -t sp3 -a 1E-1
    ;;
  2c) # ex22 BEIDOU
    pod -y ex22_pod_fit_bds.yaml | tee pod22c.out
    mv pod22c.out ex22c/pod.out
    tar cfz run-results-ex22c.tar.gz ex22c
    aws s3 cp run-results-ex22c.tar.gz s3://ginan-pipeline-results/$TAG/
    python ../scripts/download_examples.py --push --dirs ex22c --tag $TAG
    python ../scripts/download_examples.py --dirs ex22c --tag $OLDTAG
    python ../scripts/diffutil.py -i ex22c/pod.out -o solutions/ex22c/pod.out -t pod -a 1E-4 --passthrough
    python ../scripts/diffutil.py -i ex22c/gag20624.sp3 -o products/COD0MGXFIN_20191990000_01D_05M_ORB.SP3 -t sp3 -a 1E-1
    ;;
  2j) # ex22 QZSS
    pod -y ex22_pod_fit_qzss.yaml | tee pod22j.out
    mv pod22j.out ex22j/pod.out
    tar cfz run-results-ex22j.tar.gz ex22j
    aws s3 cp run-results-ex22j.tar.gz s3://ginan-pipeline-results/$TAG/
    python ../scripts/download_examples.py --push --dirs ex22j --tag $TAG
    python ../scripts/download_examples.py --dirs ex22j --tag $OLDTAG
    python ../scripts/diffutil.py -i ex22j/pod.out -o solutions/ex22j/pod.out -t pod -a 1E-4 --passthrough
    python ../scripts/diffutil.py -i ex22j/gag20624.sp3 -o products/COD0MGXFIN_20191990000_01D_05M_ORB.SP3 -t sp3 -a 1E-1
    ;;
  3)
    pod -y ex23_pod_prd_gps.yaml | tee pod23.out
    mv pod23.out ex23/pod.out
    tar cfz run-results-ex23.tar.gz ex23
    aws s3 cp run-results-ex23.tar.gz s3://ginan-pipeline-results/$TAG/
    python ../scripts/download_examples.py --push --dirs ex23 --tag $TAG
    python ../scripts/download_examples.py --dirs ex23 --tag $OLDTAG
    python ../scripts/diffutil.py -i ex23/pod.out -o solutions/ex23/pod.out -t pod -a 1E-4 --passthrough
    python ../scripts/diffutil.py -i ex23/gag20010.sp3 -o products/igs20010.sp3 -t sp3 -a 1E-1
    ;;
  4)
    pod -y ex24_pod_ic_gps.yaml | tee pod24.out
    mv pod24.out ex24/pod.out
    tar cfz run-results-ex24.tar.gz ex24
    aws s3 cp run-results-ex24.tar.gz s3://ginan-pipeline-results/$TAG/
    python ../scripts/download_examples.py --push --dirs ex24 --tag $TAG
    python ../scripts/download_examples.py --dirs ex24 --tag $OLDTAG
    python ../scripts/diffutil.py -i ex24/pod.out -o solutions/ex24/pod.out -t pod -a 1E-4 --passthrough
    python ../scripts/diffutil.py -i ex24/gag20624.sp3 -o products/igs20624.sp3 -t sp3 -a 2E-1
    ;;
  5)
    pod -y ex25_pod_fit_gps.yaml | tee pod25.out
    mv pod25.out ex25/pod.out
    tar cfz run-results-ex25.tar.gz ex25
    aws s3 cp run-results-ex25.tar.gz s3://ginan-pipeline-results/$TAG/
    python ../scripts/download_examples.py --push --dirs ex25 --tag $TAG
    python ../scripts/download_examples.py --dirs ex25 --tag $OLDTAG
    python ../scripts/diffutil.py -i ex25/pod.out -o solutions/ex25/pod.out -t pod -a 1E-4 --passthrough
    python ../scripts/diffutil.py -i ex25/gag19424.sp3 -o products/igs19424.sp3 -t sp3 -a 1E-1
    ;;
  6)
    pod -y ex26_pod_fit_meo.yaml | tee pod26.out
    mv pod26.out ex26/pod.out
    tar cfz run-results-ex26.tar.gz ex26
    aws s3 cp run-results-ex26.tar.gz s3://ginan-pipeline-results/$TAG/
    python ../scripts/download_examples.py --push --dirs ex26 --tag $TAG
    python ../scripts/download_examples.py --dirs ex26 --tag $OLDTAG
    python ../scripts/diffutil.py -i ex26/pod.out -o solutions/ex26/pod.out -t pod -a 1E-4 --passthrough
    python ../scripts/diffutil.py -i ex26/gag21330.sp3 -o products/ilrsa.orb.lageos1.201128.v70.sp3 -t sp3 -a 1E-1
    ;;
esac

