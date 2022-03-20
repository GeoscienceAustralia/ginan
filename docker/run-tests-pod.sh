#!/bin/bash

set -euo pipefail

# activate conda environment
eval "$(/root/.miniconda3/bin/conda shell.bash hook)"
conda activate gn37

# download example tests
cd /ginan
OLDTAG=f82e335
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
    cd ex21
    python ../../scripts/rms_bar_plot.py -i gag19424_igs19424_orbdiff_rtn.out -d . -c G >pod.rms
    python ../../scripts/download_examples.py --push --dirs ex21 --tag $TAG
    python ../../scripts/download_examples.py --dirs ex21 --tag $OLDTAG
    python ../../scripts/compare_pod_rms.py -ro pod.out -rr pod.rms -so ../solutions/ex21/pod.out -sr ../solutions/ex21/pod.rms -em 0.0002

    ;;
  2g) # ex22 GPS
    pod -y ex22_pod_fit_gps.yaml | tee pod22g.out
    mv pod22g.out ex22g/pod.out
    tar cfz run-results-ex22g.tar.gz ex22g
    aws s3 cp run-results-ex22g.tar.gz s3://ginan-pipeline-results/$TAG/
    cd ex22g
    python ../../scripts/rms_bar_plot.py -i gag20624__orbdiff_rtn.out -d . -c G >pod_G.rms
    python ../../scripts/download_examples.py --push --dirs ex22g --tag $TAG
    python ../../scripts/download_examples.py --dirs ex22g --tag $OLDTAG
    python ../../scripts/compare_pod_rms.py -ro pod.out -rr pod_G.rms -so ../solutions/ex22g/pod.out -sr ../solutions/ex22g/pod_G.rms -em 0.0002
    ;;
  2r) # ex22 GLONASS
    pod -y ex22_pod_fit_glo.yaml | tee pod22r.out
    mv pod22r.out ex22r/pod.out
    tar cfz run-results-ex22r.tar.gz ex22r
    aws s3 cp run-results-ex22r.tar.gz s3://ginan-pipeline-results/$TAG/
    cd ex22r
    python ../../scripts/rms_bar_plot.py -i gag20624__orbdiff_rtn.out -d . -c R >pod_R.rms
    python ../../scripts/download_examples.py --push --dirs ex22r --tag $TAG
    python ../../scripts/download_examples.py --dirs ex22r --tag $OLDTAG
    python ../../scripts/compare_pod_rms.py -ro pod.out -rr pod_R.rms -so ../solutions/ex22r/pod.out -sr ../solutions/ex22r/pod_R.rms -em 0.0002
    ;;
  2e) # ex22 GALILEO
    pod -y ex22_pod_fit_gal.yaml | tee pod22e.out
    mv pod22e.out ex22e/pod.out
    tar cfz run-results-ex22e.tar.gz ex22e
    aws s3 cp run-results-ex22e.tar.gz s3://ginan-pipeline-results/$TAG/
    cd ex22e
    python ../../scripts/rms_bar_plot.py -i gag20624__orbdiff_rtn.out -d . -c E >pod_E.rms
    python ../../scripts/download_examples.py --push --dirs ex22e --tag $TAG
    python ../../scripts/download_examples.py --dirs ex22e --tag $OLDTAG
    python ../../scripts/compare_pod_rms.py -ro pod.out -rr pod_E.rms -so ../solutions/ex22e/pod.out -sr ../solutions/ex22e/pod_E.rms -em 0.0002
    ;;
  2c) # ex22 BEIDOU
    pod -y ex22_pod_fit_bds.yaml | tee pod22c.out
    mv pod22c.out ex22c/pod.out
    tar cfz run-results-ex22c.tar.gz ex22c
    aws s3 cp run-results-ex22c.tar.gz s3://ginan-pipeline-results/$TAG/
    cd ex22c
    python ../../scripts/rms_bar_plot.py -i gag20624__orbdiff_rtn.out -d . -c C >pod_C.rms
    python ../../scripts/download_examples.py --push --dirs ex22c --tag $TAG
    python ../../scripts/download_examples.py --dirs ex22c --tag $OLDTAG
    python ../../scripts/compare_pod_rms.py -ro pod.out -rr pod_C.rms -so ../solutions/ex22c/pod.out -sr ../solutions/ex22c/pod_C.rms -em 0.0002
    ;;
  2j) # ex22 QZSS
    pod -y ex22_pod_fit_qzss.yaml | tee pod22j.out
    mv pod22j.out ex22j/pod.out
    tar cfz run-results-ex22j.tar.gz ex22j
    aws s3 cp run-results-ex22j.tar.gz s3://ginan-pipeline-results/$TAG/
    cd ex22j
    python ../../scripts/rms_bar_plot.py -i gag20624__orbdiff_rtn.out -d . -c J >pod_J.rms
    python ../../scripts/download_examples.py --push --dirs ex22j --tag $TAG
    python ../../scripts/download_examples.py --dirs ex22j --tag $OLDTAG
    python ../../scripts/compare_pod_rms.py -ro pod.out -rr pod_J.rms -so ../solutions/ex22j/pod.out -sr ../solutions/ex22j/pod_J.rms -em 0.0002
    ;;
  3)
    pod -y ex23_pod_prd_gps.yaml | tee pod23.out
    mv pod23.out ex23/pod.out
    tar cfz run-results-ex23.tar.gz ex23
    aws s3 cp run-results-ex23.tar.gz s3://ginan-pipeline-results/$TAG/
    cd ex23
    python ../../scripts/rms_bar_plot.py -i gag20010_igs20010_orbdiff_rtn.out -d . -c G >pod.rms
    python ../../scripts/download_examples.py --push --dirs ex23 --tag $TAG
    python ../../scripts/download_examples.py --dirs ex23 --tag $OLDTAG
    python ../../scripts/compare_pod_rms.py -ro pod.out -rr pod.rms -so ../solutions/ex23/pod.out -sr ../solutions/ex23/pod.rms -em 0.0002
    ;;
  4)
    pod -y ex24_pod_ic_gps.yaml | tee pod24.out
    mv pod24.out ex24/pod.out
    tar cfz run-results-ex24.tar.gz ex24
    aws s3 cp run-results-ex24.tar.gz s3://ginan-pipeline-results/$TAG/
    cd ex24
    python ../../scripts/rms_bar_plot.py -i gag20624_igs20624_orbdiff_rtn.out -d . -c G >pod.rms
    python ../../scripts/download_examples.py --push --dirs ex24 --tag $TAG
    python ../../scripts/download_examples.py --dirs ex24 --tag $OLDTAG
    python ../../scripts/compare_pod_rms.py -ro pod.out -rr pod.rms -so ../solutions/ex24/pod.out -sr ../solutions/ex24/pod.rms -em 0.0002
    ;;
  5)
    pod -y ex25_pod_fit_gps.yaml | tee pod25.out
    mv pod25.out ex25/pod.out
    tar cfz run-results-ex25.tar.gz ex25
    aws s3 cp run-results-ex25.tar.gz s3://ginan-pipeline-results/$TAG/
    cd ex25
    python ../../scripts/rms_bar_plot.py -i gag19424_igs19424_orbdiff_rtn.out -d . -c G >pod.rms
    python ../../scripts/download_examples.py --push --dirs ex25 --tag $TAG
    python ../../scripts/download_examples.py --dirs ex25 --tag $OLDTAG
    python ../../scripts/compare_pod_rms.py -ro pod.out -rr pod.rms -so ../solutions/ex25/pod.out -sr ../solutions/ex25/pod.rms -em 0.0002
    ;;
  6)
    pod -y ex26_pod_fit_meo.yaml | tee pod26.out
    mv pod26.out ex26/pod.out
    tar cfz run-results-ex26.tar.gz ex26
    aws s3 cp run-results-ex26.tar.gz s3://ginan-pipeline-results/$TAG/
    cd ex26
    python ../../scripts/rms_bar_plot.py -i gag21330_ilrsa.orb.lageos1.201128.v70_orbdiff_rtn.out -d . -c G >pod.rms
    python ../../scripts/download_examples.py --push --dirs ex26 --tag $TAG
    python ../../scripts/download_examples.py --dirs ex26 --tag $OLDTAG
    python ../../scripts/compare_pod_rms.py -ro pod.out -rr pod.rms -so ../solutions/ex26/pod.out -sr ../solutions/ex26/pod.rms -em 0.002
    ;;
esac

