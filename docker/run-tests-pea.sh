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
    pea --config ex11_pea_pp_user_gps.yaml | tee pea11.out
    tar cfz run-results-ex11.tar.gz ex11
    aws s3 cp run-results-ex11.tar.gz s3://ginan-pipeline-results/$TAG/
    python ../scripts/download_examples.py --push --dirs ex11 --tag $TAG
    python ../scripts/download_examples.py --dirs ex11 --tag $OLDTAG
    python ../scripts/diffutil.py   -i ex11/ex1120624.snx            -o solutions/ex11/ex1120624.snx            --type sinex --passthrough -a $ATOL
    python ../scripts/diffutil.py   -i ex11/ex1120624.snx            -o solutions/ex11/ex1120624.snx            --type sinex --passthrough

    sed -i 's/REC_SYS_BIAS/   REC_CLOCK/g' solutions/ex11/*.TRACE
    for trace in `ls ex11/*.TRACE`; do
    tracebase="$(basename $trace)";
    python ../scripts/diffutil.py   -i $trace -o "solutions/ex11/$tracebase"  --type trace --passthrough -a $ATOL;
    python ../scripts/diffutil.py   -i $trace -o "solutions/ex11/$tracebase"  --type trace;
    done
    ;;
  2)
    pea --config ex12_pea_pp_user_gnss.yaml | tee pea12.out
    tar cfz run-results-ex12.tar.gz ex12
    aws s3 cp run-results-ex12.tar.gz s3://ginan-pipeline-results/$TAG/
    python ../scripts/download_examples.py --push --dirs ex12 --tag $TAG
    python ../scripts/download_examples.py --dirs ex12 --tag $OLDTAG
    python ../scripts/diffutil.py   -i ex12/ex1220624.snx            -o solutions/ex12/ex1220624.snx            --type sinex --passthrough -a $ATOL
    python ../scripts/diffutil.py   -i ex12/ex1220624.snx            -o solutions/ex12/ex1220624.snx            --type sinex --passthrough

    sed -i 's/REC_SYS_BIAS/   REC_CLOCK/g' ex12/*.TRACE
    python ../scripts/diffutil.py   -i ex12/ex12-ALIC201919900.TRACE -o solutions/ex12/ex12-ALIC201919900.TRACE --type trace --passthrough -a $ATOL
    python ../scripts/diffutil.py   -i ex12/ex12-ALIC201919900.TRACE -o solutions/ex12/ex12-ALIC201919900.TRACE --type trace
    ;;
  3)

    pea --config ex13_pea_pp_user_gps_sf.yaml | tee pea13.out
    tar cfz run-results-ex13.tar.gz ex13
    aws s3 cp run-results-ex13.tar.gz s3://ginan-pipeline-results/$TAG/
    python ../scripts/download_examples.py --push --dirs ex13 --tag $TAG
    python ../scripts/download_examples.py --dirs ex13 --tag $OLDTAG
    python ../scripts/diffutil.py   -i ex13/ex1320624.snx            -o solutions/ex13/ex1320624.snx            --type sinex --passthrough -a $ATOL
    python ../scripts/diffutil.py   -i ex13/ex1320624.snx            -o solutions/ex13/ex1320624.snx            --type sinex --passthrough

    sed -i 's/REC_SYS_BIAS/   REC_CLOCK/g' solutions/ex13/*.TRACE
    for trace in `ls ex13/*.TRACE`; do
    tracebase="$(basename $trace)";
    python ../scripts/diffutil.py   -i $trace -o "solutions/ex13/$tracebase"  --type trace --passthrough -a $ATOL;
    python ../scripts/diffutil.py   -i $trace -o "solutions/ex13/$tracebase"  --type trace;
    done
    ;;
  4)

    pea --config ex14_pea_pp_user_gnss_ar.yaml | tee pea14.out
    tar cfz run-results-ex14.tar.gz ex14
    aws s3 cp run-results-ex14.tar.gz s3://ginan-pipeline-results/$TAG/
    python ../scripts/download_examples.py --push --dirs ex14 --tag $TAG
    python ../scripts/download_examples.py --dirs ex14 --tag $OLDTAG
    python ../scripts/diffutil.py   -i ex14/ex1420624.snx            -o solutions/ex14/ex1420624.snx            --type sinex --passthrough -a $ATOL
    python ../scripts/diffutil.py   -i ex14/ex1420624.snx            -o solutions/ex14/ex1420624.snx            --type sinex --passthrough

    sed -i 's/REC_SYS_BIAS/   REC_CLOCK/g' solutions/ex14/TUG/*.TRACE
    python ../scripts/diffutil.py   -i ex14/TUG/ex14-ALIC201919900.TRACE -o solutions/ex14/TUG/ex14-ALIC201919900.TRACE --type trace --passthrough -a $ATOL
    python ../scripts/diffutil.py   -i ex14/TUG/ex14-ALIC201919900.TRACE -o solutions/ex14/TUG/ex14-ALIC201919900.TRACE --type trace
    ;;
  5)

    pea --config ex15_pea_rt_user_gnss_ar.yaml | tee pea15.out
    tar cfz run-results-ex15.tar.gz ex15
    aws s3 cp run-results-ex15.tar.gz s3://ginan-pipeline-results/$TAG/
    python ../scripts/download_examples.py --push --dirs ex15 --tag $TAG
    python ../scripts/download_examples.py --dirs ex15 --tag $OLDTAG
    python ../scripts/diffutil.py   -i ex15/ex1521912.snx            -o solutions/ex15/ex1521912.snx            --type sinex --passthrough -a $ATOL
    python ../scripts/diffutil.py   -i ex15/ex1521912.snx            -o solutions/ex15/ex1521912.snx            --type sinex --passthrough

    python ../scripts/diffutil.py   -i ex15/ex15-ALIC_2022-01-04_00:00.TRACE -o solutions/ex15/ex15-ALIC_2022-01-04_00:00.TRACE --type trace --passthrough -a $ATOL
    python ../scripts/diffutil.py   -i ex15/ex15-ALIC_2022-01-04_00:00.TRACE -o solutions/ex15/ex15-ALIC_2022-01-04_00:00.TRACE --type trace
    ;;
  6)

    pea --config ex16_pea_pp_ionosphere.yaml | tee pea16.out
    tar cfz run-results-ex16.tar.gz ex16
    aws s3 cp run-results-ex16.tar.gz s3://ginan-pipeline-results/$TAG/
    python ../scripts/download_examples.py --push --dirs ex16 --tag $TAG
    python ../scripts/download_examples.py --dirs ex16 --tag $OLDTAG
    
    python ../scripts/diffutil.py   -i ex16/AUSG1990.19I            -o solutions/ex16/AUSG1990.19I              --type ionex --passthrough -a $ATOL
    python ../scripts/diffutil.py   -i ex16/AUSG1990.19I            -o solutions/ex16/AUSG1990.19I              --type ionex --passthrough

    python ../scripts/diffutil.py   -i ex16/PEA2019-07-18_00\:00201919900.SUM -o solutions/ex16/PEA2019-07-18_00\:00201919900.SUM --type trace --states_only --passthrough -a $ATOL
    python ../scripts/diffutil.py   -i ex16/PEA2019-07-18_00\:00201919900.SUM -o solutions/ex16/PEA2019-07-18_00\:00201919900.SUM --type trace --states_only

    python ../scripts/diffutil.py   -i ex16/ex16_201919900.stec     -o solutions/ex16/ex16_201919900.stec       --type stec --passthrough  -a $ATOL
    python ../scripts/diffutil.py   -i ex16/ex16_201919900.stec     -o solutions/ex16/ex16_201919900.stec       --type stec
    # python ../scripts/diffsnx.py -i ex16/ex1620624.snx -o solutions/ex16/ex1620624.snx #should be removed from yaml (?) as ESTIMATE blk empty
    #loop over 87 TRACE files. space in name so need to find trick to get it to work
    ;;
  7)

    pea --config ex17_pea_pp_netw_gnss_ar.yaml | tee pea17.out
    tar cfz run-results-ex17.tar.gz ex17
    aws s3 cp run-results-ex17.tar.gz s3://ginan-pipeline-results/$TAG/
    python ../scripts/download_examples.py --push --dirs ex17 --tag $TAG
    python ../scripts/download_examples.py --dirs ex17 --tag $OLDTAG
    python ../scripts/diffutil.py   -i ex17/ex1720624.snx            -o solutions/ex17/ex1720624.snx            --type sinex --passthrough -a $ATOL
    python ../scripts/diffutil.py   -i ex17/ex1720624.snx            -o solutions/ex17/ex1720624.snx            --type sinex --passthrough

    python ../scripts/diffutil.py   -i ex17/PEA2019-07-18_00\:00201919900.SUM -o solutions/ex17/PEA2019-07-18_00\:00201919900.SUM --type trace --passthrough -a $ATOL
    python ../scripts/diffutil.py   -i ex17/PEA2019-07-18_00\:00201919900.SUM -o solutions/ex17/PEA2019-07-18_00\:00201919900.SUM --type trace
    # for trace in `ls ex17/*.TRACE`; do
    # tracebase="$(basename $trace)"; #need to check what blocks to compare. $ or res are not persent
    # python ../scripts/difftrace.py -i $trace -o "solutions/ex1.7_/$tracebase";
    # done
    ;;
  8)
    # TODO not working:
    # pea --config ex18_pea_rt_netw_gnss_ar.yaml
    ;;
esac

