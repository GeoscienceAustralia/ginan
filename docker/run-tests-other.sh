#!/bin/bash
set -euo pipefail
GINAN=/ginan
PATH=$GINAN/bin:$PATH


# download example tests
source $GINAN/docker/tags
source $GINAN/docker/run-aux.sh

# /ginan/scripts/download_examples.py -d -p # tag is ignored for products and data tarballs
$GINAN/scripts/download_example_input_data.py -d -p -l # tag is ignored for products and data tarballs

#ensure mongo is running
mkdir -p $GINAN/db
/usr/bin/mongod --dbpath $GINAN/db --bind_ip 127.0.0.1 &
sleep 5

# run example tests
TEST_NUM=$1

cd $GINAN/pipelineTests

ATOL=1E-4
# to add new examples one needs to add a dictionary to EX_GLOB_DICT so download_example_input_data.py knows which files to push, otherwise it will fail. Also, make sure that example dir name starts with `ex` and has no more than 3 chars following (at least for now). If a group of examples is being created, e.g. ex9* - add an example type to get_example_type and a respective tag to the `tags` file.

case $TEST_NUM in
  1)
    DIR="ex51"
    mkdir -p $DIR
    make_otl_blq --type o --config ex51_otl_fes2014b_gb.yaml --code 'ALIC 50137M0014' --location 133.8855 -23.6701 --output $DIR/alic.blq
    make_otl_blq --type o --config ex51_otl_fes2014b_gb.yaml --code 'BRO1 50176M003' --location 122.2090 -18.0039 --output $DIR/bro1.blq

    export OMP_NUM_THREADS=2 #gets killed in the pipeline if not overridden
    make_otl_blq --type o --config ex51_otl_fes2014b_gb.yaml --input ../inputData/loading/sites_coastal50.csv --output $DIR/coastal50.blq
    make_otl_blq --type o --config ex51_otl_fes2014b_prem.yaml --input ../inputData/loading/sites_coastal50.csv --output $DIR/coastal50_PREM.blq

    results2s3 $DIR
     ../scripts/download_example_input_data.py --push $DIR --tag $TAG
     ../scripts/download_example_input_data.py $DIR --tag $OTHER
    ATOL=1.3E-2 # BRST has diff of 1.23E-2
    runAllAndDiffOnFailure diffex $DIR/*.blq
    diffutil -i $DIR/coastal50_PREM.blq ../inputData/loading/blq/C50_FES2014b_PREM_CE.blq -a $ATOL
    ;;
  2)
    DIR="ex52"
    mkdir -p $DIR
    interpolate_loading --type o --grid ../inputData/loading/grids/oceantide.nc --code 'ALIC 50137M0014' --location 133.8855 -23.6701 --output $DIR/alic.blq
    interpolate_loading --type o --grid ../inputData/loading/grids/oceantide.nc --code 'BRO1 50176M003' --location 122.2090 -18.0039 --output $DIR/bro1.blq
    interpolate_loading --type o --grid ../inputData/loading/grids/oceantide.nc --input ../inputData/loading/sites_coastal50.csv --output $DIR/coastal50.blq
    results2s3 $DIR
     ../scripts/download_example_input_data.py --push $DIR --tag $TAG
     ../scripts/download_example_input_data.py $DIR --tag $OTHER
    ATOL=1E-2
    runAllAndDiffOnFailure diffex $DIR/*.blq
    ;;
  3)
    pea --config ex43_pea_pp_user_gps.yaml -v | tee pea43.out
    sed -f edit_ex43.sed < ex43_pea_pp_user_gps.yaml > ex43a_pea_pp_user_gps.yaml
    pea --config ex43a_pea_pp_user_gps.yaml -v | tee pea43a.out # TODO which branch is this config in? Need to modify output dir
    DIR="ex43" # having ex11a or ex11 will download results with PEA TAG, not OTHER, if no TAG provided
    results2s3 $DIR
    ../scripts/download_example_input_data.py $DIR --tag $TAG --push
    ../scripts/download_example_input_data.py $DIR --tag $OTHER
    DIR="ex43a" # having ex11a or ex11 will download results with PEA TAG, not OTHER, if no TAG provided
    results2s3 $DIR
    ../scripts/download_example_input_data.py $DIR --tag $TAG --push
    ../scripts/download_example_input_data.py $DIR --tag $OTHER
    runAllAndDiffOnFailure diffex $DIR/{*snx,!(*Network*).TRACE}
    DIR="ex43" # having ex11a or ex11 will download results with PEA TAG, not OTHER, if no TAG provided
    runAllAndDiffOnFailure diffex $DIR/{*snx,!(*Network*).TRACE}
    diffutil -a 1e-4 -i ex43/ex43-AGGO*.TRACE ex43a/ex43a-AGGO*.TRACE
    diffutil -a 1e-4 -i ex43/ex43-ALIC*.TRACE ex43a/ex43a-ALIC*.TRACE
    diffutil -a 1e-4 -i ex43/ex43-BAKO*.TRACE ex43a/ex43a-BAKO*.TRACE
    diffutil -a 1e-4 -i ex43/ex43-COCO*.TRACE ex43a/ex43a-COCO*.TRACE
    ;;
  9)
    pea --config ex41_gin2_pp_user.yaml
    DIR="ex41"
    results2s3 $DIR
     ../scripts/download_example_input_data.py $DIR --tag $TAG --push
     ../scripts/download_example_input_data.py $DIR --tag $OTHER
     runAllAndDiffOnFailure diffex $DIR/*.TRACE
    ;;
  8)
	# up trace level in ex11 to level 5 and change name to ex44
	pea -dex44 -l5 --config ex11_pea_pp_user_gps.yaml -v | tee pea44.out
    DIR="ex44" 
	results2s3 $DIR
    ../scripts/download_example_input_data.py $DIR --tag $TAG --push
    ../scripts/download_example_input_data.py $DIR --tag $OTHER
    #diffex $DIR/{*snx,*ALIC*.TRACE} # no need to check results - this is done in ex11
    count=`grep "STARTING SPP LSQ" $DIR/*ALIC*.TRACE | wc -l`
    if [ $count -eq 0 ]
	then
        echo "no indication of LSQ in use"
		exit 1
	else
		echo "LSQ is demonstrated"
	fi
    ;;
esac
