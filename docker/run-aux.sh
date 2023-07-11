#!/bin/bash

function results2s3()
{
  # needs dir and tag
  tar cfz run-results-$1.tar.gz $1;
  aws s3 cp run-results-$1.tar.gz s3://ginan-pipeline-results/$TAG/ --quiet
}

shopt -s extglob
shopt -s nullglob

function diffex()
{
  for file in $@; do
    echo $file
    if [ ${file: -3} = 'snx' ]
    then
      diffutil -i $file ../inputData/solutions/$file --passthrough -a $ATOL
      diffutil -i $file ../inputData/solutions/$file --passthrough
    elif [ ${file: -8} = 'smoothed' ]
    then
      diffutil -i $file ../inputData/solutions/$file -a $ATOL
    else
      diffutil -i $file ../inputData/solutions/$file -a $ATOL
      diffutil -i $file ../inputData/solutions/$file
    fi
  done
}


runAllAndDiffOnFailure() {

PROG=$1; shift  #now remove this parameter from the list so we can iterate over the rest

set +e
declare -i FAIL=0

for file; 
    do 
    echo %%%%%%%%%% Running $PROG on $file
    $PROG $file;
    if [ "$?" -eq "0" ]; 
    then 
        echo %%%%%%%%%% OKIE DOKIE; 
    else
        echo %%%%%%%%%% Failure running $PROG on $file:
        echo %%%%%%%%%% Diffing $file and solutions/$file:
        echo %%%%%%%%%%
        diff -W 300 -y -w --suppress-common-lines ../inputData/solutions/$file $file | head -n 600;
        echo %%%%%%%%%%
        echo %%%%%%%%%% .......
        echo %%%%%%%%%% 
        FAIL+=1;
    fi
done

set -e
if [ "$FAIL" -eq "0" ]; 
then 
	echo %%%%%%%%%% All tests passed; 
else
	echo %%%%%%%%%% $FAIL tests failed:
	false;
fi
}
