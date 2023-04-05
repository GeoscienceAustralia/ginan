#!/bin/bash

function results2s3()
{
  # needs dir and tag
  tar cfz run-results-$1.tar.gz $1;
  aws s3 cp run-results-$1.tar.gz s3://ginan-pipeline-results/$TAG/
}

shopt -s extglob

function diffex()
{
  for file in $@; do
    ../scripts/diffutil.py   -i $file -o solutions/$file -a $ATOL
    ../scripts/diffutil.py   -i $file -o solutions/$file
  done
}