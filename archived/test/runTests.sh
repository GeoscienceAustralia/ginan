#!/usr/bin/env sh

#
# download test data for antenna routines
#   igs14_2000.atx
#
mkdir -p data/
mkdir -p data/antenna/
cd data/antenna/
wget -c https://s3-ap-southeast-2.amazonaws.com/peanpod/test/data/antenna/igs14_2000.atx 
cd ../..
make
./test_antenna
#
