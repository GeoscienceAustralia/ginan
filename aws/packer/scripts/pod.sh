#!/bin/bash -x
#==============================================================================
# Copy the source code into the /data directory and give it geodesy permissions
#==============================================================================
sudo -u geodesy mkdir -p /data/src
sudo -u geodesy mkdir -p /data/src/acs
sudo -u geodesy mkdir -p /data/src/acs/pod
cp -r /tmp/* /data/src/acs/pod/
sudo chown -R geodesy.geodesy /data/src/acs/*
#
#==============================================================================
# Compile the pod
#==============================================================================
cd /data/src/acs/pod/src
ls -l
sudo -u geodesy make
#cd /data/src/acs/pde/src/test
#make
#sudo -u geodesy ./runTests.sh
