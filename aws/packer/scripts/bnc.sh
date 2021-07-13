#!/bin/bash -x
#==============================================================================
# Install BNC used to create the RINEX from real-time streams
#==============================================================================
cd /data
wget https://s3-ap-southeast-2.amazonaws.com/peanpod/install_deps/bnc_install.tar.gz ./
gunzip bnc_install.tar.gz
tar xvf bnc_install.tar
mv bnc_install/bnc /data/
chown -R geodesy.geodesy bnc
sudo -u geodesy /data/hourly
#==============================================================================
# Copy the executable to /usr/local/bin/
#==============================================================================
cp bnc_install/bnc-2.12.8-el6-64bit-static /usr/local/bin/bnc
chmod a+x /usr/local/bin/bnc
rm bnc_install.tar
rm -fr bnc_install
#==============================================================================
# DEPENDENCIES: Install the libmng library needed by bnc
#==============================================================================
yum -y install libmng.x86_64 libtiff.x86_64 libpng.x86_64 libSM.x86_64 libXrender.x86_64
yum -y install libXext.x86_64 libpng12.x86_64
ln -s /usr/lib64/libtiff.so.5 /usr/lib64/libtiff.so
# Force so.5 to look like so.3
ln -s /usr/lib64/libtiff.so.5 /usr/lib64/libtiff.so.3

