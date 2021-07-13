#!/bin/bash -x
#==============================================================================
# set hostname
#==============================================================================
hostname acs-pod
#==============================================================================
# Update Yum
#==============================================================================
yum update -y
yum -y install zip unzip bzip2.x86_64 wget ftp.x86_64 git
#==============================================================================
# Install the amazon CLI
#==============================================================================
cd /home/ec2-user
curl "https://s3.amazonaws.com/aws-cli/awscli-bundle.zip" -o "awscli-bundle.zip"
unzip awscli-bundle.zip
./awscli-bundle/install -i /usr/local/aws -b /usr/local/bin/aws
rm -fr awscli-bundle awscli-bundle.zip
#
#==============================================================================
# Install GCC libraries
#==============================================================================
#
yum -y install gcc-objc.x86_64 gcc-objc++.x86_64 compat-gcc-44.x86_64 compat-gcc-44-c++.x86_64
yum -y install glibc.x86_64 glibc-devel.x86_64 glibc-headers.x86_64 glibc-utils.x86_64 
yum -y install glibc-common.x86_64 compat-glibc.x86_64 compat-glibc-headers.x86_64 glibc-devel.i686
yum -y install gcc-gfortran.x86_64
#
#==============================================================================
# install the dependincies for the pea boost, lapack, blas
#==============================================================================
yum -y install boost-devel.x86_64 lapack.x86_64 blas.x86_64
#==============================================================================
# lapack installs as so.3 need to manually link at as a .so
#==============================================================================
ln -s /usr/lib64/liblapack.so.3 /usr/lib64/liblapack.so
ln -s /usr/lib64/libblas.so.3 /usr/lib64/libblas.so
#==============================================================================
# Install hard drive onto server image
#==============================================================================
mkfs -t xfs /dev/xvdb
sleep 30
# Mount the drives
mount /dev/xvdb /data
chmod a+rw /data
sudo -u root echo "/dev/xvdb /data xfs defaults 0 0" >> /etc/fstab
