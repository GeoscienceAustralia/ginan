#!/bin/bash

set -e  # Exit immediately if any command fails

# Check if sudo is available
sudo_cmd="sudo"
if ! command -v sudo >/dev/null 2>&1; then
    sudo_cmd=""
fi

# Check if the system is Fedora
if [[ ! -f /etc/os-release ]] || ! grep -iq "fedora" /etc/os-release; then
    echo "This script is designed for Fedora. Please run it on a Fedora system."
    exit 1
fi

# Check if the system is Fedora 38
fedora_version=$(grep -oP '(?<=^VERSION_ID=)\d+' /etc/os-release)
if [[ "$fedora_version" != "38" ]]; then
    echo "This script is designed for Fedora 38. Do you want to continue? (y/n)"
    read -r response
    if [[ ! $response =~ ^[Yy]$ ]]; then
        echo "Script execution aborted."
        exit 0
    fi
fi

# MongoDB library version numbers
mongo_cxx_driver_version="r3.8.0"

echo "Updating package repositories..."
$sudo_cmd dnf update -y

echo "Installing dependencies..."
$sudo_cmd dnf install --setopt=install_weak_deps=False -y \
    gcc \
    gcc-c++ \
    gdb \
    gfortran \
    openssl \
    curl \
    net-tools \
    wget \
    openssh-server \
    ca-certificates \
    openblas \
    netcdf \
    netcdf-cxx \
    gzip \
    gnupg2 \
    git \
    cmake \
    make \
    yaml-cpp \
    openssl-devel \
    boost-devel \
    boost-static \
    eigen3-devel \
    yaml-cpp-devel \
    netcdf-devel \
    mongo-c-driver-devel \
    blas64.x86_64 \
    openblas-devel.x86_64 \
    ncurses-devel \
    netcdf-cxx4-devel.x86_64

echo "Creating build directory..."
mkdir -p /tmp/build
cd /tmp/build

echo "Downloading and extracting mongo-cxx-driver version $mongo_cxx_driver_version..."
wget --no-check-certificate https://github.com/mongodb/mongo-cxx-driver/releases/download/r3.8.0/mongo-cxx-driver-$mongo_cxx_driver_version.tar.gz
tar -xzf mongo-cxx-driver-$mongo_cxx_driver_version.tar.gz

echo "Building and installing mongo-cxx-driver..."
cd mongo-cxx-driver-$mongo_cxx_driver_version/build
cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local
$sudo_cmd make -j 1
$sudo_cmd make install
cd /tmp
rm -rf *
echo "Installation of Mongodb"


echo "[mongodb-org-6.0]" | $sudo_cmd tee  /etc/yum.repos.d/mongodb-org-6.0.repo
echo "name=MongoDB Repository" | $sudo_cmd tee  -a /etc/yum.repos.d/mongodb-org-6.0.repo
echo "baseurl=https://repo.mongodb.org/yum/redhat/9/mongodb-org/6.0/x86_64/" | $sudo_cmd tee  -a /etc/yum.repos.d/mongodb-org-6.0.repo
echo "gpgcheck=1" | $sudo_cmd tee  -a /etc/yum.repos.d/mongodb-org-6.0.repo
echo "enabled=1" | $sudo_cmd tee  -a /etc/yum.repos.d/mongodb-org-6.0.repo
echo "gpgkey=https://www.mongodb.org/static/pgp/server-6.0.asc" | $sudo_cmd tee -a  /etc/yum.repos.d/mongodb-org-6.0.repo
$sudo_cmd dnf update -y
$sudo_cmd dnf install -y mongodb-org

echo "Cleaning up mongo-cxx-driver files..."
cd /tmp
rm -rf /tmp/build

echo "Installation completed successfully."
