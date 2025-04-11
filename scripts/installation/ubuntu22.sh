#!/bin/bash

set -e  # Exit immediately if any command fails

# Check if sudo is available
sudo_cmd="sudo"
if ! command -v sudo >/dev/null 2>&1; then
    sudo_cmd=""
fi

# Check if the system is Ubuntu
if [[ ! -f /etc/os-release ]] || ! grep -iq "ubuntu" /etc/os-release; then
    echo "This script is designed for Ubuntu. Please run it on an Ubuntu system."
    exit 1
fi

# Check if the system is Ubuntu 22.04
ubuntu_version=$(grep -oP '(?<=^VERSION_ID=")\d{2}\.\d{2}(?="$)' /etc/os-release)
if [[ "$ubuntu_version" != "22.04" ]]; then
    echo "This script is designed for Ubuntu 22.04. Do you want to continue? (y/n)"
    read -r response
    if [[ ! $response =~ ^[Yy]$ ]]; then
        echo "Script execution aborted."
        exit 0
    fi
fi

# MongoDB library version numbers
mongo_cxx_driver_version="r3.6.7"

echo "Updating package repositories..."
$sudo_cmd apt update -y

echo "Installing dependencies..."
$sudo_cmd apt-get install --no-install-recommends --yes \
    libgomp1 \
    gcc \
    g++ \
    gdb \
    gfortran \
    openssl \
    curl \
    net-tools \
    wget \
    openssh-server \
    apt-transport-https \
    ca-certificates \
    libopenblas0 \
    libnetcdf19 \
    libnetcdf-c++4-1 \
    gzip \
    gnupg2 \
    git \
    cmake \
    make \
    libyaml-cpp0.7 \
    libssl-dev \
    libboost-all-dev \
    libeigen3-dev \
    libyaml-cpp-dev \
    libnetcdf-dev \
    libnetcdf-c++4-dev \
    libzstd-dev \
    libssl-dev \
    libopenblas-dev \
    libmongoc-1.0-0 \
    libmongoc-dev \
    libncurses5-dev \
    python3-pip \


echo "Creating build directory..."
mkdir -p /tmp/build
cd /tmp/build

echo "Downloading and extracting mongo-cxx-driver version $mongo_cxx_driver_version..."
wget --no-check-certificate https://github.com/mongodb/mongo-cxx-driver/releases/download/$mongo_cxx_driver_version/mongo-cxx-driver-$mongo_cxx_driver_version.tar.gz
tar -xzf mongo-cxx-driver-$mongo_cxx_driver_version.tar.gz
rm mongo-cxx-driver-$mongo_cxx_driver_version.tar.gz

echo "Building and installing mongo-cxx-driver..."
mkdir -p mongo-cxx-driver-$mongo_cxx_driver_version/build
cd mongo-cxx-driver-$mongo_cxx_driver_version/build
cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local
$sudo_cmd make -j 1
$sudo_cmd make install

echo "Installation of Mongodb"
curl -fsSL https://pgp.mongodb.com/server-6.0.asc |    $sudo_cmd gpg -o /usr/share/keyrings/mongodb-server-6.0.gpg    --dearmor
echo "deb [ arch=amd64,arm64 signed-by=/usr/share/keyrings/mongodb-server-6.0.gpg ] https://repo.mongodb.org/apt/ubuntu jammy/mongodb-org/6.0 multiverse" | $sudo_cmd tee /etc/apt/sources.list.d/mongodb-org-6.0.list
$sudo_cmd apt update
$sudo_cmd apt-get install -y mongodb-org

echo "Installation completed"

