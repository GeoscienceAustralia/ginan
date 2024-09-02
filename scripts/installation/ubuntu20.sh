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
if [[ "$ubuntu_version" != "20.04" && "$ubuntu_version" != "18.04" ]]; then
    echo "This script is designed for Ubuntu 20.04 or 18.04. Do you want to continue? (y/n)"
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
$sudo_cmd apt-get install --no-install-recommends --yes git gobjc gobjc++ gfortran libopenblas-dev openssl libncurses5-dev curl net-tools  openssh-server cmake make libssl-dev wget sudo python3 python3-pip software-properties-common libnetcdf-dev libnetcdf-c++4-dev gpg-agent

$sudo_cmd add-apt-repository ppa:ubuntu-toolchain-r/test -y

$sudo_cmd apt update

$sudo_cmd apt install -y gcc-9 g++-9

$sudo_cmd update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-9 51

$sudo_cmd update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-9 51

echo "Creating build directory..."
mkdir -p /tmp
cd /tmp

echo "Downloading, extracting, building, and installing yaml..."
cd /tmp
git clone https://github.com/jbeder/yaml-cpp.git
cd yaml-cpp
mkdir cmake-build
cd cmake-build
cmake .. -DCMAKE\_INSTALL\_PREFIX=/usr/local/ -DYAML\_CPP\_BUILD\_TESTS=OFF
$sudo_cmd make install yaml-cpp -j2
cd /tmp

echo "Downloading, extracting, building, and installing Boost..."
cd /tmp
wget -c https://boostorg.jfrog.io/artifactory/main/release/1.73.0/source/boost_1_73_0.tar.gz
tar -xf boost_1_73_0.tar.gz
cd boost_1_73_0/
./bootstrap.sh
$sudo_cmd ./b2 -j2 install
cd /tmp

echo "Downloading, extracting, building, and installing Eigen..."
cd /tmp
git clone https://gitlab.com/libeigen/eigen.git
cd eigen
git checkout 3.4.0
mkdir cmake-build
cd cmake-build
cmake ..
$sudo_cmd make -j2 install
cd /tmp


echo "Downloading, extracting, building, and installing Mongo drivers..."
cd /tmp
wget https://github.com/mongodb/mongo-c-driver/releases/download/1.17.1/mongo-c-driver-1.17.1.tar.gz
tar -xf mongo-c-driver-1.17.1.tar.gz
cd mongo-c-driver-1.17.1/
mkdir cmake-build
cd cmake-build/
cmake -DENABLE_AUTOMATIC_INIT_AND_CLEANUP=OFF -DENABLE_EXAMPLES=OFF ../
cmake --build . -- -j 2
$sudo_cmd cmake --build . --target install -- -j 2
cd /tmp
curl -OL https://github.com/mongodb/mongo-cxx-driver/releases/download/r3.6.0/mongo-cxx-driver-r3.6.0.tar.gz
tar -xf mongo-cxx-driver-r3.6.0.tar.gz
cd mongo-cxx-driver-r3.6.0/build
cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local -DENABLE_EXAMPLES=OFF ../
sudo cmake --build . --target EP_mnmlstc_core -- -j 2
$sudo_cmd cmake --build . -- -j 2
$sudo_cmd cmake --build . --target install
cd /tmp

echo "Downloading, extracting, building, and installing MongoDB ..."
cd /tmp
wget -qO - https://www.mongodb.org/static/pgp/server-4.4.asc | $sudo_cmd apt-key add -
echo "deb [ arch=amd64,arm64 ] https://repo.mongodb.org/apt/ubuntu focal/mongodb-org/4.4 multiverse" | $sudo_cmd tee /etc/apt/sources.list.d/mongodb-org-4.4.list
echo "deb [ arch=amd64,arm64 ] https://repo.mongodb.org/apt/ubuntu bionic/mongodb-org/4.4 multiverse" | $sudo_cmd tee -a /etc/apt/sources.list.d/mongodb-org-4.4.list
$sudo_cmd apt update
$sudo_cmd apt install -y mongodb-org

echo "Installation completed"

