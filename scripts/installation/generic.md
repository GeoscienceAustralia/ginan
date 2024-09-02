

## Dependencies

If instead you wish to build Ginan from source, there are several software dependencies:

* C/C++ and Fortran compiler. We use and recommend [gcc, g++, and gfortran](https://gcc.gnu.org)
* BLAS and LAPACK linear algebra libraries. We use and recommend [OpenBlas](https://www.openblas.net/) as this contains both libraries required
* CMAKE     > 3.0
* YAML      > 0.6
* Boost     >= 1.73 (tested on 1.73). On Ubuntu 22.04 which uses gcc-11, you need Boost >= 1.74.0
* MongoDB
* Mongo_C >= 1.71.1
* Mongo_cxx >= 3.6.0
* Eigen3    > 3.4
* netCDF4
* Python >= 3.7

If using gcc verion 11 or about, the minimum version of libraries are:
* Boost >= 1.74.0
* Mongo_cxx = 3.7.0

***
## Installing dependencies (Example with Ubuntu)

Update the base operating system and install base utilities `gcc`, `gfortran`, `git`, `openssl`, `openblas` etc:

```
dir=$PWD

sudo apt update

sudo apt upgrade -y

sudo apt install -y git gobjc gobjc++ gfortran libopenblas-dev openssl curl net-tools libncurses5-dev openssh-server cmake make libssl1.0-dev wget sudo python3 software-properties-common

sudo -H pip3 install wheel pandas boto3 unlzw tdqm scipy gnssanalysis
```

Ginan requires at least version 9 of both gcc and g++, so make sure to update the gcc/g++ alternatives prior to compilation:
(this is not required on Ubuntu 22.04)

```
sudo add-apt-repository ppa:ubuntu-toolchain-r/test -y

sudo apt update

sudo apt install -y gcc-9 g++-9

sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-9 51

sudo update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-9 51
```

***
## Building additional dependencies

Depending on the user's installation choice: install PEA-only, POD-only or all software packages, a set of additional dependencies that need to be built may change. Below, we explain building all the additional dependencies:

Note that many `make` commands here have the option `-j 2` applied, this will enable parallel compilation and may speed up installation time. The number of threads can be increased by changing the number, such as `-j 8`, but be aware that each new thread may require up to 2GB of memory.

First, create a temporary directory structure to make the dependencies in, it can be removed after the installation process is done:

```
mkdir $dir/tmp
```

### YAML-CPP
We are using the [yaml-cpp](https://github.com/jbeder/yaml-cpp) library to parse the configuration files used to run many of the programs found in this library. Here is an example of how to install the yaml library from source:

```
cd $dir/tmp

git clone https://github.com/jbeder/yaml-cpp.git

cd yaml-cpp

mkdir cmake-build

cd cmake-build

cmake .. -DCMAKE\_INSTALL\_PREFIX=/usr/local/ -DYAML\_CPP\_BUILD\_TESTS=OFF

sudo make install yaml-cpp -j2

cd $dir/tmp

rm -rf yaml-cpp
```

### Boost (PEA)
PEA relies on a number of the utilities provided by [boost](https://www.boost.org/), such as their time and logging libraries.
NB for compilation using gcc-11, you need to change this to boost_1_74_0

```
cd $dir/tmp

wget -c https://boostorg.jfrog.io/artifactory/main/release/1.73.0/source/boost_1_73_0.tar.gz

tar -xf boost_1_73_0.tar.gz

cd boost_1_73_0/

./bootstrap.sh

sudo ./b2 -j2 install

cd $dir/tmp

sudo rm -rf boost_1_73_0 boost_1_73_0.tar.gz
```

### Eigen3 (PEA)
Eigen3 is used for performing matrix calculations in PEA, and has a very nice API.

```
cd $dir/tmp

git clone https://gitlab.com/libeigen/eigen.git

cd eigen

git checkout 3.4.0

mkdir cmake-build

cd cmake-build

cmake ..

sudo make -j2 install

cd $dir/tmp

rm -rf eigen
```


### Mongo_cxx_driver (PEA)
Needed for json formatting and other self-descriptive markup.

```
cd $dir/tmp
# NB for compilation using gcc-11, you need to change this to 1.21.2

wget https://github.com/mongodb/mongo-c-driver/releases/download/1.17.1/mongo-c-driver-1.17.1.tar.gz

tar -xf mongo-c-driver-1.17.1.tar.gz

cd mongo-c-driver-1.17.1/

mkdir cmake-build

cd cmake-build/

cmake -DENABLE_AUTOMATIC_INIT_AND_CLEANUP=OFF -DENABLE_EXAMPLES=OFF ../

cmake --build . -- -j 2

sudo cmake --build . --target install -- -j 2

cd $dir/tmp

NB for compilation using gcc-11, you need to change this to 3.7.0

curl -OL https://github.com/mongodb/mongo-cxx-driver/releases/download/r3.6.0/mongo-cxx-driver-r3.6.0.tar.gz

tar -xf mongo-cxx-driver-r3.6.0.tar.gz

cd mongo-cxx-driver-r3.6.0/build

cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local -DENABLE_EXAMPLES=OFF ../

sudo cmake --build . --target EP_mnmlstc_core -- -j 2

cmake --build . -- -j 2

sudo cmake --build . --target install

cd $dir/tmp

sudo rm -rf mongo-c-driver-1.17.1  mongo-c-driver-1.17.1.tar.gz  mongo-cxx-driver-r3.6.0  mongo-cxx-driver-r3.6.0.tar.gz
```

### MongoDB (PEA, optional)
Using the mongo database is optional, but is needed for use of the realtime plotting and statistics available through the `GinanEDA`

Prepare access to repositories and download and install mongo, follow the steps on [Mongo website](https://www.mongodb.com/docs/manual/installation/)


### netcdf4 (OTL package)

```
sudo apt -y install libnetcdf-dev libnetcdf-c++4-dev
```

***

## Install gnssanalysis python module

```
sudo pip install gnssanalysis
```

## Download

You can download Ginan source from github using git clone:


```
if [ ! -d "../ginan" -o ! -f CHANGELOG.md ]
then
cd $dir


git clone https://github.com/GeoscienceAustralia/ginan.git

cd ginan
else
echo "already in a checkout directory, no need to download again"
fi
```

Then download all of the example data using the python script provided (requires `gnssanalysis`):

```
python3 scripts/download_example_input_data.py
```
***
