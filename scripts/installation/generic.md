

## Dependencies

If instead you wish to build Ginan from source, there are several software dependencies:

* C/C++ and Fortran compiler. We use and recommend [gcc, g++, and gfortran](https://gcc.gnu.org)
* BLAS and LAPACK linear algebra libraries. We use and recommend [OpenBlas](https://www.openblas.net/) as this contains both libraries required
* CMAKE     > 3.0
* YAML      > 0.6
* Boost     >= 1.74
* MongoDB
* Mongo_C >= 1.71.1 (automatically installed with mongo-cxx-driver)
* Mongo_cxx >= 3.9.0
* Eigen3    > 3.4
* netCDF4
* Python >= 3.7


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


***
## Building additional dependencies

Depending on the user's installation choice: install PEA-only, or all software packages, a set of additional dependencies that need to be built may change. Below, we explain building all the additional dependencies:

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


```
cd $dir/tmp

wget -c https://archives.boost.io/release/1.83.0/source/boost_1_83_0.tar.gz

tar -xf boost_1_83_0.tar.gz

cd boost_1_83_0/

./bootstrap.sh

sudo ./b2 -j2 install

cd $dir/tmp

sudo rm -rf boost_1_83_0 boost_1_83_0.tar.gz
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
Needed for connection to the MongoDB database, which is used for realtime plotting and statistics in `GinanEDA`.

Note: for later version of mongo-cxx-driver install (3.9.0 and above) it also the mongo-c driver, so it is not needed to install it separately.

```
cd $dir/tmp

curl -OL https://github.com/mongodb/mongo-cxx-driver/releases/download/r3.11.0/mongo-cxx-driver-r3.11.0.tar.gz

tar -xf mongo-cxx-driver-r3.11.0.tar.gz

cd mongo-cxx-driver-r3.11.0/build

cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local

make -j 1

sudo make install

cd $dir/tmp

sudo rm -rf   mongo-cxx-driver-r3.11.0  mongo-cxx-driver-r3.11.0.tar.gz
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
