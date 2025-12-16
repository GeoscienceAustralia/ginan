# Installation procedure on Apple

Tested on Macbook Pro (Intel) with Somona OSX and Macbook Pro (ARM64) with Sonoma OSX

## Install Ginan dependencies

After installation of homebrew, install the following packages using brew

```bash
brew install boost cmake eigen netcdf-cxx netcdf mongo-c-driver mongo-cxx-driver openblas openssl@3 yaml-cpp libomp
```
***

Follow the instructions here to install the MongoDB application:
https://www.mongodb.com/docs/manual/tutorial/install-mongodb-on-os-x/`

## Install gnssanalysis python module

```
pip3 install gnssanalysis
```

## Download Ginan from Github

You can download Ginan source from github using git clone:

```
#git clone https://github.com/GeoscienceAustralia/ginan.git
git clone -b develop-weekly --depth 1 --single-branch https://github.com/GeoscienceAustralia/ginan.git

cd ginan
cd src
mkdir build
cd build
cmake -DCMAKE_TOOLCHAIN_FILE=compile_mac_arm64.cmake ..
make -j4 pea

cd ../..
./bin/pea --help
```

## Download Demo data and products

Then download all of the example data using the python script provided (requires `gnssanalysis`):

``` 
cd inputData
cd products
getProducts.sh
cd ../data
getData.sh
```