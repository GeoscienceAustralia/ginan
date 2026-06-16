# Installation procedure on Apple

Tested on MacBook Pro (Intel) with Sonoma macOS and MacBook Pro (ARM64) with Sonoma macOS.

## Install Ginan dependencies

After installation of homebrew, install the following packages using brew

```bash
brew install boost cmake eigen netcdf-cxx netcdf mongo-c-driver mongo-cxx-driver openblas openssl@3 yaml-cpp libomp
```
***

Follow the instructions here to install the MongoDB application:
https://www.mongodb.com/docs/manual/tutorial/install-mongodb-on-os-x/

## Install gnssanalysis python module

```
pip3 install gnssanalysis
```

## Download Ginan from Github

You can download Ginan source from github using git clone:

```
git clone https://github.com/GeoscienceAustralia/ginan.git

cd ginan
export VCPKG_ROOT="$PWD/vcpkg"
export VCPKG_COMMIT="4c5ae6b55f3e3e39d291679f89822f496cf190ee"

git clone https://github.com/Microsoft/vcpkg.git "$VCPKG_ROOT"
git -C "$VCPKG_ROOT" fetch --depth 1 origin "$VCPKG_COMMIT"
git -C "$VCPKG_ROOT" checkout --detach "$VCPKG_COMMIT"
"$VCPKG_ROOT/bootstrap-vcpkg.sh" -disableMetrics

cd src

# Apple silicon:
cmake --preset macos-arm64-release
cmake --build --preset macos-arm64-release --target pea --parallel 4

# Intel Mac:
# cmake --preset macos-x64-release
# cmake --build --preset macos-x64-release --target pea --parallel 4

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
