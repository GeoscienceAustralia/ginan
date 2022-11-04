#!/bin/sh
set -e

echo "This script downloads and install the dependencies for GINA on OSX."
echo "You must first install xcode on your mac."
echo "It installs the the dependencies in /usr/local after making local_backup.tar.gz in the Documents directory."
echo "Find installation does not work try sudo rm -r /usr/local/* to clear all previously installed packages."
echo "It makes the entire GINAN (including Fortan) as well as a xcdoe project in src/xcode"
echo "It makes the directory ginan_dep in documents and then deletes it when finished."
echo "It does not install a mongoDB server (drivers only)."
echo "It expects to be run from the ginan/scripts directory."
echo "To run make requires a define on the command line, cmake -DBoost_INCLUDE_DIR=/usr/local/include .."

xcode-select --install || true
echo "Please wait for xcode command line tools to install then press a key."
read -n 1


ginan_dir=$PWD/..
doc_dir=~/
build=~/ginan_dep
sudo rm -r $build || true
mkdir -p $build

backup_file=$doc_dir/local_backup.tar.gz
if test -f "$backup_file"; then
    echo "Backup of /usr/local already exists."
else
    sudo tar -cvf $backup_file /usr/local
fi

CC=gcc
FC=gfortran
CPPFLAGS=-I/usr/local/include
LDFLAGS=-L/usr/local/lib

# gcc and gfortan
cd $build
if [[ `uname -m` == 'arm64' ]]; then
    curl -OL https://sourceforge.net/projects/hpc/files/hpc/gcc/gcc-m1-bin.tar.gz
else
    curl -OL https://sourceforge.net/projects/hpc/files/hpc/gcc/gcc-11.2-bin.tar.gz
fi

sudo tar -xvf gcc-11.2-bin.tar.gz -C /. || true

cd $build
curl -OL http://gnu.mirrors.hoobly.com/m4/m4-1.4.tar.gz
tar -xvf m4-1.4.tar.gz
cd m4-1.4
./configure --prefix=/usr/local
make -j8
sudo make install

cd $build
curl -OL https://github.com/Kitware/CMake/releases/download/v3.22.0-rc3/cmake-3.22.0-rc3-macos-universal.tar.gz
tar -xvf cmake-3.22.0-rc3-macos-universal.tar.gz
sudo mv ./cmake-3.22.0-rc3-macos-universal/CMake.app /Applications || true
sudo "/Applications/CMake.app/Contents/bin/cmake-gui" --install

cd $build
curl -OL http://ftpmirror.gnu.org/autoconf/autoconf-2.68.tar.gz
tar xzf autoconf-2.68.tar.gz
cd autoconf-2.68
./configure --prefix=/usr/local 
make -j8
sudo make install
export PATH=$PATH:$build/autotools-bin/bin

cd $build
curl -OL http://ftpmirror.gnu.org/automake/automake-1.11.tar.gz
tar xzf automake-1.11.tar.gz
cd automake-1.11
./configure --prefix=/usr/local 
make -j8
sudo make install

cd $build
curl -OL http://ftpmirror.gnu.org/libtool/libtool-2.4.tar.gz
tar xzf libtool-2.4.tar.gz
cd libtool-2.4
./configure --prefix=/usr/local 
make -j8
sudo make install

cd $build
curl -OL https://sourceforge.net/projects/libpng/files/zlib/1.2.11/zlib-1.2.11.tar.gz
tar -xvf zlib-1.2.11.tar.gz
cd zlib-1.2.11
./configure --prefix=/usr/local --static --archs="-arch x86_64"
./configure --prefix=/usr/local
make -j4
sudo make install

cd $build
curl -OL https://ftp.gnu.org/pub/gnu/gettext/gettext-0.21.tar.gz
tar -xvf gettext-0.21.tar.gz
cd gettext-0.21
./configure --prefix=/usr/local
make -j8
sudo make install


cd $build
curl -O https://mac.r-project.org/openmp/openmp-12.0.1-darwin20-Release.tar.gz
sudo tar fvxz openmp-12.0.1-darwin20-Release.tar.gz -C / || true

cd $build
curl -OL https://github.com/xianyi/OpenBLAS/releases/download/v0.3.17/OpenBLAS-0.3.17.tar.gz
tar -xvf OpenBLAS-0.3.17.tar.gz
cd OpenBLAS-0.3.17
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/usr/local ..
make -j8
sudo make install

cd $build
curl -OL https://www.openssl.org/source/old/3.0/openssl-3.0.0-beta2.tar.gz
tar -xvf openssl-3.0.0-beta2.tar.gz
cd openssl-3.0.0-beta2
./Configure --prefix=/usr/local
make -j8
sudo make install

cd $build
sudo git clone https://github.com/jbeder/yaml-cpp.git
cd yaml-cpp
sudo mkdir cmake-build
cd cmake-build
sudo cmake .. -DCMAKE_INSTALL_PREFIX=/usr/local -DYAML_CPP_BUILD_TESTS=OFF
sudo make install yaml-cpp

cd $build
git clone https://github.com/erdc/szip.git
cd szip
./configure --prefix=/usr/local
make -j8
sudo make install

cd $build
git clone https://github.com/HDFGroup/hdf5.git
cd hdf5
sudo mkdir build
cd build
sudo CC=gcc CPPFLAGS=-I/usr/local/include LDFLAGS=-L/usr/local/lib cmake -DCMAKE_INSTALL_PREFIX=/usr/local -DHDF5_ENABLE_Z_LIB_SUPPORT=ON -DBUILD_SHARED_LIBS=ON -DHDF5_BUILD_FORTRAN=ON -DBUILD_STATIC_LIBS=ON ..
sudo CC=gcc CPPFLAGS=-I/usr/local/include LDFLAGS=-L/usr/local/lib make -j8 install

#cd $build
#git clone https://github.com/Unidata/netcdf-fortran.git
#cd netcdf-fortran
#mkdir build
#cd build
#sudo cmake .. -DCMAKE_INSTALL_PREFIX=/usr/local
#sudo make -j8 install

cd $build
git clone https://github.com/Unidata/netcdf-c.git
cd netcdf-c
mkdir build
cd build
sudo CPPFLAGS=-I/usr/local/include LDFLAGS=-L/usr/local/lib cmake .. -DCMAKE_INSTALL_PREFIX=/usr/local
sudo CPPFLAGS=-I/usr/local/include LDFLAGS=-L/usr/local/lib make -j8 install

cd $build
git clone https://github.com/Unidata/netcdf-cxx4.git
cd netcdf-cxx4
git fetch
git checkout 4.3.0
sudo CPPFLAGS=-I/usr/local/include ./configure --prefix=/usr/local
sudo make -j8 install

cd $build
curl -OL https://github.com/mongodb/mongo-c-driver/releases/download/1.17.1/mongo-c-driver-1.17.1.tar.gz
tar -xvf mongo-c-driver-1.17.1.tar.gz
cd mongo-c-driver-1.17.1/
mkdir cmakebuild
cd cmakebuild/
sudo cmake -DENABLE_AUTOMATIC_INIT_AND_CLEANUP=OFF -DCMAKE_INSTALL_PREFIX=/usr/local ..
sudo make -j8 install


cd $build
curl -OL https://github.com/mongodb/mongo-cxx-driver/releases/download/r3.6.0/mongo-cxx-driver-r3.6.0.tar.gz
tar -xzf mongo-cxx-driver-r3.6.0.tar.gz
cd mongo-cxx-driver-r3.6.0/
cd build/
sudo cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local ..
sudo cmake --build . --target EP_mnmlstc_core -j8
sudo cmake --build . -j8
sudo cmake --build . --target install -j8


cd $build
sudo git clone https://gitlab.com/libeigen/eigen.git
cd eigen
sudo mkdir cmake-build
cd cmake-build
sudo cmake -DCMAKE_INSTALL_PREFIX=/usr/local ..
make -j8
sudo make install

cd $build
curl -OL https://boostorg.jfrog.io/artifactory/main/release/1.73.0/source/boost_1_73_0.tar.gz
tar -xvf boost_1_73_0.tar.gz
cd boost_1_73_0/
sudo ./bootstrap.sh
sudo ./b2 install

sudo rm -r $build

cd $ginan_dir
cd src
mkdir build
cd build
cmake -DBoost_INCLUDE_DIR=/usr/local/include ..
make -j8 pea
make -j8 pod

cd $ginan_dir
cd src
mkdir xcode
cd xcode
cmake -DBoost_INCLUDE_DIR=/usr/local/include -DFORTRAN_TARGETS=OFF  -G Xcode ..

cd $ginan_dir
cd scripts
python3 download_examples.py

echo "Installation complete."
