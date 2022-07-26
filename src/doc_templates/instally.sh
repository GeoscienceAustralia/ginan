#!/bin/sh -xe

# execute doxygen
# $1 is the path to the Doxyfile
# $2 is the directory where doxygen should be executed
# $3 is a boolean: true -> enable latex generation, false -> skip latex generation
# $4 is a string with extra alpine packages to be installed (i.e. font-fira-code)

if [ ! -d $2 ]; then
  echo "Path $2 could not be found!"
  exit 1
fi
cd $2

if [ ! -f $1 ]; then
  echo "File $1 could not be found!"
  exit 1
fi

PACKAGES="graphviz git python python3 flex gcc g++ cmake bison $4"
apt update
apt install -y $PACKAGES

ls
git clone https://github.com/doxygen/doxygen.git
cd doxygen
git checkout 77074075dc61f8c1be7fdba940bddbb7315980da
ls
pwd
ls ../
mv /0001-Patch-to-make-call-graphs-output-in-order-of-calling.patch ./
git apply 0001-Patch-to-make-call-graphs-output-in-order-of-calling.patch
mkdir build
cd build
cmake -G "Unix Makefiles" ..
make -j4
make install
