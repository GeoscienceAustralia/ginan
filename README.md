# ![gn_logo](https://raw.githubusercontent.com/GeoscienceAustralia/ginan/gh-pages/images/GinanLogo273.png)

# Ginan: Software toolkit and service

: Note - this file is executable. \
: You can run it by saving the raw markdown to disk, then `` chmod +x README.md``, then ``./README.md`` \
: It will execute all of the code blocks that finish with the `` :<<'```executable' `` tag. \
: This script will install all dependencies, clone the Ginan repo into the current directory and build the Ginan toolkit.


#### `Ginan v1.5.1 release`

## Overview

Ginan is a processing package being developed to processes GNSS observations for geodetic applications.  

We currently support the processing of:

* the United States' Global Positioning System (**GPS**);
* the European Union's Galileo system (**Galileo**);
* the Russian GLONASS system (**GLONASS**)\*;
* the Chinese Navigation Satellite System (**BeiDou**)\*;
* the Japanese QZSS develop system (**QZSS**)\*.

We are actively developing Ginan to have the following capabilities and features:

* Precise Orbit & Clock determination of GNSS satellites (GNSS POD);
* Precise Point Positioning (PPP) of GNSS stations in network and individual mode;
* Real-Time corrections for PPP users;
* Analyse full, single and multi-frequency, multi-GNSS data;
* Delivering atmospheric products such as ionosphere and troposphere models;
* Servicing a wide range of users and receiver types;
* Delivering outputs usable and accessible by non-experts;
* Providing both a real-time and off-line processing capability;
* Delivering both position and integrity information;
* Routinely produce IGS final, rapid, ultra-rapid and real-time (RT) products;
* Model Ocean Tide Loading (OTL) displacements.

The software consists of three main components:
* Network Parameter Estimation Algorithm (PEA) 
* Precise Orbit Determination (POD), and
* Various scripts for combination and analysis of solutions

***
## Supported Platforms

Ginan is supported on the following platforms

* Linux: tested on Ubuntu 18.04 and 20.04
* MacOS: tested on 10.15 (x86)
* Windows: via docker or WSL on Windows 10 and above
***


## Using Ginan with Docker

You can quickly download a ready-to-run Ginan environment using docker by running:

    docker run -it -v /data:/data gnssanalysis/ginan:v1.5.1 bash

This command connects the `/data` directory on the host (your pc), with the `/data` directory in the container, to allow file access between the two systems, and opens a command line (`bash`) for executing commands.

You will need to have [docker](https://docs.docker.com/get-docker/) installed to use this method.

To verify you have the Ginan executables available once at the Ginan command line, run:

    pea --help
    
    pod --help

## Dependencies

If instead you wish to build Ginan from source, there are several software dependencies:

* C/C++ and Fortran compiler. We use and recommend [gcc, g++, and gfortran](https://gcc.gnu.org)
* BLAS and LAPACK linear algebra libraries. We use and recommend [OpenBlas](https://www.openblas.net/) as this contains both libraries required
* CMAKE     > 3.0 
* YAML      > 0.6
* Boost     >= 1.79 (tested on 1.73)
* Mongo_cxx >= 3.6.0 (and Mongo_C >= 1.17.1)
* Eigen3    > 3.4 (we have used 3.4.0)
* netCDF4
* Python3 (tested on Python 3.7)

***
## Installing dependencies with Ubuntu

Update the base operating system and install base utilities `gcc`, `gfortran`, `git`, `openssl`, `openblas` etc:

```executable
dir=$PWD

sudo apt update

sudo apt upgrade -y

sudo apt install -y git gobjc gobjc++ gfortran libopenblas-dev openssl curl net-tools openssh-server cmake make libssl1.0-dev wget sudo python3 software-properties-common

<<<<<<< HEAD
apt install python3-pip

pip3 install wheel pandas boto3 unlzw tdqm
=======
pip3 install wheel pandas boto3 unlzw tdqm scipy
>>>>>>> 0b9e6135ec5f44fbcb1f281adb157c7b694c9e02
:<<'```executable'
```    

Ginan requires at least version 9 of both gcc and g++, so make sure to update the gcc/g++ alternatives prior to compilation:

```executable
sudo add-apt-repository ppa:ubuntu-toolchain-r/test -y

sudo apt update

sudo apt install -y gcc-9 g++-9

sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-9 51

sudo update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-9 51
:<<'```executable'
```    
    
***
## Building additional dependencies 

Depending on the user's installation choice: install PEA-only, POD-only or all software packages, a set of additional dependencies that need to be built may change. Below, we explain building all the additional dependencies:

Note that many `make` commands here have the option `-j 2` applied, this will enable parallel compilation and may speed up installation time. The number of threads can be increased by changing the number, such as `-j 8`, but be aware that each new thread may require up to 2GB of memory.

First, create a temporary directory structure to make the dependencies in, it can be removed after the installation process is done:

```executable
mkdir $dir/tmp
:<<'```executable'
```    

### YAML-CPP
We are using the [yaml-cpp](https://github.com/jbeder/yaml-cpp) library to parse the configuration files used to run many of the programs found in this library. Here is an example of how to install the yaml library from source:

```executable
cd $dir/tmp

git clone https://github.com/jbeder/yaml-cpp.git

cd yaml-cpp

mkdir cmake-build

cd cmake-build

cmake .. -DCMAKE\_INSTALL\_PREFIX=/usr/local/ -DYAML\_CPP\_BUILD\_TESTS=OFF

sudo make install yaml-cpp -j2

cd $dir/tmp

rm -rf yaml-cpp
:<<'```executable'
```    

### Boost (PEA)
PEA relies on a number of the utilities provided by [boost](https://www.boost.org/), such as their time and logging libraries.

```executable
cd $dir/tmp

wget -c https://boostorg.jfrog.io/artifactory/main/release/1.79.0/source/boost_1_79_0.tar.gz

tar -xf boost_1_79_0.tar.gz

cd boost_1_79_0/

./bootstrap.sh

sudo ./b2 -j2 install

cd $dir/tmp

sudo rm -rf boost_1_79_0 boost_1_79_0.tar.gz
:<<'```executable'
```    

### Eigen3 (PEA)
Eigen3 is used for performing matrix calculations in PEA, and has a very nice API.

```executable
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
:<<'```executable'
```    


### Mongo_cxx_driver (PEA)
Needed for json formatting and other self-descriptive markup.

```executable
cd $dir/tmp

wget https://github.com/mongodb/mongo-c-driver/releases/download/1.17.1/mongo-c-driver-1.17.1.tar.gz

tar -xf mongo-c-driver-1.17.1.tar.gz

cd mongo-c-driver-1.17.1/

mkdir cmake-build

cd cmake-build/

cmake -DENABLE_AUTOMATIC_INIT_AND_CLEANUP=OFF -DENABLE_EXAMPLES=OFF ../

cmake --build .

<<<<<<< HEAD
cmake --build . --target install
=======
sudo cmake --build . --target install -- -j 2
>>>>>>> 0b9e6135ec5f44fbcb1f281adb157c7b694c9e02

cd $dir/tmp

curl -OL https://github.com/mongodb/mongo-cxx-driver/releases/download/r3.6.0/mongo-cxx-driver-r3.6.0.tar.gz

tar -xf mongo-cxx-driver-r3.6.0.tar.gz

cd mongo-cxx-driver-r3.6.0/build

cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local -DENABLE_EXAMPLES=OFF ../

sudo cmake --build . --target EP_mnmlstc_core

cmake --build .

sudo cmake --build . --target install

cd $dir/tmp

sudo rm -rf mongo-c-driver-1.17.1  mongo-c-driver-1.17.1.tar.gz  mongo-cxx-driver-r3.6.0  mongo-cxx-driver-r3.6.0.tar.gz
:<<'```executable'
```    


### MongoDB (PEA, optional)
Using the mongo database is optional, but is needed for use of the realtime plotting and statistics available through the `GinanEDA`

Prepare access to repositories and download and install mongo:

    cd $dir/tmp

    wget -qO - https://www.mongodb.org/static/pgp/server-4.4.asc | sudo apt-key add -
    
    echo "deb [ arch=amd64,arm64 ] https://repo.mongodb.org/apt/ubuntu focal/mongodb-org/4.4 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-4.4.list
    
    echo "deb [ arch=amd64,arm64 ] https://repo.mongodb.org/apt/ubuntu bionic/mongodb-org/4.4 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-4.4.list

    sudo apt update

    sudo apt install -y mongodb-org


To start MongoDB service:

    sudo systemctl start mongod
    
    sudo systemctl status mongod
    
    mongod

To autostart MongoDB on system startup:

    sudo systemctl enable mongod.service


<!-- If you are using WSL see the notes below and skip this next section: -->
<!-- #### For WSL:
Even though the documentation on `MongoDB` says that WSL is not supported it is still possible to install.
In order to run mongod as a service on WSL you will need to:

Copy the script from : https://raw.githubusercontent.com/mongodb/mongo/master/debian/init.d
to /etc/init.d/mongod

Make the script executable:

     sudo chmod a+x /etc/init.d/mongod

Now you can start it as a service by:

    sudo service mongod start 
-->

### netcdf4 (OTL package)

```executable
sudo apt -y install libnetcdf-dev libnetcdf-c++4-dev
:<<'```executable'
```    

***

## Download

You can download Ginan source from github using git clone:

```executable
<<<<<<< HEAD
git clone https://github.com/vduong1005/ginan.git
=======
cd $dir

git clone https://github.com/GeoscienceAustralia/ginan.git
>>>>>>> 0b9e6135ec5f44fbcb1f281adb157c7b694c9e02

cd ginan
:<<'```executable'
```    

Then download all of the example data using the python script provided:

```executable
python3 scripts/download_examples.py
:<<'```executable'
```    
***
### Directory Structure

Upon installation, thie ginan directory should have the following structure:

    ginan/
    ├── README.md           ! General README information
    ├── LICENSE.md          ! Software License information
    ├── ChangeLOG.md        ! Release Chnage history
    ├── aws/                ! Amazon Web Services config
    ├── bin/                ! Binary executables directory*
    ├── Docs/               ! Documentation directory
    ├── examples/           ! Ginan examples directory
    │   ├── data/           ! example dataset (rinex files)**
    │   ├── products/       ! example products and aux files**
    │   ├── solutions/      ! example solutions for QC**
    │   --------------PEA examples--------------
    │   ├── ex11            ! PEA example 1
    │   ├── ex12            ! PEA example 2
    │   ├── ex13            ! PEA example 3
    │   ├── ex14            ! PEA example 4
    │   ├── ex15            ! PEA example 5
    │   ├── ex17            ! PEA example 7
    │   ├── ex18            ! PEA example 8
    │   --------------POD examples--------------
    │   ├── ex21            ! POD example 1
    │   ├── ex22            ! POD example 2 full GNSS pod fit example (5 constellations)
    │   ├── ex22/gps        ! POD example 2 US GPS constellation only
    │   ├── ex22/glo        ! POD example 2 Russian GLONASS constellation only
    │   ├── ex22/gal        ! POD example 2 European GALILEO constellation only
    │   ├── ex22/bds        ! POD example 2 Chinese BEIDOU constellation only
    │   ├── ex22/qzss       ! POD example 2 Japanese QZSS constellation only
    │   ├── ex23            ! POD example 3
    │   ├── ex24            ! POD example 4
    │   ├── ex25            ! POD example 5
    │   └── ex26            ! POD example 6
    │   --------------long test examples--------------
    │   ├── ex31/pod_fit    ! POD fit (ex31 stage 1)
    │   ├── ex31/pea        ! PEA re-estimate parameters
    │   ├── ex31/pod_ic     ! POD ic integrate
    │
    ├── lib/                ! Compiled objectlibrary directory*
    ├── scripts/            ! Auxillary Python and Shell scripts and libraries
    └── src/                ! Source code directory
        ├── cpp/            ! PEA source code
        ├── fortran/        ! POD source code
        ├── cmake/   
        ├── doc_templates/
        ├── build/          ! Cmake build directory*
        └── CMakeLists.txt

*\*created during installation process*

*\*\*created by `download_examples.py` script*
***
## Build
Prepare a directory to build in - it's better practice to keep this separated from the source code.
From the Ginan git root directory:

```executable
mkdir -p src/build

cd src/build
:<<'```executable'
```    

Run cmake to find the build dependencies and create the make file.

By default you will compile an optimised version, typically this version will run 3 times faster but if you run into compile problems depending on your system, add the `-DOPTIMISATION=FALSE` flag:

```executable
cmake ../
:<<'```executable'
```    

To build every package simply run `make` or `make -jX` , where X is a number of parallel threads you want to use for the compilation:

```executable
make -j2
:<<'```executable'
```    

Alternatively, to build only a specific package (e.g. PEA or POD), run as below:

    make pea -j2
    
    make pod -j2

This should create executables in the `bin` directory of Ginan.

Check to see if you can execute the PEA from the examples directory

```executable
cd ../../examples

../bin/pea --help
:<<'```executable'
```    

and you should see something similar to:

    PEA starting... (doxyjen release-1.4.1-beta-436-g26247195 from Thu Jul 21 12:24:55 2022)
    Options:
      -h [ --help ]                    Help
      -q [ --quiet ]                   Less output
      -v [ --verbose ]                 More output
      -V [ --very-verbose ]            Much more output
               .
               .
               .
      --input_persistance              Begin with previously stored filter and 
                                       navigation states
      --output_persistance             Store filter and navigation states for 
                                       restarting
    PEA finished


Similarly, check the POD:

```executable
../bin/pod --help
:<<'```executable'
```    
This returns:

    Earth Radiation Model (ERM):   1

    Default master POD config file = POD.in (old - no longer supported) - use a yaml config
    
    yaml config file options by default can be overridden on the command line
    
    Command line: ../bin/pod -m -s -o -a -p -r -t -n -i -u -q -k -w -y -h 
    
    Where: 
        -m --podmode = POD Mode:
                                    1 - Orbit Determination (pseudo-observations; orbit fitting)
                                    2 - Orbit Determination and Prediction
                                    3 - Orbit Integration (Equation of Motion only)
                                    4 - Orbit Integration and Partials (Equation of Motion and Variational Equations)
                                    5 - Single satellite integration from pod_data section of yaml
        -s --pobs    = Pseudo observations orbit .sp3 file name (must be ITRF)
        -o --cobs    = Comparison orbit .sp3 file name
        -a --arclen  = Orbit Estimation Arc length (hours)
        -p --predlen = Orbit Prediction Arc length (hours)
        -r --eopf    = Earth Orientation Paramaeter (EOP) values file
        -t --eopsol  = Earth Orientation Paramaeter file type: (1,2,3)
                                    1 - IERS C04 EOP
                                    2 - IERS RS/PC Daily EOP
                                    3 - IGS RP + IERS RS/PC Daily (dX,dY)
        -n --nutpre  = IAU Precession / Nutation model
                                    2000 - IAU2000A
                                    2006 - IAU2006/2000A
        -i --estiter = Orbit Estimatimation Iterations (1 or greater)
        -u --sp3vel  = Output .sp3 file with velocities
                                    0 - Do not write Velocity vector to sp3 orbit
                                    1 - Write Velocity vector to sp3 orbit
        -q --icmode  = Initial condition from parameter estimation procedure
        -k --srpmodel= 1: ECOM1, 2:ECOM2, 12:ECOM12, 3:SBOX
        -w --empmodel= 1: activated, 0: no estimation
        -d --verbosity = output verbosity level [Default: 0]
        -y --config = yaml config file
        -h --help.   = Print program help
    
    Examples:
    
            ../bin/pod -m 1 -q 1 -k 1 -w 0 -s igs16403.sp3 -o igs16403.sp3 -y ex21.yaml
            ../bin/pod -m 2 -q 1 -k 1 -w 0 -s igs16403.sp3 -p 12 -y exi22.yaml
    
    For orbit updates using Parameter Estimation Algorithm (PEA):
            ../bin/pod -m 4 -q 2 -k 1 -w 0 -s igs16403.sp3 -o igs16403.sp3 -y ex23.yaml



<!-- You should now have the executables in the bin directory: 
    ├── bin			        

    * pod 
    * crs2trs 
    * timesystem 
    * brdc2ecef -->
***
## Documentation

Ginan documentation consists of two parts: these documents, and separate doxygen-generated documentation that shows the actual code infrastructure.
It can be found [here](codeDocs/index.html), or generated manually as below.

### Doxygen

The Doxygen documentation for Ginan requires `doxygen` and `graphviz`. If not already installed, type as follows:

```executable
sudo apt -y install doxygen graphviz
:<<'```executable'
```    

On success, proceed to the build directory and call make with `docs` target:

```executable
cd ../src/build

cmake ../

make docs
:<<'```executable'
```    

The documentation can then be found at `Docs/codeDocs/index.html`. 

Note that documentation is also generated automatically if `make` is called without arguments and `doxygen` and `graphviz` dependencies are satisfied.

***
## Ready!
Congratulations! You are now ready to trial the examples of PEA and POD from the examples directory. See Ginan's manual for detailed explanation of each example. Note that examples have relative paths to files in them and rely on the presence of `products`, `data` and `solutions` directories inside the `examples` directory. Make sure you've run `download_examples.py` from the **Download** step of this instruction.

The paths are relative to the examples directory and hence all the examples must be run from the `examples` directory.

NB Examples may be configured to use mongoDB. If you have not installed it, please set `enable_mongo` to false in the pea config files

To run the first example of the PEA:

```
cd ../examples

../bin/pea --config ex11_pea_pp_user_gps.yaml
```    

This should create `ex11` directory with `ex11-ALIC201919900.TRACE` and `ex1120624.snx` output files. You can remove the need for path specification to the executable by adding Ginan's bin directory to `~/.bachrc` file:

    PATH="path_to_ginan_bin:$PATH"

And an example of POD:

    ../bin/pod -y ex21_pod_fit_gps.yaml

At the completion of the test run, `ex21` directory should be created. The `ex21_.sh` script will return any differences to the standard test resuts.


***
## Python Installation for Plotting, Processing, etc.
Lastly, to run many of the included scripts for fast parsing of .`trace`/`.snx` files, plotting of results, automatic running of the PEA based on input date/times and stations, etc. then a number of python dependencies are needed.

The file `scripts/conda_gn37.yaml` has a list of the necessary python dependencies.
The best way to take advantage of this is to install the Miniconda virtual environment manager.
This will allow you to pass the `.yaml` file into the conda command and automatically set up a new python environment.

### Install Miniconda
To install Miniconda, download and execute the Miniconda shell file:

    wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
    bash Miniconda3-latest-Linux-x86_64.sh

And follow the on-screen instructions  (choosing all defaults is fine).

### Create virtual environment
After installation you can create the `gn37` python environment using a prepared receipy. First open a new terminal session and enter:

    conda env create -f <dir_to_ginan>/scripts/conda_gn37.yaml

You have now created the virtual python environment `gn37` with all necessary dependencies. Anytime you wish you run python scripts, ensure you are in the virtual environment by activating:

    conda activate gn37

And then run your desired script from the `scripts` directory.

### Acknowledgements:
We have used routines obtained from RTKLIB, released under a BSD-2 license, these routines have been preserved with minor modifications in the folder `cpp/src/rtklib`. The original source code from RTKLib can be obtained from https://github.com/tomojitakasu/RTKLIB.

We have used routines obtained from Better Enums, released under the BSD-2 license, these routines have been preserved in the folder `cpp/src/3rdparty` The original source code from Better Enums can be obtained from http://github.com/aantron/better-enums.

We have used routines obtained from EGM96, released under the zlib license, these routines have been preserved in the folder `cpp/src/egm96` The original source code from EGM96 can be obtained from https://github.com/emericg/EGM96.

```executable
echo 'We have used this gist to make this readme executable https://gist.github.com/pjeby/c137ace4d91e61e8f1f80e92d84e8b70'
exit $?
```
