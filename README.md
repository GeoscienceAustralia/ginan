![gn_logo](https://raw.githubusercontent.com/GeoscienceAustralia/ginan/gh-pages/images/GinanLogo273.png)

# Ginan: software toolkit and service


#### `Ginan v1.3-beta release`

## Overview

Ginan is a processing package being developed to processes GNSS observations for geodetic applications.  

We currently support the processing of:

* the United States' Global Positioning System (**GPS**);
* the Russian GLONASS system (**GLONASS**);
* the European Union's Galileo system (**Galileo**);
* the Chinese Navigation Satellite System (**BeiDou**);
* the Japanese QZSS develop system (**QZSS**).

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

The software consists of two main components: Network Parameter Estimation Algorithm (PEA) and Precise Orbit Determination (POD).
***
## Supported Platforms

Ginan is supported on the following platforms

* Linux
* MacOS
***
## Download

You can download Ginan source from github using git clone:

    git clone https://github.com/GeoscienceAustralia/ginan.git
    
Then download all of the example data using the python script provided:

    pip3 install wheel pandas boto3 unlzw
    python3 scripts/download_examples.py 
***
## Directory Structure

    ginan/
    ├── README.md			! General README information
    ├── LICENSE.md		    ! Software License information
    ├── ChangeLOG.md		    ! Release Chnage history
    ├── aws/		        ! Amazon Web Services config
    ├── bin/		        ! Binary executables directory*
    ├── CMakeLists.txt		! Cmake build file
    ├── docs/			    ! Documentation directory
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
    ├── lib/		        ! Compiled objectlibrary directory*
    ├── scripts/		    ! Auxillary Python and Shell scripts and libraries
    └── src/		        ! Source code directory
        ├── cpp/            ! PEA source code
        ├── fortran/        ! POD source code
        ├── cmake/   
        ├── doc_templates/
        ├── build/			! Cmake build directory*
        └── CMakeLists.txt

*\*created during installation process*

*\*\*created by `download_examples.py` script*
***

## Using Ginan with Docker

With docker, you can quickly create your environment by downloading the docker image:

    docker pull gnssanalysis/ginan:v1.0-alpha

Then you can run `bash` inside image as follows:

    docker run -it -v /data:/data gnssanalysis/ginan:v1.0-alpha bash

To verify you have the Ginan executables available, run in this bash session:

    pea --help
    
    pod --help

More details on how to use the image is available in the "Docker" section of Ginan manual (s3://ginan-manual).

## Dependencies

Otherwise Ginan has several software dependencies:

* C/C++ and Fortran compiler. We use and recommend [gcc-g++ and gfortran](https://gcc.gnu.org)
* BLAS and LAPACK linear algebra libraries. We use and recommend [OpenBlas](https://www.openblas.net/) as this contains both libraries required
* CMAKE  > 3.0 
* YAML   > 0.6
* Boost  > 1.70 (tested on 1.73)
* Eigen3
* netCDF4
* Python3 (tested on Python 3.7

Acknowledgements:
We have used routines obtained from RTKLIB, released under a BSD-2 license, these routines have been preserved with minor modifications in the folder cpp/src/rtklib The original source code from RTKLib can be obtained from https://github.com/tomojitakasu/RTKLIB

We have used routines obtained from Better Enums, released under the BSD-2 license, these routines have been preserved in the folder cpp/src/3rdparty The original source code from Better Enums can be obtained from http://github.com/aantron/better-enums.
***
## Installing dependencies with Ubuntu

Update the base operating system:

    sudo apt update
    
    sudo apt upgrade

Install base utilities `gcc`, `gfortran`, `git`, `openssl`, `openblas` etc:

    sudo apt install -y git gobjc gobjc++ gfortran libopenblas-dev openssl curl net-tools openssh-server cmake make libssl1.0-dev

Since Ginan v1.2-alpha both gcc and g++ of version 9 are required, so make sure to update the gcc/g++ alternatives prior to compilation:

    sudo add-apt-repository ppa:ubuntu-toolchain-r/test
    
    sudo apt update
    
    sudo apt install gcc-9 g++-9
    
    sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-9 51
    
    sudo update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-9 51
    
***
## Building additional dependencies 

Depending on the user's installation choice: install PEA-only, POD-only or all software packages, a set of additional dependencies that need to be built may change. Below, we explain building all the additional dependencies:

First, create a temporary directory structure to make the dependencies in, it can be removed after the installation process is done:

    mkdir ~/tmp
    cd ~/tmp

Note that `~/tmp` is only used here as example and can be any directory

### YAML
We are using the [YAML](https://github.com/jbeder/yaml-cpp) library to parse the configuration files used to run many of the programs found in this library. Here is an example of how to install the yaml library from source:

    cd ~/tmp
    
    git clone https://github.com/jbeder/yaml-cpp.git
    
    cd yaml-cpp
    
    mkdir cmake-build
    
    cd cmake-build
    
    cmake .. -DCMAKE\_INSTALL\_PREFIX=/usr/local/ -DYAML\_CPP\_BUILD\_TESTS=OFF
    
    sudo make install yaml-cpp
    
    cd ../..
    
    rm -rf yaml-cpp

### Boost (PEA)
PEA relies on a number of the utilities provided by [boost](https://www.boost.org/), such as their time and logging libraries.

    cd ~/tmp
    
    wget -c https://boostorg.jfrog.io/artifactory/main/release/1.73.0/source/boost_1_73_0.tar.gz
    
    gunzip boost_1_73_0.tar.gz
    
    tar xvf boost_1_73_0.tar
    
    cd boost_1_73_0/
    
    ./bootstrap.sh
    
    sudo ./b2 install
    
    cd ..
    
    sudo rm -rf boost_1_73_0/ boost_1_73_0.tar

### Eigen3 (PEA)
Eigen3 is used for performing matrix calculations in PEA, and has a very nice API.

    cd ~/tmp
    
    git clone https://gitlab.com/libeigen/eigen.git
    
    cd eigen
    
    git checkout dfa51767
    
    mkdir cmake-build
    
    cd cmake-build
    
    cmake ..
    
    sudo make install
    
    cd ../..
    
    rm -rf eigen


### MongoDB (PEA, optional but now is defaulted to on)
Needed for realtime preview of the processed results (developers-only)

    cd ~/tmp
    
    wget https://github.com/mongodb/mongo-c-driver/releases/download/1.17.1/mongo-c-driver-1.17.1.tar.gz
    
    tar -xvf mongo-c-driver-1.17.1.tar.gz
    
    cd mongo-c-driver-1.17.1/
    
    mkdir cmakebuild
    
    cd cmakebuild/
    
    cmake -DENABLE_AUTOMATIC_INIT_AND_CLEANUP=OFF ..
    
    cmake --build .
    
    sudo cmake --build . --target install

    cd ~/tmp
    
    curl -OL https://github.com/mongodb/mongo-cxx-driver/releases/download/r3.6.0/mongo-cxx-driver-r3.6.0.tar.gz
    
    tar -xzf mongo-cxx-driver-r3.6.0.tar.gz
    
    cd mongo-cxx-driver-r3.6.0/build
    
    cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local ..
    
    sudo cmake --build . --target EP_mnmlstc_core
    
    cmake --build .
    
    sudo cmake --build . --target install

    wget -qO - https://www.mongodb.org/static/pgp/server-4.4.asc | sudo apt-key add -
    
    echo "deb [ arch=amd64,arm64 ] https://repo.mongodb.org/apt/ubuntu focal/mongodb-org/4.4 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-4.4.list
    
    echo "deb [ arch=amd64,arm64 ] https://repo.mongodb.org/apt/ubuntu bionic/mongodb-org/4.4 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-4.4.list
    
    sudo apt update
    
    sudo apt install mongodb-org   

    cd ../..
    
    sudo rm -rf mongo-c-driver-1.17.1  mongo-c-driver-1.17.1.tar.gz  mongo-cxx-driver-r3.6.0  mongo-cxx-driver-r3.6.0.tar.gz


To start MongoDB:

    sudo systemctl start mongod
    sudo systemctl status mongod
    mongod

To autostart MongoDB on system startup:

    systemctl enable mongod.service


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

    sudo apt -y install libnetcdf-dev libnetcdf-c++4-dev
***
## Build
Prepare a directory to build in - it's better practice to keep this separated from the source code.
From the Ginan git root directory:

    cd src
    
    mkdir build
    
    cd build

Run cmake to find the build dependencies and create the make file. If you wish to disable the optional MONGO DB utilities you will need to add the `-DENABLE_MONGODB=FALSE` flag. By default you will compile an optimised version, typically this version will run 3 times faster but you may run into compile problems depending on your system, add the `-DOPTIMISATION=FALSE` flag:

    cmake [-DENABLE_MONGODB=FALSE] [-DENABLE_OPTIMISATION=FALSE] ..

To build every package simply run `make` or `make -j 2` , where 2 is a number of parallel threads you want to use for the compilation:

    make [-j 2]

To build specific package (e.g. PEA or POD), run as below:

    make pea -j 2
    make pod -j 2

This should create executables in the `bin` directory of Ginan.

cd ../examples

Check to see if you can execute the PEA:

    ../bin/pea --help

and you should see something similar to:

    PEA starting... (pea_pod_examples vbf8c9cc from Tue Jul 6 06:09:50 2021)
    Options:
    --help                      Help
    --quiet                     Less output
    --verbose                   More output
    --very-verbose              Much more output
    --config arg                Configuration file
    --trace_level arg           Trace level
    --antenna arg               ANTEX file
    --navigation arg            Navigation file
    --sinex arg                 SINEX file
    --sp3files arg              Orbit (SP3) file
    --clkfiles arg              Clock (CLK) file
    --dcbfiles arg              Code Bias (DCB) file
    --bsxfiles arg              Bias Sinex (BSX) file
    --ionfiles arg              Ionosphere (IONEX) file
    --podfiles arg              Orbits (POD) file
    --blqfiles arg              BLQ (Ocean loading) file
    --erpfiles arg              ERP file
    --elevation_mask arg        Elevation Mask
    --max_epochs arg            Maximum Epochs
    --epoch_interval arg        Epoch Interval
    --rnx arg                   RINEX station file
    --root_input_dir arg        Directory containg the input data
    --root_output_directory arg Output directory
    --start_epoch arg           Start date/time
    --end_epoch arg             Stop date/time
    --run_rts_only arg          RTS filename (without _xxxxx suffix)
    --dump-config-only          Dump the configuration and exit
    --input_persistance         Begin with previously stored filter and 
                                navigation states
    --output_persistance        Store filter and navigation states for restarting
    PEA finished


Similarly, check the POD:

    ../bin/pod --help
This returns:

    Earth Radiation Model (ERM):   1

    Default master POD config file = POD.in (old - no longer supported) - use a yaml config
    
    yaml config file options by defaut can be overridden on the command line
    
    Command line: ../bin/pod -m -s -o -a -p -r -t -n -i -u -q -k -w -y -h 
    
    Where: 
        -m --podmode = POD Mode:
                                    1 - Orbit Determination (pseudo-observations; orbit fitting)
                                    2 - Orbit Determination and Prediction
                                    3 - Orbit Integration (Equation of Motion only)
                                    4 - Orbit Integration and Partials (Equation of Motion and Variational Equations)
        -s --pobs    = Pseudo observations orbit .sp3 file name
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
    * brdc2ecef -->
***
## Documentation

Ginan documentation consists of two parts: a doxygen-generated documentation that shows the actual code infrastructure and a detailed manual, written in latex, that provides an overview of the software, a thoretical background, a detailed "how to" guide etc.
Below, we explain on how to generate each bit of documentation:

### Doxygen
Move back to the root of your ginan installation

    cd ..
The Doxygen documentation for Ginan requires `doxygen` and `graphviz`. If not already installed, type as follows:

    sudo apt -y install doxygen graphviz

On success, proceed to the build directory and call make with `doc_doxygen` target:

    cd src/build
    make doc_doxygen

The docs can then be found at `docs/html/index.html`. Note that documentation is generated automatically if `make` is called without arguments and `doxygen` and `graphviz` dependencies are satisfied.

### Latex
A detailed Ginan manual is located in `docs/manual` and is in latex format. To compile Latex to pdf you will need a compiler, such as texlive:

    sudo apt install texlive-latex-base texlive-latex-extra

Now, go to `docs/manual` and generate the pdf:

    cd docs/manual
    pdflatex main.tex
    makeglossaries main
    pdflatex main.tex

`main.pdf` file should now appear in the directory.
***
## Ready!
Congratulations! You are now ready to trial the examples of PEA and POD from the examples directory. See Ginan's manual for detailed explanation of each example. Note that examples have relative paths to files in them and rely on the presence of `products`, `data` and `solutions` directories inside the `examples` directory. Make sure you've run `download_examples.py` from the **Download** step of this instruction.

The paths are relative to the examples directory and hence all the examples must be run from the `examples` directory.

    cd ../examples

To run the first example of the PEA:

    ../bin/pea --config ex11_pea_pp_user_gps.yaml

This should create `ex11` directory with `ex11-ALIC201919900.TRACE` and `ex1120624.snx` output files. You can remove the need for path specification to the executable by adding Ginan's bin directory to `~/.bachrc` file:

    PATH="path_to_ginan_bin:$PATH"

And an example of POD:

    ../bin/pod -y ex21_pod_fit_gps.yaml

At the completion of the test run, `ex21` directory should be create. The `ex21_.sh` script will return any differences to the standard test resuts.


***
## Python Installation for Plotting, Processing, etc.
Lastly, to run many of the included scripts for fast parsing of .trace/.snx files, plotting of results, automatic running of the PEA based on input date/times and stations, etc. then a number of python dependencies are needed.

The file scripts/conda_gn37.yaml has a list of the necessary python dependencies.
The best way to take advantage of this is to install the Miniconda virtual environment manager.
This will allow you to pass the .yaml file into the conda command and automatically set up a new python environment.

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
