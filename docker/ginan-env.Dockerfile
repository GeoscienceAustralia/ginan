FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive
ENV HOME /root

RUN apt-get update -y 								\
    && apt-get upgrade -y --no-install-recommends 	\
    && apt-get install -y --no-install-recommends 	\
    git 											\
    gcc 											\
    g++ 											\
    ccache 											\
    graphviz 										\
    openssl 										\
    net-tools 										\
    curl 											\
    wget 											\
    cmake 											\
    make 											\
    gzip 											\
    vim 											\
    libopenblas-dev 								\
    liblapack-dev 									\
    libssl-dev 										\
    libnetcdf-dev 									\
    libnetcdf-c++4-dev 								\
    libncurses-dev 									\
    libzstd-dev 									\
    libssl-dev 										\
    libgomp1 										\
    python3-pip 									\
    python3-dev 									\
    ssh 											\
    && rm -rf /var/lib/apt/lists/* 					\
    && apt-get clean

RUN ulimit -c unlimited

RUN mkdir -p /tmp/build
WORKDIR /tmp/build

COPY scripts/requirements.txt /tmp/build/requirements.txt
RUN python3 -m pip install -r requirements.txt --no-cache-dir --break-system-packages

ENV CFLAGS="-fno-omit-frame-pointer"
ENV CXXFLAGS="-fno-omit-frame-pointer"
ENV CMAKE_CXX_STANDARD="20"
RUN git clone --depth 1 --branch 0.8.0 https://github.com/jbeder/yaml-cpp.git 				\
    && mkdir -p yaml-cpp/cmake-build 														\
    && cd yaml-cpp/cmake-build 																\
    && cmake .. -DCMAKE\_INSTALL\_PREFIX=/usr/local/ -DYAML\_CPP\_BUILD\_TESTS=OFF 			\
    && make install yaml-cpp 																\
    && cd - 																				\
    && rm -rf yaml-cpp

RUN git clone --depth 1 --branch 3.4.0 https://gitlab.com/libeigen/eigen.git 				\
    && mkdir -p eigen/cmake-build 															\
    && cd eigen/cmake-build 																\
    && cmake .. 																			\
    && make install 																		\
    && cd - 																				\
    && rm -rf eigen

RUN git clone --depth 1 --branch 1.26.1 https://github.com/mongodb/mongo-c-driver.git 		\
    && mkdir -p mongo-c-driver/cmake-build 													\
    && cd mongo-c-driver/cmake-build 														\
    && cmake -DENABLE_AUTOMATIC_INIT_AND_CLEANUP=OFF -DENABLE_EXTRA_ALIGNMENT=OFF .. 		\
    && cmake --build . 																		\
    && cmake --build . --target install 													\
    && cd - 																				\
    && rm -rf mongo-c-driver

RUN git clone --depth 1 --branch r3.10.1 https://github.com/mongodb/mongo-cxx-driver.git	\
    && mkdir -p mongo-cxx-driver/cmake-build 												\
    && cd mongo-cxx-driver/cmake-build 														\
    && cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local 				\
    && cmake --build . 																		\
    && cmake --build . --target install														\
    && cd - 																				\
    && rm -rf mongo-cxx-driver

ARG BOOST_VER=1.75.0
RUN BOOST_NAME=$(echo boost_${BOOST_VER} | tr . _);																		\
    wget -c --no-verbose https://boostorg.jfrog.io/artifactory/main/release/${BOOST_VER}/source/${BOOST_NAME}.tar.gz 	\
    && tar xf ${BOOST_NAME}.tar.gz 																						\
    && rm ${BOOST_NAME}.tar.gz 																							\
    && cd ${BOOST_NAME}/ 																								\
    && ./bootstrap.sh 																									\
    && ./b2 -j6 install 																								\
    && cd - 																											\
    && rm -rf ${BOOST_NAME}/

# Note there should be no actual data left in this directory apart from requirements.txt.
# This is a cosmetic rather than space saving cleanup step.

RUN rm -rf /tmp/build

# Install a forked version of doxygen that has some nice features made just for ginan docs
# To avoid an extra container image layer, we also copy the patch to the parent directory not the doxygen working dir

RUN apt-get update -y																				\
    && apt-get install -y --no-install-recommends flex bison 										\
    && git clone --depth 1 --branch customColors https://github.com/polytopes-design/doxygen.git 	\
    && mkdir -p doxygen/build																		\
    && cd doxygen/build																				\
    && cmake -G "Unix Makefiles" .. 																\
    && make -j4 																					\
    && make install 																				\
    && cd - 																						\
    && rm -rf doxygen 																				\
    && apt-get remove -y flex bison 																\
    && apt-get autoremove -y m4 																	\
    && rm -rf /var/lib/apt/lists/* 																	\
    && apt-get clean

# For clean up, we uninstall flex and bison (apparently build-only dependencies for doxygen).
# We also remove m4 (required by bison/flex) but do this conditionally on whether anything else still needs it.
# Finally, we delete the apt package list, and cached packages.
