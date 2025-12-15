FROM ubuntu:24.04 as ginan-env
# These are set for Bitbucket pipelines memory ceiling. You can set higher on a development machine if you have the memory for it.
ARG BUILD_THREADS_DOXYGEN=4
ARG BUILD_THREADS_GENERAL=2

ARG DEBIAN_FRONTEND=noninteractive
ENV HOME /root

RUN apt-get update -y 								\
    && apt-get upgrade -y --no-install-recommends 	\
    && apt-get install -y --no-install-recommends 	\
    ca-certificates									\
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
    libboost1.83-all-dev							\
    libeigen3-dev 									\
    libopenblas-dev 								\
    liblapack-dev 									\
    liblapacke-dev 									\
    libssl-dev 										\
    libmongoc-1.0-0									\
    libnetcdf-dev 									\
    libnetcdf-c++4-dev 								\
    libncurses-dev 									\
    libzstd-dev 									\
    libssl-dev 										\
    libgomp1 										\
    libyaml-cpp-dev 								\
    ssh 											\
    && rm -rf /var/lib/apt/lists/* 					\
    && apt-get clean

# As of Dec 2024, Ubuntu 24.04 has:
# - libeigen3-dev: version 3.4.0-4
# - boost 1.74 and 1.83 (we previously built version 1.75 from source)
# Note: ca-certificates is a dependency of python3-pip (so would commonly be installed as a result of that).
# It is needed to validate github's cert.

RUN ulimit -c unlimited

RUN mkdir -p /tmp/build
WORKDIR /tmp/build

ENV CFLAGS="-fno-omit-frame-pointer"
ENV CXXFLAGS="-fno-omit-frame-pointer"
ENV CMAKE_CXX_STANDARD="20"

# We currently avoid building this (installing via apt above). But the tradeoff is being
# slightly behind. The latest on Ubuntu 24.04 is currently from Feb (1.26.0).
# RUN git clone --depth 1 --branch 1.26.1 https://github.com/mongodb/mongo-c-driver.git 	\
#     && mkdir -p mongo-c-driver/cmake-build 												\
#     && cd mongo-c-driver/cmake-build 														\
#     && cmake -DENABLE_AUTOMATIC_INIT_AND_CLEANUP=OFF -DENABLE_EXTRA_ALIGNMENT=OFF .. 		\
#     && cmake --build . -j${BUILD_THREADS_GENERAL}											\
#     && cmake --build . --target install -j${BUILD_THREADS_GENERAL}						\
#     && cd - 																				\
#     && rm -rf mongo-c-driver

# Amazingly, this is not available as a built library. Our only realistic option is to build it
# See here: https://www.mongodb.com/docs/languages/cpp/cpp-driver/current/get-started/download-and-install/
RUN git clone --depth 1 --branch r3.10.1 https://github.com/mongodb/mongo-cxx-driver.git	\
    && mkdir -p mongo-cxx-driver/cmake-build 												\
    && cd mongo-cxx-driver/cmake-build 														\
    && cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local 				\
    && cmake --build . -j${BUILD_THREADS_GENERAL}											\
    && cmake --build . --target install	-j${BUILD_THREADS_GENERAL}							\
    && cd - 																				\
    && rm -rf mongo-cxx-driver

# Copy the built MongoCXX driver library into a dedicated directory from which it can be picked up
# by the Ginan minimal image build. Also copy libbsoncxx, a dependency(?) of mongocxx.
# We use the following somewhat convoluted command because:
# - The Dockerfile COPY directive can't filter based on a glob
# - The default shell is sh, so we can't use shopt -s dotglob
RUN mkdir -p /opt/mongocxx/lib/ \
    && cd /usr/local/lib \
    && find -mindepth 1 -maxdepth 1 -name "libmongocxx*" -exec cp --parents -t /opt/mongocxx/lib/ {} + \
    && find -mindepth 1 -maxdepth 1 -name "libbsoncxx*" -exec cp --parents -t /opt/mongocxx/lib/ {} +

# TODO: We're making the assumption we don't need the cmake (ie /usr/local/lib/cmake/mongocxx*) and pkgconfig
# subdirs, but that assumption may be wrong.
# Leaving out -maxdepth 1 could also allow us to grab other relevant files from their respective dirs,
# and keep dir structure. But it might not find everything...


# Note there should be no actual data left in this directory apart from requirements.txt.
# This is a cosmetic rather than space saving cleanup step.
RUN rm -rf /tmp/build

# Install a forked version of doxygen that has some nice features made just for ginan docs
# To avoid an extra container image layer, we also copy the patch to the parent directory not the doxygen working dir
# TODO: revist where to install this fork from, and whether we need it here.
RUN apt-get update -y																				\
    && apt-get install -y --no-install-recommends flex bison 										\
    && git clone --depth 1 --branch customColors https://github.com/polytopes-design/doxygen.git 	\
    && mkdir -p doxygen/build																		\
    && cd doxygen/build																				\
    && cmake -G "Unix Makefiles" .. 																\
    && make -j${BUILD_THREADS_DOXYGEN} 																\
    && make -j${BUILD_THREADS_GENERAL} install 														\
    && cd - 																						\
    && rm -rf doxygen 																				\
    && apt-get remove -y flex bison 																\
    && rm -rf /var/lib/apt/lists/* 																	\
    && apt-get clean

# For clean up, we uninstall flex and bison (apparently build-only dependencies for doxygen).
# Finally, we delete the apt package list, and cached packages.