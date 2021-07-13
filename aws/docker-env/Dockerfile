FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive
ENV HOME /root

RUN apt update -y \
    && apt install -y \
        git \
        gobjc \
        gobjc++ \
        gfortran \
        libopenblas-dev \
        openssl \
        curl \
        net-tools \
        openssh-server \
        cmake \
        make \
        liblapack-dev \
        gzip \
        vim \
        libssl-dev \
        awscli \
        gnupg2 \
        libnetcdf-dev \
        libnetcdf-c++4-dev \
        libzstd-dev \
        libssl-dev \
        doxygen \
        libgomp1 \
        graphviz \
     && rm -rf /var/lib/apt/lists/*

RUN ulimit -c unlimited
RUN systemctl mask apport.service

RUN mkdir -p /tmp/build
WORKDIR /tmp/build

RUN git clone https://github.com/jbeder/yaml-cpp.git \
    && mkdir -p yaml-cpp/cmake-build \
    && cd yaml-cpp/cmake-build \
    && cmake .. -DCMAKE\_INSTALL\_PREFIX=/usr/local/ -DYAML\_CPP\_BUILD\_TESTS=OFF \
    && make install yaml-cpp \
    && cd ../.. \
    && rm -rf yaml-cpp

RUN git clone https://gitlab.com/libeigen/eigen.git \
    && mkdir eigen/cmake-build \
    && cd eigen/cmake-build \
    && cmake .. \
    && make install \
    && cd ../.. \
    && rm -rf eigen

RUN wget https://github.com/mongodb/mongo-c-driver/releases/download/1.17.1/mongo-c-driver-1.17.1.tar.gz \
    && tar -xvf mongo-c-driver-1.17.1.tar.gz \
    && rm mongo-c-driver-1.17.1.tar.gz \
    && mkdir -p mongo-c-driver-1.17.1/cmakebuild \
    && cd mongo-c-driver-1.17.1/cmakebuild \
    && cmake -DENABLE_AUTOMATIC_INIT_AND_CLEANUP=OFF .. \
    && cmake --build . \
    && cmake --build . --target install \
    && cd ../.. \
    && rm -rf mongo-c-driver-1.17.1

RUN curl -OL https://github.com/mongodb/mongo-cxx-driver/releases/download/r3.6.0/mongo-cxx-driver-r3.6.0.tar.gz \
    && tar -xzf mongo-cxx-driver-r3.6.0.tar.gz \
    && rm mongo-cxx-driver-r3.6.0.tar.gz \
    && mkdir -p mongo-cxx-driver-r3.6.0/build \
    && cd mongo-cxx-driver-r3.6.0/build \
    && cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local \
    && cmake --build . --target EP_mnmlstc_core \
    && cmake --build . \
    && cmake --build . --target install \
    && cd ../.. \
    && rm -rf mongo-cxx-driver-r3.6.0

RUN wget -qO - https://www.mongodb.org/static/pgp/server-4.4.asc | apt-key add -
RUN echo "deb [ arch=amd64,arm64 ] https://repo.mongodb.org/apt/ubuntu focal/mongodb-org/4.4 multiverse" | tee /etc/apt/sources.list.d/mongodb-org-4.4.list
RUN echo "deb [ arch=amd64,arm64 ] https://repo.mongodb.org/apt/ubuntu bionic/mongodb-org/4.4 multiverse" | tee /etc/apt/sources.list.d/mongodb-org-4.4.list
RUN apt update -y \
    && apt -y install mongodb-org \
    && rm -rf /var/lib/apt/lists/*

ENV PATH "${HOME}/.miniconda3/bin:${PATH}"
COPY scripts/conda_gn37.yaml /tmp/build/conda_gn37.yaml
RUN wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O /tmp/build/miniconda.sh \
    && bash /tmp/build/miniconda.sh -b -p "$HOME/.miniconda3" \
    && rm /tmp/build/miniconda.sh \
    && conda env create -f /tmp/build/conda_gn37.yaml \
    && conda clean -a

RUN wget -c https://boostorg.jfrog.io/artifactory/main/release/1.75.0/source/boost_1_75_0.tar.gz \
    && gunzip boost_1_75_0.tar.gz \
    && tar xvf boost_1_75_0.tar \
    && rm boost_1_75_0.tar \
    && cd boost_1_75_0/ \
    && ./bootstrap.sh \
    && ./b2 install \
    && cd .. \
    && rm -rf boost_1_75_0/

RUN rm -rf /tmp/build
