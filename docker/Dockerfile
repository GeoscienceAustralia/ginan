FROM gnssanalysis/ginan-env:latest

ENV HOME /root
RUN mkdir -p /ginan
ADD . /ginan
WORKDIR /ginan
RUN mkdir -p src/build \
    && cd src/build \
    && cmake -DENABLE_OPTIMISATION=TRUE .. \
    && make -j 3 \
    && cd ../.. \
    && rm -rf src/build

ENV PATH "/ginan/bin:${PATH}"
