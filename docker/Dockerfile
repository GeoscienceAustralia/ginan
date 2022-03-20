FROM gnssanalysis/ginan-env:latest

ENV HOME /root
RUN mkdir -p /ginan
ADD . /ginan
WORKDIR /ginan
ARG TARGET
RUN mkdir -p src/build \
    && cd src/build \
    && cmake -DENABLE_OPTIMISATION=TRUE -DENABLE_PARALLELISATION=TRUE -DENABLE_MONGODB=TRUE .. \
    && make -j 3 $TARGET \
    && cd ../.. \
    && rm -rf src/build

ENV PATH "/ginan/bin:${PATH}"
RUN cd /ginan/scripts && conda run -n gn37 pip install .
COPY ./docker/.bashrc /root/.bashrc
