FROM gnssanalysis/ginan-env:latest
ENV HOME /root
RUN mkdir -p /ginan
ADD . /ginan
WORKDIR /ginan
ARG TARGET
RUN mkdir -p src/build \
    && cd src/build \
    && cmake -DENABLE_OPTIMISATION=TRUE -DENABLE_PARALLELISATION=TRUE .. \
    && make -j 2 $TARGET \
    && cd ../.. \
    && rm -rf src/build

ENV PATH "/ginan/bin:${PATH}"
# Update gnssanalysis version for the pipeline here if needed.
RUN pip3 install gnssanalysis==0.0.42

