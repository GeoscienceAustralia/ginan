
# ------ Docker build target: Run heavyweight build environment locally, on top of ginan-env image ------ #
FROM gnssanalysis/ginan-env:latest as ginan-build
# ^ Recent build with patched (old) version of Doxygen.
# TODO switch from custom build to latest, once Doxygen build is incorporated into it.

# NOTE: the doc build step later on requires a version of ginan-env with Doxygen installed in it.
# This is added by (internal) Ginan PR #520, around Apr 2024

# Grab ccache from a previous compilation, to speed up the build of PEA.
# Note we could use docker native cache, but this is problematic given size constraints and other limitations of
# Bitbucket pipelines.
# NOTE: On Ubuntu 24 based ginan-env images, use 'build-cache-v24'. On older ginan-env images, use 'build-cache'.
# This separation is created as the caches for each version aren't interoperable, so we don't want to wipe them.
COPY --from=gnssanalysis/ginan-cache:build-cache-v24 /root/.ccache /root/.ccache


ENV HOME /root
WORKDIR /ginan

# Code assets for building PEA
ADD src /ginan/src

# Docs, example configs, data and products. All relied on by Doxygen to generate documentation.
# Add exampleConfigs and logical subdirs (ADD doesn't seem to follow symlinks). These are used by Doxygen.
ADD exampleConfigs      /ginan/exampleConfigs
ADD inputData/data      /ginan/exampleConfigs/data
ADD inputData/products  /ginan/exampleConfigs/products
# Add docs sources (used by Doxygen).
ADD Docs /ginan/Docs

# Makefile build target. Options include: pea, docs, ALL
ARG TARGET=pea

# Values >2 may cause memory issues on BitBucket pipelines. On development machines, set higher for speed (eg 4).
ARG BUILD_THREADS=2

# Main outputs are ginan/bin and (if TARGET = pea or ALL) ginan/Docs. Don't need to keep intermediaries in 'build'.
RUN \
    mkdir -p src/build \
    && cd src/build \
    && cmake -DENABLE_OPTIMISATION=TRUE -DENABLE_PARALLELISATION=TRUE -DBUILD_DOC=TRUE .. \
    && make -j $BUILD_THREADS $TARGET \
    && cd - \
    && rm -rf src/build



# ------ Docker build target: store the ccache for future builds ------ #
# Use the Docker no-op / blank image 'scratch' to drop files on top of. Cache gets pushed to DockerHub for later use.
FROM scratch as ginan-build-cache

# Ccache from the latest compilation, to be used to speed up the next build.
COPY --from=ginan-build /root/.ccache /root/.ccache


# ------ Docker build target: minimal image with just PEA and its dependencies (TODO base on Ubuntu) ------ #
FROM gnssanalysis/ginan-env:latest as ginan-minimal
# TODO this should really be based on Ubuntu:24.04, with only those packages installed that are necessary to run pea
# and utilities.

ENV PATH "/ginan/bin:${PATH}"

ADD scripts /ginan/scripts

# Ensure our Python environment is installed so we can run gnssanalysis and relevant scripts.
# Somewhat redundant while image is based on ginan-env, but will still get updates to gnssanalysis applied quicker.
COPY scripts/requirements.txt /tmp/requirements.txt
WORKDIR /tmp/
RUN python3 -m pip install -r requirements.txt --no-cache-dir --break-system-packages

COPY --from=ginan-build /ginan/bin/ /ginan/bin
