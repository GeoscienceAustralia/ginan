
# ------ Docker build target: Run heavyweight build environment locally, on top of ginan-env image ------ #
FROM gnssanalysis/ginan-env:latest as ginan-build

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

# Add .git directory so build process can extract current tag / commit hash to embed in build.
# This data will not be published. The build image is not pushed, only the built artefacts (which are copied
# into the minimal image)
ADD .git /ginan/.git

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

# Main outputs are ginan/bin and (if TARGET = pea or ALL) ginan/Docs.
# NOTE: while the cmake line appears to enable doc building, docs don't actually get written unless the
# make target on the following line is set to 'docs' or 'ALL'.
RUN \
    mkdir -p src/build \
    && cd src/build \
    && cmake -DCMAKE_BUILD_TYPE=Release -DBUILD_DOC=TRUE .. \
    && make -j$BUILD_THREADS $TARGET \
    && cd - \
    && rm -rf src/build



# ------ Docker build target: store the ccache for future builds ------ #
# Use the Docker no-op / blank image 'scratch' to drop files on top of. Cache gets pushed to DockerHub for later use.
FROM scratch as ginan-build-cache

# Ccache from the latest compilation, to be used to speed up the next build.
COPY --from=ginan-build /root/.ccache /root/.ccache


# ------ Docker build target: minimal image with just PEA and its dependencies ------ #
FROM ubuntu:24.04 as ginan-minimal

# Was 1 thread, before being broken out into an argument
ARG BUILD_THREADS=1

# Install runtime dependencies, then clean up apt artifacts/caches to avoid saving them in the image
# Curl and gpg only required here to set up Mongo repo file.
RUN apt-get update -y \
    && apt-get upgrade -y --no-install-recommends \
    && apt-get install -y --no-install-recommends \
    curl \
    gpg \
    liblapacke-dev \
    libopenblas0 \
    libyaml-cpp0.8 \
    libncurses6 \
    libgomp1 \
    libmongoc-1.0-0 \
    python3-pip \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

    #TODO check which of these libraries we can remove, if we build PEA with the static version of them instead.

# Install MongoDB 7, via third party repo, clean up apt caches
RUN curl -fsSL https://pgp.mongodb.com/server-7.0.asc | gpg -o /usr/share/keyrings/mongodb-server-7.0.gpg --dearmor \
    && echo "deb [ arch=amd64,arm64 signed-by=/usr/share/keyrings/mongodb-server-7.0.gpg ] https://repo.mongodb.org/apt/ubuntu jammy/mongodb-org/7.0 multiverse" | tee /etc/apt/sources.list.d/mongodb-org-7.0.list \
    && apt update -y \
    && apt-get install -y --no-install-recommends mongodb-org \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean


# Ensure our Python environment is installed so we can run gnssanalysis and relevant scripts.
COPY scripts/requirements.txt /tmp/requirements.txt
WORKDIR /tmp/
RUN python3 -m pip install -r requirements.txt --no-cache-dir --break-system-packages

ADD scripts /ginan/scripts

# Copy the built PEA binary from the build stage
COPY --from=ginan-build /ginan/bin/ /usr/bin/
# TODO: we could update the build process to output to /usr/bin/ instead
# For backwards compatibility, put a link at /ginan/bin/pea pointing to /usr/bin/pea:
RUN mkdir /ginan/bin/ \
    && ln -s /usr/bin/pea /ginan/bin/pea \
    && chmod go-w /usr/bin/pea
# Permissions update probably not important (all processes have the same access level in the container)

# Copy Mongocxx driver (including symlinks) from what is effectively the pre-build stage (ginan-env.Dockerfile)
COPY --from=gnssanalysis/ginan-env:latest /opt/mongocxx/lib/ /usr/local/lib/
# Alternatively, if we built mongocxx at the pea build stage above:
# COPY --from=ginan-build /opt/mongocxx/lib/ /usr/local/lib/

# Render yaml of PEA's default parameter values, for reference. Note: -Y 3 is used in pipeline.
# This doubles as a check that pea loads successfully.
RUN pea -Y 3 > /ginan/pea-defaults.yaml