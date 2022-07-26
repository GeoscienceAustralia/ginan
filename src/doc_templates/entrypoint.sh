#!/bin/sh -xe

mkdir -p /data/ginan/src/builddocs
cd /data/ginan/src/builddocs
cmake ../doc_templates -Wno-dev || true
make docs