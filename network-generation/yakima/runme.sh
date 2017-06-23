#! /bin/sh

set -xue

python ../generate.py \
    --verbose \
    --notch-width=0.0 \
    --notch-depth=0.0 \
    --upstream-discharge=0.0 \
    --downstream-depth=10.0 \
    --basin-discharge=3500.0 \
    --mannings=0.0325 \
    --gage-output \
    --dry-start \
    Yakima_network_info.dat.orig
../../build/mass1
