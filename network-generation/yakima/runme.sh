#! /bin/sh

set -xue

python ../generate.py \
    --verbose \
    --notch-width=1.0 \
    --notch-depth=4.0 \
    --upstream-discharge=3.0 \
    --downstream-depth=10.0 \
    --basin-discharge=3500.0 \
    --mannings=0.0325 \
    Yakima_network_info.dat
../../mass1
