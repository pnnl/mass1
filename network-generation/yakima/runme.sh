#! /bin/sh

set -xue

notchw=0.0
notchd=0.0
hotstart="restart.dat.zero"

rm -f *.out

python ../generate.py \
    --verbose \
    --notch-width=$notchw \
    --notch-depth=$notchd \
    --upstream-discharge=0.0 \
    --downstream-depth=10.0 \
    --basin-discharge=1000.0 \
    --mannings=0.04 \
    --gage-output \
    Yakima_network_info.dat.orig
../../build/mass1_new

cp restart.dat restart.dat.0
rm -f *.out

python ../generate.py \
    --verbose \
    --notch-width=$notchw \
    --notch-depth=$notchd \
    --upstream-discharge=0.0 \
    --downstream-depth=10.0 \
    --basin-discharge=3500.0 \
    --mannings=0.04 \
    --gage-output \
    --hot-start="restart.dat.0" \
    Yakima_network_info.dat.orig
../../build/mass1_new
