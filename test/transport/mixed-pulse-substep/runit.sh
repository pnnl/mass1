#! /bin/sh
# -------------------------------------------------------------
# file: runit.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 11, 1998 by William A. Perkins
# Last Change: 2019-05-07 10:36:45 d3g096
# -------------------------------------------------------------
# $Id$

set -x
set -e

                                # to trap floating point errors on SGI

model=${MODEL-../../../build/mass1_new}

rm -f mass1.cfg
ln -f -s mass1-warmup.cfg mass1.cfg
$model
rm -f mass1.cfg

ln -f -s mass1-run.cfg mass1.cfg
$model
rm -f mass1.cfg

