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
# Last Change: 2019-03-15 06:14:19 d3g096
# -------------------------------------------------------------
# $Id$

set -x
set -e

model=${MODEL-../../../build/mass1_new}

$model

gnuplot < plot.gp > plot.eps

gnuplot < plot-elev.gp > plot-elev.eps

