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
# Last Change: 2019-03-14 13:28:23 d3g096
# -------------------------------------------------------------
# $Id$

set -x
set -e

model=${MODEL-../../../build/mass1_new}

$model

gnuplot < plot.gp > plot.eps

gnuplot < plot-elev.gp > plot-elev.eps

