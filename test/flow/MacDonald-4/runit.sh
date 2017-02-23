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
# Last Change: 2017-01-16 08:36:57 d3g096
# -------------------------------------------------------------
# $Id$

set -x
set -e

model=${MODEL-../../../mass1}

$model

gnuplot < plot-depth.gp > plot-depth.eps
gnuplot < plot-elev.gp > plot-elev.eps

