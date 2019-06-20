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
# Last Change: 2019-06-19 13:40:33 d3g096
# -------------------------------------------------------------
# $Id$

set -x
set -e

model=${MODEL-../../../build/mass1_new}

$model

gnuplot compare.1992.gp > compare.1992.eps
