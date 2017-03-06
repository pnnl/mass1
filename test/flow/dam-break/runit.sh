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
# Last Change: 2017-03-06 09:51:13 d3g096
# -------------------------------------------------------------
# $Id$

set -x
set -e

model=${MODEL-../../../mass1}

$model

(echo \
    set terminal postscript eps color solid \"Helvetica\" 18 \; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot.eps

