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
# Last Change: 2020-05-04 14:04:43 d3g096
# -------------------------------------------------------------
# $Id$

set -x
set -e

                                # to trap floating point errors on SGI

model=${MODEL-../../../build/mass1_new}

$model


(echo \
    set terminal postscript eps color solid \"Helvetica\" 14\; \
    set output \"plot.eps\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot.eps
