#! /bin/sh
# -------------------------------------------------------------
# file: runit.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  2, 1999 by William A. Perkins
# Last Change: 2018-08-15 14:32:12 d3g096
# -------------------------------------------------------------
# $Id$

set -x
set -e

# This will not work with the old code
model=${MODEL-../../../build/mass1_new}

rm -f mass1.cfg
ln -f -s mass1-inflow.cfg mass1.cfg
$model
mv -f profile1.out profile1.inflow.out

rm -f mass1.cfg
ln -f -s mass1-outflow.cfg mass1.cfg
$model
rm -f mass1.cfg
mv -f profile1.out profile1.outflow.out

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot.ps
(echo \
    set terminal postscript eps color solid \"Helvetica\" 22\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot.eps

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    load \"plot-elev.gp\"\; ) | \
        gnuplot > plot-elev.ps
(echo \
    set terminal postscript eps color solid \"Helvetica\" 22\; \
    load \"plot-elev.gp\"\; ) | \
        gnuplot > plot-elev.eps

