#! /bin/sh
# -------------------------------------------------------------
# file: runit-nt.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created August 12, 1999 by William A. Perkins
# Last Change: Thu Aug 12 15:00:42 1999 by William A. Perkins <perk@mack.pnl.gov>
# -------------------------------------------------------------
# $Id$ 

set -x
set -e

model=../../../Release/mass1_v082

rm -f mass1.cfg
cp mass1-inflow.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Lateral Inflow\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-inflow.ps

rm -f mass1.cfg
cp mass1-outflow.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Lateral Outflow\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-outflow.ps
