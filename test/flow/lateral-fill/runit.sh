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
# Last Change: 2017-06-05 10:39:18 d3g096
# -------------------------------------------------------------
# $Id$

set -x
set -e

                                # to trap floating point errors on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../build/mass1}
python=${PYTHON-python}
tecplot=${TECPLOT-tecplot}
convert=${CONVERT-convert}

$model

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot.ps
(echo \
    set terminal postscript eps color solid \"Helvetica\" 22 \; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot.eps

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    load \"plot-elev.gp\"\; ) | \
        gnuplot > plot-elev.ps
(echo \
    set terminal postscript eps color solid \"Helvetica\" 22 \; \
    load \"plot-elev.gp\"\; ) | \
        gnuplot > plot-elev.eps

(echo \
    set terminal postscript eps color solid \"Helvetica\" 22 \; \
    load \"plot-disch.gp\"\; ) | \
        gnuplot > plot-disch.eps

"$python" ../../../scripts/profile_tecplot.py profile1.out > profile1.dat
"$tecplot" -b -p ../../../scripts/xy-looper-png.mcr stage1.lay
"$convert" -delay 25 -loop 1 looper-0*.png lateral-fill.gif

