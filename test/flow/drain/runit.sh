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
# Last Change: 2017-06-22 15:08:47 d3g096
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

gnuplot < plot.gp > plot.eps

gnuplot < plot-elev.gp > plot-elev.eps

gnuplot < plot-disch.gp > plot-disch.eps

"$python" ../../../scripts/profile_tecplot.py profile1.out > profile1.dat
"$tecplot" -b -p ../../../scripts/xy-looper-png.mcr stage1.lay
"$convert" -delay 20 -loop 1 looper-0*.png drain.gif

