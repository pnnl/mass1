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
# Last Change: Wed Dec  8 14:36:35 1999 by William A. Perkins <perk@mack.pnl.gov>
# -------------------------------------------------------------
# $Id$

set -x
set -e

                                # to trap floating point errors on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../build/mass1}

rm -f mass1.cfg
ln -f -s mass1-warmup.cfg mass1.cfg
$model
rm -f mass1.cfg

ln -f -s mass1-n=1.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    'set terminal postscript landscape color solid "Helvetica" 14;' \
    'set title "Variable Flow: 1 Transport Iteration";' \
    'load "plot.gp";' ) | \
        gnuplot > plot-n=1.ps

ln -f -s mass1-n=5.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Variable Flow: 5 Transport Iterations\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-n=5.ps

ln -f -s mass1-n=10.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Variable Flow: 10 Transport Iterations\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-n=10.ps

