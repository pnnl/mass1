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
# Last Change: Tue Dec 14 22:46:58 1999 by William A. Perkins <perk@mack.pnl.gov>
# -------------------------------------------------------------
# $Id$

set -x
set -e

                                # to trap floating point errors on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../mass1_v084}

rm -f mass1.cfg
ln -f -s mass1-inflow.cfg mass1.cfg
$model

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Lateral Inflow\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-inflow.ps
(echo \
    set terminal postscript eps color solid \"Helvetica\" 14\; \
    set title \"Lateral Inflow\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-inflow.eps

rm -f mass1.cfg
ln -f -s mass1-outflow.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Lateral Outflow\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-outflow.ps
(echo \
    set terminal postscript eps color solid \"Helvetica\" 14\; \
    set title \"Lateral Outflow\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-outflow.eps

