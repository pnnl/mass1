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
# Last Change: Tue Dec 14 16:07:24 1999 by William A. Perkins <perk@mack.pnl.gov>
# -------------------------------------------------------------
# $Id$

set -x
set -e

                                # to trap floating point errors on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../mass1_v084}

touch nolatinflow

rm -f mass1.cfg
ln -f -s mass1-warmup.cfg mass1.cfg
$model
rm -f mass1.cfg

ln -f -s mass1-Cn=1.0.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    'set terminal postscript landscape color solid "Helvetica" 14;' \
    'set title "Multiple Segment Advection: Courant Number = 1.0";' \
    'load "plot.gp";' ) | \
        gnuplot > plot-Cn=1.0.ps
(echo \
    'set terminal postscript eps color solid "Helvetica" 14;' \
    'set title "Multiple Segment Advection: Courant Number = 1.0";' \
    'load "plot.gp";' ) | \
        gnuplot > plot-Cn=1.0.eps

ln -f -s mass1-Cn=0.1.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Multiple Segment Advection: Courant Number = 0.1\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-Cn=0.1.ps
(echo \
    set terminal postscript eps color solid \"Helvetica\" 14\; \
    set title \"Multiple Segment Advection: Courant Number = 0.1\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-Cn=0.1.eps
