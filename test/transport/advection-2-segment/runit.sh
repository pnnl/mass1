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
# Last Change: Fri Jul  2 08:00:50 1999 by William A. Perkins <perk@hughie.pnl.gov>
# -------------------------------------------------------------
# $Id$

set -x
set -e

                                # to trap floating point errors on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=../../../mass1_v083

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

ln -f -s mass1-Cn=0.1.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Multiple Segment Advection: Courant Number = 0.1\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-Cn=0.1.ps

