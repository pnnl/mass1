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
# Last Change: Mon Jan  9 07:00:36 2006 by William A. Perkins <perk@McPerk.pnl.gov>
# -------------------------------------------------------------
# $Id$

set -x
set -e

                                # to trap floating point errors on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../build/mass1}

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

