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
# Last Change: Tue Mar 23 14:49:19 1999 by William A. Perkins <perk@erebus.pnl.gov>
# -------------------------------------------------------------
# $Id$

set -x
set -e

                                # to trap floating point errors on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=../../../mass1_v081

ln -f -s mass1-warmup.cfg mass1.cfg
$model

ln -f -s mass1-test.cfg mass1.cfg
$model

echo 'set terminal postscript landscape color solid "Helvetica" 14; load "plot.gp"' | \
    gnuplot > plot.ps