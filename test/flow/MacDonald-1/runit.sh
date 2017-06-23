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
# Last Change: Sat Jan  7 21:17:30 2006 by William A. Perkins <perk@mcperktop.local>
# -------------------------------------------------------------
# $Id$

set -x
set -e

                                # to trap floating point errors on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../build/mass1}

$model

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    load \"plot-depth.gp\"\; ) | \
        gnuplot > plot-depth.ps
(echo \
    set terminal postscript eps color solid \"Helvetica\" 22 \; \
    load \"plot-depth.gp\"\; ) | \
        gnuplot > plot-depth.eps

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    load \"plot-elev.gp\"\; ) | \
        gnuplot > plot-elev.ps
(echo \
    set terminal postscript eps color solid \"Helvetica\" 22 \; \
    load \"plot-elev.gp\"\; ) | \
        gnuplot > plot-elev.eps

