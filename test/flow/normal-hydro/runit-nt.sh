#! /bin/sh
# -------------------------------------------------------------
# file: runit-nt.sh
# intended to be run with Cygnus Win 32 BASH on NT
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 11, 1998 by William A. Perkins
# Last Change: Tue Mar  7 21:45:01 2000 by William A. Perkins <perk@mack.pnl.gov>
# -------------------------------------------------------------
# $Id$

set -x
set -e

model=../../../Release/mass1_v082

$model

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Normal Flow: 1000 cfs\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot.ps
(echo \
    set terminal postscript eps color solid \"Helvetica\" 14 \; \
    set title \"Normal Flow: 1000 cfs\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot.ps
