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
# Last Change: Tue Apr  6 16:01:28 1999 by William A. Perkins <perk@tophet>
# -------------------------------------------------------------
# $Id$

set -x
set -e

model=../../../Release/mass1_v082

rm -f mass1.cfg
cp mass1-warmup.cfg mass1.cfg
$model
rm -f mass1.cfg

cp mass1-n=1.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    'set terminal postscript landscape color solid "Helvetica" 14;' \
    'set title "Variable Flow: 1 Transport Iteration";' \
    'load "plot.gp";' ) | \
        gnuplot > plot-n=1.ps

cp mass1-n=5.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Variable Flow: 5 Transport Iterations\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-n=5.ps

cp mass1-n=10.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Variable Flow: 10 Transport Iterations\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-n=10.ps
