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
# Last Change: Thu Apr  1 11:47:46 1999 by William A. Perkins <perk@tophet>
# -------------------------------------------------------------
# $Id$

set -x
set -e

model=../../../Release/mass1_v082

rm -f mass1.cfg
cp mass1-warmup.cfg mass1.cfg
$model
rm -f mass1.cfg

cp mass1-Cn=1.0.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    'set terminal postscript landscape color solid "Helvetica" 14;' \
    'set title "Multiple Segment Advection Diffusion: Courant Number = 1.0";' \
    'load "plot.gp";' ) | \
        gnuplot > plot-Cn=1.0.ps

cp mass1-Cn=0.1.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Multiple Segment Advection Diffusion: Courant Number = 0.1\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-Cn=0.1.ps

cp mass1-Cn=0.01.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Multiple Segment Advection Diffusion: Courant Number = 0.01\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-Cn=0.01.ps

