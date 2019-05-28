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
# Last Change: Fri Jan  6 14:35:46 2006 by William A. Perkins <perk@McPerk.pnl.gov>
# -------------------------------------------------------------
# $Id$

set -x
set -e

                                # to trap floating point errors on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../build/mass1_new}

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
    'set terminal postscript eps color solid "Helvetica" 22;' \
    'set title "Multiple Segment Advection: Courant Number = 1.0";' \
    'set size 1, 1.25; load "plot.gp";' ) | \
        gnuplot > plot-Cn=1.0.eps

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Advection: Courant Number = 0.1\"\; \
    load \"mass-plot.gp\"\; ) | \
        gnuplot > mass-plot-Cn=0.1.ps
(echo \
    set terminal postscript eps color solid \"Helvetica\" 22\; \
    set title \"Advection: Courant Number = 0.1\"\; \
    set size 1, 1.25\; load \"mass-plot.gp\"\; ) | \
        gnuplot > mass-plot-Cn=0.1.eps

ln -f -s mass1-Cn=0.1.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Multiple Segment Advection: Courant Number = 0.1\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-Cn=0.1.ps
(echo \
    set terminal postscript eps color solid \"Helvetica\" 22\; \
    set title \"Multiple Segment Advection: Courant Number = 0.1\"\; \
    set size 1, 1.25\; load \"plot.gp\"\; ) | \
        gnuplot > plot-Cn=0.1.eps

(echo \
    'set terminal postscript landscape color solid "Helvetica" 14;' \
    'set title "Advection: Courant Number = 1.0";' \
    'load "mass-plot.gp";' ) | \
        gnuplot > mass-plot-Cn=1.0.ps
(echo \
    'set terminal postscript eps color solid "Helvetica" 22;' \
    'set title "Advection: Courant Number = 1.0";' \
    'set size 1, 1.25; load "mass-plot.gp";' ) | \
        gnuplot > mass-plot-Cn=1.0.eps

