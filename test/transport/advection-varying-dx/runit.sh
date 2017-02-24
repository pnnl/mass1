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
# Last Change: Wed Sep 29 14:14:28 2010 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------
# $Id$

set -x
set -e

                                # to trap floating point errors on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../build/mass1}

ln -f -s mass1-1.cfg mass1.cfg
$model
rm -f mass1.cfg
cp ts131.out tsend.out

                                # profile plots of concentration at
                                # various time steps

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Advection: Variable Spacing, Case 1\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-1.ps
(echo \
    set terminal postscript eps color solid \"Helvetica\" 14\; \
    set title \"Advection: Variable Spacing, Case 1\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-1.eps

                                # time series plot of inflow and
                                # outflow mass

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Advection: Variable Spacing, Case 1\"\; \
    load \"mass-plot.gp\"\; ) | \
        gnuplot > mass-plot-1.ps
(echo \
    set terminal postscript eps color solid \"Helvetica\" 14\; \
    set title \"Advection: Variable Spacing, Case 1\"\; \
    load \"mass-plot.gp\"\; ) | \
        gnuplot > mass-plot-1.eps

                                # plot of spacing and courant number

tail -n 31 profile1.out | \
    gawk '\
        BEGIN { lastx = -1; } \
        lastx >= 0 { \
            x = $4; \
            v = $7; \
            print (x + lastx) /2.0, x - lastx, v*36/(x - lastx); } \
        {lastx = $4}' > courant.dat

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Advection: Variable Spacing, Case 1\"\; \
    load \"Cn-plot.gp\"\; ) | \
        gnuplot > Cn-plot-1.ps
(echo \
    set terminal postscript eps color solid \"Helvetica\" 14\; \
    set title \"Advection: Variable Spacing, Case 1\"\; \
    load \"Cn-plot.gp\"\; ) | \
        gnuplot > Cn-plot-1.eps



ln -f -s mass1-2.cfg mass1.cfg
$model
rm -f mass1.cfg

cp ts148.out tsend.out

(echo \
    'set terminal postscript landscape color solid "Helvetica" 14;' \
    'set title "Advection: Variable Spacing, Case 2";' \
    'load "plot.gp";' ) | \
        gnuplot > plot-2.ps
(echo \
    'set terminal postscript eps color solid "Helvetica" 14;' \
    'set title "Advection: Variable Spacing, Case 2";' \
    'load "plot.gp";' ) | \
        gnuplot > plot-2.eps

(echo \
    'set terminal postscript landscape color solid "Helvetica" 14;' \
    'set title "Advection: Variable Spacing, Case 2";' \
    'load "mass-plot.gp";' ) | \
        gnuplot > mass-plot-2.ps
(echo \
    'set terminal postscript eps color solid "Helvetica" 14;' \
    'set title "Advection: Variable Spacing, Case 2";' \
    'load "mass-plot.gp";' ) | \
        gnuplot > mass-plot-2.eps


                                # plot of spacing and courant number

tail -n 48 profile1.out | \
    gawk '\
        BEGIN { lastx = -1; } \
        lastx >= 0 { \
            x = $4; \
            v = $7; \
            print (x + lastx) /2.0, x - lastx, v*36/(x - lastx); } \
        {lastx = $4}' > courant.dat

(echo \
    set terminal postscript landscape color solid \"Helvetica\" 14\; \
    set title \"Advection: Variable Spacing, Case 2\"\; \
    load \"Cn-plot.gp\"\; ) | \
        gnuplot > Cn-plot-2.ps
(echo \
    set terminal postscript eps color solid \"Helvetica\" 14\; \
    set title \"Advection: Variable Spacing, Case 2\"\; \
    load \"Cn-plot.gp\"\; ) | \
        gnuplot > Cn-plot-2.eps




