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
# Last Change: 2019-04-15 14:22:44 d3g096
# -------------------------------------------------------------
# $Id$

set -x
set -e

model=${MODEL-../../../build/mass1_new}

rm -f mass1.cfg
ln -f -s mass1-warmup.cfg mass1.cfg
$model
rm -f mass1.cfg

ln -f -s mass1-Cn=1.0.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    'set terminal postscript eps color solid "Helvetica" 14;' \
    'set title "Advective Diffusion Analytic Solution: Courant Number = 1.0";' \
    'load "plot.gp";' ) | \
        gnuplot > plot-Cn=1.0.eps

ln -f -s mass1-Cn=0.1.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    set terminal postscript eps color solid \"Helvetica\" 14\; \
    set title \"Advective Diffusion Analytic Solution: Courant Number = 0.1\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-Cn=0.1.eps

ln -f -s mass1-Cn=0.01.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    set terminal postscript eps color solid \"Helvetica\" 14\; \
    set title \"Advective Diffusion Analytic Solution: Courant Number = 0.01\"\; \
    load \"plot.gp\"\; ) | \
        gnuplot > plot-Cn=0.01.eps

rm -f mass1.cfg
ln -f -s mass1-warmup-2.cfg mass1.cfg
$model
rm -f mass1.cfg

ln -f -s mass1-Cn=1.0-2.cfg mass1.cfg
$model
rm -f mass1.cfg

(echo \
    set terminal postscript eps color solid \"Helvetica\" 22\; \
    set title \"Advective Diffusion Analytic Solution: Courant Number = 1.0 \(D = 70\)\"\; \
    load \"plot-2.gp\"\; ) | \
        gnuplot > plot-Cn=1.0-2.eps

