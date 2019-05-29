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
# Last Change: 2019-05-10 07:47:27 d3g096
# -------------------------------------------------------------
# $Id$

set -x
set -e

                                # to trap floating point errors on SGI

model=${MODEL-../../../build/mass1_new}

touch nolatinflow

gnuplot bcplot.gp

rm -f mass1.cfg
ln -f -s mass1-warmup.cfg mass1.cfg
$model
rm -f mass1.cfg

ln -f -s mass1-Cn=0.1.cfg mass1.cfg
$model
cp profile1.out profile1-Cn=0.1.out
cp ts11.out ts11-Cn=0.1.out
cp ts1149.out ts1149-Cn=0.1.out
rm -f mass1.cfg


(echo \
     pfile = \"profile1-Cn=0.1.out\"\; \
     set terminal postscript landscape color solid \"Helvetica\" 14\; \
     set title \"Advection: Courant Number = 0.1\"\; \
     load \"plot.gp\"\; ) | \
    gnuplot > plot-Cn=0.1.ps
(echo \
     pfile = \"profile1-Cn=0.1.out\"\; \
     set terminal postscript eps color solid \"Helvetica\" 22\; \
     set title \"Advection: Courant Number = 0.1\"\; \
     set size 1,1.25\; load \"plot.gp\"\; ) | \
    gnuplot > plot-Cn=0.1.eps

(echo \
     'case = "Cn=0.1";' \
     set terminal postscript landscape color solid \"Helvetica\" 14\; \
     set title \"Advection: Courant Number = 0.1\"\; \
     load \"mass-plot.gp\"\; ) | \
    gnuplot > mass-plot-Cn=0.1.ps
(echo \
     'case = "Cn=0.1";' \
     set terminal postscript eps color solid \"Helvetica\" 22\; \
     set title \"Advection: Courant Number = 0.1\"\; \
     load \"mass-plot.gp\"\; ) | \
    gnuplot > mass-plot-Cn=0.1.eps

ln -f -s mass1-Cn=1.0.cfg mass1.cfg
$model
cp profile1.out profile1-Cn=1.0.out
cp ts11.out ts11-Cn=1.0.out
cp ts1149.out ts1149-Cn=1.0.out
rm -f mass1.cfg

(echo \
     pfile = \"profile1-Cn=1.0.out\"\; \
     'set terminal postscript landscape color solid "Helvetica" 14;' \
     'set title "Advection: Courant Number = 1.0";' \
     'load "plot.gp";' ) | \
    gnuplot > plot-Cn=1.0.ps
(echo \
     pfile = \"profile1-Cn=1.0.out\"\; \
     'set terminal postscript eps color solid "Helvetica" 22;' \
     'set title "Advection: Courant Number = 1.0";' \
     'set size 1,1.25; load "plot.gp";' ) | \
    gnuplot > plot-Cn=1.0.eps

(echo \
     'case = "Cn=1.0";' \
     'set terminal postscript landscape color solid "Helvetica" 14;' \
     'set title "Advection: Courant Number = 1.0";' \
     'load "mass-plot.gp";' ) | \
    gnuplot > mass-plot-Cn=1.0.ps
(echo \
     'case = "Cn=1.0";' \
     'set terminal postscript eps color solid "Helvetica" 22;' \
     'set title "Advection: Courant Number = 1.0";' \
     'set size 1,1.25; load "mass-plot.gp";' ) | \
    gnuplot > mass-plot-Cn=1.0.eps

