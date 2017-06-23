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
# Last Change: Mon Jan  9 07:01:55 2006 by William A. Perkins <perk@McPerk.pnl.gov>
# -------------------------------------------------------------
# $Id$

set -x
set -e

                                # to trap floating point errors on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../build/mass1}

                                # run warmup with stage BC's

rm -f mass1.cfg
ln -f -s mass1-warmup.cfg mass1.cfg
$model

                                # run inflow/outflow pulse with flow BC's

rm -f mass1.cfg
ln -f -s mass1-flowbc.cfg mass1.cfg
$model
rm -f mass1.cfg

                                # save output and compute channel storage

mv profile1.out profile1-flowbc.out
mv ts11.out ts11-flowbc.out
mv ts1149.out ts1149-flowbc.out
gawk -f profile-storage.gawk profile1-flowbc.out > storage-flowbc.out

( \
    echo \
        set terminal postscript landscape color solid \"Helvetica\" 14\; \
        set title \"MASS1 Storage Test \(Inflow/Outflow Pulse\)\"\; ; \
    sed -e 's/@CASE@/flowbc/' plot.gp \
) | gnuplot > plot-flowbc.ps
( \
    echo \
        set terminal postscript eps color solid \"Helvetica\" 22\; ; \
    sed -e 's/@CASE@/flowbc/' plot.gp \
) | gnuplot > plot-flowbc.eps

( \
    echo \
        set terminal postscript landscape color solid \"Helvetica\" 14\; \
        set title \"MASS1 Storage Test \(Inflow/Outflow Pulse\)\"\; ; \
    sed -e 's/@CASE@/flowbc/' plot-stage.gp \
) | gnuplot > plot-flowbc-stage.ps
( \
    echo \
        set terminal postscript eps color solid \"Helvetica\" 22\; ; \
    sed -e 's/@CASE@/flowbc/' plot-stage.gp \
) | gnuplot > plot-flowbc-stage.eps


                                # run lateral inflow/outflow pulse with flow BC's

rm -f mass1.cfg
ln -f -s mass1-lateral.cfg mass1.cfg
$model
rm -f mass1.cfg

                                # save output and compute channel storage

mv profile1.out profile1-lateral.out
mv ts11.out ts11-lateral.out
mv ts1149.out ts1149-lateral.out
gawk -f profile-storage.gawk profile1-lateral.out > storage-lateral.out

( \
    echo \
        set terminal postscript landscape color solid \"Helvetica\" 14\; \
        set title \"MASS1 Storage Test \(Lateral Inflow/Outflow Pulse\)\"\; ; \
    sed -e 's/@CASE@/lateral/' plot.gp \
) | gnuplot > plot-lateral.ps
( \
    echo \
        set terminal postscript eps color solid \"Helvetica\" 22\; ; \
    sed -e 's/@CASE@/lateral/' plot.gp \
) | gnuplot > plot-lateral.eps

( \
    echo \
        set terminal postscript landscape color solid \"Helvetica\" 14\; \
        set title \"MASS1 Storage Test \(Inflow/Outflow Pulse\)\"\; ; \
    sed -e 's/@CASE@/lateral/' plot-stage.gp \
) | gnuplot > plot-lateral-stage.ps
( \
    echo \
        set terminal postscript eps color solid \"Helvetica\" 22\; ; \
    sed -e 's/@CASE@/lateral/' plot-stage.gp \
) | gnuplot > plot-lateral-stage.eps
