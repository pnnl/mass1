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
# Last Change: Thu Aug 12 14:07:51 1999 by William A. Perkins <perk@mack.pnl.gov>
# -------------------------------------------------------------
# $Id$

set -x
set -e

model=../../../Release/mass1_v082

rm -f mass1.cfg
cp mass1-warmup.cfg mass1.cfg
$model
rm -f mass1.cfg

cp mass1-flowbc.cfg mass1.cfg
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



cp mass1-lateral.cfg mass1.cfg
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
