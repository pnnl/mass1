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
# Last Change: 2017-03-22 14:21:40 d3g096
# -------------------------------------------------------------
# $Id$

set -x
set -e

                                # to trap floating point errors on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../build/mass1}

                                # downstream stage boundary condition

cp mass1-run.cfg mass1.cfg
$model
sed -e 's/@TIMESTEP@/1 hour/' flowplot.gp | gnuplot > flowplot-1.00hr.ps

sed -e '50s/1.00/0.1/'  -e '51s/^1/10/' mass1-run.cfg > mass1.cfg
$model
sed -e 's/@TIMESTEP@/6 minutes/' flowplot.gp | gnuplot > flowplot-0.10hr.ps

sed -e '50s/1.00/0.05/' -e '51s/^1/20/'  mass1-run.cfg > mass1.cfg
$model
sed -e 's/@TIMESTEP@/3 minutes/' flowplot.gp | gnuplot > flowplot-0.05hr.ps

                                # downstream flow boundary condition

sed -e '13s/^0/1/' -e '22s/^0/1/' \
    -e '33s/link-bc-files.dat/link-bc-dsflow.dat/' \
    mass1-run.cfg > mass1.cfg
$model
sed -e 's/@TIMESTEP@/1 hour/' flowplot.gp | gnuplot > flowplot-1.00hr-qbc.ps

sed -e '13s/^0/1/' -e '22s/^0/1/' \
    -e '33s/link-bc-files.dat/link-bc-dsflow.dat/' \
    -e '50s/1.00/0.1/'  -e '51s/^1/10/' mass1-run.cfg > mass1.cfg
$model
sed -e 's/@TIMESTEP@/6 minutes/' flowplot.gp | gnuplot > flowplot-0.10hr-qbc.ps

sed -e '13s/^0/1/' -e '22s/^0/1/' \
    -e '33s/link-bc-files.dat/link-bc-dsflow.dat/' \
    -e '50s/1.00/0.05/' -e '51s/^1/20/'  mass1-run.cfg > mass1.cfg
$model
sed -e 's/@TIMESTEP@/3 minutes/' flowplot.gp | gnuplot > flowplot-0.05hr-qbc.ps

