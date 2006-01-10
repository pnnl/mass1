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
# Last Change: Mon Jan  9 07:02:24 2006 by William A. Perkins <perk@McPerk.pnl.gov>
# -------------------------------------------------------------
# $Id$

set -x
set -e

                                # to trap floating point errors on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../mass1}

                                # the first set is done at 100,000 cfs
                                # and a normal depth of 20.0 feet

cp inflow-bc.base inflow-bc.dat

for d in 15.0 20.0 30.0 50.0; do
    cat > tailwater-bc.dat <<EOF
# 
01-01-1900  00:00:00   $d /
12-31-2100  23:59:59   $d /
EOF

    cp mass1-warmup.cfg mass1.cfg
    $model > /dev/null

    cp mass1-run.cfg mass1.cfg
    $model > /dev/null

    sed -e 's/@D@/'"$d"/ -e 's/@NORMAL@/20/' \
        -e 's/@MIN@/70000/' -e 's/@MAX@/130000/' \
        plot.gp | gnuplot > plot-shallow-$d.eps
 
done

                                # the second set has a normal depth of
                                # 100.0 feet, so the flow is 500,000
                                # cfs

gawk 'NR == 1 { print; next; } { $3 = $3 + 400000; print $1, $2, $3, $4; }' inflow-bc.base > inflow-bc.dat

for d in 50.0 100.0 150.0 250.0; do
    cat > tailwater-bc.dat <<EOF
# 
01-01-1900  00:00:00   $d /
12-31-2100  23:59:59   $d /
EOF

    cp mass1-warmup.cfg mass1.cfg
    $model > /dev/null

    cp mass1-run.cfg mass1.cfg
    $model > /dev/null

    sed -e 's/@D@/'"$d"/ -e 's/@NORMAL@/100/' \
        -e 's/@MIN@/470000/' -e 's/@MAX@/530000/' \
        plot.gp | gnuplot > plot-deep-$d.eps
    
done
