#! /bin/sh
# -------------------------------------------------------------
# file: runit-all.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 11, 1998 by William A. Perkins
# Last Change: Fri Jul 15 13:40:15 2005 by William A. Perkins <perk@McPerk.pnl.gov>
# -------------------------------------------------------------
set -x
set -ex

                                # to trap floating point errors on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

G95_FPU_DIVZERO=Yes
G95_FPU_INVALID=Yes
G95_FPU_OVERFLOW=Yes
G95_MEM_SEGMENTS=0
export G95_FPU_DIVZERO
export G95_FPU_INVALID
export G95_FPU_OVERFLOW
export G95_MEM_SEGMENTS

model=${MODEL-../../../../mass1}

for cfg in mass1-warmup-*.cfg; do
    rm -f mass1.cfg
    cp $cfg mass1.cfg
    $model
done


