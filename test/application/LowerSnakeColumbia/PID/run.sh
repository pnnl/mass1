#! /bin/sh
# -------------------------------------------------------------
# file: runpid.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 11, 1998 by William A. Perkins
# Last Change: Fri Jul 27 10:35:58 2001 by William A. Perkins <perk@gehenna.pnl.gov>
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

rm -f mass1.cfg
cp mass1-run.cfg mass1.cfg
$model

(cd compare; sh compare.sh)
