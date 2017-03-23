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
# Last Change: 2017-03-22 14:21:25 d3g096
# -------------------------------------------------------------
# $Id$

set -x
set -e

                                # to trap floating point errors on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../build/mass1}
gnuplot=${GNUPLOT-gnuplot}

cp mass1-warmup.cfg mass1.cfg
$model > /dev/null


case=0

while [ $case -lt 4 ]; do
    case=`expr $case + 1`
    case $case in
        1)
            title="Flow Step Change"
            start="03-04-1997"
            end="03-10-1997"
            code="qstep"
            ;;
        2)
            title="Flow Square Wave"
            start="04-04-1997"
            end="04-10-1997"
            code="square"
            ;;
        3)
            title="Flow Sine Wave"
            start="05-31-1997"
            end="06-04-1997"
            code="sine"
            ;;
        4)
            title="Stage Step Change"
            start="07-04-1997"
            end="07-09-1997"
            code="hstep"
            ;;
        *)
            
    esac
    title="Case $case: $title"

                                # first run the case with a clamped
                                # stage for comparison
    
    sed -e '46s/^/'"$start"/ \
        -e '48s/^/'"$end"/ \
        -e '30s/-with-pid//' \
        mass1-run.cfg > mass1.cfg
    $model > /dev/null
    sed -e 's/@TITLE@/MASS1 with Regular Dam Link\\n'"$title"/ plot.gp | \
        $gnuplot > plot-$code-clamp.ps

                                # then run the case with a PID link
    
    sed -e '46s/^/'"$start"/ \
        -e '48s/^/'"$end"/ \
        mass1-run.cfg > mass1.cfg
    $model  > /dev/null
    sed -e 's/@TITLE@/MASS1 with PID Dam Link\\n'"$title"/ plot.gp | \
        $gnuplot > plot-$code.ps
    
done





