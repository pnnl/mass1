#! /bin/sh
# -------------------------------------------------------------
# file: runit.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 21, 2001 by William A. Perkins
# Last Change: Wed Dec  1 08:50:24 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------

# This script runs all specific cases for the Wanapum pool test
# application.  These cases test the PID process control link
# representing dams, and compare to standard clamped internal boundary
# links.  Each of the cases use observed from July 2000.  

pwd=`pwd`
model=${MODEL-$pwd/../../../mass1}

subdir=" \
    Rampdown \
    Warmup \
    StageBC \
    FlowBC \
    StagePID-1 \
    StagePID-2 \
    FlowPID
"

for dir in $subdir; do
    cd $dir
    $model > /dev/null

    case $dir in
        StageBC)
            gnuplot < ../BaseFiles/flow-plot.gp
            ;;
        FlowBC)
            gnuplot < ../BaseFiles/stage-plot.gp
            ;;
        *PID*)
            gnuplot < ../BaseFiles/flow-plot.gp
            gnuplot < ../BaseFiles/stage-plot.gp
            ;;
        *)
            
    esac
    cd ..
done

