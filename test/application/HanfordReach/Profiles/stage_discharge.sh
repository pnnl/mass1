#! /bin/sh
# -------------------------------------------------------------
# file: stage_discharge.sh
#
# Generate data and plots of stage/discharge at 10km intervals in the
# Hanford Reacm
#
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July 23, 2012 by William A. Perkins
# Last Change: 2014-12-29 15:03:38 d3g096
# -------------------------------------------------------------

set -xue

python=python
python=python26
extract="../../../../scripts/profile_extract.py"
profile="profile1.out"

kmstart=540
kmend=635
kmstep=5

km="$kmstart"

while [ "$km" -le "$kmend" ]; do
    rm=`echo "${km}*0.621371" | bc `
    "$python" "$extract" --river-mile="$rm" "$profile" | \
        awk 'NR > 1 { print; } {next;}' > "stage_discharge_${km}.dat"
    sed -e "s/@KM@/$km/g" stage_discharge.gp | gnuplot > "stage_discharge_${km}.eps"
    convert -density 200 "stage_discharge_${km}.eps" "stage_discharge_${km}.png"
    km=`expr "$km" + "$kmstep" `
done

