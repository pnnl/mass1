#! /bin/sh
# -------------------------------------------------------------
# file: runem.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December  1, 2010 by William A. Perkins
# Last Change: Wed Dec  1 08:42:37 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------

set -u

dirs=" \
    advection-1-segment \
    advection-2-segment \
    advection-diffusion-1-segment \
    advection-diffusion-2-segment \
    advection-varying-dx \
    lateral-inflow \
    variable-flow \
"

for d in $dirs; do
    /bin/echo -n "Running $d: "
    (cd $d; sh runit.sh 2>&1 ) > /dev/null 
    if [ "$?" -eq "0" ]; then
        /bin/echo complete
    else
        /bin/echo failed
    fi
done