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
# Last Change: Wed Dec  1 08:22:19 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------

set -u

dirs=" \
    normal \
    slopebreak1 \
    slopebreak2 \
    lateral \
    storage \
    MacDonald-1 \
    MacDonald-3 \
    varied \
    varied-pid \
    varied-pool \
"

for d in $dirs; do
    /bin/echo -n "Running $d: "
    if (cd $d; sh runit.sh 2>&1 ) > /dev/null ; then
        /bin/echo complete
    else
        /bin/echo failed
    fi
done