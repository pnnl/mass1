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
# Last Change: 2017-01-16 08:24:57 d3g096
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
    MacDonald-4 \
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
