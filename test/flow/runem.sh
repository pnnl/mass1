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
# Last Change: 2018-08-21 12:42:23 d3g096
# -------------------------------------------------------------

set -u

dirs=" \
    normal \
    normal-hydro \
    slopebreak1 \
    slopebreak2 \
    lateral \
    storage \
    MacDonald-1 \
    MacDonald-3 \
    MacDonald-4 \
    dam-break \
    drain \
    fill \
    fill-us \
    lateral-fill \
    hlink-normal \
    hlink-lateral \
    hlink-slopebreak1 \
    hlink-trib \
    hlink-upstream \
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
