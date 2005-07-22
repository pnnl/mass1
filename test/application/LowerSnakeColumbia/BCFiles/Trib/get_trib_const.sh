#! /bin/sh
# -------------------------------------------------------------
# file: get_trib_const.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 19, 2001 by William A. Perkins
# Last Change: Mon Mar 19 10:09:31 2001 by William A. Perkins <perk@gehenna.pnl.gov>
# -------------------------------------------------------------
set -x

# -------------------------------------------------------------
# Tributaries where we do not have 1977 flows use some constant flow
# -------------------------------------------------------------
names=" \
    Wind        100.0 \
    Sandy      1000.0 \
    Clatskaine  500.0 \
"

set $names
while [ $# -gt 0 ]; do
    name=$1
    flow=$2
    out="$name-Flow.dat"
    cat > $out <<EOF
# Assumed constant flow for the $name tributary
01-01-1900 00:00:00  $flow /
01-01-2500 00:00:00  $flow /
EOF
    shift 2
done
