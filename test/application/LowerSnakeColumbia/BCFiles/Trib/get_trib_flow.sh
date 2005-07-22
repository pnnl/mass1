#! /bin/sh
# -------------------------------------------------------------
# file: get_trib_flow.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 19, 2001 by William A. Perkins
# Last Change: Fri Jul 15 12:13:30 2005 by William A. Perkins <perk@McPerk.pnl.gov>
# -------------------------------------------------------------
set -x

program=`basename $0`

dbdir=${DO9DBDIR-/home/perk/do9/database}
massbc="$dbdir/massbc.pl"
perlinc="-I$dbdir"

if [ -z "$PGHOST" ]; then
    PGHOST=leechong
    export PGHOST
fi

if [ -z "$PGDATABASE" ]; then
    PGDATABASE=columbia
    export PGDATABASE
fi

start="$1"
end="$2"

if [ -z "$1" -o -z "$2" ]; then
    echo usage: $program start end
    exit 3;
fi

# -------------------------------------------------------------
# Tributary and Boundary Flow where we have 1977 data
# -------------------------------------------------------------

gages=" \
    12510500    Yakima \
    13340000    Orofino \
    13351000    Palouse \
    13344500    Tucannon \
    14018500    WallaWalla \
    14033500    Umatilla \
    14048000    JohnDay \
    14103000    Deschutes \
    14113000    Klickitat \
    14120000    Hood \
    14123500    WhiteSalmon \
    14211720    Willamette \
    14220500    Lewis \
    14243000    Cowlitz"

set $gages
while [ $# -gt 0 ]; do
    gage=$1
    name=$2
    out="$name-Flow.dat"
    perl $perlinc $massbc -g -Q -o "$out" $gage $start $end
    shift 2;
done

