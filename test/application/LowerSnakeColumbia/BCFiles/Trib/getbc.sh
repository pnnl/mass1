#! /bin/sh
# -------------------------------------------------------------
# file: getbc.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February 14, 2001 by William A. Perkins
# Last Change: Wed Apr  4 12:12:56 2001 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
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

start='01/01/1998'
end='06/02/2005'

                                  # gaged tributary inflows (hourly)

list=" \
    Anatone     13334300 \
"

set $list

while [ $# -gt 0 ]; do
    trib=$1
    gage=$2
    perl $perlinc $massbc -g -h -Q -O '30 minutes' -o "$trib-Flow.dat" $gage $start $end
    shift 2
done

                                # gaged tributary inflows (daily)

# sh get_trib_flow.sh $start $end

                                # guessed flows for ungaged inflows

# sh get_trib_const.sh 
