#! /bin/sh
# -------------------------------------------------------------
# file: getbc.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March  1, 2001 by William A. Perkins
# Last Change: Wed Jun 13 12:25:26 2001 by William A. Perkins <perk@gehenna.pnl.gov>
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

dams="DWR LWG LGS LMN IHR MCN JDA TDA BON"
dams="DWR"
start="01/01/1998"
end="06/02/2005"

                                # total flows at dams

for d in $dams; do
    srcflag="-w"
    perl $perlinc $massbc $srcflag -Q -O '-30 minutes' -o "$d-Qtotal.dat" $d $start $end
done

#sh plots.sh | gnuplot
