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
# Last Change: Tue Jul 19 11:21:36 2005 by William A. Perkins <perk@McPerk.pnl.gov>
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

dams="BON TDA JDA MCN PRD IHR LMN LGS LWG"
start="01/01/1998"
end="06/02/2005"

                                # dam tailwater stage

for d in $dams; do
    perl $perlinc $massbc -T -0 -o "$d-TWE.dat" $d $start $end
done
