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
# Last Change: Thu Mar  1 13:35:31 2001 by William A. Perkins <perk@gehenna.pnl.gov>
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

dams="BON TDA JDA MCN IHR LMN LGS LWG"
start="01/01/1998"
end="06/02/2005"

                                # dam forebay stage

for d in $dams; do
    perl $perlinc $massbc -F -w -0 -o "$d-FBE.dat" $d $start $end
done

                                # tidal stage

perl $perlinc  $massbc -g -h -S -0 -o Tidal-Stage.dat 9439040 $start $end
