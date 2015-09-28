#! /bin/bash
# -------------------------------------------------------------
# file: run.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 27, 2011 by William A. Perkins
# Last Change: 2015-09-28 14:38:55 d3g096
# -------------------------------------------------------------

# ONLY on FLOPHOUSE!

set -ue

PATH=/usr/local/bin:$PATH
export path

MODEL="/home/d3g096/Projects/MASS1/src/mass1.linux/mass1"
export MODEL

GDFONTPATH=/usr/share/fonts/liberation
export GDFONTPATH

# cd /home/d3g096/Projects/GrantPUD/HanfordReachForecast/forecast/Forecast
exec 2>&1
exec > run.log

python26 ./runmass1.py 2>&1
for i in *.eps; do
    png=`expr "$i" : '\(.*\)\.eps' `.png
    convert -density 144 -background white -flatten \
        $i $png
done
# cp results.txt /projects/hanford_forecast/current/mass1-current.csv
# cp q???.png e???.png tw???.png /projects/hanford_forecast/current

