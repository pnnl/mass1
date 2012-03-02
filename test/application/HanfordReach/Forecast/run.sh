#! /bin/sh
# -------------------------------------------------------------
# file: run.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 27, 2011 by William A. Perkins
# Last Change: Tue Jan 10 09:35:17 2012 by William A. Perkins <d3g096@flophouse>
# -------------------------------------------------------------

# ONLY on FLOPHOUSE!

set -ue

PATH=/usr/local/bin:$PATH
export path

cd /home/d3g096/Projects/GrantPUD/HanfordReachForecast/forecast/Forecast
exec 2>&1
exec > run.log
MODEL="/home/d3g096/Projects/MASS1/src/mass1.linux/mass1"
export MODEL
python26 ./runmass1.py 2>&1
cp results.txt /projects/hanford_forecast/current/mass1-current.csv
cp q???.png e???.png tw???.png /projects/hanford_forecast/current

