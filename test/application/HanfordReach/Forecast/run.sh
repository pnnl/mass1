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
# Last Change: Wed Mar 23 11:19:45 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------
set -ue

PATH=/usr/local/bin:$PATH
export path

cd /home/d3g096/Projects/GrantPUD/HanfordReachForecast/test/Forecast
exec 2>&1
exec > run.log
MODEL="../../../../mass1"
export MODEL
/usr/local/python2.5/bin/python ./runmass1.py 2>&1
cp results.txt /projects/hanford_forecast/current/mass1-current.csv
cp q???.png e???.png /projects/hanford_forecast/current

