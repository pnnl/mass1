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
# Last Change: Mon Feb 21 10:28:53 2011 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------
set -ue

# cd /home/d3g096/Projects/GrantPUD/HanfordReachForecast/test/Forecast
exec > run.log
MODEL="../../../../mass1"
export MODEL
/usr/local/python2.5/bin/python ./runmass1.py 2>&1
