#! /bin/sh
# -------------------------------------------------------------
# file: runit.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December  1, 2010 by William A. Perkins
# Last Change: Wed Dec  1 11:07:12 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------
# $Id$ Battelle PNL

set -xue

prdqfile="BCFiles/PRD-Qtotal.dat"
prdstart=`head -n 2 BCFiles/PRD-Qtotal.dat | tail -n 1`
startdate=`echo "$prdstart" | awk '{print $1}' `
starttime=`echo "$prdstart" | awk '{print $2}' ` 
prdend=`tail -n 2 BCFiles/PRD-Qtotal.dat | head -n 1`
prdend=`echo $prdend | perl date+.pl `
enddate=`echo "$prdend" | awk '{print $1}' `
endtime=`echo "$prdend" | awk '{print $2}' ` 

sed -e "s/@STARTDATE@/$startdate/g" \
    -e "s/@STARTTIME@/$starttime/g" \
    -e "s/@ENDDATE@/$enddate/g" \
    -e "s/@ENDTIME@/$endtime/g" \
    mass1.cfg.base > mass1.cfg

