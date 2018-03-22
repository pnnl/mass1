# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: 2018-03-22 12:27:38 d3g096
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color solid "Helvetica,18"

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set format x "%d%b\n%H:%M"
set format y "%.0f"
set ylabel 'Discharge, cfs'
set pointsize 0.7
set timestamp
unset key

plot "ts175.out" using 1:4 with lines lt 1


