# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: 2017-06-12 11:22:08 d3g096
# -------------------------------------------------------------
# $Id$

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set format x "%d%b\n%H:%M"
set format y "%.0f"
set ylabel 'Discharge, cfs'
set pointsize 0.7
set timestamp
unset key

plot "ts175.out" using 1:4 with lines lt 1


