# -------------------------------------------------------------
# file: mass-plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 16, 2000 by William A. Perkins
# Last Change: Tue Nov 30 13:41:31 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------
# $Id$

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set style data lines

set format x '%H:%M'
set xlabel 'Simulation Time'

set format y '%.1e'
set ylabel 'Contaminant Mass'

set key bottom

plot 7200000 notitle with lines lt 0, \
     "<awk '$0 !~ /^#/ {sum=sum+$4*$8*144; print $1, $2, sum; }' ts11.out" using 1:3 title 'Inflow' with lines lt 1, \
     "<awk '$0 !~ /^#/ {sum=sum+$4*$8*144; print $1, $2, sum; }' tsend.out" using 1:3 title 'Outflow' with lines lt 3
     