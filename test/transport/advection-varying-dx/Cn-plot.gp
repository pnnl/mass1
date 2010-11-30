# -------------------------------------------------------------
# file: Cn-plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 16, 2000 by William A. Perkins
# Last Change: Tue Nov 30 13:37:33 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------
# $Id$

set xlabel 'Distance along Channel, feet'
set ylabel 'Cross Section Spacing, feet'
set y2label 'Courant Number'
set y2range [0:*]
set ytics nomirror
set y2tics nomirror

set pointsize 0.5

plot 'courant.dat' using (10656 - $1):2 axes x1y1 title 'Spacing' with linespoints lt 1, \
     'courant.dat' using (10656 - $1):3 axes x1y2 title 'Courant Number' with linespoints lt 7