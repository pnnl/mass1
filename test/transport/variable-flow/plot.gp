# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: Tue Nov 30 13:43:45 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------
# $Id$

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set format y "%.1f"
set format y2 "%.0f"
set ytics nomirror
set y2tics nomirror
set ylabel 'Concentration'
set y2label 'Discharge, cfs'
set style data linespoints
set pointsize 0.5
set timestamp
set key below

plot 'ts11.out' using 1:7 title "Inflow Concentration", \
     'ts11.out' using 1:4 axes x1y2 title "Inflow", \
     'ts175.out' using 1:7 title "Segment 1 Outflow Concentration", \
     'ts175.out' using 1:4 axes x1y2 title "Segment 1 Outflow", \
     'ts275.out' using 1:7 title "Segment 2 Outflow Concentration", \
     'ts275.out' using 1:4 axes x1y2 title "Segment 2 Outflow" with linespoints lt 7

plot 'ts11.out' using 1:8 title "Inflow Concentration", \
     'ts11.out' using 1:4 axes x1y2 title "Inflow", \
     'ts175.out' using 1:8 title "Segment 1 Outflow Concentration", \
     'ts175.out' using 1:4 axes x1y2 title "Segment 1 Outflow", \
     'ts275.out' using 1:8 title "Segment 2 Outflow Concentration", \
     'ts275.out' using 1:4 axes x1y2 title "Segment 2 Outflow" with linespoints lt 7

