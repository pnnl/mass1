# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July 13, 1999 by William A. Perkins
# Last Change: 2017-01-20 10:42:37 d3g096
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color solid "Helvetica,18"

set xdata time
set timefmt "%m-%d-%Y %H:%M:%S"

set ytics nomirror
set y2tics nomirror
set ylabel 'Discharge, cfs'
set y2label 'Storage, cubic feet'
set yrange [0:2000]

set pointsize 0.4
set key below

plot 'ts11.flowbc.out' using 1:4 axes x1y1 title 'Inflow' with linespoints lt 1, \
     'ts1149.flowbc.out' using 1:4 axes x1y1 title 'Ouflow' with lines lt 3, \
     'storage.flowbc.out' using 1:3 axes x1y2 title 'Channel Storage' with lines lt 4
     