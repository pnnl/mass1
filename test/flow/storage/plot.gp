# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July 13, 1999 by William A. Perkins
# Last Change: Tue Dec 14 22:40:18 1999 by William A. Perkins <perk@mack.pnl.gov>
# -------------------------------------------------------------
# $Id$

set xdata time
set timefmt "%m-%d-%Y %H:%M:%S"

set ytics nomirror
set y2tics nomirror
set ylabel 'Discharge, cfs'
set y2label 'Storage, cubic feet'
set yrange [0:2000]

set pointsize 0.4
set key below

plot 'ts11-@CASE@.out' using 1:4 axes x1y1 title 'Inflow' with linespoints 2, \
     'ts1149-@CASE@.out' using 1:4 axes x1y1 title 'Ouflow' with lines 3, \
     'storage-@CASE@.out' using 1:3 axes x1y2 title 'Channel Storage' with lines 4
     