# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July 13, 1999 by William A. Perkins
# Last Change: Wed Mar  8 11:01:50 2000 by William A. Perkins <perk@mack.pnl.gov>
# -------------------------------------------------------------
# $Id$

set xdata time
set timefmt "%m-%d-%Y %H:%M:%S"

set ylabel 'Elevation, feet'
# set yrange [0:2000]

set pointsize 0.4
# set key below

plot 'ts1149-@CASE@.out' using 1:3 axes x1y1 title "Downstream Stage" with linespoints 1
     