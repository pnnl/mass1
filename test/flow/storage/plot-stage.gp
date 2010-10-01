# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July 13, 1999 by William A. Perkins
# Last Change: Thu Sep 30 08:06:17 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------
# $Id$

set xdata time
set timefmt "%m-%d-%Y %H:%M:%S"

set ylabel 'Elevation, feet'
# set yrange [0:2000]

set pointsize 0.4
# set key below

plot 'ts1149-@CASE@.out' using 1:3 axes x1y1 title "Downstream Stage" with linespoints lt 1
     