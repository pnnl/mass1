# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July 13, 1999 by William A. Perkins
# Last Change: 2017-01-20 09:41:04 d3g096
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color solid "Helvetica,18"

set xdata time
set timefmt "%m-%d-%Y %H:%M:%S"

set ylabel 'Elevation, feet'
# set yrange [0:2000]

set pointsize 0.4
# set key below

plot 'ts1149-flowbc.out' using 1:3 axes x1y1 title "Downstream Stage" with linespoints lt 1
     