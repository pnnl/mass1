# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: 2019-01-18 11:25:42 d3g096
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color solid "Helvetica,18"

set samples 2000
set format x "%.0f"
set xrange [0:300]
set xlabel 'Distance, m'
set format y "%.1f"
set ylabel 'Depth, m'
set yrange [0:*]
set ytics nomirror
set y2label 'Froude Number'
set y2range [0:*]
set y2tics nomirror
set pointsize 0.5
# set timestamp
set key left

plot "<tail -n 200 profile1.out" using ($4*0.3048):($8*0.3048) axes x1y1 title "Depth" with lines ls 1, \
     "" using ($4*0.3048):17 axes x1y2 title "Froude Number" with lines ls 3

