# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: 2019-01-18 10:41:46 d3g096
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color solid "Helvetica,18"

set samples 2000
set format x "%.0f"
set xrange [0:300]
set xlabel 'Distance, m'
set format y "%.1f"
set ylabel 'Elevation, m'
set yrange [0:*]
set pointsize 0.5
# set timestamp
set key left

plot "<tail -n 200 profile1.out" using ($4*0.3048):($5*0.3048) title "Water Surface" w l, \
     '' using ($4*0.3048):($13*0.3048) title "Thalweg" with lines ls 7



