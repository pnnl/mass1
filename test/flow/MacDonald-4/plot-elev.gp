# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: 2017-05-16 10:47:05 d3g096
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color solid "Helvetica,18" 

set xlabel "Longitudinal Distance, m"
set ylabel "Elevation, m"
set format y "%.1f"
set pointsize 0.5
set key below
set key font ",16"
set xrange [0:1000]

set ytics nomirror
set y2tics nomirror
set y2label "Froude Number"
set format y2 "%.1f"

plot "<head -46 profile1.out" using ($4*0.3048):($5*0.3048) axes x1y1 title "Initial Conditions" with linespoints lt 1, \
     "<tail -46 profile1.out" using ($4*0.3048):($5*0.3048) axes x1y1 title "Steady State" with points lt 7, \
     "<tail -46 profile1.out" using ($4*0.3048):($17) axes x1y2 title "Froude Number" with lines lt 5, \
     1.0 axes x1y2 notitle with lines lt 0, \
     "solution.dat" using ($1*0.3048):($5*0.3048)axes x1y1  title "Analytic Solution" with lines lt 3, \
     "<tail -45 profile1.out" using ($4*0.3048):($13*0.3048) axes x1y1 title "Bottom" with lines lt 7

     
     
