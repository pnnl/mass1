# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: 2017-01-16 08:37:26 d3g096
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

plot "<head -46 profile1.out" using ($4*0.3048):($5*0.3048) title "Initial Conditions" with linespoints lt 1, \
     "<tail -46 profile1.out" using ($4*0.3048):($5*0.3048) title "Steady State" with points lt 7, \
     "solution.dat" using ($1*0.3048):($5*0.3048) title "Analytic Solution" with lines lt 3, \
     "<tail -45 profile1.out" using ($4*0.3048):($13*0.3048) title "Bottom" with lines lt 7

     
     
