# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: Thu Sep 30 07:14:36 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------
# $Id$

set auto
set xlabel "Longitudinal Distance, m"
set ylabel "Elevation, m"
set format y "%.1f"
set pointsize 0.5
set key below

plot "<head -106 profile1.out" using ($4*0.3048):($5*0.3048) title "Initial Conditions" with linespoints lt 1, \
     "<tail -106 profile1.out" using ($4*0.3048):($5*0.3048) title "Steady State" with points lt 7, \
     "point.dat" using ($3*0.3048):(($5+$10)*0.3048) title "Analytic Solution" with lines lt 3, \
     "point.dat" using ($3*0.3048):($5*0.3048) title "Bottom" with lines lt 7
     
     
     
