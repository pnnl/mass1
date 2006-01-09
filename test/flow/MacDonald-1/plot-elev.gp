# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: Sun Jan  8 09:50:49 2006 by William A. Perkins <perk@mcperktop.local>
# -------------------------------------------------------------
# $Id$

set auto
set xlabel "Longitudinal Distance, m"
set ylabel "Elevation, m"
set format y "%.1f"
set pointsize 0.5
set key below

plot "<head -106 profile1.out" using ($4*0.3048):($5*0.3048) title "Initial Conditions" with linespoints 1, \
     "<tail -106 profile1.out" using ($4*0.3048):($5*0.3048) title "Steady State" with points 7, \
     "point.dat" using ($3*0.3048):(($5+$10)*0.3048) title "Analytic Solution" with lines 3, \
     "point.dat" using ($3*0.3048):($5*0.3048) title "Bottom" with lines 7
     
     
     
