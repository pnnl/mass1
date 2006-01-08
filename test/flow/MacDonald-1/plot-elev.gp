# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: Sat Jan  7 20:43:20 2006 by William A. Perkins <perk@mcperktop.local>
# -------------------------------------------------------------
# $Id$

set auto
set xlabel "Longitudinal Distance, m"
set ylabel "Elevation, m"
set format y "%.1f"
set pointsize 0.5
set key left

plot "<awk '/Date: 04-06-1996  Time: 00:00:00/,/END/' profile1.out" using ($4*0.3048):($5*0.3048) title "Simulated" with points 7, \
     "point.dat" using ($3*0.3048):(($5+$10)*0.3048) title "Analytic Solution" with lines 1, \
     "point.dat" using ($3*0.3048):($5*0.3048) title "Bottom" with lines 7
     
     
     
