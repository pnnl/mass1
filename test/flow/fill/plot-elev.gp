# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: 2018-03-22 12:20:56 d3g096
# -------------------------------------------------------------
# $Id$

set term post enh eps color dashed "Helvetica" 

set samples 2000
set format x "%.1f"
set xrange [0:12000]
set xlabel 'Longitudinal Distance, ft'
set format y "%.1f"
set ylabel 'Elevation, ft'
set yrange [0:6.5]
set pointsize 0.5
# set timestamp
set key below

plot "profile1.out" using (10656 - $4):5 notitle with lines lt 9, \
     "<head -n 105 profile1.out" using (10656 - $4):13 title "Thalweg" with lines lt 1 lc 7, \
     "<head -n 105 profile1.out" using (10656 - $4):5 title 'Initial Conditions' with points lt 1 lc 1, \
     "<tail -n 105 profile1.out" using (10656 - $4):5 title 'Final Conditions' with points lt 7 lc 3
     


