# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: 2017-01-13 11:13:39 d3g096
# -------------------------------------------------------------
# $Id$


set samples 2000
set format x "%.1f"
set xrange [0:12000]
set xlabel 'Longitudinal Distance, ft'
set format y "%.1f"
set ylabel 'Elevation, ft'
# set yrange [4:6]
set pointsize 0.5
# set timestamp
set key below

plot "<head -n 79 profile1.out" using (10656 - $4):13 title "Thalweg" with linespoint lt 3, \
     "<head -n 79 profile1.out" using (10656 - $4):5 title 'Initial Conditions' with linespoint lt 1, \
     "profile1.out" using (10656 - $4):5 title 'Simulated' with lines lt 7, \
     "<tail -n 79 profile1.out" using (10656 - $4):5 title 'Final Conditions' with points lt 7
     

