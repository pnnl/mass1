# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: 2020-05-19 12:36:10 d3g096
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color solid "Helvetica,18"


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

plot "<head -n 80 profile1.out" using (10656 - $4):13 title "Thalweg" with linespoint lt 3, \
     "" using (10656 - $4):5 title 'Initial Conditions' with linespoint lt 1, \
     "<tail -n 80 profile1.out" using (10656 - $4):5 title 'Steady State' with linespoints lt 7  


