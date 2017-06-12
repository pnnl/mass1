# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: 2017-06-12 09:54:34 d3g096
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color solid "Helvetica,18"

set samples 2000
set format x "%.1f"
set xrange [0:12000]
set xlabel 'Longitudinal Distance, ft'
set format y "%.1f"
set ylabel 'Depth, ft'
# set yrange [4:6]
set pointsize 0.5
# set timestamp
set key below

plot "<head -n 105 profile1.out" using (10656 - $4):8 title 'Initial Conditions' with linespoint lt 1, \
     "<tail -n 105 profile1.out" using (10656 - $4):8 title 'Final Conditions' with linespoints lt 7  


