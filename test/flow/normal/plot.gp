# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: 2017-01-19 10:12:09 d3g096
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

plot "<awk '/Time: 03:00/, /Time: 05:/ { if ($0 !~ /^ *#/) {print $4, $8;} }' profile1.out" using (10656 - $1):2 title 'Initial Conditions' with linespoint lt 1, \
     "<awk '/Time: 05:/, /END/ { if ($0 !~ /^ *#/) {print $4, $8;} }' profile1.out" using (10656 - $1):2 title 'Steady State' with linespoints lt 7  


