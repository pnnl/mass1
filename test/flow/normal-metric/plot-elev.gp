# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: 2019-02-13 09:58:58 d3g096
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color solid "Helvetica,18"


set samples 2000
set format x "%.1f"
set xrange [0:3300]
set xlabel 'Longitudinal Distance, m'
set format y "%.1f"
set ylabel 'Elevation, m'
# set yrange [4:6]
set pointsize 0.5
# set timestamp
set key below

L = 3247.95

plot "<awk '/Time: 03:00/, /Time: 05:/ { if ($0 !~ /^ *#/) {print $4, $13;} }' profile1.out" using (L - $1):2 title "Thalweg" with linespoint lt 3, \
     "<awk '/Time: 03:00/, /Time: 05:/ { if ($0 !~ /^ *#/) {print $4, $5;} }' profile1.out" using (L - $1):2 title 'Initial Conditions' with linespoint lt 1, \
     "<awk '/Time: 05:/, /END/ { if ($0 !~ /^ *#/) {print $4, $5;} }' profile1.out" using (L - $1):2 title 'Steady State' with linespoints lt 7  


