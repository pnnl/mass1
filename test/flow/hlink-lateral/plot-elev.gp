# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  2, 1999 by William A. Perkins
# Last Change: 2018-08-15 14:23:25 d3g096
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color solid "Helvetica,18"
set format x "%.0f"
set format y "%.0f"
set nokey

set xlabel "Distince Along Channel, feet"
set ylabel "Simulated Water Surface Elevation, ft"
set yrange [0:*]
set key below

set pointsize 0.5
plot "<awk '/Time: 06:/, /END/ { if ($0 !~ /^ *#/) {print $4, $5;} }' profile1.inflow.out" using (10656 - $1):2 title "w/ Lateral Inflow" with linespoints lt 1, \
     "<awk '/Time: 06:/, /END/ { if ($0 !~ /^ *#/) {print $4, $5;} }' profile1.outflow.out" using (10656 - $1):2 title "w/ Lateral Outflow" with linespoints lt 3

