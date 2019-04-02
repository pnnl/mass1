# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: 2019-03-15 06:55:20 d3g096
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color solid "Helvetica,18"

set samples 2000
set format x "%.1f"
# set xrange [0:22000*0.3048]
set xlabel 'Longitudinal Distance, ft'
set format y "%.1f"
set ylabel 'Depth, ft'
set yrange [0:*]
set pointsize 0.5
# set timestamp
set key below

set arrow from 0*0.3048,5*0.3048 to 10656*0.3048,5*0.3048 nohead lt 0
set arrow from 10656*0.3048,2.5*0.3048 to 2*10656*0.3048,2.5*0.3048 nohead lt 0

plot "<awk '/Time: 03:/, /Time: 05:/ { if ($0 !~ /^ *#/) {print $4, $8;} }' profile1.out" using (2*10656*0.3048 - $1):2 title 'Initial Conditions' with linespoint lt 1, \
     "<awk '/Time: 11:/, /END/ { if ($0 !~ /^ *#/) {print $4, $8;} }' profile1.out" using (2*10656*0.3048 - $1):2 title 'Steady State' with linespoints lt 7

