# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  2, 1999 by William A. Perkins
# Last Change: 2019-05-23 13:59:38 d3g096
# -------------------------------------------------------------
# $Id$


set format x "%.0f"
set format y "%.1f"
set nokey

set pointsize 0.5

set multiplot
set origin 0.0, 0.5
set size 1.0, 0.5
set lmargin 10

set xlabel ''
set ylabel "Simulated Discharge, cfs"
set yrange [0:2000]

plot '<tail -n +7 profile1.out' using (10656 - $4):6 with linespoints

set origin 0.0, 0.0
set size 1.0, 0.5
set lmargin 10

set title ''

set xlabel "Distince Along Channel, feet"
set ylabel "Simulated Temperature, C"
set yrange [12:18]
set grid


plot 'profile1.out' using (10656 - $4):10 with lines

set nomultiplot
