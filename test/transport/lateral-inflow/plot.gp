# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  2, 1999 by William A. Perkins
# Last Change: Thu Mar  9 11:05:04 2000 by William A. Perkins <perk@mack.pnl.gov>
# -------------------------------------------------------------
# $Id$


set format x "%.0f"
set format y "%.1f"
set nokey

set pointsize 0.5

set multiplot
set origin 0.0, 0.5
set size 1.0, 0.5
set lmargin 8

set xlabel ''
set ylabel "Simulated Discharge, cfs"
set yrange [0:2000]

plot '<tail +155 profile1.out' using (10656 - $4):6 with linespoints

set origin 0.0, 0.0
set size 1.0, 0.5
set lmargin 8

set title ''

set xlabel "Distince Along Channel, feet"
set ylabel "Simulated Temperature, C"
set yrange [12:13]
set grid


plot 'profile1.out' using (10656 - $4):10 with lines

set nomultiplot
