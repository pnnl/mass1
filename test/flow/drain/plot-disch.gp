# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: 2017-01-20 07:56:14 d3g096
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color solid "Helvetica,18"

set samples 2000
set format x "%.1f"
set xrange [0:12000]
set xlabel 'Longitudinal Distance, ft'
set format y "%.0f"
set ylabel 'Discharge, cfs'
set pointsize 0.7
set yrange [0:100]
# set timestamp
set key below

plot "profile1.out" using (10656 - $4):6 title 'Simulated' with lines lt 1, \
     "<tail -n 105 profile1.out" using (10656 - $4):6 title 'Final Conditions' with points lt 7
     


