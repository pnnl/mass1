# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: Wed Mar 31 15:02:44 1999 by William A. Perkins <perk@tophet>
# -------------------------------------------------------------
# $Id$


u = 2.0
Co = 10.0
Tp = 12 * 60
C(t,Tp) = (t < 0) ? 0 : ((t < Tp) ? (1 - (Tp-t)/Tp)*Co : ((t < 2*Tp) ? ((2*Tp - t)/Tp)*Co : 0))

set samples 2000
set format x "%.1f"
set xrange [0:12000]
set xlabel 'Longitudinal Distance, ft'
set format y "%.1f"
set ylabel 'Concentration'
set pointsize 0.5
set timestamp

plot C(24*60 - x/u,Tp) title 'Translated BC @ t = 24 min' with lines, \
     C(48*60 - x/u,Tp) title 'Translated BC @ t = 48 min' with lines, \
     C(72*60 - x/u,Tp) title 'Translated BC @ t = 72 min' with lines, \
     'profile1.out' using (10656 - $4):10 title 'Simulated' with linespoints 7


