# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: Tue Nov 30 13:38:00 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
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
# set timestamp
set key below

plot C(24*60 - x/u,Tp) title 'Translated BC @ t = 24 min' with lines, \
     C(48*60 - x/u,Tp) title 'Translated BC @ t = 48 min' with lines, \
     C(72*60 - x/u,Tp) title 'Translated BC @ t = 72 min' with lines, \
     C(96*60 - x/u,Tp) title 'Translated BC @ t = 96 min' with lines, \
     "<awk '/Time: 06:24/, /Time: 06:26/ { if ($0 !~ /^ *#/) {print $4, $10;} }' profile1.out" using (10656 - $1):2 title 'Simulated' with linespoints lt 7, \
     "<awk '/Time: 06:48/, /Time: 06:50/ { if ($0 !~ /^ *#/) {print $4, $10;} }' profile1.out" using (10656 - $1):2 notitle with linespoints lt 7, \
     "<awk '/Time: 07:12/, /Time: 07:14/ { if ($0 !~ /^ *#/) {print $4, $10;} }' profile1.out" using (10656 - $1):2 notitle with linespoints lt 7, \
     "<awk '/Time: 07:36/, /Time: 07:38/ { if ($0 !~ /^ *#/) {print $4, $10;} }' profile1.out" using (10656 - $1):2 notitle with linespoints lt 7


