# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  2, 1999 by William A. Perkins
# Last Change: Tue Jul 13 08:04:44 1999 by William A. Perkins <perk@hazel.pnl.gov>
# -------------------------------------------------------------
# $Id$

set format x "%.0f"
set format y "%.0f"
set nokey

set xlabel "Distince Along Channel, feet"
set ylabel "Simulated Discharge, cfs"
set yrange [0:2000]

set pointsize 0.5
plot 'profile1.out' using (10656 - $4):6 with linespoints
