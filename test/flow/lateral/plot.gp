# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  2, 1999 by William A. Perkins
# Last Change: Fri Jul  2 10:36:38 1999 by William A. Perkins <perk@hughie.pnl.gov>
# -------------------------------------------------------------
# $Id$

set format x "%.0f"
set format y "%.0f"
set nokey

set xlabel "Distince Along Channel, feet"
set ylabel "Simulated Discharge, cfs"
set title "Lateral Inflow"

set pointsize 0.5
plot 'profile1.out' using (10656 - $4):6 with linespoints
