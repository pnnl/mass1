# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: Wed Jan  7 09:44:27 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

set term postscript eps enhanced mono dashed "Helvetica" 22

set xlabel "Longitudinal Distance, ft"
set ylabel "Elevation, ft"
set pointsize 0.5


plot "<perl ../../../scripts/mass2slice.pl -i plot.nc zbot 1 36" using ($3*0.3048):($4*0.3048) title "Bottom" with lines 7, \
     "<perl ../../../scripts/mass2slice.pl -i -l plot.nc wsel 1 36" using ($3*0.3048):($4*0.3048) title "Simulated" with points 1, \
     "<perl ../../../scripts/mass2slice.pl -i plot.nc zbot 1 36" using ($3*0.3048):($4*0.3048 + (1.125+0.25*sin(pi*$3*0.3048/500))) title "Analytic" with lines 3
     
