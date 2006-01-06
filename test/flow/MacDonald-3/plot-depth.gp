# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: Wed Jan  7 09:42:57 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

set term postscript eps enhanced mono dashed "Helvetica" 22

set xlabel "Longitudinal Distance, m"
set yrange [0.8:1.5]
set ylabel "Depth, m"
set format y "%.1f"
set pointsize 0.5

# d(x) = (9./8. + 1./4.*sin(3.141569*x*0.3048/500.))/0.3048
d(x) = (9./8. + 1./4.*sin(3.141569*x/500.))

plot "<perl ../../../scripts/mass2slice.pl -i -l plot.nc depth 1 36" using ($3*0.3048):($4*0.3048) title "Simulated" with points 1, \
     d(x) title "Analytic" with lines 3
     
