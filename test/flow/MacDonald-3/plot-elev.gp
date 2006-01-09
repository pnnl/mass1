# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: Sun Jan  8 09:49:44 2006 by William A. Perkins <perk@mcperktop.local>
# -------------------------------------------------------------
# $Id$

set auto
set xlabel "Longitudinal Distance, m"
set ylabel "Elevation, m"
set format y "%.1f"
set pointsize 0.5
set key below

# d(x) = (9./8. + 1./4.*sin(3.141569*x*0.3048/500.))/0.3048
d(x) = (9./8. + 1./4.*sin(pi*x/500.))

plot "<head -506 profile1.out" using ($4*0.3048):($5*0.3048) title "Initial Conditions" with linespoints 1, \
     "<tail -506 profile1.out" using ($4*0.3048):($5*0.3048) title "Steady State" with points 7, \
     "<tail -506 profile1.out" using ($4*0.3048):($13*0.3048 + (9./8. + 1./4.*sin(pi*($4*0.3048-1500)/500.))) title "Analytic Solution" with lines 3, \
     "<tail -506 profile1.out" using ($4*0.3048):($13*0.3048) title "Bottom" with lines 7

     
     
