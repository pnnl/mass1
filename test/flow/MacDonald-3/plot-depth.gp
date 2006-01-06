# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: Thu Jan  5 20:34:31 2006 by William A. Perkins <perk@mcperktop.local>
# -------------------------------------------------------------
# $Id$

set auto
set xlabel "Longitudinal Distance, m"
set ylabel "Depth, m"
set format y "%.1f"
set pointsize 0.5
set key bottom

# d(x) = (9./8. + 1./4.*sin(3.141569*x*0.3048/500.))/0.3048
d(x) = (9./8. + 1./4.*sin(pi*x/500.))

plot "<awk '/Date: 04-06-1996  Time: 00:00:00/,/END/' profile1.out" using ($4*0.3048):(($5-$13)*0.3048) title "Simulated" with points 7, \
     d(x-1500) title "Analytic Solution" with lines 1

     
