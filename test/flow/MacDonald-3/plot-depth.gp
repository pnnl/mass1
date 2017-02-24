# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: 2017-01-19 14:15:43 d3g096
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color solid "Helvetica,18"
set auto
set xlabel "Longitudinal Distance, m"
set ylabel "Depth, m"
set format y "%.1f"
set pointsize 0.5
set key below

# d(x) = (9./8. + 1./4.*sin(3.141569*x*0.3048/500.))/0.3048
d(x) = (9./8. + 1./4.*sin(pi*x/500.))

plot "<tail -506 profile1.out" using ($4*0.3048):(($8)*0.3048) title "Simulated" with points lt 7, \
     d(x-1500) title "Analytic Solution" with lines lt 3

     
