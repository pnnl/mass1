# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: Thu Sep 30 07:13:42 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------
# $Id$

set auto
set xlabel "Longitudinal Distance, m"
set ylabel "Depth, m"
set format y "%.1f"
set pointsize 1.0
set key below


plot "<tail -106 profile1.out" using ($4*0.3048):(($5-$13)*0.3048) title "Steady State" with points lt 7, \
      "point.dat" using ($3*0.3048):($10*0.3048) title "Analytic Solution" with lines lt 3

     
