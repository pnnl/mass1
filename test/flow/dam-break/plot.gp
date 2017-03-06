# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March  6, 2017 by William A. Perkins
# Last Change: 2017-03-06 09:46:42 d3g096
# -------------------------------------------------------------

set key left

set yrange [0:*]
set ytics nomirror
set ylabel "Elevation, ft"
set format y "%.0f"

set y2tics nomirror
set y2label "Froude Number"
set format y2 "%.1f"

set xrange [0:10]
set xlabel "Distance, miles"
set format x "%.1f"


plot '<head -n 2005 profile1.out' using ($4/5280.0):5 axes x1y1 title "Initial Elevation" with lines ls 1, \
     '<tail -n 2005 profile1.out' using ($4/5280.0):5 axes x1y1 title "Simulated Elevation" with lines ls 3, \
     '<tail -n 2005 profile1.out' using ($4/5280.0):17 axes x1y2 title "Simulated Froude Number" with lines ls 1 lc 7