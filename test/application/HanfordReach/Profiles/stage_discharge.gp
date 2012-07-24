# -------------------------------------------------------------
# file: stage_discharge.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July 23, 2012 by William A. Perkins
# Last Change: Mon Jul 23 10:27:14 2012 by William A. Perkins <d3g096@pe10900.pnl.gov>
# -------------------------------------------------------------

set terminal postscript eps enh color solid "Helvetica" 16

set grid

set xtics nomirror
set x2tics nomirror
set xlabel "Discharge, m^{3}/s"
set x2label "Discharge, kcfs"
set format x "%.0f"
set format x2 "%.0f"
set xrange [0:14158.423]
set x2range [0:500]

set ytics nomirror
set y2tics nomirror
set ylabel "Water Surface Elevation, m NGVD29"
set y2label "Water Surface Elevation, ft NGVD29"
set format y "%.1f"
set format y2 "%.0f"

set dataf sep ','
set key title "River kilometer @KM@"

set auto y
set auto y2

set output "/dev/null"
plot "stage_discharge_@KM@.dat" using ($4/1000.0):($3) axes x2y2 notitle with lines lt 1

set output
set y2range [GPVAL_Y2_MIN : GPVAL_Y2_MAX]
set yrange [GPVAL_Y2_MIN*0.3048 : GPVAL_Y2_MAX*0.3048]
replot



