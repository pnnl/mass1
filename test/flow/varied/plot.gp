# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 29, 2001 by William A. Perkins
# Last Change: Thu Sep 30 08:06:40 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------

set term postscript eps color dashed "Helvetica" 22
set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set ylabel 'Discharge, cfs'
set yrange [@MIN@:@MAX@]
set xrange ['05-31-1997 00:00:00':'06-04-1997 00:00:00']
set key below

set title 'Normal Depth = @NORMAL@ ft, Boundary Depth = @D@'

#  plot 'ts11.out' using 1:4 title "Boundary Inflow" with lines 1, \
#       'ts1100.out' using 1:4 title "50 Miles Dowstream" with lines 3, \
#       'ts1200.out' using 1:4 title "100 Miles Dowstream" with lines 4, \
#       'ts1301.out' using 1:4 title "150 Miles Dowstream" with lines 5
plot 'ts11.out' using 1:4 title "Boundary Inflow" with lines lt 1, \
     'ts1201.out' using 1:4 title "50 Miles Dowstream" with lines lt 3, \
     'ts1401.out' using 1:4 title "100 Miles Dowstream" with lines lt 4, \
     'ts1601.out' using 1:4 title "150 Miles Dowstream" with lines lt 5
