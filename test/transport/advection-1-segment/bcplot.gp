# -------------------------------------------------------------
# file: bcplot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 14, 1999 by William A. Perkins
# Last Change: Tue Dec 14 16:25:16 1999 by William A. Perkins <perk@mack.pnl.gov>
# -------------------------------------------------------------
# $Id$

set term postscript eps color dashed "Helvetica" 14
set output 'bcplot.eps'

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set xrange ['01-01-1997  05:30:00': '01-01-1997  07:00:00']
set format x '%H:%M'
set xlabel 'Time'
set ylabel 'Concentration'

set format y '%.1f'

plot 'conc-bc.dat' using 1:3 notitle with lines

set output
