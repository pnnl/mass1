# -------------------------------------------------------------
# file: bcplot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 14, 1999 by William A. Perkins
# Last Change: Tue Dec 14 22:26:14 1999 by William A. Perkins <perk@mack.pnl.gov>
# -------------------------------------------------------------

# $Id$

set terminal postscript eps color dashed "Helvetica" 14
set output 'bcplot.eps'

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set xrange ['01-01-1997 03:00:00' : '01-02-1997 09:00:00' ]
set format x "%d%b\n%H:%M"

set format y '%.0f'
set ylabel 'Lateral Inflow, cfs'
set xzeroaxis

set nokey

plot 'lateral-inflow.dat' using 1:($3*10656.0) with lines

set output