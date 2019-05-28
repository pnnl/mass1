# -------------------------------------------------------------
# file: mass-plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 16, 2000 by William A. Perkins
# Last Change: 2019-05-10 07:44:56 d3g096
# -------------------------------------------------------------
# $Id$

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set format x '%H:%M'
set xlabel 'Simulation Time'

set format y '%.1e'
set ylabel 'Contaminant Mass'

set key bottom

infile = sprintf("ts11-%s.out", case)
outfile = sprintf("ts1149-%s.out", case)
cmdstr = "<awk '$0 !~ /^#/ {sum=sum+$4*$8*144; print $1, $2, sum; }' %s"

plot 7200000 notitle with lines lt 0, \
     sprintf(cmdstr, infile) using 1:3 title 'Inflow' with lines lt 1, \
     sprintf(cmdstr, outfile) using 1:3 title 'Outflow' with lines lt 3
     