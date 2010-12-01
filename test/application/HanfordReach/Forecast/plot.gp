# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December  1, 2010 by William A. Perkins
# Last Change: Wed Dec  1 11:12:46 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------
# $Id$ Battelle PNL


set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set xrange ["11-28-2010 14:30:00": "11-30-2010 02:30:00"] noreverse
set format x "%m/%d\n%H:%M"

unset xlabel

set ylabel "Stage Downstream of Savage Island"
set y2label "Stage @ Priest Rapids Dam"
set format y "%.1f"
set format y2 "%.1f"
set ytics nomirror
set y2tics nomirror

set arrow 1 from first "11-29-2010 14:30:00", graph 0 to first "11-29-2010 14:30:00", graph 1 nohead lt 7

set key below

plot 'ts1127.out' using 1:3 axes x1y1 title "Downstream of Savage Island" with lines lt 1 , \
     'ts11.out' using 1:3 axes x1y2 title "Priest Rapids Dam" with lines lt 3
     