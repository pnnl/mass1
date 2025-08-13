
set xdata time
set timefmt "%m-%d-%Y %H:%M:%S"
set format x "%b%d"

set ytics nomirror
set ylabel "Temperature, {/Symbol \260}C"
set y2tics nomirror
set y2label "Discharge, cfs"

set pointsize 0.5
set key bottom

plot "tsInlet.out"   using 1:9 axes x1y1 title "Inlet Temperature" with lines ls 1, \
     "tsInlet.out"   using 1:4 axes x1y2 title "Inlet Discharge" with points ls 1, \
     "tsOutlet.out"  using 1:9 axes x1y1 title "Outlet Temperature" with lines ls 3, \
     "tsOutlet.out"  using 1:4 axes x1y2 title "Outlet Discharge" with points ls 3

