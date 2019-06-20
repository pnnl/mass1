
set term post enh eps color solid "Helvetica" 18
set key font "Helvetica, 16"

set xdata time
set timefmt "%m-%d-%Y %H:%M:%S"
set xlabel "Day"
set xrange ["06-03-2001 00:00:00":"06-09-2001 00:00:00"]
set format x "%d"

set colors classic

set key bottom
set key maxrows 3


set ylabel "Temperature, {/Symbol \260}C"
set yrange [0:*]
plot "channel/tsInlet.out" using 1:8 title "Inlet" with lines ls 1, \
     "channel/tsOutlet.out" using 1:8 title "Outlet (fluvial)" with lines ls 3, \
     "channel-hydro/tsOutlet.out" using 1:8 title "Outlet (single hydrologic)" with lines ls 4, \
     "channel-hydro-multi/tsOutlet.out" using 1:8 title "Outlet (hydrologic cascade)" with lines ls 5, \
     "channel/weather.dat" using 1:3 title "Air" with lines ls 7
