
set term post enh eps color dashed "Helvetica" 18

set xdata time
set timefmt "%m-%d-%Y %H:%M:%S"
set xlabel "Day"
set xrange ["06-03-2001 00:00:00":"06-15-2001 00:00:00"]
set format x "%d"

set ylabel "Temperature, {/Symbol \260}C"
set yrange [0:*]
plot "tsInlet.out" using 1:9 title "Inlet" with lines ls 1, \
     "tsOutlet.out" using 1:9 title "Outlet" with lines ls 3, \
     "../channel/equilibrium_temp.dat" using 1:3 title "Equilibrium" with lines ls 4, \
     "../channel/weather.dat" using 1:3 title "Air" with lines ls 7
