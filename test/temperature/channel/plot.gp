
set term post enh eps color solid "Helvetica" 18

set xdata time
set timefmt "%m-%d-%Y %H:%M:%S"

set format x "%d"
set xlabel "Day"
set ylabel "Temperature, {/Symbol \260}C"
plot "tsInlet.out" using 1:8 title "Inlet" with lines ls 1, \
     "tsOutlet.out" using 1:8 title "Outlet" with lines ls 3
