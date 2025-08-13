set term post enh eps color dashed "Helvetica" 18

set xdata time
set timefmt "%m-%d-%Y %H:%M:%S"

set xtics mirror format "%m/%d\n%H:%M"
set ylabel "Temperature, \260C"
set key bottom

plot "tsInlet.out" using 1:9 title "Inlet" with lines ls 1, \
     "tsUpstream.out" using 1:9 title "Above Withdrawal" with lines ls 2, \
     "tsStorage.out" using 1:9 title "Storage" with lines ls 3, \
     "tsDownstream.out" using 1:9 title "Below Return" with lines ls 4, \
     "tsOutlet.out" using 1:9 title "Outlet" with lines ls 5

