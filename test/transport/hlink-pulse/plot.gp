
set xdata time
set timefmt "%m-%d-%Y %H:%M:%S

set format x "%H:%M"
set format y "%.1f"
set ylabel "Concentration"

set yrange [0:1.4]

plot 'ts11.out' using 1:9 title "Inlet" w lines, \
     'ts12.out' using 1:9 title "Outlet" w lines

