
set term post enh eps color dashed "Helvetica" 18

set xdata time
set timefmt "%m-%d-%Y %H:%M:%S"

W = 1.0
H = 1.5
h = 0.4
set size W, H
set multiplot layout 3, 1

set tmargin 1
set bmargin 0
set lmargin 8
set rmargin 2
set key left reverse Left
set ylabel 'Discharge, m^{3}/s'
set format y '%4.1f'

set size W, h
set origin 0.0, H-h
set xzeroaxis
set yrange [-2:2]
set xtics mirror format ""
set key title 'Pumping/Generation'

plot 'tsPump.out' using 1:4 notitle with lines ls 1

set size W, h
set origin 0.0, H-2*h
set auto y
set key title "Storage"
set ylabel "Elevation, m"

plot 'tsStorage.out' using 1:3 notitle with lines ls 1

set origin 0.0, 0.0
set size W, H-2*h
set yrange [1:5]
set xtics mirror
set bmargin 3
set xtics mirror format "%m/%d\n%H:%M"
set ylabel 'Discharge, m^{3}/s'
set format y '%4.1f'
set key title "Channel"

plot 'tsInlet.out' using 1:4 title "Upstream" w lines ls 1, \
     'tsOutlet.out' using 1:4 title "Downstream" w lines ls 3

unset multiplot

