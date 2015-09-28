set terminal png \
    font "LiberationSans-Regular.ttf" \
    size 800,600
set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'
set xrange ["@PSTART@": "@PEND@"] noreverse

thefile = "results.txt"
now = "@NOW@"

set arrow 1 from first now, graph 0 to first now, graph 1 nohead lt 7
set grid
set timestamp
set format x "%d%b%y\n%H:%M"

quad = 0

load 'plot1quad.gp'

