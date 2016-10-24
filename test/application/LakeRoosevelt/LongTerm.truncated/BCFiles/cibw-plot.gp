# -------------------------------------------------------------
# file: cibw-plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 31, 2002 by William A. Perkins
# Last Change: 2016-10-24 10:44:48 d3g096
# -------------------------------------------------------------
set term postscript eps enhanced color solid "Helvetica" 16

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set pointsize 0.5
set nokey

set title 'Columbia River Temperature @ International Boundary'


set ylabel 'Temperature, {/Symbol \260}C'
set yrange [0:25]

set format x "%d%b\n%Y"
set grid

set output 'cibw-plot-1.eps'
set xrange ['01-01-1971 00:00:00' : '01-01-1976 00:00:00']
plot 'Border-Temperature.dat' using 1:3 with linespoints ls 1

set output 'cibw-plot-2.eps'
set xrange ['01-01-1976 00:00:00' : '01-01-1981 00:00:00']
plot 'Border-Temperature.dat' using 1:3 with linespoints ls 1

set output 'cibw-plot-3.eps'
set xrange ['01-01-1981 00:00:00' : '01-01-1986 00:00:00']
plot 'Border-Temperature.dat' using 1:3 with linespoints ls 1

set output 'cibw-plot-4.eps'
set xrange ['01-01-1986 00:00:00' : '01-01-1991 00:00:00']
plot 'Border-Temperature.dat' using 1:3 with linespoints ls 1

set output 'cibw-plot-5.eps'
set xrange ['01-01-1991 00:00:00' : '01-01-1996 00:00:00']
plot 'Border-Temperature.dat' using 1:3 with linespoints ls 1

set output 'cibw-plot-6.eps'
set xrange ['01-01-1996 00:00:00' : '01-01-2001 00:00:00']
plot 'Border-Temperature.dat' using 1:3 with linespoints ls 1

set output 'cibw-plot-7.eps'
set xrange ['01-01-2001 00:00:00' : '01-01-2006 00:00:00']
plot 'Border-Temperature.dat' using 1:3 with linespoints ls 1

set output 'cibw-plot-8.eps'
set xrange ['01-01-2006 00:00:00' : '01-01-2011 00:00:00']
plot 'Border-Temperature.dat' using 1:3 with linespoints ls 1

set output 'cibw-plot-9.eps'
set xrange ['01-01-2011 00:00:00' : '01-01-2017 00:00:00']
plot 'Border-Temperature.dat' using 1:3 with linespoints ls 1

