#!/usr/bin/gnuplot -persist
#
#    
#    	G N U P L O T
#    	Linux version 3.7
#    	patchlevel 1
#    	last modified Fri Oct 22 18:00:00 BST 1999
#    
#    	Copyright(C) 1986 - 1993, 1998, 1999
#    	Thomas Williams, Colin Kelley and many others
#    
#    	Type `help` to access the on-line reference manual
#    	The gnuplot FAQ is available from
#    	<http://www.ucc.ie/gnuplot/gnuplot-faq.html>
#    
#    	Send comments and requests for help to <info-gnuplot@dartmouth.edu>
#    	Send bugs, suggestions and mods to <bug-gnuplot@dartmouth.edu>
#    
set term postscript landscape enhanced color dashed "Helvetica" 14
# set output
set noclip points
set clip one
set noclip two
set bar 1.000000
set border 31 lt -1 lw 1.000
set xdata time
set ydata
set zdata
set x2data
set y2data
set boxwidth
set dummy x,y
set format x "%g"
set format y "%g"
set format x2 "%g"
set format y2 "%g"
set format z "%g"
set angles radians
set nogrid
set key title ""
set key below
set nolabel
set noarrow
set nologscale
set offsets 0, 0, 0, 0
set pointsize 1
set encoding default
set nopolar
set noparametric
set view 60, 30, 1, 1
set samples 100, 100
set isosamples 10, 10
set surface
set nocontour
set clabel '%8.3g'
set mapping cartesian
set nohidden3d
set cntrparam order 4
set cntrparam linear
set cntrparam levels auto 5
set cntrparam points 5
set size ratio 0 1,1
set origin 0,0
set xzeroaxis lt -2 lw 1.000
set x2zeroaxis lt -2 lw 1.000
set yzeroaxis lt -2 lw 1.000
set y2zeroaxis lt -2 lw 1.000
set tics in
set ticslevel 0.5
set mxtics default
set mytics default
set mx2tics default
set my2tics default
set xtics border mirror norotate autofreq 
set ytics border mirror norotate autofreq 
set ztics border nomirror norotate autofreq 
set nox2tics
set noy2tics
set title "MASS1 - {/Symbol D}t = @TIMESTEP@" 
set timestamp "" bottom norotate 
set rrange [ * : * ] noreverse nowriteback  # (currently [-0.00000:10.0000] )
set trange [ * : * ] noreverse nowriteback  # (currently ["12-31-1999 23:59:55":"01-01-2000 00:00:05"] )
set urange [ * : * ] noreverse nowriteback  # (currently ["12-31-1999 23:59:55":"01-01-2000 00:00:05"] )
set vrange [ * : * ] noreverse nowriteback  # (currently [-5.00000:5.00000] )
set xlabel "" 
set x2label "" 
set timefmt "%m-%d-%Y %H:%M:%S"
set xrange [ '05-31-1997 00:00:00' : '06-04-1997 00:00:00' ] noreverse nowriteback  # (currently ["12-31-1999 23:59:50":"01-01-2000 00:00:10"] )
set x2range [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set y2label "" 
set yrange [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set y2range [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set zlabel "" 
set zrange [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set zero 1e-08
set lmargin -1
set bmargin -1
set rmargin -1
set tmargin -1
set locale "C"
set ylabel "Discharge, cfs" 
plot 'ts11.out' using 1:4 title 'Upstream Boundary' with lines, \
     'ts1101.out' using 1:4 title '50 miles Downstream' with lines, \
     'ts1201.out' using 1:4 title '100 miles Downstream' with lines, \
     'ts1301.out' using 1:4 title 'Downstream Boundary' with lines
set ylabel "Stage, feet" 
plot 'ts11.out' using 1:3 title 'Upstream Boundary' with lines, \
     'ts1101.out' using 1:3 title '50 miles Downstream' with lines, \
     'ts1201.out' using 1:3 title '100 miles Downstream' with lines, \
     'ts1301.out' using 1:3 title 'Downstream Boundary' with lines
#    EOF
