#!/usr/local/bin/gnuplot -persist
#
#    
#    	G N U P L O T
#    	Version 4.4 patchlevel 3
#    	last modified March 2011
#    	System: Linux 2.6.18-410.el5
#    
#    	Copyright (C) 1986-1993, 1998, 2004, 2007-2010
#    	Thomas Williams, Colin Kelley and many others
#    
#    	gnuplot home:     http://www.gnuplot.info
#    	faq, bugs, etc:   type "help seeking-assistance"
#    	immediate help:   type "help"
#    	plot window:      hit 'h'
# set terminal x11 
# set output
set term post eps enh color solid "Helvetica" 16
unset clip points
set clip one
unset clip two
set bar 1.000000 front
set border 31 front linetype -1 linewidth 1.000
set xdata time
set ydata
set zdata
set x2data
set y2data
set timefmt x "%m-%d-%Y %H:%M:%S"
set timefmt y "%m-%d-%Y %H:%M:%S"
set timefmt z "%m-%d-%Y %H:%M:%S"
set timefmt x2 "%m-%d-%Y %H:%M:%S"
set timefmt y2 "%m-%d-%Y %H:%M:%S"
set timefmt cb "%m-%d-%Y %H:%M:%S"
set boxwidth
set style fill  empty border
set style rectangle back fc lt -3 fillstyle   solid 1.00 border lt -1
set style circle radius graph 0.02, first 0, 0 
set dummy x,y
set format x "%d%b\n%Y"
set format y "% g"
set format x2 "% g"
set format y2 "% g"
set format z "% g"
set format cb "% g"
set angles radians
unset grid
set key title ""
set key inside right bottom vertical Right noreverse enhanced autotitles nobox
set key noinvert samplen 4 spacing 1 width 0 height 0 
set key maxcolumns 0 maxrows 0
unset label
unset arrow
set style increment default
unset style line
unset style arrow
set style histogram clustered gap 2 title  offset character 0, 0, 0
unset logscale
set offsets 0, 0, 0, 0
set pointsize 1
set pointintervalbox 1
set encoding default
unset polar
unset parametric
unset decimalsign
set view 60, 30, 1, 1
set samples 100, 100
set isosamples 10, 10
set surface
unset contour
set clabel '%8.3g'
set mapping cartesian
set datafile separator whitespace
unset hidden3d
set cntrparam order 4
set cntrparam linear
set cntrparam levels auto 5
set cntrparam points 5
set size ratio 0 1,1
set origin 0,0
set style data points
set style function lines
set xzeroaxis linetype -2 linewidth 1.000
set yzeroaxis linetype -2 linewidth 1.000
set zzeroaxis linetype -2 linewidth 1.000
set x2zeroaxis linetype -2 linewidth 1.000
set y2zeroaxis linetype -2 linewidth 1.000
set ticslevel 0.5
set mxtics default
set mytics default
set mztics default
set mx2tics default
set my2tics default
set mcbtics default
set xtics border in scale 1,0.5 mirror norotate  offset character 0, 0, 0
set xtics autofreq  norangelimit
set ytics border in scale 1,0.5 mirror norotate  offset character 0, 0, 0
set ytics autofreq  norangelimit
set ztics border in scale 1,0.5 nomirror norotate  offset character 0, 0, 0
set ztics autofreq  norangelimit
set nox2tics
set noy2tics
set cbtics border in scale 1,0.5 mirror norotate  offset character 0, 0, 0
set cbtics autofreq  norangelimit
set title "" 
set title  offset character 0, 0, 0 font "" norotate
set timestamp bottom 
set timestamp "" 
set timestamp  offset character 0, 0, 0 font "" norotate
set rrange [ * : * ] noreverse nowriteback  # (currently [8.98847e+307:-8.98847e+307] )
set trange [ * : * ] noreverse nowriteback  # (currently ["12-31-1999 23:59:55":"01-01-2000 00:00:05"] )
set urange [ * : * ] noreverse nowriteback  # (currently ["12-31-1999 23:59:50":"01-01-2000 00:00:10"] )
set vrange [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set xlabel "" 
set xlabel  offset character 0, 0, 0 font "" textcolor lt -1 norotate
set x2label "" 
set x2label  offset character 0, 0, 0 font "" textcolor lt -1 norotate
set xrange [ "04-01-2016 00:00:00" : "10-01-2016 00:00:00" ] noreverse nowriteback
set x2range [ * : * ] noreverse nowriteback  # (currently [5.15376e+08:5.20646e+08] )
set ylabel "Temperature, \312C" 
set ylabel  offset character 0, 0, 0 font "" textcolor lt -1 rotate by -270
set y2label "" 
set y2label  offset character 0, 0, 0 font "" textcolor lt -1 rotate by -270
set yrange [ * : * ] noreverse nowriteback  # (currently [1240.00:1320.00] )
set y2range [ * : * ] noreverse nowriteback  # (currently [1246.13:1314.44] )
set zlabel "" 
set zlabel  offset character 0, 0, 0 font "" textcolor lt -1 norotate
set zrange [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set cblabel "" 
set cblabel  offset character 0, 0, 0 font "" textcolor lt -1 rotate by -270
set cbrange [ * : * ] noreverse nowriteback  # (currently [8.98847e+307:-8.98847e+307] )
set zero 1e-08
set lmargin  -1
set bmargin  -1
set rmargin  -1
set tmargin  -1
set locale "en_US.UTF-8"
set pm3d explicit at s
set pm3d scansautomatic
set pm3d interpolate 1,1 flush begin noftriangles nohidden3d corners2color mean
set palette positive nops_allcF maxcolors 0 gamma 1.5 color model RGB 
set palette rgbformulae 7, 5, 15
set colorbox default
set colorbox vertical origin screen 0.9, 0.2, 0 size screen 0.05, 0.6, 0 front bdefault
set loadpath 
set fontpath 
set fit noerrorvariables
GNUTERM = "x11"
set output "Gifford-Temp-Series.eps"
set key title "Gifford RM 87.40"
plot "Observed/temperature/Gifford-Temp.prn" using 1:3 title "Observed" with lines ls 1, \
     'ts595.out' using 1:8 title "Simulated" with lines ls 3
set output "French_Rocks-Temp-Series.eps"
set key title "French Rocks, RM 103.81"
plot "Observed/temperature/French_Rocks-Temp.prn" using 1:3 title "Observed" with lines ls 1, \
     'ts530.out' using 1:8 title "Simulated" with lines ls 3
set output "Snag_Cove-Temp-Series.eps"
set key title "Snag Cove, RM 126.39"
plot "Observed/temperature/Snag_Cove-Temp.prn" using 1:3 title "Observed" with lines ls 1, \
     'ts1155.out' using 1:8 title "Simulated" with lines ls 3
set output "Flat_Creek-Temp-Series.eps"
set key title "Flat Creek, RM 134.89"
plot "Observed/temperature/Flat_Creek-Temp.prn" using 1:3 title "Observed" with lines ls 1, \
     'ts1104.out' using 1:8 title "Simulated" with lines ls 3
set output "Little_Dalles-Temp-Series.eps"
set key title "Little Dalles, RM 142.15"
plot "Observed/temperature/Little_Dalles-Temp.prn" using 1:3 title "Observed" with lines ls 1, \
     'ts169.out' using 1:8 title "Simulated" with lines ls 3
set output "Onion_Creek-Temp-Series.eps"
set key title "Onion Creek, RM 144.07"
plot "Observed/temperature/Onion_Creek-Temp.prn" using 1:3 title "Observed" with lines ls 1, \
     'ts161.out' using 1:8 title "Simulated" with lines ls 3
set output "Deadmans-Temp-Series.eps"
set key title "Deadmans, RM 151.71"
plot "Observed/temperature/Deadmans-Temp.prn" using 1:3 title "Observed" with lines ls 1, \
     'ts130.out' using 1:8 title "Simulated" with lines ls 3
set output "Border-Temp-Series.eps"
set key title "Border, RM 159.04"
plot "Observed/temperature/Border-Temp.prn" using 1:3 title "Observed" with lines ls 1, \
     'ts11.out' using 1:8 title "Simulated" with lines ls 3


#    EOF