# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: Tue Nov 30 13:36:28 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------
# $Id$


u = 2.0
D = 70.0
Co = 10.0
C(x,t) = (Co/2)*(erfc((x-u*t)/sqrt(4*D*t))+erfc((x+u*t)/sqrt(4*D*t))*exp(u*x/D))

set format x "%.1f"
set xlabel 'Longitudinal Distance, ft'
set format y "%.1f"
set ylabel 'Concentration'
set xrange [0:12000]
set pointsize 0.5
set timestamp

                                # show the internal connection between
                                # segments
                                
set arrow from 5328.0,0.0 to 5328.0,10.0 nohead lt 0


                                # If delta t is 18.0s (0.005 hr), t =
                                # 360.0 is 20 time steps, t = 1080.0
                                # is 60 time steps, and t = 1800.0 is
                                # 100 time steps

plot C(x, 24*60) title "Analytic: t = 24.0 min", \
     C(x, 48*60) title "Analytic: t = 48.0 min", \
     C(x, 72*60) title "Analytic: t = 72.0 min", \
     'profile1.out' using (10656 - $4):10 title 'MASS1 Simulated' with linespoints lt 7


