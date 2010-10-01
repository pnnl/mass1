# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: Thu Sep 30 11:46:52 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------
# $Id$


u = 2.0
D = 30.0
Co = 10.0
C(x,t) = (Co/2)*(erfc((x-u*t)/sqrt(4*D*t))+erfc((x+u*t)/sqrt(4*D*t))*exp(u*x/D))

set format x "%.1f"
set xlabel 'Longitudinal Distance, ft'
set format y "%.1f"
set ylabel 'Concentration'
set xrange [0:5280]
set pointsize 0.5
#set timestamp
set key below

                                # If delta t is 18.0s (0.005 hr), t =
                                # 360.0 is 20 time steps, t = 1080.0
                                # is 60 time steps, and t = 1800.0 is
                                # 100 time steps

plot C(x, 360.0) title "Analytic: t = 6.0 min", \
     C(x, 720.0) title "Analytic: t = 12.0 min", \
     C(x, 1080.0) title "Analytic: t = 18.0 min", \
     C(x, 1440.0) title "Analytic: t = 24.0 min", \
     C(x, 1800.0) title "Analytic: t = 30.0 min", \
     '<tail +310 profile1.out' using (10584 - $4):10 title 'MASS1 Simulated' with points lt 7


