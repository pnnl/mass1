# -------------------------------------------------------------
# file: outflow.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 23, 2017 by William A. Perkins
# Last Change: 2017-06-23 13:04:02 d3g096
# -------------------------------------------------------------


set xdata time
set timefmt "%m-%d-%Y %H:%M:%S"

set ylabel 'Discharge, kcfs"
set format y "%.1f"
set format x "%d%b\n%Y"

set xrange ["02-01-2000 00:00:00":"03-01-2000 00:00:00"]

plot 'ts32126.out' using 1:($4/1000) with lines