#!/usr/bin/env python
# -*- mode: python; py-which-shell: "python2";-*-
# -------------------------------------------------------------
# file: bcsteps.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 26, 2012 by William A. Perkins
# Last Change: 2017-06-22 11:30:44 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

import sys, os
from optparse import OptionParser
from datetime import datetime, timedelta

# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
program = os.path.basename(sys.argv[0])

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
usage = "Usage: %prog [options] network.file.txt"

parser = OptionParser(usage=usage)

parser.add_option("-v", "--verbose",
                  dest="verbose", action="store_true", default=False,
                  help="show what's going on")

parser.add_option("-S", "--time-step", type="float",
                  dest="timestep", action="store", default=7.0,
                  help="time step, days")

parser.add_option("-R", "--ramp-time", type="float",
                  dest="ramp", action="store", default=0.0,
                  help="time, days, to ramp between steps")

parser.add_option("-I", "--initial", type="float",
                  dest="initial", action="store", default=0.0,
                  help="initial boundary value")

parser.add_option("-F", "--final", type="float",
                  dest="final", action="store", default=10.0,
                  help="final boundary value")

parser.add_option("-s", "--bc-step", type="float",
                  dest="step", action="store", default=1.0,
                  help="boundary value step")

parser.add_option("-o", "--output", type="string",
                  dest="output", action="store")

(options, args) = parser.parse_args()

doverbose = options.verbose

bcinitial = options.initial
bcfinal = options.final
bcstep = options.step
daystep = timedelta(options.timestep)
rampstep = timedelta(options.ramp)

if bcinitial > bcfinal:
    bctest = "bc >= bcfinal"
    bcstep = -abs(bcstep)
elif bcinitial < bcfinal:
    bctest = "bc <= bcfinal"
    bcstep = abs(bcstep)
else:
    sys.stderr.write("%s: error: initial and final value are the same\n" % (program))
    sys.exit(3)

if options.output:
    output = open(options.output, mode='w')
else:
    output = sys.stdout

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

dfmt = "%m-%d-%Y %H:%M:%S"
bcfmt = "%s %15.3f\n"

output.write("# A stepped BC file\n")

bc = bcinitial
thedate = datetime(1900, 1, 1, 0, 0, 0)
output.write(bcfmt % (thedate.strftime(dfmt), bc))


thedate = datetime(2000, 2, 1, 0, 0, 0)
while eval(bctest):
    output.write(bcfmt % ((thedate+rampstep).strftime(dfmt), bc))
    thedate += daystep
    output.write(bcfmt % (thedate.strftime(dfmt), bc))
    bc += bcstep
    
bc -= bcstep
thedate = datetime(3000, 1, 1, 0, 0, 0)
output.write(bcfmt % (thedate.strftime(dfmt), bc))

output.close()
