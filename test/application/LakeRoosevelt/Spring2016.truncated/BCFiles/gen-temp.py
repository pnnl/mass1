#!/usr/bin/env python2
# -*- mode: python; py-which-shell: "python2";-*-
# -------------------------------------------------------------
# file: gen-temp.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October 18, 2016 by William A. Perkins
# Last Change: 2016-10-18 14:55:49 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

import sys, os
from optparse import OptionParser
from datetime import datetime,timedelta
from math import *

# -------------------------------------------------------------
# Tx
# The base, 3-parameter curve
# -------------------------------------------------------------
def Tx(x, a, b, c):
    return a + b*sin(2*pi*(x+c)/365.0)

# -------------------------------------------------------------
# Hx
# The departure from during snow melt
# -------------------------------------------------------------
def Hx(x, d, e, f):
    return - 0.5*d*(1.0 - sin(2*pi*(x - e + (f - e)/4.0)/(f - e)))

# -------------------------------------------------------------
# T
# -------------------------------------------------------------
def T(x, a, b, c, d, e, f):
    t = Tx(x, a, b, c)
    if (e <= x and x <= f):
        t += Hx(x, d, e, f)
    return t

# -------------------------------------------------------------
# dowy
# -------------------------------------------------------------
def dowy(d):
    if (d.month >= 10):
        y = d.year
    else:
        y = d.year - 1
    d0 = datetime(y, 10, 1, 0, 0, 0)
    return (d - d0).total_seconds()/86400.0

# -------------------------------------------------------------
# doy
# -------------------------------------------------------------
def doy(d):
    d0 = datetime(d.year, 10, 1, 0, 0, 0)
    return (d - d0).total_seconds()/86400.0


# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
program = os.path.basename(sys.argv[0])
usage = "usage: " + program

dfmt = "%m-%d-%Y %H:%M:%S"

start = datetime(2016, 1, 1, 12, 0, 0)
end = datetime(2016, 12, 31, 12, 0, 0)
dt = timedelta(1, 0, 0)
docy = False
title = ""

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
usage = "Usage: %prog [options] a b c d e f"
parser = OptionParser(usage=usage)

parser.add_option("-v", "--verbose",
                  dest="verbose", action="store_true", default=False,
                  help="show what's going on")

parser.add_option("-o", "--output", type="string",
                  dest="output", action="store")

parser.add_option("-C", "--calendar-year", 
                  dest="docy", action="store_true", default=False,
                  help="the specified coefficients are for calendar year days")

parser.add_option("-T", "--title", type="string",
                  dest="title", action="store",
                  default="Generated Temperature",
                  help="documentation string to put in output")

(options, args) = parser.parse_args()

doverbose = options.verbose

if options.output:
    output = open(options.output, mode='w')
else:
    output = sys.stdout

docy = options.docy
title = options.title

if (len(args) < 6):
    parser.error("coefficients not specified")

try:
    a = float(args[0])
    b = float(args[1])
    c = float(args[2])
    d = float(args[3])
    e = float(args[4])
    f = float(args[5])
except:
    parser.error("coefficients error")

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------


output.write("# %s\n" % (title))

thedate = start - dt
while (thedate <= end + dt):
    jan1 = datetime(thedate.year, 1, 1)
    delta = thedate - jan1
    if (docy):
        days = doy(thedate)
    else:
        days = dowy(thedate)
    temp = T(days, a, b, c, d, e, f)
    output.write("%s %6.2f /\n" % (thedate.strftime(dfmt), temp));
    thedate += dt


