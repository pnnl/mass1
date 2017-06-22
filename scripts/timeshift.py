#!/usr/bin/env python2
# -*- mode: python; py-which-shell: "python2";-*-
# -------------------------------------------------------------
# file: timeshift.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March  9, 2016 by William A. Perkins
# Last Change: 2017-06-22 11:46:54 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

import sys, os
from optparse import OptionParser
import re
from datetime import *
import fileinput

# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
program = os.path.basename(sys.argv[0])
usage = "usage: " + program

fmt = "%m-%d-%Y %H:%M:%S"

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
usage = "Usage: %prog [options] [file]"
parser = OptionParser(usage=usage)

parser.add_option("-v", "--verbose",
                  dest="verbose", action="store_true", default=False,
                  help="show what's going on")

parser.add_option("-o", "--output", type="string",
                  dest="output", action="store")

parser.add_option("-H", "--hour-offset", type="int", default=0,
                  dest="hroffset", action="store")

(options, args) = parser.parse_args()

doverbose = options.verbose
hroffset = options.hroffset

theoffset = timedelta(hours=int(hroffset))

if options.output:
    output = open(options.output, mode='w')
else:
    output = sys.stdout

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

rdatetime = re.compile(r'^\s*(\d\d)-(\d\d)-(\d\d\d\d)\s+(\d\d):(\d\d):(\d\d)(.*)$')

if len(args) > 0:
    inp = fileinput.input(args[0])
else:
    inp = fileinput.input("-") 

for line in inp:
    l = line.rstrip()
    m = rdatetime.match(l)
    if (rdatetime.match(l)):
        s = rdatetime.split(l)      # need to ignore s[0]
        pdatetime = datetime(month=int(s[1]), day=int(s[2]), year=int(s[3]),
                             hour=int(s[4]), minute=int(s[5]), second=int(s[6]));
        newtime = pdatetime + theoffset
        output.write("%s %s\n" % (newtime.strftime(fmt), s[7]))
        
    else:
        output.write("%s\n" % (l))
