#!/usr/bin/env python
# -*- mode: python;-*-
# -------------------------------------------------------------
# file: profile_tecplot.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created May 23, 2017 by William A. Perkins
# Last Change: 2017-06-22 11:43:46 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

import sys, os
from optparse import OptionParser
import re
from datetime import *
from time import *

# -------------------------------------------------------------
# read_next_profile
#
# profile is an open MASS1 profile output file
# after is a date time which is the earliest date to extract from profile
#
# the date of the profile read and a list of tuples (rm, wsel, q) is returned
# -------------------------------------------------------------
def read_next_profile(profile, dometric):


    fldidx = (
        1,
        6,
        12,
        18,
        28,
        37,
        53,
        63,
        73,
        85,
        93,
        101,
        109,
        119,
        131,
        141,
        149,
        155,
        161,
        167,
        177,
        189
        )

    fldconv = {
        'Distance' : 1.0,
        'Stage' : 0.3048,
        'Discharge' : 0.028316847,
        'Velocity' : 0.3048,
        'Depth' : 0.3048,
        'Thalweg' : 0.3048,
        'Area' : 0.3048*0.3048,
        'TopWidth' : 0.3048,
        'HydraulicRadius' : 0.3048,     # ft --> m
        'BedShear' : 47.880259         # lb/ft^2 --> Pa
        }
    rdatetime = re.compile(r'.*Date:\s+(\d\d)-(\d\d)-(\d\d\d\d)\s+Time:\s+(\d\d):(\d\d):(\d\d).*')
    rdataline = re.compile(r'^  *\d+')
    rcomline = re.compile(r'^#')
    
    found = False
    pdatetime = None
    theprofile = []
    while (True):
        l = profile.readline()
        if (len(l) == 0):
            break
        l.rstrip()
        if (len(l) == 0):
            break
        if (rdatetime.match(l)):
            s = rdatetime.split(l)      # need to ignore s[0]
            pdatetime = datetime(month=int(s[1]), day=int(s[2]), year=int(s[3]),
                                 hour=int(s[4]), minute=int(s[5]), second=int(s[6]));
            found = True
            # skip three lines
            l = profile.readline()
            l = profile.readline()
            l = profile.readline()
            continue
        
        if (found and rcomline.match(l)):
            break
        
        if (found and rdataline.match(l)):
            thedict = {}
            fld = []
            for i in range(len(fldidx)-1):
                fstr = l[fldidx[i]-1:fldidx[i+1]-1]
                f = None
                try:
                    f = float(fstr)
                except ValueError:
                    sys.stderr.write("Bad float value (%s) for field %d\n" % (fstr, i))
                thedict[fldname[i]] = f

            #thetuple = (float(fld[3]), float(fld[4]), float(fld[5]), float(fld[9]))
            # thetuple = (thedict['distance'], thedict['ws_elevation'],
            #             thedict['discharge'], thedict['temperature'])
            # theprofile.append(thetuple)
            if (dometric):
                mdict = thedict
                for f in fldconv.keys():
                    mdict[f] = mdict[f]*fldconv[f]
                theprofile.append(mdict)
            else:
                theprofile.append(thedict)
            continue

    return (pdatetime, theprofile)


# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
program = os.path.basename(sys.argv[0])
usage = "usage: " + program

startdate = datetime(month=1, day=1, year=1900)
enddate = datetime(month=1, day=1, year=3000)

fldname = (
    'link',
    'point',
    'pid',
    'Distance',
    'Stage',
    'Discharge',
    'Velocity',
    'Depth',
    'TDGConcentration',
    'Temperature',
    'TDGSaturation',
    'TDGPressure',
    'Thalweg',
    'Area',
    'TopWidth',
    'HydraulicRadius',
    'FroudeNumber',
    'CourantNumber',
    'DiffusionNumber',
    'FrictionSlope',
    'BedShear'
)

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
usage = "Usage: %prog [options] profile"
parser = OptionParser(usage=usage)

parser.add_option("-v", "--verbose",
                  dest="verbose", action="store_true", default=False,
                  help="show what's going on")

parser.add_option("-o", "--output", type="string",
                  dest="output", action="store")

parser.add_option("-S", "--start", dest="start", action="store", 
                  help="starting output date/time (MM/DD/YYYY HH:MM)")

parser.add_option("-E", "--end", dest="end", action="store", 
                  help="ending output date/time (MM/DD/YYYY HH:MM)")

parser.add_option("-m", "--metric", dest="metric",
                  action="store_true", default=False,
                  help="convert English units to metric")

(options, args) = parser.parse_args()

doverbose = options.verbose
dometric=options.metric

if (options.start):
    try:
        lt = strptime(options.start, dfmt)
        startdate = datetime(lt.tm_year, lt.tm_mon, lt.tm_mday, lt.tm_hour, lt.tm_min, 0, 0)
    except:
        sys.stderr.write("Error parsing --start argument (%s)\n" % (options.start))
        sys.exit(3)

if (options.end):
    try:
        lt = strptime(options.end, dfmt)
        enddate = datetime(lt.tm_year, lt.tm_mon, lt.tm_mday, lt.tm_hour, lt.tm_min, 0, 0)
    except:
        sys.stderr.write("Error parsing --end argument (%s)\n" % (options.end))
        sys.exit(3)

if options.output:
    output = open(options.output, mode='w')
else:
    output = sys.stdout

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

if (len(args) != 1):
    parser.print_help()
    sys.exit(3)
    
profilename = args[0]

try:
    pfile = open(profilename, "r")
except IOError:
    sys.stderr.write("%s: error: cannot open %s\n" %
                     (program, profilename))
    sys.exit(3)

pdata = []
dohdr = True
nzone = 0
while (True):
    (pdatetime, profile) = read_next_profile(pfile, dometric)
    if (pdatetime):
        if (pdatetime > enddate):
            break
        if (pdatetime >= startdate):
            if (doverbose):
                sys.stderr.write("%s: info: profile for %s read\n" % 
                                 (program, pdatetime.strftime("%m/%d/%Y %H:%M:%S")))
            if (dohdr):
                output.write("VARIABLES = ")
                first = True
                for v in fldname:
                    if (not first):
                        output.write(",\n")
                    output.write("    \"%s\"" % (v))
                    first = False
                output.write("\n")
            dohdr = False

            nzone = nzone + 1

            output.write("ZONE T=\"%s\", I=%d, J=1, K=1, ZONETYPE=ORDERED, DATAPACKING=POINT\n" %
                         (pdatetime.strftime("%m/%d/%Y %H:%M:%S"), len(profile)))

            for p in profile:
                for v in fldname: 
                    if p[v] is not None:
                        outv = float(p[v])
                    else:
                        outv = 0.0
                        sys.stderr.write("%s: warning: substituting zero for bad %s value in zone %d\n" %
                                         (program, v, nzone))
                    output.write("%10.4g " % outv)
                output.write("\n")
                
                                 
    else:
        break
        
pfile.close()

