#!/usr/bin/env python
# -*- mode: python -*-
# -------------------------------------------------------------
# file: profile_extract.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 15, 2011 by William A. Perkins
# Last Change: 2017-06-22 11:43:34 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

import sys

# check version

if sys.hexversion < 0x02050000:
    sys.stderr.write("Error: Python 2.5 or greater required (got %d.%d.%d)\n" %
                     (sys.version_info[0], sys.version_info[1], sys.version_info[2]))
    sys.exit(14)

import os
from optparse import OptionParser
from operator import itemgetter
import re
from datetime import *
from time import *
import csv


# -------------------------------------------------------------
# interpolate_profile
# -------------------------------------------------------------
def interpolate_profile(prof, qprof):
    qtemp = []
    for i in range(len(qprof)):
        (box, rm) = qprof[i]

        idx = 1
        while (rm < prof[idx]['distance']):
            idx = idx + 1

        prm0 = prof[idx-1]['distance']
        prm1 = prof[idx]['distance']

        tmpprof = prof[idx]
        tmpprof['distance'] = rm
        if (rm == prm0):
            tmpprof = prof[idx-1]
        else:
            f = (rm - prm0)/(prm1-prm0)
            for k in tmpprof.keys():
                # if any of the prof[] values are None, this will
                # throw an exception; if that happens change the value
                # to something obviously wrong

                try:
                    tmpprof[k] = f*(prof[idx][k] - prof[idx-1][k]) + prof[idx-1][k]
                except TypeError:
                    tmpprof[k] = -9999.0
                    
        tmpprof['id'] = box
        
        # (prm1, pq1, pe1, pt1) = prof[idx-1]
        # (prm0, pq0, pe0, pt0) = prof[idx]
        
        # if (rm == prm0):
        #     tmpprof = prof[idx]
        #     q = pq0
        #     e = pe0
        #     t = pt0
        # else:
        #     f = (rm - prm0)/(prm1-prm0)
        #     q = f*(pq1 - pq0) + pq0
        #     e = f*(pe1 - pe0) + pe0
        #     t = f*(pt1 - pt0) + pt0

        # tpl = (box, rm, q, e, t)
        qtemp.append(tmpprof)
    return qtemp
        
# -------------------------------------------------------------
# read_next_profile
#
# profile is an open MASS1 profile output file
# after is a date time which is the earliest date to extract from profile
#
# the date of the profile read and a list of tuples (rm, wsel, q) is returned
# -------------------------------------------------------------
def read_next_profile(profile, dometric):

    fldname = (
        'link',
        'point',
        'pid',
        'distance',
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
        'Froude',
        'Courant',
        'D#',
        'friction_slope',
        'BedShear'
        )

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
        'distance' : 1.0,
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
                f = l[fldidx[i]-1:fldidx[i+1]-1]
                try:
                    f = float(f)
                except ValueError:
                    f = None
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
# read_rm_file
# -------------------------------------------------------------
def read_rm_file(rmname):
    
    sys.stderr.write("Attempting to open \"%s\"\n" % (rmname))
    try:
        f = open(rmname)
    except:
        sys.stderr.write("unable to open \"%s\"\n" % (rmname))

    sys.stderr.write("\"%s\" successfully opened\n" % (rmname))
    thermlist = []
    for l in f:
        l.rstrip()
        fld = l.split()

        try:
            box = int(fld[0])
            rm = float(fld[1])
        except ValueError:
            continue

        t = (box, rm)
        thermlist.append(t)
    f.close()
    return (thermlist)

# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
program = os.path.basename(sys.argv[0])

dotemp = False
dosection = False
doheader = False
theid = None
startdate = datetime(month=1, day=1, year=1900)
enddate = datetime(month=1, day=1, year=3000)

basic_fields = [ 'Stage', 'Discharge', 'Velocity' ]
section_fields = [ 'Area', 'TopWidth', 'Depth', 'HydraulicRadius' ]
temp_fields = [ 'Temperature' ]
tdg_fields = [ 'TDGConcentration', 'TDGPressure', 'TDGSaturation' ]

dfmt = "%m/%d/%Y %H:%M"
rmlist = []

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
usage = "Usage: %prog [options] rm profile"
parser = OptionParser(usage=usage)

parser.add_option("-T", "--temperature",
                  action="store_true", dest="temperature", default=False,
                  help="include simulated temperature in output")

parser.add_option("-s", "--section",
                  action="store_true", dest="section", default=False,
                  help="include section properties in output")

parser.add_option("-G", "--tdg",
                  action="store_true", dest="tdg", default=False,
                  help="include simulated TDG fields in output")

parser.add_option("-c", "--column-names",
                  action="store_true", dest="column", default=False,
                  help="include column names in output")
                  
parser.add_option("-I", "--id",
                  action="store", dest="id", type="int", default="-1",
                  help="include a nonnegative identifier in output as the first field")

parser.add_option("-S", "--start", dest="start", action="store", 
                  help="starting output date/time (MM/DD/YYYY HH:MM)")

parser.add_option("-E", "--end", dest="end", action="store", 
                  help="ending output date/time (MM/DD/YYYY HH:MM)")


parser.add_option("-r", "--river-mile", type="float",
                  action="store", dest="rm",
                  help="extract time series at one location (id = 1)")

parser.add_option("-f", "--file",
                  action="store", dest="rmfile",
                  help="read id, rivermile from specified file")

parser.add_option("-m", "--metric", dest="metric",
                  action="store_true", default=False,
                  help="convert English units to metric")

(options, args) = parser.parse_args()

dotemp = options.temperature
dotdg = options.tdg
dosection = options.section
doheader = options.column
theid = options.id
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

if options.rm:
    rmlist.append( (theid, options.rm) )
elif options.rmfile:
    rmlist = read_rm_file(options.rmfile)
else:
    sys.stderr.write("%s: error: locations not specified with --rivermile or --file\n" %
                     (program))
    sys.exit(3)

if (len(args) != 1):
    parser.print_help()
    sys.exit(3)
    
profilename = args[0]

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

try:
    pfile = open(profilename, "r")
except IOError:
    sys.stderr.write("%s: error: cannot open %s\n" %
                     (program, profilename))
    sys.exit(3)

pdata = []
while (True):
    (pdatetime, profile) = read_next_profile(pfile, dometric)
    profile.reverse()
    if (pdatetime):
        if (pdatetime > enddate):
            break
        if (pdatetime >= startdate):
            qtemp = interpolate_profile(profile, rmlist)
            for tpl in qtemp:
                tpl['date'] = pdatetime
                #(box, rm, e, q, t) = tpl
                #tpl = (pdatetime, box, rm, e, q, t)
                pdata.append(tpl)
    else:
        break
        
pfile.close()

pdata.sort(key=itemgetter('id','date'), reverse=False)

if (doheader):
    if (rmlist[0][0] >= 0):
        sys.stdout.write("ID, ")
    sys.stdout.write("Date, Rivermile")
    for f in basic_fields:
        sys.stdout.write(", %s" % (f));
    if (dotemp):
        for f in temp_fields:
            sys.stdout.write(", %s" % (f));
    if (dotdg):
        for f in tdg_fields:
            sys.stdout.write(", %s" % (f));
    if (dosection):
        for f in section_fields:
            sys.stdout.write(", %s" % (f));
            
    sys.stdout.write("\n")

for p in pdata:
    pdatetime = p['date']
    box = p['id']
    rm = p['distance']
    
    if (box >= 0):
        sys.stdout.write("%d, " % (box))
    sys.stdout.write("%s, %.2f" %
                (pdatetime.strftime("%m/%d/%Y %H:%M:%S"), rm))
    for f in basic_fields:
        sys.stdout.write(", %.2f" % (p[f]));

    if (dotemp):
        for f in temp_fields:
            sys.stdout.write(", %.2f" % (p[f]));

    if (dotdg):
        for f in tdg_fields:
            sys.stdout.write(", %.2f" % (p[f]));

    if (dosection):
        for f in section_fields:
            sys.stdout.write(", %.2f" % (p[f]));
            

    sys.stdout.write("\n")


