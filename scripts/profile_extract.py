#!/usr/bin/env python
# -*- mode: python -*-
# -------------------------------------------------------------
# file: profile_extract.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 15, 2011 by William A. Perkins
# Last Change: Tue Sep  4 14:07:59 2012 by William A. Perkins <d3g096@pe10900.pnl.gov>
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
                tmpprof[k] = f*(prof[idx][k] - prof[idx-1][k]) + prof[idx-1][k]
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
def read_next_profile(profile):

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
        'bed_shear'
        )

    fldidx = (
        1,
        6,
        12,
        18,
        28,
        37,
        51,
        59,
        68,
        80,
        88,
        96,
        104,
        114,
        126,
        136,
        144,
        150,
        156,
        162,
        172,
        184
        )

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
            theprofile.append(thedict)
            continue

    return (pdatetime, theprofile)


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

(options, args) = parser.parse_args()

dotemp = options.temperature
dotdg = options.tdg
dosection = options.section
doheader = options.column
theid = options.id
if (options.start):
    try:
        lt = strptime(options.start, "%m/%d/%Y %H:%M")
        startdate = datetime(lt.tm_year, lt.tm_mon, lt.tm_mday, lt.tm_hour, lt.tm_min, 0, 0)
    except:
        sys.stderr.write("Error parsing --start argument (%s)\n" % (options.start))
        sys.exit(3)

if (options.end):
    try:
        lt = strptime(options.end, "%m/%d/%Y %H:%M")
        enddate = datetime(lt.tm_year, lt.tm_mon, lt.tm_mday, lt.tm_hour, lt.tm_min, 0, 0)
    except:
        sys.stderr.write("Error parsing --end argument (%s)\n" % (options.end))
        sys.exit(3)

rm = 0.0

if (len(args) != 2):
    parser.print_help()
    sys.exit(3)
    
try:
    rm = float(args[0])
except ValueError:
        sys.stderr.write("%s: error: specified rivermile (%s) not understood\n" %
                         (program, args[0]))
        sys.exit(3)

profilename = args[1]


# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

quads = []
quads.append( (theid, rm) )

try:
    pfile = open(profilename, "r")
except IOError:
    sys.stderr.write("%s: error: cannot open %s\n" %
                     (program, profilename))
    sys.exit(3)

pdata = []
while (True):
    (pdatetime, profile) = read_next_profile(pfile)
    profile.reverse()
    if (pdatetime):
        if (pdatetime > enddate):
            break
        if (pdatetime >= startdate):
            qtemp = interpolate_profile(profile, quads)
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
    if (theid >= 0):
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


