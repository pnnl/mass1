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
# Last Change: Tue Jul 24 14:45:42 2012 by William A. Perkins <d3g096@pe10900.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

import sys, os
from optparse import OptionParser
from operator import itemgetter
import re
from datetime import *
from time import *

# -------------------------------------------------------------
# interpolate_profile
# -------------------------------------------------------------
def interpolate_profile(prof, qprof):
    idx = 0
    qtemp = []
    for i in range(len(qprof)):
        (box, rm) = qprof[i]

        while (rm < prof[idx][0]):
            idx = idx + 1

        (prm1, pq1, pe1, pt1) = prof[idx-1]
        (prm0, pq0, pe0, pt0) = prof[idx]
        
        if (rm == prm0):
            q = pq0
            e = pe0
            t = pt0
        else:
            f = (rm - prm0)/(prm1-prm0)
            q = f*(pq1 - pq0) + pq0
            e = f*(pe1 - pe0) + pe0
            t = f*(pt1 - pt0) + pt0

        tpl = (box, rm, q, e, t)
        qtemp.append(tpl)
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
            fld = l.split()
            thetuple = (float(fld[3]), float(fld[4]), float(fld[5]), float(fld[9]))
            theprofile.append(thetuple)
            continue

    return (pdatetime, theprofile)


# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
program = os.path.basename(sys.argv[0])

dotemp = 1
startdate = datetime(month=1, day=1, year=1900)
enddate = datetime(month=1, day=1, year=3000)

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
usage = "Usage: %prog rm profile"
parser = OptionParser()

parser.add_option("-T", "--temperature",
                  action="store_true", dest="temperature", default=False,
                  help="include simulated temperature in output")

parser.add_option("-S", "--start", dest="start", action="store", 
                  help="starting output date/time (MM/DD/YYYY HH:MM)")

parser.add_option("-E", "--end", dest="end", action="store", 
                  help="ending output date/time (MM/DD/YYYY HH:MM)")

(options, args) = parser.parse_args()

dotemp = options.temperature
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
    sys.stderr.write("%s\n" % usage)
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
quads.append( (1, rm) )

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
                (box, rm, e, q, t) = tpl
                tpl = (pdatetime, box, rm, e, q, t)
                pdata.append(tpl)
    else:
        break
        
pfile.close()

pdata.sort(key=itemgetter(1,0), reverse=False)

for p in pdata:
    (pdatetime, box, rm, e, q, t) = p
    sys.stdout.write("%s, %.2f, %.2f, %.2f" %
                (pdatetime.strftime("%m/%d/%Y %H:%M:%S"),
                 rm, e, q))
    if (dotemp):
        sys.stdout.write(", %.2f" % (t));

    sys.stdout.write("\n")


