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
# Last Change: Tue Mar 15 08:17:56 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

import sys, os
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

        (prm1, pq1, pe1) = prof[idx-1]
        (prm0, pq0, pe0) = prof[idx]
        
        if (rm == prm0):
            q = pq0
            e = pe0
        else:
            f = (rm - prm0)/(prm1-prm0)
            q = f*(pq1 - pq0) + pq0
            e = f*(pe1 - pe0) + pe0

        t = (box, rm, q, e)
        qtemp.append(t)
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
            thetuple = (float(fld[3]), float(fld[4]), float(fld[5]))
            theprofile.append(thetuple)
            continue

    return (pdatetime, theprofile)

# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
program = os.path.basename(sys.argv[0])
usage = "usage: " + program



# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

quads = []
quads.append( (1, 392.66) )
quads.append( (2, 373.59) )

pfile = open("profile1.out", "r")
pdata = []
while (True):
    (pdatetime, profile) = read_next_profile(pfile)
    profile.reverse()
    if (pdatetime):
        qtemp = interpolate_profile(profile, quads)
        for t in qtemp:
            (box, rm, e, q) = t
            t = (pdatetime, box, rm, e, q)
            pdata.append(t)
    else:
        break
        
pfile.close()

pdata.sort(key=itemgetter(1,0), reverse=False)

for p in pdata:
    (pdatetime, box, rm, e, q) = p
    sys.stdout.write("%s, %d, %.2f, %.2f, %.2f\n" %
                (pdatetime.strftime("%m/%d/%Y %H:%M:%S"),
                 box, rm, e, q))


