#!/usr/bin/env python2
# -*- mode: python; py-which-shell: "python2";-*-
# -------------------------------------------------------------
# file: gcpud.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March  9, 2011 by William A. Perkins
# Last Change: Fri Mar 11 08:11:23 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

import getopt, sys, os
import urllib
import urllib2
from datetime import *
from time import *

thefmt = "%m-%d-%Y %H:%M:%S"

# -------------------------------------------------------------
# download_prdq_recent
#
# Gets PRD discharge directly from GCPUD.  Usually current through
# midnight the previous day.
# -------------------------------------------------------------
def download_prdq_recent(now, outname):
    urlbase = "http://www.gcpud.org/data/water/fixed/%Y/csvform/%m%d%y.csv"
    m = now.month
    d = now.day
    y = now.year

    offset = timedelta(minutes=30)

    start = datetime(y, m, d, 0, 0)
    start -= timedelta(days=10)

    outf = open(outname, "w")
    outf.write("# Priest Rapids Discharge, retrieved %s from www.gcpud.org\n" %
           (datetime.now().strftime("%m/%d/%Y %H:%M:%S %Z%z")))
    

    while (start <= now):
        url = start.strftime(urlbase)
        sys.stderr.write("Trying URL: %s\n" % (url))
        start += timedelta(1)
        try:
            f = urllib2.urlopen(url)
        except:
            sys.stderr.write("unable to get URL: %s\n" % (url))
            continue

        thedate = None
        lnum = 0
        for l in f:
            l.rstrip()
            lnum += 1
            fld = l.split(",")
            if (len(fld) < 20):
                continue

            if (l.find("/") < 0):
                continue
            
            if (fld[1].find("/") > 0):
                ioff = 0
            elif (fld[0].find("/") > 0):
                ioff = 1
            else:
                sys.stderr.write("%s: %d: not in proper column\n" % (url, lnum))

            dstr = "%s %04d" % (fld[1-ioff], int(fld[2-ioff]))
            lt = strptime(dstr, "%m/%d/%Y %H%M")
            d = datetime(lt.tm_year, lt.tm_mon, lt.tm_mday,
                         hour=lt.tm_hour, minute=lt.tm_min, second=lt.tm_sec)
            q = float(fld[19-ioff])*1000.0
            if (d <= now):
                d -= offset
                outf.write("%s %9.1f /\n" % (d.strftime(thefmt), q))
        f.close()
    outf.close()
    return


# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
program = os.path.basename(sys.argv[0])
usage = "usage: " + program

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
try:
    opts, args = getopt.getopt(sys.argv[1:], "h?")
except getopt.GetoptError:
    sys.stderr.write(usage + "\n")
    sys.exit(2)

for o, a in opts:
    if (o == "-h" or o == "-?"):
        sys.stderr.write(usage + "\n")
        sys.exit(0)

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

now = datetime.today() - timedelta(30)
now = datetime(2011, 3, 1, 8, 0)
download_prdq_recent(now, "PRD-Qtotal.dat")
