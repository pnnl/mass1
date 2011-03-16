#!/usr/bin/env python2
# -*- mode: python; -*-
# -------------------------------------------------------------
# file: wmd.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March  9, 2011 by William A. Perkins
# Last Change: Fri Mar 11 07:52:42 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

import getopt, sys, os
import urllib
import urllib2
from datetime import *
from time import *

thefmt = "%m-%d-%Y %H:%M:%S"

# -------------------------------------------------------------
# download_wmd_old
# -------------------------------------------------------------
def download_wmd_old(now, code, fld, outname):

    urlbase = "http://www.nwd-wc.usace.army.mil/perl/dataquery.pl"
    
    today = datetime.now()
    start = now - timedelta(days=10)
    end = now + timedelta(days=1)

    scale = 1.0
    offset = timedelta(minutes=0)
    if (fld.lower() == "q"):
        query = "id:%s+record://%s/qr//ir-month/hrxzzazd/" % (code,code)
        offset = timedelta(minutes=30)
        scale = 1000.0
    elif (fld.lower() == "fb"):
        query = "id:%s+record://%s/hf//ir-month/irxzzazd/" % (code,code)
    elif (fld.lower() == "tw"):
        query = "id:%s+record://%s/ht//ir-month/irxzzazd/" % (code,code)

    qdata = { "k" : query,
              "sd" : start.day,
              "sm" : start.month,
              "sy" : start.year,
              "ed" : end.day,
              "em" : end.month,
              "ey" : end.year,
              "of" : "Text+Space-Delimited",
              "et" : "Screen",
              "dc" : "One+Column" }

    params = urllib.urlencode(qdata)
    url = "%s?%s" % ( urlbase, params )
    sys.stderr.write("Trying WMD for %s, url: \"%s\"\n" % (code, url))
    f = urllib2.urlopen(url)

    lnum = 0

    outf = open(outname, "w")
    outf.write("# WMD Historic Data (%s): obtained %s\n" %
           (query, today.strftime("%m/%d/%Y %H:%M:%S %Z%z")))

    found = None
    for l in f:
        l.rstrip()
        lnum += 1

        fld = l.split()
        # print "%05d: %s" % (lnum, l)
        if (l.find("Date      Time  Data") >= 0):
            found = 1
            continue

        if (not found):
            continue

        fld = l.split()
        if (len(fld) < 3):
            continue
        dstr = fld[0]
        lt = strptime(dstr.lower(), "%d%b%Y")
        d = datetime(lt.tm_year, lt.tm_mon, lt.tm_mday,
                     hour=0, minute=0)
        (hr, mn) = fld[1].split(":")
        hr = int(hr)
        mn = int(mn)
        d += timedelta(hours=hr, minutes=mn)
        q = float(fld[2])*scale
        d -= offset
        outf.write("%s %9.3f /\n" % (d.strftime(thefmt), q))

    f.close()
    outf.close()

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

now = datetime.today()
download_wmd_old(now, "ihr", "q", "Snake-Flow.dat")
download_wmd_old(now, "mcn", "fb", "MCN-FBE.dat")
