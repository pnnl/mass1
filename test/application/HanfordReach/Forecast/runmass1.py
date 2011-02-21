#!/usr/bin/env python
# -*- mode: python -*-
# -------------------------------------------------------------
# file: runmass1.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 26, 2011 by William A. Perkins
# Last Change: Mon Feb 21 10:24:36 2011 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

import getopt, sys, os
from operator import itemgetter
import urllib
import urllib2
import re
from datetime import *
from time import *
import shutil

thefmt = "%m-%d-%Y %H:%M:%S"

# -------------------------------------------------------------
# download_usgs_recent
#
# Just discharge
# -------------------------------------------------------------
def download_usgs_recent(gage, outname):

    urlbase = "http://nwis.waterdata.usgs.gov/nwis/uv"

    qdata = {'cb_00060' : "on",
             'format' : "rdb",
             'period' : "10",
             'site_no' : gage }

    params = urllib.urlencode(qdata)
    url = "%s?%s" % ( urlbase, params )
    sys.stderr.write("Trying USGS gage %s, url: \"%s\"\n" % (gage, url))
    f = urllib2.urlopen(url)

    lastdate = None
    lastz = None
    lnum = 0

    outf = open(outname, "w")
    outf.write("# USGS Recent Data (%s): obtained %s\n" %
           (gage, datetime.today().strftime("%m/%d/%Y %H:%M:%S %Z%z")))

    for l in f:
        l.rstrip()
        lnum += 1

        if (len(l) <= 0):
            continue

        if (l.find("USGS") == 0):
            fld = l.split()

            if (len(fld) < 5):
                continue

            z = float(fld[5])

            dstr = fld[2] + " " + fld[3] + " " + fld[4]
            lt = strptime(dstr, "%Y-%m-%d %H:%M %Z")
            thedate = datetime(lt.tm_year, lt.tm_mon, lt.tm_mday,
                               hour=lt.tm_hour, minute=lt.tm_min, second=lt.tm_sec)
            lastdate = thedate
            lastz = z

            # FIXME: Daylight Savings Time

            outf.write("%s %.2f /\n" % (thedate.strftime(thefmt), z))

    if (lastdate):
        d = lastdate + timedelta(days=365)
        outf.write("%s %.2f /\n" % (d.strftime(thefmt), z))


    f.close()
    outf.close()
    return lastdate

# -------------------------------------------------------------
# download_wmd_recent
# -------------------------------------------------------------
def download_wmd_recent(code, fld, outname):

    urlbase = "http://www.nwd-wc.usace.army.mil/ftppub/project_data/hourly/%s_%ddaysback.txt"

    i0 = 0
    i1 = 0
    scale = 1.0
    outdt = timedelta(0)
    if (fld.lower() == "q"):
        i0 = 19
        i1 = 28
        scale = 1000.0
        outdt = timedelta(minutes=-30)
    elif (fld.lower() == "fb"):
        i0 = 43
        i1 = 52
    elif (fld.lower() == "tw"):
        i0 = 52
        i1 = 61

    if (i0 == i1):
        return None

    daysback = range(8)
    daysback.reverse()

    lastdate = None
    lastz = None

    outf = open(outname, "w")
    
    outf.write("# WMD Recent Data (%s, %s): obtained %s\n" %
           (code.lower(), fld.lower(), datetime.today().strftime("%m/%d/%Y %H:%M:%S %Z%z")))
    for d in daysback:

        url = ( urlbase % ( code.lower(), d ))

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

            # line 2 has the date at the end of the line (don't know where exactly)
            if (lnum == 2):
                fld = l.split()
                dstr = fld[-3] + " " + fld[-2] + " " + fld[-1] + " 00"
                lt = strptime(dstr, "%B %d, %Y %H")
                thedate = datetime(lt.tm_year, lt.tm_mon, lt.tm_mday, 0, 0, 0, 0)

            # skip blank lines
            if (len(l) == 0):
                continue

            # the data starts after line 7
            if (lnum > 7):

                # the line with TOT indicates the end of data
                if (l.find("TOT") >= 0):
                    break

                try:
                    hr = int(l[0:7])
                except ValueError:
                    continue

                zstr = l[i0:i1]
                try:
                    z = float(zstr)
                except ValueError:
                    continue

                z = z * scale

                dt = timedelta(hours=hr)
                # FIXME: Daylight Savings Time
                lastdate = thedate + dt
                d = lastdate + outdt
                lastz = z
                outf.write("%s %.2f /\n" % (d.strftime(thefmt), z));

        f.close()

    d = lastdate + timedelta(days=365)
    outf.write("%s %.2f /\n" % (d.strftime(thefmt), lastz))
    outf.close()

        # return the last available date, data
    return (lastdate, lastz)

# -------------------------------------------------------------
# prep_boundary_conditions
# -------------------------------------------------------------
def prep_boundary_conditions():
    download_usgs_recent("12510500", "Yakima-Flow.dat")
    download_wmd_recent("mcn", "fb", "MCN-FBE.dat")
    (lastdate, lastq) = download_wmd_recent("prd", "q",  "PRD-Qtotal.dat")
    download_wmd_recent("ihr", "q",  "Snake-Flow.dat")
    return lastdate, lastq

# -------------------------------------------------------------
# run_mass1
# -------------------------------------------------------------
def run_mass1(now):
    simstart = now - timedelta(days=7, minutes=30)
    simstart = simstart.replace(minute=0, second=0)
    simend = now + timedelta(days=1, minutes=30)
    simend = simend.replace(minute=0, second=0)

    cmd = "sed -e \"s/@STARTDATE@/%s/g\" -e \"s/@STARTTIME@/%s/g\" -e \"s/@ENDDATE@/%s/g\" -e \"s/@ENDTIME@/%s/g\" mass1.cfg.base > mass1.cfg"
    
    retval = os.system(cmd % (simstart.strftime("%m-%d-%Y"), simstart.strftime("%H:%M:%S"),
                              simend.strftime("%m-%d-%Y"), simend.strftime("%H:%M:%S")))
    if (retval != 0):
        return retval

    mass1 = "./mass1"
    if (os.environ["MODEL"]):
        mass1 = os.environ["MODEL"]
        
    retval = os.system(mass1)
    
    if (retval != 0):
        return retval

    return 0

# -------------------------------------------------------------
# do_plots
# -------------------------------------------------------------
def do_plots(now):
    pltstart = now - timedelta(days=1, minutes=30)
    pltstart = pltstart.replace(minute=0, second=0)
    pltend = now + timedelta(hours=12, minutes=30)
    pltend = pltend.replace(minute=0, second=0)

    cmd = "sed -e \"s/@PSTART@/%s/g\" -e \"s/@PEND@/%s/g\" -e \"s/@NOW@/%s/g\" stage.gp.base > stage.gp"
    os.system(cmd % (pltstart.strftime(thefmt), pltend.strftime(thefmt),
                     now.strftime(thefmt)))
    os.system("gnuplot stage.gp")

    cmd = "sed -e \"s/@PSTART@/%s/g\" -e \"s/@PEND@/%s/g\" -e \"s/@NOW@/%s/g\" discharge.gp.base > discharge.gp"
    os.system(cmd % (pltstart.strftime(thefmt), pltend.strftime(thefmt),
                     now.strftime(thefmt)))
    os.system("gnuplot discharge.gp")


# -------------------------------------------------------------
# read_quads
# -------------------------------------------------------------
def read_quads():
    
    qname = "quadrm.txt"

    sys.stderr.write("Attempting to open \"%s\"\n" % (qname))
    try:
        f = open(qname)
    except:
        sys.stderr.write("unable to open \"%s\"\n" % (qname))

    sys.stderr.write("\"%s\" successfully opened\n" % (qname))
    thequads = []
    for l in f:
        l.rstrip()
        fld = l.split('|')

        try:
            box = int(fld[0])
            rm = float(fld[3])
        except ValueError:
            continue

        t = (box, rm)
        thequads.append(t)
    f.close()
    return (thequads)
        
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
def read_next_profile(profile, first):

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
            if (pdatetime < first):
                continue
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
# format_profiles
# -------------------------------------------------------------
def format_profiles(now, lastprddate, lastprdq, outname):
    
    quads = read_quads()

    pfile = open("profile1.out", "r")
    pstart = now - timedelta(days=1, minutes=30)
    pstart = pstart.replace(minute=0, second=0)

    pdata = []
    while (True):
        (pdatetime, profile) = read_next_profile(pfile, pstart)
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

    pdata.sort(key=itemgetter(1,0), reverse=True)

    pout = []
    for p in pdata:
        (pdatetime, box, rm, e, q) = p

        if (now < pdatetime):
            if (abs(q - lastprdq)/lastprdq < 0.00005 ):
                #sys.stdout.write("Skipping %d, %s, %.2f\n" % (box, pdatetime.strftime(thefmt), q))
                continue
            else:
                pout.append(p)
        else:
            pout.append(p)

    pdata = None

    pout.sort(key=itemgetter(1,0), reverse=False)

    ofile = open(outname, "w")

    ofile.write("# Processing Date: %s\n" % (now.strftime(thefmt)))
    ofile.write("# Last PRD Q Date: %s\n" % (lastprddate.strftime(thefmt)))
    ofile.write("# Last PRD Q: %.1f\n" % (lastprdq))
    for p in pout:
        (pdatetime, box, rm, e, q) = p
        ofile.write("%s, %d, %.2f, %.2f, %.2f\n" %
                    (pdatetime.strftime(thefmt),
                     box, rm, e, q))

    ofile.close()
    return
    
# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
program = os.path.basename(sys.argv[0])
usage = "usage: " + program

download = 1

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
try:
    opts, args = getopt.getopt(sys.argv[1:], "h?n")
except getopt.GetoptError:
    sys.stderr.write(usage + "[-n] \n")
    sys.exit(2)

for o, a in opts:
    if (o == "-h" or o == "-?"):
        sys.stderr.write(usage + "\n")
        sys.exit(0)
    if (o == "-n"):
        download = None

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

now = datetime.now()

if (download):
    try:
        lastprddate, lastprdq = prep_boundary_conditions()
    except:
        sys.stderr.write("Error setting boundary conditions\n")
        sys.exit(3)

try:
    sys.stderr.write("Running MASS1... \n")
    notok = run_mass1(now)
    if (notok):
        sys.stderr.write("Error running MASS1\n")
        sys.exit(3)
except:
    sys.stderr.write("Error running MASS1\n")
    sys.exit(3)

#try:
sys.stderr.write("Formatting output...\n")
format_profiles(now, lastprddate, lastprdq, "results.txt")
do_plots(lastprddate)
#except:
#    sys.stderr.write("Error formatting output\n")
#    sys.exit(3)

shutil.copy("results.txt", "/projects/hanford_forecast/current/mass1-current.csv")