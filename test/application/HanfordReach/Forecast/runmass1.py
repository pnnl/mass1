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
# Last Change: 2015-09-29 09:04:20 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

import sys, os

# because we run this on stupid Redhat systems, we need to make sure
# the Python is from this century
if sys.version_info < (2, 5):
    raise "must use python 2.5 or greater"

from optparse import OptionParser
from operator import itemgetter
import urllib
import urllib2
import httplib
from HTMLParser import HTMLParser
import re
from datetime import *
from time import *
import shutil

thefmt = "%m-%d-%Y %H:%M:%S"

# -------------------------------------------------------------
# flatline
#
# reads the specified BC file and flatlines from the end for a year,
# if needed.  The last "real" date and value are extracted and
# returned.
# -------------------------------------------------------------
def flatline(bcname):

    sys.stderr.write("%s: flat lining ...\n" % (bcname))
    
    bcfile = open(bcname, "r")
    lastdate = []
    lastz = []
    line = 0
    for l in bcfile:
        l.rstrip()
        line += 1

        if (line == 1):
            continue
        if (len(l) <= 0):
            continue

        fld = l.split()
        dstr = fld[0] + " " + fld[1]
        lt = strptime(dstr, thefmt)
        thedate = datetime(lt.tm_year, lt.tm_mon, lt.tm_mday,
                           hour=lt.tm_hour, minute=lt.tm_min, second=lt.tm_sec)

        lastdate.append(thedate)

        z = float(fld[2])
        lastz.append(z)

        if (len(lastdate) > 2):
            lastdate.pop(0)
            lastz.pop(0)
            
    bcfile.close()

    if (len(lastdate) > 1):
        sys.stderr.write("%s: last date found: %s\n" %
                         ( bcname, lastdate[1].strftime(thefmt)))

        onemonth =  timedelta(days=30)
        dt = lastdate[1] - lastdate[0]
        if (dt < onemonth):
            d = lastdate[1] + timedelta(days=365)
            z = lastz[1]
            bcfile = open(bcname, "a")
            sys.stderr.write("%s: flat lining to: %s\n" %
                             ( bcname, d.strftime(thefmt)))
            bcfile.write("%s %.2f /\n" % (d.strftime(thefmt), z))
            lastdate.pop(0)
            lastz.pop(0)
        else:
            d = lastdate[1]
            sys.stderr.write("%s: already flat lined to: %s\n" %
                             ( bcname, d.strftime(thefmt)))
            
    else:
        return None, None
    return (lastdate[0], lastz[0])
    

# -------------------------------------------------------------
# download_usgs_recent
#
# Just discharge
# -------------------------------------------------------------
def download_usgs_recent(now, gage, outname, defq):

    today = datetime.now()
    days = (today - now).days + 10

    urlbase = "http://nwis.waterdata.usgs.gov/nwis/uv"

    qdata = {'cb_00060' : "on",
             'format' : "rdb",
             'period' : days,
             'site_no' : gage }

    params = urllib.urlencode(qdata)
    url = "%s?%s" % ( urlbase, params )
    sys.stderr.write("Trying USGS gage %s, url: \"%s\"\n" % (gage, url))

    lastdate = None
    lastz = None
    lnum = 0
    rcount = 0

    outf = open(outname, "w")
    try:
        f = urllib2.urlopen(url)

        outf.write("# USGS Recent Data (%s): obtained %s\n" %
                   (gage, today.strftime("%m/%d/%Y %H:%M:%S %Z%z")))

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
                rcount += 1

        f.close()
    except Exception, detail:
        sys.stderr.write("USGS gage %s, reading url failed: %s\n" % 
                         (gage, detail))

    if rcount <= 0:
        outf.write("%s %.2f /\n" % ("01-01-1900 00:00:00", defq))
        outf.write("%s %.2f /\n" % ("01-01-3000 00:00:00", defq))
        
    outf.close()

    if (lastdate):
        (lastdate, lastz) = flatline(outname)

    return (lastdate, lastz)

# -------------------------------------------------------------
# download_usgs_old
#
# Just daily discharge, if now is more than 100 days in the past
# -------------------------------------------------------------
def download_usgs_old(now, gage, outname):

    today = datetime.now()
    start = now - timedelta(days=14)
    end = now + timedelta(days=1)

    urlbase = "http://nwis.waterdata.usgs.gov/nwis/dv"

    qdata = {'cb_00060' : "on",
             'format' : "rdb",
             'site_no' : gage,
             'begin_date' : start.strftime("%Y-%m-%d"),
             'end_date' : end.strftime("%Y-%m-%d"),
             'format' : 'rdb',
             'referred_module' : 'sw'
             }

    params = urllib.urlencode(qdata)
    url = "%s?%s" % ( urlbase, params )
    sys.stderr.write("Trying USGS gage %s, url: \"%s\"\n" % (gage, url))
    f = urllib2.urlopen(url)

    lastdate = None
    lastz = None
    lnum = 0

    outf = open(outname, "w")
    outf.write("# USGS Historic Data (%s): obtained %s\n" %
           (gage, today.strftime("%m/%d/%Y %H:%M:%S %Z%z")))

    for l in f:
        l.rstrip()
        lnum += 1

        if (len(l) <= 0):
            continue

        if (l.find("USGS") == 0):
            fld = l.split()

            if (len(fld) < 4):
                continue

            z = float(fld[3])

            dstr = fld[2] + " 12:00"
            lt = strptime(dstr, "%Y-%m-%d %H:%M")
            thedate = datetime(lt.tm_year, lt.tm_mon, lt.tm_mday,
                               hour=lt.tm_hour, minute=lt.tm_min, second=lt.tm_sec)
            lastdate = thedate
            lastz = z

            # FIXME: Daylight Savings Time

            outf.write("%s %.2f /\n" % (thedate.strftime(thefmt), z))

    f.close()
    outf.close()
    if (lastdate):
        (lastdate, lastz) = flatline(outname)

    return (lastdate, lastz)

# -------------------------------------------------------------
# download_wmd_recent
# -------------------------------------------------------------
def download_wmd_recent(now, code, thefld, outname, defvalue):

    urlbase = "http://www.nwd-wc.usace.army.mil/ftppub/project_data/hourly/%s_%ddaysback.txt"
    # urlbase = "http://www.nwd-wc.usace.army.mil/ftppub/project_data/hourly/%s_%ddaysback_cwms.txt"

    i0 = 0
    i1 = 0
    scale = 1.0
    outdt = timedelta(0)
    if (thefld.lower() == "q"):
        i0 = 19
        i1 = 28
        scale = 1000.0
        outdt = timedelta(minutes=-30)
    elif (thefld.lower() == "fb"):
        i0 = 43
        i1 = 52
    elif (thefld.lower() == "tw"):
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
           (code.lower(), thefld.lower(), datetime.now().strftime("%m/%d/%Y %H:%M:%S %Z%z")))
    outf.write("%s %.2f /\n" % ("01-01-1900 00:00:00", defvalue))
    for d in daysback:

        url = ( urlbase % ( code.lower(), d ))
        header = {"pragma-directive" : "no-cache"}
        req = urllib2.Request(url, headers=header)

        try:
            f = urllib2.urlopen(req)
            # print f.headers
        except:
            sys.stderr.write("unable to get URL: %s\n" % (url))
            continue

        thedate = None
        lnum = 0
        outlines = 0
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

                if (thefld.lower() == "q" and z <= 0.0):
                    continue

                z = z * scale

                dt = timedelta(hours=hr)
                # FIXME: Daylight Savings Time
                lastdate = thedate + dt
                outdate = lastdate + outdt
                lastz = z
                if (thedate <= now):
                    #outf.write("%s\n" % (l))
                    outf.write("%s %.2f /\n" % (outdate.strftime(thefmt), z));
                    outlines += 1

        f.close()
        if (outlines <= 0):
            thedate = now - timedelta(days=d)
            outf.write("%s %.2f /\n" % (thedate.strftime(thefmt), defvalue));
        
    outf.close()
    if (lastdate):
        (lastdate, lastz) = flatline(outname)

    # return the last available date, data
    return (lastdate, lastz)

# -------------------------------------------------------------
# download_wmd_old
# -------------------------------------------------------------
def download_wmd_old(now, code, thefld, outname):

    urlbase = "http://www.nwd-wc.usace.army.mil/perl/dataquery.pl"
    urlbase = "http://www.nwd-wc.usace.army.mil/cgi-bin/dataquery.pl"
    
    today = datetime.now()
    start = now - timedelta(days=10)
    end = now + timedelta(days=1)

    scale = 1.0
    offset = timedelta(minutes=0)
    if (thefld.lower() == "q"):
        query = "id:%s+record://%s/qr//ir-month/hrxzzazd/" % (code,code)
        offset = timedelta(minutes=30)
        scale = 1000.0
    elif (thefld.lower() == "fb"):
        query = "id:%s+record://%s/hf//ir-month/irxzzazd/" % (code,code)
    elif (thefld.lower() == "tw"):
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

        # skip blank lines
        if (len(l) == 0):
            continue

        if (l.find("Date") == 0):
            found = True
            continue

        # the data starts after the line that starts w/ "Date"
        if (found):

            # the dash line indicates the end of data
            if (l.find("-----------------") >= 0):
                break

            # the data line should start with an interger (day number)
            try:
                hr = int(l[0:2])
            except:
                continue

            fld = l.split();

            # don't bother if there's no value
            if (len(fld) != 3):
                continue

            # reformat the date
            dstr = fld[0]
            lt = strptime(dstr.lower(), "%d%b%Y")
            d = datetime(lt.tm_year, lt.tm_mon, lt.tm_mday,
                         hour=0, minute=0)
            # have to do this because of "24:00" times
            (hr, mn) = fld[1].split(":")
            hr = int(hr)
            mn = int(mn)
            d += timedelta(hours=hr, minutes=mn)
            d -= offset

            try:
                z = float(fld[2])
            except ValueError:
                continue

            z = z * scale

            rec = "%s %9.3f /\n" % (d.strftime(thefmt), z)
            # sys.stderr.write(rec);
            outf.write(rec)

    f.close()
    outf.close()
    return flatline(outname)

# -------------------------------------------------------------
# class RedirectHandler
# -------------------------------------------------------------
class RedirectHandler(urllib2.HTTPRedirectHandler):
    def http_error_301(self, req, fp, code, msg, headers):  
        result = urllib2.HTTPRedirectHandler.http_error_301( 
            self, req, fp, code, msg, headers)              
        result.status = code                                 
        raise Exception("Permanent Redirect: %s" % 301)

    def http_error_302(self, req, fp, code, msg, headers):
        result = urllib2.HTTPRedirectHandler.http_error_302(
            self, req, fp, code, msg, headers)              
        result.status = code                                
        raise Exception("Temporary Redirect: %s" % 302)

# -------------------------------------------------------------
# class GCPUD_Recent_Parser
# -------------------------------------------------------------
class GCPUD_Recent_Parser(HTMLParser):
    
    def __init__(self, mindate):
        HTMLParser.__init__(self)
        self.mindate = mindate
        # print type(self.mindate)
        self.data_tbl_id = 'DataGrid1'
        self.data_tbl_found = False
        self.data_tbl_indata = False
        self.data_tbl_field = 0
        self.data_tbl = []
        self.data_line = []
        
    def handle_starttag(self, tag, attrs):
        if tag == 'table':
            if (not self.data_tbl_found):
                for name, value in attrs:
                    if (name == 'id'):
                        if (value == self.data_tbl_id):
                            self.data_tbl_found = True
                            # print "Found Table", value
            else:
                self.data_tbl_found = False

        if tag == 'td' and self.data_tbl_found:
            self.data_tbl_field += 1
            self.data_tbl_indata = True


    def handle_endtag(self, tag):
        if tag == 'table' and self.data_tbl_found:
            self.data_tbl_found = False
            self.data_tbl_indata = False

        if tag == 'tr' and self.data_tbl_found:
            if (len(self.data_line) > 0):
                if type(self.data_line[0]) is datetime:
                    if (self.data_line[0] > self.mindate):
                        self.data_tbl.append(self.data_line)
            self.data_line = []
            self.data_tbl_field = 0

        if tag == 'td' and self.data_tbl_indata:
            self.data_tbl_indata = False

    def handle_data(self, data):
        if self.data_tbl_indata:
            if (self.data_tbl_field == 1):
                try:
                    lt = strptime(data, "%m/%d/%Y %H:%M")
                    thedate = datetime(lt.tm_year, lt.tm_mon, lt.tm_mday,
                                       hour=lt.tm_hour, minute=lt.tm_min, second=lt.tm_sec)
                    self.data_line.append(thedate)
                except:
                    self.data_line.append("#bogus")

            if (self.data_tbl_field == 2):
                try:
                    q = float(data)*1000;
                    self.data_line.append(q)
                except:
                    self.data_line.append(data)
                    

    def output(self, f):
        l = self.data_tbl
        l.reverse()
        for r in l:
            thedate = r[0] - timedelta(minutes=30)
            f.write("%s %9.1f /\n" % (thedate.strftime(thefmt), r[1]))
        return


# -------------------------------------------------------------
# download_prdq_current
#

# Gets PRD discharge directly from GCPUD "72 hour" data. Writes to an
# open file.

# -------------------------------------------------------------
def download_prdq_current(lastdate, outfile):
    urlbase = "http://grantpud2.org/data/water/WQMInfo.php"
    params = urllib.urlencode( { "SITE_PID" : 2,
                                 "SITE_TITLE" : "Priest Rapids Tailrace" } )
    url = "%s?%s" % ( urlbase, params )
    header = {"pragma-directive" : "no-cache"}
    req = urllib2.Request(url, headers=header)

    sys.stderr.write("Trying GCPUD, url: \"%s\"\n" % (url))
    
    f = urllib2.urlopen(req)
    # print f.headers
    html = f.read()
    f.close()
    p = GCPUD_Recent_Parser(lastdate)
    p.feed(html)
    p.output(outfile)
    return
    

# -------------------------------------------------------------
# download_prdq_recent
#
# Gets PRD discharge directly from GCPUD.  Usually current through
# midnight the previous day.
# -------------------------------------------------------------
def download_prdq_recent(now, outname):
    urlbase = "http://grantpud2.org/data/water/fixed/%Y/csvform/%m%d%y.csv"
    m = now.month
    d = now.day
    y = now.year

    offset = timedelta(minutes=30)

    start = datetime(y, m, d, 0, 0)
    start -= timedelta(days=10)

    # Bad things happen if we try to get "fixed" data that is not
    # there.  We need to use as much of the "72-hour" data as
    # possible, so check to see if we can

    ravail = datetime.now() - timedelta(hours=72)
    finish = ravail
    if (now < finish):
        finish = now

    outf = open(outname, "w")
    outf.write("# Priest Rapids Discharge, retrieved %s from www.gcpud.org\n" %
           (datetime.now().strftime("%m/%d/%Y %H:%M:%S %Z%z")))
    
    lastdate = start
    while (start <= finish):
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
                sys.stderr.write("%s: %d: date not in proper column\n" % (url, lnum))
                continue

            dstr = "%s %04d" % (fld[1-ioff], int(fld[2-ioff]))
            lt = strptime(dstr, "%m/%d/%Y %H%M")
            d = datetime(lt.tm_year, lt.tm_mon, lt.tm_mday,
                         hour=lt.tm_hour, minute=lt.tm_min, second=lt.tm_sec)
            qfld = fld[19-ioff]
            qfld.rstrip()
            if (len(qfld) > 0):
                q = float(qfld)*1000.0
                if (d <= now):
                    lastdate = d
                    d -= offset
                    outf.write("%s %9.1f /\n" % (d.strftime(thefmt), q))
        f.close()
        print lastdate, now

    # get the most recent if now is within 72 hours of the current
    # time, but warn if there's a gap

    if (now >= ravail):
        if (lastdate < ravail):
            sys.stderr.write("warning: PRD discharge data gap from %s to %s (approximate)\n" % 
                             (lastdate, ravail))
        download_prdq_current(lastdate, outf)
    outf.close()
    return flatline(outname)


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

    cmd = "sed -e \"s/@PSTART@/%s/g\" -e \"s/@PEND@/%s/g\" -e \"s/@NOW@/%s/g\" plotall.gp | gnuplot"
    os.system(cmd % (pltstart.strftime(thefmt), pltend.strftime(thefmt),
                     now.strftime(thefmt)))


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

        (prm1, pq1, pe1, ptw1) = prof[idx-1]
        (prm0, pq0, pe0, ptw0) = prof[idx]
        
        if (rm == prm0):
            q = pq0
            e = pe0
            tw = ptw0
        else:
            f = (rm - prm0)/(prm1-prm0)
            q = f*(pq1 - pq0) + pq0
            e = f*(pe1 - pe0) + pe0
            tw = f*(ptw1 - ptw0) + ptw0

        t = (box, rm, q, e, tw)
        qtemp.append(t)
    return qtemp
        
# -------------------------------------------------------------
# read_next_profile
#
# profile is an open MASS1 profile output file
# after is a date time which is the earliest date to extract from profile
#
# the date of the profile read and a list of tuples (rm, wsel, q, topw) is returned
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
            thetuple = (float(fld[3]), float(fld[4]), float(fld[5]), float(fld[14]))
            theprofile.append(thetuple)
            continue

    return (pdatetime, theprofile)


# -------------------------------------------------------------
# format_profiles
# -------------------------------------------------------------
def format_profiles(now, lastprddate, lastprdq, outname):
    
    quads = read_quads()

    pfile = open("profile1.out", "r")
    pstart = now - timedelta(days=1, hours=12, minutes=30)
    pstart = pstart.replace(minute=0, second=0)

    pdata = []
    while (True):
        (pdatetime, profile) = read_next_profile(pfile, pstart)
        profile.reverse()
        if (pdatetime):
            qtemp = interpolate_profile(profile, quads)
            for t in qtemp:
                (box, rm, e, q, tw) = t
                t = (pdatetime, box, rm, e, q, tw)
                pdata.append(t)
        else:
            break
            
    pfile.close()

    pdata.sort(key=itemgetter(1,0), reverse=True)

    pout = []
    for p in pdata:
        (pdatetime, box, rm, e, q, tw) = p

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
        (pdatetime, box, rm, e, q, tw) = p
        ofile.write("%s, %d, %.2f, %.2f, %.2f, %.2f\n" %
                    (pdatetime.strftime(thefmt),
                     box, rm, e, q, tw))

    ofile.close()
    return
    
# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
program = os.path.basename(sys.argv[0])
usage = "usage: " + program

dodownload = 1
dorun = 1
doplot = 1

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
usage = "Usage: %prog [options]"
parser = OptionParser()
parser.add_option("-d", "--dry-run",
                  action="store_false", dest="upload", default=True,
                  help="do everything, but don't upload the results")
parser.add_option("-r", "--disable-run",
                  action="store_false", dest="run", default=True,
                  help="do not run")
parser.add_option("-b", "--disable-bc",
                  action="store_false", dest="download", default=True,
                  help="use existing bc files, do not download")
parser.add_option("-p", "--disable-plots",
                  action="store_false", dest="plot", default=True,
                  help="do not make any plots")
parser.add_option("-f", "--disable-flatline", 
                  action="store_false", dest="flatline", default=True,
                  help="do not flat line bc files")
parser.add_option("-N", "--now", dest="now", action="store", 
                  help="set the \"now\" time (MM/DD/YYYY HH:MM)")
parser.add_option("-v", "--verbose",
                  dest="verbose", action="store_true", default=False,
                  help="show what's going on")
(options, args) = parser.parse_args()

today = datetime.now()
now = today
if (options.now):
    try:
        lt = strptime(options.now, "%m/%d/%Y %H:%M")
        now = datetime(lt.tm_year, lt.tm_mon, lt.tm_mday, lt.tm_hour, lt.tm_min, 0, 0)
        if (now > today):
            sys.stderr.write("Error: --now argument (%s) in future\n" % (options.now))
            sys.exit(3)
    except:
        sys.stderr.write("Error parsing --now argument (%s)\n" % (options.now))
        sys.exit(3)

doverbose = options.verbose
dodownload = options.download
dorun = options.run
doplot = options.plot
doupload = options.upload
doflatline = options.flatline

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

opener = urllib2.build_opener(RedirectHandler)
urllib2.install_opener(opener)

try:
    if (dodownload):
        delta = datetime.now() - now
        if (delta.days < 2):
            # (lastprddate, lastprdq) = download_wmd_recent(now, "prd", "q", "PRD-Qtotal.dat", 40000.0)
            (lastprddate, lastprdq) = download_prdq_recent(now, "PRD-Qtotal.dat")
            download_wmd_old(now, "ihr", "q", "Snake-Flow.dat")
            download_wmd_old(now, "mcn", "fb", "MCN-FBE.dat")
            # download_wmd_recent(now, "mcn", "fb", "MCN-FBE.dat", 340.0)
            # download_wmd_recent(now, "ihr", "q",  "Snake-Flow.dat", 9500.0)
        else:
            (lastprddate, lastprdq) = download_prdq_recent(now, "PRD-Qtotal.dat")
            download_wmd_old(now, "ihr", "q", "Snake-Flow.dat")
            download_wmd_old(now, "mcn", "fb", "MCN-FBE.dat")
        if (delta.days > 90):
            download_usgs_old(now, "12510500", "Yakima-Flow.dat")
        else:
            download_usgs_recent(now, "12510500", "Yakima-Flow.dat", 3000)
    else:
        if (doflatline):
            lastprddate, lastprdq = flatline("PRD-Qtotal.dat")
            flatline("Yakima-Flow.dat")
            flatline("Snake-Flow.dat")
            flatline("MCN-FBE.dat")
            sys.stderr.write("Last PRD Q = %.1f kcfs @ %s\n" %
                             (lastprdq, lastprddate.strftime(thefmt)))
except Exception, detail:
    sys.stderr.write("Error setting boundary conditions:%s\n" %
                     (detail))
    sys.exit(3)

try:
    if (dorun):
        sys.stderr.write("Running MASS1... \n")
        notok = run_mass1(now)
        if (notok):
            sys.stderr.write("Error running MASS1\n")
            sys.exit(3)
except:
    sys.stderr.write("Error running MASS1\n")
    sys.exit(3)


try:
    sys.stderr.write("Formatting output...\n")
    format_profiles(now, lastprddate, lastprdq, "results.txt")
    if (doplot):
        do_plots(lastprddate)
except:
    sys.stderr.write("Error formatting output\n")
    sys.exit(3)

