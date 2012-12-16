#!/usr/bin/env python2
# -*- mode: python -*-
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
from HTMLParser import HTMLParser

thefmt = "%m-%d-%Y %H:%M:%S"

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
    urlbase = "http://gcpud.org/data/water/WQMInfo.php"
    params = urllib.urlencode( { "SITE_PID" : 2,
                                 "SITE_TITLE" : "Priest Rapids Tailrace" } )
    url = "%s?%s" % ( urlbase, params )
    sys.stderr.write("Trying GCPUD, url: \"%s\"\n" % (url))
    f = urllib2.urlopen(url)
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
    urlbase = "http://www.gcpud.org/data/water/fixed/%Y/csvform/%m%d%y.csv"
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
    return lastdate


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
# now = datetime(2011, 3, 1, 8, 0)
opener = urllib2.build_opener(RedirectHandler)
urllib2.install_opener(opener)

lastdate = download_prdq_recent(now, "PRD-Qtotal.dat")
