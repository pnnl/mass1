#!/usr/bin/env python
# -*- mode: python; -*-
# -------------------------------------------------------------
# file: gcpud_prd_recent.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November 29, 2012 by William A. Perkins
# Last Change: Thu Jun  3 06:45:08 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

import sys, os
from HTMLParser import HTMLParser
from datetime import *
from time import *

# -------------------------------------------------------------
# class GCPUD_Recent_Parser
# -------------------------------------------------------------
class GCPUD_Recent_Parser(HTMLParser):
    
    def __init__(self):
        HTMLParser.__init__(self)
        self.data_tbl_id = 'DataGrid1'
        self.data_tbl_found = False
        self.data_tbl_indata = False
        self.data_tbl_field = 0
        self.data_tbl = []
        self.data_line = ""
        
    def handle_starttag(self, tag, attrs):
        if tag == 'table':
            if (not self.data_tbl_found):
                for name, value in attrs:
                    if (name == 'id'):
                        if (value == self.data_tbl_id):
                            self.data_tbl_found = True
                            print "Found Table", value
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
            self.data_line += " /"
            self.data_tbl.append(self.data_line)
            self.data_line = ""
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
                    self.data_line += thedate.strftime("%m-%d-%Y %H:%M:%S")
                except:
                    self.data_line += "#bogus"

            if (self.data_tbl_field == 2):
                try:
                    q = float(data)*1000;
                    self.data_line += " %.0f" % (q)
                except:
                    self.data_line += data
                    

    def result(self):
        l = self.data_tbl
        l.reverse()
        l.pop()
        return l

# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
program = os.path.basename(sys.argv[0])
usage = "usage: " + program

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

f = open('gcpud_prd_recent.html')
html = f.read()
f.close()
p = GCPUD_Recent_Parser()
p.feed(html)

print p.result()
