#!/usr/bin/python
# -*- mode: python; py-which-shell: "python";-*-
# -------------------------------------------------------------
# file: profile2cgns.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December  3, 2007 by William A. Perkins
# Last Change: Mon Feb 11 15:19:48 2008 by William A. Perkins <d3g096@mcperk.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

import sys, os
import fileinput
from optparse import OptionParser
import numarray
import CGNS
import mx.DateTime

# -------------------------------------------------------------
# striptitle
#
# This routine finds the important parts of a profile title -- date,
# time, and number of points -- then returns them.  
# -------------------------------------------------------------
def striptitle(line):
    i = line.index('Date:')             # throws an exception if not found
    date = line[i+6:i+16]
    time = line[i+24:i+32]
    i = line.index('=')
    num = int(line[i+1:len(line)])
    return (date, time, num)

# -------------------------------------------------------------
# thesign
#
# If i is zero, returns -1, otherwise returns 1
# -------------------------------------------------------------
def thesign(i):
    if (i == 0):
        return -1
    return 1

# -------------------------------------------------------------
# buildzone
#
# This builds and writes a structured 3D zone using the specified
# X-coordinate.  The x-coordinate defines the cell boundaries.  The
# cells are centered on zero with a width of 2 in the y and z
# direction.  Returns the zone id.
# -------------------------------------------------------------
def buildzone(file, baseidx, x1d):
    width = 120.0
    isize = x1d.shape[0]
    isize -= 1
    jsize = 1
    ksize = 1

    size = range(9)

    size[0] = isize + 1
    size[1] = jsize + 1
    size[2] = ksize + 1

    size[3] = isize
    size[4] = jsize
    size[5] = ksize

    size[6] = 0
    size[7] = 0
    size[8] = 0

    zoneidx = file.zonewrite(baseidx, 'Zone', size, CGNS.Structured)

    x3d = numarray.array(numarray.ones((ksize+1, jsize+1, isize+1), numarray.Float))
    y3d = numarray.array(numarray.ones((ksize+1, jsize+1, isize+1), numarray.Float))
    z3d = numarray.array(numarray.ones((ksize+1, jsize+1, isize+1), numarray.Float))
    
    for k in range(ksize+1):
        for j in range(jsize+1):
            for i in range(isize+1):
                x3d[k,j,i] = x1d[i]
                y3d[k,j,i] = thesign(j)*width*0.5
                z3d[k,j,i] = thesign(k)*width*0.5

    file.coordwrite(baseidx, zoneidx, CGNS.RealDouble, CGNS.CoordinateX, x3d)
    file.coordwrite(baseidx, zoneidx, CGNS.RealDouble, CGNS.CoordinateY, y3d)
    file.coordwrite(baseidx, zoneidx, CGNS.RealDouble, CGNS.CoordinateZ, z3d)
    return zoneidx

# -------------------------------------------------------------
# writefield
# -------------------------------------------------------------
def writefield(file, baseidx, zoneixd, solidx, vname, value):
    n = value.shape[0]
    vout = numarray.array(numarray.ones((n-1,), numarray.Float))
    for i in range(n-1):
        vout[i] = 0.5*(value[i] + value[i+1])
    fidx = file.fieldwrite(baseidx, zoneidx, solidx, CGNS.RealDouble, vname, vout)
    return fidx


# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------

basedate = None

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
parser = OptionParser()

parser.add_option("-s", "--start", type="int", dest="pstart",
                  default=1, metavar="n",
                  help="start with the nth profile in the input")
parser.add_option("-e", "--end", type="int", dest="pend",
                  default=1, metavar="n",
                  help="end with the nth profile in the input")
parser.add_option("-u", "--units", type="choice", dest="dunits",
                  choices=("mile", "foot"), default="mile", metavar="mile|foot",
                  help="specify the units of the profile distance column")
parser.add_option("-B", "--base-date", type="string", metavar="date", dest="basedate",
                  help="Date/time representing time = 0s")
parser.set_usage("usage: %prog [options] profile.dat output.cgns") 

(options, args) = parser.parse_args()

dtometer = 1.0
if (options.dunits == 'foot'):
    dtometer = 0.3048
elif (options.dunits == 'mile'):
    dtometer = 0.3048*5280.0
else:
    parser.error('unknown distance units (should not happen)')
    
if (options.pstart > options.pend):
    options.pend = options.pstart

if (options.basedate):
    try:
        basedate = mx.DateTime.strptime(options.basedate, "%m-%d-%Y %H:%M:%S")
    except:
        parser.error("specified base date (%s) not understood\n" % options.basedate)
        
if (len(args) < 3):
    parser.error("input and/or output not specified")


profilename = args[0]
cgnsname = args[1]
sollistname = args[2]

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

cgns = CGNS.pyCGNS(cgnsname, CGNS.MODE_WRITE)
baseidx = cgns.basewrite("FromMASS1", 3, 3)

sollist = open(sollistname, "w")

ip = 0
ipt = 0
datetime = None
found1 = None
first = 1
zname = 'Undefined'
zoneidx = None

for line in fileinput.input(args[0]):
    if (line.find('Date:') >= 0):

        if (found1):
            if (first):
                zoneidx = buildzone(cgns, baseidx, x)
                first = None
            solidx = cgns.solwrite(baseidx, zoneidx, zname, CGNS.CellCenter)

            # assuming that X increases in the upstream direction,
            # velocity should be negative
            
            v *= -1.0
            writefield(cgns, baseidx, zoneidx, solidx, CGNS.VelocityX, v)
            v *= 0.0
            writefield(cgns, baseidx, zoneidx, solidx, CGNS.VelocityY, v)
            writefield(cgns, baseidx, zoneidx, solidx, CGNS.VelocityZ, v)
            datetime = mx.DateTime.strptime(zname, "%m-%d-%Y %H:%M:%S")
            delta = None
            if (basedate):
                delta = datetime - basedate
            else:
                delta = datetime - DateTime(datetime.year, 1, 1)
            (d, s) = delta.absvalues()
            
            sollist.write("%12.0f %s %10d # %s\n" %
                          ( (d*86400 + s), cgnsname, solidx, zname))
            
        
        ip += 1
        if (ip > options.pend):
            break
        
        if (ip >= options.pstart and ip <= options.pend):
            found1 = 1
            (date, time, num) = striptitle(line)
            zname = date + ' ' + time
            x = numarray.array(numarray.zeros((num,), numarray.Float))
            v = numarray.array(numarray.zeros((num,), numarray.Float))
            t = numarray.array(numarray.zeros((num,), numarray.Float))
            c = numarray.array(numarray.zeros((num,), numarray.Float))
            # print zname, num
            ipt = 0
        else:
            found1 = None
        continue
    elif (line.find('#') >= 0):
        continue
    elif (len(line.rstrip()) <= 0):
        continue

    if (found1):
        fld = line.split()
        x[ipt] = float(fld[3])*dtometer # convert to m
        v[ipt] = float(fld[6])*0.3048   # convert to m/s
        c[ipt] = float(fld[8])          # units unchanged
        t[ipt] = float(fld[9])          # units unchanged
        ipt += 1
    
cgns.close()
