#!/usr/bin/python
# -*- mode: python; -*-
# -------------------------------------------------------------
# file: profile2cgns.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December  3, 2007 by William A. Perkins
# Last Change: 2017-06-22 11:43:26 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

import sys, os
import fileinput
from optparse import OptionParser
import numarray
import CGNS
import datetime

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
# adjustx
#
# This takes the list of x coordinates and adjusts it so that there
# are no zero length segments.  
# -------------------------------------------------------------
def adjustx(x1d, fudge):
    x1dnew = numarray.array(x1d)
    x1dnew[0] = x1d[0];
    for i in range(1, x1d.shape[0]):
        x1dnew[i] = x1d[i]
        if (x1d[i] == x1d[i-1]):
            x1dnew[i] += fudge
    return x1dnew

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

    dy = width/float(jsize)
    dz = width/float(ksize)

    yorig = -width/2.0
    zorig = -width/2.0

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
                y3d[k,j,i] = yorig + j*dy
                z3d[k,j,i] = zorig + k*dz

    file.coordwrite(baseidx, zoneidx, CGNS.RealDouble, CGNS.CoordinateX, x3d)
    file.coordwrite(baseidx, zoneidx, CGNS.RealDouble, CGNS.CoordinateY, y3d)
    file.coordwrite(baseidx, zoneidx, CGNS.RealDouble, CGNS.CoordinateZ, z3d)
    return (zoneidx, isize, jsize, ksize)

# -------------------------------------------------------------
# writefield
# -------------------------------------------------------------
def writefield(file, baseidx, zoneixd, solidx, vname, value, nx, ny, nz):
    vout = numarray.array(numarray.zeros((nz, ny, value.shape[0]-1), numarray.Float))
    
    for k in range(nz):
        for j in range(ny):
            for i in range(value.shape[0] - 1):
                vout[k,j,i] = 0.5*(value[i] + value[i+1])
    fidx = file.fieldwrite(baseidx, zoneidx, solidx, CGNS.RealDouble, vname, vout)
    return fidx


# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------

basedate = None
fudge = 5.0
verbose = False

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
program = os.path.basename(sys.argv[0])

parser = OptionParser()

parser.add_option("-v", "--verbose", action="store_true", dest="verbose",
                  help="spew lots of messages while processing")
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
parser.add_option("-f", "--fudge", type="float", metavar="factor", dest="fudge",
                  help="Amount to move cross sections in zero length segments, m")
parser.set_usage("usage: %prog [options] profile.dat output.cgns output.list") 

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
        basedate = datetime.datetime.strptime(options.basedate, "%m-%d-%Y %H:%M:%S")
    except:
        parser.error("specified base date (%s) not understood\n" % options.basedate)

if (options.fudge):
    fudge = options.fudge

if (options.verbose):
    verbose = options.verbose
        
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
thedatetime = None
found1 = None
first = 1
zname = 'Undefined'
zoneidx = None

for line in fileinput.input(args[0]):
    if (line.find('Date:') >= 0):

        if (found1):
            if (first):
                x = adjustx(x, fudge)
                (zoneidx, ni, nj, nk) = buildzone(cgns, baseidx, x)
                if (verbose):
                    sys.stderr.write("%s: %s: wrote zone\n" %
                                     (program, cgnsname))
                first = None
            solidx = cgns.solwrite(baseidx, zoneidx, zname, CGNS.CellCenter)

            # assuming that X increases in the upstream direction,
            # velocity should be negative
            
            V *= -1.0
            writefield(cgns, baseidx, zoneidx, solidx, CGNS.VelocityX, V, ni, nj, nk)
            V *= 0.0
            writefield(cgns, baseidx, zoneidx, solidx, CGNS.VelocityY, V, ni, nj, nk)
            writefield(cgns, baseidx, zoneidx, solidx, CGNS.VelocityZ, V, ni, nj, nk)
            writefield(cgns, baseidx, zoneidx, solidx, "Discharge", Q, ni, nj, nk)
            writefield(cgns, baseidx, zoneidx, solidx, "Stage", E, ni, nj, nk)
            writefield(cgns, baseidx, zoneidx, solidx, "Depth", D, ni, nj, nk)
            writefield(cgns, baseidx, zoneidx, solidx, "Temperature", T, ni, nj, nk)
            writefield(cgns, baseidx, zoneidx, solidx, "ShearStress", S, ni, nj, nk)
            thedatetime = datetime.datetime.strptime(zname, "%m-%d-%Y %H:%M:%S")
            delta = None
            if (basedate):
                delta = thedatetime - basedate
            else:
                delta = thedatetime - datetime(thedatetime.year, 1, 1)
            # (d, s) = delta.absvalues()
            s = delta.total_seconds()
            
            sollist.write("%12.0f %s %10d # %s\n" %
                          ( (s), cgnsname, solidx, zname))
            if (verbose):
                sys.stderr.write("%s: %s: wrote solution %d, \"%s\"\n" %
                                 (program, cgnsname, solidx, zname))
            
        
        ip += 1

        if (verbose):
            sys.stderr.write("%s: %s: found profile record %d\n" % (program, args[0], ip))
            
        if (ip > options.pend):
            break
        
        if (ip >= options.pstart and ip <= options.pend):
            found1 = 1
            (date, time, num) = striptitle(line)
            zname = date + ' ' + time
            x = numarray.array(numarray.zeros((num,), numarray.Float))
            V = numarray.array(numarray.zeros((num,), numarray.Float))
            T = numarray.array(numarray.zeros((num,), numarray.Float))
            C = numarray.array(numarray.zeros((num,), numarray.Float))
            Q = numarray.array(numarray.zeros((num,), numarray.Float))
            E = numarray.array(numarray.zeros((num,), numarray.Float))
            S = numarray.array(numarray.zeros((num,), numarray.Float))
            D = numarray.array(numarray.zeros((num,), numarray.Float))
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
        V[ipt] = float(fld[6])*0.3048   # convert to m/s
        C[ipt] = float(fld[8])          # units unchanged
        T[ipt] = float(fld[9])          # units unchanged
        Q[ipt] = float(fld[5])*0.0283168# convert to m^3/s
        E[ipt] = float(fld[4])*0.3048   # convert stage to m
        D[ipt] = float(fld[7])*0.3048   # convert depth to m
        S[ipt] = float(fld[20])*47.880259# convert stress to N
        ipt += 1
    
cgns.close()
