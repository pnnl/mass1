#!/usr/bin/env python
# -*- mode: python -*-
# -------------------------------------------------------------
# file: test.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October 24, 2011 by William A. Perkins
# Last Change: 2018-04-06 12:05:25 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

import sys, os
from optparse import OptionParser
import MASS1

# -------------------------------------------------------------
# read_network
# -------------------------------------------------------------
def read_network(f, manning):
    f.readline()                        # skip first line

    links = []
    basins = []

    upstream = {}
    downstream = {}
    
    lnum = 1
    for l in f:
        lnum += 1
        l.rstrip()
        fld = l.split()
        subbasin = int(fld.pop(0))
        upnode = fld.pop(0)
        dnnode = fld.pop(0)
        length = float(fld.pop(0))
        slope = float(fld.pop(0))
        width = float(fld.pop(0))
        depth = float(fld.pop(0))
        downelev = float(fld.pop(0))
        upelev = float(fld.pop(0))
        areaacc = float(fld.pop(0))
        arealocal = float(fld.pop(0))

        # convert to English units
        length /= 0.3048
        slope /= 100.0
        width /= 0.3048
        depth /= 0.3048
        downelev /= 0.3048
        upelev /= 0.3048
        areaacc *= 107639.1
        arealocal *= 107639.1

        sb = MASS1.Subbasin(subbasin, arealocal)
        basins.append(sb)
        s = MASS1.Section(subbasin, width, depth)
        
        point = MASS1.Point(subbasin, length, upelev, downelev, s)
        point.manning = manning
        link = MASS1.Link(subbasin, slope, point, sb)
        sb.add_link(link)
        
        if (not upstream.has_key(dnnode)):
            upstream[dnnode] = []
        upstream[dnnode].append(link)
        if (not downstream.has_key(upnode)):
            downstream[upnode] = []
        downstream[upnode].append(link)

        links.append(link)

#     for n in downstream.keys():
#         sys.stdout.write("Downstream of %s: " % (n))
#         for l in downstream[n]:
#             sys.stdout.write("%02d, " % (l.id))
#         sys.stdout.write("\n");
        
#     for n in upstream.keys():
#         sys.stdout.write("Upstream of %s: " % (n))
#         for l in upstream[n]:
#             sys.stdout.write("%02d, " % (l.id))
#         sys.stdout.write("\n");

    for n in downstream.keys():
        for dlink in downstream[n]:
            if (upstream.has_key(n)):
                for ulink in upstream[n]:
                    ulink.downstream(dlink)
                    dlink.upstream(ulink)

    return (basins, links)

# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
program = os.path.basename(sys.argv[0])

notchw = 2.0
notchd = 3.0
upq = 5.0
q0 = 0.0
d0 = 10.0
mannings = 0.0325

links_file = "links.dat"
points_file = "points.dat"
sections_file = "xsections.dat"
initial_file = "initial_dat"
linkbc_file = "link_bc.dat"
lateralq_file = "lateral_inflow.dat"
upstreamq_file = "upstreamq.dat"
downstreambc_file = "downstream.dat"
gage_file = "gage-control.dat"

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
usage = "Usage: %prog [options] network.file.txt"
parser = OptionParser(usage=usage)
parser.add_option("-v", "--verbose",
                  dest="verbose", action="store_true", default=False,
                  help="show what's going on")

parser.add_option("-w", "--notch-width", type=float,
                  dest="notchw", action="store", default=notchw,
                  help="generated cross section notch width, ft")

parser.add_option("-d", "--notch-depth", type=float,
                  dest="notchd", action="store", default=notchd,
                  help="generated cross section notch depth, ft")

parser.add_option("-q", "--upstream-discharge", type=float,
                  dest="upq", action="store", default=upq,
                  help="upstream (baseflow) discharge imposed at all upstream nodes, cfs")

parser.add_option("-Q", "--basin-discharge", type=float,
                  dest="discharge", action="store", default=q0,
                  help="total basin discharge, cfs")

parser.add_option("-D", "--downstream-depth", type=float,
                  dest="depth", action="store", default=d0,
                  help="final downstream depth, cfs")

parser.add_option("-r", "--dry-start", 
                  dest="drystart", action="store_true", default=False,
                  help="Make the initial conditions dry instead of completely flooded")

parser.add_option("-n", "--mannings", type=float,
                  dest="n", action="store", default=mannings,
                  help="Manning's coefficient used for all links")

parser.add_option("-g", "--gage-output",
                  dest="gage", action="store_true", default=False,
                  help="generate a gage control file for all link outlets and turn gage output on")

parser.add_option("-H", "--hot-start",
                  dest="hotstart", action="store", 
                  help="use the specified hotstart for the simulation")


(options, args) = parser.parse_args()

if (len(args) < 1):
    parser.print_help()
    sys.exit(3)

netfilename = args[0]

doverbose = options.verbose
dogage = options.gage
notchw = options.notchw
notchd = options.notchd
upq = options.upq
q0 = options.discharge
d0 = options.depth
mannings = options.n
drystart = options.drystart

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

if (doverbose):
    sys.stderr.write("%s: info: reading network from \"%s\"\n" %
                     (program, netfilename))
netfile = open(netfilename, mode="r")
(basins, links) = read_network(netfile, mannings)
netfile.close()

if (doverbose):
    sys.stderr.write("%s: info: read %d links from \"%s\"\n" %
                     (program, len(links), netfilename))

outlet = MASS1.outlet(links[0])
MASS1.set_order(outlet, 1)

# use slope and not elevation
MASS1.set_elevation(outlet)

maxelev = MASS1.elevation_max(outlet)
elev = outlet.point.dnelev + d0

MASS1.write_link_file(links_file, links)
MASS1.write_point_file(points_file, links, -notchd)
MASS1.write_section_file(sections_file, links, notchw, notchd)
MASS1.write_lateral_inflows(lateralq_file, "lateral_q", basins, q0)
MASS1.write_link_bc(linkbc_file, upstreamq_file, downstreambc_file,
                    upq, maxelev, elev, drystart)
MASS1.write_initial_conditions(initial_file, links,
                               maxelev, elev, drystart)

npt = 0
for l in links:
    n = l.npoints
    if n > npt: npt = n
npt += 1

confout = str(MASS1.configuration_base)
confout = confout.replace("@NLINK@", ("%d" % (len(links))))
confout = confout.replace("@NPT@", ("%d" % npt))
confout = confout.replace("@NSECT@", ("%d" % (len(links))))
confout = confout.replace("@LINKF@", links_file)
confout = confout.replace("@POINTF@", points_file)
confout = confout.replace("@SECTF@", sections_file)
confout = confout.replace("@LINKBCF@", linkbc_file)
confout = confout.replace("@INITF@", initial_file)
confout = confout.replace("@LATFLOWF@", lateralq_file)
if (options.hotstart):
    confout = confout.replace("@HOTSTART@", options.hotstart)
    confout = confout.replace("@HSFLG@", "1")
else:
    confout = confout.replace("@HSFLG@", "0")

if dogage:
    confout = confout.replace("@GFLG@", "1")
    confout = confout.replace("@GAGEF@", gage_file)
    MASS1.write_gage_control(gage_file, links)
else:
    confout = confout.replace("@GFLG@", "0")

conf = open("mass1.cfg", mode="w")
conf.write(confout)
conf.close()
