# -------------------------------------------------------------
# file: MASS1.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October 24, 2011 by William A. Perkins
# Last Change: 2018-08-16 08:55:07 d3g096
# -------------------------------------------------------------

from inspect import *


# -------------------------------------------------------------
# _classcheck
# -------------------------------------------------------------
def _classcheck(var, cls):
    if (var.__class__.__name__ != cls.__name__):
            raise ValueError, ("expected %s type, got %s" %
                               (cls.__name__,
                                var.__class__.__name__))

# -------------------------------------------------------------
# class Section
# -------------------------------------------------------------
class Section(object):

    # -------------------------------------------------------------
    # Section.__init__
    #
    # A section has a bankfull width and a depth
    # -------------------------------------------------------------
    def __init__(self, sid, width, depth):
        self.id = sid
        self.width = width
        self.depth = depth
        self.deltay = 0.5

    def getArea(self):
        return self.depth*self.width

    def getBankFullR(self):
        return 2*self.depth + self.width;
    hradius = property(getBankFullR)

    def bankfull(self, slope, n):
        a = self.area
        r = self.hradius
        q = 1.49*pow(r, 2.0/3.0)*sqrt(slope)/n
        return q

    def write_general(self, outfd, notchw, notchd):
        outfd.write("%8d%5d%5d    Section %d\n" %
                    (self.id, 50, 1, self.id))
        fudge = 0.01
        x = []
        y = []
        x.append(0.0)
        y.append(self.depth)
        x.append(0.0 + fudge)
        y.append(0.0)

        if (notchw > 0.0 and notchd > 0.0):
            x.append(self.width/2.0 - notchw/2.0)
            y.append(-fudge)
        
            x.append(self.width/2.0)
            y.append(-notchd)

            x.append(self.width/2.0 + notchw/2.0)
            y.append(-fudge)

        x.append(self.width - fudge)
        y.append(0.0)
        x.append(self.width)
        y.append(self.depth)

        n = len(x)
        outfd.write("%5.2f%5d\n" % (self.deltay, n))

        j = 0
        for i in range(n):
            outfd.write("%10.2f%10.2f" % (x[i], y[i]))
            j += 1
            if (j >= 4):
                outfd.write("\n")
                j = 0
        outfd.write(" / \n");
        return

    def write_rectangular(self, outfd, notchw, notchd):

        if (notchw > 0.0 and notchd > 0.0):
            outfd.write("%8d%5d    Section %d\n" %
                        (self.id, 2, self.id))
            outfd.write("%.1f %.1f %.1f /\n" % 
                        ( notchd, notchw, self.width ))
        else:
            outfd.write("%8d%5d    Section %d\n" %
                        (self.id, 1, self.id))
            outfd.write("%.1f /\n" % 
                        ( self.width ))
        return

    def write(self, outfd, notchw, notchd):
        self.write_rectangular(outfd, notchw, notchd)
        return


# -------------------------------------------------------------
# class Point
#
# Assuming that all points will be in the MASS1 input option 2.
# -------------------------------------------------------------
class Point(object):

    # -------------------------------------------------------------
    # Point.__init__
    # -------------------------------------------------------------
    def __init__(self, pid, length, upelev, dnelev, sect):
        self.id = pid;
        self.length = length
        self.upelev = upelev
        self.dnelev = dnelev
        self.section = sect

        # do some checks:

        if (length <= 0.0):
            raise ValueError, ("class %s: bad length (%.2f)" %
                               (self.__class__.__name__, length))

        if (dnelev > upelev):
            raise ValueError, ("class %s: dowstream elevation (%.2f) greater than upstream (%.2f)" %
                               (self.__name__, dnelev, upstream))

        if (sect.__class__.__name__ != Section.__name__):
            raise ValueError, ("class %s: expected %s type, got %s" %
                               (self.__class__.__name__,
                                Section.__name__,
                                sect.__class__.__name__))
            
        # some default coefficients
        self.manning = 0.0325
        self.kdiff = 1000.0
        self.ksurf = 0.001

    def getSlope(self):
        s = (self.upelev - self.dnelev)/self.length
        return s
    slope = property(getSlope)

    def getBankFullQ(self):
        slope = (self.upelev - self.dnelev)/length
        return section.bankfull(slope)
    bankfull = property(getBankFullQ)

    def getBankFullFr(self):
        Fr = 0.0
        return Fr
    froude = property(getBankFullFr)

    def write(self, outfd, linkid, offset):
        outfd.write("%8d %10.2f %8.1f %8.1f %8d %8.4f %8.1f %8.4f\n" %
                    (linkid,
                     self.length,
                     self.upelev + offset,
                     self.dnelev + offset,
                     self.section.id,
                     self.manning,
                     self.kdiff,
                     self.ksurf));
                     

# -------------------------------------------------------------
# class Subbasin
# -------------------------------------------------------------
class Subbasin(object):

    def __init__(self, bid, area):
        self.id = bid
        self.area = area
        self._links = []

    def add_link(self, link):
        try:
            _classcheck(link, Link)
        except ValueError, (s):
            raise ValueError, ("class %s: %s" % (self.__class__.__name__, s))
        self._links.append(link)

    def link_length(self):
        len = 0.0
        for l in self._links:
            len += l.length
        return len;
    length = property(link_length)

# -------------------------------------------------------------
# class Link
# -------------------------------------------------------------
class Link(object):

    def __init__(self, id, slope, point, basin):
        self.id = id
        self.slope = slope
        self.point = point
        self.basin = basin

        # do some checks

        try:
            _classcheck(point, Point)
        except ValueError, (s):
            raise ValueError, ("class %s: %s" % (self.__class__.__name__, s))

        try:
            _classcheck(basin, Subbasin)
        except ValueError, (s):
            raise ValueError, ("class %s: %s" % (self.__class__.__name__, s))

        self.order = 0
        self._uplink = []
        self._dnlink = []

    def get_length(self):
        return self.point.length
    length = property(get_length)

    def downstream(self, link):
        try:
            _classcheck(link, Link)
        except ValueError, (s):
            raise ValueError, ("class %s: %s" % (self.__class__.__name__, s))

        self._dnlink.append(link)

    def upstream(self, link):
        try:
            _classcheck(link, Link)
        except ValueError, (s):
            raise ValueError, ("class %s: %s" % (self.__class__.__name__, s))

        self._uplink.append(link)

    def get_npoints(self):
        npt = int(self.point.length/5280.0*16.0) + 1 # put points every 1/162th of a mile
        if (npt < 2): npt = 2
        return npt
    npoints = property(get_npoints)

    def write(self, outfd):
        ptopt = 2                       # format of point input
        lntype = 60                      # link type

        usbc = 0                        # there is only one upstream bc file
        if (len(self._uplink) == 0):
            usbc = 1

        dsbc = 0                        # there is only one downstream bc file
        if (len(self._dnlink) == 0):
            dsbc = 2

        latbc = self.basin.id          # there is a lateral bc file for each basin

        transbc = 0                     # worry about these later
        tempbc = 0
        lattransbc = 0
        lattempbc = 0
        metzone = 1
        lpi = 2.0

        outfd.write("%8d %5d %5d %5d %5d %5d %5d %5d %5d %5d %5d %5d %5d %5d %5.1f /\n" %
                    (self.id,
                     ptopt,
                     self.npoints,
                     self.order,
                     lntype,
                     len(self._uplink),
                     usbc,
                     dsbc,
                     transbc,
                     tempbc,
                     metzone,
                     latbc,
                     lattransbc,
                     lattempbc,
                     lpi))
        if (len(self._dnlink) > 0):
            outfd.write("%8d" % (self._dnlink[0].id))
        else:
            outfd.write("%8d" % (0))
        for l in self._uplink:
            outfd.write("%8d" % (l.id))
        outfd.write(" / \n")

# -------------------------------------------------------------
# outlet
# link is any link in the network
# -------------------------------------------------------------
def outlet(link):
    if (len(link._dnlink) == 0):
        return link
    else:
        return outlet(link._dnlink[0])

# -------------------------------------------------------------
# set_elevation
# uses the link slope attribute to update all elevations in the
# network; the first call should use the result of outlet()
# -------------------------------------------------------------
def set_elevation(link):
    if (len(link._dnlink) > 0):
        link.point.dnelev = link._dnlink[0].point.upelev
    link.point.upelev = link.slope*link.length + link.point.dnelev
    for l in link._uplink:
        set_elevation(l)
    return

# -------------------------------------------------------------
# elevation_max
# the first call should use the result of outlet()
# -------------------------------------------------------------
def elevation_max(link):
    max = link.point.upelev
    if (len(link._uplink) > 0):
        for l in link._uplink:
            m = elevation_max(l)
            if m > max: 
                max = m
    return max

# -------------------------------------------------------------
# set_order
# the first call should use the result of outlet()
# -------------------------------------------------------------
def set_order(link, o0):
    o = o0
    if (len(link._uplink) > 0):
        for l in link._uplink:
            o = set_order(l, o)
    link.order = o
    return o + 1

# -------------------------------------------------------------
# write_section_file
# -------------------------------------------------------------
def write_section_file(name, links, notchw, notchd):
    out = open(name, mode="w")
    for l in links:
        s = l.point.section
        s.write(out, notchw, notchd)
    out.close()
# -------------------------------------------------------------
# write_point_file
# -------------------------------------------------------------
def write_point_file(name, links, offset):
    out = open(name, mode="w")
    for l in links:
        id = l.id
        l.point.write(out, id, offset)
    out.close()

# -------------------------------------------------------------
# write_link_file
# -------------------------------------------------------------
def write_link_file(name, links):
    out = open(name, mode="w")
    for l in links:
        l.write(out)
    out.close()

# -------------------------------------------------------------
# write_link_bc
# -------------------------------------------------------------
def write_link_bc(name, qname, dsname, q0, z0, z1, drystart):
    myz0 = z0
    if (drystart):
        myz0 = z1
    
    bcout = open(name, mode="w")

    bcout.write("%8d \"%s\"\n" % (1, qname))
    qout = open(qname, mode="w")
    qout.write("# Upstream inflow for all upstream boundaries\n")
    qout.write("01-01-1900 00:00:00 %.6g / \n" % (q0))
    qout.write("01-01-3100 00:00:00 %.6g / \n" % (q0))
    qout.close()

    bcout.write("%8d \"%s\"\n" % (2, dsname))
    dsout = open(dsname, mode="w")
    dsout.write("# Downstream stage\n")
    dsout.write("01-01-1900 00:00:00 %.6g / \n" % (myz0))
    dsout.write("02-01-2000 00:00:00 %.6g / \n" % (myz0))
    dsout.write("06-01-2000 00:00:00 %.6g / \n" % (z1))
    dsout.write("01-01-3100 00:00:00 %.6g / \n" % (z1))
    dsout.close()

    bcout.close()

# -------------------------------------------------------------
# write_lateral_inflows
#
# -------------------------------------------------------------
def write_lateral_inflows(name, base, basins, basinq):

    atotal = 0.0
    for b in basins:
        # print b.id, b.area, atotal
        atotal += b.area
    rate = basinq/atotal
        
    fmt = ("%s_%%d.dat" % (base))
    listout = open(name, mode="w")
    for b in basins:
        lname = fmt % (b.id)
        listout.write("%8d \"%s\" \n" % (b.id, lname))
        out = open(lname, mode="w")
        q = b.area*rate
        latq = q/b.length
        out.write("# lateral inflow for basin %d, q = %.2f cfs, len = %.1f ft\n" %
                  (b.id, q, b.length))
        out.write("01-01-1900 00:00:00 %.6g / \n" % (latq))
        out.write("01-01-3100 00:00:00 %.6g / \n" % (latq))
        out.close()
    listout.close()


# -------------------------------------------------------------
# write_initial_conditions
# -------------------------------------------------------------
def write_initial_conditions(name, links, z0, z1, drystart):
    myz0 = z0
    if (drystart):
        myz0 = z1
        
    out = open(name, mode="w")
    for l in links:
        out.write("%8d %8.1f %8.1f %8.1f %8.1f\n" %
                  (l.id, 0.0, myz0, 35.0, 5.0))
    out.close()
        

# -------------------------------------------------------------
# write_gage_control
# -------------------------------------------------------------
def write_gage_control(name, links):
    out = open(name, mode="w")
    for l in links:
        out.write("%8d %5d / \n" % (l.id, l.npoints))
    out.close()
    

configuration_base = """MASS1 Configuration File
1       / Do Flow
1       / Do Lateral Inflow
0       / Do Gas
0       / Do Temp
0       / Do Printout
@GFLG@       / Do Gage Printout
0       / Do Profile Printout
0       / Do Gas Dispersion
0       / Do Gas Air/Water Exchange
0       / Do Temp Dispersion
0       / Do Temp surface exchange
@HSFLG@       / Do Hotstart read file
1       / Do Restart write file
1       / Do Print section geometry
0       / Do write binary section geom
0       / Do read binary section geom
0       / units option
1       / time option
2       / time units
0       / channel length units
0       / downstream bc type
@NLINK@      / max links
@NPT@     / max points on a link
50      / max bc table
21936   / max times in a bc table
@NSECT@      / total number of x-sections
0       / number of transport substeps
0       / debug print flag
\"@LINKF@\"             / link file name
\"@POINTF@\"            / point file name
\"@SECTF@\"         / Section file name
\"@LINKBCF@\"           / linkBC file name
\"@INITF@\"           / initial file name
\"output.out\"            / output file name
no-gasbc-files          / gas transport file name
no-tempbc-files         / temperature input
no-weather-files        / weather files
no-hydropower           / hydropower file name
no-tdgcoeff             / TDG Coeff file name
\"@HOTSTART@\"         / Read Hotstart file name
\"restart.dat\"           / Write restart file name
\"@GAGEF@\"    / gage control file name
\"profile-control.dat\"   / profile file name
\"@LATFLOWF@\"    / lateral inflow bs file name
02-01-2000              / date run begins
00:00:00                / time run begins
07-31-2000              / date run ends
23:59:59                / time run ends
0.50000                / delta t in hours (4*36s=144s)
2                     / printout frequency
"""
