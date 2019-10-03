#! /usr/bin/env perl
# -*- cperl -*-
# -------------------------------------------------------------
# file: CHARIMAthalweg.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November 27, 1996 by William A. Perkins
# Last Change: 2017-06-22 11:40:36 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

use XSection;

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
($program = $0) =~ s/.*\///;

# -------------------------------------------------------------
# do the work
# -------------------------------------------------------------
while (($xsection = CHARIMASection::read_section(STDIN)) != 0) {
  ($x, $z) = $xsection->thalweg();
  printf("%5d %10.3f %10.2f %10.3f\n", $xsection->{ID}, 
         $xsection->{rivermile}, $x, $z);
  # printf("%s (%d): %s\n", $xsection->{river}, $xsection->points(), 
  #        $xsection->{comment});
}
