#! /usr/unsupported/bin/perl5 -I/home/perk/gas-transport/utilities
# -*- cperl -*-
# -------------------------------------------------------------
# file: CHARIMAthalweg.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November 27, 1996 by William A. Perkins
# Last Change: Mon Dec  2 10:21:03 1996 by William A. Perkins <perk@doofus.pnl.gov>
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
  printf("%5d %10.2f %10.2f %10.2f\n", $xsection->{ID}, 
         $xsection->{rivermile}, $x, $z);
  # printf("%s (%d): %s\n", $xsection->{river}, $xsection->points(), 
  #        $xsection->{comment});
}
