#! /usr/unsupported/bin/perl
# -*- mode: cperl -*-
# -------------------------------------------------------------
# file: date+.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December  1, 2010 by William A. Perkins
# Last Change: Wed Dec  1 10:36:32 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

use strict;
use Date::Manip;

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
my $program;
($program = $0) =~ s/.*\///;
my $usage = "usage: $program";

Date::Manip::Date_Init("TZ=US/Pacific");

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

while (<>) {
  chop;
  split;
  
  my $datestr = $_[0];
  $datestr =~ s/-/\//g;
  my $timestr = $_[1];
  my $date = Date::Manip::ParseDate($datestr . " " . $timestr);

  # print $_ . "\n";
  # print $datestr . " " . $timestr . "\n";

  if ($date) {
    $date = Date::Manip::DateCalc($date, "+ 12 hour");
    printf(STDOUT "%s\n", Date::Manip::UnixDate($date, "%m-%d-%Y %H:%M:%S"));
  } 
  
}
