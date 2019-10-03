#! /usr/bin/env perl
# -*- cperl -*-
# -------------------------------------------------------------
# file: HEC2CHARIMA.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December  2, 1996 by William A. Perkins
# Last Change: 2017-06-22 11:41:07 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

use XSection;
use Getopt::Std;

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
($program = $0) =~ s/.*\///;
$usage = "usage: $program -r river [-o output] file [file ...]";

$start = 10;
$step = 10;
$river = "UNKNOWN";

# -------------------------------------------------------------
# check command line
# -------------------------------------------------------------
die "$usage\n" if (getopt("r:o:"));

$river = uc($opt_r) if ($opt_r);

if ($opt_o) {
  unless (open(OUTPUT, ">$opt_o")) {
    printf(STDERR "$program: error: unable to open file \"%s\" for writing\n", 
           $opt_o);
    die "$usage\n";
  }
} else {
  unless (open(OUTPUT, ">&STDOUT")) {
    die "$program: error: Unable to dup STDOUT\n";
  }
}

# -------------------------------------------------------------
# do the work
# -------------------------------------------------------------

die "$usage\n" if (scalar(@ARGV) <= 0);

while (scalar(@ARGV) > 0) {
  $file = shift(@ARGV);
  if ($file eq '-') {
    unless (open(XSECT, "<&STDIN")) {
      die "$program: error: Unable to dup STDIN\n";
    }
  } else {
    unless (open(XSECT, "<$file")) {
      printf(STDERR "$program: error: unable to open file \"%s\"\n", $file);
      die $usage;
    }
  }
  while (($hec2sect = HEC2Section::read_section(\*XSECT, $river)) != 0) {
    $charimasect = CHARIMASection::fromHEC2($hec2sect, $start);
    if (scalar($charimasect->points()) > 0) {
      $charimasect->write_section(\*OUTPUT);
      $start += $step;
    }
  }
  close(XSECT);
}
close (OUTPUT);

