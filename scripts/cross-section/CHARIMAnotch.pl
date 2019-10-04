#! /usr/bin/env perl
# -*- cperl -*-
# -------------------------------------------------------------
# file: CHARIMAnotch.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December  2, 1996 by William A. Perkins
# Last Change: 2017-07-14 10:09:03 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

use XSection;
use Getopt::Std;

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
($program = $0) =~ s/.*\///;
$usage = "usage: $program [-v] [-w width] [-h height] [-o output] file [file ...]";

                                # default notch size

$verbose = 0;
$width = 1.0;
$height = 5.0;

# -------------------------------------------------------------
# check command line
# -------------------------------------------------------------
die "$usage\n" if (not getopts('vw:h:o:'));

$verbose = 1 if ($opt_v);
$width = $opt_w if ($opt_w);
$height = $opt_h if ($opt_h);

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

  while (($xsection = CHARIMASection::read_section(\*XSECT)) != 0) {
    if ($verbose) {
      printf(STDERR "$program: info: read section %d (%d pts)\n", 
             $xsection->ID(), $xsection->points());
    }
    ($stn, $thalweg) = $xsection->thalweg();
    if ($verbose) {
      printf(STDERR "$program: info: old: stn = %.6g, thalweg = %.6g\n", $stn, $thalweg);
    }

    $xsection->addpoint($stn, $thalweg - $height);
    $xsection->addpoint($stn - $width/2.0 - 0.1, $thalweg);
    $xsection->addpoint($stn - $width/2.0, $thalweg - $height);
    $xsection->addpoint($stn + $width/2.0, $thalweg - $height);
    $xsection->addpoint($stn + $width/2.0 + 0.1, $thalweg);

    if ($verbose) {
      ($stn, $thalweg) = $xsection->thalweg();
      printf(STDERR "$program: info: new: stn = %.6g, thalweg = %.6g\n", $stn, $thalweg);
    }

    $xsection->write_section(\*OUTPUT);
    undef $xsection;
  }
  
  close(XSECT);
}
