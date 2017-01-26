#! /usr/unsupported/bin/perl5 -I/home/perk/gas-transport/utilities
# -*- cperl -*-
# -------------------------------------------------------------
# file: CHARIMAnotch.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December  2, 1996 by William A. Perkins
# Last Change: Mon Dec  2 13:13:05 1996 by William A. Perkins <perk@doofus.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

use XSection;
use Getopt::Std;

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
($program = $0) =~ s/.*\///;
$usage = "usage: $program [-w width] [-h height] [-o output] file [file ...]";

                                # default notch size

$width = 1.0;
$height = 5.0;

# -------------------------------------------------------------
# check command line
# -------------------------------------------------------------
die "$usage\n" if (getopt("w:h:o:"));

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
    ($stn, $thalweg) = $xsection->thalweg();
    $xsection->addpoint($stn, $thalweg - $height);
    $xsection->addpoint($stn - $width/2.0 - 0.1, $thalweg);
    $xsection->addpoint($stn - $width/2.0, $thalweg - $height);
    $xsection->addpoint($stn + $width/2.0, $thalweg - $height);
    $xsection->addpoint($stn + $width/2.0 + 0.1, $thalweg);
    $xsection->write_section(\*OUTPUT);
    undef $xsection;
  }
  
  close(XSECT);
}
