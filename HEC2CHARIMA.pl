#! /usr/unsupported/bin/perl5 -I/home/perk/gas-transport/utilities
# -*- cperl -*-
# -------------------------------------------------------------
# file: HEC2CHARIMA.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December  2, 1996 by William A. Perkins
# Last Change: Mon Dec  2 15:05:39 1996 by William A. Perkins <perk@doofus.pnl.gov>
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
    $charimasect->write_section(\*OUTPUT);
    $start += $step;
  }
  close(XSECT);
}
close (OUTPUT);

