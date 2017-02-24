#! /usr/unsupported/bin/perl5 -I/home/perk/gas-transport/utilities
# -*- cperl -*-
# -------------------------------------------------------------
# file: CHARIMAfill.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created September 16, 1997 by William A. Perkins
# Last Change: Wed Mar 23 09:01:01 2005 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

use XSection;
use Getopt::Std;

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
($program = $0) =~ s/.*\///;
$usage = "usage: $program [-o output] base_file fill_file mingap";

# -------------------------------------------------------------
# check command line
# -------------------------------------------------------------
die "$usage\n" if (getopt("o:"));

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

die "$usage\n" unless $basefile = shift(@ARGV);
unless (open(BASESECT, "<$basefile")) {
  printf(STDERR "$program: error: Unable to open file \"%s\"\n", $basefile);
  die $usage;
}

die "$usage\n" unless $insfile = shift(@ARGV);
unless (open(FILLER, "<$insfile")) {
  printf(STDERR "$program: error: Unable to open file \"%s\"\n", $insfile);
  die $usage;
}

die "$usage\n" unless $gapsize = shift(@ARGV);
unless ($gapsize+0.0 > 0.0) {
  printf(STDERR "$$program: error: Invalid gap size: \"%s\"\n", $gapsize);
  die $usage
}

$fill_sect = {};

while (($xsection = CHARIMASection::read_section(\*FILLER)) != 0) {
  $fill_sect->{$xsection->rivermile()} = $xsection;
}
close (FILLER);

while (($xsection = CHARIMASection::read_section(\*BASESECT)) != 0) {
  if ($fill_sect->{$xsection->rivermile()}) {
    $xsection->fill($fill_sect->{$xsection->rivermile()}, $gapsize);
  }
  $xsection->write_section(STDOUT);
}
close (BASESECT);

