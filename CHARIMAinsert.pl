#! /usr/unsupported/bin/perl5 -I/home/perk/gas-transport/utilities
# -*- cperl -*-
# -------------------------------------------------------------
# file: CHARIMAinsert.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created September 16, 1997 by William A. Perkins
# Last Change: Wed Mar 23 12:12:23 2005 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

use XSection;
use Getopt::Std;

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
($program = $0) =~ s/.*\///;
$usage = "usage: $program [-o output] base_file insert_file";

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
  printf("$program: error: Unable to open file \"%s\"\n", $basefile);
  die $usage;
}

die "$usage\n" unless $insfile = shift(@ARGV);
unless (open(INSECT, "<$insfile")) {
  printf("$program: error: Unable to open file \"%s\"\n", $insfile);
  die $usage;
}

$insert_sect = {};

while (($xsection = CHARIMASection::read_section(\*INSECT)) != 0) {
  $insert_sect->{$xsection->rivermile()} = $xsection;
}
close (INSECT);

while (($xsection = CHARIMASection::read_section(\*BASESECT)) != 0) {
  if (defined($insert_sect->{$xsection->rivermile()})) {
    $xsection->insert($insert_sect->{$xsection->rivermile()});
  }
  $xsection->write_section(\*STDOUT);
}
close (BASESECT);

