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
# Last Change: Wed Dec 10 09:25:57 1997 by William A. Perkins <perk@owl.pnl.gov>
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

$base_sect = {};

while (($xsection = CHARIMASection::read_section(\*BASESECT)) != 0) {
  $base_sect->{$xsection->rivermile()} = $xsection;
}
close (BASESECT);

while (($xsection = CHARIMASection::read_section(\*INSECT)) != 0) {
  $xsect_new = 
    $base_sect->{$xsection->rivermile()}->insert($xsection);
  $xsect_new->write_section(STDOUT);
}
close (INSECT);
