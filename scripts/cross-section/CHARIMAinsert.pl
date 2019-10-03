#! /usr/bin/env perl
# -*- cperl -*-
# -------------------------------------------------------------
# file: CHARIMAinsert.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created September 16, 1997 by William A. Perkins
# Last Change: 2017-06-22 11:40:03 d3g096
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

