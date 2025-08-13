#! /usr/bin/env perl
# -*- mode: cperl -*-
# -------------------------------------------------------------
# file: XSECTgen.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 27, 2021 by Perkins
# Last Change: 2021-01-28 09:02:29 d3g096
# -------------------------------------------------------------

# use strict;
use XSection;
use Getopt::Std;

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
my $program;
($program = $0) =~ s/.*\///;
my $usage = "usage: $program";


# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

my $irver = "Conn";
my $id = 0;
my $rm = 0.0; 

while (scalar(@ARGV) > 0) {
  my $fname;
  my $file = shift(@ARGV);
  if ($file eq '-') {
    unless (open(PTS, "<&STDIN")) {
      die "$program: error: Unable to dup STDIN\n";
    }
    $fname = "stdin";
  } else {
    unless (open(PTS, "<$file")) {
      printf(STDERR "$program: error: unable to open file \"%s\"\n", $file);
      die $usage;
    }
    $fname = $file;
  }

  $id = $id + 10;
  $rm = $rm + 1.0;
  
  my $line = 0;
  my $xs = CHARIMASection->new($river, $rm, $id, "generated from file", 50);

  while (<PTS>) {
    my @fld = ();
    my $stn = undef;
    my $elev = undef;

    $line += 1;
    next unless ($line > 1);

    chop;
    s/\t/ /g;
    @fld = split(',');
    $stn = $fld[0];
    $elev = $fld[1];

    # printf(STDOUT "%10.3f %10.3f\n", $x, $y);

    $xs->addpoint($stn, $elev);

  }    
  $xs->write_section(STDOUT);
  close (PTS);
}
