#! /usr/bin/env perl
# -*- mode: cperl -*-
# -------------------------------------------------------------
# file: HECRASCHARIMA.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 25, 2005 by William A. Perkins
# Last Change: 2017-06-22 11:41:17 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

use strict;
use XSection;

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
my $program;
($program = $0) =~ s/.*\///;
my $usage = "usage: $program [file]";

my $nextid = 10;
my $idstep = 10;
my $river = "UNKNOWN";
my $metric = 1;
my $m2ft = 1.0/0.3048;
my $km2mi = 0.6213712;
my $rm0 = 516.08;

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
my $input = undef;
my $output = undef;

if (scalar(@ARGV) > 0) {
  $input = shift @ARGV;
  unless (open(INPUT, "<$input")) {
    printf(STDERR "$program: error: unable to read file \"%s\"\n", $input);
    die "$usage\n";
  }
} else {
  unless (open(INPUT, "<&STDIN")) {
    die "$program: error: Unable to dup STDIN\n";
  }
}

if (scalar(@ARGV) > 0) {
  $output = shift @ARGV;
  unless (open(OUTPUT, "<$output")) {
    printf(STDERR "$program: error: unable to open file \"%s\" for output\n", 
           $output);
    die "$usage\n";
  }
} else {
  unless (open(OUTPUT, ">&STDOUT")) {
    die "$program: error: Unable to dup STDOUT\n";
  }
}


# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

my $xsection;
my $id;
my $comment;
my ($type, $rm, $lbdist, $cldist, $rbdist);
my ($npts, $ptcnt);
my $junk;

my $havetype = undef;
my $havedesc = undef;
my $havepts = undef;

while (<INPUT>) {
  if (/^Type/) {
    # cross section definition starts with "Type" line
    my $data;
    ($junk, $data) = split(/=/);
    ($type, $rm, $lbdist, $cldist, $rbdist) = split(/,/, $data);
    # $rm *= $km2mi if ($metric);
    $rm = $rm0;
    if ($metric) {
      $rm0 -= $cldist*$m2ft/5280.0;
    } else {
      $rm0 -= $cldist/5280.0;
    }
    $havetype = 1;
  } elsif ($havetype && /BEGIN\s+DESCRIPTION/) {
    $havedesc = 1;
    $comment = "";
  } elsif ($havedesc && /END\s+DESCRIPTION/) {
    $havedesc = undef;
    $xsection = CHARIMASection->new($river, $rm, $nextid, $comment);
    # printf(STDERR "$program: info: creating cross section %d: \"%s\"\n", 
    #        $nextid, $comment);
    $nextid += $idstep;
  } elsif ($havetype && $havedesc) {
    $comment .= $_
  } elsif (/Sta\/Elev=\s*(\d+)\s*$/ && $havetype) {
    $npts = $1 + 0;
    $havepts = 1;
    $ptcnt = 0;
  } elsif ($havepts) {
    my $f = 0;
    $junk = $_;
    while ($ptcnt < $npts && $f < 10) {
      my $x = substr($junk, 0, 8) + 0.0;
      $x *= $m2ft if ($metric);
      $f++;
      my $y = substr($junk, 8, 8) + 0.0;
      $y *= $m2ft if ($metric);
      $f++;
      $xsection->addpoint($x, $y);
      $ptcnt += 1;
      $junk = substr($junk, 16);
    }
    $havepts = undef unless ($ptcnt < $npts);
  } elsif (/^XS.*,\s*(\d+)\s*$/ && $havetype) {
    $xsection->{steps} = $1 + 0;
  } elsif ($havetype && /^\s*$/) {
    # cross section definition ends with an empty line
    $xsection->write_section(\*OUTPUT);
    $havetype = undef;
    $havedesc = undef;
    $havepts = undef
    $xsection = undef;
  }
}
