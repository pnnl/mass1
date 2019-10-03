#! /usr/bin/env perl
# -*- mode: cperl -*-
# -------------------------------------------------------------
# file: CHARIMAmetric.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 25, 2005 by William A. Perkins
# Last Change: 2019-09-13 11:44:31 d3g096
# -------------------------------------------------------------

use XSection;
use Getopt::Std;

# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
($program = $0) =~ s/.*\///;
$usage = "usage: $program [-k] [-o out] file [file ...]";

$fname = "stdin";
undef $dokm;

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
die "$usage\n" if (!getopts("ko:"));

$dokm = $opt_k if ($opt_k);

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

if ($dokm) {
  $rmconvert = 1.609344;
} else {
  $rmconvert = 1609.344;
}

$ft2m = 0.3048;
  
while (($xsection = CHARIMASection::read_section(STDIN)) != 0) {
  $osect =
    CHARIMASection->new($xsection->river(),
                        $xsection->rivermile()*$rmconvert, # mile to ...
                        $xsection->ID(),
                        $xsection->{comment},
                        $xsection->{steps});
  foreach $stn (sort { $a <=> $b } keys(%{$xsection->{points}}) ) {
    # ft to m
    $osect->addpoint($stn*$ft2m, $xsection->{points}->{$stn}*$ft2m);
  }
  $osect->write_section(STDOUT);
}
