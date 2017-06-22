#! /usr/unsupported/bin/perl5 -I/home/perk/gas-transport/utilities
# -*- cperl -*-
# -------------------------------------------------------------
# file: CHARIMAextract.pl
#
# usage: CHARIMAextract.pl -R -l rm,... [file] 
#        CHARIMAextract.pl -R -m min_rm -M max_rm  [file]
#        CHARIMAextract.pl -I -l id,... [file] 
#        CHARIMAextract.pl -I -m min_id -M max_id [file]
#
# This program is used to select and extract a set of cross sections
# from a CHARIMA format cross section file.  The cross sections can be
# specified by id or rivermile, using individual values or as a range.
#
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 10, 1997 by William A. Perkins
# Last Change: Wed Dec 10 13:03:47 1997 by William A. Perkins <perk@owl.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$


use XSection;
use Getopt::Std;

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
($program = $0) =~ s/.*\///;
$usage = 
  "usage: $program -I -l id,... [file]\n" .
  "       $program -I -m min_id -M max_id [file]\n" .
  "       $program -R -m min_rm -M max_rm [file]\n" .
  "       $program -R -l id,... [file]";

$min = 0.0;
$max = 99999.0;
@list = ();

undef $use_list;
undef $use_range;
undef $use_rm;
undef $use_id;

# -------------------------------------------------------------
# command line arguments
# -------------------------------------------------------------
die "$usage" if (!getopts('IRl:m:M:'));

$use_rm = defined($opt_R);
$use_id = defined($opt_I);

die "$program: error: specify -R or -I, not both\n$usage\n" 
  if ($use_rm and $use_id);

die "$program: error: specify selection by rivermile (-R) or id (-I)\n$usage" 
  if (!$use_rm && !$use_id);

$use_range = defined($opt_m) || defined($opt_M);
$use_list = defined($opt_l);

die "$program: error: specify a list (-l) or range (-m and -M), not both\n$usage\n"
  if ($use_list && $use_range);

die "$program: error: specify a selection list (-l) or range (-m and -M)\n$usage\n"
  if (!$use_list && !$use_range);

$min = $opt_m + 0 if defined($opt_m);
$max = $opt_M + 0 if defined($opt_M);

if (defined($opt_l)) {
  @tmp = split(/,/, $opt_l);
  die "$program: error: empty selection list (-l)\n$usage\n" 
    if (scalar(@tmp) <= 0);
  foreach $i (@tmp) { push (@list, ($i + 0)); }
}



                                # open file if specified or use std input

if (scalar(@ARGV) > 0) {
  $fname = $ARGV[0];
  unless (open(XSECT, "<$fname")) {
    die "$program: error: unable to open file \"$fname\"\n$usage\n";
  } 
} else {
  unless (open(XSECT, "<&STDIN")) {
    die "$program: error: unable to dup STDIN\n$usage\n";
  }
}

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

while ($xsection = CHARIMASection::read_section(\*XSECT)) {
  if ($use_rm) {
    if ($use_range) {
      $xsection->write_section(STDOUT) 
        if $xsection->rivermile() >= $min and $xsection->rivermile() <= $max;
    } elsif ($use_list) {
      foreach $rm (@list) {
        if ($rm == $xsection->rivermile()) {
          $xsection->write_section(STDOUT);
          last;
        }
      }
    }
  } elsif ($use_id) {
    if ($use_range) {
      $xsection->write_section(STDOUT) 
        if $xsection->ID() >= $min and $xsection->ID() <= $max;
    } elsif ($use_list) {
      foreach $rm (@list) {
        if ($rm == $xsection->ID()) {
          $xsection->write_section(STDOUT);
          last;
        }
      }
    }
  }
}
