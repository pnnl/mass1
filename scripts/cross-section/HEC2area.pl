#! /usr/unsupported/bin/perl
# -*- cperl -*-
# -------------------------------------------------------------
# file: HEC2area.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  4, 1998 by William A. Perkins
# Last Change: Sun Jan  4 11:14:16 1998 by William A. Perkins <perk@localhost.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

use XSection;
use Getopt::Std;

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
($program = $0) =~ s/.*\///;
$usage = "usage: $program [-f] [-e elev] [-o out] file [file ...]";

$fname = "stdin";
undef $usefname;
undef $elev;

# -------------------------------------------------------------
# check command line
# -------------------------------------------------------------
die "$usage\n" if (!getopts("fe:o:"));

$elev = $opt_e if ($opt_e);
$usefname = 1 if ($opt_f);

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
    $fname = "stdin";
  } else {
    unless (open(XSECT, "<$file")) {
      printf(STDERR "$program: error: unable to open file \"%s\"\n", $file);
      die $usage;
    }
    $fname = $file;
  }

  while (($xsection = HEC2Section::read_section(\*XSECT)) != 0) {
    my $area = $xsection->area($elev);
    if ($usefname) {
      printf(OUTPUT "%s: ", $fname);
    }
    printf(OUTPUT "%.2f %.2f\n", 
	   $xsection->rivermile(),$area);
    undef $xsection;
  }
  close(XSECT);
}

