#! /usr/unsupported/bin/perl5 -I/home/perk/gas-transport/utilities
# -*- cperl -*-
# -------------------------------------------------------------
# file: XSECTprops.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  5, 1998 by William A. Perkins
# Last Change: 2017-06-22 11:42:27 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

use XSection;
use Getopt::Std;

# -------------------------------------------------------------
# documentation
# -------------------------------------------------------------
=pod

=head1 NAME

XSECTprops.pl - compute cross section hydraulic properties

=head1 SYNOPSIS

B<XSECTprops.pl> S<B<-F> HEC2|CHARIMA> S<[B<-f>]> S<[B<-atRD>]> S<[B<-e> I<elev>]> S<[B<-o> I<output>]> I<file> S<[I<file> ...]>

=head1 DESCRIPTION



=head1 OPTIONS

=over 4

=item S<B<-F> HEC2|CHARIMA>

Specifies the format of the cross section I<file> read.

=item B<-a>

Compute cross sectional area below the specified elevation.

=item B<-D>

Compute cross sectional hydraulic depth, which is the ratio of cross
section area (B<-a> option) to top width (B<-t> option)

=item B<-f>

Prepend the name of the input file to each output line.

=item B<-R>

(not yet implemented) Compute cross section hydraulic radius at the
specified elevation.

=item B<-t>

Compute cross section topwidth at the specified elevation.

=item S<B<-e> I<elev>>

I<elev> specifies the elevation at which channel properties are to be
computed.  If an elevation is not specified, the maximum elevation at
each cross section is used (which is not very useful).

=back 

=head1 AUTHOR

William A. Perkins, Pacific Northwest National Laboratory

=cut
 

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
($program = $0) =~ s/.*\///;
$usage = "usage: $program -F format [-f] [-at] [-e elev] [-o out] file [file ...]";

$fname = "stdin";
undef $usefname;
undef $elev;
undef $fmt;

undef $doarea;
undef $dowidth;

# -------------------------------------------------------------
# check command line
# -------------------------------------------------------------
die "$usage\n" if (!getopts("fatF:e:o:"));
die "$program: error: a format must be specified\n$usage\n" if (!defined($opt_F));

$opt_F =~ tr/a-z/A-Z/;

if ($opt_F =~ /^HEC2$/) {
  $fmt = 'HEC2';
} elsif ($opt_F =~ /^CHARIMA$/) {
  $fmt = 'CHARIMA';
} else {
  die "$program: error: unknown cross section format \"$optF\"\n$usage\n";
}


$elev = $opt_e if ($opt_e);
$usefname = 1 if ($opt_f);

$doarea = 1 if ($opt_a);
$dowidth = 1 if ($opt_t);

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

  FLOOP: while (1) {
    $xsection = 0;
    if ($fmt eq 'HEC2') {
      $xsection = HEC2Section::read_section(\*XSECT);
    } elsif ($fmt eq 'CHARIMA') {
      $xsection = CHARIMASection::read_section(\*XSECT);
    }
    if ($xsection == 0) {
      last FLOOP;
    } 
    if ($usefname) {
      printf(OUTPUT "%s: ", $fname);
    }
    if ($fmt eq 'CHARIMA' ) {
      printf(OUTOUT "%d ", $xsection->ID());
    }
    printf(OUTPUT "%.2f ", $xsection->rivermile());
    if ($doarea) {
      printf(OUTPUT "%.2f ", $xsection->area($elev));
    }
    if ($dowidth) {
      printf(OUTPUT "%.2f ", $xsection->topwidth($elev));
    }
    printf(OUTPUT "\n");
  }
  close (XSECT);
}



