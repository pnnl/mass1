#! /usr/unsupported/gnu/bin/perl
# -*- cperl -*-
# -------------------------------------------------------------
# file: mass1bc.pl

# This script makes a MASS1/MASS2 format boundary condition file from
# a MASS1 time-series output file

# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October  4, 1999 by William A. Perkins
# Last Change: Sun Oct 31 10:27:23 1999 by William A. Perkins <perk@mack.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

use strict;
use Getopt::Std;
use Date::Manip;

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
my $program;
($program = $0) =~ s/.*\///;
my $usage = 
  "usage: $program -f field [-0|-O offset] [-o output] [file]\n" . 
  "       $program -l";

                                # fields that can be used for BC files

my %fields = 
(
 wselev => 3, flow => 4, vel => 5, depth => 6,
 conc => 7, temp => 8, sat => 9, tdgpress => 10
);

my %fieldnames = 
(
 wselev => "Water Surface Elevation", 
 flow => "Discharge", 
 vel => "Velocity", 
 depth => "Depth",
 conc => "TDG Gas Concentration", 
 temp => "Water Temperature", 
 sat => "TDG Saturation", 
 tdgpress => "TDG Pressure"
);

Date::Manip::Date_Init("TZ=PST");

my $field = undef;
my $offset = undef;
my $title = undef;

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
my %opts = ();
die "$usage\n" unless getopts("f:o:O:t:l0", \%opts);

unless ($opts{f}) {
  printf(STDERR "$program: error: a field must be specified\n");
  die "$usage\n";
}

$field = $opts{f};
$title = $opts{t} if $opts{t};

if (! $fields{$field}) {
  printf(STDERR "$program: error: the specified field \"%s\" is not known\n", $field);
  printf(STDERR "$program: error: use -l to get a list of fields\n");
  die "$usage\n";
}

if ($opts{O}) {
  $offset = Date::Manip::ParseDateDelta($opts{O});
  unless ($offset) {
    printf(STDERR "$program: error: time offset \"%s\" not understood\n", $opts{O});
    die "$usage\n";
  }
} elsif ($opts{0}) {
  $offset = Date::Manip::ParseDateDelta("0 hour");
}

if ($opts{l}) {
  printf(STDERR "The following \"field\"s are recognized:\n");
  foreach (keys(%fields)) {
    printf(STDERR "\t%s:\t%s\n", $_, $fieldnames{$_});
  }
  printf(STDERR "$usage\n");
}

if ($opts{o}) {
  my $name = $opts{o};
  unless (open(OUTPUT, ">$name")) {
    printf(STDERR "$program: error: unable to open file \"%s\" for writing\n", 
           $name);
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

my $fldno = $fields{$field};
my $line = 0;

if ($title) {
  printf(OUTPUT "# %s: %s (Automatically generated using $program)\n",
         $title, $fieldnames{$field});
} else {
  printf(OUTPUT "# %s (Automatically generated using $program)\n", 
         $fieldnames{$field});  
}

while (<>) {
  chop;
  $line++;
  if (/^\d\d-\d\d-\d\d\d\d/) {
    my @fld = split();
    my $datestr = $fld[0] . " " . $fld[1];
    $datestr =~ s/-/\//g;
    my $date = Date::Manip::ParseDate($datestr);

    

    die "$program: error: line ${line}: date not understood\n" unless ($date);

    my $value = $fld[$fldno - 1];

    $date = Date::Manip::DateCalc($date, $offset) if ($offset);
    
    printf(OUTPUT "%s   %15.3f  /\n", Date::Manip::UnixDate($date, '%m-%d-%Y %H:%M:%S'), $value);
  }
}

