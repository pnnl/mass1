#! /usr/bin/env perl -I/home/perk/gas-transport/utilities
# -*- mode: cperl -*-
# -------------------------------------------------------------
# file: CHARIMAview.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 23, 2005 by William A. Perkins
# Last Change: 2017-06-22 11:40:48 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

use strict;
use Getopt::Std;
use XSection;
use PDL;
use PDL::Graphics::PGPLOT::Window;

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
my $program;
($program = $0) =~ s/.*\///;
my $usage = "usage: $program [-d device] file";

my $device = "/xserve";

my $plotopts = 
  [
   { color => 'BLACK' },
   { color => 'RED' },
   { color => 'BLUE' },
   { color => 'ORANGE' },
   { color => 'MAGENTA' },
   { color => 'DARKGRAY' },
   { color => 'CYAN' }
  ];

# -------------------------------------------------------------
# read_section_file
# -------------------------------------------------------------
sub read_section_file {
  my $name = shift;
  my $result = {};
  my $xsection;

  unless (open(BASESECT, "<$name")) {
    printf(STDERR "$program: error: Unable to open file \"%s\"\n", $name);
    return undef;
  }

  while (($xsection = CHARIMASection::read_section(\*BASESECT)) != 0) {
    $result->{$xsection->rivermile()} = $xsection;
  }
  close (BASESECT);

  return $result;
}

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
my %opts;
die "$usage" if (!getopts('sd:', \%opts));

$device = $opts{'d'} if (defined($opts{'d'}));

my @basefile;
while (@ARGV) {
  push(@basefile, shift(@ARGV));
}

die "$usage\n" if (scalar(@basefile) <= 0);

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

my $xsection;
my @xsect;
my $i;

for ($i = 0; $i < scalar(@basefile); $i++) {
  my $xs = read_section_file($basefile[$i]);
  die "$usage\n" if (!defined($xs));
  @xsect[$i] = $xs;
}

my @rmlist;
@rmlist = sort {$a <=> $b} keys %{$xsect[0]};


printf(STDERR "$program: info: using PGPLOT device \"%s\"\n", $device);

my $plot = PDL::Graphics::PGPLOT::Window->new({Dev=>$device, font=>1, HardFont=>1});

my $junk = sprintf("%s", $plot->info('CURSOR'));
my $havecursor = undef;

printf(STDERR "has cursor: %s\n", $junk);
$havecursor = ($junk =~ /YES/);

my $idx = 0;
my $done = undef;

while (not $done) {
  my $rm = $rmlist[$idx];
  my $id;
  my ($xmin, $xmax, $ymin, $ymax) = (1.0e10, -1.0e10, 1.0e10, -1.0e10);
  for ($i = 0; $i < scalar(@basefile); $i++) {
    $xsection = $xsect[$i]->{$rm};
    if (defined($xsection)) {
      $xmin = $xsection->min_station() if $xsection->min_station() < $xmin;
      $ymin = $xsection->min_elevation() if $xsection->min_elevation() < $ymin;
      $xmax = $xsection->max_station() if $xsection->max_station() > $xmax;
      $ymax = $xsection->max_elevation() if $xsection->max_elevation() > $ymax;
    }
  }
  $plot->env($xmin, $xmax, $ymin, $ymax);
  $plot->hold();
  my $leg = { Text => [],
              Color => [],
              LineStyle => []
            };

  for ($i = 0; $i < scalar(@basefile); $i++) {
    $xsection = $xsect[$i]->{$rm};
    if (defined($xsection)) {
      $id = sprintf("%d: %s", $xsection->{ID}, $xsection->{comment});
      
      printf(STDERR "Plotting section %d: %s (%s)\n", $idx, $id, $basefile[$i]);
      
      my $x = zeroes($xsection->points());
      my $y = zeroes($xsection->points());
      
      my $sidx = 0;
      my $stn;
      foreach $stn (sort {$a <=> $b;} keys %{$xsection->{points}}) {
        $x->set($sidx, $stn + 0.0);
        $y->set($sidx, $xsection->{points}->{$stn} + 0.0);
        $sidx += 1;
      }
      $plot->points($x, $y, {COLOR => $i+1, SYMBOL => $i, CHARSIZE => 1.0 });
      $plot->line($x, $y, {COLOR => $i+1});
      push(@{$leg->{Text}}, $basefile[$i]);
      push(@{$leg->{Colour}}, $i+1);
      push(@{$leg->{LineStyle}}, 'Solid');
      push(@{$leg->{Symbol}}, $i);
    }
  }
  $plot->label_axes("Station, ft", "Elevation, ft", $id, {COLOR=>'BLACK'});
  $leg->{Width} = 0.25*($xmax - $xmin);
  $leg->{Height} = 0.03*($ymax - $ymin)*scalar(@{$leg->{Text}});
  $leg->{TextColour} = $leg->{Color};
  $plot->legend($leg->{Text}, 0.5*($xmin+$xmax), $ymax - 0.05*($ymax - $ymin), $leg);

  if ($havecursor) {

    my ($xc, $yc, $ch, $xref, $yref) = $plot->cursor({Type => 'Default'});

    # printf(STDERR "response = %s\n", $ch);

    if ($ch eq "A") {
      $idx += 1;
    } elsif ($ch eq "X") {
      $idx -= 1;
    } else {
      $done = 1;
    }
    $idx = 0 if ($idx < 0);
    $idx = scalar(@rmlist) - 1 if ($idx > scalar(@rmlist) - 1);
  } else {
    $idx += 1;
    $done = ($idx >= scalar(@rmlist))
  }

  $plot->release();
}


$plot->close();
