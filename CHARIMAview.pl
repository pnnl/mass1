#! env perl -I/home/perk/gas-transport/utilities
# -*- mode: cperl -*-
# -------------------------------------------------------------
# file: CHARIMAview.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 23, 2005 by William A. Perkins
# Last Change: Wed Mar 23 12:28:40 2005 by William A. Perkins <perk@leechong.pnl.gov>
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
die "$usage" if (!getopts('d:'));

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

my $xsect = read_section_file($basefile[0]);

die "$usage\n" if (!defined($xsect));

my @rmlist = sort {$a <=> $b} keys %{$xsect};



printf(STDERR "$program: info: using PGPLOT device \"%s\"\n", $device);

my $plot = PDL::Graphics::PGPLOT::Window->new({Dev=>$device});

my $havecursor = undef;

printf(STDERR "has cursor: %s\n", $plot->info('CURSOR'));
$havecursor = ($plot->info('CURSOR') == 'YES');

my $idx = 0;
my $done = undef;

while (not $done) {
  my $rm = $rmlist[$idx];
  $xsection = $xsect->{$rm};
  my $id = sprintf("%d: %s", $xsection->{ID}, $xsection->{comment});

  printf(STDERR "Plotting section %d: %s\n", $idx, $id);

  my $x = zeroes($xsection->points());
  my $y = zeroes($xsection->points());
  
  my $sidx = 0;
  my $stn;
  foreach $stn (sort {$a <=> $b;} keys %{$xsection->{points}}) {
    $x->set($sidx, $stn + 0.0);
    $y->set($sidx, $xsection->{points}->{$stn} + 0.0);
    $sidx += 1;
  }
  $plot->points($x, $y);
  $plot->hold();
  $plot->line($x, $y);
  $plot->label_axes("Station, ft", "Elevation, ft", $id);

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
