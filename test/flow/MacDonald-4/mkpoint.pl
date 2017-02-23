#! /usr/unsupported/bin/perl
# -*- mode: cperl -*-
# -------------------------------------------------------------
# file: mkgrid.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October 15, 2003 by William A. Perkins
# Last Change: 2017-01-13 15:36:23 d3g096
# -------------------------------------------------------------

# RCS ID: $Id: mkgrid.pl 618 2007-02-02 16:05:36Z perk $

=pod

=head1 NAME

mkgrid.pl - 

=head1 SYNOPSIS

perl mkgrid.pl [B<-s>]

=head1 DESCRIPTION

=head1 OPTIONS

=over

=item B<-s>

Output the solution (depth, stage) rather than the grid.

=back

=back

=head1 ENVIRONMENT

=head1 EXAMPLES

=head1 AUTHOR

William A. Perkins, Battelle

=cut

use strict;
use Getopt::Std;
use Math::Trig;


# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
my $program;
($program = $0) =~ s/.*\///;
my $usage = "usage: $program [-s]";


my $L = 1000.0;
my $B = 10.0;
my $SS = 1.0;
my $D = 2.0;
my $W = $B + 2*$D/$SS;
my $Z0 = 3.7+1.92661041e-01*0.3048;
my $dx = 25.0;
my $dy = 0.25;

my $pi = atan2(1.0,1.0)*4.0;
my $g = 9.81;

my $dogrid = 1;

my $cnv = 0.3048;

# -------------------------------------------------------------
# handle command-line
# -------------------------------------------------------------
my %opts = ();
die "$usage\n" unless (getopts("s", \%opts));

$dogrid = undef if ($opts{'s'});

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

my $dhat;
my $d;
my $z = $Z0;
my $i;
my $j;

my $x;
my $y;
my $So;

my @a = (-0.111051, 0.026876, -0.217567);

# if ($dogrid) {
#   printf("%10d %10d\n", $L/$dx + 1, $W/$dy + 1);
# }

$i = 1;
for ($x = 0.0; $x <= $L; $x += $dx, $i += 1) {
  if ($x <= 300.0) {
    $d = 0.723449*(1 - tanh($x/1000 - 3/10));
    $dhat = -0.723449e-03*sech($x/1000 - 3/10)**2;
  } elsif ($x <= 600) {
    $d = 0.723449*(1.0 - 1.0/6.0*tanh(6*($x/1000.0 - 3/10)));
    $dhat = -0.723449e-03*sech(6.0*($x/1000.0 - 3/10))**2.0;
  } else {
    my $k;
    $d = 3/4;
    $dhat = 0.0;
    for ($k = 0; $k < 3; $k++) {
      $d += $a[$k]*exp(-20*($k+1)*($x/1000.0 - 3/5));
      $dhat += - ($k+1)*$a[$k]*exp(-20*($k+1)*($x/1000.0 - 3/5));
    }
    $d += 3/5*exp($x/1000 - 1);
    $dhat /= 50.0;
    $dhat += 3/5000*exp($x/1000 - 1);
  }
  $So = (1 - (400*(10+2*$d))/($g*(10+$d)**3.0*$d**3.0))*$dhat +
    0.16*((10+2*$d*sqrt(2))**(4/3))/((10+$d)**(10/3))/($d**(10/3));

  $z -= $So*$dx;

  if ($dogrid) {
    printf("%5d %5d %12.8e %5d %12.8e %8.3f 1000 0.001\n",
           1, $i, ($L-$x)/$cnv, 100, $z/$cnv, 0.020);
  } else {
    printf("%12.8e %12.8e %12.8e %12.8e %12.8e\n", 
           ($L-$x)/$cnv, $d/$cnv, $So, $z/$cnv, ($z+$d)/$cnv);
  }

}
