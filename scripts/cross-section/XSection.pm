#!/usr/unsupported/bin/perl5
# -------------------------------------------------------------
# file: XSection.pm
# A module of routines for handling cross-sections.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November 19, 1996 by William A. Perkins
# Last Change: 2017-07-14 12:55:26 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

package XSection;

# -------------------------------------------------------------
# documentation
# -------------------------------------------------------------


# -------------------------------------------------------------
# XSection::new
# create an XSection instance
# -------------------------------------------------------------
sub new {
  my $type = shift;
  my $self = {};
  bless $self, $type;
  $self->_initialize(@_);
  return $self;
}

# -------------------------------------------------------------
# XSection::_initialize
# -------------------------------------------------------------
sub _initialize {
  $self = shift;
  $self->{river} = shift;
  $self->{rivermile} = shift;
  $self->{points} = {};
  return;
}

# -------------------------------------------------------------
# XSection::points
# Returns the number of points in the cross section
# -------------------------------------------------------------
sub points {
  my $self = shift;
  return scalar(keys(%{$self->{points}}));
}

# -------------------------------------------------------------
# XSection::rivermile
# -------------------------------------------------------------
sub rivermile {
  my $self = shift;
  $self->{rivermile} = shift if (scalar(@_) > 0);
  return $self->{rivermile};
}

# -------------------------------------------------------------
# XSection::river
# -------------------------------------------------------------
sub river {
  my $self = shift;
  $self->{river} = shift if (scalar(@_) > 0);
  return $self->{river};
}

# -------------------------------------------------------------
# XSection::addpoint
# -------------------------------------------------------------
sub addpoint {
  my $self = shift;
  my $x = shift;
  my $y = shift;
  $x = $x + 0;
  $y = $y + 0;
  $self->{'points'}->{$x} = $y;
  return ($self->points());
}

# -------------------------------------------------------------
# XSection::removepoint
# -------------------------------------------------------------
sub removepoint {
  my $self = shift;
  my $stn = shift;
  my $elev = shift;
  if (exists($self->{'points'}->{$stn})) {
    $elev = $self->{'points'}->{$stn};
    delete $self->{'points'}->{$stn};
    return $elev;
  }    
  return;
}

# -------------------------------------------------------------
# XSection::min_station
# Returns the minimum station, and cooresponding elevation, in the x
# section
# -------------------------------------------------------------
sub min_station {
  my $self = shift;
  my @junk = sort { $a <=> $b } keys %{$self->{'points'}};
  my $min = shift @junk;
  return ($min);
}

# -------------------------------------------------------------
# XSection::max_station
# -------------------------------------------------------------
sub max_station {
  my $self = shift;
  my @junk = sort { $a <=> $b } keys %{$self->{'points'}};
  my $max = pop @junk;
  return ($max);
}

# -------------------------------------------------------------
# XSection::min_elevation
# -------------------------------------------------------------
sub min_elevation {
  my $self = shift;
  my @junk = sort { $a <=> $b } values %{$self->{'points'}};
  my $min = shift @junk;
  return ($min);
}

# -------------------------------------------------------------
# XSection::max_elevation
# -------------------------------------------------------------
sub max_elevation {
  my $self = shift;
  my @junk = sort { $a <=> $b } values %{$self->{'points'}};
  my $max = pop @junk;
  return ($max);
}

# -------------------------------------------------------------
# XSection::area
# computes the cross section area below the specified elevation
# if the elevation is not specified, the cross section's maximum
# elevation is used.
# -------------------------------------------------------------
sub area {
  my $self = shift;
  my $elev = shift;
  my $area = 0.0;
  my $laststn = undef;
  my $stn;

  if (! defined($elev)) {
    $elev = $self->max_elevation();
  }

  foreach $stn (sort { $a <=> $b } keys %{$self->{'points'}} ) {
    if (defined($laststn)  && 
	( $self->{'points'}->{$stn} <= $elev && 
	  $self->{'points'}->{$laststn} <= $elev) ) {
      $area += abs($stn - $laststn)*
	( ($elev - $self->{'points'}->{$stn}) + 
	  ($elev - $self->{'points'}->{$laststn}))/2.0;
    } elsif (defined($laststn)  && 
	( $self->{'points'}->{$stn} <= $elev || 
	  $self->{'points'}->{$laststn} <= $elev) ) {
      my $b;
      my $h;
      if ($self->{'points'}->{$stn} <= $elev) {
	$h = $elev - $self->{'points'}->{$stn};
      } else {
	$h = $elev - $self->{'points'}->{$laststn};
      }
      $b = abs($stn - $laststn)*$h/
	abs($self->{'points'}->{$stn} - $self->{'points'}->{$laststn});
      $area += $b*$h/2.0;
    }
    $laststn = $stn;
  }
  return $area;
}

# -------------------------------------------------------------
# XSection::topwidth
# -------------------------------------------------------------
sub topwidth {
  my $self = shift;
  my $elev = shift;
  my $width = 0.0;
  my $stn;
  my $laststn = undef;

  if (! defined($elev)) {
    $elev = $self->max_elevation();
  }

  foreach $stn (sort { $a <=> $b } keys %{$self->{'points'}} ) {
    if (defined($laststn)  && 
	( $self->{'points'}->{$stn} <= $elev && 
	  $self->{'points'}->{$laststn} <= $elev) ) {
      $width += abs($stn - $laststn);
    } elsif (defined($laststn)  && 
	( $self->{'points'}->{$stn} <= $elev || 
	  $self->{'points'}->{$laststn} <= $elev) ) {
      my $b;
      my $h;
      if ($self->{'points'}->{$stn} <= $elev) {
	$h = $elev - $self->{'points'}->{$stn};
      } else {
	$h = $elev - $self->{'points'}->{$laststn};
      }
      $b = abs($stn - $laststn)*$h/
	abs($self->{'points'}->{$stn} - $self->{'points'}->{$laststn});
      $width += $b;
    }
    $laststn = $stn;
  }
  return $width;
}

# -------------------------------------------------------------
# XSection::insert
# inserts one cross section into another, but only if it fits
# completely inside the original cross section.  
# -------------------------------------------------------------
sub insert {
  my $self = shift;
  my $sect = shift;
  my ($min, $max) = ($self->min_station() + 0.0, 
		     $self->max_station() + 0.0);
  my ($in_min, $in_max) = ($sect->min_station() + 0.0, 
			   $sect->max_station() + 0.0);
  if ($min = $in_min && $max >= $in_max) {
    my $stn;
    foreach $stn (keys %{$self->{'points'}}) {
      if ($stn + 0.0 >= $in_min && $stn + 0.0 <= $in_max) {
	$self->removepoint($stn);
      }
    }
    foreach $stn (sort { $a <=> $b } keys %{$sect->{'points'}}) {
      $self->addpoint($stn, $sect->{'points'}->{$stn});
    }
  }
  return $self;
}

# -------------------------------------------------------------
# XSection::fill
# Uses another section to fill gaps in this section
# -------------------------------------------------------------
sub fill {
  my $self = shift;
  my $sect = shift;
  my $gapsize  = shift;             # minimum gap size 
  my $min = $self->min_station() + 0.0;

  my @stns = sort {$a <=> $b;} keys %{$self->{'points'}};
  my @fillstns = sort { $a <=> $b } keys %{$sect->{'points'}};
  my $laststn = $min;
  my $stn;
  my $fstn;
  foreach $stn (@stns) {
    if ($stn - $laststn >= $gapsize) {
      foreach $fstn (@fillstns) {
        if ($fstn > $laststn && $fstn < $stn) {
          $self->addpoint($fstn, $sect->{'points'}->{$fstn});
        }
      }
    }
    $laststn = $stn
  }
}


# -------------------------------------------------------------
# XSection::thalweg
# finds the thalweg in a cross section
# -------------------------------------------------------------
sub thalweg {
  my $self = shift;
  my $minstn = -1.0;
  my $minelv = 9999999.0;
  my $stn;
  foreach $stn (sort keys %{$self->{'points'}} ) {
    if ( $self->{'points'}->{$stn} < $minelv ) {
      $minstn = $stn + 0.0;
      $minelv = $self->{'points'}->{$stn} + 0.0;
    }
  }
  return ($minstn, $minelv);
}

# -------------------------------------------------------------
# print_table
# -------------------------------------------------------------
sub print_table {
  my $self = shift;
  my $river = $self->river();
  my $rivermile = $self->rivermile();
  my $stn;
  foreach $stn (sort keys %{$self->{'points'}} ) {
    printf("\"%s\",%.2f,%.2f,%.2f\n", $river, $rivermile, 
           $stn, $self->{'points'}->{$stn});
  } 
  return;
}

package CHARIMASection;

@ISA = qw( XSection );

# -------------------------------------------------------------
# CHARIMASection::_initialize
# -------------------------------------------------------------
sub _initialize {
  my $self = shift;
  my $river = shift;
  my $rivermile = shift;
  $self->XSection::_initialize($river, $rivermile);
  $self->{ID} = shift;
  $self->{comment} = shift;
  $self->{steps} = shift if (scalar(@_) > 0);
  return;
}

# -------------------------------------------------------------
# CHARIMASection::ID
# -------------------------------------------------------------
sub ID {
  my $self = shift;
  $self->{ID} = shift if (scalar(@_) > 0);
  return $self->{ID};
}

# -------------------------------------------------------------
# CHARIMASection::read_section
# This is like a static C++ class method it should not be called by an
# instance
# -------------------------------------------------------------
sub read_section {
  my $fd = shift;
  my $self;
  my $rec;
  my ($id, $rm, $river, $steps);
  my $comment;
  my $npts;
  my ($x, $z);

                                # first line has ID, etc.

  return 0 unless defined($rec = <$fd>);
  chop $rec;
  $id = substr($rec, 0, 5) + 0;
  $steps = substr($rec, 5, 5) + 0;
  $comment = substr($rec, 20);
  if ($comment =~ /\d+\.?\d*/) {
    $rm = $& + 0.0;
  } else {
    $rm = 0.0
  }
  if ($comment =~ /\s*(\w+)\s+[Rr][Ii][Vv][Ee][Rr]/) {
    $river = uc($1);
  } else {
    $river = "UNKNOWN";
  }
  $self = CHARIMASection->new($river,$rm, $id, $comment,$steps);

                                # second line has number of points

  return 0 unless defined($rec = <$fd>);
  chop $rec;
  $npts = substr($rec, 5, 5);

                                # the rest of the lines have the points
  while ($npts > 0) {
    return 0 unless defined($rec = <$fd>);
    chop $rec;
    $rec =~ s/\s+$//;
    while (length($rec) > 0 && $npts > 0) {
      $x = substr($rec,0,10); $rec = substr($rec,10);
      $z = substr($rec,0,10); $rec = substr($rec,10);
      $self->addpoint($x, $z);
      $npts--;
    }
  }

                                # last line should be -1.0

  $rec = <$fd>;
  return $self;
}

# -------------------------------------------------------------
# CHARIMASection::write_section
# -------------------------------------------------------------
sub write_section {
  my $self = shift;
  my $fd = shift;
  my $id = $self->ID();
  my $rm = $self->rivermile();
  my $steps;
  my $count = scalar($self->points());
  my $river = $self->river();
  my ($stn, $step);

  $id = shift if (scalar(@_) > 0);
  $rm = shift if (scalar(@_) > 0);
  if (defined($self->{steps})) {
    $steps = $self->{steps};
  } else {
    $steps = 50;
  }

  printf($fd "%5d%5d%10.1f   RM=%6.3f, %s River\n", 
         $id, $steps, 1, $rm, $river);
  printf($fd "%5d%5d\n", 1, $count);
  
  $step = 0;
  foreach $stn (sort { $a <=> $b } keys(%{$self->{points}}) ) {
    if ($step > 3) {
      printf($fd "\n");
      $step = 0;
    }
    printf($fd "%10.1f%10.1f", $stn, $self->{points}->{$stn});
    $step++;
  }
  printf($fd "\n%5.1f\n", -1);
  return;
}

# -------------------------------------------------------------
# CHARIMASection::fromHEC2
# -------------------------------------------------------------
sub fromHEC2 {
  my $hec2sect = shift;
  my $id = shift;
  my $self;
  my ($stn, $elev);

  $self = CHARIMASection->new($hec2sect->river(), $hec2sect->rivermile(), 
                              $id, "from HEC2 format");
  foreach $stn (keys %{$hec2sect->{points}}) {
    $elev = $hec2sect->{points}->{$stn};
    $self->{points}->{$stn} = $elev;
  }
  return $self;
}

package HEC2Section;

@ISA = ( qw(XSection) );

# -------------------------------------------------------------
# HEC2Section::_initialize
# -------------------------------------------------------------
sub _initialize {
  my $self = shift;
  my $river = shift;
  my $rivermile = shift;
  $self->XSection::_initialize($river, $rivermile);
  $self->{comment} = shift;

                                # we can add the l/r bank stuff later.
  return;
}

# -------------------------------------------------------------
# HEC2Section::read_section
# -------------------------------------------------------------
sub read_section {
  my $fd = shift;
  my $river = shift;
  my $self;
  my $rec;
  my ($rm);
  my $comment;
  my $npts;
  my ($x, $z);
  my ($found, $i);

                                # look for an X1 card.

  $found = 0;
  while (!$found) {
    return 0 unless defined($rec = <$fd>);
    $found = 1 if ($rec =~ /^X1/)
  }

                                # process X1 card
  chop $rec;
  $rm = substr($rec, 2, 6);
  $npts = substr($rec, 8, 8);
  $comment = substr($rec, 9);

                                # get l/r bank stuff later

  $self = HEC2Section->new($river, $rm, $comment);

                                # find the points on GR cards

  while ($npts > 0) {
    $found = 0;
    while (!$found) {
      return 0 unless defined($rec = <$fd>);
      $found = 1 if ($rec =~ /^GR/)
    }
    chop $rec;
    for ($i = 0; $i < 5 && $npts > 0; $i++) {
      if ($i == 0) {
        $start = 2;
        $z = substr($rec, $start, 6);
        $x = substr($rec, $start + 6, 8);
      } else {
        $start = $i*16;
        $z = substr($rec, $start, 8);
        $x = substr($rec, $start + 8, 8);
      }
      $npts--;
      $self->addpoint($x, $z)
    }
  }
  return $self;
}


1;

