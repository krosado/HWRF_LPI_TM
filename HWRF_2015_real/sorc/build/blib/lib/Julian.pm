#
# Copyright (C) 2011 Timothy Brown
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA.
#
# $Id: Julian.pm 139 2011-04-20 19:56:30Z tpbrown@ucar.edu $
#

package Julian;

use 5.008002;
use strict;
use warnings;

use Carp;
use POSIX          qw(floor);

require Exporter;

our @ISA = qw(Exporter);
our $VERSION = '0.01';

=head1 NAME

Julian - Perl routines for Julian Day Number calculations.

=head1 SYNOPSIS

  use Julian qw();

  $jdn = time_jdn("2008-08-27 18:00");
  ($year, $month, $day, $hour, $minute, $sec) = jdn_time($jdn);


=head1 DESCRIPTION

The Julian module provides Perl functions to convert between the
Gregorian date format and the Astronomical Julian date.

It should be noted that if B<Astro::Time> is available on your system,
you should use that instead. A lot of these routines were blatantly
copied from B<Astro::Time> and.

The epoch of the Julian Day Number is 1200hrs UTC, 24/11/4713 BC.

=head2 EXPORT

I<Everything is exported by default>.

=over

=item jdn

=item date

=item split_date

=cut

our %EXPORT_TAGS = ( 'all' => [ qw(
	jdn
	date
	split_date
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );


=head2 METHODS

=head3 jdn

	my $jdn = jdn("2008/08/12 18:00");

Calculates the Julian Day Number from a given Gregorian date. If no date
is given the Julian Day Number for the current time is returned.

=cut

sub jdn {
	if (ref($_[0]) eq __PACKAGE__) {
		my $self   = shift;
	}

	my ($year, $month, $day, $hour, $minute, $second);

	if ($#_ == -1) {
		my ($wday,$yday,$isdst);
		($second,$minute,$hour,$day,$month,$year,$wday,$yday,$isdst) =
			gmtime;
		$year  += 1900;
		$month += 1;
	} elsif ($#_ == 0) {
		($year, $month, $day, $hour, $minute, $second) =
			split_date(shift);
	} else {
		($year, $month, $day, $hour, $minute, $second) = @_;
	}

	# From postgreSQL src/backend/utils/adt/datetime.c
	my ($julian, $centry);
	{
		use integer;
		if ($month > 2) {
			$month += 1;
			$year  += 4800;
		} else {
			$month += 13;
			$year  += 4799;
		}

		$centry = $year / 100;
		$julian = $year * 365 - 32167;

		$julian += $year / 4 - $centry + $centry / 4;
		$julian += 7834 * $month / 256 + $day;
	}

	$julian += ($hour - 12)/24
	           + $minute/1440
		   + $second/86400;

	return $julian;

}

=head3 date

	my ($year, $month, $day, $hour, $min, $sec) = date($jdn);

Calculates the Gregorian date from a given Julian Day Number.

=cut

sub date {
	if (ref($_[0]) eq __PACKAGE__) {
		my $self   = shift;
	}

	my $jdn = shift || return undef;
	my ($y, $m, $d, $H, $M, $S) = (0, 0, 0, 0, 0, 0);

	# From postgreSQL src/backend/utils/adt/datetime.c
	my ($julian, $quad, $extra, $x);
	$julian = floor($jdn + 0.5);
	{
		use integer;
		$julian += 32044;
		$quad = $julian / 146097;
		$extra = ($julian - $quad * 146097) * 4 + 3;
		$julian += 60 + $quad * 3 + $extra / 146097;
		$quad = $julian / 1461;
		$julian -= $quad * 1461;
		$x = $julian * 4 / 1461;
		$julian = (($x != 0) ? (($julian + 305) % 365)
			             : (($julian + 306) % 366)) + 123;
		$x += $quad * 4;
		$y = $x - 4800;
		$quad = $julian * 2141 / 65536;
		$d = $julian - 7834 * $quad / 256;
		$m = ($quad + 10) % 12 + 1;
	}
	my $frac = ($jdn + 0.5) - (floor($jdn + 0.5));
	$H = int(floor($frac * 24));
	$frac -= ($H / 24);
	$M = int(floor($frac * 24 * 60));
	$frac -= ($M / (24* 60));
	$S = int(floor($frac * 24 * 60 * 60));
	$frac -= ($S / (24 * 60 * 60));
	my $mm = int(floor($frac * 24 * 60 * 60 * 1000 + 0.5));

	if ($mm >= 500) {
		$mm -= 1000;
		$S  += 1;
	}

	if ($S >= 60) {
		$S -= 60;
		$M += 1;
	}

	if ($M >= 60) {
		$M -= 60;
		$H += 1;
	}

	return wantarray ? ($y, $m, $d, $H, $M, $S)
			 : sprintf("%4d %02d %02d %02d %02d %02d",
				 $y, $m, $d, $H, $M, $S);
}


=head3 split_date

	my ($year, $month, $day, $hour, $min, $sec) = split_date(2009081203);

The function B<split_date> will try and split the date specified into
the following fields:

=over

=item YYYY Four digit year

=item MM Two digit month

=item DD Two digit day

=item HH Two digit hour

=item mm Two digit minute

=item ss Two digit seconds

=back

=cut

sub split_date {
	my ($self, $date);
	if (ref($_[0]) eq __PACKAGE__) {
		$self   = shift;
	}
	if ($#_ == 0) {
		$date = shift;
	}

	if (!defined $date) {
		croak "no input date to split/parse";
	}

        my ($y, $m, $d, $H, $M, $S) = (0, 0, 0, 0, 0, 0);

# Perl > 5.8
#	# The date formats
#	my $fmt1 = '^\d{10,12}$';                             # yyyymmddHH[MM]
#	my $y_fmt1 = '(?<y>\d{4})-(?<m>\d{2})-(?<d>\d{2})';   # yyyy-mm-dd
#	my $y_fmt2 = '(?<d>\d{2})/(?<m>\d{2})/(?<y>\d{4})';   # dd/mm/yyyy
#	my $y_fmt3 = '(?<d>\d{2})\.(?<m>\d{2})\.(?<y>\d{4})'; # dd.mm.yyyy
#	my $h_fmt1 = '(?<H>\d{2}):(?<M>\d{2}):(?<S>\d{2})';   # HH:MM:SS
#	my $h_fmt2 = '(?<H>\d{2}):(?<M>\d{2}):(?<S>\d{2})\.\d*';# HH:MM:SS.ss
#
#	if ($date =~ m{$fmt1} ) {
#		($y, $m, $d, $H, $M) = unpack("A4A2A2A2A2", $date);
#	} elsif (($date =~ m{($y_fmt1|$y_fmt2|$y_fmt3)\s+($h_fmt1|$h_fmt2)}) ||
#		 ($date =~ m{($h_fmt1|$h_fmt2)\s+($y_fmt1|$y_fmt2|$y_fmt3)})) {
#		$y = $+{y};           $H = $+{H};
#		$m = $+{m};           $M = $+{M};
#		$d = $+{d};           $S = $+{S};
#	} else {
#		croak "unable to parse $date into a date";
#	}

	my $x = '(?:-|\.|\/)'; # Date seperator - or . or / (non-capture)
	if ($date =~ /^\d{4,}$/ ) {
		# yyyymmddHH[MM][SS]
		my ($Xy, $Xm, $Xd, $XH, $XM, $XS) = unpack('A4(A2)*', $date);
		$y = $Xy || 0;           $H = $XH || 0;
		$m = $Xm || 0;           $M = $XM || 0;
		$d = $Xd || 0;           $S = $XS || 0;

	} elsif ( $date =~ /^(\d{4})-(\d{2})-(\d{2})\ (\d{2}):(\d{2}).*$/) {
		# yyyy-mm-dd HH:MM
		$y = $1; $m = $2; $d = $3; $H = $4; $M = $5;
	} elsif ( $date =~ /^
			     (\d{2}):(\d{2}):(\d{2})\     # HH:MM:SS
		             (\d{2})$x(\d{2})$x(\d{4})$   # dd-mm-yyyy
			     /x) {
		# HH:MM yyyy-mm-dd
		$H = $1; $M = $2; $S = $3; $d = $4; $m = $5; $y = $6;
	} elsif ( $date =~ /^
			     (\d{2}):(\d{2})\             # HH:MM
		             (\d{2})$x(\d{2})$x(\d{4})$   # dd-mm-yyyy
			     /x) {
		# HH:MM yyyy-mm-dd
		$H = $1; $M = $2; $d = $3; $m = $4; $y = $5;
	} elsif ( $date =~ /^
			     (\d{2}):(\d{2})\             # HH:MM
		             (\d{4})$x(\d{2})$x(\d{2})$   # yyyy-mm-dd
			     /x) {
		# HH:MM yyyy-mm-dd
		$H = $1; $M = $2; $y = $3; $m = $4; $d = $5;
	} else {
		croak "unable to parse $date into a date";
	}

	# swap the month and day if the month is greater than 12.... ECK!
	if ($m > 12) {
		my $tmp = $m;
		$m = $d;
		$d = $tmp;
	}

	# Sanity checks
	if ($y < 1900) {
		croak "unable to handle years before 1900: $y";
	}
	if ($y > 2900) {
		croak "unable to handle years after 2900: $y";
	}
	if (($m < 1) or ($m > 12)) {
		croak "month is outside of range 01-12: $m";
	}
	if (($d < 1) or ($d > 31)) {
		croak "day is outside of range 01-31: $d";
	}
	if (($H < 0) or ($H > 23)) {
		croak "hour is outside of range 00-23: $H";
	}
	if (($M < 0) or ($M > 59)) {
		croak "minute is outside of range 00-59: $M";
	}
	if (($S < 0) or ($S > 59)) {
		croak "second is outside of range 00-59: $S";
	}

	return wantarray ? ($y, $m, $d, $H, $M, $S)
			 : sprintf("%4d %02d %02d %02d %02d %02d",
				 $y, $m, $d, $H, $M, $S);
}

=head1 SEE ALSO

perl (1),
DTC L<http://www.dtcenter.org/>,
HWRF L<http://www.dtcenter.org/HurrWRF/users/index.php>

=head1 AUTHOR

Timothy P Brown, E<lt>Timothy.P.Brown@noaa.govE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2011 by Timothy P Brown

=cut

1;
__END__
