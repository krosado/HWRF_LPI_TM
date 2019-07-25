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
# $Id: WPS.pm 140 2011-04-20 20:33:03Z tpbrown@ucar.edu $
#

package HWRF::WPS;

use 5.008002;
use strict;
use warnings;

use lib "..";
use Data::Dumper;
use HWRF::Component;
use Carp;
use Cwd             qw(abs_path cwd);
use Verbose         qw(:all);

require Exporter;

our @ISA = qw(Exporter HWRF::Component);
our $VERSION = '0.02';

=head1 NAME

HWRF::WPS - Perl extensions for the HWRF Testing and Evaluation suite.

=head1 SYNOPSIS

  use HWRF::WPS;
  my $wps = HWRF::WPS->new();
  $wps->source();
  $wps->compile();

=head1 DESCRIPTION

The B<HWRF::WPS> module provides Perl functions that are commonly used in
building and running.

It is a B<HWRF::Component>, all methods exported by are provided for
this package too.

=head2 EXPORT

None by default.

=cut

our %EXPORT_TAGS = ( 'all' => [ qw(
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

=head2 METHODS

=head3 new

	my $wps = HWRF::WPS->new();

Instantiates an HWRF::WPS object.

=cut

sub new {
	my $proto  = shift;
	my $class  = ref($proto) || $proto;
	my (%args) = @_;

	my $self = $class->SUPER::new(%args);

	$self->{NAME} = "WPS";

	# Load the defaults
	if (! defined $self->{DST}) {
		if (defined $self->{TOPLEVEL}) {
			$self->{DST} = abs_path($self->{TOPLEVEL} ."/WPSV3");
		} else {
			$self->{DST} = abs_path("WPSV3");
		}
	}
	if (! defined $self->{SVN}->{URL}) {
		$self->{SVN}->{URL} = "https://svn-wrf-wps.cgd.ucar.edu"
	}
	if (! defined $self->{SVN}->{DIR}) {
		$self->{SVN}->{DIR} = "/trunk";
	}
	if (! defined $self->{SVN}->{REV}) {
		$self->{SVN}->{REV} = "HEAD";
	}
	if (! defined $self->{SRC}->{PKG}) {
		$self->{SRC}->{PKG} = "WPSV3.tar.gz";
	}
	if (! defined $self->{CONFIG}->{PAR}) {
		if(-d $ENV{JASPERINC} && -d $ENV{JASPERLIB}) {
			$self->{CONFIG}->{PAR} = 'dmpar';
		} else {
			$self->{CONFIG}->{PAR} = 'dmpar_NO_GRIB2';
		}
	}
	if (! defined $self->{CONFIG}->{FILE}) {
		$self->{CONFIG}->{FILE} = "configure.wps";
	}
	if (! defined $self->{EXES}) {
		$self->{EXES} = [
			"geogrid.exe",
			"metgrid.exe",
			"ungrib.exe",
		];
	}

        $self->use_renamer();  # let the renamer override the exe list

	return $self;
}

=head3 update_conf

	$wps->update_conf();

The function B<update_conf> will patch the configuration file adding the
debugging options to the C and Fortran compiler flags.

=cut

sub update_conf {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	if (defined $args{'file'}) {
		$self->{CONFIG}->{FILE} = $args{'file'};
	}

	# Change into our destination
	my $cwd = cwd();
	my $dir = abs_path($self->{DST});
	chdir($dir) or croak "can not change into $dir : $!";

	# Slurp in the makefile
	open (FILE, '<', $self->{CONFIG}->{FILE})
		or croak "unable to read ".$self->{CONFIG}->{FILE} .": $!";
	my @file = <FILE>;
	close(FILE);

	# Replace the debugging flags
	open (FILE, '>', $self->{CONFIG}->{FILE})
		or croak "unable to write ".$self->{CONFIG}->{FILE} .": $!";
	foreach my $line (@file) {
		if ($self->{DEBUG} and $line =~ /^(FFLAGS)\s*=(.*)$/) {
			print FILE "$1 = $2 ".$self->{COMPILER}->{FC_DEBUG}."\n";
		} else {
			print FILE $line;
		}
	}
	close(FILE);

	chdir($cwd) or croak "can not change back into $cwd : $!";

	TRACE_OUT;
}

=head3 namelist

	$wps->namelist();

The function B<namelist> will copy a default namelist, editing it to
represent the current simulation.

=cut

sub namelist {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	my ($fmt, $geog, $lat, $lon);

	if (!defined $args{'src'}) {
		croak "no namelist source filename given";
	}
	if (!defined $args{'dst'}) {
		croak "no namelist destination filename given";
	}
	if (!defined $args{'fmt'}) {
		$fmt = 2; # Default to NetCDF
	} else {
		$fmt = $args{'fmt'};
	}

	# Read the domain lat and lon

	# Slurp in the namelist
	open (FILE, '<', $args{'src'})
			or croak "unable to read " .$args{'src'} .": $!";
	my $file = <FILE>;
	close(FILE);

	# Write the new namelist
	open (FILE, '>', $args{'dst'})
		or croak "unable to write " .$args{'dst'} .": $!";
	$file =~ s/DOMAINLAT/$lat/;
	$file =~ s/DOMAINLON/$lon/;
	if (defined $args{'geog'}) {
		$file =~ s/OPTGGRIDPATH/$geog/;
	}
	print FILE $file;
	close(FILE);

	TRACE_OUT;
}

=head3 geogrid

	$wps->geogrid();

The function B<geogrid> will setup and submit a geogrid.exe run.

=cut

sub geogrid {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;
	TRACE_OUT;
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
