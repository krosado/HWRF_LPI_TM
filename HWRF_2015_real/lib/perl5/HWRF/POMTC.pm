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
# $Id: POMTC.pm 115 2011-04-01 15:31:39Z tpbrown@ucar.edu $
#

package HWRF::POMTC;

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
our $VERSION = '0.01';

=head1 NAME

HWRF::POMTC - Perl extensions for the HWRF Testing and Evaluation suite.

=head1 SYNOPSIS

  use HWRF::POMTC;
  my $pomtc = HWRF::POMTC->new();
  $pomtc->source();
  $pomtc->compile();

=head1 DESCRIPTION

The B<HWRF::POMTC> module provides Perl functions that are commonly used in
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

	my $pomtc = HWRF::POMTC->new();

Instantiates an HWRF::POMTC object.

=cut

sub new {
	my $proto  = shift;
	my $class  = ref($proto) || $proto;
	my (%args) = @_;

	my $self = $class->SUPER::new(%args);

	$self->{NAME} = "POMTC";

	# Load the defaults
	if (! defined $self->{DST}) {
		if (defined $self->{TOPLEVEL}) {
			$self->{DST} = abs_path($self->{TOPLEVEL} ."/pomtc");
		} else {
			$self->{DST} = abs_path("pomtc");
		}
	}
	if (! defined $self->{SVN}->{URL}) {
		$self->{SVN}->{URL} = "https://svn-dtc-pomtc.cgd.ucar.edu";
	}
	if (! defined $self->{SVN}->{DIR}) {
		$self->{SVN}->{DIR} = "/trunk";
	}
	if (! defined $self->{SVN}->{REV}) {
		$self->{SVN}->{REV} = "HEAD";
	}
	if (! defined $self->{SRC}->{PKG}) {
		$self->{SRC}->{PKG} = "POMTC.tar.gz";
	}
	if (! defined $self->{CONFIG}->{PAR}) {
		$self->{CONFIG}->{PAR} = "dmpar";
	}
	if (! defined $self->{CONFIG}->{FILE}) {
		$self->{CONFIG}->{FILE} = "configure.pom";
	}
	if (! defined $self->{EXES}) {
		$self->{EXES} = [
			"ocean_exec/pomprep_fbtr.xc",
                        "ocean_exec/pomprep_idel.xc",
			"ocean_exec/pomprep_gdm3.xc",
                        "ocean_exec/pomprep_hycu.xc",
			"ocean_exec/pomprep_ncda.xc",
			"ocean_exec/transatl06prep.xc",
			"ocean_exec/gfdl_date2day.exe",
			"ocean_exec/gfdl_day2date.exe",
			"ocean_exec/gfdl_getsst.exe",
			"ocean_exec/gfdl_sharp_mcs_rf_l2m_rmy5.exe",
			"ocean_exec/hwrf_ocean_init.exe",
			"ocean_exec/hwrf_ocean_fcst.exe",
                        "ocean_exec/readsstuvhflux.exe",
		];
	}

        $self->use_renamer();  # let the renamer override the exe list

	return $self;
}

=head3 update_conf

	$c->update_conf();

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
	open (FILE, '<', $self->{CONFIG}->{FILE}) or croak $!;
	my @file = <FILE>;
	close(FILE);

	# Replace the debugging flags
	open (FILE, '>', $self->{CONFIG}->{FILE}) or croak $!;
	foreach my $line (@file) {
		if ($self->{DEBUG}) {
			if ($line =~ /^(DEBUG_FLAGS)\s*=/) {
				print FILE "$1 = ".
					$self->{COMPILER}->{FC_DEBUG}."\n";
			} elsif ($line =~ /^(FFLAGS)\s*=(.*)$/) {
				$2 =~ s/\b-O[1-9]\b//g;
				print FILE "$1 = $2 \$\(DEBUG_FLAGS\)\n";
			} else {
				print FILE $line;
			}
		} else {
			print FILE $line;
		}
	}
	close(FILE);

	chdir($cwd) or croak "can not change back into $cwd : $!";

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