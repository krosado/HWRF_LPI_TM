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
# $Id: Utils.pm 156 2011-05-16 20:24:43Z timothy.p.brown@noaa.gov $
#

package HWRF::Utils;

use 5.008002;
use strict;
use warnings;

use lib "..";
use Data::Dumper;
use HWRF::Component;
use Julian          qw(split_date);
use Carp;
use Cwd             qw(abs_path cwd);
use Verbose         qw(:all);

require Exporter;

our @ISA = qw(Exporter HWRF::Component);
our $VERSION = '0.01';

=head1 NAME

HWRF::Utils - Perl extensions for the HWRF Utilities.

=head1 SYNOPSIS

  use HWRF::Utils;
  my $utils = HWRF::Utils->new();
  $utils->source();
  $utils->compile();

=head1 DESCRIPTION

The B<HWRF::Utils> module provides Perl functions that are commonly used in
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

	my $utils = HWRF::Utils->new();

Instantiates an HWRF::Utils object.

=cut

sub new {
	my $proto  = shift;
	my $class  = ref($proto) || $proto;
	my (%args) = @_;

	my $self = $class->SUPER::new(%args);

	$self->{NAME} = "HWRF Utilities";

	# Load the defaults
	if (! defined $self->{DST}) {
		if (defined $self->{TOPLEVEL}) {
			$self->{DST} = abs_path($self->{TOPLEVEL}
						."/hwrf-utilities");
		} else {
			$self->{DST} = abs_path("hwrf-utilities");
		}
	}
	if (! defined $self->{SVN}->{URL}) {
		$self->{SVN}->{URL} =
			"https://svn-dtc-hwrf-utilities.cgd.ucar.edu";
	}
	if (! defined $self->{SVN}->{DIR}) {
		$self->{SVN}->{DIR} = "/trunk";
	}
	if (! defined $self->{SVN}->{REV}) {
		$self->{SVN}->{REV} = "HEAD";
	}
	if (! defined $self->{SRC}->{PKG}) {
		$self->{SRC}->{PKG} = "HWRF-UTILITIES.tar.gz";
	}
	if (! defined $self->{CONFIG}->{PAR}) {
		$self->{CONFIG}->{PAR} = "dmpar";
	}
	if (! defined $self->{CONFIG}->{FILE}) {
		$self->{CONFIG}->{FILE} = "configure.hwrf";
	}
	if (! defined $self->{EXES}) {
		$self->{EXES} = [
                        "exec/serpoe.exe",
			"exec/diffwrf_3dvar.exe",
			"exec/grbindex.exe",
			"exec/hwrf_combinetrack.exe",
                        "exec/hwrf_netcdf_grads.exe",
			"exec/hwrf_anl_4x_step2.exe",
			"exec/hwrf_anl_bogus_10m.exe",
			"exec/hwrf_anl_cs_10m.exe",
			"exec/hwrf_atcf_to_stats.exe",
			"exec/hwrf_aux_rw.exe",
			"exec/hwrf_create_trak_fnl.exe",
			"exec/hwrf_create_trak_guess.exe",
                        "exec/hwrf_rem_prepbufr_typ_in_circle.exe",
                        "exec/hwrf_change_prepbufr_qm_in_circle.exe",
                        "exec/hwrf_change_prepbufr_qm_typ.exe",
                        "exec/hwrf_readtdrstmid.exe",
                        "exec/hwrf_readtdrtime.exe",
                        "exec/hwrf_readtdrtrigger.exe",
                        "exec/hwrf_readtdrtrack.exe",
			"exec/hwrf_gridgenfine.exe",
			"exec/hwrf_inter_2to1.exe",
			"exec/hwrf_inter_2to2.exe",
			"exec/hwrf_inter_2to6.exe",
			"exec/hwrf_inter_4to2.exe",
			"exec/hwrf_inter_4to6.exe",
			"exec/hwrf_merge_nest_4x_step12_3n.exe",
			"exec/hwrf_pert_ct1.exe",
			"exec/hwrf_prep.exe",
			"exec/hwrf_bdy_update.exe",
			"exec/hwrf_read_indi_write_all.exe",
			"exec/hwrf_split1.exe",
			"exec/hwrf_metgrid_levels.exe",
			"exec/hwrf_supvit.exe",
			"exec/hwrf_swath.exe",
			"exec/hwrf_swcorner_dynamic.exe",
			"exec/hwrf_bin_io.exe",
			"exec/hwrf_afos.exe",
			"exec/hwrf_blend_gsi.exe",
			"exec/hwrf_old_gettrk.exe",
                        "exec/hwrf_htcfstats.exe",
			"exec/mdate.exe",
			"exec/ndate.exe",
			"exec/nhour.exe",
			"exec/wgrib.exe",
                        "exec/hwrf_wrfout_newtime.exe",
                        "exec/hwrf_nhc_products.exe",
		];
	}
	if (! defined $self->{OUTPUTS}) {
		$self->{OUTPUTS} = {
			DIR    => "messages",
			FILES  => [
				"domain.center",
				"storm.center",
				"tcvital",
				"tcvital.as",
			],
		};
	}

        $self->use_renamer();  # let the renamer override the exe list

	return $self;
}

=head3 envs

	%vars = $utils->envs(dst => 'foo')

The function B<envs> will set the environment variables needed to
compile HWRF and all its components.

An optional argument B<dst> can be provided to specify the
destination/location of the HWRF Utilities directory.

Currently these are:

=over

=item LIB_W3_PATH=F<$self->{DST}/libs/>

=item LIB_SP_PATH=F<$self->{DST}/libs/>

=item LIB_SFCIO_PATH=F<$self->{DST}/libs/>

=item LIB_BACIO_PATH=F<$self->{DST}/libs/>

=item LIB_BLAS_PATH=F<$self->{DST}/libs/

=item LIB_G2_PATH=F<$self->{DST}/libs/

=back

=cut

sub envs {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	if (defined $args{'dst'}) {
		$self->{DST} = $args{'dst'};
	}

	$ENV{'LIB_W3_PATH'}    = $self->{DST} . "/libs";
	$ENV{'LIB_SP_PATH'}    = $self->{DST} . "/libs";
	$ENV{'LIB_SFCIO_PATH'} = $self->{DST} . "/libs";
	$ENV{'LIB_BACIO_PATH'} = $self->{DST} . "/libs";
	$ENV{'LIB_BLAS_PATH'}  = $self->{DST} . "/libs";
	$ENV{'LIB_G2_PATH'}    = $self->{DST} . "/libs";

	TRACE_OUT;

	return {
		LIB_W3_PATH    => $self->{DST} . "/libs",
		LIB_SP_PATH    => $self->{DST} . "/libs",
		LIB_SFCIO_PATH => $self->{DST} . "/libs",
		LIB_BACIO_PATH => $self->{DST} . "/libs",
		LIB_BLAS_PATH  => $self->{DST} . "/libs",
		LIB_G2_PATH    => $self->{DST} . "/libs",
	};
}

=head3 update_conf

	$utils->update_conf();

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
				$2 =~ s/\b#\b//g;
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

=head3

	@tcvitals = $utils->tcvital(sid    => '07L',
				    date   => '2008082512',
				    tcdir  => '/ptmp/datasets/Tcvitals',
				    output => 'tcvitals');

The function B<tcvital> will locate the TC Vital record from the
F<syndat_tcvitals> file. It will first look for the latest tcvital
for the storm that has 72hr guidance. If that is not found it will
try and locate a record without the 72hr guidance.

If an B<output> argument is given it will write the tcvitals to this file
and also a file with the ".as" suffix containing the tcvitals in the
vortex initialization format.

=cut

sub tcvital {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	if (!defined $args{'sid'}) {
		carp "unable to obtain tcvitals for unknown sid";
	}
	if (!defined $args{'date'}) {
		carp "unable to obtain tcvitals for unknown date";
	}
	if (!defined $args{'tcdir'}) {
		carp "unable to obtain tcvitals for unknown tc directory";
	}

	my ($y, $m, $d, $H, $M, $S) = Julian::split_date($args{'date'});
	my $tc_file = $args{'tcdir'} ."/".  "syndat_tcvitals." .$y;
	if (! -f $tc_file) {
		croak "unable to locate TC Vitals file $tc_file : $!";
	}

	my @guidance;
	my @no_guidance;
	my $sid = $args{'sid'};

	open(TCVITAL, "<", $tc_file) or croak "can not open $tc_file: $!";
	while (my $line = <TCVITAL>) {
		if ($line =~ /$sid\s+\w+\s+$y$m$d\s+$H/) {
			chomp($line);
			if (length($line) > 93) {
				push(@guidance, $line);
			} else {
				push(@no_guidance, $line);
			}
		}
	}
	close(TCVITAL);

	my @tcvital;
	if ($guidance[-1]) {
		@tcvital = split(/\s+/, $guidance[-1]);
	} elsif ($no_guidance[-1]) {
		@tcvital = split(/\s+/, $guidance[-1]);
	} else {
		croak "unable to locate TC Vital record for $sid in $tc_file";
	}

	if (defined $args{'output'}) {
		my $file = $args{'output'};
		# Write the tcvital file for the storm
		open(TCVITAL, ">", $file) or
		  croak "unable to write TC Vitals file: $file: $!";
		print TCVITAL "@tcvital\n";
		close(TCVITAL);

		# Write the tcvital.as file for the vortex initialization
		my $fmt = "%-5s %-4s %-10s %-9s %-5s %-5s %-6s %-4s %-4s ".
		          "%-5s %-5s %-5s %-3s %-4s %-5s %-5s %-5s %-5s ".
		          "%-2s %-5s %-5s %-5s %-5s %-3s %-5s %-5s %-5s ".
			  "%-5s %-5s %-5s\n";
		$file .= ".as";
		open(TCVITAL, ">", $file) or
		  croak "unable to write TC Vitals for vortex init: $file: $!";
		printf TCVITAL $fmt,  @tcvital;
		close(TCVITAL);
	}

	TRACE_OUT;

	return @tcvital;
}

=head3

	($lat, $lon) = $sim->storm_centre(tcvitals => \@vitals,
					  output => 'storm.center');

The function B<storm_centre> will calculate the storm centre of the
Simulation based upon the TC Vitals.

It will return the domain centre latitude and longitude.

If the TC Vitals array is not specified it will call B<tcvitals> will
all of it input arguments.

If the optional argument B<output> is given, the latitude and longitude
will be written to this file. The first line being the latitude and
the second the longitude.

=cut

sub storm_centre {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	my @tcvitals;
	if (defined $args{'tcvitals'}) {
		@tcvitals = @{$args{'tcvitals'}};
	} else {
		# save our output file name
		my $output;
		if (defined $args{'output'}) {
			$output = $args{'output'};
			$args{'output'} = "tcvitals";
		}
		# call tcvitals
		@tcvitals = $self->tcvital(@_);
		# put back out output
		if (defined $output) {
			$args{'output'} = $output;
		}
	}

	my ($lat, $lon, $lat_h, $lon_h);             # Lat & Lon @  0hrs

	# Get the Storm's Latitude and Longitude. Converting it
	# to degrees and +ve for N & E, -ve for S & W.
	($lat, $lat_h) = ($tcvitals[5] =~ /(\d+)([NnSs])/);
	($lon, $lon_h) = ($tcvitals[6] =~ /(\d+)([EeWw])/);
	$lat /= 10.0;
	$lon /= 10.0;
	if ($lat_h =~ /[Ss]/) {
		$lat *= -1.0;
	}
	if ($lon_h =~ /[Ww]/) {
		$lon *= -1.0;
	}

	if (defined $args{'output'}) {
		# Write the lat & lon file for the storm
		open(FILE, ">", $args{'output'}) or
		  croak "unable to write storm centre: ".$args{'output'}." $!";
		print FILE "$lat\n$lon\n";
		close(FILE);
	}

	TRACE_OUT;

	return $lat, $lon;
}

=head3

	($lat, $lon) = $utils->domain_centre(tcvitals => \@vitals,
					     output => 'domain.center');

The function B<domain_centre> will calculate the domain centre of the
Simulation based upon the TC Vitals.

It will return the domain centre latitude and longitude.

If the optional argument B<output> is given, the latitude and longitude
will be written to this file. The first line being the latitude and
the second the longitude.

=cut

sub domain_centre {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	my @tcvitals;
	if (defined $args{'tcvitals'}) {
		@tcvitals = @{$args{'tcvitals'}};
	} else {
		# save our output file name
		my $output;
		if (defined $args{'output'}) {
			$output = $args{'output'};
			$args{'output'} = "tcvitals";
		}
		# call tcvitals
		@tcvitals = $self->tcvital(@_);
		# put back out output
		if (defined $output) {
			$args{'output'} = $output;
		}
	}

	# Lat & Lon @ 0hrs
	my ($lat, $lon) = $self->storm_centre(tcvitals => \@tcvitals);

	my ($lat_72, $lon_72, $lat_72_h, $lon_72_h); # Lat & Lon @ 72hrs
	my ($clat, $clon);                           # Centre Lat & Lon

	if ($tcvitals[24] and $tcvitals[25]) {
		($lat_72, $lat_72_h) = ($tcvitals[24] =~ /(\d+)([NnSs])/);
		($lon_72, $lon_72_h) = ($tcvitals[25] =~ /(\d+)([EeWw])/);
		$lat_72 /= 10.0;
		$lon_72 /= 10.0;
		if ($lat_72_h =~ /[Ss]/) {
			$lat_72 *= -1.0;
		}
		if ($lon_72_h =~ /[Ww]/) {
			$lon_72 *= -1.0;
		}
	} else {
		if ($lon >= 0.0) {
			$lon_72 = $lon + 20.0;
		} else {
			$lon_72 = $lon - 20.0;
		}
	}

	# Reference latitude of HWRF domain
	# page ?? HWRF Users Guide?....
	if ($lat >= 55) {
		$clat = 50.0;
	} elsif ($lat >= 50) {
		$clat = 45.0;
	} elsif ($lat >= 45) {
		$clat = 40.0;
	} elsif ($lat >= 40) {
		$clat = 35.0;
	} elsif ($lat >= 35) {
		$clat = 30.0;
	} elsif ($lat >= 25) {
		$clat = 25.0;
	} elsif ($lat >= 15) {
		$clat = $lat;
	} elsif ($lat < 15)  {
		$clat = 15.0;
	}

	# Reference longitude of HWRF domain
	$clon = POSIX::ceil(($lon + $lon_72)/2.0);
	if ($clon > ($lon +5.0)) {
		$clon = $lon +5.0;
	}
	if ($clon < ($lon -5.0)) {
		$clon = $lon -5.0;
	}

	if (defined $args{'output'}) {
		# Write the lat & lon file for the storm
		open(FILE, ">", $args{'output'}) or
		  croak "unable to write domain centre: ".$args{'output'}." $!";
		print FILE "$clat\n$clon\n";
		close(FILE);
	}

	TRACE_OUT;

	return $clat, $clon;
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
