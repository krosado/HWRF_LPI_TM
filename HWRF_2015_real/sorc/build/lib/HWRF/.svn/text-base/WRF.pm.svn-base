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
# $Id: WRF.pm 124 2011-04-05 21:49:53Z tpbrown@ucar.edu $
#

package HWRF::WRF;

use 5.008002;
use strict;
use warnings;

use lib "..";
use Data::Dumper;
use HWRF::Component;
use Cwd              qw(abs_path cwd);
use Carp;
use Verbose          qw(:all);

require Exporter;

our @ISA = qw(Exporter HWRF::Component);
our $VERSION = '0.02';

=head1 NAME

HWRF::WRF - Perl extensions for the HWRF Testing and Evaluation suite.

=head1 SYNOPSIS

  use HWRF::WRF;
  my $wrf = HWRF::WRF->new();
  $wrf->source();
  $wrf->compile();

=head1 DESCRIPTION

The B<HWRF::WRF> module provides Perl functions that are commonly used in
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

	my $wrf = HWRF::WRF->new();

Instantiates an HWRF::WRF object.

=cut

sub new {
	my $proto  = shift;
	my $class  = ref($proto) || $proto;

	my $self = $class->SUPER::new(@_);

	$self->{NAME} = "WRF";

	# Load the defaults
	if (! defined $self->{DST}) {
		if (defined $self->{TOPLEVEL}) {
			$self->{DST} = abs_path($self->{TOPLEVEL} ."/WRFV3");
		} else {
			$self->{DST} = abs_path("WRFV3");
		}
	}
	if (! defined $self->{SVN}->{URL}) {
		$self->{SVN}->{URL} = "https://svn-wrf-model.cgd.ucar.edu";
	}
	if (! defined $self->{SVN}->{DIR}) {
		$self->{SVN}->{DIR} = "/trunk";
	}
	if (! defined $self->{SVN}->{REV}) {
		$self->{SVN}->{REV} = "HEAD";
	}
	if (! defined $self->{SRC}->{PKG}) {
		$self->{SRC}->{PKG} = "WRFV3.tar.gz";
	}
	if (! defined $self->{CONFIG}->{PAR}) {
		$self->{CONFIG}->{PAR} = "dmpar";
	}
	if (! defined $self->{CONFIG}->{FILE}) {
		$self->{CONFIG}->{FILE} = "configure.wrf";
	}
	if (! defined $self->{EXES}) {
		$self->{EXES} = [
			"main/real_nmm.exe",
			"main/wrf.exe",
		];
	}

        $self->use_renamer();  # let the renamer override the exe list

	return $self;
}

=head3 env

	%vars = $wrf->envs();

Set the environment variables needed to build WRF. This currently is

=over 4

=item HWRF = 1

=item WRF_NMM_CORE = 1

=item WRF_NMM_NEST = 1

=back

and, if WRFIO_NCD_LARGE_FILE_SUPPORT is not yet defined, its value will
be set by using detect_64bit_support() to automatically detect whether
the NetCDF installation supports the 64 bit offset file format.

=cut

sub envs {
	my $self = shift;
	my (%args) = @_;
	my $vars = {
		HWRF         => 1,
		WRF_NMM_CORE => 1,
		WRF_NMM_NEST => 1,
                PNETCDF_QUILT => 1,
	};

	TRACE_IN;

	my $hwrf   = HWRF->new();
	my ($nven, $nver, $is64) = $hwrf->netcdf();
	my ($cven, $cver, $cc, $fc, $cdebug, $fdebug) =
		$hwrf->compiler(vendor => $self->{COMPILER}->{VENDOR});

	if ($is64 and ! defined $ENV{WRFIO_NCD_LARGE_FILE_SUPPORT}) {
		$vars->{'WRFIO_NCD_LARGE_FILE_SUPPORT'} = 1;
	}

	if ($cven eq "AIX" and $fc eq "xlf") {
		$vars->{'IBM_REDUCE_BUG_WORKAROUND'} = 1;
	}

	while (my ($k, $v) = each(%$vars)) {
		if (defined $ENV{$k} and $ENV{$k} ne $v) {
			$ENV{$k} = $v;
			VERB_3("Setting $k = $v");
		}
	}

	TRACE_OUT;

        return $vars;
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
	chdir($self->{DST}) or croak "can not change into ".$self->{DST}.": $!";

	# Slurp in the makefile
	open (FILE, '<', $self->{CONFIG}->{FILE})
		or croak "unable to slurp in ". $self->{CONFIG}->{FILE} .": $!";
	my @file = <FILE>;
	close(FILE);

	# Replace the debugging flags
	open (FILE, '>', $self->{CONFIG}->{FILE})
		or croak "unable to open ". $self->{CONFIG}->{FILE} .": $!";
	foreach my $line (@file) {
		if ($self->{DEBUG} and $line =~ /^(FCDEBUG)\s*=/) {
			print FILE "$1 = ".$self->{COMPILER}->{FC_DEBUG}."\n";
		} else {
			print FILE $line;
		}
	}
	close(FILE);

	chdir($cwd) or croak "can not change back into $cwd : $!";

	TRACE_OUT;

}

=head3 patch_config

	$c->patch_config();

The function B<patch_config> will patch the file F<arch/Config_new.pl>
unless the option I<file> is specified. It will add on the first
blank line in the file:
	select((select(STDOUT), $|=1)[0]);
This is to make STDOUT hot, so that perl flushes it whenever data
is written, even if it is not attached to a tty.

=cut

sub patch_config {
	my $self   = shift;

	TRACE_IN;

	# By default WRF uses Config_new.pl now....
	if (@_) {
		$self->SUPER::patch_config(@_);
	} else {
		$self->SUPER::patch_config(file => "arch/Config_new.pl");
	}

	TRACE_OUT;

}

=head3 compile

	$c->compile();

The function B<compile> will compile the WRF module, writing stdout to
F<compile.log> and stderr to F<compile.err>.

This really just calls

	$c->SUPER::compile(args => 'nmm_real');

=cut

sub compile {
	my $self   = shift;

	TRACE_IN;

	if (@_) {
		$self->SUPER::compile(@_);
	} else {
		$self->SUPER::compile(args => 'nmm_real');
	}

	TRACE_OUT;
}

sub configure_options {
    my @a=('-f');
    return @a;
}

=head1 SEE ALSO

perl (1),
DTC L<http://www.dtcenter.org/>,
HWRF L<http://www.dtcenter.org/HurrWRF/users/index.php>
WRF L<http://www.dtcenter.org/wrf-nmm/users>

=head1 AUTHOR

Timothy P Brown, E<lt>Timothy.P.Brown@noaa.govE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2011 by Timothy P Brown


=cut

1;
__END__
