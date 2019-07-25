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
# $Id: GSI.pm 124 2011-04-05 21:49:53Z tpbrown@ucar.edu $
#

package HWRF::GSI;

use 5.008002;
use strict;
use warnings;

use lib "..";
use Data::Dumper;
use HWRF::Component;
use Carp;
use Cwd             qw(abs_path cwd);
use POSIX           qw(uname);
use Verbose         qw(:all);

require Exporter;
our @ISA = qw(Exporter HWRF::Component);
our $VERSION = '0.02';

=head1 NAME

HWRF::GSI - Perl extensions for the HWRF Testing and Evaluation suite.

=head1 SYNOPSIS

  use HWRF::GSI;
  my $gsi = HWRF::GSI->new();
  $gsi->source();
  $gsi->compile();

=head1 DESCRIPTION

The B<HWRF::GSI> module provides Perl functions that are commonly used in
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

	my $gsi = HWRF::GSI->new();

Instantiates an HWRF::GSI object.

=cut

sub new {
	my $proto  = shift;
	my $class  = ref($proto) || $proto;
	my (%args) = @_;

	my $self = $class->SUPER::new(%args);

	$self->{NAME} = "GSI";

	# Load the defaults
	if (! defined $self->{DST}) {
		if (defined $self->{TOPLEVEL}) {
			$self->{DST} = abs_path($self->{TOPLEVEL} ."/GSI");
		} else {
			$self->{DST} = abs_path("GSI");
		}
	}
	if (! defined $self->{SVN}->{URL}) {
		$self->{SVN}->{URL} = "https://gsi.fsl.noaa.gov/svn/comgsi";
	}
	if (! defined $self->{SVN}->{DIR}) {
		$self->{SVN}->{DIR} = "/trunk";
	}
	if (! defined $self->{SVN}->{REV}) {
		$self->{SVN}->{REV} = "HEAD";
	}
	if (! defined $self->{SRC}->{PKG}) {
		$self->{SRC}->{PKG} = "comGSI_v2.tar.gz";
	}
	if (! defined $self->{CONFIG}->{PAR}) {
		$self->{CONFIG}->{PAR} = "dmpar,optimize";
	}
	if (! defined $self->{CONFIG}->{FILE}) {
		$self->{CONFIG}->{FILE} = "configure.gsi";
	}
	if (! defined $self->{EXES}) {
		$self->{EXES} = [
			"run/gsi.exe",
		];
	}
	if (! defined $self->{OUTPUTS}) {
		$self->{OUTPUTS} = [
			"gsiprd/wrfghost_d02/wrf_inout",
			"gsiprd/wrfinput_d01/wrf_inout",
		];
	}

        $self->use_renamer();  # let the renamer override the exe list

	return $self;
}

=head3 patch_config

        $c->patch_config(file => "arch/Config_new.pl");

The function B<patch_config> will patch the file F<arch/Config.pl>
unless the option I<file> is specified. It will call:
	SUPER::patch_config(file => "arch/Config_new.pl");
that will add on the first blank line in the file:
	select((select(STDOUT), $|=1)[0]);
This is to make STDOUT hot, so that perl flushes it whenever data
is written, even if it is not attached to a tty.

For I<GSI> it will also see if there is a file F<comGSI_v2.patch>
in the source directory, if it does exist it will call:
	patch -p1 < comGSI_v2.patch
on the file before calling B<SUPER::patch_config()>.

=cut

sub patch_config {
	my $self   = shift;

	TRACE_IN;

	# Change into our destination
	my $cwd = cwd();
	chdir($self->{DST}) or croak "can not change into ".$self->{DST}.": $!";

	if (-f "comGSI_v2.patch") {
		my $p_res=`patch -p1 <comGSI_v2.patch`;
	}

	chdir($cwd) or croak "can not change back into $cwd : $!";

	$self->SUPER::patch_config(@_);

	TRACE_OUT;
}

=head3 env

	%vars = $gsi->envs();

Set the environment variables needed to build GSI. This currently is

=over 4

=item LAPACK_PATH

=back

If you are linux this will be set to $MKLROOT/lib/$arch/.

=cut

sub envs {
	my $self = shift;
	my (%args) = @_;
	
	my $os = $^O;
	my $mach = (POSIX::uname())[4];
	my $arch = '';
	if ($mach =~ /x86_64/) {
		$arch = "intel64";
	} elsif ($mach =~ /i686/) {
		$arch = "32";
	}

	if ( $os =~ /linux/ and ! defined $ENV{LAPACK_PATH}) {
		if (defined $ENV{MKL}) {                # Jet
			$ENV{LAPACK_PATH} = $ENV{MKL};
		} elsif (defined $ENV{MKLROOT}) {       # Zeus
			my $mlib = $ENV{MKLROOT} ."/lib/$arch/";
			if ( -d $mlib) {
				$ENV{LAPACK_PATH} = $mlib;
			}
		}

#		if (! defined $ENV{LAPACK_PATH}) {
#			croak "unable to set LAPACK_PATH";
#		}
	}

	TRACE_OUT;

	return;
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
