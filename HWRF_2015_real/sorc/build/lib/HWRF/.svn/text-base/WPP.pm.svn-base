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
# $Id: WPP.pm 152 2011-05-10 19:04:36Z tpbrown@ucar.edu $
#

package HWRF::WPP;

use 5.008002;
use strict;
use warnings;

use lib "..";
use Data::Dumper;
use HWRF::Component;
use Carp;
use Cwd              qw(abs_path cwd);
use Verbose          qw(:all);

require Exporter;

our @ISA = qw(Exporter HWRF::Component);
our $VERSION = '0.01';

=head1 NAME

HWRF::WPP - Perl extensions for the HWRF Testing and Evaluation suite.

=head1 SYNOPSIS

  use HWRF::WPP;
  my $wpp = HWRF::WPP->new();
  $wpp->source();
  $wpp->compile();

=head1 DESCRIPTION

The B<HWRF::WPP> module provides Perl functions that are commonly used in
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

	my $wpp = HWRF::WPP->new();

Instantiates an HWRF::WPP object.

=cut

sub new {
	my $proto  = shift;
	my $class  = ref($proto) || $proto;
	my (%args) = @_;

	my $self = $class->SUPER::new(%args);

	$self->{NAME} = "WPP";

	# Load the defaults
	if (! defined $self->{DST}) {
		if (defined $self->{TOPLEVEL}) {
			$self->{DST} = abs_path($self->{TOPLEVEL} ."/WPPV3");
		} else {
			$self->{DST} = abs_path("WPPV3");
		}
	}
	if (! defined $self->{SVN}->{URL}) {
		$self->{SVN}->{URL} =
			"https://svn-dtc-wrfpostproc.cgd.ucar.edu/";
	}
	if (! defined $self->{SVN}->{DIR}) {
		$self->{SVN}->{DIR} = "/trunk";
	}
	if (! defined $self->{SVN}->{REV}) {
		$self->{SVN}->{REV} = "HEAD";
	}
	if (! defined $self->{SRC}->{PKG}) {
		$self->{SRC}->{PKG} = "WPPV3.tar.gz";
	}
	if (! defined $self->{CONFIG}->{PAR}) {
		$self->{CONFIG}->{PAR} = "dmpar";
	}
	if (! defined $self->{CONFIG}->{FILE}) {
		$self->{CONFIG}->{FILE} = "configure.wpp";
	}
	if (! defined $self->{EXES}) {
		$self->{EXES} = [
			"exec/copygb.exe",
			"exec/ndate.exe",
			"exec/wrfpost.exe",
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
		if ($self->{DEBUG} and $line =~ /^(FFLAGS)\s*=(.*)$/) {
			$2 =~ s/\b-O[1-9]\b//g;
			print FILE "$1 = $2 ".$self->{COMPILER}->{FC_DEBUG}."\n";
		} else {
			print FILE $line;
		}
	}
	close(FILE);

	chdir($cwd) or croak "can not change back into $cwd : $!";

	TRACE_OUT;
}

=head3 patch_config

        $c->patch_config(file => "arch/Config_new.pl");

The function B<patch_config> will patch the file F<arch/Config.pl>
unless the option I<file> is specified. It will 

=over

=item Change all occurrences of B<atm_pom> to B<atm_ocn>

=item Add on the first blank line in the file:
	select((select(STDOUT), $|=1)[0]);

=back

This is to make STDOUT hot, so that perl flushes it whenever
data is written, even if it is not attached to a tty.

=cut

sub patch_config {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	my $file;
	if (defined $args{'file'}) {
		$file = $args{'file'};
	} else {
		$file = "arch/Config.pl";
	}

	# Change into our destination
	my $cwd = cwd();
	my $dir = abs_path($self->{DST});
	chdir($dir) or croak "can not change into $dir : $!";

	# Slurp in the file
	open (CONF, '<', $file) or croak $!;
	my @file = <CONF>;
	close(CONF);

	# Replace atm_pom with atm_ocn and write it out
	open (CONF, '>', $file) or croak $!;
	foreach my $line (@file) {
		$line =~ s/atm_pom/atm_ocn/g;
		print CONF $line;
	}
	close(CONF);

	chdir($cwd) or croak "can not change back into $cwd : $!";

	$self->SUPER::patch_config(@_);

	TRACE_OUT;
}

=head3

        $c->patch();

The function B<patch> will apply any package/compiler/site specific patches
needed before compiling the module.

In the case of WPP, this adds I<-ftz> to the I<FFLAGS> when using the
Intel compiler. This prevents float point exceptions in the w3 library,
namely lib/w3lib/getbit.f:65
	NINT(G(I1)*S)/S

=cut

sub patch {
	my $self   = shift;
	my (%args) = @_;

	if ($self->{COMPILER}->{VENDOR} ne "Intel") {
		return;
	}

	TRACE_IN;

	my $file;
	if (defined $args{'file'}) {
		$file = $args{'file'};
	} else {
		$file = $self->{CONFIG}->{FILE};
	}

	# Change into our destination
	my $cwd = cwd();
	my $dir = abs_path($self->{DST});
	chdir($dir) or croak "can not change into $dir : $!";

	# Slurp in the file
	open (CONF, '<', $file) or croak $!;
	my @file = <CONF>;
	close(CONF);

	# Replace atm_pom with atm_ocn and write it out
	open (CONF, '>', $file) or croak $!;
	foreach my $line (@file) {
		if ($line =~ /^FFLAGS/) {
			chomp($line);
			print CONF "$line -ftz\n";
		} else {
			print CONF $line;
		}
	}
	close(CONF);

	$self->SUPER::patch(@_);

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
