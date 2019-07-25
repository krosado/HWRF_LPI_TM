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
# $Id: UPP.pm 156 2011-05-16 20:24:43Z timothy.p.brown@noaa.gov $
#

package HWRF::UPP;

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

HWRF::UPP - Perl extensions for the HWRF Testing and Evaluation suit.

=head1 SYNOPSIS

  use HWRF::UPP;
  my $wpp = HWRF::UPP->new();
  $wpp->source();
  $wpp->compile();

=head1 DESCRIPTION

The B<HWRF::UPP> module provides Perl functions that are commonly used in
building and running the Unified Post Processor.

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

	my $wpp = HWRF::UPP->new();

Instantiates an HWRF::UPP object.

=cut

sub new {
	my $proto  = shift;
	my $class  = ref($proto) || $proto;
	my (%args) = @_;

	my $self = $class->SUPER::new(%args);

	$self->{NAME} = "UPP";

	# Load the defaults
	if (! defined $self->{DST}) {
		if (defined $self->{TOPLEVEL}) {
			$self->{DST} = abs_path($self->{TOPLEVEL} ."/UPP");
		} else {
			$self->{DST} = abs_path("UPP");
		}
	}
	if (! defined $self->{SVN}->{URL}) {
		$self->{SVN}->{URL} =
			"https://svn-dtc-unifiedpostproc.cgd.ucar.edu";
	}
	if (! defined $self->{SVN}->{DIR}) {
		$self->{SVN}->{DIR} = "/trunk";
	}
	if (! defined $self->{SVN}->{REV}) {
		$self->{SVN}->{REV} = "HEAD";
	}
	if (! defined $self->{SRC}->{PKG}) {
		$self->{SRC}->{PKG} = "UPP0.5c.tar.gz";
	}
	if (! defined $self->{CONFIG}->{PAR}) {
		$self->{CONFIG}->{PAR} = "dmpar";
	}
	if (! defined $self->{CONFIG}->{FILE}) {
		$self->{CONFIG}->{FILE} = "configure.upp";
	}
	if (! defined $self->{EXES}) {
		$self->{EXES} = [
			"bin/copygb.exe",
			"bin/cnvgrib.exe",
			"bin/unipost.exe",
		];
	}

        $self->use_renamer();  # let the renamer override the exe list

	return $self;
}

=head3 env

	%vars = $wrf->envs();

Set the environment variables needed to build UPP. This currently is just HWRF=1.

=cut

sub envs {
	my $self = shift;
	my (%args) = @_;

	TRACE_IN;
	$ENV{'HWRF'} = 1;
	#$ENV{LIB_G2_PATH}=$self->{DST};
	TRACE_OUT;

        return { 
            HWRF=> 1,
        };
	#LIB_G2_PATH => $self->{DST}
}

=head3 update_conf

	$c->update_conf();

The function B<update_conf> will patch the configuration file adding/changing
any options that are specified.

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
	my @contents = <FILE>;
	close(FILE);

	# Replace the debugging flags and CPP
	my $i = 0;
	foreach my $l (@contents) {
		if ($self->{DEBUG} and $l =~ /^(FFLAGS)\s*=(.*)$/) {
			my $opt = $2;
			$opt =~ s/\b-O[1-9]\b//g;
			$contents[$i] =
			  "FFLAGS = ".$self->{COMPILER}->{FC_DEBUG}." $opt\n";
		}
		if ($^O =~ /aix/i and $l =~ /^(CPP)\s*=(.*)$/) {
			$contents[$i] = "CPP = /usr/ccs/lib/cpp\n";
		}
		++$i;
	}

	open (FILE, '>', $self->{CONFIG}->{FILE}) or croak $!;
	print FILE @contents;
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
