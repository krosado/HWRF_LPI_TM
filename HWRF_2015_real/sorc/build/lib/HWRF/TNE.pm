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
# $Id: TNE.pm 123 2011-04-05 15:59:19Z tpbrown@ucar.edu $
#

package HWRF::TNE;

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

HWRF::TNE - Perl extensions for the HWRF Testing and Evaluation suite.

=head1 SYNOPSIS

  use HWRF::TNE;
  my $tne = HWRF::TNE->new();
  $tne->source();
  $tne->compile();

=head1 DESCRIPTION

The B<HWRF::TNE> module provides Perl functions that are commonly used in
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

	my $tne = HWRF::TNE->new();

Instantiates an HWRF::TNE object.

=cut

sub new {
	my $proto  = shift;
	my $class  = ref($proto) || $proto;
	my (%args) = @_;

	my $self = $class->SUPER::new(%args);

	$self->{NAME} = "HWRF TnE";

	# Load the defaults
	if (! defined $self->{DST}) {
		if (defined $self->{TOPLEVEL}) {
			$self->{DST} = abs_path($self->{TOPLEVEL} ."/src");
		} else {
			$self->{DST} = abs_path("src");
		}
	}
	if (! defined $self->{SVN}->{URL}) {
		$self->{SVN}->{URL} = "https://svn-dtc-hwrf-tne.cgd.ucar.edu";
	}
	if (! defined $self->{SVN}->{DIR}) {
		#$self->{SVN}->{DIR} = "/trunk";
		$self->{SVN}->{DIR} = "/branches/cc";
	}
	if (! defined $self->{SVN}->{REV}) {
		$self->{SVN}->{REV} = "HEAD";
	}
	if (! defined $self->{SRC}->{PKG}) {
		$self->{SRC}->{PKG} = "HWRF-TNE.tar.gz";
	}

	return $self;
}

## Internal initialization function. This is not exported and should not
## be called by the calling application.
#sub _init {
#	my $proto = shift;
#	my $class = ref($proto) || $proto;
#	my $self = shift;
#	my %args = $_[0];
#
#}

=head3

	$tne->patch_config();

This function within the TNE package does nothing as there is no
arch/Config.pl file to patch to make stdout hot no matter what.

It is here as a placeholder so as to keep the I<build_HWRF> script clean.

=cut

sub patch_config {
	my $self   = shift;
	my (%args) = @_;
}

=head3

	$tne->configure();

This function within the TNE package does nothing.

It is here as a placeholder so as to keep the I<build_HWRF> script clean.

=cut

sub configure {
	my $self   = shift;
	my (%args) = @_;
}

=head3

	$tne->compile();

This function within the TNE package does nothing.

It is here as a placeholder so as to keep the I<build_HWRF> script clean.

=cut

sub compile {
	my $self   = shift;
	my (%args) = @_;
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
