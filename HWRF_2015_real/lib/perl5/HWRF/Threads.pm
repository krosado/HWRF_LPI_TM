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
# $Id: Threads.pm 112 2011-03-31 18:17:47Z tpbrown@ucar.edu $
#

package HWRF::Threads;

use 5.008002;
use strict;
use warnings;
use threads::shared;

use lib "..";
use Cwd             qw(abs_path cwd);
our $cwd = cwd;                       # Per thread cwd, init on startup from cwd
our $cwd_mutex : shared;              # Variable we use to sync
our $Cwd_cwd = \&Cwd::cwd;            # Alias Cwd::cwd()
*Cwd::cwd = *_cwd;                    # Hijack of Cwd::cwd()

our $VERSION = '0.01';

=head1 NAME

HWRF::Threads - Thread safe OS calls

=head1 SYNOPSIS

  use HWRF::Threds;
  use Cwd;
  chdir("/tmp");
  threads->create(sub { chdir("/usr") } )->join();
  print cwd() eq '/tmp' ? "ok" : "nok";

=head1 DESCRIPTION

The B<HWRF::Threads> module provides Perl functions that are commonly used in
interacting with the operating system.

=head2 EXPORT

None by default.

=cut

#our %EXPORT_TAGS = ( 'all' => [ qw(
#) ] );

#our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

=head2 METHODS

=head3 _cwd

Thread safe call to cwd().

=cut

sub _cwd {
	lock($cwd_mutex);
	CORE::chdir($cwd);
	$Cwd_cwd->(@_);
}

=head3 chdir

Override the I<GLOBAL> B<chdir()> function.

=cut

*CORE::GLOBAL::chdir = sub {
	lock($cwd_mutex);
	CORE::chdir($_[0]) || return undef;
	$cwd = $Cwd_cwd->();
};

=head3 mkdir

Override the I<GLOBAL> B<mkdir()> function.

=cut

*CORE::GLOBAL::mkdir = sub {
	lock($cwd_mutex);
	CORE::chdir($cwd);
	if(@_ > 1) {
		CORE::mkdir($_[0], $_[1]);
	} else {
		CORE::mkdir($_[0]);
	}
};

=head3 rmdir

Override the I<GLOBAL> B<rmdir()> function.

=cut

*CORE::GLOBAL::rmdir = sub {
	lock($cwd_mutex);
	CORE::chdir($cwd);
	CORE::rmdir($_[0]);
};

=head3 open

Override the I<GLOBAL> B<open()> function.

=cut

*CORE::GLOBAL::open = sub (*;$@) {
	lock($cwd_mutex);
	CORE::chdir($cwd);
	if (defined($_[0])) {
		use Symbol qw();
		my $handle = Symbol::qualify($_[0],(caller)[0]);
		no strict 'refs';
		if (@_ == 1) {
			return CORE::open($handle);
		} elsif (@_ == 2) {
			return CORE::open($handle, $_[1]);
		} else {
			return CORE::open($handle, $_[1], @_[2..$#_]);
		}
	} else {
		if (@_ == 1) {
			return CORE::open($_[0]);
		} elsif (@_ == 2) {
			return CORE::open($_[0], $_[1]);
		} else {
			return CORE::open($_[0], $_[1], @_[2..$#_]);
		}
	}
};

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
