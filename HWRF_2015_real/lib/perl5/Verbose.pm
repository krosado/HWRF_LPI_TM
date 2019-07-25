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
# $Id: Verbose.pm 3632 2015-06-25 14:02:57Z samuel.trahan@noaa.gov $
#

package Verbose;

use 5.008002;
use strict;
use warnings;

use Data::Dumper;
use POSIX qw(strftime);

require Exporter;

our @ISA = qw(Exporter);
our $VERSION = '0.01';

=head1 NAME

Verbose - Perl routines for logging messages.

=head1 SYNOPSIS

  use Verbose qw();

  DEBUG "Entering"
  open (FILE, "< foo") or FATAL "Unable to open file foo: $!";

=head1 DESCRIPTION

The Verbose module provides Perl functions to report messages.

A lot of these routines were blatantly copied from B<Log::Log4perl>
with ideas also coming from B<Logger::Logger>.


=head2 EXPORT

I<Everything is exported by default>.

=over 4

=item VERB_1("message");
=item VERB_2("message");
=item VERB_3("message");
=item VERB_4("message");
=item VERB_5("message");
=item TRACE_IN;
=item TRACE_OUT;

=back

=cut

our %EXPORT_TAGS = ( 'all' => [ qw(
	TRACE_IN
	TRACE_OUT
	VERB_1
	VERB_2
	VERB_3
	VERB_4
	VERB_5
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

=head2 METHODS

=head3 VERB_1..VERB_5

	VERB_1("value of foo: $foo");

All the B<VERB_> methods check to make sure B<$main::VERBOSE> is greater
than or equal to the value of the method called then print the message on
STDERR. That is if $main::VERBOSE = 1 and you call B<VERB_1("here")> the
message will be printed, however if you call B<VERB_2("there")> nothing
is printed.

=head3 TRACE_IN TRACE_OUT

	TRACE_IN;

These subroutines print the message "Entering" and "Leaving" a subroutine
when $main::VERBOSE is greater than or equal to 5.

=cut

sub VERB_1    { __message(1, @_) };
sub VERB_2    { __message(2, @_) };
sub VERB_3    { __message(3, @_) };
sub VERB_4    { __message(4, @_) };
sub VERB_5    { __message(5, @_) };
sub TRACE_IN  { __message(5, "Entering") };
sub TRACE_OUT { __message(5, "Leaving" ) };

=head3 __message

This is an internal subroutine that is not exported and should not
be called outside fo the B<Verbose> package. It calls B<__caller_info()>
to obtain the calling information and prints a formatted message to
STDERR. Of the form:
"time [file:line] package::subroutine message\n"

For example if called as B<__message("here I am")> then it will print:
Jul 26 08:43:44 [./foo: 13] main::goo here I am

=cut

sub __message {
	my ($level, @message) = @_;
	if (defined $main::VERBOSE and $main::VERBOSE >= $level) {
		my ($pkg, $sub, $file, $line, $time) = __caller_info();
		print STDERR "$time [$file:$line] $pkg"."::".$sub." @message\n";
	}
	return
}

=head3 __caller_info

This is an internal subroutine that is not exported and should not
be called outside of the B<Verbose> package.

It obtains the caller information and returns:

=over 4

=item package

=item subroutine

=item file name

=item line number

=item current time

=back

=cut

sub __caller_info {
	my $self = shift;

	my ($pkg, $sub, $file, $line, $now);

	# get the time
	$now = strftime("%b %d %H:%M:%S", localtime);

	# get the callers
	my @caller_1 = caller(2);
	my @caller_2 = caller(3);

	# caller parameters
	$pkg  = $caller_1[0];
	$file = $caller_1[1];
	$line = $caller_1[2];

	# Get the last package name
	$pkg = (split(/::/, $pkg))[-1];

	# Get the last of the filename (non-UNIX!!!)
	$file = (split(/\//, $file))[-1];

	# subroutine may be the main of a package.
	if (defined $caller_2[3]) {
		# if we are an eval find our parent
		($caller_2[3] =~ /eval/) ? ($sub = (caller(4))[3])
					 : ($sub = $caller_2[3]);
		$sub =~ s/^.*?::(.*)$/$1/;
		# get the last subroutine name
		$sub = (split(/::/, $sub))[-1];
	} else {
		$sub = "main";
	}

	return ($pkg, $sub, $file, $line, $now);
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
