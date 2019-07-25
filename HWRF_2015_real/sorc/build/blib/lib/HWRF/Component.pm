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
# $Id: Component.pm 156 2011-05-16 20:24:43Z timothy.p.brown@noaa.gov $
#

package HWRF::Component;

use 5.008002;
use strict;
use warnings;

use Data::Dumper;
use Carp;
use Fcntl         qw(:flock O_CREAT O_RDWR O_RDONLY SEEK_SET SEEK_CUR SEEK_END);
use POSIX         qw(:sys_wait_h);
use IPC::Open3;
use IO::Select;
use IO::Handle;
use Symbol        qw(gensym);
use Cwd           qw(abs_path cwd);
use Storable      qw(nfreeze thaw);
use File::Basename;
#use URI;
#use Net::Netrc;
use HWRF;
use Verbose       qw(:all);

require Exporter;

our @ISA = qw(Exporter);
our $VERSION = '0.02';

=head1 NAME

HWRF::Component - Perl generic module for HWRF Components.

=head1 SYNOPSIS

  package HWRF::TNE;
  our @ISA = qw(Exporter HWRF::Component);

=head1 DESCRIPTION

The HWRF::Component module provides a skelaton class for all the HWRF
modules.

=head2 EXPORT

None by default.

=cut

our %EXPORT_TAGS = ( 'all' => [ qw(
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

### install get/set accessors for this object.
#for my $key ( keys %$tmpl ) {
#	no strict 'refs';
#	*{__PACKAGE__."::$key"} = sub {
#		my $self = shift;
#		$self->{$key} = $_[0] if @_;
#		return $self->{$key};
#	}
#}

=head2 METHODS

=head3 new

	my $c = HWRF::Component->new();

Instantiates an HWRF::Component object.

=cut

sub new {
	my $proto  = shift;
	my $class  = ref($proto) || $proto;
	my (%args) = @_;

	my $self = {};

	bless($self, $class);
	__PACKAGE__->_init($self, %args);
	return $self;
}

# Internal initialization function. This is not exported and should not
# be called by the calling application.
sub _init {
	my $proto  = shift;
	my $class  = ref($proto) || $proto;
	my $self   = shift;
	my (%args) = @_;

        # Store the HWRF::Renamer for later use, if one is given:
        if(defined $args{renamer}) {
            $self->{RENAMER}=$args{renamer};
        }

        $self->{USE_RENAMER}=0; # set to 1 by use_renamer if renamer is to be used

	# Package/Program human readable name
	$self->{NAME}     = undef;

	# Toplevel destination
	$self->{TOPLEVEL} = cwd();
	if (defined $args{'toplevel'}) {
		$self->{TOPLEVEL} = abs_path($args{'toplevel'});
	}

	# Source destination
	$self->{DST}      = undef;
	if (defined $args{'dst'}) {
		$self->{DST} = abs_path($args{'dst'});
	}

	# Source (location on disk for a stable revision tarball)
	$self->{SRC}      = {
		'DIR' => undef,
		'PKG' => undef,
		'RET' => 1,
	};
	if (defined $args{'src'} and ref($args{'src'}) eq 'HASH') {
		while (my ($k, $v) = each %{$args{'src'}}) {
			if ($k eq "dir") {
				$self->{SRC}->{DIR} = $v;
			} elsif ($k eq "file") {
				$self->{SRC}->{PKG} = $v;
			} elsif ($k eq "retrieve") {
				$self->{SRC}->{RET} = $v;
			}
		}
	}

	# Patches (location on disk of patches)
	$self->{PATCHES}  = undef;
	if (defined $args{'patches'}) {
		$self->{PATCHES} = $args{'patches'};
	}

	# Subversion
	$self->{SVN}      = {
		'URL'      => undef,
		'DIR'      => undef,
		'REV'      => undef,
		'USERNAME' => undef,
		'PASSWORD' => undef,
	};
	if (defined $args{'svn'} and ref($args{'svn'}) eq 'HASH') {
		while (my ($k, $v) = each %{$args{'svn'}}) {
			if ($k eq "url") {
				$self->{SVN}->{URL} = $v;
			} elsif ($k eq "rev") {
				$self->{SVN}->{REV} = $v;
			} elsif ($k eq "path") {
				$self->{SVN}->{DIR} = $v;
			} elsif ($k eq "username") {
				$self->{SVN}->{USERNAME} = $v;
			} elsif ($k eq "password") {
				$self->{SVN}->{PASSWORD} = $v;
			}
		}
	}

	# Compiler
	$self->{COMPILER} = {
		'VENDOR'   => undef,
		'VERSION'  => undef,
		'CC'       => undef,
		'CC_DEBUG' => undef,
		'FC'       => undef,
		'FC_DEBUG' => undef,
	};
	if (defined $args{'compiler'} and ref($args{'compiler'}) eq 'HASH') {
		while (my ($k, $v) = each %{$args{'compiler'}}) {
			if ($k eq "vendor") {
				$self->{COMPILER}->{VENDOR} = $v;
			} elsif ($k eq "version") {
				$self->{COMPILER}->{VERSION} = $v;
			} elsif ($k eq "cc") {
				$self->{COMPILER}->{CC} = $v;
			} elsif ($k eq "cc_debug") {
				$self->{COMPILER}->{CC_DEBUG} = $v;
			} elsif ($k eq "fc") {
				$self->{COMPILER}->{FC} = $v;
			} elsif ($k eq "fc_debug") {
				$self->{COMPILER}->{FC_DEBUG} = $v;
			}
		}
	}

	# Configuration parallel mode and other options
	$self->{CONFIG} = {
		'PAR'  => undef,
		'OPT'  => undef,
		'FILE' => undef,
	};
	if (defined $args{'configure'} and ref($args{'configure'}) eq 'HASH') {
		while (my ($k, $v) = each %{$args{'configure'}}) {
			if ($k eq "mode") {
				$self->{CONFIG}->{PAR} = $v;
			} elsif ($k eq "options") {
				$self->{CONFIG}->{OPT} = $v;
			} elsif ($k  eq "file") {
				$self->{CONFIG}->{FILE} = $v;
			}
		}
	}

	# Built executables
	$self->{EXES} = undef;
	if (defined $args{'execs'}) {
		if (ref($args{'execs'} eq 'ARRAY')) {
			$self->{EXES} = $args{'execs'};
		}
	}

	# Component output file names
	$self->{OUTPUTS} = undef;
	if (defined $args{'outs'}) {
		if (ref($args{'outs'} eq 'ARRAY')) {
			$self->{OUTPUTS} = $args{'outs'};
		}
	}

	# Build Information filename
	$self->{INFO} = undef;
	if (defined $args{'info'}) {
		$self->{INFO} = $args{'info'};
	}

	$self->{VERBOSE} = 0;
	if (defined $args{'verbose'}) {
		$self->{VERBOSE} = $args{'verbose'};
	}

	$self->{DEBUG} = undef;
	if (defined $args{'debug'}) {
		$self->{DEBUG} = 1;
	}

}

=head3

	$c->use_renamer();

Updates the internal list of executables from the HWRF::Renamer
specified when running HWRF::Component->new.  Unless this is called,
the HWRF::Renamer will be unused by all other routines.

=cut

sub use_renamer() {
    my $self=shift;

    # Do nothing unless we have a renamer:
    return unless defined $self->{RENAMER};

    my $simplename=basename($self->{DST});
    my @exes=$self->{RENAMER}->exes_for($simplename);
    if($#exes<0) {
        croak("Component $simplename has no executables!!  Please update the Renamer's executable list.\n");
    }
    $self->{EXES}=[ @exes ];
    $self->{USE_RENAMER}=1;
}

=head3

	$c->run(cmd => "command",
		args => \@arguments,
		envs => \%vars,
		stdout => "stdout.txt",
		stderr => "stderr.txt"
	);

The function B<run> will run the B<command> using I<IPC::Open3>.

If there are any arguments they will be appended to the command
when run.

Standard output and error can be saved to files if desired. If only
a filename for standard output is give, standard error is sent to the
same file.

If the verbose level is above 5, standard output and error are printed
to STDOUT and STDERR as well.

If there is an error (non-zero exit status from the command),
the routine will die using B<carp>.

=cut

sub run {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	if (!defined $args{'cmd'}) {
		croak "no command to run";
	}
	my @cmd;
	my ($wtr, $rdr, $err, $sout, $serr);

	# set up any package env vars
	if (defined $args{'envs'}) {
		push(@cmd, "env");
		while (my ($k, $v) = each(%{$args{'envs'}})) {
			push(@cmd, "$k=$v");
		}
	}

	# define the command
	push(@cmd, $args{'cmd'});

	# add any arguments
	if (defined $args{'args'}) {
		if (ref($args{'args'}) eq "ARRAY") {
			push(@cmd, @{$args{'args'}});
		} else {
			push(@cmd, $args{'args'});
		}
	}

	if (defined $args{'stdout'}) {
		open($sout, "> $args{'stdout'}") or
			croak "can not open output: ".$args{'stdout'}." : $!";
		if (!defined $args{'stderr'}) {
			$serr = $sout;
		}
	}
	if (defined $args{'stderr'}) {
		open($serr, "> $args{'stderr'}") or
			croak "can not open output: ".$args{'stderr'}." : $!";
	}

	VERB_1("Running: @cmd");

	my $pid = open3($wtr, $rdr, $err, @cmd);
	my $sel = IO::Select->new();
	$sel->add($rdr, $err);
	outer: while (my @ready = $sel->can_read) {
		foreach my $handle (@ready) {
			my ($count, $buf);
			$count = sysread($handle, $buf, 4096);
			if (!defined($count) || $count == 0) {
				$sel->remove($handle);
				next;
			} else {
				if (fileno($handle) == fileno($rdr)) {
					VERB_5($buf);
					print $sout $buf;
				} elsif (fileno($handle) == fileno($err)) {
					VERB_5($buf);
					print $serr $buf;
				}
			}
		}
	}
	my $kid = waitpid($pid, 0);

	TRACE_OUT;

	if ($kid != $pid) {
		carp "running @cmd failed";
	}
}

=head3

	$c->svn_co(dst => 'foo', url => 'https://example.com',
		path => '/trunk', rev => 'HEAD');

The function B<svn_co> will issue a SVN checkout command.
Options are:

=over 4

=item B<dst => 'foo'>

The destination directory to check the source into.

=item B<url => 'https://example.com'>

The repository URL.

=item B<path => '/trunk'>

Any futher path specifications for the URL.

=item B<rev => 'HEAD'>

The repository revision number.

=back

This command calls I<$self->run()>

=cut

sub svn_co {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	# Update our configuration
	if (defined $args{'dst'}) {
		$self->{DST} = abs_path($args{'dst'});
	}
	if (defined $args{'rev'}) {
		$self->{SVN}->{REV} = $args{'rev'};
	}
	if (defined $args{'url'}) {
		$self->{SVN}->{URL} = $args{'url'};
	}
	if (defined $args{'path'}) {
		$self->{SVN}->{DIR} = $args{'path'};
	}

	if (! defined $self->{DST}) {
		croak "no destination specified";
	}

	# build up the arguments
	my @args = ("-q", "--non-interactive", "--trust-server-cert",
		"co", "-r", $self->{SVN}->{REV},
		$self->{SVN}->{URL} . $self->{SVN}->{DIR},
		$self->{DST});

	$self->run(cmd => "svn", args => \@args) or
		croak "couldn't checkout " . $self->{SVN}->{URL};

#	my $err = $self->run(cmd => "svn", args => \@args)
#		
#	if (!$err) {
#		# Check out failed for some reason, try with our ~/.netrc
#		my $url = URI->new($self->{SVN}->{URL});
#		my $host = $url->host();
#		my $mach = Net::Netrc->lookup($host);
#		my ($user, $pass, $acc) = $mach->lpq;
#		unshift(@args, "--username", $user, "--password", $pass);
#		$err = $self->run(cmd => "svn", args => \@args);
#	}
#
#	if (!$err) {
#		croak "couldn't checkout " . $self->{SVN}->{URL};
#	}

	TRACE_OUT;
}

=head3 source

	$c->source();

The function B<source> will extract the source into the destination directory.

It will do nothing if B<$c->{SRC}->{RET}> is not set to 1 (default value).

If the package exists on the filesystem in a tar ball, it will extract
that using B<untar()> otherwise it will try and do an SVN check-out
using B<svn_co()>.

=cut

sub source {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	# Update our configuration
	if (defined $args{'dir'}) {
		$self->{SRC}->{DIR} = abs_path($args{'dir'});
	}
	if (defined $args{'file'}) {
		$self->{SRC}->{PKG} = $args{'file'};
	}
	if (defined $args{'retrieve'}) {
		$self->{SRC}->{RET} = $args{'retrieve'};
	}

	# If "retrieve" isn't set we return 0
	if (! $self->{SRC}->{RET}) {
		return;
	}

	# Use the tar file on the filesystem in preference to SVN source.
	# Check to see if
	# - PKG contains the full file
	# - DIR & PKG do
	# - SVN
	if (defined $self->{SRC}->{PKG} and -e $self->{SRC}->{PKG}) {
		$self->untar();
	} elsif (defined $self->{SRC}->{DIR} and
		 defined $self->{SRC}->{PKG} and
		 -e $self->{SRC}->{DIR} ."/". $self->{SRC}->{PKG}) {
		$self->untar();
	} elsif (defined $self->{SVN}->{URL}) {
		$self->svn_co();
	} else {
		croak "unable to reteieve source for " . $self->{NAME};
	}

	TRACE_OUT;

}

=head3 untar

	$c->untar();

The function B<untar> will a source tarball. It does this by calling
B<tar> and optionally B<gzip> or B<bzip2>.

It should be noted that B<Archive::Tar> could be a nicer solution.

=cut

sub untar {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	# Update our configuration
	if (defined $args{'dst'}) {
		$self->{DST} = abs_path($args{'dst'});
	}
	if (defined $args{'dir'}) {
		$self->{SRC}->{DIR} = $args{'dir'};
	}
	if (defined $args{'file'}) {
		$self->{SRC}->{PKG} = $args{'file'};
	}


	my $abs_filename = undef;
	if (defined $self->{SRC}->{DIR}) {
		$abs_filename = abs_path($self->{SRC}->{DIR} ."/".
			$self->{SRC}->{PKG});
	} else {
		$abs_filename = abs_path($self->{SRC}->{PKG});
	}
	if (! defined($abs_filename)) {
		croak "unable to resolve absolute path of " .
			$self->{SRC}->{PKG};
	}

	# Break the file name up
	my @suffs = (".tar", ".gz", ".bz2");
	my ($fn, $fp, $fs) = fileparse($abs_filename, @suffs);

	my @args = ();
	# Uncompress the file if need be
	if ( -f $abs_filename) {
		if ($fs =~ /\.gz/ ) {
			@args = ("-d", $abs_filename);
			$self->run(cmd => "gzip", args => \@args) or
				croak "couldn't uncompress " . $abs_filename;
			$abs_filename = $fp.$fn;
		} elsif ($fs =~ /\.bz2/ ) {
			@args = ("-d", $abs_filename);
			$self->run(cmd => "bzip2", args => \@args) or
				croak "couldn't uncompress " . $abs_filename;
			$abs_filename = $fp.$fn;
		}
	} elsif (-f $fp.$fn) {
		$abs_filename = $fp.$fn;
	} else {
		croak "can not find the file $abs_filename";
	}

	if ($abs_filename =~ /tar$/ and -f $abs_filename) {
		# Grab the directory that it will untar into
		my $dst_dir;
		sysopen(F, $abs_filename, O_RDONLY) or
			croak "can not open $abs_filename: $!";
		sysread(F, $dst_dir, 100);
		close(F);
		$dst_dir = cwd() . "/" . $dst_dir;

		# Untar the file
		@args = ("xf", $abs_filename);
		$self->run(cmd => "tar", args => \@args) or
			croak "couldn't untar " . $abs_filename;

		# Move the untared file to the destination
		if ($dst_dir ne $self->{DST} ."/") {
			@args = ("-s", $dst_dir, $self->{DST});
			$self->run(cmd => "ln", args => \@args) or
				croak "couldn't link $dst_dir to ".$self->{DST};
		}
	}

	TRACE_OUT;
}

=head3 env

	$c->envs();

Set the environment variables needed for the component.

=cut

sub envs {
	my $self   = shift;
	my (%args) = @_;

#	$self->__verbose_3((caller(0))[3], "Entering");
#	$self->__verbose_3((caller(0))[3], "Leaving");
	return undef;
}

=head3 configure

	$c->configure();

Configure the module. This will run I<configure> within the modules
source directory.

=cut

sub configure_options {
    my @a=();
    return @a;
}

sub configure {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	if (defined $args{'debug'}) {
		$self->{DEBUG} = 1;
	}

	# Update our configuration
	$self->compiler(%args);

	# Get the site config
	my $hwrf = HWRF->new();
	$hwrf->site(compiler => $self->{COMPILER}->{VENDOR});
	my $config = $hwrf->config();

	# See if we have any arguments
	if (defined $args{'mode'}) {
		$self->{CONFIG}->{PAR} = $args{'mode'};
	}

	if (defined $args{'options'}) {
		$self->{CONFIG}->{OPT} = $args{'options'};
	}

	# Add the HWRF options
	if (defined $config->{PAR}) {
		$self->{CONFIG}->{PAR} .= " " . $config->{PAR};
	}
	if (defined $config->{OPT}) {
		$self->{CONFIG}->{OPT} .= " " . $config->{OPT};
	}
	if (! defined $self->{CONFIG}->{FILE}) {
		$self->{CONFIG}->{FILE} = $config->{FILE};
	}

	# Change into our destination
	my $cwd = cwd();
	chdir($self->{DST}) or croak "can not change into ".$self->{DST}.": $!";

	# If we have any enviroment variables
	my $envs = $self->envs();

	# merge the hashes
	if (defined $args{'envs'}) {
		while (my ($k, $v) = each(%{$args{'envs'}})) {
			$envs->{$k} = $v;
		}
	}

	my @cmd;
	if (defined $envs) {
		push(@cmd, "env");
		while (my ($k, $v) = each(%{$envs})) {
			push(@cmd, "$k=$v");
		}
	}
        my @copts=$self->configure_options();
	push(@cmd, "./configure",@copts);

	# What we want to look for
	my $ven = defined $self->{COMPILER}->{VENDOR} ?
		quotemeta($self->{COMPILER}->{VENDOR}) : '';
	my $cc  = defined $self->{COMPILER}->{CC} ?
		quotemeta($self->{COMPILER}->{CC}): '';
	my $fc  = defined $self->{COMPILER}->{FC} ?
		quotemeta($self->{COMPILER}->{FC}): '';
	my $par = defined $self->{CONFIG}->{PAR}  ?
		$self->{CONFIG}->{PAR} : '';
	my $opt = defined $self->{CONFIG}->{OPT}  ?
		$self->{CONFIG}->{OPT} : '';

	# Get really chatty if we are on verbose => 10
        my @alloptlist=($ven,$cc,$fc,$par,$opt);
        VERB_4("Run @cmd");
        VERB_4("  want options: @alloptlist");

	my ($rel, $res);  # configure line and response number
	my $partial = ''; # partial line from sysread
	my $pid = open3(\*WRITE, \*READ, \*ERROR, @cmd);
	my $sel = IO::Select->new();

        my $found;
	$sel->add(\*READ, \*ERROR);
CONF:	while (my @ready = $sel->can_read(5)) {
		foreach my $handle (@ready) {
			my ($count, $buf);
			$count = sysread($handle, $buf, 4096);
			if ($count == 0) {
				$sel->remove($handle);
				close($handle);
				next;
			} else {
				substr($buf,0,0) = $partial;
				my @buf = split(/\r?\n/, $buf);
				if ($buf =~ /\n$/) {
					$partial = '';
				} else {
					$partial = pop(@buf);
				}
				foreach my $l (@buf) {
					if ($self->{VERBOSE} >= 10) {
						print "$l\n";
					}
					if ($l =~ /$ven|$fc|$cc/i  and
					    $l =~ /$par/i and
					    $l =~ /$opt/i ) {
                                            if ($l =~ m/(\d+)\.\s\($par\) .* $fc .* $cc .* /ix) {
                                                ($res) = ($l =~ m/(\d+)\.\s\($par\)/ix);
                                                $rel = $l;
                                                $found=1;
                                                last CONF;
                                            } else {
                                                ($res) = ($l =~ /(\d{1,})/);
                                                $rel = $l;
                                                $found=1;
                                                last CONF;
					    }
					}
				}
			}
		}
	}

        if(!$found) {
            croak("Cannot find configuration $ven $fc $cc $par $opt\n"
                  ."when running command \"".join(" ",@cmd)."\"\n");
        }

	# Print out the selection for medium or high verbosity levels:
	VERB_3("Using configuration #$res\n$rel");

	print WRITE "$res\n";
	close(WRITE);
	close(ERROR);

	my $kid = waitpid($pid, 0);
	if ($kid != $pid) {
		croak "configure exited\n";
	}

	# Update the configuration file
	$self->update_conf();

	# Apply any patches to the package
	$self->patch();

	chdir($cwd) or croak "can not change back into $cwd : $!";

	TRACE_OUT;
}

=head3 patch_config

	$c->patch_config(file => "arch/Config_new.pl");

The function B<patch_config> will patch the file F<arch/Config.pl>
unless the option I<file> is specified. It will add on the first
blank line in the file:
	select((select(STDOUT), $|=1)[0]);
This is to make STDOUT hot, so that perl flushes it whenever data
is written, even if it is not attached to a tty.

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

	my $hot = 'select((select(STDOUT), $|=1)[0]);';

	# Change into our destination
	my $cwd = cwd();
	chdir($self->{DST}) or croak "can not change into ".$self->{DST}.": $!";

	my $patched = 0;
	open (CONF, '<', $file) or croak "unable to read $file : $!";
	my @file = <CONF>;
	close(CONF);
	
	my $q_hot = quotemeta($hot);
	if (!grep(/$q_hot/, @file)) {
		open (CONF, '>', $file) or croak $!;
		foreach my $line (@file) {
			if (!$patched and
			    ($line =~ /^$/ or $line =~ /^(\s)*$/)) {
				print CONF "$hot\n";
				$patched = 1;
			} else {
				print CONF $line;
			}
		}
		close(CONF);
	}

	chdir($cwd) or croak "can not change back into $cwd : $!";

	TRACE_OUT;
}

=head3

        $c->patch();

The function B<patch> will apply any package/compiler/site specific
patches needed before compiling the module.

This is only a place holder function in this module.

=cut

sub patch {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	# Change into our destination
	my $cwd = cwd();
	chdir($self->{DST}) or croak "can not change into ".$self->{DST}.": $!";

	if (defined $self->{PATCHES}) {
		my @args = ("-p0", "<", $self->{PATCHES});
		$self->run(cmd    => "patch",
			   args   => @args);
	}

	chdir($cwd) or croak "can not change back into $cwd : $!";

	TRACE_OUT;
}

=head3 update_conf

	$c->update_conf();

The function B<update_conf> will update the configuration file adding/changing
any options that might be needed.

Typical usages:

=over 4

=item
Changing the debugging options to the C and Fortran compiler flags.

=item
Changing the path of CPP for different platforms.

=back

This is only a place holder function in this module.

=cut

sub update_conf {
	my $self   = shift;
	my (%args) = @_;

}

=head3 compile

	$c->compile();

Compile the module, writing stdout to F<compile.log> and stderr
to F<compile.err>.

=cut

sub compile {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	# Change into our destination
	my $cwd = cwd();
	chdir($self->{DST}) or croak "can not change into ".$self->{DST}.": $!";

	# If we have any environment variables
	my $envs = $self->envs();

	# merge the hashes
	if (defined $args{'envs'}) {
		while (my ($k, $v) = each(%{$args{'envs'}})) {
			$envs->{$k} = $v;
		}
	}

	$self->run(cmd    => "./compile",
		   args   => $args{'args'},
		   envs   => $envs,
		   stdout => "compile.log");

	chdir($cwd) or croak "can not change back into $cwd : $!";

	TRACE_OUT;
}

=head3 compiler

	$c->compiler();

The function B<compiler> sets the compiler options. These are:

=over

=item Vendor

The vendors name of the compiler suite.

=item Version

The version number of the compiler suite.

=item CC

The C compiler program name.

=item CC_DEBUG

The C flags to enable debugging. That is the compile unopimized and
debugging symbols flags.

=item FC

The Fortran compiler program name.

=item FC_DEBUG

The Fortran flags to enable debugging. That is the compile unopimized and
debugging symbols flags.

=back

=cut

sub compiler {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	# Get the site defaults
	my $hwrf = HWRF->new();
	my ($ven, $ver, $cc, $fc, $cc_debug, $fc_debug) =
		$hwrf->compiler(vendor => $self->{COMPILER}->{VENDOR});

	# Update/reset our configuration
	if (defined $args{'vendor'}) {
		$self->{COMPILER}->{VENDOR} = $args{'vendor'};
	} else {
		$self->{COMPILER}->{VENDOR} = $ven;
	}
	if (defined $args{'version'}) {
		$self->{COMPILER}->{VERSION} = $args{'version'};
	} else {
		$self->{COMPILER}->{VERSION} = $ver;
	}
	if (defined $args{'cc'}) {
		$self->{COMPILER}->{CC} = $args{'cc'};
	} else {
		$self->{COMPILER}->{CC} = $cc;
	}
	if (defined $args{'fc'}) {
		$self->{COMPILER}->{FC} = $args{'fc'};
	} else {
		$self->{COMPILER}->{FC} = $fc;
	}
	if (defined $args{'cc_debug'}) {
		$self->{COMPILER}->{CC_DEBUG} = $args{'cc_debug'};
	} else {
		$self->{COMPILER}->{CC_DEBUG} = $cc_debug;
	}
	if (defined $args{'fc_debug'}) {
		$self->{COMPILER}->{FC_DEBUG} = $args{'fc_debug'};
	} else {
		$self->{COMPILER}->{FC_DEBUG} = $fc_debug;
	}

	TRACE_OUT;
}

=head3 info

	$c->info();

The function B<info> will retrieve all the information regarding the
component. If the option B<file> is given it will append the output
to the named file, otherwise it will be written to STDOUT.

Currently the following information is written:

=over 4

=item SVN Revision.

The components SVN revision number.

=item SVN Commit time.

The components SVN commit time for the current revision.

=back

=cut

sub info {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	# Change into our destination
	my $cwd = cwd();
	chdir($self->{DST}) or croak "can not change into ".$self->{DST}.": $!";

	my $svn_rev   = "UNKNOWN";
	my $svn_rdate = "UNKNOWN";
	my ($svn_info, $svn_log, $svn_url);

	# File source or SVN Info
	$svn_info = `svn --non-interactive info . 2>&1`;
	if (grep(/not a working copy/, $svn_info)) {
		# Dump the package source name hoping a version number
		# is in it, since there is no VERSION file or such for
		# all these packages.
		$svn_rev = $self->{SRC}->{PKG};
	} else {
		($svn_rev) = ($svn_info =~ /Revision:\s+(\d+)$/m);
		($svn_url) = ($svn_info =~ /Root:\s+(.*)$/m);

		# SVN Revision date
		$svn_log = `svn --non-interactive -q log -r $svn_rev $svn_url`;
		($svn_rdate) = ($svn_log =~
			/(\d{4}-\d\d-\d\d\s\d\d:\d\d:\d\d)\s[+-]\d{4}/m);
	}

	if (defined $args{'file'}) {
		open(my $fh, ">>", $args{'file'})
			or croak "can not open ". $args{'file'} ." : $!";
		flock($fh, LOCK_EX)
			or croak "can not lock ". $args{'file'} ." : $!";
		seek($fh, 0, SEEK_END)
			or croak "can not seek to EOF: ".$args{'file'}.": $!";
		printf $fh "%25s: %s %s\n", $self->{NAME}, $svn_rev, $svn_rdate;
		flock($fh, LOCK_UN)
			or croak "can not unlock ". $args{'file'} ." : $!";
		close($fh);
	} else {
		print "" .$self->{NAME} ."ver: $svn_rev $svn_rdate\n";
	}

	chdir($cwd) or croak "can not change back into $cwd : $!";

	TRACE_OUT;
}

=head3

	$c->store(file => "component.dat");

The function B<store> will dump the modules data structures to the named file
using B<Storable>.

=cut

sub store {
	my $self   = shift;
	my (%args) = @_;

	my ($file, $serialized);

	TRACE_IN;

	if (defined $args{'file'}) {
		$file = $args{'file'};
	} else {
		croak "cowardly refusing to store without a filename";
	}

	open(my $fh, ">>:raw", $file) or croak "can not open $file : $!";
	flock($fh, LOCK_EX) or croak "can not lock $file : $!";

	$serialized = nfreeze($self);

	my ($fmt, $h_len, $d_len);
	$h_len = length($self->{NAME});
	$d_len = length($serialized);
	$fmt = "NNA".$h_len ."A".$d_len;
	my $buf = pack($fmt, $h_len, $d_len, $self->{NAME}, $serialized);
	print $fh $buf;

	flock($fh, LOCK_UN) or croak "can not unlock $file : $!";
	close($fh);

	TRACE_OUT;
}

=head3

	$c->retrieve(file => "component.dat");

The function B<retrieve> will retrieve the modules data structures from
the named file using B<Storable>.

=cut

sub retrieve {
	my $self   = shift;
	my (%args) = @_;

	my ($file, $buf, $name, $serialized, $h_len, $d_len);

	TRACE_IN;

	if (defined $args{'file'}) {
		$file = $args{'file'};
	} else {
		croak "cowardly refusing to retrieve without a filename";
	}

	open(my $fh, "<:raw", $file) or croak "can not open $file : $!";
	flock($fh, LOCK_EX) or croak "can not lock $file : $!";

	while (read($fh, $buf, 8)) { # expecting 2 x 32bit network ints
		($h_len, $d_len) = unpack('NN', $buf);
		if (read($fh, $buf, $h_len) != $h_len) {
			croak "unable to unpack the name from $file";
		}
		$name = unpack('A'.$h_len, $buf);
		if ($name eq $self->{NAME}) {
			if (read($fh, $buf, $d_len) != $d_len) {
				croak "unable to unpack the data from $file";
			}
			$serialized = unpack('A'.$d_len, $buf);
			last;
		} else {
			seek($fh, $d_len, SEEK_CUR);
		}
	}

	flock($fh, LOCK_UN) or croak "can not unlock $file : $!";
	close($fh);

	my $tmp = thaw($serialized);

	TRACE_OUT;

	return $tmp;
}

=head3 check_execs

	$c->check_execs();

The function B<check_execs> will check for the existance of all the components
executables.

=cut

sub check_execs {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	# Change into our destination
	my $cwd = cwd();
	chdir($self->{DST}) or croak "can not change into ".$self->{DST}.": $!";

	foreach my $exec (@{$self->{EXES}}) {
		if (! -x $exec and ! -B $exec) {
			croak "" .$self->{DST}.
				"/$exec does not exist or is not a binary";
		}
	}
	chdir($cwd) or croak "can not change back into $cwd : $!";

	TRACE_OUT;
}

=head3 install

	$c->install(dst => "/usr/local/bin", (options));

The function B<install> will install all the components executables in
the F<dst> directory.  If use_renamer was called prior to this
function, then the renamer will be used to decide where to install
programs, where to get them from, and which programs to install.

If no HWRF::Renamer was given, the options I<prefix> and I<suffix> are used:

Optional I<prefix> argument can be specified to to add a prefix to all
executables. B<Note> this is messy as if the executable already has this
prefix then it is B<NOT> added.

Optional I<suffix> argument can be specified to modify the default file
suffix of ".exe".

=cut

sub install {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	my ($dst, $dname, $prefix, $suffix);

	if (!defined $args{'dst'}) {
		croak "no installation directory defined";
	} else {
		$dst = abs_path($args{'dst'});
	}

        if (defined $args{'prefix'}) {
		$prefix = $args{'prefix'};
	}

	if (defined $args{'suffix'}) {
		if ($args{'suffix'} =~ /^\./) {
			$suffix = $args{'suffix'};
		} elsif ($args{'suffix'} eq "") {
			$suffix = "";
		} else {
			$suffix = "." . $args{'suffix'};
		}
	}

	# Make sure we have all the exec's
	$self->check_execs();

        my @cmd = ("cp","-pf");

        # Change into our source destination
        my $cwd = cwd();
        chdir($self->{DST}) or croak "can not change into ".$self->{DST}.": $!";

        if(defined($self->{RENAMER}) && $self->{USE_RENAMER}==1) {
            # A renamer was given, so use it to install the
            # executables.
            VERB_2 "Using HWRF::Renamer to rename executables...";

            my $tgt;
            my $simplename=basename($self->{DST});
            foreach my $exec (@{$self->{EXES}}) {
                $tgt=$self->{RENAMER}->exe_for($simplename,$exec);
                VERB_1("INSTALL $exec => $dst/$tgt");
                if(!defined($tgt) || $tgt eq '') {
                    croak "$exec: renamer failed to rename this one!!";
                }
                my @cp_cmd=(@cmd,$exec,"$dst/$tgt");
                VERB_5("@cp_cmd");
                system(@cp_cmd)==0
                    or croak "system @cp_cmd failed: $?";
            }
        } else {
            # No renamer was given, so use prefix and suffix instead:
            foreach my $exec (@{$self->{EXES}}) {
		my $start = rindex($exec, q{/});
		if ($start > 0) {
                    $dname = substr($exec, $start + 1);
		} else {
                    $dname = $exec;
		}
		if (defined $prefix and $dname !~ /^$prefix/) {
                    $dname = $dst . "/" . $prefix . $dname;
		} else {
                    $dname = $dst . "/" . $dname;
		}
		if (defined $suffix) {
                    my $end = rindex($dname, q{.});
                    if ($end > 0) {
                        $dname = substr($dname, 0, $end);
                    }
                    $dname .= $suffix;
		}
		push(@cmd, $exec, $dname);
                VERB_1("INSTALL $exec => $dname");
		VERB_4("@cmd");
		system(@cmd) == 0
                    or croak "system @cmd failed: $?";
		pop(@cmd);
		pop(@cmd);
            }
        }

        chdir($cwd) or croak "can not change back into $cwd : $!";

	TRACE_OUT;
}

=head3

	$util->cmp(sim1 => '4820/results/07L/2008082718',
		   sim2 => '4881/results/07L/2008082718');

The function B<cmp> will compare by calling the systems diff command,
the output of two simulation outputs.

It will return a list of file names that differ.

=cut

sub cmp {
	my $self   = shift;

	TRACE_IN;

	my ($s1, $s2);
	if ($#_ == 1) {
		$s1 = abs_path(shift);
		$s2 = abs_path(shift);
	} else {
		my (%args) = @_;
		$s1 = abs_path($args{'sim1'});
		$s2 = abs_path($args{'sim2'});
	}

	if (not defined $s1 or not defined $s2) {
		croak "unable to compare simulations, need two directories";
	}

	if (! -d $s1) {
		croak "sim1 $s1 is not a directory";
	}

	if (! -d $s2) {
		croak "sim1 $s2 is not a directory";
	}

	my @diffs;
	foreach my $file (@{$self->{OUTPUTS}}) {
		# make sure the files exist
		my @f1 = <$s1/$file>;
		my @f2 = <$s2/$file>;
		my $fs1 = scalar(@f1);
		my $fs2 = scalar(@f2);
		if ($fs1 != $fs2) {
			my %m = map {basename($_) => 1} @f1;
			my @not = grep(!defined $m{basename($_)}, @f2);
			{
				local $" = "\n";
				print "missing files:\n@not\n";
			}
			croak "unable to compare uneven number of files for "
				. $self->{NAME};
		}
		my $i = 0;
		for ($i = 0; $i < $fs1; ++$i) {
			my @cmd = ("cmp", "-s", $f1[$i], $f2[$i]);
			VERB_4(@cmd);
			system(@cmd);
			if ($? == -1) {
				croak "failed to execute @cmd: $!";
			} elsif ($? & 127) {
				croak "@cmd died with signal: ". ($? & 127);
			} elsif ($? > 0) {
				my $dfile=$f1[$i];
				$dfile =~ s/$s1\///g;
				push(@diffs, $dfile);
				if ($self->{VERBOSE} >= 3) {
					$self->diffwrf(file1 => $f1[$i],
						       file2 => $f2[$i]);
				}
			}
		}
	}

	TRACE_OUT;

	return \@diffs;
}

=head3

	$util->diffwrf(file1 => 'a/wrfout_d02_2008-09-02_00:00:00',
		       file2 => 'b/wrfout_d02_2008-09-02_00:00:00');

The function B<diffwrf> will call diffwrf on the two WRF output files.

It will not hide any of the gory details, they will be written to STDOUT.

=cut

sub diffwrf {
	my $self   = shift;

	TRACE_IN;

	my ($f1, $f2, $diff);
	if ($#_ == 1) {
		$f1 = abs_path(shift);
		$f2 = abs_path(shift);
	} else {
		my (%args) = @_;
		$f1 = abs_path($args{'file1'});
		$f2 = abs_path($args{'file2'});
	}

	# Get the file magic number
	my $hdr;
	open(FH, "< $f1") or croak "can not read $f1: $!";
	binmode(FH);
	read(FH, $hdr, 4);
	close(FH);

	if (-T $f1) {
		chomp($diff = `which diff`);
	} elsif (unpack('a3',$hdr)  =~ /CDF/) {
		# NetCDF magic number header is CDF\001
		$diff = $self->{TOPLEVEL}."/WRFV3/external/io_netcdf/diffwrf";
	} elsif (unpack('N',$hdr) == 2048) {
		# WRF Binary IO first record length is 2048
		$diff = $self->{TOPLEVEL}."/WRFV3/external/io_int/diffwrf";
	} else {
		croak "unable to identify file type of $f1";
	}

	# make sure the executable is there
	if (! -x $diff) {
		croak "unable to find diff program: $diff: $!";
	}
	my @cmd = ("$diff", "$f1", "$f2");
	system(@cmd);

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
