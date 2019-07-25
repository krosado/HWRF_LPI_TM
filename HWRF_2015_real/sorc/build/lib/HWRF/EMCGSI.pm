#
# Compiles GSI using the EMC build system.  Use the HWRF::GSI package
# instead if you are building from the DTC build system.
#

package HWRF::EMCGSI;

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
			$self->{DST} = abs_path($self->{TOPLEVEL} ."/EMCGSI");
		} else {
			$self->{DST} = abs_path("EMCGSI");
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
			"src/global_gsi",
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

sub simplename() {
    return "EMCGSI";
}

=head3 configure

The function B<configure> runs the correct configuration script for
this system:

NOAA RDHPCS Jet  - configure
NOAA RDHPCS Zeus - configure zeus
NCEP WCOSS       - configure wcoss

On other sites, we run configure $site where $site is the lower-case
site name.  Note that, as of the date this was written, only Jet, Zeus
and WCOSS had configure scripts.

=cut

sub configure {
    my $self = shift;
    my (%args) = @_;

    TRACE_IN;

    my $envs = $self->envs();

    my $cwd = cwd();
    chdir("$self->{DST}/src") or croak "can not change into $self->{DST}/src: $!";

    my $site;
    my @cmdargs;

    my $hwrf=HWRF->new();
    $hwrf->site();
    $site=lc($hwrf->{SITE});

    if($site eq 'jet') {
        push @cmdargs, 'jet';
    } elsif($site eq 'zeus') {
        push @cmdargs, 'zeus';
    } elsif($site eq 'theia') {
        push @cmdargs, 'theia';
    } elsif($site eq 'wcoss') {
        push @cmdargs, 'wcoss';
    } else {
        push @cmdargs, $site;
    }

    $self->run(cmd=>'./configure', args=>[@cmdargs], envs=>$envs, 
               stdout=>'configure.log');

    chdir($cwd) or croak "cannot change back to $cwd: $!";

    TRACE_OUT;
}

=head3 compile
  $c->compile();
Runs the EMC GSI build system by running "make"
=cut

sub compile {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	# Change into our destination
	my $cwd = cwd();
	chdir("$self->{DST}/src") or croak "can not change into $self->{DST}/src: $!";

	# If we have any environment variables
	my $envs = $self->envs();

	# merge the hashes
	if (defined $args{'envs'}) {
		while (my ($k, $v) = each(%{$args{'envs'}})) {
			$envs->{$k} = $v;
		}
	}

        my $NETCDF="$ENV{NETCDF}";
        my $INCnetcdf="$NETCDF/include";
        my $LIBnetcdf=" -L$NETCDF/lib -lnetcdf";
        my @moreargs;
        if(-e "$NETCDF/lib/libnetcdff.a" || -e "$NETCDF/lib/libnetcdff.so") {
            $LIBnetcdf .= " -lnetcdff"
        }
        my $hwrf=HWRF->new();
        $hwrf->site();
        my    $site=lc($hwrf->{SITE});
        if($site eq 'theia') {
            push @moreargs,"CF=mpif90 -f90=ifort";
            push @moreargs,"CC=icc";
        }

        warn "CMD = make -f Makefile WRFPATH=../../WRFV3 INCnetcdf='$INCnetcdf' LIBnetcdf='$LIBnetcdf'";

	$self->run(cmd    => "make",
		   args   => [
                       '-f','Makefile',
                       "WRFPATH=../../WRFV3",
                       "INCnetcdf=$INCnetcdf",
                       "LIBnetcdf=$LIBnetcdf",
                       "NETCDF_INCLUDE=$INCnetcdf",
                       "NETCDF_LDFLAGS_F=$LIBnetcdf",
                       @moreargs
                   ],
		   envs   => $envs,
		   stdout => "compile.log");

	chdir($cwd) or croak "can not change back into $cwd : $!";

	TRACE_OUT;
}



=head3 patch_config
patch_config: overridden from parent class to do nothing.
=cut
sub patch_config { }


=head3 untar
untar: overridden from parent class to do nothing.
=cut
sub untar { }

=head3 source
source: overridden from parent class to do nothing.
=cut
sub source { }


=head3 svn_co
svn_co: overridden from parent class to do nothing.
=cut
sub svn_co { }


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
