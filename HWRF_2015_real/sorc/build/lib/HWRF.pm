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
# $Id: HWRF.pm 153 2011-05-11 21:42:18Z tpbrown@ucar.edu $
#

package HWRF;

use 5.008002;
use strict;
use warnings;

use Data::Dumper;
use Carp;
use POSIX                   qw(strftime);
use File::Copy;
use File::Path              qw(mkpath);
use Cwd                     qw(abs_path cwd);
use Net::Domain             qw(hostfqdn);
use lib "..";
use Verbose                 qw(:all);

require Exporter;

our @ISA = qw(Exporter);
our $VERSION = '0.02';

=head1 NAME

HWRF - Perl extensions for HWRF

=head1 SYNOPSIS

  use HWRF;
  my $hwrf = HWRF->new();
  $hwrf->sandbox({dir => "foo"});
  $hwrf->site();
  $hwrf->compiler(vendor => "GNU", cc => "gcc", fc => "gfortran");
  $hwrf->smtp(smtp => "localhost");

=head1 DESCRIPTION

The HWRF module provides Perl functions that are commonly used in
scripts interacting with HWRF.

=head2 EXPORT

None by default.

=cut

our %EXPORT_TAGS = ( 'all' => [ qw(
	make_dir
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );


=head2 METHODS

=head3 new

	my $hwrf = HWRF->new();

Instantiates an HWRF object.

=cut

sub new {
	my $proto  = shift;
	my $class  = ref($proto) || $proto;
	my (%args) = @_;

	my $self = {};

	$self->{SANDBOX}              = undef;
	$self->{ARCH}                 = $^O;
	$self->{SITE}                 = undef;
	$self->{SMTP}                 = "localhost";
	$self->{ENV}                  = {};
	$self->{COMPILER}             = {
		VENDOR   => undef,
		VERSION  => undef,
		CC       => undef,
		FC       => undef,
		CC_DEBUG => undef,
		FC_DEBUG => undef,
	};
	$self->{CONFIG}               = {
		PAR      => undef,
		OPT      => undef,
		FILE     => undef,
	};
	$self->{MPI}                  = {
		VENDOR   => undef,
		VERSION  => undef,
		CMD      => undef,
	};
	$self->{NETCDF}               = {
		VENDOR   => undef,
		VERSION  => undef,
		'64BIT'  => undef,
	};
	$self->{PNETCDF}               = {
		VENDOR   => undef,
		VERSION  => undef,
		'64BIT'  => undef,
	};

	bless($self, $class);
	return $self;
}

# Internal initialization function. This is not exported and should not
# be called by the calling application.
#sub _init {
#	my $proto  = shift;
#	my $class  = ref($proto) || $proto;
#	my $self   = shift;
#	my (%args) = @_;
#
#	# process the args (if any)
#	while (my ($k, $v) = each %args) {
#		$self->$k($v);
#	}
#}

=head3 site

	my $site = $hwrf->site();

The function B<site> will try and figure out the site you are currently
running on. This will set the default site name, smtp server and compilers.

=cut

sub site {
	my $self   = shift;
	my (%args) = @_;

	my $os = $^O;
	my $host = hostfqdn();
	$self->{ARCH} = $os;

	# list of host names with their sites
	if ($os =~ /linux/) {
		if ($host =~ /boulder/) {
			$self->_site_jet(%args);
		} elsif ($host =~ /zeus/) {
			$self->_site_zeus(%args);
		} elsif ($host =~ /ncep/) {
			$self->_site_wcoss(%args);
		} elsif ($host =~ /yellowstone/) {
			$self->_site_yellowstone(%args);
		} elsif ($host =~ /narl.org.tw/) {
                        $self->_site_ttfri_saola(%args);
		} elsif ($host =~ /theia.fairmont.rdhpcs.noaa.gov/) {
                    $self->_site_theia(%args);
                }
	} elsif ($os =~ /aix/) {
		if ($host =~ /ucar/) {
			$self->_site_bluefire(%args);
		} elsif ($host =~ /vapor/) {
			$self->_site_vapor(%args);
		} elsif ($host=~/ncep/) {
                    $self->_site_ccs(%args);
                }
	}

	if (! defined $self->{SITE}) {
		croak "unknown site for hostname: $host";
	}

	return $self->{SITE};
}

# Internal routine to set Jet specific information.
sub _site_jet {
	my $self   = shift;
	my (%args) = @_;

	$self->{SITE} = "Jet";
	$self->{SMTP} = "jetmail.rdhpcs.noaa.gov";

	if (defined $args{'compiler'}) {
		$self->compiler(vendor => $args{'compiler'});
	} else {
		$self->_compiler_intel();
	}
	$ENV{JASPERLIB}='/usr/lib64' unless(defined($ENV{JASPERLIB}) && -d "$ENV{JASPERLIB}");
	$ENV{JASPERINC}='/usr/include' unless(defined($ENV{JASPERINC}) && -d "$ENV{JASPERINC}");
}

# Internal routine to set Theia specific information.
sub _site_theia {
	my $self   = shift;
	my (%args) = @_;

        $self->_mpi_intel();
	$self->{SITE} = "Theia";
	$self->{SMTP} = "tfe01";

	if (defined $args{'compiler'}) {
		$self->compiler(vendor => $args{'compiler'});
	} else {
		$self->_compiler_intel();
	}
	$ENV{JASPERLIB}='/usr/lib64' unless(defined($ENV{JASPERLIB}) && -d "$ENV{JASPERLIB}");
	$ENV{JASPERINC}='/usr/include' unless(defined($ENV{JASPERINC}) && -d "$ENV{JASPERINC}");
}

# Internal routine to set TTFRI Saola information.
sub _site_ttfri_saola {
	my $self   = shift;
	my (%args) = @_;

	$self->{SITE} = "TTFRISaola";
	$self->{SMTP} = "localhost";

	if (defined $args{'compiler'}) {
		$self->compiler(vendor => $args{'compiler'});
	} else {
		$self->_compiler_intel();
	}
	$ENV{JASPERLIB}='/usr/lib' unless(defined($ENV{JASPERLIB}) && -d "$ENV{JASPERLIB}");
	$ENV{JASPERINC}='/usr/include' unless(defined($ENV{JASPERINC}) && -d "$ENV{JASPERINC}");
	$ENV{JASPER}='/usr' unless(defined($ENV{JASPER}) && -d "$ENV{JASPER}");
}


# Internal routine to set WCOSS specific information.
sub _site_wcoss {
	my $self   = shift;
	my (%args) = @_;

	$self->{SITE} = "WCOSS";
	$self->{SMTP} = "localhost";

	$self->_compiler_intel();
        $self->_mpi_lsf_poe();
	$self->config(opt => 'IBM POE');
	$ENV{JASPERLIB}='/usr/lib64' unless(defined($ENV{JASPERLIB}) && -d "$ENV{JASPERLIB}");
	$ENV{JASPERINC}='/usr/include' unless(defined($ENV{JASPERINC}) && -d "$ENV{JASPERINC}");
}

# Internal routine to set Bluefire specific information.
sub _site_bluefire {
	my $self   = shift;
	my (%args) = @_;

	$self->{SITE} = "Bluefire";
	$self->{SMTP} = "localhost";

	$self->_compiler_aix();
	$self->_mpi_loadleveler_poe();

	$self->{MPI}->{CMD} = "mpirun.lsf";
}

# Internal routine to set Vapor specific information.
sub _site_vapor {
	my $self   = shift;
	my (%args) = @_;

	$self->{SITE} = "Vapor";
	$self->{SMTP} = "localhost";

	$self->_compiler_aix();
	$self->_mpi_loadleveler_poe();

}

# Internal routine to set CCS specific information.
sub _site_ccs {
	my $self   = shift;
	my (%args) = @_;

	$self->{SITE} = "CCS";
	$self->{SMTP} = "localhost";

	$self->_compiler_aix();
	$self->_mpi_loadleveler_poe();

}

# Internal routine to set Zeus specific information.
sub _site_zeus {
	my $self   = shift;
	my (%args) = @_;

	$self->{SITE} = "Zeus";

	$self->_compiler_intel();
	$self->_mpi_sgi();
	$self->config(opt => 'SGI MPT');
        $ENV{JASPERLIB}='/usr/lib64' unless(defined($ENV{JASPERLIB}) && -d "$ENV{JASPERLIB}");
        $ENV{JASPERINC}='/usr/include' unless(defined($ENV{JASPERINC}) && -d "$ENV{JASPERINC}");
}

# Internal routine to set Yellowstone specific information.
sub _site_yellowstone {
	my $self   = shift;
	my (%args) = @_;

	$self->{SITE} = "Yellowstone";
	$self->{SMTP} = "localhost";

	if (defined $args{'compiler'}) {
		$self->compiler(vendor => $args{'compiler'});
	} else {
		$self->_compiler_intel();
		$self->config(opt => 'IBM POE');
	}
	$self->_mpi_lsf_poe();

}

=head3 env

	$c->envs();

Set the environment variables needed for the HWRF build.

=cut

sub envs {
	my $self   = shift;
	my (%args) = @_;

	TRACE_IN;

	if (! defined $ENV{'TMPDIR'}) {
		$ENV{'TMPDIR'}   = "/tmp";
	}
        if(!defined($ENV{NETCDF4}) && defined($ENV{NETCDF})) {
            $ENV{NETCDF4}=$ENV{NETCDF};
        }
        my $var;
        foreach $var ( qw{PNETCDF NETCDF4} ) {
            if (! defined $ENV{$var}) {
		croak "$var env var is not set. Please set it.  Aborting\n";
            }
	}
        if(!defined($ENV{NETCDF})) {
            $ENV{NETCDF}=$ENV{NETCDF4};
        }
        if($ENV{NETCDF} ne $ENV{NETCDF4}) {
            croak "NETCDF and NETCDF4 variables point to different locations.  Fix it.  Aborting\n";
        }

        delete $ENV{NETCDF4}; # to avoid confusing WRF build scripts
        # (latest version of wrf configure script crashes)

	TRACE_OUT;

	return {
		TMPDIR => "/tmp",
		NETCDF => 0,
		PNETCDF => 0,
	};
}

=head3 check_env

       $c->check_env();

Checks the environment to ensure the user did not set anything that
conflicts with HWRF.

=cut

sub check_env {
    my $bad='';
    if(defined($ENV{IDEAL_NMM_TC})) {
        print STDERR "Do not set \$IDEAL_NMM_TC when compiling the real-case HWRF.  It produces horrible forecasts in real-case mode.\n";
        $bad.=' $IDEAL_NMM_TC';
    }
    my @naughtyvars=qw{WRF_DFI_RADAR WRF_CHEM WRF_HYDRO WRF_MARS
                       WRF_TITAN WRF_VENUS DA_CORE 4D_DA_CORE};
    foreach my $var (@naughtyvars) {
        if(defined($ENV{$var})) {
            print STDERR 'Do not set $'.$var.".  It is not supported by the HWRF configuration of WRF.\n";
            $bad.=',' if length($bad)>0;
            $bad.=' $'.$var;
        }
    }
    if(length($bad)>0) {
        croak "Exitting because of bad variables in environment:$bad\n";
    }
}

=head3 check_lib_versions

	$c->check_lib_versions();

Checks library versions to make sure they are sufficient.

=cut

sub check_lib_versions {
    my $ncv_prog="$ENV{NETCDF}/bin/nc-config";
    if(!-x $ncv_prog){
        croak "$ncv_prog program is missing or not executable.  You must use at least version 4.1 of NetCDF with C, Fortran 77 and Fortran 90 bindings enabled.  Aborting\n";
    }
    my $pncv_prog="$ENV{PNETCDF}/bin/pnetcdf_version";
    if(!-x $pncv_prog){
        croak "$pncv_prog is missing or not executable.  You must use at least version 1.5.0 of PNetCDF with Fortran bindings enabled.  Aborting\n";
    }
    my $line;
    my $pncv='-1';
    my $ncv='-1';
    foreach $line (split(/\n+/,`$pncv_prog 2>&1`)) {
        chomp $line;
        if($line =~ /pnetcdf\s+version\s*:\s*([0-9.]+)/i) {
            $pncv=$1;
        } elsif($line=~/Hostfile or pool must be used/i) {
            print "$line: cannot run pnetcdf_version on non-compute nodes with IBMPE \& LSF.  Will parse pnetcdf.h instead.\n";
            #last;
        }
    }
    if($pncv eq '-1') {
        print "Could not figure out PNetCDF version from pnetcdf_version.  Will try parsing pnetcdf.h.\n";
        my $pnch="$ENV{PNETCDF}/include/pnetcdf.h";
        open(PH,"<$pnch") or croak "$pnch: cannot open for reading: $!  Aborting\n";
        my @ver=(-1,-1,-1);
        while(defined($_=<PH>)) {
            # We're looking for this:
            #define PNETCDF_VERSION_MAJOR 1
            #define PNETCDF_VERSION_MINOR 5
            #define PNETCDF_VERSION_SUB 0
            
            /PNETCDF_VERSION_MAJOR\s+(\d+)/ and $ver[0]=$1;
            /PNETCDF_VERSION_MINOR\s+(\d+)/ and $ver[1]=$1;
            /PNETCDF_VERSION_SUB\s+(\d+)/ and $ver[2]=$1;
        }
        croak "$pnch: cannot find PNETCDF_VERSION_MAJOR\n" if($ver[0]==-1);
        croak "$pnch: cannot find PNETCDF_VERSION_MINOR\n" if($ver[1]==-1);
        croak "$pnch: cannot find PNETCDF_VERSION_SUB\n" if($ver[2]==-1);
        $pncv="$ver[0].$ver[1].$ver[2]";
        print "$pnch: version $pncv\n";
        close(PH);
    }
    foreach $line (split(/\n+/,`$ncv_prog --version`)) {
        chomp $line;
        if($line =~ /netcdf(?:\s+fortran)?\s*([0-9.]+)/i) {
            $ncv=$1;
        }
    }
    if($ncv eq '-1') {
        croak "Could not figure out NetCDF version.  Aborting\n";
    }
    _impl_check_version($pncv,'1.5.0','PNetCDF library');
    _impl_check_version($ncv,"4.1","NetCDF Fortran library");
}

=head3 env

    _impl_check_version(version,required_version,what_is_it);

This is an internal implementation function that you should not call
directly.

Given a pair of dotted decimal library versions, this checks to see if
the first is greater than or equal to the other.  If it is not, it
croaks with an intelligible message about $what_is_it library version
being too old.

=cut

sub _impl_check_version($$$) {
    my ($version,$reqversion,$what)=@_;
    my @ver=split(/\./,$version);
    my @rver=split(/\./,$reqversion);
    while(scalar(@ver)<scalar(@rver)) {
        push @ver,0;
    }
    while(scalar(@rver)<scalar(@ver)) {
        push @rver,0;
    }
    my $i;
    for($i=0;$i<=$#ver;$i++) {
        if($ver[$i]<$rver[$i]) {
            croak "$what version is wrong: $version < $reqversion.  Aborting\n";
        }
    }
    print "Rejoice! $what version $version >= $reqversion\n";
}

=head3 compiler

	$hwrf->compiler(vendor => "Intel");

	my ($vendor, $version, $cc, $fc, $cc_debug, $fc_debug) = $hwrf->compiler();

The function B<compiler> cn be used to set or get the current system
compiler.

=cut

sub compiler {
	my $self   = shift;
	my (%args) = @_;

	# update our entry if we were given arguments
	if (defined $args{'vendor'}) {
		my $func = "_compiler_" . lc($args{'vendor'});
		# Load the vendor defaults
		if ($self->can($func)) {
			$self->$func();
		}
	}
	if (defined $args{'cc'}) {
		$self->{COMPILER}->{CC} = $args{'cc'};
	}
	if (defined $args{'fc'}) {
		$self->{COMPILER}->{FC} = $args{'fc'};
	}
	if (defined $args{'cc_debug'}) {
		$self->{COMPILER}->{CC_DEBUG} = $args{'cc_debug'};
	}
	if (defined $args{'fc_debug'}) {
		$self->{COMPILER}->{FC_DEBUG} = $args{'fc_debug'};
	}

	# If we don't have a vendor by now, load the site default
	if (!defined $self->{COMPILER}->{VENDOR}) {
		my $func = "";
		my $os = $^O;
		if ($os =~ /linux/) {
			$func = "_compiler_intel";
		} elsif ($os =~ /aix/) {
			$func = "_compiler_aix";
		} elsif ($os =~ /darwin/) {
			$func = "_compiler_pgi";
		}
		# Load the vendor defaults
		if ($self->can($func)) {
			$self->$func();
		}
	}

	# return an array of our set-up
	return ($self->{COMPILER}->{VENDOR},
		$self->{COMPILER}->{VERSION},
		$self->{COMPILER}->{CC},
		$self->{COMPILER}->{FC},
		$self->{COMPILER}->{CC_DEBUG},
		$self->{COMPILER}->{FC_DEBUG}
	);
}

sub _compiler_intel {
	my $self   = shift;
	my (%args) = @_;

	$self->{COMPILER}->{VENDOR}   = "Intel";
	$self->{COMPILER}->{CC}       = "icc";
	$self->{COMPILER}->{FC}       = "ifort";
	# $self->{COMPILER}->{CC_DEBUG} = "-g -O0 -debug all -check all " .
	$self->{COMPILER}->{CC_DEBUG} = "-g -O0 -debug all " .
					"-fpe0 -ftrapuv -traceback -heap-arrays ";
	$self->{COMPILER}->{FC_DEBUG} = "-g -O0 -debug all " .
					"-fp-model precise -ftz -fp-stack-check";
	# How about?
	# -O0 -g -traceback -fpe:0 -check all -debug all -fpstkchk \
	# -heap-arrays -ftrapuv
	
	# try and get the version number
	open(PIPE, "ifort --version 2>&1 |") or croak "can not execute ifort";
	my $ver = do { local $/; <PIPE> };
	close(PIPE);
	($self->{COMPILER}->{VERSION}) = ($ver =~ /\b(\d+\.\d+)\b/m);
}

sub _compiler_aix {
	my $self   = shift;
	my (%args) = @_;

	$self->{COMPILER}->{VENDOR}   = "AIX";
	$self->{COMPILER}->{CC}       = "xlc";
	$self->{COMPILER}->{FC}       = "xlf";
	$self->{COMPILER}->{CC_DEBUG} = "-g -O0 -C -qfullpath -qcheck ".
		"-qflttrap=inv:ov:zero:en";
	$self->{COMPILER}->{FC_DEBUG} = $self->{COMPILER}->{CC_DEBUG};

	# try and get the version number
	open(PIPE, "xlf -qversion 2>&1 |") or croak "can not execute xlf";
	my $ver = do { local $/; <PIPE> };
	close(PIPE);
	($self->{COMPILER}->{VERSION}) =
		($ver =~ /Version:\s+((\d+|\.)+)$/m)[0];
}

sub _compiler_gnu {
	my $self   = shift;
	my (%args) = @_;

	$self->{COMPILER}->{VENDOR}   = "GNU";
	$self->{COMPILER}->{CC}       = "gcc";
	$self->{COMPILER}->{FC}       = "gfortran";
	$self->{COMPILER}->{CC_DEBUG} = "-g -O0 -fbounds-check " .
		"-Wuninitialized -O -ftrapv -fimplicit-none -fno-automatic";
	$self->{COMPILER}->{FC_DEBUG} = $self->{COMPILER}->{CC_DEBUG};

	# try and get the version number
	open(PIPE, "gfortran --version 2>&1 |")
		or croak "can not execute gfortran";
	my $ver = do { local $/; <PIPE> };
	close(PIPE);
	($self->{COMPILER}->{VERSION}) = ($ver =~ /\b(\d+\.\d+\.\d+)\b/m);
}

sub _compiler_pgi {
	my $self   = shift;
	my (%args) = @_;

	$self->{COMPILER}->{VENDOR}   = "PGI";
	$self->{COMPILER}->{CC}       = "pgcc";
	$self->{COMPILER}->{FC}       = "pgfortran";
	$self->{COMPILER}->{CC_DEBUG} = "-g -O0";
	$self->{COMPILER}->{FC_DEBUG} = $self->{COMPILER}->{CC_DEBUG};

	# try and get the version number
	open(PIPE, "pgfortran --version 2>&1 |")
		or croak "can not execute pgfortran";
	my $ver = do { local $/; <PIPE> };
	close(PIPE);
	($self->{COMPILER}->{VERSION}) = ($ver =~ /\b(\d+\.\d+-\d+)\b/m);
}

sub _compiler_lahey {
	my $self   = shift;
	my (%args) = @_;

	$self->{COMPILER}->{VENDOR}   = "Lahey";
	$self->{COMPILER}->{CC}       = "cc";
	$self->{COMPILER}->{FC}       = "lfc";
	$self->{COMPILER}->{CC_DEBUG} = "-g -O0";
	$self->{COMPILER}->{FC_DEBUG} = $self->{COMPILER}->{CC_DEBUG};

	# try and get the version number
	open(PIPE, "lf95 --version 2>&1 |") or croak "can not execute lf95";
	my $ver = do { local $/; <PIPE> };
	close(PIPE);
	($self->{COMPILER}->{VERSION}) = ($ver =~ /\b(L\d+\.\d+[a-z])\b/m);
}

=head3 smtp

	$hwrf->smtp(smtp => "localhost");

	my $smtp_server = $hwrf->smtp();

The function B<smtp> can be used to set or get the current system
SMTP mail server.

=cut

sub smtp {
	my $self   = shift;
	my (%args) = @_;

	# update our entry if we were given arguments
	if (defined $args{'smtp'}) {
		$self->{SMTP} = $args{'smtp'};
	}

	return $self->{SMTP};
}

=head3 config

	$hwrf->config();

The function B<config> can be used to set and get the local
site configuration variables.

=cut

sub config {
	my $self   = shift;
	my (%args) = @_;

	# update our entry if we were given arguments
	if (defined $args{'par'}) {
		$self->{CONFIG}->{PAR} = $args{'par'};
	}
	if (defined $args{'opt'}) {
		$self->{CONFIG}->{OPT} = $args{'opt'};
	}
	if (defined $args{'file'}) {
		$self->{CONFIG}->{FILE} = $args{'file'};
	}

	return $self->{CONFIG};
}

=head3 make_dir

	$hwrf->make_dir(dir => "foo", mode => mask);
or
	HWRF::make_dir(dir => "foo", mode => mask);

The function B<make_dir> will create the directory F<foo> given in the
argument hash as I<dir>. If a mode argument is given it will set the
directories permissions to I<mask>.

If a directory already exists, it will be renamed with the last modified
timestamp appended to it.

B<make_dir> can be called in a non-OO method.

=cut

sub make_dir {
	my ($self, %args);
	if (ref($_[0]) eq __PACKAGE__) {
		$self   = shift;
	}
	(%args) = @_;

	my $dir  = $args{'dir'}  || undef;
	my $mask = $args{'mode'} || undef;

	if (! defined $dir) {
		croak("no directory name given");
	}

	# if the directory exits (or file, etc), rename it.
	if (-e $dir) {
		my $mtime = strftime("%Y%m%d%H%M%S",
			localtime(((stat($dir))[9])));
		move($dir, $dir.".".$mtime) or
			croak("unable to rename $dir: $!");
	}

	# if we want to override the default umask
	if (defined $mask) {
		mkpath($dir, 0, $mask);
	} else {
		mkpath($dir);
	}

	return abs_path($dir);
}

=head3 sandbox

	$hwrf->sandbox(dir => "foo", mode => mask);

The function B<sandbox> will create an empty sandbox directory structure
to compile and run HWRF in. It will create a toplevel directory named with
the hash argument I<dir> and two subdirectories called F<src> and F<results>.

If a I<mode> argument is given it will set the directories permissions
to I<mask>.

If no arguments are given it will return the absolute path of the current
sandbox (or undef).

This command calls I<make_dir>.

=cut

sub sandbox {
	my $self   = shift;
	my (%args) = @_;

	my $dir  = $args{'dir'}  || undef;
	my $mask = $args{'mode'} || undef;

	if (! defined $dir) {
		return $self->{SANDBOX};
	}

	my $cwd = cwd();
	if ($dir ne $cwd) {
		$self->make_dir(dir => $dir, mask => $mask);
	}
	$self->make_dir(dir => $dir . "/src",     mask => $mask);
	$self->make_dir(dir => $dir . "/results", mask => $mask);

	$self->{SANDBOX} = abs_path($dir);
}

=head3 build_info

	$hwrf->build_info(file => 'build.info');

The function B<build_info> will write information about the HWRF
build parameters. It will write to STDOUT unless a file argument is given.

Be warned it uses B<caller(0)[1]> to obtain the script filename, this
is not very robust and can return the wrong filename.

=cut

sub build_info {
	my $self   = shift;
	my (%args) = @_;

	my $output;
	if (defined $args{'file'}) {
		open($output, ">", $args{'file'});
	} else {
		$output = \*STDOUT;
	}

	my $cmd = (caller(0))[1];

	# Make sure everything is defined
	if (!defined $self->{COMPILER}->{VENDOR}) {
		$self->compiler();
	}
	if (!defined $self->{MPI}->{VENDOR}) {
		$self->mpi();
	}
	if (!defined $self->{NETCDF}->{VENDOR}) {
		$self->netcdf();
	}
	if (!defined $self->{PNETCDF}->{VENDOR}) {
		$self->netcdf(type=>'pnetcdf');
	}

	my $now = gmtime;
	print $output "#" x 80 ."\n";
	printf $output "%25s: %s %s\n\n", "command line", $cmd, $args{'args'};
	printf $output "%25s: %s\n", "by", getpwuid($<);
	printf $output "%25s: %s (UTC)\n", "at", $now;
	printf $output "%25s: %s", "hostname", `hostname`;
	printf $output "%25s: %s", "uname", `uname -a`;
	printf $output "%25s: %s %s\n", "compiler",
					$self->{COMPILER}->{VENDOR},
					$self->{COMPILER}->{VERSION};
	printf $output "%25s: %s %s\n", "mpi",
					$self->{MPI}->{VENDOR},
					$self->{MPI}->{VERSION};
	printf $output "%25s: %s %s\n", "netcdf",
					$self->{NETCDF}->{VENDOR},
					$self->{NETCDF}->{VERSION};
	printf $output "%25s: %s %s\n", "pnetcdf",
					$self->{PNETCDF}->{VENDOR},
					$self->{PNETCDF}->{VERSION};

}

=head3

	($vendor, $version) = $hwrf->mpi(file => 'mpi_info');

The function B<mpi> will obtain information about the current
MPI implementation. It will return the information as a array and
write it to a file if a file argument is given.

=cut

sub mpi {
	my $self   = shift;
	my (%args) = @_;

	# update our entry if we were given arguments
	if (defined $args{'vendor'}) {
		$self->{MPI}->{VENDOR} = $args{'vendor'};
		my $func = "_mpi_" . lc($self->{MPI}->{VENDOR});
		# Load the vendor defaults
		if ($self->can($func)) {
			$self->$func();
		}
	}
	if (defined $args{'version'}) {
		$self->{MPI}->{VERSION} = $args{'version'};
	}

	# If we don't have a vendor by now, try and figure it out
	if (!defined $self->{MPI}->{VENDOR}) {
		my ($info, $ven);
		if ($self->{ARCH} =~ /linux/i) {
			open(PIPE, "mpif90 -show 2>/dev/null |");
			$info = do { local $/; <PIPE> };
			close(PIPE);
			if ($info) {
				($ven) = ($info =~ /(mvapich2|openmpi)/);
				my $func = "_mpi_" . $ven;
				if ($self->can($func)) {
					$self->$func();
				}
			}
		} elsif ($self->{ARCH} =~ /aix/i) {
			$self->_mpi_poe();
		}
	}

	# Croak if we still don't know our vendor
	if (!defined $self->{MPI}->{VENDOR}) {
	    carp "unable to figure out the MPI implementation";
	    $self->{MPI}->{VENDOR}='unknown';
	    $self->{MPI}->{VERSION}='unknown';
	}

	if (defined $args{'file'}) {
		open(FILE, ">", $args{'file'});
		print FILE $self->{MPI}->{VENDOR}  ."\n".
			   $self->{MPI}->{VERSION} ."\n";
		close(FILE);
	}

	# return an array of our set-up
	return ($self->{MPI}->{VENDOR},
		$self->{MPI}->{VERSION},
	);

}

sub _mpi_openmpi {
	my $self   = shift;

	my $info = `ompi_info`;
	my ($ven, $ver) = ($info =~ /(Open\sMPI):\s+((\d+|\.)+)/);
	$self->{MPI}->{VENDOR}  = $ven;
	$self->{MPI}->{VERSION} = $ver;
	$self->{MPI}->{CMD}     = "mpirun";
}

sub _mpi_intel {
	my $self   = shift;

        my $ver='unknown';
        open( my $fd, "mpirun --version |");
        while(defined($_=<$fd>)) {
            if(/(Version.*)/) {
                $ver=$1;
                last;
            }
        }
	$self->{MPI}->{VENDOR}  = 'Intel';
	$self->{MPI}->{CMD}     = "mpirun";
	$self->{MPI}->{VERSION} = $ver;
}

sub _mpi_mvapich2 {
	my $self   = shift;

	chomp($self->{MPI}->{VENDOR}  = `mpiname -n`);
	chomp($self->{MPI}->{VERSION} = `mpiname -v`);
	$self->{MPI}->{CMD} = "mpirun";
}

sub _mpi_lsf_poe {
	my $self  = shift;

	my $info = `poe -v | head -1`;
	$self->{MPI}->{VENDOR} = "IBM";
	($self->{MPI}->{VERSION}) = ($info =~ /.*?-((\d+|\.)+)/);
}

sub _mpi_loadleveler_poe {
    my $self  = shift;
    
    my $info = `lslpp -l`;
    $self->{MPI}->{VENDOR} = "IBM";
    ($self->{MPI}->{VERSION}) = ($info =~ /poe\s+((\d+|\.)+)/);
}

sub _mpi_sgi {
	my $self  = shift;

	my $info = `rpm -qi sgi-mpt`;

	$self->{MPI}->{VENDOR}    = "SGI MPT";
	($self->{MPI}->{VERSION}) = ($info =~ /Version\s*:\s*((\d+|\.)+)/);
	$self->{MPI}->{CMD}       = "mpiexec_mpt";
}

=head3

	$hwrf->mpirun(cmds => (a, b, c), cores => (1, 1, 2));

The function B<mpirun> will run commands specified. It requires
two arguments, I<cmds> and I<cores>. These must be arrays of equal
length. As each command will be launched with the corresponding number
of processors. This is to facilitate multiple program, multiple data (MPMD).

=cut

sub mpirun {
	my $self   = shift;
	my (%args) = @_;

	if ($self->{MPI}->{CMD} eq "mpirun") {
		$self->_mpirun(@_);
	} elsif ($self->{MPI}->{CMD} eq "mpirun.lsf") {
		$self->_mpirun_lsf(@_);
	} else {
		croak "unsupported MPI environment: ".$self->{MPI}->{CMD};
	}

}

=head3

	$hwrf->_mpirun(cmds => (a, b, c), cores => (1, 1, 2));

The function B<_mpirun> is an internal command to execute a MPI
job on systems that use B<mpirun> with arguments I<-np> for the 
number of processes per command.

It requires two arguments, I<cmds> and I<cores>. These must be arrays of
equal length. As each command will be launched with the corresponding
number of processors. This is to facilitate multiple program, multiple
data (MPMD).

=cut

sub _mpirun {
	my $self   = shift;
	my (%args) = @_;

	if (!defined $args{'cmds'}) {
		croak "no commands were given to execute";
	}
	if (!defined $args{'cores'}) {
		croak "no number cores were given";
	}
	if (scalar($args{'cmds'}) != scalar($args{'cores'})) {
		croak "number of commands and cores do not match";
	}

	my @cmd = ($self->{MPI}->{CMD});
	my $max = scalar($args{'cmds'});
	for (my $i = 0; $i < $max; ++$i) {
		push(@cmd, "-np", $args{'cores'}[$i], $args{'cmds'}[$i], ":");
	}
	pop(@cmd);
}

=head3

	$hwrf->_mpirun_lsf(cmds => (a, b, c), cores => (1, 1, 2));

The function B<_mpirun_lsf> is an internal command to execute a MPI
job on systems that use B<mpirun.lsf> with a command file specifying
number of processes and command (one per line).

It will write the commands to a file called F<cmd.$$>, in the current
working directory.

It requires two arguments, I<cmds> and I<cores>. These must be arrays of
equal length. As each command will be launched with the corresponding
number of processors. This is to facilitate multiple program, multiple
data (MPMD).

=cut

sub _mpirun_lsf {
	my $self   = shift;
	my (%args) = @_;

	if (!defined $args{'cmds'}) {
		croak "no commands were given to execute";
	}
	if (!defined $args{'cores'}) {
		croak "no number cores were given";
	}
	if (scalar($args{'cmds'}) != scalar($args{'cores'})) {
		croak "number of commands and cores do not match";
	}

	my @cmd = ($self->{MPI}->{CMD});
	my $max = scalar($args{'cmds'});
	if ($max == 1) {
		push(@cmd, "-procs", $args{'cores'}[0], $args{'cmd'}[0]);
	} else {
		my $file = "cmd.$$";
		open(FILE, "> $file")
			or croak "unable to create mpirun command: $file: $!";
		#for (my $i = 0; $i < $max; ++$i) {
		#}
		close(FILE);
	}
}

=head3

	($vendor, $version) = $hwrf->netcdf(file => 'netcdf_info');

The function B<netcdf> will obtain information about the current
NetCDF library. It will return the information as a array and
write it to a file if a file argument is given.

=cut

sub netcdf {
	my $self   = shift;
	my (%args) = @_;
        my $type="netcdf";
        my $TYPE="NETCDF";
        my $vendor="Unidata";

        if(defined($args{type}) && $args{type} eq 'pnetcdf') {
            $type="pnetcdf";
            $TYPE="PNETCDF";
            $vendor="Argonne National Laboratory"
        }

	# update our entry if we were given arguments
	if (defined $args{'vendor'}) {
		$self->{$TYPE}->{VENDOR} = $args{'vendor'};
	}
	if (defined $args{'version'}) {
		$self->{$TYPE}->{VERSION} = $args{'version'};
	}
	if (defined $args{'64bit'}) {
		$self->{$TYPE}->{'64BIT'} = $args{'64bit'};
	}

	# If we don't have a vendor by now, try and figure it out
	if (!defined $self->{$TYPE}->{VENDOR}) {
		$self->{$TYPE}->{VENDOR} = $vendor;
		my $info = `strings $ENV{$TYPE}/lib/lib$type.a`;
		my ($ver) = ($info =~ /\bversion\b\s+(?:=\s+)?"?([0-9.]+)"?/i);
		$self->{$TYPE}->{VERSION} = $ver || "UNKNOWN";
		open(FILE,"$ENV{$TYPE}/include/$type.inc") or
		croak "unable to open $type Fortran include file: $!";
		my $is64 = grep /nf_format_64bit/i, <FILE>;
		close(FILE);
		$self->{$TYPE}->{VERSION} = $ver || "UNKNOWN";
		$self->{$TYPE}->{'64BIT'} = 1 if ($is64);
	}

	# Croak if we still don't know our vendor
	if (!defined $self->{$TYPE}->{VENDOR}) {
		croak "unable to figure out the NetCDF library";
	}

	if (defined $args{'file'}) {
		open(FILE, ">", $args{'file'});
		print FILE $self->{$TYPE}->{VENDOR}  ."\n".
			   $self->{$TYPE}->{VERSION} ."\n".
			   $self->{$TYPE}->{'64BIT'} ."\n";
		close(FILE);
	}

	# return an array of our set-up
	return ($self->{$TYPE}->{VENDOR},
		$self->{$TYPE}->{VERSION},
		$self->{$TYPE}->{'64BIT'},
	);
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
