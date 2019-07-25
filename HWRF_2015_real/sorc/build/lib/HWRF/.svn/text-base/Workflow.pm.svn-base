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
# $Id: Workflow.pm 134 2011-04-12 17:36:46Z tpbrown@ucar.edu $
#

package HWRF::Workflow;

use 5.008002;
use strict;
use warnings;

use Data::Dumper;
use Cwd           qw(abs_path cwd);
use Carp;
use Julian        qw(jdn date split_date);
use Verbose       qw(:all);

require Exporter;

our @ISA = qw(Exporter);
our $VERSION = '0.01';

=head1 NAME

HWRF::Workflow - Perl extensions for the HWRF Workflow Manager.

=head1 SYNOPSIS

  use HWRF::Workflow;
  my $mgr = HWRF::Workflow->new();
  $mgr->xml();

=head1 DESCRIPTION

The B<HWRF::Workflow> module provides Perl functions for the Workflow
Manager.

=head2 EXPORT

None by default.

=cut

our %EXPORT_TAGS = ( 'all' => [ qw(
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

# IO formats for WPS, WRF, WPP and UPP.
# The hash values are needed in the namelist files for each program.
use constant IO_FMT => {
	BINARY => 1,
	NETCDF => 2,
	GRIB1  => 3,
};

=head2 METHODS

=head3 new

=cut

sub new {
	my $proto  = shift;
	my $class  = ref($proto) || $proto;
	my (%args) = @_;

	my $self = {};

	$self->{XML}      = {
		'FILE'    => undef,
		'STORE'   => undef,
	};
	$self->{EXEC}     = {
		'FILE'    => undef,
		'FREQ'    => 180,
	};
	$self->{ENTITIES} = {
		'SITE'    => undef,
		'STORM'   => {
			'STORM_NAME'    => undef,
			'SID'           => undef,
			'BASIN'         => undef,
		},
		'FCST'    => {
			'FCST_LENGTH'   => undef,
			'FCST_INTERVAL' => undef,
		},
		'OPTIONS' => {
			'IO_FMT'        => undef,
			'COUPLED'       => undef,
			'CYCLE'         => undef,
			'VORTEX_INIT'   => undef,
		},
	};
	$self->{START_TIME} = {
		'YEAR'    => undef,
		'MONTH'   => undef,
		'DAY'     => undef,
		'HOUR'    => undef,
		'MINUTE'  => undef,
		'SECOND'  => undef,
	};

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

	for my $key (reverse sort keys %args) {
		if ($self->can($key)) {
			my $iam = ref($args{$key});
			if ($iam eq "HASH") {
				$self->$key(%{$args{$key}});
			} elsif ($iam eq "") {
				$self->$key($args{$key});
			}
		}
	}

	if (defined $self->{XML}->{FILE}) {
		$self->write_xml();
	}

}

# Internal routine to set Jet specific information.
sub _site_jet {
	my $self   = shift;
	my (%args) = @_;

	my $prefix = "/lfs1/projects/dtc-hurr/datasets";
	$self->{ENTITIES}->{SITE} = {
		HWRF_HOME             => '',
		HWRF_SCRIPTS          => '&HWRF_HOME;/bin',
		HWRF_DATA             => '',
		HWRF_LOG              => '&HWRF_DATA;/log',

		ARCH                  => 'LINUX_Intel',
		PE                    => 'hfip',
		SERIALPE              => 'hfipserial 1',
		PROJECT               => 'dtc-hurr',
		SCHEDULER             => 'sge',
		MPIRUN                => 'mpirun',

		TCVITALS              => "$prefix/Tcvitals",
		GEOG_DATA_PATH        => "/lfs0/projects/WPS/GEOG",
		GFS_SPECTRAL_DIR      => "$prefix/GFS_pre13d_spectral",
		GFS_GRIDDED_DIR       => "$prefix/GFS_pre13d_gridded",
		OBS_ROOT              => "$prefix/obs",
		LOOP_CURRENT_DIR      => "$prefix/Loop_current",
		OCEAN_FIXED_DIR       => "$prefix/fix/ocean",

		HWRF_GRAPHICS_INSTALL => '&HWRF_HOME;/emc_hwrf_graphics',
		GRADS_BIN             => "/home/dtc/opengrads1.10/bin",
		GADDIR                => "/home/dtc/opengrads1.10/data",
		GA1UDXT               => "/home/dtc/opengrads1.10/bin/gex/udxt",
		ADECKPATH             => "$prefix/abdecks",
	};

	$self->{ENTITIES}->{OPTIONS}->{IO_FMT} = $self->IO_FMT->{NETCDF};

}

# Internal routine to set Bluefire specific information.
sub _site_bluefire {
	my $self   = shift;
	my (%args) = @_;

	my $prefix = "/rap/dtc/HWRF_datasets";
	my $grads  = "/contrib/opengrads/1.10.r2.oga";
	$self->{ENTITIES}->{SITE} = {
		HWRF_HOME             => '',
		HWRF_SCRIPTS          => '&HWRF_HOME;/bin',
		HWRF_DATA             => '',
		HWRF_LOG              => '&HWRF_DATA;/log',

		ARCH                  => 'IBM_LSF',
		PE                    => 'regular',
		SERIALPE              => 'regular 1',
		PROJECT               => '48500053',
		SCHEDULER             => 'lsf',
		MPIRUN                => 'mpirun.lsf',

		TCVITALS              => "$prefix/Tcvitals",
		GEOG_DATA_PATH        => "$prefix/geog",
		GFS_SPECTRAL_DIR      => "$prefix/GFS_pre13d_spectral",
		GFS_GRIDDED_DIR       => "$prefix/GFS_pre13d_gridded",
		OBS_ROOT              => "$prefix/obs",
		LOOP_CURRENT_DIR      => "$prefix/Loop_current",
		OCEAN_FIXED_DIR       => "$prefix/fix/ocean",

		HWRF_GRAPHICS_INSTALL => '&HWRF_HOME;/emc_hwrf_graphics',
		GRADS_BIN             => "$grads",
		GADDIR                => "$grads/Resources/SupportData/",
		GA1UDXT => "$grads/AIX/Versions/1.10.r2.oga/00CBBBA14C00/gex/udxt",
		ADECKPATH             => "$prefix/abdecks",
	};

	$self->{ENTITIES}->{OPTIONS}->{IO_FMT} = $self->IO_FMT->{BINARY};

}

# Internal routine to set Vapor specific information.
sub _site_vapor {
	my $self   = shift;
	my (%args) = @_;

	$self->{ENTITIES}->{SITE} = {
		HWRF_HOME             => '',
		HWRF_SCRIPTS          => '&HWRF_HOME;/bin',
		HWRF_DATA             => '',
		HWRF_LOG              => '&HWRF_DATA;/log',

		ARCH                  => 'IBM_LSF',
		PE                    => '',
		SERIALPE              => '',
		PROJECT               => '',
		SCHEDULER             => 'll',
		MPIRUN                => 'mpirun',

		TCVITALS              => "",
		GEOG_DATA_PATH        => "",
		GFS_SPECTRAL_DIR      => "",
		GFS_GRIDDED_DIR       => "",
		OBS_ROOT              => "",
		LOOP_CURRENT_DIR      => "",
		OCEAN_FIXED_DIR       => "",

		HWRF_GRAPHICS_INSTALL => '&HWRF_HOME;/emc_hwrf_graphics',
		GRADS_BIN             => "",
		GADDIR                => "",
		GA1UDXT               => "",
		ADECKPATH             => "",
	};

	$self->{ENTITIES}->{OPTIONS}->{IO_FMT} = $self->IO_FMT->{BINARY};

}

#
# This function will update all the xml entities. It should be given
# a hash of entities to update. I.e.
#
#	$self->_update_xml_entities( %{$self->{ENTITIES}->{SITE}} );
#
sub _update_xml_entities {
	my $self   = shift;
	my (%args) = @_;

	if (!defined $self->{XML}->{FILE}) {
		croak "workflow XML file is undefined";
	}

	# Slurp in the xml file
	open (FILE, '<', $self->{XML}->{FILE}) or
		croak "unable to open xml file: $self->{XML}->{FILE}: $!";
	my @file = <FILE>;
	close(FILE);

	# ENTITY start and end tags
	my $s = '<!ENTITY';
	my $e = '>';

	# set up a hash of all the arguments marking them as unseen.
	my %seen;
	foreach my $key (keys %args) {
		$seen{$key} = 0;
	}

	foreach my $l (@file) {
		while (my ($k, $v) = each(%args)) {
			if ($v) {
				if ($l =~ s/(?<=\Q$s\E)          # start tag
					    (\s*)$k(\s*)(.*?)    # key
					    (?=\Q$e\E)           # end tag
					    /$1$k$2"$v\"/sx){
					# make a hash of keys updated
					$seen{$k} = 1;
				}
			}
		}
	}

	open (FILE, '>', $self->{XML}->{FILE}) or
		croak "unable to open xml file: $self->{XML}->{FILE}: $!";
	foreach my $l (@file) {
		if ($l =~ /\]>/) { # end of DOCTYPE
			while (my ($k, $v) = each(%seen)) {
				if (!$v and $args{$k}) {
				  print FILE "  $s  $k \"".$args{$k}."\"$e\n";
				}
			}
		}
		print FILE $l;
	}
	close(FILE);
}

#
# This function will update all the xml cycles.
#
sub _update_xml_cycles {
	my $self   = shift;
	my (%args) = @_;

	if (!defined $self->{XML}->{FILE}) {
		croak "workflow XML file is undefined";
	}

	# Slurp in the xml file
	open (FILE, '<', $self->{XML}->{FILE}) or
		croak "unable to open xml file: $self->{XML}->{FILE}: $!";
	my @file = <FILE>;
	close(FILE);

	# build the cycle(s) list
	my @times;
	my @cycles = $self->cycle_times();

	# write out the cycles
	open (FILE, '>', $self->{XML}->{FILE}) or
		croak "unable to open xml file: $self->{XML}->{FILE}: $!";

	my $flow = 0;    # Within the workflow tag
	my $seen = 0;    # Seen a <cycle></cycle> tag

	foreach my $l (@file) {
		# check to see if we are in the workflow
		if ($l =~ /<workflow/) {
			$flow = 1;
		}
		if ($l =~ /^(\s*)(<cycle>)(?:(?:\d|\s|,)*)(<\/cycle>)/) {
			if (!$seen and $flow) {
				foreach my $t (@cycles) {
					print FILE "$1$2$t$3\n";
				}
				$seen = 1;
			}
		} elsif ($l =~ /(<\/workflow>)|(<log>)/ and !$seen) {
			foreach my $t (@cycles) {
				print FILE "  <cycle>$t</cycle>\n";
			}
			$seen = 1;
			print FILE $l; # put back the end of workflow tag
		} else {
			print FILE $l;
		}
	}
	close(FILE);
}

=head3 xml

	$mgr->xml(file => $filename, store => $store);

The function B<xml> sets the Workflow Managers XML file and optional
store file.

If the store file is not specified a ".store" is appended to the xml
filename.

=cut

sub xml {
	my $self   = shift;
	my (%args) = @_;

	my ($filename, $store);
	if ($#_ == 0) {
		$filename = abs_path(shift);
	} else {
		my (%args) = @_;
		if (defined $args{'file'}) {
			$filename = abs_path($args{'file'});
		}
		if (defined $args{'store'}) {
			$store = abs_path($args{'store'});
		}
	}

	if (defined $filename) {
		$self->{XML}->{FILE} = $filename;
	}
	if (defined $store) {
		$self->{XML}->{STORE} = $store;
	} elsif (defined $self->{XML}->{FILE}) {
		$self->{XML}->{STORE} = $self->{XML}->{FILE} .".store";
	}

	return ($self->{XML}->{FILE}, $self->{XML}->{STORE});
}

=head3 site

	$mgr->site();

The function B<site> will update the Workflow Managers site
specific entities.

=cut

sub site {
	my $self   = shift;
	my (%args) = @_;

	my $hwrf = HWRF->new();
	my $func = "_site_" . lc($hwrf->site());
	if ($self->can($func)) {
		$self->$func();
	} else {
		croak "unable to update the Workflow XML for an unknown site";
	}

	if (defined $args{'hwrf_home'}) {
		$self->{ENTITIES}->{SITE}->{HWRF_HOME} =
			abs_path($args{'hwrf_home'});
	}
	if (defined $args{'hwrf_data'}) {
		$self->{ENTITIES}->{SITE}->{HWRF_DATA} =
			abs_path($args{'hwrf_data'});
	}

	return $self->{ENTITIES}->{SITE};
}

=head3 storm

	$mgr->storm();

The function B<storm> will update the Workflow Managers storm
specific entities.

=cut

sub storm {
	my $self   = shift;
	my (%args) = @_;

	# storm mappings
	while (my ($k, $v) = each(%args)) {
		if (lc($k) eq "name") {
			$self->{ENTITIES}->{STORM}->{STORM_NAME} = $v;
		} elsif (lc($k) eq "id") {
			$self->{ENTITIES}->{STORM}->{SID} = $v;
		} elsif (lc($k) eq "basin") {
			$self->{ENTITIES}->{STORM}->{BASIN} = $v;
		}
	}

	return ($self->{ENTITIES}->{STORM}->{STORM_NAME},
		$self->{ENTITIES}->{STORM}->{SID},
		$self->{ENTITIES}->{STORM}->{BASIN}
	);
}

=head3 forecast

	$mgr->forecast();

The function B<forecast> will update the Workflow Managers forecast
specific entities.

=cut

sub forecast {
	my $self   = shift;
	my (%args) = @_;

	# forecast mappings
	while (my ($k, $v) = each(%args)) {
		if (lc($k) eq "length") {
			$self->{ENTITIES}->{FCST}->{FCST_LENGTH} = $v;
		} elsif (lc($k) eq "interval") {
			$self->{ENTITIES}->{FCST}->{FCST_INTERVAL} = $v;
		}
	}
	return ($self->{ENTITIES}->{FCST}->{FCST_LENGTH},
		$self->{ENTITIES}->{FCST}->{FCST_INTERVAL},
	);
}

=head3

	$mgr->io(format => "netcdf");

The function B<io> will update the Workflow Managers IO method for
WPS, WRF, WPP and UPP.

=cut

sub io {
	my $self   = shift;

	my $fmt;
	if ($#_ == 0) {
		$fmt = shift;
	} else {
		my (%args) = @_;
		$fmt = $args{'format'};
	}
	
	if (defined $fmt) {
		$self->{ENTITIES}->{OPTIONS}->{IO_FMT} =
			$self->IO_FMT->{uc($fmt)};
	}

	return $self->{ENTITIES}->{OPTIONS}->{IO_FMT};
}

=head3

	$mgr->coupled(coupled => "true");

The function B<coupled> will update the Workflow Managers COUPLED
entity. To indicate if HWRF should run the coupler to an ocean model.

=cut

sub coupled {
	my $self   = shift;

	my $cpl;
	if ($#_ == 0) {
		$cpl = shift;
	} else {
		my (%args) = @_;
		$cpl = $args{'coupled'};
	}

	if (defined $cpl) {
		if ($cpl =~ /^t|1/i) {
			$self->{ENTITIES}->{OPTIONS}->{COUPLED} = "T";
		} elsif ($cpl =~ /^f|0/i) {
			$self->{ENTITIES}->{OPTIONS}->{COUPLED} = "F";
		} else {
			croak "unable to interpret coupled: $cpl";
		}
	}

	return $self->{ENTITIES}->{OPTIONS}->{COUPLED};
}

=head3

	($year, $month, $day, $hours, $minute) = $mgr->start_time(200808251200);

The function B<start_time> will set the simulation start time.
It will call B<Julian::split_date> to parse the date format.

=cut

sub start_time {
	my $self   = shift;

	my $t;
	if ($#_ == 0) {
		$t = shift;
	} else {
		my (%args) = @_;
		$t = $args{'time'};
	}

	if (defined $t) {
		my @start_time = split_date($t);
		$self->{START_TIME}->{YEAR}   = $start_time[0];
		$self->{START_TIME}->{MONTH}  = $start_time[1];
		$self->{START_TIME}->{DAY}    = $start_time[2];
		$self->{START_TIME}->{HOUR}   = $start_time[3];
		$self->{START_TIME}->{MINUTE} = $start_time[4];
		$self->{START_TIME}->{SECOND} = $start_time[5];
	}

	return wantarray ? ( $self->{START_TIME}->{YEAR},
			    $self->{START_TIME}->{MONTH},
			    $self->{START_TIME}->{DAY},
			    $self->{START_TIME}->{HOUR},
			    $self->{START_TIME}->{MINUTE},
			    $self->{START_TIME}->{SECOND})
			 : sprintf("%4d %02d %02d %02d %02d %02d",
				  $self->{START_TIME}->{YEAR},
				  $self->{START_TIME}->{MONTH},
				  $self->{START_TIME}->{DAY},
				  $self->{START_TIME}->{HOUR},
				  $self->{START_TIME}->{MINUTE},
				  $self->{START_TIME}->{SECOND});
}

=head3

	$mgr->cycle(cycle => "true");

The function B<cycle> will update the Workflow Managers CYCLE
entity. To indicate if HWRF should run as a cycled storm or as a
cold start.

=cut

sub cycle {
	my $self   = shift;

	my $cycle;
	if ($#_ == 0) {
		$cycle = shift;
	} else {
		my (%args) = @_;
		$cycle = $args{'cycle'};
	}

	if (defined $cycle) {
		if ($cycle =~ /^t|1/i) {
			$self->{ENTITIES}->{OPTIONS}->{CYCLE} = "T";
		} elsif ($cycle =~ /^f|0/i) {
			$self->{ENTITIES}->{OPTIONS}->{CYCLE} = "F";
		} else {
			croak "unable to interpret cycle: $cycle";
		}
	}

	return $self->{ENTITIES}->{OPTIONS}->{CYCLE};
}

=head3

	@times = $mgr->cycle_times();

The function B<cycle_times> will return a list of all the start times if
cycling is enabled.

That is it will start from B<start_time> and increase in forecast increments
till the end of the forecast interval.

=cut

sub cycle_times {
	my $self   = shift;

	my @times;

	if ($self->cycle() eq "F") {
		my $t = $self->start_time();
		push(@times, $t);
		return @times;
	}

	my ($len, $int) = $self->forecast();
	my $max = int($len / $int);

	my $time = jdn($self->start_time());

	for (my $i = 0; $i < $max; ++$i) {
		my $d = date($time);
		push(@times, $d);
		$time += $int / 24;
	}

	return @times;
}

=head3

	$mgr->vortex(init => "true");

The function B<vortex> will update the Workflow Managers VORTEX_INIT
entity. To indicate if HWRF should run a vortex initialization stage.

=cut

sub vortex {
	my $self   = shift;

	my $init;
	if ($#_ == 0) {
		$init = shift;
	} else {
		my (%args) = @_;
		$init = $args{'init'};
	}

	if (defined $init) {
		if ($init =~ /^t|1/i) {
			$self->{ENTITIES}->{OPTIONS}->{VORTEX_INIT} = "T";
		} elsif ($init =~ /^f|0/i) {
			$self->{ENTITIES}->{OPTIONS}->{VORTEX_INIT} = "F";
		} else {
			croak "unable to interpret vortex init state: $init";
		}
	}

	return $self->{ENTITIES}->{OPTIONS}->{VORTEX_INIT};

}


=head3

	$mgr->write_xml();

The function B<write_xml> will update the Workflow Managers XML with
all the currently defined entities.

=cut

sub write_xml {
	my $self   = shift;
	my (%args) = @_;

	$self->_update_xml_entities(
		%{$self->{ENTITIES}->{SITE}},
		%{$self->{ENTITIES}->{FCST}},
		%{$self->{ENTITIES}->{STORM}},
		%{$self->{ENTITIES}->{OPTIONS}}
	);

	$self->_update_xml_cycles();
}

=head3

	$mgr->exe(file => "workflowmgr/workflowmgr.rb",
		  frequency => 180);

The function B<exe> sets the path to the workflow manager executable
and frequency to execute it when B<run> is called.

=cut

sub exe {
	my $self   = shift;
	my (%args) = @_;

	if (defined $args{'file'}) {
		$self->{EXEC}->{FILE} = abs_path($args{'file'});
	}
	if (defined $args{'frequency'}) {
		$self->{EXEC}->{FREQ} = $args{'frequency'};
	}

	return ($self->{EXEC}->{FILE}, $self->{EXEC}->{FREQ});
}

=head3

	$mgr->run();

The function B<run> will run the workflow manger in a continuous
loop.

It will 

=cut

sub run {
	my $self   = shift;

	# update our options
	$self->exe(@_);

	# make sure our options are defined
	if (!defined $self->{EXEC}->{FILE}) {
		croak "unable to run the workflow manager: executable unknown";
	}

	if (!defined $self->{EXEC}->{FREQ}) {
		croak "unable to run the workflow manager: frequency unknown";
	}

	# make sure the executable and xml file exist
	if (! -x $self->{EXEC}->{FILE}) {
		croak "unable to execute the workflow manager: "
			. $self->{EXEC}->{FILE};
	}

	my ($xml, $store) = $self->xml();
	if (! -r $xml) {
		croak "unable to read the workflow manager XML: $xml";
	}
	if (-e $store) {
		unlink $store;
	}

	my $c = $self->{EXEC}->{FILE} ." -x " . $xml ." -s " . $store;

	# create a hash of return values
	my %err;
	my $e = 0;
	my $max = 1000;
	for (my $i = 0; $i < $max; ++$i) {
		# create the "messages/go" for the first cycle time
		# once the directory has been created
		if ($i == 1) {
			$self->create_result();
		}
		my $res = `$c`;
		$e = $?;
		if ($e == -1) {
			croak "failed to exec $c";
		} elsif ($e & 127) {
			croak "workflow manager died with signal: ".($e & 127);
		}
		if ($e >> 8) {
			last;
		}
		if ($res) {
			print $res;
			my ($prog, $errno);
			if ($res =~ /crashed/) {
				($prog, $errno) =
					($res =~ /::\s(\w+)\s.*=(\d+)$/);
				$err{$prog} = $errno;
			} elsif ($res =~ /giving\s+up/) {
				$prog = ($res =~ /::\s(\w+)/);
				$e = $err{$prog};
				last;
			}
		}
		sleep($self->{EXEC}->{FREQ});
	}

	return $e;
}

=head3

	$mgr->create_result(dir   => "messages",
			    file  => "go",
			    wait  => 60,
			    tries => 5)

The function B<create_result> will create/touch a result file.

It was written to create the "messages/go" file for the first cycle.
So these are the defaults.

This should be called after the first workflow manager iteration has
occurred. As the workflow manager will move the directory out of the
way if it exists prior to it running.

=cut

sub create_result {
	my $self   = shift;
	my (%args) = @_;

	# set the defaults for relocate
	my ($dir, $file, $wait, $tries) = ("messages", "go", 60, 5);

	# allow arguments to override the defaults
	if (defined $args{'dir'}) {
		$dir = $args{'dir'};
	}
	if (defined $args{'file'}) {
		$file = $args{'file'};
	}
	if (defined $args{'wait'}) {
		$wait = $args{'wait'};
	}
	if (defined $args{'tries'}) {
		$tries = $args{'tries'};
	}

	# get the start time, data dir and storm ID
	my $time  = $self->{START_TIME}->{YEAR}  .
		    $self->{START_TIME}->{MONTH} .
		    $self->{START_TIME}->{DAY}   .
		    $self->{START_TIME}->{HOUR};
	my $cdir = $self->{ENTITIES}->{SITE}->{HWRF_DATA} ."/"
		 . $self->{ENTITIES}->{STORM}->{SID}      ."/"
		 . $time ."/". $dir;

	# should set an alarm to make sure the directory exists
	for (my $i = 0; $i <= $tries; ++$i) {
		if ( -d $cdir) {
			`touch $cdir/$file`;
			last;
		} else {
			sleep($wait);
		}
	}

	if (! -e $cdir."/".$file) {
		croak "file creation failed: $cdir/$file : $!";
	}

}

=head1 SEE ALSO

perl (1),
DTC L<http://www.dtcenter.org/>,
HWRF L<http://www.dtcenter.org/HurrWRF/users/index.php>,
Workflow Manager
L<https://intranet.fsl.noaa.gov/twiki/bin/view/WorkflowMgr/WebHome>,

=head1 AUTHOR

Timothy P Brown, E<lt>Timothy.P.Brown@noaa.govE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2011 by Timothy P Brown


=cut

1;
__END__
