#!/usr/bin/env perl
#
# Configuration script for Vortex tracker code
#
# 
#

use strict;
use warnings;
use Getopt::Long   qw(:config no_ignore_case);
use File::Find;

# set a max depth to search
my $max_depth = 3;

# make sure we do not buffer stdout
select((select(STDOUT), $|=1)[0]);

my %sw=(corepath 	=> '',
	libw3		=> '',
	libbacio	=> '',
	libg2		=> '',
	libz		=> '',
	libjasper	=> '',
	libpng		=> '',
	os		=> $^O,
	ompparallel	=> '',
	dmparallel	=> '',
	mach		=> '',
	fc		=> $ENV{'FC'} || '',
	cc		=> $ENV{'CC'} || '',
	f90		=> $ENV{'F90'}|| '',
	);

GetOptions(\%sw,
	'os=s'		=> $sw{'os'},
	'corepath=s'	=> $sw{'corepath'},
	'ompparallel=s'	=> $sw{'ompparallel'},
	'dmparallel=s'	=> $sw{'dmparallel'},
	'mach=s'	=> $sw{'mach'},
	'fc=s'		=> $sw{'fc'},
	'cc=s'		=> $sw{'cc'},
	'f90=s'		=> $sw{'f90'},
	'libw3=s'	=> $sw{'libw3'},
	'libbacio=s'	=> $sw{'libbacio'},
	'libg2=s'	=> $sw{'libg2'},
	'libz=s'	=> $sw{'libz'},
	'libjasper=s'	=> $sw{'libjasper'},
	'libpng=s'	=> $sw{'libpng'},
);

# GetOptions doesn't like makefile syntax
if (! $sw{'fc'}) {
	$sw{'fc'} = '$(SFC)';
}
if (! $sw{'cc'}) {
	$sw{'cc'} = '$(SCC)';
}
if (! $sw{'f90'}) {
	$sw{'f90'} = '$(SF90)';
}

my (%fflags,%ldflags,%libs,%found);
foreach my $lib ( grep /^lib/, keys %sw) {
	my $libdir = $sw{$lib};
	if (-d $libdir) {
		no warnings "File::Find";
		find( sub { flib($lib, $_); }, $libdir);
	}
	if (!exists $found{$lib}) {
		die "Unable to find $lib\n";
	}
}

sub flib {
	my ($name, $filename) = @_;
	my $dir  = $File::Find::dir;

	my $depth = tr/\///;
	return if $depth > $max_depth;

	# The tracker is ALWAYS compiled as i4 r8, so we need
	# to link against those libs.
	if ($filename =~ /${name}(_i4r8)?\.[a|so]/) {
		my $ldflag = "-L".$dir;
		$found{$name} = 1;
		$filename =~ s/^lib/-l/;
		$filename =~ s/\..*//;
		$ldflags{$ldflag} = 1;
		$libs{$filename}  = 1;
	} elsif ($filename =~ m/mod$/ && $dir =~ m/mods/) {
		my $fflag = "-I".$dir;
		$fflags{$fflag} = 1;
	}
}

my $tr_fflags=join(' ', keys %fflags);
my $tr_libs=join(' ', keys %ldflags, keys %libs);

sub fixup_configure_lines($) {
    $_=$_[0];
    s/CONFIGURE_TRACKER_FFLAGS/$tr_fflags/g;
    s/CONFIGURE_TRACKER_LIBS/$tr_libs/g;
    s/CONFIGURE_PNG_PATH/$sw{libpng}/g ;
    s/CONFIGURE_Z_PATH/$sw{libz}/g ;
    s/CONFIGURE_G2_PATH/$sw{libg2}/g ;
    s/CONFIGURE_JASPER_PATH/$sw{libjasper}/g ;
    s/CONFIGURE_W3_PATH/$sw{libw3}/g ;
    s/CONFIGURE_BACIO_PATH/$sw{libbacio}/g ;
    s/CONFIGURE_FC/$sw{fc}/g ;
    s/CONFIGURE_F90/$sw{f90}/g ;
    s/CONFIGURE_CC/$sw{cc}/g ;
    return $_;
}

# parse the configure.defaults file
my $validresponse = 0 ;
my @platforms = qw ( serial ) ;
my (@optstr, $response, $optchoice);

# Display the choices to the user and get selection
until ( $validresponse ) {
  printf "------------------------------------------------------------------------\n" ;
  printf "Please select from among the following supported platforms.\n\n" ;

  my $opt = 1 ;
  open CONFIGURE_DEFAULTS, "< ./arch/configure.defaults" 
      or die "Cannot open ./arch/configure.defaults for reading" ;
  while ( <CONFIGURE_DEFAULTS> )
  {
    for my $paropt ( @platforms ) 
    {
      if ( substr( $_, 0, 5 ) eq "#ARCH" && ( index( $_, $sw{os} ) >= 0 ) && ( index( $_, $sw{mach} ) >= 0 )
	   && ( index($_, $paropt) >= 0 )  )
      {
        $optstr[$opt] = substr($_,6) ;
        $optstr[$opt] =~ s/^[ 	]*// ;
        $optstr[$opt] =~ s/#.*$//g ;
        chomp($optstr[$opt]) ;
        $optstr[$opt] = $optstr[$opt]." (".$paropt.")" ;
        if ( substr( $optstr[$opt], 0,4 ) ne "NULL" )
        {
          printf "  %2d.  %s\n",$opt,$optstr[$opt] ;
          $opt++ ;
        }
      }
    }
  }
  close CONFIGURE_DEFAULTS ;

  $opt -- ;

  printf "\nEnter selection [%d-%d] : ",1,$opt ;
  $response = <STDIN> ;

  if ( $response == -1 ) { exit ; }

  if ( $response >= 1 && $response <= $opt ) 
  { $validresponse = 1 ; }
  else
  { printf("\nInvalid response (%d)\n",$response);}
}
printf "------------------------------------------------------------------------\n" ;

$optchoice = $response ;

open CONFIGURE_DEFAULTS, "< ./arch/configure.defaults" 
      or die "Cannot open ./arch/configure.defaults for reading" ;
my $latchon = 0 ;
my @machopts;
while ( <CONFIGURE_DEFAULTS> )
{
  if ( substr( $_, 0, 5 ) eq "#ARCH" && $latchon == 1 )
  {
    $latchon = 0 ;
  }
  if ( $latchon == 1 )
  {
    $_=fixup_configure_lines($_);
    push @machopts, $_;
  }
  for my $paropt ( @platforms )
  {
    if ( substr( $_, 0, 5 ) eq "#ARCH" && $latchon == 0
          && ( index( $_, $sw{os} ) >= 0 ) && ( index( $_, $sw{mach} ) >= 0 )
          && ( index($_, $paropt) >= 0 ) )
    {

    my $x=substr($_,6) ;
    $x=~s/^[     ]*// ;
    $x =~ s/#.*$//g ;
    chomp($x) ;
    $x = $x." (".$paropt.")" ;
    if ( $x eq $optstr[$optchoice] )
    {
      $latchon = 1 ;
      $sw{ompparallel} = "" ;
      $sw{dmparallel} = "" ;
      $validresponse = 0 ;

      if ( $paropt eq 'dmpar' ) 
      {
        $sw{dmparallel} = "RSL_LITE" ;
        $sw{dmparallelflag} = "-DDM_PARALLEL" ;
        $sw{fc} = "\$(DM_FC)" ;
        $sw{f90} = "\$(DM_F90)" ;
        $sw{cc} = "\$(DM_CC)" ;
      }
    }
    }

  }
}
close CONFIGURE_DEFAULTS ;


open CONFIGURE_TRK, "> configure.trk" or die "cannot append configure.trk" ;
open ARCH_PREAMBLE, "< arch/preamble" or die "cannot open arch/preamble" ;
my @preamble;
# apply substitutions to the preamble...
while ( defined($_=<ARCH_PREAMBLE>) ) {
    $_=fixup_configure_lines($_);
    print CONFIGURE_TRK $_;
}
close ARCH_PREAMBLE ;
printf CONFIGURE_TRK "# Settings for %s", $optstr[$optchoice] ;
print CONFIGURE_TRK @machopts  ;
open ARCH_POSTAMBLE, "< arch/postamble" or die "cannot open arch/postamble" ;
while ( defined($_=<ARCH_POSTAMBLE>) ) {
    $_=fixup_configure_lines($_);
    print CONFIGURE_TRK $_
}
close ARCH_POSTAMBLE ;
close CONFIGURE_TRK ;

printf "Configuration successful. To build the Vortex Tracker, type: compile \n" ;
printf "------------------------------------------------------------------------\n" ;


