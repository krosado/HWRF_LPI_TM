#!/usr/bin/perl
#
# Configuration script for POM code
# 
# Be sure to run as ./configure (to avoid getting a system configure command by mistake)
#

$sw_core_path = "" ;
$sw_lib_w3 = "" ;
$sw_lib_sp = "" ;
$sw_lib_sfcio = "" ;
$sw_lib_blas = "" ;
$sw_os = "ARCH" ;           # ARCH will match any
$sw_mach = "ARCH" ;         # ARCH will match any
$sw_dmparallel = "" ;
$sw_ompparallel = "" ;
$sw_fc = "\$(SFC)" ;
$sw_cc = "\$(SCC)" ;
$sw_f90 = "\$(SF90)" ;

# make sure we do not buffer stdout
select((select(STDOUT), $|=1)[0]);

while ( substr( $ARGV[0], 0, 1 ) eq "-" ) {
    if($ARGV[0]=~/^-([^=]+)=(.*)$/) {
        my ($arg,$opt)=($1,$2);
        $sw_netcdf=$opt      if $arg eq 'netcdf';
        $sw_pnetcdf=$opt     if $arg eq 'pnetcdf';
        $sw_core_path=$opt   if $arg eq 'corepath';
        $sw_lib_w3=$opt      if $arg eq 'libw3';
        $sw_lib_sp=$opt      if $arg eq 'libsp';
        $sw_lib_sfcio=$opt   if $arg eq 'libsfcio';
        $sw_lib_blas=$opt    if $arg eq 'libblas';
        $sw_os=$opt          if $arg eq 'os';
        $sw_mach=$opt        if $arg eq 'mach';
        $sw_dmparallel=$opt  if $arg eq 'dmparallel';
        $sm_omparallel=$opt  if $arg eq 'omparallel';
    }
    shift @ARGV ;
}

$netcdfinc="-I$sw_netcdf/include";
if ( -e "$sw_netcdf/lib/libnetcdff.a" || -e "$sw_netcdf/lib/libnetcdff.so" ) {
    $netcdflib="-L$sw_netcdf/lib -lnetcdf -lnetcdff $netcdfinc";
} else {
    $netcdflib="-L$sw_netcdf/lib -lnetcdf $netcdfinc";
}

# parse the configure.defaults file

$validresponse = 0 ;
@platforms = qw ( serial dmpar ) ;

# Display the choices to the user and get selection
until ( $validresponse ) {
  printf "------------------------------------------------------------------------\n" ;
  printf "Please select from among the following supported platforms.\n\n" ;

  $opt = 1 ;
  open CONFIGURE_DEFAULTS, "< ./arch/configure.defaults" 
      or die "Cannot open ./arch/configure.defaults for reading" ;
  while ( <CONFIGURE_DEFAULTS> )
  {
    for $paropt ( @platforms ) 
    {
      if ( substr( $_, 0, 5 ) eq "#ARCH" && ( index( $_, $sw_os ) >= 0 ) && ( index( $_, $sw_mach ) >= 0 )
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
$latchon = 0 ;
while ( <CONFIGURE_DEFAULTS> )
{
  if ( substr( $_, 0, 5 ) eq "#ARCH" && $latchon == 1 )
  {
    $latchon = 0 ;
  }
  if ( $latchon == 1 )
  {
      s/CONFIGURE_W3_PATH/$sw_lib_w3/g ;
      s/CONFIGURE_SP_PATH/$sw_lib_sp/g ;
      s/CONFIGURE_SFCIO_PATH/$sw_lib_sfcio/g ;
      s/CONFIGURE_BLAS_PATH/$sw_lib_blas/g ;
      s/CONFIGURE_FC/$sw_fc/g ;
      s/CONFIGURE_F90/$sw_f90/g ;
      s/CONFIGURE_CC/$sw_cc/g ;
      s/CONFIGURE_NETCDF_PATH/$sw_netcdf/g ;
      s/CONFIGURE_PNETCDF_PATH/$sw_pnetcdf/g ;
      s/CONFIGURE_NETCDFINC/$netcdfinc/g;
      s/CONFIGURE_NETCDFLIB/$netcdflib/g;

    @machopts = ( @machopts, $_ ) ;
  }
  for $paropt ( @platforms )
  {
    if ( substr( $_, 0, 5 ) eq "#ARCH" && $latchon == 0
          && ( index( $_, $sw_os ) >= 0 ) && ( index( $_, $sw_mach ) >= 0 )
          && ( index($_, $paropt) >= 0 ) )
    {

    $x=substr($_,6) ;
    $x=~s/^[     ]*// ;
    $x =~ s/#.*$//g ;
    chomp($x) ;
    $x = $x." (".$paropt.")" ;
    if ( $x eq $optstr[$optchoice] )
    {
      $latchon = 1 ;
      $sw_ompparallel = "" ;
      $sw_dmparallel = "" ;
      $validresponse = 0 ;

      if ( $paropt eq 'dmpar' ) 
      {
        $sw_dmparallel = "RSL_LITE" ;
        $sw_dmparallelflag = "-DDM_PARALLEL" ;
	$sw_fc = "\$(DM_FC)" ;
	$sw_f90 = "\$(DM_F90)" ;
	$sw_cc = "\$(DM_CC)" ;
      }
    }
    }

  }
}
close CONFIGURE_DEFAULTS ;


open CONFIGURE_POM, "> configure.pom" or die "cannot append configure.pom" ;
open ARCH_PREAMBLE, "< arch/preamble" or die "cannot open arch/preamble" ;
my @preamble;
# apply substitutions to the preamble...
while ( <ARCH_PREAMBLE> )
  {
  @preamble = ( @preamble, $_ ) ;
  }
close ARCH_PREAMBLE ;
print CONFIGURE_POM @preamble  ;
close ARCH_PREAMBLE ;
printf CONFIGURE_POM "# Settings for %s", $optstr[$optchoice] ;
print CONFIGURE_POM @machopts  ;
open ARCH_POSTAMBLE, "< arch/postamble" or die "cannot open arch/postamble" ;
while ( <ARCH_POSTAMBLE> ) {
  print CONFIGURE_POM;
 }
close ARCH_POSTAMBLE ;
close CONFIGURE_POM ;

printf "Configuration successful. To build the POM-TC, type: compile \n" ;
printf "------------------------------------------------------------------------\n" ;


