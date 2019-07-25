#! /usr/bin/perl -w 

use strict;
use Getopt::Std qw{getopts};

#################### Parse arguments

my %opts;
getopts('vSVB:p:b:n:c:m:s:',\%opts);

my $verbose=defined($opts{v});
my $skipvars=defined($opts{V});
my $wantbasin=defined($opts{b}) ? uc($opts{b}) : undef;
my $wantcyc=defined($opts{c}) ? uc($opts{c}) : undef;
my $wantnum=defined($opts{n}) ? uc($opts{n}) : undef;
my $wantmodel=$opts{m};
my $prefix=defined($opts{p}) ? $opts{p} : 'cyc_stats_';
my $bound=$opts{B};
my $statsfile=$opts{"s"};
my $simplestatsfile=defined($opts{"S"});

sub verbosely { warn @_ if($verbose) } 

#################### Test arguments for errors

(!defined($statsfile) or $statsfile=~/\A[^\r\n\']+\z/)
  or die "Invalid stats file name \"$statsfile\": must not be empty, and must not contain end-of-line characters or the \' character.  Aborting";
$prefix=~/\A(?:[A-Z_][A-Z_0-9]*)?\z/i
  or die "Invalid variable name prefix \"$prefix\": must be blank or a valid sh identifier.  Aborting.";
(!defined($wantbasin) or $wantbasin=~/\A[A-Z]{2}\z/i)
  or die "Invalid basin \"$opts{b}\": must be two letters.  Aborting";
(!defined($bound) or $bound=~/\A[0-9.e+-]*\z/i)
  or die "Invalid boundary width \"$opts{B}\": must be a valid real number.  Aborting";
(!defined($wantcyc) or $wantcyc=~/\A\d{10}\z/)
  or die "Invalid cycle number \"$opts{c}\": must be ten digits: YYYYMMDDHH.  Aborting";
(!defined($wantnum) or $wantnum=~/\A\d{1,2}\z/)
  or die "Invalid storm number \"$opts{n}\": must be a one or two digit number (no basin).  Aborting";
(!defined($wantmodel) or (length($wantmodel)<=4 && $wantmodel=~/\A\s*[A-Z_0-9]{1,4}\z/))
  or die "Invalid model name \"$opts{m}\": must be a valid, capitalized ATCF model id.  Aborting";

#################### Read the input lines

my @lines=<STDIN>;              # Read in the entire input file
chomp @lines;                   # remove end-of-line characters

#################### Parse the input lines

my (@lats,@lons,@hours,@wind,@pres);
my ($minlat,$maxlat,$minlon,$maxlon)=('','','','');
my $valid_lines=0;
my %found;
my $prevhour=-1e12;
foreach (@lines) {
  verbosely "Line: $_\n";

  # Split the line into its components:
  unless ( ~/^(\w\w)\s*,\s*(\d+)\s*,\s*(\d{10})\s*,\s*\d+\s*,\s*(....)\s*,\s*(\d+)\s*,\s*(\d+)([NS ])\s*,\s*(\d+)([EW ])\s*,\s*(\d+)\s*,\s*(\d+)\s*,/ ) {
    # Invalid line.
    warn "Found unparsable line: \"$_\".  Continuing anyway";
    next;
  }
  my ($basin,$num,$cyc,$model,$hour,$lat,$latdir,$lon,$londir,$wind,$pres)
    = ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11);
  if (!defined($pres)) {
    warn "Found unparsable line: \"$_\".  Continuing anyway";
    next;
  }

  # Skip badval lines:
  next if $londir eq ' ' || $latdir eq ' ';

  # Skip lines that don't match the user's requirements:
  next if(defined($wantbasin) && uc($basin) ne $wantbasin);
  next if(defined($wantnum) && $num != $wantnum);
  next if(defined($wantcyc) && $cyc != $wantcyc);
  next if(defined($wantmodel) && $model ne $wantmodel);

  # Ignore duplicate lines:
  if ($prevhour==$hour) {
    verbosely "Ignoring duplicate of hour $hour: \"$_\"\n";
    next;
  }
  $prevhour=$hour;

  verbosely("  ... line is valid\n");
  $valid_lines++;        # got a valid line, so increment line counter
  $lat=-$lat if($latdir eq 'S');
  $lon=-$lon if($londir eq 'W');

  $lat/=10.0;                   # convert to degrees
  $lon/=10.0;

  $lon+=360 if $lon<0;          # Make all longitudes positive

  $minlon=$lon if($minlon eq '' or $lon<$minlon);
  $minlat=$lat if($minlat eq '' or $lat<$minlat);
  $maxlon=$lon if($maxlon eq '' or $lon>$maxlon);
  $maxlat=$lat if($maxlat eq '' or $lat>$maxlat);

  push @lats,0+$lat;
  push @lons,0+$lon;
  push @hours,0+$hour;
  push @wind,0+$wind;
  push @pres,0+$pres;
}

# Done parsing the input file.

#################### Generate the output variables if requested

if (!$skipvars) {
  print<<EOT;
export ${prefix}NUMDATA='$valid_lines' ;
export ${prefix}MAXLAT='$maxlat' ;
export ${prefix}MAXLON='$maxlon' ;
export ${prefix}MINLAT='$minlat' ;
export ${prefix}MINLON='$minlon' ;
EOT
  if (defined($bound)) {
    if ($valid_lines>0) {
      print<<EOT3;
export ${prefix}NORTHBOUND='${\($maxlat+$bound)}' ;
export ${prefix}SOUTHBOUND='${\($minlat-$bound)}' ;
export ${prefix}EASTBOUND='${\($maxlon+$bound)}' ;
export ${prefix}WESTBOUND='${\($minlon-$bound)}' ;
EOT3
    } else {
      print<<EOT2;
export ${prefix}NORTHBOUND='' ;
export ${prefix}SOUTHBOUND='' ;
export ${prefix}EASTBOUND='' ;
export ${prefix}WESTBOUND='' ;
EOT2
    }
  }
}

#################### Create the stats file if requested
sub clipstr( $$ ) {
  my($len,$str)=@_;
  if(length($str)>$len) {
    return '*' x $len;
  } else {
    return substr($str,0,$len);
  }
}
if (defined($statsfile)) {
  open(STATS,"> $statsfile")
    or warn "Unable to open output file \"$statsfile\": $!.  Continuing anyway";
  for(my $i=0;$i<=$#lats;$i++) {
    my $lon=$lons[$i];
    $lon-=360 if $lon>180;
    my @data=( clipstr(5,sprintf("%5.1f",0+$hours[$i])),
               clipstr(8,sprintf("%8.2f",$lon)),
               clipstr(7,sprintf("%7.2f",$lats[$i]+0)),
               clipstr(7,sprintf("%7.2f",$pres[$i]+0)),
               clipstr(6,sprintf("%6.2f",$wind[$i])) );
    if($simplestatsfile) {
        print(STATS join(' ',@data)."\n")
            or warn "Error writing to output file \"$statsfile\": $!.  Continuing anyway";
    } else {
        print(STATS "HOUR:$data[0]  LONG:$data[1]  LAT:$data[2]"
          ."  MIN PRESS (hPa): $data[3]   MAX SURF WIND (KNOTS):"
          ."$data[4]\n")
            or warn "Error writing to output file \"$statsfile\": $!.  Continuing anyway";
    }
#    printf(STATS "HOUR:%5.1f  LONG:%8.2f  LAT:%7.2f  "
#           ."MIN PRESS (hPa): %7.2f   MAX SURF WIND (KNOTS):%6.2f\n",
#           $hours[$i],$lons[$i]-360,$lats[$i],$pres[$i],$wind[$i])
  }
  close(STATS)
    or warn "Error closing output file \"$statsfile\": $!.  Continuing anyway";
  if (!$skipvars) {
    print "export ${prefix}STATSFILE='$statsfile' ; \n";
  }
}

#################### Generate the end of the input variable definition text

if (!$skipvars) {
  print "/bin/true\n";       # to avoid syntax error from trailing ";"
}
