#! /usr/bin/perl -w

use strict;
use Getopt::Std;

my %opts;
getopts('c:p:',\%opts);
my $command=(defined($opts{c}) ? $opts{c} : '');
my $prefix=(defined($opts{p}) ? $opts{p} : 'pl_');

( $prefix eq '' || $prefix=~/\A[A-Za-z_][A-Za-z_0-9]*\z/)
  or die "Invalid variable name prefix \"$prefix\": must be a valid sh variable name (begin with a letter or underscore, and contain only letters, numbers and underscores).  Aborting";

if ($#ARGV!=0) {
  die<<EOT;
Format: 
    $0 [-p varname_prefix] /path/to/plot-info.txt

        This script reads in the plot info file and writes out some
        bash/ksh/sh code that will set various variables to the contents of
        the plotinfo code.  The -p option lets you specify a string to prepend
        to the variable names (default is pl_).  For more information about
        plot info files, see the the hwrf_gr_plot_info.gs.inc file in the HWRF
        Graphics Library fix directory.

IMPORTANT: Unlike the grads plot_info() function, this script does not
have knowledge of what variables should be defined in the file.
Hence, it will not set defaults for variables that are not specified
in the DEFAULTS section.

The following variables will be set:

pl_COUNT -- set to the number of models
pl_MODELS -- set to the model names
pl_FIELDS -- set to the full list of fields that were found
pl_MMMM_VAR -- set to value of VAR for model MMMM.  

Example plot info file:

DEFAULTS
deck = /default/path/to/abdeck
title = A Fine Plot Title
somevar = 1

HWRF
deck = /path/to/hwrf/adeck
color = 3

H050
deck = /path/to/h050/adeck
color = 4
somevar = 5

BEST
deck = /path/to/bdeck
color = 1
anothervar = 6

Running this script on that plot info file will produce this code:

pl_COUNT='3' ;
pl_MODELS='HWRF H050 BEST' ;
pl_DEFAULTS_deck='/default/path/to/abdeck' ;
pl_DEFAULTS_title='A Fine Plot Title' ;
pl_DEFAULTS_somevar='1' ;
pl_HWRF_title='A Fine Plot Title' ;
pl_HWRF_somevar='1' ;
pl_HWRF_desc='HWRF' ;
pl_HWRF_deck='/path/to/hwrf/adeck' ;
pl_HWRF_color='3' ;
pl_H050_title='A Fine Plot Title' ;
pl_H050_deck='/path/to/h050/adeck' ;
pl_H050_color='4' ;
pl_H050_desc='H050' ;
pl_H050_somevar='5' ;
pl_BEST_title='A Fine Plot Title' ;
pl_BEST_somevar='1' ;
pl_BEST_deck='/path/to/bdeck' ;
pl_BEST_color='1' ;
pl_BEST_desc='BEST' ;
pl_BEST_anothervar='6' ;
/bin/true

Note that only BEST has the "anothervar" variable\'s value stored
since only the BEST record contained that variable.  However, all
models have deck, title and somevar since those variables are listed
in the DEFAULTS record.

SCRIPT REQUIRES ONE ARGUMENT.  ABORTING
EOT
}

my $file=$ARGV[0];

open(FILE,"< $file")
  or die "Cannot open input file \"$file\" for reading: $!.  Aborting";
my @lines=<FILE>;
close(FILE)
  or warn "Error closing input file \"$file\": $!.  Continuing anyway";

chomp(@lines);
my %defaults;
my %records=('DEFAULTS'=>\%defaults);
my @models;
my $recordname='DEFAULTS';
foreach (@lines) {
  /^\s*$/ and next; # ignore blank lines
  /^\s*[\#*]/ and next; # ignore comment lines
  if(/^\s*([A-Z0-9_a-z]+)\s*$/) {
    # Found a record
    $recordname=$1;
    if($recordname eq 'defaults' || $recordname eq 'DEFAULTS') {
      $recordname='DEFAULTS';
    } elsif(!ref($records{$recordname})) {
      $records{$recordname}={%defaults,desc=>$recordname};
      push @models,$recordname;
    }
  } elsif(/^\s*([A-Z0-9_a-z]+)\s*=\s*(.*?)\s*$/) {
    my ($var,$value)=($1,$2);
    $records{$recordname}->{$var}=$value;
  }
}

sub fixup( $ ) { # replaces ' with '\''
  my $in=shift;
  $in=~s/\'/\'\\\'\'/g;
  return $in;
}

print "$command ${prefix}COUNT='".(1+$#models)."' ;\n";
print "$command ${prefix}MODELS='".join(' ',@models)."' ;\n";

foreach(keys(%defaults)) {
  print "$command ${prefix}DEFAULTS_$_='".fixup($defaults{$_})."' ;\n";
}

my $model;
foreach $model (@models) {
  foreach(keys(%defaults)) {
    if(!defined($records{$model}->{$_})) {
      print "$command ${prefix}${model}_$_=\"\$${prefix}DEFAULTS_$_\" ;\n";
    }
  }
  foreach(keys %{$records{$model}}) {
    print "$command $prefix${model}_$_='".fixup($records{$model}->{$_})."' ;\n";
  }
}
print "/bin/true\n";
