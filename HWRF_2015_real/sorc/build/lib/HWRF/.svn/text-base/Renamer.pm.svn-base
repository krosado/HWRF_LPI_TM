package HWRF::Renamer;

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
our $VERSION = '0.01';

=head1 NAME

HWRF::Renamer - target executable name generator for HWRF

=head1 SYNOPSIS

  package HWRF::Renamer;
  our @ISA = qw(Exporter HWRF::Renamer);

=head1 DESCRIPTION

The HWRF::Renamer generates a target executable name, given a rule list and 
the name of a source executable.

=head2 EXPORT

None by default.

=cut

our %EXPORT_TAGS = ( 'all' => [ qw(
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

=head2 METHODS

=head3 new

	my $renamer = HWRF::Renamer->new("/path/to/executables.lst");

Instantiates an HWRF::Renamer object.

=cut

sub new {
    my $proto=shift;
    my $class  = ref($proto) || $proto;
    my $rulefile=shift;
    my $self={};


    bless($self, $class);
    __PACKAGE__->_init($self, $rulefile);
    return $self;
}

sub _init {
    my ($proto,$self,$rulefile)=@_;

    open(RULES,"< $rulefile") or croak "$rulefile: cannot open: $!\n";
    my ($pak,$to,$from,$when,$install);
    while(defined($_=<RULES>)) {
        chomp;
        # Skip blank lines and comment lines:
        next if /^\s*(?:\#.*)?$/;

        # Read a line of the form:
        #   (component_name)  (target/exec/hwrf_exename)  (package/bin/name.exe)  (when)
        if(/^\s*(\S+)\s+(\S+)\s+(\S+)\s*$/) {
            ($pak,$from,$to)=($1,$2,$3);

            $self->{$pak}->{$from}=$to;
        } else {
            warn "$rulefile: syntax error: could not parse \"$_\".\n";
        }
    }
    close(RULES);
}

=head3 exes_for

	$c->exes_for($package_name);

The function B<exes_for> will return a list of executables expected to be
produced in the source directory of package $package_name

=cut

sub exes_for {
    my ($self,$pak)=@_;
    my $exes=$self->{$pak};
    if(!ref $exes) {
        return ();
    } else {
        return keys %{$exes};
    }
}

=head3 exes_for

	($to,$install)=$c->exe_for($package_name,$source_exe);

The function B<exes_for> will return the installation location $to of
the executable whose location is $source_exe within package
$package_name.  The $install will contain "yes" if the executable
should be installed and "no" otherwise.

=cut

sub exe_for {
    my ($self,$pak,$from)=@_;
    my $exes=$self->{$pak};
    if(!ref $exes) {
        return undef;
    }
    return $exes->{$from};
}

1;
__END__

