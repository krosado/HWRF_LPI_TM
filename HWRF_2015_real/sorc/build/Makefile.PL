use 5.008002;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'HWRF',
    VERSION_FROM      => 'lib/HWRF.pm', # finds $VERSION
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM  => 'lib/HWRF.pm', # retrieve abstract from module
       AUTHOR         => 'Timothy P Brown <Timothy.P.Brown@noaa.gov>') : ()),
    EXE_FILES         => ['bin/build_HWRF',
			  'bin/install_HWRF',
			  'bin/cmp_HWRF',
			  'bin/run_HWRF',],
    PREREQ_PM         => {
	 'Carp'                 => 0,
	 'Cwd'                  => 0,
	 'FindBin'              => 0,
	 'File::Basename'       => 0,
	 'File::Copy'           => 0,
	 'File::Path'           => 0,
	 'IO::Handle'           => 0,
	 'IO::Select'           => 0,
	 'IPC::Open3'           => 0,
	 'POSIX'                => 0,
	 'Symbol'               => 0,
    },
    LIBS              => [""],
    INC               => "",
    DEFINE            => "",
);

