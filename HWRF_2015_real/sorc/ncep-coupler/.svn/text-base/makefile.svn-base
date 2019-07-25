SHELL=/bin/sh
# NCEP Coupler makefile
#------------
# Include machine dependent compile & load options
#------------
include ./configure.cpl

SUBDIRS = hwrf_wm3c

all: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
                make FC="$(DM_FC)" F90="$(DM_F90)"  \
		FFLAGS="$(FFLAGS)" CC="$(SCC)" CFLAGS="$(CFLAGS)" CFLAGS2="$(CFLAGS2)"\
		CPPFLAGS="$(CPPFLAGS)" CPP="$(CPP)" ); \
        done

clean: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
             make $@ ); \
        done
