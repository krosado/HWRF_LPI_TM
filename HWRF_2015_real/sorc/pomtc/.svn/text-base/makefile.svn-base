SHELL=/bin/sh
# POM makefile
#------------
# Include machine dependent compile & load options
#------------
include ./configure.pom

SUBDIRS = ocean_init ocean_main ocean_diag

all: $(SUBDIRS)

$(SUBDIRS)::
	cd $@ && $(MAKE)


clean::
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
             $(MAKE) $@ ); \
        done
