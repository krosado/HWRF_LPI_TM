SHELL=/bin/sh
# Vortex Tracker makefile
#------------
# Include machine dependent compile & load options
#------------

SUBDIRS = hwrf_gettrk hwrf_vint hwrf_tave

all: $(SUBDIRS)

$(SUBDIRS)::
	cd $@ ; make

clean::
	@for dir in $(SUBDIRS); do                                       \
           ( cd $$dir; echo "Making $@ in `pwd`" ;                       \
             $(MAKE) $@ );                                               \
        done

