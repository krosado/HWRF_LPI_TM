SHELL=/bin/sh
#
# HWRF graphics makefile 
#
SUBDIRS = plot_track \
	  plot_ship  \
	  plot_swath


all: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; make ); \
	done

clean: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
	     make $@) ; \
	done
