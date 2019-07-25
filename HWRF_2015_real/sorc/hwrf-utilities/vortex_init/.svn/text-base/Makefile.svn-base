SHELL=/bin/sh
# HWRF Vortex Init makefile

SUBDIRS = hwrf_anl_bogus hwrf_anl_cs hwrf_anl_step2 \
	  hwrf_create_trak_fnl hwrf_create_trak_guess hwrf_pert_ct \
	  hwrf_split interpolate merge_nest hwrf_set_ijstart hwrf_diffwrf_3dvar


all: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; make ); \
	done

clean: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
	     make $@) ; \
	done
