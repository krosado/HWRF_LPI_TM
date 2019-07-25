SHELL=/bin/sh
# POM Init makefile

SUBDIRS = date2day day2date getsst sharp_mcs_rf_l2m_rmy5 \
	  fbtr gdm3 idel ncda hycu pom tran

all: $(SUBDIRS)

$(SUBDIRS)::
	cd $@ ; make

clean::
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
	     make $@ ) ; \
	done
