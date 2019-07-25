SHELL=/bin/sh
# POM Diag makefile

SUBDIRS = fortran

all: $(SUBDIRS)


$(SUBDIRS)::
	cd $@ ; make

clean:
	echo "subdirs is: $(SUBDIRS)"
	for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
             make $@ ) ; \
        done
