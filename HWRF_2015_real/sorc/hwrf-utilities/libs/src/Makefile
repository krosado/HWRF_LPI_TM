SHELL=/bin/sh
# HWRF LIBS makefile

SUBDIRS = bacio blas bufr sfcio sp w3 sigio hwrfutil g2


all: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; make ); \
        done

clean: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
             make $@) ; \
        done
