SHELL=/bin/sh
# Makefile for diagnostic file generation. 
# 
# Version 1.1, last modified 02/11/2011
# This version (v1.0) is for HWRF reruns.
# Version 2.1, last modified 12/31/2012
#
# make/make all   - compile all associated fortran programs
# make clean      - removes all intermediate files (does not remove *.x)
# make lstest.exe - makes only test program for diagnostic calculation

include ../../configure.hwrf

# Fortran compiler options

## (all)
all: nameparse.exe gridparse.exe getcenter.exe inddiag.exe inddiagnull.exe totaldiag.exe move

## (nameparse.exe) 
nameparse.exe: nameparse.f
	$(SFC) $(FFLAGS_SHIPS) nameparse.f -o nameparse.exe

## (gridparse.exe) 
gridparse.exe: gridparse.f
	$(SFC) $(FFLAGS_SHIPS) gridparse.f -o gridparse.exe

## (totaldiag.exe)
totaldiag.exe: totaldiag.f diag_util.mod diag_util.o dataio.o upcase.o
	$(SFC) $(FFLAGS_SHIPS) totaldiag.f diag_util.o dataio.o upcase.o -o totaldiag.exe

## (inddiag.exe)
inddiag.exe: inddiag.f diag_util.mod diag_util.o dataio.o upcase.o lsdiags.o gbland.o
	$(SFC) $(FFLAGS_SHIPS) inddiag.f diag_util.o dataio.o upcase.o lsdiags.o gbland.o -o inddiag.exe

## (inddiagnull.exe)
inddiagnull.exe: inddiagnull.f diag_util.mod diag_util.o dataio.o upcase.o
	$(SFC) $(FFLAGS_SHIPS) inddiagnull.f diag_util.o dataio.o upcase.o -o inddiagnull.exe

## (getcenter.exe)
getcenter.exe: getcenter.f diag_util.mod diag_util.o dataio.o upcase.o
	$(SFC) $(FFLAGS_SHIPS) getcenter.f diag_util.o dataio.o upcase.o -o getcenter.exe

lsdiags.o: lsdiags.f diag_util.mod diag_util.o gbland.o
	$(SFC) $(FFLAGS_SHIPS) -c lsdiags.f diag_util.o gbland.o

diag_util.mod: diag_util.f dataio.o
	$(SFC) $(FFLAGS_SHIPS) -c diag_util.f dataio.o

diag_util.o: diag_util.f dataio.o
	$(SFC) $(FFLAGS_SHIPS) -c diag_util.f dataio.o

dataio.o: dataformats.inc dataioparms.inc dataio.f
	$(SFC) $(FFLAGS_SHIPS) -c dataio.f

gbland.o: gbland.f
	$(SFC) $(FFLAGS_SHIPS) -c gbland.f

upcase.o: upcase.f
	$(SFC) $(FFLAGS_SHIPS) -c upcase.f

move: 
	cp *.exe ../../exec
   
clean:
	rm -rf *.o *~ *.mod *.exe
