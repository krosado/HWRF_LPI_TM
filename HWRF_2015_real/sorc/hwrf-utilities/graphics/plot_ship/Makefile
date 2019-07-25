include ../../configure.hwrf

# Fortran pgf90 compiler options
#FFLAGS_SHIPS	= -C -Minform=inform

# Fortran ifort compiler options
#FFLAGS_SHIPS	= -w

## (all)
all: nameparse.x gridparse.x getcenter.x inddiag.x inddiagnull.x totaldiag.x

## (nameparse.x) 
nameparse.x: nameparse.f
	$(SFC) $(FFLAGS_SHIPS) nameparse.f -o nameparse.x
	mv nameparse.x ../../exec/grp_nameparse.exe

## (gridparse.x) 
gridparse.x: gridparse.f
	$(SFC) $(FFLAGS_SHIPS) gridparse.f -o gridparse.x
	mv gridparse.x ../../exec/grp_gridparse.exe

## (totaldiag.x)
totaldiag.x: totaldiag.f diag_util.mod diag_util.o dataio.o upcase.o
	$(SFC) $(FFLAGS_SHIPS) totaldiag.f diag_util.o dataio.o upcase.o -o totaldiag.x
	mv totaldiag.x ../../exec/grp_totaldiag.exe

## (inddiag.x)
inddiag.x: inddiag.f diag_util.mod diag_util.o dataio.o upcase.o lsdiags.o gbland.o
	$(SFC) $(FFLAGS_SHIPS) inddiag.f diag_util.o dataio.o upcase.o lsdiags.o gbland.o -o inddiag.x
	mv inddiag.x ../../exec/grp_inddiag.exe

## (inddiagnull.x)
inddiagnull.x: inddiagnull.f diag_util.mod diag_util.o dataio.o upcase.o
	$(SFC) $(FFLAGS_SHIPS) inddiagnull.f diag_util.o dataio.o upcase.o -o inddiagnull.x
	mv inddiagnull.x ../../exec/grp_inddiagnull.exe

## (getcenter.x)
getcenter.x: getcenter.f diag_util.mod diag_util.o dataio.o upcase.o
	$(SFC) $(FFLAGS_SHIPS) getcenter.f diag_util.o dataio.o upcase.o -o getcenter.x
	mv getcenter.x ../../exec/grp_getcenter.exe

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

clean:
	rm -rf *.o *~ *.mod *.x
