include ../../configure.hwrf
include ../../pure-openmp.inc

.f.o:
	$(RM) $@ $*.mod
	$(FC) $(FFLAGS) $(FFLAGS_DOUBLE) -c $<

SRCS =	modules.f wrf_split_new2.f BOUND.f fft99.f sig_p_convt1.f\
	SEPAR.f WNLIT.f FDUMP.f H12.f I1MACH.f J4SAVE.f XGETUA.f\
	WNLSM.f WNNLS.f XERABT.f XERCTL.f XERPRT.f XERROR.f XERRWV.f \
	XERSAV.f srotm.f srotmg.f amatrix.f rodist.f landcmsk.f\
        create_rel_domain.f

OBJS =	modules.o wrf_split_new2.o BOUND.o fft99.o sig_p_convt1.o\
	SEPAR.o WNLIT.o FDUMP.o H12.o I1MACH.o J4SAVE.o XGETUA.o\
	WNLSM.o WNNLS.o XERABT.o XERCTL.o XERPRT.o XERROR.o XERRWV.o \
	XERSAV.o srotm.o srotmg.o amatrix.o rodist.o landcmsk.o\
        create_rel_domain.o

# Requires either essl or blas library.

LIBS  = -L../../libs -lsp_i4r8 -lw3_i4r8 -lbufr_i4r8 $(ESSLLIB)
EXEC  =  ../../exec/hwrf_split1.exe

# Lines from here on down should not need to be changed.

$(EXEC): $(OBJS)
	$(SFC) $(FFLAGS) $(FFLAGS_DOUBLE) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.mod *.exe


