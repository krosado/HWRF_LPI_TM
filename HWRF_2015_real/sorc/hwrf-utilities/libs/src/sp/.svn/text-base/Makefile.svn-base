LIB_i4r4 = libsp_i4r4.a
LIB_i4r8 = libsp_i4r8.a

include ../../../configure.hwrf

.SUFFIXES: .o .f .F


OBJS=bll2ps.o spanaly.o spdz2uv.o speps.o spfft1.o spffte.o spfft.o  \
spfftpt.o spgradq.o spgradx.o spgrady.o splaplac.o splegend.o sppad.o  \
spsynth.o sptezd.o sptez.o sptezmd.o sptezm.o sptezmv.o sptezv.o  \
sptgpmd.o sptgpm.o sptgpmv.o sptgpsd.o sptgps.o sptgpsv.o sptgptd.o  \
sptgpt.o sptgptsd.o sptgptvd.o sptgptv.o sptrand.o sptran.o sptranf0.o  \
sptranf1.o sptranf.o sptranfv.o sptranv.o sptrund.o sptrun.o sptrung.o  \
sptrungv.o sptrunl.o sptrunm.o sptrunmv.o sptruns.o sptrunsv.o  \
sptrunv.o spuv2dz.o spvar.o spwget.o

OBJS1=fftpack.o lapack_gen.o ncpus.o splat.o

FFLAGS+=$(OMP_FFLAGS)
FFLAGS+=$(FFLAGS_SP)
SRC_F77 = $(OBJS:.o=.f)

SRC_F = $(OBJS1:.o=.F)


ALL: $(LIB_i4r4) $(LIB_i4r8)

.PHONY: ALL clean

$(LIB_i4r4): $(OBJS)
	$(FC) $(FFLAGS) $(FFLAGS_SINGLE) -c $(SRC_F) $(SRC_F77)
	$(AR) $(ARFLAGS) $(LIB_i4r4) $(OBJS) $(OBJS1)
	$(CP) $(LIB_i4r4) ../..
	$(RM) $(OBJS) $(OBJS1)  *.mod *.a

$(LIB_i4r8): $(OBJS)
	$(FC) $(FFLAGS) $(FFLAGS_DOUBLE) -c $(SRC_F) $(SRC_F77)
	$(AR) $(ARFLAGS) $(LIB_i4r8) $(OBJS) $(OBJS1)
	$(CP) $(LIB_i4r8) ../..
	$(RM) $(OBJS) $(OBJS1)  *.mod *.a

clean:
	$(RM) $(OBJS) $(OBJS1) *.mod *.a

