MAKEFLAGS += -r
.SUFFIXES: # workaround for GNU Make bug
.SUFFIXES: .f90 .c .o

include ../../../configure.hwrf

FFLAGS+=-I. $(NETCDF_INCLUDE)
LDFLAGS+=$(NETCDF_LDFLAGS) $(NETCDF_INCLUDE)

include ../../../pure-openmp.inc

# FFLAGS+=-O0 -check all -debug all -traceback

LIB =  ../../libhwrfutil_i4r4.a
OBJS =  constants_module.o \
        decomp_module.o \
        etime.o \
        interp_module.o \
        out4wave_module.o \
        projection_module.o \
        swathgen_module.o \
        sysutil_module.o \
        tcf_module.o \
        vardata_module.o \
        wrfdiag_module.o
INCMOD = ../../mods/hwrfutil

all: $(LIB)

clean:
	$(RM) *.o *~ \#*

bare: clean
	$(RM) *.mod $(LIB)

$(LIB): $(OBJS)
	$(AR) $(ARFLAGS) $(AFLAGS) $(LIB) $(OBJS)
	$(MKDIR) $(INCMOD)
	$(CP) *.mod $(INCMOD)

.f90.o:
	$(F90) -c $(FFLAGS) $(FFLAGS_SINGLE) $<

.c.o:
	$(CC) -c $(CFLAGS) $<

wrfdiag_module.o: projection_module.o vardata_module.o decomp_module.o \
	sysutil_module.o tcf_module.o etime.o
swathgen_module.o: projection_module.o vardata_module.o decomp_module.o \
	interp_module.o wrfdiag_module.o sysutil_module.o
projection_module.o: constants_module.o sysutil_module.o
vardata_module.o: decomp_module.o projection_module.o sysutil_module.o
interp_module.o: vardata_module.o decomp_module.o projection_module.o sysutil_module.o
out4wave_module.o: sysutil_module.o tcf_module.o
tcf_module.o: sysutil_module.o
decomp_module.o: sysutil_module.o
