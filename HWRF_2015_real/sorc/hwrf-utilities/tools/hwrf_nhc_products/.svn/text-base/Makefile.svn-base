include ../../configure.hwrf
include ../../pure-openmp.inc

OBJS = products.o
EXEC = ../../exec/hwrf_nhc_products.exe
LIBS = -L../../libs -lhwrfutil_i4r4 $(NETCDF_LDFLAGS)

.SUFFIXES: # GNU Make bug workaround
.SUFFIXES: .f90 .o

# FFLAGS+=-check all -debug all -traceback

FFLAGS+=-I../../libs/src/hwrfutil $(NETCDF_INCLUDE)

# Lines from here on down should not need to be changed.
all: $(EXEC)
$(EXEC): $(OBJS) ../../libs/libhwrfutil_i4r4.a
	$(SF90) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS)  $(LIBS)

.PHONY: clean

bare: clean
	$(RM) $(EXEC)

clean:
	$(RM) $(OBJS) *.mod *~ a.out
