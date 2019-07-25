include ../../configure.hwrf

OBJS = sample.o
EXEC = ../../exec/wave_sample.exe
LIBS = -L../../libs -lhwrfutil_i4r4 -lnetcdf
FFLAGS += $(NETCDF_FFLAGS) -I../../libs/src/hwrfutil $(OMP_FFLAGS)
#FFLAGS+=-g -O0 -check all -debug all -traceback
LDFLAGS += $(NETCDF_LDFLAGS) $(OMP_FFLAGS)

F90=$(SFC) 
CC=$(SCC)

.SUFFIXES: # GNU Make bug workaround
.SUFFIXES: .f90 .o

# Lines from here on down should not need to be changed.
all: $(EXEC)
$(EXEC): $(OBJS) ../../libs/libhwrfutil_i4r4.a
	$(SF90) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS)  $(LIBS)

.PHONY: clean

bare: clean
	$(RM) $(EXEC)

clean:
	$(RM) $(OBJS) *.mod *~ a.out
