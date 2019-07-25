include ../../configure.hwrf

OBJS =	hwrf_wrfout_newtime.o

LIBS  =  $(NETCDF_LDFLAGS)
EXEC  =  ../../exec/hwrf_wrfout_newtime.exe
CFLAGS= $(NETCDF_INCLUDE)

# Lines from here on down should not need to be changed.

$(EXEC): $(OBJS)
	$(SCC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.mod
