include ../../configure.hwrf

EXEC=../../exec/hwrf_metgrid_levels.exe
OBJS=hwrf_metgrid_levels.c
LIBS=$(NETCDF_INCLUDE) $(NETCDF_LDFLAGS)

$(EXEC): $(OBJS)
	$(SCC) $(LDFLAGS) $(CFLAGS) -o $(EXEC) $(OBJS) $(LIBS)

.PHONY: clean

clean:
	rm -f *~ *.o a.out \#*\#