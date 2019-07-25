include ../../configure.hwrf

OBJS = atcf_to_stats.o

LIBS = 
EXEC = ../../exec/hwrf_atcf_to_stats.exe

# Lines from here on down should not need to be changed.

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.mod
