include ../../configure.hwrf

OBJS = stats.o etime.o
EXEC = ../../exec/hwrf_htcfstats.exe
LIBS =

# Lines from here on down should not need to be changed.
$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.mod
