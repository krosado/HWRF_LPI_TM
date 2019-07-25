include ../../configure.hwrf

OBJS = combinetrack.o
EXEC = ../../exec/hwrf_combinetrack.exe
LIBS  = -L../../libs -lw3_i4r4

# Lines from here on down should not need to be changed.
$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.mod
