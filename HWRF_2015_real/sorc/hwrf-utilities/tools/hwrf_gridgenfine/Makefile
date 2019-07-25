include ../../configure.hwrf

OBJS=gridgenfineparent.o

LIBS =          
EXEC = ../../exec/hwrf_gridgenfine.exe

# Lines from here on down should not need to be changed.

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.mod
