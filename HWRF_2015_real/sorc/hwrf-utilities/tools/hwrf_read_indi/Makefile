include ../../configure.hwrf

OBJS = read_indi_write_all.o
EXEC = ../../exec/hwrf_read_indi_write_all.exe
LIBS = 

# Lines from here on down should not need to be changed.

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.mod
