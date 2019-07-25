include ../../configure.hwrf

# Must use hwrf-utilities w3 library
LIBS = -L../../libs -lw3_i4r4
EXEC = ../../exec/hwrf_swath.exe
OBJS = create_sw2.o interadsp.o ismin.o

# Lines from here on down should not need to be changed.

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.mod
