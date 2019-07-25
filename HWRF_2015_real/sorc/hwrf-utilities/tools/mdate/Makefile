include ../../configure.hwrf

OBJS = mdate.o
EXEC = ../../exec/mdate.exe

# Must use hwrf-utilities w3 library
LIBS  = -L../../libs -lw3_i4r4

# Lines from here on down should not need to be changed.

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.mod
