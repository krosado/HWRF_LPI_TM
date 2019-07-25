include ../../configure.hwrf

OBJS = supvit_modules.o supvit_main.o
EXEC = ../../exec/hwrf_supvit.exe
# Must use hwrf-utilities w3 library
LIBS  = -L../../libs -lw3_i4r8

# Lines from here on down should not need to be changed.

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.mod
