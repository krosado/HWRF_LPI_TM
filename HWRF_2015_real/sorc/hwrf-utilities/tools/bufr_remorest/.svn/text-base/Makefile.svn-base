include ../../configure.hwrf

# Must use hwrf-utilities w3 library
LIBS = -L../../libs -lw3_i4r4 -lbufr_i4r4 -lsp_i4r4
EXEC = ../../exec/bufr_remorest.exe
OBJS = remorest.o

# Lines from here on down should not need to be changed.

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.mod
