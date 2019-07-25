include ../../configure.hwrf


OBJS =	grbindex.o

LIBS  = -L../../libs -lw3_i4r4 -lbacio
EXEC  =  ../../exec/grbindex.exe

# Lines from here on down should not need to be changed.

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) $(FFLAGS) -o $(@) grbindex.f $(LIBS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.mod 


