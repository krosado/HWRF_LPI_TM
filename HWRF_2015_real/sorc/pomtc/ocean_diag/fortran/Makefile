#------------
# Include machine dependent compile & load options
#------------
include ../../configure.pom

LIBS     =  -L$(LIB_W3_PATH) -lw3_i4r8 $(MATHLIB)

EXEC     =  ../../ocean_exec/readsstuvhflux.exe

$(EXEC): 
	$(SFC) $(FFLAGS) $(NETCDFINC) readsstuvhflux.f90 -o $(EXEC) $(LIBS) $(NETCDFLIB)
clean:
	$(RM) $(EXEC) *.o
