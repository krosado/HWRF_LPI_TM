#------------
# Include machine dependent compile & load options
#------------
include ../../configure.pom

LIBS     =  -L$(LIB_W3_PATH) -lw3_i4r8  $(MATHLIB)
EXEC     =  ../../ocean_exec/gfdl_day2date.exe


$(EXEC):
	$(SFC) $(FFLAGS) day2date.f -o $(EXEC) $(LIBS)

clean:
	$(RM) $(EXEC) *.o
