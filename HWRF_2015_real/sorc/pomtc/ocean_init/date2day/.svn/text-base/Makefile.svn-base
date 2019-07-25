#------------
# Include machine dependent compile & load options
#------------
include ../../configure.pom

LIBS     =  -L$(LIB_W3_PATH) -lw3_i4r8 $(MATHLIB)
EXEC     =  ../../ocean_exec/gfdl_date2day.exe

$(EXEC):
	$(SFC) $(FFLAGS) date2day.f -o $(EXEC) $(LIBS)

clean:
	$(RM) $(EXEC) *.o
