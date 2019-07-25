#------------
# Include machine dependent compile & load options
#------------
include ../../configure.pom

LIBS     =  -L$(LIB_W3_PATH) -lw3_i4r8 $(MATHLIB)

EXEC     =  ../../ocean_exec/transatl06prep.xc

$(EXEC): 
	$(SFC) $(FFLAGS) transatl06prep.f -o $(EXEC) $(LIBS)
clean:
	$(RM) $(EXEC) *.o
