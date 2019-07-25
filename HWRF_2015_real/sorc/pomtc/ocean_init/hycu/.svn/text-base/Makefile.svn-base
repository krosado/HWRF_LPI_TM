#------------
# Include machine dependent compile & load options
#------------
include ../../configure.pom

LIBS     =  -L$(LIB_W3_PATH) -lw3_i4r8 $(MATHLIB)

EXEC     =  ../../ocean_exec/pomprep_hycu.xc

$(EXEC): 
	$(SFC) $(FFLAGS) $(NETCDFINC) hycuprep.f90 serftempr.f horinterp.f mixsstz.f sp.f -o $(EXEC) $(LIBS) $(NETCDFLIB)
clean:
	$(RM) $(EXEC) *.o
