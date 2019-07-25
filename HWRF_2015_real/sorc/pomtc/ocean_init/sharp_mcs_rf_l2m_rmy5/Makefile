#------------
# Include machine dependent compile & load options
#------------
include ../../configure.pom

LIBS     =  -L$(LIB_W3_PATH) -lw3_i4r8 $(MATHLIB)

EXEC     =  ../../ocean_exec/gfdl_sharp_mcs_rf_l2m_rmy5.exe

$(EXEC): 
	$(SFC) $(FFLAGS) gfdl_sharp_mcs_rf_l2m_rmy5.f -o $(EXEC) $(LIBS)

clean:
	$(RM) $(EXEC) *.o
