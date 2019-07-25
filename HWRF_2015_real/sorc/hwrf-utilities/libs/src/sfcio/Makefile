
LIB = ../../libsfcio_i4r4.a
INCMOD = ../../mods/sfcio

include ../../../configure.hwrf

$(LIB): sfcio_module.f90
	$(RM) sfcio_module.o sfcio_module.mod
	$(F90) $(FFLAGS) -c sfcio_module.f90
	$(AR) $(ARFLAGS) $(AFLAGS) $@ sfcio_module.o
	$(RM) sfcio_module.o
	$(MKDIR) $(INCMOD)
	$(CP) *.mod $(INCMOD)

clean:
	$(RM) *.o

