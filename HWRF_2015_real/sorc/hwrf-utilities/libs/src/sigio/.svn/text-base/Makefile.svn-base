
LIB = ../../libsigio_i4r4.a
INCMOD = ../../mods/sigio

include ../../../configure.hwrf

.SUFFIXES: .o .f .F .c

$(LIB): $(LIB)(sigio_module.o)
	$(MKDIR) $(INCMOD)
	$(CP) *.mod $(INCMOD)


.f.a:
	$(F90) -c $(FFLAGS) $(FFLAGS_SINGLE) $<
	$(AR) $(ARFLAGS) $(AFLAGS) $@ $*.o
	$(RM) $*.o

.c.a:
	$(CC) -c $(CFLAGS) $<
	$(AR) $(ARFLAGS)  $(AFLAGS) $@ $*.o
	$(RM) $*.o

clean:
	$(RM) *.o *.mod

