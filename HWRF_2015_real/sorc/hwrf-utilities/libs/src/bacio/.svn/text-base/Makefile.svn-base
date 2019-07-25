include ../../../configure.hwrf

LIB = ../../libbacio.a

OBJS = bacio_v1_3.o baciof.o

INCMOD = ../../mods/bacio/

$(LIB): $(OBJS)
	$(AR) $(ARFLAGS) $(LIB) $(OBJS)
	$(MKDIR) $(INCMOD)
	$(CP) *.mod $(INCMOD)

$(OBJS):
	ln -f clib4.h clib.h
	$(SCC) -c bacio_v1_3.c
	$(SFC) $(FFLAGS) -c baciof.f

.f.o:
	$(SFC) $(FFLAGS) $(FFLAGS_SINGLE) -c $<
.c.o:
	$(SCC) $(CFLAGS) $(DEFS) -c $<
.PHONY: clean

clean:
	$(RM) *.o *.mod *.a clib.h

