include ../../../configure.hwrf

LIB_i4r4 = libw3_i4r4.a
LIB_i4r8 = libw3_i4r8.a

.SUFFIXES: .o .f .c
#
# *****************************************************************
#
OBJ_MOD =  bacio_module.o

OBJS	= \
getgb.o     getgbmh.o   putgbex.o   w3fi73.o     baciof.o \
getgb1r.o   getgbmp.o   putgbn.o    w3fi74.o     orders.o \
getgb1re.o  getgbp.o    r63w72.o    w3fi75.o \
getgb1s.o   getgi.o     sbyte.o     w3fi76.o \
getgbe.o    getgir.o    sbytes.o    w3fi82.o \
getgbeh.o   idsdef.o    skgb.o      w3fi83.o \
getgbem.o   iw3jdn.o    w3difdat.o  w3fs26.o \
getgbemh.o  ixgb.o      w3doxdat.o  w3log.o \
getgbemp.o  lengds.o    w3fi01.o    w3movdat.o \
getgbens.o  pdsens.o    w3fi58.o    w3reddat.o \
getgbep.o   pdseup.o    w3fi59.o    w3tagb.o \
errmsg.o    getgbex.o   putgb.o     w3fi63.o    \
gbytes.o                getgbexm.o  putgbe.o    w3fi68.o    \
gbytes_char.o           getgbh.o    putgben.o   w3fi71.o \
getbit.o                getgbm.o    putgbens.o  w3fi72.o \
errexit.o fparsei.o fparser.o instrument.o start.o summary.o w3utcdat.o \
w3fs21.o w3locdat.o w3valdat.o hostname.o c_hostname.o \
isrchne.o gtbits.o

OBJS_CC= bacio.v1.3.o mova2i.o

LIB_W3_PATH=../../

SRC	= $(OBJ_MOD:.o=.f) $(OBJS:.o=.f) $(OBJS_CC:.o=.c)
SRC_F77	= $(OBJ_MOD:.o=.f) $(OBJS:.o=.f)
SRC_CC	= $(OBJS_CC:.o=.c)
#
# *****************************************************************
#
ALL: i4r4 i4r8

.PHONY: ALL i4r4 i4r8

# Bacio should not be in w3lib at all, but if it is, it must be compiled
# with four byte reals, even when the rest of the library has eight
# byte reals.
baciof.o: baciof.f
	$(FC) $(FFLAGS) $(FFLAGS_SINGLE) -c $*.f -o $*.o

bacio_module.o: bacio_module.f
	$(FC) $(FFLAGS) $(FFLAGS_SINGLE) -c $*.f -o $*.o

i4r4:
	$(RM) -f $(OBJ_MOD) $(OBJS) $(OBJS_CC) 
	make -f Makefile FFLAGS="$(FFLAGS) $(FFLAGS_SINGLE)" $(LIB_i4r4)

i4r8:
	$(RM) -f $(OBJ_MOD) $(OBJS) $(OBJS_CC) 
	make -f Makefile FFLAGS="$(FFLAGS) $(FFLAGS_DOUBLE)" $(LIB_i4r8)

$(LIB_i4r4): $(OBJ_MOD) $(OBJS) $(OBJS_CC) 
	$(AR) $(ARFLAGS) $(LIB_i4r4) $(OBJ_MOD) $(OBJS) $(OBJS_CC)
	$(CP) $(LIB_i4r4) $(LIB_W3_PATH)/$(LIB_i4r4)
	$(RM) $(OBJ_MOD) $(OBJS) $(OBJS_CC) *.mod *.a

$(LIB_i4r8): $(OBJ_MOD) $(OBJS) $(OBJS_CC) 
	$(AR) $(ARFLAGS) $(LIB_i4r8) $(OBJ_MOD) $(OBJS) $(OBJS_CC)
	$(CP) $(LIB_i4r8) $(LIB_W3_PATH)/$(LIB_i4r8)
	$(RM) $(OBJ_MOD) $(OBJS) $(OBJS_CC) *.mod *.a

clean:
	$(RM) $(OBJ_MOD) $(OBJS) $(OBJS_CC) *.mod *.a

