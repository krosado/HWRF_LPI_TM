include ../../configure.trk

LIBS = -L$(LIB_W3_PATH) -lw3_i4r8  -L$(LIB_BACIO_PATH) -lbacio
OBJS = gettrk_modules.o module_waitfor.o cwaitfor.o # all non-main objects
EXEC = ../../trk_exec/hwrf_gettrk.exe

FFLAGS += $(GRIB2_FFLAGS)
LIBS += $(GRIB2_LIBS)

all: $(EXEC)

$(EXEC):      gettrk_main.f $(OBJS) 
	$(SFC) $(FFLAGS) $(LDFLAGS) $(OBJS) gettrk_main.f $(LIBS) -o $(EXEC)

.PHONY: clean

clean:
	$(RM) *.o  *.mod
