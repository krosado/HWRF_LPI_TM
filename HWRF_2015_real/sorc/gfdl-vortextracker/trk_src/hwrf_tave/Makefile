include ../../configure.trk

LIBS = -L$(LIB_W3_PATH) -lw3_i4r8  -L$(LIB_BACIO_PATH) -lbacio
EXEC = ../../trk_exec/hwrf_tave.exe

FFLAGS += $(GRIB2_FFLAGS)
LIBS += $(GRIB2_LIBS)

all: $(EXEC)

$(EXEC):      tave.f 
	$(SFC) $(FFLAGS) $(LDFLAGS) tave.f $(LIBS) -o $(EXEC)

.PHONY: clean

clean:
	$(RM) *.o  *.mod
