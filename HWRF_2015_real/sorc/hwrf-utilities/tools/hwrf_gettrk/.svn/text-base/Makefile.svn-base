include ../../configure.hwrf

EXEC=../../exec/hwrf_old_gettrk.exe
FFLAGS = $(FFLAGS_TRACKER)
OBJS = gettrk_main.o gettrk_modules.o
LIBS  = -L../../libs -lw3_i4r8 

# Lines from here on down should not need to be changed.

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.mod

gettrk_main.o: gettrk_modules.o
