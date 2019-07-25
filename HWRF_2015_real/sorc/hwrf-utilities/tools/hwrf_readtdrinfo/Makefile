include ../../configure.hwrf

OBJS =  readtdrstmid.o readtdrtime.o readtdrtrigger.o readtdrtrack.o

LIBS  = -L../../libs -lw3_i4r8 -lbufr_i4r8

ALL: hwrf_readtdrstmid.exe hwrf_readtdrtime.exe hwrf_readtdrtrigger.exe \
     hwrf_readtdrtrack.exe

.PHONY: ALL

hwrf_readtdrstmid.exe: readtdrstmid.o
	$(SFC) $(LDFLAGS) $(FFLAGS_BUFR) -o $(@) readtdrstmid.o $(LIBS)
	$(CP) *.exe ../../exec
	$(RM) *.exe

hwrf_readtdrtime.exe: readtdrtime.o
	$(SFC) $(LDFLAGS) $(FFLAGS_BUFR) -o $(@) readtdrtime.o $(LIBS)
	$(CP) *.exe ../../exec
	$(RM) *.exe

hwrf_readtdrtrigger.exe: readtdrtrigger.o
	$(SFC) $(LDFLAGS) $(FFLAGS_BUFR) -o $(@) readtdrtrigger.o $(LIBS)
	$(CP) *.exe ../../exec
	$(RM) *.exe

hwrf_readtdrtrack.exe: readtdrtrack.o
	$(SFC) $(LDFLAGS) $(FFLAGS_BUFR) -o $(@) readtdrtrack.o $(LIBS)
	$(CP) *.exe ../../exec
	$(RM) *.exe

.PHONY: clean

clean:
	$(RM) $(OBJS) *.exe
