include ../../configure.hwrf

OBJS	= create_trak_fnl.o
EXEC	= ../../exec/hwrf_create_trak_fnl.exe

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) -o $@ $(OBJS) 

.PHONY: clean

clean:
	$(RM) $(OBJS) *.exe

