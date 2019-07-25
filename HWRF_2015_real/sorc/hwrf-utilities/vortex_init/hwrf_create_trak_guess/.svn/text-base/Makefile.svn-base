include ../../configure.hwrf

OBJS	= create_trak_guess.o
EXEC	= ../../exec/hwrf_create_trak_guess.exe

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) -o $@ $(OBJS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.exe

