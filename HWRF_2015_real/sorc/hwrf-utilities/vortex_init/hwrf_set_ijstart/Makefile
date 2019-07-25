include ../../configure.hwrf

OBJS	= swcorner_dynamic.o
EXEC	= ../../exec/hwrf_swcorner_dynamic.exe

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) -o $@ $(OBJS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.exe

