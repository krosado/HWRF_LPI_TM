include ../../configure.hwrf
include ../../pure-openmp.inc

OBJS	= hwrf_blend_gsi.o

EXEC	= ../../exec/hwrf_blend_gsi.exe

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) -o $@ $(OBJS) 

.PHONY: clean

clean:
	$(RM) $(OBJS) *.exe

