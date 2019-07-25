include ../../configure.hwrf
include ../../pure-openmp.inc

OBJS	= merge_nest_4x_step12_3n.o interp_coef.o

EXEC	= ../../exec/hwrf_merge_nest_4x_step12_3n.exe

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) -o $@ $(OBJS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.exe

