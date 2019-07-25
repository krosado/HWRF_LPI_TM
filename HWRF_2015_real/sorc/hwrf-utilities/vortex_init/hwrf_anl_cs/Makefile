include ../../configure.hwrf
include ../../pure-openmp.inc

OBJS	= hwrf_anl_cs_10m.o convert_axi_xy_8.o correct_mat_2.o interp_coef.o

EXEC	= ../../exec/hwrf_anl_cs_10m.exe

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) -o $@ $(OBJS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.exe

