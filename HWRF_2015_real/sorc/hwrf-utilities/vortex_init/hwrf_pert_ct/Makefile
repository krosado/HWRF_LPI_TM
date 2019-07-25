include ../../configure.hwrf
include ../../pure-openmp.inc

OBJS	= hwrf_pert_ct.o correct_mat.o fill_nmm_gridg.o grads.o

EXEC	= ../../exec/hwrf_pert_ct1.exe

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) -o $@ $(OBJS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.exe

