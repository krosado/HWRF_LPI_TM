include ../../configure.hwrf
include ../../pure-openmp.inc

OBJS	= hwrf_anl_4x_step2.o Split_Sym_Asy.o correct_mat_1.o interp_coef.o \
	  fill_nmm_gridg.o grads.o

EXEC	= ../../exec/hwrf_anl_4x_step2.exe

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) -o $@ $(OBJS) 

.PHONY: clean

clean:
	$(RM) $(OBJS) *.exe

