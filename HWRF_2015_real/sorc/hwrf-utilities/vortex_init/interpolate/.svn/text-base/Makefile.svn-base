include ../../configure.hwrf
include ../../pure-openmp.inc

OBJS	= hwrf_inter_2to1.o hwrf_inter_2to6_R.o hwrf_inter_4to2.o  \
          hwrf_inter_4to6_R.o hwrf_inter_2to2_R.o interp_coef.o \
          fill_nmm_gridg.o unfill_nmm_gridg.o grads.o 

ALL: hwrf_inter_2to1.exe hwrf_inter_2to6.exe hwrf_inter_4to2.exe hwrf_inter_4to6.exe \
     hwrf_inter_2to2.exe

.PHONY: ALL

hwrf_inter_2to1.exe: hwrf_inter_2to1.o interp_coef.o
	$(SFC) $(LDFLAGS) -o $@ hwrf_inter_2to1.o interp_coef.o
	$(CP) *.exe ../../exec
	$(RM) *.exe

hwrf_inter_2to6.exe: hwrf_inter_2to6_R.o interp_coef.o fill_nmm_gridg.o grads.o unfill_nmm_gridg.o
	$(SFC) $(LDFLAGS) -o $@ hwrf_inter_2to6_R.o interp_coef.o fill_nmm_gridg.o grads.o unfill_nmm_gridg.o
	$(CP) *.exe ../../exec
	$(RM) *.exe

hwrf_inter_2to2.exe: hwrf_inter_2to2_R.o interp_coef.o
	$(SFC) $(LDFLAGS) -o $@ hwrf_inter_2to2_R.o interp_coef.o
	$(CP) *.exe ../../exec
	$(RM) *.exe

hwrf_inter_4to2.exe: hwrf_inter_4to2.o interp_coef.o
	$(SFC) $(LDFLAGS) -o $@ hwrf_inter_4to2.o interp_coef.o
	$(CP) *.exe ../../exec
	$(RM) *.exe

hwrf_inter_4to6.exe: hwrf_inter_4to6_R.o interp_coef.o
	$(SFC) $(LDFLAGS) -o $@ hwrf_inter_4to6_R.o interp_coef.o
	$(CP) *.exe ../../exec
	$(RM) *.exe

.PHONY: clean

clean: 
	$(RM) $(OBJS) *.exe

