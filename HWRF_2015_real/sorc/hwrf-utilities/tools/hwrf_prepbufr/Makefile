include ../../configure.hwrf

OBJS  = rem_specified_TYP_in_circle_prepbufr.o change_prepbufr_qm_in_circle.o  \
        change_prepbufr_qm_typ.o
LIBS  = -L../../libs -lw3_i4r4 -lbufr_i4r4

ALL: hwrf_rem_prepbufr_typ_in_circle.exe hwrf_change_prepbufr_qm_in_circle.exe  \
     hwrf_change_prepbufr_qm_typ.exe

.PHONY: ALL

hwrf_rem_prepbufr_typ_in_circle.exe: rem_specified_TYP_in_circle_prepbufr.o
	$(SFC) $(LDFLAGS) $(FFLAGS) -o $@ rem_specified_TYP_in_circle_prepbufr.o $(LIBS)
	$(CP) *.exe ../../exec
	$(RM) *.exe

hwrf_change_prepbufr_qm_in_circle.exe: change_prepbufr_qm_in_circle.o
	$(SFC) $(LDFLAGS) $(FFLAGS) -o $@ change_prepbufr_qm_in_circle.o $(LIBS)
	$(CP) *.exe ../../exec
	$(RM) *.exe

hwrf_change_prepbufr_qm_typ.exe: change_prepbufr_qm_typ.o
	$(SFC) $(LDFLAGS) $(FFLAGS)-o $@ change_prepbufr_qm_typ.o $(LIBS)
	$(CP) *.exe ../../exec
	$(RM) *.exe

.PHONY: clean

clean: 
	$(RM) $(OBJS) *.exe

