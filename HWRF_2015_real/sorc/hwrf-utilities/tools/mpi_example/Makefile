SHELL=/bin/sh

include ../../configure.hwrf

F90=$(DM_F90)
OBJS=mpi_example.o
EXE=../../exec/mpi_example.exe

$(EXE): $(OBJS)
	$(F90) -o $(EXE) $(OBJS)

clean: 
	rm -f $(OBJS) $(EXE) *~