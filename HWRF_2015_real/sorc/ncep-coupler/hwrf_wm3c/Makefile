include ../configure.cpl

SRCS  =	cplmp.comm.f mpi_more.f interp.f wrf_stuff.f cpl3.f

OBJS  =	${SRCS:.f=.o}

EXEC  =	../cpl_exec/hwrf_wm3c.exe

# Lines from here on down should not need to be changed. 

$(EXEC):	$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS)

.f.o:	$(SRCS)
	$(FC) $(FFLAGS) $(FFLAGS_DOUBLE) -c $<

.PHONY: clean
clean:
	$(RM) $(OBJS) *.mod *.exe

