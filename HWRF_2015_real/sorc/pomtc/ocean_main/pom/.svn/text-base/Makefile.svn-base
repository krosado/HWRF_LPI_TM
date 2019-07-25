#
# POM makefile
#
include ../../configure.pom
#-----------------------------------------------------------------------
# Settings that depend on the system and the compiler
#-----------------------------------------------------------------------
# Set macros
# Set libraries and include files
NETCDFINC = -I$(PNETCDF)/include
NETCDFLIB = -L$(PNETCDF)/lib
LIBS = $(NETCDFLIB) -lpnetcdf

#-----------------------------------------------------------------------
# Set the executable
#-----------------------------------------------------------------------
BIN = ../../ocean_exec/hwrf_ocean_fcst.exe


#-----------------------------------------------------------------------
# Define source directory
#-----------------------------------------------------------------------
SRCDIR = .

#-----------------------------------------------------------------------
# Define objects
#-----------------------------------------------------------------------
OBJS = cmp.comm.o       \
       pom.comm.o       \
       mpi_more.o       \
       parallel_mpi.o   \
       io_pnetcdf.o     \
       solver.o         \
       bounds_forcing.o \
       advance.o        \
       initialize.o     \
       ocm_atm.o        \
       drive_ocean.o    \
       make_om_grid.o   \
       mp_arr_pros_om.o \
       hurricane_wind.o \
       smoothing.o      \
       make_om_msk.o    \
       atm_ocm.o        \
       geovel.o         \
       calcpwave.o
VPATH = $(SRCDIR)

#-----------------------------------------------------------------------
# Set implicit rules for compilation
#-----------------------------------------------------------------------
.f.o:
	@echo
	$(DM_FC) -c $(FFLAGS_MPI_POM) $(NETCDFINC) $<

#-----------------------------------------------------------------------
# Set implicit rules for dependencies
#-----------------------------------------------------------------------
.F.o:
	@echo
	$(DM_FC) -c $(FFLAGS_MPI_POM) $(NETCDFINC) $<

#-----------------------------------------------------------------------
# Create the executable
#-----------------------------------------------------------------------
$(BIN): $(OBJS)
	@echo
	$(DM_FC) $(LDFLAGS) $(FFLAGS_MPI_POM) $(NETCDFINC) -o $(BIN) $(OBJS) $(LIBS)

#-----------------------------------------------------------------------
# Cleaning target
#-----------------------------------------------------------------------
clean:
	@rm -f *.o *.mod
