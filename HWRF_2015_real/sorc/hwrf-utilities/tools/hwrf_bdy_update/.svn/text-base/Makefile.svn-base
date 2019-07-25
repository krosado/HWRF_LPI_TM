SHELL=/bin/sh

include ../../configure.hwrf

SRCS=   readgsifile.F compute_start_and_end.F \
	bdyupdate.F update_wrfnmm.F

OBJS=   readgsifile.o compute_start_and_end.o \
	bdyupdate.o update_wrfnmm.o

INCfram= $(WRF_DIR)/frame 
INCshare= $(WRF_DIR)/share
INCmain=  $(WRF_DIR)/main 
INCinc= $(WRF_DIR)/inc
INCesmf= $(WRF_DIR)/external/esmf_time_f90

# LIBS		List of libraries
LIBS =			-I$(WRF_DIR)/frame \
                        -I$(WRF_DIR)/share \
                        -I$(WRF_DIR)/main \
                        -I$(WRF_DIR)/inc \
                        -L$(WRF_DIR)/main -lwrflib  \
                        ${WRF_DIR}/frame/module_internal_header_util.o \
                        ${WRF_DIR}/frame/pack_utils.o \
                        ${WRF_DIR}/external/esmf_time_f90/module_utility.o \
			$(WRF_GRIB2) $(WRF_PNETCDF)

EXEC =		../../exec/hwrf_bdy_update.exe

FFLAGS_N = -I$(INCfram) -I$(INCshare) -I$(INCmain) -I$(INCinc) -I$(INCesmf)

LDFFLAGS = $(FFLAGS) $(FFLAGS_N)

.F.o:
	$(RM) $@ $*.mod
	$(CPP) $(FPPFLAGS) $(FDEFS)  $< > $*.f90
	$(F90) $(FFLAGS) $(FFLAGS_N) -c $*.f90
	$(RM) $*.f90

$(EXEC):		$(OBJS)
	$(DM_FC) $(DM_LDFLAGS) -o $(@)  $(OBJS) -I. $(LIBS) $(LIB_EXTERNAL) $(NETCDF_LDFLAGS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.mod
