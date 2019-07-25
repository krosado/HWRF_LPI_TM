include ../../configure.hwrf

# Need to define $WRF_DIR and $NETCDF
#
#
OBJS	= wrf_nmm_netcdf_grads_ijk.o
EXEC	= ../../exec/hwrf_netcdf_grads.exe
LIBS    = $(WRF_DIR)/frame/pack_utils.o $(WRF_DIR)/frame/module_internal_header_util.o \
          $(WRF_DIR)/frame/module_driver_constants.o $(WRF_DIR)/frame/module_machine.o \
          $(WRF_DIR)/external/io_netcdf/wrf_io.o  \
          $(WRF_DIR)/external/io_netcdf/module_wrfsi_static.o   \
          $(WRF_DIR)/external/io_netcdf/field_routines.o  \
	$(NETCDF_LDFLAGS) 

INC     = $(NETCDF_INCLUDE) -I$(WRF_DIR)/external/ioapi_share \
          -I$(WRF_DIR)/external/io_netcdf

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

$(OBJS):  wrf_nmm_netcdf_grads_ijk.f90
	$(F90) $(FFLAGS) -c wrf_nmm_netcdf_grads_ijk.f90 $(INC)

.PHONY: clean

clean:
	$(RM) $(OBJS) $(EXEC) *.mod

