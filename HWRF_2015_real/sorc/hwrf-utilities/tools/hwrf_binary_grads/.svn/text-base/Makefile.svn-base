include ../../configure.hwrf

# Need to define $WRF_DIR and $NETCDF
#
#
OBJS	= wrf_nmm_binary_grads_ijk.o
EXEC	= ../../exec/hwrf_binary_grads.exe

LIBwrfio_int    = $(WRF_DIR)/external/io_int/libwrfio_int.a
LIBwrfio_netcdf = $(WRF_DIR)/external/io_netcdf/libwrfio_nf.a
OBJwrf_frame_pk = $(WRF_DIR)/frame/pack_utils.o

WRFLIB = $(LIBwrfio_int) $(LIBwrfio_netcdf) $(OBJwrf_frame_pk)

LIBS    = $(WRFLIB)

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

$(OBJS):  wrf_nmm_binary_grads_ijk.f90
	$(F90) $(FFLAGS) -c wrf_nmm_binary_grads_ijk.f90

.PHONY: clean

clean:
	$(RM) $(OBJS) $(EXEC) *.mod

