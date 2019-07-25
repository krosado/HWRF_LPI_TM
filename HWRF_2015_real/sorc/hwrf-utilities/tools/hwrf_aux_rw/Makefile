include ../../configure.hwrf

OBJS = hwrf_simple_aux_rw.o \
       var_summary.o \
       wrf_lookalikes.o

EXEC = ../../exec/hwrf_aux_rw.exe

WRF_LIB         = \
 $(WRF_DIR)/frame/module_machine.o \
 $(WRF_DIR)/frame/module_internal_header_util.o \
 $(WRF_DIR)/frame/pack_utils.o \
 -L$(WRF_DIR)/external/io_int -lwrfio_int \
 -L$(WRF_DIR)/external/fftpack/fftpack5 -lfftpack \
 -L$(WRF_DIR)/external/io_netcdf -lwrfio_nf \
 -L$(WRF_DIR)/external/RSL_LITE -lrsl_lite \
 -L$(WRF_DIR)/external/io_grib1 -lio_grib1 \
 -L$(WRF_DIR)/external/io_grib_share -lio_grib_share \
 -L$(WRF_DIR)/external/esmf_time_f90 -lesmf_time \
 -L$(WRF_DIR)/external/io_netcdf -lwrfio_nf \
 -L$(WRF_DIR)/external/atm_ocn -latm_ocn

LIBS = $(WRF_LIB) $(NETCDF_LDFLAGS)

# Lines from here on down should not need to be changed.

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.mod
