include ../../configure.hwrf

TARGET = ../../exec/hwrf_bin_io.exe

WRFPATH = $(WRF_DIR)
NETCDFPATH = $(NETCDF)

ARCH     = auto
WRFLIBS   = $(WRF_DIR)/frame/pack_utils.o \
	    $(WRF_DIR)/frame/module_internal_header_util.o \
	    $(WRF_DIR)/frame/module_driver_constants.o \
	    $(WRF_DIR)/frame/module_machine.o \
	    $(WRF_DIR)/external/io_netcdf/wrf_io.o  \
	    $(WRF_DIR)/external/io_netcdf/module_wrfsi_static.o   \
	    $(WRF_DIR)/external/io_netcdf/field_routines.o  \
	    $(NETCDF_LDFLAGS) \
	    $(WRFPATH)/main/libwrflib.a \
	    -L$(WRFPATH)/external/io_int/ -lwrfio_int

NCDLIBS = $(NETCDF_LDFLAGS)
NCDFFLAGS = $(NETCDF_INCLUDE)
WRFFFLAGS = -I$(WRFPATH)/external/io_quilt \
	    -I$(WRFPATH)/frame \
	    -I$(WRFPATH)/external/io_int

ESSL     = $(ESSLLIB)
MASS     = $(MASSLIB)

SEARCH   =
#
# Assemble Options
#
FFLAGS += $(NETCDFFLAGS) $(WRFFFLAGS)

LIBS    = $(ESSL) $(MASS) $(NCDLIBS) $(WRFLIBS)
LIBS    += -L../../libs -lw3_i4r4 -lbacio

# Free-form fortran files:
OBJST=	kinds_mod.o \
	mpiio_rw.o


# Fixed-form fortran files:
OBJS=	MASKS_mod.o SOIL_mod.o VRBLS2D_mod.o VRBLS3D_mod.o \
	COLLECT_LOC.o \
	MPI_FIRST.o \
	PARA_RANGE.o \
	SERVER.o  SETUP_SERVERS.o \
        MICROINIT.o GPVS.o hwrf_binary_read_write.o hwrf_bin_main.o

# Includes
INCLUDES= parm.tbl

# Common Blocks
COMMS=	LOOKUP.comm   CTLBLK.comm  \
        GRIDSPEC.comm CMASSI.comm RHGRD.comm

DEPS= $(COMMS) $(INCLUDES)

$(TARGET):	$(OBJST) $(OBJS) $(DEPS)
	$(DM_FC) $(DM_LDFLAGS) -o $@ $(OBJST) $(OBJS) $(LIBS)

clean:
	/bin/rm -f  $(TARGET) *.lst *.o *.mod
#
