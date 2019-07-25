SHELL=/bin/sh

include ../../configure.hwrf

SRCS=   IJ2LL.f SIG2HYB.f  read_geogrid_heights.f90 \
        LL2pts.f MAKBND.f QFRMTP.f FLIP.f ROTLLE.f \
        COF2GRD.f etaPTS.f SIG2HYB_init.f LL2pts_init.f \
	boundary_smooth.f90 MAKBND_main.f SP_GRID_MODULE.f \
	SPLINE2.f

OBJS=   IJ2LL.o SIG2HYB.o  read_geogrid_heights.o \
        LL2pts.o MAKBND.o QFRMTP.o FLIP.o ROTLLE.o \
        COF2GRD.o etaPTS.o SIG2HYB_init.o LL2pts_init.o \
	boundary_smooth.o MAKBND_main.o SP_GRID_MODULE.o \
	SPLINE2.o

FFLAGS+=$(OMP_FFLAGS)
LDFLAGS+=$(OMP_FFLAGS)

# LIBS		List of libraries
# CMD		Name of the executable
#
#LDFLAGS = -qsmp=noauto
#LDFLAGS =

#CPPFLAGS=-C -P
#CPP=/usr/bin/cpp


# Remember to change the sigio link to /nwprod/lib once the new GFS goes operational
# NOTE: don't include any -L/nwprod/lib or you'll get the wrong version of NetCDF.
#LIBS =          -L/usr/local/lib -lessl -lmass \
#LIBS =          $(WRF_DIR)/external/io_netcdf/wrf_io.o \
#		$(WRF_DIR)/external/io_netcdf/field_routines.o \
#		$(WRF_DIR)/external/io_netcdf/module_wrfsi_static.o \
#		$(WRF_DIR)/external/io_int/module_internal_header_util.o \
#		$(WRF_DIR)/frame/pack_utils.o \
#		$(WRF_DIR)/external/io_int/io_int.o \
#		-L$(WRF_DIR)/external/io_netcdf -lwrfio_nf \
#		$(WRF_DIR)/frame/module_machine.o \
#		$(WRF_DIR)/frame/module_wrf_error.o \
#		-L../../libs -lw3_i4r4 \
#		-L../../libs -lsp_i4r4 \
#                -L$(GSI_DIR)/lib -lsigio 
LIBS =			$(WRF_DIR)/frame/module_internal_header_util.o \
			$(WRF_DIR)/frame/pack_utils.o \
			-L$(WRF_DIR)/external/io_int -lwrfio_int \
			-L$(WRF_DIR)/external/fftpack/fftpack5 -lfftpack \
			-L$(WRF_DIR)/external/io_netcdf -lwrfio_nf \
			-L$(WRF_DIR)/external/RSL_LITE -lrsl_lite \
			-L$(WRF_DIR)/external/io_grib1 -lio_grib1 \
			-L$(WRF_DIR)/external/io_grib_share -lio_grib_share \
			-L$(WRF_DIR)/external/esmf_time_f90 -lesmf_time \
			-L$(WRF_DIR)/external/io_netcdf -lwrfio_nf \
			$(WRF_DIR)/frame/module_machine.o \
                        -L$(WRF_DIR)/external/atm_ocn -latm_ocn \
			-L../../libs -lw3_i4r4 \
			-L../../libs -lsp_i4r4 \
			-L../../libs -lsigio_i4r4 ${ESSLLIB}




# The path to NetCDF below should match the path used when compiling WRF
LIBS2 = 	$(NETCDF_LDFLAGS)

CMD =		../../exec/hwrf_prep.exe
PROFLIB =	#-lprof

# FFLAGS = -O -g -qfullpath -qcheck -qrealsize=4 -qmaxmem=-1 -I$(INCMOD) -qstrict=all
# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

MAKBND.o: SIG2HYB.o SIG2HYB_init.o
SIG2HYB.o SIG2HYB_init.o: read_geogrid_heights.o FLIP.o SPLINE2.o

.f.o:
	$(RM) $@ $*.mod
	$(SFC) $(FFLAGS) -I../../libs/src/sigio -c $< 

.F90.o:
	$(RM) $@ $*.mod
	$(CPP) $(FPPFLAGS) $(FDEFS)  $< > $*.f90
	$(SF90) $(FFLAGS) -c $*.f90 
	$(RM) $*.f90

#LL2pts.o:
#	$(SFC) $(FFLAGS) LL2pts.f -o LL2pts.o -c
#
#LL2pts_init.o:
#	$(SFC) $(FFLAGS) LL2pts_init.f -o LL2pts_init.o -c
#

$(CMD):		$(OBJS)
	$(SFC) $(LDFLAGS) -o $(@)  $(OBJS) -I. $(LIBS) $(LIBS2)

# Make the profiled version of the command and call it a.out.prof
#
#$(CMD).prof:	$(OBJS)
#	$FC) $(LIBS2) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

#ead_geogrid_heights.o
#	$(CPP) $(CPPFLAGS) -I$(WRFINC) $*.f90 > $*.fortran90
#	$(F90)  $(FFLAGS) $*.fortran90 -c -o $*.o

clean:
	rm -f $(OBJS) *.o *.lst *.mod

clobber:	clean
	rm -f $(CMD) $(CMD).prof

MAKBND.o: SP_GRID_MODULE.o
COF2GRD.o: SP_GRID_MODULE.o