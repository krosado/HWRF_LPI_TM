#------------
# Include machine dependent compile & load options
#------------
include ../../configure.pom

LIBS = $(ESSLLIB) -L$(LIB_W3_PATH) -lw3_i4r4 -L$(LIB_SP_PATH) -lsp_i4r4 -L$(LIB_SFCIO_PATH) -lsfcio_i4r4  $(MATHLIB)

FFLAGS+=$(OMP_FFLAGS)
LDFLAGS+=$(OMP_FFLAGS)

INCMOD   =  -I$(LIB_SFCIO_PATH)/src/sfcio

EXEC     =  ../../ocean_exec/gfdl_getsst.exe

SRCS= modules.f drive.f getsm.f gaussg.f ordleg.f avnglatlon.f

OBJS=	${SRCS:.f=.o}

# Lines from here on down should not need to be changed. 
#

.f.o:	$(SRCS)
	$(SFC) $(FFLAGS) -c $<  $(INCMOD)

$(EXEC):	$(OBJS)
	$(SFC) $(LDFLAGS) -o $(EXEC) $(OBJS) $(LIBS)

clean:
	$(RM) $(OBJS) $(EXEC) *.mod
