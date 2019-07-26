fc = ifort
fcflags = -O3 -DLINUX -fp-model precise -assume byterecl
fcflag_db = -O0 -g -traceback -check all -fpe0 -ftrapuv -DLINUX -fp-model precise -assume byterecl
#=================================================
srcs = trans2Polar.f interpb_hunt.f findcenter.f gradientw.f hydrostatic.f axisymmet.f polarinfo.f dist2latlon.f twindsum.f trans2Polar_3D.f axisymmet_3D.f
objs = $(srcs:.f=.o)

target = diag.exe

hwrf : $(objs)
	$(fc) $(fcflags) -o $(target) tcdiagnose.f $(objs)
gfdl : $(objs)
	$(fc) $(fcflags) -o $(target) tcdiagnose_gfdl.f $(objs)
radi : $(objs)
	$(fc) $(fcflags) -o radinf.exe radius_info.f $(objs)
merge : $(objs)
	$(fc) $(fcflags) -o merge.exe merge_d23.f
$(objs) : $(srcs)
	$(fc) $(fcflags) -c  $(srcs)
clean :
	rm -f *.o $(target) radinf.exe *.mod

