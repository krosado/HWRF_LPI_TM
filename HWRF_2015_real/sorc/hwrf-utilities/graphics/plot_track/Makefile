include ../../configure.hwrf
OBJ1	= statsin_domain_TI.o
OBJ2	= hwrf_atcf_tracks.o
all: hwrf_atcf_tracks.exe statsin_domain_TI.exe \
     hwrf_atcf_intensity.exe statsin_domain.exe 

hwrf_atcf_tracks.exe: hwrf_atcf_tracks.f90
	$(SFC)  hwrf_atcf_tracks.f90 -o hwrf_atcf_tracks.exe
	mv hwrf_atcf_tracks.exe ../../exec/grp_hwrf_atcf_tracks.exe

hwrf_atcf_intensity.exe: hwrf_atcf_intensity.f90
	$(SFC)  hwrf_atcf_intensity.f90 -o hwrf_atcf_intensity.exe
	mv hwrf_atcf_intensity.exe ../../exec/grp_hwrf_atcf_intensity.exe

statsin_domain_TI.exe: statsin_domain_TI.f90
	$(SFC) statsin_domain_TI.f90 -o statsin_domain_TI.exe
	mv statsin_domain_TI.exe ../../exec/grp_statsin_domain_TI.exe

statsin_domain.exe: statsin_domain.f90
	$(SFC) statsin_domain.f90 -o statsin_domain.exe
	mv statsin_domain.exe ../../exec/grp_statsin_domain.exe 

.PHONY: clean

clean:
	$(RM) *.o *.exe 

