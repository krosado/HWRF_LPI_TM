include ../../configure.hwrf
all: atcf_to_stats.exe

atcf_to_stats.exe: atcf_to_stats.f90
	$(SFC)  atcf_to_stats.f90 -o atcf_to_stats.exe
	mv atcf_to_stats.exe ../../exec/grp_atcf_to_stats.exe

.PHONY: clean

clean:
	$(RM) *.o *.exe 

