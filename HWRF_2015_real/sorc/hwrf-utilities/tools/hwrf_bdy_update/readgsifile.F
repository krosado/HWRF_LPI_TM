        SUBROUTINE readgsifile (filename,BDIM,BDIM_V,KDIM &
                               ,PD,T,Q,CWM,U,V )

        character(*) :: filename
        integer :: BDIM, BDIM_V, KDIM
        
        integer :: iyear,imn,idy,ihr,imin,isec,bdima

        real, intent(out):: PD(BDIM), T(BDIM,KDIM), Q(BDIM,KDIM), CWM(BDIM,KDIM)  &
                          ,U(BDIM_V,KDIM),V(BDIM_V,KDIM)

! add core as a switch in calling list for this routine?

	write(0,*) 'know that filename is: ', trim(filename)
	write(0,*) 'BDIM,BDIM_V,KDIM in: ', BDIM,BDIM_V,KDIM
        open (unit=27,file=trim(filename),form='unformatted')
	read(27)  ! 'WRF-NMM-BINARY'
        read(27) iyear,imn,idy,ihr,imin,isec,IDIM,JDIM,KDIM


	write(0,*) 'IDIM, JDIM, BDIM: ', IDIM, JDIM, BDIM

        read(27) PD,T,Q,CWM,U,V  ! first is guess
        read(27) PD,T,Q,CWM,U,V  ! second is updated analysis fields
        close(unit=27)

	write(0,*) 'min,max PD=',minval(PD),maxval(PD)
	write(0,*) 'min,max T=',minval(T),maxval(T)
	write(0,*) 'min,max Q=',minval(Q),maxval(Q)
	write(0,*) 'min,max CWM=',minval(CWM),maxval(CWM)
	write(0,*) 'min,max Q=',minval(U),maxval(U)
	write(0,*) 'min,max Q=',minval(V),maxval(V)

        END SUBROUTINE readgsifile
