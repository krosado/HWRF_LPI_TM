program products
  use sysutil_module
  use tcf_module
  use out4wave_module
  use vardata_module
  use wrfdiag_module
  use swathgen_module
  implicit none
  integer, parameter :: filenamelen=200
  integer :: max_atcf
  integer, parameter :: maxdom=100
  integer :: parent_nest_ratio(maxdom)
  real :: resolution_cutoffs(maxdom)
  character*(filenamelen) :: outpre, inwrfdiag, inhifreq, inatcf, &
       intcvitals, mdstatus, outlist, htcf_more, out4wave_out
  character*(filenamelen) :: header_coupled,header_uncoupled
  character*(4) :: submodel, model
  type(atcf) :: atcf0
  type(atcf), allocatable :: atcfs(:)
  type(data4wave) :: auxdat
  type(wrfdiag_file) :: wrfdiagdat
  integer :: natcf, nauxphr,nunint, iunit
  real :: last_hr_coupled, dlmd, dphd, clat, clon, wbd, sbd, fcst_len
  real :: htcf_distcutoff,htcf_hourstep
  real :: moad_resolution, final_hour, min_swath_hours
  real :: swath_latres, swath_lonres, swath_latpad, swath_lonpad
  integer :: time_step, time_step_fract_num, time_step_fract_den
  integer :: ide,jde, nesting_level
  integer :: n_dt! number of timesteps in innermost domain (guess)
  logical :: grads_byteswap, realtime
  character(len=filenamelen) :: wrfdiag_parent, wrfdiag_nest
  integer :: ioutlist
  integer, external :: omp_get_max_threads

308 format('Running with ',I0,' OpenMP threads.')
  print 308,omp_get_max_threads()

  call namelist_and_tracks('products.nml','products.nml.out')

  ioutlist=get_unit()
  open(ioutlist,file=trim(outlist),form='FORMATTED')

  call afos_stats_tpc()
  call TierI()
  if(len_trim(inhifreq)>1) then
     call htcf_and_htcfstats()
  endif
  if(len_trim(inwrfdiag)>1) then
     if(final_hour+1e-3>min_swath_hours) then
        write(ioutlist,'(A)') trim(outpre)//'wind10m.ascii'
        write(ioutlist,'(A)') trim(outpre)//'swath.ctl'
        write(ioutlist,'(A)') trim(outpre)//'swath.dat'
        write(ioutlist,'(A)') trim(outpre)//'rainfall.ascii'
        write(ioutlist,'(A)') trim(outpre)//'wind10hrly.ascii'

        ! Generate swath and maybe also generate out4wave files.
        call handle_wrfdiag(nesting_level+1)
     else
        call no_swath()
     endif
  endif
  close(ioutlist)

contains

  subroutine wrfify_filename(inname,outname,idom,includedom)
    character*(filenamelen), intent(in) :: inname
    character*(filenamelen), intent(out) :: outname
    integer, intent(in) :: idom
    logical, optional, intent(out) :: includedom
    integer :: i,l
    character*2 :: domainid
    character*8 :: p='<DOMAIN>'

    outname=' '
    i=index(inname,p)
    if(i==0) then
       outname=inname
       if(present(includedom)) includedom=.false.
    else
       if(present(includedom)) includedom=.true.
       l=len(trim(inname))
101    format("0",I1) 
102    format(I2)
       if(idom<10) then
          write(domainid,101) idom
       else
          write(domainid,102) idom
       endif
       outname=trim(inname(1:i-1) // domainid // inname(i+8:l))
    endif
  end subroutine wrfify_filename

  subroutine handle_wrfdiag(maxfile)
    ! This subroutine generates the swath, and if requested, will also
    ! generate the out4wave files while doing so.  This is done
    ! intelligently: data is only ever read in once, and is not cached
    ! in memory.
    integer, intent(in) :: maxfile
    character*(filenamelen) :: diagnames(maxfile),out4names(maxfile)
    type(data4wave) :: files4wave(maxfile)
    type(wrfdiag_file), target :: parent
    class(wrfdiag_file), pointer :: f
    type(wrfdiag_var) :: u10(maxfile), v10(maxfile), acprec(maxfile)
    character*(8), parameter :: p='<DOMAIN>'
    character*(2) :: domainid
    integer :: nfiles=0, ifile,i, itime,j
    type(swathgen) :: swath
    logical :: make_out4wave, odom,wdom, first
    integer :: lastitime
    character*255 :: message
    make_out4wave = (len_trim(out4wave_out)>1)

    call wrfify_filename(inwrfdiag,diagnames(1),1,wdom)
    ! Note: wdom is .true. if <DOMAIN> is in the basename.  That tells
    ! us we are processing an array of domains.  We will process
    ! everything from the nesting level, up to the nest just below the
    ! MOAD.  There may be only one domain in the array if
    ! nesting_level=1.

    if(make_out4wave) then
       call wrfify_filename(out4wave_out,out4names(1),1,odom)
       if(odom .neqv. wdom) then
          call fail('When specifying requesting out4wave file generation from wrfdiag files, you must include <DOMAIN> in both filenames, or in neither.')
       endif
    endif

    if(.not.wdom) then
       call fail('No <DOMAIN> in the inwrfdiag namelist variable.')
    endif

    call init_swathgen(swath,maxfile)

    if(make_out4wave) then
       if(wdom) then
          ! We don't send the parent domain wrfdiag file to the swath
          ! generator, so we need to open it directly in this
          ! subroutine.
          call init_wrfdiag_file(parent,diagnames(1))
       endif
       call open4wave(files4wave(1),out4names(1),1,parent%nx+1,parent%ny+1)
    endif

    if(wdom) then ! need to generate other nesting levels' filenames
       do i=2,nesting_level+1
          call wrfify_filename(inwrfdiag,diagnames(i),i)
          if(make_out4wave) call wrfify_filename(out4wave_out,out4names(i),i)
          call swath%add_file(diagnames(i))
          lastitime=floor(final_hour/swath%nauxphr)+1
          print *,'lastitime=',final_hour,swath%nauxphr,lastitime
          f=>swath%files(i-1)
          if(make_out4wave) then
             call open4wave(files4wave(i),out4names(i),i,f%nx+1,f%ny+1)
          endif
          call swath%setup_proj(swath_latres, swath_lonres, swath_latpad, &
                                swath_latpad, swath_lonpad, swath_lonpad, &
                                first_time=2, last_time=lastitime)
       enddo
       nfiles=nesting_level+1
    else
       call swath%add_file(inwrfdiag)
       lastitime=floor(final_hour/swath%nauxphr)+1
       print *,'lastitime=',final_hour,swath%nauxphr,lastitime
       call swath%setup_proj(swath_latres, swath_lonres, swath_latpad, &
                             swath_latpad, swath_lonpad, swath_lonpad, &
                             first_time=2, last_time=lastitime)
       nfiles=1
    endif

    first=.true.
    timeloop: do itime=1,swath%ntimes
308    format('Process time ',I0,'...')
       print 308,itime

       if(itime>1 .and. itime<=lastitime) then
          print '(A)','  - Update swath.'
          call swath%interp_time(itime)
       endif

       makewaves: if(make_out4wave) then
          fileloop: do ifile=1,nfiles
309          format('  - Make out4wave file for domain ',I0,'.')
             print 309,ifile
             if(ifile==1 .and. wdom) then
                f=>parent
             elseif(.not.wdom) then
                f=>swath%files(ifile)
             else
                f=>swath%files(ifile-1)
             endif

             call u10(ifile)%open(f,'U10',itime)
             call v10(ifile)%open(f,'V10',itime)
             call acprec(ifile)%open(f,'ACPREC',itime)
             call f%make_latlonrot(itime)

             ! call init_wrfdiag_var(tlow(ifile),f,'TLOW',itime)
             ! if(first) call norot(ifile)%free()
             ! call init_vardata_real(u10rot(ifile),f,'U10',itime,reinit=.not.first)
             ! call init_vardata_real(v10rot(ifile),f,'V10',itime,reinit=.not.first)
             ! call init_proj_nmme(norot,f%eproj(itime),earth_winds=.true.)
             ! call init_bilinear_real(interp(ifile),u10,u10rot)
             ! call interp(ifile)%prep()
             ! call interp(ifile)%hwind(u10rot,v10rot,u10,v10,nomask=.true.)

             !$OMP PARALLEL DO PRIVATE(i,j)
             do j=1,f%ny
                do i=1,f%nx
                   files4wave(ifile)%u10(i,j)=u10(ifile)%rdata(i,j,1)
                   files4wave(ifile)%v10(i,j)=v10(ifile)%rdata(i,j,1)
                   files4wave(ifile)%acprec(i,j)=acprec(ifile)%rdata(i,j,1)
                   !files4wave(ifile)%acprec(i,j)=tlow(ifile)%rdata(i,j,1)
                   files4wave(ifile)%lat(i,j)=f%lats%rdata(i,j,1)
                   files4wave(ifile)%lon(i,j)=f%lons%rdata(i,j,1)
                enddo
             enddo
             !$OMP END PARALLEL DO
             if(files4wave(ifile)%write()/=0) then
319             format(A,': error writing file')
                write(message,319) out4names(ifile)
                call fail(message)
             endif
          end do fileloop
          first=.false.
       elseif(itime>lastitime) then
          print *,'Done processing times.'
          exit timeloop
       end if makewaves
    enddo timeloop

    if(make_out4wave) then
       do ifile=1,nfiles
          call u10(ifile)%free()
          call v10(ifile)%free()
          call acprec(ifile)%free()
          call files4wave(ifile)%close()
       enddo
    end if

    call swath%to_crazy_us_units()
    call swath%write(outpre,atcfs,natcf,grads_byteswap)
    call swath%free()
  end subroutine handle_wrfdiag

  subroutine TierI()
    implicit none
    integer :: inunit,outunit,ierr, L
    character*200 :: filename
    character*1000 :: line

    call TierI_filename(atcf0,filename,model,submodel,realtime)

100 format('TierI filename is "',A,'".')
    write(line,100) trim(filename)
    call warn(line)

    if(trim(filename)==' ') then
       call warn('Somehow ended up with a blank TierI filename.  Will not write any TierI file.')
       return
    else
       write(ioutlist,'(A)') trim(filename)
    endif
    outunit=get_unit()
    open(outunit,file=trim(filename),form='FORMATTED',status='UNKNOWN')

    inunit=get_unit()
    open(inunit,file=trim(inatcf),status='OLD',form='FORMATTED')

    ierr=0
    do while(ierr==0)
       read(inunit,'(A)',iostat=ierr) line
       if(ierr==0) then
          L=len_trim(line)
          if(L>1) then ! skip blank lines
             ! Convert model to the specified one
             if(L>24) line(25:28)=model
             write(outunit,'(A)',iostat=ierr) trim(line)
          endif
       endif
    enddo
    close(inunit)
    close(outunit)
  end subroutine TierI

  ! ------------------------------------------------------------------------

  subroutine no_swath()
    implicit none
    integer :: iunit
    character*200 :: message

    write(ioutlist,'(A)') trim(outpre)//'no.storm'
    write(ioutlist,'(A)') trim(outpre)//'no.stats'

    iunit=get_unit()
    open(iunit,file=trim(outpre)//'no.storm',form='FORMATTED',status='UNKNOWN')
    write(iunit,'(A)') "NO STORM"
    close(iunit)

    message=' '
    write(message,100) max(0,nint(final_hour))
    call warn(message)

    message=' '
    write(message,101)
    call warn(message)

    open(iunit,file=trim(outpre)//'no.stats',form='FORMATTED',status='UNKNOWN')
    write(iunit,102) nint(min_swath_hours)
    close(iunit)

100 format("Storm died at ",I0,"; can't generate swaths for max. wind or precip")
101 format("This program exits without producing swath files")
102 format( "Model run did not produce track beyond ",I0," hours.  No storm bulletin can be generated")
  end subroutine no_swath

  subroutine htcf_and_htcfstats()
    implicit none
    integer :: max_htcf, nhtcf
    integer, parameter :: fudge=100
    type(htcf), allocatable :: htcfs(:)
    character*200 :: message
    max_htcf=n_dt+fudge

    allocate(htcfs(max_htcf))
    print *,'Generating HTCF data from WRF hifreq file',trim(inhifreq)
    call read_htcf(inhifreq,htcfs,max_htcf,nhtcf,model,submodel,atcf0)

    print *,'Write HTCF file ',trim(outpre)//trim(htcf_more)//'htcf'
    write(ioutlist,'(A)') trim(outpre)//trim(htcf_more)//'htcf'
    iunit=get_unit()
    open(iunit,file=trim(outpre)//trim(htcf_more)//'htcf', &
         form='FORMATTED',status='UNKNOWN')
    call write_htcf(iunit,htcfs,nhtcf)
    close(iunit)

    if(natcf>1) then
       if(atcfs(natcf)%fcsthour-1e-3>htcf_hourstep) then
          write(ioutlist,'(A)') trim(outpre)//'resolution'
          write(ioutlist,'(A)') trim(outpre)//trim(htcf_more)//'htcfstats'
          call interp_atcf_to_htcf(htcfs,nhtcf,atcfs,natcf,htcf_distcutoff)
          call htcf_stats(htcfs,nhtcf,atcfs,natcf,htcf_hourstep, &
               trim(outpre)//trim(htcf_more)//'htcfstats', &
               trim(outpre)//'resolution', resolution_cutoffs,nesting_level, &
               moad_resolution,parent_nest_ratio)
       else
2013      format("Need forecasts beyond hour ",I0," to generate htcfstats and resolution files (last hour is ",I0,")")
          write(message,2013) nint(htcf_hourstep),nint(atcfs(natcf)%fcsthour)
          call warn(message)
          call warn('This program exits without producing htcfstats or resolution files.')
       endif
    else
       write(message,'(A)') 'Cannot generate htcfstats or resolution files without at least two atcf times.'
       call warn(message)
       call warn('This program exits without producing htcfstats or resolution files.')
    endif

    deallocate(htcfs)
  end subroutine htcf_and_htcfstats

  ! ------------------------------------------------------------------------

  subroutine namelist_and_tracks(nmlfile,nmloutfile)
    implicit none
    character*(*) :: nmlfile, nmloutfile

    character*(3) :: want_stid
    character*(4) :: want_centername
    integer :: want_ymdh, ierr, nmlin, i, ide_moad,jde_moad, last, pnr, iunit
    real :: fcouple, coupler_dt, grace_period
    real :: dlmd_moad,dphd_moad
    character*200 :: message
    real, parameter :: deg2rad=0.017453292519943295

    outlist='outlist'
    mdstatus='MDstatus'
    intcvitals='in.tcvitals'
    inatcf='in.atcfunix'
    outpre=' '
    htcf_more=' '
    want_stid='12L'
    want_centername='NHC'
    header_coupled='NCEP COUPLED HWRF HURRICANE MODEL'
    header_uncoupled='NCEP UNCOUPLED HWRF HURRICANE MODEL'
    want_ymdh=2012083018
    coupler_dt=540.
    fcst_len=126.0
    grace_period=12.0

    realtime=.False.
    inhifreq='hifreq_d03.htcf'
    submodel='PARA'
    model=' '
    htcf_distcutoff=150000.
    htcf_hourstep=3.0
    time_step_fract_num=0
    time_step_fract_den=1
    time_step=45
    moad_resolution=-99.
    resolution_cutoffs=0.0

    swath_latres=0.05
    swath_lonres=0.05
    swath_latpad=0.3
    swath_lonpad=0.3

    out4wave_out='*'
    inwrfdiag='*'
    ide_moad=216
    jde_moad=432
    dlmd_moad=0.18
    dphd_moad=0.18
    nesting_level=2
    parent_nest_ratio=3
    clat=25.2
    clon=-81.9
    nauxphr=1
    nunint=3
    grads_byteswap=.true.
    min_swath_hours=6.0
    namelist/nhc_products/ intcvitals, inatcf, outpre, want_stid, want_centername, &
         want_ymdh, mdstatus, coupler_dt, fcst_len, grace_period, &
         ide_moad, jde_moad, out4wave_out, dlmd_moad, dphd_moad, clat, clon, &
         nesting_level, parent_nest_ratio, nauxphr,nunint, grads_byteswap, &
         time_step_fract_num, time_step_fract_den, time_step, inhifreq, &
         model, submodel, htcf_distcutoff, htcf_hourstep, &
         header_coupled, header_uncoupled, moad_resolution, resolution_cutoffs, &
         min_swath_hours, outlist, realtime, inwrfdiag, swath_latpad, &
         swath_lonpad, swath_latres, swath_lonres

    if(nesting_level>maxdom-1 .or. nesting_level<0) then
308    format("Invalid nesting_level ",I0,": it must be between ",I0," and ",I0," inclusive.")
       write(message,308) nesting_level,0,maxdom-1
       call fail(message)
    endif

    nmlin=get_unit()
    open(nmlin,file=nmlfile,status='OLD')
    read(nmlin,nml=nhc_products)
    close(nmlin)
    open(nmlin,file=nmloutfile,status='UNKNOWN',delim='APOSTROPHE')
    write(nmlin,nml=nhc_products)
    close(nmlin)

    n_dt=ceiling(fcst_len*3600*time_step_fract_den / &
         (time_step_fract_den*time_step+time_step_fract_num))
    dlmd=dlmd_moad
    dphd=dphd_moad
    ide=ide_moad
    jde=jde_moad
    WBD=-(ide_moad-2)*dlmd_moad   ! MOAD west bound
    SBD=-(jde_moad-2)/2*dphd_moad ! MOAD south bound

    if(moad_resolution<1e-7) then
       ! Calculate the approximate mean point-to-point distance for the MOAD in km:
       moad_resolution = sqrt(dlmd_moad*dlmd_moad + dphd_moad*dphd_moad) * &
            (111.*ide_moad*sqrt(2.) + jde_moad/sqrt(2.)*111.* &
            cos(deg2rad*dlmd_moad*jde_moad/4./sqrt(2.))) / &
            (ide_moad*sqrt(2.) + jde_moad/sqrt(2.))
410    format("No moad_resolution specified.  Guessing ",F0.3,"km from domain information.")
       write(message,410) moad_resolution
       call warn(message)
    endif

    do i=1,nesting_level
       pnr=parent_nest_ratio(i)
       dlmd=dlmd/pnr
       dphd=dphd/pnr
       ide=(ide-1)*pnr+1
       jde=(jde-1)*pnr+1
       n_dt=n_dt*pnr
    enddo
    if(mod(ide,2)==0) then
       ide=ide+1 ! to mimic 2013 HWRF
    endif
    print '("Large fine mesh IDE=",I0," JDE=",I0)',ide,jde

    call read_tcvitals_as_atcf(intcvitals,atcf0,want_stid,'ANY ',want_ymdh)

    if(trim(outpre)==' ') then
       ! Come up with a default outpre like "leslie12l.2012083018."
       ! (note that the trailing "." is part of the name)
105    format(A,A,A,'.',I10,'.')
       write(outpre,105) trim(atcf0%stormname), trim(atcf0%storm2), &
            trim(atcf0%basin1), atcf0%ymdh
       last=len_trim(outpre)

       ! Replace spaces with zeros:
       do i=1,last
          if(outpre(i:i)==' ') outpre(i:i)='0'
       enddo
    endif

    if(trim(htcf_more)==' ') then
111    format('hwrf_d0',I1,'.')
112    format('hwrf_d',I2,'.')
       if(nesting_level+1<10) then
          write(htcf_more,111) nesting_level+1
       else
          write(htcf_more,112) nesting_level+1
       endif
    endif

    iunit=get_unit()
    open(iunit,file=trim(inatcf),status='OLD',form='FORMATTED')
    max_atcf=10
    do
       read(iunit,*,end=300)
       max_atcf=max_atcf+1
    enddo
300 continue
    allocate(atcfs(max_atcf))
    close(iunit)

    call read_atcf(inatcf,atcfs,max_atcf,natcf)
    print '("Read ",I0," ATCF records")',natcf

    if(trim(model) == ' ') then
       if(natcf>0) then
          model=atcfs(1)%model4
       else
          model='HWRF'
       endif
    endif
    print '("Model ",A4," Submodel ",A4)',model,submodel
    if(natcf<1) then
       final_hour=-999.
    else
       final_hour=atcfs(natcf)%fcsthour
    endif

    call parse_MDstatus(trim(mdstatus),fcst_len,last_hr_coupled,coupler_dt,&
         grace_period)
  end subroutine namelist_and_tracks

  ! ------------------------------------------------------------------------

  subroutine afos_stats_tpc()
    character*(filenamelen) :: header
    integer :: iunit

    if(last_hr_coupled>0) then
       header=header_coupled
    else
       header=header_uncoupled
    endif
    iunit=get_unit()

    write(ioutlist,'(A)') trim(outpre)//'stats.short'
    open(iunit,file=trim(outpre)//'stats.short',status='UNKNOWN',form='FORMATTED')
    call write_stats_short(iunit,natcf,atcfs,atcf0)
    close(iunit)

    write(ioutlist,'(A)') trim(outpre)//'stats.tpc'
    open(iunit,file=trim(outpre)//'stats.tpc',status='UNKNOWN',form='FORMATTED')
    call write_header(iunit,atcf0,header)
    write(iunit,*)
    call write_stats_tpc(iunit,natcf,atcfs,atcf0,last_hr_coupled,fcst_len-6.)
    write(iunit,*)
    call write_disclaimer(iunit,atcf0)
    close(iunit)

    write(ioutlist,'(A)') trim(outpre)//'afos'
    open(iunit,file=trim(outpre)//'afos',status='UNKNOWN',form='FORMATTED')
    call write_header(iunit,atcf0,header)
    write(iunit,*)
    call write_disclaimer(iunit,atcf0)
    write(iunit,*)
    write(iunit,*)
    call write_afos(iunit,natcf,atcfs,atcf0)
    close(iunit)
  end subroutine afos_stats_tpc
end program products
