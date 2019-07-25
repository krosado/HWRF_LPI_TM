module vardata_module
  use sysutil_module, only: fail, warn
  use projection_module
  use decomp_module
  implicit none

  private

  public :: vardata, init_vardata, free_vardata
  public :: vardata_real, init_vardata_real, free_vardata_real, &
       alloc_vardata_real, initdat_vardata_real, full_init_vardata_real
  public :: vardata_int, init_vardata_int, free_vardata_int, &
       alloc_vardata_int, initdat_vardata_int

  type vardata
     class(projection), pointer :: pj=>NULL() ! projection for this variable
     class(decomp), pointer :: dc=>NULL()     ! grid decomposition of the variable
     logical, pointer :: mask(:,:,:)=>NULL()  ! mask: optional, allocated by alloc_mask
     logical :: ownmask = .true.              ! am I responsible for deallocating mask?
   contains
     procedure alloc => alloc_vardata
     procedure getmask => getmask_vardata
     procedure getreal => getreal_vardata
     procedure getint => getint_vardata
     procedure alloc_mask => alloc_mask_vardata
     procedure is_masked => is_masked_vardata
     procedure is_loaded => is_loaded_vardata
     !procedure load => load_vardata
     !procedure save => save_vardata
     procedure free => free_vardata
     procedure average => average_vardata
     procedure fill_mask => fill_mask_vardata
  end type vardata

  interface init_vardata
     module procedure full_init_vardata
     module procedure empty_vardata
  end interface

  type, extends(vardata) :: vardata_real
     real, pointer :: rdata(:,:,:)
     logical :: owns
   contains
     procedure getreal => getreal_vardata_real
     procedure alloc => alloc_vardata_real
     procedure free => free_vardata_real
     procedure average => average_vardata_real
     procedure is_loaded => is_loaded_vardata_real
  end type vardata_real

  interface init_vardata_real
     module procedure full_init_vardata_real
     module procedure empty_vardata_real
     module procedure initdat_vardata_real
  end interface

  type, extends(vardata) :: vardata_int
     integer, pointer :: idata(:,:,:)
     logical :: owns
   contains
     procedure getint => getint_vardata_int
     procedure alloc => alloc_vardata_int
     procedure free => free_vardata_int
     procedure average => average_vardata_int
     procedure is_loaded => is_loaded_vardata_int
  end type vardata_int

  interface init_vardata_int
     module procedure full_init_vardata_int
     module procedure empty_vardata_int
     module procedure initdat_vardata_int
  end interface

contains ! -----------------------------------------------------

  logical function is_masked_vardata(this)
    class(vardata), intent(inout) :: this
    is_masked_vardata = associated(this%mask)
  end function is_masked_vardata

  logical function is_loaded_vardata(this)
    class(vardata), intent(inout) :: this
    is_loaded_vardata=.false.
  end function is_loaded_vardata

  function getmask_vardata(this,alloc,fill) result(mask)
    ! Returns this variable's mask, possibly allocating it as well, if requested.
    class(vardata), intent(inout) :: this
    logical, pointer :: mask(:,:,:)
    logical, intent(in), optional :: fill, alloc
    logical :: ialloc,fillval

    ialloc=.false.
    if(present(alloc)) ialloc=alloc
    fillval=.true.
    if(present(fill)) fillval=fill
    if(associated(this%mask)) then
       !write(0,*) 'associated(this%mask)'
    elseif(.not.ialloc) then
       !write(0,*) 'not ialloc'
    else
       call this%alloc_mask(fillval)
    endif
    mask=>this%mask
  end function getmask_vardata

  function getreal_vardata(this) result(rdata)
    ! In subclasses, returns this variable's real data, if there is
    ! any, or nullifies the pointer if there isn't.  May do other
    ! things too, such as reading in the data, or allocating and
    ! initializing a blank array.
    class(vardata), intent(inout) :: this
    real, pointer :: rdata(:,:,:)
    nullify(rdata)
  end function getreal_vardata

  function getint_vardata(this) result(idata)
    class(vardata), intent(inout) :: this
    integer, pointer :: idata(:,:,:)
    nullify(idata)
  end function getint_vardata

  subroutine average_vardata(s,t,iloc,jloc,weight,n,kloc)
    class(vardata), intent(inout) :: t ! target data
    class(vardata), intent(in) :: s    ! source data
    integer, intent(in) :: n
    integer, dimension(n,t%dc%ims:t%dc%ime,t%dc%jms:t%dc%jme,t%dc%kms:t%dc%kme), &
         intent(in) :: iloc,jloc
    integer, dimension(n,t%dc%ims:t%dc%ime,t%dc%jms:t%dc%jme,t%dc%kms:t%dc%kme), &
         intent(in), optional :: kloc
    real, dimension(n,t%dc%ims:t%dc%ime,t%dc%jms:t%dc%jme,t%dc%kms:t%dc%kme), &
         intent(in) :: weight

    call fail('ABORT: a subclass of vardata did not implement average_vardata')
  end subroutine average_vardata

  subroutine fill_mask_vardata(this,fill)
    class(vardata), intent(inout) :: this
    class(decomp), pointer :: dc
    logical, intent(in), optional :: fill
    integer :: i,j,k
    logical :: kmask, fillval

    dc=>this%dc
    fillval=.true.
    if(present(fill)) fillval=fill
    fillfalse: if(.not. fillval) then
       this%mask=.false.
    else
       ! For fill=.true. or if fill is missing, then fill regions
       ! inside the patch with .true., and outside with .false.:
       bigk: do k=dc%kms,dc%kme
          kmask = (k>=dc%kps .and. k<=dc%kpe)
          !$OMP PARALLEL DO PRIVATE(i,j)
          do j=dc%jms,dc%jps-1
             do i=dc%ims,dc%ime
                this%mask(i,j,k) = .false.
             enddo
          enddo
          !$OMP PARALLEL DO PRIVATE(i,j)
          do j=dc%jps,dc%jpe
             do i=dc%ims,dc%ips-1
                this%mask(i,j,k) = .false.
             enddo
             do i=dc%ips,dc%ipe
                this%mask(i,j,k) = kmask
             enddo
             do i=dc%ipe+1,dc%ime
                this%mask(i,j,k) = .false.
             enddo
          enddo
          !$OMP PARALLEL DO PRIVATE(i,j)
          do j=dc%jpe+1,dc%jme
             do i=dc%ims,dc%ime
                this%mask(i,j,k) = .false.
             enddo
          enddo
       enddo bigk
    endif fillfalse
  end subroutine fill_mask_vardata

  subroutine alloc_mask_vardata(this,fill)
    class(vardata), intent(inout) :: this
    class(decomp), pointer :: dc
    logical, intent(in), optional :: fill
    integer :: i,j,k
    logical :: kmask, fillval, alloced

    alloced=.false.
    nomask: if(.not. associated(this%mask)) then
       alloced=.true.
       dc=>this%dc
       allocate(this%mask(dc%ims:dc%ime,dc%jms:dc%jme,dc%kms:dc%kme))
    endif nomask
    if(present(fill) .or. alloced) then
       if(.not. associated(this%mask)) then
          call fail('Asserion error: mask is not allocated in alloc_mask')
       endif
       call fill_mask_vardata(this,fill)
    endif
  end subroutine alloc_mask_vardata

  subroutine empty_vardata(this)
    class(vardata), intent(inout) :: this
    nullify(this%pj)
    nullify(this%dc)
    nullify(this%mask)
    this%ownmask=.true.
  end subroutine empty_vardata

  subroutine full_init_vardata(this,proj,dec,alloc,mask,reinit,fill)
    class(vardata), intent(inout) :: this
    class(projection), target, intent(in) :: proj
    class(decomp), target, intent(in) :: dec
    logical, optional, intent(in) :: alloc, mask, reinit, fill
    logical :: is_reinit, wrongdims

    is_reinit=.false.
    if(present(reinit)) is_reinit=reinit
    if(is_reinit) then
       if(.not.this%dc%same_dims(dec)) then
          call free_vardata(this)
       else
          this%dc=>dec
          this%pj=>proj
          return
       endif
    endif

    this%pj=>proj
    this%dc=>dec
    this%ownmask=.true.
    nullify(this%mask)
    if(present(alloc)) then
       if(alloc) call this%alloc()
    endif
    if(present(mask)) then
       if(mask) call this%alloc_mask(fill=fill)
    endif
  end subroutine full_init_vardata

  subroutine free_vardata(this)
    class(vardata), intent(inout) :: this
    nullify(this%pj)
    nullify(this%dc)
    if(associated(this%mask) .and. this%ownmask) then
       deallocate(this%mask)
    endif
    nullify(this%mask)
  end subroutine free_vardata

  logical function is_loaded_vardata_real(this)
    class(vardata_real), intent(inout) :: this
    is_loaded_vardata_real=associated(this%rdata)
  end function is_loaded_vardata_real

  subroutine alloc_vardata(this)
    ! Allocates space for the variable's data in subclasses.
    class(vardata), intent(inout) :: this
  end subroutine alloc_vardata

  subroutine make_vardata(this,regen)
    ! In subclasses, this routine would generate, or regenerate, the
    ! variable's data.  If the data already exists, then it is only
    ! regenerated if regen is present and .true.
    class(vardata), intent(inout) :: this
    logical, intent(in), optional :: regen
  end subroutine make_vardata

  ! ------------------------------------------------------------

  function getreal_vardata_real(this) result(rdata)
    class(vardata_real), intent(inout) :: this
    real, pointer :: rdata(:,:,:)
    rdata=>this%rdata
  end function getreal_vardata_real

  subroutine average_vardata_real(s,t,iloc,jloc,weight,n,kloc)
    class(vardata_real), intent(in) :: s    ! source data
    class(vardata), intent(inout) :: t ! target data
    integer, intent(in) :: n
    integer, dimension(n,t%dc%ims:t%dc%ime,t%dc%jms:t%dc%jme,t%dc%kms:t%dc%kme), &
         intent(in) :: iloc,jloc
    integer, dimension(n,t%dc%ims:t%dc%ime,t%dc%jms:t%dc%jme,t%dc%kms:t%dc%kme), &
         intent(in), optional :: kloc
    real, dimension(n,t%dc%ims:t%dc%ime,t%dc%jms:t%dc%jme,t%dc%kms:t%dc%kme), &
         intent(in) :: weight

    real, pointer, dimension(:,:,:) :: rt
    integer, pointer, dimension(:,:,:) :: it
    logical, pointer, dimension(:,:,:) :: mask
    integer :: i,j,k,o,c,x,y,z
    real :: den,num,w

    nullify(rt,it,mask)
    mask=>t%getmask()
    rt=>t%getreal()
    it=>t%getint()

    if(.not.associated(it) .and. .not.associated(rt)) then
       call fail('In vardata_module, no real or integer target array is available in average_vardata_real')
    endif

    kloop: do k=max(t%dc%kps,t%dc%kds),min(t%dc%kpe,t%dc%kde)
       !$OMP PARALLEL DO PRIVATE(num,den,c,o,x,y,z,w)
       jloop: do j=max(t%dc%jds,t%dc%jps),min(t%dc%jpe,t%dc%jde)
          iloop: do i=max(t%dc%ids,t%dc%ips),min(t%dc%ipe,t%dc%ide)
             num=0
             den=0
             c=0
             do o=1,n
                x=iloc(o,i,j,k)
                y=jloc(o,i,j,k)
                if(present(kloc))  then
                   z=kloc(o,i,j,k)
                else
                   z=s%dc%kds
                endif
                if(  x>=s%dc%ips .and. x<=s%dc%ipe .and. &
                     y>=s%dc%jps .and. y<=s%dc%jpe .and. &
                     z>=s%dc%kps .and. z<=s%dc%kpe) then
                   w=weight(o,i,j,k)
                   num=num+w*s%rdata(x,y,z)
                   den=den+w
                   c=c+1
                endif
             enddo
             if(c>0 .and. den>0) then
                if(associated(it)) it(i,j,k)=nint(num/den)
                if(associated(rt)) rt(i,j,k)=num/den
                if(associated(mask)) &
                     mask(i,j,k)=.true.
             else
                if(associated(mask))              mask(i,j,k)=.false.
33              format('at ',I0,',',I0,',',I0,' no data available: c=',I0,' num=',F0.3,' den=',F0.3)
                write(0,33) i,j,k,c,num,den
             endif
          enddo iloop
       enddo jloop
       !$OMP END PARALLEL DO
    enddo kloop
  end subroutine average_vardata_real

  subroutine alloc_vardata_real(this)
    class(vardata_real), intent(inout) :: this
    integer :: starts(3),ends(3)
    if(associated(this%rdata)) return
    call this%dc%memdims(starts,ends)
    allocate(this%rdata(starts(1):ends(1), &
                        starts(2):ends(2), &
                        starts(3):ends(3)))
    this%owns=.true.
  end subroutine alloc_vardata_real

  subroutine free_vardata_real(this)
    class(vardata_real), intent(inout) :: this
    if(this%owns) then
       if(associated(this%rdata)) deallocate(this%rdata)
    else
       nullify(this%rdata)
       this%owns=.true.
    endif
    call free_vardata(this)
  end subroutine free_vardata_real

  subroutine empty_vardata_real(this)
    class(vardata_real), intent(inout) :: this
    nullify(this%rdata)
    this%owns=.true.
    call empty_vardata(this)
  end subroutine empty_vardata_real

  subroutine full_init_vardata_real(this,proj,dec,alloc,mask,reinit,fill)
    class(vardata_real), intent(inout) :: this
    class(projection), target, intent(in) :: proj
    class(decomp), target, intent(in) :: dec
    logical, intent(in), optional :: alloc, mask, reinit, fill
    if(present(reinit)) then
       if(reinit) then
          if(this%dc%same_dims(dec)) then
             call init_vardata(this,proj,dec,alloc=alloc,mask=mask,reinit=.true.)
             return
          else
             call this%free()
          endif
       endif
    endif
    this%owns=.true.
    nullify(this%rdata)
    call init_vardata(this,proj,dec,alloc=alloc,mask=mask,reinit=.false.,fill=fill)
  end subroutine full_init_vardata_real

  subroutine initdat_vardata_real(this,proj,dec,rdata,own,reinit)
    class(vardata_real), intent(inout) :: this
    class(projection), pointer, intent(in) :: proj
    class(decomp), pointer, intent(in) :: dec
    real, intent(in), pointer :: rdata(:,:,:)
    logical, intent(in), optional :: own, reinit

    if(present(reinit)) then
       if(reinit) call this%free()
    endif
    call init_vardata(this,proj,dec)
    
    this%owns=.true.
    nullify(this%rdata)

    if(present(own)) this%owns=own
    call this%dc%validate_dims((/size(rdata,1),size(rdata,2),size(rdata,3)/))
    this%rdata=>rdata
  end subroutine initdat_vardata_real

  ! ------------------------------------------------------------

  function getint_vardata_int(this) result(idata)
    class(vardata_int), intent(inout) :: this
    integer, pointer :: idata(:,:,:)
    idata=>this%idata
  end function getint_vardata_int

  logical function is_loaded_vardata_int(this)
    class(vardata_int), intent(inout) :: this
    is_loaded_vardata_int=associated(this%idata)
  end function is_loaded_vardata_int

  subroutine average_vardata_int(s,t,iloc,jloc,weight,n,kloc)
    class(vardata), intent(inout) :: t ! target data
    class(vardata_int), intent(in) :: s    ! source data
    integer, intent(in) :: n
    integer, dimension(n,t%dc%ims:t%dc%ime,t%dc%jms:t%dc%jme,t%dc%kms:t%dc%kme), &
         intent(in) :: iloc,jloc
    integer, dimension(n,t%dc%ims:t%dc%ime,t%dc%jms:t%dc%jme,t%dc%kms:t%dc%kme), &
         intent(in), optional :: kloc
    real, dimension(n,t%dc%ims:t%dc%ime,t%dc%jms:t%dc%jme,t%dc%kms:t%dc%kme), &
         intent(in) :: weight

    real, pointer, dimension(:,:,:) :: rt
    integer, pointer, dimension(:,:,:) :: it
    logical, pointer, dimension(:,:,:) :: mask
    integer :: i,j,k,o,c,x,y,z
    real :: den,num,w

    nullify(rt,it,mask)
    mask=>t%getmask()
    rt=>t%getreal()
    it=>t%getint()

    z=1
    kloop: do k=max(t%dc%kps,t%dc%kps),min(t%dc%kpe,t%dc%kde)
       !$OMP PARALLEL DO PRIVATE(num,den,c,o,x,y,z,w)
       jloop: do j=max(t%dc%jds,t%dc%jps),min(t%dc%jpe,t%dc%jde)
          iloop: do i=max(t%dc%ids,t%dc%ips),min(t%dc%ipe,t%dc%ide)
             num=0
             den=0
             c=0
             do o=1,n
                x=iloc(n,i,j,k)
                y=jloc(n,i,j,k)
                if(present(kloc)) z=kloc(n,i,j,k)
                if(  x>=s%dc%ips .and. x<=s%dc%ipe .and. &
                     y>=s%dc%jps .and. x<=s%dc%jpe .and. &
                     z>=s%dc%kps .and. z<=s%dc%kpe .and. &
                     w>=0 ) then
                   w=weight(n,i,j,k)
                   num=num+w*s%idata(i,j,k)
                   den=den+w
                   c=c+1
                endif
             enddo
             if(c>n/2 .and. den>0) then
                if(associated(rt)) rt(i,j,k)=num/den
                if(associated(it)) it(i,j,k)=nint(num/den)
                if(associated(mask)) &
                     mask(i,j,k)=.true.
             elseif(associated(mask)) then
                mask(i,j,k)=.false.
             endif
          enddo iloop
       enddo jloop
       !$OMP END PARALLEL DO
    enddo kloop
  end subroutine average_vardata_int

  subroutine alloc_vardata_int(this)
    class(vardata_int), intent(inout) :: this
    integer :: starts(3),ends(3)
    if(associated(this%idata)) return
    call this%dc%memdims(starts,ends)
    allocate(this%idata(starts(1):ends(1), &
                        starts(2):ends(2), &
                        starts(3):ends(3)))
    this%owns=.true.
  end subroutine alloc_vardata_int

  subroutine free_vardata_int(this)
    class(vardata_int), intent(inout) :: this
    if(this%owns) then
       if(associated(this%idata)) deallocate(this%idata)
    else
       nullify(this%idata)
       this%owns=.true.
    endif
    call free_vardata(this)
  end subroutine free_vardata_int

  subroutine empty_vardata_int(this)
    class(vardata_real), intent(inout) :: this
    nullify(this%rdata)
    this%owns=.true.
    call empty_vardata(this)
  end subroutine empty_vardata_int

  subroutine full_init_vardata_int(this,proj,dec,alloc,mask,reinit)
    class(vardata_int), intent(inout) :: this
    class(projection), target, intent(in) :: proj
    class(decomp), target, intent(in) :: dec
    logical, intent(in), optional :: alloc, mask, reinit
    if(present(reinit)) then
       if(reinit) then
          if(this%dc%same_dims(dec)) then
             call init_vardata(this,proj,dec,alloc=alloc,mask=mask,reinit=.true.)
             return
          else
             call this%free()
          endif
       endif
    endif
    this%owns=.true.
    nullify(this%idata)
    call init_vardata(this,proj,dec,alloc=alloc,mask=mask,reinit=.false.)
  end subroutine full_init_vardata_int

  subroutine initdat_vardata_int(this,proj,dec,idata,own,reinit)
    class(vardata_int), intent(inout) :: this
    class(projection), pointer, intent(in) :: proj
    class(decomp), pointer, intent(in) :: dec
    integer, intent(in), pointer :: idata(:,:,:)
    logical, intent(in), optional :: own, reinit

    if(present(reinit)) then
       if(reinit) call this%free()
    endif
    call init_vardata(this,proj,dec)
    
    this%owns=.true.
    nullify(this%idata)

    if(present(own)) this%owns=own
    call this%dc%validate_dims((/size(idata,1),size(idata,2),size(idata,3)/))
    this%idata=>idata
  end subroutine initdat_vardata_int

end module vardata_module
