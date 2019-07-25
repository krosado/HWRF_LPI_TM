module decomp_module
  use sysutil_module, only: fail, warn
  implicit none
  private

  public :: decomp, init_decomp, decomp_dummy

  integer, parameter :: DECOMP_NO_RANK = -1

  type decomp
     ! The decomp base class represents no decomposition.  The p
     ! dimensions are the d dimensions, and everything is reported to
     ! be on no rank (DECOMP_NO_RANK).
     integer :: ids,ide,jds,jde,kds,kde
     integer :: ims,ime,jms,jme,kms,kme
     integer :: ips,ipe,jps,jpe,kps,kpe
   contains
     procedure same_dims
     procedure free => free_decomp
     procedure memdims => memdims_decomp
     procedure validate_dims => validate_dims_decomp
     procedure my_rank => my_rank_decomp
     procedure ranks_for => ranks_for_decomp
     procedure decomp_rank => decomp_rank_decomp
  end type decomp

  type, extends(decomp) :: decomp_dummy
     ! This exists as a workaround for an Intel compiler bug
   contains
     procedure free => free_decomp_dummy
  end type decomp_dummy

contains

  logical function same_dims(this,that)
    class(decomp), intent(in) :: this,that
    same_dims = &
            this%ims==that%ims .and. &
            this%ime==that%ime .and. &
            this%jms==that%jms .and. &
            this%jme==that%jme .and. &
            this%kms==that%kms .and. &
            this%kme==that%kme .and. &
            this%ids==that%ids .and. &
            this%ide==that%ide .and. &
            this%jds==that%jds .and. &
            this%jde==that%jde .and. &
            this%kds==that%kds .and. &
            this%kde==that%kde .and. &
            this%ips==that%ips .and. &
            this%ipe==that%ipe .and. &
            this%jps==that%jps .and. &
            this%jpe==that%jpe .and. &
            this%kps==that%kps .and. &
            this%kpe==that%kpe
  end function same_dims

  integer function my_rank_decomp(this)
    class(decomp), intent(in) :: this
    my_rank_decomp=DECOMP_NO_RANK
  end function my_rank_decomp

  subroutine decomp_rank_decomp(this,rank,that)
    class(decomp), intent(in) :: this
    class(decomp), intent(out), pointer :: that
    integer, intent(in) :: rank

    allocate(that)
    call init_decomp(that, &
         this%ids,this%ide,this%jds,this%jde,this%kds,this%kde, &
         this%ims,this%ime,this%jms,this%jme,this%kms,this%kme)
  end subroutine decomp_rank_decomp

  subroutine init_decomp(this, &
                         ids,ide,jds,jde,kds,kde, &
                         ims,ime,jms,jme,kms,kme)
    integer, intent(in) :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme
    class(decomp), intent(inout) :: this
    this%ids=ids ; this%ide=ide ; this%ims=ims ; this%ime=ime
    this%jds=jds ; this%jde=jde ; this%jms=jms ; this%jme=jme
    this%kds=kds ; this%kde=kde ; this%kms=kms ; this%kme=kme

    ! Patch dimensions are domain dimensions
    this%ips=ids ; this%jps=jds ; this%kps=kds
    this%ipe=ide ; this%jpe=jde ; this%kpe=kde
  end subroutine init_decomp

  subroutine free_decomp(this)
    class(decomp), intent(in) :: this
  end subroutine free_decomp

  subroutine free_decomp_dummy(this)
    class(decomp_dummy), intent(in) :: this
    call free_decomp(this)
  end subroutine free_decomp_dummy

  subroutine memdims_decomp(this,starts,ends)
    class(decomp), intent(in) :: this
    integer, intent(out) :: starts(3), ends(3)
    starts(1)=this%ims ; ends(1)=this%ime
    starts(2)=this%jms ; ends(2)=this%jme
    starts(3)=this%kms ; ends(3)=this%kme
  end subroutine memdims_decomp

  subroutine validate_dims_decomp(this,othersizes)
    class(decomp), intent(in) :: this
    integer, intent(in) :: othersizes(3)
    integer :: starts(3),ends(3),sizes(3),i
    character*255 :: message
    call this%memdims(starts,ends)
    sizes=ends-starts+1
    do i=1,3
       if(othersizes(i)/=sizes(i)) then
100       format('dimension mismatch: ',/, &
                 '  expected: ',I0,',',I0,',',I0,/, &
                 ' but found: ',I0,',',I0,',',I0)
          write(message,100) sizes, othersizes
          call fail(message)
       endif
    enddo
  end subroutine validate_dims_decomp

  subroutine ranks_for_decomp(this,iloc,jloc,kloc,rank, &
       ids,ide,jds,jde,kds,kde, & ! note: dimensions in another grid
       ims,ime,jms,jme,kms,kme, &
       ips,ipe,jps,jpe,kps,kpe)
    integer, intent(in) :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme
    integer, intent(in) :: ips,ipe,jps,jpe,kps,kpe
    class(decomp), intent(in) :: this
    integer, intent(in) :: iloc(ims:ime,jms:jme,kms:kme)
    integer, intent(in) :: jloc(ims:ime,jms:jme,kms:kme)
    integer, intent(in) :: kloc(ims:ime,jms:jme,kms:kme)
    integer, intent(out) :: rank(ims:ime,jms:jme,kms:kme)
    integer :: i,j,k
    do k=max(kds,kps),min(kde,kpe)
       !$OMP PARALLEL DO PRIVATE(i,j)
       do j=max(jds,jps),min(jde,jpe)
          do i=max(ids,ips),min(ide,ipe)
             rank(i,j,k)=DECOMP_NO_RANK
          enddo
       enddo
       !$OMP END PARALLEL DO
    enddo
  end subroutine ranks_for_decomp
end module decomp_module
