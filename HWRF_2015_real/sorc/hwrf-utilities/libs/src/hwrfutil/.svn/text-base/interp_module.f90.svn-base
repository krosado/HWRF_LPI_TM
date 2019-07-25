module interp_module
  use sysutil_module, only: fail, warn
  use projection_module
  use decomp_module
  use vardata_module

  implicit none
  private

  public :: interpolator, init_interpolator, free_interpolator
  public :: bilinear_real, init_bilinear_real, free_bilinear_real
  public :: pd_init_bilinear_real

  ! INTERPOLATION OPERATIONS:
  integer, parameter, public :: &
       OP_REPLACE=0, OP_MAX=1, OP_MIN=2, OP_ADD=3, OP_SUB=4
  ! OP_REPLACE  -  simply replace target data with interpolated source data
  ! OP_SUB - subtract interpolated source data from target
  ! OP_ADD - add interpolated source data to target
  ! OP_MAX/OP_MIN - replace target data with maximum of target and
  !   interpolated source.  For horizontal wind vectors, both
  !   components are replaced if the wind magnitude is greater for
  !   OP_MAX, or lesser for OP_MIN
  ! Note that all operations are the same as OP_REPLACE when the
  ! target mask is all .false.

  type interpolator
   contains
     procedure clear => clear_interpolator
     procedure free => free_interpolator
     procedure prep => prep_interpolator
     procedure scalar => scalar_interpolator ! scalar (not wind) interpolation
     procedure hwind => hwind_interpolator  ! horizontal wind interpolation
  end type interpolator

  type, extends(interpolator) :: bilinear_real
     ! Horizontal-only interpolation.  Assumes the interpolation
     ! weights and indices do not vary with k.  It DOES correctly
     ! handle different k levels having different masks though, such
     ! as an above-ground masked Tv, for example.

     ! NOTE: If possible, use this%scalar(tgt,src,nomask=.true.).
     ! That invokes the fast unmasked_interp subroutine which uses
     ! pre-calculated weights and indices, and contains as few
     ! branches as possible.  It can only be used when the source data
     ! has no mask (target can still have a mask) and the operation
     ! being done is op=OP_REPLACE (simple data replacement).
     integer :: ifirst,ilast, jfirst,jlast, nnear

     class(projection), pointer :: psrc,ptgt
     class(decomp), pointer :: dsrc,dtgt
     logical :: prepped

     real, pointer :: tgtlat(:,:,:), tgtlon(:,:,:), srcx(:,:,:), srcy(:,:,:)
     integer, pointer :: inear(:,:,:,:),jnear(:,:,:,:), count(:,:)
     real, pointer :: weight(:,:,:,:), denom(:,:)

     real, pointer :: cossrc(:,:,:), sinsrc(:,:,:)
     real, pointer :: costgt(:,:,:), sintgt(:,:,:)

     logical, pointer :: ihave(:), jhave(:)
   contains
     procedure free => free_bilinear_real
     procedure prep => prep_bilinear_real
     procedure scalar => scalar_bilinear_real
     procedure hwind => hwind_bilinear_real

     procedure masked_interp => masked_bilinear_real
     procedure unmasked_interp => unmasked_bilinear_real
  end type bilinear_real

  interface init_bilinear_real
     module procedure clear_bilinear_real
     module procedure pd_init_bilinear_real
     module procedure var_init_bilinear_real
  end interface

contains ! ----------------------------------------------------
  subroutine clear_interpolator(this)
    class(interpolator), intent(inout) :: this
  end subroutine clear_interpolator
  subroutine prep_interpolator(this)
    class(interpolator), intent(inout) :: this
  end subroutine prep_interpolator
  subroutine free_interpolator(this)
    class(interpolator), intent(inout) :: this
  end subroutine free_interpolator
  subroutine init_interpolator(this)
    class(interpolator), intent(inout) :: this
  end subroutine init_interpolator
  subroutine scalar_interpolator(this,tgtvar,srcvar,op,nomask)
    class(interpolator), intent(inout) :: this
    class(vardata), intent(inout) :: tgtvar,srcvar
    logical, intent(in) , optional :: nomask
    integer, intent(in), optional :: op
  end subroutine scalar_interpolator
  subroutine hwind_interpolator(this,tgtu,tgtv,srcu,srcv,op,nomask)
    class(interpolator), intent(inout) :: this
    class(vardata), intent(inout) :: tgtu,tgtv,srcu,srcv
    logical, intent(in) , optional :: nomask
    integer, intent(in), optional :: op
  end subroutine hwind_interpolator

  ! ------------------------------------------------------------

  subroutine clear_bilinear_real(this)
    class(bilinear_real), intent(inout) :: this
    this%prepped=.false.
    this%ifirst=0
    this%jfirst=0
    this%ilast=0
    this%jlast=0
    nullify(this%tgtlat,this%tgtlon,this%srcx,this%srcy,this%ihave,this%jhave)
    nullify(this%inear,this%jnear,this%count,this%denom) ! ,this%knear
  end subroutine clear_bilinear_real

  subroutine var_init_bilinear_real(this,tgt,src)
    class(bilinear_real), intent(inout) :: this
    class(vardata), intent(inout) :: tgt,src
    this%ptgt=>tgt%pj
    this%dtgt=>tgt%dc
    this%psrc=>src%pj
    this%dsrc=>src%dc
    call clear_bilinear_real(this)
  end subroutine var_init_bilinear_real

  subroutine pd_init_bilinear_real(this,ptgt,dtgt,psrc,dsrc)
    class(bilinear_real), intent(inout) :: this
    class(projection), target, intent(inout) :: ptgt,psrc
    class(decomp), target, intent(inout) :: dtgt,dsrc
    this%ptgt=>ptgt
    this%dtgt=>dtgt
    this%psrc=>psrc
    this%dsrc=>dsrc
    call clear_bilinear_real(this)
  end subroutine pd_init_bilinear_real

  subroutine free_bilinear_real(this)
    class(bilinear_real), intent(inout) :: this

    if(associated(this%tgtlat)) deallocate(this%tgtlat)
    if(associated(this%tgtlon)) deallocate(this%tgtlon)
    if(associated(this%cossrc)) deallocate(this%cossrc)
    if(associated(this%sinsrc)) deallocate(this%sinsrc)
    if(associated(this%costgt)) deallocate(this%costgt)
    if(associated(this%sintgt)) deallocate(this%sintgt)
    if(associated(this%srcx))   deallocate(this%srcx)
    if(associated(this%srcy))   deallocate(this%srcy)
    if(associated(this%inear))  deallocate(this%inear)
    if(associated(this%jnear))  deallocate(this%jnear)
    !if(associated(this%knear))  deallocate(this%knear)
    if(associated(this%count))  deallocate(this%count)
    if(associated(this%weight)) deallocate(this%weight)
    if(associated(this%denom))  deallocate(this%denom)
    if(associated(this%ihave))  deallocate(this%ihave)
    if(associated(this%jhave))  deallocate(this%jhave)

    call clear_bilinear_real(this)
    nullify(this%ptgt,this%dtgt,this%psrc,this%dsrc)

    call free_interpolator(this)
  end subroutine free_bilinear_real

  subroutine scalar_bilinear_real(this,tgtvar,srcvar,op,nomask)
    class(bilinear_real), intent(inout) :: this
    class(vardata), intent(inout) :: tgtvar,srcvar
    class(decomp), pointer :: sd, td
    logical, intent(in) , optional :: nomask
    integer, intent(in), optional :: op
    real, pointer :: sr(:,:,:), tr(:,:,:)
    integer, pointer :: si(:,:,:), ti(:,:,:)
    logical, pointer :: sm(:,:,:), tm(:,:,:)
    logical :: nomaskit
    integer :: the_op

    nomaskit=.not.srcvar%is_masked()
    the_op=OP_REPLACE
    if(present(op)) the_op=op
    if(present(nomask)) nomaskit=nomask

    sd=>this%dsrc
    td=>this%dtgt

    sm=>srcvar%getmask() ! Default initialization
    tm=>tgtvar%getmask(alloc=.true.,fill=.false.) ! Initialize to "no data here" for all points

    ! Try to get integer data first so we interpolate integer fields
    ! as integer:
    si=>srcvar%getint()
    ti=>tgtvar%getint()

    ! Next, get real valued data if we don't have integer:
    tcheck: if(.not. associated(ti)) then
       tr=>tgtvar%getreal()
       if(.not.associated(tr)) then
          call fail('No real or integer data for target field.')
       endif
    endif tcheck
    scheck: if(.not. associated(si)) then
       sr=>srcvar%getreal()
       if(.not.associated(sr)) then
          call fail('No real or integer data for source field.')
       endif
    endif scheck

    nomasks: if(the_op==OP_REPLACE .and. (.not. associated(sm) .or. nomaskit)) then
       ! Run simple routines with pre-calculated actions when source
       ! data has no mask:
       call this%unmasked_interp(tr,ti,tm,sr,si)
    else
       ! Much more complicated routine for masked source data:
       call this%masked_interp(tr,ti,tm,sr,si,sm,the_op)
    endif nomasks
  end subroutine scalar_bilinear_real

  subroutine hwind_bilinear_real(this,tgtu,tgtv,srcu,srcv,op,nomask)
    class(bilinear_real), intent(inout) :: this
    class(vardata), intent(inout) :: tgtu,tgtv,srcu,srcv
    logical, intent(in) , optional :: nomask
    integer, intent(in), optional :: op
    integer :: opp
    class(decomp), pointer :: td,sd
    real, pointer, dimension(:,:,:) :: su,sv,tu,tv
    logical, pointer, dimension(:,:,:) :: smu,smv,tmu,tmv
    integer :: i,j,k, ic,c,o
    real :: unum,vnum,den, x,y,w, u,v, uu,vv, m1,m2
    logical :: mask, tw, sw, tm, big_case

    opp=OP_REPLACE
    if(present(op)) opp=op

    sw=this%psrc%earth_winds
    tw=this%ptgt%earth_winds

108 format('interp: target ew:',I0,' source ew:',I0)
    !print 108, tw, sw

    mask=.true.
    if(present(nomask)) mask=.not.nomask

    sd=>this%dsrc
    su=>srcu%getreal()
    sv=>srcv%getreal()
    smu=>srcu%getmask()
    smv=>srcv%getmask()

    td=>this%dtgt
    tu=>tgtu%getreal()
    tv=>tgtv%getreal()
    tmu=>tgtu%getmask(fill=.false.)
    tmv=>tgtv%getmask(fill=.false.)

    tm = associated(tmu) .and. associated(tmv)

    if(.not.associated(su) .or. .not.associated(sv)) then
       call fail('ABORT: in bilinear real, when interpolating winds, no real-valued data was provided from source')
    endif

    if(.not.associated(tu) .or. .not.associated(tv)) then
       call fail('ABORT: in bilinear real, when interpolating winds, no real-valued data space was provided by target')
    endif

    ! Much more complicated case of source and target masks.  Same
    ! code as masked_bilinear_real, but for two variables, followed by
    ! a matrix multiply.

    !tm_intersect = ( associated(tm) .and. .not. merge )
    !tm_union     = ( associated(tm) .and. merge )
    kmask: do k=td%kps,td%kpe
       !$OMP PARALLEL DO PRIVATE(i,j,c,den,ic,vnum,unum,o,x,y,w,u,v,uu,vv,big_case,m1,m2)
       jmask: do j=td%jps,td%jpe
          !big_case = (opp/=OP_REPLACE) ! reset later on if target mask present
          big_case=.true.
          if(.not. this%jhave(j)) cycle jmask
          imask: do i=max(td%ips,this%ifirst),min(td%ipe,this%ilast)
             c=this%count(i,j)
             if(c==0) cycle imask

             ! ###########################################
             ! Interpolate source data to target
             ! ###########################################

             den=0
             ic=0
             vnum=0
             unum=0
             omask: do o=1,c
                x=this%inear(o,i,j,td%kps)
                y=this%jnear(o,i,j,td%kps)
                w=this%weight(o,i,j,td%kps)
                if(mask) then
                   if(.not.smu(x,y,k)) then
                      !print *,'no source u at ',x,y,k
                      cycle omask
                   endif
                   if(.not.smv(x,y,k)) then
                      !print *,'no source v at ',x,y,k
                      cycle omask
                   endif
                endif
                den=den+w
                unum=unum+su(x,y,k)*w
                vnum=vnum+sv(x,y,k)*w
                ic=ic+1
             enddo omask
             tstore: if(ic>this%nnear/2 .and. den>0) then
                u=unum/den
                v=vnum/den

                ! ########################################
                ! Rotate winds
                ! ########################################

                if(.not.sw) then
                   ! Rotate source to earth winds
                   uu =   this%cossrc(i,j,td%kps)*u - this%sinsrc(i,j,td%kps)*v
                   vv =   this%sinsrc(i,j,td%kps)*u + this%cossrc(i,j,td%kps)*v
                   u=uu ; v=vv
                endif
                if(.not.tw) then
                   ! Rotate target to grid winds
                   uu =   this%costgt(i,j,td%kps)*u + this%sintgt(i,j,td%kps)*v
                   vv = - this%sintgt(i,j,td%kps)*u + this%costgt(i,j,td%kps)*v
                   u=uu ; v=vv
                endif

                ! ########################################
                ! Perform calculation and storage
                ! ########################################

                !if(tm) big_case = ( tmu(i,j,k) .and. tmv(i,j,k) )

                simple_assign: if(.not. big_case) then
                   ! We only get here if there is no target mask and
                   ! the operation is OP_REPLACE.
                   tu(i,j,k)=u
                   tv(i,j,k)=v
                   call fail('should not get here')
                else
                   operation: select case(opp)
                   case(OP_ADD)
                      tu(i,j,k)=tu(i,j,k)+u
                      tv(i,j,k)=tv(i,j,k)+v
                   case(OP_SUB)
                      tu(i,j,k)=tu(i,j,k)-u
                      tv(i,j,k)=tv(i,j,k)-v
                   case(OP_MAX)
                      ! Vector max: replace both values if magnitude is greater.
                      m1=tu(i,j,k)**2+tv(i,j,k)**2
                      m2=u**2+v**2
                      if(m2>m1) then
                         tu(i,j,k)=max(tu(i,j,k),u)
                         tv(i,j,k)=max(tv(i,j,k),v)
                      endif
                   case(OP_MIN)
                      ! Vector min: replace both values if magnitude is less.
                      m1=tu(i,j,k)**2+tv(i,j,k)**2
                      m2=u**2+v**2
                      if(m2<m1) then
                         tu(i,j,k)=max(tu(i,j,k),u)
                         tv(i,j,k)=max(tv(i,j,k),v)
                      endif
                   case default
                      ! OP_REPLACE or invalid option.
                      tu(i,j,k)=u
                      tv(i,j,k)=v
33                    format('storing data at ',I0,',',I0,',',I0,' u=',F0.3,' v=',F0.3)
                      !print 33,i,j,k,u,v
                   end select operation
                   if(tm) then
                      tmu(i,j,k)=.true.
                      tmv(i,j,k)=.true.
34                    format('storing mask at ',I0,',',I0,',',I0)
                      !print 34,i,j,k
                   endif
                endif simple_assign
             else
                !print 40, i,j,k, ic,den
40              format('ic or den bad at ',I0,',',I0,',',I0,': ic=',I0,' den=',F0.6)
             endif tstore
          enddo imask
       enddo jmask
       !$OMP END PARALLEL DO
    enddo kmask
  end subroutine hwind_bilinear_real

  subroutine masked_bilinear_real(this,tr,ti,tm,sr,si,sm,op)
    ! Interpolate using source mask (sm).  This is much more expensive
    ! than unmasked_bilinear_real since the point count threshold (#
    ! of src points per tgt pt) must be recomputed.  Also the
    ! assignment of data is in a big select statment that handles the
    ! various operations.
    class(bilinear_real), intent(inout) :: this
    integer, intent(in) :: op
    class(decomp), pointer :: td,sd
    integer, pointer, intent(in), dimension(:,:,:) :: si
    integer, pointer, intent(out), dimension(:,:,:) :: ti
    real, pointer, intent(in), dimension(:,:,:) :: sr
    real, pointer, intent(out), dimension(:,:,:) :: tr
    logical, pointer, intent(in), dimension(:,:,:) :: sm
    logical, pointer, intent(out), dimension(:,:,:) :: tm
    integer :: i,j,k,c,o,ic
    real :: num,den,x,y,w
    logical :: have

    sd=>this%dsrc
    td=>this%dtgt

    ! Much more complicated case of source and target masks.
    !tm_intersect = ( associated(tm) .and. .not. merge )
    !tm_union     = ( associated(tm) .and. merge )
    kmask: do k=td%kps,td%kpe
       !$OMP PARALLEL DO PRIVATE(i,j,c,den,ic,num,o,x,y,w,have)
       jmask: do j=td%jps,td%jpe
          have = (op/=OP_REPLACE) ! reset later if target mask is present
          if(.not. this%jhave(j)) cycle jmask
          imask: do i=max(td%ips,this%ifirst),min(td%ipe,this%ilast)
             c=this%count(i,j)
             if(c==0) cycle imask
             den=0
             ic=0
             num=0
             omask: do o=1,c
                x=this%inear(o,i,j,td%kps)
                y=this%jnear(o,i,j,td%kps)
                w=this%weight(o,i,j,td%kps)
                if(.not.sm(x,y,k)) cycle omask
                den=den+w
                if(associated(si)) then
                   num=num+si(x,y,k)*w
                else
                   num=num+sr(x,y,k)*w
                endif
                ic=ic+1
             enddo omask
             tstore: if(ic>this%nnear/2 .and. den>0) then
                if(associated(tm)) then
                   have=tm(i,j,k)
                   tm(i,j,k)=.true.
                endif
                have_data: if(have) then
                   if(associated(ti)) then
                      select case(op)
                      case(OP_MAX) ; ti(i,j,k)=max(ti(i,j,k),nint(num/den))
                      case(OP_MIN) ; ti(i,j,k)=min(ti(i,j,k),nint(num/den))
                      case(OP_ADD) ; ti(i,j,k)=ti(i,j,k)+nint(num/den)
                      case(OP_SUB) ; ti(i,j,k)=ti(i,j,k)-nint(num/den)
                      case default ; ti(i,j,k)=nint(num/den)
                      end select
                   else
                      select case(op)
                      case(OP_MAX) ; tr(i,j,k)=max(tr(i,j,k),num/den)
                      case(OP_MIN) ; tr(i,j,k)=min(tr(i,j,k),num/den)
                      case(OP_ADD) ; tr(i,j,k)=tr(i,j,k)+num/den
                      case(OP_SUB) ; tr(i,j,k)=tr(i,j,k)-num/den
                      case default ; tr(i,j,k)=num/den
                      end select
                   endif
                else ! No data available, so simply assign.
                   if(associated(ti)) then
                      ti(i,j,k)=nint(num/den)
                   else
                      tr(i,j,k)=num/den
                   endif
                endif have_data
                if(associated(tm)) tm(i,j,k)=.true.
             endif tstore
          enddo imask
       enddo jmask
       !$OMP END PARALLEL DO
    enddo kmask
  end subroutine masked_bilinear_real

  subroutine unmasked_bilinear_real(this,tr,ti,tm,sr,si)
    class(bilinear_real), intent(inout) :: this
    class(decomp), pointer :: td

    real, pointer, intent(in), dimension(:,:,:) :: sr
    integer, pointer, intent(in), dimension(:,:,:) :: si
    real, pointer, intent(out), dimension(:,:,:) :: tr
    integer, pointer, intent(out), dimension(:,:,:) :: ti
    logical, pointer, intent(inout), dimension(:,:,:) :: tm
    integer :: i,j,k,c,o
    real :: num,den

    td=>this%dtgt
    knomask: do k=td%kps,td%kpe
       !$OMP PARALLEL DO PRIVATE(i,j,o, c,num)
       jnomask: do j=td%jps,td%jpe
          if(.not. this%jhave(j)) cycle jnomask
          inomask: do i=max(td%ips,this%ifirst),min(td%ipe,this%ilast)
             c=this%count(i,j)
             if(c==0) cycle

             ! Average data.  
             intdata: if(associated(si)) then
                num = si(this%inear(1,i,j,1),this%jnear(1,i,j,1),k) &
                     *this%weight(1,i,j,1)
                do o=2,c
                   num = num + si(this%inear(o,i,j,1),this%jnear(o,i,j,1),k)&
                        *this%weight(o,i,j,1)
                enddo
             else
                num = sr(this%inear(1,i,j,1),this%jnear(1,i,j,1),k) &
                     *this%weight(1,i,j,1)
                do o=2,c
                   num = num + sr(this%inear(o,i,j,1),this%jnear(o,i,j,1),k)&
                        *this%weight(o,i,j,1)
                enddo
             endif intdata

             ! Store data.  Prefer integer over real:
             if(associated(ti)) then
                ti(i,j,k)=nint(num/this%denom(i,j))
             else
                tr(i,j,k)=num/this%denom(i,j)
             endif
             if(associated(tm)) tm(i,j,k)=.true.
          enddo inomask
       enddo jnomask
    enddo knomask
  end subroutine unmasked_bilinear_real

  subroutine prep_bilinear_real(this)
    class(bilinear_real), intent(inout) :: this
    class(decomp),pointer :: sd, td

    integer :: i,j,c,x,y,o,k
    real :: w,den

    call clear_bilinear_real(this)

    sd=>this%dsrc
    td=>this%dtgt

    ! Get longitudes of target domain:
    allocate(this%tgtlat(td%ims:td%ime,td%jms:td%jme,td%kps:td%kps))
    allocate(this%tgtlon(td%ims:td%ime,td%jms:td%jme,td%kps:td%kps)) 
    allocate(this%costgt(td%ims:td%ime,td%jms:td%jme,td%kps:td%kps))
    allocate(this%sintgt(td%ims:td%ime,td%jms:td%jme,td%kps:td%kps))
    call this%ptgt%projinfo( &
         td%ims,td%ime,td%jms,td%jme,td%kms,td%kme, &
         td%ids,td%ide,td%jds,td%jde,td%kds,td%kde, &
         td%ips,td%ipe,td%jps,td%jpe,td%kps,td%kps, &  ! NOTE: end is kps
         lat=this%tgtlat, lon=this%tgtlon,          &
         crot=this%costgt, srot=this%sintgt         )

    ! Get real-valued x,y locations in source domain of target lats & lons:
    allocate(this%srcx(td%ims:td%ime,td%jms:td%jme,td%kps:td%kps))
    allocate(this%srcy(td%ims:td%ime,td%jms:td%jme,td%kps:td%kps))
    allocate(this%cossrc(td%ims:td%ime,td%jms:td%jme,td%kps:td%kps))
    allocate(this%sinsrc(td%ims:td%ime,td%jms:td%jme,td%kps:td%kps))
    this%srcx=-999
    this%srcy=-999
    call this%psrc%locate(this%tgtlat, this%tgtlon, &
         td%ims,td%ime,td%jms,td%jme,td%kms,td%kme, &
         td%ids,td%ide,td%jds,td%jde,td%kds,td%kde, &
         td%ips,td%ipe,td%jps,td%jpe,td%kps,td%kps, &  ! NOTE: end is kps
         xpoints=this%srcx, ypoints=this%srcy,      &
         irad=1, nnear=this%nnear,                  &
         crot=this%cossrc, srot=this%sinsrc         )

300 format('xy at ',I0,',',I0,',',I0,': x=',F0.3,' y=',F0.3)
    ! do j=td%jps,td%jpe
    !    do i=td%ips,td%ipe
    !       print 300, i,j,td%kps, this%srcx(i,j,td%kps), this%srcy(i,j,td%kps)
    !    enddo
    ! enddo

    ! Get interpolation weights and point locations:
    allocate(this%inear(this%nnear,td%ims:td%ime,td%jms:td%jme,td%kps:td%kps))
    allocate(this%jnear(this%nnear,td%ims:td%ime,td%jms:td%jme,td%kps:td%kps))
    allocate(this%weight(this%nnear,td%ims:td%ime,td%jms:td%jme,td%kps:td%kps))
    !allocate(this%knear(this%nnear,td%ims:td%ime,td%jms:td%jme,td%kps:td%kps))
    this%inear=-999
    this%jnear=-999
    this%weight=-999
    call this%psrc%near(this%srcx,this%srcy,1, &
         this%inear,this%jnear,this%weight, &
         td%ims,td%ime,td%jms,td%jme,td%kms,td%kme, &
         td%ids,td%ide,td%jds,td%jde,td%kds,td%kde, &
         td%ips,td%ipe,td%jps,td%jpe,td%kps,td%kps, &  ! NOTE: end is kps
         this%nnear)

    allocate(this%count(td%ims:td%ime,td%jms:td%jme))
    allocate(this%denom(td%ims:td%ime,td%jms:td%jme))

    allocate(this%jhave(td%jms:td%jme), this%ihave(td%ims:td%ime))

    this%ihave(td%ips:td%ipe)=.false.

301 format('prep: at ',I0,',',I0,',',I0,',',I0,': inear=',I0,' jnear=',I0,' weight=',F0.3)
    ! do j=td%jps,td%jpe
    !    do i=td%ips,td%ipe
    !       do o=1,this%nnear
    !       print 301, o,i,j,td%kps, &
    !            this%inear(o,i,j,td%kps),this%jnear(o,i,j,td%kps),this%weight(o,i,j,td%kps)
    !    enddo
    !    enddo
    ! enddo
303 format('td: ips=',I0,' ipe=',I0,' jps=',I0,' jpe=',I0)
304 format('sd: ips=',I0,' ipe=',I0,' jps=',I0,' jpe=',I0)
    !print 303,td%ips,td%ipe,td%jps,td%jpe
    !print 304,sd%ips,sd%ipe,sd%jps,sd%jpe
    k=td%kps
    !$OMP PARALLEL DO PRIVATE(i,j,c,x,y,w,den,o)
    jint: do j=td%jps,td%jpe
       this%jhave(j)=.false.
       iint: do i=td%ips,td%ipe
          c=0
          den=0
          oint: do o=1,this%nnear
             x=this%inear(o,i,j,td%kps)
             y=this%jnear(o,i,j,td%kps)
             if(x==badpoint) cycle
             if(y==badpoint) cycle
             xyok: if(x>=sd%ips .and. x<=sd%ipe .and. y>=sd%jps .and. y<=sd%jpe) then
                w=this%weight(o,i,j,td%kps)
                wok: if(w>0) then
                   c=c+1
                   this%inear(c,i,j,td%kps)=x
                   this%jnear(c,i,j,td%kps)=y
                   this%weight(c,i,j,td%kps)=w
                   den=den+w
                else
                   !print *,'zero weight: ',i,j,x,y,w
                endif wok
             else
                !print *,'x,y outside: ',i,j,x,y
             endif xyok
          enddo oint
          ofill: do o=c+1,this%nnear
             this%inear(o,i,j,td%kps)=0
             this%jnear(o,i,j,td%kps)=0
             this%weight(o,i,j,td%kps)=0
          end do ofill
          found: if(c>this%nnear/2 .and. den>0) then
             !print *,'have data at i=',i,' j=',j,' count=',c,' den=',den
             this%ihave(i)=.true.
             this%jhave(j)=.true.
             this%count(i,j)=c
             this%denom(i,j)=den
          else
             !print *,'no data at i=',i,' j=',j
             this%count(i,j)=0
             this%denom(i,j)=0
          endif found
       enddo iint
    enddo jint
    !$OMP END PARALLEL DO

    ! Find the first and last elements in both I and J.  This is
    ! serial, but should be fast enough.  I could parallelize it, but
    ! the OpenMP setup time would likely be larger than any speed
    ! gain.

    ifirst: do i=td%ips,td%ipe
       if(this%ihave(i)) then
          this%ifirst=i
          exit
       endif
    enddo ifirst


    ilast: do i=td%ipe,td%ips,-1
       if(this%ihave(i)) then
          this%ilast=i
          exit
       endif
    enddo ilast

    jfirst: do j=td%jps,td%jpe
       if(this%jhave(j)) then
          this%jfirst=j
          exit
       endif
    enddo jfirst

    jlast: do j=td%jpe,td%jps,-1
       if(this%jhave(j)) then
          this%jlast=j
          exit
       endif
    enddo jlast
  end subroutine prep_bilinear_real
end module interp_module
