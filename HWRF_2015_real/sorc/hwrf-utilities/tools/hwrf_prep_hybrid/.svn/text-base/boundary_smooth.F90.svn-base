#define DBGLINE      ! write(6,*) __FILE__,':',__LINE__,': ',
SUBROUTINE  boundary_smooth(h, landmask, nsmth , nrow, NX,NY)

  implicit none

  integer, intent(in) :: NX,NY,nrow,nsmth
  real,intent(inout) ::    h(1:NX,1:NY),landmask(1:NX,1:NY)

  integer :: IDS,IDE,JDS,JDE
  integer :: ITS,ITE,JTS,JTE

  integer:: ihw(1:NY),ihe(1:NY)
  real ::   h_old(1:NX,1:NY)
  real ::   hbms(1:NX,1:NY)
  real ::   hse(1:NX,1:NY)
  real ::   hne(1:NX,1:NY)
  integer :: ihl, ihh, m2l, ibas,jmelin
  integer :: I,J,KS,IOFFSET,JSTART,JEND
  character (len=255) :: message

  ! The tile is the domain in prep_hybrid:
  ITS=1     ;    IDS=1
  ITE=NX+1  ;    IDE=NX+1
  JTS=1     ;    JDS=1
  JTE=NY+1  ;    JDE=NY+1

  DBGLINE 'ITS=',ITS,' ITE=',ITE,' JTS=',JTS,' JTE=',JTE
  DBGLINE 'IDS=',IDS,' IDE=',IDE,' JDS=',JDS,' JDE=',JDE

  do j= JTS,min(JTE,JDE-1)
     ihw(J)=-mod(J,2)
     ihe(j)=ihw(J)+1
  end do

  do J=JTS,min(JTE,JDE-1)
     do I=ITS,min(ITE,IDE-1)
        hbms(I,J)=landmask(I,J)
     enddo
  enddo

  jmelin=(JDE-1)-nrow+1
  ibas=nrow/2
  m2l=mod(nrow,2)

  do j=jts,min(jte,jde-1)
     ihl=ibas+mod(j,2)+m2l*mod(J+1,2)
     ihh=(IDE-1)-ibas-m2l*mod(J+1,2)
     do i=its,min(ite,ide-1)
        if (I .ge. ihl .and. I .le. ihh .and. J .ge. nrow .and. J .le. jmelin) then
           hbms(I,J)=0.
        endif
     end do
  end do

634 format(30(f2.0,1x))

  do KS=1,nsmth
     DBGLINE 'KS=',KS
     h_old=h

     do J=JTS,min(JTE,JDE-1)
        DBGLINE 'first big I/J loop: J=',J
        do I=ITS, min(ITE,IDE-1)
           if (I .ge. (IDS+mod(J,2)) .and. J .gt. JDS .and. J .lt. JDE-1 .and. I .lt. IDE-1) then
              h(i,j)= ( h_old(i+ihe(j),j+1) + h_old(i+ihw(j),j-1) + h_old(i+ihe(j),j-1) + h_old(i+ihw(j),j+1) - &
                   4. *h_old(i,j) )*hbms(i,j)*0.125+h_old(i,j)
           endif

        enddo
     enddo
     DBGLINE 'got past first big I/J loop.'

     !       special treatment for four corners
     DBGLINE 'treat a corner'
     if (hbms(1,1) .eq. 1 .and. ITS .le. 1 .and. JTS .le. 1) then
        DBGLINE 'ihe(1)=',ihe(1)
        h(1,1)=0.75*h(1,1)+0.125*h(1+ihe(1),2)+  &
             0.0625*(h(2,1)+h(1,3))
     endif

     DBGLINE 'treat a corner'
     if (hbms(IDE-1,1) .eq. 1 .and. ITE .ge. IDE-2 .and. JTS .le. 1) then
        DBGLINE 'ihw(1)=',ihw(1)
        h(IDE-1,1)=0.75*h(IDE-1,1)+0.125*h(IDE-1+ihw(1),2)+ &
             0.0625*(h(IDE-1-1,1)+h(IDE-1,3))
     endif

     DBGLINE 'treat a corner'
     if (hbms(1,JDE-1) .eq. 1 .and. ITS .le. 1 .and. JTE .ge. JDE-2) then
        DBGLINE 'ihe(JDE-1)=ihe(',JDE,'-1)=',ihe(JDE-1)
        h(1,JDE-1)=0.75*h(1,JDE-1)+0.125*h(1+ihe(JDE-1),JDE-1-1)+ &
             0.0625*(h(2,JDE-1)+h(1,JDE-1-2))
     endif

     DBGLINE 'treat a corner'
     if (hbms(IDE-1,JDE-1) .eq. 1 .and. ITE .ge. IDE-2 .and. JTE .ge. JDE-2) then
        DBGLINE 'ihw(JDE-1)=ihw(',JDE,'-1)=',ihw(JDE-1)
        h(IDE-1,JDE-1)=0.75*h(IDE-1,JDE-1)+0.125*h(IDE-1+ihw(JDE-1),JDE-1-1)+ &
             0.0625*(h(IDE-1-1,JDE-1)+h(IDE-1,JDE-1-2))
     endif

     !       S bound
     DBGLINE 'S bound'
     if (JTS .eq. JDS) then
        J=JTS

        do I=ITS,ITE
           if (I .ge. IDS+1 .and. I .le. IDE-2) then
              if (hbms(I,J) .eq. 1) then
                 h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J+1)+h(I+ihe(J),J+1))
              endif
           endif
        enddo

     endif

     !       N bound
     DBGLINE 'N bound'
     if (JTE .eq. JDE) then
        J=JDE-1
        DBGLINE 'DOING N BOUND SMOOTHING for J= ', J
        do I=ITS,min(ITE,IDE-1)
           if (hbms(I,J) .eq. 1 .and. I .ge. IDS+1 .and. I .le. IDE-2) then
              h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J-1)+h(I+ihe(J),J-1))
           endif
        enddo
     endif

     !       W bound
     DBGLINE 'W bound'
     if (ITS .eq. IDS) then
        I=ITS
        do J=JTS,min(JTE,JDE-1)
           if (hbms(I,J) .eq. 1 .and. J .ge. JDS+2 .and. J .le. JDE-3 .and. mod(J,2) .eq. 1) then
              h(I,J)=0.75*h(I,J)+0.125*(h(I+ihe(J),J+1)+h(I+ihe(J),J-1))
           endif
        enddo
     endif

     !       E bound
     DBGLINE 'E bound'
     if (ITE .eq. IDE) then
        DBGLINE 'DOING E BOUND SMOOTHING for I= ', min(ITE,IDE-1)
        I=min(ITE,IDE-1)
        do J=JTS,min(JTE,JDE-1)
           if (hbms(I,J) .eq. 1  .and. J .ge. JDS+2 .and. J .le. JDE-3 .and. mod(J,2) .eq. 1) then
              h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J+1)+h(I+ihw(J),J-1))
           endif
        enddo
     endif

  enddo   ! end ks loop

  ! extra smoothing along inner boundary

  DBGLINE 'inner boundary; first loop'
  if (JTS .eq. JDS) then
     if (ITE .eq. IDE) then
        IOFFSET=1
     else
        IOFFSET=0
     endif
     !  Southern Boundary
     do i=its,min(ITE,IDE-1)-IOFFSET
        h(i,2)=0.25*(h(i,1)+h(i+1,1)+ &
             h(i,3)+h(i+1,3))
     enddo
  endif


  DBGLINE 'inner boundary; second loop'
  if (JTE .eq. JDE) then
     if (ITE .eq. IDE) then
        IOFFSET=1
     else
        IOFFSET=0
     endif
     do i=its,min(ITE,IDE-1)-IOFFSET
        h(i,(JDE-1)-1)=0.25*(h(i,(JDE-1)-2)+h(i+1,(JDE-1)-2)+ &
             h(i,JDE-1)+h(i+1,JDE-1))
     enddo
  endif

  DBGLINE 'JSTART JEND stuff'
  if (JTS .eq. 1) then
     JSTART=4
  else
     JSTART=JTS+mod(JTS,2) ! needs to be even
  endif

  if (JTE .eq. JDE) then
     JEND=(JDE-1)-3
  else
     JEND=JTE
  endif

  DBGLINE 'ITS==IDS stuff'
  if (ITS .eq. IDS) then

     !  Western Boundary
     do j=JSTART,JEND,2
        h(1,j)=0.25*(h(1,j-1)+h(2,j-1)+ &
             h(1,j+1)+h(2,j+1))

     enddo
  endif


  DBGLINE 'ITE==IDE stuff'
  if (ITE .eq. IDE) then
     !  Eastern Boundary
     do j=JSTART,JEND,2
        h((IDE-1)-1,j)=0.25*(h((IDE-1)-1,j-1)+h((IDE-1),j-1)+        &
             h((IDE-1)-1,j+1)+h((IDE-1),j+1))
     enddo
  endif


END SUBROUTINE boundary_smooth
