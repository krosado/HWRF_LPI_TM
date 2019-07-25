      subroutine calcpwave(prod)
!RMY: Uses Malek Ghantous' method to calculate wave-induced turbulence 
      implicit none
      include 'pom.h'
      real prod(im,jm,kb)
      real pwave(im,jm,kb) ! array to hold wave-induced turbulence
      real zdepth ! z-coordinate in metres (positive up)
      real bwave ! scaling parameter (steepness-dependent?)
      real wk, hs, term1, term2 ! wave parameters, passed from wave model
      integer i,j,k
      integer imws,imwe,jmws,jmwe

      pwave = 0.0
      if(ionedim.ne.1) then !RMY: added ionedim flag
        jmws=2
        jmwe=jmm1
        imws=2
        imwe=imm1
      else
        jmws=1
        jmwe=jm
        imws=1
        imwe=im
      end if !RMY: end of ionedim-controlled if-statement

      do j=jmws,jmwe
        do i=imws,imwe
          if ( mdp(i,j) .gt. 0.0 ) then   ! mdp = lamda/4pi ; lamda is wave length         !BT
            wk =  1.0/( 2.0 * mdp(i,j) )  ! k = 2pi/lamda = 1/(2 mdp); k is wavenumber     !BT
          else
            wk = 0.01                     ! Abnormal values (if any) are limited to 0.01(?)!BT
          endif 
          hs = whs(i,j) 
!          wk = 0.01
!          hs = 2.0
          bwave = 1.25*(wk*hs)**2         !
          term1 = bwave * wk**(2.5)       ! Rearrange terms in the equation
          term2 = sqrt(grav)*hs/2.        ! and  
          do k=2,kbm1                     ! loops to do computation efficiently            !BT
! Malek: add Alex's wave-induced turbulence term (only formulated for
! deep water at this stage)
            zdepth=z(k)*h(i,j)+etf(i,j)*(z(k)+1.)
            pwave(i,j,k)=term1*(term2*exp(wk*zdepth))**3
            prod(i,j,k)=prod(i,j,k)+pwave(i,j,k)
          end do
        end do
      end do

      if(ionedim.ne.1) call exchange3d_mpi(prod(:,:,2:kbm1),im,jm,kbm2)

      return
      end
