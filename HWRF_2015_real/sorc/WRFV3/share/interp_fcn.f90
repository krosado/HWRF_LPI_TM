MODULE module_interp_info
     INTEGER , PARAMETER :: NOT_DEFINED_YET    = 0
     INTEGER , PARAMETER :: BILINEAR           = 1
     INTEGER , PARAMETER :: SINT               = 2
     INTEGER , PARAMETER :: NEAREST_NEIGHBOR   = 3
     INTEGER , PARAMETER :: QUADRATIC          = 4
     INTEGER , PARAMETER :: SPLINE             = 5
     INTEGER , PARAMETER :: SINT_NEW           = 12

     INTEGER             :: interp_method_type = 0
CONTAINS
   SUBROUTINE interp_info_init



     interp_method_type = 2

   END SUBROUTINE interp_info_init
END MODULE module_interp_info







   SUBROUTINE interp_init
      USE module_interp_info
      CALL interp_info_init
   END SUBROUTINE interp_init








   SUBROUTINE interp_mask_land_field ( enable,                   &  
                                       cfld,                     &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &  
                           clu, nlu                              )

      USE module_configure
      USE module_wrf_error
      USE module_dm, only : wrf_dm_sum_reals, wrf_dm_sum_integers

      IMPLICIT NONE
   
   
      LOGICAL, INTENT(IN) :: enable
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             shw,                                  &
                             ipos, jpos,                           &
                             nri, nrj
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
   
      REAL, DIMENSION ( cims:cime, cjms:cjme ) :: clu
      REAL, DIMENSION ( nims:nime, njms:njme ) :: nlu
   
      
   
      INTEGER ci, cj, ck, ni, nj, nk, ip, jp
      INTEGER :: icount , ii , jj , ist , ien , jst , jen , iswater, ierr
      REAL :: avg , sum , dx , dy
      INTEGER , PARAMETER :: max_search = 5
      CHARACTER(LEN=255) :: message
      INTEGER :: icount_n(nkts:nkte), idummy(nkts:nkte)
      REAL :: avg_n(nkts:nkte), sum_n(nkts:nkte), dummy(nkts:nkte)
   
      
   
      CALL nl_get_iswater(1,iswater)

      
   
      IF ( ( .NOT. xstag) .AND. ( .NOT. ystag ) ) THEN

         

       IF ( enable ) THEN

         DO nj = njts, njte
            IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
               cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1 
            ELSE
               cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1 
            END IF
            DO nk = nkts, nkte
               ck = nk
               DO ni = nits, nite
                  IF ( imask(ni, nj) .NE. 1 ) cycle
                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     ci = ( ni + (nri/2)-1 ) / nri + ipos -1 
                  ELSE
                     ci = ( ni + (nri-1)/2 ) / nri + ipos -1 
                  END IF
   



                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  


                  

                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     dx = ( REAL ( MOD ( ni+(nri-1)/2 , nri ) ) + 0.5 ) / REAL ( nri ) 
                  ELSE 
                     dx =   REAL ( MOD ( ni+(nri-1)/2 , nri ) )         / REAL ( nri ) 
                  END IF
                  IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                     dy = ( REAL ( MOD ( nj+(nrj-1)/2 , nrj ) ) + 0.5 ) / REAL ( nrj ) 
                  ELSE 
                     dy =   REAL ( MOD ( nj+(nrj-1)/2 , nrj ) )         / REAL ( nrj ) 
                  END IF
   
                  

                  IF      ( ( NINT(nlu(ni  ,nj  )) .EQ. iswater ) ) THEN
                     nfld(ni,nk,nj) =  cfld(ci  ,ck,cj  )

                  
                  

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .NE. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .NE. iswater ) ) THEN
                     nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * cfld(ci  ,ck,cj  )   + &
                                                             dy   * cfld(ci  ,ck,cj+1) ) + &
                                             dx   * ( ( 1. - dy ) * cfld(ci+1,ck,cj  )   + &
                                                             dy   * cfld(ci+1,ck,cj+1) )

                  
                  

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .EQ. iswater ) ) THEN
                     nfld(ni,nk,nj) = -1

                  
                  
                  ELSE IF ( NINT(nlu(ni  ,nj  )) .NE. iswater ) THEN
                     icount = 0
                     sum = 0
                     IF ( NINT(clu(ci  ,cj  )) .NE. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci+1,cj  )) .NE. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci  ,cj+1)) .NE. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj+1)
                     END IF
                     IF ( NINT(clu(ci+1,cj+1)) .NE. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj+1)
                     END IF
                     nfld(ni,nk,nj) = sum / REAL ( icount ) 
                  END IF
               END DO
            END DO
         END DO


         

         sum_n     = 0
         icount_n  = 0
         DO nj = njts, njte
            DO nk = nkts, nkte
               DO ni = nits, nite
                  IF ( nfld(ni,nk,nj) .NE. -1 ) THEN
                     IF ( NINT(nlu(ni,nj)) .NE. iswater ) THEN
                       icount_n(nk)  = icount_n(nk) + 1
                       sum_n(nk) = sum_n(nk) + nfld(ni,nk,nj)
                     END IF
                  END IF
               END DO
            END DO
         END DO

         CALL wrf_dm_sum_reals(      sum_n(nkts:nkte),  dummy(nkts:nkte))
         sum_n    = dummy
         CALL wrf_dm_sum_integers(icount_n(nkts:nkte), idummy(nkts:nkte))
         icount_n = idummy
         DO nk = nkts, nkte
            IF ( icount_n(nk) .GT. 0 )  &
              avg_n(nk)  = sum_n(nk) / icount_n(nk)
         END DO
       ENDIF

       IF ( enable ) THEN
         IF ( ANY(nfld .EQ. -1) ) THEN

         
         

           DO nj = njts, njte
              DO nk = nkts, nkte
                 DO ni = nits, nite
                    IF ( imask(ni, nj) .NE. 1 ) cycle
                    IF ( nfld(ni,nk,nj) .EQ. -1 ) THEN
                       IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                          cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1 
                       ELSE
                          cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1 
                       END IF
                       IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                          ci = ( ni + (nri/2)-1 ) / nri + ipos -1 
                       ELSE
                          ci = ( ni + (nri-1)/2 ) / nri + ipos -1 
                       END IF
                       ist = MAX (ci-max_search,cits)
                       ien = MIN (ci+max_search,cite,cide-1)
                       jst = MAX (cj-max_search,cjts)
                       jen = MIN (cj+max_search,cjte,cjde-1)
                       icount = 0 
                       sum = 0
                       DO jj = jst,jen
                          DO ii = ist,ien
                             IF ( NINT(clu(ii,jj)) .NE. iswater ) THEN
                                icount = icount + 1
                                sum = sum + cfld(ii,nk,jj)
                             END IF
                          END DO
                       END DO
                       IF ( icount .GT. 0 ) THEN
                          nfld(ni,nk,nj) = sum / REAL ( icount ) 
                       ELSE
                          Write(message,fmt='(a,i4,a,i4,a,f10.4)') &
                            'horizontal interp error - island (', ni, ',', nj, '), using average ', avg_n(nk)
                          CALL wrf_message ( message )
                          nfld(ni,nk,nj) = avg_n(nk)
                       END IF        
                    END IF
                 END DO
              END DO
           END DO
         ENDIF
       ENDIF
      ELSE
         CALL wrf_error_fatal3("<stdin>",283,&
"only unstaggered fields right now" )
      END IF

   END SUBROUTINE interp_mask_land_field

   SUBROUTINE interp_mask_water_field ( enable,                  &  
                                        cfld,                    &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &  
                           clu, nlu, cflag, nflag                )

      USE module_configure
      USE module_wrf_error
      USE module_dm, only : wrf_dm_sum_reals, wrf_dm_sum_integers

      IMPLICIT NONE
   
   
      LOGICAL, INTENT(IN) :: enable
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             shw,                                  &
                             ipos, jpos,                           &
                             nri, nrj, cflag, nflag
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
   
      REAL, DIMENSION ( cims:cime, cjms:cjme ) :: clu
      REAL, DIMENSION ( nims:nime, njms:njme ) :: nlu
   
      
   
      INTEGER ci, cj, ck, ni, nj, nk, ip, jp
      INTEGER :: icount , ii , jj , ist , ien , jst , jen, ierr
      REAL :: avg , sum , dx , dy
      INTEGER , PARAMETER :: max_search = 5
      INTEGER :: icount_n(nkts:nkte), idummy(nkts:nkte)
      REAL :: avg_n(nkts:nkte), sum_n(nkts:nkte), dummy(nkts:nkte)
      CHARACTER(LEN=255) :: message

      
   
      IF ( ( .NOT. xstag) .AND. ( .NOT. ystag ) ) THEN

       IF ( enable ) THEN
         

         DO nj = njts, njte
            IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
               cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1 
            ELSE
               cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1 
            END IF
            DO nk = nkts, nkte
               ck = nk
               DO ni = nits, nite

                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     ci = ( ni + (nri/2)-1 ) / nri + ipos -1 
                  ELSE
                     ci = ( ni + (nri-1)/2 ) / nri + ipos -1 
                  END IF
   



                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  


                  

                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     dx = ( REAL ( MOD ( ni+(nri-1)/2 , nri ) ) + 0.5 ) / REAL ( nri ) 
                  ELSE 
                     dx =   REAL ( MOD ( ni+(nri-1)/2 , nri ) )         / REAL ( nri ) 
                  END IF
                  IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                     dy = ( REAL ( MOD ( nj+(nrj-1)/2 , nrj ) ) + 0.5 ) / REAL ( nrj ) 
                  ELSE 
                     dy =   REAL ( MOD ( nj+(nrj-1)/2 , nrj ) )         / REAL ( nrj ) 
                  END IF
   
                  

                  IF      ( ( NINT(nlu(ni  ,nj  )) .NE. nflag ) ) THEN
                     nfld(ni,nk,nj) = cfld(ci  ,ck,cj  )

                  
                  

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .EQ. nflag ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .EQ. nflag ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .EQ. nflag ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .EQ. nflag ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .EQ. nflag ) ) THEN
                     nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * cfld(ci  ,ck,cj  )   + &
                                                             dy   * cfld(ci  ,ck,cj+1) ) + &
                                             dx   * ( ( 1. - dy ) * cfld(ci+1,ck,cj  )   + &
                                                             dy   * cfld(ci+1,ck,cj+1) )

                  
                  

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .EQ. nflag ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .NE. nflag ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .NE. nflag ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .NE. nflag ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .NE. nflag ) ) THEN
                     nfld(ni,nk,nj) = -4

                  
                  
                  ELSE IF ( NINT(nlu(ni  ,nj  )) .EQ. nflag ) THEN
                     icount = 0
                     sum = 0
                     IF ( NINT(clu(ci  ,cj  )) .EQ. nflag ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci+1,cj  )) .EQ. nflag ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci  ,cj+1)) .EQ. nflag ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj+1)
                     END IF
                     IF ( NINT(clu(ci+1,cj+1)) .EQ. nflag ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj+1)
                     END IF
                     nfld(ni,nk,nj) = sum / REAL ( icount ) 
                  END IF
               END DO
            END DO
         END DO

         

         sum_n     = 0
         icount_n  = 0
         DO nj = njts, njte
            DO nk = nkts, nkte
               DO ni = nits, nite
                  IF ( nfld(ni,nk,nj) .NE. -1 ) THEN
                     IF ( NINT(nlu(ni,nj)) .EQ. nflag ) THEN
                       icount_n(nk)  = icount_n(nk) + 1
                       sum_n(nk) = sum_n(nk) + nfld(ni,nk,nj)
                     END IF
                  END IF
               END DO
            END DO
         END DO

         CALL wrf_dm_sum_reals(      sum_n(nkts:nkte),  dummy(nkts:nkte))
         sum_n    = dummy
         CALL wrf_dm_sum_integers(icount_n(nkts:nkte), idummy(nkts:nkte))
         icount_n = idummy
         DO nk = nkts, nkte
            IF ( icount_n(nk) .GT. 0 )  &
              avg_n(nk)  = sum_n(nk) / icount_n(nk)
         END DO
       ENDIF

       IF ( enable ) THEN
         IF ( ANY(nfld .EQ. -4) ) THEN

           
           

           DO nj = njts, njte
              DO nk = nkts, nkte
                 DO ni = nits, nite

                    IF ( nfld(ni,nk,nj) .EQ. -4 ) THEN
                       IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                          cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1 
                       ELSE
                          cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1 
                       END IF
                       IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                          ci = ( ni + (nri/2)-1 ) / nri + ipos -1 
                       ELSE
                          ci = ( ni + (nri-1)/2 ) / nri + ipos -1 
                       END IF
                       ist = MAX (ci-max_search,cits)
                       ien = MIN (ci+max_search,cite,cide-1)
                       jst = MAX (cj-max_search,cjts)
                       jen = MIN (cj+max_search,cjte,cjde-1)
                       icount = 0 
                       sum = 0
                       DO jj = jst,jen
                          DO ii = ist,ien
                             IF ( NINT(clu(ii,jj)) .EQ. nflag ) THEN
                                icount = icount + 1
                                sum = sum + cfld(ii,nk,jj)
                             END IF
                          END DO
                       END DO
                       IF ( icount .GT. 0 ) THEN
                          nfld(ni,nk,nj) = sum / REAL ( icount ) 
                       ELSE
                         Write(message,fmt='(a,i4,a,i4,a,f10.4)') &
                            'horizontal interp error - lake (', ni, ',', nj, '), using average ', avg_n(nk)
                         CALL wrf_message ( message )                         
                          nfld(ni,nk,nj) = avg_n(nk)
                       END IF        
                    END IF
                 END DO
              END DO
           END DO
         ENDIF
       ENDIF
      ELSE
         CALL wrf_error_fatal3("<stdin>",527,&
"only unstaggered fields right now" )
      END IF

   END SUBROUTINE interp_mask_water_field








  SUBROUTINE force_sst_nmm    (cfld,                                 &  
                               cids, cide, ckds, ckde, cjds, cjde,   &
                               cims, cime, ckms, ckme, cjms, cjme,   &
                               cits, cite, ckts, ckte, cjts, cjte,   &
                               nfld,                                 &  
                               nids, nide, nkds, nkde, njds, njde,   &
                               nims, nime, nkms, nkme, njms, njme,   &
                               nits, nite, nkts, nkte, njts, njte,   &
                               shw,                                  &  
                               imask,                                &  
                               xstag, ystag,                         &  
                               ipos, jpos,                           &  
                               nri, nrj,                             &  
                               CII, IIH, CJJ, JJH, CBWGT1, HBWGT1,   &  
                               CBWGT2, HBWGT2, CBWGT3, HBWGT3,       &  
                               CBWGT4, HBWGT4, CCSST, CSST           )  
     USE module_timing
     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme ) :: nfld
     REAL,    DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CBWGT1,CBWGT2,CBWGT3,CBWGT4    
     REAL,    DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIH,JJH
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
     INTEGER , INTENT(IN) :: csst(*), ccsst(*)


     LOGICAL  FLIP
     INTEGER  i,j,k,n
     REAL     SUM,AMAXVAL
     REAL,    DIMENSION (4, nims:nime, njms:njme ) :: NBWGT

     if(csst(1) /= 1) return
















     DO J=NJTS,MIN(NJTE,NJDE-1)
      DO I=NITS,MIN(NITE,NIDE-1)
            NBWGT(1,I,J)=HBWGT1(I,J)
            NBWGT(2,I,J)=HBWGT2(I,J)
            NBWGT(3,I,J)=HBWGT3(I,J)
            NBWGT(4,I,J)=HBWGT4(I,J)
      ENDDO
     ENDDO

     DO J=NJTS,MIN(NJTE,NJDE-1)
      DO I=NITS,MIN(NITE,NIDE-1)
          AMAXVAL=0.
          DO N=1,4
            AMAXVAL=amax1(NBWGT(N,I,J),AMAXVAL)
          ENDDO

          FLIP=.TRUE.
          SUM=0.0
          DO N=1,4
             IF(AMAXVAL .EQ. NBWGT(N,I,J) .AND. FLIP)THEN
               NBWGT(N,I,J)=1.0
               FLIP=.FALSE.
             ELSE
               NBWGT(N,I,J)=0.0
             ENDIF
             SUM=SUM+NBWGT(N,I,J)
             IF(SUM .GT. 1.0)CALL wrf_error_fatal3("<stdin>",628,&
"horizontal interp error - interp_hnear_nmm" )
          ENDDO
      ENDDO
     ENDDO

       DO J=NJTS,MIN(NJTE,NJDE-1)
        DO I=NITS,MIN(NITE,NIDE-1)
            IF(MOD(JJH(I,J),2) .NE. 0)THEN    
                NFLD(I,J) = NBWGT(1,I,J)*CFLD(IIH(I,J),  JJH(I,J)  ) &
                          + NBWGT(2,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ) &
                          + NBWGT(3,I,J)*CFLD(IIH(I,J),  JJH(I,J)-1) &
                          + NBWGT(4,I,J)*CFLD(IIH(I,J),  JJH(I,J)+1)
            ELSE
                NFLD(I,J) = NBWGT(1,I,J)*CFLD(IIH(I,J),  JJH(I,J)  ) &
                          + NBWGT(2,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ) &
                          + NBWGT(3,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)-1) &
                          + NBWGT(4,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)+1)
            ENDIF
        ENDDO
       ENDDO


     END SUBROUTINE force_sst_nmm

   SUBROUTINE nmm_smoother_ikj ( cfld , &
                             cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             xstag, ystag,                         &
                             ipos, jpos,                           &
                             nri, nrj                              &
                             )

      USE module_configure
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             nri, nrj,                             &
                             ipos, jpos
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(INOUT) :: cfld
      LOGICAL, INTENT(IN) :: xstag, ystag


      

      INTEGER             :: feedback
      INTEGER, PARAMETER  :: smooth_passes = 5

      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfldnew
      INTEGER :: ci, cj, ck
      INTEGER :: is, npass
      REAL    :: AVGH
      CHARACTER (LEN=256) :: a_message

      RETURN
      

      CALL nl_get_feedback       ( 1, feedback  )
      IF ( feedback == 0 ) RETURN

      WRITE(a_message,*)'SIMPLE SMOOTHER IS SWITCHED ON FOR HEIGHT'
      CALL wrf_message ( a_message )                       

      DO npass = 1, smooth_passes

      DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  
       if(mod(cj,2) .eq. 0)THEN
        is=0 
       else
        is=1 
       endif
       DO ck = ckts, ckte
        DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) 
            IF(IS==0)THEN    
             AVGH = CFLD(CI,CK,CJ+1) + CFLD(CI,CK,CJ-1) + CFLD(CI+1,CK,CJ+1) + CFLD(CI+1,CK,CJ-1)
            ELSE
             AVGH = CFLD(CI,CK,CJ+1) + CFLD(CI,CK,CJ-1) + CFLD(CI-1,CK,CJ+1) + CFLD(CI-1,CK,CJ-1)
            ENDIF
            CFLDNEW(CI,CK,CJ) = (AVGH + 4*CFLD(CI,CK,CJ)) / 8.0
        ENDDO
       ENDDO
      ENDDO

      DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  
       if(mod(cj,2) .eq. 0)THEN
        is=0 
       else
        is=1 
       endif
       DO ck = ckts, ckte
        DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) 
           CFLD(CI,CK,CJ) = CFLDNEW(CI,CK,CJ)
        ENDDO
       ENDDO
      ENDDO

      ENDDO   

   END SUBROUTINE nmm_smoother_ikj


   SUBROUTINE nmm_smoother_ijk ( cfld , &
                             cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             xstag, ystag,                         &
                             ipos, jpos,                           &
                             nri, nrj                              &
                             )

      USE module_configure
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             nri, nrj,                             &
                             ipos, jpos
      REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ), INTENT(INOUT) :: cfld
      LOGICAL, INTENT(IN) :: xstag, ystag


      

      INTEGER             :: feedback
      INTEGER, PARAMETER  :: smooth_passes = 5

      REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfldnew
      INTEGER :: ci, cj, ck
      INTEGER :: is, npass
      REAL    :: AVGH
      CHARACTER (LEN=256) :: a_message

      RETURN
      

      CALL nl_get_feedback       ( 1, feedback  )
      IF ( feedback == 0 ) RETURN

      WRITE(a_message,*)'SIMPLE SMOOTHER IS SWITCHED ON FOR HEIGHT'
      CALL wrf_message ( a_message )                       

      DO npass = 1, smooth_passes

      DO ck = ckts, ckte
       DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  
        if(mod(cj,2) .eq. 0)THEN
         is=0 
        else
         is=1 
        endif
        DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) 
            IF(IS==0)THEN    
             AVGH = CFLD(CI,CJ+1,CK) + CFLD(CI,CJ-1,CK) + CFLD(CI+1,CJ+1,CK) + CFLD(CI+1,CJ-1,CK)
            ELSE
             AVGH = CFLD(CI,CJ+1,CK) + CFLD(CI,CJ-1,CK) + CFLD(CI-1,CJ+1,CK) + CFLD(CI-1,CJ-1,CK)
            ENDIF
            CFLDNEW(CI,CJ,CK) = (AVGH + 4*CFLD(CI,CJ,CK)) / 8.0
        ENDDO
       ENDDO
      ENDDO

      DO ck = ckts, ckte
       DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  
        if(mod(cj,2) .eq. 0)THEN
         is=0 
        else
         is=1 
        endif
        DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) 
           CFLD(CI,CJ,CK) = CFLDNEW(CI,CJ,CK)
        ENDDO
       ENDDO
      ENDDO

      ENDDO   

   END SUBROUTINE nmm_smoother_ijk


   SUBROUTINE nmm_vsmoother_ikj ( cfld , &
                             cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             xstag, ystag,                         &
                             ipos, jpos,                           &
                             nri, nrj                              &
                             )

      USE module_configure
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             nri, nrj,                             &
                             ipos, jpos
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(INOUT) :: cfld
      LOGICAL, INTENT(IN) :: xstag, ystag


      

      INTEGER             :: feedback
      INTEGER, PARAMETER  :: smooth_passes = 5

      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfldnew
      INTEGER :: ci, cj, ck
      INTEGER :: is, npass
      REAL    :: AVGV
      CHARACTER (LEN=256) :: a_message

      RETURN
      

      CALL nl_get_feedback       ( 1, feedback  )
      IF ( feedback == 0 ) RETURN

      WRITE(a_message,*)'SIMPLE SMOOTHER IS SWITCHED ON FOR VELOCITY'
      CALL wrf_message ( a_message )                       

      DO npass = 1, smooth_passes

      DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  
       if(mod(cj,2) .eq. 0)THEN
        is=1 
       else
        is=0 
       endif
       DO ck = ckts, ckte
        DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) 
            IF(IS==0)THEN    
             AVGV = CFLD(CI,CK,CJ+1) + CFLD(CI,CK,CJ-1) + CFLD(CI+1,CK,CJ+1) + CFLD(CI+1,CK,CJ-1)
            ELSE
             AVGV = CFLD(CI,CK,CJ+1) + CFLD(CI,CK,CJ-1) + CFLD(CI-1,CK,CJ+1) + CFLD(CI-1,CK,CJ-1)
            ENDIF
            CFLDNEW(CI,CK,CJ) = (AVGV + 4*CFLD(CI,CK,CJ)) / 8.0
        ENDDO
       ENDDO
      ENDDO

      DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  
       if(mod(cj,2) .eq. 0)THEN
        is=1 
       else
        is=0 
       endif
       DO ck = ckts, ckte
        DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) 
           CFLD(CI,CK,CJ) = CFLDNEW(CI,CK,CJ)
        ENDDO
       ENDDO
      ENDDO

      ENDDO

   END SUBROUTINE nmm_vsmoother_ikj








   
   subroutine NoInterpMany(cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj,                             &  
        cpint,npint, cpd,npd, cq,nq, ct,nt,   &
        cfis,nfis)
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK

     

     REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(IN)           :: CFLD,cpint,ct,cq
     REAL,DIMENSION(cims:cime,cjms:cjme),             INTENT(IN)           :: cpd,cfis

     

     REAL,DIMENSION(nims:nime,njms:njme),             INTENT(IN)           :: nFIS,npd
     REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(IN)           :: NFLD,npint,nt,nq

   end subroutine NoInterpMany

   subroutine DownAged2D(junk,                &
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj, &
        c_age,n_age, cfld)

     use module_interp_nmm, only: c2n_copy2d_nomask
     use module_interp_store
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     integer, intent(in) :: c_age
     integer, intent(inout) :: n_age

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme),   INTENT(IN)           :: CFLD,junk
     REAL,DIMENSION(nims:nime,njms:njme),   INTENT(INOUT)        :: nfld

     logical bad
     integer i,j
     
     
     
     if(n_age==c_age .and. n_age/=0 .and. c_age/=0) then
        
        
        return
     end if
     n_age=c_age
     

     call c2n_copy2d_nomask(IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4,  &
          cfld,nfld,                &
          cims, cime, cjms, cjme,   &
          nids, nide, njds, njde,   &
          nims, nime, njms, njme,   &
          nits, nite, njts, njte, .true.)

   end subroutine DownAged2D

   subroutine ForceNearSST   (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj, &
        cactivate, nactivate)
     use module_interp_nmm, only: c2n_sst
     use module_interp_store
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     integer, intent(In), dimension(*) :: cactivate, nactivate

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme),   INTENT(IN)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme),   INTENT(INOUT)        :: nfld

     if(nactivate(1)/=1) return

     call c2n_sst(hnear_i,hnear_j,          &
          cfld,nfld,                &
          cims, cime, cjms, cjme,   &
          nids, nide, njds, njde,   &
          nims, nime, njms, njme,   &
          nits, nite, njts, njte)
   end subroutine ForceNearSST

   subroutine DownNear      (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj)                                
     use module_interp_nmm, only: c2n_near2d, c2n_near3d
     use module_interp_store
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme),   INTENT(IN)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme),   INTENT(INOUT)          :: nfld

     if(nkts==nkte) then
        call c2n_near2d(hnear_i,hnear_j,   &
             cfld,nfld,imask,                       &
             cims, cime, cjms, cjme,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte)
     else
        call c2n_near3d(hnear_i,hnear_j,   &
             cfld,nfld,imask,                       &
             cims, cime, cjms, cjme, ckms, ckme,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte)
     endif
   end subroutine DownNear

   subroutine DownNearIKJ   (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj)                                
     use module_interp_nmm, only: c2n_near3dikj
     use module_interp_store
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,ckms:ckme,cjms:cjme),   INTENT(IN)           :: CFLD
     REAL,DIMENSION(nims:nime,nkms:nkme,njms:njme),   INTENT(INOUT)          :: nfld
     
     if(nkts==nkte) &
          call wrf_error_fatal3("<stdin>",1127,&
'IJ interpolation of an IKJ variable is not supported and makes no sense anyway.  Use DownNear instead.')

     call c2n_near3dikj(hnear_i,hnear_j,   &
          cfld,nfld,imask,                       &
          cims, cime, cjms, cjme, ckms, ckme,  &
          nids, nide, njds, njde, nkds, nkde,  &
          nims, nime, njms, njme, nkms, nkme,  &
          nits, nite, njts, njte, nkts, nkte)
   end subroutine DownNearIKJ

   subroutine UpNear(cfld,                    &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj)                                
     use module_interp_store
     use module_interp_nmm, only: n2c_near2d
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme),   INTENT(OUT)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme),   INTENT(IN)          :: nfld

     if(nkts/=nkte) &
          call wrf_error_fatal3("<stdin>",1168,&
'Up nearest neighbor interpolation is not implemented.')

     call n2c_near2d( cfld,nfld,ipos,jpos,                  &
          cids, cide, cjds, cjde,   &
          cims, cime, cjms, cjme,   &
          cits, cite, cjts, cjte,   &
          nids, nide, njds, njde,   &
          nims, nime, njms, njme,   &
          nits, nite, njts, njte)
   end subroutine UpNear

   subroutine DownINear      (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj)                                
     use module_interp_nmm, only: c2n_inear2d
     use module_interp_store
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     INTEGER,DIMENSION(cims:cime,cjms:cjme),   INTENT(IN)           :: CFLD
     INTEGER,DIMENSION(nims:nime,njms:njme),   INTENT(INOUT)          :: nfld

     if(nkts/=nkte) &
          call wrf_error_fatal3("<stdin>",1210,&
'3D integer nearest neighbor interpolation is not implemented.')

     call c2n_inear2d(hnear_i,hnear_j,   &
          cfld,nfld,imask,                       &
          cims, cime, cjms, cjme,   &
          nids, nide, njds, njde,   &
          nims, nime, njms, njme,   &
          nits, nite, njts, njte)
   end subroutine DownINear

   subroutine UpINear        (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj)                                
     use module_interp_store
     use module_interp_nmm, only: n2c_inear2d
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     INTEGER,DIMENSION(cims:cime,cjms:cjme),   INTENT(OUT)           :: CFLD
     INTEGER,DIMENSION(nims:nime,njms:njme),   INTENT(IN)          :: nfld

     if(nkts/=nkte) &
          call wrf_error_fatal3("<stdin>",1251,&
'3D integer nearest neighbor interpolation is not implemented.')

     call n2c_inear2d( cfld,nfld,ipos,jpos,                  &
          cids, cide, cjds, cjde,   &
          cims, cime, cjms, cjme,   &
          cits, cite, cjts, cjte,   &
          nids, nide, njds, njde,   &
          nims, nime, njms, njme,   &
          nits, nite, njts, njte)
   end subroutine UpINear

   subroutine DownMass      (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj,                             &  
        emethod, evalue)                         
     use module_interp_nmm, only: c2n_mass, c2n_copy2d
     use module_interp_store
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj, emethod
     real, intent(in) :: evalue

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(IN)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(INOUT)          :: nfld

     if(nkts==nkte) then
        call c2n_copy2d(IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4,  &
             cfld,nfld,imask,                &
             cims, cime, cjms, cjme,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .true.)
     else
        call c2n_mass(IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4,   &
             cfld,nfld,iinfo,winfo,imask,          &
             emethod,evalue,                       &
             cims, cime, cjms, cjme, ckms, ckme,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte)
     endif
   end subroutine DownMass

   subroutine DownMassIKJ   (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj,                             &  
        emethod, evalue)                         
     use module_interp_nmm, only: c2n_massikj
     use module_interp_store
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj,emethod
     real, intent(in) :: evalue

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,ckms:ckme,cjms:cjme),   INTENT(IN)           :: CFLD
     REAL,DIMENSION(nims:nime,nkms:nkme,njms:njme),   INTENT(INOUT)          :: nfld

     if(nkts==nkte) &
          call wrf_error_fatal3("<stdin>",1344,&
'IKJ 2D interpolation of an IJ array is not implemented (and makes no sense anyway).  Use DownCopy instead.')
     call c2n_massikj(IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4,   &
          cfld,nfld,iinfo,winfo,imask,          &
          emethod, evalue,                      &
          cims, cime, cjms, cjme, ckms, ckme,   &
          nids, nide, njds, njde, nkds, nkde,   &
          nims, nime, njms, njme, nkms, nkme,   &
          nits, nite, njts, njte, nkts, nkte)
   end subroutine DownMassIKJ

   subroutine UpMassIKJ     (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj,                             &  
        emethod, evalue)                         
     use module_interp_store
     use module_interp_nmm, only: n2c_massikj, n2c_copy2d
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj, emethod
     real, intent(in) :: evalue

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,ckms:ckme,cjms:cjme),   INTENT(OUT)           :: CFLD
     REAL,DIMENSION(nims:nime,nkms:nkme,njms:njme),   INTENT(IN)          :: nfld

     if(nkts==nkte) then
        call n2c_copy2d(&
             cfld,nfld,ipos,jpos,                   &
             cids, cide, cjds, cjde,   &
             cims, cime, cjms, cjme,   &
             cits, cite, cjts, cjte,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .true.)
     else
        call n2c_massikj(&
             cfld,nfld,parent_iinfo,parent_winfo,  &
             ipos,jpos,emethod, evalue,            &
             cids, cide, cjds, cjde, ckds, ckde,   &
             cims, cime, cjms, cjme, ckms, ckme,   &
             cits, cite, cjts, cjte, ckts, ckte,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte)
     endif
   end subroutine UpMassIKJ

   subroutine UpMass        (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj,                             &  
        emethod, evalue)                         
     use module_interp_store
     use module_interp_nmm, only: n2c_mass, n2c_copy2d
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj, emethod
     real, intent(in) :: evalue

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(OUT)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(IN)          :: nfld

     if(nkts==nkte) then
        call n2c_copy2d(&
             cfld,nfld,ipos,jpos,                   &
             cids, cide, cjds, cjde,   &
             cims, cime, cjms, cjme,   &
             cits, cite, cjts, cjte,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .true.)
     else
        call n2c_mass(&
             cfld,nfld,parent_iinfo,parent_winfo,  &
             ipos,jpos,emethod, evalue,            &
             cids, cide, cjds, cjde, ckds, ckde,   &
             cims, cime, cjms, cjme, ckms, ckme,   &
             cits, cite, cjts, cjte, ckts, ckte,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte)
     endif
   end subroutine UpMass

   subroutine UpCopy         (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj)                                

     use module_interp_nmm, only: n2c_copy3d, n2c_copy2d
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(OUT)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(IN)          :: nfld

     if(nkts==nkte) then
        call n2c_copy2d(&
             cfld,nfld,ipos,jpos,                   &
             cids, cide, cjds, cjde,   &
             cims, cime, cjms, cjme,   &
             cits, cite, cjts, cjte,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .true.)
     else
        call n2c_copy3d(&
             cfld,nfld,ipos,jpos,                   &
             cids, cide, cjds, cjde, ckds, ckde,   &
             cims, cime, cjms, cjme, ckms, ckme,   &
             cits, cite, cjts, cjte, ckts, ckte,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte, .true.)
     endif
   end subroutine UpCopy

   subroutine UpMax          (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj)                                

     use module_interp_nmm, only: n2c_max3d, n2c_max2d
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(OUT)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(IN)          :: nfld

     if(nkts==nkte) then
        call n2c_max2d(&
             cfld,nfld,ipos,jpos,                   &
             cids, cide, cjds, cjde,   &
             cims, cime, cjms, cjme,   &
             cits, cite, cjts, cjte,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .true.)
     else
        call n2c_max3d(&
             cfld,nfld,ipos,jpos,                   &
             cids, cide, cjds, cjde, ckds, ckde,   &
             cims, cime, cjms, cjme, ckms, ckme,   &
             cits, cite, cjts, cjte, ckts, ckte,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte, .true.)
     endif
   end subroutine UpMax

   subroutine DownCopy      (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj)                                
     use module_interp_store
     use module_interp_nmm, only: c2n_copy3d, c2n_copy2d
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(IN)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(INOUT)          :: nfld

     if(nkts==nkte) then
        call c2n_copy2d(IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4,  &
             cfld,nfld,imask,                &
             cims, cime, cjms, cjme,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .true.)
     else
        call c2n_copy3d(IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4,  &
             cfld,nfld,imask,                &
             cims, cime, cjms, cjme, ckms, ckme,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte, .true.)
     endif
   end subroutine DownCopy

   subroutine UpVel   (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj)                                

     use module_interp_nmm, only: n2c_copy3d, n2c_copy2d
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(OUT)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(IN)          :: nfld

     if(nkts==nkte) then
        call n2c_copy2d( cfld,nfld,ipos,jpos,             &
             cids, cide, cjds, cjde,   &
             cims, cime, cjms, cjme,   &
             cits, cite, cjts, cjte,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .false.)
     else
        call n2c_copy3d(&
             cfld,nfld,ipos,jpos,                   &
             cids, cide, cjds, cjde, ckds, ckde,   &
             cims, cime, cjms, cjme, ckms, ckme,   &
             cits, cite, cjts, cjte, ckts, ckte,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte, .false.)
     endif
   end subroutine UpVel

   subroutine DownVel      (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj)                                
     use module_interp_store
     use module_interp_nmm, only: c2n_copy3d, c2n_copy2d
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(IN)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(INOUT)          :: nfld

     if(nkts==nkte) then
        call c2n_copy2d(IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4,  &
             cfld,nfld,imask,                &
             cims, cime, cjms, cjme,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .false.)
     else
        call c2n_copy3d(IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4,  &
             cfld,nfld,imask,                &
             cims, cime, cjms, cjme, ckms, ckme,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte, .false.)
     endif
   end subroutine DownVel

   SUBROUTINE BdyMass (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj,                             &  
        c_bxs,n_bxs,                          &
        c_bxe,n_bxe,                          &
        c_bys,n_bys,                          &
        c_bye,n_bye,                          &
        c_btxs,n_btxs,                        &
        c_btxe,n_btxe,                        &
        c_btys,n_btys,                        &
        c_btye,n_btye,                        &
        emethod,evalue)                          

     
     USE module_configure
     USE module_wrf_error
     use module_interp_store
     use module_interp_nmm, only: c2b_mass, c2b_copy2d

     IMPLICIT NONE
     integer, parameter :: bdyw = 1

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,                                  &
          ipos, jpos,                           &
          nri, nrj, emethod
     real, intent(in) :: evalue

     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld,ccwm
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld,ncwm
     
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     
     real,dimension(nims:nime,nkms:nkme,bdyw) :: n_bys,n_bye
     real,dimension(njms:njme,nkms:nkme,bdyw) :: n_bxs,n_bxe

     
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_bxs,c_bxe,c_bys,c_bye
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_btxs,n_btxs,c_btxe,n_btxe,c_btys,n_btys,c_btye,n_btye

     if(nkts==nkte) then
        call c2b_copy2d(iih,jjh,                   &
             hbwgt1,hbwgt2,hbwgt3,hbwgt4,          &
             cfld,                                 &
             n_bxs, n_bxe, n_bys, n_bye,           &
             cims, cime, cjms, cjme,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte,   &
             .true.)
     else
        call c2b_mass(iih,jjh,                     &
             hbwgt1,hbwgt2,hbwgt3,hbwgt4,          &
             cfld,                                 &
             iinfo_bxs,iinfo_bxe,iinfo_bys,iinfo_bye,          &
             winfo_bxs,winfo_bxe,winfo_bys,winfo_bye,          &
             n_bxs, n_bxe, n_bys, n_bye,           &
             emethod, evalue,                      &
             cims, cime, cjms, cjme, ckms, ckme,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte)
     endif
   END SUBROUTINE BdyMass

   
   
   
   SUBROUTINE BdyCopy  (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj,                             &  
        c_bxs,n_bxs,                          &
        c_bxe,n_bxe,                          &
        c_bys,n_bys,                          &
        c_bye,n_bye,                          &
        c_btxs,n_btxs,                        &
        c_btxe,n_btxe,                        &
        c_btys,n_btys,                        &
        c_btye,n_btye)

     
     USE module_configure
     USE module_wrf_error
     use module_interp_nmm, only: c2b_copy3d, c2b_copy2d
     use module_interp_store

     IMPLICIT NONE
     integer, parameter :: bdyw = 1

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,                                  &
          ipos, jpos,                           &
          nri, nrj

     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld
     
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     
     real,dimension(nims:nime,nkms:nkme,bdyw) :: n_bys,n_bye
     real,dimension(njms:njme,nkms:nkme,bdyw) :: n_bxs,n_bxe

     
     
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_bxs,c_bxe,c_bys,c_bye
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_btxs,n_btxs,c_btxe,n_btxe,c_btys,n_btys,c_btye,n_btye

     if(nkts==nkte) then
        call c2b_copy2d(iiv,jjv,                   &
             vbwgt1,vbwgt2,vbwgt3,vbwgt4,          &
             cfld,                                 &
             n_bxs, n_bxe, n_bys, n_bye,           &
             cims, cime, cjms, cjme,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .false.)
     else
        call c2b_copy3d(iih,jjh,                     &
             hbwgt1,hbwgt2,hbwgt3,hbwgt4,          &
             cfld,                                 &
             n_bxs, n_bxe, n_bys, n_bye,           &
             cims, cime, cjms, cjme, ckms, ckme,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte, .false.)
     endif

   END SUBROUTINE BdyCopy
   
   
   
   subroutine NoInterp()
   end subroutine NoInterp
   
   
   
   SUBROUTINE BdyVel  (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj,                             &  
        c_bxs,n_bxs,                          &
        c_bxe,n_bxe,                          &
        c_bys,n_bys,                          &
        c_bye,n_bye,                          &
        c_btxs,n_btxs,                        &
        c_btxe,n_btxe,                        &
        c_btys,n_btys,                        &
        c_btye,n_btye)

     
     USE module_configure
     USE module_wrf_error
     use module_interp_nmm, only: c2b_copy3d, c2b_copy2d
     use module_interp_store

     IMPLICIT NONE
     integer, parameter :: bdyw = 1

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,                                  &
          ipos, jpos,                           &
          nri, nrj

     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld
     
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     
     real,dimension(nims:nime,nkms:nkme,bdyw) :: n_bys,n_bye
     real,dimension(njms:njme,nkms:nkme,bdyw) :: n_bxs,n_bxe

     
     
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_bxs,c_bxe,c_bys,c_bye
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_btxs,n_btxs,c_btxe,n_btxe,c_btys,n_btys,c_btye,n_btye

     if(nkts==nkte) then
        call c2b_copy2d(iiv,jjv,                     &
             vbwgt1,vbwgt2,vbwgt3,vbwgt4,          &
             cfld,                                 &
             n_bxs, n_bxe, n_bys, n_bye,           &
             cims, cime, cjms, cjme,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .false.)
     else
        call c2b_copy3d(iiv,jjv,                     &
             vbwgt1,vbwgt2,vbwgt3,vbwgt4,          &
             cfld,                                 &
             n_bxs, n_bxe, n_bys, n_bye,           &
             cims, cime, cjms, cjme, ckms, ckme,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte, .false.)
     endif
   END SUBROUTINE BdyVel
   
   
   
   SUBROUTINE BdyNear (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj,                             &  
        c_bxs,n_bxs,                          &
        c_bxe,n_bxe,                          &
        c_bys,n_bys,                          &
        c_bye,n_bye,                          &
        c_btxs,n_btxs,                        &
        c_btxe,n_btxe,                        &
        c_btys,n_btys,                        &
        c_btye,n_btye)

     
     USE module_configure
     USE module_wrf_error
     use module_interp_nmm, only: c2b_near2d
     use module_interp_store

     IMPLICIT NONE
     integer, parameter :: bdyw = 1

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,                                  &
          ipos, jpos,                           &
          nri, nrj

     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme ) :: nfld

     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     
     real,dimension(nims:nime,1,bdyw) :: n_bys,n_bye
     real,dimension(njms:njme,1,bdyw) :: n_bxs,n_bxe

     
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_bxs,c_bxe,c_bys,c_bye
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_btxs,n_btxs,c_btxe,n_btxe,c_btys,n_btys,c_btye,n_btye

     if(nkts/=nkte) &
          call wrf_error_fatal3("<stdin>",2011,&
'3D boundary nearest neighbor interpolation is not implemented.')
     call c2b_near2d(hnear_i,hnear_j,cfld,      &
          n_bxs, n_bxe, n_bys, n_bye,           &
          cims, cime, cjms, cjme,   &
          nids, nide, njds, njde,   &
          nims, nime, njms, njme,   &
          nits, nite, njts, njte)

   END SUBROUTINE BdyNear

   SUBROUTINE BdyINear (cfld,                                 &  
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  
        imask,                                &  
        xstag, ystag,                         &  
        ipos, jpos,                           &  
        nri, nrj,                             &  
        c_bxs,n_bxs,                          &
        c_bxe,n_bxe,                          &
        c_bys,n_bys,                          &
        c_bye,n_bye,                          &
        c_btxs,n_btxs,                        &
        c_btxe,n_btxe,                        &
        c_btys,n_btys,                        &
        c_btye,n_btye)

     
     USE module_configure
     USE module_wrf_error
     use module_interp_nmm, only: c2b_inear2d
     use module_interp_store

     IMPLICIT NONE
     integer, parameter :: bdyw = 1

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,                                  &
          ipos, jpos,                           &
          nri, nrj

     LOGICAL, INTENT(IN) :: xstag, ystag

     INTEGER, DIMENSION ( cims:cime, cjms:cjme ) :: cfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: nfld

     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     
     integer,dimension(nims:nime,1,bdyw) :: n_bys,n_bye
     integer,dimension(njms:njme,1,bdyw) :: n_bxs,n_bxe

     
     integer,  DIMENSION( * ), INTENT(INOUT) :: c_bxs,c_bxe,c_bys,c_bye
     integer,  DIMENSION( * ), INTENT(INOUT) :: c_btxs,n_btxs,c_btxe,n_btxe,c_btys,n_btys,c_btye,n_btye

     if(nkts/=nkte) &
          call wrf_error_fatal3("<stdin>",2079,&
'3D boundary nearest neighbor interpolation is not implemented.')
     call c2b_inear2d(hnear_i,hnear_j,cfld,      &
          n_bxs, n_bxe, n_bys, n_bye,           &
          cims, cime, cjms, cjme,   &
          nids, nide, njds, njde,   &
          nims, nime, njms, njme,   &
          nits, nite, njts, njte)

   END SUBROUTINE BdyINear








