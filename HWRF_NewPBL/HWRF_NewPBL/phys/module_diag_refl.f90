


MODULE module_diag_refl
CONTAINS


   SUBROUTINE diagnostic_output_calc_refl(                            &
                      ids,ide, jds,jde, kds,kde,                      &
                      ims,ime, jms,jme, kms,kme,                      &
                      its,ite, jts,jte, kts,kte,                      & 
                      diagflag,                                       &
                      refd_max,refl_10cm                              &
                                                                     )



   IMPLICIT NONE




























   INTEGER,      INTENT(IN   )    ::                             &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                      its,ite, jts,jte, kts,kte

   LOGICAL,   INTENT(IN   )    ::   diagflag


   INTEGER :: i,j,k

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) ::   &
                                                      refl_10cm

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) ::        &
                                                       refd_max

       DO j=jts,jte
       DO i=its,ite
         refd_max(i,j)    = -35.
       ENDDO
       ENDDO

     DO j=jts,jte
     DO k=kts,kte
     DO i=its,ite



       IF ( refl_10cm(i,k,j) .GT. refd_max(i,j) ) THEN
         refd_max(i,j) = refl_10cm(i,k,j)
       ENDIF
     ENDDO
     ENDDO
     ENDDO
!  !$OMP END PARALLEL DO


   END SUBROUTINE diagnostic_output_calc_refl




END MODULE module_diag_refl
