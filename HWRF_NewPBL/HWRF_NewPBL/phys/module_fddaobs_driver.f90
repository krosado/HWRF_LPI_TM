
MODULE module_fddaobs_driver































 
CONTAINS


SUBROUTINE fddaobs_driver( inest, domid, parid, restart,         &
               config_flags,                                     &
               nudge_opt, iprt_errob, iprt_nudob,                &
               fdasta, fdaend,                                   &
               nudge_wind, nudge_temp, nudge_mois,               &
               nudge_pstr,                                       &
               coef_wind, coef_temp, coef_mois,                  &
               coef_pstr, rinxy, rinsig,                         &
               npfi, ionf,                                       &
               obs_prt_max, obs_prt_freq, idynin, dtramp,        &
               parent_grid_ratio, maxdom, itimestep,             &
               xtime,                                            &
               dt, gmt, julday,                                  &



               max_obs, nobs_ndg_vars,                           &
               nobs_err_flds, nstat, varobs, errf, dx,           &
               KPBL, HT, mut, muu, muv,                          &
               msftx, msfty, msfux, msfuy, msfvx, msfvy, p_phy, t_tendf, t0,             &
               ub, vb, tb, qvb, pbase, ptop, pp, phb, ph,        &
               uratx, vratx, tratx, ru_tendf, rv_tendf,          &
               moist_tend, savwt,                                &
               regime, pblh, z_at_w,                             &
               z,                                                &
               ids,ide, jds,jde, kds,kde,                        & 
               ims,ime, jms,jme, kms,kme,                        & 
               its,ite, jts,jte, kts,kte                         ) 


  USE module_domain
  
  USE module_model_constants, ONLY : g, rcp
  USE module_fddaobs_rtfdda






  IMPLICIT NONE












  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde  
  INTEGER, intent(in)  :: ims,ime, jms,jme, kms,kme  
  INTEGER, intent(in)  :: its,ite, jts,jte, kts,kte  

  INTEGER, intent(in)  :: inest
  INTEGER, intent(in)  :: maxdom
  INTEGER, intent(in)  :: domid(maxdom)           
  INTEGER, intent(in)  :: parid(maxdom)           
  LOGICAL, intent(in)  :: restart
  TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
  INTEGER, intent(in)  :: itimestep
  INTEGER, intent(in)  :: nudge_opt
  LOGICAL, intent(in)  :: iprt_errob 
  LOGICAL, intent(in)  :: iprt_nudob 
  REAL, intent(in)     :: fdasta
  REAL, intent(in)     :: fdaend
  INTEGER, intent(in)  :: nudge_wind
  INTEGER, intent(in)  :: nudge_temp
  INTEGER, intent(in)  :: nudge_mois
  INTEGER, intent(in)  :: nudge_pstr
  REAL, intent(in) :: coef_wind
  REAL, intent(in) :: coef_temp
  REAL, intent(in) :: coef_mois
  REAL, intent(in) :: coef_pstr
  REAL, intent(inout)  :: rinxy
  REAL, intent(inout)  :: rinsig
  INTEGER, intent(in) :: npfi
  INTEGER, intent(in) :: ionf
  INTEGER, intent(in) :: obs_prt_max      
  INTEGER, intent(in) :: obs_prt_freq     
  INTEGER, intent(in) :: idynin
  REAL, intent(inout) :: dtramp
  INTEGER, intent(in) :: parent_grid_ratio
  REAL, intent(in)     :: xtime           
  REAL, intent(in)     :: dt
  REAL, intent(in)     :: gmt
  INTEGER, intent(in)  :: julday
  INTEGER, intent(in)  :: max_obs         
  INTEGER, intent(in)  :: nobs_ndg_vars
  INTEGER, intent(in)  :: nobs_err_flds
  INTEGER, intent(in)  :: nstat
  REAL, intent(inout)  :: varobs(nobs_ndg_vars, max_obs)
  REAL, intent(inout)  :: errf(nobs_err_flds, max_obs)
  REAL, intent(in)     :: dx           
  INTEGER, INTENT(IN) :: kpbl( ims:ime, jms:jme ) 
  REAL, INTENT(IN) :: ht( ims:ime, jms:jme )
  REAL, INTENT(IN) :: mut( ims:ime , jms:jme )   
  REAL, INTENT(IN) :: muu( ims:ime , jms:jme )   
  REAL, INTENT(IN) :: muv( ims:ime , jms:jme )   
  REAL, INTENT(IN) :: msftx( ims:ime , jms:jme )  
  REAL, INTENT(IN) :: msfty( ims:ime , jms:jme )  
  REAL, INTENT(IN) :: msfux( ims:ime , jms:jme )  
  REAL, INTENT(IN) :: msfuy( ims:ime , jms:jme )  
  REAL, INTENT(IN) :: msfvx( ims:ime , jms:jme )  
  REAL, INTENT(IN) :: msfvy( ims:ime , jms:jme )  

  REAL, INTENT(IN) :: p_phy( ims:ime, kms:kme, jms:jme )
  REAL, INTENT(INOUT) :: t_tendf( ims:ime, kms:kme, jms:jme )
  REAL, INTENT(IN) :: t0
  REAL, INTENT(INOUT) :: savwt( nobs_ndg_vars, ims:ime, kms:kme, jms:jme )
  REAL, INTENT(INOUT) :: regime( ims:ime, jms:jme )
  REAL, INTENT(IN) :: pblh( ims:ime, jms:jme )
  REAL, INTENT(IN) :: z_at_w( ims:ime, kms:kme, jms:jme ) 
  REAL, INTENT(IN) :: z( ims:ime, kms:kme, jms:jme )  





  REAL,   INTENT(IN) :: ub( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: vb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: tb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: qvb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: pbase( ims:ime, kms:kme, jms:jme ) 
  REAL,   INTENT(IN) :: ptop
  REAL,   INTENT(IN) :: pp( ims:ime, kms:kme, jms:jme )  
  REAL,   INTENT(IN) :: phb( ims:ime, kms:kme, jms:jme ) 
  REAL,   INTENT(IN) :: ph( ims:ime, kms:kme, jms:jme )  
  REAL,   INTENT(IN) :: uratx( ims:ime, jms:jme )     
  REAL,   INTENT(IN) :: vratx( ims:ime, jms:jme )     
  REAL,   INTENT(IN) :: tratx( ims:ime, jms:jme )     
  REAL,   INTENT(INOUT) :: ru_tendf( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(INOUT) :: rv_tendf( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(INOUT) :: moist_tend( ims:ime, kms:kme, jms:jme )


  logical            :: nudge_flag   
  integer            :: KTAU         
  real               :: dtmin        
  integer            :: i, j, k      
  integer            :: idom         
  integer            :: nsta         
  integer            :: infr         
  integer            :: idarst       
  real               :: dtr          
  real               :: tconst       
  real    :: vih_uv(its:ite,jts:jte,2) 
  real    :: vih_t (its:ite,jts:jte,2) 
  real    :: vih_q (its:ite,jts:jte,2) 
  integer :: vik_uv(its:ite,jts:jte,2) 
  integer :: vik_t (its:ite,jts:jte,2) 
  integer :: vik_q (its:ite,jts:jte,2) 
  real    :: z_at_p( kms:kme )       



  character(len=200) :: msg  

  END SUBROUTINE fddaobs_driver

  SUBROUTINE compute_VIH(vif, hmax, fullmin, rampmin,       &
                         regime, pblh, terrh, z, vih,       &
                         ids,ide, jds,jde, kds,kde,         & 
                         ims,ime, jms,jme, kms,kme,         &
                         its,ite, jts,jte, kts,kte)

  USE module_fddaobs_rtfdda

  IMPLICIT NONE




  REAL,    INTENT(IN)  :: vif(6)                     
  REAL,    INTENT(IN)  :: hmax                       
  REAL,    INTENT(IN)  :: fullmin                    
  REAL,    INTENT(IN)  :: rampmin                    
  REAL,    INTENT(IN)  :: regime(ims:ime,jms:jme)    
  REAL,    INTENT(IN)  :: pblh(ims:ime,jms:jme)      
  REAL,    INTENT(IN)  :: terrh(ims:ime,jms:jme)     
  REAL,    INTENT(IN)  :: z(ims:ime,kms:kme,jms:jme) 
  REAL,    INTENT(OUT) :: vih(its:ite,jts:jte,2)     

  INTEGER, INTENT(IN)  :: ids,ide, jds,jde, kds,kde  
  INTEGER, INTENT(IN)  :: ims,ime, jms,jme, kms,kme  
  INTEGER, INTENT(IN)  :: its,ite, jts,jte, kts,kte  


  real  :: fullr(its:ite)    
  real  :: rampr(its:ite)    
  character(len=200) :: msg  
  integer:: i, j             

  integer k     


  do j = jts, jte


    do i = its, ite

      if(regime(i,j).eq.1.0) then        
        fullr(i) = vif(1)
        rampr(i) = vif(2)
      elseif(regime(i,j).eq.2.0) then    
        fullr(i) = vif(3)
        rampr(i) = vif(4)
      elseif(regime(i,j).eq.3.0 .or. regime(i,j).eq.4.0) then    
        fullr(i) = vif(5)
        rampr(i) = vif(6)
      else
        write(msg,'(a,f5.1,2(a,i4))') 'Unknown regime type ', regime(i,j),    &
                                 ' at grid coordinate i = ',i,' j = ',j
        call wrf_message(msg)
        call wrf_error_fatal3("<stdin>",255,&
'fddaobs_driver: compute_VIH STOP' )
        
      endif

    enddo


    CALL get_vif_hts_slab(fullr, rampr, pblh(ims,j),         &
                          hmax, fullmin, rampmin,            &
                          vih(its,j,1), vih(its,j,2),        &
                          ims,ime, its,ite)
  enddo
  END SUBROUTINE compute_VIH

  SUBROUTINE get_vif_hts_slab(fullr, rampr, pblh, hmax, fullmin, rampmin, &
                              ht1, ht2, ims,ime, its,ite)


  IMPLICIT NONE

  REAL, INTENT(IN)    :: fullr(its:ite)   
  REAL, INTENT(IN)    :: rampr(its:ite)   
  REAL, INTENT(IN)    :: pblh(ims:ime)    
  REAL, INTENT(IN)    :: hmax             
  REAL, INTENT(IN)    :: fullmin          
  REAL, INTENT(IN)    :: rampmin          
  REAL, INTENT(OUT)   :: ht1(its:ite)     
  REAL, INTENT(OUT)   :: ht2(its:ite)     
  INTEGER, INTENT(IN) :: ims,ime          
  INTEGER, INTENT(IN) :: its,ite          


  integer :: i

  do i = its, ite


    if(fullr(i).ge.0.0) then        
      ht1(i) = fullr(i)
    else                            
      ht1(i) = pblh(i) - (fullr(i)+5000.)
    endif


    ht1(i) = max(fullmin,ht1(i))




    if(rampr(i).ge.0.0) then

      ht2(i) = ht1(i) + max(rampmin,rampr(i))
    else

      ht2(i) = max( ht1(i)+rampmin, pblh(i)-(rampr(i)+5000.) )
    endif


    ht1(i) = min(ht1(i), hmax-rampmin)
    ht2(i) = min(ht2(i), hmax) 
  enddo
  END SUBROUTINE get_vif_hts_slab

  SUBROUTINE get_vik_slab( h, hlevs, ht, vik, ims,ime, kms,kme, its,ite, kts,kte )



  IMPLICIT NONE

  REAL,    INTENT(IN) :: h(its:ite)          
  REAL,    INTENT(IN) :: hlevs(ims:ime,kms:kme) 
  REAL,    INTENT(IN) :: ht(ims:ime)         
  INTEGER, INTENT(OUT):: vik(its:ite)        
  INTEGER, INTENT(IN) :: ims,ime, kms,kme    
  INTEGER, INTENT(IN) :: its,ite, kts,kte    


  integer :: i
  integer :: k
  real    :: ht_ag(kts:kte)

  do i = its, ite


    do k = kts,kte
       ht_ag(k) = hlevs(i,k) - ht(i)
    enddo

    vik(i) = ht_to_k( h(i), ht_ag, kts,kte ) 
  enddo
  END SUBROUTINE get_vik_slab

  INTEGER FUNCTION ht_to_k( h, hlevs, kts,kte )
  IMPLICIT NONE

  REAL,    INTENT(IN)  :: h                     
  REAL,    INTENT(IN)  :: hlevs(kts:kte)        
  INTEGER, INTENT(IN)  :: kts,kte               


  INTEGER :: k               
  INTEGER :: klo             

  KLEVS: do k = kts, kte
    klo = k-1
    if(h .le. hlevs(k)) then
      EXIT KLEVS
    endif
  enddo KLEVS
  klo = max0(1,klo)
  ht_to_k = min0(kte,klo)
  RETURN
  END FUNCTION ht_to_k

END MODULE module_fddaobs_driver
