

MODULE module_fddaobs_rtfdda


































CONTAINS


  SUBROUTINE fddaobs_init(nudge_opt, maxdom, inest, parid,             &
                          idynin, dtramp, fdaend, restart,             &
                          twindo_cg, twindo, itimestep,                &
                          no_pbl_nudge_uv,                             &
                          no_pbl_nudge_t,                              &
                          no_pbl_nudge_q,                              &
                          sfc_scheme_horiz, sfc_scheme_vert,           &
                          maxsnd_gap,                                  &
                          sfcfact, sfcfacr, dpsmx,                     &
                          nudge_wind, nudge_temp, nudge_mois,          &
                          nudgezfullr1_uv, nudgezrampr1_uv,            &
                          nudgezfullr2_uv, nudgezrampr2_uv,            &
                          nudgezfullr4_uv, nudgezrampr4_uv,            &
                          nudgezfullr1_t,  nudgezrampr1_t,             &
                          nudgezfullr2_t,  nudgezrampr2_t,             &
                          nudgezfullr4_t,  nudgezrampr4_t,             &
                          nudgezfullr1_q,  nudgezrampr1_q,             &
                          nudgezfullr2_q,  nudgezrampr2_q,             &
                          nudgezfullr4_q,  nudgezrampr4_q,             &
                          nudgezfullmin,   nudgezrampmin, nudgezmax,   &
                          xlat, xlong,                                 &
                          start_year, start_month, start_day,          &
                          start_hour, start_minute, start_second,      &
                          p00, t00, tlp,                               &
                          znu, p_top,                                  &



                          iprt,                                        &
                          ids,ide, jds,jde, kds,kde,                   &
                          ims,ime, jms,jme, kms,kme,                   &
                          its,ite, jts,jte, kts,kte)     




  USE module_model_constants, ONLY : g, r_d
  USE module_domain
  USE module_dm, ONLY : wrf_dm_min_real

  IMPLICIT NONE





  INTEGER, intent(in)  :: maxdom
  INTEGER, intent(in)  :: nudge_opt(maxdom)
  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde,                 &
                          ims,ime, jms,jme, kms,kme,                 &
                          its,ite, jts,jte, kts,kte
  INTEGER, intent(in)  :: inest
  INTEGER, intent(in)  :: parid(maxdom)
  INTEGER, intent(in)  :: idynin         
  REAL,    intent(in)  :: dtramp         
  REAL,    intent(in)  :: fdaend(maxdom) 
  LOGICAL, intent(in)  :: restart
  REAL, intent(in)     :: twindo_cg      
  REAL, intent(in)     :: twindo
  INTEGER, intent(in)  :: itimestep
  INTEGER , INTENT(IN) :: no_pbl_nudge_uv(maxdom)  
  INTEGER , INTENT(IN) :: no_pbl_nudge_t(maxdom)   
  INTEGER , INTENT(IN) :: no_pbl_nudge_q(maxdom)   
  INTEGER , INTENT(IN) :: sfc_scheme_horiz 
  INTEGER , INTENT(IN) :: sfc_scheme_vert  
  REAL    , INTENT(IN) :: maxsnd_gap      
  REAL, intent(in)     :: sfcfact      
  REAL, intent(in)     :: sfcfacr      
  REAL, intent(in)     :: dpsmx        
  INTEGER , INTENT(IN) :: nudge_wind(maxdom)       
  INTEGER , INTENT(IN) :: nudge_temp(maxdom)       
  INTEGER , INTENT(IN) :: nudge_mois(maxdom)       
  REAL, INTENT(IN)     :: nudgezfullr1_uv  
  REAL, INTENT(IN)     :: nudgezrampr1_uv  
  REAL, INTENT(IN)     :: nudgezfullr2_uv  
  REAL, INTENT(IN)     :: nudgezrampr2_uv  
  REAL, INTENT(IN)     :: nudgezfullr4_uv  
  REAL, INTENT(IN)     :: nudgezrampr4_uv  
  REAL, INTENT(IN)     :: nudgezfullr1_t   
  REAL, INTENT(IN)     :: nudgezrampr1_t   
  REAL, INTENT(IN)     :: nudgezfullr2_t   
  REAL, INTENT(IN)     :: nudgezrampr2_t   
  REAL, INTENT(IN)     :: nudgezfullr4_t   
  REAL, INTENT(IN)     :: nudgezrampr4_t   
  REAL, INTENT(IN)     :: nudgezfullr1_q   
  REAL, INTENT(IN)     :: nudgezrampr1_q   
  REAL, INTENT(IN)     :: nudgezfullr2_q   
  REAL, INTENT(IN)     :: nudgezrampr2_q   
  REAL, INTENT(IN)     :: nudgezfullr4_q   
  REAL, INTENT(IN)     :: nudgezrampr4_q   
  REAL, INTENT(IN)     :: nudgezfullmin    
  REAL, INTENT(IN)     :: nudgezrampmin    
  REAL, INTENT(IN)     :: nudgezmax        
  REAL, INTENT(IN)     :: xlat ( ims:ime, jms:jme )        
  REAL, INTENT(IN)     :: xlong( ims:ime, jms:jme )        
  INTEGER, intent(in)  :: start_year   
  INTEGER, intent(in)  :: start_month  
  INTEGER, intent(in)  :: start_day    
  INTEGER, intent(in)  :: start_hour   
  INTEGER, intent(in)  :: start_minute 
  INTEGER, intent(in)  :: start_second 
  REAL, INTENT(IN)     :: p00          
  REAL, INTENT(IN)     :: t00          
  REAL, INTENT(IN)     :: tlp          
  REAL, INTENT(IN)     :: p_top        
  REAL, INTENT(IN)     :: znu( kms:kme )      



  LOGICAL, intent(in)  :: iprt         


  logical            :: nudge_flag      
  integer            :: ktau            
  integer            :: nest            
  integer            :: idom            
  integer            :: parent          
  real               :: conv            
  real               :: tl1             
  real               :: tl2             
  real               :: xn1
  real               :: known_lat       
  real               :: known_lon       
  character(len=200) :: msg             
  real               :: z_at_p( kms:kme )  
  integer            :: i,j,k           

  END SUBROUTINE fddaobs_init


END MODULE module_fddaobs_rtfdda

