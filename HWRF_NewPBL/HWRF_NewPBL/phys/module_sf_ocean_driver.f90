

MODULE module_sf_ocean_driver

CONTAINS


   SUBROUTINE OCEAN_DRIVER(tml,t0ml,hml,h0ml,huml,hvml,ust,u_phy,v_phy, &
                      tmoml,f,g,oml_gamma,                         &
                      XLAND,HFX,LH,TSK,GSW,GLW,EMISS,              &
                      DELTSM,STBOLT,                               &
                      ids,ide, jds,jde, kds,kde,                   &
                      ims,ime, jms,jme, kms,kme,                   &
                      its,ite, jts,jte, kts,kte,                   &
                      sf_ocean_physics,okms, okme,                 & 
                      om_tmp,om_s,om_u, om_v, om_depth, om_ml,     & 
                      om_lat, om_lon,                              & 
                      QFX,                                         & 
                      rdx, rdy, msfu, msfv, msft,xtime,om_tini,om_sini,id,omdt, & 
                      itimestep)








   IMPLICIT NONE




























   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte

   REAL,     INTENT(IN   )   ::     DELTSM, STBOLT

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN   )    ::                          EMISS, &
                                                         XLAND, &
                                                           GSW, &
                                                           GLW, &
                                                           HFX, &
                                                            LH

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                            TSK

   REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::     &
                                    TML,T0ML,HML,H0ML,HUML,HVML

   REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::     &
                                             U_PHY,V_PHY

   REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN   ) ::     &
                                             UST, F, TMOML

   REAL,    INTENT(IN   )   ::     G
   REAL,    INTENT(IN   )   ::     OML_GAMMA



   INTEGER ::  I,J



  INTEGER, OPTIONAL, INTENT(IN )::  sf_ocean_physics
  integer :: okms, okme
  real, dimension(ims:ime, okms:okme, jms:jme), INTENT(INOUT):: OM_TMP,OM_S,OM_U,OM_V,OM_DEPTH
  real, dimension(ims:ime, okms:okme, jms:jme):: om_density 
  real, dimension(ims:ime, okms:okme, jms:jme), INTENT(IN):: OM_TINI,OM_SINI
  real, dimension(ims:ime, jms:jme),INTENT(INOUT):: OM_ML, OM_LAT, OM_LON
  REAL, INTENT(IN   ) :: rdx, rdy,xtime,omdt
  REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: msfu, msfv, msft,qfx
  INTEGER , INTENT(IN)        :: id,itimestep
  integer :: stepom


   END SUBROUTINE OCEAN_DRIVER



END MODULE module_sf_ocean_driver
