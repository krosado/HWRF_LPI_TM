






SUBROUTINE med_nest_egrid_configure ( parent , nest )
 USE module_domain
 USE module_configure
 USE module_timing

 IMPLICIT NONE
 TYPE(domain) , POINTER             :: parent , nest
 REAL, PARAMETER                    :: ERAD=6371200.
 REAL, PARAMETER                    :: DTR=0.01745329
 REAL, PARAMETER                    :: DTAD=1.0
 REAL, PARAMETER                    :: CP=1004.6
 INTEGER                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER                            :: ITS,ITE,JTS,JTE,KTS,KTE
 CHARACTER(LEN=255)                 :: message




















    IF(MOD(parent%ed32,2) .NE. 0)THEN
     CALL wrf_error_fatal3("<stdin>",44,&
"PARENT DOMAIN: JMAX IS EVEN, INCREASE e_sn IN THE namelist.input BY 1")
    ENDIF




    IF(MOD(nest%ed32,2) .NE. 0)THEN
     CALL wrf_error_fatal3("<stdin>",52,&
"NESTED DOMAIN: JMAX IS EVEN, INCREASE e_sn IN THE namelist.input BY 1")
    ENDIF



    IDS = parent%sd31
    IDE = parent%ed31
    JDS = parent%sd32
    JDE = parent%ed32
    KDS = parent%sd33
    KDE = parent%ed33

    IMS = parent%sm31
    IME = parent%em31
    JMS = parent%sm32
    JME = parent%em32
    KMS = parent%sm33
    KME = parent%em33

    ITS = parent%sp31
    ITE = parent%ep31
    JTS = parent%sp32
    JTE = parent%ep32
    KTS = parent%sp33
    KTE = parent%ep33



    
    if (parent%parent_id == 0 ) then        
       parent%wbd0 = -(IDE-2)*parent%dx     
       parent%sbd0 = -((JDE-1)/2)*parent%dy 
       parent%wbd0var = parent%wbd0
       parent%sbd0var = parent%sbd0
    end if
    nest%wbd0   = parent%wbd0 + (nest%i_parent_start-1)*2.*parent%dx + mod(nest%j_parent_start+1,2)*parent%dx
    nest%sbd0   = parent%sbd0 + (nest%j_parent_start-1)*parent%dy
    nest%wbd0var = nest%wbd0
    nest%sbd0var = nest%sbd0
    nest%dx     = parent%dx/nest%parent_grid_ratio
    nest%dy     = parent%dy/nest%parent_grid_ratio

    write(message,*)" - i_parent_start = ",nest%i_parent_start
    CALL wrf_message(trim(message))
    write(message,*)" - j_parent_start = ",nest%j_parent_start
    CALL wrf_message(trim(message))
    write(message,*)" - parent%wbd0    = ",parent%wbd0
    CALL wrf_message(trim(message))
    write(message,*)" - parent%sbd0    = ",parent%sbd0
    CALL wrf_message(trim(message))
    write(message,*)" - nest%wbd0      = ",nest%wbd0
    CALL wrf_message(trim(message))
    write(message,*)" - nest%sbd0      = ",nest%sbd0
    CALL wrf_message(trim(message))
    write(message,*)" - nest%dx        = ",nest%dx
    CALL wrf_message(trim(message))
    write(message,*)" - nest%dy        = ",nest%dy
    CALL wrf_message(trim(message))

    CALL nl_set_dx (nest%id , nest%dx)   
    CALL nl_set_dy (nest%id , nest%dy)   



    CALL nl_get_cen_lat (parent%id, parent%cen_lat) 
    CALL nl_get_cen_lon (parent%id, parent%cen_lon) 

    nest%cen_lat=parent%cen_lat
    nest%cen_lon=parent%cen_lon

    CALL nl_set_cen_lat ( nest%id , nest%cen_lat)  
    CALL nl_set_cen_lon ( nest%id , nest%cen_lon)  

    write(message,*)" - nest%cen_lat   = ",nest%cen_lat
    CALL wrf_message(trim(message))
    write(message,*)" - nest%cen_lon   = ",nest%cen_lon
    CALL wrf_message(trim(message))






    if ( .not.nest%analysis ) then

    nest%sldpth  = parent%sldpth
    nest%dzsoil  = parent%dzsoil
    nest%rtdpth  = parent%rtdpth

    endif




    nest%deta        = parent%deta
    nest%aeta        = parent%aeta
    nest%etax        = parent%etax
    nest%dfl         = parent%dfl
    nest%deta1       = parent%deta1
    nest%aeta1       = parent%aeta1
    nest%eta1        = parent%eta1
    nest%deta2       = parent%deta2
    nest%aeta2       = parent%aeta2
    nest%eta2        = parent%eta2
    nest%pdtop       = parent%pdtop
    nest%pt          = parent%pt
    nest%dfrlg       = parent%dfrlg
    nest%num_soil_layers = parent%num_soil_layers
    nest%num_moves       = parent%num_moves








    nest%dlmd   = nest%dx
    nest%dphd   = nest%dy
    nest%dy_nmm = erad*(nest%dphd*dtr)
    nest%cpgfv  = -nest%dt/(48.*nest%dy_nmm)
    nest%en     = nest%dt/( 4.*nest%dy_nmm)*dtad
    nest%ent    = nest%dt/(16.*nest%dy_nmm)*dtad
    nest%f4d    = -.5*nest%dt*dtad
    nest%f4q    = -nest%dt*dtad
    nest%ef4t   = .5*nest%dt/cp



   CALL nl_get_truelat1 (parent%id, parent%truelat1 )
   CALL nl_get_truelat2 (parent%id, parent%truelat2 )


   CALL nl_get_stand_lon (parent%id, parent%stand_lon )

   CALL nl_get_map_proj (parent%id, parent%map_proj )
   CALL nl_get_iswater (parent%id, parent%iswater )

   nest%truelat1=parent%truelat1
   nest%truelat2=parent%truelat2

   nest%stand_lon=parent%stand_lon

   nest%map_proj=parent%map_proj
   nest%iswater=parent%iswater

   CALL nl_set_truelat1(nest%id, nest%truelat1)
   CALL nl_set_truelat2(nest%id, nest%truelat2)

   CALL nl_set_stand_lon(nest%id, nest%stand_lon)

   CALL nl_set_map_proj(nest%id, nest%map_proj)
   CALL nl_set_iswater(nest%id, nest%iswater)







END SUBROUTINE med_nest_egrid_configure
SUBROUTINE med_set_egrid_locs ( parent , nest )
 USE module_domain
 USE module_configure
 USE module_timing

 IMPLICIT NONE
 TYPE(domain)             :: parent , nest
 LOGICAL, EXTERNAL                  :: wrf_dm_on_monitor
 INTEGER                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER                            :: I,J,II,JJ,NII,NJJ
 REAL                               :: parent_CLAT,parent_CLON,parent_WBD,parent_SBD,parent_DLMD,parent_DPHD
 REAL                               :: nest_WBD,nest_SBD,nest_DLMD,nest_DPHD
 REAL                               :: SW_LATD,SW_LOND
 REAL                               :: ADDSUM1,ADDSUM2
 REAL                               :: xr,zr,xc
character*255 :: message








    CALL nl_get_cen_lat (parent%ID, parent_CLAT)
    CALL nl_get_cen_lon (parent%ID, parent_CLON)



    IDS = parent%sd31
    IDE = parent%ed31
    JDS = parent%sd32
    JDE = parent%ed32
    KDS = parent%sd33
    KDE = parent%ed33

    IMS = parent%sm31
    IME = parent%em31
    JMS = parent%sm32
    JME = parent%em32
    KMS = parent%sm33
    KME = parent%em33

    ITS  = parent%sp31
    ITE  = parent%ep31
    JTS  = parent%sp32
    JTE  = parent%ep32
    KTS  = parent%sp33
    KTE  = parent%ep33

    parent_DLMD = parent%dx                
    parent_DPHD = parent%dy                
    parent_WBD  = parent%wbd0
    parent_SBD  = parent%sbd0



    CALL EARTH_LATLON ( parent%HLAT,parent%HLON,parent%VLAT,parent%VLON,  & 
                        parent_DLMD,parent_DPHD,parent_WBD,parent_SBD,                    & 
                        parent_CLAT,parent_CLON,                                          &
                        IDS,IDE,JDS,JDE,KDS,KDE,                                          &
                        IMS,IME,JMS,JME,KMS,KME,                                          &
                        ITS,ITE,JTS,JTE,KTS,KTE                                           )

    if(nest%id == parent%id) return 



    IDS = nest%sd31
    IDE = nest%ed31
    JDS = nest%sd32
    JDE = nest%ed32
    KDS = nest%sd33
    KDE = nest%ed33

    IMS = nest%sm31
    IME = nest%em31
    JMS = nest%sm32
    JME = nest%em32
    KMS = nest%sm33
    KME = nest%em33

    ITS  = nest%sp31
    ITE  = nest%ep31
    JTS  = nest%sp32
    JTE  = nest%ep32
    KTS  = nest%sp33
    KTE  = nest%ep33

    nest_DLMD = nest%dx
    nest_DPHD = nest%dy
    nest_WBD  = nest%wbd0
    nest_SBD  = nest%sbd0






    CALL EARTH_LATLON ( nest%HLAT,nest%HLON,nest%VLAT,nest%VLON, & 
                        nest_DLMD,nest_DPHD,nest_WBD,nest_SBD,                   & 
                        parent_CLAT,parent_CLON,                                 & 
                        IDS,IDE,JDS,JDE,KDS,KDE,                                 & 
                        IMS,IME,JMS,JME,KMS,KME,                                 &
                        ITS,ITE,JTS,JTE,KTS,KTE                                  )
  end SUBROUTINE med_set_egrid_locs

SUBROUTINE med_construct_egrid_weights ( parent , nest )
 USE module_domain
 USE module_configure
 USE module_timing

 IMPLICIT NONE
 TYPE(domain) , POINTER             :: parent , nest
 LOGICAL, EXTERNAL                  :: wrf_dm_on_monitor
 INTEGER                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER                            :: I,J,II,JJ,NII,NJJ
 REAL                               :: parent_CLAT,parent_CLON,parent_WBD,parent_SBD,parent_DLMD,parent_DPHD
 REAL                               :: nest_WBD,nest_SBD,nest_DLMD,nest_DPHD
 REAL                               :: SW_LATD,SW_LOND
 REAL                               :: ADDSUM1,ADDSUM2
 REAL                               :: xr,zr,xc








    CALL nl_get_cen_lat (parent%ID, parent_CLAT)
    CALL nl_get_cen_lon (parent%ID, parent_CLON)



    IDS = parent%sd31
    IDE = parent%ed31
    JDS = parent%sd32
    JDE = parent%ed32
    KDS = parent%sd33
    KDE = parent%ed33

    IMS = parent%sm31
    IME = parent%em31
    JMS = parent%sm32
    JME = parent%em32
    KMS = parent%sm33
    KME = parent%em33

    ITS  = parent%sp31
    ITE  = parent%ep31
    JTS  = parent%sp32
    JTE  = parent%ep32
    KTS  = parent%sp33
    KTE  = parent%ep33

    parent_DLMD = parent%dx                
    parent_DPHD = parent%dy                
    parent_WBD  = parent%wbd0
    parent_SBD  = parent%sbd0



    CALL EARTH_LATLON ( parent%HLAT,parent%HLON,parent%VLAT,parent%VLON,  & 
                        parent_DLMD,parent_DPHD,parent_WBD,parent_SBD,                    & 
                        parent_CLAT,parent_CLON,                                          &
                        IDS,IDE,JDS,JDE,KDS,KDE,                                          &
                        IMS,IME,JMS,JME,KMS,KME,                                          &
                        ITS,ITE,JTS,JTE,KTS,KTE                                           )



    IDS = nest%sd31
    IDE = nest%ed31
    JDS = nest%sd32
    JDE = nest%ed32
    KDS = nest%sd33
    KDE = nest%ed33

    IMS = nest%sm31
    IME = nest%em31
    JMS = nest%sm32
    JME = nest%em32
    KMS = nest%sm33
    KME = nest%em33

    ITS  = nest%sp31
    ITE  = nest%ep31
    JTS  = nest%sp32
    JTE  = nest%ep32
    KTS  = nest%sp33
    KTE  = nest%ep33

    nest_DLMD = nest%dx
    nest_DPHD = nest%dy
    nest_WBD  = nest%wbd0
    nest_SBD  = nest%sbd0






    CALL EARTH_LATLON ( nest%HLAT,nest%HLON,nest%VLAT,nest%VLON, & 
                        nest_DLMD,nest_DPHD,nest_WBD,nest_SBD,                   & 
                        parent_CLAT,parent_CLON,                                 & 
                        IDS,IDE,JDS,JDE,KDS,KDE,                                 & 
                        IMS,IME,JMS,JME,KMS,KME,                                 &
                        ITS,ITE,JTS,JTE,KTS,KTE                                  )



  if(nest%vortex_tracker /= 1) then
    CALL G2T2H_new(    nest%IIH,nest%JJH,                            & 
                       nest%HBWGT1,nest%HBWGT2,                      & 
                       nest%HBWGT3,nest%HBWGT4,                      &
                       nest%I_PARENT_START,nest%J_PARENT_START,      & 
                       3,                              & 
                       IDS,IDE,JDS,JDE,KDS,KDE,            & 
                       IMS,IME,JMS,JME,KMS,KME,            &
                       ITS,ITE,JTS,JTE,KTS,KTE      )
  else
    CALL G2T2H( nest%IIH,nest%JJH,                       & 
                nest%HBWGT1,nest%HBWGT2,                 & 
                nest%HBWGT3,nest%HBWGT4,                 &
                nest%HLAT,nest%HLON,                     & 
                parent_DLMD,parent_DPHD,parent_WBD,parent_SBD,   & 
                parent_CLAT,parent_CLON,                         & 
                parent%ed31,parent%ed32,                         & 
                IDS,IDE,JDS,JDE,KDS,KDE,                         & 
                IMS,IME,JMS,JME,KMS,KME,                         & 
                ITS,ITE,JTS,JTE,KTS,KTE                          ) 
  endif




  if(nest%vortex_tracker /= 1) then
    CALL G2T2V_new(    nest%IIV,nest%JJV,                            & 
                       nest%VBWGT1,nest%VBWGT2,                      & 
                       nest%VBWGT3,nest%VBWGT4,                      &
                       nest%I_PARENT_START,nest%J_PARENT_START,      & 
                       3,                              & 
                       IDS,IDE,JDS,JDE,KDS,KDE,            & 
                       IMS,IME,JMS,JME,KMS,KME,            &
                       ITS,ITE,JTS,JTE,KTS,KTE      )
  else
    CALL G2T2V( nest%IIV,nest%JJV,                       & 
                nest%VBWGT1,nest%VBWGT2,                 & 
                nest%VBWGT3,nest%VBWGT4,                 &
                nest%VLAT,nest%VLON,                     & 
                parent_DLMD,parent_DPHD,parent_WBD,parent_SBD,   & 
                parent_CLAT,parent_CLON,                         & 
                parent%ed31,parent%ed32,                         & 
                IDS,IDE,JDS,JDE,KDS,KDE,                         & 
                IMS,IME,JMS,JME,KMS,KME,                         & 
                ITS,ITE,JTS,JTE,KTS,KTE                          ) 
  endif



  call INIT_HNEAR(nest%iih, nest%jjh, nest%hbwgt1, nest%hbwgt2, &
                  nest%hbwgt3, nest%hbwgt4,                     &
                  nest%hnear_i, nest%hnear_j,                   &
                  IDS,IDE,JDS,JDE,KDS,KDE,                      &
                  IMS,IME,JMS,JME,KMS,KME,                      &
                  ITS,ITE,JTS,JTE,KTS,KTE)




     CALL WEIGTS_CHECK(nest%HBWGT1,nest%HBWGT2,          & 
                       nest%HBWGT3,nest%HBWGT4,          &
                       nest%VBWGT1,nest%VBWGT2,          & 
                       nest%VBWGT3,nest%VBWGT4,          &
                       IDS,IDE,JDS,JDE,KDS,KDE,                  & 
                       IMS,IME,JMS,JME,KMS,KME,                  & 
                       ITS,ITE,JTS,JTE,KTS,KTE                   )



    CALL BOUNDS_CHECK( nest%IIH,nest%JJH,nest%IIV,nest%JJV,   &
                       nest%i_parent_start,nest%j_parent_start,nest%shw,      &
                       IDS,IDE,JDS,JDE,KDS,KDE,                               & 
                       IMS,IME,JMS,JME,KMS,KME,                               & 
                       ITS,ITE,JTS,JTE,KTS,KTE                                )



END SUBROUTINE med_construct_egrid_weights 






SUBROUTINE EARTH_LATLON ( HLAT,HLON,VLAT,VLON,     & 
                          DLMD1,DPHD1,WBD1,SBD1,   & 
                          CENTRAL_LAT,CENTRAL_LON, & 
                          IDS,IDE,JDS,JDE,KDS,KDE, &  
                          IMS,IME,JMS,JME,KMS,KME, &
                          ITS,ITE,JTS,JTE,KTS,KTE  )


 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME 
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE  
 REAL,       INTENT(IN   )                            :: DLMD1,DPHD1,WBD1,SBD1
 REAL,       INTENT(IN   )                            :: CENTRAL_LAT,CENTRAL_LON
 REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(OUT)        :: HLAT,HLON,VLAT,VLON




 INTEGER,PARAMETER                           :: KNUM=SELECTED_REAL_KIND(13) 
 INTEGER                                     :: I,J
 REAL(KIND=KNUM)                             :: WB,SB,DLM,DPH,TPH0,STPH0,CTPH0
 REAL(KIND=KNUM)                             :: TDLM,TDPH,TLMH,TLMV,TLMH0,TLMV0,TPHH,TPHV,DTR
 REAL(KIND=KNUM)                             :: STPH,CTPH,STPV,CTPV,PI_2
 REAL(KIND=KNUM)                             :: SPHH,CLMH,FACTH,SPHV,CLMV,FACTV
 REAL(KIND=KNUM), DIMENSION(IMS:IME,JMS:JME) :: GLATH,GLONH,GLATV,GLONV
 REAL(KIND=KNUM) :: DLMD8,DPHD8,WBD8,SBD8,CLAT8,CLON8
 REAL(KIND=KNUM) :: CPHH, CPHV

 DLMD8=DLMD1
 DPHD8=DPHD1
 WBD8=WBD1
 SBD8=SBD1
 CLAT8=CENTRAL_LAT
 CLON8=CENTRAL_LON

      PI_2 = ACOS(0.)
      DTR  = PI_2/90.
      WB   = WBD8 * DTR                 
      SB   = SBD8 * DTR                 
      DLM  = DLMD8 * DTR                
      DPH  = DPHD8 * DTR                
      TDLM = DLM + DLM                  
      TDPH = DPH + DPH                  



      TPH0  = CLAT8*DTR                
      STPH0 = SIN(TPH0)
      CTPH0 = COS(TPH0)

                                                
      DO J = JTS,MIN(JTE,JDE-1)                 

         TLMH0 = WB - TDLM + MOD(J+1,2) * DLM   
         TLMV0 = WB - TDLM + MOD(J,2) * DLM     
         TPHH = SB + (J-1)*DPH                  
         TPHV = TPHH                            
         STPH = SIN(TPHH)
         CTPH = COS(TPHH)
         STPV = SIN(TPHV)
         CTPV = COS(TPHV)
                                                              
         DO I = ITS,MIN(ITE,IDE-1)                            
           TLMH = TLMH0 + I*TDLM                              

           SPHH = CTPH0 * STPH + STPH0 * CTPH * COS(TLMH)     
           CPHH = sqrt(1-SPHH**2)
           GLATH(I,J)=ASIN(SPHH)                              
           
           
           CLMH = (CTPH*COS(TLMH)-SPHH*STPH0) / (CPHH*CTPH0)
           IF(CLMH .GT. 1.) CLMH = 1.0
           IF(CLMH .LT. -1.) CLMH = -1.0
           FACTH = 1.
           IF(TLMH .GT. 0.) FACTH = -1.
           GLONH(I,J) = -CLON8*DTR + FACTH*ACOS(CLMH)

         ENDDO                                    

         DO I = ITS,MIN(ITE,IDE-1)
           TLMV = TLMV0 + I*TDLM
           SPHV = CTPH0 * STPV + STPH0 * CTPV * COS(TLMV)
           CPHV = sqrt(1-SPHV**2)
           GLATV(I,J) = ASIN(SPHV)
           
           
           CLMV = (CTPV*COS(TLMV)-SPHV*STPH0) / (CPHV*CTPH0)
           IF(CLMV .GT. 1.) CLMV = 1.
           IF(CLMV .LT. -1.) CLMV = -1.
           FACTV = 1.
           IF(TLMV .GT. 0.) FACTV = -1.
           GLONV(I,J) = -CLON8*DTR + FACTV*ACOS(CLMV)

         ENDDO

      ENDDO



      DO J = JTS, MIN(JTE,JDE-1)
       DO I = ITS, MIN(ITE,IDE-1)
          HLAT(I,J) = GLATH(I,J) / DTR
          HLON(I,J)= -GLONH(I,J)/DTR
          IF(HLON(I,J) .GT. 180.) HLON(I,J) = HLON(I,J)  - 360.
          IF(HLON(I,J) .LT. -180.) HLON(I,J) = HLON(I,J) + 360.

          VLAT(I,J) = GLATV(I,J) / DTR
          VLON(I,J) = -GLONV(I,J) / DTR
          IF(VLON(I,J) .GT. 180.) VLON(I,J) = VLON(I,J)  - 360.
          IF(VLON(I,J) .LT. -180.) VLON(I,J) = VLON(I,J) + 360.

       ENDDO
      ENDDO

END SUBROUTINE EARTH_LATLON



subroutine init_hnear(iih,jjh,hbwgt1,hbwgt2,hbwgt3,hbwgt4, &
                      hnear_i,hnear_j,                     &
                      IDS,IDE,JDS,JDE,KDS,KDE,             &
                      IMS,IME,JMS,JME,KMS,KME,             &
                      ITS,ITE,JTS,JTE,KTS,KTE)
  implicit none
  integer, intent(in) :: ids,ide,jds,jde,kds,kde, &
                         ims,ime,jms,jme,kms,kme, &
                         its,ite,jts,jte,kts,kte, &
                         iih(ims:ime,jms:jme), jjh(ims:ime,jms:jme)
  integer, dimension(ims:ime,jms:jme), intent(out) :: hnear_i,hnear_j
  real, dimension(ims:ime,jms:jme), intent(in) :: hbwgt1, hbwgt2, hbwgt3, hbwgt4

  integer :: i,j,iweight,a
  real :: rweight

  do j=jts,min(jte,jde-1)
     do i=its,min(ite,ide-1)
        a=1-mod(JJH(i,j),2)

        iweight=1
        rweight=hbwgt1(i,j)

        if(hbwgt2(i,j)>rweight) then
           iweight=2 ; rweight=hbwgt2(i,j)
        endif

        if(hbwgt3(i,j)>rweight) then
           iweight=3 ; rweight=hbwgt3(i,j)
        endif

        if(hbwgt4(i,j)>rweight) then
           iweight=4
        endif

        select case(iweight)
        case(1)
           hnear_i(i,j)=IIH(I,J)   ; hnear_j(i,j)=JJH(I,J)
        case(2)
           hnear_i(i,j)=IIH(I,J)+1 ; hnear_j(i,j)=JJH(I,J)
        case(3)
           hnear_i(i,j)=IIH(I,J)+a ; hnear_j(i,j)=JJH(I,J)-1
        case default 
           hnear_i(i,j)=IIH(I,J)+a ; hnear_j(i,j)=JJH(I,J)+1
        end select
     enddo
  enddo
end subroutine init_hnear



  SUBROUTINE G2T2H( IIH,JJH,                     & 
                    HBWGT1,HBWGT2,               & 
                    HBWGT3,HBWGT4,               & 
                    HLAT,HLON,                   & 
                    DLMD1,DPHD1,WBD1,SBD1,       & 
                    CENTRAL_LAT,CENTRAL_LON,     & 
                    P_IDE,P_JDE,                 & 
                    IDS,IDE,JDS,JDE,KDS,KDE,     & 
                    IMS,IME,JMS,JME,KMS,KME,     &
                    ITS,ITE,JTS,JTE,KTS,KTE      )












 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: P_IDE,P_JDE
 REAL,       INTENT(IN   )                            :: DLMD1,DPHD1,WBD1,SBD1
 REAL,       INTENT(IN   )                            :: CENTRAL_LAT,CENTRAL_LON
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(IN)   :: HLAT,HLON
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIH,JJH



 INTEGER,PARAMETER :: KNUM=SELECTED_REAL_KIND(13)
 INTEGER           :: IMT,JMT,N2R,MK,K,I,J,DSLP0,DSLOPE
 INTEGER           :: NROW,NCOL,KROWS
 REAL(KIND=KNUM)   :: X,Y,Z,TLAT,TLON
 REAL(KIND=KNUM)   :: PI_2,D2R,R2D,GLAT,GLON,DPH,DLM,TPH0,TLM0,WB,SB                
 REAL(KIND=KNUM)   :: ROW,COL,SLP0,TLATHC,TLONHC,DENOM,SLOPE
 REAL(KIND=KNUM)   :: TLAT1,TLAT2,TLON1,TLON2,DLM1,DLM2,DLM3,DLM4,D1,D2
 REAL(KIND=KNUM)   :: DLA1,DLA2,DLA3,DLA4,S1,R1,DS1,AN1,AN2,AN3                    
 REAL(KIND=KNUM)   :: DL1,DL2,DL3,DL4,DL1I,DL2I,DL3I,DL4I,SUMDL,TLONO,TLATO 
 REAL(KIND=KNUM)   :: DTEMP
 REAL   , DIMENSION(IMS:IME,JMS:JME)    :: TLATHX,TLONHX
 INTEGER, DIMENSION(IMS:IME,JMS:JME)    :: KOUTB


  IMT=2*P_IDE-2             
  JMT=P_JDE/2               
  PI_2=ACOS(0.)
  D2R=PI_2/90.
  R2D=1./D2R
  DPH=DPHD1*D2R
  DLM=DLMD1*D2R
  TPH0= CENTRAL_LAT*D2R
  TLM0=-CENTRAL_LON*D2R        
  WB=WBD1*D2R                   
  SB=SBD1*D2R
  SLP0=DPHD1/DLMD1
  DSLP0=NINT(R2D*ATAN(SLP0))
  DS1=SQRT(DPH*DPH+DLM*DLM)    
  AN1=ASIN(DLM/DS1)
  AN2=ASIN(DPH/DS1)

  DO J =  JTS,MIN(JTE,JDE-1) 
    DO I = ITS,MIN(ITE,IDE-1) 








      GLAT=HLAT(I,J)*D2R
      GLON= (360. - HLON(I,J))*D2R
      X=COS(TPH0)*COS(GLAT)*COS(GLON-TLM0)+SIN(TPH0)*SIN(GLAT)
      Y=-COS(GLAT)*SIN(GLON-TLM0)
      Z=COS(TPH0)*SIN(GLAT)-SIN(TPH0)*COS(GLAT)*COS(GLON-TLM0)
      TLAT=R2D*ATAN(Z/SQRT(X*X+Y*Y))
      TLON=R2D*ATAN(Y/X)




      ROW=(TLAT-SBD1)/DPHD1+1     
      COL=(TLON-WBD1)/DLMD1+1     

      NROW=INT(ROW + 0.001)     
      NCOL=INT(COL + 0.001)     
      TLAT=TLAT*D2R
      TLON=TLON*D2R













      IF(MOD(NROW,2).EQ.1.AND.MOD(NCOL,2).EQ.1.OR.     &     
         MOD(NROW,2).EQ.0.AND.MOD(NCOL,2).EQ.0)THEN        
           TLAT1=(NROW-JMT)*DPH                           
           TLAT2=TLAT1+DPH                              
           TLON1=(NCOL-(P_IDE-1))*DLM                                   
           TLON2=TLON1+DLM                                 
           DLM1=TLON-TLON1                                 
           DLM2=TLON-TLON2


           DTEMP=MIN(1.0_KNUM,COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
           D1=ACOS(DTEMP)
           DTEMP=MIN(1.0_KNUM,COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
           D2=ACOS(DTEMP)
            IF(D1.GT.D2)THEN
             NROW=NROW+1                    
             NCOL=NCOL+1                    
            ENDIF 
      ELSE












           TLAT1=(NROW+1-JMT)*DPH
           TLAT2=TLAT1-DPH
           TLON1=(NCOL-(P_IDE-1))*DLM
           TLON2=TLON1+DLM
           DLM1=TLON-TLON1
           DLM2=TLON-TLON2


           DTEMP=MIN(1.0_KNUM,COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
           D1=ACOS(DTEMP)
           DTEMP=MIN(1.0_KNUM,COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
           D2=ACOS(DTEMP)
             IF(D1.LT.D2)THEN
              NROW=NROW+1                    
             ELSE
              NCOL=NCOL+1                    
             ENDIF
      ENDIF

      KROWS=((NROW-1)/2)*IMT
      IF(MOD(NROW,2).EQ.1)THEN
        K=KROWS+(NCOL+1)/2
      ELSE
        K=KROWS+P_IDE-1+NCOL/2
      ENDIF





























    N2R=K/IMT
    MK=MOD(K,IMT)

    IF(MK.EQ.0)THEN
      TLATHC=SB+(2*N2R-1)*DPH
    ELSE
      TLATHC=SB+(2*N2R+(MK-1)/(P_IDE-1))*DPH
    ENDIF

    IF(MK.LE.(P_IDE-1))THEN
      TLONHC=WB+2*(MK-1)*DLM
    ELSE
      TLONHC=WB+(2*(MK-(P_IDE-1))-1)*DLM
    ENDIF
  






    IF(ABS(TLON-TLONHC) .LE. 1.E-4)TLONHC=TLON
    IF(ABS(TLAT-TLATHC) .LE. 1.E-4)TLATHC=TLAT
    DENOM=(TLON-TLONHC)







    IF(DENOM.EQ.0.0)THEN

      IF(TLATHC.EQ.TLAT)THEN
        KOUTB(I,J)=K
        IIH(I,J) = NCOL
        JJH(I,J) = NROW
        TLATHX(I,J)=TLATHC
        TLONHX(I,J)=TLONHC
        HBWGT1(I,J)=1.0
        HBWGT2(I,J)=0.0
        HBWGT3(I,J)=0.0
        HBWGT4(I,J)=0.0
      ELSE                                      

         IF(TLATHC .GT. TLAT)THEN      
          KOUTB(I,J)=K-(P_IDE-1)
          IIH(I,J) = NCOL-1
          JJH(I,J) = NROW-1
          TLATHX(I,J)=TLATHC-DPH
          TLONHX(I,J)=TLONHC-DLM
         ELSE                                   
          KOUTB(I,J)=K+(P_IDE-1)-1
          IIH(I,J) = NCOL-1
          JJH(I,J) = NROW+1
          TLATHX(I,J)=TLATHC+DPH
          TLONHX(I,J)=TLONHC-DLM
         ENDIF










       TLATO=TLATHX(I,J)
       TLONO=TLONHX(I,J)
       DLM1=TLON-TLONO
       DLA1=TLAT-TLATO                                               

       DL1=SQRT(DLM1*DLM1+DLA1*DLA1)                                 

       TLATO=TLATHX(I,J)
       TLONO=TLONHX(I,J)+2.*DLM
       DLM2=TLON-TLONO
       DLA2=TLAT-TLATO                                               

       DL2=SQRT(DLM2*DLM2+DLA2*DLA2)                                 

       TLATO=TLATHX(I,J)-DPH
       TLONO=TLONHX(I,J)+DLM
       DLM3=TLON-TLONO
       DLA3=TLAT-TLATO                                               

       DL3=SQRT(DLM3*DLM3+DLA3*DLA3)                                 
 
       TLATO=TLATHX(I,J)+DPH
       TLONO=TLONHX(I,J)+DLM
       DLM4=TLON-TLONO
       DLA4=TLAT-TLATO                                               

       DL4=SQRT(DLM4*DLM4+DLA4*DLA4)                                 





       AN3=ATAN2(DLA1,DLM1)                                          
       R1=DL1*SIN(AN2-AN3)/SIN(2.*AN1)
       S1=DL1*SIN(2.*PI_2-2*AN1-AN2+AN3)/SIN(2.*AN1)
       R1=R1/DS1
       S1=S1/DS1
       DL1I=(1.-R1)*(1.-S1)
       DL2I=R1*S1
       DL3I=R1*(1.-S1)
       DL4I=(1.-R1)*S1

       HBWGT1(I,J)=DL1I
       HBWGT2(I,J)=DL2I
       HBWGT3(I,J)=DL3I
       HBWGT4(I,J)=DL4I

      ENDIF

    ELSE



      SLOPE=(TLAT-TLATHC)/DENOM
      DSLOPE=NINT(R2D*ATAN(SLOPE))

      IF(DSLOPE.LE.DSLP0.AND.DSLOPE.GE.-DSLP0)THEN
        IF(TLON.GT.TLONHC)THEN
          IF(TLONHC.GE.-WB-DLM)CALL wrf_error_fatal3("<stdin>",1002,&
"1H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K
          IIH(I,J) = NCOL
          JJH(I,J) = NROW
          TLATHX(I,J)=TLATHC
          TLONHX(I,J)=TLONHC
        ELSE
          IF(TLONHC.LE.WB+DLM)CALL wrf_error_fatal3("<stdin>",1010,&
"2H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K-1
          IIH(I,J) = NCOL-2
          JJH(I,J) = NROW
          TLATHX(I,J)=TLATHC
          TLONHX(I,J)=TLONHC -2.*DLM
        ENDIF


      ELSEIF(DSLOPE.GT.DSLP0)THEN
        IF(TLON.GT.TLONHC)THEN
          IF(TLATHC.GE.-SB-DPH)CALL wrf_error_fatal3("<stdin>",1022,&
"3H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K+(P_IDE-1)-1
          IIH(I,J) = NCOL-1
          JJH(I,J) = NROW+1
          TLATHX(I,J)=TLATHC+DPH
          TLONHX(I,J)=TLONHC-DLM
        ELSE
          IF(TLATHC.LE.SB+DPH)CALL wrf_error_fatal3("<stdin>",1030,&
"4H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K-(P_IDE-1)
          IIH(I,J) = NCOL-1
          JJH(I,J) = NROW-1
          TLATHX(I,J)=TLATHC-DPH
          TLONHX(I,J)=TLONHC-DLM
        ENDIF


      ELSEIF(DSLOPE.LT.-DSLP0)THEN
        IF(TLON.GT.TLONHC)THEN
          IF(TLATHC.LE.SB+DPH)CALL wrf_error_fatal3("<stdin>",1042,&
"5H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K-(P_IDE-1)
          IIH(I,J) = NCOL-1
          JJH(I,J) = NROW-1
          TLATHX(I,J)=TLATHC-DPH
          TLONHX(I,J)=TLONHC-DLM
        ELSE
          IF(TLATHC.GE.-SB-DPH)CALL wrf_error_fatal3("<stdin>",1050,&
"6H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K+(P_IDE-1)-1
          IIH(I,J) = NCOL-1
          JJH(I,J) = NROW+1
          TLATHX(I,J)=TLATHC+DPH
          TLONHX(I,J)=TLONHC-DLM
        ENDIF
      ENDIF




















      
      TLATO=TLATHX(I,J)
      TLONO=TLONHX(I,J)
      DLM1=TLON-TLONO
      DLA1=TLAT-TLATO                                               

      DL1=SQRT(DLM1*DLM1+DLA1*DLA1)                                 

      TLATO=TLATHX(I,J)                                             
      TLONO=TLONHX(I,J)+2.*DLM
      DLM2=TLON-TLONO
      DLA2=TLAT-TLATO                                               

      DL2=SQRT(DLM2*DLM2+DLA2*DLA2)                                 

      TLATO=TLATHX(I,J)-DPH
      TLONO=TLONHX(I,J)+DLM
      DLM3=TLON-TLONO
      DLA3=TLAT-TLATO                                               

      DL3=SQRT(DLM3*DLM3+DLA3*DLA3)                                 

      TLATO=TLATHX(I,J)+DPH
      TLONO=TLONHX(I,J)+DLM
      DLM4=TLON-TLONO
      DLA4=TLAT-TLATO                                               

      DL4=SQRT(DLM4*DLM4+DLA4*DLA4)                                 



      AN3=ATAN2(DLA1,DLM1)                                          
      R1=DL1*SIN(AN2-AN3)/SIN(2.*AN1)
      S1=DL1*SIN(2.*PI_2-2*AN1-AN2+AN3)/SIN(2.*AN1)
      R1=R1/DS1
      S1=S1/DS1
      DL1I=(1.-R1)*(1.-S1)
      DL2I=R1*S1
      DL3I=R1*(1.-S1)
      DL4I=(1.-R1)*S1

      HBWGT1(I,J)=DL1I
      HBWGT2(I,J)=DL2I
      HBWGT3(I,J)=DL3I
      HBWGT4(I,J)=DL4I

    ENDIF 




      IIH(I,J)=NINT(0.5*IIH(I,J))

      HBWGT1(I,J)=MAX(HBWGT1(I,J),0.0)   
      HBWGT2(I,J)=MAX(HBWGT2(I,J),0.0)   
      HBWGT3(I,J)=MAX(HBWGT3(I,J),0.0)   
      HBWGT4(I,J)=MAX(HBWGT4(I,J),0.0)   






   ENDDO
  ENDDO


  RETURN 
  END SUBROUTINE G2T2H



  SUBROUTINE G2T2V( IIV,JJV,                     & 
                    VBWGT1,VBWGT2,               & 
                    VBWGT3,VBWGT4,               &
                    VLAT,VLON,                   & 
                    DLMD1,DPHD1,WBD1,SBD1,       & 
                    CENTRAL_LAT,CENTRAL_LON,     & 
                    P_IDE,P_JDE,                 & 
                    IDS,IDE,JDS,JDE,KDS,KDE,     & 
                    IMS,IME,JMS,JME,KMS,KME,     &
                    ITS,ITE,JTS,JTE,KTS,KTE      )












 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: P_IDE,P_JDE
 REAL,       INTENT(IN   )                            :: DLMD1,DPHD1,WBD1,SBD1
 REAL,       INTENT(IN   )                            :: CENTRAL_LAT,CENTRAL_LON
 REAL,    DIMENSION(IMS:IME,JMS:JME),   INTENT(IN)    :: VLAT,VLON
 REAL,    DIMENSION(IMS:IME,JMS:JME),   INTENT(OUT)   :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),   INTENT(OUT)   :: IIV,JJV



 INTEGER,PARAMETER :: KNUM=SELECTED_REAL_KIND(13)     
 INTEGER           :: IMT,JMT,N2R,MK,K,I,J,DSLP0,DSLOPE
 INTEGER           :: NROW,NCOL,KROWS
 REAL(KIND=KNUM)   :: X,Y,Z,TLAT,TLON
 REAL(KIND=KNUM)   :: PI_2,D2R,R2D,GLAT,GLON,DPH,DLM,TPH0,TLM0,WB,SB
 REAL(KIND=KNUM)   :: ROW,COL,SLP0,TLATVC,TLONVC,DENOM,SLOPE
 REAL(KIND=KNUM)   :: TLAT1,TLAT2,TLON1,TLON2,DLM1,DLM2,DLM3,DLM4,D1,D2
 REAL(KIND=KNUM)   :: DLA1,DLA2,DLA3,DLA4,S1,R1,DS1,AN1,AN2,AN3                    
 REAL(KIND=KNUM)   :: DL1,DL2,DL3,DL4,DL1I,DL2I,DL3I,DL4I,SUMDL,TLONO,TLATO
 REAL(KIND=KNUM)   :: DTEMP
 REAL  , DIMENSION(IMS:IME,JMS:JME)      :: TLATVX,TLONVX
 INTEGER, DIMENSION(IMS:IME,JMS:JME)     :: KOUTB


  IMT=2*P_IDE-2             
  JMT=P_JDE/2               
  PI_2=ACOS(0.)
  D2R=PI_2/90.
  R2D=1./D2R
  DPH=DPHD1*D2R
  DLM=DLMD1*D2R
  TPH0= CENTRAL_LAT*D2R
  TLM0=-CENTRAL_LON*D2R        
  WB=WBD1*D2R                   
  SB=SBD1*D2R
  SLP0=DPHD1/DLMD1
  DSLP0=NINT(R2D*ATAN(SLP0))
  DS1=SQRT(DPH*DPH+DLM*DLM)    
  AN1=ASIN(DLM/DS1)
  AN2=ASIN(DPH/DS1)

  DO J =  JTS,MIN(JTE,JDE-1)
    DO I = ITS,MIN(ITE,IDE-1)







      GLAT=VLAT(I,J)*D2R
      GLON=(360. - VLON(I,J))*D2R
      X=COS(TPH0)*COS(GLAT)*COS(GLON-TLM0)+SIN(TPH0)*SIN(GLAT)
      Y=-COS(GLAT)*SIN(GLON-TLM0)
      Z=COS(TPH0)*SIN(GLAT)-SIN(TPH0)*COS(GLAT)*COS(GLON-TLM0)
      TLAT=R2D*ATAN(Z/SQRT(X*X+Y*Y))
      TLON=R2D*ATAN(Y/X)




      ROW=(TLAT-SBD1)/DPHD1+1     
      COL=(TLON-WBD1)/DLMD1+1     

      NROW=INT(ROW + 0.001)     
      NCOL=INT(COL + 0.001)
      TLAT=TLAT*D2R
      TLON=TLON*D2R














      IF(MOD(NROW,2).EQ.0.AND.MOD(NCOL,2).EQ.1.OR.     &
         MOD(NROW,2).EQ.1.AND.MOD(NCOL,2).EQ.0)THEN
           TLAT1=(NROW-JMT)*DPH
           TLAT2=TLAT1+DPH
           TLON1=(NCOL-(P_IDE-1))*DLM
           TLON2=TLON1+DLM
           DLM1=TLON-TLON1
           DLM2=TLON-TLON2


           DTEMP=MIN(1.0_KNUM,COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
           D1=ACOS(DTEMP)
           DTEMP=MIN(1.0_KNUM,COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
           D2=ACOS(DTEMP)
            IF(D1.GT.D2)THEN
             NROW=NROW+1                    
             NCOL=NCOL+1                    
            ENDIF
      ELSE












           TLAT1=(NROW+1-JMT)*DPH
           TLAT2=TLAT1-DPH
           TLON1=(NCOL-(P_IDE-1))*DLM
           TLON2=TLON1+DLM
           DLM1=TLON-TLON1
           DLM2=TLON-TLON2


           DTEMP=MIN(1.0_KNUM,COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
           D1=ACOS(DTEMP)
           DTEMP=MIN(1.0_KNUM,COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
           D2=ACOS(DTEMP)
             IF(D1.LT.D2)THEN
              NROW=NROW+1                    
             ELSE
              NCOL=NCOL+1                    
             ENDIF

      ENDIF

      KROWS=((NROW-1)/2)*IMT
      IF(MOD(NROW,2).EQ.1)THEN
        K=KROWS+NCOL/2
      ELSE
        K=KROWS+P_IDE-2+(NCOL+1)/2     
      ENDIF





























      N2R=K/IMT
      MK=MOD(K,IMT)

      IF(MK.EQ.0)THEN
        TLATVC=SB+(2*N2R-1)*DPH
      ELSE
        TLATVC=SB+(2*N2R+MK/(P_IDE-1))*DPH
      ENDIF

      IF(MK.LE.(P_IDE-1)-1)THEN
        TLONVC=WB+(2*MK-1)*DLM
      ELSE
        TLONVC=WB+2*(MK-(P_IDE-1))*DLM
      ENDIF






       IF(ABS(TLON-TLONVC) .LE. 1.E-4)TLONVC=TLON
       IF(ABS(TLAT-TLATVC) .LE. 1.E-4)TLATVC=TLAT
       DENOM=(TLON-TLONVC)







     IF(DENOM.EQ.0.0)THEN

       IF(TLATVC.EQ.TLAT)THEN
         KOUTB(I,J)=K
         IIV(I,J) = NCOL
         JJV(I,J) = NROW
         TLATVX(I,J)=TLATVC
         TLONVX(I,J)=TLONVC
         VBWGT1(I,J)=1.0
         VBWGT2(I,J)=0.0
         VBWGT3(I,J)=0.0
         VBWGT4(I,J)=0.0
       ELSE                              
          
         IF(TLATVC .GT. TLAT)THEN      
          KOUTB(I,J)=K-(P_IDE-1)
          IIV(I,J) = NCOL-1
          JJV(I,J) = NROW-1
          TLATVX(I,J)=TLATVC-DPH
          TLONVX(I,J)=TLONVC-DLM
         ELSE                                   
          KOUTB(I,J)=K+(P_IDE-1)-1
          IIV(I,J) = NCOL-1
          JJV(I,J) = NROW+1
          TLATVX(I,J)=TLATVC+DPH
          TLONVX(I,J)=TLONVC-DLM
         ENDIF











       TLATO=TLATVX(I,J)
       TLONO=TLONVX(I,J)
       DLM1=TLON-TLONO
       DLA1=TLAT-TLATO                                               

       DL1=SQRT(DLM1*DLM1+DLA1*DLA1)                                 

       TLATO=TLATVX(I,J)
       TLONO=TLONVX(I,J)+2.*DLM
       DLM2=TLON-TLONO
       DLA2=TLAT-TLATO                                               

       DL2=SQRT(DLM2*DLM2+DLA2*DLA2)                                 

       TLATO=TLATVX(I,J)-DPH
       TLONO=TLONVX(I,J)+DLM
       DLM3=TLON-TLONO
       DLA3=TLAT-TLATO                                               

       DL3=SQRT(DLM3*DLM3+DLA3*DLA3)                                 

       TLATO=TLATVX(I,J)+DPH
       TLONO=TLONVX(I,J)+DLM
       DLM4=TLON-TLONO
       DLA4=TLAT-TLATO                                               

       DL4=SQRT(DLM4*DLM4+DLA4*DLA4)                                 
                 


       AN3=ATAN2(DLA1,DLM1)                                          
       R1=DL1*SIN(AN2-AN3)/SIN(2.*AN1)
       S1=DL1*SIN(2.*PI_2-2*AN1-AN2+AN3)/SIN(2.*AN1)
       R1=R1/DS1
       S1=S1/DS1
       DL1I=(1.-R1)*(1.-S1)
       DL2I=R1*S1
       DL3I=R1*(1.-S1)
       DL4I=(1.-R1)*S1

       VBWGT1(I,J)=DL1I
       VBWGT2(I,J)=DL2I
       VBWGT3(I,J)=DL3I
       VBWGT4(I,J)=DL4I      

      ENDIF

    ELSE




      SLOPE=(TLAT-TLATVC)/DENOM
      DSLOPE=NINT(R2D*ATAN(SLOPE))

      IF(DSLOPE.LE.DSLP0.AND.DSLOPE.GE.-DSLP0)THEN
        IF(TLON.GT.TLONVC)THEN
          IF(TLONVC.GE.-WB-DLM)CALL wrf_error_fatal3("<stdin>",1469,&
"1V:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K
          IIV(I,J)=NCOL
          JJV(I,J)=NROW
          TLATVX(I,J)=TLATVC
          TLONVX(I,J)=TLONVC
        ELSE
          IF(TLONVC.LE.WB+DLM)CALL wrf_error_fatal3("<stdin>",1477,&
"2V:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K-1
          IIV(I,J) = NCOL-2
          JJV(I,J) = NROW
          TLATVX(I,J)=TLATVC
          TLONVX(I,J)=TLONVC-2.*DLM
        ENDIF
 
      ELSEIF(DSLOPE.GT.DSLP0)THEN
        IF(TLON.GT.TLONVC)THEN
          IF(TLATVC.GE.-SB-DPH)CALL wrf_error_fatal3("<stdin>",1488,&
"3V:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K+(P_IDE-1)-1
          IIV(I,J) = NCOL-1
          JJV(I,J) = NROW+1
          TLATVX(I,J)=TLATVC+DPH
          TLONVX(I,J)=TLONVC-DLM
        ELSE
          IF(TLATVC.LE.SB+DPH)CALL wrf_error_fatal3("<stdin>",1496,&
"4V:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K-(P_IDE-1)
          IIV(I,J) = NCOL-1
          JJV(I,J) = NROW-1
          TLATVX(I,J)=TLATVC-DPH
          TLONVX(I,J)=TLONVC-DLM
        ENDIF
 
      ELSEIF(DSLOPE.LT.-DSLP0)THEN
        IF(TLON.GT.TLONVC)THEN
          IF(TLATVC.LE.SB+DPH)CALL wrf_error_fatal3("<stdin>",1507,&
"5V:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K-(P_IDE-1)
          IIV(I,J) = NCOL-1
          JJV(I,J) = NROW-1
          TLATVX(I,J)=TLATVC-DPH
          TLONVX(I,J)=TLONVC-DLM
        ELSE
          IF(TLATVC.GE.-SB-DPH)CALL wrf_error_fatal3("<stdin>",1515,&
"6V:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K+(P_IDE-1)-1
          IIV(I,J) = NCOL-1
          JJV(I,J) = NROW+1
          TLATVX(I,J)=TLATVC+DPH
          TLONVX(I,J)=TLONVC-DLM
        ENDIF
      ENDIF




















      TLATO=TLATVX(I,J)
      TLONO=TLONVX(I,J)
      DLM1=TLON-TLONO
      DLA1=TLAT-TLATO                                               

      DL1=SQRT(DLM1*DLM1+DLA1*DLA1)                                 

      TLATO=TLATVX(I,J)
      TLONO=TLONVX(I,J)+2.*DLM
      DLM2=TLON-TLONO
      DLA2=TLAT-TLATO                                               

      DL2=SQRT(DLM2*DLM2+DLA2*DLA2)                                 

      TLATO=TLATVX(I,J)-DPH
      TLONO=TLONVX(I,J)+DLM
      DLM3=TLON-TLONO
      DLA3=TLAT-TLATO                                               

      DL3=SQRT(DLM3*DLM3+DLA3*DLA3)                                 

      TLATO=TLATVX(I,J)+DPH
      TLONO=TLONVX(I,J)+DLM
      DLM4=TLON-TLONO
      DLA4=TLAT-TLATO                                               

      DL4=SQRT(DLM4*DLM4+DLA4*DLA4)                                 
 


      AN3=ATAN2(DLA1,DLM1)                                          
      R1=DL1*SIN(AN2-AN3)/SIN(2.*AN1)
      S1=DL1*SIN(2.*PI_2-2*AN1-AN2+AN3)/SIN(2.*AN1)
      R1=R1/DS1
      S1=S1/DS1
      DL1I=(1.-R1)*(1.-S1)
      DL2I=R1*S1
      DL3I=R1*(1.-S1)
      DL4I=(1.-R1)*S1

      VBWGT1(I,J)=DL1I
      VBWGT2(I,J)=DL2I
      VBWGT3(I,J)=DL3I
      VBWGT4(I,J)=DL4I

     ENDIF




      IIV(I,J)=NINT(0.5*IIV(I,J))

      VBWGT1(I,J)=MAX(VBWGT1(I,J),0.0)   
      VBWGT2(I,J)=MAX(VBWGT2(I,J),0.0)   
      VBWGT3(I,J)=MAX(VBWGT3(I,J),0.0)   
      VBWGT4(I,J)=MAX(VBWGT4(I,J),0.0)   

    ENDDO
  ENDDO

 RETURN
 END SUBROUTINE G2T2V



 SUBROUTINE G2T2H_new( IIH,JJH,                            & 
                       HBWGT1,HBWGT2,                      & 
                       HBWGT3,HBWGT4,                      &
                       I_PARENT_START,J_PARENT_START,      & 
                       RATIO,                              & 
                       IDS,IDE,JDS,JDE,KDS,KDE,            & 
                       IMS,IME,JMS,JME,KMS,KME,            &
                       ITS,ITE,JTS,JTE,KTS,KTE      )






 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIH,JJH

 INTEGER                                              :: I,J
 INTEGER                                              :: JP






































 DO J=JTS,MIN(JTE,JDE-1)
 DO I=ITS,MIN(ITE,IDE-1)
    JP = MOD(J,RATIO*2)
    SELECT CASE(JP)
    CASE ( 1 )
      CALL SUB1H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &
                I_PARENT_START,J_PARENT_START,           & 
                RATIO,                                   & 
                IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 2 )
      CALL SUB2H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &    
                I_PARENT_START,J_PARENT_START,           & 
                RATIO,                                   & 
                IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 3 )
      CALL SUB3H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &
                I_PARENT_START,J_PARENT_START,           & 
                RATIO,                                   & 
                IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 4 )
      CALL SUB4H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &    
                I_PARENT_START,J_PARENT_START,           & 
                RATIO,                                   & 
                IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 5 )
      CALL SUB5H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &
                I_PARENT_START,J_PARENT_START,           & 
                RATIO,                                   & 
                IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 0 )
      CALL SUB6H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &    
                I_PARENT_START,J_PARENT_START,           & 
                RATIO,                                   & 
                IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    END SELECT
  105  format(a,4i4,5f7.3)
 END DO
 END DO

 RETURN
 END SUBROUTINE G2T2H_new
 SUBROUTINE SUB1H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & 
                 RATIO,                                   & 
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIH,JJH

 INTEGER                                              :: I,J
 INTEGER                                              :: IP

  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 1.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 0.0
    HBWGT4(I,J) = 0.0
   CASE ( 2 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 4.0/9.0
    HBWGT2(I,J) = 1.0/9.0
    HBWGT3(I,J) = 2.0/9.0
    HBWGT4(I,J) = 2.0/9.0
   CASE ( 0 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 1.0/9.0
    HBWGT2(I,J) = 4.0/9.0
    HBWGT3(I,J) = 2.0/9.0
    HBWGT4(I,J) = 2.0/9.0
   END SELECT
 RETURN
 END SUBROUTINE SUB1H
 SUBROUTINE SUB2H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & 
                 RATIO,                                   & 
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIH,JJH

 INTEGER                                              :: I,J
 INTEGER                                              :: IP

  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 2.0/3.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 0.0
    HBWGT4(I,J) = 1.0/3.0
   CASE ( 2 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 2.0/9.0
    HBWGT2(I,J) = 2.0/9.0
    HBWGT3(I,J) = 1.0/9.0
    HBWGT4(I,J) = 4.0/9.0
   CASE ( 0 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)+1
    HBWGT1(I,J) = 1.0/3.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 2.0/3.0
    HBWGT4(I,J) = 0.0
   END SELECT
 RETURN
 END SUBROUTINE SUB2H
 SUBROUTINE SUB3H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & 
                 RATIO,                                   & 
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIH,JJH

 INTEGER                                              :: I,J
 INTEGER                                              :: IP

  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)-1
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)+1
    HBWGT1(I,J) = 2.0/9.0
    HBWGT2(I,J) = 2.0/9.0
    HBWGT3(I,J) = 4.0/9.0
    HBWGT4(I,J) = 1.0/9.0
   CASE ( 2 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 1.0/3.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 0.0
    HBWGT4(I,J) = 2.0/3.0
   CASE ( 0 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)+1
    HBWGT1(I,J) = 2.0/3.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 1.0/3.0
    HBWGT4(I,J) = 0.0
   END SELECT
 RETURN
 END SUBROUTINE SUB3H
 SUBROUTINE SUB4H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & 
                 RATIO,                                   & 
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIH,JJH

 INTEGER                                              :: I,J
 INTEGER                                              :: IP

  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)-1
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 1.0/9.0
    HBWGT2(I,J) = 4.0/9.0
    HBWGT3(I,J) = 2.0/9.0
    HBWGT4(I,J) = 2.0/9.0
   CASE ( 2 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 1.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 0.0
    HBWGT4(I,J) = 0.0
   CASE ( 0 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 4.0/9.0
    HBWGT2(I,J) = 1.0/9.0
    HBWGT3(I,J) = 2.0/9.0
    HBWGT4(I,J) = 2.0/9.0
   END SELECT
 RETURN
 END SUBROUTINE SUB4H
 SUBROUTINE SUB5H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & 
                 RATIO,                                   & 
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIH,JJH

 INTEGER                                              :: I,J
 INTEGER                                              :: IP

  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)-1
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 2.0/9.0
    HBWGT2(I,J) = 2.0/9.0
    HBWGT3(I,J) = 1.0/9.0
    HBWGT4(I,J) = 4.0/9.0
   CASE ( 2 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)+1
    HBWGT1(I,J) = 1.0/3.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 2.0/3.0
    HBWGT4(I,J) = 0.0
   CASE ( 0 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 2.0/3.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 0.0
    HBWGT4(I,J) = 1.0/3.0
   END SELECT
 RETURN
 END SUBROUTINE SUB5H
 SUBROUTINE SUB6H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & 
                 RATIO,                                   & 
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIH,JJH

 INTEGER                                              :: I,J
 INTEGER                                              :: IP

  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO) + 1
    HBWGT1(I,J) = 2.0/3.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 1.0/3.0
    HBWGT4(I,J) = 0.0
   CASE ( 2 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO) + 1
    HBWGT1(I,J) = 2.0/9.0
    HBWGT2(I,J) = 2.0/9.0
    HBWGT3(I,J) = 4.0/9.0
    HBWGT4(I,J) = 1.0/9.0
   CASE ( 0 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 1.0/3.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 0.0
    HBWGT4(I,J) = 2.0/3.0
   END SELECT
 RETURN
 END SUBROUTINE SUB6H



 SUBROUTINE G2T2V_new( IIV,JJV,                            & 
                       VBWGT1,VBWGT2,                      & 
                       VBWGT3,VBWGT4,                      &
                       I_PARENT_START,J_PARENT_START,      & 
                       RATIO,                              & 
                       IDS,IDE,JDS,JDE,KDS,KDE,            & 
                       IMS,IME,JMS,JME,KMS,KME,            &
                       ITS,ITE,JTS,JTE,KTS,KTE      )






 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIV,JJV

 INTEGER                                              :: I,J
 INTEGER                                              :: JP




































 DO J=JTS,MIN(JTE,JDE-1)
 DO I=ITS,MIN(ITE,IDE-1)
    JP = MOD(J,RATIO*2)
    SELECT CASE(JP)
    CASE ( 1 )
      CALL SUB1V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &
                I_PARENT_START,J_PARENT_START,           & 
                RATIO,                                   & 
                IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 2 )
      CALL SUB2V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &    
                I_PARENT_START,J_PARENT_START,           & 
                RATIO,                                   & 
                IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 3 )
      CALL SUB3V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &
                I_PARENT_START,J_PARENT_START,           & 
                RATIO,                                   & 
                IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 4 )
      CALL SUB4V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &    
                I_PARENT_START,J_PARENT_START,           & 
                RATIO,                                   & 
                IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 5 )
      CALL SUB5V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &
                I_PARENT_START,J_PARENT_START,           & 
                RATIO,                                   & 
                IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 0 )
      CALL SUB6V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &    
                I_PARENT_START,J_PARENT_START,           & 
                RATIO,                                   & 
                IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    END SELECT
 END DO
 END DO

 RETURN
 END SUBROUTINE G2T2V_new
 SUBROUTINE SUB1V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & 
                 RATIO,                                   & 
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIV,JJV

 INTEGER                                              :: I,J
 INTEGER                                              :: IP
  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO) - 1
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 1.0/9.0
    VBWGT2(I,J) = 4.0/9.0
    VBWGT3(I,J) = 2.0/9.0
    VBWGT4(I,J) = 2.0/9.0
   CASE ( 2 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO) 
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 1.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 0.0
    VBWGT4(I,J) = 0.0
   CASE ( 0 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 4.0/9.0
    VBWGT2(I,J) = 1.0/9.0
    VBWGT3(I,J) = 2.0/9.0
    VBWGT4(I,J) = 2.0/9.0
   END SELECT
 RETURN
 END SUBROUTINE SUB1V
 SUBROUTINE SUB2V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & 
                 RATIO,                                   & 
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIV,JJV

 INTEGER                                              :: I,J
 INTEGER                                              :: IP
  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO) - 1
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 2.0/9.0
    VBWGT2(I,J) = 2.0/9.0
    VBWGT3(I,J) = 1.0/9.0
    VBWGT4(I,J) = 4.0/9.0
   CASE ( 2 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO) + 1
    VBWGT1(I,J) = 1.0/3.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 2.0/3.0
    VBWGT4(I,J) = 0.0
   CASE ( 0 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 2.0/3.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 0.0
    VBWGT4(I,J) = 1.0/3.0
   END SELECT
 RETURN
 END SUBROUTINE SUB2V
 SUBROUTINE SUB3V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & 
                 RATIO,                                   & 
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIV,JJV

 INTEGER                                              :: I,J
 INTEGER                                              :: IP
  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO) + 1
    VBWGT1(I,J) = 2.0/3.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 1.0/3.0
    VBWGT4(I,J) = 0.0
   CASE ( 2 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO) + 1
    VBWGT1(I,J) = 2.0/9.0
    VBWGT2(I,J) = 2.0/9.0
    VBWGT3(I,J) = 4.0/9.0
    VBWGT4(I,J) = 1.0/9.0
   CASE ( 0 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 1.0/3.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 0.0
    VBWGT4(I,J) = 2.0/3.0
   END SELECT
 RETURN
 END SUBROUTINE SUB3V
 SUBROUTINE SUB4V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & 
                 RATIO,                                   & 
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIV,JJV

 INTEGER                                              :: I,J
 INTEGER                                              :: IP
  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 1.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 0.0
    VBWGT4(I,J) = 0.0
   CASE ( 2 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 4.0/9.0
    VBWGT2(I,J) = 1.0/9.0
    VBWGT3(I,J) = 2.0/9.0
    VBWGT4(I,J) = 2.0/9.0
   CASE ( 0 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 1.0/9.0
    VBWGT2(I,J) = 4.0/9.0
    VBWGT3(I,J) = 2.0/9.0
    VBWGT4(I,J) = 2.0/9.0
   END SELECT
 RETURN
 END SUBROUTINE SUB4V
 SUBROUTINE SUB5V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & 
                 RATIO,                                   & 
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIV,JJV

 INTEGER                                              :: I,J
 INTEGER                                              :: IP
  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 2.0/3.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 0.0
    VBWGT4(I,J) = 1.0/3.0
   CASE ( 2 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 2.0/9.0
    VBWGT2(I,J) = 2.0/9.0
    VBWGT3(I,J) = 1.0/9.0
    VBWGT4(I,J) = 4.0/9.0
   CASE ( 0 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO) + 1
    VBWGT1(I,J) = 1.0/3.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 2.0/3.0
    VBWGT4(I,J) = 0.0
   END SELECT
 RETURN
 END SUBROUTINE SUB5V
 SUBROUTINE SUB6V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & 
                 RATIO,                                   & 
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & 
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIV,JJV

 INTEGER                                              :: I,J
 INTEGER                                              :: IP
  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO) - 1
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO) + 1
    VBWGT1(I,J) = 2.0/9.0
    VBWGT2(I,J) = 2.0/9.0
    VBWGT3(I,J) = 4.0/9.0
    VBWGT4(I,J) = 1.0/9.0
   CASE ( 2 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO) 
    VBWGT1(I,J) = 1.0/3.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 0.0
    VBWGT4(I,J) = 2.0/3.0
   CASE ( 0 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO) + 1
    VBWGT1(I,J) = 2.0/3.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 1.0/3.0
    VBWGT4(I,J) = 0.0
   END SELECT
 RETURN
 END SUBROUTINE SUB6V




SUBROUTINE WEIGTS_CHECK(HBWGT1,HBWGT2,HBWGT3,HBWGT4,       & 
                        VBWGT1,VBWGT2,VBWGT3,VBWGT4,       &
                        IDS,IDE,JDS,JDE,KDS,KDE,           &
                        IMS,IME,JMS,JME,KMS,KME,           & 
                        ITS,ITE,JTS,JTE,KTS,KTE            )

  IMPLICIT NONE
  INTEGER, INTENT(IN)                                 :: IDS,IDE,JDS,JDE,KDS,KDE,  &
                                                         IMS,IME,JMS,JME,KMS,KME,  &
                                                         ITS,ITE,JTS,JTE,KTS,KTE
  REAL,    DIMENSION(IMS:IME,JMS:JME),   INTENT(IN)   :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
  REAL,    DIMENSION(IMS:IME,JMS:JME),   INTENT(IN)   :: VBWGT1,VBWGT2,VBWGT3,VBWGT4



  REAL , PARAMETER :: EPSI=1.0E-3
  INTEGER          :: I,J
  REAL             :: ADDSUM
 CHARACTER(LEN=255):: message







  IF((ITE-ITS) .LE. 5 .OR. (JTE-JTS) .LE. 5)THEN
   WRITE(message,*)'ITE-ITS=',ITE-ITS,'JTE-JTS=',JTE-JTS
   CALL wrf_message(trim(message))
   CALL wrf_error_fatal3("<stdin>",2396,&
'NESTED DOMAIN:PLEASE OPTIMIZE THE NUMBER OF PROCESSES; TRY SQUARES OF NUMBERS') 
  ENDIF





  ADDSUM=0.
  DO J = JTS, MIN(JTE,JDE-1)
   DO I = ITS, MIN(ITE,IDE-1)
      ADDSUM=HBWGT1(I,J)+HBWGT2(I,J)+HBWGT3(I,J)+HBWGT4(I,J)
      IF(ABS(1.0-ADDSUM) .GE. EPSI)THEN
       WRITE(message,*)'I=',I,'J=',J,'WEIGHTS=',HBWGT1(I,J),HBWGT2(I,J),HBWGT3(I,J),HBWGT4(I,J),1-ADDSUM
       CALL wrf_message(trim(message))
       CALL wrf_error_fatal3("<stdin>",2411,&
'NESTED DOMAIN:SOMETHING IS WRONG WITH WEIGHTS COMPUTATION AT MASS POINTS') 
      ENDIF
   ENDDO
  ENDDO

  ADDSUM=0.
  DO J = JTS, MIN(JTE,JDE-1)
   DO I = ITS, MIN(ITE,IDE-1)
      ADDSUM=VBWGT1(I,J)+VBWGT2(I,J)+VBWGT3(I,J)+VBWGT4(I,J)
      IF(ABS(1.0-ADDSUM) .GE. EPSI)THEN 
       WRITE(message,*)'I=',I,'J=',J,'WEIGHTS=',VBWGT1(I,J),VBWGT2(I,J),VBWGT3(I,J),VBWGT4(I,J),1-ADDSUM
       CALL wrf_message(trim(message))
       CALL wrf_error_fatal3("<stdin>",2424,&
'NESTED DOMAIN:SOMETHING IS WRONG WITH WEIGHTS COMPUTATION AT VELOCITY POINTS')
      ENDIF
   ENDDO
  ENDDO

END SUBROUTINE WEIGTS_CHECK



SUBROUTINE BOUNDS_CHECK( IIH,JJH,IIV,JJV,          &
                         IPOS,JPOS,SHW,            &
                         IDS,IDE,JDS,JDE,KDS,KDE,  & 
                         IMS,IME,JMS,JME,KMS,KME,  & 
                         ITS,ITE,JTS,JTE,KTS,KTE   )

 IMPLICIT NONE
 INTEGER, INTENT(IN) :: IPOS,JPOS,SHW,            &
                        IDS,IDE,JDS,JDE,KDS,KDE,  & 
                        IMS,IME,JMS,JME,KMS,KME,  & 
                        ITS,ITE,JTS,JTE,KTS,KTE   

 INTEGER, DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IIH,JJH,IIV,JJV



 INTEGER :: I,J
 CHARACTER(LEN=255)                 :: message







  IF(IPOS .LE. SHW)CALL wrf_error_fatal3("<stdin>",2459,&
'NESTED DOMAIN TOO CLOSE TO PARENTs X-BOUNDARY')
  IF(JPOS .LE. SHW)CALL wrf_error_fatal3("<stdin>",2461,&
'NESTED DOMAIN TOO CLOSE TO PARENTs Y-BOUNDARY')

  DO J = JTS, MIN(JTE,JDE-1)
   DO I = ITS, MIN(ITE,IDE-1)
      IF(IIH(I,J) .EQ. 0)CALL wrf_error_fatal3("<stdin>",2466,&
'IIH=0: SOMETHING IS WRONG')
      IF(JJH(I,J) .EQ. 0)CALL wrf_error_fatal3("<stdin>",2468,&
'JJH=0: SOMETHING IS WRONG')
   ENDDO
  ENDDO

  DO J = JTS, MIN(JTE,JDE-1)
   DO I = ITS, MIN(ITE,IDE-1)
      IF(IIH(I,J) .LT. (IPOS-SHW) .OR. JJH(I,J) .LT. (JPOS-SHW) .OR.   &
         IIV(I,J) .LT. (IPOS-SHW) .OR. JJV(I,J) .LT. (JPOS-SHW))THEN
         WRITE(message,*)I,J,IIH(I,J),IPOS,JJH(I,J),JPOS,SHW
         CALL wrf_message(trim(message))
         WRITE(message,*)I,J,IIV(I,J),IPOS,JJV(I,J),JPOS,SHW
         CALL wrf_message(trim(message))
         CALL wrf_error_fatal3("<stdin>",2481,&
'CHECK NESTED DOMAIN BOUNDS: TRY INCREASING STENCIL WIDTH') 
      ENDIF
   ENDDO
  ENDDO

END SUBROUTINE BOUNDS_CHECK




SUBROUTINE BASE_STATE_PARENT ( Z3d,Q3d,T3d,PSTD,        &
                               PINT,T,Q,CWM,            &
                               FIS,QS,PD,PDTOP,PTOP,    &
                               ETA1,ETA2,               &
                               DETA1,DETA2,             &
                               IDS,IDE,JDS,JDE,KDS,KDE, &
                               IMS,IME,JMS,JME,KMS,KME, &
                               ITS,ITE,JTS,JTE,KTS,KTE  )


 USE MODULE_MODEL_CONSTANTS
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 REAL,       INTENT(IN   )                            :: PDTOP,PTOP
 REAL, DIMENSION(KMS:KME),                 INTENT(IN) :: ETA1,ETA2,DETA1,DETA2
 REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(IN) :: FIS,PD,QS
 REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(IN) :: PINT,T,Q,CWM
 REAL, DIMENSION(KMS:KME),                 INTENT(OUT):: PSTD
 REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(OUT):: Z3d,Q3d,T3d



 INTEGER,PARAMETER                                    :: JTB=134
 INTEGER                                              :: I,J,K,ILOC,JLOC
 REAL, PARAMETER                                      :: LAPSR=6.5E-3, GI=1./G,D608=0.608
 REAL, PARAMETER                                      :: COEF3=287.05*GI*LAPSR, COEF2=-1./COEF3
 REAL, PARAMETER                                      :: TRG=2.0*R_D*GI,LAPSI=1.0/LAPSR
 REAL, PARAMETER                                      :: P_REF=103000.
 REAL                                                 :: A,B,APELP,RTOPP,DZ,ZMID
 REAL, DIMENSION(IMS:IME,JMS:JME)                     :: SLP,TSFC,ZMSLP
 REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME)             :: Z3d_IN
 REAL,DIMENSION(JTB)                                  :: PIN,ZIN,Y2,PIO,ZOUT,DUM1,DUM2
 REAL,DIMENSION(JTB)                                  :: QIN,QOUT,TIN,TOUT




    DO K=KDS,KDE
     DO J = JTS, MIN(JTE,JDE-1)
      DO I = ITS, MIN(ITE,IDE-1)
       Z3d(I,J,K)=0.0
       T3d(I,J,K)=0.0
       Q3d(I,J,K)=0.0
      ENDDO
     ENDDO
    ENDDO 




    DO J = JTS, MIN(JTE,JDE-1)
      DO I = ITS, MIN(ITE,IDE-1)
       Z3d_IN(I,J,1)=FIS(I,J)*GI
      ENDDO
    ENDDO 

    DO K = KDS,KDE-1
     DO J = JTS, MIN(JTE,JDE-1)
      DO I = ITS, MIN(ITE,IDE-1)
        APELP    = (PINT(I,J,K+1)+PINT(I,J,K))

        RTOPP    = TRG*T(I,J,K)*(1.0+Q(I,J,K)*P608)/APELP
        DZ       = RTOPP*(DETA1(K)*PDTOP+DETA2(K)*PD(I,J))   
        Z3d_IN(I,J,K+1) = Z3d_IN(I,J,K) + DZ
      ENDDO
     ENDDO
    ENDDO




    DO K=KDS,KDE                         
       PSTD(K) = ETA1(K)*PDTOP + ETA2(K)*(P_REF -PDTOP - PTOP) + PTOP
    ENDDO





    DO J = JTS, MIN(JTE,JDE-1)
      DO I = ITS, MIN(ITE,IDE-1)
        TSFC(I,J) = T(I,J,1)*(1.+D608*Q(I,J,1)) + LAPSR*(Z3d_IN(I,J,1)+Z3d_IN(I,J,2))*0.5
        A         = LAPSR*Z3d_IN(I,J,1)/TSFC(I,J)
        SLP(I,J)  = PINT(I,J,1)*(1-A)**COEF2    
        B         = (PSTD(1)/SLP(I,J))**COEF3
        ZMSLP(I,J)= TSFC(I,J)*LAPSI*(1.0 - B)   
      ENDDO
    ENDDO




    DO J = JTS, MIN(JTE,JDE-1)
      DO I = ITS, MIN(ITE,IDE-1)



      PIN=0.;ZIN=0.;Y2=0;PIO=0.;ZOUT=0.;DUM1=0.;DUM2=0.

       DO K=KDS,KDE                           
         PIN(K) = PINT(I,J,KDE-K+1)
         ZIN(K) = Z3d_IN(I,J,KDE-K+1)
       ENDDO

       IF(PINT(I,J,1) .LE. PSTD(1))THEN
          PIN(KDE) = PSTD(1)
          ZIN(KDE) = ZMSLP(I,J)
       ENDIF

       Y2(1  )=0.
       Y2(KDE)=0.

       DO K=KDS,KDE
          PIO(K)=PSTD(K)
       ENDDO

       CALL SPLINE1(I,J,JTB,KDE,PIN,ZIN,Y2,KDE,PIO,ZOUT,DUM1,DUM2)  


       DO K=KDS,KDE                           
         Z3d(I,J,K)=ZOUT(K)
       ENDDO

      ENDDO
    ENDDO




    DO J = JTS, MIN(JTE,JDE-1)
      DO I = ITS, MIN(ITE,IDE-1)



      PIN=0.;TIN=0.;Y2=0;PIO=0.;TOUT=0.;DUM1=0.;DUM2=0.

       DO K=KDS+1,KDE                           
         PIN(K-1) = EXP((ALOG(PINT(I,J,KDE-K+1))+ALOG(PINT(I,J,KDE-K+2)))*0.5)
         TIN(K-1) = T(I,J,KDE-K+1)
       ENDDO

       IF(PINT(I,J,1) .LE. PSTD(1))THEN
         PIN(KDE-1) = EXP((ALOG(PSTD(1))+ALOG(PSTD(2)))*0.5)
         ZMID     = 0.5*(Z3d_IN(I,J,1)+Z3d_IN(I,J,2))
         TIN(KDE-1) = T(I,J,1) + LAPSR*(ZMID-ZMSLP(I,J))
       ENDIF

       Y2(1    )=0.
       Y2(KDE-1)=0.

       DO K=KDS,KDE-1
          PIO(K)=EXP((ALOG(PSTD(K))+ALOG(PSTD(K+1)))*0.5)
       ENDDO

       CALL SPLINE1(I,J,JTB,KDE-1,PIN,TIN,Y2,KDE-1,PIO,TOUT,DUM1,DUM2)  


       DO K=KDS,KDE-1                           
         T3d(I,J,K)=TOUT(K)
       ENDDO

      ENDDO
    ENDDO





    DO J = JTS, MIN(JTE,JDE-1)
      DO I = ITS, MIN(ITE,IDE-1)




      PIN=0.;QIN=0.;Y2=0;PIO=0.;QOUT=0.;DUM1=0.;DUM2=0.

       DO K=KDS+1,KDE                           
         PIN(K-1) = EXP((ALOG(PINT(I,J,KDE-K+1))+ALOG(PINT(I,J,KDE-K+2)))*0.5)
         QIN(K-1) = Q(I,J,KDE-K+1)
       ENDDO

       IF(PINT(I,J,1) .LE. PSTD(1))THEN
          PIN(KDE-1) = EXP((ALOG(PSTD(1))+ALOG(PSTD(2)))*0.5)

       ENDIF

       Y2(1    )=0.
       Y2(KDE-1)=0.

       DO K=KDS,KDE-1
          PIO(K)=EXP((ALOG(PSTD(K))+ALOG(PSTD(K+1)))*0.5)
       ENDDO

       CALL SPLINE1(I,J,JTB,KDE-1,PIN,QIN,Y2,KDE-1,PIO,QOUT,DUM1,DUM2)  

       DO K=KDS,KDE-1                          
         Q3d(I,J,K)=QOUT(K)
       ENDDO

      ENDDO
    ENDDO

END SUBROUTINE BASE_STATE_PARENT

      SUBROUTINE SPLINE1(I,J,JTBX,NOLD,XOLD,YOLD,Y2,NNEW,XNEW,YNEW,P,Q)
























      IMPLICIT NONE

      INTEGER,INTENT(IN) :: I,J,JTBX,NNEW,NOLD
      REAL,DIMENSION(JTBX),INTENT(IN) :: XNEW,XOLD,YOLD
      REAL,DIMENSION(JTBX),INTENT(INOUT) :: P,Q,Y2
      REAL,DIMENSION(JTBX),INTENT(OUT) :: YNEW

      INTEGER :: II,JJ,K,K1,K2,KOLD,NOLDM1
      REAL :: AK,BK,CK,DEN,DX,DXC,DXL,DXR,DYDXL,DYDXR                 &
             ,RDX,RTDXC,X,XK,XSQ,Y2K,Y2KP1
      CHARACTER(LEN=255) :: message




      II=9999 
      JJ=9999 


      NOLDM1=NOLD-1

      DXL=XOLD(2)-XOLD(1)
      DXR=XOLD(3)-XOLD(2)
      DYDXL=(YOLD(2)-YOLD(1))/DXL
      DYDXR=(YOLD(3)-YOLD(2))/DXR
      RTDXC=0.5/(DXL+DXR)

      P(1)= RTDXC*(6.*(DYDXR-DYDXL)-DXL*Y2(1))
      Q(1)=-RTDXC*DXR

      IF(NOLD.EQ.3)GO TO 150

      K=3

  100 DXL=DXR
      DYDXL=DYDXR
      DXR=XOLD(K+1)-XOLD(K)
      DYDXR=(YOLD(K+1)-YOLD(K))/DXR
      DXC=DXL+DXR
      DEN=1./(DXL*Q(K-2)+DXC+DXC)

      P(K-1)= DEN*(6.*(DYDXR-DYDXL)-DXL*P(K-2))
      Q(K-1)=-DEN*DXR

      K=K+1
      IF(K.LT.NOLD)GO TO 100

  150 K=NOLDM1

  200 Y2(K)=P(K-1)+Q(K-1)*Y2(K+1)

      K=K-1
      IF(K.GT.1)GO TO 200

      K1=1

  300 XK=XNEW(K1)

      DO 400 K2=2,NOLD

      IF(XOLD(K2).GT.XK)THEN
        KOLD=K2-1
        GO TO 450
      ENDIF

  400 CONTINUE

      YNEW(K1)=YOLD(NOLD)
      GO TO 600

  450 IF(K1.EQ.1)GO TO 500
      IF(K.EQ.KOLD)GO TO 550

  500 K=KOLD

      Y2K=Y2(K)
      Y2KP1=Y2(K+1)
      DX=XOLD(K+1)-XOLD(K)
      RDX=1./DX

      AK=.1666667*RDX*(Y2KP1-Y2K)
      BK=0.5*Y2K
      CK=RDX*(YOLD(K+1)-YOLD(K))-.1666667*DX*(Y2KP1+Y2K+Y2K)

  550 X=XK-XOLD(K)
      XSQ=X*X

      YNEW(K1)=AK*XSQ*X+BK*XSQ+CK*X+YOLD(K)





  600 K1=K1+1
      IF(K1.LE.NNEW)GO TO 300

      RETURN
      END SUBROUTINE SPLINE1


SUBROUTINE NEST_TERRAIN ( nest, config_flags )

 USE module_domain
 USE module_configure
 USE module_timing
 USE module_TERRAIN
 USE wrfsi_static
 USE module_SMOOTH_TERRAIN

 IMPLICIT NONE

 TYPE(domain) , POINTER                        :: nest
 TYPE(grid_config_rec_type) , INTENT(IN)       :: config_flags





 LOGICAL, EXTERNAL                 :: wrf_dm_on_monitor
 INTEGER                           :: ids,ide,jds,jde,kds,kde
 INTEGER                           :: ims,ime,jms,jme,kms,kme
 INTEGER                           :: its,ite,jts,jte,kts,kte 
 INTEGER                           :: i_parent_start, j_parent_start
 INTEGER                           :: parent_grid_ratio
 INTEGER                           :: n,i,j,ii,jj,nnxp,nnyp
 INTEGER                           :: i_start,j_start,level
 REAL, DIMENSION(1,1), TARGET      :: nothing
 REAL                              :: lah_nest_11, loh_nest_11
 INTEGER                           :: im_big, jm_big, i_add
 INTEGER                           :: im, jm
 CHARACTER(LEN=6)                  :: nestpath

 integer                           :: input_type
 character(len=128)                :: input_fname
 character (len=32)                :: cname
 integer                           :: ndim
 character (len=3)                 :: memorder
 character (len=32)                :: stagger
 integer, dimension(3)             :: domain_start, domain_end
 integer                           :: wrftype
 character (len=128), dimension(3) :: dimnames

 integer :: istatus
 integer :: handle
 integer :: comm_1, comm_2

 real, allocatable, dimension(:,:,:) :: real_domain

 character (len=10), dimension(24)  :: name = (/ "XLAT_M    ", &
                                                "XLONG_M   ", &
                                                "XLAT_V    ", &
                                                "XLONG_V   ", &
                                                "E         ", &
                                                "F         ", &
                                                "LANDMASK  ", &
                                                "LANDUSEF  ", &
                                                "LU_INDEX  ", &
                                                "HCNVX     ", &
                                                "HSTDV     ", &
                                                "HASYW     ", &
                                                "HASYS     ", &
                                                "HASYSW    ", &
                                                "HASYNW    ", &
                                                "HLENW     ", &
                                                "HLENS     ", &
                                                "HLENSW    ", &
                                                "HLENNW    ", &
                                                "HANIS     ", &
                                                "HSLOP     ", &
                                                "HANGL     ", &
                                                "HZMAX     ", & 
                                                "HGT_M     " /)

 integer, parameter :: IO_BIN=1, IO_NET=2

 integer :: io_form_auxinput2
 integer :: itsok,iteok,jtsok,jteok

 CHARACTER(LEN=512) :: message

 write(message,'("Nest d",I2," entering nest_terrain")') nest%id
 call wrf_debug(1,trim(message))

 call START_TIMING()

 write(message,*)"in NEST_TERRAIN config_flags%io_form_auxinput2 = ", config_flags%io_form_auxinput2
 CALL wrf_debug(2,trim(message))
 write(message,*)"in NEST_TERRAIN config_flags%auxinput2_inname = ", config_flags%auxinput2_inname
 CALL wrf_debug(2,trim(message))

 io_form_auxinput2 = config_flags%io_form_auxinput2
 
 
 
 if (config_flags%auxinput1_inname(1:7) == "met_nmm") then
    input_type = 2
 else
    input_type = 1
 end if



      IDS = nest%sd31
      IDE = nest%ed31
      JDS = nest%sd32
      JDE = nest%ed32
      KDS = nest%sd33
      KDE = nest%ed33

      IMS = nest%sm31
      IME = nest%em31
      JMS = nest%sm32
      JME = nest%em32
      KMS = nest%sm33
      KME = nest%em33

      ITS = nest%sp31
      ITE = nest%ep31
      JTS = nest%sp32
      JTE = nest%ep32
      KTS = nest%sp33
      KTE = nest%ep33

      i_parent_start = nest%i_parent_start
      j_parent_start = nest%j_parent_start
      parent_grid_ratio = nest%parent_grid_ratio

      NNXP=IDE-1
      NNYP=JDE-1
      im = NNXP
      jm = NNYP

       
       call find_ijstart_level (nest,i_start,j_start,level)
       write(message,*)" nest%id =", nest%id , " i_start,j_start,level =", i_start,j_start,level
       CALL wrf_debug(2,trim(message))
       if ( level <= 0 ) then
          CALL wrf_error_fatal3("<stdin>",2960,&
'this routine NEST_TERRAIN should not be called for top-level domain')
       end if

      
      monitor_only: IF ( wrf_dm_on_monitor() ) THEN
         call wrf_debug(1,'NEST_TERRAIN MASTER PROCESS')
         call MASTER(IDS,IDE,JDS,JDE)
      ELSE
         call wrf_debug(1,'NEST_TERRAIN SLAVE PROCESS')
         call SLAVE(IDS,IDE,JDS,JDE)
      ENDIF monitor_only

      if(config_flags%terrain_smoothing==2) then     
         call wrf_debug(1,'Call fast smoother (smooth_terrain)')
         call smooth_terrain(nest,12,12, &
              IDS,IDE,JDS,JDE,KDS,KDE, &
              IMS,IME,JMS,JME,KMS,KME, &
              ITS,ITE,JTS,JTE,KTS,KTE)
      elseif(config_flags%terrain_smoothing==1) then
         continue 
      elseif(config_flags%terrain_smoothing==0) then
         call wrf_debug(1,'Terrain smoothing is disabled.')
      else
         write(message,*) 'Invalid option for terrain_smoothing: ',config_flags%terrain_smoothing
         call wrf_error_fatal3("<stdin>",2985,&
message)
      endif

     DO J = jts,jte
        DO I = its,ite
           nest%hres_fis(i,j)=9.81*nest%hres_avc(i,j)
        ENDDO
     ENDDO

     write(message,'("Nest d",I0," nest_terrain")') nest%id
     call END_TIMING(trim(message))


CONTAINS
  SUBROUTINE SLAVE(IDS,IDE,JDS,JDE)
    IMPLICIT NONE
    integer, intent(in) :: IDS,IDE,JDS,JDE
    REAL, DIMENSION(1,1) :: avc_nest,lnd_nest

     call wrf_debug(1,'call wrf_global_to_patch_real in nest_terrain')
     call wrf_global_to_patch_real(avc_nest,nest%hres_avc,nest%domdesc,'z','xy', &
                                   ids,   ide-1, jds,   jde-1, 1, 1, &
                                   ims,   ime,   jms,   jme,   1, 1, &
                                   its,   ite,   jts,   jte,   1, 1)
     call wrf_global_to_patch_real(lnd_nest,nest%hres_lnd,nest%domdesc,'z','xy', &
                                   ids,   ide-1, jds,   jde-1, 1, 1, &
                                   ims,   ime,   jms,   jme,   1, 1, &
                                   its,   ite,   jts,   jte,   1, 1)
     call wrf_debug(1,'back from wrf_global_to_patch_real in nest_terrain')


  END SUBROUTINE SLAVE
  SUBROUTINE MASTER(IDS,IDE,JDS,JDE)
    IMPLICIT NONE
    integer, intent(in) :: IDS,IDE,JDS,JDE
    REAL, DIMENSION(IDS:IDE,JDS:JDE)     :: avc_nest, lnd_nest
    type(nmm_terrain), pointer :: tr

    nullify(tr)
    avc_nest = 0.0
    lnd_nest = 0.0
    
    tr=>terrain_for(level,input_type,io_form_auxinput2)

    
    i_add = mod(j_start+1,2) 
    DO j=jds,jde
       DO i=ids,ide
          avc_nest(i,j) = tr%avc(i_start+i-1 + mod(j+1,2)*i_add, j_start+j-1)
          lnd_nest(i,j) = tr%lnd(i_start+i-1 + mod(j+1,2)*i_add, j_start+j-1)
       END DO
    END DO

    i=1 ; j=1
    lah_nest_11 = tr%lah(i_start+i-1 + mod(j+1,2)*i_add, j_start+j-1)
    loh_nest_11 = tr%loh(i_start+i-1 + mod(j+1,2)*i_add, j_start+j-1)

    IF(ABS(lah_nest_11-nest%HLAT(1,1)) .GE. 0.5 .OR.  & 
         ABS(loh_nest_11-nest%HLON(1,1)) .GE. 0.5)THEN 

       WRITE(message,*)'SOME MATCHING TEST i_parent_start, j_parent_start',i_parent_start,j_parent_start
       CALL wrf_message(trim(message))
       CALL wrf_message('WRFSI LAT      COMPUTED LAT')
       WRITE(message,*)lah_nest_11,nest%HLAT(1,1)
       CALL wrf_message(trim(message))
       CALL wrf_message('WRFSI LON      COMPUTED LON')
       WRITE(message,*)loh_nest_11,nest%HLON(1,1)
       CALL wrf_message(trim(message))

       CALL wrf_message('CHECK WRFSI CONFIGURATION AND INPUT HIGH RESOLUTION TOPOGRAPHY AND/OR GRID RATIO') 
       CALL wrf_error_fatal3("<stdin>",3056,&
'LATLON MISMATCH: ERROR READING static FILE FOR THE NEST')
    ENDIF

    if(config_flags%terrain_smoothing==1) then
       call wrf_debug(1,'Call slow smoother (smdhld).')
       call smdhld(ids,ide,jds,jde,avc_nest,lnd_nest,12,12)
    endif

    call wrf_debug(1,'call wrf_global_to_patch_real in nest_terrain')
    call wrf_global_to_patch_real(avc_nest,nest%hres_avc,nest%domdesc,'z','xy', &
                                  ids,   ide-1, jds,   jde-1, 1, 1, &
                                  ims,   ime,   jms,   jme,   1, 1, &
                                  its,   ite,   jts,   jte,   1, 1)
    call wrf_global_to_patch_real(lnd_nest,nest%hres_lnd,nest%domdesc,'z','xy', &
                                  ids,   ide-1, jds,   jde-1, 1, 1, &
                                  ims,   ime,   jms,   jme,   1, 1, &
                                  its,   ite,   jts,   jte,   1, 1)
    call wrf_debug(1,'back from wrf_global_to_patch_real in nest_terrain')
  END SUBROUTINE MASTER
     
END SUBROUTINE NEST_TERRAIN





SUBROUTINE med_init_domain_constants_nmm ( parent, nest)   
  
   USE module_domain
   USE module_configure
   USE module_timing
   IMPLICIT NONE
   TYPE(domain) , POINTER                     :: parent, nest, grid


   INTERFACE
     SUBROUTINE med_initialize_nest_nmm ( grid  &   







,szj,s1z,spz,tcs,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist, &
dfi_moist_bxs,dfi_moist_bxe,dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar, &
scalar_bxs,scalar_bxe,scalar_bys,scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs, &
dfi_scalar_bxe,dfi_scalar_bys,dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,chem,ozmixm &


                           )
        USE module_domain
        USE module_configure
        USE module_timing
        IMPLICIT NONE
        TYPE(domain) , POINTER                  :: grid






real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_szj)           :: szj
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_s1z)           :: s1z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_spz)           :: spz
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tcs)           :: tcs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)           :: dfi_scalar
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32,num_chem)           :: chem
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%levsiz,grid%sm32:grid%em32,num_ozmixm)           :: ozmixm

     END SUBROUTINE med_initialize_nest_nmm 
   END INTERFACE








   grid => nest

   CALL med_initialize_nest_nmm( grid &   







,grid%szj,grid%s1z,grid%spz,grid%tcs,grid%moist,grid%moist_bxs,grid%moist_bxe,grid%moist_bys,grid%moist_bye,grid%moist_btxs, &
grid%moist_btxe,grid%moist_btys,grid%moist_btye,grid%dfi_moist,grid%dfi_moist_bxs,grid%dfi_moist_bxe,grid%dfi_moist_bys, &
grid%dfi_moist_bye,grid%dfi_moist_btxs,grid%dfi_moist_btxe,grid%dfi_moist_btys,grid%dfi_moist_btye,grid%scalar,grid%scalar_bxs, &
grid%scalar_bxe,grid%scalar_bys,grid%scalar_bye,grid%scalar_btxs,grid%scalar_btxe,grid%scalar_btys,grid%scalar_btye, &
grid%dfi_scalar,grid%dfi_scalar_bxs,grid%dfi_scalar_bxe,grid%dfi_scalar_bys,grid%dfi_scalar_bye,grid%dfi_scalar_btxs, &
grid%dfi_scalar_btxe,grid%dfi_scalar_btys,grid%dfi_scalar_btye,grid%chem,grid%ozmixm &


                       )

END SUBROUTINE med_init_domain_constants_nmm

SUBROUTINE med_initialize_nest_nmm( grid &







,szj,s1z,spz,tcs,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist, &
dfi_moist_bxs,dfi_moist_bxe,dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar, &
scalar_bxs,scalar_bxe,scalar_bys,scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs, &
dfi_scalar_bxe,dfi_scalar_bys,dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,chem,ozmixm &


                           )

 USE module_domain
 USE module_configure
 USE module_timing
 IMPLICIT NONE



 INTEGER :: ids, ide, jds, jde, kds, kde, &
            ims, ime, jms, jme, kms, kme, &
            its, ite, jts, jte, kts, kte, &
            i, j, k, nnxp, nnyp 

 TYPE(domain) , POINTER                     :: grid



 LOGICAL, EXTERNAL                   :: wrf_dm_on_monitor
 INTEGER                             :: KHH,KVH,JAM,JA,IHL, IHH, L
 INTEGER                             :: II,JJ,ISRCH,ISUM
 INTEGER, ALLOCATABLE, DIMENSION(:)  :: KHL2,KVL2,KHH2,KVH2,KHLA,KHHA,KVLA,KVHA
 INTEGER,PARAMETER                   :: KNUM=SELECTED_REAL_KIND(13)

 REAL(KIND=KNUM)                     :: WB,SB,DLM,DPH,TPH0,STPH0,CTPH0
 REAL(KIND=KNUM)                     :: STPH,CTPH,TDLM,TDPH,FP,TPH,TLM,TLM0
 REAL                                :: TPH0D,TLM0D,ANBI,TSPH,DTAD,DTCF,DT
 REAL                                :: ACDT,CDDAMP,DXP
 REAL                                :: WBD,SBD,WBI,SBI,EBI
 REAL                                :: DY_NMM0
 REAL                                :: RSNOW,SNOFAC
 REAL, ALLOCATABLE, DIMENSION(:)     :: DXJ,WPDARJ,CPGFUJ,CURVJ,   &
                                        FCPJ,FDIVJ,EMJ,EMTJ,FADJ,  &
                                        HDACJ,DDMPUJ,DDMPVJ

 REAL, PARAMETER:: SALP=2.60
 REAL, PARAMETER:: SNUP=0.040
 REAL, PARAMETER:: W_NMM=0.08

 REAL, PARAMETER:: CODAMP=6.4
 REAL, PARAMETER:: TWOM=.00014584
 REAL, PARAMETER:: CP=1004.6
 REAL, PARAMETER:: DFC=1.0    
 REAL, PARAMETER:: DDFC=1.0  
 REAL, PARAMETER:: ROI=916.6
 REAL, PARAMETER:: R=287.04
 REAL, PARAMETER:: CI=2060.0
 REAL, PARAMETER:: ROS=1500.
 REAL, PARAMETER:: CS=1339.2
 REAL, PARAMETER:: DS=0.050
 REAL, PARAMETER:: AKS=.0000005
 REAL, PARAMETER:: DZG=2.85
 REAL, PARAMETER:: DI=.1000
 REAL, PARAMETER:: AKI=0.000001075
 REAL, PARAMETER:: DZI=2.0
 REAL, PARAMETER:: THL=210.
 REAL, PARAMETER:: PLQ=70000.
 REAL, PARAMETER:: ERAD=6371200.
 REAL, PARAMETER:: DTR=0.01745329

 REAL :: COAC

 CHARACTER(LEN=255) :: message

   






real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_szj)           :: szj
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_s1z)           :: s1z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_spz)           :: spz
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tcs)           :: tcs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)           :: dfi_scalar
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32,num_chem)           :: chem
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%levsiz,grid%sm32:grid%em32,num_ozmixm)           :: ozmixm












   CALL get_ijk_from_grid (  grid ,                           &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             its, ite, jts, jte, kts, kte     )






   call nl_get_coac(grid%id,coac)

    DT=grid%dt         
    NNXP=min(ITE,IDE-1)
    NNYP=min(JTE,JDE-1)
    JAM=6+2*((JDE-1)-10)         

    WRITE(message,*)'TIME STEP ON DOMAIN',grid%id,'==',dt
    CALL wrf_message(trim(message))

    WRITE(message,*)'IDS,IDE ON DOMAIN',grid%id,'==',ids,ide
    CALL wrf_message(trim(message))

    ALLOCATE(KHL2(JTS:NNYP),KVL2(JTS:NNYP),KHH2(JTS:NNYP),KVH2(JTS:NNYP))
    ALLOCATE(DXJ(JTS:NNYP),WPDARJ(JTS:NNYP),CPGFUJ(JTS:NNYP),CURVJ(JTS:NNYP))
    ALLOCATE(FCPJ(JTS:NNYP),FDIVJ(JTS:NNYP),FADJ(JTS:NNYP))
    ALLOCATE(HDACJ(JTS:NNYP),DDMPUJ(JTS:NNYP),DDMPVJ(JTS:NNYP))
    ALLOCATE(KHLA(JAM),KHHA(JAM))
    ALLOCATE(KVLA(JAM),KVHA(JAM))










   IF(.not. grid%analysis)THEN
    DO J = JTS, MIN(JTE,JDE-1)
     DO I = ITS, MIN(ITE,IDE-1)

      IF (grid%sm(I,J).GT.0.9) THEN                              
         grid%epsr(I,J)= 0.97
         grid%embck(I,J)= 0.97
         grid%gffc(I,J)= 0.
         grid%albedo(I,J)=.06
         grid%albase(I,J)=.06
      ENDIF

      IF (grid%sice(I,J).GT.0.9) THEN                            
         grid%sm(I,J)=0.
         grid%si(I,J)=0.
         grid%gffc(I,J)=0.
         grid%albedo(I,J)=.60
         grid%albase(I,J)=.60
      ENDIF

      IF (grid%sm(I,J).LT.0.5.AND.grid%sice(I,J).LT.0.5) THEN         
           grid%si(I,J)=5.0*grid%weasd(I,J)/1000. 
           grid%epsr(I,J)=1.0                
           grid%embck(I,J)=1.0               
           grid%gffc(I,J)=0.0                
           grid%sno(I,J)=grid%si(I,J)*.20         
      ENDIF

     ENDDO
    ENDDO




    DO J = JTS, MIN(JTE,JDE-1)
     DO I = ITS, MIN(ITE,IDE-1)
         grid%vegfra(I,J)=grid%vegfrc(I,J)
     ENDDO
    ENDDO




 
    DO J = JTS, MIN(JTE,JDE-1) 
     DO I = ITS, MIN(ITE,IDE-1)

          IF(grid%sm(I,J).LT.0.9.AND.grid%sice(I,J).LT.0.9) THEN

            IF ( (grid%sno(I,J) .EQ. 0.0) .OR. &            
                           (grid%albase(I,J) .GE. grid%mxsnal(I,J) ) ) THEN
              grid%albedo(I,J) = grid%albase(I,J)
            ELSE
              IF (grid%sno(I,J) .LT. SNUP) THEN             
                  RSNOW = grid%sno(I,J)/SNUP                
                  SNOFAC = 1. - ( EXP(-SALP*RSNOW) - RSNOW*EXP(-SALP))
              ELSE
                  SNOFAC = 1.0                         
              ENDIF
              grid%albedo(I,J) = grid%albase(I,J) &
                          + (1.0-grid%vegfra(I,J))*SNOFAC*(grid%mxsnal(I,J)-grid%albase(I,J))
            ENDIF

          END IF

          grid%si(I,J)=5.0*grid%weasd(I,J)
          grid%sno(I,J)=grid%weasd(I,J)


        IF (grid%sm(I,J) .gt. 0.5)THEN 
           grid%landmask(I,J)=0.0
        ELSE 
           grid%landmask(I,J)=1.0
        ENDIF 

        IF (grid%sice(I,J) .eq. 1.0) then 
         grid%isltyp(I,J)=16
         grid%ivgtyp(I,J)=24
        ENDIF 

     ENDDO
    ENDDO








    DO J = JTS, MIN(JTE,JDE-1)
     DO I = ITS,MIN(ITE,IDE-1)
      IF(grid%sm(I,J).GT.0.9 .AND. grid%vegfra(I,J) .NE. 0) THEN
        WRITE(message,*)'PROBLEM AT THE LAND-WATER INTERFACE (VEGFRA):', &
             I,J,grid%sm(I-1,J),grid%vegfra(I-1,j),grid%sm(I,J),grid%vegfra(I,J)
        CALL wrf_message(trim(message))
      ENDIF

      
      
      
     ENDDO
    ENDDO




        grid%rtdpth=0.
        grid%rtdpth(1)=0.1
        grid%rtdpth(2)=0.3
        grid%rtdpth(3)=0.6



        grid%sldpth=0.
        grid%sldpth(1)=0.1
        grid%sldpth(2)=0.3
        grid%sldpth(3)=0.6
        grid%sldpth(4)=1.0


   ENDIF 


    DO J = JTS, MIN(JTE,JDE-1)
     DO I = ITS, MIN(ITE,IDE-1)
       grid%res(I,J)=1.
     ENDDO
    ENDDO




 
    grid%hbm2=0.
    DO J = JTS, MIN(JTE,JDE-1)
      DO I = ITS, MIN(ITE,IDE-1)
       IF((J .GE. 3 .and. J .LE. (JDE-1)-2) .AND.   &
          (I .GE. 2 .and. I .LE. (IDE-1)-2+MOD(J,2))) THEN
          grid%hbm2(I,J)=1.
        ENDIF
      ENDDO
    ENDDO   



    grid%hbm3=0.
    DO J=JTS,MIN(JTE,JDE-1)
     grid%ihwg(J)=mod(J+1,2)-1
      IF (J .ge. 4 .and. J .le. (JDE-1)-3) THEN
        IHL=(IDS+1)-grid%ihwg(J) 
        IHH=(IDE-1)-2
        DO I=ITS,MIN(ITE,IDE-1)
           IF (I .ge. IHL  .and. I .le. IHH) grid%hbm3(I,J)=1.
        ENDDO 
      ENDIF
    ENDDO 
   


    grid%vbm2=0.
    DO J=JTS,MIN(JTE,JDE-1)
     DO I=ITS,MIN(ITE,IDE-1)
       IF((J .ge. 3 .and. J .le. (JDE-1)-2)  .AND.  &
          (I .ge. 2 .and. I .le. (IDE-1)-1-MOD(J,2))) THEN
           grid%vbm2(I,J)=1.
       ENDIF
     ENDDO
    ENDDO



    grid%vbm3=0.
    DO J=JTS,MIN(JTE,JDE-1)
      DO I=ITS,MIN(ITE,IDE-1)
        IF((J .ge. 4 .and. J .le. (JDE-1)-3)  .AND.  &
           (I .ge. 3-MOD(J,2) .and. I .le. (IDE-1)-2)) THEN
           grid%vbm3(I,J)=1.
        ENDIF
      ENDDO
    ENDDO

    TPH0D  = grid%CEN_LAT
    TLM0D  = grid%CEN_LON
    TPH0   = TPH0D*DTR
    WBD    = grid%WBD0   
    WB     = WBD*DTR
    SBD    = grid%SBD0   
    SB     = SBD*DTR
    DLM    = grid%dlmd*DTR  
    DPH    = grid%dphd*DTR  
    TDLM   = DLM+DLM
    TDPH   = DPH+DPH
    WBI    = WB+TDLM
    SBI    = SB+TDPH
    EBI    = WB+((ide-1)-2)*TDLM  
    ANBI   = SB+((jde-1)-3)*DPH   
    STPH0  = SIN(TPH0)
    CTPH0  = COS(TPH0)
    TSPH   = 3600./grid%DT
    DTAD   = 1.0
    DTCF   = 4.0    
    DY_NMM0= grid%dy_nmm 




    DO J=JTS,MIN(JTE,JDE-1)
      TLM0=WB-TDLM+MOD(J,2)*DLM           
      TPH =SB+float(J-1)*DPH
      STPH=SIN(TPH)
      CTPH=COS(TPH)
      DO I=ITS,MIN(ITE,IDE-1)
         TLM=TLM0 + I*TDLM
         FP=TWOM*(CTPH0*STPH+STPH0*CTPH*COS(TLM))
         grid%f(I,J)=0.5*grid%DT*FP
      ENDDO
    ENDDO


    DO J=JTS,MIN(JTE,JDE-1)
      KHL2(J)=(IDE-1)*(J-1)-(J-1)/2+2
      KVL2(J)=(IDE-1)*(J-1)-J/2+2
      KHH2(J)=(IDE-1)*J-J/2-1
      KVH2(J)=(IDE-1)*J-(J+1)/2-1
    ENDDO


    TPH=SB-DPH
    DO J=JTS,MIN(JTE,JDE-1)
      TPH=SB+float(J-1)*DPH
      DXP=ERAD*DLM*COS(TPH)
      DXJ(J)=DXP
      WPDARJ(J)=-W_NMM*((ERAD*DLM*AMIN1(COS(ANBI),COS(SBI)))**2+DY_NMM0**2)/  & 
                (grid%DT*32.*DXP*DY_NMM0)
      CPGFUJ(J)=-grid%DT/(48.*DXP)
      CURVJ(J)=.5*grid%DT*TAN(TPH)/ERAD
      FCPJ(J)=grid%DT/(CP*192.*DXP*DY_NMM0)
      FDIVJ(J)=1./(12.*DXP*DY_NMM0)
      FADJ(J)=-grid%DT/(48.*DXP*DY_NMM0)*DTAD
      ACDT=grid%DT*SQRT((ERAD*DLM*AMIN1(COS(ANBI),COS(SBI)))**2+DY_NMM0**2)
      CDDAMP=CODAMP*ACDT
      HDACJ(J)=COAC*ACDT/(4.*DXP*DY_NMM0)
      DDMPUJ(J)=CDDAMP/DXP
      DDMPVJ(J)=CDDAMP/DY_NMM0
    ENDDO



     WRITE(message,*)'NEW CHANGE',grid%f4d,grid%ef4t,grid%f4q
     CALL wrf_message(trim(message))

      DO L=KDS,KDE-1
        grid%rdeta(L)=1./grid%deta(L)
        grid%f4q2(L)=-.25*grid%DT*DTAD/grid%deta(L)
      ENDDO

       DO J=JTS,MIN(JTE,JDE-1)
        DO I=ITS,MIN(ITE,IDE-1)
          grid%dx_nmm(I,J)=DXJ(J)
          grid%wpdar(I,J)=WPDARJ(J)*grid%hbm2(I,J)
          grid%cpgfu(I,J)=CPGFUJ(J)*grid%vbm2(I,J)
          grid%curv(I,J)=CURVJ(J)*grid%vbm2(I,J)
          grid%fcp(I,J)=FCPJ(J)*grid%hbm2(I,J)
          grid%fdiv(I,J)=FDIVJ(J)*grid%hbm2(I,J)
          grid%fad(I,J)=FADJ(J)
          grid%hdacv(I,J)=HDACJ(J)*grid%vbm2(I,J)
          grid%hdac(I,J)=HDACJ(J)*1.25*grid%hbm2(I,J)
        ENDDO
       ENDDO

       DO J=JTS, MIN(JTE,JDE-1)
        IF (J.LE.5.OR.J.GE.(JDE-1)-4) THEN
          KHH=(IDE-1)-2+MOD(J,2) 
          DO I=ITS,MIN(ITE,IDE-1)
             IF (I .ge. 2 .and. I .le. KHH) THEN
               grid%hdac(I,J)=grid%hdac(I,J)* DFC
             ENDIF
          ENDDO
        ELSE
          KHH=2+MOD(J,2)
          DO I=ITS,MIN(ITE,IDE-1)
             IF (I .ge. 2 .and. I .le. KHH) THEN
               grid%hdac(I,J)=grid%hdac(I,J)* DFC
             ENDIF
          ENDDO
          KHH=(IDE-1)-2+MOD(J,2)

          DO I=ITS,MIN(ITE,IDE-1)
             IF (I .ge. (IDE-1)-2 .and. I .le. KHH) THEN
               grid%hdac(I,J)=grid%hdac(I,J)* DFC
             ENDIF
          ENDDO
        ENDIF
      ENDDO

      DO J=JTS,min(JTE,JDE-1)
      DO I=ITS,min(ITE,IDE-1)
        grid%ddmpu(I,J)=DDMPUJ(J)*grid%vbm2(I,J)
        grid%ddmpv(I,J)=DDMPVJ(J)*grid%vbm2(I,J)
        grid%hdacv(I,J)=grid%hdacv(I,J)*grid%vbm2(I,J)
      ENDDO
      ENDDO



        DO J=JTS,MIN(JTE,JDE-1)
        IF (J.LE.5.OR.J.GE.JDE-1-4) THEN
          KVH=(IDE-1)-1-MOD(J,2)
          DO I=ITS,MIN(ITE,IDE-1)
            IF (I .ge. 2 .and.  I .le. KVH) THEN
             grid%ddmpu(I,J)=grid%ddmpu(I,J)*DDFC
             grid%ddmpv(I,J)=grid%ddmpv(I,J)*DDFC
             grid%hdacv(I,J)=grid%hdacv(I,J)*DFC
            ENDIF
          ENDDO
        ELSE
          KVH=3-MOD(J,2)
          DO I=ITS,MIN(ITE,IDE-1)
           IF (I .ge. 2 .and.  I .le. KVH) THEN
            grid%ddmpu(I,J)=grid%ddmpu(I,J)*DDFC
            grid%ddmpv(I,J)=grid%ddmpv(I,J)*DDFC
            grid%hdacv(I,J)=grid%hdacv(I,J)*DFC
           ENDIF
          ENDDO
          KVH=(IDE-1)-1-MOD(J,2)
          DO I=ITS,MIN(ITE,IDE-1)
           IF (I .ge. IDE-1-2 .and. I .le. KVH) THEN
            grid%ddmpu(I,J)=grid%ddmpu(I,J)*DDFC
            grid%ddmpv(I,J)=grid%ddmpv(I,J)*DDFC
            grid%hdacv(I,J)=grid%hdacv(I,J)*DFC
           ENDIF
          ENDDO
        ENDIF
      ENDDO


 
     DO J = JTS, MIN(JTE,JDE-1)
       DO I = ITS, MIN(ITE,IDE-1)
          grid%GLAT(I,J)=grid%HLAT(I,J)*DTR
          grid%GLON(I,J)=grid%HLON(I,J)*DTR
       ENDDO
     ENDDO




      
     ALLOCATE(EMJ(JDS:JDE-1),EMTJ(JDS:JDE-1))
     DO J=JDS,JDE-1
       TPH=SB+float(J-1)*DPH
       DXP=ERAD*DLM*COS(TPH)
       EMJ(J)= grid%DT/( 4.*DXP)*DTAD
       EMTJ(J)=grid%DT/(16.*DXP)*DTAD
     ENDDO

          JA=0
          DO 161 J=3,5
          JA=JA+1
          KHLA(JA)=2
          KHHA(JA)=(IDE-1)-1-MOD(J+1,2)
 161      grid%emt(JA)=EMTJ(J)
          DO 162 J=(JDE-1)-4,(JDE-1)-2
          JA=JA+1
          KHLA(JA)=2
          KHHA(JA)=(IDE-1)-1-MOD(J+1,2)
 162      grid%emt(JA)=EMTJ(J)
          DO 163 J=6,(JDE-1)-5
          JA=JA+1
          KHLA(JA)=2
          KHHA(JA)=2+MOD(J,2)
 163      grid%emt(JA)=EMTJ(J)
          DO 164 J=6,(JDE-1)-5
          JA=JA+1
          KHLA(JA)=(IDE-1)-2
          KHHA(JA)=(IDE-1)-1-MOD(J+1,2)
 164      grid%emt(JA)=EMTJ(J)



          JA=0
          DO 171 J=3,5
          JA=JA+1
          KVLA(JA)=2
          KVHA(JA)=(IDE-1)-1-MOD(J,2)
 171      grid%em(JA)=EMJ(J)
          DO 172 J=(JDE-1)-4,(JDE-2)-2
          JA=JA+1
          KVLA(JA)=2
          KVHA(JA)=(IDE-1)-1-MOD(J,2)
 172      grid%em(JA)=EMJ(J)
          DO 173 J=6,(JDE-1)-5
          JA=JA+1
          KVLA(JA)=2
          KVHA(JA)=2+MOD(J+1,2)
 173      grid%em(JA)=EMJ(J)
          DO 174 J=6,(JDE-1)-5
          JA=JA+1
          KVLA(JA)=(IDE-1)-2
          KVHA(JA)=(IDE-1)-1-MOD(J,2)
 174      grid%em(JA)=EMJ(J)






                                                                
        IF (ABS(IDE-1-ITE) .eq. 1 ) THEN                        
         CALL wrf_message('zero phantom winds')                 
         DO K=KDS,KDE-1                                         
          DO J=JDS,JDE-1,2                                      
           IF (J .ge. JTS .and. J .le. JTE) THEN                
             grid%u(IDE-1,J,K)=0.                                    
             grid%v(IDE-1,J,K)=0.                                    
           ENDIF                                                
          ENDDO                                                 
         ENDDO                                                  
        ENDIF                                                   


























    DEALLOCATE(KHL2,KVL2,KHH2,KVH2)
    DEALLOCATE(DXJ,WPDARJ,CPGFUJ,CURVJ)
    DEALLOCATE(FCPJ,FDIVJ,FADJ)
    DEALLOCATE(HDACJ,DDMPUJ,DDMPVJ)
    DEALLOCATE(KHLA,KHHA)
    DEALLOCATE(KVLA,KVHA)


END SUBROUTINE med_initialize_nest_nmm




LOGICAL FUNCTION cd_feedback_mask_orig( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, xstag, ystag )
   INTEGER, INTENT(IN) :: pig, ips_save, ipe_save , pjg, jps_save, jpe_save
   LOGICAL, INTENT(IN) :: xstag, ystag

   INTEGER ioff, joff

   ioff = 0 ; joff = 0
   IF ( xstag  ) ioff = 1
   IF ( ystag  ) joff = 1

   cd_feedback_mask_orig = ( pig .ge. ips_save+2 .and. &
                            pjg .ge. jps_save+3 .and. &
                            pig .le. ipe_save-2-mod(pjg-jps_save,2) .and. &
                            pjg .le. jpe_save-3 )

END FUNCTION cd_feedback_mask_orig

LOGICAL FUNCTION cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, xstag, ystag )
   INTEGER, INTENT(IN) :: pig, ips_save, ipe_save , pjg, jps_save, jpe_save
   LOGICAL, INTENT(IN) :: xstag, ystag

   INTEGER ioff, joff

   ioff = 0 ; joff = 0
   IF ( xstag  ) ioff = 1
   IF ( ystag  ) joff = 1

   cd_feedback_mask = ( pig .ge. ips_save+1 .and. &
                            pjg .ge. jps_save+2 .and. &
                            pig .le. ipe_save-1-mod(pjg-jps_save,2) .and. &
                            pjg .le. jpe_save-2 )

END FUNCTION cd_feedback_mask

LOGICAL FUNCTION cd_feedback_mask_v( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, xstag, ystag )
   INTEGER, INTENT(IN) :: pig, ips_save, ipe_save , pjg, jps_save, jpe_save
   LOGICAL, INTENT(IN) :: xstag, ystag

   INTEGER ioff, joff

   ioff = 0 ; joff = 0
   IF ( xstag  ) ioff = 1
   IF ( ystag  ) joff = 1
   
   cd_feedback_mask_v = ( pig .ge. ips_save+2 .and. &
                            pjg .ge. jps_save+3 .and. &
                            pig .le. ipe_save-2-mod(pjg-jps_save+1,2) .and. &
                            pjg .le. jpe_save-3 )

END FUNCTION cd_feedback_mask_v




RECURSIVE SUBROUTINE find_ijstart_level ( grid, i_start, j_start, level )



   USE module_domain

   IMPLICIT NONE

   

   TYPE(domain) :: grid
   INTEGER, INTENT (OUT) :: i_start, j_start, level
   INTEGER :: iadd

      if (grid%parent_id == 0 ) then
         i_start = 1
         j_start = 1
         level = 0
      else
         call find_ijstart_level ( grid%parents(1)%ptr, i_start, j_start, level )
         if (level > 0) then
             iadd = (i_start-1)*3
             if ( mod(j_start,2).ne.0 .and. mod(grid%j_parent_start,2).ne.0 ) iadd = iadd - 1
             if ( mod(j_start,2).eq.0 .and. mod(grid%j_parent_start,2).eq.0 ) iadd = iadd + 2
         else
             iadd = -mod(grid%j_parent_start,2)
         end if
         i_start = iadd + grid%i_parent_start*3 - 1
         j_start = ( (j_start-1) + (grid%j_parent_start-1) ) * 3 + 1
         level = level + 1
      end if

END SUBROUTINE find_ijstart_level
