	  �0  k   k820309    4          12.1        �S^Y                                                                                                           
       CRTM_Utility.f90 CRTM_UTILITY              MATINV DOUBLE_GAUSS_QUADRATURE LEGENDRE_M ASYMTX ASYMTX_TL ASYMTX_AD ASYMTX2_AD gen@LEGENDRE                      @                             
                            @                              
                            @                             
                                                              u #LEGENDRE_SCALAR    #LEGENDRE_RANK1 	   #         @     @X                                              #LEGENDRE_SCALAR%REAL    #MOA    #ANG    #PL                  @             @                   REAL           
   @                                                   
   @                                  
               D  @  �                                               
 
    p           & p         5 � p        r          5 � p        r    p         p                          #         @     @X                            	                  #LEGENDRE_RANK1%REAL 
   #MOA    #NU    #ANG    #PL                  @             @              
     REAL           
   @                                                   
   @                                                  
   @                                                 
    p          5 � p        r        5 � p        r                               D  @  �                                               
       p          5 � p        r    p         p        p           & p         5 � p        r      5 � p        r          5 � p        r    p         p          5 � p        r                                                                                                                                                                                                                                                                                   
                
                  -DT�!	@        3.141592653589793238462643383279                                                 
                
                       �?        0.25                                                 
                
                       �?        0.5                                                 
                
                       �?        1.0                                                 
                
                        @        2.0                                                                                                    0                                                                                                   3                                                 
                
                                 0.0#         @ �                                                 #DISPLAY_MESSAGE%TRIM    #DISPLAY_MESSAGE%PRESENT    #ROUTINE_NAME    #MESSAGE    #ERROR_STATE    #MESSAGE_LOG                  @                                 TRIM               @                                 PRESENT           
   @                                                 1           
   @                                                 1           
   @                                                   
  @                                                 1 (        `                                                    	               
    #MATINV%MAXLOC !   #MATINV%SIZE "   #MATINV%ABS #   #A $   #ERROR_STATUS %     p        H r "     7
S
O p        j                      j                        n                                          1            p          H r "     7
S
O p        j                      j                        n                                          1              H r "     7
S
O p        j                      j                        n                                      2                H r "     7
S
O p        j                      j                        n                                          1              H r "     7
S
O p        j                      j                        n                                      2                                                         @                            !     MAXLOC               @                            "     SIZE               @                            #     ABS        0  
 @@                             $                   
              &                   &                                                     D  @                              %            #         @                                  &                  #DOUBLE_GAUSS_QUADRATURE%ABS '   #DOUBLE_GAUSS_QUADRATURE%COS (   #NUM )   #ABSCISSAS *   #WEIGHTS +                 @                            '     ABS               @                            (     COS           
   @                              )                     D  @                             *                   
               &                                                     D  @                             +                   
               &                                           #         @                                  ,                  #LEGENDRE_M%FLOAT -   #LEGENDRE_M%SQRT .   #MF /   #N 0   #U 1   #PL 2                 @                            -     FLOAT               @                            .     SQRT           
  @@                              /                     
   @                              0                     
   @                             1     
               D  @  �                           2                    
     p           & p         5 � p        r 0         5 � p        r 0   p         p                          #         @                                  3                  #ASYMTX%INT 4   #ASYMTX%MIN0 5   #ASYMTX%SIGN 6   #ASYMTX%DABS 7   #ASYMTX%DSQRT 8   #ASYMTX%SQRT 9   #AAD :   #M ;   #IA <   #IEVEC =   #EVECD >   #EVALD ?   #IER @                 @                            4     INT               @                            5     MIN0               @                            6     SIGN               @                            7     DABS               @                            8     DSQRT               @                            9     SQRT           D  @                             :                   
               &                   &                                                       @                              ;                        @                              <                        @                              =                      D  @                             >                   
               &                   &                                                     D  @                             ?                   
               &                                                     D  @                              @            #         @                                  A               	   #ASYMTX_TL%MATMUL B   #ASYMTX_TL%TRIM C   #ASYMTX_TL%ABS D   #COS_ANGLE E   #N_STREAMS F   #NZ G   #V H   #VAL I   #A_TL J   #V_TL K   #VAL_TL L   #ERROR_STATUS M                 @                            B     MATMUL               @                            C     TRIM               @                            D     ABS           
   @                             E     
                  @                              F                        @                              G                        @                             H                   
               &                   &                                                       @                             I                   
               &                                                       @                             J                   
               &                   &                                                     D  @                             K                   
               &                   &                                                     D  @                             L                   
               &                                                     D @@                              M            #         @                                  N               	   #ASYMTX_AD%TRANSPOSE O   #ASYMTX_AD%MATMUL P   #ASYMTX_AD%TRIM Q   #ASYMTX_AD%ABS R   #COS_ANGLE S   #N_STREAMS T   #NZ U   #V V   #VAL W   #V_AD X   #VAL_AD Y   #A_AD Z   #ERROR_STATUS [                 @                            O     TRANSPOSE               @                            P     MATMUL               @                            Q     TRIM               @                            R     ABS             @                             S     
                   @                              T                        @                              U                       @@                             V                   
               &                   &                                                       @                             W                   
               &                                                       @                             X                   
               &                   &                                                       @                             Y                   
               &                                                     D  @                             Z                   
               &                   &                                                     D @@                              [            #         @                                  \               	   #ASYMTX2_AD%TRANSPOSE ]   #ASYMTX2_AD%MATMUL ^   #ASYMTX2_AD%ABS _   #COS_ANGLE `   #N_STREAMS a   #NZ b   #V c   #VAL d   #V_AD e   #VAL_AD f   #A_AD g   #ERROR_STATUS h                 @                            ]     TRANSPOSE               @                            ^     MATMUL               @                            _     ABS             @                             `     
                   @                              a                        @                              b                       @@                             c                   
                &                   &                                                       @                             d                   
 #              &                                                     D  @                             e                   
 !              &                   &                                                     D  @                             f                   
 $              &                                                     D  @                             g                   
 "              &                   &                                                     D @@                              h               �   &      fn#fn "   �   m   b   uapp(CRTM_UTILITY    3  @   J  TYPE_KINDS     s  @   J  CRTM_PARAMETERS     �  @   J  MESSAGE_HANDLER    �  i       gen@LEGENDRE     \  |      LEGENDRE_SCALAR %   �  =      LEGENDRE_SCALAR%REAL $     @   a   LEGENDRE_SCALAR%MOA $   U  @   a   LEGENDRE_SCALAR%ANG #   �  �   a   LEGENDRE_SCALAR%PL    y  �      LEGENDRE_RANK1 $   �  =      LEGENDRE_RANK1%REAL #   9  @   a   LEGENDRE_RANK1%MOA "   y  @   a   LEGENDRE_RANK1%NU #   �  �   a   LEGENDRE_RANK1%ANG "   m  t  a   LEGENDRE_RANK1%PL    �  p       FP+TYPE_KINDS #   Q  p       FP_KIND+TYPE_KINDS #   �  �       PI+CRTM_PARAMETERS )   Q	  t       POINT_25+CRTM_PARAMETERS (   �	  s       POINT_5+CRTM_PARAMETERS $   8
  s       ONE+CRTM_PARAMETERS $   �
  s       TWO+CRTM_PARAMETERS (     q       SUCCESS+MESSAGE_HANDLER (   �  q       FAILURE+MESSAGE_HANDLER %      s       ZERO+CRTM_PARAMETERS 0   s  �       DISPLAY_MESSAGE+MESSAGE_HANDLER :   3  =      DISPLAY_MESSAGE%TRIM+MESSAGE_HANDLER=TRIM @   p  @      DISPLAY_MESSAGE%PRESENT+MESSAGE_HANDLER=PRESENT =   �  L   e   DISPLAY_MESSAGE%ROUTINE_NAME+MESSAGE_HANDLER 8   �  L   e   DISPLAY_MESSAGE%MESSAGE+MESSAGE_HANDLER <   H  @   e   DISPLAY_MESSAGE%ERROR_STATE+MESSAGE_HANDLER <   �  L   e   DISPLAY_MESSAGE%MESSAGE_LOG+MESSAGE_HANDLER    �  j      MATINV    >  ?      MATINV%MAXLOC    }  =      MATINV%SIZE    �  <      MATINV%ABS    �  �   a   MATINV%A $   �  @   a   MATINV%ERROR_STATUS (   �  �       DOUBLE_GAUSS_QUADRATURE ,   �  <      DOUBLE_GAUSS_QUADRATURE%ABS ,   �  <      DOUBLE_GAUSS_QUADRATURE%COS ,     @   a   DOUBLE_GAUSS_QUADRATURE%NUM 2   A  �   a   DOUBLE_GAUSS_QUADRATURE%ABSCISSAS 0   �  �   a   DOUBLE_GAUSS_QUADRATURE%WEIGHTS    Y  �       LEGENDRE_M !   �  >      LEGENDRE_M%FLOAT     (  =      LEGENDRE_M%SQRT    e  @   a   LEGENDRE_M%MF    �  @   a   LEGENDRE_M%N    �  @   a   LEGENDRE_M%U    %  �   a   LEGENDRE_M%PL    	  �       ASYMTX    �  <      ASYMTX%INT    5  =      ASYMTX%MIN0    r  =      ASYMTX%SIGN    �  =      ASYMTX%DABS    �  >      ASYMTX%DSQRT    *  =      ASYMTX%SQRT    g  �   a   ASYMTX%AAD      @   a   ASYMTX%M    K  @   a   ASYMTX%IA    �  @   a   ASYMTX%IEVEC    �  �   a   ASYMTX%EVECD    o  �   a   ASYMTX%EVALD    �  @   a   ASYMTX%IER    ;  �       ASYMTX_TL !   (   ?      ASYMTX_TL%MATMUL    g   =      ASYMTX_TL%TRIM    �   <      ASYMTX_TL%ABS $   �   @   a   ASYMTX_TL%COS_ANGLE $    !  @   a   ASYMTX_TL%N_STREAMS    `!  @   a   ASYMTX_TL%NZ    �!  �   a   ASYMTX_TL%V    D"  �   a   ASYMTX_TL%VAL    �"  �   a   ASYMTX_TL%A_TL    t#  �   a   ASYMTX_TL%V_TL !   $  �   a   ASYMTX_TL%VAL_TL '   �$  @   a   ASYMTX_TL%ERROR_STATUS    �$        ASYMTX_AD $   �%  B      ASYMTX_AD%TRANSPOSE !   ,&  ?      ASYMTX_AD%MATMUL    k&  =      ASYMTX_AD%TRIM    �&  <      ASYMTX_AD%ABS $   �&  @   a   ASYMTX_AD%COS_ANGLE $   $'  @   a   ASYMTX_AD%N_STREAMS    d'  @   a   ASYMTX_AD%NZ    �'  �   a   ASYMTX_AD%V    H(  �   a   ASYMTX_AD%VAL    �(  �   a   ASYMTX_AD%V_AD !   x)  �   a   ASYMTX_AD%VAL_AD    *  �   a   ASYMTX_AD%A_AD '   �*  @   a   ASYMTX_AD%ERROR_STATUS    �*  �       ASYMTX2_AD %   �+  B      ASYMTX2_AD%TRANSPOSE "   ,  ?      ASYMTX2_AD%MATMUL    ^,  <      ASYMTX2_AD%ABS %   �,  @   a   ASYMTX2_AD%COS_ANGLE %   �,  @   a   ASYMTX2_AD%N_STREAMS    -  @   a   ASYMTX2_AD%NZ    Z-  �   a   ASYMTX2_AD%V    �-  �   a   ASYMTX2_AD%VAL     �.  �   a   ASYMTX2_AD%V_AD "   ./  �   a   ASYMTX2_AD%VAL_AD     �/  �   a   ASYMTX2_AD%A_AD (   ^0  @   a   ASYMTX2_AD%ERROR_STATUS 