	  m  D   k820309    4          12.1        �S^Y                                                                                                           
       Zeeman_Input_Define.f90 ZEEMAN_INPUT_DEFINE              ZEEMAN_INPUT_TYPE ZEEMAN_INPUT_GETVALUE ZEEMAN_INPUT_SETVALUE ZEEMAN_INPUT_ISVALID ZEEMAN_INPUT_INSPECT ZEEMAN_INPUT_DEFINEVERSION i@                      @                              
       FP                      @                              
       INFORMATION DISPLAY_MESSAGE                      @                              
       u@LESSTHAN                                                            #ZEEMAN_INPUT_EQUAL    %         H    @X                                                      #X    #Y              
   @                                                  #ZEEMAN_INPUT_TYPE              
   @                                                  #ZEEMAN_INPUT_TYPE                                                              o #EQUALTO_REAL_SINGLE    #EQUALTO_REAL_DOUBLE    #EQUALTO_COMPLEX_SINGLE    #EQUALTO_COMPLEX_DOUBLE    %         H    @                                                    #EQUALTO_REAL_SINGLE%ABS 	   #EQUALTO_REAL_SINGLE%SPACING 
   #EQUALTO_REAL_SINGLE%MAX    #X    #Y                  @                            	     ABS               @                            
     SPACING               @                                 MAX           
   @                                  	                
   @                                  	      %         H    @                                                   #EQUALTO_REAL_DOUBLE%ABS    #EQUALTO_REAL_DOUBLE%SPACING    #EQUALTO_REAL_DOUBLE%MAX    #X    #Y                  @                                 ABS               @                                 SPACING               @                                 MAX           
   @                                  
                
   @                                  
      %         H    @                                                    #EQUALTO_COMPLEX_SINGLE%REAL    #EQUALTO_COMPLEX_SINGLE%AIMAG    #X    #Y                  @              @                  REAL               @                                 AIMAG           
   @                                                  
   @                                        %         H    @                                                    #EQUALTO_COMPLEX_DOUBLE%REAL    #EQUALTO_COMPLEX_DOUBLE%AIMAG    #X    #Y                  @              @                  REAL               @                                 AIMAG           
   @                                                  
   @                                                                                                                                                                                                                                                 1#         @ �                                                  #DISPLAY_MESSAGE%TRIM !   #DISPLAY_MESSAGE%PRESENT "   #ROUTINE_NAME #   #MESSAGE $   #ERROR_STATE %   #MESSAGE_LOG &                 @                            !     TRIM               @                            "     PRESENT           
   @                             #                    1           
   @                             $                    1           
   @                              %                     
  @                             &                    1                �  @                               '                     #BE '   #COS_THETAB (   #COS_PHIB )   #DOPPLER_SHIFT *               � D                             '               
                           �                    
                 333333�?        0.3                � D                             (              
                           �                    
                                 0.0                � D                             )              
                           �                    
                                 0.0                � D                             *              
                           �                    
                                 0.0    #         H        
                          +                  #ZEEMAN_INPUT_GETVALUE%PRESENT ,   #ZEEMAN_INPUT -   #FIELD_STRENGTH .   #COS_THETAB /   #COS_PHIB 0   #DOPPLER_SHIFT 1                 @                            ,     PRESENT           
   @                              -                    #ZEEMAN_INPUT_TYPE              F @@                             .     
                 F @@                             /     
                 F @@                             0     
                 F @@                             1     
       #         H        
                          2                  #ZEEMAN_INPUT_SETVALUE%PRESENT 3   #ZEEMAN_INPUT 4   #FIELD_STRENGTH 5   #COS_THETAB 6   #COS_PHIB 7   #DOPPLER_SHIFT 8                 @                            3     PRESENT           
D  @                              4                     #ZEEMAN_INPUT_TYPE              
 @@                             5     
                
 @@                             6     
                
 @@                             7     
                
 @@                             8     
      %         @                                 9                          #ZEEMAN_INPUT_ISVALID%ABS :   #ZEEMAN_INPUT_ISVALID%TRIM ;   #Z <                 @                            :     ABS               @                            ;     TRIM           
   @                              <                    #ZEEMAN_INPUT_TYPE    #         @                                  =                   #Z >             
   @                              >                    #ZEEMAN_INPUT_TYPE    #         @                                  ?                   #ID @             D  @                             @                     1    �   4      fn#fn )   �   �   b   uapp(ZEEMAN_INPUT_DEFINE    l  C   J  TYPE_KINDS     �  \   J  MESSAGE_HANDLER &     K   J  COMPARE_FLOAT_NUMBERS    V  X      i@ #   �  ^      ZEEMAN_INPUT_EQUAL %     _   a   ZEEMAN_INPUT_EQUAL%X %   k  _   a   ZEEMAN_INPUT_EQUAL%Y 0   �  �      u@EQUALTO+COMPARE_FLOAT_NUMBERS :   t  �      EQUALTO_REAL_SINGLE+COMPARE_FLOAT_NUMBERS B   -  <      EQUALTO_REAL_SINGLE%ABS+COMPARE_FLOAT_NUMBERS=ABS J   i  @      EQUALTO_REAL_SINGLE%SPACING+COMPARE_FLOAT_NUMBERS=SPACING B   �  <      EQUALTO_REAL_SINGLE%MAX+COMPARE_FLOAT_NUMBERS=MAX <   �  @   e   EQUALTO_REAL_SINGLE%X+COMPARE_FLOAT_NUMBERS <   %  @   e   EQUALTO_REAL_SINGLE%Y+COMPARE_FLOAT_NUMBERS :   e  �      EQUALTO_REAL_DOUBLE+COMPARE_FLOAT_NUMBERS B     <      EQUALTO_REAL_DOUBLE%ABS+COMPARE_FLOAT_NUMBERS=ABS J   Z  @      EQUALTO_REAL_DOUBLE%SPACING+COMPARE_FLOAT_NUMBERS=SPACING B   �  <      EQUALTO_REAL_DOUBLE%MAX+COMPARE_FLOAT_NUMBERS=MAX <   �  @   e   EQUALTO_REAL_DOUBLE%X+COMPARE_FLOAT_NUMBERS <     @   e   EQUALTO_REAL_DOUBLE%Y+COMPARE_FLOAT_NUMBERS =   V  �      EQUALTO_COMPLEX_SINGLE+COMPARE_FLOAT_NUMBERS G   �  =      EQUALTO_COMPLEX_SINGLE%REAL+COMPARE_FLOAT_NUMBERS=REAL I   4	  >      EQUALTO_COMPLEX_SINGLE%AIMAG+COMPARE_FLOAT_NUMBERS=AIMAG ?   r	  @   e   EQUALTO_COMPLEX_SINGLE%X+COMPARE_FLOAT_NUMBERS ?   �	  @   e   EQUALTO_COMPLEX_SINGLE%Y+COMPARE_FLOAT_NUMBERS =   �	  �      EQUALTO_COMPLEX_DOUBLE+COMPARE_FLOAT_NUMBERS G   �
  =      EQUALTO_COMPLEX_DOUBLE%REAL+COMPARE_FLOAT_NUMBERS=REAL I   �
  >      EQUALTO_COMPLEX_DOUBLE%AIMAG+COMPARE_FLOAT_NUMBERS=AIMAG ?     @   e   EQUALTO_COMPLEX_DOUBLE%X+COMPARE_FLOAT_NUMBERS ?   N  @   e   EQUALTO_COMPLEX_DOUBLE%Y+COMPARE_FLOAT_NUMBERS    �  p       FP+TYPE_KINDS ,   �  q       INFORMATION+MESSAGE_HANDLER 0   o  �       DISPLAY_MESSAGE+MESSAGE_HANDLER :   /  =      DISPLAY_MESSAGE%TRIM+MESSAGE_HANDLER=TRIM @   l  @      DISPLAY_MESSAGE%PRESENT+MESSAGE_HANDLER=PRESENT =   �  L   e   DISPLAY_MESSAGE%ROUTINE_NAME+MESSAGE_HANDLER 8   �  L   e   DISPLAY_MESSAGE%MESSAGE+MESSAGE_HANDLER <   D  @   e   DISPLAY_MESSAGE%ERROR_STATE+MESSAGE_HANDLER <   �  L   e   DISPLAY_MESSAGE%MESSAGE_LOG+MESSAGE_HANDLER "   �  �       ZEEMAN_INPUT_TYPE %   Y  �   !   ZEEMAN_INPUT_TYPE%BE -      �   !   ZEEMAN_INPUT_TYPE%COS_THETAB +   �  �   !   ZEEMAN_INPUT_TYPE%COS_PHIB 0   N  �   !   ZEEMAN_INPUT_TYPE%DOPPLER_SHIFT &   �  �       ZEEMAN_INPUT_GETVALUE .   �  @      ZEEMAN_INPUT_GETVALUE%PRESENT 3   �  _   a   ZEEMAN_INPUT_GETVALUE%ZEEMAN_INPUT 5   V  @   a   ZEEMAN_INPUT_GETVALUE%FIELD_STRENGTH 1   �  @   a   ZEEMAN_INPUT_GETVALUE%COS_THETAB /   �  @   a   ZEEMAN_INPUT_GETVALUE%COS_PHIB 4     @   a   ZEEMAN_INPUT_GETVALUE%DOPPLER_SHIFT &   V  �       ZEEMAN_INPUT_SETVALUE .     @      ZEEMAN_INPUT_SETVALUE%PRESENT 3   X  _   a   ZEEMAN_INPUT_SETVALUE%ZEEMAN_INPUT 5   �  @   a   ZEEMAN_INPUT_SETVALUE%FIELD_STRENGTH 1   �  @   a   ZEEMAN_INPUT_SETVALUE%COS_THETAB /   7  @   a   ZEEMAN_INPUT_SETVALUE%COS_PHIB 4   w  @   a   ZEEMAN_INPUT_SETVALUE%DOPPLER_SHIFT %   �  �       ZEEMAN_INPUT_ISVALID )   K  <      ZEEMAN_INPUT_ISVALID%ABS *   �  =      ZEEMAN_INPUT_ISVALID%TRIM '   �  _   a   ZEEMAN_INPUT_ISVALID%Z %   #  O       ZEEMAN_INPUT_INSPECT '   r  _   a   ZEEMAN_INPUT_INSPECT%Z +   �  P       ZEEMAN_INPUT_DEFINEVERSION .   !  L   a   ZEEMAN_INPUT_DEFINEVERSION%ID 