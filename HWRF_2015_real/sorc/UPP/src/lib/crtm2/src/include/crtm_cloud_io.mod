	  �  P   k820309    4          12.1        �S^Y                                                                                                           
       CRTM_Cloud_IO.f90 CRTM_CLOUD_IO              CRTM_CLOUD_INQUIREFILE CRTM_CLOUD_READFILE CRTM_CLOUD_WRITEFILE CRTM_CLOUD_IOVERSION                      @                              
       FILE_OPEN FILE_EXISTS                      @                              
       SUCCESS FAILURE WARNING INFORMATION DISPLAY_MESSAGE                      @                              
       OPEN_BINARY_FILE                      @                              
       SET                      @                              
       CRTM_CLOUD_TYPE CRTM_CLOUD_ASSOCIATED CRTM_CLOUD_DESTROY CRTM_CLOUD_CREATE                �                                       u #FILE_UNIT_EXISTS    #FILE_NAME_EXISTS    %         @    @                                                      #FILEID              
   @                                         %         @    @                                                     #FILENAME 	             
   @                             	                    1                �                                       u #FILE_OPEN_BY_UNIT 
   #FILE_OPEN_BY_NAME    %         @    @                           
                           #FILEID              
   @                                         %         @    @                                                     #FILENAME              
   @                                                 1                   !@               A                '�                    #IS_ALLOCATED    #MAX_LAYERS    #N_LAYERS    #N_ADDED_LAYERS    #TYPE    #EFFECTIVE_RADIUS    #EFFECTIVE_VARIANCE    #WATER_CONTENT                �                                                                         �1                                                                       �                                                                        �1                                                    0                �                                                                        �1                                                    0                �                                                                        �1                                                    0                �                                                                        �1                                                     0               �                                                           
            &                                                      �                                          `                 
            &                                                      �                                          �                 
            &                                                                                                                                               0                                                                                                   3                                                                                                   2                                                                                                   1#         @ �                                                 #DISPLAY_MESSAGE%TRIM    #DISPLAY_MESSAGE%PRESENT    #ROUTINE_NAME    #MESSAGE    #ERROR_STATE     #MESSAGE_LOG !                 @                                 TRIM               @                                 PRESENT           
   @                                                 1           
   @                                                 1           
   @                                                    
  @                             !                    1 %         @                                "                          #OPEN_BINARY_FILE%PRESENT #   #OPEN_BINARY_FILE%TRIM $   #FILENAME %   #FILEID &   #FOR_OUTPUT '   #NO_CHECK (   #MESSAGE_LOG )                 @                            #     PRESENT               @                            $     TRIM           
   @                             %                    1             @                              &                      
  @                              '                     
  @                              (                     
  @                             )                    1                                              *                                                      1%         H                               +                           #CLOUD ,             
   @                              ,     �              #CRTM_CLOUD_TYPE    #         H        
                         -                   #CLOUD .               @                              .     �               #CRTM_CLOUD_TYPE    #         H        
                         /                   #CLOUD 0   #N_LAYERS 1               @                              0     �               #CRTM_CLOUD_TYPE              
   @                              1           %         @                                 2                          #CRTM_CLOUD_INQUIREFILE%PRESENT 3   #CRTM_CLOUD_INQUIREFILE%TRIM 4   #FILENAME 5   #N_CLOUDS 6                 @                            3     PRESENT               @                            4     TRIM           
  @@                             5                    1           F @@                              6            %         @                                 7                          #CRTM_CLOUD_READFILE%SIZE 8   #CRTM_CLOUD_READFILE%PRESENT 9   #CRTM_CLOUD_READFILE%TRIM :   #FILENAME ;   #CLOUD <   #QUIET =   #NO_CLOSE >   #N_CLOUDS ?   #DEBUG @                 @                            8     SIZE               @                            9     PRESENT               @                            :     TRIM           
@ @@                             ;                    1           D@@                              <            �                       &                                           #CRTM_CLOUD_TYPE              
 @@                              =                     
 @@                              >                     F @@                              ?                      
 @@                              @           %         @                                 A                          #CRTM_CLOUD_WRITEFILE%ANY B   #CRTM_CLOUD_WRITEFILE%SIZE C   #CRTM_CLOUD_WRITEFILE%PRESENT D   #CRTM_CLOUD_WRITEFILE%TRIM E   #FILENAME F   #CLOUD G   #QUIET H   #NO_CLOSE I   #DEBUG J                 @                            B     ANY               @                            C     SIZE               @                            D     PRESENT               @                            E     TRIM           
@ @@                             F                    1           
 @@                              G            �                      &                                           #CRTM_CLOUD_TYPE              
 @@                              H                     
 @@                              I                     
 @@                              J           #         @                                  K                   #ID L             D  @                             L                     1    �   (      fn#fn #   �   e   b   uapp(CRTM_CLOUD_IO    -  V   J  FILE_UTILITY     �  t   J  MESSAGE_HANDLER $   �  Q   J  BINARY_FILE_UTILITY     H  D   J  CRTM_PARAMETERS "   �  �   J  CRTM_CLOUD_DEFINE -     l       gen@FILE_EXISTS+FILE_UTILITY .   �  \      FILE_UNIT_EXISTS+FILE_UTILITY 5   �  @   e   FILE_UNIT_EXISTS%FILEID+FILE_UTILITY .     ^      FILE_NAME_EXISTS+FILE_UTILITY 7   }  L   e   FILE_NAME_EXISTS%FILENAME+FILE_UTILITY +   �  n       gen@FILE_OPEN+FILE_UTILITY /   7  \      FILE_OPEN_BY_UNIT+FILE_UTILITY 6   �  @   e   FILE_OPEN_BY_UNIT%FILEID+FILE_UTILITY /   �  ^      FILE_OPEN_BY_NAME+FILE_UTILITY 8   1  L   e   FILE_OPEN_BY_NAME%FILENAME+FILE_UTILITY 2   }  �       CRTM_CLOUD_TYPE+CRTM_CLOUD_DEFINE ?   \  �   a   CRTM_CLOUD_TYPE%IS_ALLOCATED+CRTM_CLOUD_DEFINE =      �   a   CRTM_CLOUD_TYPE%MAX_LAYERS+CRTM_CLOUD_DEFINE ;   �  �   a   CRTM_CLOUD_TYPE%N_LAYERS+CRTM_CLOUD_DEFINE A   J	  �   a   CRTM_CLOUD_TYPE%N_ADDED_LAYERS+CRTM_CLOUD_DEFINE 7   �	  �   a   CRTM_CLOUD_TYPE%TYPE+CRTM_CLOUD_DEFINE C   �
  �   a   CRTM_CLOUD_TYPE%EFFECTIVE_RADIUS+CRTM_CLOUD_DEFINE E   (  �   a   CRTM_CLOUD_TYPE%EFFECTIVE_VARIANCE+CRTM_CLOUD_DEFINE @   �  �   a   CRTM_CLOUD_TYPE%WATER_CONTENT+CRTM_CLOUD_DEFINE (   P  q       SUCCESS+MESSAGE_HANDLER (   �  q       FAILURE+MESSAGE_HANDLER (   2  q       WARNING+MESSAGE_HANDLER ,   �  q       INFORMATION+MESSAGE_HANDLER 0     �       DISPLAY_MESSAGE+MESSAGE_HANDLER :   �  =      DISPLAY_MESSAGE%TRIM+MESSAGE_HANDLER=TRIM @     @      DISPLAY_MESSAGE%PRESENT+MESSAGE_HANDLER=PRESENT =   Q  L   e   DISPLAY_MESSAGE%ROUTINE_NAME+MESSAGE_HANDLER 8   �  L   e   DISPLAY_MESSAGE%MESSAGE+MESSAGE_HANDLER <   �  @   e   DISPLAY_MESSAGE%ERROR_STATE+MESSAGE_HANDLER <   )  L   e   DISPLAY_MESSAGE%MESSAGE_LOG+MESSAGE_HANDLER 5   u  �       OPEN_BINARY_FILE+BINARY_FILE_UTILITY E   G  @      OPEN_BINARY_FILE%PRESENT+BINARY_FILE_UTILITY=PRESENT ?   �  =      OPEN_BINARY_FILE%TRIM+BINARY_FILE_UTILITY=TRIM >   �  L   e   OPEN_BINARY_FILE%FILENAME+BINARY_FILE_UTILITY <     @   e   OPEN_BINARY_FILE%FILEID+BINARY_FILE_UTILITY @   P  @   e   OPEN_BINARY_FILE%FOR_OUTPUT+BINARY_FILE_UTILITY >   �  @   e   OPEN_BINARY_FILE%NO_CHECK+BINARY_FILE_UTILITY A   �  L   e   OPEN_BINARY_FILE%MESSAGE_LOG+BINARY_FILE_UTILITY $     q       SET+CRTM_PARAMETERS 8   �  [       CRTM_CLOUD_ASSOCIATED+CRTM_CLOUD_DEFINE >   �  ]   e   CRTM_CLOUD_ASSOCIATED%CLOUD+CRTM_CLOUD_DEFINE 5   E  S       CRTM_CLOUD_DESTROY+CRTM_CLOUD_DEFINE ;   �  ]   e   CRTM_CLOUD_DESTROY%CLOUD+CRTM_CLOUD_DEFINE 4   �  a       CRTM_CLOUD_CREATE+CRTM_CLOUD_DEFINE :   V  ]   e   CRTM_CLOUD_CREATE%CLOUD+CRTM_CLOUD_DEFINE =   �  @   e   CRTM_CLOUD_CREATE%N_LAYERS+CRTM_CLOUD_DEFINE '   �  �       CRTM_CLOUD_INQUIREFILE /   �  @      CRTM_CLOUD_INQUIREFILE%PRESENT ,   �  =      CRTM_CLOUD_INQUIREFILE%TRIM 0   !  L   a   CRTM_CLOUD_INQUIREFILE%FILENAME 0   m  @   a   CRTM_CLOUD_INQUIREFILE%N_CLOUDS $   �  �       CRTM_CLOUD_READFILE )   �  =      CRTM_CLOUD_READFILE%SIZE ,   �  @      CRTM_CLOUD_READFILE%PRESENT )   "  =      CRTM_CLOUD_READFILE%TRIM -   _  L   a   CRTM_CLOUD_READFILE%FILENAME *   �  �   a   CRTM_CLOUD_READFILE%CLOUD *   L  @   a   CRTM_CLOUD_READFILE%QUIET -   �  @   a   CRTM_CLOUD_READFILE%NO_CLOSE -   �  @   a   CRTM_CLOUD_READFILE%N_CLOUDS *     @   a   CRTM_CLOUD_READFILE%DEBUG %   L        CRTM_CLOUD_WRITEFILE )   W  <      CRTM_CLOUD_WRITEFILE%ANY *   �  =      CRTM_CLOUD_WRITEFILE%SIZE -   �  @      CRTM_CLOUD_WRITEFILE%PRESENT *     =      CRTM_CLOUD_WRITEFILE%TRIM .   M  L   a   CRTM_CLOUD_WRITEFILE%FILENAME +   �  �   a   CRTM_CLOUD_WRITEFILE%CLOUD +   :  @   a   CRTM_CLOUD_WRITEFILE%QUIET .   z  @   a   CRTM_CLOUD_WRITEFILE%NO_CLOSE +   �  @   a   CRTM_CLOUD_WRITEFILE%DEBUG %   �  P       CRTM_CLOUD_IOVERSION (   J  L   a   CRTM_CLOUD_IOVERSION%ID 