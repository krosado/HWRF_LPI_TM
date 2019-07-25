C
      DATA NORTH/'N'/,SOUTH/'S'/,EAST/'E'/,WEST/'W'/,SHALO/'S'/,
     1     MEDIUM/'M'/,DEEP/'D'/,RELO/'R'/,EXE/'X'/,DASH/'-'/,STAR/'*'/,
     2     COLON/':'/,IPRIVR/1/,VITFAC/2*1.0,2*0.1,1.0,0.1,14*1.0,
     3     2*0.1/,
     3     FMTVIT/'(I6.6)','(I4.4)','(I3.3)','(I4.4)',2*'(I3.3)',
     4            3*'(I4.4)','(I2.2)','(I3.3)',8*'(I4.4)','(I2.2)',
     5            '(I3.3)','(I4.4)'/,
     6     FMTMIS/'(I6)','(I4)','(I3)','(I4)',2*'(I3)',3*'(I4)',
     7            '(I2)','(I3)',8*'(I4)','(I2)','(I3)','(I4)'/,
     8     SWITSB/'Z'/,BLNK18/'                  '/,BLNK2/'  '/,
     9     LTLNSB/'L'/,CLIMO/'C'/,RSMAVG/'A'/,PERSIS/'P'/,OVRLAP/'O'/,
     O     DRSPSB/'S'/,BANG/'!'/,
     9     MISSNG/-99999,-999,-99,-999,2*-99,3*-999,-9,-99,8*-999,-9,
     O     2*-999/,
     O     ISTVAR/20,27,32,37,43,47,51,56,61,66,69,73,78,83,88,95,100,
     O     105,110,115,118,123/,
     1     IENVAR/25,30,34,40,45,49,54,59,64,67,71,76,81,86,91,98,103,
     1     108,113,116,120,126/,
     2     LENHED/18/,IFSTFL/19/,STMTOP/-99.0,700.,400.,200./,
     3     ISTRSM/1/,IENRSM/4/,ISTID/6/,IENID/8/
C
      DATA IDBASN/'L','E','C','W','O','T','U','P','S','B','A'/
C
      DATA NABASN/'ATLANTIC        ','EAST PACIFIC    ',
     1            'CENTRAL PACIFIC ','WEST PACIFIC    ',
     2            'SOUTH CHINA SEA ','EAST CHINA SEA  ',
     3            'AUSTRALIA       ','SOUTH PACIFIC   ',
     4            'SOUTH INDIAN OCN','BAY OF BENGAL   ',
     5            'NRTH ARABIAN SEA'/
C
C     BASIN BOUNDARIES: MIN AND MAX LATITUDES; MIN AND MAX LONGITUDES
C       NOTE: SOME BOUNDARIES MAY OVERLAP, BUT SCANNING IS IN ORDER OF
C             DECREASING PRIORITY SO BASINS SHOULD BE CAPTURED PROPERLY
C
      DATA BSLTMN/3*-20.,2*0.0,20.,3*-50.,2*0.0/,
     1     BSLTMX/4*60.,25.,40.,3*0.0,2*30./,
     2     BSLNMN/260.,220.,180.,2*100.,110.,90.,160.,40.,75.,40./,
     3     BSLNMX/350.,260.,220.,180.,125.,140.,160.,290.,90.,100.,75./
 
      DATA RSMCID/'NHC ','JTWC','ADRM','JMA '/,
     1     RSMCAP/'N','W','A','J'/,
     2     RSMCPR/3*1,3*2,3,4*2/,RSMCWT/1.0,0.25/
C
      DATA NAMVAR/'DATE ','TIME ','LAT. ','LONG.','DIR  ','SPEED',
     1            'PCEN ','PENV ','RMAX ','VMAX ','RMW  ','R15NE',
     2            'R15SE','R15SW','R15NW','DEPTH','R26NE','R26SE',
     3            'R26SW','R26NW','FCSTP','FCLAT','FCLON'/
C                                                                       58240063
C     CARDINAL NUMBER STORM NAMES FOR UNNAMED ATLANTIC AND EAST PACIFIC 58250063
C       STORMS                                                          58260063
C                                                                       58270063
      DATA CARDNM/'ONE      ','TWO      ','THREE    ',                  58280063
     1            'FOUR     ','FIVE     ','SIX      ',                  58290063
     2            'SEVEN    ','EIGHT    ','NINE     ',                  58300063
     3            'TEN      ','ELEVEN   ','TWELVE   ',                  58310063
     4            'THIRTEEN ','FOURTEEN ','FIFTEEN  ',                  58320063
     5            'SIXTEEN  ','SEVENTEEN','EIGHTEEN ',                  58330063
     6            'NINETEEN ','TWENTY   ','TWENTY-ON',                  58340063
     7            'TWENTY-TW','TWENTY-TH','TWENTY-FO',                  58350063
     8            'TWENTY-FI','TWENTY-SI','TWENTY-SE',                  58360063
     9            'TWENTY-EI','TWENTY-NI','THIRTY   ',                  58370063
     O            'THIRTY-ON','THIRTY-TW','THIRTY-TH',                  58370063
     1            'THIRTY-FO','THIRTY-FI','THIRTY-SI'/                  58370063
C                                                                       58240063
C     GREEK STORM NAMES FOR ATLANTIC AND EAST PACIFIC                   58250063
C       STORMS                                                          58260063
C                                                                       58270063
      DATA GREKNM/'ALPHA    ','BETA     ','GAMMA    ',                  58280063
     1            'DELTA    ','EPSILON  ','ZETA     ',                  58280063
     2            'ETA      ','THETA    ','IOTA     ',                  58280063
     3            'KAPPA    ','LAMBDA   ','MU       ',                  58280063
     4            'NU       ','XI       ','OMICRON  ',                  58280063
     5            'PI       ','RHO      ','SIGMA    ',                  58280063
     6            'TAU      ','UPSILON  ','PHI      ',                  58280063
     7            'CHI      ','PSI      ','OMEGA    '/                  58280063
C                                                                       58380063
      DATA ICRDCH/3,3,5,4,4,3,5,5,4,3,6,6,8,8,7,7,9,8,8,10*9,7*9/       58390063
      DATA IGRKCH/5,4,5,5,7,4,3,5,4,5,6,2,2,2,7,2,3,5,3,7,3,3,3,5/      58390063
C                                                                       58380063
