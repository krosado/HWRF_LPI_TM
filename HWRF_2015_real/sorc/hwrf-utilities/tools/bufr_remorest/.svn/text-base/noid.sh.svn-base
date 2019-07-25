set -eau

in=$1   out=$2  DATA=`pwd`

DATA=`pwd`
USHREM=$(dirname $0) 
REMX=$USHREM/bufr_remorest
REMC=bufr_remorest.parm


cat <<\EOFparm > bufr_remorest.parm
=========================================================================

  Cards for PREPBUFR Version of BUFR_REMOREST -- Version 21 November 2007

 &SWITCHES
   MSG_RESTR = '        ',   ! These are the Table A Entries for
               '        ',   !  BUFR messages for which ALL reports
               '        ',   !  are RESTRICTED and will be REMOVED.
               '        ',
               '        ',
               '        ',
               '        ',
               '        ',
               '        ',
               '        '
   MSG_MIXED = 'MSONET  ',   ! These are the Table A Entries for
               'ADPSFC  ',   !  BUFR messages which contain a MIXTURE
               '        ',   !  of restricted and unrestricted
               '        ',   !  reports (based on mnemonic "RSRD").  All
               '        ',   !  restricted reports will be REMOVED.
               '        ',
               '        ',
               '        ',
               '        ',
               '        '
   MSG_MASKA = 'SFCSHP  ',   ! These are the Table A Entries for
               '        ',   !  BUFR messages for which ALL reports
               '        ',   !  are RESTRICTED if their dump report type is
               '        ',   !  one of up to 10 possible listed in switch
               '        ',   !  IMASK_T29 below (each line in IMASK_T29 applies
               '        ',   !  to the Table A entry in the same line number
               '        ',   !  here). Restricted reports will not be removed,
               '        ',   !  but their report ids will be unilaterally
               '        ',   !  changed to "MASKSTID"
               '        '
   IMASK_T29 = 10*99999,        ! Dump report types restricted in MSG_MASKA(1)
               10*99999,        ! Dump report types restricted in MSG_MASKA(2)
               10*99999,        ! Dump report types restricted in MSG_MASKA(3)
               10*99999,        ! Dump report types restricted in MSG_MASKA(4)
               10*99999,        ! Dump report types restricted in MSG_MASKA(5)
               10*99999,        ! Dump report types restricted in MSG_MASKA(6)
               10*99999,        ! Dump report types restricted in MSG_MASKA(7)
               10*99999,        ! Dump report types restricted in MSG_MASKA(8)
               10*99999,        ! Dump report types restricted in MSG_MASKA(9)
               10*99999         ! Dump report types restricted in MSG_MASKA(10)

 /

    Note 1: A particular Table A entry should NEVER appear in more than one
            of MSG_RESTR, MSG_MIXED or MSG_MASKA.
    Note 2: Any Table A entry not in either MSG_RESTR, MSG_MIXED or MSG_MASKA
            is assumed to be a Table A entry for BUFR messages for which
            ALL reports are UNRESTRICTED (these messages are copied
            intact, no reports are unpacked).
    Note 3: Always fill in the arrays MSG_RESTR, MSG_MIXED and MSG_MASKA
            beginning with word 1.  If there are less than 10 words filled in
            an array, set the extra words to "        " (8 blank characters).
    Note 4: In array IMASK_T29, a value of "99999" means not applicable whereas
            a value of "000" means reports in all dump report types in the
            corresponding Table A entry in MSG_MASKA should be restricted
            (masked) {in this case IMASK_T29(1,x) would be set to 000 and
            IMASK_T29(2:10,x) would be set to 99999 for all reports in Table A
            entry MSG_MASKA(x) since they would all be ignored - this is the
            default for all Table A entries MSG_MASKA(1:10) if this is not set
            (i.e., for data dump files)}

=========================================================================
EOFparm

cp -p $in $out                               

$USHREM/bufr_remorest.sh $out >/dev/null 2>/dev/null 

rm -f bufr_remorest.parm

