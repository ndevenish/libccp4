C          CCP4 PARSER Routines       ( 19-Feb-1990)
C          ====================
C
C  Original Author: Based on Mike Levitt's routine of the same name.
C  Modified By: Peter Brick, Phil Evans, Eleanor Dodson
C
C
C
C
C
C  Routine PARSER.FOR  contains the following subroutines and functions
C
C
C
C
C  SUBROUTINES 
C
C    PARSER(KEY,LINE,IBEG,IEND,ITYP,FVALUE,CVALUE,IDEC,NTOK,LEND,PRINT)
C    PARSE(LINE,IBEG,IEND,ITYP,FVALUE,CVALUE,IDEC,N)
C    KEYNUM(N,NSTART,LINE,IBEG,IEND,ITYP,NTOK)
C    KEYERR(I,MODE,LINE,IBEG,IEND,ITYP)
C    CHKNUM(ISYSW,N1,N2,NTOK,ITYP,IBEG,IEND,LINE)
C    CHKTOK(ISYSW,I,IWANT,NTOK,ITYP,IBEG,IEND,LINE)
C    GETREA(N,X,NTOK,ITYP,FVALUE)
C    GETINT(N,I,NTOK,ITYP,FVALUE)
C    GTNREA(N,M,X,NTOK,ITYP,FVALUE)
C    GTNINT(N,M,J,NTOK,ITYP,FVALUE)
C    GTPREA(N,X,NTOK,ITYP,FVALUE)
C    GTPINT(N,I,NTOK,ITYP,FVALUE)
C    GETSTR(N,STRING,NTOK,ITYP,IBEG,IEND,LINE)
C    SBLANK(ARRAY,N1,N2)
C    GTCFLD(NFIELD,ITEXT,NCHAR,MINC,MAXC,IGFLAG)
C    CPYCHR(STRNGA,STRNGB,NCHAR)
C    CMOVE(STRNGA,STRNGB,NCHAR)
C    CHKKEY(KEY,WORDS,NWORDS,IKEY)
C
C FUNCTIONS
C
C    REAL FUNCTION DOCALC(VALUE,OPER,VALUE0)
C    LOGICAL FUNCTION CMATCH(STRING1,STRING2,NCHAR)
C
C
C
C     =================================================================
      SUBROUTINE PARSER(KEY,LINE,IBEG,IEND,ITYP,FVALUE,CVALUE,IDEC,NTOK,
     +                  LEND,PRINT)
C     =================================================================
C
C
C
C  7-12-89  EJD: Two corrections
C      1) There was no check if there were more than the permitted
C         number of items(nitem) on each line ( currently nitem = 20)
C         The first call to PARSER now should transfer nitem in the
C         NTOK.  If NTOK .lt.20 I will set NITEM = 20
C
C      2) Line needed a ' ' as the last character.
C         Now you are warned if either of these faults has occurred.
C                  
C 28-11-89  Parser from Sandra - more or less the same as PARSER77NOW
C
C---- Read control card from stream (default = 5), and interpret
C     Stream 5 is the standard input stream (= terminal or batch 
C     stream), but a line beginning with @name starts reading 
C     reading from a file of name 'name' (on stream 11), 
C     until end-of-file
C
C
C
C
C On entry
C ========
C
C   LINE       if blank, read line
C              if not blank, parse this
C   NTOK       if .gt. 20, set the maximum number of fields = NTOK,
C              else set = 20
C   PRINT      = .true. reflect line
C              = .false. don't
C
C Returns
C =======
C
C   KEY  keyword at beginning of line (CHARACTER*4) (if present)
C          Uppercased before returning
C
C  NTOK fields.
C       NTOK returned = -1 for line beginning '$' DCL command
C
C For I=1,NTOK :
C   IBEG(I)      1st column number in field (1 - 80)
C   IEND(I)      last column number in field
C   ITYP(I)      =0  null field
C                =1  character string
C                =2  number
C   FVALUE(I)    value of number . Items in FVALUE and CVALUE are left
C                 unchanged for null fields
C   CVALUE(I)    character string (1st 4 characters),
C                 for numbers as well as strings
C   IDEC(I)      number of 'digits'
C                 for string, number number of characters (=4 if.gt.4)
C                 for integer, number of digits-1
C                 for real number, (number of digits before point+1)*100
C                                  +number of digits after point
C
C
C Returns LEND = .FALSE. for control card
C              = .TRUE.  for end-of-file
C
C
C---- Each logical 'card' may be continued on next line by
C     the continuation character '&' or '-'  at the end of
C     the line: this character is dropped from the list
C     returned to the calling routine.
C
C     Comment may be present on the line, following the
C     character '#': any continuation character ('&' or '-')
C     must  PRECEED the comment character '#'.
C
C---- Lines containing ONLY comment will not be returned from
C     this routine E3.1
C
C---- For strings, the routine will accept quoted or
C     unquoted strings
C
C
C     .. Parameter ..
      INTEGER NPARM
      PARAMETER (NPARM=200)
C
C     .. Scalar Arguments ..
      INTEGER NTOK
      LOGICAL LEND,PRINT
      CHARACTER KEY*4,LINE*(*)
C     ..
C     .. Array Arguments ..
      REAL FVALUE(*)
      INTEGER IBEG(*),IDEC(*),IEND(*),ITYP(*)
      CHARACTER CVALUE(*)*4
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,J,K,KSTRM,LENLIN,LINLEN,LSTRM,MSTRM,N,
     +        NITEM,ISTERR,IFGERR
      LOGICAL FIRST
      CHARACTER IAMP*1,IDASH*1,FLNAME*60,LINERR*800,LINEX*800
      CHARACTER ICOMM1*1,ICOMM2*1
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPDPN,CCPUPC,PARSE,LERROR,PUTLIN
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC LEN,MAX
C     ..
C     .. Scalars in Common ..
      CHARACTER STROUT*800
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
      DATA LSTRM/5/,KSTRM/11/,MSTRM/5/
      DATA IAMP/'&'/,IDASH/'-'/,NITEM/0/
C Comment characters
      DATA ICOMM1,ICOMM2/'#','!'/
C     ..
C
      LINEX = ' '
      NINCHR = 0
      ICX = 0
C
   10 FIRST = .TRUE.
C
C---- Find length of line
C
      LINLEN = LEN(LINE)
C
C---- Find out dimensions of ibeg etc                        
C
      J = 1
      LEND = .FALSE.
C
C---- Initialisations
C     - on first call check if NTOK has been set = NITEM.
C       Default NITEM = 20 - you would only want to increase it!
C
      IF (NITEM.EQ.0) NITEM = MAX(NTOK,20)
      NTOK = 0
C
C---- Use Negative N to pass NITEM To PARSE for first call.
C
      N = -NITEM
C
C---- Skip read if LINE already has something in it
C                                            
      IF (LINE.NE.'    ') GO TO 30
C
   20 CONTINUE
      READ (LSTRM,FMT=6000,END=40) LINEX
 6000 FORMAT (A)
C
C
      IF(LINEX(1:1).EQ.ICOMM1 .OR. LINEX(1:1).EQ.ICOMM2) THEN
        STROUT = ' '
        WRITE(STROUT,FMT=6666) LINEX(2:LENSTR(LINEX))
 6666   FORMAT(' Comment line--- ',A)
        GO TO 20
      END IF
C
C
      LX = LENSTR(LINEX)
      IF (LX.EQ.0) GO TO 20
C
C---- Count total number of characters on line
C    
      NINCHR = NINCHR + LX + 1
      IF (NINCHR .GT. LINLEN) THEN
C----- Line overflow
        WRITE (6, FMT='(A,I5,A/(A))') ' *** WARNING - More than ',
     .       LINLEN,' characters in (continued) line ***',
     .       ' *** Parsing truncated line ***',LINEX(1:LX)
        WRITE (LINERR, FMT='(A,I5,A/(A))') ' *** WARNING - More than ',
     .       LINLEN,' characters in (continued) line ***',
     .       ' *** Parsing truncated line ***',LINEX(1:LX)
        ISTERR = 1
        IFGERR = 0
        CALL LERROR(ISTERR,IFGERR,LINERR)
C---- Reset LX to truncate line
        LX = LX - NINCHR + LINLEN
      ENDIF
C
C---- Continuation line
C
      IF (LINEX(LX:LX).EQ.'-' .OR. LINEX(LX:LX).EQ.'&') THEN
C      
        LINEX(LX:LX) = ' '
        LL = LENSTR(LINE) + 1
C
        IF (FIRST) THEN
          LINE = LINEX   // ' '
          FIRST = .FALSE.
        ELSE
          LINE(LL:) = ' ' // LINEX(1:LX) // ' '
        END IF
        GO TO 20
C
C
      END IF
C
C---- Not a continuation line
C
      IF (FIRST) THEN
         LINE = LINEX   // ' '
         FIRST = .FALSE.
         GO TO 30
      ELSE
        LX = LENSTR(LINEX)
        LL = LENSTR(LINE) + 1
        LINE(:LL) = ' ' // LINEX(1:LX) // ' '
      END IF
C
C
C
   30 LENLIN = LENSTR(LINE)
      IF (LENLIN.EQ.0) GO TO 20
C
C---- Interpret
C     (note that PARSE likes a terminating space in its input string
C      I suppose this is a bug)
C
C  2-12-89
C  EJD : attempt to fix it; OK unless LENLIN gt.LINLEN (80 characters)
C
      IF (LENLIN.LT.LINLEN) THEN
        LINE(LENLIN+1:LENLIN+1) = ' '
        LENLIN = LENLIN + 1
      ELSE
C
C
        IF (LINE(LENLIN:LENLIN).NE.' ') THEN
          WRITE (6,FMT='(A)') 
     +  '  *** WARNING - PARSER likes a terminating space ****'
          WRITE (LINERR,FMT='(A)') 
     +  '  *** WARNING - PARSER likes a terminating space ****'
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
          WRITE (LINERR,FMT='(A,/,4x,A)') 
     +      '  ***  This line may not be parsed correctly ***',
     +      LINE(1:120)
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
         ENDIF
C
C
      END IF
C
C
      IF (PRINT .OR. LSTRM.NE.MSTRM) THEN
        STROUT = ' '
        WRITE (STROUT,FMT=6002) LINE(1:LENSTR(LINE))
 6002   FORMAT (' Data line--- ',A)
        CALL PUTLIN(STROUT,'HLPWIN')
      END IF 
C
C
C          ********************************************************
      CALL PARSE(LINE,IBEG,IEND,ITYP,FVALUE,CVALUE,IDEC,N)
C          ********************************************************
C
C---- Count fields
C
      IF (N.GT.0) NTOK = NTOK + N
C
C---- Copy keyword string to KEY if present
C
      IF (NTOK.GT.0 .AND. ITYP(1).EQ.1 ) THEN
        KEY = CVALUE(1) (1:4)
        CALL CCPUPC(KEY)
      END IF
C
C---- Test if first field begins '@'
C
      IF (NTOK.GT.0 .AND. ITYP(1).EQ.1 .AND.
     +    CVALUE(1) (1:1).EQ.'@') THEN
C
C---- Get filename if present (just '@' resets stream to MSTRM)
C
        IF (IDEC(1).EQ.1) THEN
          LSTRM = MSTRM
        ELSE
          FLNAME = LINE(IBEG(1)+1:IEND(1))
C
C---- Open file
C
          LSTRM = KSTRM
          IFAIL = 1
C
C              *********************************************
          CALL CCPDPN(LSTRM,FLNAME,'READONLY','F',0,IFAIL)
C              *********************************************
C
          IF (IFAIL.GE.0) THEN
C
C---- and start to read it
C
            LINE = ' '
            GO TO 10
          ELSE
C
C---- Failed to open file
C
            LSTRM = MSTRM
          WRITE (LINERR,FMT='(A,A,A)') 
     +   ' File ',
     +    FLNAME(1:LENSTR(FLNAME)),
     +  ' does not exist'
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
            NTOK = 0
          END IF
        END IF
C
        RETURN
      END IF
C
C---- PARSE returns N = 0 for lines containing only comment,
C           Ignore these
C
      IF (N.LE.0) THEN
        LINE = ' '
        GO TO 20
      END IF
C
C
      K = IEND(NTOK)
      IF (LINE(K:K).EQ.IAMP .OR. LINE(K:K).EQ.IDASH) THEN
C
C---- Continuation
C
        FIRST = .FALSE.
        J = NTOK
C
C---- Overwrite continuation character
C
        NTOK = NTOK - 1
C
C---- Read next line
C
        GO TO 20
      ELSE
        RETURN
      END IF
C
C---- End of file found, return to main input stream if not
C
   40 IF (LSTRM.NE.MSTRM) THEN
        CLOSE (UNIT=LSTRM)
        LSTRM = MSTRM
      ELSE
        LEND = .TRUE.
      END IF
C
      RETURN
C
C
      END
C            
C
C     ==========================================================
      SUBROUTINE PARSE(LINE,IBEG,IEND,ITYP,FVALUE,CVALUE,IDEC,N)
C     ==========================================================
C
C---- Free format read routine
C
C---- Enters with characters in LINE (CHARACTER)
C     ======
C
C     N    if .lt. 0, first entry,
C          NITEM = -N = maximum number
C          of fields to interpret on subsequent
C          entries for continuation lines, = number so far
C
C---- Returns N fields, N = 0 if blank or comment
C     =======
C
C For I=1,N :
C   IBEG(I)      1st column number in field
C   IEND(I)      last column number in field
C   ITYP(I)      =0  null field
C                =1  character string
C                =2  number
C   FVALUE(I)    value of number.
C                Items in FVALUE and CVALUE are left
C                unchanged for null fields
C   CVALUE(I)    character string (1st 4 characters),
C                for numbers as well as strings
C   IDEC(I)      number of 'digits'
C                 for string, number number of characters (=4 if.gt.4)
C                 for integer, number of digits-1
C                 for real number, (number of digits before point+1)*100
C                                  +number of digits after point
C
C---- For strings, the routine will accept
C     quoted or unquoted strings
C
C   30/4/91 Changed to restrict number of delimiters
C                Phil Evans
C
C     .. Parameter ..
      INTEGER NPARM
      PARAMETER (NPARM=200)
C
C     .. Scalar Arguments ..
      INTEGER N, NNEWDL, NSPECD
      CHARACTER LINE* (*)
C     ..
C     .. Array Arguments ..
      REAL FVALUE(*)
      INTEGER IBEG(*),IDEC(*),IEND(*),ITYP(*)
      CHARACTER CVALUE(*)*4, NEWDLM*(*)
C     ..
C     .. Local Scalars ..
      REAL F10,SIGN,SIGN0,VALUE,VALUE0
      INTEGER I,IDOT,J,L,LENG,LENLIM,LINLEN,NCHK,NDELM,NDIGS,NDONE,
     +        NITEM,NPLACE,NSPDLM,OPER,NDDELM,NDSDLM
      LOGICAL NULL,NUMBER,OPRATR,QUOTE,TOKEN,TQUOTE
      CHARACTER BLANK*1,LETQT*1,OLDQUT*1,DBLQT*1,TAB*1
      CHARACTER LINERR*800,ICOMM1*1,ICOMM2*1
C     ..
C     .. Local Arrays ..
      INTEGER ISGN(2)
      INTEGER MAXDLM
      PARAMETER (MAXDLM=20)
      CHARACTER DELIM(MAXDLM)*1,DDELIM(MAXDLM)*1,DIGS(18)*1
C     ..
C     .. External Functions ..
      REAL DOCALC
      EXTERNAL DOCALC
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC LEN
      SAVE DELIM,NDELM,NSPDLM,DDELIM,NDDELM,NDSDLM
C     ..
C     .. Data statements ..
C
      DATA LETQT,DBLQT/'''','"'/,BLANK/' '/,ICOMM1,ICOMM2/'#','!'/
      DATA DIGS/'0','1','2','3','4','5','6','7','8','9','+','-','*','/',
     +     'E','.','e',' '/
      DATA ISGN/1,-1/
      DATA NDIGS/17/
C Delimiters
C---- DELIM  array of NDELM delimiters
C---- DDELIM  default array of NDDELM delimiters
C---- NSPDLM (NDSDLM default) is number of special delimiters which
C            cannot delimit a null field these are
C            at the beginning of the delimiter array
C
C-- Note that delimiters may be changed by a call to PARSDL (entry point)
C
      DATA DDELIM/' ', ' ', '=',',',16*' '/
      DATA NDDELM/4/,NDSDLM/3/,NDELM/-1/
C     ..
C Setup delimiters if not done
      IF (NDELM .LT. 0) THEN
        NDELM=NDDELM
        NSPDLM=NDSDLM
        DO 1, I = 1, MAXDLM
          DELIM(I)=DDELIM(I)
 1      CONTINUE
C  Set tab
        TAB = CHAR(9)
        DELIM(2) = TAB
      ENDIF
C
C
C---- First call  - N = - NITEM ; NDONE = 0
C
      IF (N.LT.0) THEN
        NITEM = -N
        NDONE = 0
C
C---- Continuation line: N = number already read
C
      ELSE IF (N.GE.0) THEN
        NDONE = N
      END IF
C
      LENLIM = LEN(LINE) - 1
      N = 1
      TOKEN = .FALSE.
      VALUE = 0.0
      OPRATR = .TRUE.
      IDOT = 0
      SIGN = 1.0
      OPER = 0
      OLDQUT = BLANK
      QUOTE = .FALSE.
      TQUOTE = .FALSE.
      NUMBER = .FALSE.
C
C---- First scan line for comment character,
C     any characters beyond this are ignored
C
      DO 10 I = 1,LENLIM + 1
        IF (LINE(I:I).EQ.ICOMM1 .OR. LINE(I:I).EQ.ICOMM2) GO TO 20
   10 CONTINUE
      I = LENLIM + 2
   20 LINLEN = I - 2
C
      IF (LINLEN.LT.0) THEN
        N = 0
      ELSE
C
C---- Main loop over character buffer
C
        DO 70 I = 1,LINLEN + 1
C
C---- Look for quotation marks
C
          IF (LINE(I:I).EQ.LETQT .OR. LINE(I:I).EQ.DBLQT) THEN
C----  1st quote must come at beginning of string, otherwise treat as normal
            IF (OLDQUT.EQ.BLANK .AND. .NOT. TOKEN) THEN
              OLDQUT = LETQT
C
C---- Start of quoted string
C
              QUOTE = .TRUE.
            ELSE IF (OLDQUT.EQ.LETQT) THEN
              OLDQUT = BLANK
              QUOTE = .FALSE.
C
C---- End of quoted string
C
            END IF
C
          ELSE
C
C---- Check for delimiting characters
C
            DO 30 J = 1,NDELM
              IF (LINE(I:I).EQ.DELIM(J)) GO TO 40
   30       CONTINUE
            J = NDELM + 1
   40       CONTINUE
C
            IF (.NOT.QUOTE .AND. (J.LE.NDELM.OR.I.GT.LINLEN)) THEN
C
C---- Have found a delimiter
C
              NULL = .FALSE.
              IF (.NOT.TOKEN .AND. J.GT.NSPDLM) THEN
C
C---- Allow delimiters other than
C     <space> & <tab> to delimit null fields
C
                IBEG(N) = I
                IEND(N) = I
                ITYP(N) = 0
                NULL = .TRUE.
              END IF
              IF (TOKEN) THEN
C
C---- End of token
C
                IEND(N) = I - 1
C
C---- Exclude quote from token
C
                IF (TQUOTE) IEND(N) = I - 2
C
C---- Store first 4 characters in cvalue for all types
C
                LENG = IEND(N) - IBEG(N) + 1
                IF (LENG.GT.4) LENG = 4
                L = IBEG(N)
                CVALUE(N) = LINE(L:L+LENG-1)
C
C---- Token is a number
C
                IF (NUMBER) THEN
                  ITYP(N) = 2
                  FVALUE(N) = VALUE*SIGN
                  IF (OPER.GT.0) FVALUE(N) = DOCALC(FVALUE(N),OPER,
     +                SIGN0*VALUE0)
                  IDEC(N) = 100*IDOT + NPLACE
                ELSE
C
C---- Token is alphameric
C
                  ITYP(N) = 1
                  IDEC(N) = LENG
                END IF
              END IF
              IF (TOKEN .OR. NULL) THEN
                N = N + 1
                NCHK = N + NDONE
                TOKEN = .FALSE.
                VALUE = 0.0
                OPRATR = .TRUE.
                IDOT = 0
                SIGN = 1.0
                OPER = 0
                TQUOTE = .FALSE.
                NUMBER = .FALSE.
C
C---- Check number of items.
C
                IF (NCHK.GT.NITEM) THEN
                  GO TO 80
                ELSE
C
                END IF
              END IF
C
C
C---- If delimiter was "+" or "-", also
C     treat it as part of the next token
C
              IF (DELIM(J).EQ.'+' .OR. DELIM(J).EQ.'-') THEN
                J = NDELM + 1
                GO TO 40
              ELSE
                GO TO 70
              END IF
C
            END IF
C
C---- Not a delimiter so must be a token
C      Suspect numeric token
C
            IF (.NOT.TQUOTE .AND. (.NOT.TOKEN.OR.NUMBER)) THEN
              IF (.NOT.QUOTE) THEN
                DO 50 J = 1,NDIGS
                  IF (LINE(I:I).EQ.DIGS(J)) GO TO 60
   50           CONTINUE
                J = NDIGS + 1
C
 60             CONTINUE
C---- Change "e" to "E"
                IF (J.EQ.17) J=15
C
           IF (J.LE.NDIGS) THEN
C
C---- May be number
C
                  NUMBER = .TRUE.
C
C---- Have a digit 0-9
C
                  IF (J.LE.10) THEN
                    IF (IDOT.EQ.0) VALUE = VALUE*10 + (J-1)
C
C---- Before decimal point
C
                    IF (IDOT.EQ.1) THEN
                      VALUE = (J-1)*F10 + VALUE
                      F10 = F10*0.1
C
C---- After decimal point
C
                      NPLACE = NPLACE + 1
                    END IF
                    OPRATR = .FALSE.
C
C---- Find + or - as signs not operators
C
                  ELSE IF (OPRATR .AND. (J.EQ.11.OR.J.EQ.12)) THEN
                    OPRATR = .FALSE.
C
C---- Set sign of number
C
                    SIGN = ISGN(J-10)
C
C---- Find + - * / e as operators
C
                  ELSE IF (J.GE.11 .AND. J.LE.15) THEN
C
C---- Do not allow 2 operators
C
                    IF (OPRATR) NUMBER = .FALSE.
                    VALUE0 = VALUE
                    SIGN0 = SIGN
                    OPER = J - 10
                    VALUE = 0.0
                    SIGN = 1.0
                    IDOT = 0
                    OPRATR = .TRUE.
C
C---- Find a decimal point
C       decimal point
C
                  ELSE IF (J.EQ.16) THEN
                    IDOT = IDOT + 1
                    NPLACE = 0
                    F10 = 0.1
                    IF (IDOT.EQ.2) NUMBER = .FALSE.
C
C---- A valid number has one point
C
                    OPRATR = .FALSE.
                  END IF
                ELSE
C
C---- Token is not number
C
                  NUMBER = .FALSE.
                END IF
C
              END IF
C
C---- Start a new token
C
              IF (.NOT.TOKEN) THEN
C
C---- Of any type
C
                TOKEN = .TRUE.
                IBEG(N) = I
C
C---- Start quoted string
C
                IF (QUOTE) THEN
                  TQUOTE = .TRUE.
                  NUMBER = .FALSE.
                END IF
              END IF
            END IF
          END IF
   70   CONTINUE
        N = N - 1
        RETURN
   80   CONTINUE
C
C
          WRITE (6,FMT='(A,I4,A)') 
     +    '  ***** WARNING - MORE THAN ',NITEM,
     +    ' ITEMS IN THIS LINE - IGNORING THE REST****'
          WRITE (LINERR,FMT='(A,I4,A)') 
     +    '  ***** WARNING - MORE THAN ',NITEM,
     +    ' ITEMS IN THIS LINE - IGNORING THE REST****'
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
          WRITE (LINERR,FMT='(A)')  LINE(1:LENSTR(LINE)) 
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
        N = N - 1
      END IF
      RETURN
C
C----------------------------------------------------------------------------
C
      ENTRY PARSDL(NEWDLM,NNEWDL,NSPECD)
C     =================================
C
C Call to change delimiters
C
C  NEWDLM  character string (NB NOT array) containing NNEWDL new delimiters
C          if NNEWDL .le. 0, reset delimiters to the standard default set
C  NSPECD  number of "special" delimiters (the first NSPECD) which
C            cannot delimit a null field
C
      IF (NNEWDL .LE. 0) THEN
C Reset default delimiters
        NDELM=NDDELM
        NSPDLM=NDSDLM
        DO 200, I = 1, MAXDLM
          DELIM(I)=DDELIM(I)
 200    CONTINUE
C  Set tab
        TAB = CHAR(9)
        DELIM(2) = TAB
C
      ELSE
        DO 210, I = 1, NNEWDL
          DELIM(I) = NEWDLM(I:I)
 210    CONTINUE
C
        NDELM  = NNEWDL
        NSPDLM = NSPECD
      ENDIF
C
      END
C
C
C     ====================================================
      SUBROUTINE KEYNUM(N,NSTART,LINE,IBEG,IEND,ITYP,NTOK)
C     ====================================================
C
C  Check that correct number of numbers are present
C
C
C
C     .. Parameter ..
      INTEGER NPARM
      PARAMETER (NPARM=200)
C
C     .. Scalar Arguments ..
      INTEGER           N,NSTART,NTOK
      CHARACTER LINE*(*)
C     ..
C     .. Array Arguments ..
      INTEGER           IBEG(*),IEND(*),ITYP(*)
C     ..
C     .. Local Scalars ..
      INTEGER           I,ISTERR,IFGERR
      CHARACTER   LINERR*200
C     ..
C     .. External Subroutines ..
      EXTERNAL          KEYERR,LERROR
C     ..
C
C
      DO 10 I = NSTART,NSTART + N - 1
          IF (I.GT.NTOK) THEN
              GO TO 30
          ELSE IF (ITYP(I).NE.2) THEN
              GO TO 20
          END IF
   10 CONTINUE
C
C
      RETURN
C
C          *******************************
   20 CALL KEYERR(I,2,LINE,IBEG,IEND,ITYP)
C          *******************************
C
      CALL CCPERR(1,' stop inparser.for 7777')
   30 CONTINUE
C
          WRITE (LINERR,FMT='(A,I4,A,I4,A)') 
     +   ' *** TOO FEW NUMBERS - ',
     +    (I - NSTART),
     +   ' FOUND WHEN ',N,' EXPECTED'
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
      call ccperr(1,' stop in parser.for 7788')
C
C
      END
C
C
C     =============================================
      SUBROUTINE KEYERR(I,MODE,LINE,IBEG,IEND,ITYP)
C     =============================================
C
C  Print warning when token not of correct type.
C
C     .. Parameter ..
      INTEGER NPARM
      PARAMETER (NPARM=200)
C
C     .. Scalar Arguments ..
      INTEGER           I,MODE
      CHARACTER LINE*(*)
C     ..
C     .. Array Arguments ..
      INTEGER           IBEG(*),IEND(*),ITYP(*)
C     ..
C     .. Local Arrays ..
      CHARACTER         TYPE(3)*12
C     ..
C     .. Local Scalars ..
      INTEGER ISTERR,IFGERR
      CHARACTER LINERR*150
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Data statements ..
      DATA              TYPE/'alphanumeric','numeric     ',
     +                  'quoted      '/
C     ..
C
C
C
      IF (MODE.EQ.0) THEN
          WRITE (LINERR,FMT='(A,A,A)') 
     +  ' ** ERROR : Key word < ',
     +  LINE(IBEG(I) : IEND(I)),
     +  ' > not recognized and has therefore been ignored'
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
      ELSE
          WRITE (LINERR,FMT='(A,A,A,A,A,A,A)') 
     + ' ** ERROR: Token < ',
     +  LINE(IBEG(I) : IEND(I)),
     + ' > is ',
     + TYPE(ITYP(I)),
     + ' while a ',TYPE(I),' token was expected'
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
      END IF
C
C
      END
C
C
      SUBROUTINE CHKNUM(ISYSW,N1,N2,NTOK,ITYP,IBEG,IEND,LINE)
C     ======================================================
C
C  used with the parser routine to check that correct number
C  of numbers are present on line
C
C  variables used by parser routine
C
C     .. Parameter ..
      INTEGER NPARM
      PARAMETER (NPARM=200)
C
C     .. Scalar Arguments ..
      INTEGER           ISYSW,N1,N2,NTOK
      CHARACTER         LINE*(*)
C     ..
C     .. Array Arguments
      INTEGER           IBEG(*),IEND(*),ITYP(*)
C     ..
C     .. Local Scalars ..
      INTEGER           I,I1,I2,ISTERR,IFGERR
      CHARACTER LINERR*200
C     ..
C     .. External Subroutines ..
      EXTERNAL          CHKTOK,LERROR
C
      SAVE
C
      IF (N2.GT.NTOK) THEN
          I1 = N2 - N1 + 1
          I2 = NTOK - N1 + 1
C
C
          WRITE (LINERR,FMT='(A,I4,A,I4,A)') 
     +  ' *** TOO FEW NUMBERS :',I2,' FOUND WHEN',I1,
     +       ' EXPECTED'
          ISTERR = 2
          IFGERR = 1
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
          call ccperr(1,' stop in parser.for 7766')
      ELSE
C
          DO 10 I = N1,N2
C
C                 ******************************************
             CALL CHKTOK(ISYSW,I,2,NTOK,ITYP,IBEG,IEND,LINE)
C                 ******************************************
C
   10     CONTINUE
C
      END IF
C
C
      END
C
      SUBROUTINE CHKTOK(ISYSW,I,IWANT,NTOK,ITYP,IBEG,IEND,LINE)
C     ========================================================
C
C  check token is of correct type
C
C      I     is token position in string line
C      iwant is code for desired token
C
C     .. Parameter ..
      INTEGER NPARM
      PARAMETER (NPARM=200)
C
C     .. Scalar Arguments ..
      INTEGER           I,ISYSW,IWANT,NTOK
      CHARACTER         LINE*(*)
C     ..
C     .. Array Arguments ..
      INTEGER          ITYP(*),IBEG(*),IEND(*)
C     ..
C     .. Local Scalars ..
      INTEGER ISTERR,IFGERR
      CHARACTER LINERR*200
C     ..
C     .. Local Arrays ..
      CHARACTER        TYPE(3)*12
C
      SAVE
C     ..
C     .. Data statements ..
      DATA              TYPE/'ALPHANUMERIC','NUMERIC     ',
     +                  'QUOTED      '/
C     ..
C
C                   
      IF (ITYP(I).NE.IWANT) THEN
          WRITE (LINERR,FMT='(A,A,A,A,A,A,A)') 
     +  ' Token ',
     +  LINE(IBEG(I) : IEND(I)),
     +  ' is ',
     +  TYPE(ITYP(I)),
     +  ' while a ',
     +  TYPE(IWANT),
     + ' token was expected'
C
          ISTERR = 2
          IFGERR = 1
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
          caLL CCPERR(1,' stop in parser.for 7755')
      END IF
C
C
      END
C 
C
      SUBROUTINE GETREA(N,X,NTOK,ITYP,FVALUE)
C     ======================================
C
C Extract real number X from N'th value Parser array FVALUE, if possible
C If no value, X = 0.0 . If illegal, write message
C
C     .. Parameter ..
      INTEGER NPARM
      PARAMETER (NPARM=200)
C
C     .. Scalar Arguments ..
      REAL              X
      INTEGER           N,NTOK
C     ..
C     .. Array Arguments ..
      REAL              FVALUE(*)
      INTEGER           ITYP(*)
C     ..
C     .. Local Scalars ..
      INTEGER ISTERR,IFGERR
      CHARACTER LINERR*200
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C
C
      X = 0.0
C
C
      IF (N.LE.NTOK) THEN
C
C
          IF (ITYP(N).EQ.2) THEN
              X = FVALUE(N)
          ELSE IF (ITYP(N).EQ.1) THEN
          WRITE (LINERR,FMT='(A,I4)') 
     +    ' Illegal number in field ',N
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
          END IF
      END IF
C
C
      END
C
C
C
      SUBROUTINE GETINT(N,I,NTOK,ITYP,FVALUE)
C     ======================================
C
C Extract integer I from N'th value Parser array FVALUE, if possible
C If no value, I = 0  . If illegal, write message
C
C     .. Parameter ..
      INTEGER NPARM
      PARAMETER (NPARM=200)
C
C     .. Scalar Arguments ..
      INTEGER           I,N,NTOK
C     ..
C     .. Array Arguments ..
      REAL              FVALUE(*)
      INTEGER           ITYP(*)
C     ..
C     .. Local scalars ..
      INTEGER ISTERR,IFGERR
      CHARACTER LINERR*100
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         NINT
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C
C
      I = 0
C
C
      IF (N.LE.NTOK) THEN
          IF (ITYP(N).EQ.2) THEN
              I = NINT(FVALUE(N))
          ELSE IF (ITYP(N).EQ.1) THEN
C 
          WRITE (LINERR,FMT='(A,I4)') 
     +   ' Illegal number in field ',N
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
          END IF
      END IF
C
C
      END
C
C
C
      SUBROUTINE GTNREA(N,M,X,NTOK,ITYP,FVALUE)
C     ========================================
C
C  Extract M real numbers X from N'th value Parser array FVALUE, 
C  if possible. If no value, X = 0.0 . If illegal, write message
C
C     .. Parameter ..
      INTEGER NPARM
      PARAMETER (NPARM=200)
C
C     .. Scalar Arguments ..
      INTEGER           M,N,NTOK
C     ..
C     .. Array Arguments ..
      INTEGER           ITYP(*)
      REAL              X(M),FVALUE(*)
C     ..
C     .. Local Scalars ..
      INTEGER           I,K,ISTERR,IFGERR
      CHARACTER LINERR*100
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C
C
      DO 10 I = 1,M
          K = I + N - 1
          X(I) = 0.0
C
C
          IF (K.LE.NTOK) THEN
              IF (ITYP(K).EQ.2) THEN
                  X(I) = FVALUE(K)
              ELSE IF (ITYP(K).EQ.1) THEN
           WRITE (LINERR,FMT='(A,I4)') 
     +    ' Illegal number in field ',K
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
              END IF
          END IF
   10 CONTINUE
C
C
      END
C
C
C
      SUBROUTINE GTNINT(N,M,J,NTOK,ITYP,FVALUE)
C     ========================================
C
C Extract M integers J from N'th value Parser array FVALUE, if possible
C If no value, J = 0  . If illegal, write message
C
C     .. Parameter ..
      INTEGER NPARM
      PARAMETER (NPARM=200)
C
C     .. Scalar Arguments ..
      INTEGER           M,N,NTOK
C     ..
C     .. Array Arguments ..
      INTEGER           J(M),ITYP(*)
      REAL              FVALUE(*)
C     ..
C     .. Local Scalars ..
      INTEGER           I,K,ISTERR,IFGERR
      CHARACTER LINERR*200
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         NINT
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C
C
      DO 10 I = 1,M
          K = I + N - 1
          J(I) = 0
          IF (K.LE.NTOK) THEN
              IF (ITYP(K).EQ.2) THEN
                  J(I) = NINT(FVALUE(K))
              ELSE IF (ITYP(K).EQ.1) THEN
           WRITE (LINERR,FMT='(A,I4)') 
     +    ' Illegal number in field ',K
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
              END IF
          END IF
   10 CONTINUE
C
C
      END
C
C
C
      SUBROUTINE GTPREA(N,X,NTOK,ITYP,FVALUE)
C     ======================================
C
C---- Extract real number X from N'th value Parser array FVALUE,
C     if possible
C
C---- If no value, leave X unchanged. If illegal, write message
C
C     .. Parameters ..
      INTEGER NPARM
      PARAMETER (NPARM=200)
C     ..
C     .. Scalar Arguments ..
      REAL X
      INTEGER N,NTOK
C     ..
C     .. Array arguments ..
      REAL FVALUE(*)
      INTEGER ITYP(*)
C     ..
C     .. Local Scalars ..
      INTEGER ISTERR,IFGERR
      CHARACTER LINERR*200
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C
C
      IF (N.LE.NTOK) THEN
        IF (ITYP(N).EQ.2) THEN
          X = FVALUE(N)
        ELSE IF (ITYP(N).EQ.1) THEN
           WRITE (LINERR,FMT='(A,I4)') 
     +    ' Illegal number in field ',N
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
        END IF
      END IF
C
C
      END
C
C
C
      SUBROUTINE GTPINT(N,I,NTOK,ITYP,FVALUE)
C     ======================================
C
C---- Extract integer I from N'th value Parser array FVALUE,
C     if possible
C
C---- If no value, leave I unchanged. If illegal, write message
C
C     .. Parameters ..
      INTEGER NPARM
      PARAMETER (NPARM=200)
C     ..
C     .. Scalar Arguments ..
      INTEGER I,N,NTOK
C     ..
C     .. Arrays arguments ..
      REAL FVALUE(*)
      INTEGER ITYP(*)
C     ..
C     .. Local Scalars ..
      INTEGER ISTERR,IFGERR
      CHARACTER LINERR*100
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
C
C
      IF (N.LE.NTOK) THEN
        IF (ITYP(N).EQ.2) THEN
          I = NINT(FVALUE(N))
        ELSE IF (ITYP(N).EQ.1) THEN
           WRITE (LINERR,FMT='(A,I4)') 
     +    ' Illegal number in field ',N
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
        END IF
      END IF
C
C
      END
C
C
C
      SUBROUTINE GETSTR(N,STRING,NTOK,ITYP,IBEG,IEND,LINE)
C     ===================================================
C
C Extract string STRING from N'th value Parser array CVALUE, if possible
C If no value, STRING  = blank '    '.
C
C     .. Parameter ..
      INTEGER NPARM
      PARAMETER (NPARM=200)
C
C     .. Scalar Arguments ..
      INTEGER           N,NTOK
      INTEGER           ITYP(*),IBEG(*),IEND(*)
      CHARACTER         STRING*(*), LINE*(*)
C     ..
C
C
      STRING = ' '
      IF (N.LE.NTOK .AND. ITYP(N).NE.0) STRING = LINE(IBEG(N):IEND(N))
C
C
      END
C
C
C
      REAL FUNCTION DOCALC(VALUE,OPER,VALUE0)
C     =======================================
C
C      Do simple arithmetic on two arguments
C
C     .. Scalar Arguments ..
      REAL                 VALUE,VALUE0
      INTEGER              OPER
C     ..
C     .. Local Scalars ..
      INTEGER              DIVIDE,EENOTN,MINUS,MULT,PLUS
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC            ABS
C     ..
C     .. Data statements ..
      DATA                 PLUS,MINUS,MULT,DIVIDE,EENOTN/1,2,3,4,5/
C     ..
C
C
      IF (OPER.EQ.PLUS) DOCALC = VALUE0 + VALUE
      IF (OPER.EQ.MINUS) DOCALC = VALUE0 - VALUE
      IF (OPER.EQ.MULT) DOCALC = VALUE0*VALUE
C
C
      IF (OPER.EQ.DIVIDE) THEN
          IF (VALUE.NE.0.0) DOCALC = VALUE0/VALUE
          IF (VALUE.EQ.0.0) DOCALC = 0.0
      END IF
C
C
      IF (OPER.EQ.EENOTN) THEN
          IF (ABS(VALUE).LE.76.0) DOCALC = VALUE0* (10.0**VALUE)
          IF (ABS(VALUE).GT.76.0) DOCALC = 0.0
      END IF
C
C
      END
C
C
C
      SUBROUTINE SBLANK(ARRAY,N1,N2)
C     ==============================
C
C---- Blank characters N1 to N2 of ARRAY
C
      CHARACTER*1 ARRAY(*)
C
      DO 10 I=N1,N2
         ARRAY(I)=' '
10     CONTINUE
C
C
      RETURN
      END
C
C
C
      SUBROUTINE GTCFLD(NFIELD,ITEXT,NCHAR,MIN,MAX,IGFLAG)
C     ====================================================
C
C
C
C---- This subroutine finds the minimum and maximum character 
C     numbers in a packed text string for a requested field number. 
C     The character fields are assumed to be separated by spaces
C
C
C
C PARAMETERS
C
C      NFIELD (I) the number of the field to be retrieved
C       ITEXT (I) array containing the packed character string to be
C                 interpreted (character array)
C       NCHAR (I) the no. of characters in the text string
C         MIN (O) the no. of the first character in the requested field
C         MAX (O) the no. of the final character in the requested field
C
C    RETURN 1     blank field found (end of text string)
C                 IGFLAG = -1
C
      CHARACTER*1 ITEXT(NCHAR)
C
C---- initialisations
C
      IGFLAG=0
      IFIELD=0
      I=0
      MIN=0
      MAX=NCHAR
C
C---- skip spaces up to start of next field
C
10    I=I+1
C
C
      IF(I.GT.NCHAR) THEN
       IGFLAG = -1
       RETURN 
       ENDIF
C
C
      IF(ITEXT(I).EQ.' ')GO TO 10
C
C---- character field found
C
      IFIELD=IFIELD+1
      MIN=I
C
C---- search for end of the character field
C
20    I=I+1
      IF(I.GT.NCHAR)GO TO 100
      IF(ITEXT(I).NE.' ')GO TO 20
C
C---- end of character field found.  see if required field
C     has been found
C
      IF(IFIELD.NE.NFIELD)GO TO 10
      MAX=I-1
      RETURN
C
C---- end of string reached
C
100   IF(IFIELD.NE.NFIELD) THEN
            IGFLAG=-1
            RETURN 
            END IF
      RETURN
      END
C
C
C
      SUBROUTINE CPYCHR(STRNGA,STRNGB,NCHAR)
C     ========================================
C
C
C
C---- copy nc characters from character array b to a
C
C
      INTEGER NCHAR
      CHARACTER*1 STRNGA(*),STRNGB(*)
C
C
      DO 10 I=1,NCHAR
      STRNGA(I)=STRNGB(I)
10    CONTINUE
C
C
      RETURN
      END
C
C
C
      LOGICAL FUNCTION CMATCH(STRING1,STRING2,NCHAR)
C     ==============================================
C
C
C---- test k characters against characters read into a
C     character variable f 
C     return cmatch .true. if all match, else .false.
C
      CHARACTER*(*) STRING1,STRING2
      INTEGER NCHAR
C
C
      IF(STRING1(1:NCHAR).EQ.STRING2(1:NCHAR)) THEN
          CMATCH=.TRUE.
      ELSE
          CMATCH=.FALSE.
      ENDIF
      RETURN
      END
C
C
C
      SUBROUTINE CMOVE(STRNGA,STRNGB,NCHAR)
C     =======================================
C
C---- copy n characters from b to a :  
C     (only used for character data  IN FORTRAN77
C
C
C
      CHARACTER*1 STRNGA(*),STRNGB(*)
      INTEGER NCHAR
C
C
      IF(NCHAR.LE.0)RETURN
C
C
      DO 10 I=1,NCHAR
      STRNGA(I)=STRNGB(I)
10    CONTINUE
C
C
      RETURN
      END
C
C
C
      SUBROUTINE CHKKEY(KEY,WORDS,NWORDS,IKEY)
C     ========================================
C
C Check keyword KEY against list of NWORDS possible keywords WORDS
C Allows abbreviated or extended keys provided they are not ambiguous
C  If KEY = '?', list all words
C
C Returns:
C  IKEY  = keyword number found (.gt.0)
C        = 0 if not found or null
C        = -1 if ambiguous
C
C     .. Scalar Arguments ..
      INTEGER NWORDS, IKEY
      CHARACTER KEY*(*)
C     ..
C     .. Array Arguments ..
      CHARACTER WORDS(NWORDS)*(*)
C     ..
C     .. Local Scalars ..
      INTEGER LK,I,L,NFOUND,NFMAX,ISTERR,IFGERR
      CHARACTER OUTWIN*6,LINERR*200
C     ..
C     .. Local Arrays ..
      INTEGER LFOUND(20)
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL PUTLIN,LERROR
C     ..
C     .. Scalars in Common ..
      CHARACTER STROUT*800
C     ..
      SAVE
C
      DATA NFMAX/20/
C
C---- Get minimum significant length of KEY 
C     ( function LENSTR returns the length
C     of the character string excluding trailing blanks)
C
      IKEY=0
C
C        ***********
      LK=LENSTR(KEY)
C        ***********
C
C---- Ignore null string
C
      IF(LK.LE.0) RETURN
C
      IF(KEY(1:1).EQ.'?') THEN
C  
       STROUT = ' Possible keywords are:'
C
C           ****************
       CALL PUTLIN(STROUT,'HLPWIN')
C           ****************
C
       DO 10 JDO = 1,NWORDS
        STROUT = WORDS(JDO)
C
C            ****************
        CALL PUTLIN(STROUT,'HLPWIN')
C            ****************
C
10      CONTINUE
C
C
            IKEY=0
            RETURN
      ENDIF
C
C
      NFOUND=0
C
C---- Check all possible words in case of ambiguities
C
      DO 20 I=1,NWORDS
C
C----  Key may be longer than word in list
C
C              ****************
      L=MIN(LK,LENSTR(WORDS(I)))
C              ****************
C
      IF(L.LE.0) GO TO 20
C
C---- Find out if KEY is an initial substring of this option word
C
      IF(INDEX(WORDS(I),KEY(1:L)).EQ.1) THEN
            NFOUND=NFOUND+1
C
C
            IF(NFOUND.GT.NFMAX) THEN
          WRITE (LINERR,FMT='(A,I5)') 
     +  ' CHKKEY: too many ambiguities : ',NFMAX
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
                  NFOUND=NFMAX
            ELSE
                  LFOUND(NFOUND)=I
            ENDIF
      ENDIF
20     CONTINUE
C
C---- If keyword is ambiguous, list possibilities
C
      IF(NFOUND.GT.1) THEN
          WRITE (LINERR,FMT='(A,A,A)') 
     +   ' Keyword ',
     +   KEY(1:LK),
     +  ' is ambiguous: possibilities are -'
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
       DO 30 JDO = 1,NWORDS
        STROUT = WORDS(JDO)
C
C            ****************
        CALL PUTLIN(STROUT,'HLPWIN')
C            ****************
C
30      CONTINUE
            IKEY=-1
      ELSEIF (NFOUND.EQ.1) THEN
C
C---- Success if only 1 found
C
            IKEY=LFOUND(1)
      ENDIF
C
      RETURN
      END
C
C
C     =========================
      SUBROUTINE PUTLIN(STROUT,OUTWIN)
C     =========================
C
C
C
C---- This is a dummy PUTLIN to link with the MTZ routines mark 1 -
C     all it does is write the line in STROUT to lun 6. Later the
C     routines will be linked with the Compose-Parser etc. from Kim
C     where PUTLIN does a few more things !
C
C
C
C
C
C
C     .. Scalar Arguments ..
      CHARACTER OUTWIN* (*)
      CHARACTER STROUT* (*)
C     ..
C     .. Local Scalars ..
      INTEGER LUNOUT
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. Data statements ..
C
C
      DATA LUNOUT/6/
C     ..
C
C
      LL = LENSTR(STROUT)
      IF (LL.GT.133) THEN
      LX = 1
      LS = 132
10    CONTINUE
      WRITE (LUNOUT,FMT=6000) STROUT(LX:LS)
      IF (LS.EQ.LL) GOTO 20
      LX = LS  + 1
      LS = LS + 131
      IF (LS.GT.LL) LS = LL
      GO TO 10
      ELSE
       IF (LL.EQ.0) THEN
           WRITE(LUNOUT,FMT=6000)
            ELSE
      WRITE (LUNOUT,FMT=6000) STROUT(1:LL)
           END IF
      END IF
20    CONTINUE
C
C---- Format statements
C
 6000 FORMAT (' ',A)
C
C
      END
C
C
C     ===============================
      SUBROUTINE BLANK(OUTWIN,NLINES)
C     ===============================
C
C
C
C---- This subroutine calls PUTLIN to output NLINES blank lines to the
C     window OUTWIN
C
C---- Arguments :
C
C     OUTWIN    CHARACTER*6     output window
C
C     NLINES    INTEGER         number of blank lines to output
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER NLINES
      CHARACTER OUTWIN*6
C     ..
C     .. Scalars in Common ..
      CHARACTER STROUT*800
C     ..
C     .. Local Scalars ..
      INTEGER JDO10
C     ..
C     .. External Subroutines ..
      EXTERNAL PUTLIN
C     ..
      SAVE
C
C
      IF (NLINES.LT.1) NLINES = 1
      STROUT = ' '
C
C
      DO 10 JDO10 = 1,NLINES
C
C            **************
        CALL PUTLIN(STROUT,OUTWIN)
C            **************
C
   10 CONTINUE
C
C
      END
C
C
C     =======================================
      SUBROUTINE LERROR(ERRFLG,IFAIL,ERRMSG)
C     =======================================
C
C
C
C---- General error reporting subroutine, for the MTZ routines, etc
C
C---- Arguments:
C
C     ERRFLG    INTEGER         =1 output meesage as warning
C                               =2 output message as fatal
C
C     IFAIL     INTEGER         =0 return after fatal error
C                               =-1 STOP after reporting fatal error
C
C     ERRMSG    CHARACTER*(*)   character string containing error
C                               message to output
C
C
C
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER ERRFLG,IFAIL
      CHARACTER ERRMSG* (*)
C     ..
C     .. Scalars in Common ..
      CHARACTER STROUT*800
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL BLANK,PUTLIN
C     ..
      SAVE
C
C
      IF (ERRFLG.EQ.1) THEN
C
C---- Output a warning message and return
C
C            *****************
        CALL BLANK('ERRWIN',1)
C            *****************
C
        STROUT = '***  Warning'
C
C            ****************
        CALL PUTLIN(STROUT,'ERRWIN')
C            ****************
C
        WRITE (STROUT,FMT='(A)') ERRMSG(1:LENSTR(ERRMSG))
C
C            ****************
        CALL PUTLIN(STROUT,'ERRWIN')
        CALL BLANK('ERRWIN',1)
C            *****************
C
      ELSE IF (ERRFLG.EQ.2) THEN
C
C---- Output a fatal message, and quit or return depending on IFAIL
C
        CALL BLANK('ERRWIN',1)
        STROUT = '***  Error'
        CALL PUTLIN(STROUT,'ERRWIN')
        WRITE (STROUT,FMT='(A)') ERRMSG(1:LENSTR(ERRMSG))
        CALL PUTLIN(STROUT,'ERRWIN')
        IF (IFAIL.LT.0) THEN
          STROUT = '*** Program Terminated '
          CALL PUTLIN(STROUT,'ERRWIN')
          call ccperr(1,' stop in parser.for 7733')
        ELSE
          CALL BLANK('ERRWIN',1)
        END IF
        RETURN
      ELSE
C
C---- Bad errflg, output message and continue
C
        CALL BLANK('ERRWIN',1)
        STROUT = '*** Unrecognised  error'
        CALL PUTLIN(STROUT,'ERRWIN')
        WRITE (STROUT,FMT='(A)') ERRMSG(1:LENSTR(ERRMSG))
        CALL PUTLIN(STROUT,'ERRWIN')
        STROUT = 'Program continuing ...'
        CALL PUTLIN(STROUT,'ERRWIN')
        CALL BLANK('ERRWIN',1)
C
      END IF
C
C
      END
C
C
C
C
      SUBROUTINE RDSYMM(JTOK,LINE,IBEG,IEND,ITYP,FVALUE,NTOK,
     .    SPGNAM,NUMSGP,PGNAME,NSYM,NSYMP,RSYM)
C     =========================================
C     
C     Read and decode symmetry specification
C     
C     On entry:
C     LINE,IBEG,IEND,ITYP,FVALUE,NTOK contain information from Parser
C     JTOK  is first field to interpret
C     
C     NSYM  number of symmetry operations already read (should be cleared
C     to 0 at beginning)
C     
C     On exit
C     SPGNAM  Spacegroup name
C     NUMSGP  Spacegroup number
C     PGNAME  Pointgroup name
C     NSYM    Number of symmetry operations (including non-primitive)
C     NSYMP   Number of primitive symmetry operations
C     RSYM    Symmetry matrices (4x4)
C     
C
C     
      INTEGER JTOK,NTOK
      INTEGER IBEG(NTOK),IEND(NTOK),ITYP(NTOK)
      REAL FVALUE(NTOK)
      CHARACTER*(*)LINE,SPGNAM,PGNAME
      INTEGER NUMSGP,NSYM,NSYMP
      REAL RSYM(4,4,*)
C     
      CHARACTER*100 STROUT
C     
C     
C---- Look at next field on line: this can be
C     (a) a space-group number
C     (b) a space-group name, ie a string beginning P,I,R,F,A,B or C
C     (c) a symmetry operation (anything else)
C     
C---- for cases (a) & (b), this is a single field:
C     case (c) is more than 1 field
C     
      IF (JTOK.GT.NTOK) THEN
         WRITE (STROUT,FMT='(A)') ' No symmetry data !!!'
         CALL  PUTLIN(STROUT,'CURWIN')
      ELSE
         IF (JTOK.EQ.NTOK) THEN
            SPGNAM = ' '
            IF (NSYM.GT.0) THEN
               WRITE (STROUT,FMT='(A)')
     +             'Warning: symmetry already given'
               CALL  PUTLIN(STROUT,'CURWIN')
            ENDIF
C     
C---- A single field, see if it is a number or a string
C     
            IF (ITYP(JTOK).EQ.2) THEN
C     
C---- it's a number, treat as space-group number
C     
               NUMSGP = NINT(FVALUE(JTOK))
            ELSE
C     
C---- it's a string, treat as space-group name
C     
               SPGNAM = LINE(IBEG(JTOK) :IEND(JTOK))
               NUMSGP = 0
            END IF
C     
C---- Read symmetry (all operations) from SYMOP
C     open symop on channel 24 - closed at end of reading
C     NSYMP returns number of primitive operations
C     
            CALL  CCPUPC(SPGNAM)
            CALL  MSYMLB(24,NUMSGP,SPGNAM,PGNAME,NSYMP,NSYM,RSYM)
         ELSE
C     
C     
C---- Read symmetry operations
C     
            NSYM = NSYM + 1
            NSYMP = NSYM
            CALL  CCPUPC(LINE)
            CALL  SYMFR2(LINE,IBEG(JTOK),NSYM,RSYM)
            NUMSGP = 0
            SPGNAM = ' '
            PGNAME = ' '
C     
         END IF
      END IF
C     
      RETURN
      END
C     
C     
C     
      SUBROUTINE RDHEAD(JTOK,LINE,IBEG,IEND,ITYP,FVALUE,NTOK,
     .    MTZPRT,MTZBPR)
C     =========================================
C     
C     Read and decode HEADER command, to set print flags for MTZ headers
C     
C     On entry:
C     LINE,IBEG,IEND,ITYP,FVALUE,NTOK contain information from Parser
C     JTOK  is first field to interpret
C     
C     On exit:
C     
C     MTZPRT flag to control printout from MTZ file header
C     
C     NONE  sets  MTZPRT = 0
C     no header o/p
C     BRIEF  sets MTZPRT = 1  (default)
C     brief header o/p
C     HISTORY sets MTZPRT = 2
C     brief + mtz history
C     ALL     sets MTZPRT = 3
C     full header o/p from mtz reads
C     
C     MTZBPR controls printout from BATCH HEADERS
C     
C     NOBATCH sets  MTZBPR = 0
C     no batch header o/p
C     BATCH    sets MTZBPR = 1  (default)
C     batch titles o/p
C     ORIENTATION sets MTZBPR = 2
C     batch orientation also
C     
C
C     
      INTEGER JTOK,NTOK
      INTEGER IBEG(NTOK),IEND(NTOK),ITYP(NTOK)
      REAL FVALUE(NTOK)
      CHARACTER*(*) LINE
      INTEGER MTZPRT,MTZBPR
C     
      CHARACTER*100 STROUT
C     
      INTEGER NKEYS
      PARAMETER (NKEYS=7)
      CHARACTER*12 KEYS(NKEYS)
C     
C     Locals
      INTEGER I,IKEY
      CHARACTER KEY*12
C     
      DATA KEYS/'NONE','BRIEF','HISTORY','ALL',
     $     'NOBATCH','BATCH','ORIENTATION'/
C     Set defaults
      MTZPRT = 1
      MTZBPR = 1
C     
C     Loop keywords
      IF (NTOK .GE. JTOK) THEN
         DO 10, I=JTOK,NTOK
            KEY = LINE(IBEG(I):IEND(I))
            CALL CCPUPC(KEY)
            CALL CHKKEY(KEY,KEYS,NKEYS,IKEY)
            IF (IKEY .LE. 0) THEN
               WRITE(STROUT,FMT='(A,A)')
     $      'Unrecognized or ambiguous subkeyword to HEADER: ',KEY
               CALL PUTLIN(STROUT,'CURWIN')
            ELSE
               IF (IKEY .EQ. 1) MTZPRT = 0
               IF (IKEY .EQ. 2) MTZPRT = 1
               IF (IKEY .EQ. 3) MTZPRT = 2
               IF (IKEY .EQ. 4) MTZPRT = 3
               IF (IKEY .EQ. 5) MTZBPR = 0
               IF (IKEY .EQ. 6) MTZBPR = 1
               IF (IKEY .EQ. 7) MTZBPR = 2
            ENDIF
 10      CONTINUE 
      ENDIF
C     
      RETURN
      END
C     
C   
C     ==============================================
      SUBROUTINE RDCELL(ITOK,ITYPE,FVALUE,NTOK,CELL)
C     ==============================================
C     
C     
C     
C---- Read and decode resolution limits.
C     
C     On entry:
C     
C     ITYPE,FVALUE,NTOK contain information from Parser
C     ITOK  is first field to interpret
C     
C     On exit:
C     
C     CELL(1-6)  Cell dimensions.
C     
C     
C
C     
C     
C     .. Scalar Arguments ..
      INTEGER           ITOK,NTOK
C     ..
C     .. Array Arguments ..
      REAL              CELL(6),FVALUE(*)
      INTEGER           ITYPE(*)
C     ..
C     ..
C     .. External Subroutines ..
      EXTERNAL          GTPREA
C     ..
C     
C     
      CELL(4) = 90.0
      CELL(5) = 90.0
      CELL(6) = 90.0
C     
C     ***************************************
      CALL GTPREA(ITOK,CELL(1),NTOK,ITYPE,FVALUE)
      CALL GTPREA(ITOK+1,CELL(2),NTOK,ITYPE,FVALUE)
      CALL GTPREA(ITOK+2,CELL(3),NTOK,ITYPE,FVALUE)
C     ***************************************
C     
C     *********************************************
      IF (ITOK+3.LE.NTOK) CALL GTPREA(ITOK+3,CELL(4),NTOK,ITYPE,FVALUE)
      IF (ITOK+4.LE.NTOK) CALL GTPREA(ITOK+4,CELL(5),NTOK,ITYPE,FVALUE)
      IF (ITOK+5.LE.NTOK) CALL GTPREA(ITOK+5,CELL(6),NTOK,ITYPE,FVALUE)
C     *********************************************
C     
C     
      RETURN
      END
C     
C   
C     
C     =====================================================
      SUBROUTINE RDRESO(ITOK,ITYPE,FVALUE,NTOK,RESMIN,
     +                  RESMAX,SMIN,SMAX)
C     =====================================================
C     
C     
C---- Read and decode resolution limits.
C     
C     
C     On entry:
C     
C     ITYPE,FVALUE,NTOK contain information from Parser
C     ITOK  is first field to interpret
C     
C     On exit:
C     
C     RESMIN  Minimum resolution (in As)
C     RESMAX  Maximum resolution (in As)
C     SMIN    Minimum resolution ( 4sin**2/lambda**2)
C     SMAX    Maximum resolution ( 4sin**2/lambda**2)
C     
C
C     
C     
C     .. Scalar Arguments ..
      REAL              RESMAX,RESMIN,SMAX,SMIN
      INTEGER           ITOK,NTOK
C     ..
C     .. Array Arguments ..
      REAL              FVALUE(*)
      INTEGER           ITYPE(*)
C     ..
C     .. Local Scalars ..
      REAL              RESTEM,STEM
C     ..
C     .. External Subroutines ..
      EXTERNAL          GTPREA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         SQRT
C     ..
C
C---- Global defaults set here
C
        RESMAX = 100.0
        RESMIN = .1
C     
C---- Look at next field on line: this can be
C     read resolution limits in A, if only one treat as high
C     resolution limit
C     
C     *************************************
      CALL GTPREA(ITOK,RESMIN,NTOK,ITYPE,FVALUE)
C     *************************************
C     
      IF (ABS(RESMIN).LE.0.000001) RESMIN = 0.00001
C     
C     ***************************************
      IF (NTOK.GE.(ITOK+1))
     +      CALL GTPREA(ITOK+1,RESMAX,NTOK,ITYPE,FVALUE)
C     ***************************************
C     
      IF (ABS(RESMAX).LE.0.0000001) RESMAX = 100.0
C     
C     
      IF (RESMIN.LE.RESMAX) THEN
         RESTEM = RESMAX
         RESMAX = RESMIN
         RESMIN = RESTEM
      END IF
C     
C---- option to read 4sin**2/lamda**2
C     
      IF (RESMIN.LE.1.0 .AND. RESMAX.LE.1.0) THEN
C     
C---- swap over smin and resmin etc
C     
         SMIN = RESMIN
         SMAX = RESMAX
         RESMAX = SQRT(1.0/SMIN)
         RESMIN = SQRT(1.0/SMAX)
      ELSE
         SMIN = 1.0/RESMAX**2
         SMAX = 1.0/RESMIN**2
      END IF
C     
C     
      IF (SMIN.GT.SMAX) THEN
         STEM = SMAX
         SMAX = SMIN
         SMIN = STEM
      END IF
C     
C     
      RETURN
      END
C     
      
C     
      SUBROUTINE RDSCAL(ITOK,LINE,IBEG,IEND,ITYP,FVALUE,NTOK,
     .    NLPRGI,LSPRGI,ILPRGI,SCAL,BB)
C     =========================================
C     
C     Read and decode SCALE .
C     
C     LSPRGI    CHARACTER*30    program label strings (array)
C     L(abel) S(tring) PRG(rammme) I(nput)
C     
C     
C     On entry:
C     LINE,IBEG,IEND,ITYP,FVALUE,NTOK contain information from Parser
C     ITOK  is first field to interpret
C     
C     On exit
C     ILPRGI - number in array of LSPRGI whose scale has been reset
C     SCAL - scale factor.
C     BB   - temperature factor.
C
C     
      INTEGER MCOLS
      PARAMETER (MCOLS=200)
      INTEGER ITOK,NTOK,ILPRGI,NLPRGI,JDO
      INTEGER IBEG(NTOK),IEND(NTOK),ITYP(NTOK)
      REAL FVALUE(NTOK)
      CHARACTER*(*) LINE
      CHARACTER LSPRGI(MCOLS)*30,CWORK*30
      REAL SCAL,BB
C     
      CHARACTER*100 STROUT
C     
      CWORK = LINE(IBEG(ITOK) :IEND(ITOK))
          DO 10 JDO = 1,NLPRGI
C     
         IF (CWORK.EQ.LSPRGI(JDO)) GO TO 20
C     
 10   CONTINUE
C     
      STROUT = ' **** Error input assignment does not match'//
     +               ' program labels'
C     
C     ***********************
      CALL PUTLIN(STROUT,'ERRWIN')
C     ***********************
C     
C     
C     
 20   ILPRGI = JDO
      CALL GTPREA(ITOK+1,SCAL,NTOK,ITYP,FVALUE)
      BB = 0
      IF(ITOK+2.LE.NTOK)CALL GTPREA(ITOK+2,BB,NTOK,ITYP,FVALUE)
C     
      RETURN
      END 
