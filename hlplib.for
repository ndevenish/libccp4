        SUBROUTINE CCPHLP(LOGHLP,HLPSTR)
C       ================================
C
C
C
C---- Command Line help by reading data file
C
C
C
C     .. Arguments ..
      CHARACTER HLPSTR*(*)
      CHARACTER LOGHLP*(*)
C
C     .. Local Scalars ..
      CHARACTER HLPFIL*255
C
      INTEGER LUNIN,LUNOUT,IHELP,NSCREN
      LOGICAL VMSVAX,FIRSTT,HLPFAL,HLPCAL
C     .. Common blocks ..
      COMMON /HINOUT/LUNIN,LUNOUT,IHELP,VMSVAX,NSCREN,HLPFAL,HLPCAL
C
      SAVE
C     ..
C     .. External Subroutines ..
      INTEGER LENSTR
      EXTERNAL UGTENV,LENSTR,GETHLP
C     ..
C
      DATA FIRSTT/.TRUE./
C
C
      IF (FIRSTT) THEN
          LUNIN = 5
          LUNOUT = 6
          IHELP = 90
C
      HLPFAL = .FALSE.
      HLPFIL = ' '
C
C---- Check for environment variable LOGHLP
C
C          ****************************************
      CALL UGTENV (LOGHLP(1:LENSTR(LOGHLP)),HLPFIL)
C          ****************************************
C
      IF (LENSTR(HLPFIL).EQ.0) THEN
          WRITE (LUNOUT,*) ' Error cant translate logical name ',
     +                     LOGHLP(1:LENSTR(LOGHLP))
          RETURN
      END IF
C
C
       FIRSTT = .FALSE.
       END IF
C
C
       IF (HLPSTR.EQ.' ') HLPSTR = 'HELP ?'
C
C          *********************
      CALL GETHLP (HLPFIL,HLPSTR)
C          *********************
C
      RETURN
C
C
      END
C
C
C
      SUBROUTINE CLREOL
C     =================
C
C
C PURPOSE: Clear screen from active cursor position to end of line
C          on an ansi terminal.
C
C
C
C---- ESC : ascii code for escape character.
C
C     .. Local Scalars ..
      CHARACTER OUTSTR*3,SPACE*1
C     ..
C     .. External Subroutines ..
      EXTERNAL HLPSED
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC CHAR
C     ..
C
C
      OUTSTR(1:1) = CHAR(27)
      OUTSTR(2:2) = '['
      OUTSTR(3:3) = 'K'
      SPACE = ' '
C
C          ********************
      CALL HLPSED(OUTSTR,SPACE)
C          ********************
C
      END
C
C
C
      SUBROUTINE CLRMID(IBEGIN,NLINES)
C     ================================
C
C
C PURPOSE: Clear a portion of the screen.
C
C
C IBEGIN : Line to start clear screen
C NLINES : Number of lines to clear
C
C
C---- Each line specified in turn
C
C     .. Scalar Arguments ..
      INTEGER IBEGIN,NLINES
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
C     .. External Subroutines ..
      EXTERNAL CLREOL,CURSLC
C     ..
C
C
      DO 10 I = 1,NLINES
C
C            ********************
        CALL CURSLC(IBEGIN+I-1,1)
        CALL CLREOL
C            ********************
C
   10 CONTINUE
C
C
      END
C
C
C
      SUBROUTINE CLRSCR
C     ==================
C
C
C PURPOSE: Clear entire screen of a vt100 compatible terminal.
C
C
C
C---- ESC : ascii code for escape character.
C
C     .. Local Scalars ..
      CHARACTER OUTSTR*4,SPACE*1
C     ..
C     .. External Subroutines ..
      EXTERNAL HLPSED
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC CHAR
C     ..
C
C
      OUTSTR(1:1) = CHAR(27)
      OUTSTR(2:2) = '['
      OUTSTR(3:3) = '2'
      OUTSTR(4:4) = 'J'
      SPACE = ' '
C
C          ********************
      CALL HLPSED(OUTSTR,SPACE)
C          ********************
C
      END

C
C
C
      SUBROUTINE CURSLC(NLINE,NCOL)
C     =============================
C
C PURPOSE: Move the active cursor position to the specified line and
C          column.
C
C NLINE  : Line position of cursor
C NCOL   : Column postion of cursor
C
C
C
C---- Arguments out of range exit
C
C     .. Scalar Arguments ..
      INTEGER NCOL,NLINE
C     ..
C     .. Local Scalars ..
      CHARACTER STOUT1*6,STOUT2*7,STOUT3*7,STOUT4*8,SPACE*1
C     ..
      INTEGER LUNIN,LUNOUT,IHELP,NSCREN
      LOGICAL VMSVAX,HLPFAL,HLPCAL
C     .. Common blocks ..
      COMMON /HINOUT/LUNIN,LUNOUT,IHELP,VMSVAX,NSCREN,HLPFAL,HLPCAL
C     ..
C     .. External Subroutines ..
      EXTERNAL HLPSED
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC CHAR
C     ..
      SAVE
C
C
      SPACE = ' '
C
      IF ((NLINE.GE.1) .AND. (NLINE.LE.NSCREN)) THEN
        IF ((NCOL.GE.1) .AND. (NCOL.LE.80)) THEN
C
C---- Send ansi escape to terminal (i2 format needed if lin/col >9)
C
          IF ((NLINE.LT.10) .AND. (NCOL.LT.10)) THEN
            STOUT1(1:1) = CHAR(27)
            STOUT1(2:2) = '['
            WRITE (STOUT1(3:3),FMT='(I1)') NLINE
            STOUT1(4:4) = ';'
            WRITE (STOUT1(5:5),FMT='(I1)') NCOL
            STOUT1(6:6) = 'H'
C
C                ********************
            CALL HLPSED(STOUT1,SPACE)
C                ********************
C
          ELSE IF ((NLINE.LT.10) .AND. (NCOL.GE.10)) THEN
            STOUT2(1:1) = CHAR(27)
            STOUT2(2:2) = '['
            WRITE (STOUT2(3:3),FMT='(I1)') NLINE
            STOUT2(4:4) = ';'
            WRITE (STOUT2(5:6),FMT='(I2)') NCOL
            STOUT2(7:7) = 'H'
C
C                ********************
            CALL HLPSED(STOUT2,SPACE)
C                ********************
C
          ELSE IF ((NLINE.GE.10) .AND. (NCOL.LT.10)) THEN
            STOUT3(1:1) = CHAR(27)
            STOUT3(2:2) = '['
            WRITE (STOUT3(3:4),FMT='(I2)') NLINE
            STOUT3(5:5) = ';'
            WRITE (STOUT3(6:6),FMT='(I1)') NCOL
            STOUT3(7:7) = 'H'
C
C                ********************
            CALL HLPSED(STOUT3,SPACE)
C                *********************
C
          ELSE
C
            STOUT4(1:1) = CHAR(27)
            STOUT4(2:2) = '['
            WRITE (STOUT4(3:4),FMT='(I2)') NLINE
            STOUT4(5:5) = ';'
            WRITE (STOUT4(6:7),FMT='(I2)') NCOL
            STOUT4(8:8) = 'H'
C
C                ********************
            CALL HLPSED(STOUT4,SPACE)
C                ********************
C
          END IF
        END IF
      END IF
C
C
      END
C
C
C
      SUBROUTINE FNDWRD(LINHLP,NCHARS,OPTIOS,NOPTS,MAXLEV)
C     ====================================================
C
C
C     .. Scalar Arguments ..
      INTEGER MAXLEV,NCHARS,NOPTS
      CHARACTER LINHLP* (*)
C     ..
C     .. Array Arguments ..
      CHARACTER OPTIOS(*)*12
C     ..
C     .. Local Scalars ..
      INTEGER I,J,KLEN,LENGTH,MAXLEN,NSTART
      CHARACTER SPACE*1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX,LEN
C     ..
C
C---- Initialise
C              
      SPACE = ' '
      MAXLEN = 12
      NSTART = 1
C
C
      DO 30 J = 1,MAXLEV
C
C---- Find first non-blank character
C
        DO 10 I = NSTART,NCHARS
C
C
          IF (LINHLP(I:I).EQ.SPACE) THEN
            NSTART = NSTART + 1
          ELSE
            GO TO 20
          END IF
C
C
   10   CONTINUE
C
C---- Find length of word
C
   20   LENGTH = INDEX(LINHLP(NSTART:NCHARS),' ') - 1
        IF (LENGTH.LE.0) LENGTH = LEN(LINHLP(NSTART:NCHARS))
        KLEN = LENGTH
        IF (KLEN.GT.MAXLEN) KLEN = MAXLEN
C
C---- Copy word to aray
C
        OPTIOS(J) = LINHLP(NSTART:KLEN+NSTART-1)
        NSTART = NSTART + LENGTH
        IF (NSTART.GE.NCHARS) GO TO 40
   30 CONTINUE
C
C
      GO TO 50
   40 NOPTS = J
   50 RETURN
C
C
      END
C
C
C
      SUBROUTINE GETHLP (HLPFIL,HLPSTR)
C     =================================
C
C---- using file HLPFIL get help using HLPSTR (VAX style help)
C     It can be called from within a Keyworded program. Simply
C     pass the help line and the filename of the help file to call
C
C     .. Parameters ..
      INTEGER NPARM
      PARAMETER (NPARM=200)
C
C     .. Arguments ..
      CHARACTER*(*) HLPFIL
      CHARACTER*(*) HLPSTR
C     ..
      INTEGER IBOTOM,ITOP,MAXLEV,LKIM,I,NWORDS
      CHARACTER WORDS(NPARM)*12,KEY*4
      REAL FVALUE(NPARM)
      INTEGER IBEG(NPARM),IDEC(NPARM),IEND(NPARM),ITYP(NPARM)
      CHARACTER CVALUE(NPARM)*4
      INTEGER NTOK
C
C     .. Commons ..
      INTEGER IHELP,LUNIN,LUNOUT,NSCREN
      LOGICAL VMSVAX,ABORT,HLPFIR,HLPFAL,HLPCAL
C
      COMMON /HINOUT/LUNIN,LUNOUT,IHELP,VMSVAX,NSCREN,HLPFAL,HLPCAL
      COMMON /PGSTAT/ ABORT 
C
      SAVE
C     .. External subroutines ..
      INTEGER LENSTR
      LOGICAL VAXVMS
      EXTERNAL PARSE,HLPOPN,LENSTR,VAXVMS
C     ..
      DATA HLPFIR/.TRUE./
C
C
             ABORT = .FALSE.
C
             NSCREN = 24
C                 ******
             CALL HLPSET
C                 ******
C
C---- open help file if first pass
C
       IF (HLPFIR) THEN
             HLPCAL = .TRUE.
             HLPFIR = .FALSE.
             IHELP =  90
             LUNOUT = 6
             LUNIN = 5
             VMSVAX = .FALSE.
C
C                *********
             IF (VAXVMS()) VMSVAX = .TRUE.
C                *********
C
C                *************
            CALL HLPOPN(HLPFIL)
C                *************
C
C
C---- Return on open failure
C
            IF (HLPFAL) THEN
C
C                    ******
                CALL HLPEND
C                    ******
C
                HLPFIR = .TRUE.
C
C                     ******
                 CALL HLPRET
C                     ******
C
                RETURN
            END IF
C
C
        END IF
C
C---- Make sure HLPSTR has terminating space
C
      LKIM = LENSTR(HLPSTR)
      HLPSTR(1:LKIM+1) = HLPSTR(1:LKIM) // ' '
      LKIM = LKIM + 1
C
C
      NTOK = -NPARM
      KEY = ' '
C
C---- Parse it
C
C          *************************************************
      CALL PARSE(HLPSTR(1:LKIM),IBEG,IEND,ITYP,FVALUE,CVALUE,
     +            IDEC,NTOK)
C          *************************************************
C
      IF (NTOK.GT.NPARM) NTOK = NPARM
C
C---- Set up the list of words for help to search through
C
      DO 100 I=2,NTOK
        WORDS(I-1) = HLPSTR(IBEG(I):IEND(I))
100   CONTINUE
C
C---- Set up count of number of words
C
      NWORDS = NTOK - 1
C
C---- Call Help Routines
C
C
C PURPOSE: Read help library generated with HLPGEN
C 
C
C
C IBOTOM : Bottom line of 'display' area
C ITOP   : Top line of 'display' area
C NWORDS : Number of words entered on command line
C MAXLEV : Maximum number of levels allowed to help file
C WORDS  : Words entered on command line
C
C
      ITOP = 1
      IBOTOM = NSCREN - 2
      MAXLEV = 6
C
C---- Clear and set up screen
C
C          ******
      CALL CLRSCR
C          ******
C
C---- Read help file
C
C          ***************************************
      CALL RDHELP(WORDS,NWORDS,ITOP,IBOTOM,MAXLEV)
C          ***************************************
C
C          ******
      CALL HLPRET
C          ******
C
C
      RETURN
C
C
      END
C
C
C
        SUBROUTINE HLPEND
C       =================
C
C
      CHARACTER TRMNAM*80,LOGTRM*80,SPACE*1,TRMWRK*4
      INTEGER LUNIN,LUNOUT,IHELP,NSCREN
      LOGICAL VMSVAX,HLPFAL,HLPCAL
C     .. Common blocks ..
      COMMON /HINOUT/LUNIN,LUNOUT,IHELP,VMSVAX,NSCREN,HLPFAL,HLPCAL
C
      SAVE
C
C--- You have to call hlpend from the main program
C    after all possible calls to ccphlp, check here if ever used
C
        IF (.NOT.HLPCAL) RETURN
C
C---- Finish
C
C          *************
      CALL CLRSCR
      CALL SCROLR(1,NSCREN)
      CALL CURSLC(NSCREN,1)
C          *************
C
C---- Check if "xterm"
C     and reset scrolling region = default of full size of window
C
      TRMNAM = ' '
C
C---- NOTE on ESV machines the variable TERM = xterm
C                                              once X running
C                          BUT variable term = esconsole
C
      LOGTRM = 'TERM'
C
C           *********************
      CALL  UGTENV(LOGTRM,TRMNAM)
C           *********************
C
       IF (TRMNAM.EQ.'xterm') THEN
C
C---- Set scrolling space to full default size of window
C
           SPACE = ' '
           TRMWRK(1:1) = CHAR(27)
           TRMWRK(2:2) = '['
           TRMWRK(3:3) = ';'
           TRMWRK(4:4) = 'r'
C
C               ********************
           CALL HLPSED(TRMWRK,SPACE)
C               ********************
C
      END IF
C
C
        IF (HLPFAL) CLOSE (UNIT=IHELP)
C
C                  ******
              CALL RESCUR
C                  ******
C
C
        END 

C
C
C
      SUBROUTINE HLPERR(IERR)
C     =======================
C
C
C PURPOSE: Print I/O error code and message issued by IOSTAT=IERR
C          Also contains error codes and messages for RUNTIME ERRORS
C          including SYSTEM detected errors, Arithmetic or I/O errors.
C
C IERR  : Error code
C
C     .. Scalar Arguments ..
      INTEGER IERR
C     ..
C     .. Scalars in Common ..
      INTEGER IHELP,LUNIN,LUNOUT,NSCREN
      LOGICAL VMSVAX,HLPFAL,HLPCAL
C     ..
C     .. Local Scalars ..
      INTEGER LENMES
      CHARACTER ERRMES*20
C     ..
C     .. Local Arrays ..
      CHARACTER MESSAG(150)*200
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. Common blocks ..
      COMMON /HINOUT/LUNIN,LUNOUT,IHELP,VMSVAX,NSCREN,HLPFAL,HLPCAL
C     ..
      SAVE
C     .. Data statements ..
C
      DATA ERRMES/'!!! ERROR '/
      DATA (MESSAG(JDO),JDO=1,30)/' Not Owner',
     +     ' No such file or Directory',' No such Process',
     +     ' Interrupted system call',' I/O error in read or write',
     +     ' No such device or address',' Arg list  too long',
     +     ' Exec format error',' Bad file number',' No children',
     +     ' No more processes',' Not enough core',' Permission denied',
     +     ' Bad address',' Block device required',' Mount device busy',
     +     ' File exists',' Cros-device link',' No such device',
     +     ' Not a directory',' Is a directory',' Invalid argument',
     +     ' File Table Overflow',' Too many open files',
     +     ' Not a typewriter',' Text file busy (allocated)',
     +     ' File too large',' No space left on device',' Illegal seek',
     +     ' Read-only file system'/
      DATA (MESSAG(JDO),JDO=31,62)/32*' '/
      DATA MESSAG(63)/' File name too long'/
      DATA (MESSAG(JDO),JDO=64,99)/36*' '/
      DATA (MESSAG(JDO),JDO=100,120)/' Error in format',
     +     ' Illegal unit number',' Formatted I/O not allowed',
     +     ' Unformatted I/O not allowed',' Direct I/O not allowed',
     +     ' Sequential I/O not allowed',' Can not backspace file',
     +     ' Off beginning of record',' Can not "stat" file',
     +     ' No * after repeat count',' Off end of record',
     +     ' Truncation failed',' Incoprehensible list input',
     +     ' Out of free space',' Unit not connected ',
     +     ' Read unexpected character',' Blank logical input field',
     +     ' "New" file exists',' Can not find "old" file',
     +   ' Unknown system error !!! Contact Technical Assistance Center'
     +     ,' Requires seek ability'/
      DATA (MESSAG(JDO),JDO=121,140)/' Illegal argument',
     +     ' Negative repeat count',' Illegal operation for unit',
     +     ' New record not allowed',
     +     ' Numeric keyword variable overflowed',
     +     ' Record number out of range',' File is READONLY',
     +     ' Variable record format not allowed',
     +     ' Exceeded record length',
     +     ' Exceeds maximum number of open files',
     +     ' data type size too small for REAL',
     +     ' Infinite loop in FORMAT',
     +     ' Fixed-record type not allowed for PRINT files',
     +     ' Attempt to read nonexistent record',
     +     ' Reopening file with different unit',
     +     ' I/O list item incompatible with format code',
     +     ' Unknown record length',
     +     ' Asynchronous I/O not allowed on this file',
     +     ' Synchronous I/O not allowed on this file',
     +     ' Incompatible format structure. Recompile'/
      DATA (MESSAG(JDO),JDO=141,145)/' Namelist error',
     +     ' Apparanet recursive logical name definition',
     +     ' Recursive Input/Output operation',
     +   ' Out of space, poss from performing unform I/O on format file'
     +     ,' Error in conversion of string to numeric'/
C     ..
C
      IF (IERR.GT.0 .AND. IERR.LE.145) THEN
        LENMES = LENSTR(MESSAG(IERR))
        WRITE (LUNOUT,FMT=6000) ERRMES,IERR,MESSAG(IERR) (1:LENMES)
      ELSE
        WRITE (LUNOUT,FMT=*) '!!! Failure code ',IERR,' not documented'
      END IF
C
C---- Format statements
C
 6000 FORMAT (1X,A,I6,A)
C
C
      END
C
C
       SUBROUTINE HLPOPN(HLPFIL)
C      =========================
C
C
C
      CHARACTER*(*) HLPFIL
C
C
      CHARACTER FULNAM*400,HANDLE*5,HLPDIR*255,HLPWRK*80
      INTEGER LUNIN,LUNOUT,IHELP,NSCREN,LRECL,IBYTEV,IFAIL
      LOGICAL VMSVAX,HLPFAL,HLPCAL
C     .. Common blocks ..
      COMMON /HINOUT/LUNIN,LUNOUT,IHELP,VMSVAX,NSCREN,HLPFAL,HLPCAL
C
C
      EXTERNAL UGTENV,UBYTES
      INTEGER LENSTR
      EXTERNAL LENSTR
C
      SAVE
C
C---- LRECL  : Record length of help file
C
      LRECL = 80
C
C---- Set up Filename of binary help file
C
      HLPDIR = ' '
      HLPWRK = 'CCP4_HELPDIR'
C
C          ***************************************
      CALL UGTENV(HLPWRK(1:LENSTR(HLPWRK)),HLPDIR)
C          ***************************************
C
      IF (LENSTR(HLPDIR).EQ.0) THEN
        WRITE (LUNOUT,*) 
     +     ' Warning: cannot translate variable CCP4_HELPDIR',
     +     ' Using current directory **'
        FULNAM = HLPFIL(1:LENSTR(HLPFIL))  
      ELSE
        FULNAM = HLPDIR(1:LENSTR(HLPDIR)) // 
     +           HLPFIL(1:LENSTR(HLPFIL)) 
      END IF
C
C---- Open help file
C
C          ********************
      CALL UBYTES(IBYTEV,HANDLE)
C          ********************
C
        lrecl = lrecl * ibytev
      IF (HANDLE.EQ.'WORDS') LRECL = LRECL/IBYTEV
C
           write(6,8877) FULNAM(1:LENSTR(FULNAM))
8877       format(' fulnam: ',a)
      OPEN (UNIT=IHELP,
     +     FILE=FULNAM(1:LENSTR(FULNAM)),
     +     STATUS='OLD',
     +     ACCESS='DIRECT',
     +     RECL=LRECL,
     +     FORM='UNFORMATTED',
     +     ERR=160,
     +     IOSTAT=IFAIL)
C
C
      RETURN
C
C---- Report errors
C
  160 CONTINUE
C
C          ****************
      CALL CLRSCR
      CALL SCROLR(1,NSCREN)
      CALL CURSLC(NSCREN,1)
      CALL REVVID
C          ****************
C
      WRITE (LUNOUT,FMT=6008) FULNAM(1:LENSTR(FULNAM))
 6008 FORMAT (' !!! Error opening help file ',A)
C
C          ************
      CALL HLPERR(IFAIL)
C          ************
C
      WRITE (LUNOUT,FMT=6666) 
 6666 FORMAT(' >>> Press RETURN to continue',$)
      READ (LUNIN,FMT='(A)')
C
C          ******
      CALL NORVID
C          ******
C
       HLPFAL = .TRUE.
C
       RETURN
       END
C
C
C
      SUBROUTINE HLPPAG(IBOTOM,NPRINT,EFLAG,EXTRAS,NEXTRA)
C     ====================================================
C
C
C PURPOSE: Prompt for continue or abort
C =======
C
C called by: LSTHLP
C
C IBOTOM : Bottom line of scroll region
C NPRINT : Keeps account of number of lines printed
C EFLAG  : Set .true. if <control e> entered
C
C
C CONTLA : <CONTROL-A>
C CONTLD : <CONTROL-D>
C CONTLE : <CONTROL-E>
C ICH    : Character entered on keyboard
C
C
C     .. Scalar Arguments ..
      INTEGER IBOTOM,NEXTRA,NPRINT
      LOGICAL EFLAG
C     ..
C     .. Array Arguments ..
      CHARACTER EXTRAS(5)*12
C     ..
C     .. Scalars in Common ..
      INTEGER IHELP,LUNIN,LUNOUT,NSCREN
      LOGICAL ABORT,VMSVAX,HLPFAL,HLPCAL
C     ..
C     .. Local Scalars ..
      INTEGER CONTLA,CONTLD,CONTLE,IRET,NCHARS
      CHARACTER LINHLP*60,DOLLAR*1,HLPWRK*255,SPACE*1
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL CLREOL,CURSLC,FNDWRD
C     ..
C     .. Common blocks ..
      COMMON /HINOUT/LUNIN,LUNOUT,IHELP,VMSVAX,NSCREN,HLPFAL,HLPCAL
      COMMON /PGSTAT/ABORT
C     ..
      SAVE
C
C
      CONTLA = 1
      CONTLD = 4
      CONTLE = 5
      DOLLAR = '$'
      HLPWRK = ' Press <RETURN> to continue: '
      SPACE = ' '
C
C---- Prompt for response at bottom of screen
C
C          ***************************************
      CALL CURSLC(NSCREN,1)
      CALL CLREOL
      CALL HLPSED (HLPWRK(1:LENSTR(HLPWRK)),SPACE)
cc                                          DOLLAR)
C          ***************************************
C
C---- Get reply
C
      NCHARS = 0
      IRET = 1
      NEXTRA = 0
      NCHARS = 0
      LINHLP = ' '
      IRET = 1
C
      READ (LUNIN,FMT=6000) LINHLP
      NCHARS = LENSTR(LINHLP)
      IF (NCHARS.EQ.0) IRET = 0
      IF (.NOT.ABORT) THEN
        IF (IRET.EQ.2) EFLAG = .TRUE.         
C
C                        ****************************************
        IF (NCHARS.NE.0) CALL FNDWRD(LINHLP,NCHARS,EXTRAS,NEXTRA,5)
C                        ****************************************
C
C---- Clear prompt and set cursor at bottom of scroll region
C
C            *****************
        CALL CURSLC(NSCREN,1)
        CALL CLREOL
        CALL CURSLC(IBOTOM,1)
C            *****************
C
C---- Scroll a line and reset nprint
C
        NPRINT = 0
        RETURN
      END IF
C
C---- Format statements
C
 6000 FORMAT (A)
C
C
      END
C
C
C
        SUBROUTINE HLPRET
C       =================
C
C
      CHARACTER TRMNAM*80,LOGTRM*80,SPACE*1,TRMWRK*4
      INTEGER LUNIN,LUNOUT,IHELP,NSCREN
      LOGICAL VMSVAX,HLPFAL,HLPCAL
C     .. Common blocks ..
      COMMON /HINOUT/LUNIN,LUNOUT,IHELP,VMSVAX,NSCREN,HLPFAL,HLPCAL
C
      SAVE
C
C---- Finish
C
C          *************
      CALL CLRSCR
      CALL SCROLR(1,NSCREN)
      CALL CURSLC(NSCREN,1)
      CALL RESCUR
C          *************
C
C---- Check if "xterm"
C     and reset scrolling region = default of full size of window
C
      TRMNAM = ' '
C
C---- NOTE on ESV machines the variable TERM = xterm
C                                              once X running
C                          BUT variable term = esconsole
C
      LOGTRM = 'TERM'
C
C           *********************
      CALL  UGTENV(LOGTRM,TRMNAM)
C           *********************
C
       IF (TRMNAM.EQ.'xterm') THEN
C
C---- Set scrolling space to full default size of window
C
           SPACE = ' '
           TRMWRK(1:1) = CHAR(27)
           TRMWRK(2:2) = '['
           TRMWRK(3:3) = ';'
           TRMWRK(4:4) = 'r'
C
C               ********************
           CALL HLPSED(TRMWRK,SPACE)
C               ********************
C
      END IF
C
C
        END 
C
C
C
      SUBROUTINE HLPSED(OUTSTR,FORM)
C     ============================
C
C
C---- Write character string to graphics screen on a terminal
C
C
C Parameters
C
C      STR (I)  Character string or substring to be sent. the
C               number of characters sent will be equal to the
C               length of the string or substring. The following
C               codes are recognised:
C               If first character is a + then no cr/lf is sent 
C               before string. 
C               If last character is a $ then no cr/lf is sent 
C               after string. 
C
C---- Send string
C
C     .. Scalar Arguments ..
      CHARACTER OUTSTR* (*),FORM*1
C     ..
C     .. Scalars in Common ..
      INTEGER IHELP,LUNIN,LUNOUT,NSCREN
      LOGICAL VMSVAX,HLPFAL,HLPCAL
C     ..
C     .. Common blocks ..
      COMMON /HINOUT/LUNIN,LUNOUT,IHELP,VMSVAX,NSCREN,HLPFAL,HLPCAL
C     ..
      SAVE
C     .. External subroutines ..
       INTEGER LENSTR
       EXTERNAL LENSTR
C
      IF(FORM.EQ.'+') THEN
        WRITE(LINOUT,100) OUTSTR(1:LENSTR(OUTSTR))
      ELSE IF (FORM.EQ.'$') THEN
        WRITE(LINOUT,200) OUTSTR(1:LENSTR(OUTSTR))
      ELSE IF (FORM.EQ.' ') THEN
        WRITE (LINOUT,300) OUTSTR(1:LENSTR(OUTSTR))
      END IF
C
C
 100  FORMAT('+',A)
 200  FORMAT(' ',A,$)
 300  FORMAT(' ',A)
C
C
      END
C
C
       SUBROUTINE HLPSET
C      =================
C
C
C
      CHARACTER FULNAM*400,LOGTRM*4,HANDLE*5,TRMNAM*80,
     +          HLPDIR*255,HLPWRK*80
      INTEGER LUNIN,LUNOUT,IHELP,NSCREN,LRECL,IBYTEV,IFAIL
      LOGICAL VMSVAX,HLPFAL,HLPCAL
C     .. Common blocks ..
      COMMON /HINOUT/LUNIN,LUNOUT,IHELP,VMSVAX,NSCREN,HLPFAL,HLPCAL
C
C
      EXTERNAL UGTENV,UBYTES,UCURSE
      INTEGER LENSTR
      EXTERNAL LENSTR
C
      SAVE
C
C             ******
         CALL SAVCUR
C             ******
C
C---- Check if xterm
C     and reset scrolling region = default of full size of window
C
      TRMNAM = ' '
C
C---- NOTE on ESV machines the variable TERM = xterm
C                                              once X running
C                          BUT variable term = esconsole
C
      LOGTRM = 'TERM'
C
C           *********************
      CALL  UGTENV(LOGTRM,TRMNAM)
C           *********************
C
       IF (TRMNAM.EQ.'xterm') THEN
C
C
C                  ********************
              CALL UCURSE (IROWS,ICOLS)
C                  ********************
C
                NSCREN = IROWS
C
        END IF
C
C
      RETURN
C
C
       END
C
C
      SUBROUTINE LSTHLP(ITOP,IBOTOM,NREC,LEVEL,LOWER,EFLAG,PARENT,
     +                  EXTRAS,NEXTRA)
C     ==============================================================
C
C
C PURPOSE: List help text on screen with paging
C ========
C
C Called by: RDHELP
C
C
C ITOP   : Top line of screen to use
C IBOTOM : Bottom line of screen to use
C NREC   : Record number to read (not including header)
C LEVEL  : 'level' of text read
C LOWER  : Set if there are lower level options for text displayed
C LSTALL : List all text about a particular topic
C WHOLE  : If .true. display all help text
C EFLAG  : <control-e> was entered (try to go a level)
C
C
C
C NCOUNT : Keeps account of the record number in the help file
C NPRINT : Keeps account of the number of lines displayed.
C NLINES : Number of lines in the scroll region
C SPACE  : ' ' character
C LINHLP : String read from file
C MARKER : Indicates 'end of header' in help file
C ENDTXT : Indicates end of text in help file (inserted by hlpgen)
C
C     .. Scalar Arguments ..
      INTEGER IBOTOM,ITOP,LEVEL,NEXTRA,NREC
      LOGICAL EFLAG,LOWER
C     ..
C     .. Array Arguments ..
      INTEGER PARENT(6)
      CHARACTER EXTRAS(5)*12
C     ..
C     .. Scalars in Common ..
      INTEGER IHELP,LUNIN,LUNOUT,NSCREN
      LOGICAL ABORT,VMSVAX,HLPFAL,HLPCAL
C     ..
C     .. Local Scalars ..
      INTEGER HEADC,NCOUNT,NEWLEV,NLINES,NPRINT,LEND,LBEG,LKIM
      CHARACTER SPACE*1,ENDTXT*80,LINHLP*80,
     +          MARKER*80,OUTLIN*100
C     ..
C     .. External Subroutines ..
      INTEGER LENSTR
      EXTERNAL CLRSCR,CURSLC,NORVID,HLPPAG,REVVID,LENSTR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ICHAR
C     ..
C     .. Common blocks ..
      COMMON /HINOUT/LUNIN,LUNOUT,IHELP,VMSVAX,NSCREN,HLPFAL,HLPCAL
      COMMON /PGSTAT/ABORT
C     ..
      SAVE
C
C
C---- Initialise
C
      SPACE = ' '
      MARKER = 'ZZZZZZZZZZZZ'
      ENDTXT = '0ENDENDEND'
      LOWER = .TRUE.
      EFLAG = .FALSE.
      NCOUNT = 0
      NPRINT = 0
      NEXTRA = 0
      NLINES = IBOTOM - ITOP
C
C          **************
      CALL CLRSCR
      CALL CURSLC(ITOP,1)
C          **************
C
   10 CONTINUE
C
C---- Skip header
C
      NCOUNT = NCOUNT + 1
      HEADC = NCOUNT
      READ (IHELP,REC=NCOUNT,ERR=60) LINHLP
      IF (LINHLP.NE.MARKER) GO TO 10
      NCOUNT = NCOUNT + NREC
C
C---- Get level number of text to be displayed and write name
C
      READ (IHELP,REC=NCOUNT,ERR=60) LINHLP
      LEVEL = ICHAR(LINHLP(1:1)) - 48
C
C
      IF (LEVEL.GE.0) THEN
        LINHLP(1:1) = SPACE
        WRITE (LUNOUT,FMT=6006) LINHLP(1:LENSTR(LINHLP))
        NPRINT = NPRINT + 1
C
C                             *******************************
        IF (NPRINT.EQ.NLINES) CALL HLPPAG(IBOTOM,NPRINT,EFLAG,
     +                                    EXTRAS,NEXTRA)
C                             *******************************
C
        IF (ABORT .OR. EFLAG .OR. NEXTRA.NE.0) THEN
          RETURN
        ELSE
   20     CONTINUE
C
C---- Display text until next topic name is found (unless (whole))
C
          NCOUNT = NCOUNT + 1
          READ (IHELP,REC=NCOUNT,ERR=60) LINHLP
C
C
          IF (LINHLP(1:1).EQ.SPACE) THEN
            IF (LINHLP.EQ.ENDTXT) THEN
              RETURN
            ELSE
              WRITE (LUNOUT,FMT='(A)') LINHLP(1:LENSTR(LINHLP))
              NPRINT = NPRINT + 1
C
C                                   **************************
              IF (NPRINT.EQ.NLINES) CALL HLPPAG(IBOTOM,NPRINT,
     +                                   EFLAG,EXTRAS,NEXTRA)
C                                   **************************
C
              IF (ABORT .OR. EFLAG .OR. NEXTRA.NE.0) THEN
                RETURN
              ELSE
                GO TO 20
              END IF
            END IF
          END IF
C
C---- Is next topic lower level or same level ?
C
          NEWLEV = ICHAR(LINHLP(1:1)) - 48
          IF (NEWLEV.LE.LEVEL) LOWER = .FALSE.
C
C
          IF (LOWER) THEN
C
C---- Display lower level topic names (in groups of 3 per line)
C
            WRITE (LUNOUT,FMT=6000)
            NPRINT = NPRINT + 1
C
C                                 **************************
            IF (NPRINT.EQ.NLINES) CALL HLPPAG(IBOTOM,NPRINT,
     +                                 EFLAG,EXTRAS,NEXTRA)
C                                 **************************
C
            IF (ABORT .OR. EFLAG .OR. NEXTRA.NE.0) THEN
              RETURN
            ELSE
              NCOUNT = NCOUNT - 1
   30         CONTINUE
C
C
                OUTLIN = ' '
                LEND = 0
                LBEG = 0
C
C
   40           CONTINUE
                NCOUNT = NCOUNT + 1
                READ (IHELP,REC=NCOUNT,ERR=60) LINHLP
C
C
                IF (ICHAR(LINHLP(1:1)).EQ.32) THEN
                  GO TO 40
                ELSE IF (ICHAR(LINHLP(1:1))-48.LT.NEWLEV) THEN
                    LKIM = LENSTR(OUTLIN)
C
C                                  *********************************
                    IF (LKIM.NE.0) CALL HLPSED(OUTLIN(1:LKIM),SPACE)
C                                  *********************************
C

                  RETURN
                ELSE IF (ICHAR(LINHLP(1:1))-48.GT.NEWLEV) THEN
                  GO TO 40
                END IF
C
C
                LBEG = LEND + 1
                LEND = LEND + 10
                WRITE (OUTLIN(LBEG:LEND),FMT=6767) LINHLP(2:10)
                LKIM = LENSTR(OUTLIN)
C
C
                IF(LKIM.GE.59) THEN
C
C                    ****************************
                CALL HLPSED(OUTLIN(1:LKIM),SPACE)
C                    ****************************
C
                GO TO 50
                ELSE
                GOTO 40
                END IF
C
C
   50         CONTINUE
              NPRINT = NPRINT + 1
C
C                                   **************************
              IF (NPRINT.EQ.NLINES) CALL HLPPAG(IBOTOM,NPRINT,
     +                                   EFLAG,EXTRAS,NEXTRA)
C                                   **************************
C
              IF (ABORT .OR. EFLAG .OR. NEXTRA.NE.0) THEN
                RETURN
              ELSE
                GO TO 30
              END IF
C
C
            END IF
          ELSE
            RETURN
          END IF
        END IF
      END IF
C
C---- Report errors
C
   60 CONTINUE
C
C          ******
      CALL REVVID
C          ******
C
      WRITE (LUNOUT,FMT=6004)
C
C          ******
      CALL NORVID
C          ******
C
C---- Format statements
C
 6000 FORMAT ('  Further information is available on these topics...',/)
 6022 FORMAT(2X)
 6004 FORMAT ('!!! ERROR READING HELP FILE')
 6006 FORMAT (A)
 6767 FORMAT(' ',A9)
C
C
      END
C
C
C
      SUBROUTINE NORVID
C     =================
C
C PURPOSE: Set character mode to normal video. -- light text on dark
C          background
C
C
C---- ESC : ascii code for escape character.
C
C     .. Local Scalars ..
      CHARACTER OUTSTR*4,SPACE*1
C     ..
C     .. External Subroutines ..
      EXTERNAL HLPSED
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC CHAR
C     ..
C
      SPACE = ' '
C
      OUTSTR(1:1) = CHAR(27)
      OUTSTR(2:2) = '['
      OUTSTR(3:3) = '0'
      OUTSTR(4:4) = 'm'
C
C          *******************
      CALL HLPSED(OUTSTR,SPACE)
C          *******************
C
C
      END
C
C
C
      SUBROUTINE RDHELP(WORDS,NWORDS,ITOP,IBOTOM,MAXLEV)
C     ==================================================
C
C
C
C PURPOSE: Initiate read help file
C =======
C
C
C WORDS  : Words entered in command line
C NWORDS : Number of words in command line
C ITOP   : Top line of scroll region
C IBOTOM : Bottom line of scroll region
C MAXLEV : Maximum number of levels allowed
C
C
C OPTIOS : Options selected by user
C STAR   : * Character
C QUEST  : ? Character
C LEVEL  : 'LEVEL' of option in file (= number in column 1 of help file)
C NCHAR  : Number of chars. in each word typed by user
C NREC   : Record number of help text in file
C NCOUNT : Keeps account of record numbers read
C NOPTS  : Number of options selected by user
C LOWER  : Set .true. if lower levels for option selected
C LSTALL : List all text on a particular topic
C WHOLE  : If .true. list all help text
C EFLAG  : <control-e> was entered
C MARKER : 'end of header' marker in help file
C OPTION : Options read from help file header
C
C
C
C     .. Scalar Arguments ..
      INTEGER IBOTOM,ITOP,MAXLEV,NWORDS
C     ..
C     .. Array Arguments ..
      CHARACTER WORDS(200)*12
C     ..
C     .. Scalars in Common ..
      INTEGER IHELP,LUNIN,LUNOUT,NSCREN
      LOGICAL ABORT,VMSVAX,HLPFAL,HLPCAL
C     ..
C     .. Local Scalars ..
      INTEGER I,IRET,J,LEVEL,NCHARS,NCOUNT
      INTEGER NEXTRA,NOPTS,NREC
      LOGICAL EFLAG,LOWER
      CHARACTER MARKER*12,QUEST*12,STAR*12,FORM*35,
     +          OUTLIN*100,LINHLP*80
C     ..
C     .. Local Arrays ..
      INTEGER NCHAR(6),PARENT(6)
      CHARACTER EXTRAS(5)*12,OPTION(6)*12,OPTIOS(31)*12,SAVOPT(6)*12,
     +          DOLLAR*1,HLPWRK*255
C     ..                  
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPUPC,CLREOL,CLRSCR,CURSLC,FNDWRD,HLPERR,
     +         LSTHLP,NORVID,REVVID,SCROLR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX,LEN
C     ..
C     .. Common blocks ..
      COMMON /HINOUT/LUNIN,LUNOUT,IHELP,VMSVAX,NSCREN,HLPFAL,HLPCAL
      COMMON /PGSTAT/ABORT
C
C     .. Save Statement ..
      SAVE
C     ..
C
      MARKER = 'ZZZZZZZZZZZZ'
      STAR = '*           '
      QUEST = '?           '
      DOLLAR = '$'
      HLPWRK = ' >>> Enter option : '
C
C---- Initialise screen
C
C          *******************
      CALL SCROLR(ITOP,IBOTOM)
      CALL CLRSCR
C          *******************
C
10    CONTINUE
C
C
C---- If no options specified list intro. to help. if * requested
C     list either whole file or all on specified topic.
C
      NREC = 1
      IRET = 1
C
C
      IF (NWORDS.GT.0) THEN
        IF (WORDS(1).EQ.QUEST) THEN
          NWORDS = 0
          NREC = PARENT(LEVEL)
        ELSE
C
C---- Find length of words
C
          DO 20 I = 1,NWORDS
C
C                ****************
            CALL CCPUPC(WORDS(I))
C                ****************
C
            NCHAR(I) = INDEX(WORDS(I),' ') - 1
            IF (NCHAR(I).LE.0) NCHAR(I) = LEN(WORDS(I))
   20     CONTINUE
C
C---- Search for match
C
          DO 30 I = 1,6
            PARENT(I) = 0
   30     CONTINUE
C
C
          NCOUNT = 0
   40     CONTINUE
          NCOUNT = NCOUNT + 1
          READ (IHELP,REC=NCOUNT) (OPTION(I),I=1,6),NREC
C
C
          DO 50 I = 1,6
C
C                *****************
            CALL CCPUPC(OPTION(I))
C                *****************
C
   50     CONTINUE
C
C
          IF (OPTION(1).NE.MARKER) THEN
C
C
            DO 60 J = 1,NWORDS
              IF (WORDS(J) (1:LENSTR(WORDS(J))).NE.
     +            OPTION(J) (1:LENSTR(WORDS(J)))) THEN
                GO TO 40
              ELSE IF (PARENT(J).EQ.0) THEN
                PARENT(J) = NREC
              END IF
C
C
   60       CONTINUE
C
C
            GO TO 80
          END IF
C
C---- Failed to match
C
C              ************
          CALL CURSLC(23,1)
          CALL CLREOL
C              ************
C
          WRITE (LUNOUT,FMT=6002) (WORDS(I),I=1,NWORDS)
C
C
          DO 70 I = 1,6
            OPTION(I) = SAVOPT(I)
   70     CONTINUE
C
C
          NWORDS = NWORDS - 1
          LEVEL = LEVEL - 1
          GO TO 110
        END IF
C
C
      END IF
C
C---- Matched --- display text
C
   80 CONTINUE
C
C
      DO 90 I = 1,6
        SAVOPT(I) = OPTION(I)
   90 CONTINUE
C
C          *******************************************************
      CALL LSTHLP(ITOP,IBOTOM,NREC,LEVEL,LOWER,EFLAG,PARENT,EXTRAS,
     +            NEXTRA)
C          *******************************************************
C
      IF (ABORT) THEN
        GO TO 150
      ELSE
        IF (EFLAG) NWORDS = NWORDS - 1
        IF (NEXTRA.NE.0) THEN
C
C
          DO 100 I = 1,NEXTRA
            WORDS(LEVEL+1) = EXTRAS(I)
            LEVEL = LEVEL + 1
  100     CONTINUE
C
C
          NWORDS = NWORDS + NEXTRA
          NCOUNT = 0
          GO TO 10
        END IF
      END IF
C
C
  110 CONTINUE
C
C---- Prompt for option/suboption name
C
C          ****************
      CALL CURSLC(NSCREN,1)
      CALL CLREOL
C          *****************
C
      IF (.NOT.LOWER .OR. IRET.EQ.0 .OR. IRET.EQ.2) THEN
        NWORDS = NWORDS - 1
        LEVEL = LEVEL - 1
      END IF
C
C
      IF (LEVEL.LT.0) THEN
        GO TO 140
      ELSE
        IF (NWORDS.NE.0) THEN
C
C              ****************
          CALL CURSLC(NSCREN,1)
C              ****************
C
          I = LENSTR(OPTION(NWORDS))
C
C
          FORM = ' Enter suboption to '
          OUTLIN = FORM(1:LENSTR(FORM)) // '- ' //
     +                  OPTION(NWORDS)(1:I) //
     +                  ' : ...>> '
C
C                ***************************************
            CALL HLPSED(OUTLIN(1:LENSTR(OUTLIN)),DOLLAR)
C                ***************************************
C
        ELSE
C
C              **************************************
          CALL CURSLC(NSCREN,1)
          CALL HLPSED(HLPWRK(1:LENSTR(HLPWRK)),DOLLAR)
C              **************************************
C
          NWORDS = 0
          LEVEL = 0
        END IF
C
C
        NCHARS = 0
        LINHLP = ' '
        IRET = 1
C
        READ (LUNIN,FMT=6000) LINHLP
        NCHARS = LENSTR(LINHLP)
        IF (NCHARS.EQ.0) IRET = 0
C
C---- Clear any 'no help' message from last search
C
C            ****************
        CALL CURSLC(NSCREN,1)
        CALL CLREOL
C            ****************
C
C---- Exit or carry on ?
C
        IF (ABORT) THEN
          GO TO 130
        ELSE IF (IRET.EQ.0 .OR. IRET.EQ.2) THEN
          GO TO 110
C
C---- Decode line into words
C
        ELSE IF (LINHLP(1:1).EQ.'?') THEN
          GO TO 10
        ELSE
C
C              ***************************************
          CALL FNDWRD(LINHLP,NCHARS,OPTIOS,NOPTS,MAXLEV)
C              ***************************************
C
          IF (NOPTS.LE.0) GO TO 110
        END IF
      END IF
C
C
      DO 120 I = 1,NOPTS
        WORDS(LEVEL+1) = OPTIOS(I)
        LEVEL = LEVEL + 1
  120 CONTINUE
C
C
      NWORDS = NWORDS + NOPTS
C
C---- Back and read more
C
      NCOUNT = 0
      GO TO 10
  130 CONTINUE
      RETURN
  140 ABORT = .TRUE.
      RETURN
C
C---- Return to main program
C
  150 CONTINUE
      RETURN
C
C---- Format statements
C
 6000 FORMAT (A)
 6002 FORMAT (' === NO HELP FOR ',6 (A12,1X))
 6010 FORMAT (80 (' '))
C
C
      END
C
C
C
      SUBROUTINE RESCUR
C     ==================
C
C
C PURPOSE: Restore cursor position and its attributes (video, bold,
C          underscore & blink)
C
C
C--- ESC : ascii code for escape character.
C
C     .. Local Scalars ..
      CHARACTER OUTSTR*2,SPACE*1
C     ..
C     .. External Subroutines ..
      EXTERNAL HLPSED
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC CHAR
C     ..
C
      SPACE = ' '
C
      OUTSTR(1:1) = CHAR(27)
      OUTSTR(2:2) = '8'
C
C          *******************
      CALL HLPSED(OUTSTR,SPACE)
C          *******************
C
      END
C
C
C
      SUBROUTINE REVVID
C     ==================
C
C PURPOSE: Set character mode to reverse video -- dark text on light
C          background
C
C
C---- ESC : ascii code for escape character.
C
C     .. Local Scalars ..
      CHARACTER OUTSTR*4,SPACE*1
C     ..
C     .. External Subroutines ..
      EXTERNAL HLPSED
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC CHAR
C     ..
C
      SPACE = ' '
C
      OUTSTR(1:1) = CHAR(27)
      OUTSTR(2:2) = '['
      OUTSTR(3:3) = '7'
      OUTSTR(4:4) = 'm'
C
C          *******************
      CALL HLPSED(OUTSTR,SPACE)
C          *******************
C
      END
C
C
C
      SUBROUTINE SAVCUR
C     ==================
C
C PURPOSE: Save cursor position & its attributes (video, bold,
C          undescore & blink)
C
C
C
C---- ESC : ascii code for escape character.
C
C     .. Local Scalars ..
      CHARACTER OUTSTR*2,SPACE*1
C     ..
C     .. External Subroutines ..
      EXTERNAL HLPSED
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC CHAR
C     ..
C
      SPACE = ' '
C
      OUTSTR(1:1) = CHAR(27)
      OUTSTR(2:2) = '7'
C
C          *******************
      CALL HLPSED(OUTSTR,SPACE)
C          *******************
C
      END
C
C
C
      SUBROUTINE SCROLR(ITOP,IBOTOM)
C     ================================
C
C
C
C PURPOSE: Specify the region scrolled by the vt100.
C 
C
C ITOP   : Top line of scrolling region
C IBOTOM : Bottom line of scrolling region
C
C
C---- Arguments out of range exit
C
C     .. Scalar Arguments ..
      INTEGER IBOTOM,ITOP
C     ..
      INTEGER LUNIN,LUNOUT,IHELP,NSCREN
      LOGICAL VMSVAX,HLPFAL,HLPCAL
C     .. Common blocks ..
      COMMON /HINOUT/LUNIN,LUNOUT,IHELP,VMSVAX,NSCREN,HLPFAL,HLPCAL
C     ..
C     .. Local Scalars ..
      CHARACTER STOUT1*6,STOUT2*7,STOUT3*7,STOUT4*8,SPACE*1
C     ..
C     .. External Subroutines ..
      EXTERNAL HLPSED
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC CHAR
C     ..
      SAVE
C
      SPACE = ' '
C
      IF ((ITOP.GE.1) .AND. (ITOP.LE.NSCREN)) THEN
        IF ((IBOTOM.GE.1) .AND. (IBOTOM.LE.NSCREN)) THEN
          IF (IBOTOM.GT.ITOP) THEN
C
C---- Ansi escape to terminal (i2 format needed if lin/col >9)
C
            IF ((ITOP.LT.10) .AND. (IBOTOM.LT.10)) THEN
              STOUT1(1:1) = CHAR(27)
              STOUT1(2:2) = '['
              WRITE (STOUT1(3:3),FMT='(I1)') ITOP
              STOUT1(4:4) = ';'
              WRITE (STOUT1(5:5),FMT='(I1)') IBOTOM
              STOUT1(6:6) = 'r'
C
C                  *******************
              CALL HLPSED(STOUT1,SPACE)
C                  *******************
C
            ELSE IF ((ITOP.LT.10) .AND. (IBOTOM.GE.10)) THEN
              STOUT2(1:1) = CHAR(27)
              STOUT2(2:2) = '['
              WRITE (STOUT2(3:3),FMT='(I1)') ITOP
              STOUT2(4:4) = ';'
              WRITE (STOUT2(5:6),FMT='(I2)') IBOTOM
              STOUT2(7:7) = 'r'
C
C                  *******************
              CALL HLPSED(STOUT2,SPACE)
C                  *******************
C
            ELSE IF ((ITOP.GE.10) .AND. (IBOTOM.LT.10)) THEN
              STOUT3(1:1) = CHAR(27)
              STOUT3(2:2) = '['
              WRITE (STOUT3(3:4),FMT='(I2)') ITOP
              STOUT3(5:5) = ';'
              WRITE (STOUT3(6:6),FMT='(I1)') IBOTOM
              STOUT3(7:7) = 'r'
C
C                  *******************
              CALL HLPSED(STOUT3,SPACE)
C                  *******************
C
            ELSE
              STOUT4(1:1) = CHAR(27)
              STOUT4(2:2) = '['
              WRITE (STOUT4(3:4),FMT='(I2)') ITOP
              STOUT4(5:5) = ';'
              WRITE (STOUT4(6:7),FMT='(I2)') IBOTOM
              STOUT4(8:8) = 'r'
C
C                  *******************
              CALL HLPSED(STOUT4,SPACE)
C                  *******************
C
            END IF
          END IF
        END IF
      END IF
C
C
      END
