dnl *** This file is meant to be processed by m4 with an appropriate 
dnl     definition for the system type to produce unix.f
dnl
changequote([,])dnl * use [] as quotes rather than `'
dnl
dnl * units for record lengths:
ifelse(_sgi,1,
  [define(_ubytes,'WORDS')],
  [define(_ubytes,'BYTES')])dnl
dnl
dnl * fortran compiler may or may not accept READONLY specifier on OPEN:
ifelse(ifdef([_convex],1)ifdef([_sgi],1),1,
  [define(_readonly,[READONLY,])],
  [define(_readonly,)])dnl
dnl
dnl * fortran compiler may or may not accept CARRIAGECONTROL specifier on OPEN:
ifelse(ifdef([_convex],1)ifdef([_sgi],1),1,
  [define(_carriagecontrol,[CARRIAGECONTROL=CCNTRL,])],
  [define(_carriagecontrol,)])dnl
dnl
dnl * cometimes we can use IOINIT to specify carriagecontrol:
ifelse(_sun,1,
  [undefine(_has_carriagecontrol)define(_ioinit,1)])dnl
C
C  *** this file was automatically generated by configure
C  *** edit by hand only in extremis
C
C ========
C UNIX.FOR
C ========
C
C CCPERR - report error or normal termination and stop
C CCPOPN - open a file
C NOCRLF - write line supressing cr/lf to unit 6
C UBYTES - Returns number of bytes per word and 'words'/'bytes'
C          to indicate if byte handling is available
C UCPUTM - Get CPU time
C UGERR  - Get error explanation
C UGTENV - Get value of env. variable
C UGTIUD - Get user id - it's name
C UIDATE - Get date in 3 integer format
C UISATT - Is file a terminal?
C USTIME - Get absolute time in seconds (-1 for VMS)
C UTIME  - Get current time
C VAXVMS - Logical function returns TRUE if VAX/VMS
C
C  
C           ===============================
            SUBROUTINE CCPERR(ISTAT,ERRSTR)
C           ===============================
C
C
            CHARACTER ERRSTR*(*)
            INTEGER ISTAT
C
C
      WRITE(6,6000) ERRSTR(1:LENSTR(ERRSTR))
      CALL GETELAPSED
      CALL EXIT (ISTAT)
C
6000  FORMAT (' ',A)
            END
C
C
C
      SUBROUTINE CCPOPN(IIUN,LOGNAM,KSTAT,ITYPE,LREC,IFAIL)
C     ====================================================
C
C---- This subroutine is used to open a file
C
C     The requirement to specify that leading carriage control
C     characters in the output records should be obeyed (or not) can't
C     be implemented portably; likewise specifying readonly opening.
C     Some compilers accept VAXtran `carriagecontrol=' and `readonly'
C     specifiers; if so we use them.  Others have IOINIT, which can be
C     used to specify the carriage control.  The HPUX compiler is said
C     not to have any means of doing this, sigh.  [Unfortunately,
C     carriagecontrol specification isn't even defined in Fortan90,
C     although `ACTION="READ"' can be used.]
C
C PARAMETERS
C ==========
C
C        IIUN (I)   UNIT NUMBER
C      LOGNAM (I)   LOGICAL FILE NAME (UP TO 8 CHARACTERS)
C       KSTAT (I)   FILE STATUS FLAG =1, 'UNKNOWN'
C                                    =2, 'SCRATCH'
C                                    =3, 'OLD'
C                                    =4, 'NEW'
C                                    =5, 'READONLY'
C                                    =6, 'PRINTER'
C       ITYPE (I)   FILE TYPE FLAG =1, 'SEQUENTIAL' 'FORMATTED'
C                                  =2, 'SEQUENTIAL' 'UNFORMATTED'
C                                  =3, 'DIRECT'     'FORMATTED'
C                                  =4, 'DIRECT'     'UNFORMATTED'
C        LREC (I)   RECORD LENGTH FOR DIRECT ACCESS FILE (NO. OF
C                   CHARACTERS FOR A FORMATTED FILE OR WORDS FOR
C                   AN UNFORMATTED FILE). NOT RELEVANT FOR A SEQUENTIAL
C                   FILE
C       IFAIL (I/O) ON INPUT:     =0, STOP ON OPEN FAILURE
C                                 =1, CONTINUE AFTER OPEN FAILURE
C                   ON OUTPUT:    UNCHANGED IF FILE OPEN OK
C                                 =-1, ERROR IN OPENING FILE
C
C     .. Scalar Arguments ..
      INTEGER IFAIL,KSTAT,ITYPE,IIUN,LREC
      CHARACTER LOGNAM* (*)
C     ..
C     .. Local Scalars ..
      INTEGER LLREC,IUN,IBYTES,ISTAT
      CHARACTER CCNTRL*7,ST*7,FRM*12,ERRSTR*80,FULNAM*255,
     +     NAMFIL*255,HANDLE*5,OPNVAR*20, access*10
      INTEGER UNKNWN, SCRTCH, OLD, NEW, RDONLY, PRINTR
      PARAMETER (UNKNWN=1, SCRTCH=2, OLD=3, NEW=4, RDONLY=5, PRINTR=6)
ifdef(_ioinit[      LOGICAL JUNK])dnl
C     ..
C     .. Local Arrays ..
      CHARACTER STAT(6)*7
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      LOGICAL VAXVMS
ifdef(_ioinit,[
      LOGICAL IOINIT
      EXTERNAL IOINIT])dnl
      EXTERNAL LENSTR,VAXVMS
C     ..
C     .. External Subroutines ..
      EXTERNAL UGERR,UGTENV
C     ..
C     .. Data statements ..
      DATA STAT/'UNKNOWN','SCRATCH','OLD','NEW','OLD','UNKNOWN'/
C     ..
C     
      ISTAT = KSTAT
C     Negative unit number means don't give messages for successful open
      IUN = IIUN
      IF (IIUN.LT.0) IUN = -IIUN
C     Check args:
      IF (ISTAT.LT.1 .OR. ISTAT.GT.6 .OR. ITYPE.LT.1 .OR. ITYPE.GT.4)
     +     THEN 
        WRITE (6,'('' **CCPOPN ERROR** Invalid parameters in call'',/)')
        IF (IFAIL.EQ.0) THEN
          CALL CCPERR(1,'**CCPOPN ERROR** IN CCPLIB 6006')
        ELSE
          IFAIL = -1
        END IF
        RETURN
      ENDIF 
C
C     Do nothing for pre-connected units (what's the significance of
C     `TERM...'?) 
      IF (LOGNAM.EQ.'DATA' .OR. LOGNAM.EQ.'PRINTER' .OR.
     $     LOGNAM(:4).EQ.'TERM') RETURN
C
C     if environment variable CCP4_OPEN has value `UNKNOWN', open files
C     with status UNKNOWN rather than new if they exist
      IF (ISTAT.EQ.NEW) THEN
        OPNVAR = ' '
        CALL UGTENV('CCP4_OPEN',OPNVAR)
        IF (OPNVAR.EQ.'UNKNOWN') ISTAT = 1
      END IF
C
C     type of open
      ST = STAT(ISTAT)
      IF (ITYPE.EQ.2 .OR. ITYPE.EQ.4) THEN
        FRM = 'UNFORMATTED'
      ELSE
        FRM = 'FORMATTED'
      ENDIF 
      IF (ITYPE .EQ. 1 .OR. ITYPE.EQ.2) THEN
        ACCESS='SEQUENTIAL'
      ELSE
        ACCESS='DIRECT'
      ENDIF
C
C     check for `logical name' referencing real file
      CALL UGTENV(LOGNAM,NAMFIL)
      IF (NAMFIL.EQ.' ') NAMFIL = LOGNAM
C       
C     Special case:  /dev/null should be opened UNKNOWN
      IF ( NAMFIL.EQ.'/dev/null') ISTAT = 1
C     
      IF (access.eq.'DIRECT') THEN
C       Need to check is record length in words or bytes and set LLREC
C       accordingly. 
        CALL UBYTES (IBYTES,HANDLE)
        LLREC = LREC*IBYTES
        IF (HANDLE.EQ.'WORDS'.AND.ITYPE.EQ.4) LLREC=LLREC/IBYTES
        IF (ISTAT.EQ.RDONLY) THEN
C         _readonly may be defined as null or as `READONLY,'
          OPEN(UNIT=IUN,STATUS='UNKNOWN',ACCESS='DIRECT',FORM=FRM,
     +         _readonly
     +         FILE=NAMFIL,RECL=LLREC,IOSTAT=IOS,ERR=5)
        ELSE
          OPEN(UNIT=IUN,STATUS='UNKNOWN',ACCESS='DIRECT',FORM=FRM,
     +         FILE=NAMFIL,RECL=LLREC,IOSTAT=IOS,ERR=5)
        ENDIF 
      ELSE
C       if available, carriagecontrol='fortran' for print file, else = 
C       'list'.  we can use ioinit instead where it's available (see e.g.
C       Sun manual). 
        IF (ISTAT.EQ.PRINTR) THEN
C         want to obey format characters in column 1
          CCNTRL = 'FORTRAN'
          FRM = 'FORMATTED'
ifdef(_ioinit,
[      JUNK = IOINIT(.TRUE., .FALSE., .FALSE., ' ' , .FALSE.)
])dnl
        ELSE
C         no special significance to column 1
          CCNTRL = 'LIST'
ifdef(_ioinit,
[      JUNK = IOINIT(.FALSE., .FALSE., .FALSE., ' ' , .FALSE.)
])dnl
        END IF
        IF (FRM .EQ. 'UNFORMATTED') THEN
C         (carriage control not relevant)
          IF (ISTAT.EQ.RDONLY) THEN
            OPEN(UNIT=IUN, FILE=NAMFIL, STATUS=ST, ACCESS='SEQUENTIAL',
     +           _readonly
     +           FORM=FRM, ERR=5, IOSTAT=IOS)
          ELSE
            OPEN(UNIT=IUN, FILE=NAMFIL, STATUS=ST, ACCESS='SEQUENTIAL',
     +           FORM=FRM, ERR=5, IOSTAT=IOS)
          ENDIF
        ELSE
          IF (ISTAT.EQ.RDONLY) THEN
            OPEN(UNIT=IUN, FILE=NAMFIL, STATUS=ST, ACCESS='SEQUENTIAL',
     +           _readonly
     +           _carriagecontrol
     +           FORM=FRM, ERR=5, IOSTAT=IOS)
          ELSE
            OPEN(UNIT=IUN, FILE=NAMFIL, STATUS=ST, ACCESS='SEQUENTIAL',
     +           _carriagecontrol
     +           FORM=FRM, ERR=5, IOSTAT=IOS)
          ENDIF
        ENDIF
      ENDIF
C
C     Scratch files are immediately unlinked from the directory; they
C     become inaccessible only when closed, but don't appear in the
C     directory and the name can be re-used.
      IF (ISTAT.EQ.SCRTCH) CALL CUNLINK (NAMFIL)
C
C     Error check
 5    IF (IOS.NE.0) THEN
        CALL UGERR(IOS,ERRSTR)
        IF (IFAIL.EQ.0) THEN
C         hard failure
          WRITE (6,FMT=6002) IUN, NAMFIL(1:LENSTR(NAMFIL)), ST
 6002     FORMAT (' **CCPOPN ERROR** Unit:',I4,', File: ',A,
     +         ', Status: ',A,/)
          WRITE (6,'(1X,2A/)') 'System error: ',
     +         ERRSTR(1:LENSTR(ERRSTR))
          CALL CCPERR(1, '**CCPOPN ERROR** IN CCPLIB 6008')
        else
C         soft failure
          WRITE (6,FMT=6004) FRM, ST, IUN, 
     +         LOGNAM(1:LENSTR(LOGNAM)), NAMFIL(1:LENSTR(NAMFIL))
 6004     FORMAT (' **CCPOPN ERROR**  ',A,3X,A,
     +         ' file open failure on unit ',I3,/' Logical name: ',
     +         A,', ','File name: ',A,/)
          WRITE (6,'(1X,2A/)') 'System error: ',
     +         ERRSTR(1:LENSTR(ERRSTR))
          IFAIL = -1
          RETURN            
        ENDIF
      ELSE
        IF (IIUN.LE.0) RETURN 
        INQUIRE (FILE=NAMFIL,NAME=FULNAM)
C       DJGL: why is this inquire necessary rather than using NAMFIL?
        WRITE (6,FMT=6000) FRM,ST,IUN,LOGNAM(1:LENSTR(LOGNAM)),
     +       FULNAM(1:LENSTR(FULNAM))
 6000   FORMAT (/1X,A,3X,A,' file opened on unit ',I3,/
     +       ' Logical name: ',A,', Full name: ',A,/)
      ENDIF 
      END
C
C
C     =======================
      SUBROUTINE NOCRLF(LINE)
C     =======================
C
C---- Output a line supressing cr/lf. 
C     Alliant FX2800 and SGI do not use '+', change to ' '
C
C     .. Scalar Arguments ..
      CHARACTER LINE* (*)
C     ..
      WRITE (6,FMT=6000) LINE
C
 6000 FORMAT (' ',A,$)
      END
C
C
C     ==============================
      SUBROUTINE UBYTES(INUM,STRING)
C     ==============================
C
C UBYTES - Return statistics about byte handling
C
C Input:  none
C
C Output:    INUM - number of bytes per word
C            HANDLE - 'WORDS' or 'BYTES'
C            HANDLE - For unformatted files records are usually
C                     counted in 'BYTES', however both VAX and 
C                     SGI swap to 'WORDS' for this file type.
C
C Arguments: INTEGER     INUM
C            CHARACTER*5 HANDLE
C
C Usage:     CALL UBYTES (INUM,HANDLE)
C
C     .. Scalar Arguments ..
      INTEGER INUM
      CHARACTER STRING*5
C     ..
C
C
      INUM = 4
      STRING = _ubytes
C
      END
C
C
C     ======================
      SUBROUTINE UCPUTM(SEC)
C     ======================
C
C UCPUTM - Get CPU time
C
C Input:     SEC <= 0.0 to initialize timer, other value reads cpu time
C
C Output:    SEC
C
C Arguments: REAL    SEC,ELAPS
C
C Usage:     CALL UCPUTM(IFLAG,SEC)
C
C     .. Scalar Arguments ..
      REAL SEC
C     ..
C     .. Local Scalars ..
      LOGICAL IFLAG
C     ..
C     .. Local Arrays ..
      REAL TARRAY(2)
C     ..
C     .. Save statement ..
      SAVE IFLAG
C     ..
C     .. External Functions ..
      REAL DTIME
      EXTERNAL DTIME
C     ..
C     .. Data statements ..
      DATA IFLAG/.TRUE./
C     ..
      IF (IFLAG) THEN
        VALUE = DTIME (TARRAY)
        IFLAG = .FALSE.
      ELSE
        SEC = DTIME (TARRAY)
      ENDIF
C
      END
C
C
C     ===============================
      SUBROUTINE UGERR(STATUS,ERRSTR)
C     ===============================
C
C UGERR - Get error message string for error number in status
C
C Input:     STATUS - Error number (if negative print error message)
C
C Output:    ERRSTR - Error message string
C
C Arguments: INTEGER       STATUS
C            CHARACTER*(*) ERRSTR
C
C Usage:     CALL UGERR(STATUS, ERRSTR)
C
C     .. Scalar Arguments ..
      INTEGER STATUS
      CHARACTER ERRSTR* (*)
C     ..
C     .. Local Scalars ..
      LOGICAL IPRINT
C     ..
C     .. External Subroutines ..
      EXTERNAL GERROR
C     ..
      IPRINT = .FALSE.
      IF (STATUS.LT.0) THEN
        IPRINT = .TRUE.
        STATUS = -STATUS
      END IF
C
C---- Get error message from system
C
      CALL GERROR(ERRSTR)
      IF (IPRINT) WRITE (6,FMT=6000) 'UGERR',ERRSTR
C
 6000 FORMAT (' ',A,': ',A)
      END
C
C     ================================
      SUBROUTINE UGTENV(NAMENV,VALENV)
C     ================================
C
C UGTENV - Get value of env. variable
C
C Input:     NAMENV - Logical Name
C
C Output:    VALENV - It's value
C
C Arguments: CHARACTER*(*) NAMENV, VALENV
C
C Usage:     CALL UGTENV(NAMENV, VALENV)
C
C     .. Scalar Arguments ..
      CHARACTER NAMENV* (*),VALENV* (*)
C     ..
C     .. External Subroutines ..
ifelse(
_hpux,1,
[      EXTERNAL GETENV_
      CALL GETENV_ (NAMENV(1:LENSTR(NAMENV))//CHAR(0),VALENV)],
_AIX,1,
[      INTRINSIC GETENV
      CALL GETENV(NAMENV,VALENV)],
[     EXTERNAL GETENV 
      CALL GETENV(NAMENV,VALENV)])
C
      END
C
C
C     =========================
      SUBROUTINE UGTUID(USRNAM)
C     =========================
C
C UGTUID - Get user ID
C
C Input:     none
C
C Output:    UID - user ID string
C
C Arguments: CHARACTER*(*) UID
C
C Usage:     CALL UGTUID(UID)
C
C     .. Scalar Arguments ..
      CHARACTER USRNAM* (*)
C     ..
C     .. External Subroutines ..
ifelse(
_hpux,1,
[      EXTERNAL GETENV_
      CALL GETENV_ ('USER'//CHAR(0),USRNAM)
      IF (USRNAM.EQ.' ') CALL GETENV_ ('LOGNAME'//CHAR(0),USRNAM)],
_AIX,1,
[      INTINSIC GETENV
      CALL GETENV('USER',USRNAM)
      IF (USRNAM.EQ.' ') CALL GETENV('LOGNAME',USRNAM)],
[      EXTERNAL GETENV
      CALL GETENV('USER',USRNAM)
      IF (USRNAM.EQ.' ') CALL GETENV('LOGNAME',USRNAM)])
C
      END
C
C
C     ====================================
      SUBROUTINE UIDATE(IMONTH,IDAY,IYEAR)
C     ====================================
C
C UIDATE - Get date in 3 integer format. Alliant uses INTEGER*4
C          and order is IDAY,IMONTH,IYEAR
C
C Input:     none
C
C Output:    MONTH,DAY,YEAR
C
C Arguments: INTEGER MONTH, DAY, YEAR
C
C Usage:     CALL UIDATE(MONTH, DAY, YEAR)
C
C     .. Scalar Arguments ..
      INTEGER IDAY,IMONTH,IYEAR
C     .. Local Arrays ..
      INTEGER IARRAY(3)
C     ..
C     .. External Subroutines ..
ifdef(_hpux,,
[      EXTERNAL IDATE
])dnl
C     ..
      CALL IDATE(IARRAY(1),IARRAY(2),IARRAY(3))
      IDAY = IARRAY(2)
      IMONTH = IARRAY(1)
      IYEAR = MOD(IARRAY(3), 100)
C
      END
C
C
C     ==============================
      SUBROUTINE UISATT(FLUN,ANSWER)
C     ==============================
C
C UISATT - This function determines whether a program is being
C          run on-line if this information is available.
C
C Input:     FLUN - Fortran Unit Number
C
C Output:    ANS - 1 for on-line, 0 otherwise
C
C Arguments: INTEGER FLUN, ANS
C
C Usage:     CALL UISATT (FLUN,ANS)
C
C     .. Scalar Arguments ..
      INTEGER ANSWER,FLUN
C     ..
C     .. External Functions ..
      ifelse(ifdef(_hpux,1)ifdef(_AIX,1),1,[INTEGER],[LOGICAL]) ISATTY
      EXTERNAL ISATTY
C     ..
      ANSWER = 0
ifelse(
_hpux,1,
[      IF (ISATTY(FNUM(FLUN)).EQ.1) ANSWER = 1],dnl
_AIX,1,dnl
[      IF (ISATTY(%VAL(FLUN)).EQ.1) ANSWER = 1],dnl
[      IF (ISATTY(FLUN)) ANSWER = 1])
C
      END
CCCC
CCCC
CCCC     =================================
CCC      SUBROUTINE URENAM(FROM,TO,STATUS)
CCCC     =================================
CCCC
CCCC URENAM - Rename file assigned to FROM to TO.
CCCC
CCCC Input:     NAME1, NAME2   the file names
CCCC
CCCC Output:    STATUS       =0 if ok <>0 if an error
CCCC
CCCC Arguments: CHARACTER*(*) FROM, TO
CCCC            INTEGER       STATUS
CCCC
CCCC Usage:     CALL URENAM (FROM,TO,STATUS)
CCCC
CCCC     .. Scalar Arguments ..
CCC      INTEGER STATUS
CCC      CHARACTER FROM* (*),TO* (*)
CCCC     ..
CCCC     .. Local Scalars ..
CCC      CHARACTER ERRSTR*200
CCCC     ..
CCCC     .. External Subroutines ..
CCC      EXTERNAL UGERR
CCCC     ..
CCCC     .. External Functions ..
CCCifdef(_hpux,
CCC[      INTEGER RENAME_
CCC      EXTERNAL RENAME_      
CCC      STATUS = RENAME_ (FROM(1:LENSTR(FROM))//CHAR(0),
CCC     +                  TO(1:LENSTR(TO))//CHAR(0))],
CCC[      INTEGER RENAME
CCC      EXTERNAL RENAME
CCC      STATUS = RENAME(FROM,TO)
CCC      IF (STATUS.NE.0) CALL UGERR(-STATUS,ERRSTR)])
CCCC
CCC      END
C
C
C     =======================
      SUBROUTINE USTIME(ISEC)
C     =======================
C
C USTIME - Get absolute time in seconds. (returns with -1 under VMS)
C          Convex uses STIME (), others seem to use TIME (). Alliant
C          has TIME defined as INTEGER*4
C
C Input:     none
C
C Output:    SEC
C
C Arguments: INTEGER SEC
C
C Usage:     CALL USTIME(SEC)
C
      INTEGER ISEC
C
ifelse(_convex,1,[      INTEGER STIME
      EXTERNAL STIME
C
      ISEC = STIME()],[      INTEGER TIME
      EXTERNAL TIME
C
      ISEC = TIME()])
C
C
      END
C
C
C     =======================
      SUBROUTINE UTIME(CTIME)
C     =======================
C
C UTIME - Get current time hh:mm:ss
C
C Input:     none
C
C Output:    TIME - as ASCII string
C
C Arguments: CHARACTER*(*) CTIME
C
C Usage:     CALL UTIME(CTIME)
C
C     .. Scalar Arguments ..
      CHARACTER CTIME* (*)
C     ..
C     .. Local Arrays ..
      INTEGER IARRAY(3)
C     ..
C     .. External Subroutines ..
      EXTERNAL ITIME
C     ..
ifdef(_hpux,
[      CALL TIME(CTIME)],
[      CALL ITIME(IARRAY)
      WRITE (CTIME,FMT=6000) IARRAY(1),IARRAY(2),IARRAY(3)])
C
 6000 FORMAT (I2,2 (':',I2.2))
      END
C
C
C     =========================
      LOGICAL FUNCTION VAXVMS()
C     =========================
C
C VAXVMS - Operating Sytem in use returns .TRUE. if VAXVMS
C
C Input:     none
C
C Returns:   .TRUE. for VAXVMS, .FALSE. otherwise
C
C Arguments: none
C
C Usage:     VAXVMS ()
C
      VAXVMS = .FALSE.
C
      END
