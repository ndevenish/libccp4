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
cc     exit - terminate program with status
cc     Terminates the executing process with 
cc      the specified status after flushing
cc      and closing all open units.
cc     call exit [([status.i4])]
cc
ccDESCRIPTION
cc  If the process is a child process on which the parent is waiting, the
cc  status is returned in the third byte of the wait argument and the parent
cc  is activated.
cc
ccARGUMENTS
cc     status: integer*4
cc    The status value for the exiting process; if omitted, defaults to 0.
cc
ccEXAMPLES
cc     The following source code:
cc                  integer*4 fork
cc                 id = fork()
cc                  if (id.eq.0) then
cc                    call child ()
cc                  else
cc                    call parent (id)
cc                  end if
cc                  end
cc                  subroutine parent (id)
cc                  integer*4 wait,status
cc                  byte    s(4)
cc                  equivalence (status,s)
cc          12      print *, 'I am the parent'
cc                  print *, wait(status)
cc                  print *, status, s(1), s(2), s(3), s(4)
cc                  end
cc                  subroutine child ()
cc                  call exit (99)
cc                  end
cc
cc     Produces the following results:
cc
cc          I am the parent
cc          5099
cc          25344    0    0   99    0
cc
cc
cc
cc            if (istat.eq.0) then
cc             write(6,6000) errstr(1:lenstr(errstr))
cc             call ccperc(istat,errstr)
cc             stop
cc            else
cc              call ccperc(istat,errstr)
cc            end if
cc
cc
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
      STRING = 'WORDS'
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
C ========
C UNIX.FOR
C ========
C
C UGTENV - Get value of env. variable
C UGERR  - Get error explanation
C UGTIUD - Get user id - it's name
C UIDATE - Get date in 3 integer format
C UTIME  - Get current time
C USTIME - Get absolute time in seconds (-1 for VMS)
C UCPUTM - Get CPU time
C UISATT - Is file a terminal?
C URENAM - Rename file
C VAXVMS - Logical function returns TRUE if VAX/VMS
C UBYTES - Returns number of bytes per word and 'words'/'bytes'
C          to indicate if byte handling is available
C CCPERR - uses routine ccperc in library.c
C CCPFYP - process command line arguments (uses s/r args in unix.c)
C NOCRLF - write line supressing cr/lf
C
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
      EXTERNAL GETENV
C     ..
      CALL GETENV(NAMENV,VALENV)
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
      EXTERNAL GETENV
C     ..
      CALL GETENV('USER',USRNAM)
      IF (USRNAM.EQ.' ') CALL GETENV('LOGNAME',USRNAM)
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
      EXTERNAL IDATE
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
      LOGICAL ISATTY
      EXTERNAL ISATTY
C     ..
      ANSWER = 0
      IF (ISATTY(FLUN)) ANSWER = 1
C
      END
C
C
C     =================================
      SUBROUTINE URENAM(FROM,TO,STATUS)
C     =================================
C
C URENAM - Rename file assigned to FROM to TO.
C
C Input:     NAME1, NAME2   the file names
C
C Output:    STATUS       =0 if ok <>0 if an error
C
C Arguments: CHARACTER*(*) FROM, TO
C            INTEGER       STATUS
C
C Usage:     CALL URENAM (FROM,TO,STATUS)
C
C     .. Scalar Arguments ..
      INTEGER STATUS
      CHARACTER FROM* (*),TO* (*)
C     ..
C     .. Local Scalars ..
      CHARACTER ERRSTR*200
C     ..
C     .. External Subroutines ..
      EXTERNAL UGERR
C     ..
C     .. External Functions ..
      INTEGER RENAME
      EXTERNAL RENAME
C     ..
      STATUS = RENAME(FROM,TO)
      IF (STATUS.NE.0) CALL UGERR(-STATUS,ERRSTR)
C
      END
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
C     .. Scalar Arguments ..
      INTEGER ISEC
cc      INTEGER*4 TIME
C     ..
C     .. External Functions ..
      INTEGER TIME
      EXTERNAL TIME
C     ..
      ISEC = TIME()
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
      CALL ITIME(IARRAY)
      WRITE (CTIME,FMT=6000) IARRAY(1),IARRAY(2),IARRAY(3)
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
C
C
C
      SUBROUTINE CCPOPN(IIUN,LOGNAM,KSTAT,ITYPE,LREC,IFAIL)
C     ====================================================
C
C---- This subroutine is used to open a file
C
C---- If appropriate the subroutine will use the standard fortran77
C     open statement. on machines where this is not adequate, machine
C     specific parameters may be included. if necessary each program may
C     be given its own version of 'ccpopn' if a general version cannot
C     meet the requirements.
C
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
C
C I/O STATUS RETURNED IN 'IOS' IN COMMON 'CCPSTT' IF NEEDED
C
C
C SPECIFICATION STATEMENTS
C
C
C     .. Scalar Arguments ..
      INTEGER IFAIL,KSTAT,ITYPE,IIUN,LREC
      CHARACTER LOGNAM* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER IOS
C     ..
C     .. Local Scalars ..
      INTEGER LLREC,IUN,IBYTES,ISTAT
      CHARACTER CCNTRL*7,DISPOS*7,ST*7,FRM*12,ERRSTR*80,FULNAM*255,
     +          NAMFIL*255,HANDLE*5,CCP4_OPEN*20
C     ..
C     .. Local Arrays ..
      CHARACTER STAT(6)*7
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      LOGICAL VAXVMS
      EXTERNAL LENSTR,VAXVMS
C     ..
C     .. External Subroutines ..
      EXTERNAL UGERR,UGTENV
C     ..
C     .. Common blocks ..
      COMMON /CCPSTT/IOS
C     ..
C     .. Save statement ..
      SAVE /CCPSTT/
C     ..
C     .. Data statements ..
      DATA STAT/'UNKNOWN','SCRATCH','OLD','NEW','OLD','UNKNOWN'/
C     ..
C
      ISTAT = KSTAT
C
C---- Negative unit number means dont give messages for
C     successful open
C
      IUN = IIUN
      IF (IIUN.LT.0) IUN = -IIUN
C
C---- Open the file
C
      IF (LOGNAM.NE.'DATA' .AND. LOGNAM.NE.'PRINTER') THEN
        IF (LOGNAM(1:4).NE.'TERM') THEN
          IF (ISTAT.GE.1 .AND. ISTAT.LE.6) THEN
            IF (ITYPE.GE.1 .AND. ITYPE.LE.4) THEN
C
C---- Executive call (see unix.for or vms.for)
C
              CALL UGTENV(LOGNAM,NAMFIL)
              IF (NAMFIL.EQ.' ') NAMFIL = LOGNAM
              ST = STAT(ISTAT)
              FRM = 'FORMATTED'
              DISPOS = 'KEEP'
C
C--------------------------------------start of patch for null file 
C---- patch for NULL ???
C
              IF ( NAMFIL.EQ.'/dev/null') THEN
              ISTAT = 1
              ST = STAT(ISTAT)
              END IF
C-----------------------------------------end of patch for null file
C
C-------------------------------------------start of patch for override
C                                           NEW to UNKNOWN via
C                                           environment variable
C                                           CCP4_OPEN
C
       IF (ISTAT.EQ.4) THEN
         CCP4_OPEN = ' '
         CALL UGTENV('CCP4_OPEN',CCP4_OPEN)
         IF (CCP4_OPEN.EQ.'UNKNOWN') THEN
          ISTAT = 1
          ST = STAT(ISTAT)
          END IF
        END IF
C
C
C---- If scratch file on DL Convex,
C     then treat as UNKNOWN with DISPOSE=DELETE
C     The file should be defined by setting the name of a file
C     within one of the scratch areas /SCR1/ or /SCR2/ 
C     in a environment variable with
C     a name of the required logical file name
C
              IF (ISTAT.EQ.2) THEN
                ST = 'UNKNOWN'
                DISPOS = 'DELETE'
              END IF
C
              IF (ITYPE.EQ.2 .OR. ITYPE.EQ.4) FRM = 'UNFORMATTED'
              IF (ITYPE.GT.2) THEN
C
C---- direct access
C
C---- These few lines are necessary due to VAX/VMS, which uses different
C     record length specifivation for FORMATTED and UNFORMATTED file.
C     (See VAX/VMS  VMSSUPPORT.FOR file).
C     If you remember, LREC for UNFORMATED file is meassured in WORDS!!!
C
                CALL UBYTES (IBYTES,HANDLE)
                LLREC = LREC*IBYTES
                IF (HANDLE.EQ.'WORDS'.AND.ITYPE.EQ.4)LLREC=LLREC/IBYTES
                IF (IFAIL.EQ.0) THEN
C
C---- If 'SCRATCH'
C
                  IF (ISTAT.EQ.2) THEN
C
C---- open for Convex and VMS
C
        OPEN(UNIT=IUN,STATUS=ST,ACCESS='DIRECT',FORM=FRM,
     +  FILE=NAMFIL,RECL=LLREC,DISPOSE=DISPOS,IOSTAT=IOS)
C
C---- limited standard open
C
cc      OPEN (UNIT=IUN,STATUS='SCRATCH',ACCESS='DIRECT',
cc     +  FORM=FRM,RECL=LLREC,IOSTAT=IOS)
        CALL CUNLINK (NAMFIL)
                  ELSE
C
C
C---- open for Convex and VMS
C
       OPEN(UNIT=IUN,STATUS=ST,ACCESS='DIRECT',FORM=FRM,
     + FILE=NAMFIL,RECL=LLREC,DISPOSE=DISPOS,IOSTAT=IOS)
C
C---- limited standard open
C
cc      OPEN (UNIT=IUN,STATUS=ST,ACCESS='DIRECT',
cc     +  FILE=NAMFIL,FORM=FRM,RECL=LLREC,IOSTAT=IOS)
C
                  END IF
C
C
                  IF (IOS.NE.0) THEN
C
C---- Executive call  (see uxsupport.c)
C
                    CALL UGERR(IOS,ERRSTR)
                    WRITE (6,FMT=6002) IUN,NAMFIL(1:LENSTR(NAMFIL)),ST
                    WRITE (6,FMT=6008) ERRSTR(1:LENSTR(ERRSTR))
                    CALL CCPERR(1, '**CCPOPN ERROR** IN CCPLIB 6008')
                  ELSE
                    GO TO 10
                  END IF
                ELSE
C
C---- open for Convex and VMS
C
      OPEN(UNIT=IUN,STATUS=ST,ACCESS='DIRECT',FORM=FRM,FILE=NAMFIL,
     + RECL=LLREC,DISPOSE=DISPOS,IOSTAT=IOS)
C
C---- standard open
C
cc      OPEN (UNIT=IUN,STATUS=ST,ACCESS='DIRECT',FILE=NAMFIL,
cc     +   FORM=FRM,RECL=LLREC,IOSTAT=IOS)
C
                  IF (IOS.NE.0) THEN
C
C---- Executive call (see uxsupport.c)
C
                    CALL UGERR(IOS,ERRSTR)
                  ELSE
                    GO TO 10
                  END IF
                END IF
              ELSE
C
C---- Set carriagecontrol='fortran' for print file, else = 'list'
C
                IF (ISTAT.EQ.6) THEN
                  CCNTRL = 'FORTRAN'
                ELSE
                  CCNTRL = 'LIST'
                END IF
C
C---- ISTAT = 5 for readonly
C
                IF (ISTAT.EQ.5) THEN
C
C---- open for Convex and VMS
C
                    IF (ITYPE .EQ. 1) THEN
       OPEN(UNIT=IUN,STATUS=ST,ACCESS='SEQUENTIAL',FILE=NAMFIL,
     + CARRIAGECONTROL=CCNTRL,READONLY,FORM=FRM,DISPOSE=DISPOS,
     + IOSTAT=IOS)
                    ELSE
       OPEN(UNIT=IUN,STATUS=ST,ACCESS='SEQUENTIAL',FILE=NAMFIL,
     + READONLY,FORM=FRM,DISPOSE=DISPOS,
     + IOSTAT=IOS)
                    ENDIF
C
C---- open for limited number of keywords
C
cc      OPEN (UNIT=IUN,STATUS=ST,ACCESS='SEQUENTIAL',
cc     +  FILE=NAMFIL,FORM=FRM,IOSTAT=IOS)
C
C---- ISTAT = 2 for 'SCRATCH'
C
                ELSE IF (ISTAT.EQ.2) THEN
C
C---- open for limited number of keywords
C
cc      OPEN (UNIT=IUN,STATUS='SCRATCH',ACCESS='SEQUENTIAL',
cc     +  FORM=FRM,IOSTAT=IOS)
C
C---- open for Convex and VMS
C
                    IF (ITYPE .EQ. 1) THEN
        OPEN(UNIT=IUN,STATUS=ST,ACCESS='SEQUENTIAL',FILE=NAMFIL,
     + CARRIAGECONTROL=CCNTRL,FORM=FRM,DISPOSE=DISPOS,IOSTAT=IOS)
                    ELSE
        OPEN(UNIT=IUN,STATUS=ST,ACCESS='SEQUENTIAL',FILE=NAMFIL,
     + FORM=FRM,DISPOSE=DISPOS,IOSTAT=IOS)
                    ENDIF
                ELSE
C
C---- open for Convex and VMS
C
                    IF (ITYPE .EQ. 1) THEN
        OPEN(UNIT=IUN,STATUS=ST,ACCESS='SEQUENTIAL',FILE=NAMFIL,
     + CARRIAGECONTROL=CCNTRL,FORM=FRM,DISPOSE=DISPOS,IOSTAT=IOS)
                    ELSE
        OPEN(UNIT=IUN,STATUS=ST,ACCESS='SEQUENTIAL',FILE=NAMFIL,
     + FORM=FRM,DISPOSE=DISPOS,IOSTAT=IOS)
                    ENDIF
C
C---- open for limied number of keywords
C
cc      OPEN (UNIT=IUN,STATUS=ST,ACCESS='SEQUENTIAL',
cc     +  FILE=NAMFIL,FORM=FRM,IOSTAT=IOS)
C
                END IF
C
                IF (IOS.NE.0) THEN
C
C---- UNIX Executive call  (uxsupport.c)
C
                  CALL UGERR(IOS,ERRSTR)
                  IF (IFAIL.EQ.0) THEN
C
C---- Hard fail
C
                    WRITE (6,FMT=6002) IUN,NAMFIL(1:LENSTR(NAMFIL)),ST
                    WRITE (6,FMT=6008) ERRSTR(1:LENSTR(ERRSTR))
                    CALL CCPERR(1, '**CCPOPN ERROR** IN CCPLIB 6008-1')
                  ELSE
C
C----  Soft fail
C
                  END IF
                ELSE
                  GO TO 10
                END IF
              END IF
C
c---- error return
C
C---- Soft fail (IFAIL .ne. 0)
C
              WRITE (6,FMT=6004) FRM,ST,IUN,LOGNAM(1:LENSTR(LOGNAM)),
     +          NAMFIL(1:LENSTR(NAMFIL))
              WRITE (6,FMT=6008) ERRSTR(1:LENSTR(ERRSTR))
              IFAIL = -1
              RETURN
C
C---- Successful open, print file info except for files DATA,
C     PRINTER or TERM...
C
   10         CONTINUE
              IF (IIUN.LE.0) GO TO 15
              IF (LOGNAM.NE.'DATA' .AND. LOGNAM.NE.'PRINTER' .AND.
     +            LOGNAM(1:4).NE.'TERM') THEN
                INQUIRE (FILE=NAMFIL,NAME=FULNAM)
                WRITE (6,FMT=6000) FRM,ST,IUN,LOGNAM(1:LENSTR(LOGNAM)),
     +            FULNAM(1:LENSTR(FULNAM))
              END IF
   15         CONTINUE
              RETURN
            END IF
          END IF
C
C---- Invalid arguments
C
          WRITE (6,FMT=6006)
          IF (IFAIL.EQ.0) THEN
            CALL CCPERR(1,'**CCPOPN ERROR** IN CCPLIB 6006')
          ELSE
            IFAIL = -1
          END IF
        END IF
      END IF
C
c---- format statements
C
 6000 FORMAT (/1X,A,3X,A,' file opened on unit ',I3,/' Logical name: ',
     +       A,', Full name: ',A,/)
 6002 FORMAT (' **CCPOPN ERROR** Unit',I4,', File ',A,', Status ',A,/)
 6004 FORMAT (' **CCPOPN ERROR**  ',A,3X,A,
     +       ' file open failure on unit ',I3,/' Logical name: ',A,', ',
     +       'File name: ',A,/)
 6006 FORMAT (' **CCPOPN ERROR** Invalid parameters in call',/)
 6008 FORMAT (' Op-system  error: ',A,/)
C
C
      END
