dnl -*-fortran-*-
C
C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
dnl *** This file is meant to be processed by m4 with an appropriate 
dnl     definition for the system type to produce Unix.f.
dnl     Be very careful about changing the m4-isms -- it's particularly easy 
dnl     to introduce spurious spaces.  It would be less fragile with cpp, but
dnl     we can't rely on being able to find it and use it with Fortran.
dnl
dnl     Be careful about declaring system library routines either external or
dnl     intrinsic -- it's probably better to leave them undeclared and let the
dnl     compiler sort it out since this behaviour may (does?) differ.
dnl
dnl $Id$
dnl
changequote([,])dnl * use [] as quotes rather than `'
dnl
dnl * units for record lengths:
ifelse(_sgi,1,
  [define(_ubytes,'WORDS')],
dnl Changed in `DEC fortran' as opposed to (old) MIPS compiler on Ultrix:
_dec_fortran,1,
  [define(_ubytes,'WORDS')],
  [define(_ubytes,'BYTES')])dnl
dnl
dnl * fortran compiler may or may not accept READONLY specifier on OPEN.
dnl   specifying it might catch the occasional bug.
ifelse(
_convex,1,
  [define(_readonly,[READONLY,])],
_sgi,1,
  [define(_readonly,[READONLY,])],
_pgf,1,
  [define(_readonly,[READONLY,])],
dnl * in Dec Fortran V3.0-2, at least, this seems to be *necessary*:
_dec_fortran,1,
  [define(_readonly,[READONLY,])],
  [define(_readonly,)])dnl
dnl
dnl * fortran compiler may or may not accept CARRIAGECONTROL specifier on OPEN:
ifelse(
_convex,1,
  [define(_carriagecontrol,[CARRIAGECONTROL=CCNTRL,])],
_sgi,1,
  [define(_carriagecontrol,[CARRIAGECONTROL=CCNTRL,])],
_lf95,1,
  [define(_carriagecontrol,[CARRIAGECONTROL=CCNTRL,])],
_concentrix,1,
  [define(_carriagecontrol,[CARRIAGECONTROL=CCNTRL,])],
  [define(_carriagecontrol,)])dnl
dnl
dnl * sometimes we can use IOINIT to specify carriagecontrol:
ifelse(_sun,1,
  [undefine(_has_carriagecontrol)define(_ioinit,1)])dnl
dnl
dnl * In some cases we can't unlink scratch files in case they're rewond
dnl   since REWIND is implemented as close + open.  Maybe a better solution
dnl   would use a routine doing a rewind instead of REWIND on scratch files.
dnl   There does *not* seem to be a problem with the following systems:
dnl     irix (4.0.5) (but see below), osf/1 (1.3), concentrix 2800 (3.0),
dnl     aix (2.3?), HPUX (9.01), SunOS (4.1.3/SunPro 2.0.1)
dnl  I'm inclined to think _cant_unlink should be the default now...
ifelse(_convex,1,
  [define(_cant_unlink,1)],dnl  OS 10, at least
_titan,1,
  [define(_cant_unlink,1)],dnl
_irix51,1,
  [define(_cant_unlink,1)],dnl  necessary in irix 5.1 up
dnl I thought _cant_unlink wasn't necessary for libf2c, but it certainly
dnl seems to be now (Irix5).  I wonder if this is somehow to do
dnl with stdio rather than the fortran library itself...
_f2c,1,
  [define(_cant_unlink,1)],dnl
_g77,1,
  [define(_cant_unlink,1)],dnl
_lf95,1,
  [define(_cant_unlink,1)],dnl
_esv,1,
  [define(_cant_unlink,1)])dnl
dnl * In the case above, we then want to open STATUS='DELETE', if 
dnl   supported.  Note the file will still be left around if the program
dnl   crashes.  Irix (f77 3.4.4) tolerates the unlink but re-instates
dnl   the file when you try to do anything with it...
ifelse(_convex,1,
  [define(_dispose,[DISPOSE=DISP,])],
_sgi,1,
  [define(_dispose,[DISPOSE=DISP,])],
_esv,1,
  [define(_dispose,[DISPOSE=DISP,])],
  [define(_dispose,)])dnl
dnl
C  *** this file was automatically generated by configure
C  *** edit by hand only in extremis
C
C ========
C UNIX.FOR
C ========
C
C CCPOPN - open a file
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
C TTSEND - Write string to terminal with various carriage control
C     options
C GETELAPSED - Print timing info for CCPERR
C UGTARG - Get command-line argument
C GETREF - Abstracted from abscale since it has BYTE declaration.
C CCPSPW - Spawns a new process to run shell command
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
C     not to have any means of doing this and AIX seems to be likewise,
C     sigh; they both seem to obey the normal Unix convention of
C     printing the format as-is rather than obeying the first character
C     as carriage control.  Concentrix does obey the first column a la
C     VMS and `traditional' Fortran; the MIPS compilers have a compile
C     (link?) option to do so.  [Unfortunately, carriagecontrol
C     specification isn't even defined in Fortan90, although
C     `ACTION="READ"' can be used.]
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
C                                 =2, CONTINUE SILENTLY AFTER OPEN FAILURE
C                   ON OUTPUT:    UNCHANGED IF FILE OPEN OK
C                                 =-1, ERROR IN OPENING FILE
C
C     .. Scalar Arguments ..
      INTEGER IFAIL,KSTAT,ITYPE,IIUN,LREC
      CHARACTER LOGNAM* (*)
C     ..
C     .. Local Scalars ..
      INTEGER LLREC,IUN,IBYTES,ISTAT,L
      CHARACTER CCNTRL*7,ST*7,FRM*12,ERRSTR*500,
     +     NAMFIL*255,HANDLE*5,OPNVAR*20, access*10
      INTEGER UNKNWN, SCRTCH, OLD, NEW, RDONLY, PRINTR
      LOGICAL CCPEXS, LNONAM
      PARAMETER (UNKNWN=1, SCRTCH=2, OLD=3, NEW=4, RDONLY=5, PRINTR=6)
ifdef(_ioinit,[      LOGICAL JUNK])dnl
C     ..
C     .. Local Arrays ..
      CHARACTER STAT(6)*7, DISP*6
C     ..
C     .. External Functions ..
      INTEGER LENSTR, LUNSTO
ifdef(_ioinit,[
      LOGICAL IOINIT])dnl
      EXTERNAL LENSTR, LUNSTO
C     ..
C     .. External Subroutines ..
      EXTERNAL UGERR,UGTENV,CCPEXS
C     ..
C     .. Data statements ..
C     NB mustn't have SCRATCH in here, because result is system
C     -dependent
      DATA STAT/'UNKNOWN','UNKNOWN','OLD','NEW','OLD','UNKNOWN'/
C     ..
C     
      ISTAT = KSTAT
C     Negative unit number means don't give messages for successful open
      IUN = IIUN
      IF (IIUN.LT.0) IUN = -IIUN
C     Check args:
      IF (ISTAT.LT.1 .OR. ISTAT.GT.6 .OR. ITYPE.LT.1 .OR. ITYPE.GT.4)
     +     THEN 
        IF (IFAIL.EQ.0) THEN
          CALL CCPERR(1,
     +         '**CCPOPN ERROR** Invalid parameters in call')
        ELSE
          WRITE (LUNSTO(1),
     +         '('' **CCPOPN ERROR** Invalid parameters in call'',/)')
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
C     check for `logical name' referencing real file
      CALL UGTENV(LOGNAM,NAMFIL)
      LNONAM = .FALSE.
      IF (NAMFIL.EQ.' ') THEN
        IF (.NOT. CCPEXS(LOGNAM)) LNONAM = .TRUE.
        NAMFIL = LOGNAM
      END IF
C     VMS null device (VMS code canonicalises /dev/null)
      IF (NAMFIL.EQ.'NL:' .OR. NAMFIL.EQ.'nl:') NAMFIL='/dev/null'
C     Special case:  /dev/null should be opened UNKNOWN
      IF ( NAMFIL.EQ.'/dev/null') ISTAT = 1
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
      IF (ISTAT.EQ.SCRTCH) THEN
        DISP = 'DELETE'
      ELSE
        DISP = 'KEEP'
      ENDIF
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
     +         _dispose
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
     +           _dispose
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
     +           _dispose
     +           FORM=FRM, ERR=5, IOSTAT=IOS)
          ENDIF
        ENDIF
      ENDIF
C
C     Scratch files are immediately unlinked from the directory; they
C     become inaccessible only when closed, but don't appear in the
C     directory and the name can be re-used.
C     NB this may break with REWIND if that is implemented as close +
C     reopen, sigh.  See also _dispose above
ifelse(_cant_unlink,1,,[
      IF (ISTAT.EQ.SCRTCH) CALL CUNLINK (NAMFIL)]
)dnl
C
C     Error check
 5    CONTINUE
C     don't report UNKNOWN if actually SCRATCH
      IF (ISTAT.EQ.SCRTCH) ST = 'SCRATCH'
      IF (IOS.NE.0) THEN
        CALL UGERR(IOS,ERRSTR)
        IF (IFAIL.EQ.0) THEN
C         warning if there was no file associated with logical name
          IF (LNONAM) THEN
             ERRSTR = 'CCPOPN Logical name '//LOGNAM
             ERRSTR(LENSTR(ERRSTR)+2:) = 'has no associated file name'
             CALL CCPERR(2,ERRSTR)
          END IF
C         hard failure
          WRITE (LUNSTO (1),FMT=6002) IUN, NAMFIL(1:LENSTR(NAMFIL)),
     +         LOGNAM(1:LENSTR(LOGNAM))
 6002     FORMAT (' Open failed: Unit:',I4,', File: ',A, ' (logical: ',
     +         A, ')')
          ERRSTR = ' Open failed: File: ' // NAMFIL
          CALL CCPERR(-1, ERRSTR)
        else
C         soft failure
          IF (IFAIL.EQ.1) WRITE (lunsto (1),FMT=6004) FRM, ST, IUN, 
     +         LOGNAM(1:LENSTR(LOGNAM)), NAMFIL(1:LENSTR(NAMFIL)),
     +         ERRSTR(1:LENSTR(ERRSTR))
 6004     FORMAT (' **CCPOPN ERROR**  ',A,3X,A,
     +         ' file open failure on unit ',I3,/' Logical name: ',
     +         A,', ','File name: ',A/1X,A/)
          IFAIL = -1
          RETURN            
        ENDIF
      ELSE
        IF (IIUN.LE.0) RETURN 
        WRITE (ERRSTR,FMT=6000) FRM,ST,IUN
        CALL QPRINT (1, ' ')
        CALL QPRINT (1, ERRSTR)
        call ccp4h_summary_beg()
        ERRSTR = 'Logical name: '
        ERRSTR (15:) = LOGNAM
        L = MIN(LENSTR (ERRSTR) + 1, LEN (ERRSTR))
        ERRSTR (L:) = ', Filename: ' // NAMFIL
        CALL QPRINT (1, ERRSTR)
        call ccp4h_summary_end()
        CALL QPRINT (1, ' ')
 6000 FORMAT (A,3X,A,' file opened on unit ',I3)
      ENDIF 
      END
C
C
C     =====================
      SUBROUTINE GETELAPSED
C     =====================
C
      EXTERNAL LUNSTO, USTIME
      INTEGER LUNSTO
      REAL TARRAY(2), JUNK
      INTEGER ELAPS, START
      LOGICAL INITED
      SAVE START, INITED
      DATA INITED /.FALSE./
C     
      JUNK = ETIME(TARRAY)
      CALL USTIME(ELAPS)
      ELAPS = ELAPS - START
C     don't print anything if it hasn't been initialised (by CCPFYP)
      IF (INITED) WRITE(LUNSTO(1),6000) TARRAY(1), TARRAY(2), 
     +     ELAPS/60, MOD(ELAPS, 60)
 6000 FORMAT(' Times: User: ', F9.1, 's System: ', F6.1, 's Elapsed:',
     +     I5 , ':',I2.2)
C     
      ENTRY INITFYP
      CALL USTIME(START)
      INITED = .TRUE.
C     Machine-dependent startup, e.g. set FPE on SunOS
ifelse(_sun,1,[
C     common FPEs get sigfpe_abort
      i=ieee_handler('set','common',%val(2))
      ])
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
C     Get CPU time in seconds
C
C     Parameter:
C     REAL SEC (i/o): If sec<=0.0, initialize timer and return current
C                     elapsed cpu time since start of execution, otherwise
C                     return elapsed cpu since timer was initialized.
C                     Time is in seconds.
C
C     .. Scalar Arguments ..
      REAL SEC
C     ..
C     .. Local Scalars ..
      REAL TLAST
C     ..
C     .. Local Arrays ..
      REAL TARRAY(2)
C     ..
C     .. Save statement ..
      SAVE TLAST
C     ..
      IF (SEC.LE.0.0) THEN
        TLAST = ETIME (TARRAY)
        SEC = TLAST
      ELSE
        SEC = ETIME (TARRAY) - TLAST
      ENDIF
      END
C
C
C     ===============================
      SUBROUTINE UGERR(STATUS,ERRSTR)
C     ===============================
C
C UGERR - Get error message string for error number in STATUS
C     (supposedly).  Actually it ignores STATUS and always uses the
C     *last* error that occurred.
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
      INTEGER IERRNO, LUNSTO
      EXTERNAL IERRNO, LUNSTO
C     ..
      IPRINT = .FALSE.
      IF (STATUS.LT.0) THEN
        IPRINT = .TRUE.
        STATUS = -STATUS
      END IF
C
C---- Get error message from system
C
      IF (IERRNO().NE.0) THEN
        CALL GERROR(ERRSTR)
      ELSE
        ERRSTR = ' '
      ENDIF
      IF (IPRINT) WRITE (LUNSTO(1),FMT=6000) 'UGERR',ERRSTR
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
C Input:     NAMENV - Logical Name (trailing blanks are stripped)
C
C Output:    VALENV - Its value
C
C Arguments: CHARACTER*(*) NAMENV, VALENV
C
C Usage:     CALL UGTENV(NAMENV, VALENV)
C
C     .. Scalar Arguments ..
      CHARACTER NAMENV* (*),VALENV* (*)
C     ..
C     .. External Subroutines ..
C     don't declare getenv
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
      CALL GETENV(NAMENV(:LENSTR(NAMENV)),VALENV)
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
C     don't declare getenv
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
C     don't declare IDATE -- it's often an intrinsic to avoid confusion
C     between the two possible calling sequences.  On SGI, it's only
C     documented in one style but seems to work with the canonical
C     Unix one too; however, the order of the arguments of the
C     documented version (used here) *isn't* the same as for VMS...
C
ifelse(_sgi,1,
[C     Updating 3 array elements separately would be illegal aliasing,
C     of course
      CALL IDATE(IDAY,IMONTH,IYEAR)],dnl VMS-style
_esv,1,
[      CALL IDATE(IDAY,IMONTH,IYEAR)],
[      CALL IDATE (IARRAY)
      IDAY = IARRAY(1)
      IMONTH = IARRAY(2)
      IYEAR = IARRAY(3)])
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
dnl there doesn't seem to be an AIX hook to translate between
dnl            fortran units and Unix stream numbers
ifelse(_AIX,1,
[      INTEGER IFLUN
      INTEGER ISATTY
      EXTERNAL ISATTY
      ANSWER = 0
      IFLUN = FLUN
      IF (FLUN.EQ.5) THEN
        IFLUN = 0
      ELSE IF (FLUN.EQ.6) THEN
        IFLUN = 1
      ELSE
        CALL CCPERR(1,'Unit number other than 5 or 6 used with'//
     +       'UISATT or ISATTY (AIX restriction)')
      ENDIF
      IF (ISATTY(%VAL(IFLUN)).EQ.1) ANSWER = 1],
_hpux,1,
[      INTEGER ISATTY
      EXTERNAL ISATTY
      ANSWER = 0
      IF (ISATTY(%VAL(FNUM(FLUN))) .EQ.1) ANSWER = 1],
dnl (else)
[      LOGICAL ISATTY
      EXTERNAL ISATTY
      ANSWER = 0
      IF (ISATTY(FLUN)) ANSWER = 1])
C
      END
C
C
C     =======================
      SUBROUTINE USTIME(ISEC)
C     =======================
C
C USTIME - Get absolute time in seconds.
C          Convex uses STIME (), others seem to use TIME ().
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
ifelse(_convex,1,
[      INTEGER STIME
      ISEC = STIME()],
_hpux,1,
[ifelse(_hpux9,1,,[ifelse(_hpux10,1,[      isec=time()],[      ISEC = SECNDS(0.0)])])],
[      INTEGER TIME
C
      ISEC = TIME()])
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
ifdef(_hpux,
[      CALL TIME(CTIME)],
[      CALL ITIME(IARRAY)
      WRITE (CTIME,FMT=6000) IARRAY(1),IARRAY(2),IARRAY(3)
 6000 FORMAT (I2,2 (':',I2.2))])
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
C     =========================
      LOGICAL FUNCTION WINMVS()
C     =========================
C
C WINMVS - Windows mircrosoft Visual Studio
C
C Input:     none
C
C Returns:   .TRUE. for WINMVS, .FALSE. otherwise
C
C Arguments: none
C
C Usage:     WINMVS ()
C
      WINMVS = .FALSE.
C
      END
C
C
C SUBROUTINE 'TTSEND'
C ===================
C
C Write a string to a terminal with various carriage control options
C [for LAUE]
C
      SUBROUTINE TTSEND (IUN, STR, ICC)
C
C Parameters:
C
C         IUN (I)   Unit number for the output
C         STR (I)   The string to be output
C         ICC (I)   = 0, no carriage control at the end of the string
C                        (for prompts)
C                        e.g. for routine TPROMP
C                   = 1, normal carriage control
C                        e.g. for routine TWRITE
C                   = 2, no carriage control (for sending escape/control
C                        character sequences to ANSI/T4014 terminals)
C                        e.g. for QSCREEN graphics routines
C                   = 3, Output line at current point on screen (no leading
C                        line feed or carriage return - trailing does not
C                        matter)
C
C Machine dependence examples: Convex   1000  FORMAT (A,$)
C                                       1001  FORMAT (A)
C                                       1002  FORMAT (A,$)
C                                       1003  FORMAT (A)
C                              
C                              Vax      1000  FORMAT (' ',A,$)
C                                       1001  FORMAT (' ',A)
C                                       1002  FORMAT ('+',A,$)
C                                       1003  FORMAT ('+',A)
C
C
C====== Specification statements
C
      CHARACTER*(*) STR
      CHARACTER*10 CCNTRL
C
C====== Write string
C
C     'LIST' is the equivalent of the normal Unix state
      CCNTRL = 'LIST'
ifdef(_carriagecontrol,[],,
      INQUIRE(IUN,_carriagecontrol)
)dnl
C     in the case of systems obeying the carriagecontrol specifier, 
C     we assume the stream has actually been opened, so that the
C     specifier is suitably defined -- on the Alliant, for instance,
C     it will be 'UNKNOWN' for an unopened stream (6 is pre-opened)
C
      IF (CCNTRL .EQ. 'FORTRAN') THEN
C       VMS-type
        IF (ICC.EQ.0) THEN
          WRITE (IUN,1004) STR
        ELSE IF (ICC.EQ.2) THEN
          WRITE (IUN,1006) STR
        ELSE IF (ICC.EQ.3) THEN
          WRITE (IUN,1007) STR
        ELSE
          WRITE (IUN,1005) STR
        ENDIF
      ELSE
        IF (ICC.EQ.0) THEN
          WRITE (IUN,1000) STR
        ELSE IF (ICC.EQ.2) THEN
          WRITE (IUN,1002) STR
        ELSE IF (ICC.EQ.3) THEN
          WRITE (IUN,1003) STR
        ELSE
          WRITE (IUN,1001) STR
        ENDIF
      ENDIF
C     these formats are mostly non-standard, of course...
1000  FORMAT (A,$)
1001  FORMAT (A)
1002  FORMAT (A,$)
1003  FORMAT (A)
 1004 FORMAT (' ',A,$)
 1005 FORMAT (' ',A)
 1006 FORMAT ('+',A,$)
 1007 FORMAT ('+',A)
      END
C
      SUBROUTINE UGTARG(I, ARG)
      INTEGER I
      CHARACTER *(*) ARG
ifelse(_hpux,1,
[C Maybe HPUX doesn't need to be different here.  The Fortran/9000
C Reference says:
C   4.  GETARG can be accessed only with the
C 
C       $HP9000_800 INTRINSICS  ON
C 
C       compiler directive.  GETARG is similar to IGETARG except that
C       GETARG is called as a subroutine instead of as a function.  It
C       accepts two arguments:  n and str.  n is an integer specifying
C       which command-line argument is requested.  When n=1, it returns
C       the program name (unlike IGETARG which returns the program name
C       when n equals zero).  str is a character variable that will
C       contain the requested command-line argument, padded with blanks on
C       the end.
      INTEGER J
       J = IGETARG(I, ARG, LEN(ARG))],
[      CALL GETARG(I, ARG)])
      END
C     
C     =====================================================
      SUBROUTINE GETREF(KERFLG,NREAD,NSPOTS,DYNAM,MAXOMITL)
C     =====================================================
C
C     [This has been abtracted from ABSCALE because of the BYTE
C     declaration.]
C
C        implicit none
C     
C     
C     
C     
C     
C     Read one reflection into common /IND/, skipping unmeasured reflections
C     Return 1 if end of file or all N spots found
C     Both integrated and profile fitted I's and SD's are stored, one in
C     INTT,SD and the other in INTT2,SD2. The values in INTT,SD are used
C     in scaling, and this is chosen on input card 1 to be either the 
C     integrated or profile fitted value.
C     
C
C This routine is probably VAX specific in its unpacking of indices
C
C
C
C---- IC format generate file variables
C
C
C
C     .. Scalar Arguments ..
      INTEGER           NREAD,NSPOTS,KERFLG,MAXOMITL
      LOGICAL DYNAM
C     ..
C     .. Scalars in Common ..
      INTEGER           IREC,IX,IY,JGUNIT,JH,JK,JL,MND
      LOGICAL           PROFILE
C     ..
C     .. Arrays in Common ..
      REAL              SPACER(12)
      INTEGER           INTT(3),INTT2(3),ISD(3),ISD2(3),JUNK(2)
C     ..
C     .. Local Scalars ..
      INTEGER           I,ICOL,ICOL2,IER,I4INTS,I4INTP
      BYTE              IR,IM
C     ..
C     .. Local Arrays ..
cejd      INTEGER*2         IBUF(18)
      INTEGER*2         IBUF(19)
      BYTE              B(2)
C     ..
C     .. External Subroutines ..
      EXTERNAL          QREAD
C     ..
C     .. Common blocks ..
       LOGICAL BRIEF
       INTEGER IBRIEF
       COMMON /BRF/ BRIEF,IBRIEF
      COMMON      /IND/JH,JK,JL,MND,JUNK,IX,IY,SPACER,INTT,ISD,
     +            INTT2,ISD2
      COMMON      /INREC/JGUNIT,IREC
      COMMON      /INTTYP/PROFILE
C     ..
C     .. Equivalences ..
      EQUIVALENCE       (B(1),IBUF(4)), (B(1),IR), (B(2),IM)
      EQUIVALENCE       (I4INTS,IBUF(7)),(I4INTP,IBUF(13))
C     ..
      SAVE
C
C
          KERFLG = 0
C
C
   10 CONTINUE
      NREAD = NREAD + 1
C
C
      IF (NREAD.GT.NSPOTS) THEN
          GO TO 40
      ELSE
C
C              *************************
          CALL QREAD(JGUNIT,IBUF,36,IER)
C              *************************
C
          IREC = IREC + 1
          IF (IER.NE.0) THEN
              GO TO 30
C
C---- If rejected, skip to next refl
C
CAL ALLOW IR TO HAVE VALUES 5,6
          ELSE IF ((IR.NE.0).AND.(IR.LE.4)) THEN
              GO TO 10
          END IF
      END IF
C
C
      JH = IBUF(1)
      JK = IBUF(2)
      JL = IBUF(3)
      MND = IM
      IF (MND.LT.0) MND = 8
      IX = IBUF(5)
      IY = IBUF(6)
C
C---- A film intensity in ibuf(7) for integrated intensities or
C     ibuf(13) for profile fitted intensities
C
      IF (PROFILE) THEN
          ICOL = 13
          ICOL2 = 7
      ELSE
          ICOL = 7
          ICOL2 = 13
      END IF
C
C
      DO 20 I = 1,3
          IF (DYNAM) THEN
           ISD(I) = IBUF(ICOL+2)
           ISD2(I) = IBUF(ICOL2+2)
           IF (PROFILE) THEN
             INTT(I) = I4INTP
             INTT2(I) = I4INTS
           ELSE
             INTT(I) = I4INTS
             INTT2(I) = I4INTP
           END IF
          ELSE
           INTT(I) = IBUF(ICOL)
           ISD(I) = IBUF(ICOL+1)
           INTT2(I) = IBUF(ICOL2)
           ISD2(I) = IBUF(ICOL2+1)
          END IF
C
C---- Test for badspots (isd=-9999) change to unmeasured
C     this will also reject overloaded reflections
C-AL   Change this so overloads are rejected (and counted) in RDREF
C
       IF ( (ISD(I)   .EQ. -9999) .AND.
     +      (INTT(I)  .NE. MAXOMITL) )       INTT(I) = -9999
       IF ( (ISD2(I)  .EQ. -9999) .AND.
     +      (INTT2(I) .NE. MAXOMITL) ) 
     +                                     INTT2(I) = -9999
C
C
          ICOL = ICOL + 2
          ICOL2 = ICOL2 + 2
   20     CONTINUE
      RETURN
   30 KERFLG = -1
      RETURN 
   40 KERFLG = -1
      RETURN
C
C
      END
C_BEGIN_CCPSPW
      SUBROUTINE CCPSPW(STRING)
C     =========================
C
C     Spawns a new process to run shell command
C
C Arguments:
C ==========
C
C  STRING (I)   CHARACTER*(*): string containing command
C_END_CCPSPW
C
       CHARACTER STRING*(*)
       EXTERNAL SYSTEM
       CALL SYSTEM(STRING)
       END
C
      SUBROUTINE CEXIT (ICODE)
C     trivial interface to system-dependent EXIT routine
      INTEGER ICODE
dnl * for AIX we need an underscore to get the fortran-callable one
dnl   according to an item on the net, but that doesn't work in xlf with
dnl   AIX 2.3.  The EXIT here does seem to be a Fortran one which 
dnl   flushes the o/p buffers.  This may be different in more recent 
dnl   versions where the exit here might end up calling the libc one.
ifelse(_AIX,1,
[      CALL EXIT (%VAL(ICODE))
],
_hpux,1,
[C     The HP-UX fortran manual implies CALL EXIT flushes buffers
C     but it seems not to.  This works in HP-UX A.09.01.
      CALL F77EXIT (ICODE)
],
[      CALL EXIT (ICODE)
])dnl
      END

dnl * f2c needs bit-twiddling stuff (also in library.c)

ifelse(_f2c,1,
[      INTEGER FUNCTION IEOR (I,J)
      INTRINSIC XOR
      IEOR = XOR (I,J)
      END
      INTEGER FUNCTION IOR (I,J)
      INTRINSIC OR
      IOR = OR (I,J)
      END
      INTEGER FUNCTION IAND (I,J)
      INTRINSIC AND
      IAND = AND (I,J)
      END
])
dnl * Dummy GL stubs iff not SGI
ifelse(_sgi,1,,
[        subroutine gdummy
        character *(*) char_dummy
        entry  qreset
          return
        entry  reshap
          return
        entry  qdevic(keybd)
          return
        entry  winope(char_dummy,i0)
          return
        entry  keepas(i1,i2)
          return
        entry  draw2i(i3,i4)
          return
        entry  move2i(i5,i6)
          return
        entry  loadma(i7)
          return
        entry  gconfi
          return
        entry  mmode(i8)
          return
        entry  foregr
          return
        entry  getval(i9)
          return
        entry  color(i10)
          return
        entry  getsiz(r1,r2)
          return
cc        entry  clear this is in somewhere else in -ltermcap
cc          return
        entry  ortho2(r3,r4,r5,r6)
          return
        entry  getori(r7,r8)
          return
        end
        subroutine clear
        end
])dnl

C
CA dummy function for Unix
C     =========================
       CHARACTER FUNCTION RTNBKS()
C     =========================
C
C RTNBKS - Returns a Backslash for nt as Unix compilers are fussy!
C
C Input:     none
C
C Returns:   \ if WIN32 or not if Unix or vms
C
C Arguments: none
C
C Usage:     RTNBKS ()
C
      RTNBKS=' '
C
      END

c     ============================
      subroutine hciftime(ciftime)
c     ============================
ccFrom GERARD@XRAY.BMC.UU.SE Thu Sep 24 00:25:25 1998
c
      implicit none
c
      character ciftime*(*)
c
      integer gmt_hour,gmt_minutes,localdaymonth,
     +        localhours,localminutes,localmonth,localseconds,
     +        localyear,nhours,nminutes,stime,diff
c
      character gmt_diff*1
c
      integer gmtarray(9),tarray(9)
      integer time
c
      intrinsic abs
c
code ...
c
c ... check if the argument can hold 25 characters
c     (better to return an error flag, of course ;-)
c
dnl * len is quoted since also m4 macro 
      if ([len](ciftime) .lt. 25) then
        print *,'error --- hciftime: string too short'
        ciftime = ' '
        return
      end if
c
      stime = time()
      call gmtime(stime,gmtarray)
dnl * for AIX - ltime is not resolved on linking - only ltime_ exists
dnl   in the Fortran library.
ifelse(_AIX,1,
[      call ltime_(stime,tarray)
],
[      call ltime(stime,tarray)
])dnl
c
      nminutes = gmtarray(2)
      nhours = gmtarray(3)
      localseconds = tarray(1)
      localminutes = tarray(2)
      localhours = tarray(3)
      localdaymonth = tarray(4)
      localmonth = tarray(5) + 1
c .. tarray(6) should be years since 1900 so is Y2K-compliant
      localyear = tarray(6) + 1900
c
c ... calculate time difference in minutes (some time zones
c     differ by N hours + 30 minutes from gmt)
c
      diff = (60*localhours + localminutes) -
     +       (60*nhours + nminutes)
c
c ... allow for different dates to avoid Kim's midnight bug
c     (fudge by simply checking if the day of the month is
c     identical or not; should be okay)
c
      if (diff .lt. 0 .and. tarray(4) .ne. gmtarray(4)) then
        diff = diff + 24*60
      else if (diff .gt. 0 .and. tarray(4) .ne. gmtarray(4)) then
        diff = diff - 24*60
      end if
c
c ... get hour differences by taking INT(minutes)/60
c     since INT(-1.5) would be -2, use ABS and adjust sign
c
      gmt_hour = abs(diff) / 60
      if (diff .lt. 0) gmt_hour = - gmt_hour
      gmt_minutes = diff - 60*gmt_hour
      if (gmt_hour .lt. 0 .or. gmt_minutes .lt. 0) then
        gmt_diff = '-'
      else
        gmt_diff = '+'
      end if
c
      write (ciftime,fmt=6000) localyear,localmonth,localdaymonth,
     +  localhours,localminutes,localseconds,gmt_diff,abs(gmt_hour),
     +  abs(gmt_minutes)
c
c ... NOTE: "i4" in the following format makes that this routine
c           is not Year-10,000-compliant !!!
c
 6000 FORMAT (i4,'-',i2.2,'-',i2.2,'T',i2.2,':',i2.2,':',i2.2,a1,i2.2,
     +       ':',i2.2)
c
      return
      end

C
