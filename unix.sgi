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
