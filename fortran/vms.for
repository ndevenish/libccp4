C
C   ---------------- vms.for --------------------
C   replaces vmssupport.for and is replaced by unix.for 
C   for unix systems.
C
C  From: P.J. Daly <pjd@uk.ac.dl.cxa>
C  Date: Thu, 17 Oct 91 09:57:09 +0100
C
C
C VMS.FOR
C =======
C
C UGTENV - Get value of env. variable
C UGERR  - Get error explanation
C UGTIUD - Get user id - it's name
C UIDATE - Get date in 3 integer format
C UTIME  - Get current time
C USTIME - Get absolute time in seconds (unused in VMS)
C UCPUTM - Get CPU time
C UISATT - Is file a terminal?
C URENAM - Rename file
C VAXVMS - Logical function returns TRUE if VAX/VMS
C UBYTES - Returns number of bytes per word and 'words'/'bytes'
C          to indicate if byte handling is available
C CCPERR - Signal VMS that serious error occurred and must not continue
C GETPID - Get unique process id.
C USTENV - Create logical name.
C NOCRLF - write line supressing cr/lf
C
C
C     ================================                         
      SUBROUTINE UGTENV(NAMENV,VALENV)
C     ================================
C
C UGTENV - Get value of env. variable
C
C Input:  NAMENV - Logical Name
C
C Output: VALENV - It's value
C
C Arguments: CHARACTER*(*) NAMENV, VALENV
C
C Usage:     CALL UGTENV(NAMENV, VALENV)
C
      CHARACTER*(*) NAMENV,VALENV
C
      INCLUDE '($LNMDEF)'
      INCLUDE '($SSDEF)'
C
      INTEGER       LN,LENGTH
      INTEGER*4     ITEMLIST(4),SYS$TRNLNM
      INTEGER*2     NAME_LEN_CODE(2)
C
C---- Equivalences
C
      EQUIVALENCE (NAME_LEN_CODE(1),ITEMLIST(1))
C                                  
      VALENV = ' '
      LN = LENSTR(NAMENV)
      IF (LN.LE.0) RETURN
C
C---- Setup item list for routine
C
      NAME_LEN_CODE(1) = LEN(VALENV) ! Length of buffer
      NAME_LEN_CODE(2) = LNM$_STRING ! item code for returning equivalence name
      ITEMLIST(2) = %LOC(VALENV)     ! Address to return equivalence name
      ITEMLIST(3) = %LOC(LENGTH)     ! Address to return name length
      ITEMLIST(4) = 0                ! terminator
C
C
10    IERR=SYS$TRNLNM(LNM$M_CASE_BLIND,'LNM$DCL_LOGICAL',
     .    NAMENV(1:LN),,ITEMLIST)
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
C Input:  STATUS - Error number (if negative then print error message)
C
C Output: ERRSTR - Error message string
C
C Arguments: INTEGER       STATUS
C            CHARACTER*(*) ERRSTR
C
C Usage:     CALL UGERR(STATUS, ERRSTR)
C                                                    
      INTEGER       STATUS,IPRINT,ISTAT
      CHARACTER*(*) ERRSTR
      INTEGER       ISTART,IEND,COND,IFLAGS,IRET
C
C
C---- IFLAGS masks out irrelevant parts if the error message
C
      IFLAGS = 13
C
C---- Set up print option
C
      IPRINT = 0                                  
      IF (STATUS .LT. 0) THEN
        IPRINT = 1
        STATUS = -STATUS
      ENDIF
C
C---- Remember STATUS because a call to ERRSNS destroys it !
C
      ISTAT = STATUS
C
C---- Get error message from system
C
      CALL ERRSNS (STATUS,,,,COND)
C
C---- Translate it
C                
      IRET = LIB$SYS_GETMSG(COND,ILEN,ERRSTR,IFLAGS)
C
C---- If not a fortran error then get system error instead
C
      IF (IRET .EQ. SS$_MSGNOTFND) 
     +    IRET = LIB$SYS_GETMSG(ISTAT,ILEN,ERRSTR,IFLAGS)
C
C---- Remove rubbish
C
      ISTART = INDEX(ERRSTR,' ') + 1
      IEND   = INDEX(ERRSTR,'!') + 1
      ERRSTR = ERRSTR(ISTART:IEND)
C
C---- Print result if appropriate
C
      IF (IPRINT.EQ.1) THEN
        WRITE (6,100) ISTAT,ERRSTR
100     FORMAT (' OS error: ',I5,' Message: ',A)
      ENDIF
C
      END
C
C
C     ===========================
      SUBROUTINE UGTUID(USERNAME)
C     ===========================
C
C UGTUID - Get user ID
C
C Input:  none
C
C Output: UID - user ID string
C               
C Arguments: CHARACTER*(*) UID
C
C Usage:     CALL UGTUID(UID)
C
      CHARACTER*(*) USERNAME
C
      INCLUDE '($JPIDEF)'
C
      CALL LIB$GETJPI(JPI$_USERNAME,,,,USERNAME)
C
      END
C
C
C     =================================
      SUBROUTINE UIDATE(MONTH,DAY,YEAR)
C     =================================
C
C UIDATE - Get date in 3 integer format
C
C Input:  none
C
C Output: MONTH,DAY,YEAR
C
C Arguments: INTEGER MONTH, DAY, YEAR
C
C Usage:     CALL UIDATE(MONTH, DAY, YEAR)
C
      INTEGER     MONTH,DAY,YEAR
C
      CALL IDATE(MONTH,DAY,YEAR)
      YEAR = MOD (YEAR, 100)
C
      END
C
C
C     =======================
      SUBROUTINE UTIME(CTIME)
C     =======================
C
C UTIME - Get current time  as hh:mm:ss
C
C Input:  none
C         
C Output: TIME - as ASCII string
C 
C Arguments: CHARACTER*(*) TIME
C
C Usage:     CALL UTIME(TIME)
C
      CHARACTER*(*) CTIME
C
      CALL TIME(CTIME)
C
      END
C
C
C     ======================
      SUBROUTINE USTIME(ISEC)         
C     ======================
C
C USTIME - Get absolute time in seconds. (returns with -1 under VMS)
C
C Input:  none
C
C Output: SEC
C
C Arguments: INTEGER SEC
C
C Usage:     CALL USTIME(SEC)
C
      INTEGER ISEC            
C
      ISEC = -1
C
      END
C
C
C     ======================
      SUBROUTINE UCPUTM(SEC)
C     ======================
C
C UCPUTM - Get CPU timE
C
C Input:  SEC = 0.0 will initialize timer, any other value reads cpu time.
C
C Output: SEC
C
C Arguments: REAL    SEC
C
C Usage:     CALL UCPUTM(SEC)
C
      REAL    SEC,ELAPS,SECNDS
      INTEGER IFLAG
C
      SAVE /ELAPS/
C
      DATA ELAPS/0.0/
C
      IF (SEC.LT.0.0001) THEN
        ELAPS = SECNDS(0.0)
      ELSE
        SEC = SECNDS(ELAPS)
      ENDIF
C
      END
C
C
C     ==============================
      SUBROUTINE UISATT(FLUN,ANSWER)
C     ==============================
C
C UISATT - This function determines whether a program is being run 
C          on-line if this information is available.
C
C Input:  FLUN - Fortran Unit Number
C
C Output: ANS - 1 for on-line, 0 otherwise
C
C Arguments: INTEGER FLUN, ANS
C
C Usage:     CALL UISATT (FLUN,ANS)
C
      INTEGER   FLUN,ANSWER
      INTEGER*2 LENGTH,CODE,ITEMLIST(8),RLN
      INTEGER   BUFADDR,RLNADDR,SYS$GETJPI
      CHARACTER ERRSTR*100
C
C---- Equivalences
C
      EQUIVALENCE (ITEMLIST(1),LENGTH),(ITEMLIST(2),CODE),
     .  (ITEMLIST(3),BUFADDR),(ITEMLIST(5),RLNADDR),(ITEMLIST(7),JEND)
      INTEGER BUF(5)
      INCLUDE '($JPIDEF)'
C
C---- Set up item list
C
      LENGTH=20                 ! Length of return buffer in bytes
      CODE=JPI$_TERMINAL        ! Code for information required ( = '31D'X)
      BUFADDR=%LOC(BUF)         ! Address of return buffer
      RLNADDR=%LOC(RLN)         ! Address to receive length of returned 
      JEND=0                    ! Terminator of item list
      I=SYS$GETJPI(,,,%REF(LENGTH),,,)
      ANSWER = 0
C
C---- Set mode. Length of information = 0 if in batch
C
      IF (RLN.NE.0) ANSWER = 1
C
      END
C
C
C     =========================
      LOGICAL FUNCTION VAXVMS()
C     =========================
C
C VAXVMS - Operating Sytem in use returns .TRUE. if VAXVMS
C
C Input:  none
C
C Returns: .TRUE. for VAXVMS, .FALSE. otherwise
C
C Arguments: none
C
C Usage:     VAXVMS ()
C
      VAXVMS = .TRUE.
C
      END
C
C
C     ===============================
      SUBROUTINE UBYTES (INUM,STRING)
C     ===============================
C
C UBYTES - Return statistics about byte handling
C
C Input:  none
C
C Output: INUM - number of bytes per word
C         HANDLE - 'WORDS' or 'BYTES'
C
C Arguments: INTEGER     INUM
C            CHARACTER*5 HANDLE
C
C Usage:     CALL UBYTES (INUM,HANDLE)
C
      INTEGER INUM
      CHARACTER STRING*5
C
      INUM = 4
      STRING = 'WORDS'
C
      END
C
C
C     =====================================
      SUBROUTINE URENAM(NAME1,NAME2,STATUS)
C     =====================================
C
C URENAM - Rename file assigned to NAME1 to NAME2. 
C
C Input:  NAME1, NAME2   the file names
C
C Output: STATUS       =0 if ok <>0 if an error
C
C Arguments: CHARACTER*(*) NAME1, NAME2
C            INTEGER       STATUS
C
C Usage:     CALL URENAM (NAME1,NAME2,STATUS)
C
      INCLUDE '($SSDEF)'
C
      INTEGER       STATUS
      CHARACTER*(*) NAME1,NAME2
C
C---- Rename file
C
      STATUS = LIB$RENAME_FILE (NAME1,NAME2)
      IF (STATUS .EQ. SS$_NORMAL) STATUS = 0
C
      END
C
C
C     ============================
      SUBROUTINE CCPERR(ISTAT,MSG)
C     ============================
C
C CCPERR - Return serious error to OS and stops processing completely.
C
C Input:  none
C
C Output: none
C
C Arguments: ISTAT, MSG
C
C Usage:     CALL CCPERR
C
      INTEGER ISTAT
      CHARACTER*(*) MSG
C
      WRITE (6, 100) MSG(1:LENSTR(MSG))
100   FORMAT (' ',A)      
      CALL EXIT(ISTAT)
C
      END
C
C
C
C     ==================
      SUBROUTINE INITFYP
C     ==================
C
C---- Initialise the CRTL routines and parse CLI argument.
C
C     .. Parameters ..
      INTEGER MAXLEN,MAXPAR
      PARAMETER (MAXLEN=70,MAXPAR=10)
C     ..
C     .. Scalars in Common ..
      INTEGER IARG
C     ..
C     .. Arrays in Common ..
      CHARACTER ARGNAM* (MAXLEN)
C     ..
C     .. Local Scalars ..
      INTEGER I,J,K,L,LENARG,ISTAT
      CHARACTER CLIARG*700,NAME*200
C     ..
C     .. External Subroutines ..
      EXTERNAL LIB$GET_FOREIGN,VAXC$CRTL_INIT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX
C     ..
C     .. Common blocks ..
      COMMON /ARGCOUNT/IARG
      COMMON /ARGS/ARGNAM(MAXPAR)
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
      DATA IARG/-1/
C     ..
      INCLUDE '($SSDEF)'
      INCLUDE '($JPIDEF)'
C
      CALL VAXC$CRTL_INIT
C
C---- Get command line
C
      IARG = 0
      DO 5 J = 1, MAXPAR
        ARGNAM(J) = ' '
5     CONTINUE
C
C---- get process name and use for argv[0]
C
      CALL LIB$GETJPI(JPI$_PRCNAM,,,,ARGNAM(1))
C
      CALL LIB$GET_FOREIGN(CLIARG,,LENARG)
C
C---- Split command line into arguments.
C
      IF (LENARG.GT.0) THEN
        J = 1
   10   CONTINUE
        K = INDEX(CLIARG(J:LENARG),' ')
        IF (K.EQ.0) THEN
          K = LENARG
        ELSE
          K = J + K - 2
        END IF
        IARG = IARG + 1
        IF (IARG.EQ.MAXPAR) RETURN
        ARGNAM(IARG + 1) = CLIARG(J:K)
        DO 20 J = K + 2,LENARG
          IF (CLIARG(J:J).NE.' ') GO TO 10
   20   CONTINUE
      END IF
C
      END
C
C
C
C     ==============================
      INTEGER FUNCTION IARGC(IDUMMY)
C     ==============================
C
C---- Return number of Command line arguments
C
C     ..
C     .. Scalar Arguments ..
      INTEGER IDUMMY
C     ..
C     .. Scalars in Common ..
      INTEGER IARG
C     ..
C     .. External Subroutines ..
      EXTERNAL INITFYP
C     ..
C     .. Common blocks ..
      COMMON /ARGCOUNT/IARG
C     ..
C     .. Save statement ..
      SAVE
C     ..
      IF (IARG.EQ.-1) CALL INITFYP
      IARGC = IARG
C
      END
C
C
C
C     ============================
      SUBROUTINE GETARG(INUM,LINE)
C     ============================
C
C---- Get INUM'th command line argument or ' ' into LINE
C
C     .. Parameters ..
      INTEGER MAXLEN,MAXPAR
      PARAMETER (MAXLEN=70,MAXPAR=10)
C     ..
C     .. Scalar Arguments ..
      INTEGER INUM
      CHARACTER LINE* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER IARG
C     ..
C     .. Arrays in Common ..
      CHARACTER ARGNAM* (MAXLEN)
C     ..
C     .. External Subroutines ..
      EXTERNAL INITFYP
C     ..
C     .. Common blocks ..
      COMMON /ARGCOUNT/IARG
      COMMON /ARGS/ARGNAM(MAXPAR)
C     ..
C     .. Save statement ..
      SAVE
C     ..
      IF (IARG.EQ.-1) CALL INITFYP
      IF (INUM.LT.0 .OR. INUM.GT.IARG) THEN
        LINE = ' '
      ELSE
        LINE = ARGNAM(INUM + 1)
      END IF
C
      END
C
C
C
C     ============================
      INTEGER FUNCTION GETPID(IDUMMY)
C     ============================
C
C     Get process ID
C
      IMPLICIT NONE
      INTEGER PID,IDUMMY
C
      INTEGER ISTAT
C
      INTEGER SYS$GETJPI
C
      INCLUDE '($SSDEF)'
C
      PID = 0
      ISTAT = SYS$GETJPI(,PID,,0,,,)
      IF (ISTAT.NE.SS$_NORMAL) CALL LIB$SIGNAL(%VAL(ISTAT))
      GETPID = PID
C
      END
C
C
C
C     ===============================
      SUBROUTINE USTENV(LINE,IRESULT)
C     ===============================
C
C     Create logical name assignment LOGNAM --> FILNAM
C
      IMPLICIT NONE
      INTEGER IRESULT
      CHARACTER LOGNAM*80,FILNAM*200,LINE*(*)
C
      STRUCTURE /ITMLST/
        UNION
          MAP
            INTEGER*2 LB,IC
            INTEGER*4 IA,IR
          ENDMAP
          MAP
            INTEGER*4 IE
          ENDMAP
        ENDUNION
      ENDSTRUCTURE
C
      RECORD /ITMLST/ ITM(2)
C
      INTEGER ISTAT,ILEN
C
      INTEGER LENSTR,SYS$CRELNM
C
      INCLUDE '($SSDEF)'
      INCLUDE '($LNMDEF)'
C
      IRESULT = 0
      ILEN = INDEX(LINE, '=')
      IF (ILEN.EQ.0) THEN
        IRESULT = 1 
        RETURN
      ENDIF
      LOGNAM = LINE(1: ILEN - 1)
      FILNAM = LINE(ILEN + 1:)
      ITM(1).LB = LENSTR(FILNAM)
      ITM(1).IC = LNM$_STRING
      ITM(1).IA = %LOC(FILNAM)
      ITM(2).IE = 0
      ISTAT = SYS$CRELNM(,'LNM$PROCESS',LOGNAM(:LENSTR(LOGNAM)),,ITM)
      IF (ISTAT.NE.SS$_NORMAL) IRESULT = 2
C
      END
C
C
C
C     ========================
      SUBROUTINE NOCRLF (LINE)
C     ========================
C
C---- Output a line supressing cr/lf
C
      CHARACTER*(*) LINE
C
      WRITE (6,99) LINE
99    FORMAT ('+',A,$)
C
      END
C
C
C
      INTEGER FUNCTION SRTBEG(NKEYS,KEYB,LRECL,MEMSIZE)
C     *************************************************
      IMPLICIT NONE
C     # of keys, key descriptions, fixed record length, memory (not used)
      INTEGER   NKEYS,KEYB(*),LRECL,MEMSIZE
C
      INTEGER      ISTAT,NORMAL,DATASIZ,LUNOUT,NFILSZ
      INTEGER      I,J,JOLD
      INTEGER*2    KEYBUF(401)
C
C    .. External Functions ..
      INTEGER  SOR$BEGIN_SORT
C
C     Definition of data type = single-precision floating - only this one
C     is implemented here
      EXTERNAL DSC$K_DTYPE_F
C
C   Things for descriptor of ADATA
      INTEGER*4    MDATA(2)
      INTEGER*2    M2DATA(4)
      EQUIVALENCE  (MDATA(1),M2DATA(1))
      COMMON /BISRTC/MDATA,NORMAL,DATASIZ,LUNOUT
C
      SAVE KEYBUF
C
C
C---- Set up key buffer for number of keys. A descriptor consists of:
C     2-byte length (in bytes)
C Each following "block" contains:
C     2-byte data type (here real)
C     2-byte ascending (0) or descending (1) order
C     2-byte relative address of key in record (from 0)
C     2-byte key length in bytes
C
C     NORMAL return value from VMS sort subroutines
      NORMAL = 1
C     Length of tada type i.e. 4 for REAL
      DATASIZ = 4
      KEYBUF(1) = NKEYS
      DO 10 I = 1,NKEYS
         J = (I-1)*4 + 2
         JOLD = (I-1)*5 + 1
C
C---- Sort Data Type
C
         IF (KEYB(JOLD).NE.7) THEN
            WRITE (LUNOUT,FMT=6010) ISTAT
 6010       FORMAT (' SRTBEG only REAL data type implemented')
            STOP
         END IF
         KEYBUF(J)=%LOC(DSC$K_DTYPE_F)
C
C---- Sort Order ascending/descending
C
         KEYBUF(J+1) = KEYB(JOLD+1)
C
C---- position of 1st byte in key
C
         KEYBUF(J+2) = KEYB(JOLD+2)
C
C---- keylength in BYTES
C
         KEYBUF(J+3) = DATASIZ * KEYB(JOLD+3)
         IF (KEYB(JOLD+4).NE.0) THEN
            WRITE (LUNOUT,FMT=6011)
 6011       FORMAT(' SRTBEG - on VMS MASK fields must be .EQ. 0')
            STOP
         ENDIF
 10   CONTINUE
C
C
C     Make string descriptors for data record
C     A descriptor consists of:
C     2-byte length (in bytes)
C     2-byte class & type (probably not used)
C     4-byte address of array
C     Note MDATA is equivalenced to M2DATA
C     length of array
      M2DATA(1)=LRECL
C     class = 1, type = 0 - never used but must be present
      M2DATA(2)='100'X
C     address of array  - filled in SRTRLS & SRTRET
C
C---- Initialise sort, set parameters, etc
C
C    ******************************
      NFILSZ = 0
      ISTAT = SOR$BEGIN_SORT(KEYBUF,LRECL,,NFILSZ,,,,,)
C    ******************************
C
      IF (ISTAT.NE.NORMAL) THEN
         WRITE (LUNOUT,FMT=6008) ISTAT
 6008    FORMAT (' Sort fail : BEGIN, status=',Z9)
C
         CALL LIB$STOP(%VAL(ISTAT))
      END IF
      SRTBEG = 0
      RETURN
      END
C
C
C
      INTEGER FUNCTION SRTRLS(RECORD)
C     *******************************
      IMPLICIT NONE
      REAL  RECORD(*)
C
      INTEGER ISTAT,NORMAL,DATASIZ,LUNOUT
C
      INTEGER  SOR$RELEASE_REC
C
C     MDATA is descriptor (ie indirect address) of RECORD
C     Things for descriptor of ADATA
      INTEGER*4    MDATA(2)
      INTEGER*2    M2DATA(4)
      EQUIVALENCE  (MDATA(1),M2DATA(1))
      COMMON /BISRTC/MDATA,NORMAL,DATASIZ,LUNOUT
C
      MDATA(2)=%LOC(RECORD)
C     *************
      ISTAT = SOR$RELEASE_REC(MDATA)
C     *************
C
C     IF (ISTAT.EQ.0) THEN
      IF (ISTAT.EQ.NORMAL) THEN
         SRTRLS = 0
      ELSE
         WRITE (LUNOUT,FMT=6010) ISTAT
 6010    FORMAT (' Sort fail : SRTRLS, status=',Z9)
C
         CALL LIB$STOP(%VAL(ISTAT))
         STOP
      END IF
      RETURN
      END
C
C
C
      INTEGER FUNCTION SRTMRG()
C     *************************
      IMPLICIT NONE
C
      INTEGER  ISTAT,NORMAL,DATASIZ,LUNOUT
      INTEGER  SOR$SORT_MERGE
C
C     MDATA is descriptor (ie indirect address) of RECORD
C     Things for descriptor of ADATA
      INTEGER*4    MDATA(2)
      INTEGER*2    M2DATA(4)
      EQUIVALENCE  (MDATA(1),M2DATA(1))
      COMMON /BISRTC/MDATA,NORMAL,DATASIZ,LUNOUT
C
C     ********
      ISTAT = SOR$SORT_MERGE()
C     ********
C
C     IF (ISTAT.NE.0) THEN
      IF (ISTAT.NE.NORMAL) THEN
         WRITE (LUNOUT,FMT=6014) ISTAT
 6014    FORMAT (' Sort fail : MERGE, status=',Z9)
         CALL LIB$STOP(%VAL(ISTAT))
         STOP
      ENDIF
      SRTMRG = 0
      RETURN
      END
C
C
C
      INTEGER FUNCTION SRTRET(RECORD)
C     *******************************
      IMPLICIT NONE
C     record array
      REAL RECORD(*)
C
      INTEGER ISTAT,NORMAL,DATASIZ,LUNOUT
C
      INTEGER  SOR$RETURN_REC
      INTEGER  SOR$END_SORT
      EXTERNAL SS$_ENDOFFILE
C  NRL not used, but still present (the length, which is already known)
      INTEGER*2  NRLVMS
C
C     MDATA is descriptor (ie indirect address) of RECORD
C     Things for descriptor of ADATA
      INTEGER*4    MDATA(2)
      INTEGER*2    M2DATA(4)
      EQUIVALENCE  (MDATA(1),M2DATA(1))
      COMMON /BISRTC/MDATA,NORMAL,DATASIZ,LUNOUT
C
C     set record address
      MDATA(2)=%LOC(RECORD)
C     *************
      ISTAT = SOR$RETURN_REC(MDATA,NRLVMS)
C     *************
C
      IF (ISTAT.EQ.%LOC(SS$_ENDOFFILE)) THEN
         ISTAT = SOR$END_SORT()
         IF(ISTAT.NE.NORMAL) THEN
            WRITE(6,1005) ISTAT
 1005       FORMAT(' Sort fail : END ,status=',Z9)
            CALL LIB$STOP(%VAL(ISTAT))
         ENDIF
         SRTRET = -1
         RETURN
      ELSE IF (ISTAT.NE.NORMAL) THEN
         WRITE(6,1006) ISTAT
 1006    FORMAT(' Sort fail : RETURN, status='Z9)
         CALL LIB$STOP(%VAL(ISTAT))
         STOP
      ENDIF
      SRTRET = 0
      RETURN
      END
