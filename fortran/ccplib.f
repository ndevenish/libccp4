C      Routines originally from DL
C      CCPVRS    Print program version number and date header
C      CCPDAT    Get calendar date
C      CCPTIM    Get CPU and Elapsed times
C      CCPFIL    Return file name for an opened file
C      CCPONL    See if program is being run interactively
C      CCPOVP    See if overprinting is available
C      CCPPAG    Set paging parameters if available
C      CCPULI    See if underline option is available
C      CCPUFL    Supress underflow messages
C      CCPSUM    Sum the elements of an array
C      CCPDEX    Periodicity reduction of 1024 (for PROLSQ)
C      CCPRVR    Read variable length record of unknown length
C      CCPBYT    Indicate whether byte handling is available
C      CCPMDE    If byte handling available return nos. of bytes for map
C                modes
C      CCPMVB    Move bytes from one non-character array to another if
C                byte handling is available
C      CCPZBI    Sets an array of bytes to zero
C      CCPMVI    Move words from one non-character array to another using
C                a simple loop
C      CCPZI     Set 'n' words of an array to zero using a simple loop
C      CCPTOI    Convert n'th byte or I*2 in a non-character array to an
C                integer
C      CCPUPC    Convert a string to upper case
C
C      Routines of Phil Evans
C      CCPDPN    more friendly CCPOPN
C      CCPRNM    rename file
C      CCPASZ    set array size suitable for current working set
C      CCPUPC    make string upper-case
C      CCPEXS    test if file exists
C
C      Routines moved from DISKIO.FOR and PARSER.FOR(IJT 18-3-92)
C      QPRINT    write debug messages
C      QTYPE     returns machine type
C      LENSTR    length of string to last non-space
C
C
C
C
      SUBROUTINE CCFILL(ARR1,SCAL,NTIMES)
C     ===================================
C
C
C PARAMETERS
C ==========
C
C        ARR1 (I/O) ARRAY TO WHICH BYTES ARE TO BE COPIED
C        SCAL (I)   byte value to be copied into ARR1
C      NTIMES (I)   THE NUMBER OF BYTES TO BE COPIED
C
C     .. Scalar Arguments ..
      INTEGER NTIMES
      BYTE SCAL
C     ..
C     .. Array Arguments ..
      BYTE ARR1(*)
C     ..
C     .. Local Scalars ..
      INTEGER N
C     ..
      DO 10 N = 1,NTIMES
        ARR1(N) = SCAL
   10 CONTINUE
C
C
      END
C
C
C
      SUBROUTINE CCPASZ(A,JSIZE,MINSIZ)
C     =================================
C
C---- Set size of array A so that it remains in real memory
C
C---- On entry, JSIZE is dimension of array A
C               MINSIZ is the minimum size
C
C---- On exit:  JSIZE is the max dimension of A for an array which 
C               will be held in memory to reduce page faulting. 
C               If this info. is
C               not available just return JSIZE as input.
C
C
C----  On the Vax set JSIZE as dimension of array A set to the working
C      set size less a guessed program size, to minimise page faults in
C      using the array. Only the working set quota is used 
C      (not the extent)
C      MINSIZ  <= JSIZE <= initial JSIZE
C
C
C     .. Scalar Arguments ..
      INTEGER JSIZE,MINSIZ
C     ..
C     .. Array Arguments ..
      REAL A(*)
C     ..
      END
C
C
C
      LOGICAL FUNCTION CCPBYT(NBW)
C     ============================
C
C
C---- This function indicates whether byte handling is available or not.
C      if a value of .true. is returned then the subroutines ccpmde and
C      ccpmvb must be fully implemented.
C
C
C PARAMETERS
C ==========
C
C         NBW (O)   RETURNS THE NUMBER OF BYTES PER WORD OR A VALUE
C                   OF 1 IF NO BYTE HANDLING IS AVAILABLE.
C
C  RETURNS   CCPBYT  = .TRUE.  BYTE HANDLING AND ASSOCIATED CCPLIB
C                              ROUTINES AVAILABLE.
C                    = .FALSE. NO BYTE HANDLING AVAILABLE.
C
C ***DL CONVEX VERSION***
C
C     .. Scalar Arguments ..
      INTEGER NBW
C     ..
      CCPBYT = .TRUE.
      NBW = 4
      END
C
C
C
      SUBROUTINE CCPDAT(CALDAT)
C     =========================
C
C
C---- This subroutine returns the date if available
C
C PARAMETERS
C ==========
C
C      CALDAT (O)   CHARACTER*8 VARIABLE RETURNING DATE AS DD/MM/YY
C                   (RETURNED AS A BLANK STRING IF NOT AVAILABLE)
C
C
C
C
C SPECIFICATION STATEMENTS
C ------------------------
C
C
C---- Get date
C
C     .. Scalar Arguments ..
      CHARACTER CALDAT*8
C     ..
C     .. Local Scalars ..
      INTEGER ID,IM,IY
C     ..
C     .. External Subroutines ..
      EXTERNAL UIDATE
C     ..
C
C
      CALL UIDATE(IM,ID,IY)
      WRITE (CALDAT,FMT=6000) ID,IM,IY
C
C---- Format statements
C
 6000 FORMAT (I2,'/',I2,'/',I2)
C
C
      END
C
C
C
      SUBROUTINE CCPDEX(INDX,N)
C     ========================
C
C
C---- This subroutine performs a periodicity reduction for a period
C     of 1024 for the elements of an array. written particularly for
C     'prolsq' to allow for use of the 'and' function on the cray or 
C     'moveb' on the m28oh(iap). 
C      These are much faster than the mod function used in
C      the standard fortran77 version.
c
C PARAMETERS
C ==========
C
C        INDX (I/O) ARRAY HOLDING NUMBERS FOR PERIODICITY REDUCTION
C           N (I)   NO. OF ELEMENTS IN INDX
C
C EXAMPLE OF FUNCTIONS:
C
C FORTRAN77     INDX(I)=MOD(INDX(I),1024)+1
C CRAY-1S       INDX(I)=AND(INDX(I),1023)+1
C M280H(IAP)    CALL MOVEB(INDX(I),1,0,1,22)
C               INDX(I)=INDX(I)+1
C
C SPECIFICATION STATEMENTS AND CODE
C
C     .. Scalar Arguments ..
      INTEGER N
C     ..
C     .. Array Arguments ..
      INTEGER INDX(N)
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     ..
      DO 10 I = 1,N
        INDX(I) = MOD(INDX(I),1024) + 1
   10 CONTINUE
      END
C
C
C
      SUBROUTINE CCPDPN(IUN,LOGNAM,STATUS,TYPE,LREC,IFAIL)
C     ====================================================
C
C---- Calls CCPOPN to open a file, but with mnemonic arguments
C
C PARAMETERS
C ==========
C
C         IUN (I)   UNIT NUMBER
C      LOGNAM (I)   LOGICAL FILE NAME (UP TO 8 CHARACTERS)
C      STATUS (I)   FILE STATUS FLAG  'UNKNOWN'
C                    CHARACTER*(*)    'SCRATCH'
C                                     'OLD'
C                                     'NEW'
C                                     'READONLY'
C                                     'PRINTER'
C        TYPE (I)   FILE TYPE FLAG ='F', 'SEQUENTIAL' 'FORMATTED'
C                                  ='U', 'SEQUENTIAL' 'UNFORMATTED'
C                                  ='DF', 'DIRECT'     'FORMATTED'
C                                  ='DU', 'DIRECT'     'UNFORMATTED'
C        LREC (I)   RECORD LENGTH FOR DIRECT ACCESS FILE (NO. OF
C                   CHARACTERS FOR A FORMATTED FILE OR WORDS FOR
C                   AN UNFORMATTED FILE). NOT RELEVANT FOR A SEQUENTIAL
C                   FILE
C       IFAIL (I/O) ON INPUT:     =0, STOP ON OPEN FAILURE
C                                 =1, CONTINUE AFTER OPEN FAILURE
C                                      (only on file not found)
C                   ON OUTPUT:    UNCHANGED IF FILE OPEN OK
C                                 =-1, ERROR IN OPENING FILE
C
C
C I/O STATUS RETURNED IN 'IOS' IN COMMON 'CCPSTT' IF NEEDED
C
C
C     .. Scalar Arguments ..
      INTEGER IFAIL,IUN,LREC
      CHARACTER LOGNAM* (*),STATUS* (*),TYPE* (*)
C     ..
C     .. Local Scalars ..
      INTEGER ISTAT,ITYPE
C     ..
C     .. Local Arrays ..
      CHARACTER TYPES(4)*2,STATS(6)*8
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPOPN
C     ..
C     .. Data statements ..
      DATA STATS/'UNKNOWN','SCRATCH','OLD','NEW','READONLY','PRINTER'/
      DATA TYPES/'F','U','DF','DU'/
C     ..
C
C
      DO 10 ISTAT = 1,6
        IF (STATUS.EQ.STATS(ISTAT)) GO TO 20
   10 CONTINUE
      WRITE (6,FMT=6000) STATUS
      CALL CCPERR(1,' stop 6000 IN CCPLIB')
C
   20 DO 30 ITYPE = 1,4
        IF (TYPE.EQ.TYPES(ITYPE)) GO TO 40
   30 CONTINUE
      WRITE (6,FMT=6002) TYPE
      CALL CCPERR(1,' STOP 6002 IN CCPLIB')
C
C
   40 CALL CCPOPN(IUN,LOGNAM,ISTAT,ITYPE,LREC,IFAIL)
C
C---- Format statements
C
 6000 FORMAT (/' CCPDPN: illegal status : ',A)
 6002 FORMAT (/' CCPDPN: illegal type: ',A)
C
C
      END
C
C
C
      LOGICAL FUNCTION CCPEXS(NAME)
C     =============================
C
C---- Tests if file assigned to logical name NAME exists
C
C Returns CCPEXS  .true.  if file exists
C                 .false. if file does not exist
C
C
C     .. Scalar Arguments ..
      CHARACTER NAME* (*)
C     ..
C     .. Local Scalars ..
      LOGICAL EXIST
      CHARACTER*255 NAMFIL
C     ..
C
C---- Executive call (see unix.for or vms.for)
C
      NAMFIL = ' '
      CALL UGTENV(NAME,NAMFIL)
      IF (NAMFIL.EQ.' ') NAMFIL = NAME
C
      INQUIRE (FILE=NAMFIL,EXIST=EXIST)
      CCPEXS = EXIST
C
C
      END
C
C
C
      SUBROUTINE CCPFIL(IUN,LOGNAM,FNAM)
C     ==================================
C
C---- This subroutine returns the file/dataset name for a file on a
C      given unit number (/filename) if available
C
C
C PARAMETERS
C ==========
C
C         IUN (I)   FORTRAN UNIT NUMBER
C         LOGNAM (I)   LOGICAL FILE NAME
C        FNAM (O)   CHARACTER*60 VARIABLE RETURNING THE FILE NAME
C                   (RETURNS BLANK IF NOT AVAILABLE)
C
C---- If ccpopn uses the unit number only in opens then ccpfil should
C     only use the unit number. otherwise the logical file name 
C     may be used.
C
C
C
C     .. Scalar Arguments ..
      INTEGER IUN
      CHARACTER FNAM*(*),LOGNAM* (*), TMPNAM*255
C     ..
C
C
      TMPNAM = ' '
      CALL UGTENV(LOGNAM,TMPNAM)
      IF (TMPNAM.EQ.' ') TMPNAM = LOGNAM
       FNAM = TMPNAM
cc      INQUIRE (FILE=TMPNAM,NAME=FNAM)
C
C
      END
C
C
C     =================
      SUBROUTINE CCPFYP
C     =================
C
C---- Used to set up the environment in which the program runs
C     and then parse the command line arguments.
C
C	My changes to CCPFYP, although they fixed the bugs, do not
C work quite as Peter intended (or as the documentation says), because
C if the *.def filenames are given on the command line it looks in the
C current directory, rather than the home directory.  It would be better
C if at least the code and documentation were consistent on this point.
C The following version of CCPFYP fixes this (see also explanatory
C comments in code):
C
C	Ian Tickle
C
C
C==============================================================================
C
C---- The logic for locating the "environ.def" and "default.def" files
C     is as follows:
C
C     If the file is defined on the command line then
C       If a directory is specified then
C         Use the filename as is.
C       Else if the SYS$LOGIN or HOME variable is defined then
C         Use "SYS$LOGIN:filename" or "$HOME/filename".
C       Else
C         Use the filename as is (in current directory).
C     Else
C       If the CINCL variable is defined then
C         Use "CINCL:filename" or "$CINCL/filename".
C       Else if the SYS$LOGIN or HOME variable is defined then
C         Use "SYS$LOGIN:filename" or "$HOME/filename".
C       Else
C         Use the filename as is (in current directory).
C
C==============================================================================
C
C
C     .. Parameters ..
      INTEGER ILIMIT,ISTRLN,IENV
      PARAMETER (ILIMIT=150,ISTRLN=200,IENV=20)
C     ..
C     .. Local Scalars ..
      INTEGER EXEC,HELP,IARG,ICOUNT,IEND,IERR,II,ILOOP,IOSTAT,ISKIP,
     +        ISTART,IUNIT,JJ,LOOP,RDENVF,RDLOGF,IHELP,LREC,IFAIL
      LOGICAL DINIT,EINIT,IEOF,VAX
      CHARACTER ERRSTR* (ISTRLN),FILNAM* (ISTRLN),LINE* (ISTRLN),
     +          ENVFIL* (ISTRLN),LOGFIL* (ISTRLN),EXECF* (ISTRLN),
     +          LOGNAM* (ISTRLN),TEMP* (ISTRLN)
C     ..
C     .. Local Arrays ..
      CHARACTER ENAME(ILIMIT)* (IENV),ETYPE(ILIMIT)* (5),
     +          EXTN(ILIMIT)* (4)
C     ..
C     .. External Functions ..
      INTEGER IARGC,LENSTR
      LOGICAL VAXVMS
      CHARACTER FEXTN* (ISTRLN)
      EXTERNAL IARGC,LENSTR,VAXVMS,FEXTN
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPERR,CCPUPC,GETARG,INITFYP,QPRINT,SETENV,UGERR,UGTENV
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ICHAR,INDEX
C     ..
C     .. Save statement ..
      SAVE
C     .. Data statements ..
      DATA ICOUNT/0/,IHELP/1/,DINIT/.TRUE./,EINIT/.TRUE./,EXEC/0/,
     +     RDLOGF/0/,RDENVF/0/,ILOOP/1/,IUNIT/31/,
     +     LOGFIL/'default.def'/,ENVFIL/'environ.def'/
C     ..
      VAX = VAXVMS()
      CALL INITFYP
C
      IARG = IARGC()
C
C---- Process command line option switches first
C
   10 CONTINUE
      ISKIP = 1
      IF (ILOOP.LE.IARG) THEN
        CALL GETARG(ILOOP,LINE)
        CALL CCPUPC(LINE)
C
        IF (LINE(1:1).EQ.'-') THEN
          DO 20 II = 2,LENSTR(LINE)
            IF (LINE(II:II).EQ.'V' .OR. LINE(II:II).EQ.'H') THEN
              HELP = ILOOP + ISKIP
              IF (HELP.GT.IARG) THEN
                IHELP = 1
              ELSE
                CALL GETARG(HELP,TEMP)
                IHELP = ICHAR(TEMP(1:1)) - ICHAR('0')
                IF (IHELP.LT.0 .OR. IHELP.GT.9) IHELP = 1
              END IF
              ISKIP = ISKIP + 1
            ELSE IF (LINE(II:II).EQ.'N') THEN
              DINIT = .FALSE.
              EINIT = .FALSE.
            ELSE IF (LINE(II:II).EQ.'X') THEN
              EXEC = ILOOP + ISKIP
              IF (EXEC.GT.IARG) CALL CCPERR(1,'Use: -x filename')
              ISKIP = ISKIP + 1
            ELSE IF (LINE(II:II).EQ.'D') THEN
              RDLOGF = ILOOP + ISKIP
              IF (RDLOGF.GT.IARG) CALL CCPERR(1,'Use: -d filename')
              CALL GETARG(RDLOGF,LOGFIL)
              DINIT = .TRUE.
              ISKIP = ISKIP + 1
            ELSE IF (LINE(II:II).EQ.'E') THEN
              RDENVF = ILOOP + ISKIP
              IF (RDENVF.GT.IARG) CALL CCPERR(1,'Use: -e filename')
              CALL GETARG(RDENVF,ENVFIL)
              EINIT = .TRUE.
              ISKIP = ISKIP + 1
            ELSE
              CALL QPRINT(1,'Ignoring switch '//LINE(II:II))
            END IF
   20     CONTINUE
          ILOOP = ILOOP + ISKIP
          GO TO 10
        END IF
      END IF
C
C---- Set up debug level
C
      CALL QPRINT(IHELP,' ')
C
C---- Update argument list
C
      IF (EINIT) THEN
        II = -1
        IF (RDENVF.GT.0) THEN
          IF (VAX) THEN
            IF (INDEX(ENVFIL,':').GT.0 .OR. INDEX(ENVFIL,'[').GT.0)
     &      II = 0
          ELSE
            IF (INDEX(ENVFIL,'/').GT.0 .OR. INDEX(ENVFIL,'{').GT.0)
     &      II = 0
          ENDIF
        ELSE
          CALL UGTENV('CINCL',FILNAM)
          IF (FILNAM.NE.' ') THEN
            IF (VAX) THEN
              FILNAM = 'CINCL:'
              II = LENSTR(FILNAM)
            ELSE
              II = LENSTR(FILNAM)
              IF (FILNAM(II:II).NE.'/') THEN
                II = II + 1
                FILNAM(II:II)='/'
              ENDIF
            ENDIF
          ENDIF
        ENDIF
C
        IF (II.LT.0) THEN
          IF (VAX) THEN
            CALL UGTENV('SYS$LOGIN',FILNAM)
          ELSE
            CALL UGTENV('HOME',FILNAM)
          ENDIF
          IF (FILNAM.NE.' ') THEN
            IF (VAX) THEN
              FILNAM = 'SYS$LOGIN:'
              II = LENSTR(FILNAM)
	    ELSE
              II = LENSTR(FILNAM)
              IF (FILNAM(II:II).NE.'/') THEN
                II = II + 1
                FILNAM(II:II)='/'
              ENDIF
            ENDIF
          ELSE
            II = 0
          ENDIF
        ENDIF
        FILNAM(II+1:) = ENVFIL
        CALL QPRINT(2,'Opening file '//FILNAM(1:LENSTR(FILNAM)))
        IFAIL=0
        IXUNIT = IUNIT
        IF (IHELP.LT.2) IXUNIT = -IUNIT
        CALL CCPDPN (IXUNIT,FILNAM,'READONLY','F',LREC,IFAIL)
   30   CONTINUE
        READ (UNIT=IUNIT,FMT=6000,END=40,ERR=80,IOSTAT=IERR) LINE
        II = INDEX(LINE,'#')
        IF (II.EQ.1) THEN
          LINE = ' '
        ELSEIF (II.GT.1) THEN
          LINE = LINE(1:II-1)
        ENDIF
        IF (LINE.NE.' ') THEN
          ICOUNT = ICOUNT + 1
          IF (ICOUNT.GT.ILIMIT) CALL CCPERR(1,'No more string space')
          ISTART = INDEX(LINE,'=')
          IF (ISTART.EQ.0) CALL CCPERR(1,'No = in environment type')
          ENAME(ICOUNT) = LINE(1:ISTART-1)
          EXTN(ICOUNT) = '.'//FEXTN(LINE)
          IF (EXTN(ICOUNT).EQ.' ') CALL CCPERR(1,'error in extension')
          IEND = INDEX(LINE,EXTN(ICOUNT))
          ETYPE(ICOUNT) = LINE(ISTART+1:IEND-1)
        END IF
        GO TO 30
   40   CLOSE (UNIT=IUNIT)
      END IF
C
C---- Now get defaults file
C
      IF (DINIT) THEN
        II = -1
        IF (RDLOGF.GT.0) THEN
          IF (VAX) THEN
            IF (INDEX(LOGFIL,':').GT.0 .OR. INDEX(LOGFIL,'[').GT.0)
     &      II = 0
          ELSE
            IF (INDEX(LOGFIL,'/').GT.0 .OR. INDEX(LOGFIL,'{').GT.0)
     &      II = 0
          ENDIF
        ELSE
          CALL UGTENV('CINCL',FILNAM)
          IF (FILNAM.NE.' ') THEN
            IF (VAX) THEN
              FILNAM = 'CINCL:'
              II = LENSTR(FILNAM)
            ELSE
              II = LENSTR(FILNAM)
              IF (FILNAM(II:II).NE.'/') THEN
                II = II + 1
                FILNAM(II:II)='/'
              ENDIF
            ENDIF
          ENDIF
        ENDIF
C
        IF (II.LT.0) THEN
          IF (VAX) THEN
            CALL UGTENV('SYS$LOGIN',FILNAM)
          ELSE
            CALL UGTENV('HOME',FILNAM)
          ENDIF
          IF (FILNAM.NE.' ') THEN
            IF (VAX) THEN
              FILNAM = 'SYS$LOGIN:'
              II = LENSTR(FILNAM)
	    ELSE
              II = LENSTR(FILNAM)
              IF (FILNAM(II:II).NE.'/') THEN
                II = II + 1
                FILNAM(II:II)='/'
              ENDIF
            ENDIF
          ELSE
            II = 0
          ENDIF
        ENDIF
        FILNAM(II+1:) = LOGFIL
        CALL QPRINT(2,'Opening file '//FILNAM(1:LENSTR(FILNAM)))
        IFAIL=0
        IXUNIT = IUNIT
        IF (IHELP.LT.2) IXUNIT = -IUNIT
        CALL CCPDPN (IXUNIT,FILNAM,'READONLY','F',LREC,IFAIL)
   50   CONTINUE
        READ (UNIT=IUNIT,FMT=6000,END=60,ERR=80,IOSTAT=IERR) LINE
        II = INDEX(LINE,'#')
        IF (II.EQ.1) THEN
          LINE = ' '
        ELSEIF (II.GT.1) THEN
          LINE = LINE(1:II-1)
        ENDIF
        IF (LINE.NE.' ') THEN
          II = INDEX(LINE,'=')
          LOGNAM = LINE(1:II-1)
          FILNAM = LINE(II+1:)
C
C---- here skip = .true. in subroutine setenv
C            if logical name already exits
C
          CALL SETENV(LOGNAM,FILNAM,ENAME,ETYPE,EXTN,ICOUNT,.TRUE.)
        END IF
        GO TO 50
   60   CLOSE (UNIT=IUNIT)
      END IF
C
C---- Loop through command line arguments
C
      CALL QPRINT(2,'Processing Command Line Arguments')
      DO 70 LOOP = ILOOP,IARG,2
        CALL GETARG(LOOP,LOGNAM)
         CALL CCPUPC(LOGNAM)
        CALL GETARG(LOOP+1,FILNAM)
        IF (FILNAM.EQ.' ') CALL CCPERR(1,'Use: Logical_name filename')
C
C---- here skip = .false. in subroutine setenv
C            if logical name already exits as command line
C             takes precedence
C
        CALL SETENV(LOGNAM,FILNAM,ENAME,ETYPE,EXTN,ICOUNT,.FALSE.)
   70 CONTINUE
      CALL QPRINT(2,'End of pre-processing stage')
C
C---- Now transfer to another executable image ?
C
      IF (.NOT.VAX .AND. EXEC.NE.0) THEN
        CALL GETARG(EXEC,FILNAM)
        CALL QPRINT(2,'Swapping to process '//FILNAM(1:LENSTR(FILNAM)))
      END IF
      RETURN
C
   80 CALL UGERR(IOSTAT,LINE)
      CALL CCPERR(1,LINE)
C
 6000 FORMAT (A)
      END
C
C
C
      SUBROUTINE CCPLWC(STRING)
C     ========================
C
C---- convert a text string to lower case in situ
C
C
C
C     .. Scalar Arguments ..
      CHARACTER STRING* (*)
C     ..
C     .. Local Scalars ..
      INTEGER K,L,LL
      CHARACTER LC*26,UC*26
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX,LEN
C     ..
C     .. Data statements ..
      DATA LC/'abcdefghijklmnopqrstuvwxyz'/
      DATA UC/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C     ..
C
      LL = LEN(STRING)
      IF (LL.GT.0) THEN
        DO 10 L = 1,LL
          K = INDEX(UC,STRING(L:L))
          IF (K.NE.0) STRING(L:L) = LC(K:K)
   10   CONTINUE
      END IF
C
C
      END
C
C
C
      SUBROUTINE CCPMDE(MODE,NBYT)
C     ============================
C
C---- If byte handling is available (see ccpbyt) then this subroutine
C     returns the number of bytes per data item for the different modes
C     used, in particular, in the map handling subroutines.
C
C---- If byte handling is not available, then the number of words per
C     item is returned with zeros for the undefined items
C
C
C PARAMETERS
C =========
C
C        MODE (I)   MODE = 0,   BYTES
C                        = 1,   SHORT (2 BYTE) INTEGERS
C                        = 2,   REAL/INTEGER (SINGLE WORD)
C                        = 3,   SHORT COMPLEX (2 * 2 BYTE INTEGERS)
C                        = 4,   COMPLEX (TWO WORDS)
C        NBYT (O)        > 0,   THE NUMBER OF BYTES FOR THE ITEM  IF
C                               CCPBYT RETURNS .TRUE. OR THE  NUMBER
C                               OF WORDS IF CCPBYT RETURNS .FALSE.
C                        = 0,   NO VALUE AVAILABLE FOR THIS MODE
C                        = -1,  INVALID MODE
C
C  TYPICAL VALUES:  1  2  4  4  8    IF BYTE HANDLING AVAILABLE WITH 4
C                                    BYTES/WORD
C                   0  0  1  0  2    IF BYTE HANDLING UNAVAILABLE
C
C ***DL CONVEX VERSION***
C
C SPECIFICATION STATEMENTS
C ------------------------
C
C     .. Scalar Arguments ..
      INTEGER MODE,NBYT
C     ..
C     .. Local Arrays ..
      INTEGER MODES(0:4)
C     ..
C     .. Data statements ..
      DATA MODES/1,2,4,4,8/
C     ..
C
C---- Get number of bytes or words
C
      NBYT = -1
      IF (MODE.GE.0 .AND. MODE.LE.4) NBYT = MODES(MODE)
      END
C
C
C
      SUBROUTINE CCPMVB(ARR1,I1,ARR2,I2,NTOMOV)
C     ========================================
C
C---- This subroutine moves bytes from one non-character array 
C     to another. I must be implemented if ccpbyt returns .true. 
C     but will otherwise be a dummy routine.
C
C
C PARAMETERS
C ==========
C
C        ARR1 (I/O) ARRAY TO WHICH BYTES ARE TO BE COPIED
C          I1 (I)   THE START BYTE NUMBER IN ARR1 WHERE THE BYTES ARE
C                   TO BE COPIED
C        ARR2 (I)   ARRAY FROM WHICH BYTES ARE TO BE COPIED
C          I2 (I)   THE START BYTE NUMBER IN ARR2 FROM WHICH THE BYTES
C                   ARE TO BE COPIED
C      NTOMOV (I)   THE NUMBER OF BYTES TO BE COPIED
C
C ***DL CONVEX VERSION***
C
C SPECIFICATION STATEMENTS AND CODE
C
C
C
C     .. Scalar Arguments ..
      INTEGER I1,I2,NTOMOV
C     ..
C     .. Array Arguments ..
      BYTE ARR1(*),ARR2(*)
C     ..
C     .. Local Scalars ..
      INTEGER I,J,N
C     ..
      I = I1 - 1
      J = I2 - 1
      DO 10 N = 1,NTOMOV
        I = I + 1
        J = J + 1
        ARR1(I) = ARR2(J)
   10 CONTINUE
C
C
      END
C
C
      SUBROUTINE CCPMVI (ARR1,ARR2,NUM)
C     =================================
C
C  This routine assigns ARR2 to ARR1 using NUM words
C
C  Arguments ..........
      INTEGER*4 NUM
      REAL ARR1(NUM),ARR2(NUM)
C
      INTEGER J
C
      DO 10 J=1,NUM
   10 ARR1(J)=ARR2(J)
      END 
C
C
C
      LOGICAL FUNCTION CCPONL(IDUM)
C     ============================
C
C
C---- This function determines whether a program is being run on-line
C     if this information is available
C
C
C PARAMETERS
C ==========
C
C        IDUM (I)   DUMMY PARAMETER
C
C RETURNS .TRUE.  IF PROGRAM IS BEING RUN ON-LINE
C RETURNS .FALSE. IF BATCH MODE OR STATUS UNKNOWN
C
C
C     .. Scalar Arguments ..
      INTEGER IDUM
C     ..
C     .. Local Scalars ..
      INTEGER IYES,ITERM
C     ..
C     .. External Functions ..
      EXTERNAL UISATT
C     ..
C
C---- For Convex uisatt calls isatty... this is a logial function
C     must use unit for stdin (=0) as unit 5 in a script using
C     px program.exe LOGICAL filename .... << 'eof'
C     .. data
C     'eof'
C     cause unit 5 to be switched
C
C     using unit = 0 (fortran err)
C     then for INTERACTIVE returns TRUE   ie just tying program.exe
C          for ONLINE      returns TRUE   using a script with << 'eof'
C          for BATCH/SUBMIT returns FALSE using submit script file
C
C      test for fortran unit=6 o/p
C
      IYES = 0
      ITERM = 6
      CALL UISATT(ITERM,IYES)
       IF (IYES.EQ.1) THEN
          CCPONL = .TRUE.
       ELSE
          CCPONL = .FALSE.
      END IF
cc      CCPONL = (ISATTY(6))  direct fortran call
C
C
      END
C
C
C
      LOGICAL FUNCTION CCPOVP(IUN)
C    ============================
C
C
C---- This function determines overprinting is available on a given
C     fortran unit if this information is available
C
C PARAMETERS
C ==========
C
C        IUN (I)   FORTRAN UNIT NUMBER
C
C RETURNS .TRUE.  IF OVERPRINTING AVAILABLE
C RETURNS .FALSE. IF OVERPRINTING NOT AVAILABLE OR INFORMATION NOT
C                 AVAILABLE
C
C
C     .. Scalar Arguments ..
      INTEGER IUN
C     ..
C     .. Local Scalars ..
      INTEGER IYES
C     ..
C     .. External Subroutines ..
      EXTERNAL UISATT
C     ..
      CALL UISATT(IUN,IYES)
      IF (IYES.EQ.1) THEN
        CCPOVP = .FALSE.
      ELSE
        CCPOVP = .TRUE.
      END IF
      END
C
C
C
      SUBROUTINE CCPPAG(IUN,NCOL,NLIN)
C     ===============================
C
C
C---- This subroutine returns the number of columns and lines 
C     for a printer output page on a give fortran unit number 
C     if the information is available
C
C
C PARAMETERS
C ==========
C
C         IUN (I)   FORTRAN UNIT NUMBER
C        NCOL (O)   NUMBER OF COLUMNS IN THE PAGE
C        NLIN (O)   NUMBER OF LINES IN THE PAGE
C
C RETURNS NCOL=132 AND NLIN=66 AS DEFAULT VALUES
C
C
C Return 80,132 unless a terminal whence 0,80
C
C
C     .. Scalar Arguments ..
      INTEGER IUN,NCOL,NLIN
C     ..
C     .. Local Scalars ..
      INTEGER IYES
C     ..
C     .. External Subroutines ..
      EXTERNAL UISATT
C     ..
      CALL UISATT(IUN,IYES)
      IF (IYES.EQ.1) THEN
        NLIN = 0
        NCOL = 80
      ELSE
        NLIN = 80
        NCOL = 132
      END IF
      END
C
C
C
      SUBROUTINE CCPRNM(LOG1,LOG2)
C     ============================
C
C---- Rename file assigned to logical name LOG2 to same name as that
C     assigned to LOG1
C
C
C---- Get file-names for both streams
C
C     .. Scalar Arguments ..
      CHARACTER LOG1* (*),LOG2* (*)
C     ..
C     .. Local Scalars ..
      INTEGER ISTAT,RENAME
      CHARACTER ERRSTR*100,NAME1*100,NAME2*100
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL UGERR,URENAM
C     ..
      INQUIRE (FILE=LOG1,NAME=NAME1)
      INQUIRE (FILE=LOG2,NAME=NAME2)
C
C---- Rename file
C
      CALL URENAM(NAME2,NAME1,ISTAT)
      IF (ISTAT.NE.0) THEN
C
C---- Error
C
        CALL UGERR(ISTAT,ERRSTR)
        WRITE (6,FMT=6002) LOG1(1:LENSTR(LOG1)),NAME1(1:LENSTR(NAME1)),
     +    LOG2(1:LENSTR(LOG2)),NAME2(1:LENSTR(NAME2))
        WRITE (6,FMT=6004) ERRSTR(1:LENSTR(ERRSTR))
        CALL CCPERR(1, '***CCPRNM ERROR*** IN CCPLIB 6004')
      ELSE
        WRITE (6,FMT=6000) NAME2,NAME1
      END IF
C
C---- Format statements
C
 6000 FORMAT (/' File ',A,' renamed as ',A,/)
 6002 FORMAT (/' **RENAME FAILURE**',/' To   Logical name: ',A,', Full',
     +       ' name: ',A,/' From Logical name: ',A,', Full name: ',A,/)
 6004 FORMAT (' Op-system error: ',A,/)
C
C
      END
C
C
C
      SUBROUTINE CCPRVR(IUN,ITEXT,NCHMAX,IMAX,*,*)
C     ===========================================
C
C
C---- Read a variable length character record of unknown length from an
C     unformatted sequential file
C
C
C PARAMETERS
C ==========
C
C        IUN (I)   UNIT NUMBER FOR THE READ
C      ITEXT (O)   CHARACTER ARRAY TO HOLD THE TEXT READ
C     NCHMAX (I)   MAX. NO. OF CHARACTERS IN ITEXT
C       IMAX (O)   NO. OF CHARACTERS READ
C
C RETURN 1   END OF FILE
C RETURN 2   VARIABLE LENGTH READ OF UNKNOWN LENGTH NOT AVAILABLE
C
C ***DL CONVEX VERSION*** Function not implemented
C
C     .. Scalar Arguments ..
      INTEGER IMAX,IUN,NCHMAX
C     ..
C     .. Array Arguments ..
      CHARACTER ITEXT(NCHMAX)*1
C     ..
      RETURN 2
      END
C
C
C
      SUBROUTINE CCPSPW(STRING)
C     =========================
C
C
       CHARACTER STRING*(*)
c for vms
c       INTEGER ISTAT,LIB$SPAWN
C
C
C
C for unix (in general i think)
           CALL SYSTEM(STRING)
c  for vms
c           ISTAT = LIB$SPAWN(STRING)
C
C
         END
C
C
C
      REAL FUNCTION CCPSUM(A,N,L)
C     ======================
C
C
C---- This function sums the elements of an array. (for the cray this
C     function will call the cray 'ssum' function)
C
C
C PARAMETERS
C ==========
C
C           A (I)   ARRAY TO BE SUMMED
C           N (I)   NO. OF ELEMENTS IN THE ARRAY
C           L (I)   SUM EVERY L'TH ELEMENT
C
C  CCPSUM RETURNS THE SUM
C
C SPECIFICATION STATEMENTS AND CODE
C
C     .. Scalar Arguments ..
      INTEGER L,N
C     ..
C     .. Array Arguments ..
      REAL A(N)
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
      CCPSUM = 0.0
      DO 10 I = 1,N,L
        CCPSUM = A(I) + CCPSUM
   10 CONTINUE
      END
C
C
C
      SUBROUTINE CCPTIM(IFLAG,CPU,ELAPS)
C     ==================================
C
C---- subroutine to return cpu time and elapsed time. times are 
C     intervals From the initial call with iflag=0 and are 
C     in seconds
C
C
C PARAMETERS
C ==========
C
C       IFLAG (I/O) =0, INITIALISE, =1, RETURN TIMES, =-1 DUMMY CALL
C                   RETURNS -1 IF TIME NOT AVAILABLE
C         CPU (O)   CPU TIME IN SECONDS
C       ELAPS (O)   ELAPSED TIME IN SECONDS
C
C---- If time not available then iflag is returned as -1 and cpu and
C     elaps are set to zero
C
C
C SPECIFICATION STATEMENTS AND CODE
C
C     .. Scalar Arguments ..
      REAL CPU,ELAPS
      INTEGER IFLAG
C     ..
C     .. Scalars in Common ..
      REAL CPUX
      INTEGER TIM0
C     ..
C     .. Local Scalars ..
      INTEGER STIME
C     ..
C     .. Local Arrays ..
      REAL TARRAY(2)
C     ..
C     .. External Subroutines ..
      EXTERNAL UCPUTM,USTIME
C     ..
C     .. Common blocks ..
      COMMON /TIMSAV/TIM0,CPUX
C     ..
C     .. Save statement ..
      SAVE /TIMSAV/
C     ..
      IF (IFLAG.EQ.0) THEN
        ELAPS = 0.0
        CPU = 0.0
        CALL USTIME(TIM0)
        CALL UCPUTM(CPUX)
      ELSE
        CALL USTIME(STIME)
        ELAPS = STIME - TIM0
        CALL UCPUTM(CPU)
        CPU = CPU - CPUX
      END IF
      END
C
      SUBROUTINE CCPTOI(ARRAY,N,II,ITYP,IFAIL)
C     ========================================
C
C
C---- This subroutine converts the n'th byte or integer*2 element in a
C     non-character array to an integer value. it is used by the
C     map file handling routines and must be implemented if map modes
C     0,1,3 or 5 are to be used.
C
C
C PARAMETERS
C ==========
C
C       ARRAY (I)   REAL ARRAY CONTAINING THE ELEMENTS TO BE CONVERTED
C           N (I)   THE NUMBER OF THE ELEMENT TO BE CONVERTED
C          II (O)   THE CALCULATED INTEGER VALUE (FOR BYTES THIS WILL
C                   BE IN THE RANGE 0-255)
C        ITYP (I)   THE CONVERSION TYPE =1, BYTE TO INTEGER
C                                       -2, INTEGER*2 TO INTEGER
C       IFAIL (I/O) ON INPUT   =0, STOP IF CONVERSION NOT AVAILABLE
C                              =1, RETURN FROM SUBROUTINE ALWAYS
C                   ON OUTPUT  UNCHANGED IF CONVERSION CARRIED OUT
C                              =-1 IF CONVERSION NOT AVAILABLE
C
C ***DL CONVEX VERSION***
C
C SPECIFICATION STATEMENTS
C
C
C---- Perform conversions
C
C     .. Scalar Arguments ..
      INTEGER IFAIL,II,ITYP,N
C     ..
C     .. Array Arguments ..
      REAL ARRAY(*)
C     ..
C     .. Local Scalars ..
      REAL RR
      INTEGER IA,NB,NIH,NW
C     ..
C     .. Local Arrays ..
      BYTE IBYT(4),JBYT(4)
      INTEGER*2 JHALF(2)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     ..
C     .. Equivalences ..
      EQUIVALENCE (IA,IBYT(1))
      EQUIVALENCE (RR,JHALF(1),JBYT(1))
C     ..
C
C
      GO TO (10,20) ITYP
C
C---- Byte to integer value
C
   10 NW = (N-1)/4 + 1
      NB = MOD(NW-1,4) + 1
      IA = 0
      RR = ARRAY(NW)
      IBYT(4) = JBYT(NB)
      II = IA
      RETURN
C
C---- Integer*2 to integer value
C
   20 NW = (N-1)/2 + 1
      NIH = MOD(NW-1,2) + 1
      RR = ARRAY(NW)
      II = JHALF(NIH)
      END
C
C
C
      SUBROUTINE CCPUFL
C     =================
C
C
C---- This subroutine is called to suppress underflow error messages
C     if required and if the facility is available.
C
C
C PARAMETERS  NONE
C =========
C
C----  Not implemented, but Maybe correc
C
      END
C
C
C
      LOGICAL FUNCTION CCPULI(IUN)
C     ============================
C
C
C---- This function determines underlining is available on a given
C     fortran unit if this information is available
C
C
C PARAMETERS
C ==========
C
C        IUN (I)   FORTRAN UNIT NUMBER
C
C RETURNS .TRUE.  IF UNDERLINING AVAILABLE
C RETURNS .FALSE. IF UNDERLINING NOT AVAILABLE OR INFORMATION NOT
C                 AVAILABLE
C
C     .. Scalar Arguments ..
      INTEGER IUN
C     ..
C     .. Local Scalars ..
      INTEGER IYES
C     ..
C     .. External Subroutines ..
      EXTERNAL UISATT
C     ..
      CALL UISATT(IUN,IYES)
      IF (IYES.EQ.1) THEN
        CCPULI = .FALSE.
      ELSE
        CCPULI = .TRUE.
      END IF
      END
C
C
C
      SUBROUTINE CCPUPC(STRING)
C     ========================
C
C
C---- Convert a text string to upper case in situ
C
C
C PARAMETERS
C ==========
C
C      STRING (I/O) CHARACTER STRING TO BE CONVERTED
C
C SPECIFICATION STATEMENTS
C
C     .. Scalar Arguments ..
      CHARACTER STRING* (*)
C     ..
C     .. Local Scalars ..
      INTEGER K,L,LL
      CHARACTER LC*26,UC*26
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX,LEN
C     ..
C     .. Data statements ..
      DATA LC/'abcdefghijklmnopqrstuvwxyz'/
      DATA UC/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C     ..
C
C---- Convert string
C
      LL = LEN(STRING)
      IF (LL.GT.0) THEN
        DO 10 L = 1,LL
          K = INDEX(LC,STRING(L:L))
          IF (K.NE.0) STRING(L:L) = UC(K:K)
   10   CONTINUE
      END IF
      END
C
C
C
      SUBROUTINE CCPVRS(ILP,PROG,VDATE)
C     =================================
C
C
C---- Print program name and date of current version (also prints run
C     date if available)
C
C
C PARAMETERS
C ==========
C
C         ILP (I)   UNIT NUMBER OF PRINTER OUTPUT
C        PROG (I)   CHARACTER VARIABLE HOLDING PROGRAM NAME (MAX
C                   OF 10 CHARACTERS)
C       VDATE (I)   CHARACTER VARIABLE HOLDING DATE OF THE CURRENT
C                   VERSION AS DD/MM/YY
C
C SPECIFICATION STATEMENTS
C
C
C---- Output heading
C
C     .. Scalar Arguments ..
      INTEGER ILP
      CHARACTER PROG* (*),VDATE* (*)
C     ..
C     .. Local Scalars ..
      CHARACTER CTIME*8,DT2*8,DT*10,PR*10,UID*20
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPDAT,UGTUID,UTIME
C     ..
      PR = PROG
      DT = VDATE
      CALL CCPDAT(DT2)
      CALL UGTUID(UID)
      CALL UTIME(CTIME)
      WRITE (ILP,FMT=6000) PR,DT,UID(1:LENSTR(UID)),DT2,CTIME
C
C---- Format statements
C
 6000 FORMAT ('1### CCP PROGRAM SUITE: ',A10,2X,'FORTRAN77 VERSION: ',
     +       A8,'###',/' User: ',A,'  Run date: ',A8,'  Run time:',A,
     +       /)
C
C
      END
C
C
C
C
      SUBROUTINE CCPZBI (ARR1,NUM)
C     ============================
C
C  This routine zeros NUM bytes of the array ARR1
C
C  Arguements ......
      INTEGER NUM
      BYTE ARR1(NUM)
C
      INTEGER J
C
      DO 10 J=1,NUM
   10 ARR1(J)=0
      END
C
C
      SUBROUTINE CCPZI (ARR1,NUM)
C     ===========================
C
C  This routine assigns zero to ARR1 using NUM words
C
C  Arguments ..........
      INTEGER*4 NUM
      REAL ARR1(NUM)
C
      INTEGER J
C
      DO 10 J=1,NUM
   10 ARR1(J)=0.0
      END 
C
C
C     ==================================
      CHARACTER*(*) FUNCTION FDIR(FILNAM)
C     ==================================
C
C---- Returns the directory of a file name or ' '
C
C
C     .. Scalar Arguments ..
      CHARACTER FILNAM* (*)
C     ..
C     .. Local Scalars ..
      INTEGER II,JJ
C     ..
C     .. External Functions ..
      LOGICAL VAXVMS
      EXTERNAL VAXVMS
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX
C     ..
      FDIR = ' '
      IF (VAXVMS()) THEN
        II = INDEX(FILNAM,']')
      ELSE
        II = 0
   10   CONTINUE
        JJ = INDEX(FILNAM(II+1:),'/')
        IF (JJ.NE.0) THEN
          II = II + JJ
          GO TO 10
        END IF
        II = II - 1
        IF (II.LT.0) II = 0
      END IF
      IF (II.NE.0) FDIR = FILNAM(1:II)
C
      END
C
C
C     ===================================
      CHARACTER*(*) FUNCTION FEXTN(FILNAM)
C     ===================================
C
C---- Returns the extension of a file name or ' '
C
C
C     .. Scalar Arguments ..
      CHARACTER FILNAM* (*)
C     ..
C     .. Local Scalars ..
      INTEGER II,JJ
C     ..
C     .. External Functions ..
      LOGICAL VAXVMS
      EXTERNAL VAXVMS
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX
C     ..
      FEXTN = ' '
      IF (VAXVMS()) THEN
        II = INDEX(FILNAM,']')
      ELSE
        II = 0
   10   CONTINUE
        JJ = INDEX(FILNAM(II+1:),'/')
        IF (JJ.NE.0) THEN
          II = II + JJ
          GO TO 10
        END IF
      END IF
      JJ = INDEX(FILNAM(II+1:),'.')
      IF (JJ.NE.0) FEXTN = FILNAM(II+JJ+1:)
C
      END
C
C
C     ===================================
      CHARACTER*(*) FUNCTION FROOT(FILNAM)
C     ===================================
C
C---- Returns a file name minus an extension.
C
C
C     .. Scalar Arguments ..
      CHARACTER FILNAM* (*)
C     ..
C     .. Local Scalars ..
      INTEGER II,JJ
C     ..
C     .. External Functions ..
      LOGICAL VAXVMS
      EXTERNAL VAXVMS
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX
C     ..
      FROOT = ' '
      IF (VAXVMS()) THEN
        II = INDEX(FILNAM,']')
      ELSE
        II = 0
   10   CONTINUE
        JJ = INDEX(FILNAM(II+1:),'/')
        IF (JJ.NE.0) THEN
          II = II + JJ
          GO TO 10
        END IF
      END IF
      JJ = INDEX(FILNAM(II+1:),'.')
      IF (JJ.NE.0) FROOT = FILNAM(1:II+JJ-1)
C
      END
C
C
C
         LOGICAL FUNCTION LITEND(idum)
C        =======================
C
           integer idum
C
C
C---- Check endedness, Returns TRUE if little endian (VAX, FX2800,
C                                                   Ultrix, Convex)
C
C                              FALSE if big endian (IBM,IRIS,ESV)
C
C
         INTEGER I
         BYTE B(4)
         EQUIVALENCE (I,B(1))
C
C---- Initialise B
C
          DO 10 JDO=1,4
            B(JDO) = 0
 10       CONTINUE
C
C
          I = 1
C
C
          IF (B(1) .NE. 0) THEN
              LITEND = .TRUE.
          ELSE
              LITEND = .FALSE.
          END IF
C
        END
C
C
C
C     =======================================================
      SUBROUTINE SETENV(LNAME,FILNAM,ENAME,ETYPE,EXTN,ICOUNT,LSKIP)
C     =======================================================
C
C     .. Parameters ..
      INTEGER ILIMIT,ISTRLN,IENV
      PARAMETER (ILIMIT=150,ISTRLN=200,IENV=20)
C     ..
C     .. Scalar Arguments ..
      INTEGER ICOUNT
      CHARACTER LNAME* (*),FILNAM* (*)
      LOGICAL LSKIP
C     ..
C     .. Array Arguments ..
      CHARACTER ENAME(ILIMIT)* (IENV),ETYPE(ILIMIT)* (5),
     +          EXTN(ILIMIT)* (4)
C     ..
C     .. Local Scalars ..
      INTEGER II,ISTAT,JJ
      LOGICAL VAX
      CHARACTER ERRSTR* (ISTRLN),LIBFIL* (ISTRLN),PROGNM* (ISTRLN),
     +          TMPNAM* (ISTRLN),LINE* (ISTRLN),ENVFIL* (ISTRLN)
C     ..
C     .. External Functions ..
      INTEGER ACCESS,GETPID,LENSTR
      LOGICAL VAXVMS
      CHARACTER FDIR* (ISTRLN),FEXTN* (ISTRLN),FROOT* (ISTRLN)
      EXTERNAL ACCESS,GETPID,LENSTR,VAXVMS,FDIR,FEXTN,FROOT
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPERR,GETARG,QPRINT,UGTENV,USTENV
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX
C     ..
C
C---- Check Logical Name does not already exist BUT not for command line
C
      CALL UGTENV(LNAME,ENVFIL)
      IF (ENVFIL.NE.' ' .AND. LSKIP ) RETURN
      VAX = VAXVMS()
      CALL GETARG(0,PROGNM)
C
C---- Strip leading directories from argv[0]
C
      IF (.NOT. VAX) THEN
        IF (INDEX (PROGNM,'/').EQ.0) GOTO 5
        DO 3 II=LENSTR(PROGNM),1,-1
          IF (PROGNM(II:II).EQ.'/') GOTO 4
    3   CONTINUE
    4   PROGNM = PROGNM(II + 1:)
      ENDIF
    5 CONTINUE
      DO 10 JJ = 1,ICOUNT
        IF (ENAME(JJ).EQ.LNAME(1:LENSTR(ENAME(JJ)))) GO TO 20
   10 CONTINUE
C
C---- Unknown logical name add it to the list.
C
      TMPNAM = 'Non standard logical name '//LNAME(1:LENSTR(LNAME))
      CALL QPRINT(2,TMPNAM)
      ICOUNT = ICOUNT + 1
      IF (ICOUNT.GT.ILIMIT) CALL CCPERR(1,'No more string space')
      ENAME(ICOUNT) = LNAME
      ETYPE(ICOUNT) = 'undef'
      EXTN(ICOUNT) = FEXTN(FILNAM)
      JJ = ICOUNT
C
C---- Known logical name processing
C
   20 IF (FEXTN(FILNAM).EQ.' ') THEN
C
C---- Don't add extension if filnam is /dev/null or NL:
C
        IF (FILNAM.EQ.'/dev/null' .OR. FILNAM.EQ.'NL:') GOTO 222
        FILNAM = FILNAM(1:LENSTR(FILNAM))//EXTN(JJ)
      ENDIF
222   IF (FILNAM.EQ.'/dev/null' .OR. FILNAM.EQ.'NL:') GOTO 333
      IF (FDIR(FILNAM).EQ.' ') THEN
        IF (EXTN(JJ).EQ.'.lib' .OR. EXTN(JJ).EQ.'.prt' .OR.
     +      EXTN(JJ).EQ.'.bes' .OR. EXTN(JJ).EQ.'.dic') THEN
          CALL UGTENV('CLIBD',LIBFIL)
          IF (VAX) THEN
            IF (ENVFIL.NE.' ') THEN
               TMPNAM = 'CLIBD:' // FILNAM(1:LENSTR(FILNAM))
            ELSE
              TMPNAM = FILNAM(1:LENSTR(FILNAM))
            ENDIF
          ELSE
            TMPNAM = LIBFIL(1:LENSTR(LIBFIL))//'/'//
     +                FILNAM(1:LENSTR(FILNAM))
          END IF
          FILNAM = TMPNAM(1:LENSTR(TMPNAM))
        ELSE IF (EXTN(JJ).EQ.'.scr' .OR. 
     +          FEXTN(FILNAM).EQ.'scr') THEN
          CALL UGTENV('CCP4_SCR',TMPNAM)
          IF (VAX) THEN
            IF (TMPNAM.EQ.' ') THEN
              TMPNAM = PROGNM
            ELSE 
              TMPNAM = 'CCP4_SCR:' // PROGNM(1:LENSTR(PROGNM))
            ENDIF
          ELSE
            II = LENSTR(TMPNAM)
            IF (TMPNAM(II:II).EQ.'/') TMPNAM = TMPNAM(1:II - 1)
            TMPNAM = TMPNAM(1:LENSTR(TMPNAM))//'/'//
     +            PROGNM(1:LENSTR(PROGNM))
          END IF
          II = INDEX(FILNAM,'.')
          TMPNAM = TMPNAM(1:LENSTR(TMPNAM))//'_'//FILNAM(1:II)
          IF (VAX) THEN
            WRITE (LIBFIL,'(Z8.8)') GETPID()
          ELSE
            WRITE (LIBFIL,'(I5.5)') GETPID()
          ENDIF
          FILNAM = TMPNAM(1:LENSTR(TMPNAM))//LIBFIL
        END IF
      END IF
333   CONTINUE
C
C---- Now test input files do exist
C
      IF (ETYPE(JJ).EQ.'in') THEN
        IF (VAX) THEN
cc          II = ACCESS(FILNAM(1:LENSTR(FILNAM)),0)
            II = 0
        ELSE
          II = ACCESS(FILNAM(1:LENSTR(FILNAM)),'r')
        END IF
        IF (II.NE.0) THEN
          ERRSTR = 'Cannot find file '//FILNAM(1:LENSTR(FILNAM))
          CALL CCPERR(1,ERRSTR)
        END IF
      END IF
      LINE = LNAME(1:LENSTR(LNAME))//'='//FILNAM(1:LENSTR(FILNAM))
C
C     =======================================
      CALL USTENV(LINE(1:LENSTR(LINE)),ISTAT)
C     =======================================
C
      IF (ISTAT.NE.0) THEN
        ERRSTR = 'Cannot create environment variable '//
     +           LNAME(1:LENSTR(LNAME))
        CALL CCPERR(1,ERRSTR)
      END IF
      CALL QPRINT(3,LINE(1:LENSTR(LINE)))
      END
C
C
C======================================================================
C
C QPRINT - Set print flag
C
C Usage:  CALL QPRINT   (IFLAG,MSG)
C         INTEGER       IFLAG
C         CHARACTER*(*) MSG
C
C Input:  IFLAG         debug level 0-9 higher numbers give more output
C         MSG           the output message itself
C
C Output: None.
C
C======================================================================
C
      SUBROUTINE QPRINT(IFLAG,MSG)
C     ============================
C
C     .. Scalar Arguments ..
      INTEGER IFLAG
      CHARACTER MSG* (*)
C     ..
C     .. External Subroutines ..
      EXTERNAL CPRINT
C     ..
      CALL CPRINT(IFLAG,MSG)
C
      END
C
C
C======================================================================
C
C QTYPE - return machine type stamp
C
C Usage:  CALL QTYPE   (ISTAMP)
C         INTEGER       ISTAMP
C
C Output: ISTAMP - machine type stamp
C
C======================================================================
C
      SUBROUTINE QTYPE(ISTAMP)
C     ========================
C
C     .. Scalar Arguments ..
C
      INTEGER ISTAMP
C     ..
C     .. External Subroutines ..
      EXTERNAL CMTYPE
C     ..
      CALL CMTYPE(ISTAMP)
C
      END
C
C
C======================================================================
C
      INTEGER FUNCTION LENSTR(STRING)
C     ===============================
C
C---- Returns significant string length excluding trailing spaces
C
C
C
C     .. Scalar Arguments ..
      CHARACTER STRING* (*)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC LEN
C     ..
      LENSTR = LEN(STRING)
 10   CONTINUE
      IF (LENSTR.NE.0) THEN
        IF(STRING(LENSTR:LENSTR).EQ.' ' .OR.
     .       ICHAR(STRING(LENSTR:LENSTR)).EQ.0) THEN
          LENSTR = LENSTR - 1
          GO TO 10
        END IF
      END IF
C
C
      END
