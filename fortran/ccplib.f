C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
C_BEGIN_CCPLIB
C     These are supposedly-machine-independent low-level routines.
C     They're actually machine-dependent at least insofar as some
C     contain non-standard code, but they do compile with the compilers
C     tried on unix as well as VMS.
C
C     fixme: the bit-twiddling should be in library.c, not here.
C     amalgamate ccppsf and fdir/fext/froot.  also add tests of these
C     routines to testlib.
C
C     $Id$
C
C      CCFILL    Set specified number of elements of byte array
C      CCPALC    Call subroutine with allocated memory
C      CCPALE    Call subroutine with allocated memory set from environment
C      CCPBYI    Copy array of unsigned (or signed) bytes into integer array
C      CCPBYT    Indicate whether byte handling is available
C      CCPCPI    Copy array of BYTE or INTEGER*2 elements into integer array
C      CCPDAT    Get calendar date
C      CCPDEX    Periodicity reduction of 1024 (for PROLSQ)
C      CCPDPN    more friendly CCPOPN
C      CCPE2I    read integer from logical name value
C      CCPERR    Report error or normal termination and stop
C      CCPEXS    test if file exists
C      CCPFYP    Set up environment & parse command line arguments
C      CCPGI2    Get unsigned integer*2 value from 0 to 65535 from N'th
C                unsigned integer*2 element of array.
C      CCPGTB    Get unsigned byte value from 0 to 255 from N'th byte of
C                array.
C      CCPI2I    Copy an array of INTEGER*2 elements into an integer array
C      CCPIBY    Copy array of integers into array of bytes.
C      CCPII2    Copy array of integers into array of INTEGER*2 elements.
C      CCPLWC    Convert a string to lower case
C      CCPMDE    If byte handling available return nos. of bytes for map
C                modes
C      CCPMVB    Move bytes from one non-character array to another if
C                byte handling is available
C      CCPMVI    Move words from one integer array to another
C      CCPMVR    Move words from one real array to another
C      CCPNUN    Return an unconnected i/o unit number
C      CCPONL    See if program is being run interactively
C      CCPPSF    Parse file name into components
C      CCPRCS    Like CCPVRS but use RCS-format date string
C      CSETNV    Associate logical name with file name
C      CCPPAG    Set paging parameters if available
C      CCPSI2    Set integer value from 0 to 65535 into the N'th
C                unsigned integer*2 element of an array.
C      CCPSTB    Set integer value from 0 to 255 into N'th byte of array.
C      CCPSUM    Sum the elements of an array
C      CCPTIM    Get CPU and Elapsed times
C      CCPTOI    Convert n'th byte or I*2 in a non-character array to an
C                integer
C      CCPUFL    Supress underflow messages
C      CCPUPC    Convert a string to upper case
C      CCPVRS    Print program version number and date header
C      CCPZBI    Sets an array of bytes to zero
C      CCPZI     Set 'n' words of an integer array to zero using a simple loop
C      CCPZR     Set 'n' words of a real array to zero using a simple loop
C      FDIR      Returns the directory part of a file name
C      FEXTN     Returns the extension of a file name
C      FROOT     Returns the root of a file name
C      LITEND    determine endianness
C      LENSTR    length of string to last non-space
C      LUNSTI    Get logical unit number for input
C      LUNSTO    Get logical unit number for output
C      NBITST    Return the (unsigned) integer value held within a bit
C                field in a word
C      NOCRLF    write line supressing cr/lf to standard output
C      QPRINT    write debug messages
C      STBITS    Set a bit field within a word to a given (unsigned)
C                integer value
C      CCPLIC    Check that license conditions have been agreed
C_END_CCPLIB
C
      SUBROUTINE CCPALC(ROUTNE, N, TYPE, LENGTH)
C     ==========================================
C
C     Arrange to call subroutine ROUTNE with N array arguments each of
C     length LENGTH (i) and type indicated by TYPE (i): 'i' == integer,
C     'r' == real, 'd' == double precision, 'c' == complex, 'b' ==
C     "byte" (logical*1 or integer*1, unportable and deprecated) .  TYPE
C     elements may have either case.
C     Consider `call ccpalc (fred, 3, types, lens)' with types = (/'i',
C     'r', 'c'/)  and lens = (/1000, 2000, 3000/).  This effectively does
C        call fred (1000, arr1, 2000, arr2, 3000, arr3)
C     with
C        subroutine fred (n1, foo, n2, bar, n3, baz)
C        integer n1, n2, n3, foo (n1)
C        real bar (n2)
C        complex baz (n3)
C        ...
C     Obviously all communication with ROUTNE must be by COMMON (or,
C     possibly, extra ENTRYs).  The allocated memory is freed on return
C     from ROUTNE.  As a concession, it's initially filled with zeroed
C     bytes.

C
C Arguments:
C ==========
C
C      ROUTNE (I)   EXTERNAL: routine to call
C           N (I)   INTEGER: number of arguments to ROUTNE (<=12)
C        TYPE (I)   CHARACTER*1 (*): type of arguments to ROUTNE:
C                      'I': INTEGER; 'R': REAL; 'D': DOUBLE PRECISION;
C                      'C': COMPLEX; 'B': LOGICAL*1 or INTEGER*1
C      LENGTH (I)   INTEGER*(*): number of elements in each (array)
C                       argument of ROUTNE
C_END_CCPALC
C
C     .. Scalar Arguments ..
      INTEGER N
C     ..
C     .. Array Arguments ..
      CHARACTER TYPE (*)
      INTEGER LENGTH (*)
C     ..
      EXTERNAL ROUTNE, CCPAL1, CCPUPC
      INTEGER I, ITYPE (12)
      CHARACTER TTYPE (12)
C     ..
      IF (N.LT.1 .OR. N.GT.12)
     +     CALL CCPERR (1, 'CCPALC: bad number of arguments')
      DO 10 I=1,N
        TTYPE (I) = TYPE (I)
        CALL CCPUPC (TTYPE (I))
        ITYPE (I) = INDEX ('IRDCB', TTYPE (I))
        IF (ITYPE (I) .EQ. 0) CALL CCPERR (1, 'CCPALC: bad TYPE: '//
     +       TYPE (I))
        IF (LENGTH (I).LE.0) CALL CCPERR (1, 'CCPALC: length <=0')
 10   CONTINUE
      CALL CCPAL1 (ROUTNE, N, ITYPE, LENGTH)
      END
C
C_BEGIN_CCPALE
      SUBROUTINE CCPALE(ROUTNE, N, TYPE, LENGTH, LENDEF, PRINT)
C     =================================================
C
C     Arrange to call subroutine ROUTNE with N array arguments each of
C     length LENGTH (i) and type indicated by TYPE (i): 'i' == integer,
C     'r' == real, 'd' == double precision, 'c' == complex, 'b' == byte.
C     TYPE elements may have either case.  LENGTH points to an array of
C     environment variable (logical) names from which integer values are
C     read.  The lengths default to values from LENDEF.
C     This is a convenient interface to CCPALC to allow configuring of
C     the memory requirements on the command line where appropriate.
C     This may be useful if the memory requirements can't be determined
C     initially and it's necessary to guess.
C
C Arguments:
C ==========
C
C      ROUTNE (I)   EXTERNAL: routine to call
C           N (I)   INTEGER: number of arguments to ROUTNE (<=12)
C        TYPE (I)   CHARACTER*1 (*): type of arguments to ROUTNE:
C                      'I': INTEGER; 'R': REAL; 'D': DOUBLE PRECISION;
C                      'C': COMPLEX; 'B': LOGICAL*1 or INTEGER*1
C     LENGTH (I)   CHARACTER *(*): logical names representing the number
C                       of elements in each (array) argument of ROUTNE
C     LENDEF (I)   INTEGER (*): default lengths for the argument arrays
C     used if the appropriate LENGTH argument doesn't represent a
C     defined logical
C     PRINT  (I)   LOGICAL: whether or not to print the values of the
C     array lengths
C_END_CCPALE
C
C     .. Scalar Arguments ..
      INTEGER N
      LOGICAL PRINT
C     ..
C     .. Array Arguments ..
      CHARACTER TYPE (*),  LENGTH (*)*(*)
      INTEGER LENDEF (*)
C     ..
      EXTERNAL ROUTNE, CCPE2I, CCPALC, LUNSTO
      INTEGER I, LENG (12), CCPE2I, LUNSTO
C     ..
      DO 10 I=1,N
        LENG (I) = CCPE2I (LENGTH (I), LENDEF (I))
 10   CONTINUE
      IF (PRINT) THEN
        WRITE (LUNSTO(1), 
     +     '(/'' Memory allocation (logical name, type, elements):'')')
        WRITE (LUNSTO(1), '(3X, A, 1X, A, 3X, I10)')
     +       (LENGTH (I), TYPE (I), LENG (I), I=1,N)
      ENDIF
      CALL CCPALC (ROUTNE, N, TYPE, LENG)
      END
C
C_BEGIN_CCPDPN
      SUBROUTINE CCPDPN(IUN,LOGNAM,STATUS,TYPE,LREC,IFAIL)
C     ====================================================
C
C---- Calls CCPOPN to open a file, but with mnemonic arguments
C
C Arguments:
C ==========
C
C         IUN (I)   INTEGER: UNIT NUMBER
C
C      LOGNAM (I)   CHARACTER*(*): LOGICAL FILE NAME
C
C      STATUS (I)   CHARACTER*(*): FILE STATUS FLAG:
C                                     'UNKNOWN'
C                                     'SCRATCH'
C                                     'OLD'
C                                     'NEW'
C                                     'READONLY'
C                                     'PRINTER'
C
C        TYPE (I)   CHARACTER*(*): FILE TYPE FLAG:
C                                  ='F', 'SEQUENTIAL' 'FORMATTED'
C                                  ='U', 'SEQUENTIAL' 'UNFORMATTED'
C                                  ='DF', 'DIRECT'     'FORMATTED'
C                                  ='DU', 'DIRECT'     'UNFORMATTED'
C     [STATUS and TYPE are case-insensitive]
C
C        LREC (I)   INTEGER: RECORD LENGTH FOR DIRECT ACCESS FILE (NO. OF
C                   CHARACTERS FOR A FORMATTED FILE OR WORDS FOR
C                   AN UNFORMATTED FILE). NOT RELEVANT FOR A SEQUENTIAL
C                   FILE
C
C       IFAIL (I/O) INTEGER: ON INPUT     =0, STOP ON OPEN FAILURE
C                                         =1, CONTINUE AFTER OPEN FAILURE
C                                             (only on file not found)
C                                         =-1, As 0, but silent on success
C                                             (equivalent to negative IUN)
C                            ON OUTPUT    UNCHANGED IF FILE OPEN OK
C                                         =-1, ERROR IN OPENING FILE
C_END_CCPDPN
C
C     .. Scalar Arguments ..
      INTEGER IFAIL,IUN,IUN1,LREC
      CHARACTER LOGNAM* (*),STATUS* (*),TYPE* (*)
C     ..
C     .. Local Scalars ..
      INTEGER ISTAT,ITYPE
      CHARACTER ERRSTR*80
C     ..
C     .. Local Arrays ..
      CHARACTER TYPES(4)*2,STATS(6)*8, STAT*8, TYP*2
C     ..
C     .. External Functions ..
      INTEGER CCPNUN,LENSTR
      EXTERNAL CCPNUN,LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPOPN
C     ..
C     .. Data statements ..
      DATA STATS/'UNKNOWN','SCRATCH','OLD','NEW','READONLY','PRINTER'/
      DATA TYPES/'F','U','DF','DU'/
C     ..
C
      IF (IUN .EQ. 0) IUN = CCPNUN()
      STAT = STATUS
      TYP = TYPE
      CALL CCPUPC(STAT)
      CALL CCPUPC(TYP)
      DO 10 ISTAT = 1,6
        IF (STAT.EQ.STATS(ISTAT)) GO TO 20
   10 CONTINUE
      ERRSTR = ' CCPDPN: illegal status : '
      ERRSTR(LENSTR(ERRSTR)+2:) = STATUS
      CALL CCPERR(1,ERRSTR)
C
   20 DO 30 ITYPE = 1,4
        IF (TYP.EQ.TYPES(ITYPE)) GO TO 40
   30 CONTINUE
      ERRSTR = ' CCPDPN: illegal type: '
      ERRSTR(LENSTR(ERRSTR)+2:) = TYPE
      CALL CCPERR(1,ERRSTR)
C
 40   CONTINUE
      IUN1 = IUN
C  If IFAIL lt 0 No open message from CCPOPN
      IF(IFAIL.LT.0 .AND. IUN.GT.0) THEN
        IUN1 = -IUN
        IFAIL = 0
      ENDIF
      CALL CCPOPN(IUN1,LOGNAM,ISTAT,ITYPE,LREC,IFAIL)
C
      END
C
C_BEGIN_CCPNUN
      INTEGER FUNCTION CCPNUN ()
C     ==========================
C
C     Return (the next) unused (not connected) i/o unit number.
C     Use this to select an arbitrary unit for i/o to avoid clashes with
C     other code.  (The value returned will be the same until the unit in
C     question is opened or a lower-numbered one is closed.)
C
C_END_CCPNUN
      LOGICAL OD, EX
      EXTERNAL CCPERR
      INTEGER IOS
C     The `standard' unit 5 and 6 may or may not be reported as open,
C     normally depending on whether an appropriate read or write has
C     happened, so we'll start at 7.  Lower-numbered ones might be used
C     for other things such as standard error.  99 seems a reasonable
C     place to stop.
      DO 10 CCPNUN=7,99
        INQUIRE (UNIT=CCPNUN, OPENED=OD, IOSTAT=IOS, EXIST=EX)
        IF (EX .AND. (.NOT.OD) .AND. IOS.EQ.0) RETURN
 10   CONTINUE
      CALL CCPERR (1, 'CCPNUN: Can''t find an unused unit')
      END
C
C_BEGIN_CCPONL
      LOGICAL FUNCTION CCPONL(IDUM)
C     =============================
C
C---- This function determines whether a program is being run on-line
C     if this information is available
C
C Arguments:
C ==========
C
C        IDUM (D)   DUMMY
C
C RETURNS .TRUE.  IF PROGRAM IS BEING RUN ON-LINE
C RETURNS .FALSE. IF BATCH MODE OR STATUS UNKNOWN
C_END_CCPONL
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
C      test for fortran unit=6 o/p
C
      IYES = 0
      ITERM = 6
      CALL UISATT(ITERM,IYES)
      CCPONL = IYES.EQ.1
      END
C
C_BEGIN_CCPPSF
      SUBROUTINE CCPPSF(FILNAM,PATH,NAME,TYPE,VERS)
C     =============================================
C
C PARSE FILE NAME INTO COMPONENTS
C
C NOTE: THE ROUTINE  CONTAINS MACHINE DEPENDENT CODE
C
C
C Arguments:
C ==========
C
C      FILNAM (I)   CHARACTER*(*): FILE NAME STRING (NO EMBEDDED BLANKS ASSUMED)
C
C        PATH (O)   CHARACTER*(*): STRING RETURNING PATH OR, FOR VAX VMS,
C                   THE PART OF THE FILE SPECIFICATION UP TO THE
C                   END OF THE DIRECTORY SPECIFICATION (BLANK IF NONE)
C                   (INCLUDES TERMINATING ] or : or /)
C
C        NAME (O)   CHARACTER*(*): STRING RETURNING NAME.  (BLANK IF NONE)
C
C        TYPE (O)   CHARACTER*(*): STRING RETURNING FILE TYPE/EXTENSION
C                   (BLANK IF NONE)
C
C        VERS (O)   CHARACTER*(*): STRING RETURNING THE VERSION.
C                   (BLANK IF NONE)
C
C AFTER REMOVAL OF THE PATH PART OF THE STRING, IF PRESENT, THE VERSION ON
C A VAX IS TAKEN AS ANY TEXT FOLLOWING A SEMICOLON IN THE STRING OR, IF NO
C SEMICOLON IS PRESENT, ANY TEXT FOLLOWING THE LAST DOT IN THE STRING
C PROVIDED THAT AT LEAST TWO DOTS ARE PRESENT. ON A UNIX SYSTEM THE VERSION
C WILL ALWAYS BE RETURNED AS A BLANK.
C
C AFTER THE REMOVAL OF THE PATH AND VERSION PARTS OF THE STRING THEN, IF
C THERE IS AT LEAST ONE DOT, THE NAME IS THE STRING UP TO THE LAST DOT
C REMAINING AND THE TYPE IS THE PART OF THE STRING AFTER THE DOT. IF
C NO DOT IS PRESENT THEN THE REMAINING STRING IS THE NAME AND THE TYPE
C IS BLANK.
C_END_CCPPSF
C
C SPECIFICATION STATEMENTS
C ------------------------
C
      CHARACTER*(*) FILNAM,PATH,NAME,TYPE,VERS
      EXTERNAL VAXVMS, WINMVS, RTNBKS
      LOGICAL VAXVMS, WINMVS, VMS, MVS
      CHARACTER RTNBKS*1, BKS*1
C
C INITIALISATIONS
C ---------------
C
      PATH=' '
      NAME=' '
      TYPE=' '
      VERS=' '
      LMAX=LENSTR(FILNAM)
      IF (LMAX.EQ.0) RETURN
      LMIN=0
      VMS = VAXVMS()
      MVS = WINMVS()
      BKS = RTNBKS()      
10    LMIN=LMIN+1
      IF (FILNAM(LMIN:LMIN).EQ.' ') GO TO 10
C
C GET PATH
C --------
C
      IF (VMS) THEN
        DO 20 L=LMAX,LMIN,-1
          IF (FILNAM(L:L).EQ.':'.OR.FILNAM(L:L).EQ.']') GO TO 30
 20     CONTINUE
      ELSEIF (MVS) THEN
        DO 21 L=LMAX,LMIN,-1
          IF (FILNAM(L:L).EQ.BKS .OR. FILNAM(L:L).EQ.'/')GO TO 30
 21     CONTINUE
      ELSE
        DO 22 L=LMAX,LMIN,-1
          IF (FILNAM(L:L).EQ.'/')GO TO 30
 22     CONTINUE
      ENDIF
      GO TO 40
30    PATH=FILNAM(LMIN:L)
      LMIN=L+1
      IF (LMIN.GT.LMAX) RETURN
C
C GET VERSION IF PRESENT
C ----------------------
C
 40   CONTINUE
      IF (VMS) THEN
        LSC=INDEX(FILNAM(LMIN:LMAX),';')
        IF (LSC.GT.0) THEN
          LSC=LSC+LMIN-1
          IF (LSC.LT.LMAX) VERS=FILNAM(LSC+1:LMAX)
          LMAX=LSC-1
        ELSE
          LDOT=0
          NDOT=0
          DO 50 L=LMAX,LMIN,-1
            IF (FILNAM(L:L).EQ.'.') THEN
              NDOT=NDOT+1
              IF (LDOT.EQ.0) LDOT=L
            ENDIF
 50       CONTINUE
          IF (NDOT.GT.1) THEN
            IF (LDOT.LT.LMAX) VERS=FILNAM(LDOT+1:LMAX)
            LMAX=LDOT-1
          ENDIF
        ENDIF
      ENDIF
C
C GET NAME AND TYPE
C -----------------
C
      IF (LMAX.LT.LMIN) RETURN
      LDOT=0
      DO 60 L=LMAX,LMIN,-1
      IF (FILNAM(L:L).EQ.'.') THEN
         LDOT=L
         GO TO 70
      ENDIF
60    CONTINUE
70    IF (LDOT.EQ.0) THEN
         NAME=FILNAM(LMIN:LMAX)
         RETURN
      ELSE
         IF (LDOT.GT.LMIN) NAME=FILNAM(LMIN:LDOT-1)
         IF (LDOT.LT.LMAX) TYPE=FILNAM(LDOT+1:LMAX)
      ENDIF
      END
C
C     ===================================
C_BEGIN_FDIR
      FUNCTION FDIR(FILNAM)
      CHARACTER*(*) FDIR
C     ===================================
C
C---- Returns the path (directory) of a file name or ' '
C
C Arguments:
C
C  FILNAM (I)   CHARACTER*(*): File name
C_END_FDIR
      CHARACTER FILNAM* (*)
      CHARACTER*1 NAME, TYPE, VERS
      EXTERNAL CCPPSF
C
      CALL CCPPSF(FILNAM, FDIR, NAME, TYPE, VERS)
      END
C
C     ====================================
C_BEGIN_FEXTN
      FUNCTION FEXTN(FILNAM)
      CHARACTER*(*) FEXTN
C     ====================================
C
C---- Returns the extension of a file name or ' '
C
C Arguments:
C
C  FILNAM (I)   CHARACTER*(*): File name
C_END_FEXTN
      CHARACTER FILNAM* (*)
      CHARACTER*1 PATH, NAME, VERS
      EXTERNAL CCPPSF
C
      CALL CCPPSF(FILNAM, PATH, NAME, FEXTN, VERS)
      END
C
C_BEGIN_FROOT
      FUNCTION FROOT(FILNAM)
      CHARACTER*(*) FROOT
C     ====================================
C
C---- Returns a file name minus an extension.
C
C Arguments:
C
C  FILNAM (I)   CHARACTER*(*): File name
C_END_FROOT
C
      CHARACTER FILNAM* (*)
      CHARACTER*1 PATH, TYPE, VERS
      EXTERNAL CCPPSF
C
      CALL CCPPSF(FILNAM, PATH, FROOT, TYPE, VERS)
      END

C  Tried to get rid of these, but some programs call it
C
C_BEGIN_LUNSTI
      FUNCTION LUNSTI(IDUM)
C     =====================
C
C Returns the fortran standard input unit number
C
C Arguments:
C ==========
C
C       IDUM (D)   Dummy
C_END_LUNSTI
C
      LUNSTI = 5
      END
C
      FUNCTION LUNSTO(IDUM)
C
C Returns the fortran standard output unit number
C
C Arguments:
C ==========
C
C       IDUM (I)   Dummy argument
C
      LUNSTO = 6
      END

      SUBROUTINE CCPPAG(IUN,NCOL,NLIN)
C     ================================
C
C---- This subroutine returns the number of columns and lines
C     for a printer output page on a given fortran unit number
C     if the information is available
C
C Arguments:
C ==========
C
C         IUN (I)   INTEGER: FORTRAN UNIT NUMBER
C        NCOL (O)   INTEGER: NUMBER OF COLUMNS IN THE PAGE
C        NLIN (O)   INTEGER: NUMBER OF LINES IN THE PAGE
C
C Return 80,132 unless a terminal whence 0,80
C_END_CCPPAG
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

      INTEGER FUNCTION CCPE2I (NAME, DEFVAL)
C     ======================================
C
C     Return an integer extracted from enviroment variable NAME.  If
C     NAME isn't defined, use DEFVAL as the default.  If the value of
C     NAME isn't a representation of an integer, abort.
C
C     Arguments
C     =========
C
C     NAME (I)    CHARACTER *(*)
C     DEFVAL (I)  INTEGER
C_END_CCPE2I
      CHARACTER *(*) NAME
      CHARACTER BUFFER*80, EMESS*100
      INTEGER DEFVAL, LENSTR
      EXTERNAL UGTENV, LENSTR
      CALL UGTENV (NAME, BUFFER)
      IF (BUFFER.EQ.' ') THEN
        CCPE2I = DEFVAL
        RETURN
      ENDIF
      READ (BUFFER, '(BN,I80)', ERR=99) CCPE2I
      RETURN 
 99   EMESS = ' Logical name '
      EMESS(LENSTR(EMESS)+2:) = NAME(1:LENSTR(NAME))
      IF(LENSTR(EMESS) .LE. 99) THEN
        EMESS(LENSTR(EMESS)+1:) =' should represent an integer and is: '
        IF(LENSTR(EMESS) .LE. 98) 
     .           EMESS(LENSTR(EMESS)+2:) = BUFFER(1:LENSTR(BUFFER))
      ENDIF
      CALL CCPERR (1, EMESS)
      END
C
C_BEGIN_CCPGI2
      SUBROUTINE CCPGI2(IVAL,IA,N)
C     ============================
C
C GET AN UNSIGNED INTEGER*2 VALUE FROM 0 TO 65535 FROM THE N'TH unsigned
C INTEGER*2 ELEMENT OF AN INTEGER (OR OTHER) ARRAY.
C
C (MUST BE IMPLEMENTED IF CCPBYT FUNCTION RETURNS .TRUE.)
C [added for LAUE]
C
C Arguments:
C ==========
C
C    IVAL (O)   INTEGER: THE RETURNED VALUE FROM 0 TO 65535
C      IA (I/O) INTEGER*2 ARRAY(*): FROM WHICH THE UNSIGNED INTEGER*2 VALUE
C               IS TO BE RETRIEVED
C       N (I)   INTEGER: POSITION IN 'IA' WHERE THE UNSIGNED INTEGER*2 VALUE
C               IS TO BE RETRIEVED
C_END_CCPGI2
C
C SPECIFICATION STATEMENTS
C ------------------------
C
      INTEGER*2 IA(*)
      INTEGER*2 JBYT(2)
      EQUIVALENCE (JA,JBYT(1))
      LOGICAL CALLED, LITEND
      EXTERNAL LITEND
      INTEGER IND
      SAVE CALLED, IND
      DATA CALLED/.FALSE./
C
      IF (.NOT.CALLED) THEN
        IF (LITEND(1)) THEN
          IND = 1
        ELSE
          IND = 2
        ENDIF
        CALLED=.TRUE.
      ENDIF
C
C GET UNSIGNED INTEGER*2
C ----------------------
C
      JA=0
      JBYT(IND)=IA(N)
      IVAL=JA
      END
C
C_BEGIN_CCPSI2
      SUBROUTINE CCPSI2(IVAL,IA,N)
C     ============================
C
C SET AN INTEGER VALUE FROM 0 TO 65535 INTO THE N'TH UNSIGNED INTEGER*2 ELEMENT
C OF AN INTEGER (OR OTHER) ARRAY.
C NOTE: NO OVERFLOW CHECKING IS DONE.
C
C (MUST BE IMPLEMENTED IF CCPBYT FUNCTION RETURNS .TRUE.)
C [for LAUE]
C
C Arguments:
C ==========
C
C    IVAL (I)   INTEGER: VALUE FROM 0 TO 65535
C
C      IA (I/O) INTEGER*2 ARRAY: WHERE THE UNSIGNED INTEGER*2 VALUE IS TO BE
C               INSERTED
C
C       N (I)   INTEGER: THE POSITION IN 'IA' WHERE THE UNSIGNED INTEGER*2
C               VALUE IS TO BE INSERTED
C_END_CCPSI2
C
C SPECIFICATION STATEMENTS
C ------------------------
C
      INTEGER*2 IA(*)
      INTEGER*2 JBYT(2)
      EQUIVALENCE (JA,JBYT(1))
      LOGICAL CALLED, LITEND
      EXTERNAL LITEND
      INTEGER IND
      SAVE CALLED, IND
      DATA CALLED/.FALSE./
C
      IF (.NOT.CALLED) THEN
        IF (LITEND(1)) THEN
          IND = 1
        ELSE
          IND = 2
        ENDIF
        CALLED=.TRUE.
      ENDIF
C
C SET UNSIGNED INTEGER*2
C ----------------------
C
      JA=IVAL
      IA(N)=JBYT(IND)
      END
C
      SUBROUTINE CCPSTB(IVAL,IA,N)
C     ============================
C
C SET AN INTEGER VALUE FROM 0 TO 255 INTO THE N'TH BYTE OF AN INTEGER
C (OR OTHER) ARRAY.
C NOTE: NO OVERFLOW CHECKING IS DONE.
C
C (MUST BE IMPLEMENTED IF CCPBYT FUNCTION RETURNS .TRUE.)
C [for LAUE]
C
C Arguments:
C ==========
C
C    IVAL (I)   INTEGER: VALUE FROM 0 TO 255
C      IA (I/O) BYTE ARRAY(*): WHERE THE BYTE VALUE IS TO BE INSERTED
C       N (I)   INTEGER: THE POSITION IN 'IA' WHERE THE BYTE VALUE IS TO
C               BE INSERTED
C_END_CCPSTB
C
C SPECIFICATION STATEMENTS
C ------------------------
C
      BYTE IA(*)
      BYTE JBYT(4)
      EQUIVALENCE (JA,JBYT(1))
      EQUIVALENCE (JA,JBYT(1))
      LOGICAL CALLED, LITEND
      EXTERNAL LITEND
      INTEGER IND
      SAVE CALLED, IND
      DATA CALLED/.FALSE./
C
      IF (.NOT.CALLED) THEN
        IF (LITEND(1)) THEN
          IND = 1
        ELSE
          IND = 4
        ENDIF
        CALLED=.TRUE.
      ENDIF
C
C SET BYTE
C --------
C
      JA=IVAL
      IA(N)=JBYT(IND)
      END

      LOGICAL FUNCTION LITEND(IDUM)
C        =============================
C
C---- Check endedness, Returns TRUE if little endian (VAX, FX2800,
C                                                   Ultrix)
C                              FALSE if big endian (IBM,IRIS,ESV)
C
C Arguments:
C ==========
C
C    IDUM (D)   DUMMY
C_END_LITEND
C
      INTEGER I, IDUM
      BYTE B(4)
      EQUIVALENCE (I,B(1))
C
C---- Initialise B
C
      DO 10 JDO=1,4
         B(JDO) = 0
 10   CONTINUE
C
      I = 1
C
      IF (B(1) .NE. 0) THEN
         LITEND = .TRUE.
      ELSE
         LITEND = .FALSE.
      END IF
C
      END

      SUBROUTINE CCPGTB(IVAL,IA,N)
C     ============================
C
C GET AN UNSIGNED BYTE VALUE FROM 0 TO 255 FROM THE N'TH BYTE OF AN INTEGER
C (OR OTHER) ARRAY.
C
C (MUST BE IMPLEMENTED IF CCPBYT FUNCTION RETURNS .TRUE.)
C [for LAUE]
C
C Arguments:
C ==========
C
C    IVAL (O)   INTEGER: THE RETURNED VALUE FROM 0 TO 255
C      IA (I/O) BYTE ARRAY(*): FROM WHICH THE BYTE VALUE IS TO BE RETRIEVED
C       N (I)   INTEGER: THE POSITION IN 'IA' WHERE THE BYTE VALUE IS
C               TO BE RETRIEVED
C_END_CCPGTB
C
C SPECIFICATION STATEMENTS
C ------------------------
C
      BYTE IA(*)
      BYTE JBYT(4)
      EQUIVALENCE (JA,JBYT(1))
      LOGICAL CALLED, LITEND
      EXTERNAL LITEND
      INTEGER IND
      SAVE CALLED, IND
      DATA CALLED/.FALSE./
C
      IF (.NOT.CALLED) THEN
        IF (LITEND(1)) THEN
          IND = 1
        ELSE
          IND = 4
        ENDIF
        CALLED=.TRUE.
      ENDIF
C
C GET BYTE
C --------
C
      JA=0
      JBYT(IND)=IA(N)
      IVAL=JA
      END
C
      SUBROUTINE CCPMVI (IARR1,IARR2,NUM)
C     =================================
C
C  This routine assigns the first NUM words of IARR2 to IARR1
C
C Arguments:
C ==========
C
C    IARR1 (O)   INTEGER ARRAY(*)
C    IARR2 (O)   INTEGER ARRAY(*)
C     NUM (I)   Number of words to copy
C_END_CCPMVI
C
C  Arguments
      INTEGER NUM
      REAL IARR1(*),IARR2(*)
C
      INTEGER J
C
      DO 10 J=1,NUM
   10 IARR1(J)=IARR2(J)
      END

      SUBROUTINE CCPMVR (ARR1,ARR2,NUM)
C     =================================
C
C  This routine assigns the first NUM elements of ARR2 to ARR1
C
C Arguments:
C ==========
C
C    ARR1 (O)   REAL ARRAY(*)
C    ARR2 (O)   REAL ARRAY(*)
C     NUM (I)   Number of words to copy
C_END_CCPMVR
C
C  Arguments
      INTEGER NUM
      REAL ARR1(*),ARR2(*)
C
      INTEGER J
C
      DO 10 J=1,NUM
   10 ARR1(J)=ARR2(J)
      END

      SUBROUTINE CCPZI (IARR1,NUM)
C     ===========================
C
C  This routine assigns zero to IARR1 using NUM words
C
C Arguments:
C
C    IARR1 (O)   INTEGER ARRAY(*): array to be zeroed
C     NUM (I)   INTEGER: Number of words
C_END_CCPZI
C
C  Arguments ..........
      INTEGER NUM, IARR1(*)
C
      INTEGER J
C
      DO 10 J=1,NUM
   10 IARR1(J)=0
      END
C
      SUBROUTINE CCPZR (ARR1,NUM)
C     ===========================
C
C  This routine assigns zero to ARR1 using NUM words
C
C Arguments:
C
C    ARR1 (O)   REAL ARRAY(*): array to be zeroed
C     NUM (I)   INTEGER: Number of words
C_END_CCPZR
C
C  Arguments ..........
      INTEGER NUM
      REAL ARR1(*)
C
      INTEGER J
C
      DO 10 J=1,NUM
   10 ARR1(J)=0.0
      END

C  Trivial function from symlib.g
C     ============================
      LOGICAL FUNCTION HKLEQ(IH,KH)
C     =============================
C
C---- Returns true if indices ih = kh
C
C     .. Array Arguments ..
      INTEGER IH(3),KH(3)
C     ..
C
      HKLEQ = .FALSE.
C
      IF (IH(1).EQ.KH(1) .AND. IH(2).EQ.KH(2) .AND.
     +    IH(3).EQ.KH(3)) HKLEQ = .TRUE.
C
      END
C
C  General hashing routines. Moved here from symlib.f
C
C     =======================================
      INTEGER FUNCTION CCP4_HASH_LOOKUP(NSER)
C     =======================================
C
C---- The function ccp4_hash_lookup returns the value nfind (which was
C     input when setting up the function in the subroutine ccp4_hash_setup)
C     for the large range variable nser.  Uses hashing. (see comments for
C     CCP4_HASH_SETUP for description of hashing method).
C
      IMPLICIT NONE
C     .. Parameter (table size: MUST BE A PRIME NUMBER)
      INTEGER KPRI
      PARAMETER (KPRI=1999)
C
C     .. Scalar Arguments ..
      INTEGER NSER
C     ..
C     .. Arrays in Common ..
      INTEGER IT
C     ..
C     .. Local Scalars ..
      INTEGER NDX,NSER4
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     ..
C     .. Common blocks ..
      COMMON /LOOK/IT(2,KPRI)
      SAVE /LOOK/
C     ..
C
      NSER4 = NSER
C
   10 CONTINUE
C
      NDX = MOD(NSER4,KPRI) + 1
      IF (NSER.NE.IT(1,NDX)) THEN
        IF (IT(1,NDX).NE.0) THEN
          NSER4 = NSER4 + 3
          GO TO 10
        END IF
      END IF
C
      CCP4_HASH_LOOKUP = IT(2,NDX)
C
      END
C
C
C     ======================================
      SUBROUTINE CCP4_HASH_SETUP(NSER,NFIND)
C     ======================================
C
C---- This subroutine sets up a value for the function ccp4_hash_lookup
C     when ccp4_hash_lookup(nser) is later evaluated it will return nfind
C     this function will allow the efficient retrieval of an identifier
C     for a large range variable (such as a crystal number).  the values
C     of the function ccp4_hash_lookup(nser) are stored in the array
C     it(2, kpri) where kpri is the prime number used to generate the
C     function
C     The array it  lives in the common look which is shared by
C     ccp4_hash_setup and the function ccp4_hash_lookup
C
C     NOTES: A hash table is a way of storing information so that it
C     easily be retrieved without the need for indexing or long searches.
C     NSER is referred to as the "key", which is "hashed" (computer-
C     science speak for "messed up") by the hashing function (in this
C     case MOD(NSER4,KPRI) + 1) to determine where the value pair will
C     be stored. The function LOOKUP can then search on the same basis
C     when supplied with the key, to retreive the pair in (at most) 3
C     calculations. Note that KPRI (the table size) MUST BE A PRIME in
C     order for this method to work.
C
C     IT(1, NDX) = NSER,  IT(2, NDX) = NFIND
C
      IMPLICIT NONE
C     .. Parameter (table size: MUST BE A PRIME NUMBER)
      INTEGER KPRI
      PARAMETER (KPRI=1999)
C
C     .. Scalar Arguments ..
      INTEGER NFIND,NSER
C     ..
C     .. Arrays in Common ..
      INTEGER IT
C     ..
C     .. Local Scalars ..
      INTEGER NDX,NSER4
      CHARACTER STROUT*140
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     ..
C     .. Common blocks ..
      COMMON /LOOK/IT(2,KPRI)
      SAVE /LOOK/
C     ..
C
      NSER4 = NSER
   10 CONTINUE
      NDX = MOD(NSER4,KPRI) + 1
      IF ((NSER4-NSER) .GE. 3*KPRI) THEN
         WRITE (STROUT, '(A,I8)')
     $     ' **** Error in SETUP: overflowed hash table, size ', KPRI
         CALL PUTLIN(STROUT,'CURWIN')
         CALL CCPERR(1,'*** Filled hash table in SETUP ***')
      ENDIF
      IF (IT(1,NDX).NE.0) THEN
        NSER4 = NSER4 + 3
        GO TO 10
      END IF
C
      IT(1,NDX) = NSER
      IT(2,NDX) = NFIND
      RETURN
      END
C
C     =============================
      SUBROUTINE CCP4_HASH_ZEROIT()
C     =============================
C
      IMPLICIT NONE
C     .. Parameter (table size: MUST BE A PRIME NUMBER)
      INTEGER KPRI
      PARAMETER (KPRI=1999)
C
C     .. Arrays in Common ..
      INTEGER IT
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
C     ..
C     .. Common blocks ..
      COMMON /LOOK/IT(2,KPRI)
      SAVE /LOOK/
C
      DO 20 I = 1,KPRI
        IT(1,I) = 0
        IT(2,I) = 0
   20 CONTINUE
C
      END
C
