C
C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
C   These are supposedly-machine-independent low-level routines.
C     They're actually machine-dependent at least insofar as some
C     contain non-standard code, but they do compile with the compilers
C     tried on unix as well as VMS.
C
C     fixme: tidy this up so that the endian-dependent routines (for
C     laue) are entries with only one check on the endianness and set
C     ind2, ind4 etc. for *2, *4 references.  also amalgamate ccppsf and
C     fdir/fext/froot.  also add tests of these routines to testlib.
C
C     ccplib.for,v 1.7 1992/09/14 18:47:13 fx Exp
C     
C      CCFILL    Set specified number of elements of byte array
C      CCPASZ    set array size suitable for current working set
C      CCPBYI    Copy array of unsigned (or signed) bytes into integer array
C      CCPBYT    Indicate whether byte handling is available
C      CCPDAT    Get calendar date
C      CCPDEX    Periodicity reduction of 1024 (for PROLSQ)
C      CCPDPN    more friendly CCPOPN
C      CCPERR    Report error or normal termination and stop
C      CCPEXS    test if file exists
C      CCPFIL    Return file name for an opened file
C      CCPGI2    Get unsigned integer*2 value from 0 to 65535 from N'th
C                unsigned integer*2 element of array. 
C      CCPGTB    Get unsigned byte value from 0 to 255 from N'th byte of
C                array.
C      CCPI2I    Copy an array of INTEGER*2 elements into an integer array
C      CCPII2    Copy array of integers into array of INTEGER*2 elements. 
C      CCPIBY    Copy array of integers into array of bytes. 
C      CCPMDE    If byte handling available return nos. of bytes for map
C                modes
C      CCPMVB    Move bytes from one non-character array to another if
C                byte handling is available
C      CCPMVI    Move words from one non-character array to another
C                using a simple loop 
C      CCPONL    See if program is being run interactively
C      CCPOVP    See if overprinting is available
C      CCPPAG    Set paging parameters if available
C      CCPPSF    Parse file name into components
c      CCPRCS    Like CCPVRS but use RCS-format date string
C      CCPRVR    Read variable length record of unknown length
C      CCPSI2    Set integer value from 0 to 65535 into the N'th
C                unsigned integer*2 element of an array. 
C      CCPSPW    Spawns sub-process
C      CCPSTB    Set integer value from 0 to 255 into N'th byte of array.
C      CCPSUM    Sum the elements of an array
C      CCPTIM    Get CPU and Elapsed times
C      CCPTOI    Convert n'th byte or I*2 in a non-character array to an
C                integer
C      CCPUFL    Supress underflow messages
C      CCPULI    See if underline option is available
C      CCPUPC    Convert a string to upper case
C      CCPVRS    Print program version number and date header
C      CCPZBI    Sets an array of bytes to zero
C      CCPZI     Set 'n' words of an array to zero using a simple loop
C      FDIR      Returns the directory part of a file name
C      FEXTN     Returns the extension of a file name
C      FROOT     Returns the root of a file name
C      LENSTR    length of string to last non-space
C      LITEND    determine endianness
C      NBITST    Return the (unsigned) integer value held within a bit
C                field in a word 
C      NOCRLF    write line supressing cr/lf to standard output
C      QPRINT    write debug messages
C      STBITS    Set a bit field within a word to a given (unsigned)
C                integer value
CC
C
C
      SUBROUTINE CCFILL(ARR1,SCAL,NTIMES)
C     ===================================
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
C----  On the Vax set JSIZE as dimension of array A set to the working
C      set size less a guessed program size, to minimise page faults in
C      using the array. Only the working set quota is used 
C      (not the extent)
C      MINSIZ  <= JSIZE <= initial JSIZE
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
C SUBROUTINE 'CCPBYI'
C ===================
C
C COPY AN ARRAY OF UNSIGNED (OR SIGNED) BYTES INTO AN INTEGER ARRAY
C
C (MUST BE IMPLEMENTED IF CCPBYT FUNCTION RETURNS .TRUE.)
C [added for LAUE]
      SUBROUTINE CCPBYI(IA,IBYT,NB)
C
C PARAMETERS
C
C      IA (O)   ARRAY TO RETURN INTEGER VALUES
C    IBYT (I)   ARRAY HOLDING BYTE DATA (MAY BE AN INTEGER ARRAY FOR EXAMPLE
C               WITH DATA PACKED INTO ADJACANT BYTES
C      NB (I)   IF >0, THE NUMBER OF UNSIGNED BYTES TO BE COPIED 
C               IF <0, -THE NUMBER OF SIGNED BYTES TO BE COPIED
C
C SPECIFICATION STATEMENTS
C ------------------------
C
      INTEGER IA(*)
      BYTE IBYT(*)
      BYTE JBYT(4)
      EQUIVALENCE (JA,JBYT(1))
      LOGICAL CALLED, LITEND
      INTEGER IND
      EXTERNAL LITEND
      SAVE CALLED, IND
      DATA CALLED/.FALSE./
C
      IF (.NOT.CALLED) THEN
        CALLED=.TRUE.
        IF (LITEND(1)) THEN
          IND = 1
        ELSE
          IND = 4
        ENDIF
      ENDIF
C
C COPY DATA
C ---------
C
      NE = NB
      IF (NE.GT.0) THEN
         JA=0
         DO 10 I=1,NE
           JBYT(IND)=IBYT(I)
           IA(I)=JA
 10      CONTINUE
      ELSE
         NE = -NE
         DO 20 I=1,NE
         IA(I) = IBYT(I)
20       CONTINUE
      END IF
      END
C
C
C
      LOGICAL FUNCTION CCPBYT(NBW)
C     ============================
C
C---- This function indicates whether byte handling is available or not.
C      if a value of .true. is returned then the subroutines ccpmde and
C      ccpmvb must be fully implemented.
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
C---- This subroutine returns the date if available
C
C PARAMETERS
C ==========
C
C      CALDAT (O)   CHARACTER*8 VARIABLE RETURNING DATE AS DD/MM/YY
C                   (RETURNED AS A BLANK STRING IF NOT AVAILABLE)
C
C SPECIFICATION STATEMENTS
C ------------------------
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
      CALL UIDATE(IM,ID,IY)
      WRITE (CALDAT,FMT=6000) ID,IM,IY
C
C---- Format statements
C
 6000 FORMAT (I2,'/',I2,'/',I2)
C
      END
C
C
C
      SUBROUTINE CCPDEX(INDX,N)
C     ========================
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
      SUBROUTINE CCPDPN(IUN,LOGNAM,STATUS,TYPE,LREC,IFAIL)
C     ====================================================
C
C---- Calls CCPOPN to open a file, but with mnemonic arguments
C
C PARAMETERS
C ==========
C
C         IUN (I)   UNIT NUMBER
C      LOGNAM (I)   LOGICAL FILE NAME
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
C     [STATUS and TYPE are case-insensitive]
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
C     .. Scalar Arguments ..
      INTEGER IFAIL,IUN,LREC
      CHARACTER LOGNAM* (*),STATUS* (*),TYPE* (*)
C     ..
C     .. Local Scalars ..
      INTEGER ISTAT,ITYPE
      CHARACTER ERRSTR*80
C     ..
C     .. Local Arrays ..
      CHARACTER TYPES(4)*2,STATS(6)*8, STAT*8, TYP*2
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPOPN
C     ..
C     .. Data statements ..
      DATA STATS/'UNKNOWN','SCRATCH','OLD','NEW','READONLY','PRINTER'/
      DATA TYPES/'F','U','DF','DU'/
C     ..
C
      STAT = STATUS
      TYP = TYPE
      CALL CCPUPC(STAT)
      CALL CCPUPC(TYP)
      DO 10 ISTAT = 1,6
        IF (STAT.EQ.STATS(ISTAT)) GO TO 20
   10 CONTINUE
      WRITE (ERRSTR,FMT=6000) STATUS
      CALL CCPERR(1,ERRSTR)
C
   20 DO 30 ITYPE = 1,4
        IF (TYP.EQ.TYPES(ITYPE)) GO TO 40
   30 CONTINUE
      WRITE (ERRSTR,FMT=6002) TYPE
      CALL CCPERR(1,ERRSTR)
C
   40 CALL CCPOPN(IUN,LOGNAM,ISTAT,ITYPE,LREC,IFAIL)
C
 6000 FORMAT ('CCPDPN: illegal status : ',A)
 6002 FORMAT ('CCPDPN: illegal type: ',A)
      END
C
C
C  
C     ===============================
      SUBROUTINE CCPERR(ISTAT,ERRSTR)
C     ===============================
C
C
C     Report error or normal termination and stop.  Also reports latest
C     system error (at least under un*x)
C
C     Parameters:
C     ISTAT (I)   exit status (0 for normal termination)
C     ERRST (I)   message
C     
      CHARACTER ERRSTR*(*), ERRBUF*100
      INTEGER ISTAT
      EXTERNAL VAXVMS
      LOGICAL VAXVMS
C
C
      IF (ISTAT.NE.0) THEN
        CALL UGERR(0,ERRBUF)
        IF (ERRBUF .NE. ' ') THEN
          CALL QPRINT(0,'Last system error message:')
          CALL QPRINT(0,ERRBUF)
        ENDIF
      ENDIF
      CALL QPRINT(0,ERRSTR)
      CALL GETELAPSED
      CALL EXIT(ISTAT)
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
C     .. Scalar Arguments ..
      CHARACTER NAME* (*)
C     ..
C     .. Local Scalars ..
      CHARACTER*255 NAMFIL
C     ..
      NAMFIL = ' '
      CALL UGTENV(NAME,NAMFIL)
      IF (NAMFIL.EQ.' ') NAMFIL = NAME
C
      INQUIRE (FILE=NAMFIL,EXIST=CCPEXS)
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
C     .. Parameters ..
      INTEGER ILIMIT,ISTRLN,IENV
      PARAMETER (ILIMIT=150,ISTRLN=200,IENV=20)
C     ..
C     .. Local Scalars ..
      INTEGER EXEC,HELP,IARG,ICOUNT,IEND,IERR,II,ILOOP,IOSTAT,ISKIP,
     +        ISTART,IUNIT,LOOP,RDENVF,RDLOGF,IHELP,LREC,IFAIL
      LOGICAL DINIT,EINIT,VAX
      CHARACTER FILNAM* (ISTRLN), LINE* (ISTRLN),
     +     ENVFIL* (ISTRLN), LOGFIL* (ISTRLN), LOGNAM* (ISTRLN),
     +     TEMP* (ISTRLN)
C     ..
C     .. Local Arrays ..
      CHARACTER ENAME(ILIMIT)* (IENV),ETYPE(ILIMIT)* (5),
     +          EXTN(ILIMIT)* (4)
C     ..
C     .. External Functions ..
      INTEGER LENSTR
C     don't declare iargc
      LOGICAL VAXVMS
      CHARACTER FEXTN* (ISTRLN), FDIR*(ISTRLN)
      EXTERNAL LENSTR,VAXVMS,FEXTN
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPERR,CCPUPC,UGTARG,INITFYP,QPRINT,CSETNV,UGERR,UGTENV
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ICHAR,INDEX
C     ..
C     .. Save statement ..
      SAVE
C     .. Data statements ..
      DATA ICOUNT/0/,IHELP/1/,DINIT/.TRUE./,EINIT/.TRUE./,
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
        CALL UGTARG(ILOOP,LINE)
        CALL CCPUPC(LINE)
C
        IF (LINE(1:1).EQ.'-') THEN
          DO 20 II = 2,LENSTR(LINE)
            IF (LINE(II:II).EQ.'V' .OR. LINE(II:II).EQ.'H') THEN
              HELP = ILOOP + ISKIP
              IF (HELP.GT.IARG) THEN
                IHELP = 1
              ELSE
                CALL UGTARG(HELP,TEMP)
                IHELP = ICHAR(TEMP(1:1)) - ICHAR('0')
                IF (IHELP.LT.0 .OR. IHELP.GT.9) IHELP = 1
              END IF
              ISKIP = ISKIP + 1
            ELSE IF (LINE(II:II).EQ.'N') THEN
              DINIT = .FALSE.
              EINIT = .FALSE.
            ELSE IF (LINE(II:II).EQ.'D') THEN
              RDLOGF = ILOOP + ISKIP
              IF (RDLOGF.GT.IARG) CALL CCPERR(1,'Use: -d filename')
              CALL UGTARG(RDLOGF,LOGFIL)
              DINIT = .TRUE.
              ISKIP = ISKIP + 1
            ELSE IF (LINE(II:II).EQ.'E') THEN
              RDENVF = ILOOP + ISKIP
              IF (RDENVF.GT.IARG) CALL CCPERR(1,'Use: -e filename')
              CALL UGTARG(RDENVF,ENVFIL)
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
          IF (FDIR(ENVFIL).NE.' ') II=0
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
                IF (II.GT.ISTRLN) CALL CCPERR(1,
     +               'environ path name too long')
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
                IF (II.GT.ISTRLN) CALL CCPERR(1,
     +               'environ path name too long')
                FILNAM(II:II)='/'
              ENDIF
            ENDIF
          ELSE
            II = 0
          ENDIF
        ENDIF
        FILNAM(II+1:) = ENVFIL
        IF (II.GT.ISTRLN) CALL CCPERR(1, 'environ path name too long')
        CALL QPRINT(2,'Opening file '//FILNAM)
        IFAIL=0
        IXUNIT = IUNIT
        IF (IHELP.LT.2) IXUNIT = -IUNIT
        CALL CCPDPN (IXUNIT,FILNAM,'READONLY','F',LREC,IFAIL)
   30   CONTINUE
        READ (UNIT=IUNIT,FMT=6000,END=40,ERR=80,IOSTAT=IERR) LINE
        TEMP = LINE
C       comments from `!' or `#' to end-of-line
        II = INDEX(LINE,'#')
        IF (II.NE.0) LINE(II:) = ' '
        II = INDEX(LINE,'!')
        IF (II.NE.0) LINE(II:) = ' '
        IF (LINE.NE.' ') THEN
          ICOUNT = ICOUNT + 1
          IF (ICOUNT.GT.ILIMIT) CALL CCPERR(1,
     +         'Too many logical names in environ file: '//TEMP)
          ISTART = INDEX(LINE,'=')
          IF (ISTART.EQ.0)
     +         CALL CCPERR(1,'Missing = in environ file: '//TEMP)
          ENAME(ICOUNT) = LINE(1:ISTART-1)
          EXTN(ICOUNT) = '.'//FEXTN(LINE)
          IF (EXTN(ICOUNT).EQ.'.')
     +         CALL CCPERR(1, 'Bad extension in environ file: '//TEMP)
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
          IF (FDIR(ENVFIL).NE.' ') II=0
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
                IF (II.GT.ISTRLN) CALL CCPERR(1,
     +               'default.def path name too long')
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
                IF (II.GT.ISTRLN) CALL CCPERR(1,
     +               'default.def path name too long')
                FILNAM(II:II)='/'
              ENDIF
            ENDIF
          ELSE
            II = 0
          ENDIF
        ENDIF
        IF (II.GT.ISTRLN) CALL CCPERR(1,
     +       'default.def path name too long')
        FILNAM(II+1:) = LOGFIL
        CALL QPRINT(2,'Opening file '//FILNAM)
        IFAIL=0
        IXUNIT = IUNIT
        IF (IHELP.LT.2) IXUNIT = -IUNIT
        CALL CCPDPN (IXUNIT,FILNAM,'READONLY','F',LREC,IFAIL)
   50   CONTINUE
        READ (UNIT=IUNIT,FMT=6000,END=60,ERR=80,IOSTAT=IERR) LINE
        TEMP = LINE
C       comments from `!' or `#' to end-of-line
        II = INDEX(LINE,'#')
        IF (II.NE.0) LINE(II:) = ' '
        II = INDEX(LINE,'!')
        IF (II.NE.0) LINE(II:) = ' '
        IF (LINE.NE.' ') THEN
          II = INDEX(LINE,'=')
          IF (II.EQ.0)
     +         CALL CCPERR(1,'Missing = in defaults file: '//TEMP)
          LOGNAM = LINE(1:II-1)
          FILNAM = LINE(II+1:)
C
C---- here skip = .true. in csetnv means don't override existing logical
C         name (defined in environment)
C
          CALL CSETNV(LOGNAM,FILNAM,ENAME,ETYPE,EXTN,ICOUNT,.TRUE.)
        END IF
        GO TO 50
   60   CLOSE (UNIT=IUNIT)
      END IF
C
C---- Loop through command line arguments
C
      CALL QPRINT(2,'Processing Command Line Arguments')
      DO 70 LOOP = ILOOP,IARG,2
        CALL UGTARG(LOOP,LOGNAM)
         CALL CCPUPC(LOGNAM)
        CALL UGTARG(LOOP+1,FILNAM)
        IF (FILNAM.EQ.' ') CALL CCPERR(1,
     +       'Use: <logical name> <filename> ...')
C
C---- here skip = .false. in csetnv means override logical name
C       defined in the environment
C
        CALL CSETNV(LOGNAM,FILNAM,ENAME,ETYPE,EXTN,ICOUNT,.FALSE.)
   70 CONTINUE
      CALL QPRINT(2,'End of pre-processing stage')
      RETURN
C
 80   CALL CCPERR(1,'Error reading environ or default file')
C
 6000 FORMAT (A)
      END
C
C
C SUBROUTINE 'CCPGI2'
C ===================
C
C GET AN UNSIGNED INTEGER*2 VALUE FROM 0 TO 65535 FROM THE N'TH unsigned
C INTEGER*2 ELEMENT OF AN INTEGER (OR OTHER) ARRAY.
C
C (MUST BE IMPLEMENTED IF CCPBYT FUNCTION RETURNS .TRUE.)
C [added for LAUE]
      SUBROUTINE CCPGI2(IVAL,IA,N)
C
C PARAMETERS
C
C    IVAL (O)   THE RETURNED INTEGER VALUE FROM 0 TO 65535
C      IA (I/O) THE ARRAY FROM WHICH THE UNSIGNED INTEGER*2 VALUE IS TO BE 
C               RETRIEVED
C       N (I)   THE POSITION IN 'IA' WHERE THE UNSIGNED INTEGER*2 VALUE IS TO 
C               BE RETRIEVED
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
C
C SUBROUTINE 'CCPGTB'
C ===================
C
C GET AN UNSIGNED BYTE VALUE FROM 0 TO 255 FROM THE N'TH BYTE OF AN INTEGER
C (OR OTHER) ARRAY.
C
C (MUST BE IMPLEMENTED IF CCPBYT FUNCTION RETURNS .TRUE.)
C [for LAUE]
      SUBROUTINE CCPGTB(IVAL,IA,N)
C
C PARAMETERS
C
C    IVAL (O)   THE RETURNED INTEGER VALUE FROM 0 TO 255
C      IA (I/O) THE ARRAY FROM WHICH THE BYTE VALUE IS TO BE RETRIEVED
C       N (I)   THE POSITION IN 'IA' WHERE THE BYTE VALUE IS TO BE RETRIEVED
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
C
C SUBROUTINE 'CCPI2I'
C ===================
C
C Copy an array of INTEGER*2 elements into an integer array
C
C (Must be implemented if ccpbyt function returns .TRUE.)
C [for LAUE]
      SUBROUTINE CCPI2I(IA,I2,NE,SIGNED,SWAPB)
C
C Parameters
C
C      IA (O)   Array to return INTEGER values
C      I2 (I)   Array holding INTEGER*2 data (may be an INTEGER array for 
C               example with data packed into adjacant INTEGER*2 elements
C      NE (I)   The number of elements to be copied
C  SIGNED (I)   Logical flag =.TRUE.  Copy as signed integer*2 values
C                            =.FALSE. Copy as unsigned integer*2 values
C   SWAPB (I)   Logical flag =.TRUE.  Swap bytes in the integer*2 elements
C                            =.FALSE. Do not swap bytes
C
C====== Specification statements
C
      LOGICAL SIGNED, SWAPB
      INTEGER IA(*)
      INTEGER*2 I2(*)
      INTEGER*2 J2(2)
      INTEGER*2 IEIGHT, I255
      PARAMETER (I255=255)
      EQUIVALENCE (JA,J2(1))
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
C====== Swap bytes if required
C
      IEIGHT = 8
      IF (SWAPB) THEN
         DO 10 I = 1,NE
            I2(I) = IOR(IAND(ISHFT(I2(I),-IEIGHT),I255),
     +              ISHFT(I2(I),IEIGHT))
10       CONTINUE
      END IF
C
C====== Copy data
C
      IF (SIGNED) THEN
         DO 20 I=1,NE
            IA(I) = I2(I)
20       CONTINUE
      ELSE
         JA=0
         DO 30 I=1,NE
         J2(IND)=I2(I)
         IA(I)=JA
30       CONTINUE
      END IF
      END
C
C
C
C SUBROUTINE 'CCPIBY'
C ===================
C
C COPY AN ARRAY OF INTEGERS INTO AN ARRAY OF UNSIGNED (OR UNSIGNED) BYTES. 
C NOTE: NO OVERFLOW CHECKING IS DONE.
C
C (MUST BE IMPLEMENTED IF CCPBYT FUNCTION RETURNS .TRUE.)
C [for LAUE]
      SUBROUTINE CCPIBY(IBYT,IA,NB)
C
C PARAMETERS
C
C    IBYT (O)   ARRAY RETURNING BYTE DATA (MAY BE AN INTEGER ARRAY FOR EXAMPLE
C               WITH DATA PACKED INTO ADJACANT BYTES
C      IA (I)   ARRAY HOLDING INTEGER VALUES
C      NB (I)   IF >0, THE NUMBER OF ELEMENTS TO BE COPIED TO UNSIGNED BYTES
C               IF <0, -THE NUMBER OF ELEMENTS TO BE COPIED TO SIGNED BYTES
C
C SPECIFICATION STATEMENTS
C ------------------------
C
      INTEGER IA(*)
      BYTE IBYT(*)
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
C COPY DATA
C ---------
C
      NE = NB
      IF (NE.GT.0) THEN
         DO 10 I=1,NE
         JA=IA(I)
         IBYT(I)=JBYT(IND)
10       CONTINUE
      ELSE
         NE = -NE
         DO 20 I=1,NE
         IBYT(I) = IA(I)
20       CONTINUE
      END IF
      END
C
C
C
C SUBROUTINE 'CCPII2'
C ===================
C
C Copy an array of integers into an array of INTEGER*2 elements. 
C NOTE: No overflow checking is done.
C
C (Must be implemented if ccpbyt function returns .TRUE.)
C [for LAUE]
      SUBROUTINE CCPII2(I2,IA,NE,SIGNED,SWAPB)
C
C Parameters
C
C      I2 (O)   Array returning INTEGER*2 data (may be an INTEGER array for 
C               example with data packed into adjacant INTEGER*2 elements
C      IA (I)   Array holding INTEGER values
C      NE (I)   The number of elements to be copied
C  SIGNED (I)   Logical flag =.TRUE.  Copy as signed integer*2 values
C                            =.FALSE. Copy as unsigned integer*2 values
C   SWAPB (I)   Logical flag =.TRUE.  Swap bytes in the integer*2 elements
C                            =.FALSE. Do not swap bytes
C
C====== Specification statements
C
      LOGICAL SIGNED, SWAPB
      INTEGER IA(*)
      INTEGER*2 I2(*)
      INTEGER*2 J2(2)
      INTEGER*2 IEIGHT, I255
      PARAMETER (I255=255)
      EQUIVALENCE (JA,J2(1))
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
C====== Copy data
C
      IEIGHT = 8
      IF (SIGNED) THEN
         DO 10 I=1,NE
            I2(I) = IA(I)
10       CONTINUE
      ELSE
         DO 20 I=1,NE
            JA=IA(I)
            I2(I)=J2(IND)
20       CONTINUE
      ENDIF
C
C====== Swap bytes if required
C
      IF (SWAPB) THEN
         DO 30 I = 1,NE
            I2(I) = IOR(IAND(ISHFT(I2(I),-IEIGHT),I255),
     +              ISHFT(I2(I),IEIGHT))
30       CONTINUE
      END IF
      END
C
C
C
      SUBROUTINE CCPLWC(STRING)
C     ========================
C
C---- convert a text string to lower case in situ
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
      END
C
C
      SUBROUTINE CCPMVI (ARR1,ARR2,NUM)
C     =================================
C
C  This routine assigns the first NUM words of ARR2 to ARR1
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
C
C
C
      LOGICAL FUNCTION CCPONL(IDUM)
C     ============================
C
C---- This function determines whether a program is being run on-line
C     if this information is available
C
C PARAMETERS
C ==========
C
C        IDUM (I)   DUMMY PARAMETER
C
C RETURNS .TRUE.  IF PROGRAM IS BEING RUN ON-LINE
C RETURNS .FALSE. IF BATCH MODE OR STATUS UNKNOWN
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
CC      CCPONL = (ISATTY(6))  direct fortran call
C
      END
C
C
C
C SUBROUTINE 'CCPPSF'
C ===================
C
C PARSE FILE NAME INTO COMPONENTS
C
C NOTE: THE ROUTINE  CONTAINS MACHINE DEPENDENT CODE
C
      SUBROUTINE CCPPSF(FILNAM,PATH,NAME,TYPE,VERS)
C
C PARAMETERS
C
C      FILNAM (I)   FILE NAME STRING (NO EMBEDDED BLANKS ASSUMED)
C        PATH (O)   CHARACTER STRING RETURNING PATH OR, FOR VAX VMS,
C                   THE PART OF THE FILE SPECIFICATION UP TO THE
C                   END OF THE DIRECTORY SPECIFICATION (BLANK IF NONE)
C                   (INCLUDES TERMINATING ] or : or /)
C        NAME (O)   CHARACTER STRING RETURNING NAME.  (BLANK IF NONE)
C        TYPE (O)   CHARACTER STRING RETURNING FILE TYPE/EXTENSION
C                   (BLANK IF NONE)
C        VERS (O)   CHARACTER STRING RETURNING THE VERSION.
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
C
C SPECIFICATION STATEMENTS
C ------------------------
C
      CHARACTER*(*) FILNAM,PATH,NAME,TYPE,VERS
      EXTERNAL VAXVMS
      LOGICAL VAXVMS, VMS
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
      ELSE
        DO 21 L=LMAX,LMIN,-1
          IF (FILNAM(L:L).EQ.'/')GO TO 30
 21     CONTINUE
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
C
C     
      SUBROUTINE CCPRCS(ILP,PROG,RCSDAT)
C     ==================================
C
C     Interface to CCPVRS using RCS-format date e.g.,
C     '1992/09/14 18:47:13' or its form expanded with RCS
C     option `-kv' as for a CVS export, in which case it will have the
C     `1992/09/14 18:47:13' stripped.
C     
      CHARACTER*(*) RCSDAT, PROG
      CHARACTER*8 DATE
      INTEGER ILP
      EXTERNAL CCPVRS
C
      IF (RCSDAT(:7) .EQ. '$Date: ') THEN
C       raw form (not exported)
        DATE = '  /  /'
        DATE(1:2) = RCSDAT(13:14)
        DATE(4:5) = RCSDAT(16:17)
        DATE(7:8) = RCSDAT(10:11)
      ELSE IF (LEN(RCSDAT).GE.10 .AND. RCSDAT(:2).EQ.'19') THEN
C       after export
        DATE = '  /  /'
        DATE(1:2) = RCSDAT(9:10)
        DATE(4:5) = RCSDAT(6:7)
        DATE(7:8) = RCSDAT(3:4)
      ELSE
C       fallback
        DATE = ' '
      ENDIF
      CALL CCPVRS(ILP,PROG,DATE)
      END
C
C
C
C     =======================================================
      SUBROUTINE CSETNV(LNAME,FILNAM,ENAME,ETYPE,EXTN,ICOUNT,LSKIP)
C     =======================================================
C
C     Associate `logical name' LNAME with value FILNAM using environment
C     an variable LNAME.  It is passed arrays of (name, type, extension)
C     for ICOUNT number of name lines read from environ.def.  Doesn't
C     re-define existing name if LSKIP is true.
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
      INTEGER I,II,ISTAT,JJ
      LOGICAL VAX,EXIST
      CHARACTER ERRSTR* (ISTRLN),LIBFIL* (ISTRLN),PROGNM* (ISTRLN),
     +          TMPNAM* (ISTRLN),LINE* (ISTRLN),SCRFIL* (ISTRLN)
C     ..
C     .. External Functions ..
      INTEGER ACCESS,GETPID,LENSTR
      LOGICAL VAXVMS
      CHARACTER FDIR* (ISTRLN),FEXTN* (ISTRLN),FROOT* (ISTRLN)
      EXTERNAL ACCESS,GETPID,LENSTR,VAXVMS,FDIR,FEXTN,FROOT
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPERR,UGTARG,QPRINT,UGTENV,USTENV
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX
C     ..
      SAVE
      DATA PROGNM/' '/
C
C---- Check Logical Name does not already exist (unless processing
C     command line, in which case LSKIP will be true to override
C     environment) 
C
      CALL UGTENV(LNAME,TMPNAM)
      IF (TMPNAM.NE.' ' .AND. LSKIP ) RETURN
      VAX = VAXVMS()
C
C---- Get program name (argv[0]), but check if we have it already
C
      IF (PROGNM.EQ.' ') THEN
        CALL UGTARG(0,TMPNAM)
        PROGNM = FROOT(TMPNAM)
      ENDIF
C
C---- look through list for a match (possibly abbreviated) [is this
C     abbreviation possibility documented?]
C
      DO 10 JJ = 1,ICOUNT
        IF (ENAME(JJ).EQ.LNAME(1:LENSTR(ENAME(JJ)))) GO TO 20
   10 CONTINUE
C
C---- Unknown logical name add it to the list.
C
      TMPNAM = 'Non standard logical name '
      TMPNAM(27:) = LNAME
      CALL QPRINT(2,TMPNAM)
      ICOUNT = ICOUNT + 1
      IF (ICOUNT.GT.ILIMIT)
     +     CALL CCPERR(1,'Too many logical names')
      ENAME(ICOUNT) = LNAME
      ETYPE(ICOUNT) = 'undef'
      EXTN(ICOUNT) = FEXTN(FILNAM)
      JJ = ICOUNT
C
C---- Known logical name processing
C
   20 IF (FEXTN(FILNAM).EQ.' ') THEN
C
C---- Add extension
C
        IF (FILNAM.EQ.'/dev/null' .OR. FILNAM.EQ.'NL:') THEN
C          but not if FILNAM is /dev/null or NL:
          GOTO 333
        ELSE
          II = LENSTR(FILNAM) + 1
          FILNAM(II:) = EXTN(JJ)
        ENDIF
      ENDIF
      IF (FDIR(FILNAM).EQ.' ') THEN
CCC       this didn't agree with documentation:
CCC        IF (EXTN(JJ).EQ.'.lib' .OR. EXTN(JJ).EQ.'.prt' .OR.
CCC     +      EXTN(JJ).EQ.'.bes' .OR. EXTN(JJ).EQ.'.dic') THEN
        TMPNAM = FEXTN(FILNAM)
        IF (VAX) CALL CCPLWC(TMPNAM)
        IF (TMPNAM.EQ.'lib' .OR. TMPNAM.EQ.'prt' .OR.
     +      TMPNAM.EQ.'bes' .OR. TMPNAM.EQ.'dic') THEN
C         look for files without path but with standard extension in the
C         standard place
          CALL UGTENV('CLIBD',LIBFIL)
C         add the standard directory qualifier
          IF (VAX) THEN
C           fixme: should we insist that VMS defines CLIBD as well as un*x?
            IF (LIBFIL.NE.' ') THEN
              TMPNAM = 'CLIBD:'
              TMPNAM(7:) = FILNAM
            ELSE
              TMPNAM = FILNAM
            ENDIF
          ELSE
            IF (LIBFIL.EQ.' ') CALL CCPERR(1,'CLIBD not defined')
            II = LENSTR(LIBFIL)
            TMPNAM = LIBFIL(:II)//'/'
            II = II + 2
            TMPNAM(II:) = FILNAM
          END IF
          FILNAM = TMPNAM
        ELSE IF (EXTN(JJ).EQ.'.scr' .OR. FEXTN(FILNAM).EQ.'scr') THEN
C         scratch files in a special place
C         actually create <ccp4_scr>/<prognm>_.<pid>
          CALL UGTENV('CCP4_SCR',TMPNAM)
          IF (VAX) THEN
            IF (TMPNAM.EQ.' ') THEN
              TMPNAM = PROGNM
            ELSE 
              TMPNAM = 'CCP4_SCR:' // PROGNM
            ENDIF
          ELSE
            IF (TMPNAM.EQ.' ') CALL CCPERR(1,'CCP4_SCR not defined')
            II = LENSTR(TMPNAM) + 1
            TMPNAM(II:) = '/'//PROGNM
          END IF
          II = LENSTR(TMPNAM) + 1
          TMPNAM(II:II) = '_'
          II = II + 1
          I = INDEX(FILNAM,'.')
          TMPNAM(II:) = FILNAM(:I)
          IF (VAX) THEN
            WRITE (SCRFIL,'(Z8.8)') GETPID()
          ELSE
            WRITE (SCRFIL,'(I5.5)') GETPID()
          ENDIF
          FILNAM = TMPNAM(1:LENSTR(TMPNAM))//SCRFIL
        END IF
      END IF
333   CONTINUE
C
C---- Now test input files do exist (but not for defaults, to avoid
C     checking 40 or 50 files listed in default.def which the setup
C     should guarantee)
C
      IF (ETYPE(JJ).EQ.'in' .AND. .NOT.LSKIP) THEN
        INQUIRE(FILE=FILNAM,EXIST=EXIST)
        IF (.NOT.EXIST) THEN
          ERRSTR = 'Cannot find file '
          ERRSTR(18:) = FILNAM
          CALL CCPERR(1,ERRSTR)
        END IF
      END IF
      II = LENSTR(LNAME) + 1
      LINE = LNAME
      LINE(II:II) = '='
      II = II + 1
      LINE(II:) = FILNAM
C     =======================================
      CALL USTENV(LINE(1:LENSTR(LINE)),ISTAT)
C     =======================================
      IF (ISTAT.NE.0) THEN
        ERRSTR = 'Cannot create environment variable '
        ERRSTR(36:) = LNAME
        CALL CCPERR(1,ERRSTR)
      END IF
      CALL QPRINT(3,LINE)
      END
C
C
C
      SUBROUTINE CCPPAG(IUN,NCOL,NLIN)
C     ===============================
C
C---- This subroutine returns the number of columns and lines 
C     for a printer output page on a given fortran unit number 
C     if the information is available
C
C PARAMETERS
C ==========
C
C         IUN (I)   FORTRAN UNIT NUMBER
C        NCOL (O)   NUMBER OF COLUMNS IN THE PAGE
C        NLIN (O)   NUMBER OF LINES IN THE PAGE
C
C Return 80,132 unless a terminal whence 0,80
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
C SUBROUTINE 'CCPSI2'
C ===================
C
C SET AN INTEGER VALUE FROM 0 TO 65535 INTO THE N'TH UNSIGNED INTEGER*2 ELEMENT
C OF AN INTEGER (OR OTHER) ARRAY.
C NOTE: NO OVERFLOW CHECKING IS DONE.
C
C (MUST BE IMPLEMENTED IF CCPBYT FUNCTION RETURNS .TRUE.)
C [for LAUE]
      SUBROUTINE CCPSI2(IVAL,IA,N)
C
C PARAMETERS
C
C    IVAL (I)   THE INTEGER VALUE FROM 0 TO 65535
C      IA (I/O) THE ARRAY INTO WHICH THE UNSIGNED INTEGER*2 VALUE IS TO BE 
C               INSERTED
C       N (I)   THE POSITION IN 'IA' WHERE THE UNSIGNED INTEGER*2 VALUE IS 
C               TO BE INSERTED
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
C
C
C SUBROUTINE 'CCPSTB'
C ===================
C
C SET AN INTEGER VALUE FROM 0 TO 255 INTO THE N'TH BYTE OF AN INTEGER
C (OR OTHER) ARRAY.
C NOTE: NO OVERFLOW CHECKING IS DONE.
C
C (MUST BE IMPLEMENTED IF CCPBYT FUNCTION RETURNS .TRUE.)
C [for LAUE]
      SUBROUTINE CCPSTB(IVAL,IA,N)
C
C PARAMETERS
C
C    IVAL (I)   THE INTEGER VALUE FROM 0 TO 255
C      IA (I/O) THE ARRAY INTO WHICH THE BYTE VALUE IS TO BE INSERTED
C       N (I)   THE POSITION IN 'IA' WHERE THE BYTE VALUE IS TO BE INSERTED
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
C
C
C
      SUBROUTINE CCPSPW(STRING)
C     =========================
C
C     Spawns a new process using shell command STRING
C
       CHARACTER STRING*(*)
       EXTERNAL SYSTEM
       CALL SYSTEM(STRING)
       END
C
C
C
      REAL FUNCTION CCPSUM(A,N,L)
C     ======================
C
C---- This function sums the elements of an array. (for the cray this
C     function will call the cray 'ssum' function)
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
C---- Return cpu time and elapsed time in seconds as
C     intervals from the initial call with iflag=0.  Note that there is
C     only one timer!
C
C PARAMETERS
C ==========
C
C       IFLAG (I/O) =0, initialise, =1, return times, =-1 dummy call
C                   returns -1 if time not available, in which case CPU
C                   and ELAPS are zero
C         CPU (O)   cpu time in seconds
C       ELAPS (O)   elapsed time in seconds
C
C     .. Scalar Arguments ..
      REAL CPU,ELAPS
      INTEGER IFLAG
C     ..
C     .. Local Scalars ..
      INTEGER STIME, TIM0
C     ..
C     .. External Subroutines ..
      EXTERNAL UCPUTM,USTIME
C     ..
C     .. Save statement ..
      SAVE TIM0, CPUX
C     ..
      IF (IFLAG.EQ.0) THEN
        ELAPS = 0.0
        CPU = 0.0
        CALL USTIME(TIM0)
        CALL UCPUTM(CPU)
      ELSE
        CALL USTIME(STIME)
        ELAPS = STIME - TIM0
        CPU = 1.0
        CALL UCPUTM(CPU)
      END IF
      END
C
      SUBROUTINE CCPTOI(ARRAY,N,II,ITYP,IFAIL)
C     ========================================
C
C---- This subroutine converts the n'th byte or integer*2 element in a
C     non-character array to an integer value. it is used by the
C     map file handling routines and must be implemented if map modes
C     0,1,3 or 5 are to be used.
C
C PARAMETERS
C ==========
C
C       ARRAY (I)   REAL ARRAY CONTAINING THE ELEMENTS TO BE CONVERTED
C           N (I)   THE NUMBER OF THE ELEMENT TO BE CONVERTED
C          II (O)   THE CALCULATED INTEGER VALUE (FOR BYTES THIS WILL
C                   BE IN THE RANGE 0-255)
C        ITYP (I)   THE CONVERSION TYPE =1, BYTE TO INTEGER
C                                       =2, INTEGER*2 TO INTEGER
C       IFAIL (I/O) ON INPUT   =0, STOP IF CONVERSION NOT AVAILABLE
C                              =1, RETURN FROM SUBROUTINE ALWAYS
C                   ON OUTPUT  UNCHANGED IF CONVERSION CARRIED OUT
C                              =-1 IF CONVERSION NOT AVAILABLE
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
      LOGICAL CALLED, LITEND
      INTEGER IND
      EXTERNAL LITEND
      SAVE CALLED, IND
      DATA CALLED/.FALSE./
C
      IF (.NOT.CALLED) THEN
        CALLED=.TRUE.
        IF (LITEND(1)) THEN
          IND = 1
        ELSE
          IND = 4
        ENDIF
      ENDIF
C
      GO TO (10,20) ITYP
C
C---- Byte to integer value
C
   10 NW = (N-1)/4 + 1
      NB = MOD(N-1,4) + 1
      IA = 0
      RR = ARRAY(NW)
      IBYT(IND) = JBYT(NB)
      II = IA
      RETURN
C
C---- Integer*2 to integer value
C
   20 NW = (N-1)/2 + 1
      NIH = MOD(N-1,2) + 1
      RR = ARRAY(NW)
      II = JHALF(NIH)
      END
C
C
C
      SUBROUTINE CCPUFL
C     =================
C
C---- This subroutine is called to suppress underflow error messages
C     if required and if the facility is available.
C
C PARAMETERS  NONE
C =========
C
C----  Not implemented, but Maybe correC
      END
C
C
C
      SUBROUTINE CCPUPC(STRING)
C     ========================
C
C---- Convert a text string to upper case in situ
C
C PARAMETERS
C ==========
C
C      STRING (I/O) CHARACTER STRING TO BE CONVERTED
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
C---- Print program name and date of current version (also prints run
C     date if available)
C
C PARAMETERS
C ==========
C
C         ILP (I)   UNIT NUMBER OF PRINTER OUTPUT
C        PROG (I)   CHARACTER VARIABLE HOLDING PROGRAM NAME (MAX
C                   OF 10 CHARACTERS)
C       VDATE (I)   CHARACTER VARIABLE HOLDING DATE OF THE CURRENT
C                   VERSION AS DD/MM/YY
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
C
C---- Output heading
C
      PR = PROG
      DT = VDATE
      CALL CCPDAT(DT2)
      CALL UGTUID(UID)
      CALL UTIME(CTIME)
      WRITE (ILP,FMT=6000) PR,DT,UID(1:LENSTR(UID)),DT2,CTIME
 6000 FORMAT ('1### CCP PROGRAM SUITE: ',A10,2X,'VERSION 2.2alpha: ',
     +       A8,'###',/' User: ',A,'  Run date: ',A8,'  Run time:',A,
     +       /)
C
      END
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
      BYTE ARR1(*)
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
      INTEGER NUM
      REAL ARR1(*)
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
C---- Returns the path (directory) of a file name or ' '
C
      CHARACTER FILNAM* (*)
      CHARACTER*1 NAME, TYPE, VERS
      EXTERNAL CCPPSF
C
      CALL CCPPSF(FILNAM, FDIR, NAME, TYPE, VERS)
      END
C
C
C     ===================================
      CHARACTER*(*) FUNCTION FEXTN(FILNAM)
C     ===================================
C
C---- Returns the extension of a file name or ' '
C
      CHARACTER FILNAM* (*)
      CHARACTER*1 PATH, NAME, VERS
      EXTERNAL CCPPSF
C
      CALL CCPPSF(FILNAM, PATH, NAME, FEXTN, VERS)
      END
C
C
C     ===================================
      CHARACTER*(*) FUNCTION FROOT(FILNAM)
C     ===================================
C
C---- Returns a file name minus an extension.
C
      CHARACTER FILNAM* (*)
      CHARACTER*1 PATH, TYPE, VERS
      EXTERNAL CCPPSF
C
      CALL CCPPSF(FILNAM, PATH, FROOT, TYPE, VERS)
      END
C
C
C
         LOGICAL FUNCTION LITEND(IDUM)
C        =======================
C
C---- Check endedness, Returns TRUE if little endian (VAX, FX2800,
C                                                   Ultrix, Convex)
C                              FALSE if big endian (IBM,IRIS,ESV)
C
         INTEGER I, IDUM
         BYTE B(4)
         EQUIVALENCE (I,B(1))
C
C---- Initialise B
C
          DO 10 JDO=1,4
            B(JDO) = 0
 10       CONTINUE
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
C
C
C======================================================================
C
      INTEGER FUNCTION LENSTR(STRING)
C     ===============================
C
C---- Returns significant string length excluding trailing spaces
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
      END
C
C
C FUNCTION 'LUNSTI'
C =================
C
C Returns the fortran standard input unit number
C
      FUNCTION LUNSTI(IDUM)
C
C PARAMETERS
C
C       IDUM (I)   Dummy parameter
C
      LUNSTI = 5
      END
C
C
C FUNCTION 'LUNSTO'
C =================
C
C Returns the fortran standard output unit number
C
      FUNCTION LUNSTO(IDUM)
C
C PARAMETERS
C
C       IDUM (I)   Dummy parameter
C
      LUNSTO = 6
      END
C
C
C FUNCTION 'NBITST'
C =================
C
C Return the (unsigned) integer value held within a bit field in a word
C [for LAUE]
      FUNCTION NBITST(IWORD,LSB,NBITS)
C
C PARAMETERS
C
C      IWORD (I)    The word containing the bits to be examined
C        LSB (I)    The least significant bit offset for the bit field
C      NBITS (I)    The number of bits in the bit field (Must be less
C                   than the word length)
C
C====== Get the bit value
C
      KMSK = 2**NBITS - 1
      NBITST = IAND(ISHFT(IWORD,-LSB),KMSK)
      END
C
C
C     =======================
      SUBROUTINE NOCRLF(LINE)
C     =======================
C
C---- Output a line supressing cr/lf. 
C
      EXTERNAL LUNSTO,TTSEND
      INTEGER LUNSTO
      CHARACTER*(*) LINE
      CALL TTSEND(LUNSTO(1),LINE,0)
      END
C
C
C======================================================================
C
C QPRINT - Set print flag or conditionally print MSG (less trailing
C     blanks)
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
      INTEGER IFLAG
      CHARACTER MSG* (*)
      EXTERNAL LUNSTO, LENSTR
      INTEGER PFLAG, LUNSTO, LENSTR
      SAVE PFLAG
      DATA PFLAG /-1/
C
      IF (PFLAG.EQ.-1) PFLAG = IFLAG
      IF (IFLAG.LE.PFLAG) WRITE (LUNSTO(1),'(1x, A)')
     +     MSG(1:LENSTR(MSG))
      END
C
C
C SUBROUTINE 'STBITS'
C ===================
C
C Set a bit field within a word to a given (unsigned) integer value
C [for LAUE]
C
      SUBROUTINE STBITS (IWORD,LSB,NBITS,IVAL)
C
C PARAMETERS
C
C      IWORD (I/O)  The word in which the bits are to be set
C        LSB (I)    The least significant bit offset for the bit field
C      NBITS (I)    The number of bits in the bit field (must be less than
C                   the word length)
C       IVAL (I)    The unsigned integer value to be set in the bit
C                   field (The user should ensure that this value will
C                   fit within the requested bit field)
C
C====== Set the bits
C
      KMSK = 2**NBITS - 1
      KVAL = IVAL
      KMSK = ISHFT(KMSK,LSB)
      KMSK = NOT(KMSK)
      KVAL = ISHFT(KVAL,LSB)
      IWORD = IOR(IAND(IWORD,KMSK),KVAL)
      END
