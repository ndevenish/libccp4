C
C A set of Fortran subroutines to perform random access I/O on various
C data items (including bytes). Uses the C functions fopen, fclose,
C fread, fwrite, fseek, ftell, etc - by calling routines in library.c
C Note: IUNIT is NOT A Fortran Unit number, but an internal identifier
C
C  The calls provided are given below:
C
C  CALL QQOPEN  (IUNIT,FILNAM,ISTAT)         - Open file
C  CALL QCLOSE  (IUNIT)                      - Close file
C  CALL QMODE   (IUNIT,MODE,NMCITM)          - Change mode
C  CALL QREAD   (IUNIT,ARRAY,NITEMS,IER)     - Read nitems
C  CALL QWRITE  (IUNIT,ARRAY,NITEMS)         - Write nitems
C  CALL QSEEK   (IUNIT,IREC,IEL,LRECL)       - Move to irec,iel
C  CALL QBACK   (IUNIT,LRECL)                - Backspace 1 record
C  CALL QSKIP   (IUNIT,LRECL)                - Skip 1 record
C  CALL QQINQ   (IUNIT,LFILNM,FILNAM,LENGTH) - Get filename and length
C  CALL QLOCATE (IUNIT,LOCATE)               - Get position in file
C  CALL QOPEN   (IUNIT,FILNAM,ATBUTE)        - qqopen, for compatability
C
C  QSEEK calculates the location as (IREC - 1)CLRECL + IEL. Note: as in
C        Fortran, addressing begins at 1 for both record & el
C
C  Where:
C
C  IUNIT  = Variable returned by (Q)QOPEN to identify a file stream
C
C  FILNAM = file name for the stream (should be restricted to eight
C           characters for CCP4 programs)
C
C  ISTAT  = File status on opening the file:
C           1, 'UNKNOWN'   open as 'OLD'/'NEW' check existence
C           2, 'SCRATCH'   open as 'OLD' and delete on closing
C           3, 'OLD'       file MUST exist or program halts
C           4, 'NEW'       create (overwrite) new file
C           5, 'READONLY'  self explanitory
C
C  NOTE: When using QQOPEN or QOPEN with ISTAT = 4 a check is made on
C        the environment variable CCP4_OPEN - if this is set to UNKNOWN
C        then the file is opened with attribute UNKNOWN rather than NEW
C        to prevent overwriting files that already exist.
C
C  MODE   = Access mode = 0, BYTES
C                       = 1, SHORT INT
C                       = 2, WORD
C                       = 3, SHORT COMPLEX
C                       = 4, COMPLEX
C                       = 5, INTEGER*4
C
C  NMCITM = No. of machine items (eg bytes) per element
C  ARRAY  = Starting location for data storage in core
C  NITEMS = Number of elements to transfer
C  IER    = Error flag (0 = no error) else number of words transferred
C  IREC   = Desired record number (starts at 1)
C  IEL    = Desired element number within record (word) (starts at 1)
C  LRECL  = Record length in elements
C
C  No. of channels and buffer length in words set in #DEFINE statements
C
C
C======================================================================
C
C QQOPEN - Open a file unit
C
C Usage:  CALL QQOPEN  (IUNIT, LOGNAME, ISTAT)
C         INTEGER       IUNIT, ISTAT
C         CHARACTER*(*) LOGNAME
C
C Input:  LOGNAME       Logical name of file to open
C         ISTAT         File status: 1 (UNKNOWN), 2 (SCRATCH), 3 (OLD),
C                                    4 (NEW) or 5 (READONLY)
C
C Output: IUNIT         unit number number assigned to file. If negative
C                       the following error conditions occurred:
C                       -1 No more streams left
C                       -2 Could not open the file
C
C======================================================================
C
      SUBROUTINE QQOPEN(IUNIT,LOGNAM,ISTAT)
C     =====================================
C
C     .. Parameters ..
      INTEGER ISTRLN,ISIZE
      PARAMETER (ISTRLN=500,ISIZE=20)
C     ..
C     .. Scalar Arguments ..
      INTEGER ISTAT,IUNIT
      CHARACTER LOGNAM* (*)
C     ..
C     .. Local Arrays ..
      CHARACTER MODES(5)*10
C     ..
C     .. Local Scalars ..
      INTEGER JSTAT
      CHARACTER ERRSTR*255,REWRIT* (ISIZE),USRNAM* (ISIZE),
     +          FNAME* (ISTRLN),LNAME* (ISTRLN)
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPERR,CCPUPC,COPEN,QPRINT,UGTENV,UGTUID
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. Data statements ..
      DATA MODES/'UNKNOWN','SCRATCH','OLD','NEW','READONLY'/
C     ..
C
      IF (ISTAT.LT.1 .OR. ISTAT.GT.5) THEN
        WRITE (ERRSTR,FMT=6000) ' (Q)QOPEN has no file status ',ISTAT
        CALL CCPERR(1,ERRSTR)
      END IF
C
C---- Test CCP4_OPEN for 'U' or 'u' (UNKNOWN) to switch mode 4 to 1
C
      JSTAT = ISTAT
      REWRIT = ' '
      IF (JSTAT.EQ.4) THEN
        CALL UGTENV('CCP4_OPEN',REWRIT)
        CALL CCPUPC(REWRIT)
        IF (REWRIT(1:1).EQ.'U') JSTAT = 1
      END IF
C
C---- Check Logical Names
C
      FNAME = ' '
      LNAME = LOGNAM
      IF (LNAME.EQ.' ') LNAME = 'diskio.dft'
      CALL UGTENV(LNAME,FNAME)
      IF (FNAME(1:MAX(LENSTR(FNAME),1)).EQ.'/dev/null') THEN
        JSTAT = 1
      ELSE IF (FNAME.EQ.' ') THEN
        FNAME = LNAME
      END IF
      IF (REWRIT(1:1).EQ.'U') CALL QPRINT(2,
     +                            '(Q)QOPEN status changed from NEW to '
     +                             //'UNKNOWN for '//
     +                             LNAME(1:LENSTR(LNAME)))
C
C---- Open the file as requested
C
      CALL COPEN(IUNIT,FNAME,JSTAT)
      CALL UGTUID(USRNAM)
      WRITE (ERRSTR,FMT=6000) '(Q)QOPEN allocated stream ',IUNIT
      CALL QPRINT(1,ERRSTR)
      ERRSTR = 'User:   '//USRNAM//' Logical Name: '//
     +         LNAME(1:LENSTR(LNAME))
      CALL QPRINT(1,ERRSTR)
      ERRSTR = 'Status: '//MODES(JSTAT)//' Filename: '//
     +         FNAME(1:LENSTR(FNAME))
      CALL QPRINT(1,ERRSTR)
C
C---- Error conditions
C
      IF (IUNIT.EQ.-1) THEN
        CALL QPRINT(1,' (Q)QOPEN failed - no streams left')
        CALL CCPERR(1,' Stop on QOPEN failure')
        STOP
      ELSE IF (IUNIT.EQ.-2) THEN
        CALL QPRINT(1,' (Q)QOPEN failed - cannot open file')
        WRITE (ERRSTR,FMT=6001) '(Q)QOPEN failed - File name:',LOGNAM
        CALL CCPERR(1,' (Q)QOPEN failed - cannot open file')
        STOP
      END IF
C
 6000 FORMAT (A,I2)
 6001 FORMAT (A,5X,A)
      END
C
C
C======================================================================
C
C QCLOSE - Close file unit
C
C Usage:  CALL QCLOSE (IUNIT)
C         INTEGER      IUNIT
C
C Input:  IUNIT        unit number assigned to file
C
C Output: None.
C
C======================================================================
C
      SUBROUTINE QCLOSE(IUNIT)
C     ========================
C
C     .. Scalar Arguments ..
      INTEGER IUNIT
C     ..
C     .. External Subroutines ..
      EXTERNAL CCLOSE
C     ..
      CALL CCLOSE(IUNIT)
C
      END
C
C
C======================================================================
C
C QMODE - Set mode for file access
C
C Usage:  CALL QMODE (IUNIT, MODE, NMCITM)
C         INTEGER     IUNIT, MODE, NMCITM
C
C Input:  IUNIT       unit number to assign to file
C         MODE        mode to switch into: 0 (BYTES), 1 (SMALL INTEGER),
C                                          2 (WORDS), 3 (SHORT COMPLEX),
C                                          4 (COMPLEX) 6 (INTEGER)
C
C Output: NMCITM      number of bytes per item on this machine.
C
C======================================================================
C
      SUBROUTINE QMODE(IUNIT,MODE,NMCITM)
C     ==================================
C
C     .. Scalar Arguments ..
      INTEGER IUNIT,MODE,NMCITM
C     ..
C     .. Local Scalars ..
      CHARACTER ERRSTR*255
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPERR,CMODE
C     ..
      IF (MODE.LT.0 .OR. MODE.GT.6) THEN
        WRITE (ERRSTR,FMT=6000) ' QMODE has no such mode ',MODE
        CALL CCPERR(1,ERRSTR)
      ELSE
        CALL CMODE(IUNIT,MODE,NMCITM)
      END IF
C
 6000 FORMAT (A,I2)
      END
C
C
C======================================================================
C
C QREAD - Read from IUNIT into BUFFER, NITEMS items
C
C Usage:  CALL QREAD (IUNIT,BUFFER,NITEMS,RESULT)
C         INTEGER     IUNIT, NITEMS, RESULT
C         REAL        BUFFER
C
C Input:  IUNIT       unit number assigned to file
C         NITEMS      number of items (item size set by QMODE)
C
C Output: RESULT      0 (no error), -1 (EOF) or number of items read
C         BUFFER      holds the items read
C
C======================================================================
C
      SUBROUTINE QREAD(IUNIT,ARRAY,NITEMS,IER)
C     ========================================
C
C     .. Scalar Arguments ..
      INTEGER IER,IUNIT,NITEMS
C     ..
C     .. Array Arguments ..
      REAL ARRAY(*)
C     ..
C     .. External Subroutines ..
      EXTERNAL CREAD
C     ..
      CALL CREAD(IUNIT,ARRAY,NITEMS,IER)
C
      END
C
C
C======================================================================
C
C QWRITE - Write to IUNIT from BUFFER, NITEMS items
C
C Usage:  CALL QWRITE (IUNIT,BUFFER,NITEMS)
C         INTEGER      IUNIT, NITEMS
C         REAL         BUFFER
C
C Input:  IUNIT        unit number assigned to file
C         NITEMS       number of items (item size set by QMODE)
C         BUFFER       holds the items to write
C
C Output: None.
C
C======================================================================
C
      SUBROUTINE QWRITE(IUNIT,ARRAY,NITEMS)
C     =====================================
C
C     .. Scalar Arguments ..
      INTEGER IUNIT,NITEMS
C     ..
C     .. Array Arguments ..
      REAL ARRAY(*)
C     ..
C     .. Local Scalars ..
      INTEGER IER
      CHARACTER ERRSTR*255
C     ..
C     .. External Subroutines ..
      EXTERNAL CWRITE,QPRINT
C     ..
      CALL CWRITE(IUNIT,ARRAY,NITEMS,IER)
      IF (IER.NE.0) THEN
        WRITE (ERRSTR,FMT=6000) 'QWRITE only wrote out ',IER,
     +    ' items instead of ',NITEMS
        CALL QPRINT(2,ERRSTR)
        CALL CCPERR(1,' Stop in Qwrite failure ')
        STOP
      END IF
C
 6000 FORMAT (A,I4,A,I4)
      END
C
C
C======================================================================
C
C QSEEK - Position a file pointer in a IUNIT
C
C Usage:  CALL QSEEK (IUNIT, IRECL, IEL, LRECL)
C         INTEGER     IUNIT, IRECL, IEL, LRECL
C
C Input:  IUNIT       unit number to assign to file
C         IRECL       record number to seek
C         IEL         element number to seek
C         LRECL       length of a record
C
C Output: None
C
C======================================================================
C
      SUBROUTINE QSEEK(IUNIT,IREC,IEL,LRECL)
C     ======================================
C
C
C     .. Scalar Arguments ..
      INTEGER IEL,IREC,IUNIT,LRECL
C     ..
C     .. External Subroutines ..
      EXTERNAL CSEEK
C     ..
      CALL CSEEK(IUNIT,IREC,IEL,LRECL)
C
      END
C
C
C======================================================================
C
C QBACK - skip back 1 record of length LRECL
C
C Usage:  CALL QBACK (IUNIT,LRECL)
C         INTEGER     IUNIT, LRECL
C
C Input:  IUNIT       unit number assigned to file
C         LRECL       length of a record in items
C
C Output: None
C
C======================================================================
C
      SUBROUTINE QBACK(IUNIT,LRECL)
C     =============================
C
C
C     .. Scalar Arguments ..
      INTEGER IUNIT,LRECL
C     ..
C     .. External Subroutines ..
      EXTERNAL CBACK
C     ..
      CALL CBACK(IUNIT,LRECL)
C
      END
C
C
C======================================================================
C
C QSKIP - skip forward 1 record of length LRECL
C
C Usage:  CALL QSKIP (IUNIT,LRECL)
C         INTEGER     IUNIT, LRECL
C
C Input:  IUNIT       unit number assigned to file
C         LRECL       length of a record in items
C
C Output: None
C
C======================================================================
C
      SUBROUTINE QSKIP(IUNIT,LRECL)
C     =============================
C
C
C     .. Scalar Arguments ..
      INTEGER IUNIT,LRECL
C     ..
C     .. External Subroutines ..
      EXTERNAL CSKIP
C     ..
      CALL CSKIP(IUNIT,LRECL)
C
      END
C
C
C======================================================================
C
C QQINQ - check file name and size. Check IUNIT first, if no success
C         then try LOGNAM, if this fails use LOGNAM as filename.
C
C Usage:  CALL QQINQ   (IUNIT,LOGNAM,FILNAM,LENGTH)
C         INTEGER       IUNIT,LENGTH
C         CHARACTER*(*) LOGNAM,FILNAM
C
C Input:  IUNIT         stream to check
C         LOGNAM        Logical name
C
C Output: FILNAM        the full file name or "" if no file
C         LENGTH        file size or -1 if no file
C
C======================================================================
C
      SUBROUTINE QQINQ(IUNIT,LFN,FILNAM,LENGTH)
C     =========================================
C
C     .. Parameters ..
      INTEGER ISTRLN
      PARAMETER (ISTRLN=500)
C     ..
C     .. Scalar Arguments ..
      INTEGER IUNIT,LENGTH
      CHARACTER FILNAM* (*),LFN* (*)
C     ..
C     .. Local Scalars ..
      CHARACTER FNAME* (ISTRLN),LNAME* (ISTRLN)
C     ..
C     .. External Subroutines ..
      EXTERNAL CQINQ,UGTENV
C     ..
      FNAME = ' '
      LNAME = LFN
      IF (LNAME.EQ.' ') LNAME = 'DISKIO.DFT'
      CALL UGTENV(LNAME,FNAME)
      IF (FNAME.EQ.' ') FNAME = LNAME
      CALL CQINQ(IUNIT,FNAME,LENGTH)
      FILNAM = FNAME
C
      END
C
C
C======================================================================
C
C QLOCATE - return current position in file (measured in items)
C
C Usage:  CALL QLOCATE (IUNIT,LOCATE)
C         INTEGER       IUNIT,LOCATE
C
C Input:  IUNIT         stream to check
C
C Output: LOCATE        Current position in file or -1 for no file
C
C======================================================================
C
      SUBROUTINE QLOCATE(IUNIT,LOCATE)
C     ================================
C
C
C     .. Scalar Arguments ..
      INTEGER IUNIT,LOCATE
C     ..
C     .. External Subroutines ..
      EXTERNAL CLOCATE
C     ..
      CALL CLOCATE(IUNIT,LOCATE)
C
      END
C
C
C======================================================================
C
C QOPEN - Open a file unit
C
C Usage:  CALL QOPEN   (IUNIT, LOGNAME, ATBUTE)
C         INTEGER       IUNIT
C         CHARACTER*(*) LOGNAME, ATBUTE
C
C Input:  IUNIT         unit number number to assign to file
C         LOGNAME       Logical name of file to open
C         ATBUTE        File status = 'UNKNOWN', 'SCRATCH', 'OLD',
C                                     'NEW', or 'READONLY'
C
C Output: None.
C
C======================================================================
C
      SUBROUTINE QOPEN(IUNIT,LOGNAM,ATBUTA)
C     =====================================
C
C     .. Scalar Arguments ..
      INTEGER IUNIT
      CHARACTER ATBUTA* (*),LOGNAM* (*)
C     ..
C     .. Local Scalars ..
      INTEGER ISTAT
C     ..
C     .. External Subroutines ..
      EXTERNAL QQOPEN
C     ..
      ISTAT = 0
      IF (ATBUTA(1:1).EQ.'U' .OR. ATBUTA(1:1).EQ.'u') ISTAT = 1
      IF (ATBUTA(1:1).EQ.'S' .OR. ATBUTA(1:1).EQ.'s') ISTAT = 2
      IF (ATBUTA(1:1).EQ.'O' .OR. ATBUTA(1:1).EQ.'o') ISTAT = 3
      IF (ATBUTA(1:1).EQ.'N' .OR. ATBUTA(1:1).EQ.'n') ISTAT = 4
      IF (ATBUTA(1:1).EQ.'R' .OR. ATBUTA(1:1).EQ.'r') ISTAT = 5
C
      CALL QQOPEN(IUNIT,LOGNAM,ISTAT)
C
      END
