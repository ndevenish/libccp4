C
C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
C     $Id$
C     
C_BEGIN_CCPLIB
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
C  CALL QRARCH (IUNIT, IOFFSET)              - set up number conversion
C  CALL QWARCH (IUNIT, IOFFSET)              - write conversion info
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
C           5, 'READONLY'  self explanAtory
C
C  NOTE: When using QQOPEN or QOPEN with ISTAT = 4 a check is made on
C        the environment variable CCP4_OPEN - if this is set to UNKNOWN
C        then the file is opened with attribute UNKNOWN rather than NEW
C        to prevent overwriting files that already exist.
C
C  MODE   = Access mode = 0, BYTES
C                       = 1, SHORT INT
C                       = 2, (REAL) WORD
C                       = 3, SHORT COMPLEX
C                       = 4, COMPLEX
C                       = 6, INTEGER
C
C  NMCITM = No. of machine items (eg bytes) per element
C  ARRAY  = Starting location for data storage in core
C     NOTE: This should normally be an array of full-word fortran items
C     (REAL or INTEGER) or double-word (COMPLEX) in the case that you
C     want to transfer complex numbers (mode 4).  If necessary, unpack
C     bytes using the routines provided in the library (or new ones).
C     In particular, DON'T try to use BYTE or INTEGER*2 arrays, as these
C     will likely cause alignment errors on RISC architectures.
C  NITEMS = Number of elements to transfer
C  IER    = Error flag (0 = no error) else number of words transferred
C  IREC   = Desired record number (starts at 1)
C  IEL    = Desired element number within record (word) (starts at 1)
C  LRECL  = Record length in elements
C
C  No. of channels and buffer length in words set in #DEFINE statements
C
C
C     Author: David Agard (Phil Evans and John Campbell)
C     Modified: For Unix/F77 using words (and bytes if available) (John Campbell)
C     Modified: For ccp ascii header system implemented (Jan Zelinka)
C_END_CCPLIB
C
C======================================================================
C_BEGIN_QQOPEN
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
C Output: IUNIT         Integer handle assigned to file. If negative
C                       the following error conditions occurred:
C                       -1 No more streams left
C                       -2 Could not open the file
C
C_END_QQOPEN
C======================================================================
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
     +     FNAME* (ISTRLN),LNAME* (ISTRLN)
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPERR,CCPUPC,COPEN,QPRINT,UGTENV,UGTUID
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL CCPEXS, LENSTR, VAXVMS
      LOGICAL CCPEXS, VAXVMS
C     ..
C     .. Data statements ..
      DATA MODES/'UNKNOWN','SCRATCH','OLD','NEW','READONLY'/
C     ..
C
      IF (ISTAT.LT.1 .OR. ISTAT.GT.5) THEN
        WRITE (ERRSTR,'(1X,A,I2)') ' (Q)QOPEN: bad mode: ',ISTAT
        CALL CCPERR(1,ERRSTR)
      END IF
C
C---- Test CCP4_OPEN for 'UNKNOWN' to switch mode 4 to 1
C
      JSTAT = ISTAT
      REWRIT = ' '
      IF (JSTAT.EQ.4) THEN
        CALL UGTENV('CCP4_OPEN',REWRIT)
        CALL CCPUPC(REWRIT)
        IF (REWRIT.EQ.'UNKNOWN') JSTAT = 1
      END IF
C
C---- Check Logical Names
C
      FNAME = ' '
      LNAME = LOGNAM
      IF (LNAME.EQ.' ') LNAME = 'diskio.dft'
      CALL UGTENV(LNAME,FNAME)
      IF (FNAME.EQ.'/dev/null') THEN
        JSTAT = 1
      ELSE IF (FNAME.EQ.' ') THEN
        FNAME = LNAME
      END IF
      IF (REWRIT.EQ.'UNKNOWN') 
     +     CALL QPRINT(2, '(Q)QOPEN status changed from NEW to '
     +     //'UNKNOWN for '// LNAME)
C
C---- Open the file as requested
C
      CALL COPEN(IUNIT,FNAME,JSTAT)
C
C---- Error conditions
C
      IF (IUNIT.EQ.-1) THEN
        CALL CCPERR(1,' (Q)QOPEN failed - no streams left')
      ELSE IF (IUNIT.EQ.-2) THEN
        WRITE (ERRSTR,FMT=6001) '(Q)QOPEN failed - File name:',
     +       LOGNAM (:MIN(100,LENSTR(LOGNAM)))
        CALL CCPERR(1,ERRSTR)
      ELSE IF (JSTAT.EQ.4 .AND. CCPEXS(FNAME)) THEN
        WRITE (ERRSTR,FMT=6001)
     +       '(Q)QOPEN NEW file already exists:',
     +       FNAME(:MIN(100,LENSTR(LOGNAM)))
        CALL CCPERR(1,ERRSTR)
      END IF

      CALL UGTUID(USRNAM)
      WRITE (ERRSTR,'(1X,A,I2)') '(Q)QOPEN allocated # ',IUNIT
      CALL QPRINT(1,ERRSTR)
      ERRSTR = 'User:   '//USRNAM//' Logical Name: '//LNAME
      CALL QPRINT(1,ERRSTR)
      ERRSTR = 'Status: '//MODES(JSTAT)//' Filename: '//FNAME
      CALL QPRINT(1,ERRSTR)
 6001 FORMAT (1X,A,1X,A)
      END
C
C
C======================================================================
C_BEGIN_QCLOSE
C
C QCLOSE - Close file unit
C
C Usage:  CALL QCLOSE (IUNIT)
C         INTEGER      IUNIT
C
C Input:  IUNIT        unit number assigned to file
C
C Output: None.
C_END_QCLOSE
C See library.c
C======================================================================
C_BEGIN_QMODE
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
C_END_QMODE
C See library.c
C======================================================================
C_BEGIN_QREAD
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
C_END_QREAD
C See library.c
C======================================================================
C_BEGIN_QWRITE
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
C_END_QWRITE
C See library.c
C======================================================================
C_BEGIN_QSEEK
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
C_END_QSEEK
C See library.c
C======================================================================
C_BEGIN_QBACK
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
C_END_QBACK
C See library.c
C======================================================================
C_BEGIN_QSKIP
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
C_END_QSKIP
C See library.c
C
C
C======================================================================
C_BEGIN_QQINQ
C
C QQINQ - check file name and size. Check IUNIT first, if no success
C         then try LOGNAM, if this fails use LOGNAM as filename.
C
C Usage:  CALL QQINQ   (IUNIT,LOGNAM,FILNAM,LENGTH)
C         INTEGER       IUNIT,LENGTH
C         CHARACTER*(*) LOGNAM,FILNAM
C
C Input:  IUNIT         handle to check (as returned by QOPEN)
C         LOGNAM        Logical name
C
C Output: FILNAM        the full file name or "" if no file
C         LENGTH        file size or -1 if no file
C
C_END_QQINQ
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
      IF (LNAME.EQ.' ') LNAME = 'diskio.dft'
      CALL UGTENV(LNAME,FNAME)
      IF (FNAME.EQ.' ') FNAME = LNAME
      CALL CQINQ(IUNIT,FNAME,LENGTH)
      FILNAM = FNAME
C
      END
C
C
C======================================================================
C_BEGIN_QLOCATE
C
C QLOCATE - return current position in file (measured in items)
C
C Usage:  CALL QLOCATE (IUNIT,LOCATE)
C         INTEGER       IUNIT,LOCATE
C
C Input:  IUNIT         stream to check
C
C Output: LOCATE        Current position in file or -1 for no file
C_END_QLOCATE
C See library.c
C
C======================================================================
C_BEGIN_QOPEN
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
C_END_QOPEN
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
      CHARACTER FOO*80
C     ..
C     .. External Subroutines ..
      EXTERNAL QQOPEN, CCPUPC
C     ..
      ISTAT = 0
      CALL CCPUPC(ATBUTA)
      IF (ATBUTA(:1).EQ.'U') ISTAT = 1
      IF (ATBUTA(:1).EQ.'S') ISTAT = 2
      IF (ATBUTA(:1).EQ.'O') ISTAT = 3
      IF (ATBUTA(:1).EQ.'N') ISTAT = 4
      IF (ATBUTA(:1).EQ.'R') ISTAT = 5
      IF (ISTAT.EQ.0) THEN
        FOO = ATBUTA
        CALL CCPERR(1,'Bad attribute in QOPEN: '//FOO)
      ENDIF
C
      CALL QQOPEN(IUNIT,LOGNAM,ISTAT)
      END
C======================================================================
C_BEGIN_QRARCH
C
C QRARCH - set up number conversion
C
C Usage:  CALL QRARCH   (IUNIT, IOFFSET, IRESLT)
C         INTEGER       IUNIT, IOFFSET, IRESLT
C
C Input:  IUNIT         unit number number to assign to file
C         IOFFSET       offset in words at which to find architecture
C                       information
C
C Output: IRESLT        fileFT + (16*fileIT) (see library C code)
C                       Zero if the stamp isn't present.
C
C     Reads the `machine stamp' giving information about the
C     architecture with which the file was written and arranges to
C     translate a foreign format to native with QREAD, dependent on the
C     current diskio mode.
C
C_END_QRARCH
C======================================================================
C
C======================================================================
C_BEGIN_QWARCH
C
C QWARCH - set up number conversion
C
C Usage:  CALL QWARCH   (IUNIT, IOFFSET)
C         INTEGER       IUNIT, IOFFSET
C
C Input:  IUNIT         unit number number to assign to file
C         IOFFSET       offset in words at which to write architecture
C                       information
C
C Output: None.
C
C     Writes the `machine stamp' giving information about the
C     architecture with which the file was written and which is used by
C     QRARCH
C
C_END_QWARCH
C======================================================================

