      PROGRAM MCTEST
C
C---- Test file for Machine dependent routines
C
C     .. Parameters ..
      INTEGER LBUF,LSTR
      PARAMETER (LBUF=100,LSTR=12)
C     ..
C     .. Local Scalars ..
      REAL ERSTR,RSEED,SEC
      INTEGER I,IBYTE,IDAY,IER,II,ILENGTH,ILOOP,IMON,ISEC,ISTAT,
     +        IYEAR,IYES,LDUM,LUN,LUNIN,LUNOUT,NREC
      CHARACTER ERRSTR*40,HANDLE* (LSTR),NAME1* (LSTR),NAME2* (LSTR),
     +          ENVNAM* (LBUF),USRNAM* (LSTR),UDATE* (LSTR),
     +          USRTIM* (LSTR),REPLY* (LSTR),TSTNAM*(LSTR)
C     ..
C     .. Local Arrays ..
      REAL BUFFER(LBUF)
C     ..
C     .. External Functions ..
      LOGICAL LITEND,VAXVMS
      EXTERNAL LITEND,VAXVMS
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPERR,CCPFYP,NOCRLF,QCLOSE,QMODE,QOPEN,QQINQ,QREAD,
     +         QSEEK,QWRITE,SRAND,UBYTES,UCPUTM,UGERR,UGTENV,UGTUID,
     +         UIDATE,UISATT,USTIME,UTIME,CCPRCS,CCPDPN
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT,COS
C     ..
C     .. Data statements ..
      DATA LUNIN/5/,LUNOUT/6/,NAME1/'AAA.TST'/,NAME2/'ZZZ.TST'/,
     +     RSEED/0.567/,ILOOP/13/,TSTNAM/'TESTNAME'/
C     ..
C
C---- Initialise CPU timer
C
      SEC = -1.0
      CALL UCPUTM(SEC)
C
C---- Parse command line arguments, open printer stream and print
C     version 
C
      CALL CCPFYP
      I = 0
      CALL CCPDPN(LUNOUT,'PRINTER','PRINTER','F',0,I)
      IF (I.NE.0) CALL CCPERR(1,'Can''t open printer stream')
      CALL CCPRCS(6,'TESLIB','$Date$')
C
C---- Other initialisations
C
      DO 10 I = 1,LBUF
        BUFFER(I) = 0.0
   10 CONTINUE
C
      I = 0
      CALL CCPDPN(10,NAME1,'NEW','F',0,I)
      IF (I.NE.0) CALL CCPERR(1,' Failed to open file '//NAME1)
      WRITE (10,FMT='(A)') ' I am the test file, delete me.'
      CLOSE (10,STATUS='DELETE')
C
C---- Start of Tests
C
      WRITE (LUNOUT,FMT=6000) ' Routine  Result      Comments'
C
C---- UGTENV
C
      CALL UGTENV('TESTENV',ENVNAM)
      WRITE (LUNOUT,FMT=6002) ' UGTENV',ENVNAM,'Get value of TESTENV'
C
C---- UGTUID
C
      CALL UGTUID(USRNAM)
      WRITE (LUNOUT,FMT=6002) ' UGTUID',USRNAM,'Get Users name'
C
C---- UIDATE
C
      CALL UIDATE(IMON,IDAY,IYEAR)
      WRITE (UDATE,FMT=6006) IDAY,IMON,IYEAR
      WRITE (LUNOUT,FMT=6002) ' UIDATE',UDATE,'Todays date'
C
C---- UTIME
C
      CALL UTIME(USRTIM)
      WRITE (LUNOUT,FMT=6002) ' UTIME ',USRTIM,'Current time'
C
C---- USTIME
C
      CALL USTIME(ISEC)
      WRITE (UDATE,FMT=6020) ISEC
      WRITE (LUNOUT,FMT=6002) ' USTIME',UDATE,'Absolute time (VMS=-1)'
C
C---- UISATT
C
      CALL UISATT(LUNIN,IYES)
      IF (IYES.EQ.0) THEN
        REPLY = 'No'
      ELSE
        WRITE (REPLY,FMT=6004) 'Yes, ',LUNIN
      END IF
      WRITE (LUNOUT,FMT=6002) ' UISATT',REPLY,
     +  'are we attached to at tty? Unit number?'
C
C---- VAXVMS
C
      REPLY = 'No'
      IF (VAXVMS()) REPLY = 'Yes'
      WRITE (LUNOUT,FMT=6002) ' VAXVMS',REPLY,'Is this VMS?'
C
C---- UBYTES
C
      CALL UBYTES(IBYTE,HANDLE)
      WRITE (REPLY,FMT=6008) HANDLE,IBYTE
      WRITE (LUNOUT,FMT=6002) ' UBYTES',REPLY,
     +  'Get BYTE/WORD Handling and number of bytes per word'
C
C---- LITEND
C
      REPLY = 'Big'
      IF (LITEND()) REPLY = 'Little'
      WRITE (LUNOUT,FMT=6002) ' LITEND',REPLY,'Big/Little end machine'
CCCC
CCCC---- URENAM
CCCC
CCC      CALL URENAM(NAME1,NAME2,ISTAT)
CCC      ERRSTR = 'OK'
CCC      IF (ISTAT.NE.0) CALL UGERR(ISTAT,ERSTR)
CCC      WRITE (LUNOUT,FMT=6002) ' URENAM',ERRSTR,'Check rename status'
CCC      CALL CUNLINK (NAME2)
C
C---- UGERR
C
      OPEN (21,FILE='TESTFILE',STATUS='OLD',IOSTAT=I)
      CALL UGERR(I,ERRSTR)
      WRITE (REPLY,FMT=6020) I
      WRITE (LUNOUT,FMT=6002) ' UGERR ',REPLY,ERRSTR
C
C---- UCPUTM
C
      SEC = 99.99
      CALL UCPUTM(SEC)
      WRITE (REPLY,FMT=6016) SEC
      WRITE (LUNOUT,FMT=6002) ' UCPUTM',REPLY,'Show elapsed CPU time'
C
C---- NOCRLF
C     
      CALL NOCRLF(' NOCRLF')
      WRITE(LUNOUT,'(15X,A)') 'Should be on same line'
C
C---- End of tests
C
      WRITE (LUNOUT,FMT=6000) ' Now test diskio routines'
C
C---- Now test the diskio stuff
C
      CALL QOPEN(LUN,'DISKIO','NEW')
C
C---- Write a file of size LBUF x LBUF x WORDSIZE
C
      DO 20 I = 1,LBUF
        BUFFER(1) = I
        CALL QWRITE(LUN,BUFFER,LBUF)
   20 CONTINUE
C
C---- Close the file
C
      CALL QCLOSE(LUN)
C
C---- reset the array buffer(*)
C
      DO 30 I = 1,LBUF
        BUFFER(I) = 0.0
   30 CONTINUE
C
C---- Now do some reads on the file just created
C
      CALL QOPEN(LUN,'DISKIO','READONLY')
      CALL QMODE(LUN,2,LDUM)
C
C---- test file size
C
      CALL QQINQ(LUN,'DISKIO',REPLY,ILENGTH)
      ISTAT = LDUM*LBUF*LBUF
      IF (ISTAT.NE.ILENGTH) THEN
        WRITE (ERRSTR,6018) 'DISKIO should be ',ISTAT,' bytes'
        CALL CCPERR(2,ERRTSR)
      ENDIF
C
C---- Seed random Number Generator
C
      CALL UGTENV('SEED',REPLY)
      IF (REPLY.NE.' ') READ (REPLY,FMT=6010) RSEED
C
C---- Get number of reads to perform
C
      CALL UGTENV('READS',REPLY)
      IF (REPLY.NE.' ') READ (REPLY,FMT=6020) ILOOP
C
C---- Do random read from file
C
      CALL UGTENV('PROMPT',ENVNAM)
      IF (ENVNAM.EQ.' ') THEN
        DO 40 II = 1,ILOOP
          IF (RSEED .GT. 0.9) RSEED = RSEED - 0.9
          RSEED = COS(RSEED)**2
          NREC = NINT(100*RSEED + 1.0)
          CALL QSEEK(LUN,NREC,1,LBUF)
          CALL QREAD(LUN,BUFFER,LBUF,IER)
          WRITE (LUNOUT,FMT=6014) NREC,BUFFER(1),IER
   40   CONTINUE
      ELSE
   50   CONTINUE
          CALL NOCRLF('Record to seek (-99 to stop) > ')
          READ (LUNIN,6020) NREC
          IF (NREC.EQ.-99) GOTO 60
          CALL QSEEK(LUN,NREC,1,LBUF)
          CALL QREAD(LUN,BUFFER,LBUF,IER)
          WRITE (LUNOUT,FMT=6014) NREC,BUFFER(1),IER
        GOTO 50
      ENDIF

   60 CALL QCLOSE (LUN)
C      CALL CUNLINK ('DISKIO')
C     Now check we can open and close a scratch file
      CALL QOPEN (LUN, 'foo.bar', 'SCRATCH')
      CALL QCLOSE (LUN)
      CALL CCPERR(0,'Normal Termination')
C
C---- Format Statements
C
 6000 FORMAT (//A,/)
 6002 FORMAT (A7,3X,A12,A)
 6004 FORMAT (A,I3)
 6006 FORMAT (I2.2,2 ('/',I2.2))
 6008 FORMAT (A5,I3)
 6010 FORMAT (F8.4)
 6014 FORMAT (' Seek Record:',I5,' Read: ',F8.2,' Status: ',I4)
 6016 FORMAT (F8.2)
 6018 FORMAT (A,I8,A)
 6020 FORMAT (I10)
      END
