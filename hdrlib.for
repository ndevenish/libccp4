C
C   ccphdrs.f  Jan Zelinka --- University of York 19-04-1989
C
C
C---- Special CCP4 LCF and MAP file version.
C     External calls to CCPDPN, LENSTR & routines from DISKIO module
C
C PARAMETERs description:
C ======================
C
C SCRLUN - lun of scratch file
C          (!!!! MAY BE DANGEROUS TO YOUR FILES !!!!)
C MNHDRS - maximum number of headers
C HDNAML - maximum header name length in characters
C HDLINL - header line length (hdnaml+ X,I6,'x',I6)
C          if You want to change it, correct formats in code
C LSIZ   - line size of header
C BHDR   - line flag of begin header line
C EHDR   -              end header
C EHDRS  -              end headers
C BLIN   -              blank (empty)
C OCCUP  -              text (occupated)
C
C VARIABLEs description:
C ======================
C
C SYSINI - flag set if header system was initialized
C FFLIN  - 1st free line in scratch file  (da record number)
C LREAD  - last read line in given header (da record number)
C LINNUM - total num. of lines in given header
C LHEAD  - headerheader line (da record number)
C
C
C---- Return valuse from header management functions
C     ERR   - error
C     OK    - o.k.
C     ENDHD - end of header found
C
C
C
      INTEGER FUNCTION ZHDREX(NAME)
C     =============================
C
C
C---- Confirm by OK that header of given name exist
C
C     Input - name CHAR name of header
C
C
C
C---- For all entries in header table
C
C     .. Parameters ..
      INTEGER MNHDRS,HDNAML
      PARAMETER (MNHDRS=1000,HDNAML=30)
      INTEGER ERR,OK
      PARAMETER (ERR=-1,OK=0)
C     ..
C     .. Scalar Arguments ..
      CHARACTER NAME* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER FFLIN
      LOGICAL SYSINI
C     ..
C     .. Arrays in Common ..
      INTEGER LHEAD,LINNUM,LREAD
      CHARACTER HNAM* (HDNAML)
C     ..
C     .. Local Scalars ..
      INTEGER I
      CHARACTER HDNAM* (HDNAML)
C     ..
C     .. External Functions ..
      INTEGER ZHDRHN
      EXTERNAL ZHDRHN
C     ..
C     .. Common blocks ..
      COMMON /ZHDRDC/HNAM(MNHDRS)
      COMMON /ZHDRDT/SYSINI,FFLIN,LREAD(MNHDRS),LINNUM(MNHDRS),
     +       LHEAD(MNHDRS)
C     ..
C     .. Save statement ..
      SAVE /ZHDRDT/,/ZHDRDC/
C     ..
C
C
      I = 0
   10 CONTINUE
      I = I + 1
C
C         ***************
      IF (ZHDRHN(I,HDNAM).NE.OK) THEN
C         ***************
C
        GO TO 20
      ELSE IF (NAME.NE.HDNAM) THEN
        GO TO 10
      END IF
      ZHDREX = OK
      RETURN
   20 ZHDREX = ERR
C
C
      END
C
C
C
      INTEGER FUNCTION ZHDRRE(OLDNAM,NEWNAM)
C     ======================================
C
C
C---- Rename header - change name from oldnam to newnam
C     Input - oldnam CHAR old name of header
C             newnam CHAR new name of header
C
C
C---- For all entries in header table
C
C     .. Parameters ..
      INTEGER SCRLUN,MNHDRS,HDNAML,LSIZ
      PARAMETER (SCRLUN=41,MNHDRS=1000,HDNAML=30,LSIZ=80)
      CHARACTER*1 BHDR
      PARAMETER (BHDR='S')
      INTEGER ERR,OK
      PARAMETER (ERR=-1,OK=0)
C     ..
C     .. Scalar Arguments ..
      CHARACTER NEWNAM* (*),OLDNAM* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER FFLIN
      LOGICAL SYSINI
C     ..
C     .. Arrays in Common ..
      INTEGER LHEAD,LINNUM,LREAD
      CHARACTER HNAM* (HDNAML)
C     ..
C     .. Local Scalars ..
      INTEGER I
      CHARACTER HDNAM* (HDNAML),SCRBUF* (LSIZ+1)
C     ..
C     .. External Functions ..
      INTEGER ZHDRHN
      EXTERNAL ZHDRHN
C     ..
C     .. Common blocks ..
      COMMON /ZHDRDC/HNAM(MNHDRS)
      COMMON /ZHDRDT/SYSINI,FFLIN,LREAD(MNHDRS),LINNUM(MNHDRS),
     +       LHEAD(MNHDRS)
C     ..
C     .. Save statement ..
      SAVE /ZHDRDT/,/ZHDRDC/
C     ..
C
C
      I = 0
   10 CONTINUE
      I = I + 1
C
C         ***************
      IF (ZHDRHN(I,HDNAM).NE.OK) THEN
C         ***************
C
        GO TO 20
      ELSE IF (OLDNAM.NE.HDNAM) THEN
        GO TO 10
      END IF
      HNAM(I) = NEWNAM
      WRITE (SCRBUF,FMT='(A,A,I6,1Hx,I6)') BHDR,HNAM(I),LSIZ,LINNUM(I)
      WRITE (SCRLUN,FMT='(A)',REC=LHEAD(I)) SCRBUF
      ZHDRRE = OK
      RETURN
   20 ZHDRRE = ERR
C
C
      END
C
C
C
      INTEGER FUNCTION ZHDRHN(HNUM,HEAD)
C     ==================================
C
C
C---- Get header name
C     Input - hnum number of header slot in table
C     Output- head header name
C
C
C
C---- Stupid numbers and occupancy check
C
C     .. Parameters ..
      INTEGER MNHDRS,HDNAML
      PARAMETER (MNHDRS=1000,HDNAML=30)
      INTEGER ERR,OK
      PARAMETER (ERR=-1,OK=0)
C     ..
C     .. Scalar Arguments ..
      INTEGER HNUM
      CHARACTER HEAD* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER FFLIN
      LOGICAL SYSINI
C     ..
C     .. Arrays in Common ..
      INTEGER LHEAD,LINNUM,LREAD
      CHARACTER HNAM* (HDNAML)
C     ..
C     .. Common blocks ..
      COMMON /ZHDRDC/HNAM(MNHDRS)
      COMMON /ZHDRDT/SYSINI,FFLIN,LREAD(MNHDRS),LINNUM(MNHDRS),
     +       LHEAD(MNHDRS)
C     ..
C     .. Save statement ..
      SAVE /ZHDRDT/,/ZHDRDC/
C     ..
C
C
      IF (HNUM.LE.0 .OR. HNUM.GT.MNHDRS .OR. HNAM(HNUM).EQ.' ') THEN
        ZHDRHN = ERR
      ELSE
        ZHDRHN = OK
        HEAD = HNAM(HNUM)
      END IF
C
C
      END
C
C
C
      INTEGER FUNCTION ZHDRIN(IDUM)
C     ============================
C
C
C---- Initialize header system
C     use csratch file lun scrlun defined in zuhdrs.h
C
C
C     .. Parameters ..
      INTEGER SCRLUN,MNHDRS,HDNAML
      PARAMETER (SCRLUN=41,MNHDRS=1000,HDNAML=30)
      INTEGER OK
      PARAMETER (OK=0)
C     ..
      INTEGER IDUM
C     .. Scalars in Common ..
      INTEGER FFLIN
      LOGICAL SYSINI
C     ..
C     .. Arrays in Common ..
      INTEGER LHEAD,LINNUM,LREAD
      CHARACTER HNAM* (HDNAML)
C     ..
C     .. Local Scalars ..
      INTEGER I,JDO10
C     ..
C     .. Common blocks ..
      COMMON /ZHDRDC/HNAM(MNHDRS)
      COMMON /ZHDRDT/SYSINI,FFLIN,LREAD(MNHDRS),LINNUM(MNHDRS),
     +       LHEAD(MNHDRS)
C     ..
C     .. Save statement ..
      SAVE /ZHDRDT/,/ZHDRDC/
C     ..
C     .. Data statements ..
      DATA SYSINI/.FALSE./
C     ..
C
C---- This in internal routine and so it should not be called
C     from outside
C
C---- If system is already initialized - no action
C
      IF (.NOT.SYSINI) THEN
C
C---- Scratch file description
C
        DO 10 JDO10 = 1,MNHDRS
          HNAM(JDO10) = ' '
   10   CONTINUE
C
C
        FFLIN = 1
C
C---- Open header scratch file
      IFAIL = 0
      CALL CCPDPN(SCRLUN,'CCPHDRTMP','SCRATCH','DF',81,IFAIL)
C
CC        OPEN (UNIT=SCRLUN,FILE='ccphdrtmp',STATUS='NEW',
CC     +       FORM='FORMATTED',ACCESS='DIRECT',DISP='DELETE',RECL=81)
C
C---- Initialization is done
C
        SYSINI = .TRUE.
      END IF
      ZHDRIN = OK
C
C
      END
C
C
C
      INTEGER FUNCTION ZHDRIO(HDRLUN)
C     ===============================
C
C
C---- Read all headers from specified file
C     (must be opened on lun hdrlun)
C     Input - hdrlun file lun
C
C
C
C---- Initialize the header system
C
C     .. Parameters ..
      INTEGER MNHDRS,HDNAML
      PARAMETER (MNHDRS=1000,HDNAML=30)
      CHARACTER*1 BHDR,EHDR
      PARAMETER (BHDR='S',EHDR='E')
      CHARACTER*1 OCCUP
      PARAMETER (OCCUP='O')
      INTEGER ERR,OK
      PARAMETER (ERR=-1,OK=0)
C     ..
C     .. Scalar Arguments ..
      INTEGER HDRLUN
C     ..
C     .. Scalars in Common ..
      INTEGER FFLIN
      LOGICAL SYSINI
C     ..
C     .. Arrays in Common ..
      INTEGER LHEAD,LINNUM,LREAD
      CHARACTER HNAM* (HDNAML)
C     ..
C     .. Local Scalars ..
      INTEGER I,IER,LINLNI,LNG,NLINES,SCRBFI,ISTERR,IFGERR
      CHARACTER LINLNA*2,SCRBFA*81,HDNAM* (HDNAML),LINERR*100
C     ..
C     .. External Functions ..
      INTEGER ZHDRDF,ZHDRIN,ZHDRWR
      EXTERNAL ZHDRDF,ZHDRIN,ZHDRWR
C     ..
C     .. External Subroutines ..
      EXTERNAL QREAD,LERROR
C     ..
C     .. Common blocks ..
      COMMON /ZHDRDC/HNAM(MNHDRS)
      COMMON /ZHDRDT/SYSINI,FFLIN,LREAD(MNHDRS),LINNUM(MNHDRS),
     +       LHEAD(MNHDRS)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (LINLNA,LINLNI), (SCRBFA,SCRBFI)
C     ..
C     .. Save statement ..
      SAVE /ZHDRDT/,/ZHDRDC/
C     ..
C
C         ********
      I = ZHDRIN()
C         ********
C
   10 CONTINUE
C
C---- Read length of next line
C
C          **************************
      CALL QREAD(HDRLUN,LINLNI,2,IER)
C          **************************
C
      IF (IER.NE.0) THEN
        GO TO 40
      ELSE
        READ (LINLNA,FMT='(I2)') LNG
C
C---- and now line of given length
C
C             ****************************
         CALL QREAD(HDRLUN,SCRBFI,LNG,IER)
C             ****************************
C
C---- Decode type of line and process it
C
        IF (SCRBFA(1:1).EQ.BHDR) THEN
C
C---- Begin of new header
C
          READ (SCRBFA(2:81),FMT='(A,7X,I6)') HDNAM,NLINES
C
C               ********************
          IER = ZHDRDF(HDNAM,NLINES)
C               ********************
C
        ELSE IF (SCRBFA(1:1).EQ.EHDR) THEN
          GO TO 30
        ELSE IF (SCRBFA(1:1).EQ.OCCUP) THEN
C
C---- Some line of header
C
C               ***************************
          IER = ZHDRWR(HDNAM,SCRBFA(2:LNG))
C               ***************************
C
        ELSE
          GO TO 20
        END IF
C
C---- And now try to process next line
C
        GO TO 10
      END IF
   20 CONTINUE
C
       WRITE (LINERR,FMT='(A)') 
     +     'CCPHDRS -- BAD HEADER FORMAT'
          ISTERR = 2
          IFGERR = 1
C
C              *****************************
          CALL LERROR(ISTERR,IFGERRL,LINERR)
C              *****************************
C
      CALL CCPERR (1, 'Fatal Error')
C
C---- End of header
C
   30 CONTINUE
C
C---- READYYYYYY
C
      ZHDRIO = OK
      RETURN
   40 CONTINUE

C
       WRITE (LINERR,FMT='(A)') 
     +     'CCPHDRS -- INPUT ERROR ON DISK FILE'
          ISTERR = 2
          IFGERR = 1
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
      CALL CCPERR (1, 'Fatal Error')
C
C
      END
C
C
C
      INTEGER FUNCTION ZHDRDF(NAME,NLINES)
C     ====================================
C
C
C---- Define header
C     Input - name  name of header
C             nlines number of lines in header
C
C
C
C---- Initialize header system
C
C     .. Parameters ..
      INTEGER SCRLUN,MNHDRS,HDNAML,LSIZ
      PARAMETER (SCRLUN=41,MNHDRS=1000,HDNAML=30,LSIZ=80)
      CHARACTER*1 BHDR,EHDR,BLIN
      PARAMETER (BHDR='S',EHDR='E',BLIN='B')
      INTEGER ERR,OK
      PARAMETER (ERR=-1,OK=0)
C     ..
C     .. Scalar Arguments ..
      INTEGER NLINES
      CHARACTER NAME* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER FFLIN
      LOGICAL SYSINI
C     ..
C     .. Arrays in Common ..
      INTEGER LHEAD,LINNUM,LREAD
      CHARACTER HNAM* (HDNAML)
C     ..
C     .. Local Scalars ..
      INTEGER HDRID,I,JDO20
      CHARACTER SCRBUF* (LSIZ+1)
C     ..
C     .. External Functions ..
      INTEGER ZHDRIN
      EXTERNAL ZHDRIN
C     ..
C     .. Common blocks ..
      COMMON /ZHDRDC/HNAM(MNHDRS)
      COMMON /ZHDRDT/SYSINI,FFLIN,LREAD(MNHDRS),LINNUM(MNHDRS),
     +       LHEAD(MNHDRS)
C     ..
C     .. Save statement ..
      SAVE /ZHDRDT/,/ZHDRDC/
C     ..
C
C
C         ********
      I = ZHDRIN()
C         ********
C
C---- Find 1st free header descriptor
C
      HDRID = 1
   10 CONTINUE
      IF (HDRID.LE.MNHDRS .AND. HNAM(HDRID).NE.' ') THEN
        HDRID = HDRID + 1
        GO TO 10
      END IF
      IF (HDRID.GT.MNHDRS) THEN
        ZHDRDF = ERR
      ELSE
C
C---- Fill table
C
        HNAM(HDRID) = NAME
        LINNUM(HDRID) = NLINES
        LHEAD(HDRID) = FFLIN
        LREAD(HDRID) = FFLIN
        FFLIN = FFLIN + 1
C
C---- Label header
C
        WRITE (SCRBUF,FMT='(A,A,I6,1Hx,I6)') BHDR,HNAM(HDRID),LSIZ,
     +    NLINES
        WRITE (SCRLUN,FMT='(A)',REC=LHEAD(HDRID)) SCRBUF
C
C---- BlankHeader
C
        SCRBUF = BLIN
C
C
        DO 20 JDO20 = 1,NLINES
          WRITE (SCRLUN,FMT='(A)',REC=FFLIN) SCRBUF
          FFLIN = FFLIN + 1
   20   CONTINUE
C
C---- TailHeader
C
        SCRBUF = EHDR
        WRITE (SCRLUN,FMT='(A)',REC=FFLIN) SCRBUF
        FFLIN = FFLIN + 1
C
        ZHDRDF = OK
      END IF
C
C
      END
C
C
C
      INTEGER FUNCTION ZHDRDL(NAME)
C     =============================
C
C
C---- Delete header
C     Input - name CHAR name of header
C
C
C
C
C     .. Parameters ..
      INTEGER MNHDRS,HDNAML
      PARAMETER (MNHDRS=1000,HDNAML=30)
      INTEGER ERR,OK
      PARAMETER (ERR=-1,OK=0)
C     ..
C     .. Scalar Arguments ..
      CHARACTER NAME* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER FFLIN
      LOGICAL SYSINI
C     ..
C     .. Arrays in Common ..
      INTEGER LHEAD,LINNUM,LREAD
      CHARACTER HNAM* (HDNAML)
C     ..
C     .. Local Scalars ..
      INTEGER I,STATUS,JDO10,JDO30
      CHARACTER HDNAM* (HDNAML)
C     ..
C     .. External Functions ..
      INTEGER ZHDRHN
      EXTERNAL ZHDRHN
C     ..
C     .. Common blocks ..
      COMMON /ZHDRDC/HNAM(MNHDRS)
      COMMON /ZHDRDT/SYSINI,FFLIN,LREAD(MNHDRS),LINNUM(MNHDRS),
     +       LHEAD(MNHDRS)
C     ..
C     .. Save statement ..
      SAVE /ZHDRDT/,/ZHDRDC/
C     ..
C
C
      DO 10 JDO10 = 1,MNHDRS
C
C                *******************
        STATUS = ZHDRHN(JDO10,HDNAM)
C                *******************
C
        IF (STATUS.NE.OK) THEN
          GO TO 40
        ELSE IF (HDNAM.EQ.NAME) THEN
          GO TO 20
        END IF
   10 CONTINUE
C
C
      GO TO 40
C
   20 CONTINUE
C
C
       DO 30 JDO30 = JDO10,MNHDRS - 1
        LREAD(JDO30) = LREAD(JDO30+1)
        LINNUM(JDO30) = LINNUM(JDO30+1)
        LHEAD(JDO30) = LHEAD(JDO30+1)
        HNAM(JDO30) = HNAM(JDO30+1)
   30 CONTINUE
C
C
      HNAM(MNHDRS) = ' '
      ZHDRDL = OK
      RETURN
C
   40 ZHDRDL = ERR
C
C
      END
C
C
C
      INTEGER FUNCTION ZHDRFL(HDRLUN,NAME,OUTNAM)
C     ===========================================
C
C
C---- Flush header to output lun
C     Input - hdrlun  file lun
C             name    header name
C             outnam  header name in given file
C                     (if ' ' then same as name)
C
C
C     .. Parameters ..
      INTEGER SCRLUN,MNHDRS,HDNAML
      PARAMETER (SCRLUN=41,MNHDRS=1000,HDNAML=30)
      CHARACTER*1 EHDR
      PARAMETER (EHDR='E')
      CHARACTER*1 OCCUP
      PARAMETER (OCCUP='O')
      INTEGER ERR,OK
      PARAMETER (ERR=-1,OK=0)
C     ..
C     .. Scalar Arguments ..
      INTEGER HDRLUN
      CHARACTER NAME* (*),OUTNAM* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER FFLIN
      LOGICAL SYSINI
C     ..
C     .. Arrays in Common ..
      INTEGER LHEAD,LINNUM,LREAD
      CHARACTER HNAM* (HDNAML)
C     ..
C     .. Local Scalars ..
      INTEGER HDRID,I,LINLNI,SCRBFI,JDO20
      CHARACTER LINLNA*2,SCRBFA*81
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL QBACK,QWRITE
C     ..
C     .. Common blocks ..
      COMMON /ZHDRDC/HNAM(MNHDRS)
      COMMON /ZHDRDT/SYSINI,FFLIN,LREAD(MNHDRS),LINNUM(MNHDRS),
     +       LHEAD(MNHDRS)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (LINLNA,LINLNI), (SCRBFA,SCRBFI)
C     ..
C     .. Save statement ..
      SAVE /ZHDRDT/,/ZHDRDC/
C     ..
C
C
C---- Erase last END OF HEADER flag'
C
C          ***************
      CALL QBACK(HDRLUN,3)
C          ***************
C
C---- Find given header
C
      HDRID = 1
   10 CONTINUE
C
C
      IF (HDRID.LE.MNHDRS .AND. HNAM(HDRID).NE.NAME) THEN
        HDRID = HDRID + 1
        GO TO 10
      END IF
C
C
      IF (HDRID.GT.MNHDRS) THEN
C
C---- Error return
C
        ZHDRFL = ERR
      ELSE
C
C---- Label of header
C
        READ (SCRLUN,FMT='(A)',REC=LHEAD(HDRID)) SCRBFA
C
C---- If non empty output name - rewrite
C
        IF (OUTNAM.NE.' ') SCRBFA(2:HDNAML+1) = OUTNAM
C
C---- write HeaderHeader
C      length of line
C
        WRITE (LINLNA,FMT='(I2)') LENSTR(SCRBFA)
C
C            ***********************
        CALL QWRITE(HDRLUN,LINLNI,2)
C            ***********************
C
C---- Header line
C
C            ************************************
        CALL QWRITE(HDRLUN,SCRBFI,LENSTR(SCRBFA))
C            ************************************
C
C---- Write HeaderRecords
C
        DO 20 JDO20 = LHEAD(HDRID) + 1,
     +                LHEAD(HDRID) + LINNUM(HDRID) + 1
          READ (SCRLUN,FMT='(A)',REC=JDO20) SCRBFA
          IF (SCRBFA(1:1).NE.OCCUP) THEN
            GO TO 30
          ELSE
            WRITE (LINLNA,FMT='(I2)') LENSTR(SCRBFA)
C
C                ************************************
            CALL QWRITE(HDRLUN,LINLNI,2)
            CALL QWRITE(HDRLUN,SCRBFI,LENSTR(SCRBFA))
C                ************************************
C
          END IF
   20   CONTINUE
C
C---- Write end of header mark
C
   30   SCRBFA = EHDR
C
C                                 **************
        WRITE (LINLNA,FMT='(I2)') LENSTR(SCRBFA)
C                                 **************
C
C            ************************************
        CALL QWRITE(HDRLUN,LINLNI,2)
        CALL QWRITE(HDRLUN,SCRBFI,LENSTR(SCRBFA))
C            ************************************
C
        ZHDRFL = OK
      END IF
C
C
      END
C
C
C
      INTEGER FUNCTION ZHDRWR(NAME,LINE)
C     ==================================
C
C
C---- Write one line into given header
C     Input - name  header name
C             line  line
C
C
C
C---- Find given header
C
C     .. Parameters ..
      INTEGER SCRLUN,MNHDRS,HDNAML,LSIZ
      PARAMETER (SCRLUN=41,MNHDRS=1000,HDNAML=30,LSIZ=80)
      CHARACTER*1 BLIN
      PARAMETER (BLIN='B')
      CHARACTER*1 OCCUP
      PARAMETER (OCCUP='O')
      INTEGER ERR,ENDHD
      PARAMETER (ERR=-1,ENDHD=1)
C     ..
C     .. Scalar Arguments ..
      CHARACTER LINE* (*),NAME* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER FFLIN
      LOGICAL SYSINI
C     ..
C     .. Arrays in Common ..
      INTEGER LHEAD,LINNUM,LREAD
      CHARACTER HNAM* (HDNAML)
C     ..
C     .. Local Scalars ..
      INTEGER HDRID,I,JDO20
      CHARACTER SCRBUF* (LSIZ+1)
C     ..
C     .. Common blocks ..
      COMMON /ZHDRDC/HNAM(MNHDRS)
      COMMON /ZHDRDT/SYSINI,FFLIN,LREAD(MNHDRS),LINNUM(MNHDRS),
     +       LHEAD(MNHDRS)
C     ..
C     .. Save statement ..
      SAVE /ZHDRDT/,/ZHDRDC/
C     ..
C
C
      HDRID = 1
   10 CONTINUE
C
C
      IF (HDRID.LE.MNHDRS .AND. HNAM(HDRID).NE.NAME) THEN
        HDRID = HDRID + 1
        GO TO 10
      END IF
C
C
      IF (HDRID.GT.MNHDRS) THEN
        ZHDRWR = ERR
      ELSE
C
C---- Find it in scratch and go to 1st free line
C
        DO 20 JDO20 = LHEAD(HDRID) + 1,
     +                LHEAD(HDRID) + LINNUM(HDRID)
          READ (SCRLUN,FMT='(A)',REC=JDO20) SCRBUF
          IF (SCRBUF(1:1).EQ.BLIN) GO TO 30
   20   CONTINUE
C
C---- This header is full
C
        ZHDRWR = ENDHD
        RETURN
C
   30   SCRBUF(1:1) = OCCUP
        SCRBUF(2:) = LINE
        WRITE (SCRLUN,FMT='(A)',REC=JDO20) SCRBUF
C
        ZHDRWR = 0
      END IF
C
C
      END
C
C
C
      INTEGER FUNCTION ZHDRRD(NAME,LINE)
C     ==================================
C
C
C---- Read one line from given header
C     Input - name  header name
C     Output- line  text line
C
C
C
C---- Find given header
C
C     .. Parameters ..
      INTEGER SCRLUN,MNHDRS,HDNAML,LSIZ
      PARAMETER (SCRLUN=41,MNHDRS=1000,HDNAML=30,LSIZ=80)
      CHARACTER*1 OCCUP
      PARAMETER (OCCUP='O')
      INTEGER ERR,OK,ENDHD
      PARAMETER (ERR=-1,OK=0,ENDHD=1)
C     ..
C     .. Scalar Arguments ..
      CHARACTER LINE* (*),NAME* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER FFLIN
      LOGICAL SYSINI
C     ..
C     .. Arrays in Common ..
      INTEGER LHEAD,LINNUM,LREAD
      CHARACTER HNAM* (HDNAML)
C     ..
C     .. Local Scalars ..
      INTEGER HDRID,I
      CHARACTER SCRBUF* (LSIZ+1)
C     ..
C     .. Common blocks ..
      COMMON /ZHDRDC/HNAM(MNHDRS)
      COMMON /ZHDRDT/SYSINI,FFLIN,LREAD(MNHDRS),LINNUM(MNHDRS),
     +       LHEAD(MNHDRS)
C     ..
C     .. Save statement ..
      SAVE /ZHDRDT/,/ZHDRDC/
C     ..
C
C
      HDRID = 1
   10 CONTINUE
C
C
      IF (HDRID.LE.MNHDRS .AND. HNAM(HDRID).NE.NAME) THEN
        HDRID = HDRID + 1
        GO TO 10
      END IF
C
C
      IF (HDRID.GT.MNHDRS) THEN
        ZHDRRD = ERR
      ELSE
C
C---- Find it in scratch and read
C
        I = LREAD(HDRID) + 1
        READ (SCRLUN,FMT='(A)',REC=I) SCRBUF
        IF (SCRBUF(1:1).NE.OCCUP) THEN
          ZHDRRD = ENDHD
        ELSE
C
          LREAD(HDRID) = I
          LINE = SCRBUF(2:LSIZ+1)
          ZHDRRD = OK
        END IF
      END IF
C
C
      END
C
C
C
      INTEGER FUNCTION ZHDRRW(NAME)
C     =============================
C
C
C---- Rewind header for re-read (not rewrite)
C     Input - name CHAR name of header
C
C
C
C---- Find given header
C
C     .. Parameters ..
      INTEGER MNHDRS,HDNAML
      PARAMETER (MNHDRS=1000,HDNAML=30)
      INTEGER ERR,OK
      PARAMETER (ERR=-1,OK=0)
C     ..
C     .. Scalar Arguments ..
      CHARACTER NAME* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER FFLIN
      LOGICAL SYSINI
C     ..
C     .. Arrays in Common ..
      INTEGER LHEAD,LINNUM,LREAD
      CHARACTER HNAM* (HDNAML)
C     ..
C     .. Local Scalars ..
      INTEGER HDRID
C     ..
C     .. Common blocks ..
      COMMON /ZHDRDC/HNAM(MNHDRS)
      COMMON /ZHDRDT/SYSINI,FFLIN,LREAD(MNHDRS),LINNUM(MNHDRS),
     +       LHEAD(MNHDRS)
C     ..
C     .. Save statement ..
      SAVE /ZHDRDT/,/ZHDRDC/
C     ..
C
C
      HDRID = 1
   10 CONTINUE
C
C
      IF (HDRID.LE.MNHDRS .AND. HNAM(HDRID).NE.NAME) THEN
        HDRID = HDRID + 1
        GO TO 10
      END IF
C
C
      IF (HDRID.GT.MNHDRS) THEN
        ZHDRRW = ERR
      ELSE
C
C---- We have right header
C     so rewind
C
        LREAD(HDRID) = LHEAD(HDRID)
        ZHDRRW = OK
      END IF
C
C
      END
