C
C
C Routines for reading and writing atoms in Brookhaven or Diamond
C format. Uses CCP RBROOK, WBROOK etc for Brookhaven file, and
C makes Diamond file look similar.
C To be used in conjunction with RWBROOK
C
C Routines:
C      RDAINI  initialise input file, setting file type 
C              (Brookhaven or Diamond) optionally finds 
C              out if file is Brookhaven or Diamond, and
C              sets flags accordingly
C      RDATOM  read one atom from input file
C      WTAINI  initialise output file, setting file type 
C              (Brookhaven or Diamond)
C      WTATOM  write one atom to input file
C      WTAFIN  copy rest of input file to output, and close output file
C      SETORT  setup orthogonal to fractional matrix for Diamond file input
C              (this is done from S/r RBROOK for Brookhaven format input)
C
C On input from a Diamond file:
C   The 4-character residue name field is examined to see if its last
C   character is a digit. If it is not, then the name is split into a 
C   3-character name RESNO, and a 1-character chain identifier IDCH. 
C   Otherwise, all 4 characters are returned as the residue name RESNO
C   and IDCH is blank. The residue number IRES is read from column 6.
C
C    The atom type code IDW is returned in the extra field ISS.
C    The atomic number IZ is set to the electron count (weight), which
C      also returned as the "occupancy".
C 
C On output:
C  The residue name is made up of a 3-digit residue number and 
C  a one-character chain identifier
C
C------------------
C
      SUBROUTINE RDAINI(IUN,LTYPE,TITLE,MSG)
C     ======================================
C
C Initialise (or rewind) input file on unit IUN. The file must have
C already been opened
C
C LTYPE = 0 Brookhaven format
C       = 1 Diamond format
C       .lt. 0  unknown, find out file type (Brookhaven or Diamond)
C               return LTYPE = 0 or 1
C TITLE  for Diamond format only, returned as title read from file
C
C MSG is the message output stream
C
      PARAMETER (NSTRM=20)
      COMMON /RDATMC/ JTYPES(NSTRM),NSER
      COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE,IBRKFL
      CHARACTER*80 TITLE,JUNK*1
      SAVE /ORTHOG/
C
C
      IF(IUN.LE.0.OR.IUN.GT.NSTRM) THEN
        WRITE(MSG,*) 'RDAINI: unit number out of range'
        call ccperr(1,' stop in rwatom.for 5555')
      ENDIF
C
      REWIND IUN
      NSER=0
C
      IF(LTYPE.LT.0) THEN
C
C---- Check file type by reading 1st character of 4th line
C
        READ(IUN,1003) JUNK
 1003   FORMAT(///A1)
C
C---- If alphabetic, then file is Brookhaven
C
        IF((LGE(JUNK,'A').AND.LLE(JUNK,'Z')).OR.
     .         (LGE(JUNK,'a').AND.LLE(JUNK,'z'))) THEN
          LTYPE=0
          WRITE(MSG,1004) 'Brookhaven'
 1004     FORMAT(' Input file is ',A)
        ELSE
          LTYPE=1
          WRITE(MSG,1004) 'Diamond'
        ENDIF
        REWIND IUN
      ENDIF
C
      JTYPES(IUN)=LTYPE
C
      IF(LTYPE.EQ.0) THEN
C
C---- Brookhaven
C
        CALL RBINIT(IUN)
      ELSEIF(LTYPE.EQ.1) THEN
C
C---- Diamond
C
        READ(IUN,1001) TITLE
 1001   FORMAT(/A/)
        WRITE(MSG,1002) TITLE
 1002   FORMAT(/' Title from input atom file: ',A/)
        IBRKFL=1
      ELSE
        WRITE(MSG,*) 'RDAINI: illegal type'
        call ccperr(1,' stop in rwatom.for 5566')
      ENDIF
      RETURN
      END
C
C
C
      SUBROUTINE RDATOM(IUN,ISER,ATNAM,RESTYP,IDCH,IRES,RESNO,IS,
     .  X,Y,Z,Q,B,IZ,IOUT,MSG1,MSG2,ITER,*,*)
C     ==========================================================
C
C
C
C Parameters
C
C         IUN (I) Unit number of the input coordinate file
C        ISER (O) Atom serial number
C       ATNAM (O) Atom name        (Character*4, left justified)
C      RESTYP (O) Residue type     (Character*4, left justified)
C        IDCH (O) Chain identifier (character*1)
C        IRES (O) Residue number as an integer
C       RESNO (O) Residue number   (Character*4, right justified)
C          IS (O) Brookhaven: extra flag. Diamond: atom type number
C           X (O) coordinates (as in file
C           Y (O)     "        
C           Z (O)     "
C           Q (O) Occupancy (electron count for Diamond files)
C           B (O) Temperature factor
C          IZ (O) Atomic number (returned as 7 from ambiguous atoms)
C                 Taken from the electron count for Diamond files
C        IOUT (I) unit number to which non-atom/hetatm records are t
C                 be be written (may be 0 if reading only)
C        MSG1 (I) unit number for flagging ambiguous/unknown atom types
C                 (may be 0 if messages are not required)
C        MSG2 (I) Unit number for listing matrix (if calculated)
C                 (may be 0 if no listing required)
C        ITER (I) flag =1, return if 'TER' card found (via RETURN 1)
C                      =0, do not return when 'TER' card found
C
C  RETURN 1   return on 'TER' card found (only if ITER=1)
C  RETURN 2   return on end of file found
C
C
      CHARACTER*4 ATNAM,RESTYP,RESNO,RESNAM
      CHARACTER*1 IDCH
      PARAMETER (NSTRM=20)
      COMMON /RDATMC/ JTYPES(NSTRM),NSER
C
C---- Set error monitor stream
C
      MM=MSG1
      IF(MM.LE.0) MM=6
C
C---- Store file type for this stream
C
      LTYPE=JTYPES(IUN)
      IF(LTYPE.EQ.0) THEN
C
C---- Brookhaven input file
C
        CALL RBROOK(IUN,ISER,ATNAM,RESTYP,IDCH,IRES,RESNO,IS,
     .  X,Y,Z,Q,B,IZ,IOUT,MSG1,MSG2,ITER,*10,*11)
        RETURN
 10     RETURN 1
 11     RETURN 2
C
      ELSEIF(LTYPE.EQ.1) THEN
C 
C---- Diamond format file
C
        READ(IUN,1001,END=29) X,Y,Z,B,IDW,JRES,JUNK,WEIGHT,
     .         RESTYP,RESNAM,ATNAM
 1001   FORMAT(4F10.5,3I5,F9.0,1X,A3,A4,3X,A4)
C
C---- Atom serial number
C
        NSER=NSER+1
        ISER=NSER
C
C---- Check if last character of residue name is a digit
C
        IF(LGE(RESNAM(4:4),'0').AND.LLE(RESNAM(4:4),'9')) THEN
C
C---- Last character is a digit, try treating RESNAM as a 
C     4-digit number set chain identifier blank
C
          IDCH=' '
          RESNO=RESNAM
C
        ELSE
C
C---- Last character is not a digit, use as chain identifier
C
          IDCH=RESNAM(4:4)
          RESNO=' '//RESNAM(1:3)
        ENDIF
C
C---- Set residue number from 6th field
C
        IRES=JRES
C
C---- Occupancy = electron count
C
        Q=WEIGHT
        IS=IDW
        IZ=NINT(Q)
        RETURN
C
C---- End of file
C
 29     RETURN 2
      ENDIF
C
C
      END
C
C
C
      SUBROUTINE WTAINI(IUN,LTYPE,TITLE,MSG)
C     ======================================
C
C Initialise previously opened atom output file 
C  LTYPE = 0 Brookhaven format
C        = 1 Diamond format
C  TITLE character*80 title for Diamond file
C
C  MSG  message output stream
C
C
      PARAMETER (NSTRM=20)
      COMMON /WTATMC/ JTYPES(NSTRM),NSER
      CHARACTER*80 TITLE
C
C
      IF(IUN.LE.0.OR.IUN.GT.NSTRM) THEN
        WRITE(MSG,*) 'WTAINI: unit number out of range'
        call ccperr(1,' stop in rwatom.for 5544')
      ENDIF
      JTYPES(IUN)=LTYPE
      NSER=0
C
      IF(LTYPE.EQ.0) THEN
C
C---- Brookhaven
C
        CONTINUE
      ELSEIF(LTYPE.EQ.1) THEN
C
C---- Diamond
C
        WRITE(IUN,1001) TITLE
 1001   FORMAT(/A/)
      ELSE
        WRITE(MSG,*) 'WTAINI: illegal type'
        call ccperr(1,' stop in rwatom.for 5533')
      ENDIF
      RETURN
      END
C
C
C
      SUBROUTINE WTATOM(
     .   IUN,ISER,ATNAM,RESTYP,IDCH,IRES,IS,X,Y,Z,Q,B,IZ)
C     ===================================================
C
C
C PARAMETERS
C
C         IUN (I) unit number of the output coordinate file
C        ISER (I) atom serial number
C       ATNAM (I) atom name (character*4, left justified)
C      RESTYP (I) residue type (character*4)
C        IDCH (I) chain identifier (character*1)
C        IRES (I) residue number as an integer (max. of 3 digits)
C          IS (I) Brookhaven: extra flag. Diamond: atom type number
C           X (I) coordinates 
C           Y (I)     "
C           Z (I)     "
C           Q (I) occupancy
C           B (I) temperature factor
C          IZ (I) atomic number (may be 0 if the atomic symbol is a
C                                single character e.g. C,N,O,H,S)
C
C
      PARAMETER (NSTRM=20)
      COMMON /WTATMC/ JTYPES(NSTRM),NSER
      CHARACTER*4 ATNAM,RESTYP
      CHARACTER*1 IDCH
      PARAMETER (MAXTYP=20,NTYPES=8,NNTYPE=MAXTYP-NTYPES)
      DIMENSION JZATYP(MAXTYP)
C
C---- Atomic numbers for atoms of types 1,2,3 etc
C                C N O  S Fe H  P Mg  fill in others here as required
C
      DATA JZATYP/6,7,8,16,24,1,15,12,NNTYPE*0/
C
C
C---- Error monitor stream
C
      MM=6
C
      IF(JTYPES(IUN).EQ.0) THEN
C
C---- Brookhaven
C
        CALL WBROOK(
     .       IUN,ISER,ATNAM,RESTYP,IDCH,IRES,IS,X,Y,Z,Q,B,IZ)
      ELSEIF(JTYPES(IUN).EQ.1) THEN
C
C---- Diamond
C     Find atom type code
C
        IF(IS.LE.0) THEN
          IF(IZ.EQ.0) THEN
            IZ=7
            IDW=2
          ELSE
            DO 10,IDW=1,NTYPES
              IF(IZ.EQ.JZATYP(IDW)) GO TO 11
10                      CONTINUE
                        WRITE(MM,1001) ATNAM,IRES,IZ
 1001                   FORMAT(/
     .          ' WTATOM: no atom type number for atom ',A,2I5,
     .             ', set to 2')
                        IDW=2
                      ENDIF
                    ELSE
                      IDW=IS
                    ENDIF
C
11          WEIGHT=Q
C
            WRITE(IUN,1011) 
     .       X,Y,Z,B,IDW,IRES,ISER,WEIGHT,RESTYP,IRES,IDCH,ATNAM
 1011       FORMAT(4F10.5,3I5,F9.4,1X,A3,I3,A1,3X,A4)
      ENDIF
      RETURN
      END
C
C
C
      SUBROUTINE WTAFIN(IIN,IOUT)
C     ===========================
C
C Copy rest of input atom file to output
C
      CHARACTER*80 LINE
C
10    READ(IIN,1001,END=100) LINE
1001  FORMAT(A)
      WRITE(IOUT,1001) LINE
      GO TO 10
C
100   RETURN
      END
C
C
C
       SUBROUTINE SETORT(A,B,C,ALPHA,BETA,GAMMA,NCODEI,MSG)
C      ====================================================
C
C  Set up orthogonal to fractional (RF) and fractional to orthogonal (RO)
C  matrices with appropriate NCODE. If input is from Brookhaven file,
C  this should be done from RBROOK, so this routine should not be called.
C
      COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE,IBRKFL
      LOGICAL IFCRYS,IFSCAL,IFEND,IFTER,MATRIX
      COMMON /RBRKXX/IFCRYS,IFSCAL,IFEND,ITYP,MATRIX
      COMMON /RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
C
C
      CHARACTER*15  CODES(5)
      SAVE /RBRKZZ/,/RBRKXX/,/ORTHOG/
      DATA CODES/'a,c*xa,c*','b,a*xb,a*','c,b*xc,b*',
     . 'a+b,c*x(a+b),c*','a*,cxa*,c'/
C
C
      NCODE=NCODEI
C
      IF(NCODE.LE.0.OR.NCODE.GT.5) THEN
        WRITE(5,300)
300         FORMAT(//'  NCODE ouside range 1 to 5, reset to 1')
            NCODE=1
      ENDIF
C
C
      IF(MSG.GT.0) THEN
        WRITE(MSG,1001) NCODE,CODES(NCODE)
 1001   FORMAT(/' Orthogonalisation code ',I4,' axes along ',A)
      ENDIF
C
C
      DO 1 I=1,4
      DO 2 J=1,4
      RO(I,J)=0.
      RF(I,J)=0.
2     CONTINUE
      RO(I,I)=1.0
      RF(I,I)=1.0
1     CONTINUE
C
      CELL(1)=A
      CELL(2)=B
      CELL(3)=C
      CELL(4)=ALPHA
      CELL(5)=BETA
      CELL(6)=GAMMA
C
C---- Calculate matrices for all useful NCODEs
C
      CALL RBFROR
C
C---- and copy the one we want
C
      DO 10,I=1,3
      DO 10,J=1,3
10    RO(I,J)=RR(I,J,NCODE)
C
C---- Invert
C
      CALL RBRINV(RO,RF)
C
C---- Set flag
C
      MATRIX=.TRUE.
C
      RETURN
      END
