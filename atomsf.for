      PROGRAM TESTSF
C      ==============
C
       IMPLICIT NONE
C
C
C     .. Parameters ..
      INTEGER MAXIN
      PARAMETER (MAXIN=20)
C     ..
C     .. Local Scalars ..
      REAL WAVE
      INTEGER NGPRG,NUMIN
C     ..
C     .. Local Arrays ..
      INTEGER INGASS(MAXIN)
      CHARACTER INFORM(MAXIN)*4
C     ..
C     .. External Subroutines ..
      EXTERNAL EXAMPL
C     ..
      NGPRG = 2
C
C---- GET VALUES FROM PARSER ETC
C
      WAVE = 0.88
      NUMIN = 4
      INGASS(1) = 5
      INGASS(2) = 5
      INGASS(3) = 5
      INGASS(4) = 5
      INFORM(1) = 'Mg+2'
      INFORM(2) = 'HG  '
      INFORM(3) = 'I-1 '
      INFORM(4) = 'pt  '
C
C
      CALL EXAMPL(INFORM,INGASS,NUMIN,MAXIN,WAVE,NGPRG)
C
C
      STOP
      END
C
C
      SUBROUTINE EXAMPL(INFORM,INGASS,NUMIN,MAXIN,WAVE,NGPRG)
C      ======================================================
C
C     EXAMPLE PROGRAM FOR GETTING ATOMIC SCATTERING INFO
C
C
       IMPLICIT NONE
C
C
C     .. Parameters ..
      INTEGER MAXASF
      PARAMETER (MAXASF=20)
      INTEGER MAXBIN
      PARAMETER (MAXBIN=8)
C     ..
C     .. Scalar Arguments ..
      REAL WAVE
      INTEGER MAXIN,NGPRG,NUMIN
C     ..
C     .. Array Arguments ..
      INTEGER INGASS(MAXIN)
      CHARACTER INFORM(MAXIN)*4
C     ..
C     .. Local Scalars ..
      REAL ANOM,RESMAX,RESMIN,ZZ
      INTEGER IATNUM,IFLAG,II,ISHOW,JJ,KK,LL,LUNOUT,MCOUNT,NG,NNG,NSFPRG
      LOGICAL CALAFF,FIRST,LELEC
      CHARACTER ID*4
C     ..
C     .. Local Arrays ..
      REAL AFORM(MAXASF,MAXBIN),FMANOM(MAXASF),FMFA(4,MAXASF),
     +     FMFB(4,MAXASF),FMFC(MAXASF),SF(9)
      INTEGER IDW(MAXASF),IZATM(MAXASF),NGASS(MAXASF)
      CHARACTER PROGID(MAXASF)*4
C     ..
C     .. External Subroutines ..
      EXTERNAL ATOMSF,MAKLIB,SFCALC,SFREAD,SHOWSF
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
C     .. Data statements ..
      DATA (PROGID(JJ),JJ=1,5)/'H   ','C   ','N   ','O   ','S   '/
      DATA (FMFA(3,JJ),JJ=1,MAXASF)/MAXASF*0.0/
      DATA (FMFA(4,JJ),JJ=1,MAXASF)/MAXASF*0.0/
      DATA (FMFB(3,JJ),JJ=1,MAXASF)/MAXASF*0.0/
      DATA (FMFB(4,JJ),JJ=1,MAXASF)/MAXASF*0.0/
      DATA (FMFC(JJ),JJ=1,MAXASF)/MAXASF*0.0/
      DATA NGASS/MAXASF*2/
C     ..
      NSFPRG = 5
C
      LUNOUT = 6
C
C---- Print out available atom identifiers
C
      CALL SHOWSF(LUNOUT)
C
C---- Write out a new library file ... ATOMSF.LIB
C
      CALL MAKLIB(LUNOUT)
C
C---- Get 2 Gauss terms for program default atoms
C     i.e.  H,C,N,O,S
C
      ISHOW = 0
      IFLAG = 0
      NG = NGPRG
      IF (NGPRG.EQ.0) NG = 2
      DO 20 JJ = 1,NSFPRG
        ID = PROGID(JJ)
        CALL ATOMSF(ID,SF,IFLAG,NG,ZZ,IATNUM,LUNOUT,ISHOW,LELEC,ANOM,
     +              WAVE)
        NNG = NG
        IF (NNG.EQ.5) NNG = 4
        DO 10 KK = 1,NNG
          LL = 2*KK - 1
          FMFA(KK,JJ) = SF(LL)
          FMFB(KK,JJ) = SF(LL+1)
   10   CONTINUE
        IF (NG.EQ.5) FMFC(JJ) = SF(9)
        IZATM(JJ) = NINT(ZZ)
        IDW(JJ) = IATNUM
        IF (LELEC) THEN
          FMANOM(JJ) = ANOM
        ELSE
          FMANOM(JJ) = 1.0
        END IF
        IF (CALAFF) CALL SFCALC(JJ,SF,ZZ,AFORM,LELEC,RESMIN,RESMAX,
     +                          MAXASF,MAXBIN,NG)
C
C---- Write out found values
C
        WRITE (LUNOUT,FMT=6000) ID,NG,IZATM(JJ),IDW(JJ)
        WRITE (LUNOUT,FMT=6002) FMANOM(JJ),FMFC(JJ)
        WRITE (LUNOUT,FMT=6004)
        WRITE (LUNOUT,FMT=6006) (FMFA(LL,JJ),FMFB(LL,JJ),LL=1,NNG)
   20 CONTINUE
C
C
C
C---- Now get values for atom identifiers input
C     by keyword FORM
C
      IF (NUMIN.GT.0) THEN
        ISHOW = 0
        IFLAG = 0
        MCOUNT = NSFPRG
        DO 40 JJ = 1,NUMIN
          MCOUNT = MCOUNT + 1
          IF (MCOUNT.GT.MAXASF) THEN
            GO TO 50
          ELSE
            ID = INFORM(JJ)
            NG = INGASS(JJ)
            CALL ATOMSF(ID,SF,IFLAG,NG,ZZ,IATNUM,LUNOUT,ISHOW,LELEC,
     +                  ANOM,WAVE)
            IF (IFLAG.EQ.-1) THEN
              WRITE (LUNOUT,FMT=6008) ID
              MCOUNT = MCOUNT - 1
              IFLAG = 0
            ELSE
              NNG = NG
              IF (NNG.EQ.5) NNG = 4
              DO 30 KK = 1,NNG
                LL = 2*KK - 1
                FMFA(KK,MCOUNT) = SF(LL)
                FMFB(KK,MCOUNT) = SF(LL+1)
   30         CONTINUE
              IF (NG.EQ.5) FMFC(MCOUNT) = SF(9)
              IZATM(MCOUNT) = NINT(ZZ)
              IDW(MCOUNT) = IATNUM
              IF (LELEC) THEN
                FMANOM(MCOUNT) = ANOM
              ELSE
                FMANOM(MCOUNT) = 1.0
              END IF
              IF (CALAFF) CALL SFCALC(MCOUNT,SF,ZZ,AFORM,LELEC,RESMIN,
     +                                RESMAX,MAXASF,MAXBIN,NG)
C
C---- Write out found values
C
              WRITE (LUNOUT,FMT=6000) ID,NG,IZATM(MCOUNT),IDW(MCOUNT)
              WRITE (LUNOUT,FMT=6002) FMANOM(MCOUNT),FMFC(MCOUNT)
              WRITE (LUNOUT,FMT=6004)
              WRITE (LUNOUT,FMT=6006) (FMFA(LL,MCOUNT),FMFB(LL,MCOUNT),
     +          LL=1,NNG)
            END IF
          END IF
   40   CONTINUE
C
C
        GO TO 60
   50   WRITE (LUNOUT,FMT=6010) MAXASF
        STOP
      END IF
C
C---- Alternative to read data from file
C
   60 OPEN (UNIT=45,FILE='ATOMSF.LIB',STATUS='OLD',READONLY)
      FIRST = .TRUE.
C
C---- Get 2 Gauss terms for program default atoms from library file
C     i.e.  H,C,N,O,S
C
      IFLAG = 0
      NG = NGPRG
      IF (NGPRG.EQ.0) NG = 2
      DO 80 JJ = 1,NSFPRG
        ID = PROGID(JJ)
        CALL SFREAD(ID,SF,IFLAG,NG,ZZ,IATNUM,LUNOUT,ISHOW,LELEC,ANOM,
     +              WAVE,FIRST)
        NNG = NG
        IF (NNG.EQ.5) NNG = 4
        DO 70 KK = 1,NNG
          LL = 2*KK - 1
          FMFA(KK,JJ) = SF(LL)
          FMFB(KK,JJ) = SF(LL+1)
   70   CONTINUE
        IF (NG.EQ.5) FMFC(JJ) = SF(9)
        IZATM(JJ) = NINT(ZZ)
        IDW(JJ) = IATNUM
        IF (LELEC) THEN
          FMANOM(JJ) = ANOM
        ELSE
          FMANOM(JJ) = 1.0
        END IF
        IF (CALAFF) CALL SFCALC(JJ,SF,ZZ,AFORM,LELEC,RESMIN,RESMAX,
     +                          MAXASF,MAXBIN,NG)
C
C---- Write out found values
C
        WRITE (LUNOUT,FMT=6000) ID,NG,IZATM(JJ),IDW(JJ)
        WRITE (LUNOUT,FMT=6002) FMANOM(JJ),FMFC(JJ)
        WRITE (LUNOUT,FMT=6004)
        WRITE (LUNOUT,FMT=6006) (FMFA(LL,JJ),FMFB(LL,JJ),LL=1,NNG)
   80 CONTINUE
C
C
C
C---- Now get values for atom identifiers input from library file
C     by keyword FORM
C
      IF (NUMIN.GT.0) THEN
        ISHOW = 0
        IFLAG = 0
        MCOUNT = NSFPRG
        DO 100 JJ = 1,NUMIN
          MCOUNT = MCOUNT + 1
          IF (MCOUNT.GT.MAXASF) THEN
            GO TO 110
          ELSE
            ID = INFORM(JJ)
            NG = INGASS(JJ)
            CALL SFREAD(ID,SF,IFLAG,NG,ZZ,IATNUM,LUNOUT,ISHOW,LELEC,
     +                  ANOM,WAVE,FIRST)
            IF (IFLAG.EQ.-1) THEN
              WRITE (LUNOUT,FMT=6008) ID
              MCOUNT = MCOUNT - 1
              IFLAG = 0
            ELSE
              NNG = NG
              IF (NNG.EQ.5) NNG = 4
              DO 90 KK = 1,NNG
                LL = 2*KK - 1
                FMFA(KK,MCOUNT) = SF(LL)
                FMFB(KK,MCOUNT) = SF(LL+1)
   90         CONTINUE
              IF (NG.EQ.5) FMFC(MCOUNT) = SF(9)
              IZATM(MCOUNT) = NINT(ZZ)
              IDW(MCOUNT) = IATNUM
              IF (LELEC) THEN
                FMANOM(MCOUNT) = ANOM
              ELSE
                FMANOM(MCOUNT) = 1.0
              END IF
              IF (CALAFF) CALL SFCALC(MCOUNT,SF,ZZ,AFORM,LELEC,RESMIN,
     +                                RESMAX,MAXASF,MAXBIN,NG)
C
C---- Write out found values
C
              WRITE (LUNOUT,FMT=6000) ID,NG,IZATM(MCOUNT),IDW(MCOUNT)
              WRITE (LUNOUT,FMT=6002) FMANOM(MCOUNT),FMFC(MCOUNT)
              WRITE (LUNOUT,FMT=6004)
              WRITE (LUNOUT,FMT=6006) (FMFA(LL,MCOUNT),FMFB(LL,MCOUNT),
     +          LL=1,NNG)
            END IF
          END IF
  100   CONTINUE
C
C
        GO TO 120
  110   WRITE (LUNOUT,FMT=6010) MAXASF
        STOP
      END IF
C
C
  120 CLOSE (UNIT=45)
C
C
C---- Format statements
C
 6000 FORMAT (/' ID = ',A,' NG = ',I2,' No electrons = ',I4,' Atomic n',
     +       'umber for BRK = ',I4)
 6002 FORMAT ('  ANOM = ',F8.4,' C = ',F8.4)
 6004 FORMAT ('  FMFA and FMFB ')
 6006 FORMAT (2 (2X,F12.8))
 6008 FORMAT (' No data for atom identifier .. ',A)
 6010 FORMAT (//' **** ERROR, MAXIMUM NUMBER OF FORM FACTORS ****',
     +       /'      EXCEEDED, ONLY ',I4,' ALLOWED')
C
C
      END
C
C
      SUBROUTINE SFREAD(ID,SF,IFLAG,NG,ZZ,IATNUM,LUNOUT,ISHOW,LELEC,
     +                  ANOM,WAVE,FIRST)
C       ========================================================
C
C
       IMPLICIT NONE
C
C
C     .. Scalar Arguments ..
      REAL ANOM,WAVE,ZZ
      INTEGER IATNUM,IFLAG,ISHOW,LUNOUT,NG
      LOGICAL FIRST,LELEC
      CHARACTER ID*4
C     ..
C     .. Array Arguments ..
      REAL SF(9)
C     ..
C     .. Local Scalars ..
      REAL CUF1,CUF11,MOF1,MOF11
      INTEGER JJ
      CHARACTER ID2*6,IDIN*6
C     ..
C
C
      ID2 = ID//'  '
      IF (NG.EQ.0) NG = 2
      IF (NG.EQ.2) ID2 = ID//' 2'
      IF (.NOT.FIRST) REWIND 45
C
C---- Check that identifier supplied is correct case
C
        IF (LGE(ID2(1:1), 'a') .AND. LLE(ID2(1:1), 'z'))
     +     ID2(1:1) = CHAR(ICHAR(ID2(1:1)) - 32)
        IF (LGE(ID2(2:2), 'A') .AND. LLE(ID2(2:2), 'Z'))
     +     ID2(2:2) = CHAR(ICHAR(ID2(2:2)) + 32)
C
C---- Search for identifier
C
   10 CONTINUE
      READ (45,FMT=6000,END=40,ERR=30) IDIN
      IF (ID2.EQ.IDIN) THEN
        GO TO 50
      ELSE
        DO 20 JJ = 1,6
          READ (45,FMT=6002)
   20   CONTINUE
        GO TO 10
      END IF
C
C
   30 STOP ' **** ERROR IN ATOMSF.LIB ****'
C
C---- No match
C
   40 WRITE (LUNOUT,FMT=6016) ID2,NG
      IFLAG = -1
      GO TO 60
C
C---- Matched identifier
C
   50 CONTINUE
      READ (45,FMT=6008) ZZ,IATNUM
      READ (45,FMT=6004) SF(1),SF(2),SF(3),SF(4),SF(5),SF(6),SF(7),
     +  SF(8),SF(9)     
      READ (45,FMT=6006) CUF1,CUF11
      READ (45,FMT=6006) MOF1,MOF11
C
C
      IFLAG = 0
      IF (.NOT.LELEC) THEN
        ANOM = 1.0
      ELSE IF (WAVE.GT.0.7 .AND. WAVE.LT.0.9) THEN
        ANOM = MOF11
      ELSE IF (WAVE.GT.1.4 .AND. WAVE.LT.1.6) THEN
        ANOM = CUF11
      ELSE IF (WAVE.GT.-0.00001 .AND. WAVE.LT.0.00001) THEN
        WRITE (LUNOUT,FMT=6010)
        ANOM = CUF11
      ELSE
        WRITE (LUNOUT,FMT=6012) WAVE
        ANOM = 1.0
      END IF
C
C
C
      WRITE (LUNOUT,FMT=6014) ID,SF
C
C
C
   60 CONTINUE
C
C
      FIRST = .FALSE.
C
C---- Format statements
C
 6000 FORMAT (A6)
 6002 FORMAT (1X)
 6004 FORMAT (3 (2X,F14.8,2X))
 6006 FORMAT (2 (2X,F14.8,2X))
 6008 FORMAT (2X,F14.8,2X,I8)
 6010 FORMAT (//' **** FOR DELTAF USING COPPER RADIATION ****')
 6012 FORMAT (//' **** WAVELENGTH OF ',F8.5,' NOT SUITABLE ')
 6014 FORMAT (//2X,'Coefficients for Analytical Approximation to the',
     +       /'  Scattering Factors for atom identifier ... ',A,' are',
     +       /3 (3 (3X,F10.5),/),'  For a1, b1, a2, b2, a3, b3, a4, b4',
     +       ', c, respectively',/'  see International Tables vol4 tab',
     +       'le 2.2B')
 6016 FORMAT ('  atom identifier  ',A,' not found in library',I2)
C
C
      END
C
C     
      SUBROUTINE SHOWSF(LUNOUT)
C       =========================
C
       IMPLICIT NONE
C
C
C     .. Scalar Arguments ..
      INTEGER LUNOUT
C     ..
C     .. Local Scalars ..
      REAL ANOM,WAVE,ZZ
      INTEGER IATNUM,IFLAG,ISHOW,NG
      LOGICAL LELEC
      CHARACTER ID*4
C     ..
C     .. Local Arrays ..
      REAL SF(9)
C     ..
C     .. External Subroutines ..
      EXTERNAL ATOMSF
C     ..
      ISHOW = -1
      IFLAG = 0
      CALL ATOMSF(ID,SF,IFLAG,NG,ZZ,IATNUM,LUNOUT,ISHOW,LELEC,ANOM,WAVE)
C
C
      END
C
C
      SUBROUTINE MAKLIB(LUNOUT)
C      =========================
C
       IMPLICIT NONE
C
C
C     .. Scalar Arguments ..
      INTEGER LUNOUT
C     ..
C     .. Local Scalars ..
      REAL ANOM,WAVE,ZZ
      INTEGER IATNUM,IFLAG,ISHOW,NG
      LOGICAL LELEC
      CHARACTER ID*4
C     ..
C     .. Local Arrays ..
      REAL SF(9)
C     ..
C     .. External Subroutines ..
      EXTERNAL ATOMSF
C     ..
      ISHOW = 1
      IFLAG = 0
      CALL ATOMSF(ID,SF,IFLAG,NG,ZZ,IATNUM,LUNOUT,ISHOW,LELEC,ANOM,WAVE)
C
C
      END
C
C
      SUBROUTINE SFCALC(JJ,SF,ZZ,AFORM,LELEC,RESMIN,RESMAX,MAXASF,
     +                  MAXBIN,NG)
C      ======================================================
C
C
       IMPLICIT NONE
C
C
C     .. Scalar Arguments ..
      REAL RESMAX,RESMIN,ZZ
      INTEGER JJ,MAXASF,MAXBIN,NG
      LOGICAL LELEC
C     ..
C     .. Array Arguments ..
      REAL AFORM(MAXASF,MAXBIN),SF(9)
C     ..
C     .. Local Scalars ..
      REAL DSTMAX,DSTMIN,EEI,S,SS,TEMP
      INTEGER II,KK,LL,NNG
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC EXP
C     ..
      DSTMIN = 1.0/RESMAX
      DSTMAX = 1.0/RESMIN
      EEI = (DSTMIN-DSTMAX)/ (MAXBIN-2)
      S = DSTMIN/2.0
C
C
      NNG = NG
      IF (NG.EQ.5) NNG = NG - 1
C
C
      DO 20 LL = 1,MAXBIN
        SS = S*S
        TEMP = 0.0
        DO 10 KK = 1,NNG
          II = 2*KK - 1
          TEMP = EXP(-SF(II+1)*SS)*SF(II) + TEMP
   10   CONTINUE
        IF (NG.EQ.5) TEMP = TEMP + SF(9)
        AFORM(JJ,LL) = TEMP
        IF (.NOT.LELEC) AFORM(JJ,LL) = AFORM(JJ,LL)/ZZ
        S = S + EEI
   20 CONTINUE
C
C
      END
C
C
      SUBROUTINE ATOMSF(ID,SF,IFLAG,NG,ZZ,IATNUM,LUNOUT,ISHOW,LELEC,
     +                  ANOM,WAVE)
C      ==================================================
C
C----
C    Atomic scattering factors have been taken from International
C    tables vol 4 pages 99 -- 101 (Table 2.2B)
C
C    Values a1,b1,a2,b2,a3,b3,a4,b4,c are coefficients for the
C    analytic expression;
C
C    f(lambda-1sin(theta)) = [Sum ai exp(-bi(lambda-2)sin**2theta)] + c
C            for i = 1,2,3,4
C
C    see page 71 for meaning of Cv and Siv (ie Cval and Sival)
C----
C    Z() stores the number of electrons for each atomic symbol
C
C----
C    Real and imaginary dispersion corrections for atomic
C    scattering factors taken from International tables
C    pages 149 -- 150 (Table 2.3.1)
C
C    values are stored for Cu radiation
C    as CUF1()  for deltaf'
C       CUF11() for deltaf''
C   and
C                      for Mo radiation
C    as MOF1()  for deltaf'
C       MOF11() for deltaf''
C
C----
C    Subroutine usage:-
C
C    On entry if ISHOW = -1
C    then the list of available ATOMID's are printed
C    to unit = LUNOUT and then RETURN
C
C    On entry if ISHOW = 1
C    then a file ATOMSF.LIB is written
C    containing all the information stored in the DATA statements
C    So that the programmer has the choice of either linking
C    this subroutine or using the library file in the same
C    way as SYMOP.LIB or PGDATA.LIB are used.
C
C    On entry if ISHOW = 0
C    then get scattering factors for atom with identifier
C    held in ID... if ID is not matched with ATOMID()
C    then IFLAG = -1 and RETURN
C
C---- Values STORED
C     =============
C
C  (For ATOMSF.LIB Line 1) ATOMID(i) CHARACTER*6  Identifier
C  (...            Line 2) Z(i), IDW(i) Number of electrons
C                                       followed by atomic number
C  (...            Line 3) a1,b1,a2
C  (...            Line 4) b2,a3,b3
C  (...            Line 5) a4,b4,c        atomic scattering factors
C  (...            Line 6) Cu_f', Cu_f''  dispersion corrections
C  (...            Line 7) Mo_f', Mo_f''  dispersion corrections
C
C---- Values for 2 gauss approximation have a3,b3,a4,b4,c = 0.0
C     and are distinguished by having ATOMID(i)(6:6) = '2'
C     only values for H, C, N, O, and S are stored
C
C
       IMPLICIT NONE
C
C
C     .. Parameters ..
      INTEGER NUMATM
      PARAMETER (NUMATM=216)
C     ..
C     .. Scalar Arguments ..
      REAL ANOM,WAVE,ZZ
      INTEGER IATNUM,IFLAG,ISHOW,LUNOUT,NG
      LOGICAL LELEC
      CHARACTER ID*4
C     ..
C     .. Array Arguments ..
      REAL SF(9)
C     ..
C     .. Local Scalars ..
      INTEGER JNUM,LNATID
      CHARACTER SPACE*1,ID2*6
C     ..
C     .. Local Arrays ..
      REAL A1(NUMATM),A2(NUMATM),A3(NUMATM),A4(NUMATM),B1(NUMATM),
     +     B2(NUMATM),B3(NUMATM),B4(NUMATM),C(NUMATM),CUF1(NUMATM),
     +     CUF11(NUMATM),MOF1(NUMATM),MOF11(NUMATM),Z(NUMATM)
      INTEGER IDW(NUMATM)
      CHARACTER ATOMID(NUMATM)*6
C     ..
C     .. Data statements ..
C
      DATA SPACE/' '/
C
      DATA ATOMID(1)/'H     '/,IDW(1)/1/,Z(1)/1.0/,CUF1(1)/0.0/,
     +     CUF11(1)/0.0/,MOF1(1)/0.0/,MOF11(1)/0.0/,A1(1)/0.493002/,
     +     B1(1)/10.5109/,A2(1)/0.322912/,B2(1)/26.1257/,
     +     A3(1)/0.140191/,B3(1)/3.14236/,A4(1)/0.04081/,B4(1)/57.7997/,
     +     C(1)/0.003038/
C
      DATA ATOMID(2)/'H-1   '/,IDW(2)/1/,Z(2)/2.0/,CUF1(2)/0.0/,
     +     CUF11(2)/0.0/,MOF1(2)/0.0/,MOF11(2)/0.0/,A1(2)/0.897661/,
     +     B1(2)/53.1368/,A2(2)/0.565616/,B2(2)/15.187/,A3(2)/0.415815/,
     +     B3(2)/186.576/,A4(2)/0.116973/,B4(2)/3.56709/,C(2)/0.002389/
C
      DATA ATOMID(3)/'He    '/,IDW(3)/2/,Z(3)/2.0/,CUF1(3)/0.0/,
     +     CUF11(3)/0.0/,MOF1(3)/0.0/,MOF11(3)/0.0/,A1(3)/0.8734/,
     +     B1(3)/9.1037/,A2(3)/0.6309/,B2(3)/3.3568/,A3(3)/0.3112/,
     +     B3(3)/22.9276/,A4(3)/0.178/,B4(3)/0.9821/,C(3)/0.0064/
C
      DATA ATOMID(4)/'Li    '/,IDW(4)/3/,Z(4)/3.0/,CUF1(4)/0.001/,
     +     CUF11(4)/0.0/,MOF1(4)/0.0/,MOF11(4)/0.0/,A1(4)/1.1282/,
     +     B1(4)/3.9546/,A2(4)/0.7508/,B2(4)/1.0524/,A3(4)/0.6175/,
     +     B3(4)/85.3905/,A4(4)/0.4653/,B4(4)/168.261/,C(4)/0.0377/
C
      DATA ATOMID(5)/'Li+1  '/,IDW(5)/3/,Z(5)/3.0/,CUF1(5)/0.001/,
     +     CUF11(5)/0.0/,MOF1(5)/0.0/,MOF11(5)/0.0/,A1(5)/0.6968/,
     +     B1(5)/4.6237/,A2(5)/0.7888/,B2(5)/1.9557/,A3(5)/0.3414/,
     +     B3(5)/0.6316/,A4(5)/0.1563/,B4(5)/10.0953/,C(5)/0.0167/
C
      DATA ATOMID(6)/'Be    '/,IDW(6)/4/,Z(6)/4.0/,CUF1(6)/0.003/,
     +     CUF11(6)/0.001/,MOF1(6)/0.0/,MOF11(6)/0.0/,A1(6)/1.5919/,
     +     B1(6)/43.6427/,A2(6)/1.1278/,B2(6)/1.8623/,A3(6)/0.5391/,
     +     B3(6)/103.483/,A4(6)/0.7029/,B4(6)/0.542/,C(6)/0.0385/
C
      DATA ATOMID(7)/'Be+2  '/,IDW(7)/4/,Z(7)/2.0/,CUF1(7)/0.003/,
     +     CUF11(7)/0.001/,MOF1(7)/0.0/,MOF11(7)/0.0/,A1(7)/6.2603/,
     +     B1(7)/0.0027/,A2(7)/0.8849/,B2(7)/0.8313/,A3(7)/0.7993/,
     +     B3(7)/2.2758/,A4(7)/0.1647/,B4(7)/5.1146/,C(7)/-6.1092/
C
      DATA ATOMID(8)/'B     '/,IDW(8)/5/,Z(8)/5.0/,CUF1(8)/0.008/,
     +     CUF11(8)/0.004/,MOF1(8)/0.0/,MOF11(8)/0.001/,A1(8)/2.0545/,
     +     B1(8)/23.2185/,A2(8)/1.3326/,B2(8)/1.021/,A3(8)/1.0979/,
     +     B3(8)/60.3498/,A4(8)/0.7068/,B4(8)/0.1403/,C(8)/-0.1932/
C
      DATA ATOMID(9)/'C     '/,IDW(9)/6/,Z(9)/6.0/,CUF1(9)/0.017/,
     +     CUF11(9)/0.009/,MOF1(9)/0.002/,MOF11(9)/0.002/,A1(9)/2.31/,
     +     B1(9)/20.8439/,A2(9)/1.02/,B2(9)/10.2075/,A3(9)/1.5886/,
     +     B3(9)/0.5687/,A4(9)/0.865/,B4(9)/51.6512/,C(9)/0.2156/
C
      DATA ATOMID(10)/'Cv    '/,IDW(10)/6/,Z(10)/6.0/,CUF1(10)/0.017/,
     +     CUF11(10)/0.009/,MOF1(10)/0.002/,MOF11(10)/0.002/,
     +     A1(10)/2.26069/,B1(10)/22.6907/,A2(10)/1.56165/,
     +     B2(10)/0.656665/,A3(10)/1.05075/,B3(10)/9.75618/,
     +     A4(10)/0.839259/,B4(10)/55.5949/,C(10)/0.286977/
C
      DATA ATOMID(11)/'N     '/,IDW(11)/7/,Z(11)/7.0/,CUF1(11)/0.029/,
     +     CUF11(11)/0.018/,MOF1(11)/0.004/,MOF11(11)/0.003/,
     +     A1(11)/12.2126/,B1(11)/0.0057/,A2(11)/3.1322/,B2(11)/9.8933/,
     +     A3(11)/2.0125/,B3(11)/28.9975/,A4(11)/1.1663/,B4(11)/0.5826/,
     +     C(11)/-11.529/
C
      DATA ATOMID(12)/'O     '/,IDW(12)/8/,Z(12)/8.0/,CUF1(12)/0.047/,
     +     CUF11(12)/0.032/,MOF1(12)/0.008/,MOF11(12)/0.006/,
     +     A1(12)/3.0485/,B1(12)/13.2771/,A2(12)/2.2868/,B2(12)/5.7011/,
     +     A3(12)/1.5463/,B3(12)/0.3239/,A4(12)/0.867/,B4(12)/32.9089/,
     +     C(12)/0.2508/
C
      DATA ATOMID(13)/'O-1   '/,IDW(13)/8/,Z(13)/9.0/,CUF1(13)/0.047/,
     +     CUF11(13)/0.032/,MOF1(13)/0.008/,MOF11(13)/0.006/,
     +     A1(13)/4.1916/,B1(13)/12.8573/,A2(13)/1.63969/,
     +     B2(13)/4.17236/,A3(13)/1.52673/,B3(13)/47.0179/,
     +     A4(13)/-20.307/,B4(13)/-0.01404/,C(13)/21.9412/
C
      DATA ATOMID(14)/'F     '/,IDW(14)/9/,Z(14)/9.0/,CUF1(14)/0.069/,
     +     CUF11(14)/0.053/,MOF1(14)/0.014/,MOF11(14)/0.010/,
     +     A1(14)/3.5392/,B1(14)/10.2825/,A2(14)/2.6412/,B2(14)/4.2944/,
     +     A3(14)/1.517/,B3(14)/0.2615/,A4(14)/1.0243/,B4(14)/26.1476/,
     +     C(14)/0.2776/
C
      DATA ATOMID(15)/'F-1   '/,IDW(15)/9/,Z(15)/10.0/,CUF1(15)/0.069/,
     +     CUF11(15)/0.053/,MOF1(15)/0.014/,MOF11(15)/0.010/,
     +     A1(15)/3.6322/,B1(15)/5.27756/,A2(15)/3.51057/,
     +     B2(15)/14.7353/,A3(15)/1.26064/,B3(15)/0.442258/,
     +     A4(15)/0.940706/,B4(15)/47.3437/,C(15)/0.653396/
C
      DATA ATOMID(16)/'Ne    '/,IDW(16)/10/,Z(16)/10.0/,CUF1(16)/0.097/,
     +     CUF11(16)/0.083/,MOF1(16)/0.021/,MOF11(16)/0.016/,
     +     A1(16)/3.9553/,B1(16)/8.4042/,A2(16)/3.1125/,B2(16)/3.4262/,
     +     A3(16)/1.4546/,B3(16)/0.2306/,A4(16)/1.1251/,B4(16)/21.7184/,
     +     C(16)/0.3515/
C
      DATA ATOMID(17)/'Na    '/,IDW(17)/11/,Z(17)/11.0/,
     +     CUF1(17)/0.1290/,CUF11(17)/0.1240/,MOF1(17)/0.030/,
     +     MOF11(17)/0.025/,A1(17)/4.7626/,B1(17)/3.285/,A2(17)/3.1736/,
     +     B2(17)/8.8422/,A3(17)/1.2674/,B3(17)/0.3136/,A4(17)/1.1128/,
     +     B4(17)/129.424/,C(17)/0.676/
C
      DATA ATOMID(18)/'Na+1  '/,IDW(18)/11/,Z(18)/10.0/,
     +     CUF1(18)/0.1290/,CUF11(18)/0.1240/,MOF1(18)/0.030/,
     +     MOF11(18)/0.025/,A1(18)/3.2565/,B1(18)/2.6671/,
     +     A2(18)/3.9362/,B2(18)/6.1153/,A3(18)/1.3998/,B3(18)/0.2001/,
     +     A4(18)/1.0032/,B4(18)/14.039/,C(18)/0.404/
C
      DATA ATOMID(19)/'Mg    '/,IDW(19)/12/,Z(19)/12.0/,
     +     CUF1(19)/0.1650/,CUF11(19)/0.1770/,MOF1(19)/0.042/,
     +     MOF11(19)/0.036/,A1(19)/5.4204/,B1(19)/2.8275/,
     +     A2(19)/2.1735/,B2(19)/79.2611/,A3(19)/1.2269/,B3(19)/0.3808/,
     +     A4(19)/2.3073/,B4(19)/7.1937/,C(19)/0.8584/
C
      DATA ATOMID(20)/'Mg+2  '/,IDW(20)/12/,Z(20)/10.0/,
     +     CUF1(20)/0.1650/,CUF11(20)/0.1770/,MOF1(20)/0.042/,
     +     MOF11(20)/0.036/,A1(20)/3.4988/,B1(20)/2.1676/,
     +     A2(20)/3.8378/,B2(20)/4.7542/,A3(20)/1.3284/,B3(20)/0.185/,
     +     A4(20)/0.8497/,B4(20)/10.1411/,C(20)/0.4853/
C
      DATA ATOMID(21)/'Al    '/,IDW(21)/13/,Z(21)/13.0/,
     +     CUF1(21)/0.2040/,CUF11(21)/0.2460/,MOF1(21)/0.056/,
     +     MOF11(21)/0.052/,A1(21)/6.4202/,B1(21)/3.0387/,
     +     A2(21)/1.9002/,B2(21)/0.7426/,A3(21)/1.5936/,B3(21)/31.5472/,
     +     A4(21)/1.9646/,B4(21)/85.0886/,C(21)/1.1151/
C
      DATA ATOMID(22)/'Al+3  '/,IDW(22)/13/,Z(22)/10.0/,
     +     CUF1(22)/0.2040/,CUF11(22)/0.2460/,MOF1(22)/0.056/,
     +     MOF11(22)/0.052/,A1(22)/4.17448/,B1(22)/1.93816/,
     +     A2(22)/3.3876/,B2(22)/4.14553/,A3(22)/1.20296/,
     +     B3(22)/0.228753/,A4(22)/0.528137/,B4(22)/8.28524/,
     +     C(22)/0.706786/
C
      DATA ATOMID(23)/'Si    '/,IDW(23)/14/,Z(23)/14.0/,
     +     CUF1(23)/0.2440/,CUF11(23)/0.330/,MOF1(23)/0.072/,
     +     MOF11(23)/0.071/,A1(23)/6.2915/,B1(23)/2.4386/,
     +     A2(23)/3.0353/,B2(23)/32.3337/,A3(23)/1.9891/,B3(23)/0.6785/,
     +     A4(23)/1.541/,B4(23)/81.6937/,C(23)/1.1407/
C
      DATA ATOMID(24)/'Siv   '/,IDW(24)/14/,Z(24)/14.0/,
     +     CUF1(24)/0.2440/,CUF11(24)/0.330/,MOF1(24)/0.072/,
     +     MOF11(24)/0.071/,A1(24)/5.66269/,B1(24)/2.6652/,
     +     A2(24)/3.07164/,B2(24)/38.6634/,A3(24)/2.62446/,
     +     B3(24)/0.916946/,A4(24)/1.3932/,B4(24)/93.5458/,
     +     C(24)/1.24707/
C
      DATA ATOMID(25)/'Si+4  '/,IDW(25)/14/,Z(25)/10.0/,
     +     CUF1(25)/0.2440/,CUF11(25)/0.330/,MOF1(25)/0.072/,
     +     MOF11(25)/0.071/,A1(25)/4.43918/,B1(25)/1.64167/,
     +     A2(25)/3.20345/,B2(25)/3.43757/,A3(25)/1.19453/,
     +     B3(25)/0.2149/,A4(25)/0.41653/,B4(25)/6.65365/,
     +     C(25)/0.746297/
C
      DATA ATOMID(26)/'P     '/,IDW(26)/15/,Z(26)/15.0/,
     +     CUF1(26)/0.2830/,CUF11(26)/0.4340/,MOF1(26)/0.090/,
     +     MOF11(26)/0.095/,A1(26)/6.4345/,B1(26)/1.9067/,
     +     A2(26)/4.1791/,B2(26)/27.157/,A3(26)/1.78/,B3(26)/0.526/,
     +     A4(26)/1.4908/,B4(26)/68.1645/,C(26)/1.1149/
C
      DATA ATOMID(27)/'S     '/,IDW(27)/16/,Z(27)/16.0/,
     +     CUF1(27)/0.3190/,CUF11(27)/0.5570/,MOF1(27)/0.110/,
     +     MOF11(27)/0.1240/,A1(27)/6.9053/,B1(27)/1.4679/,
     +     A2(27)/5.2034/,B2(27)/22.2151/,A3(27)/1.4379/,B3(27)/0.2536/,
     +     A4(27)/1.5863/,B4(27)/56.172/,C(27)/0.8669/
C
      DATA ATOMID(28)/'Cl    '/,IDW(28)/17/,Z(28)/17.0/,
     +     CUF1(28)/0.3480/,CUF11(28)/0.7020/,MOF1(28)/0.1320/,
     +     MOF11(28)/0.1590/,A1(28)/11.4604/,B1(28)/0.0104/,
     +     A2(28)/7.1964/,B2(28)/1.1662/,A3(28)/6.2556/,B3(28)/18.5194/,
     +     A4(28)/1.6455/,B4(28)/47.7784/,C(28)/-9.5574/
C
      DATA ATOMID(29)/'Cl-1  '/,IDW(29)/17/,Z(29)/18.0/,
     +     CUF1(29)/0.3480/,CUF11(29)/0.7020/,MOF1(29)/0.1320/,
     +     MOF11(29)/0.1590/,A1(29)/18.2915/,B1(29)/0.0066/,
     +     A2(29)/7.2084/,B2(29)/1.1717/,A3(29)/6.5337/,B3(29)/19.5424/,
     +     A4(29)/2.3386/,B4(29)/60.4486/,C(29)/-16.378/
C
      DATA ATOMID(30)/'Ar    '/,IDW(30)/18/,Z(30)/18.0/,
     +     CUF1(30)/0.3660/,CUF11(30)/0.8720/,MOF1(30)/0.1550/,
     +     MOF11(30)/0.2010/,A1(30)/7.4845/,B1(30)/0.9072/,
     +     A2(30)/6.7723/,B2(30)/14.8407/,A3(30)/0.6539/,
     +     B3(30)/43.8983/,A4(30)/1.6442/,B4(30)/33.3929/,C(30)/1.4445/
C
      DATA ATOMID(31)/'K     '/,IDW(31)/19/,Z(31)/19.0/,
     +     CUF1(31)/0.3650/,CUF11(31)/1.066/,MOF1(31)/0.1790/,
     +     MOF11(31)/0.250/,A1(31)/8.2186/,B1(31)/12.7949/,
     +     A2(31)/7.4398/,B2(31)/0.7748/,A3(31)/1.0519/,B3(31)/213.187/,
     +     A4(31)/0.8659/,B4(31)/41.6841/,C(31)/1.4228/
C
      DATA ATOMID(32)/'K+1   '/,IDW(32)/19/,Z(32)/18.0/,
     +     CUF1(32)/0.3650/,CUF11(32)/1.066/,MOF1(32)/0.1790/,
     +     MOF11(32)/0.250/,A1(32)/7.9578/,B1(32)/12.6331/,
     +     A2(32)/7.4917/,B2(32)/0.7674/,A3(32)/6.359/,B3(32)/-0.002/,
     +     A4(32)/1.1915/,B4(32)/31.9128/,C(32)/-4.9978/
C
      DATA ATOMID(33)/'Ca    '/,IDW(33)/20/,Z(33)/20.0/,
     +     CUF1(33)/0.3410/,CUF11(33)/1.2860/,MOF1(33)/0.2030/,
     +     MOF11(33)/0.3060/,A1(33)/8.6266/,B1(33)/10.4421/,
     +     A2(33)/7.3873/,B2(33)/0.6599/,A3(33)/1.5899/,B3(33)/85.7484/,
     +     A4(33)/1.0211/,B4(33)/178.437/,C(33)/1.3751/
C
      DATA ATOMID(34)/'Ca+2  '/,IDW(34)/20/,Z(34)/18.0/,
     +     CUF1(34)/0.3410/,CUF11(34)/1.286/,MOF1(34)/0.2030/,
     +     MOF11(34)/0.3060/,A1(34)/15.6348/,B1(34)/-0.0074/,
     +     A2(34)/7.9518/,B2(34)/0.6089/,A3(34)/8.4372/,B3(34)/10.3116/,
     +     A4(34)/0.8537/,B4(34)/25.9905/,C(34)/-14.875/
C
      DATA ATOMID(35)/'Sc    '/,IDW(35)/21/,Z(35)/21.0/,
     +     CUF1(35)/0.2850/,CUF11(35)/1.533/,MOF1(35)/0.2260/,
     +     MOF11(35)/0.3720/,A1(35)/9.189/,B1(35)/9.0213/,
     +     A2(35)/7.3679/,B2(35)/0.5729/,A3(35)/1.6409/,B3(35)/136.108/,
     +     A4(35)/1.468/,B4(35)/51.3531/,C(35)/1.3329/
C
      DATA ATOMID(36)/'Sc+3  '/,IDW(36)/21/,Z(36)/18.0/,
     +     CUF1(36)/0.2850/,CUF11(36)/1.533/,MOF1(36)/0.2260/,
     +     MOF11(36)/0.3720/,A1(36)/14.4008/,B1(36)/0.29854/,
     +     A2(36)/8.0273/,B2(36)/7.9629/,A3(36)/1.65943/,
     +     B3(36)/-0.28604/,A4(36)/1.57936/,B4(36)/16.0662/,
     +     C(36)/-6.6667/
C
      DATA ATOMID(37)/'Ti    '/,IDW(37)/22/,Z(37)/22.0/,
     +     CUF1(37)/0.1890/,CUF11(37)/1.807/,MOF1(37)/0.2480/,
     +     MOF11(37)/0.4460/,A1(37)/9.7595/,B1(37)/7.8508/,
     +     A2(37)/7.3558/,B2(37)/0.5/,A3(37)/1.6991/,B3(37)/35.6338/,
     +     A4(37)/1.9021/,B4(37)/116.105/,C(37)/1.2807/
C
      DATA ATOMID(38)/'Ti+2  '/,IDW(38)/22/,Z(38)/20.0/,
     +     CUF1(38)/0.1890/,CUF11(38)/1.807/,MOF1(38)/0.2480/,
     +     MOF11(38)/0.4460/,A1(38)/9.11423/,B1(38)/7.5243/,
     +     A2(38)/7.62174/,B2(38)/0.457585/,A3(38)/2.2793/,
     +     B3(38)/19.5361/,A4(38)/0.087899/,B4(38)/61.6558/,
     +     C(38)/0.897155/
C
      DATA ATOMID(39)/'Ti+3  '/,IDW(39)/22/,Z(39)/19.0/,
     +     CUF1(39)/0.1890/,CUF11(39)/1.807/,MOF1(39)/0.2480/,
     +     MOF11(39)/0.4460/,A1(39)/17.7344/,B1(39)/0.22061/,
     +     A2(39)/8.73816/,B2(39)/7.04716/,A3(39)/5.25691/,
     +     B3(39)/-0.15762/,A4(39)/1.92134/,B4(39)/15.9768/,
     +     C(39)/-14.652/
C
      DATA ATOMID(40)/'Ti+4  '/,IDW(40)/22/,Z(40)/18.0/,
     +     CUF1(40)/0.1890/,CUF11(40)/1.807/,MOF1(40)/0.2480/,
     +     MOF11(40)/0.4460/,A1(40)/19.5114/,B1(40)/0.178847/,
     +     A2(40)/8.23473/,B2(40)/6.67018/,A3(40)/2.01341/,
     +     B3(40)/-0.29263/,A4(40)/1.5208/,B4(40)/12.9464/,C(40)/-13.28/
C
      DATA ATOMID(41)/'V     '/,IDW(41)/23/,Z(41)/23.0/,CUF1(41)/0.035/,
     +     CUF11(41)/2.11/,MOF1(41)/0.2670/,MOF11(41)/0.530/,
     +     A1(41)/10.2971/,B1(41)/6.8657/,A2(41)/7.3511/,B2(41)/0.4385/,
     +     A3(41)/2.0703/,B3(41)/26.8938/,A4(41)/2.0571/,
     +     B4(41)/102.478/,C(41)/1.2199/
C
      DATA ATOMID(42)/'V+2   '/,IDW(42)/23/,Z(42)/21.0/,CUF1(42)/0.035/,
     +     CUF11(42)/2.11/,MOF1(42)/0.2670/,MOF11(42)/0.530/,
     +     A1(42)/10.106/,B1(42)/6.8818/,A2(42)/7.3541/,B2(42)/0.4409/,
     +     A3(42)/2.2884/,B3(42)/20.3004/,A4(42)/0.0223/,
     +     B4(42)/115.122/,C(42)/1.2298/
C
      DATA ATOMID(43)/'V+3   '/,IDW(43)/23/,Z(43)/19.0/,CUF1(43)/0.035/,
     +     CUF11(43)/2.11/,MOF1(43)/0.2670/,MOF11(43)/0.530/,
     +     A1(43)/9.43141/,B1(43)/6.39535/,A2(43)/7.7419/,
     +     B2(43)/0.383349/,A3(43)/2.15343/,B3(42)/15.1908/,
     +     A4(43)/0.016865/,B4(43)/63.969/,C(43)/0.656565/
C
      DATA ATOMID(44)/'V+5   '/,IDW(44)/23/,Z(44)/18.0/,CUF1(44)/0.035/,
     +     CUF11(44)/2.11/,MOF1(44)/0.2670/,MOF11(44)/0.530/,
     +     A1(44)/15.6887/,B1(44)/0.679003/,A2(44)/8.14208/,
     +     B2(44)/5.40135/,A3(44)/2.03081/,B3(44)/9.97278/,
     +     A4(44)/-9.576/,B4(44)/0.940464/,C(44)/1.7143/
C
      DATA ATOMID(45)/'Cr    '/,IDW(45)/24/,Z(45)/24.0/,
     +     CUF1(45)/-0.1980/,CUF11(45)/2.443/,MOF1(45)/0.2840/,
     +     MOF11(45)/0.6240/,A1(45)/10.6406/,B1(45)/6.1038/,
     +     A2(45)/7.3537/,B2(45)/0.392/,A3(45)/3.324/,B3(45)/20.2626/,
     +     A4(45)/1.4922/,B4(45)/98.7399/,C(45)/1.1832/
C
      DATA ATOMID(46)/'Cr+2  '/,IDW(46)/24/,Z(46)/22.0/,
     +     CUF1(46)/-0.1980/,CUF11(46)/2.443/,MOF1(46)/0.2840/,
     +     MOF11(46)/0.6240/,A1(46)/9.54034/,B1(46)/5.66078/,
     +     A2(46)/7.7509/,B2(46)/0.344261/,A3(46)/3.58274/,
     +     B3(46)/13.3075/,A4(46)/0.509107/,B4(46)/32.4224/,
     +     C(46)/0.616898/
C
      DATA ATOMID(47)/'Cr+3  '/,IDW(47)/24/,Z(47)/21.0/,
     +     CUF1(47)/-0.1980/,CUF11(47)/2.443/,MOF1(47)/0.2840/,
     +     MOF11(47)/0.6240/,A1(47)/9.6809/,B1(47)/5.59463/,
     +     A2(47)/7.81136/,B2(47)/0.334393/,A3(47)/2.87603/,
     +     B3(47)/12.8288/,A4(47)/0.113575/,B4(47)/32.8761/,
     +     C(47)/0.518275/
C
      DATA ATOMID(48)/'Mn    '/,IDW(48)/25/,Z(48)/25.0/,
     +     CUF1(48)/-0.5680/,CUF11(48)/2.808/,MOF1(48)/0.2950/,
     +     MOF11(48)/0.7290/,A1(48)/11.2819/,B1(48)/5.3409/,
     +     A2(48)/7.3573/,B2(48)/0.3432/,A3(48)/3.0193/,B3(48)/17.8674/,
     +     A4(48)/2.2441/,B4(48)/83.7543/,C(48)/1.0896/
C
      DATA ATOMID(49)/'Mn+2  '/,IDW(49)/25/,Z(49)/23.0/,
     +     CUF1(49)/-0.5680/,CUF11(49)/2.808/,MOF1(49)/0.2950/,
     +     MOF11(49)/0.7290/,A1(49)/10.8061/,B1(49)/5.2796/,
     +     A2(49)/7.362/,B2(49)/0.3435/,A3(49)/3.5268/,B3(49)/14.343/,
     +     A4(49)/0.2184/,B4(49)/41.3235/,C(49)/1.0874/
C
      DATA ATOMID(50)/'Mn+3  '/,IDW(50)/25/,Z(50)/22.0/,
     +     CUF1(50)/-0.5680/,CUF11(50)/2.808/,MOF1(50)/0.2950/,
     +     MOF11(50)/0.7290/,A1(50)/9.84521/,B1(50)/4.91797/,
     +     A2(50)/7.87194/,B2(50)/0.294393/,A3(50)/3.56531/,
     +     B3(50)/10.8171/,A4(50)/0.323613/,B4(50)/24.1281/,
     +     C(50)/0.393974/
C
      DATA ATOMID(51)/'Mn+4  '/,IDW(51)/25/,Z(51)/21.0/,
     +     CUF1(51)/-0.5680/,CUF11(51)/2.808/,MOF1(51)/0.2950/,
     +     MOF11(51)/0.7290/,A1(51)/9.96253/,B1(51)/4.8485/,
     +     A2(51)/7.97057/,B2(51)/0.283303/,A3(51)/2.76067/,
     +     B3(51)/10.4852/,A4(51)/0.054447/,B4(51)/27.573/,
     +     C(51)/0.251877/
C
      DATA ATOMID(52)/'Fe    '/,IDW(52)/26/,Z(52)/26.0/,
     +     CUF1(52)/-1.179/,CUF11(52)/3.204/,MOF1(52)/0.3010/,
     +     MOF11(52)/0.8450/,A1(52)/11.7695/,B1(52)/4.7611/,
     +     A2(52)/7.3573/,B2(52)/0.3072/,A3(52)/3.5222/,B3(52)/15.3535/,
     +     A4(52)/2.3045/,B4(52)/76.8805/,C(52)/1.0369/
C
      DATA ATOMID(53)/'Fe+2  '/,IDW(53)/26/,Z(53)/24.0/,
     +     CUF1(53)/-1.179/,CUF11(53)/3.204/,MOF1(53)/0.3010/,
     +     MOF11(53)/0.8450/,A1(53)/11.0424/,B1(53)/4.6538/,
     +     A2(53)/7.374/,B2(53)/0.3053/,A3(53)/4.1346/,B3(53)/12.0546/,
     +     A4(53)/0.4399/,B4(53)/31.2809/,C(53)/1.0097/
C
      DATA ATOMID(54)/'Fe+3  '/,IDW(54)/26/,Z(54)/23.0/,
     +     CUF1(54)/-1.179/,CUF11(54)/3.204/,MOF1(54)/0.3010/,
     +     MOF11(54)/0.8450/,A1(54)/11.1764/,B1(54)/4.6147/,
     +     A2(54)/7.3863/,B2(54)/0.3005/,A3(54)/3.3948/,B3(54)/11.6729/,
     +     A4(54)/0.0724/,B4(54)/38.5566/,C(54)/0.9707/
C
      DATA ATOMID(55)/'Co    '/,IDW(55)/27/,Z(55)/27.0/,
     +     CUF1(55)/-2.464/,CUF11(55)/3.608/,MOF1(55)/0.2990/,
     +     MOF11(55)/0.9730/,A1(55)/12.2841/,B1(55)/4.2791/,
     +     A2(55)/7.3409/,B2(55)/0.2784/,A3(55)/4.0034/,B3(55)/13.5359/,
     +     A4(55)/2.3488/,B4(55)/71.1692/,C(55)/1.0118/
C
      DATA ATOMID(56)/'Co+2  '/,IDW(56)/27/,Z(56)/25.0/,
     +     CUF1(56)/-2.464/,CUF11(56)/3.608/,MOF1(56)/0.2990/,
     +     MOF11(56)/0.9730/,A1(56)/11.2296/,B1(56)/4.1231/,
     +     A2(56)/7.3883/,B2(56)/0.2726/,A3(56)/4.7393/,B3(56)/10.2443/,
     +     A4(56)/0.7108/,B4(56)/25.6466/,C(56)/0.9324/
C
      DATA ATOMID(57)/'Co+3  '/,IDW(57)/27/,Z(57)/24.0/,
     +     CUF1(57)/-2.464/,CUF11(57)/3.608/,MOF1(57)/0.2990/,
     +     MOF11(57)/0.9730/,A1(57)/10.338/,B1(57)/3.90969/,
     +     A2(57)/7.88173/,B2(57)/0.238668/,A3(57)/4.76795/,
     +     B3(57)/8.35583/,A4(57)/0.725591/,B4(57)/18.3491/,
     +     C(57)/0.286667/
C
      DATA ATOMID(58)/'Ni    '/,IDW(58)/28/,Z(58)/28.0/,
     +     CUF1(58)/-2.956/,CUF11(58)/0.509/,MOF1(58)/0.2850/,
     +     MOF11(58)/1.113/,A1(58)/12.8376/,B1(58)/3.8785/,
     +     A2(58)/7.292/,B2(58)/0.2565/,A3(58)/4.4438/,B3(58)/12.1763/,
     +     A4(58)/2.38/,B4(58)/66.3421/,C(58)/1.0341/
C
      DATA ATOMID(59)/'Ni+2  '/,IDW(59)/28/,Z(59)/26.0/,
     +     CUF1(59)/-2.956/,CUF11(59)/0.5090/,MOF1(59)/0.2850/,
     +     MOF11(59)/1.113/,A1(59)/11.4166/,B1(59)/3.6766/,
     +     A2(59)/7.4005/,B2(59)/0.2449/,A3(59)/5.3442/,B3(59)/8.873/,
     +     A4(59)/0.9773/,B4(59)/22.1626/,C(59)/0.8614/
C
      DATA ATOMID(60)/'Ni+3  '/,IDW(60)/28/,Z(60)/25.0/,
     +     CUF1(60)/-2.956/,CUF11(60)/0.5090/,MOF1(60)/0.2850/,
     +     MOF11(60)/1.113/,A1(60)/10.7806/,B1(60)/3.5477/,
     +     A2(60)/7.75868/,B2(60)/0.22314/,A3(60)/5.22746/,
     +     B3(60)/7.64468/,A4(60)/0.847114/,B4(60)/16.9673/,
     +     C(60)/0.386044/
C
      DATA ATOMID(61)/'Cu    '/,IDW(61)/29/,Z(61)/29.0/,
     +     CUF1(61)/-2.019/,CUF11(61)/0.5890/,MOF1(61)/0.2630/,
     +     MOF11(61)/1.266/,A1(61)/13.338/,B1(61)/3.5828/,
     +     A2(61)/7.1676/,B2(61)/0.247/,A3(61)/5.6158/,B3(61)/11.3966/,
     +     A4(61)/1.6735/,B4(61)/64.8126/,C(61)/1.191/
C
      DATA ATOMID(62)/'Cu+1  '/,IDW(62)/29/,Z(62)/28.0/,
     +     CUF1(62)/-2.019/,CUF11(62)/0.5890/,MOF1(62)/0.2630/,
     +     MOF11(62)/1.266/,A1(62)/11.9475/,B1(62)/3.3669/,
     +     A2(62)/7.3573/,B2(62)/0.2274/,A3(62)/6.2455/,B3(62)/8.6625/,
     +     A4(62)/1.5578/,B4(62)/25.8487/,C(62)/0.89/
C
      DATA ATOMID(63)/'Cu+2  '/,IDW(63)/29/,Z(63)/27.0/,
     +     CUF1(63)/-2.019/,CUF11(63)/0.5890/,MOF1(63)/0.2630/,
     +     MOF11(63)/1.266/,A1(63)/11.8168/,B1(63)/3.37484/,
     +     A2(63)/7.11181/,B2(63)/0.244078/,A3(63)/5.78135/,
     +     B3(63)/7.9876/,A4(63)/1.14523/,B4(63)/19.897/,C(63)/1.14431/
C
      DATA ATOMID(64)/'Zn    '/,IDW(64)/30/,Z(64)/30.0/,
     +     CUF1(64)/-1.612/,CUF11(64)/0.6780/,MOF1(64)/0.2220/,
     +     MOF11(64)/1.431/,A1(64)/14.0743/,B1(64)/3.2655/,
     +     A2(64)/7.0318/,B2(64)/0.2333/,A3(64)/5.1625/,B3(64)/10.3163/,
     +     A4(64)/2.41/,B4(64)/58.7097/,C(64)/1.3041/
C
      DATA ATOMID(65)/'Zn+2  '/,IDW(65)/30/,Z(65)/28.0/,
     +     CUF1(65)/-1.612/,CUF11(65)/0.6780/,MOF1(65)/0.2220/,
     +     MOF11(65)/1.431/,A1(65)/11.9719/,B1(65)/2.9946/,
     +     A2(65)/7.3862/,B2(65)/0.2031/,A3(65)/6.4668/,B3(65)/7.0826/,
     +     A4(65)/1.394/,B4(65)/18.0995/,C(65)/0.7807/
C
      DATA ATOMID(66)/'Ga    '/,IDW(66)/31/,Z(66)/31.0/,
     +     CUF1(66)/-1.354/,CUF11(66)/0.7770/,MOF1(66)/0.1630/,
     +     MOF11(66)/1.609/,A1(66)/15.2354/,B1(66)/3.0669/,
     +     A2(66)/6.7006/,B2(66)/0.2412/,A3(66)/4.3591/,B3(66)/10.7805/,
     +     A4(66)/2.9623/,B4(66)/61.4135/,C(66)/1.7189/
C
      DATA ATOMID(67)/'Ga+3  '/,IDW(67)/31/,Z(67)/28.0/,
     +     CUF1(67)/-1.354/,CUF11(67)/0.7770/,MOF1(67)/0.1630/,
     +     MOF11(67)/1.609/,A1(67)/12.692/,B1(67)/2.81262/,
     +     A2(67)/6.69883/,B2(67)/0.22789/,A3(67)/6.06692/,
     +     B3(67)/6.36441/,A4(67)/1.0066/,B4(67)/14.4122/,C(67)/1.53545/
C
      DATA ATOMID(68)/'Ge    '/,IDW(68)/32/,Z(68)/32.0/,
     +     CUF1(68)/-1.163/,CUF11(68)/0.8860/,MOF1(68)/0.081/,
     +     MOF11(68)/1.801/,A1(68)/16.0816/,B1(68)/2.8509/,
     +     A2(68)/6.3747/,B2(68)/0.2516/,A3(68)/3.7068/,B3(68)/11.4468/,
     +     A4(68)/3.683/,B4(68)/54.7625/,C(68)/2.1313/
C
      DATA ATOMID(69)/'Ge+4  '/,IDW(69)/32/,Z(69)/28.0/,
     +     CUF1(69)/-1.163/,CUF11(69)/0.8860/,MOF1(69)/0.081/,
     +     MOF11(69)/1.801/,A1(69)/12.9172/,B1(69)/2.53718/,
     +     A2(69)/6.70003/,B2(69)/0.205855/,A3(69)/6.06791/,
     +     B3(69)/5.47913/,A4(69)/0.859041/,B4(69)/11.603/,
     +     C(69)/1.45572/
C
      DATA ATOMID(70)/'As    '/,IDW(70)/33/,Z(70)/33.0/,
     +     CUF1(70)/-1.011/,CUF11(70)/1.006/,MOF1(70)/-0.030/,
     +     MOF11(70)/2.007/,A1(70)/16.6723/,B1(70)/2.6345/,
     +     A2(70)/6.0701/,B2(70)/0.2647/,A3(70)/3.4313/,B3(70)/12.9479/,
     +     A4(70)/4.2779/,B4(70)/47.7972/,C(70)/2.531/
C
      DATA ATOMID(71)/'Se    '/,IDW(71)/34/,Z(71)/34.0/,
     +     CUF1(71)/-0.8790/,CUF11(71)/1.139/,MOF1(71)/-0.1780/,
     +     MOF11(71)/2.223/,A1(71)/17.0006/,B1(71)/2.4098/,
     +     A2(71)/5.8196/,B2(71)/0.2726/,A3(71)/3.9731/,B3(71)/15.2372/,
     +     A4(71)/4.3543/,B4(71)/43.8163/,C(71)/2.8409/
C
      DATA ATOMID(72)/'Br    '/,IDW(72)/35/,Z(72)/35.0/,
     +     CUF1(72)/-0.7670/,CUF11(72)/1.283/,MOF1(72)/-0.3740/,
     +     MOF11(72)/2.456/,A1(72)/17.1789/,B1(72)/2.1723/,
     +     A2(72)/5.2358/,B2(72)/16.5796/,A3(72)/5.6377/,B3(72)/0.2609/,
     +     A4(72)/3.9851/,B4(72)/41.4328/,C(72)/2.9557/
C
      DATA ATOMID(73)/'Br-1  '/,IDW(73)/35/,Z(73)/36.0/,
     +     CUF1(73)/-0.7670/,CUF11(73)/1.283/,MOF1(73)/-0.3740/,
     +     MOF11(73)/2.456/,A1(73)/17.1718/,B1(73)/2.2059/,
     +     A2(73)/6.3338/,B2(73)/19.3345/,A3(73)/5.5754/,B3(73)/0.2871/,
     +     A4(73)/3.7272/,B4(73)/58.1535/,C(73)/3.1776/
C
      DATA ATOMID(74)/'Kr    '/,IDW(74)/36/,Z(74)/36.0/,
     +     CUF1(74)/-0.6650/,CUF11(74)/1.439/,MOF1(74)/-0.6520/,
     +     MOF11(74)/2.713/,A1(74)/17.3555/,B1(74)/1.9384/,
     +     A2(74)/6.7286/,B2(74)/16.5623/,A3(74)/5.5493/,B3(74)/0.2261/,
     +     A4(74)/3.5375/,B4(74)/39.3972/,C(74)/2.825/
C
      DATA ATOMID(75)/'Rb    '/,IDW(75)/37/,Z(75)/37.0/,
     +     CUF1(75)/-0.5740/,CUF11(75)/1.608/,MOF1(75)/-1.044/,
     +     MOF11(75)/2.973/,A1(75)/17.1784/,B1(75)/1.7888/,
     +     A2(75)/9.6435/,B2(75)/17.3151/,A3(75)/5.1399/,B3(75)/0.2748/,
     +     A4(75)/1.5292/,B4(75)/164.934/,C(75)/3.4873/
C
      DATA ATOMID(76)/'Rb+1  '/,IDW(76)/37/,Z(76)/36.0/,
     +     CUF1(76)/-0.5740/,CUF11(76)/1.608/,MOF1(76)/-1.044/,
     +     MOF11(76)/2.973/,A1(76)/17.5816/,B1(76)/1.7139/,
     +     A2(76)/7.6598/,B2(76)/14.7957/,A3(76)/5.8981/,B3(76)/0.1603/,
     +     A4(76)/2.7817/,B4(76)/31.2087/,C(76)/2.0782/
C
      DATA ATOMID(77)/'Sr    '/,IDW(77)/38/,Z(77)/38.0/,
     +     CUF1(77)/-0.4650/,CUF11(77)/1.82/,MOF1(77)/-1.657/,
     +     MOF11(77)/3.264/,A1(77)/17.5663/,B1(77)/1.5564/,
     +     A2(77)/9.8184/,B2(77)/14.0988/,A3(77)/5.422/,B3(77)/0.1664/,
     +     A4(77)/2.6694/,B4(77)/132.376/,C(77)/2.5064/
C
      DATA ATOMID(78)/'Sr+2  '/,IDW(78)/38/,Z(78)/36.0/,
     +     CUF1(78)/-0.4650/,CUF11(78)/1.82/,MOF1(78)/-1.657/,
     +     MOF11(78)/3.264/,A1(78)/18.0874/,B1(78)/1.4907/,
     +     A2(78)/8.1373/,B2(78)/12.6963/,A3(78)/2.5654/,
     +     B3(78)/24.5651/,A4(78)/-34.193/,B4(78)/-0.0138/,
     +     C(78)/41.4025/
C
      DATA ATOMID(79)/'Y     '/,IDW(79)/39/,Z(79)/39.0/,
     +     CUF1(79)/-0.3860/,CUF11(79)/2.025/,MOF1(79)/-2.951/,
     +     MOF11(79)/3.542/,A1(79)/17.776/,B1(79)/1.4029/,
     +     A2(79)/10.2946/,B2(79)/12.8006/,A3(79)/5.72629/,
     +     B3(79)/0.125599/,A4(79)/3.26588/,B4(79)/104.354/,
     +     C(79)/1.91213/
C
      DATA ATOMID(80)/'Y+3   '/,IDW(80)/39/,Z(80)/36.0/,
     +     CUF1(80)/-0.3860/,CUF11(80)/2.025/,MOF1(80)/-2.951/,
     +     MOF11(80)/3.542/,A1(80)/17.9268/,B1(80)/1.35417/,
     +     A2(80)/9.1531/,B2(80)/11.2145/,A3(80)/1.76795/,
     +     B3(80)/22.6599/,A4(80)/-33.108/,B4(80)/-0.01319/,
     +     C(80)/40.2602/
C
      DATA ATOMID(81)/'Zr    '/,IDW(81)/40/,Z(81)/40.0/,
     +     CUF1(81)/-0.3140/,CUF11(81)/2.245/,MOF1(81)/-2.965/,
     +     MOF11(81)/0.560/,A1(81)/17.8765/,B1(81)/1.27618/,
     +     A2(81)/10.948/,B2(81)/11.916/,A3(81)/5.41732/,
     +     B3(81)/0.117622/,A4(81)/3.65721/,B4(81)/87.6627/,
     +     C(81)/2.06929/
C
      DATA ATOMID(82)/'Zr+4  '/,IDW(82)/40/,Z(82)/36.0/,
     +     CUF1(82)/-0.3140/,CUF11(82)/2.245/,MOF1(82)/-2.965/,
     +     MOF11(82)/0.560/,A1(82)/18.1668/,B1(82)/1.2148/,
     +     A2(82)/10.0562/,B2(82)/10.1483/,A3(82)/1.01118/,
     +     B3(82)/21.6054/,A4(82)/-2.6479/,B4(82)/-0.10276/,
     +     C(82)/9.41454/
C
      DATA ATOMID(83)/'Nb    '/,IDW(83)/41/,Z(83)/41.0/,
     +     CUF1(83)/-0.2480/,CUF11(83)/2.482/,MOF1(83)/-2.197/,
     +     MOF11(83)/0.6210/,A1(83)/17.6142/,B1(83)/1.18865/,
     +     A2(83)/12.0144/,B2(83)/11.766/,A3(83)/4.04183/,
     +     B3(83)/0.204785/,A4(83)/3.53346/,B4(83)/69.7957/,
     +     C(83)/3.75591/
C
      DATA ATOMID(84)/'Nb+3  '/,IDW(84)/41/,Z(84)/38.0/,
     +     CUF1(84)/-0.2480/,CUF11(84)/2.482/,MOF1(84)/-2.197/,
     +     MOF11(84)/0.6210/,A1(84)/19.8812/,B1(84)/0.019175/,
     +     A2(84)/18.0653/,B2(84)/1.13305/,A3(84)/11.0177/,
     +     B3(84)/10.1621/,A4(84)/1.94715/,B4(84)/28.3389/,
     +     C(84)/-12.912/
C
      DATA ATOMID(85)/'Nb+5  '/,IDW(85)/41/,Z(85)/36.0/,
     +     CUF1(85)/-0.2480/,CUF11(85)/2.482/,MOF1(85)/-2.197/,
     +     MOF11(85)/0.6210/,A1(85)/17.9163/,B1(85)/1.12446/,
     +     A2(85)/13.3417/,B2(85)/0.028781/,A3(85)/10.799/,
     +     B3(85)/9.28206/,A4(85)/0.337905/,B4(85)/25.7228/,
     +     C(85)/-6.3934/
C
      DATA ATOMID(86)/'Mo    '/,IDW(86)/42/,Z(86)/42.0/,
     +     CUF1(86)/-0.1910/,CUF11(86)/2.735/,MOF1(86)/-1.825/,
     +     MOF11(86)/0.6880/,A1(86)/3.7025/,B1(86)/0.2772/,
     +     A2(86)/17.2356/,B2(86)/1.0958/,A3(86)/12.8876/,
     +     B3(86)/11.004/,A4(86)/3.7429/,B4(86)/61.6584/,C(86)/4.3875/
C
      DATA ATOMID(87)/'Mo+3  '/,IDW(87)/42/,Z(87)/39.0/,
     +     CUF1(87)/-0.1910/,CUF11(87)/2.735/,MOF1(87)/-1.825/,
     +     MOF11(87)/0.6880/,A1(87)/21.1664/,B1(87)/0.014734/,
     +     A2(87)/18.2017/,B2(87)/1.03031/,A3(87)/11.7423/,
     +     B3(87)/9.53659/,A4(87)/2.30951/,B4(87)/26.6307/,
     +     C(87)/-14.421/
C
      DATA ATOMID(88)/'Mo+5  '/,IDW(88)/42/,Z(88)/37.0/,
     +     CUF1(88)/-0.1910/,CUF11(88)/2.735/,MOF1(88)/-1.825/,
     +     MOF11(88)/0.6880/,A1(88)/21.0149/,B1(88)/0.014345/,
     +     A2(88)/18.0992/,B2(88)/1.02238/,A3(88)/11.4632/,
     +     B3(88)/8.78809/,A4(88)/0.740625/,B4(88)/23.3452/,
     +     C(88)/-14.316/
C
      DATA ATOMID(89)/'Mo+6  '/,IDW(89)/42/,Z(89)/36.0/,
     +     CUF1(89)/-0.1910/,CUF11(89)/2.735/,MOF1(89)/-1.825/,
     +     MOF11(89)/0.6880/,A1(89)/17.8871/,B1(89)/1.03649/,
     +     A2(89)/11.175/,B2(89)/8.48061/,A3(89)/6.57891/,
     +     B3(89)/0.058881/,A4(89)/0.0/,B4(89)/0.0/,C(89)/0.344941/
C
      DATA ATOMID(90)/'Tc    '/,IDW(90)/43/,Z(90)/43.0/,
     +     CUF1(90)/-0.1450/,CUF11(90)/3.005/,MOF1(90)/-0.59/,
     +     MOF11(90)/0.7590/,A1(90)/19.1301/,B1(90)/0.864132/,
     +     A2(90)/11.0948/,B2(90)/8.14487/,A3(90)/4.64901/,
     +     B3(90)/21.5707/,A4(90)/2.71263/,B4(90)/86.8472/,
     +     C(90)/5.40428/
C
      DATA ATOMID(91)/'Ru    '/,IDW(91)/44/,Z(91)/44.0/,
     +     CUF1(91)/-0.1050/,CUF11(91)/3.296/,MOF1(91)/-1.42/,
     +     MOF11(91)/0.8360/,A1(91)/19.2674/,B1(91)/0.80852/,
     +     A2(91)/12.9182/,B2(91)/8.43467/,A3(91)/4.86337/,
     +     B3(91)/24.7997/,A4(91)/1.56756/,B4(91)/94.2928/,
     +     C(91)/5.37874/
C
      DATA ATOMID(92)/'Ru+3  '/,IDW(92)/44/,Z(92)/41.0/,
     +     CUF1(92)/-0.1050/,CUF11(92)/3.296/,MOF1(92)/-1.42/,
     +     MOF11(92)/0.8360/,A1(92)/18.5638/,B1(92)/0.847329/,
     +     A2(92)/13.2885/,B2(92)/8.37164/,A3(92)/9.32602/,
     +     B3(92)/0.017662/,A4(92)/3.00964/,B4(92)/22.887/,
     +     C(92)/-3.1892/
C
      DATA ATOMID(93)/'Ru+4  '/,IDW(93)/44/,Z(93)/40.0/,
     +     CUF1(93)/-0.1050/,CUF11(93)/3.296/,MOF1(93)/-1.42/,
     +     MOF11(93)/0.8360/,A1(93)/18.5003/,B1(93)/0.844582/,
     +     A2(93)/13.1787/,B2(93)/8.12534/,A3(93)/4.71304/,
     +     B3(93)/0.36495/,A4(93)/2.18535/,B4(93)/20.8504/,
     +     C(93)/1.42357/
C
      DATA ATOMID(94)/'Rh    '/,IDW(94)/45/,Z(94)/45.0/,
     +     CUF1(94)/-0.077/,CUF11(94)/3.605/,MOF1(94)/-1.287/,
     +     MOF11(94)/0.9190/,A1(94)/19.2957/,B1(94)/0.751536/,
     +     A2(94)/14.3501/,B2(94)/8.21758/,A3(94)/4.73425/,
     +     B3(94)/25.8749/,A4(94)/1.28918/,B4(94)/98.6062/,C(94)/5.328/
C
      DATA ATOMID(95)/'Rh+3  '/,IDW(95)/45/,Z(95)/42.0/,
     +     CUF1(95)/-0.077/,CUF11(95)/3.605/,MOF1(95)/-1.287/,
     +     MOF11(95)/0.9190/,A1(95)/18.8785/,B1(95)/0.764252/,
     +     A2(95)/14.1259/,B2(95)/7.84438/,A3(95)/3.32515/,
     +     B3(95)/21.2487/,A4(95)/-6.1989/,B4(95)/-0.01036/,
     +     C(95)/11.8678/
C
      DATA ATOMID(96)/'Rh+4  '/,IDW(96)/45/,Z(96)/41.0/,
     +     CUF1(96)/-0.077/,CUF11(96)/3.605/,MOF1(96)/-1.287/,
     +     MOF11(96)/0.9190/,A1(96)/18.8545/,B1(96)/0.760825/,
     +     A2(96)/13.9806/,B2(96)/7.62436/,A3(96)/2.53464/,
     +     B3(96)/19.3317/,A4(96)/-5.6526/,B4(96)/-0.0102/,
     +     C(96)/11.2835/
C
      DATA ATOMID(97)/'Pd    '/,IDW(97)/46/,Z(97)/46.0/,
     +     CUF1(97)/-0.059/,CUF11(97)/3.934/,MOF1(97)/-1.177/,
     +     MOF11(97)/1.007/,A1(97)/19.3319/,B1(97)/0.698655/,
     +     A2(97)/15.5017/,B2(97)/7.98929/,A3(97)/5.29537/,
     +     B3(97)/25.2052/,A4(97)/0.605844/,B4(97)/76.8986/,
     +     C(97)/5.26593/
C
      DATA ATOMID(98)/'Pd+2  '/,IDW(98)/46/,Z(98)/44.0/,
     +     CUF1(98)/-0.059/,CUF11(98)/3.934/,MOF1(98)/-1.177/,
     +     MOF11(98)/1.007/,A1(98)/19.1701/,B1(98)/0.696219/,
     +     A2(98)/15.2096/,B2(98)/7.55573/,A3(98)/4.32234/,
     +     B3(98)/22.5057/,A4(98)/0.0/,B4(98)/0.0/,C(98)/5.2916/
C
      DATA ATOMID(99)/'Pd+4  '/,IDW(99)/46/,Z(99)/42.0/,
     +     CUF1(99)/-0.059/,CUF11(99)/3.934/,MOF1(99)/-1.177/,
     +     MOF11(99)/1.007/,A1(99)/19.2493/,B1(99)/0.683839/,
     +     A2(99)/14.79/,B2(99)/7.14833/,A3(99)/2.89289/,
     +     B3(99)/17.9144/,A4(99)/-7.9492/,B4(99)/0.005127/,
     +     C(99)/13.0174/
C
      DATA ATOMID(100)/'Ag    '/,IDW(100)/47/,Z(100)/47.0/,
     +     CUF1(100)/-0.06/,CUF11(100)/4.282/,MOF1(100)/-1.085/,
     +     MOF11(100)/1.101/,A1(100)/19.2808/,B1(100)/0.6446/,
     +     A2(100)/16.6885/,B2(100)/7.4726/,A3(100)/4.8045/,
     +     B3(100)/24.6605/,A4(100)/1.0463/,B4(100)/99.8156/,
     +     C(100)/5.179/
C
      DATA ATOMID(101)/'Ag+1  '/,IDW(101)/47/,Z(101)/46.0/,
     +     CUF1(101)/-0.06/,CUF11(101)/4.282/,MOF1(101)/-1.085/,
     +     MOF11(101)/1.101/,A1(101)/19.1812/,B1(101)/0.646179/,
     +     A2(101)/15.9719/,B2(101)/7.19123/,A3(101)/5.27475/,
     +     B3(101)/21.7326/,A4(101)/0.357534/,B4(101)/66.1147/,
     +     C(101)/5.21572/
C
      DATA ATOMID(102)/'Ag+2  '/,IDW(102)/47/,Z(102)/45.0/,
     +     CUF1(102)/-0.06/,CUF11(102)/4.282/,MOF1(102)/-1.085/,
     +     MOF11(102)/1.101/,A1(102)/19.1643/,B1(102)/0.645643/,
     +     A2(102)/16.2456/,B2(102)/7.18544/,A3(102)/4.3709/,
     +     B3(102)/21.4072/,A4(102)/0.0/,B4(102)/0.0/,C(102)/5.21404/
C
      DATA ATOMID(103)/'Cd    '/,IDW(103)/48/,Z(103)/48.0/,
     +     CUF1(103)/-0.079/,CUF11(103)/4.653/,MOF1(103)/-1.005/,
     +     MOF11(103)/1.202/,A1(103)/19.2214/,B1(103)/0.5946/,
     +     A2(103)/17.6444/,B2(103)/6.9089/,A3(103)/4.461/,
     +     B3(103)/24.7008/,A4(103)/1.6029/,B4(103)/87.4825/,
     +     C(103)/5.0694/
C
      DATA ATOMID(104)/'Cd+2  '/,IDW(104)/48/,Z(104)/46.0/,
     +     CUF1(104)/-0.079/,CUF11(104)/4.653/,MOF1(104)/-1.005/,
     +     MOF11(104)/1.202/,A1(104)/19.1514/,B1(104)/0.597922/,
     +     A2(104)/17.2535/,B2(104)/6.80639/,A3(104)/4.47128/,
     +     B3(104)/20.2521/,A4(104)/0.0/,B4(104)/0.0/,C(104)/5.11937/
C
      DATA ATOMID(105)/'In    '/,IDW(105)/49/,Z(105)/49.0/,
     +     CUF1(105)/-0.1260/,CUF11(105)/5.045/,MOF1(105)/-0.936/,
     +     MOF11(105)/1.31/,A1(105)/19.1624/,B1(105)/0.5476/,
     +     A2(105)/18.5596/,B2(105)/6.3776/,A3(105)/4.2948/,
     +     B3(105)/25.8499/,A4(105)/2.0396/,B4(105)/92.8029/,
     +     C(105)/4.9391/
C
      DATA ATOMID(106)/'In+3  '/,IDW(106)/49/,Z(106)/46.0/,
     +     CUF1(106)/-0.1260/,CUF11(106)/5.045/,MOF1(106)/-0.936/,
     +     MOF11(106)/1.31/,A1(106)/19.1045/,B1(106)/0.551522/,
     +     A2(106)/18.1108/,B2(106)/6.3247/,A3(106)/3.78897/,
     +     B3(106)/17.3595/,A4(106)/0.0/,B4(106)/0.0/,C(106)/4.99635/
C
      DATA ATOMID(107)/'Sn    '/,IDW(107)/50/,Z(107)/50.0/,
     +     CUF1(107)/-0.1940/,CUF11(107)/5.459/,MOF1(107)/-0.8730/,
     +     MOF11(107)/1.424/,A1(107)/19.1889/,B1(107)/5.8303/,
     +     A2(107)/19.1005/,B2(107)/0.5031/,A3(107)/4.4585/,
     +     B3(107)/26.8909/,A4(107)/2.4663/,B4(107)/83.9571/,
     +     C(107)/4.7821/
C
      DATA ATOMID(108)/'Sn+2  '/,IDW(108)/50/,Z(108)/48.0/,
     +     CUF1(108)/-0.1940/,CUF11(108)/5.459/,MOF1(108)/-0.8730/,
     +     MOF11(108)/1.424/,A1(108)/19.1094/,B1(108)/0.5036/,
     +     A2(108)/19.0548/,B2(108)/5.8378/,A3(108)/4.5648/,
     +     B3(108)/23.3752/,A4(108)/0.487/,B4(108)/62.2061/,
     +     C(108)/4.7861/
C
      DATA ATOMID(109)/'Sn+4  '/,IDW(109)/50/,Z(109)/46.0/,
     +     CUF1(109)/-0.1940/,CUF11(109)/5.459/,MOF1(109)/-0.8730/,
     +     MOF11(109)/1.424/,A1(109)/18.9333/,B1(109)/5.764/,
     +     A2(109)/19.7131/,B2(109)/0.4655/,A3(109)/3.4182/,
     +     B3(109)/14.0049/,A4(109)/0.0193/,B4(109)/-0.7583/,
     +     C(109)/3.9182/
C
      DATA ATOMID(110)/'Sb    '/,IDW(110)/51/,Z(110)/51.0/,
     +     CUF1(110)/-0.2870/,CUF11(110)/5.894/,MOF1(110)/-0.8160/,
     +     MOF11(110)/1.546/,A1(110)/19.6418/,B1(110)/5.3034/,
     +     A2(110)/19.0455/,B2(110)/0.4607/,A3(110)/5.0371/,
     +     B3(110)/27.9074/,A4(110)/2.6827/,B4(110)/75.2825/,
     +     C(110)/4.5909/
C
      DATA ATOMID(111)/'Sb+3  '/,IDW(111)/51/,Z(111)/48.0/,
     +     CUF1(111)/-0.2870/,CUF11(111)/5.894/,MOF1(111)/-0.8160/,
     +     MOF11(111)/1.546/,A1(111)/18.9755/,B1(111)/0.467196/,
     +     A2(111)/18.933/,B2(111)/5.22126/,A3(111)/5.10789/,
     +     B3(111)/19.5902/,A4(111)/0.288753/,B4(111)/55.5113/,
     +     C(111)/4.69626/
C
      DATA ATOMID(112)/'Sb+5  '/,IDW(112)/51/,Z(112)/46.0/,
     +     CUF1(112)/-0.2870/,CUF11(112)/5.894/,MOF1(112)/-0.8160/,
     +     MOF11(112)/1.546/,A1(112)/19.8685/,B1(112)/5.44853/,
     +     A2(112)/19.0302/,B2(112)/0.467973/,A3(112)/2.41253/,
     +     B3(112)/14.1259/,A4(112)/0.0/,B4(112)/0.0/,C(112)/4.69263/
C
      DATA ATOMID(113)/'Te    '/,IDW(113)/52/,Z(113)/52.0/,
     +     CUF1(113)/-0.4180/,CUF11(113)/6.352/,MOF1(113)/-0.7720/,
     +     MOF11(113)/1.675/,A1(113)/19.9644/,B1(113)/4.81742/,
     +     A2(113)/19.0138/,B2(113)/0.420885/,A3(113)/6.14487/,
     +     B3(113)/28.5284/,A4(113)/2.5239/,B4(113)/70.8403/,
     +     C(113)/4.352/
C
      DATA ATOMID(114)/'I     '/,IDW(114)/53/,Z(114)/53.0/,
     +     CUF1(114)/-0.5790/,CUF11(114)/6.835/,MOF1(114)/-0.7260/,
     +     MOF11(114)/1.812/,A1(114)/20.1472/,B1(114)/4.347/,
     +     A2(114)/18.9949/,B2(114)/0.3814/,A3(114)/7.5138/,
     +     B3(114)/27.766/,A4(114)/2.2735/,B4(114)/66.8776/,
     +     C(114)/4.0712/
C
      DATA ATOMID(115)/'I-1   '/,IDW(115)/53/,Z(115)/54.0/,
     +     CUF1(115)/-0.5790/,CUF11(115)/6.835/,MOF1(115)/-0.7260/,
     +     MOF11(115)/1.812/,A1(115)/20.2332/,B1(115)/4.3579/,
     +     A2(115)/18.997/,B2(115)/0.3815/,A3(115)/7.8069/,
     +     B3(115)/29.5259/,A4(115)/2.8868/,B4(115)/84.9304/,
     +     C(115)/4.0714/
C
      DATA ATOMID(116)/'Xe    '/,IDW(116)/54/,Z(116)/54.0/,
     +     CUF1(116)/-0.7830/,CUF11(116)/7.348/,MOF1(116)/-0.6840/,
     +     MOF11(116)/1.958/,A1(116)/20.2933/,B1(116)/3.9282/,
     +     A2(116)/19.0298/,B2(116)/0.344/,A3(116)/8.9767/,
     +     B3(116)/26.4659/,A4(116)/1.99/,B4(116)/64.2658/,
     +     C(116)/3.7118/
C
      DATA ATOMID(117)/'Cs    '/,IDW(117)/55/,Z(117)/55.0/,
     +     CUF1(117)/-1.022/,CUF11(117)/7.904/,MOF1(117)/-0.6440/,
     +     MOF11(117)/2.119/,A1(117)/20.3892/,B1(117)/3.569/,
     +     A2(117)/19.1062/,B2(117)/0.3107/,A3(117)/10.662/,
     +     B3(117)/24.3879/,A4(117)/1.4953/,B4(117)/213.904/,
     +     C(117)/3.3352/
C
      DATA ATOMID(118)/'Cs+1  '/,IDW(118)/55/,Z(118)/54.0/,
     +     CUF1(118)/-1.022/,CUF11(118)/7.904/,MOF1(118)/-0.6440/,
     +     MOF11(118)/2.119/,A1(118)/20.3524/,B1(118)/3.552/,
     +     A2(118)/19.1278/,B2(118)/0.3086/,A3(118)/10.2821/,
     +     B3(118)/23.7128/,A4(118)/0.9615/,B4(118)/59.4565/,
     +     C(118)/3.2791/
C
      DATA ATOMID(119)/'Ba    '/,IDW(119)/56/,Z(119)/56.0/,
     +     CUF1(119)/-1.334/,CUF11(119)/8.46/,MOF1(119)/-0.6130/,
     +     MOF11(119)/2.282/,A1(119)/20.3361/,B1(119)/3.216/,
     +     A2(119)/19.297/,B2(119)/0.2756/,A3(119)/10.888/,
     +     B3(119)/20.2073/,A4(119)/2.6959/,B4(119)/167.202/,
     +     C(119)/2.7731/
C
      DATA ATOMID(120)/'Ba+2  '/,IDW(120)/56/,Z(120)/54.0/,
     +     CUF1(120)/-1.334/,CUF11(120)/8.46/,MOF1(120)/-0.6130/,
     +     MOF11(120)/2.282/,A1(120)/20.1807/,B1(120)/3.21367/,
     +     A2(120)/19.1136/,B2(120)/0.28331/,A3(120)/10.9054/,
     +     B3(120)/20.0558/,A4(120)/0.77634/,B4(120)/51.746/,
     +     C(120)/3.02902/
C
      DATA ATOMID(121)/'La    '/,IDW(121)/57/,Z(121)/57.0/,
     +     CUF1(121)/-1.716/,CUF11(121)/9.036/,MOF1(121)/-0.5880/,
     +     MOF11(121)/2.452/,A1(121)/20.578/,B1(121)/2.94817/,
     +     A2(121)/19.599/,B2(121)/0.244475/,A3(121)/11.3727/,
     +     B3(121)/18.7726/,A4(121)/3.28719/,B4(121)/133.124/,
     +     C(121)/2.14678/
C
      DATA ATOMID(122)/'La+3  '/,IDW(122)/57/,Z(122)/54.0/,
     +     CUF1(122)/-1.716/,CUF11(122)/9.036/,MOF1(122)/-0.5880/,
     +     MOF11(122)/2.4520/,A1(122)/20.2489/,B1(122)/2.9207/,
     +     A2(122)/19.3763/,B2(122)/0.250698/,A3(122)/11.6323/,
     +     B3(122)/17.8211/,A4(122)/0.336048/,B4(122)/54.9453/,
     +     C(122)/2.4086/
C
      DATA ATOMID(123)/'Ce    '/,IDW(123)/58/,Z(123)/58.0/,
     +     CUF1(123)/-2.17/,CUF11(123)/9.648/,MOF1(123)/-0.5640/,
     +     MOF11(123)/2.632/,A1(123)/21.1671/,B1(123)/2.81219/,
     +     A2(123)/19.7695/,B2(123)/0.226836/,A3(123)/11.8513/,
     +     B3(123)/17.6083/,A4(123)/3.33049/,B4(123)/127.113/,
     +     C(123)/1.86264/
C
      DATA ATOMID(124)/'Ce+3  '/,IDW(124)/58/,Z(124)/55.0/,
     +     CUF1(124)/-2.17/,CUF11(124)/9.648/,MOF1(124)/-0.5640/,
     +     MOF11(124)/2.632/,A1(124)/20.8036/,B1(124)/2.77691/,
     +     A2(124)/19.559/,B2(124)/0.23154/,A3(124)/11.9369/,
     +     B3(124)/16.5408/,A4(124)/0.612376/,B4(124)/43.1692/,
     +     C(124)/2.09013/
C
      DATA ATOMID(125)/'Ce+4  '/,IDW(125)/58/,Z(125)/54.0/,
     +     CUF1(125)/-2.170/,CUF11(125)/9.648/,MOF1(125)/-0.5640/,
     +     MOF11(125)/2.632/,A1(125)/20.3235/,B1(125)/2.65941/,
     +     A2(125)/19.8186/,B2(125)/0.21885/,A3(125)/12.1233/,
     +     B3(125)/15.7992/,A4(125)/0.144583/,B4(125)/62.2355/,
     +     C(125)/1.5918/
C
      DATA ATOMID(126)/'Pr    '/,IDW(126)/59/,Z(126)/59.0/,
     +     CUF1(126)/-2.939/,CUF11(126)/10.535/,MOF1(126)/-0.530/,
     +     MOF11(126)/2.845/,A1(126)/22.044/,B1(126)/2.77393/,
     +     A2(126)/19.6697/,B2(126)/0.222087/,A3(126)/12.3856/,
     +     B3(126)/16.7669/,A4(126)/2.82428/,B4(126)/143.644/,
     +     C(126)/2.0583/
C
      DATA ATOMID(127)/'Pr+3  '/,IDW(127)/59/,Z(127)/56.0/,
     +     CUF1(127)/-2.939/,CUF11(127)/10.535/,MOF1(127)/-0.530/,
     +     MOF11(127)/2.845/,A1(127)/21.3727/,B1(127)/2.6452/,
     +     A2(127)/19.7491/,B2(127)/0.214299/,A3(127)/12.1329/,
     +     B3(127)/15.323/,A4(127)/0.97518/,B4(127)/36.4065/,
     +     C(127)/1.77132/
C
      DATA ATOMID(128)/'Pr+4  '/,IDW(128)/59/,Z(128)/55.0/,
     +     CUF1(128)/-2.939/,CUF11(128)/10.535/,MOF1(128)/-0.530/,
     +     MOF11(128)/2.845/,A1(128)/20.9413/,B1(128)/2.54467/,
     +     A2(128)/20.0539/,B2(128)/0.202481/,A3(128)/12.4668/,
     +     B3(128)/14.8137/,A4(128)/0.296689/,B4(128)/45.4643/,
     +     C(128)/1.24285/
C
      DATA ATOMID(129)/'Nd    '/,IDW(129)/60/,Z(129)/60.0/,
     +     CUF1(129)/-3.431/,CUF11(129)/10.933/,MOF1(129)/-0.5350/,
     +     MOF11(129)/3.018/,A1(129)/22.6845/,B1(129)/2.66248/,
     +     A2(129)/19.6847/,B2(129)/0.210628/,A3(129)/12.774/,
     +     B3(129)/15.885/,A4(129)/2.85137/,B4(129)/137.903/,
     +     C(129)/1.98486/
C
      DATA ATOMID(130)/'Nd+3  '/,IDW(130)/60/,Z(130)/57.0/,
     +     CUF1(130)/-3.431/,CUF11(130)/10.933/,MOF1(130)/-0.5350/,
     +     MOF11(130)/3.018/,A1(130)/21.961/,B1(130)/2.52722/,
     +     A2(130)/19.9339/,B2(130)/0.199237/,A3(130)/12.12/,
     +     B3(130)/14.1783/,A4(130)/1.51031/,B4(130)/30.8717/,
     +     C(130)/1.47588/
C
      DATA ATOMID(131)/'Pm    '/,IDW(131)/61/,Z(131)/61.0/,
     +     CUF1(131)/-4.357/,CUF11(131)/11.614/,MOF1(131)/-0.530/,
     +     MOF11(131)/3.225/,A1(131)/23.3405/,B1(131)/2.5627/,
     +     A2(131)/19.6095/,B2(131)/0.202088/,A3(131)/13.1235/,
     +     B3(131)/15.1009/,A4(131)/2.87516/,B4(131)/132.721/,
     +     C(131)/2.02876/
C
      DATA ATOMID(132)/'Pm+3  '/,IDW(132)/61/,Z(132)/58.0/,
     +     CUF1(132)/-4.357/,CUF11(132)/11.614/,MOF1(132)/-0.530/,
     +     MOF11(132)/3.225/,A1(132)/22.5527/,B1(132)/2.4174/,
     +     A2(132)/20.1108/,B2(132)/0.185769/,A3(132)/12.0671/,
     +     B3(132)/13.1275/,A4(132)/2.07492/,B4(132)/27.4491/,
     +     C(132)/1.19499/
C
      DATA ATOMID(133)/'Sm    '/,IDW(133)/62/,Z(133)/62.0/,
     +     CUF1(133)/-5.696/,CUF11(133)/12.320/,MOF1(133)/-0.5330/,
     +     MOF11(133)/3.442/,A1(133)/24.0042/,B1(133)/2.47274/,
     +     A2(133)/19.4258/,B2(133)/0.196451/,A3(133)/13.4396/,
     +     B3(133)/14.3996/,A4(133)/2.89604/,B4(133)/128.007/,
     +     C(133)/2.20963/
C
      DATA ATOMID(134)/'Sm+3  '/,IDW(134)/62/,Z(134)/58.0/,
     +     CUF1(134)/-5.696/,CUF11(134)/12.320/,MOF1(134)/-0.5330/,
     +     MOF11(134)/3.442/,A1(134)/23.1504/,B1(134)/2.31641/,
     +     A2(134)/20.2599/,B2(134)/0.174081/,A3(134)/11.9202/,
     +     B3(134)/12.1571/,A4(134)/2.71488/,B4(134)/24.8242/,
     +     C(134)/0.954586/
C
      DATA ATOMID(135)/'Eu    '/,IDW(135)/63/,Z(135)/63.0/,
     +     CUF1(135)/-7.718/,CUF11(135)/11.276/,MOF1(135)/-0.542/,
     +     MOF11(135)/3.669/,A1(135)/24.6274/,B1(135)/2.3879/,
     +     A2(135)/19.0886/,B2(135)/0.1942/,A3(135)/13.7603/,
     +     B3(135)/13.7546/,A4(135)/2.9227/,B4(135)/123.174/,
     +     C(135)/2.5745/
C
      DATA ATOMID(136)/'Eu+2  '/,IDW(136)/63/,Z(136)/61.0/,
     +     CUF1(136)/-7.718/,CUF11(136)/11.276/,MOF1(136)/-0.5420/,
     +     MOF11(136)/3.669/,A1(136)/24.0063/,B1(136)/2.27783/,
     +     A2(136)/19.9504/,B2(136)/0.17353/,A3(136)/11.8034/,
     +     B3(136)/11.6096/,A4(136)/3.87243/,B4(136)/26.5156/,
     +     C(136)/1.36389/
C
      DATA ATOMID(137)/'Eu+3  '/,IDW(137)/63/,Z(137)/60.0/,
     +     CUF1(137)/-7.718/,CUF11(137)/11.276/,MOF1(137)/-0.5420/,
     +     MOF11(137)/3.669/,A1(137)/23.7497/,B1(137)/2.22258/,
     +     A2(137)/20.3745/,B2(137)/0.16394/,A3(137)/11.8509/,
     +     B3(137)/11.311/,A4(137)/3.26503/,B4(137)/22.9966/,
     +     C(137)/0.759344/
C
      DATA ATOMID(138)/'Gd    '/,IDW(138)/64/,Z(138)/64.0/,
     +     CUF1(138)/-9.242/,CUF11(138)/11.946/,MOF1(138)/-0.5640/,
     +     MOF11(138)/3.904/,A1(138)/25.0709/,B1(138)/2.25341/,
     +     A2(138)/19.0798/,B2(138)/0.181951/,A3(138)/13.8518/,
     +     B3(138)/12.9331/,A4(138)/3.54545/,B4(138)/101.398/,
     +     C(138)/2.4196/
C
      DATA ATOMID(139)/'Gd+3  '/,IDW(139)/64/,Z(139)/61.0/,
     +     CUF1(139)/-9.242/,CUF11(139)/11.946/,MOF1(139)/-0.5640/,
     +     MOF11(139)/3.904/,A1(139)/24.3466/,B1(139)/2.13553/,
     +     A2(139)/20.4208/,B2(139)/0.155525/,A3(139)/11.8708/,
     +     B3(139)/10.5782/,A4(139)/3.7149/,B4(139)/21.7029/,
     +     C(139)/0.645089/
C
      DATA ATOMID(140)/'Tb    '/,IDW(140)/65/,Z(140)/65.0/,
     +     CUF1(140)/-9.498/,CUF11(140)/9.242/,MOF1(140)/-0.5910/,
     +     MOF11(140)/4.151/,A1(140)/25.8976/,B1(140)/2.24256/,
     +     A2(140)/18.2185/,B2(140)/0.196143/,A3(140)/14.3167/,
     +     B3(140)/12.6648/,A4(140)/2.95354/,B4(140)/115.362/,
     +     C(140)/3.58224/
C
      DATA ATOMID(141)/'Tb+3  '/,IDW(141)/65/,Z(141)/62.0/,
     +     CUF1(141)/-9.498/,CUF11(141)/9.242/,MOF1(141)/-0.5910/,
     +     MOF11(141)/4.151/,A1(141)/24.9559/,B1(141)/2.05601/,
     +     A2(141)/20.3271/,B2(141)/0.149525/,A3(141)/12.2471/,
     +     B3(141)/10.0499/,A4(141)/3.773/,B4(141)/21.2773/,
     +     C(141)/0.691967/
C
      DATA ATOMID(142)/'Dy    '/,IDW(142)/66/,Z(142)/66.0/,
     +     CUF1(142)/-10.423/,CUF11(142)/9.748/,MOF1(142)/-0.6190/,
     +     MOF11(142)/4.410/,A1(142)/26.507/,B1(142)/2.1802/,
     +     A2(142)/17.6383/,B2(142)/0.202172/,A3(142)/14.5596/,
     +     B3(142)/12.1899/,A4(142)/2.96577/,B4(142)/111.874/,
     +     C(142)/4.29728/
C
      DATA ATOMID(143)/'Dy+3  '/,IDW(143)/66/,Z(143)/63.0/,
     +     CUF1(143)/-10.423/,CUF11(143)/9.748/,MOF1(143)/-0.6190/,
     +     MOF11(143)/4.410/,A1(143)/25.5395/,B1(143)/1.9804/,
     +     A2(143)/20.2861/,B2(143)/0.143384/,A3(143)/11.9812/,
     +     B3(143)/9.34972/,A4(143)/4.50073/,B4(143)/19.581/,
     +     C(143)/0.68969/
C
      DATA ATOMID(144)/'Ho    '/,IDW(144)/67/,Z(144)/67.0/,
     +     CUF1(144)/-12.255/,CUF11(144)/3.704/,MOF1(144)/-0.6660/,
     +     MOF11(144)/4.678/,A1(144)/26.9049/,B1(144)/2.07051/,
     +     A2(144)/17.294/,B2(144)/0.19794/,A3(144)/14.5583/,
     +     B3(144)/11.4407/,A4(144)/3.63837/,B4(144)/92.6566/,
     +     C(144)/4.56796/
C
      DATA ATOMID(145)/'Ho+3  '/,IDW(145)/67/,Z(145)/64.0/,
     +     CUF1(145)/-12.255/,CUF11(145)/3.704/,MOF1(145)/-0.6660/,
     +     MOF11(145)/4.678/,A1(145)/26.1296/,B1(145)/1.91072/,
     +     A2(145)/20.0994/,B2(145)/0.139358/,A3(145)/11.9788/,
     +     B3(145)/8.80018/,A4(145)/4.93676/,B4(145)/18.5908/,
     +     C(145)/0.852795/
C
      DATA ATOMID(146)/'Er    '/,IDW(146)/68/,Z(146)/68.0/,
     +     CUF1(146)/-9.733/,CUF11(146)/3.937/,MOF1(146)/-0.7230/,
     +     MOF11(146)/4.958/,A1(146)/27.6563/,B1(146)/2.07356/,
     +     A2(146)/16.4285/,B2(146)/0.223545/,A3(146)/14.9779/,
     +     B3(146)/11.3604/,A4(146)/2.98233/,B4(146)/105.703/,
     +     C(146)/5.92046/
C
      DATA ATOMID(147)/'Er+3  '/,IDW(147)/68/,Z(147)/65.0/,
     +     CUF1(147)/-9.733/,CUF11(147)/3.937/,MOF1(147)/-0.7230/,
     +     MOF11(147)/4.958/,A1(147)/26.722/,B1(147)/1.84659/,
     +     A2(147)/19.7748/,B2(147)/0.13729/,A3(147)/12.1506/,
     +     B3(147)/8.36225/,A4(147)/5.17379/,B4(147)/17.8974/,
     +     C(147)/1.17613/
C
      DATA ATOMID(148)/'Tm    '/,IDW(148)/69/,Z(148)/69.0/,
     +     CUF1(148)/-8.488/,CUF11(148)/4.181/,MOF1(148)/-0.795/,
     +     MOF11(148)/5.248/,A1(148)/28.1819/,B1(148)/2.02859/,
     +     A2(148)/15.8851/,B2(148)/0.238849/,A3(148)/15.1542/,
     +     B3(148)/10.9975/,A4(148)/2.98706/,B4(148)/102.961/,
     +     C(148)/6.75621/
C
      DATA ATOMID(149)/'Tm+3  '/,IDW(149)/69/,Z(149)/66.0/,
     +     CUF1(149)/-8.488/,CUF11(149)/4.181/,MOF1(149)/-0.7950/,
     +     MOF11(149)/5.248/,A1(149)/27.3083/,B1(149)/1.78711/,
     +     A2(149)/19.332/,B2(149)/0.136974/,A3(149)/12.3339/,
     +     B3(149)/7.96778/,A4(149)/5.38348/,B4(149)/17.2922/,
     +     C(149)/1.63929/
C
      DATA ATOMID(150)/'Yb    '/,IDW(150)/70/,Z(150)/70.0/,
     +     CUF1(150)/-7.701/,CUF11(150)/4.432/,MOF1(150)/-0.8840/,
     +     MOF11(150)/5.548/,A1(150)/28.6641/,B1(150)/1.9889/,
     +     A2(150)/15.4345/,B2(150)/0.257119/,A3(150)/15.3087/,
     +     B3(150)/10.6647/,A4(150)/2.98963/,B4(150)/100.417/,
     +     C(150)/7.56672/
C
      DATA ATOMID(151)/'Yb+2  '/,IDW(151)/70/,Z(151)/68.0/,
     +     CUF1(151)/-7.701/,CUF11(151)/4.432/,MOF1(151)/-0.8840/,
     +     MOF11(151)/5.548/,A1(151)/28.1209/,B1(151)/1.78503/,
     +     A2(151)/17.6817/,B2(151)/0.15997/,A3(151)/13.3335/,
     +     B3(151)/8.18304/,A4(151)/5.14657/,B4(151)/20.39/,
     +     C(151)/3.70983/
C
      DATA ATOMID(152)/'Yb+3  '/,IDW(152)/70/,Z(152)/67.0/,
     +     CUF1(152)/-7.701/,CUF11(152)/4.432/,MOF1(152)/-0.8840/,
     +     MOF11(152)/5.548/,A1(152)/27.8917/,B1(152)/1.73272/,
     +     A2(152)/18.7614/,B2(152)/0.13879/,A3(152)/12.6072/,
     +     B3(152)/7.64412/,A4(152)/5.47647/,B4(152)/16.8143/,
     +     C(152)/2.26001/
C
      DATA ATOMID(153)/'Lu    '/,IDW(153)/71/,Z(153)/71.0/,
     +     CUF1(153)/-7.133/,CUF11(153)/4.693/,MOF1(153)/-0.9880/,
     +     MOF11(153)/5.858/,A1(153)/28.9476/,B1(153)/1.90182/,
     +     A2(153)/15.2208/,B2(153)/9.98519/,A3(153)/15.1/,
     +     B3(153)/0.261033/,A4(153)/3.71601/,B4(153)/84.3298/,
     +     C(153)/7.97628/
C
      DATA ATOMID(154)/'Lu+3  '/,IDW(154)/71/,Z(154)/68.0/,
     +     CUF1(154)/-7.133/,CUF11(154)/4.693/,MOF1(154)/-0.9880/,
     +     MOF11(154)/5.858/,A1(154)/28.4628/,B1(154)/1.68216/,
     +     A2(154)/18.121/,B2(154)/0.142292/,A3(154)/12.8429/,
     +     B3(154)/7.33727/,A4(154)/5.59415/,B4(154)/16.3535/,
     +     C(154)/2.97573/
C
      DATA ATOMID(155)/'Hf    '/,IDW(155)/72/,Z(155)/72.0/,
     +     CUF1(155)/-6.715/,CUF11(155)/4.977/,MOF1(155)/-1.118/,
     +     MOF11(155)/6.185/,A1(155)/29.144/,B1(155)/1.83262/,
     +     A2(155)/15.1726/,B2(155)/9.5999/,A3(155)/14.7586/,
     +     B3(155)/0.275116/,A4(155)/4.30013/,B4(155)/72.029/,
     +     C(155)/8.58154/
C
      DATA ATOMID(156)/'Hf+4  '/,IDW(156)/72/,Z(156)/68.0/,
     +     CUF1(156)/-6.715/,CUF11(156)/4.977/,MOF1(156)/-1.118/,
     +     MOF11(156)/6.185/,A1(156)/28.8131/,B1(156)/1.59136/,
     +     A2(156)/18.4601/,B2(156)/0.128903/,A3(156)/12.7285/,
     +     B3(156)/6.76232/,A4(156)/5.59927/,B4(156)/14.0366/,
     +     C(156)/2.39699/
C
      DATA ATOMID(157)/'Ta    '/,IDW(157)/73/,Z(157)/73.0/,
     +     CUF1(157)/-6.351/,CUF11(157)/5.271/,MOF1(157)/-1.258/,
     +     MOF11(157)/6.523/,A1(157)/29.2024/,B1(157)/1.77333/,
     +     A2(157)/15.2293/,B2(157)/9.37046/,A3(157)/14.5135/,
     +     B3(157)/0.295977/,A4(157)/4.76492/,B4(157)/63.3644/,
     +     C(157)/9.24354/
C
      DATA ATOMID(158)/'Ta+5  '/,IDW(158)/73/,Z(158)/68.0/,
     +     CUF1(158)/-6.351/,CUF11(158)/5.271/,MOF1(158)/-1.258/,
     +     MOF11(158)/6.523/,A1(158)/29.1587/,B1(158)/1.50711/,
     +     A2(158)/18.8407/,B2(158)/0.116741/,A3(158)/12.8268/,
     +     B3(158)/6.31524/,A4(158)/5.38695/,B4(158)/12.4244/,
     +     C(158)/1.78555/
C
      DATA ATOMID(159)/'W     '/,IDW(159)/74/,Z(159)/74.0/,
     +     CUF1(159)/-6.048/,CUF11(159)/5.577/,MOF1(159)/-1.421/,
     +     MOF11(159)/6.872/,A1(159)/29.0818/,B1(159)/1.72029/,
     +     A2(159)/15.43/,B2(159)/9.2259/,A3(159)/14.4327/,
     +     B3(159)/0.321703/,A4(159)/5.11982/,B4(159)/57.056/,
     +     C(159)/9.8875/
C
      DATA ATOMID(160)/'W+6   '/,IDW(160)/74/,Z(160)/68.0/,
     +     CUF1(160)/-6.048/,CUF11(160)/5.577/,MOF1(160)/-1.421/,
     +     MOF11(160)/6.872/,A1(160)/29.4936/,B1(160)/1.42755/,
     +     A2(160)/19.3763/,B2(160)/0.104621/,A3(160)/13.0544/,
     +     B3(160)/5.93667/,A4(160)/5.06412/,B4(160)/11.1972/,
     +     C(160)/1.01074/
C
      DATA ATOMID(161)/'Re    '/,IDW(161)/75/,Z(161)/75.0/,
     +     CUF1(161)/-5.790/,CUF11(161)/5.891/,MOF1(161)/-1.598/,
     +     MOF11(161)/7.232/,A1(161)/28.7621/,B1(161)/1.67191/,
     +     A2(161)/15.7189/,B2(161)/9.09227/,A3(161)/14.5564/,
     +     B3(161)/0.3505/,A4(161)/5.44174/,B4(161)/52.0861/,
     +     C(161)/10.472/
C
      DATA ATOMID(162)/'Os    '/,IDW(162)/76/,Z(162)/76.0/,
     +     CUF1(162)/-5.581/,CUF11(162)/6.221/,MOF1(162)/-1.816/,
     +     MOF11(162)/7.605/,A1(162)/28.1894/,B1(162)/1.62903/,
     +     A2(162)/16.155/,B2(162)/8.97948/,A3(162)/14.9305/,
     +     B3(162)/0.382661/,A4(162)/5.67589/,B4(162)/48.1647/,
     +     C(162)/11.0005/
C
      DATA ATOMID(163)/'Os+4  '/,IDW(163)/76/,Z(163)/72.0/,
     +     CUF1(163)/-5.581/,CUF11(163)/6.221/,MOF1(163)/-1.816/,
     +     MOF11(163)/7.605/,A1(163)/30.419/,B1(163)/1.37113/,
     +     A2(163)/15.2637/,B2(163)/6.84706/,A3(163)/14.7458/,
     +     B3(163)/0.165191/,A4(163)/5.06795/,B4(163)/18.003/,
     +     C(163)/6.49804/
C
      DATA ATOMID(164)/'Ir    '/,IDW(164)/77/,Z(164)/77.0/,
     +     CUF1(164)/-5.391/,CUF11(164)/6.566/,MOF1(164)/-2.066/,
     +     MOF11(164)/7.990/,A1(164)/27.3049/,B1(164)/1.59279/,
     +     A2(164)/16.7296/,B2(164)/8.86553/,A3(164)/15.6115/,
     +     B3(164)/0.417916/,A4(164)/5.83377/,B4(164)/45.0011/,
     +     C(164)/11.4722/
C
      DATA ATOMID(165)/'Ir+3  '/,IDW(165)/77/,Z(165)/74.0/,
     +     CUF1(165)/-5.391/,CUF11(165)/6.566/,MOF1(165)/-2.066/,
     +     MOF11(165)/7.990/,A1(165)/30.4156/,B1(165)/1.34323/,
     +     A2(165)/15.862/,B2(165)/7.10909/,A3(165)/13.6145/,
     +     B3(165)/0.204633/,A4(165)/5.82008/,B4(165)/20.3254/,
     +     C(165)/8.27903/
C
      DATA ATOMID(166)/'Ir+4  '/,IDW(166)/77/,Z(166)/73.0/,
     +     CUF1(166)/-5.391/,CUF11(166)/6.566/,MOF1(166)/-2.066/,
     +     MOF11(166)/7.990/,A1(166)/30.7058/,B1(166)/1.30923/,
     +     A2(166)/15.5512/,B2(166)/6.71983/,A3(166)/14.2326/,
     +     B3(166)/0.167252/,A4(166)/5.53672/,B4(166)/17.4911/,
     +     C(166)/6.96824/
C
      DATA ATOMID(167)/'Pt    '/,IDW(167)/78/,Z(167)/78.0/,
     +     CUF1(167)/-5.233/,CUF11(167)/6.925/,MOF1(167)/-2.352/,
     +     MOF11(167)/8.388/,A1(167)/27.0059/,B1(167)/1.51293/,
     +     A2(167)/17.7639/,B2(167)/8.81174/,A3(167)/15.7131/,
     +     B3(167)/0.424593/,A4(167)/5.7837/,B4(167)/38.6103/,
     +     C(167)/11.6883/
C
      DATA ATOMID(168)/'Pt+2  '/,IDW(168)/78/,Z(168)/76.0/,
     +     CUF1(168)/-5.233/,CUF11(168)/6.925/,MOF1(168)/-2.352/,
     +     MOF11(168)/8.388/,A1(168)/29.8429/,B1(168)/1.32927/,
     +     A2(168)/16.7224/,B2(168)/7.38979/,A3(168)/13.2153/,
     +     B3(168)/0.263297/,A4(168)/6.35234/,B4(168)/22.9426/,
     +     C(168)/9.85329/
C
      DATA ATOMID(169)/'Pt+4  '/,IDW(169)/78/,Z(169)/74.0/,
     +     CUF1(169)/-5.233/,CUF11(169)/6.925/,MOF1(169)/-2.352/,
     +     MOF11(169)/8.388/,A1(169)/30.9612/,B1(169)/1.24813/,
     +     A2(169)/15.9829/,B2(169)/6.60834/,A3(169)/13.7348/,
     +     B3(169)/0.16864/,A4(169)/5.92034/,B4(169)/16.9392/,
     +     C(169)/7.39534/
C
      DATA ATOMID(170)/'Au    '/,IDW(170)/79/,Z(170)/79.0/,
     +     CUF1(170)/-5.096/,CUF11(170)/7.297/,MOF1(170)/-2.688/,
     +     MOF11(170)/8.798/,A1(170)/16.8819/,B1(170)/0.4611/,
     +     A2(170)/18.5913/,B2(170)/8.6216/,A3(170)/25.5582/,
     +     B3(170)/1.4826/,A4(170)/5.86/,B4(170)/36.3956/,
     +     C(170)/12.0658/
C
      DATA ATOMID(171)/'Au+1  '/,IDW(171)/79/,Z(171)/78.0/,
     +     CUF1(171)/-5.096/,CUF11(171)/7.297/,MOF1(171)/-2.688/,
     +     MOF11(171)/8.798/,A1(171)/28.0109/,B1(171)/1.35321/,
     +     A2(171)/17.8204/,B2(171)/7.7395/,A3(171)/14.3359/,
     +     B3(171)/0.356752/,A4(171)/6.58077/,B4(171)/26.4043/,
     +     C(171)/11.2299/
C
      DATA ATOMID(172)/'Au+3  '/,IDW(172)/79/,Z(172)/76.0/,
     +     CUF1(172)/-5.096/,CUF11(172)/7.297/,MOF1(172)/-2.688/,
     +     MOF11(172)/8.798/,A1(172)/30.6886/,B1(172)/1.2199/,
     +     A2(172)/16.9029/,B2(172)/6.82872/,A3(172)/12.7801/,
     +     B3(172)/0.212867/,A4(172)/6.52354/,B4(172)/18.659/,
     +     C(172)/9.0968/
C
      DATA ATOMID(173)/'Hg    '/,IDW(173)/80/,Z(173)/80.0/,
     +     CUF1(173)/-4.990/,CUF11(173)/7.686/,MOF1(173)/-3.084/,
     +     MOF11(173)/9.223/,A1(173)/20.6809/,B1(173)/0.545/,
     +     A2(173)/19.0417/,B2(173)/8.4484/,A3(173)/21.6575/,
     +     B3(173)/1.5729/,A4(173)/5.9676/,B4(173)/38.3246/,
     +     C(173)/12.6089/
C
      DATA ATOMID(174)/'Hg+1  '/,IDW(174)/80/,Z(174)/79.0/,
     +     CUF1(174)/-4.990/,CUF11(174)/7.686/,MOF1(174)/-3.084/,
     +     MOF11(174)/9.223/,A1(174)/25.0853/,B1(174)/1.39507/,
     +     A2(174)/18.4973/,B2(174)/7.65105/,A3(174)/16.8883/,
     +     B3(174)/0.443378/,A4(174)/6.48216/,B4(174)/28.2262/,
     +     C(174)/12.0205/
C
      DATA ATOMID(175)/'Hg+2  '/,IDW(175)/80/,Z(175)/78.0/,
     +     CUF1(175)/-4.990/,CUF11(175)/7.686/,MOF1(175)/-3.084/,
     +     MOF11(175)/9.223/,A1(175)/29.5641/,B1(175)/1.21152/,
     +     A2(175)/18.06/,B2(175)/7.05639/,A3(175)/12.8374/,
     +     B3(175)/0.284738/,A4(175)/6.89912/,B4(175)/20.7482/,
     +     C(175)/10.6268/
C
      DATA ATOMID(176)/'Tl    '/,IDW(176)/81/,Z(176)/81.0/,
     +     CUF1(176)/-4.883/,CUF11(176)/8.089/,MOF1(176)/-3.556/,
     +     MOF11(176)/9.659/,A1(176)/27.5446/,B1(176)/0.65515/,
     +     A2(176)/19.1584/,B2(176)/8.70751/,A3(176)/15.538/,
     +     B3(176)/1.96347/,A4(176)/5.52593/,B4(176)/45.8149/,
     +     C(176)/13.1746/
C
      DATA ATOMID(177)/'Tl+1  '/,IDW(177)/81/,Z(177)/80.0/,
     +     CUF1(177)/-4.883/,CUF11(177)/8.089/,MOF1(177)/-3.556/,
     +     MOF11(177)/9.659/,A1(177)/21.3985/,B1(177)/1.4711/,
     +     A2(177)/20.4723/,B2(177)/0.517394/,A3(177)/18.7478/,
     +     B3(177)/7.43463/,A4(177)/6.82847/,B4(177)/28.8482/,
     +     C(177)/12.5258/
C
      DATA ATOMID(178)/'Tl+3  '/,IDW(178)/81/,Z(178)/78.0/,
     +     CUF1(178)/-4.883/,CUF11(178)/8.089/,MOF1(178)/-3.556/,
     +     MOF11(178)/9.659/,A1(178)/30.8695/,B1(178)/1.1008/,
     +     A2(178)/18.3841/,B2(178)/6.53852/,A3(178)/11.9328/,
     +     B3(178)/0.219074/,A4(178)/7.00574/,B4(178)/17.2114/,
     +     C(178)/9.8027/
C
      DATA ATOMID(179)/'Pb    '/,IDW(179)/82/,Z(179)/82.0/,
     +     CUF1(179)/-4.818/,CUF11(179)/8.505/,MOF1(179)/-4.133/,
     +     MOF11(179)/10.102/,A1(179)/31.0617/,B1(179)/0.6902/,
     +     A2(179)/13.0637/,B2(179)/2.3576/,A3(179)/18.442/,
     +     B3(179)/8.618/,A4(179)/5.9696/,B4(179)/47.2579/,
     +     C(179)/13.4118/
C
      DATA ATOMID(180)/'Pb+2  '/,IDW(180)/82/,Z(180)/80.0/,
     +     CUF1(180)/-4.818/,CUF11(180)/8.505/,MOF1(180)/-4.133/,
     +     MOF11(180)/10.102/,A1(180)/21.7886/,B1(180)/1.3366/,
     +     A2(180)/19.5682/,B2(180)/0.488383/,A3(180)/19.1406/,
     +     B3(180)/6.7727/,A4(180)/7.01107/,B4(180)/23.8132/,
     +     C(180)/12.4734/
C
      DATA ATOMID(181)/'Pb+4  '/,IDW(181)/82/,Z(181)/78.0/,
     +     CUF1(181)/-4.818/,CUF11(181)/8.505/,MOF1(181)/-4.133/,
     +     MOF11(181)/10.102/,A1(181)/32.1244/,B1(181)/1.00566/,
     +     A2(181)/18.8003/,B2(181)/6.10926/,A3(181)/12.0175/,
     +     B3(181)/0.147041/,A4(181)/6.96886/,B4(181)/14.714/,
     +     C(181)/8.08428/
C
      DATA ATOMID(182)/'Bi    '/,IDW(182)/83/,Z(182)/83.0/,
     +     CUF1(182)/-4.776/,CUF11(182)/8.930/,MOF1(182)/-4.861/,
     +     MOF11(182)/10.559/,A1(182)/33.3689/,B1(182)/0.704/,
     +     A2(182)/12.951/,B2(182)/2.9238/,A3(182)/16.5877/,
     +     B3(182)/8.7937/,A4(182)/6.4692/,B4(182)/48.0093/,
     +     C(182)/13.5782/
C
      DATA ATOMID(183)/'Bi+3  '/,IDW(183)/83/,Z(183)/80.0/,
     +     CUF1(183)/-4.776/,CUF11(183)/8.930/,MOF1(183)/-4.861/,
     +     MOF11(183)/10.559/,A1(183)/21.8053/,B1(183)/1.2356/,
     +     A2(183)/19.5026/,B2(183)/6.24149/,A3(183)/19.1053/,
     +     B3(183)/0.469999/,A4(183)/7.10295/,B4(183)/20.3185/,
     +     C(183)/12.4711/
C
      DATA ATOMID(184)/'Bi+5  '/,IDW(184)/83/,Z(184)/78.0/,
     +     CUF1(184)/-4.776/,CUF11(184)/8.930/,MOF1(184)/-4.861/,
     +     MOF11(184)/10.559/,A1(184)/33.5364/,B1(184)/0.91654/,
     +     A2(184)/25.0946/,B2(184)/0.39042/,A3(184)/19.2497/,
     +     B3(184)/5.71414/,A4(184)/6.91555/,B4(184)/12.8285/,
     +     C(184)/-6.7994/
C
      DATA ATOMID(185)/'Po    '/,IDW(185)/84/,Z(185)/84.0/,
     +     CUF1(185)/-4.756/,CUF11(185)/9.383/,MOF1(185)/-5.924/,
     +     MOF11(185)/11.042/,A1(185)/34.6726/,B1(185)/0.700999/,
     +     A2(185)/15.4733/,B2(185)/3.55078/,A3(185)/13.1138/,
     +     B3(185)/9.55642/,A4(185)/7.02588/,B4(185)/47.0045/,
     +     C(185)/13.677/
C
      DATA ATOMID(186)/'At    '/,IDW(186)/85/,Z(186)/85.0/,
     +     CUF1(186)/-4.772/,CUF11(186)/9.843/,MOF1(186)/-7.444/,
     +     MOF11(186)/9.961/,A1(186)/35.3163/,B1(186)/0.68587/,
     +     A2(186)/19.0211/,B2(186)/3.97458/,A3(186)/9.49887/,
     +     B3(186)/11.3824/,A4(186)/7.42518/,B4(186)/45.4715/,
     +     C(186)/13.7108/
C
      DATA ATOMID(187)/'Rn    '/,IDW(187)/86/,Z(187)/86.0/,
     +     CUF1(187)/-4.787/,CUF11(187)/10.317/,MOF1(187)/-8.862/,
     +     MOF11(187)/10.403/,A1(187)/35.5631/,B1(187)/0.6631/,
     +     A2(187)/21.2816/,B2(187)/4.0691/,A3(187)/8.0037/,
     +     B3(187)/14.0422/,A4(187)/7.4433/,B4(187)/44.2473/,
     +     C(187)/13.6905/
C
      DATA ATOMID(188)/'Fr    '/,IDW(188)/87/,Z(188)/87.0/,
     +     CUF1(188)/-4.833/,CUF11(188)/10.803/,MOF1(188)/-7.912/,
     +     MOF11(188)/7.754/,A1(188)/35.9299/,B1(188)/0.646453/,
     +     A2(188)/23.0547/,B2(188)/4.17619/,A3(188)/12.1439/,
     +     B3(188)/23.1052/,A4(188)/2.11253/,B4(188)/150.645/,
     +     C(188)/13.7247/
C
      DATA ATOMID(189)/'Ra    '/,IDW(189)/88/,Z(189)/88.0/,
     +     CUF1(189)/-4.898/,CUF11(189)/11.296/,MOF1(189)/-7.620/,
     +     MOF11(189)/8.105/,A1(189)/35.763/,B1(189)/0.616341/,
     +     A2(189)/22.9064/,B2(189)/3.87135/,A3(189)/12.4739/,
     +     B3(189)/19.9887/,A4(189)/3.21097/,B4(189)/142.325/,
     +     C(189)/13.6211/
C
      DATA ATOMID(190)/'Ra+2  '/,IDW(190)/88/,Z(190)/86.0/,
     +     CUF1(190)/-4.898/,CUF11(190)/11.296/,MOF1(190)/-7.620/,
     +     MOF11(190)/8.105/,A1(190)/35.215/,B1(190)/0.604909/,
     +     A2(190)/21.67/,B2(190)/3.5767/,A3(190)/7.91342/,
     +     B3(190)/12.601/,A4(190)/7.65078/,B4(190)/29.8436/,
     +     C(190)/13.5431/
C
      DATA ATOMID(191)/'Ac    '/,IDW(191)/89/,Z(191)/89.0/,
     +     CUF1(191)/-4.994/,CUF11(191)/11.799/,MOF1(191)/-7.725/,
     +     MOF11(191)/8.472/,A1(191)/35.6597/,B1(191)/0.589092/,
     +     A2(191)/23.1032/,B2(191)/3.65155/,A3(191)/12.5977/,
     +     B3(191)/18.599/,A4(191)/4.08655/,B4(191)/117.02/,
     +     C(191)/13.5266/
C
      DATA ATOMID(192)/'Ac+3  '/,IDW(192)/89/,Z(192)/86.0/,
     +     CUF1(192)/-4.994/,CUF11(192)/11.799/,MOF1(192)/-7.725/,
     +     MOF11(192)/8.472/,A1(192)/35.1736/,B1(192)/0.579689/,
     +     A2(192)/22.1112/,B2(192)/3.41437/,A3(192)/8.19216/,
     +     B3(192)/12.9187/,A4(192)/7.05545/,B4(192)/25.9443/,
     +     C(192)/13.4637/
C
      DATA ATOMID(193)/'Th    '/,IDW(193)/90/,Z(193)/90.0/,
     +     CUF1(193)/-5.091/,CUF11(193)/12.330/,MOF1(193)/-8.127/,
     +     MOF11(193)/8.870/,A1(193)/35.5645/,B1(193)/0.563359/,
     +     A2(193)/23.4219/,B2(193)/3.46204/,A3(193)/12.7473/,
     +     B3(193)/17.8309/,A4(193)/4.80703/,B4(193)/99.1722/,
     +     C(193)/13.4314/
C
      DATA ATOMID(194)/'Th+4  '/,IDW(194)/90/,Z(194)/86.0/,
     +     CUF1(194)/-5.091/,CUF11(194)/12.330/,MOF1(194)/-8.127/,
     +     MOF11(194)/8.870/,A1(194)/35.1007/,B1(194)/0.555054/,
     +     A2(194)/22.4418/,B2(194)/3.24498/,A3(194)/9.78554/,
     +     B3(194)/13.4661/,A4(194)/5.29444/,B4(194)/23.9533/,
     +     C(194)/13.376/
C
      DATA ATOMID(195)/'Pa    '/,IDW(195)/91/,Z(195)/91.0/,
     +     CUF1(195)/-5.216/,CUF11(195)/12.868/,MOF1(195)/-8.960/,
     +     MOF11(195)/9.284/,A1(195)/35.8847/,B1(195)/0.547751/,
     +     A2(195)/23.2948/,B2(195)/3.41519/,A3(195)/14.1891/,
     +     B3(195)/16.9235/,A4(195)/4.17287/,B4(195)/105.251/,
     +     C(195)/13.4287/
C
      DATA ATOMID(196)/'U     '/,IDW(196)/92/,Z(196)/92.0/,
     +     CUF1(196)/-5.359/,CUF11(196)/13.409/,MOF1(196)/-10.673/,
     +     MOF11(196)/9.654/,A1(196)/36.0228/,B1(196)/0.5293/,
     +     A2(196)/23.4128/,B2(196)/3.3253/,A3(196)/14.9491/,
     +     B3(196)/16.0927/,A4(196)/4.188/,B4(196)/100.613/,
     +     C(196)/13.3966/
C
      DATA ATOMID(197)/'U+3   '/,IDW(197)/92/,Z(197)/89.0/,
     +     CUF1(197)/-5.359/,CUF11(197)/13.409/,MOF1(197)/-10.673/,
     +     MOF11(197)/9.654/,A1(197)/35.5747/,B1(197)/0.52048/,
     +     A2(197)/22.5259/,B2(197)/3.12293/,A3(197)/12.2165/,
     +     B3(197)/12.7148/,A4(197)/5.37073/,B4(197)/26.3394/,
     +     C(197)/13.3092/
C
      DATA ATOMID(198)/'U+4   '/,IDW(198)/92/,Z(198)/88.0/,
     +     CUF1(198)/-5.359/,CUF11(198)/13.409/,MOF1(198)/-10.673/,
     +     MOF11(198)/9.654/,A1(198)/35.3715/,B1(198)/0.516598/,
     +     A2(198)/22.5326/,B2(198)/3.05053/,A3(198)/12.0291/,
     +     B3(198)/12.5723/,A4(198)/4.7984/,B4(198)/23.4582/,
     +     C(198)/13.2671/
C
      DATA ATOMID(199)/'U+6   '/,IDW(199)/92/,Z(199)/86.0/,
     +     CUF1(199)/-5.359/,CUF11(199)/13.409/,MOF1(199)/-10.673/,
     +     MOF11(199)/9.654/,A1(199)/34.8509/,B1(199)/0.507079/,
     +     A2(199)/22.7584/,B2(199)/2.8903/,A3(199)/14.0099/,
     +     B3(199)/13.1767/,A4(199)/1.21457/,B4(199)/25.2017/,
     +     C(199)/13.1665/
C
      DATA ATOMID(200)/'Np    '/,IDW(200)/93/,Z(200)/93.0/,
     +     CUF1(200)/-5.529/,CUF11(200)/13.967/,MOF1(200)/-11.158/,
     +     MOF11(200)/4.1480/,A1(200)/36.1874/,B1(200)/0.511929/,
     +     A2(200)/23.5964/,B2(200)/3.25396/,A3(200)/15.6402/,
     +     B3(200)/15.3622/,A4(200)/4.1855/,B4(200)/97.4908/,
     +     C(200)/13.3573/
C
      DATA ATOMID(201)/'Np+3  '/,IDW(201)/93/,Z(201)/90.0/,
     +     CUF1(201)/-5.529/,CUF11(201)/13.967/,MOF1(201)/-11.158/,
     +     MOF11(201)/4.148/,A1(201)/35.7074/,B1(201)/0.502322/,
     +     A2(201)/22.613/,B2(201)/3.03807/,A3(201)/12.9898/,
     +     B3(201)/12.1449/,A4(201)/5.43227/,B4(201)/25.4928/,
     +     C(201)/13.2544/
C
      DATA ATOMID(202)/'Np+4  '/,IDW(202)/93/,Z(202)/89.0/,
     +     CUF1(202)/-5.529/,CUF11(202)/13.967/,MOF1(202)/-11.158/,
     +     MOF11(202)/4.148/,A1(202)/35.5103/,B1(202)/0.498626/,
     +     A2(202)/22.5787/,B2(202)/2.96627/,A3(202)/12.7766/,
     +     B3(202)/11.9484/,A4(202)/4.92159/,B4(202)/22.7502/,
     +     C(202)/13.2116/
C
      DATA ATOMID(203)/'Np+6  '/,IDW(203)/93/,Z(203)/87.0/,
     +     CUF1(203)/-5.529/,CUF11(203)/13.967/,MOF1(203)/-11.158/,
     +     MOF11(203)/4.148/,A1(203)/35.0136/,B1(203)/0.48981/,
     +     A2(203)/22.7286/,B2(203)/2.81099/,A3(203)/14.3884/,
     +     B3(203)/12.33/,A4(203)/1.75669/,B4(203)/22.6581/,
     +     C(203)/13.113/
C
      DATA ATOMID(204)/'Pu    '/,IDW(204)/94/,Z(204)/94.0/,
     +     CUF1(204)/-5.712/,CUF11(204)/14.536/,MOF1(204)/-9.725/,
     +     MOF11(204)/4.330/,A1(204)/36.5254/,B1(204)/0.499384/,
     +     A2(204)/23.8083/,B2(204)/3.26371/,A3(204)/16.7707/,
     +     B3(204)/14.9455/,A4(204)/3.47947/,B4(204)/105.98/,
     +     C(204)/13.3812/
C
      DATA ATOMID(205)/'Pu+3  '/,IDW(205)/94/,Z(205)/91.0/,
     +     CUF1(205)/-5.712/,CUF11(205)/14.536/,MOF1(205)/-9.725/,
     +     MOF11(205)/4.330/,A1(205)/35.84/,B1(205)/0.484936/,
     +     A2(205)/22.7169/,B2(205)/2.96118/,A3(205)/13.5807/,
     +     B3(205)/11.5331/,A4(205)/5.66016/,B4(205)/24.3992/,
     +     C(205)/13.1991/
C
      DATA ATOMID(206)/'Pu+4  '/,IDW(206)/94/,Z(206)/90.0/,
     +     CUF1(206)/-5.712/,CUF11(206)/14.536/,MOF1(206)/-9.725/,
     +     MOF11(206)/4.330/,A1(206)/35.6493/,B1(206)/0.481422/,
     +     A2(206)/22.646/,B2(206)/2.8902/,A3(206)/13.3595/,
     +     B3(206)/11.316/,A4(206)/5.18831/,B4(206)/21.8301/,
     +     C(206)/13.1555/
C
      DATA ATOMID(207)/'Pu+6  '/,IDW(207)/94/,Z(207)/88.0/,
     +     CUF1(207)/-5.712/,CUF11(207)/14.536/,MOF1(207)/-9.725/,
     +     MOF11(207)/4.330/,A1(207)/35.1736/,B1(207)/0.473204/,
     +     A2(207)/22.7181/,B2(207)/2.73848/,A3(207)/14.7635/,
     +     B3(207)/11.553/,A4(207)/2.28678/,B4(207)/20.9303/,
     +     C(207)/13.0582/
C
      DATA ATOMID(208)/'Am    '/,IDW(208)/95/,Z(208)/95.0/,
     +     CUF1(208)/-5.930/,CUF11(208)/15.087/,MOF1(208)/-8.926/,
     +     MOF11(208)/4.511/,A1(208)/36.6706/,B1(208)/0.483629/,
     +     A2(208)/24.0992/,B2(208)/3.20647/,A3(208)/17.3415/,
     +     B3(208)/14.3136/,A4(208)/3.49331/,B4(208)/102.273/,
     +     C(208)/13.3592/
C
      DATA ATOMID(209)/'Cm    '/,IDW(209)/96/,Z(209)/96.0/,
     +     CUF1(209)/-6.176/,CUF11(209)/15.634/,MOF1(209)/-8.416/,
     +     MOF11(209)/4.697/,A1(209)/36.6488/,B1(209)/0.465154/,
     +     A2(209)/24.4096/,B2(209)/3.08997/,A3(209)/17.399/,
     +     B3(209)/13.4346/,A4(209)/4.21665/,B4(209)/88.4834/,
     +     C(209)/13.2887/
C
      DATA ATOMID(210)/'Bk    '/,IDW(210)/97/,Z(210)/97.0/,
     +     CUF1(210)/-6.498/,CUF11(210)/16.317/,MOF1(210)/-7.990/,
     +     MOF11(210)/4.908/,A1(210)/36.7881/,B1(210)/0.451018/,
     +     A2(210)/24.7736/,B2(210)/3.04619/,A3(210)/17.8919/,
     +     B3(210)/12.8946/,A4(210)/4.23284/,B4(210)/86.003/,
     +     C(210)/13.2754/
C
      DATA ATOMID(211)/'Cf    '/,IDW(211)/98/,Z(211)/98.0/,
     +     CUF1(211)/-6.798/,CUF11(211)/16.930/,MOF1(211)/-7.683/,
     +     MOF11(211)/5.107/,A1(211)/36.9185/,B1(211)/0.437533/,
     +     A2(211)/25.1995/,B2(211)/3.00775/,A3(211)/18.3317/,
     +     B3(211)/12.4044/,A4(211)/4.24391/,B4(211)/83.7881/,
     +     C(211)/13.2674/
C
C---- tables of two gaussian approximations from agarwal paper
C
      DATA ATOMID(212)/'H    2'/,IDW(212)/1/,Z(212)/1.0/,CUF1(212)/0.0/,
     +     CUF11(212)/0.0/,MOF1(212)/0.0/,MOF11(212)/0.0/,
     +     A1(212)/0.7932/,B1(212)/24.2157/,A2(212)/0.1949/,
     +     B2(212)/2.1089/,A3(212)/0.0/,B3(212)/0.0/,A4(212)/0.0/,
     +     B4(212)/0.0/,C(212)/0.0/
C
      DATA ATOMID(213)/'C    2'/,IDW(213)/6/,Z(213)/6.0/,CUF1(213)/0.0/,
     +     CUF11(213)/0.0/,MOF1(213)/0.0/,MOF11(213)/0.0/,
     +     A1(213)/2.9972/,B1(213)/30.0167/,A2(213)/2.9791/,
     +     B2(213)/2.8886/,A3(213)/0.0/,B3(213)/0.0/,A4(213)/0.0/,
     +     B4(213)/0.0/,C(213)/0.0/
C
      DATA ATOMID(214)/'N    2'/,IDW(214)/7/,Z(214)/7.0/,CUF1(214)/0.0/,
     +     CUF11(214)/0.0/,MOF1(214)/0.0/,MOF11(214)/0.0/,
     +     A1(214)/2.9924/,B1(214)/25.3766/,A2(214)/3.9986/,
     +     B2(214)/3.5004/,A3(214)/0.0/,B3(214)/0.0/,A4(214)/0.0/,
     +     B4(214)/0.0/,C(214)/0.0/
C
      DATA ATOMID(215)/'O    2'/,IDW(215)/8/,Z(215)/8.0/,CUF1(215)/0.0/,
     +     CUF11(215)/0.0/,MOF1(215)/0.0/,MOF11(215)/0.0/,
     +     A1(215)/2.4485/,B1(215)/24.7562/,A2(215)/5.5589/,
     +     B2(215)/4.1372/,A3(215)/0.0/,B3(215)/0.0/,A4(215)/0.0/,
     +     B4(215)/0.0/,C(215)/0.0/
C
      DATA ATOMID(216)/'S    2'/,IDW(216)/16/,Z(216)/16.0/,
     +     CUF1(216)/0.0/,CUF11(216)/0.0/,MOF1(216)/0.0/,
     +     MOF11(216)/0.0/,A1(216)/5.548/,B1(216)/33.7108/,
     +     A2(216)/10.4241/,B2(216)/1.9034/,A3(216)/0.0/,B3(216)/0.0/,
     +     A4(216)/0.0/,B4(216)/0.0/,C(216)/0.0/
C     ..
C
C---- Show available atom identifiers
C
      IF (ISHOW.EQ.-1) THEN
        WRITE (LUNOUT,FMT=6000)
        WRITE (LUNOUT,FMT=6002) (ATOMID(JNUM),JNUM=1,NUMATM)
        WRITE (LUNOUT,FMT=6004)
C
C---- Create new library file
C
      ELSE IF (ISHOW.EQ.1) THEN
        OPEN (UNIT=45,FILE='ATOMSF.LIB',STATUS='NEW',
     +       CARRIAGECONTROL='LIST')
        DO 10 JNUM = 1,NUMATM
          WRITE (45,FMT=6006) ATOMID(JNUM)
          WRITE (45,FMT=6012) Z(JNUM),IDW(JNUM)
          WRITE (45,FMT=6008) A1(JNUM),B1(JNUM),A2(JNUM),B2(JNUM),
     +      A3(JNUM),B3(JNUM),A4(JNUM),B4(JNUM),C(JNUM)
          WRITE (45,FMT=6010) CUF1(JNUM),CUF11(JNUM)
          WRITE (45,FMT=6010) MOF1(JNUM),MOF11(JNUM)
   10   CONTINUE
        CLOSE (UNIT=45)
      ELSE
C
C---- Search for required identifier
C
        ID2 = ID//'  '
        IF (NG.EQ.0) NG = 2
        IF (NG.EQ.2) ID2 = ID//' 2'
C
C---- Check that identifier supplied is correct case
C
        IF (LGE(ID2(1:1), 'a') .AND. LLE(ID2(1:1), 'z'))
     +     ID2(1:1) = CHAR(ICHAR(ID2(1:1)) - 32)
        IF (LGE(ID2(2:2), 'A') .AND. LLE(ID2(2:2), 'Z'))
     +     ID2(2:2) = CHAR(ICHAR(ID2(2:2)) + 32)
C
C---- Try to match identifier
C
        DO 20 JNUM = 1,NUMATM
          IF (ID2.EQ.ATOMID(JNUM)) GO TO 30
   20   CONTINUE
C
C---- No match
C
        WRITE (LUNOUT,FMT=6014) ID
        IFLAG = -1
        RETURN
C
C---- Identifier matched
C
   30   IFLAG = 0
        ZZ = Z(JNUM)
        IATNUM = IDW(JNUM)
        SF(1) = A1(JNUM)
        SF(2) = B1(JNUM)
        SF(3) = A2(JNUM)
        SF(4) = B2(JNUM)
        SF(5) = A3(JNUM)
        SF(6) = B3(JNUM)
        SF(7) = A4(JNUM)
        SF(8) = B4(JNUM)
        SF(9) = C(JNUM)
        IF (.NOT.LELEC) THEN
          ANOM = 1.0
        ELSE IF (WAVE.GT.0.7 .AND. WAVE.LT.0.9) THEN
          ANOM = MOF11(JNUM)
        ELSE IF (WAVE.GT.1.4 .AND. WAVE.LT.1.6) THEN
          ANOM = CUF11(JNUM)
        ELSE IF (WAVE.GT.-0.00001 .AND. WAVE.LT.0.00001) THEN
          WRITE (LUNOUT,FMT=6016)
          ANOM = CUF11(JNUM)
        ELSE
          WRITE (LUNOUT,FMT=6018) WAVE
          ANOM = 1.0
        END IF
C
C
C
        WRITE (LUNOUT,FMT=6020) ID,SF
C
C
      END IF
C
C---- Format statements
C
 6000 FORMAT ('  Available atom types are ....',/)
 6002 FORMAT (8 (2X,A,2X))
 6004 FORMAT ('  For the analytical expression ...',/'  f(sin(theta)/l',
     +       'ambda) = Sum(a(i)exp(-b(i)(sin**2theta/lambda**2)) + c',
     +       /'  For a(i), b(i), c where i = 1 to 4',/)
 6006 FORMAT (A)
 6008 FORMAT (3 (2X,F14.8,2X))
 6010 FORMAT (2 (2X,F14.8,2X))
 6012 FORMAT (2X,F14.8,2X,I8)
 6014 FORMAT (1X,'**** ATOMSF ERROR ****',/' Atom Identifier not match',
     +       'ed .... ',A)
 6016 FORMAT (//' **** FOR DELTAF USING COPPER RADIATION ****')
 6018 FORMAT (//' **** WAVELENGTH OF ',F8.5,' NOT SUITABLE ')
 6020 FORMAT (2X,'Coefficients for Analytical Approximation to the',
     +       /'  Scattering Factors for atom identifier ... ',A,' are',
     +       /3 (3 (3X,F10.5),/),'  For a1, b1, a2, b2, a3, b3, a4, b4',
     +       ', c, respectively',/'  see International Tables vol4 tab',
     +       'le 2.2B')
C
C
      END
