C package AACODE
C
C Conversion procedures for the 20 amino acid codes
C
C AACOD1 cf  convert three-letter code into one-letter
C AACOD3 cf  convert one-letter code into three-letter
C AACODN if  convert one-letter code into integer code
C AACODC cf  convert integer code into one-letter code
C AACODD b   initialize residue code lists
C AACODX o   residue code data structure
C
C Uses no separately compiled procedures.
C
C (C) Copyright Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C    7-May-1987  first attempts
C    3-Jun-1987  AACODN, AACODC added
C   13-Jun-1987  residue code lists changed; relative occurence order
C   24-Jul-1987  changed order in internal conversion list
C
C
C---------------------------------------
      CHARACTER*1 FUNCTION AACOD1 (AAC3)
C
C To ensure that we initialize
C
      EXTERNAL AACODD
C
      CHARACTER*(*) AAC3
C
      COMMON /AACODX/ RCOD3, RCOD1
      SAVE /AACODX/
      CHARACTER*3 RCOD3(1:25)
      CHARACTER*1 RCOD1(1:25)
C
      INTEGER     I
C
      DO 100 I = 1, 25
        IF (AAC3.EQ.RCOD3(I)) THEN
          AACOD1 = RCOD1(I)
          RETURN
        END IF
100   CONTINUE
      AACOD1 = 'X'
      RETURN
      END
C
C---------------------------------------
      CHARACTER*3 FUNCTION AACOD3 (AAC1)
C
C To ensure that we initialize
C
      EXTERNAL AACODD
C
      CHARACTER*(*) AAC1
C
      COMMON /AACODX/ RCOD3, RCOD1
      SAVE /AACODX/
      CHARACTER*3 RCOD3(1:25)
      CHARACTER*1 RCOD1(1:25)
C
      INTEGER     I
C
      DO 100 I = 1, 20
        IF (AAC1.EQ.RCOD1(I)) THEN
          AACOD3 = RCOD3(I)
          RETURN
        END IF
100   CONTINUE
      AACOD3 = 'XXX'
      RETURN
      END
C
C-----------------------------------
      INTEGER FUNCTION AACODN (AAC1)
C
C To ensure that we initialize
C
      EXTERNAL AACODD
C
      CHARACTER*(*) AAC1
C
      COMMON /AACODX/ RCOD3, RCOD1
      SAVE /AACODX/
      CHARACTER*3 RCOD3(1:25)
      CHARACTER*1 RCOD1(1:25)
C
      INTEGER I
C
      DO 100 I = 1, 20
        IF (AAC1.EQ.RCOD1(I)) THEN
          AACODN = I
          RETURN
        END IF
100   CONTINUE
      AACODN = 0
      RETURN
      END
C
C--------------------------------------
      CHARACTER*1 FUNCTION AACODC (AAN)
C
C To ensure that we initialize
C
      EXTERNAL AACODD
C
      INTEGER AAN
C
      COMMON /AACODX/ RCOD3, RCOD1
      SAVE /AACODX/
      CHARACTER*3 RCOD3(1:25)
      CHARACTER*1 RCOD1(1:25)
C
      IF (AAN.LT.1 .OR. AAN.GT.20) THEN
        AACODC = 'X'
      ELSE
        AACODC = RCOD1(AAN)
      END IF
      RETURN
      END
C
C
C----------------------
      BLOCK DATA AACODD
C
      COMMON /AACODX/ RCOD3, RCOD1
      SAVE /AACODX/
      CHARACTER*3 RCOD3(1:25)
      CHARACTER*1 RCOD1(1:25)
C
      DATA RCOD3 /'GLY', 'PRO', 'ALA', 'VAL', 'ILE',
     $            'LEU', 'MET', 'CYS', 'SER', 'THR',
     $            'ASN', 'GLN', 'ASP', 'GLU', 'LYS',
     $            'ARG', 'HIS', 'TYR', 'PHE', 'TRP',
     $            'CPR', 'CSH', 'CYH', 'CSM', 'TRY'/
      DATA RCOD1 /'G', 'P', 'A', 'V', 'I',
     $            'L', 'M', 'C', 'S', 'T',
     $            'N', 'Q', 'D', 'E', 'K',
     $            'R', 'H', 'Y', 'F', 'W',
     $            'P', 'C', 'C', 'C', 'W'/
C
      END
C Package ANAREA
C
C Analytical accessible surface area and gradient calculation.
C
C ANAREA s  compute accessible surface and gradient analytically
C ANAREB s  compute accessible surface analytically
C ANARES s  (probably) a sort procedure
C
C Originally written by Timothy J. Richmond (MRC Lab Mol Biol,
C Hills Road, Cambridge CB2 2QH, England) as a stand-alone program.
C
C Modified by Per Kraulis (Dept Molecular Biology, Uppsala University
C Sweden) into subroutines. Code somewhat cleaned up at the same time.
C
C Copyright (C) 1983 Timothy J. Richmond
C
C Reference: Timothy J. Richmond, Solvent Accessible Surface Area and
C Excluded Volume in Proteins, J Mol Biol (1984) 178, 63-89
C
C  19-Dec-1983  last modified by TJR
C  10-Jan-1987  I/O completely rewritten, code cleaned up somewhat
C  21-Apr-1988  rewritten into subroutines
C
C
C----------------------------------------------------------------------
      SUBROUTINE ANAREA (AREA, GRAD, XYZ, SUMRAD, FLAG, TOTATM, ERRCOD)
C
      INTEGER TOTATM, ERRCOD
      REAL    AREA (TOTATM), GRAD (3, TOTATM),
     $        XYZ (3, TOTATM), SUMRAD (TOTATM)
      INTEGER FLAG (TOTATM)
C
C AREA    (Out) atom accessible area value
C GRAD    (Out) atom accessible area gradient vector
C XYZ     (In)  atom coordinates
C SUMRAD  (In)  sum of atom and solvent probe radii
C FLAG    (In)  0 = ignore 1 = include in calc, 2 = calc surface
C TOTATM  (In)  total number of atoms
C ERRCOD  (Out) error code
C     0 = no error
C    <0 = internal; connectivity error on sphere (atom) number -ERRCOD
C     1 = internal overflow; MAXOVR too small
C     2 = internal overflow; MAXPTS too small
C     3 = internal overflow; MAXARC too small
C
C
C Dimensioning parameters
C
C MAXOVR  max number of in overlapping spheres
C MAXPTS  max number of overlap end points on a circle of intersection
C
      INTEGER    MAXOVR, MAXARC, MAXPTS
      PARAMETER (MAXOVR = 200, MAXARC = MAXOVR + 1, MAXPTS = 300)
C
C Mathematical constants
C
      REAL       PI, PIX2, PIX4, PID2
      PARAMETER (PI = 3.1415926536,
     $           PIX2 = 2.0 * PI, PIX4 = 4.0 * PI, PID2 = PI / 2.0)
C
C Overlap significance (also used to test if spheres colinear)
C
      REAL       SIG, SIGSQ, DELTA
      PARAMETER (SIG = 0.01, SIGSQ = SIG **2, DELTA = 0.000001)
C
      LOGICAL LONE, LTOP
C
C Internal data and pointer arrays
C
      LOGICAL ISKIP (MAXOVR)
      INTEGER INTAG1 (MAXOVR), INTAG (MAXOVR), ITAG (MAXOVR),
     $        IDER (MAXOVR)
C
      REAL XC1 (MAXOVR), YC1 (MAXOVR), ZC1 (MAXOVR),
     $     BG (MAXOVR), THER (MAXOVR), RI (MAXOVR), RISQ (MAXOVR),
     $     B1 (MAXOVR), DSQ1 (MAXOVR), BSQ1 (MAXOVR), GR (MAXOVR),
     $     XC (MAXOVR), YC (MAXOVR), ZC (MAXOVR),
     $     UX (MAXOVR), UY (MAXOVR), UZ (MAXOVR),
     $     DSQ (MAXOVR), BSQ (MAXOVR), B (MAXOVR)
C
      REAL KENT (MAXARC), KOUT (MAXARC)
      REAL ARCI (MAXPTS), ARCF (MAXPTS), EX (MAXPTS), LT (MAXPTS)
C
      REAL AXX, AXY, AXZ, AYX, AYY, AZX, AZY, AZZ, UXL, UYL, UZL,
     $     TXB, TYB, TD, TR, TXR, TYR, TK1, TK2,
     $     THE, T, TB, TXK, TYK, TZK, T1, TI, TF, TT
      REAL ARCLEN, EXANG, XR, YR, ZR, RAD1, RAD1X2, RAD1SQ, RC,
     $     RPLUS, RMINUS, CCSQ, CC, XYSQ, BSQK, BK, GL,
     $     THERK, DK, GK, RISQK, RIK, RISQL,
     $     DSQL, ARCSUM, WXLSQ, P, S
C
C Help variables
C
      INTEGER I, J, K, L, M, N, ATOM1, ATOM2, TOTOVR,
     $        JB, IB, IO1, K1, ISKIPS, KARC, II, MI
      REAL    TX, TY, TZ, RCN, BGL, BSQL, WXL, V, DEAL, DECL, DTKAL,
     $        DTKCL, W, T2, DTLAL, DTLCL, GACA, FACA, FACB, FACC,
     $        DAX, DAY, DAZ
C
C Initialize
C
      DO 100  I = 1, MAXOVR
        IDER (I) = 0
100   CONTINUE
      DO 200 I = 1, TOTATM
        AREA (I) = 0.0
        GRAD (1, I) = 0.0
        GRAD (2, I) = 0.0
        GRAD (3, I) = 0.0
200   CONTINUE
C
C Giant loop: calculate area for each atom
C
      DO 1000 ATOM1 = 1, TOTATM
C
C Skip ATOM1 if no area is to be calculated for it
C
        IF (FLAG (ATOM1) .NE. 2) GOTO 1000
C
        JB = 0
        IB = 0
        TOTOVR = 0
        EXANG = 0.0
        ARCLEN = 0.0
        LONE = .FALSE.
C
        XR = XYZ (1, ATOM1)
        YR = XYZ (2, ATOM1)
        ZR = XYZ (3, ATOM1)
        RAD1 = SUMRAD (ATOM1)
        RAD1X2 = RAD1 * 2.0
        RAD1SQ = RAD1 ** 2
C	
C Find the ATOM2 spheres which intersect the ATOM1 sphere
C
        DO 2000 ATOM2 = 1, TOTATM
C
C Skip if ATOM2 same as ATOM1, or if ATOM2 flagged "ignore completely"
C
          IF (ATOM2 .EQ. ATOM1) GOTO 2000
          IF (FLAG (ATOM2) .EQ. 0) GOTO 2000
C
C Skip if ATOM2 does not affect ATOM1
C
          RPLUS = RAD1 + SUMRAD (ATOM2)
          TX = XYZ (1, ATOM2) - XR
          IF (ABS (TX) .GE. RPLUS) GOTO 2000
          TY = XYZ (2, ATOM2) - YR
          IF (ABS (TY) .GE. RPLUS) GOTO 2000
          TZ = XYZ (3, ATOM2) - ZR
          IF (ABS (TZ) .GE. RPLUS) GOTO 2000
C
C Check for overlap of spheres by testing center to center distance
C against sum and difference of radii
C
          XYSQ = TX**2 + TY**2
          IF (XYSQ .LT. SIGSQ) THEN
            TX = SIG
            TY = 0.0
            XYSQ = SIGSQ
          ENDIF
          CCSQ = XYSQ + TZ**2
          CC = SQRT(CCSQ)
          IF (RPLUS - CC .LE. SIG) GOTO 2000
          RMINUS = RAD1 - SUMRAD (ATOM2)
C
C Skip if Is ATOM1 sphere completely buried
C
          IF (CC - ABS(RMINUS) .LE. SIG) THEN
            IF (RMINUS .LE. 0.0) GOTO 1000
            GOTO 2000
          ENDIF
C
C Calculate overlap parameters
C
          IF (TOTOVR .GE. MAXOVR) THEN
            ERRCOD = 1
            RETURN
          END IF
C
          TOTOVR = TOTOVR + 1
          XC1 (TOTOVR) = TX
          YC1 (TOTOVR) = TY
          ZC1 (TOTOVR) = TZ
          DSQ1 (TOTOVR) = XYSQ
          BSQ1 (TOTOVR) = CCSQ
          B1 (TOTOVR) = CC
          GR (TOTOVR) = (CCSQ + RPLUS * RMINUS) / (RAD1X2 * B1(TOTOVR))
          INTAG1 (TOTOVR) = ATOM2
C
2000    CONTINUE
C
C Special cases: no overlap, one overlap only
C
        IF (TOTOVR .EQ. 0) THEN
          AREA (ATOM1) = PIX4
          GOTO 6080
C
        ELSE IF (TOTOVR .EQ. 1) THEN
          K = 1
          LONE = .TRUE.
          TXK = XC1(1)
          TYK = YC1(1)
          TZK = ZC1(1)
          BSQK = BSQ1(1)
          BK = B1(1)
          INTAG(1) = INTAG1(1)
          GOTO 5080
        ENDIF
C
C Sort ATOM2 spheres by degree of overlap with ATOM1 sphere
C
        CALL ANARES (GR, TOTOVR, ITAG)
        DO 3050 L = 1, TOTOVR
          K = ITAG(L)
          ATOM2 = INTAG1(K)
          INTAG(L) = ATOM2
          XC(L) = XC1(K)
          YC(L) = YC1(K)
          ZC(L) = ZC1(K)
          DSQ(L) = DSQ1(K)
          B(L) = B1(K)
          BSQ(L) = BSQ1(K)
          ISKIP(L) = .FALSE.
3050    CONTINUE
C
        DO 3060 L = 1, TOTOVR
          GL = GR(L) * RAD1
          BG(L) = B(L) * GL
          RISQ(L) = RAD1SQ - GL**2
          RI(L) = SQRT (RISQ(L))
C	
C Radius of the ATOM2 circle on the surface of the sphere
C	
          THER(L) = PID2 - ASIN (GR(L))
3060    CONTINUE
        IO1 = TOTOVR - 1
C
C Find boundary of inaccessible area on ATOM1 sphere
C
        DO 4020 K = 1, IO1
          IF (ISKIP(K)) GOTO 4020
          TXK = XC(K)
          TYK = YC(K)
          TZK = ZC(K)
          BK = B(K)
          THERK = THER(K)
          K1 = K + 1
          DO 4010 L = K1, TOTOVR
            IF (ISKIP(L)) GOTO 4010
C
C Is L circle intersecting K circle ?
C Distance between circle centers and sum of radii
C
            CC = (TXK * XC(L) + TYK * YC(L) + TZK * ZC(L)) / (BK * B(L))
            IF (CC .GT. 1.0) THEN
              CC = ACOS (1.0)
            ELSEIF (CC .LT. -1.0) THEN
              CC = ACOS (-1.0)
            ELSE
              CC = ACOS (CC)
            ENDIF
            TD = THERK + THER(L)
C
C Circles do not enclose separate regions ?
C
            IF (CC .LT. TD) THEN
C
C Circle L completely inside circle K ?
C
              IF (CC + THER(L) .LT. THER(K)) THEN
                ISKIP(L) = .TRUE.
C
C Circles not essentially parallel ?
C
              ELSE IF (CC .LE. SIG) THEN
                ISKIP(L) = .TRUE.
C
C Skip if ATOM1 sphere completely buried
C
              ELSE
                IF (PIX2 - CC .LE. TD) GOTO 1000
              ENDIF
            ENDIF
C
4010      CONTINUE
4020    CONTINUE
C
C Large loop through list of intersecting atom spheres
C
C Find T value of circle intersections
C
        DO 6000 K = 1, TOTOVR
C
          IF (ISKIP(K)) GOTO 6000
C
          ISKIPS = ISKIP(K)
          ISKIP(K) = .TRUE.
          KARC = 0
          LTOP = .FALSE.
          TXK = XC(K)
          TYK = YC(K)
          TZK = ZC(K)
          DK = SQRT(DSQ(K))
          BSQK = BSQ(K)
          BK = B(K)
          GK = GR(K) * RAD1
          RISQK = RISQ(K)
          RIK = RI(K)
          THERK = THER(K)
C
C Rotation matrix elements
C
          T1 = TZK / (BK * DK)
          AXX = T1 * TXK
          AXY = T1 * TYK
          AXZ = DK / BK
          AYX = TYK / DK
          AYY = TXK / DK
          AZX = TXK / BK
          AZY = TYK / BK
          AZZ = TZK / BK
C
          DO 5000 L = 1, TOTOVR
C
            IF (ISKIP(L)) GOTO 5000
C
C Rotate spheres so K vector colinear with z-axis
C
            UXL =   AXX * XC(L) + AXY * YC(L) - AXZ * ZC(L)
            UYL = - AYX * XC(L) + AYY * YC(L)
            UZL =   AZX * XC(L) + AZY * YC(L) + AZZ * ZC(L)
C
            IF (ACOS (UZL/B(L)) .GE. THERK+THER(L)) GOTO 5000
C
            GL = GR(L) * RAD1
            DSQL = UXL**2 + UYL**2
            TB = UZL * GK - BG(L)
            TXB = UXL * TB
            TYB = UYL * TB
            TD = RIK * DSQL
            TR = SQRT (RISQK * DSQL - TB**2)
            TXR = UXL * TR
            TYR = UYL * TR
C
C T values of intersection for K circle
C
            TB = (TXB + TYR) / TD
            IF (ABS (TB) .GT. 1.0) TB = SIGN (1.0, TB)
            TK1 = ACOS(TB)
            IF (TYB - TXR .LT. 0.0) TK1 = PIX2 - TK1
            TB = (TXB - TYR) / TD
            IF (ABS (TB) .GT. 1.0) TB = SIGN (1.0, TB)
            TK2 = ACOS(TB)
            IF (TYB + TXR. LT. 0.0) TK2 = PIX2 - TK2
            THE = - ACOS ((RAD1SQ * UZL - GK * BG(L)) /
     $              (RIK * RI(L) * B(L)))
C
C Is TK1 entry or exit point ?  Check T=0 point.
C TI is exit point, TF is entry point.
C
            IF (0.0 .LE. (ACOS ((UZL * GK - UXL * RIK) /
     $                   (B(L) * RAD1)) - THER(L)) * (TK2 - TK1)) THEN
              TI = TK1
              TF = TK2
            ELSE
              TI = TK2
              TF = TK1
            ENDIF
C
            KARC = KARC + 1
            IF (KARC .GE. MAXPTS) THEN
              ERRCOD = 2
              RETURN
            END IF
C
            IF (TF .LE. TI) THEN
              ARCF(KARC) = TF
              ARCI(KARC) = 0.0
              TF = PIX2
              LT(KARC) = L
              EX(KARC) = THE
              LTOP = .TRUE.
              KARC = KARC + 1
            ENDIF
C
            ARCF(KARC) = TF
            ARCI(KARC) = TI
            LT(KARC) = L
            EX(KARC) = THE
            UX(L) = UXL
            UY(L) = UYL
            UZ(L) = UZL
C
5000      CONTINUE
          ISKIP(K) = ISKIPS
C
C Special case: K circle without intersections
C
          IF (KARC .LE. 0) GOTO 5080
C
C General case: sum up arclength and set connectivity code
C
          CALL ANARES (ARCI, KARC, ITAG)
          ARCSUM = ARCI(1)
          MI = ITAG(1)
          T = ARCF(MI)
          N = MI
C
          IF (KARC .NE. 1) THEN
C
            DO 5030 J = 2, KARC
              M = ITAG(J)
              IF (T .LT. ARCI(J)) THEN
                ARCSUM = ARCSUM + ARCI(J) - T
                EXANG = EXANG + EX(N)
C
                JB = JB + 1
                IF (JB .GE. MAXARC) THEN
                  ERRCOD = 3
                  RETURN
                END IF
C
                L = LT(N)
                IDER(L) = IDER(L)+1
                KENT(JB) = L * 1024 + K
                L = LT(M)
                IDER(L) = IDER(L) + 1
                KOUT(JB) = K * 1024 + L
              ENDIF
C
              TT = ARCF(M)
              IF (TT .GE. T) THEN
                T = TT
                N = M
              END IF
C
5030        CONTINUE
          ENDIF
C
          ARCSUM = ARCSUM + PIX2 - T
C
          IF (.NOT. LTOP) THEN
            EXANG = EXANG + EX(N)
            JB = JB + 1
            L = LT(N)
            IDER(L) = IDER(L) + 1
            KENT(JB) = L * 1024 + K
            L = LT(MI)
            IDER(L) = IDER(L) + 1
            KOUT(JB) = K * 1024 + L
          ENDIF
C
          DO 5060 L = 1, TOTOVR
C
            IF (IDER(L) .EQ. 0) GOTO 5060
C
            RCN = IDER(L) * RAD1SQ
            IDER(L) = 0
            UZL = UZ(L)
            GL = GR(L) * RAD1
            BGL = BG(L)
            BSQL = BSQ(L)
            RISQL = RISQ(L)
            WXLSQ = BSQL - UZL**2
            WXL = SQRT (WXLSQ)
            P = BGL - GK * UZL
            V = RISQK * WXLSQ - P**2
            IF (V. LT. DELTA) V = DELTA
            V = SQRT (V)
            T1 = RAD1 * (GK * (BGL - BSQL) + UZL * (BGL - RAD1SQ)) /
     $                  (V * RISQL * BSQL)
            DEAL = - WXL * T1
            DECL = - UZL * T1 - RAD1 / V
            DTKAL = (WXLSQ - P) / (WXL * V)
            DTKCL = (UZL - GK) / V
            S = GK * B(L) - GL * UZL
            W = WXLSQ * RISQL - S**2
            IF (W .LT. DELTA) W = DELTA
            W = SQRT (W)
            T1 = 2.0 * GK - UZL
            T2 = RAD1SQ - BGL
            DTLAL = - (RISQL * WXLSQ * B(L) * T1 -
     $              S * (WXLSQ * T2 + RISQL * BSQL)) /
     $              (RISQL * WXL * BSQL * W)
            DTLCL = - (RISQL * B(L) * (UZL * T1 - BGL)
     $              - UZL * T2 * S) / (RISQL * BSQL * W)
            GACA = RCN * (DEAL - (GK * DTKAL - GL * DTLAL) / RAD1) / WXL
            FACA = UX(L) * GACA
            FACB = UY(L) * GACA
            FACC = RCN * (DECL - (GK * DTKCL - GL * DTLCL) / RAD1)
            DAX = AXX * FACA - AYX * FACB + AZX * FACC
            DAY = AXY * FACA + AYY * FACB + AZY * FACC
            DAZ = AZZ * FACC - AXZ * FACA
            ATOM2 = INTAG(L)
            GRAD (1, ATOM1) = GRAD (1, ATOM1) + DAX
            GRAD (2, ATOM1) = GRAD (2, ATOM1) + DAY
            GRAD (3, ATOM1) = GRAD (3, ATOM1) + DAZ
            GRAD (1, ATOM2) = GRAD (1, ATOM2) - DAX
            GRAD (2, ATOM2) = GRAD (2, ATOM2) - DAY
            GRAD (3, ATOM2) = GRAD (3, ATOM2) - DAZ
C
5060      CONTINUE
C
          GOTO 5090
C
C Jump-in point from other GOTO statements
C
5080      ARCSUM = PIX2
          IB = IB + 1
C
5090      ARCLEN = ARCLEN + GR(K) * ARCSUM
          ATOM2 = INTAG(K)
          T1 = ARCSUM * RAD1SQ * (BSQK - RAD1SQ + SUMRAD(ATOM2) **2) /
     $         (RAD1X2 * BSQK * BK)
          GRAD (1, ATOM1) = GRAD (1, ATOM1) - T1 * TXK
          GRAD (2, ATOM1) = GRAD (2, ATOM1) - T1 * TYK
          GRAD (3, ATOM1) = GRAD (3, ATOM1) - T1 * TZK
          GRAD (1, ATOM2) = GRAD (1, ATOM2) + T1 * TXK
          GRAD (2, ATOM2) = GRAD (2, ATOM2) + T1 * TYK
          GRAD (3, ATOM2) = GRAD (3, ATOM2) + T1 * TZK
C
          IF (LONE) GOTO 6070
C
C End large loop
C
6000    CONTINUE
C
        IF (ARCLEN .EQ. 0.0) GOTO 1000
        IF (JB .EQ. 0) GOTO 6070
C
C Find number of independent boundaries
C
        J = 0
        DO 6050 K = 1, JB
C
          IF (KOUT(K) .EQ. 0) GOTO 6050
C
          I = K
6030      N = KOUT(I)
          KOUT(I) = 0
          J = J + 1
C
          DO 6040 II = 1, JB
            IF (N .EQ. KENT(II)) THEN
              IF (II .NE. K) THEN
                I = II
                GOTO 6030
              ENDIF
              IB = IB + 1
              IF (J .EQ. JB) GOTO 6070
              GOTO 6050
              END IF
6040        CONTINUE
6050      CONTINUE
C
C Needed only if not returning at error /PK
C
          IB = IB +1
C
          ERRCOD = - ATOM1
          RETURN
C
6070      AREA (ATOM1) = IB * PIX2 + EXANG + ARCLEN
          AREA (ATOM1) = MOD (AREA (ATOM1), PIX4)
6080      AREA (ATOM1) = RAD1SQ * AREA (ATOM1)
C
C End of giant atom loop
C
1000  CONTINUE
C
      ERRCOD = 0
      RETURN
      END
C
C
C----------------------------------------------------------------
      SUBROUTINE ANAREB (AREA, XYZ, SUMRAD, FLAG, TOTATM, ERRCOD)
C
      INTEGER TOTATM, ERRCOD
      REAL    AREA (TOTATM), XYZ (3, TOTATM), SUMRAD (TOTATM)
      INTEGER FLAG (TOTATM)
C
C AREA    (Out) atom accessible area value
C XYZ     (In)  atom coordinates
C SUMRAD  (In)  sum of atom and solvent probe radii
C FLAG    (In)  0 = ignore 1 = include in calc, 2 = calc surface
C TOTATM  (In)  total number of atoms
C ERRCOD  (Out) error code
C     0 = no error
C    <0 = internal; connectivity error on atom number -ERRCOD
C     1 = internal overflow; MAXOVR too small
C     2 = internal overflow; MAXPTS too small
C     3 = internal overflow; MAXARC too small
C
C
C Dimensioning parameters
C
C MAXOVR  max number of in overlapping spheres
C MAXPTS  max number of overlap end points on a circle of intersection
C
      INTEGER    MAXOVR, MAXARC, MAXPTS
      PARAMETER (MAXOVR = 200, MAXARC = MAXOVR + 1, MAXPTS = 300)
C
C Mathematical constants
C
      REAL       PI, PIX2, PIX4, PID2
      PARAMETER (PI = 3.1415926536,
     $           PIX2 = 2.0 * PI, PIX4 = 4.0 * PI, PID2 = PI / 2.0)
C
C Overlap significance (also used to test if spheres colinear)
C
      REAL       SIG, SIGSQ
      PARAMETER (SIG = 0.01, SIGSQ = SIG **2)
C
      LOGICAL LONE, LTOP
C
C Internal data and pointer arrays
C
      LOGICAL ISKIP (MAXOVR)
      INTEGER INTAG1 (MAXOVR), INTAG (MAXOVR), ITAG (MAXOVR),
     $        IDER (MAXOVR)
C
      REAL XC1 (MAXOVR), YC1 (MAXOVR), ZC1 (MAXOVR),
     $     BG (MAXOVR), THER (MAXOVR), RI (MAXOVR), RISQ (MAXOVR),
     $     B1 (MAXOVR), DSQ1 (MAXOVR), BSQ1 (MAXOVR), GR (MAXOVR),
     $     XC (MAXOVR), YC (MAXOVR), ZC (MAXOVR),
     $     UX (MAXOVR), UY (MAXOVR), UZ (MAXOVR),
     $     DSQ (MAXOVR), BSQ (MAXOVR), B (MAXOVR)
C
      REAL KENT (MAXARC), KOUT (MAXARC)
      REAL ARCI (MAXPTS), ARCF (MAXPTS), EX (MAXPTS), LT (MAXPTS)
C
      REAL TXB, TYB, TD, TXR, TYR, TK1, TK2,
     $     THE, T, TB, TXK, TYK, TZK, TI, TF,
     $     ARCLEN, ARCSUM, EXANG, THERK, DK, GK, RISQK, RIK,
     $     RSUM, RDIFF, CCSQ, CC, XYSQ, BSQK, DSQL, BK, GL
C
C Help variables
C
      INTEGER I, J, K, L, M, N, ATOM1, ATOM2, TOTOVR,
     $        JB, BOUND, K1, ISKIPS, KARC, MI
      REAL    RTMP, XCUR, YCUR, ZCUR, RCUR, RCURX2, RCURSQ,
     $        TX, TY, TZ, UXL, UYL, UZL,
     $        ROTXX, ROTXY, ROTXZ, ROTYX, ROTYY, ROTZX, ROTZY, ROTZZ
C
C Initialize
C
      DO 100  I = 1, MAXOVR
        IDER (I) = 0
100   CONTINUE
      DO 200 I = 1, TOTATM
        AREA (I) = 0.0
200   CONTINUE
C
C Giant loop: calculate area for each atom
C
      DO 1000 ATOM1 = 1, TOTATM
C
C Skip ATOM1 if no area is to be calculated for it
C
        IF (FLAG (ATOM1) .NE. 2) GOTO 1000
C
        JB = 0
        BOUND = 0
        TOTOVR = 0
        EXANG = 0.0
        ARCLEN = 0.0
        LONE = .FALSE.
C
        XCUR = XYZ (1, ATOM1)
        YCUR = XYZ (2, ATOM1)
        ZCUR = XYZ (3, ATOM1)
        RCUR = SUMRAD (ATOM1)
        RCURX2 = RCUR * 2.0
        RCURSQ = RCUR ** 2
C	
C Find the ATOM2 spheres which intersect the ATOM1 sphere
C
        DO 2000 ATOM2 = 1, TOTATM
C
C Skip if ATOM2 same as ATOM1, or if ATOM2 is to be ignored
C
          IF (ATOM2 .EQ. ATOM1) GOTO 2000
          IF (FLAG (ATOM2) .EQ. 0) GOTO 2000
C
C Skip if ATOM2 does not affect ATOM1
C
          RSUM = RCUR + SUMRAD (ATOM2)
          TX = XYZ (1, ATOM2) - XCUR
          IF (ABS (TX) .GE. RSUM) GOTO 2000
          TY = XYZ (2, ATOM2) - YCUR
          IF (ABS (TY) .GE. RSUM) GOTO 2000
          TZ = XYZ (3, ATOM2) - ZCUR
          IF (ABS (TZ) .GE. RSUM) GOTO 2000
C
C Check for overlap of spheres by testing center to center distance
C against sum and difference of radii
C
          XYSQ = TX **2 + TY **2
          IF (XYSQ .LT. SIGSQ) THEN
            TX = SIG
            TY = 0.0
            XYSQ = SIGSQ
          ENDIF
          CCSQ = XYSQ + TZ **2
          CC = SQRT (CCSQ)
          IF (RSUM - CC .LE. SIG) GOTO 2000
          RDIFF = RCUR - SUMRAD (ATOM2)
C
C Skip if ATOM1 sphere is completely buried
C
          IF (CC - ABS (RDIFF) .LE. SIG) THEN
            IF (RDIFF .LE. 0.0) THEN
              GOTO 1000
            ELSE
              GOTO 2000
            END IF
          ENDIF
C
C Calculate overlap parameters
C
          IF (TOTOVR .GE. MAXOVR) THEN
            ERRCOD = 1
            RETURN
          END IF
C
          TOTOVR = TOTOVR + 1
          XC1 (TOTOVR) = TX
          YC1 (TOTOVR) = TY
          ZC1 (TOTOVR) = TZ
          DSQ1 (TOTOVR) = XYSQ
          BSQ1 (TOTOVR) = CCSQ
          B1 (TOTOVR) = CC
          GR (TOTOVR) = (CCSQ + RSUM * RDIFF) / (RCURX2 * B1(TOTOVR))
          INTAG1 (TOTOVR) = ATOM2
C
2000    CONTINUE
C
C Special case: no overlap
C
        IF (TOTOVR .EQ. 0) THEN
          AREA (ATOM1) = RCURSQ * PIX4
          GOTO 1000
C
C Special case: one overlap only
C
        ELSE IF (TOTOVR .EQ. 1) THEN
          K = 1
          LONE = .TRUE.
          TXK = XC1(1)
          TYK = YC1(1)
          TZK = ZC1(1)
          BSQK = BSQ1(1)
          BK = B1(1)
          INTAG(1) = INTAG1(1)
          GOTO 5080
        ENDIF
C
C Sort ATOM2 spheres by degree of overlap with ATOM1 sphere
C
        CALL ANARES (GR, TOTOVR, ITAG)
        DO 3050 L = 1, TOTOVR
          K = ITAG(L)
          ATOM2 = INTAG1(K)
          INTAG(L) = ATOM2
          XC(L) = XC1(K)
          YC(L) = YC1(K)
          ZC(L) = ZC1(K)
          DSQ(L) = DSQ1(K)
          B(L) = B1(K)
          BSQ(L) = BSQ1(K)
          ISKIP(L) = .FALSE.
3050    CONTINUE
C
        DO 3060 L = 1, TOTOVR
          GL = GR(L) * RCUR
          BG(L) = B(L) * GL
          RISQ(L) = RCURSQ - GL**2
          RI(L) = SQRT (RISQ(L))
C	
C Radius of the ATOM2 circle on the surface of the sphere
C	
          THER(L) = PID2 - ASIN (GR(L))
3060    CONTINUE
C
C Find boundary of inaccessible area on ATOM1 sphere
C
        DO 4020 K = 1, TOTOVR - 1
          IF (ISKIP(K)) GOTO 4020
          TXK = XC(K)
          TYK = YC(K)
          TZK = ZC(K)
          BK = B(K)
          THERK = THER(K)
          K1 = K + 1
          DO 4010 L = K1, TOTOVR
            IF (ISKIP(L)) GOTO 4010
C
C Is L circle intersecting K circle ?
C Distance between circle centers and sum of radii
C
            CC = (TXK * XC(L) + TYK * YC(L) + TZK * ZC(L)) / (BK * B(L))
            IF (CC .GT. 1.0) THEN
              CC = ACOS (1.0)
            ELSEIF (CC .LT. -1.0) THEN
              CC = ACOS (-1.0)
            ELSE
              CC = ACOS (CC)
            ENDIF
            TD = THERK + THER(L)
C
C Circles do not enclose separate regions ?
C
            IF (CC .LT. TD) THEN
C
C Circle L completely inside circle K ?
C
              IF (CC + THER(L) .LT. THER(K)) THEN
                ISKIP(L) = .TRUE.
C
C Circles not essentially parallel ?
C
              ELSE IF (CC .LE. SIG) THEN
                ISKIP(L) = .TRUE.
C
C Skip if ATOM1 sphere completely buried
C
              ELSE
                IF (PIX2 - CC .LE. TD) GOTO 1000
              ENDIF
            ENDIF
C
4010      CONTINUE
4020    CONTINUE
C
C Large loop through list of intersecting atom spheres
C
C Find T value of circle intersections
C
        DO 6000 ATOM2 = 1, TOTOVR
C
          IF (ISKIP(ATOM2)) GOTO 6000
C
          ISKIPS = ISKIP(ATOM2)
          ISKIP(ATOM2) = .TRUE.
          KARC = 0
          LTOP = .FALSE.
          TXK = XC(ATOM2)
          TYK = YC(ATOM2)
          TZK = ZC(ATOM2)
          BK = B(ATOM2)
          RIK = RI(ATOM2)
          BSQK = BSQ(ATOM2)
          RISQK = RISQ(ATOM2)
          THERK = THER(ATOM2)
          GK = GR(ATOM2) * RCUR
          DK = SQRT (DSQ(ATOM2))
C
C Rotation matrix elements
C
          RTMP = TZK / (BK * DK)
          ROTXX = RTMP * TXK
          ROTXY = RTMP * TYK
          ROTXZ = DK / BK
          ROTYX = TYK / DK
          ROTYY = TXK / DK
          ROTZX = TXK / BK
          ROTZY = TYK / BK
          ROTZZ = TZK / BK
C
          DO 5000 L = 1, TOTOVR
C
            IF (ISKIP(L)) GOTO 5000
C
C Rotate spheres so ATOM2 vector colinear with z-axis
C
            UXL =   ROTXX * XC(L) + ROTXY * YC(L) - ROTXZ * ZC(L)
            UYL = - ROTYX * XC(L) + ROTYY * YC(L)
            UZL =   ROTZX * XC(L) + ROTZY * YC(L) + ROTZZ * ZC(L)
C
            IF (ACOS (UZL/B(L)) .GE. THERK+THER(L)) GOTO 5000
C
            GL = GR(L) * RCUR
            DSQL = UXL**2 + UYL**2
            TB = UZL * GK - BG(L)
            TXB = UXL * TB
            TYB = UYL * TB
            TD = RIK * DSQL
            RTMP = SQRT (RISQK * DSQL - TB**2)
            TXR = UXL * RTMP
            TYR = UYL * RTMP
C
C T values of intersection for ATOM2 circle
C
            TB = (TXB + TYR) / TD
            IF (ABS (TB) .GT. 1.0) TB = SIGN (1.0, TB)
            TK1 = ACOS(TB)
            IF (TYB - TXR .LT. 0.0) TK1 = PIX2 - TK1
            TB = (TXB - TYR) / TD
            IF (ABS (TB) .GT. 1.0) TB = SIGN (1.0, TB)
            TK2 = ACOS(TB)
            IF (TYB + TXR. LT. 0.0) TK2 = PIX2 - TK2
            THE = - ACOS ((RCURSQ * UZL - GK * BG(L)) /
     $              (RIK * RI(L) * B(L)))
C
C Is TK1 entry or exit point ?  Check T=0 point.
C TI is exit point, TF is entry point.
C
            IF (0.0 .LE. (ACOS ((UZL * GK - UXL * RIK) /
     $                   (B(L) * RCUR)) - THER(L)) * (TK2 - TK1)) THEN
              TI = TK1
              TF = TK2
            ELSE
              TI = TK2
              TF = TK1
            ENDIF
C
            KARC = KARC + 1
            IF (KARC .GE. MAXPTS) THEN
              ERRCOD = 2
              RETURN
            END IF
C
            IF (TF .LE. TI) THEN
              ARCF(KARC) = TF
              ARCI(KARC) = 0.0
              TF = PIX2
              LT(KARC) = L
              EX(KARC) = THE
              LTOP = .TRUE.
              KARC = KARC + 1
            ENDIF
C
            ARCF(KARC) = TF
            ARCI(KARC) = TI
            LT(KARC) = L
            EX(KARC) = THE
            UX(L) = UXL
            UY(L) = UYL
            UZ(L) = UZL
C
5000      CONTINUE
          ISKIP(ATOM2) = ISKIPS
C
C Special case: ATOM2 circle without intersections
C
          IF (KARC .LE. 0) GOTO 5080
C
C General case: sum up arclength and set connectivity code
C
          CALL ANARES (ARCI, KARC, ITAG)
          ARCSUM = ARCI(1)
          MI = ITAG(1)
          T = ARCF(MI)
          N = MI
C
          IF (KARC .NE. 1) THEN
C
            DO 5030 J = 2, KARC
              M = ITAG(J)
              IF (T .LT. ARCI(J)) THEN
                ARCSUM = ARCSUM + ARCI(J) - T
                EXANG = EXANG + EX(N)
C
                JB = JB + 1
                IF (JB .GE. MAXARC) THEN
                  ERRCOD = 3
                  RETURN
                END IF
C
                L = LT(N)
                IDER(L) = IDER(L)+1
                KENT(JB) = L * 1024 + ATOM2
                L = LT(M)
                IDER(L) = IDER(L) + 1
                KOUT(JB) = ATOM2 * 1024 + L
              ENDIF
C
              IF (ARCF(M) .GE. T) THEN
                T = ARCF(M)
                N = M
              END IF
C
5030        CONTINUE
          ENDIF
C
          ARCSUM = ARCSUM + PIX2 - T
C
          IF (.NOT. LTOP) THEN
            EXANG = EXANG + EX(N)
            JB = JB + 1
            L = LT(N)
            IDER(L) = IDER(L) + 1
            KENT(JB) = L * 1024 + ATOM2
            L = LT(MI)
            IDER(L) = IDER(L) + 1
            KOUT(JB) = ATOM2 * 1024 + L
          ENDIF
C
          GOTO 5090
C
C Jump-in point from other GOTO statements
C
5080      ARCSUM = PIX2
          BOUND = BOUND + 1
C
5090      ARCLEN = ARCLEN + GR(ATOM2) * ARCSUM
C
          IF (LONE) GOTO 6070
C
C End large loop
C
6000    CONTINUE
C
        IF (ARCLEN .EQ. 0.0) GOTO 1000
        IF (JB .EQ. 0) GOTO 6070
C
C Find number of independent boundaries
C
        J = 0
        DO 6050 K = 1, JB
C
          IF (KOUT(K) .EQ. 0) GOTO 6050
C
          I = K
6030      N = KOUT(I)
          KOUT(I) = 0
          J = J + 1
C
          DO 6040 L = 1, JB
            IF (N .EQ. KENT(L)) THEN
              IF (L .NE. K) THEN
                I = L
                GOTO 6030
              ENDIF
              BOUND = BOUND + 1
              IF (J .EQ. JB) THEN
                GOTO 6070
              ELSE
                GOTO 6050
              END IF
            END IF
6040      CONTINUE
6050    CONTINUE
C
C If we get here, then connectivity error on sphere
C
        ERRCOD = - ATOM1
        RETURN
C
6070    AREA (ATOM1) = RCURSQ * MOD (BOUND*PIX2+EXANG+ARCLEN, PIX4)
C
C End of giant ATOM1 loop
C
1000  CONTINUE
C
      ERRCOD = 0
      RETURN
      END
C
C
C--------------------------------
      SUBROUTINE ANARES (A,N,TAG)
C
C I don't dare touch this terrible GOTO spaghetti. / Per Kraulis
C
      INTEGER TAG, TG
      DIMENSION A(N), IU(16), IL(16), TAG(N)
C
      DO 1 I = 1, N
          TAG(I) = I
  1   CONTINUE
      M = 1
      I = 1
      J = N
  5   IF (I .GE. J) GOTO 70
 10   K = I
      IJ = (J + I) / 2
      T = A(IJ)
      IF (A(I) .LE. T) GOTO 20
      A(IJ) = A(I)
      A(I) = T
      T = A(IJ)
      TG = TAG(IJ)
      TAG(IJ) = TAG(I)
      TAG(I) = TG
 20   L = J
      IF (A(J) .GE. T) GOTO 40
      A(IJ) = A(J)
      A(J) = T
      T = A(IJ)
      TG = TAG(IJ)
      TAG(IJ) = TAG(J)
      TAG(J) = TG
      IF (A(I) .LE. T) GOTO 40
      A(IJ) = A(I)
      A(I) = T
      T = A(IJ)
      TG = TAG(IJ)
      TAG(IJ) = TAG(I)
      TAG(I) = TG
      GOTO 40
 30   A(L) = A(K)
      A(K) = TT
      TG = TAG(L)
      TAG(L) = TAG(K)
      TAG(K) = TG
 40   L = L - 1
      IF (A(L) .GT. T) GOTO 40
      TT = A(L)
 50   K = K + 1
      IF (A(K) .LT. T) GOTO 50
      IF (K .LE. L) GOTO 30
      IF (L - I .LE. J - K) GOTO 60
      IL(M) = I
      IU(M) = L
      I = K
      M = M + 1
      GOTO 80
 60   IL(M) = K
      IU(M) = J
      J = L
      M = M + 1
      GOTO 80
 70   M = M - 1
      IF (M .EQ. 0) RETURN
      I = IL(M)
      J = IU(M)
 80   IF (J - I .GE. 1) GOTO 10
      IF (I .EQ. 1) GOTO 5
      I = I - 1
 90   I = I + 1
      IF (I .EQ. J) GOTO 70
      T = A (I + 1)
      IF (A(I) .LE. T) GOTO 90
      TG = TAG (I + 1)
      K = I
 100  A (K + 1) = A(K)
      TAG (K + 1) = TAG(K)
      K = K - 1
      IF (T .LT. A(K)) GOTO 100
      A (K + 1) = T
      TAG (K + 1) = TG
      GOTO 90
      END
C Package C2DTRF
C
C 2D coordinate transformations.
C
C C2INIT s  initialize current transformation matrix
C C2ROTD s  rotate in degrees
C C2TRAN s  translate
C C2SCAL s  scale
C C2RMTX s  return current transformation matrix
C C2SMTX s  set current transformation matrix
C C2TRFC s  transform coordinate
C C2CONC hs concatenate matrix to current transformation matrix
C C2COPY hs copy matrix
C C2UNIT hs set matrix to unity
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  26-Jul-1988  written
C   7-May-1989  modified C2TRFC
C  15-Sep-1989  corrected bug involving matrix index order and rotation
C
C Uses no separately compiled procedures.
C
C
C----------------------
      SUBROUTINE C2INIT
C
C Package C2DTRF
C
C 2D coordinate transformations.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  26-Jul-1988  first attempts
C
C C2DMTX  current transformation matrix
C
      REAL C2DMTX (3, 3)
C
      COMMON /C2DAT1/ C2DMTX
      SAVE   /C2DAT1/
C
      CALL C2UNIT (C2DMTX)
      RETURN
      END
C
C
C-----------------------------
      SUBROUTINE C2ROTD (ROTD)
C
      REAL ROTD
C
C ROTD  (In)  rotation in degrees, positive counterclockwise
C
C Conversion factor to radians
C
      REAL       TORAD
      PARAMETER (TORAD = 3.1415926 / 180.0)
C
      REAL TMP (3, 3)
C
      CALL C2UNIT (TMP)
      TMP (1, 1) = COS (ROTD * TORAD)
      TMP (2, 2) = TMP (1, 1)
      TMP (1, 2) = SIN (ROTD * TORAD)
      TMP (2, 1) = - TMP (1, 2)
      CALL C2CONC (TMP)
      RETURN
      END
C
C
C-----------------------------
      SUBROUTINE C2TRAN (X, Y)
C
      REAL X, Y
C
C X, Y  (In) translation amount in x and y
C
      REAL TMP (3, 3)
C
      CALL C2UNIT (TMP)
      TMP (3, 1) = X
      TMP (3, 2) = Y
      CALL C2CONC (TMP)
      RETURN
      END
C
C
C-------------------------------
      SUBROUTINE C2SCAL (SX, SY)
C
      REAL SX, SY
C
C SX, SY  (In) scale values in x and y
C
      REAL TMP (3, 3)
C
      CALL C2UNIT (TMP)
      TMP (1, 1) = SX
      TMP (2, 2) = SY
      CALL C2CONC (TMP)
      RETURN
      END
C
C
C----------------------------
      SUBROUTINE C2RMTX (MTX)
C
C Package C2DTRF
C
C 2D coordinate transformations.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  26-Jul-1988  first attempts
C
C C2DMTX  current transformation matrix
C
      REAL C2DMTX (3, 3)
C
      COMMON /C2DAT1/ C2DMTX
      SAVE   /C2DAT1/
C
      REAL MTX (3, 3)
C
C MTX  (Out) matrix to return current transformation matrix in
C
      CALL C2COPY (MTX, C2DMTX)
      RETURN
      END
C
C
C----------------------------
      SUBROUTINE C2SMTX (MTX)
C
C Package C2DTRF
C
C 2D coordinate transformations.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  26-Jul-1988  first attempts
C
C C2DMTX  current transformation matrix
C
      REAL C2DMTX (3, 3)
C
      COMMON /C2DAT1/ C2DMTX
      SAVE   /C2DAT1/
C
      REAL MTX (3, 3)
C
C MTX   (In)  matrix to set as current transformation matrix
C
      CALL C2COPY (C2DMTX, MTX)
      RETURN
      END
C
C
C----------------------------
      SUBROUTINE C2TRFC (COO)
C
C Package C2DTRF
C
C 2D coordinate transformations.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  26-Jul-1988  first attempts
C
C C2DMTX  current transformation matrix
C
      REAL C2DMTX (3, 3)
C
      COMMON /C2DAT1/ C2DMTX
      SAVE   /C2DAT1/
C
      REAL COO (2)
C
C COO   (InOut) coordinates to transform
C
      REAL TX, TY
C
      TX = COO (1)
      TY = COO (2)
      COO (1) = TX * C2DMTX (1,1) + TY * C2DMTX (2,1) + C2DMTX (3,1)
      COO (2) = TX * C2DMTX (1,2) + TY * C2DMTX (2,2) + C2DMTX (3,2)
      RETURN
      END
C
C
C----------------------------
      SUBROUTINE C2CONC (MTX)
C
C Package C2DTRF
C
C 2D coordinate transformations.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  26-Jul-1988  first attempts
C
C C2DMTX  current transformation matrix
C
      REAL C2DMTX (3, 3)
C
      COMMON /C2DAT1/ C2DMTX
      SAVE   /C2DAT1/
C
      REAL MTX (3, 3)
C
C MTX  (In) matrix to concatenate to current transformation matrix
C
      INTEGER I, J
      REAL    TMP (3, 3)
C
      CALL C2COPY (TMP, C2DMTX)
      DO 100 I = 1, 3
        DO 100 J = 1, 3
          C2DMTX (I, J) = TMP (I, 1) * MTX (1, J) +
     $                    TMP (I, 2) * MTX (2, J) +
     $                    TMP (I, 3) * MTX (3, J)
100   CONTINUE
      RETURN
      END
C
C
C-----------------------------------
      SUBROUTINE C2COPY (MTX1, MTX2)
C
      REAL MTX1 (3, 3), MTX2 (3, 3)
C
C MTX1  (Out) matrix to copy to
C MTX2  (In)  matrix to copy from
C
      INTEGER I, J
C
      DO 100 I = 1, 3
        DO 100 J = 1, 3
          MTX1 (I, J) = MTX2 (I, J)
100   CONTINUE
      RETURN
      END
C
C
C----------------------------
      SUBROUTINE C2UNIT (MTX)
C
      REAL MTX (3, 3)
C
C MTX  (Out) matrix to set to unity
C
      MTX (1, 1) = 1.0
      MTX (1, 2) = 0.0
      MTX (1, 3) = 0.0
      MTX (2, 1) = 0.0
      MTX (2, 2) = 1.0
      MTX (2, 3) = 0.0
      MTX (3, 1) = 0.0
      MTX (3, 2) = 0.0
      MTX (3, 3) = 1.0
      RETURN
      END
C Package C3DTRF
C
C 3D coordinate transformations.
C
C C3INIT s  initialize current matrix
C C3ROTX s  rotate around x axis in degrees
C C3ROTY s  rotate around y axis in degrees
C C3ROTZ s  rotate around z axis in degrees
C C3TRAN s  translate
C C3SCAL s  scale
C C3LOOK s  viewing transformation
C C3OPRJ s  orthographic projection transformation
C C3WPRJ s  window perspective projection transformation
C C3RMTX s  return current transformation matrix
C C3SMTX s  set current transformation matrix
C C3TRFC s  transform coordinate
C C3PUSH s  push current matrix onto stack
C C3POP  s  pop off current matrix from stack
C C3CONC hs concatenate matrix to current matrix
C C3COPY hs copy matrix
C C3UNIT hs set matrix to unity
C
C Copyright (C) 1989 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  15-Sep-1989  first attempts
C  14-Dec-1990  introduced matrix stack
C
C Uses no separately compiled procedures.
C
C
C------------------------------
      SUBROUTINE C3INIT (STACK)
C
C Package C3DTRF
C
C 3D coordinate transformations.
C
C Copyright (C) 1989 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden
C  15-Sep-1988  first attempts
C
      INTEGER    MAXSTK
      PARAMETER (MAXSTK = 8)
C
C C3DSTK  current stack slot
C C3DMTX  current transformation matrix
C
      INTEGER C3DSTK
      REAL    C3DMTX (4, 4, MAXSTK)
C
      COMMON /C3DAT1/ C3DSTK, C3DMTX
      SAVE   /C3DAT1/
C
      LOGICAL STACK
C
C STACK  (In) init matrix stack also
C
      IF (STACK) C3DSTK = 1
      CALL C3UNIT (C3DMTX (1, 1, C3DSTK))
      RETURN
      END
C
C
C-----------------------------
      SUBROUTINE C3ROTX (ROTD)
C
      REAL ROTD
C
C ROTD   (In) rotation around x axis in degrees,
C             positive counterclockwise looking at origin
C
C Conversion factor to radians
C
      REAL       TORAD
      PARAMETER (TORAD = 3.1415926 / 180.0)
C
      REAL TMP (4, 4)
C
      CALL C3UNIT (TMP)
      TMP (2, 2) = COS (ROTD * TORAD)
      TMP (3, 3) = TMP (2, 2)
      TMP (2, 3) = SIN (ROTD * TORAD)
      TMP (3, 2) = - TMP (2, 3)
      CALL C3CONC (TMP)
      RETURN
      END
C
C
C-----------------------------
      SUBROUTINE C3ROTY (ROTD)
C
      REAL ROTD
C
C ROTD   (In) rotation around y axis in degrees,
C             positive counterclockwise looking at origin
C
C Conversion factor to radians
C
      REAL       TORAD
      PARAMETER (TORAD = 3.1415926 / 180.0)
C
      REAL TMP (4, 4)
C
      CALL C3UNIT (TMP)
      TMP (1, 1) = COS (ROTD * TORAD)
      TMP (3, 3) = TMP (1, 1)
      TMP (3, 1) = SIN (ROTD * TORAD)
      TMP (1, 3) = - TMP (3, 1)
      CALL C3CONC (TMP)
      RETURN
      END
C
C
C-----------------------------
      SUBROUTINE C3ROTZ (ROTD)
C
      REAL ROTD
C
C ROTD   (In) rotation around z axis in degrees,
C             positive counterclockwise looking at origin
C
C Conversion factor to radians
C
      REAL       TORAD
      PARAMETER (TORAD = 3.1415926 / 180.0)
C
      REAL TMP (4, 4)
C
      CALL C3UNIT (TMP)
      TMP (1, 1) = COS (ROTD * TORAD)
      TMP (2, 2) = TMP (1, 1)
      TMP (1, 2) = SIN (ROTD * TORAD)
      TMP (2, 1) = - TMP (1, 2)
      CALL C3CONC (TMP)
      RETURN
      END
C
C
C--------------------------------
      SUBROUTINE C3TRAN (X, Y, Z)
C
      REAL X, Y, Z
C
C X, Y, Z  (In) translation amount in x, y and z
C
      REAL TMP (4, 4)
C
      CALL C3UNIT (TMP)
      TMP (4, 1) = X
      TMP (4, 2) = Y
      TMP (4, 3) = Z
      CALL C3CONC (TMP)
      RETURN
      END
C
C
C-----------------------------------
      SUBROUTINE C3SCAL (SX, SY, SZ)
C
      REAL SX, SY, SZ
C
C SX, SY, SZ  (In) scale values in x, y and z
C
      REAL TMP (4, 4)
C
      CALL C3UNIT (TMP)
      TMP (1, 1) = SX
      TMP (2, 2) = SY
      TMP (3, 3) = SZ
      CALL C3CONC (TMP)
      RETURN
      END
C
C
C------------------------------------------------------
      SUBROUTINE C3LOOK (VX, VY, VZ, PX, PY, PZ, TWIST)
C
      REAL VX, VY, VZ, PX, PY, PZ, TWIST
C
C V      (In)  eye coordinate
C P      (In)  coordinate to look at
C TWIST  (In)  twist angle about eye z axis in degrees
C
      REAL TMP (4, 4)
      REAL XZ, XYZ
C
      XZ = SQRT ((PX -VX) **2 + (PZ - VZ) **2)
      XYZ = SQRT ((PX - VX) **2 + (PY - VY) **2 + (PZ - VZ) **2)
C
      CALL C3TRAN (- VX, - VY, - VZ)
C
      CALL C3UNIT (TMP)
      TMP (1, 1) = (VZ - PZ) / XZ
      TMP (3, 3) = TMP (1, 1)
      TMP (3, 1) = (PX - VX) / XZ
      TMP (1, 3) = - TMP (3, 1)
      CALL C3CONC (TMP)
C
      CALL C3UNIT (TMP)
      TMP (2, 2) = XZ / XYZ
      TMP (3, 3) = TMP (2, 2)
      TMP (3, 2) = (VY - PY) / XYZ
      TMP (2, 3) = - TMP (3, 2)
      CALL C3CONC (TMP)
C
      CALL C3ROTZ (- TWIST)
C
      RETURN
      END
C
C
C------------------------------------------------------------
      SUBROUTINE C3OPRJ (LEFT, RIGHT, BOTTOM, TOP, NEAR, FAR)
C
      REAL LEFT, RIGHT, BOTTOM, TOP, NEAR, FAR
C
C    (In) viewing volume in eye coordinate system
C
      REAL TMP (4,4)
C
      CALL C3UNIT (TMP)
      TMP (1, 1) = 2.0 / (RIGHT - LEFT)
      TMP (4, 1) = - (RIGHT + LEFT) / (RIGHT - LEFT)
      TMP (2, 2) = 2.0 / (TOP - BOTTOM)
      TMP (4, 2) = - (TOP + BOTTOM) / (TOP - BOTTOM)
      TMP (3, 3) = - 2.0 / (FAR - NEAR)
      TMP (4, 3) = - (FAR + NEAR) / (FAR - NEAR)
      CALL C3CONC (TMP)
      RETURN
      END
C
C
C------------------------------------------------------------
      SUBROUTINE C3WPRJ (LEFT, RIGHT, BOTTOM, TOP, NEAR, FAR)
C
      REAL LEFT, RIGHT, BOTTOM, TOP, NEAR, FAR
C
C    (In) viewing volume in eye coordinate system, measured at NEAR
C
      REAL TMP (4,4)
C
      CALL C3UNIT (TMP)
      TMP (1, 1) = 2.0 * NEAR / (RIGHT - LEFT)
      TMP (3, 1) = (RIGHT + LEFT) / (RIGHT - LEFT)
      TMP (2, 2) = 2.0 * NEAR / (TOP - BOTTOM)
      TMP (3, 2) = (TOP + BOTTOM) / (TOP - BOTTOM)
      TMP (3, 3) = - (FAR + NEAR) / (FAR - NEAR)
      TMP (4, 3) = - 2.0 * (FAR + NEAR) / (FAR - NEAR)
      TMP (3, 4) = -1.0
      CALL C3CONC (TMP)
      RETURN
      END
C
C
C----------------------------
      SUBROUTINE C3RMTX (MTX)
C
C Package C3DTRF
C
C 3D coordinate transformations.
C
C Copyright (C) 1989 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden
C  15-Sep-1988  first attempts
C
      INTEGER    MAXSTK
      PARAMETER (MAXSTK = 8)
C
C C3DSTK  current stack slot
C C3DMTX  current transformation matrix
C
      INTEGER C3DSTK
      REAL    C3DMTX (4, 4, MAXSTK)
C
      COMMON /C3DAT1/ C3DSTK, C3DMTX
      SAVE   /C3DAT1/
C
      REAL MTX (4, 4)
C
C MTX  (Out) matrix to return current transformation matrix in
C
      CALL C3COPY (MTX, C3DMTX (1, 1, C3DSTK))
      RETURN
      END
C
C
C----------------------------
      SUBROUTINE C3SMTX (MTX)
C
C Package C3DTRF
C
C 3D coordinate transformations.
C
C Copyright (C) 1989 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden
C  15-Sep-1988  first attempts
C
      INTEGER    MAXSTK
      PARAMETER (MAXSTK = 8)
C
C C3DSTK  current stack slot
C C3DMTX  current transformation matrix
C
      INTEGER C3DSTK
      REAL    C3DMTX (4, 4, MAXSTK)
C
      COMMON /C3DAT1/ C3DSTK, C3DMTX
      SAVE   /C3DAT1/
C
      REAL MTX (4, 4)
C
C MTX  (In) matrix to set as current transformation matrix in
C
      CALL C3COPY (C3DMTX (1, 1, C3DSTK), MTX)
      RETURN
      END
C
C
C----------------------------
      SUBROUTINE C3TRFC (COO)
C
C Package C3DTRF
C
C 3D coordinate transformations.
C
C Copyright (C) 1989 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden
C  15-Sep-1988  first attempts
C
      INTEGER    MAXSTK
      PARAMETER (MAXSTK = 8)
C
C C3DSTK  current stack slot
C C3DMTX  current transformation matrix
C
      INTEGER C3DSTK
      REAL    C3DMTX (4, 4, MAXSTK)
C
      COMMON /C3DAT1/ C3DSTK, C3DMTX
      SAVE   /C3DAT1/
C
      REAL COO (3)
C
C COO   (InOut) coordinates to transform
C
      REAL TX, TY, TZ, W
C
      TX = COO (1)
      TY = COO (2)
      TZ = COO (3)
C
      COO (1) = TX * C3DMTX (1, 1, C3DSTK) +
     $          TY * C3DMTX (2, 1, C3DSTK) +
     $          TZ * C3DMTX (3, 1, C3DSTK) + C3DMTX (4, 1, C3DSTK)
      COO (2) = TX * C3DMTX (1, 2, C3DSTK) +
     $          TY * C3DMTX (2, 2, C3DSTK) +
     $          TZ * C3DMTX (3, 2, C3DSTK) + C3DMTX (4, 2, C3DSTK)
      COO (3) = TX * C3DMTX (1, 3, C3DSTK) +
     $          TY * C3DMTX (2, 3, C3DSTK) +
     $          TZ * C3DMTX (3, 3, C3DSTK) + C3DMTX (4, 3, C3DSTK)
      W = TX * C3DMTX (1, 4, C3DSTK) +
     $    TY * C3DMTX (2, 4, C3DSTK) +
     $    TZ * C3DMTX (3, 4, C3DSTK) + C3DMTX (4, 4, C3DSTK)
C
      COO (1) = COO (1) / W
      COO (2) = COO (2) / W
      COO (3) = COO (3) / W
C
      RETURN
      END
C
C
C-------------------------------
      SUBROUTINE C3PUSH (ERRCOD)
C
C Package C3DTRF
C
C 3D coordinate transformations.
C
C Copyright (C) 1989 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden
C  15-Sep-1988  first attempts
C
      INTEGER    MAXSTK
      PARAMETER (MAXSTK = 8)
C
C C3DSTK  current stack slot
C C3DMTX  current transformation matrix
C
      INTEGER C3DSTK
      REAL    C3DMTX (4, 4, MAXSTK)
C
      COMMON /C3DAT1/ C3DSTK, C3DMTX
      SAVE   /C3DAT1/
C
      INTEGER ERRCOD
C
C ERRCOD  (Out) error code
C     0 = no error
C     1 = stack full
C
      IF (C3DSTK .GE. MAXSTK) THEN
        ERRCOD = 1
      ELSE
        C3DSTK = C3DSTK + 1
        CALL C3COPY (C3DMTX (1, 1, C3DSTK), C3DMTX (1, 1, C3DSTK - 1))
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C---------------------
      SUBROUTINE C3POP
C
C Package C3DTRF
C
C 3D coordinate transformations.
C
C Copyright (C) 1989 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden
C  15-Sep-1988  first attempts
C
      INTEGER    MAXSTK
      PARAMETER (MAXSTK = 8)
C
C C3DSTK  current stack slot
C C3DMTX  current transformation matrix
C
      INTEGER C3DSTK
      REAL    C3DMTX (4, 4, MAXSTK)
C
      COMMON /C3DAT1/ C3DSTK, C3DMTX
      SAVE   /C3DAT1/
C
      IF (C3DSTK .GT. 1) C3DSTK = C3DSTK - 1
      RETURN
      END
C
C
C----------------------------
      SUBROUTINE C3CONC (MTX)
C
C Package C3DTRF
C
C 3D coordinate transformations.
C
C Copyright (C) 1989 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden
C  15-Sep-1988  first attempts
C
      INTEGER    MAXSTK
      PARAMETER (MAXSTK = 8)
C
C C3DSTK  current stack slot
C C3DMTX  current transformation matrix
C
      INTEGER C3DSTK
      REAL    C3DMTX (4, 4, MAXSTK)
C
      COMMON /C3DAT1/ C3DSTK, C3DMTX
      SAVE   /C3DAT1/
C
      REAL MTX (4, 4)
C
C MTX  (In) matrix to concatenate to current transformation matrix
C
      INTEGER I, J
      REAL    TMP (4, 4)
C
      CALL C3COPY (TMP, C3DMTX (1, 1, C3DSTK))
      DO 100 I = 1, 4
        DO 100 J = 1, 4
          C3DMTX (I, J, C3DSTK) = TMP (I, 1) * MTX (1, J) +
     $                            TMP (I, 2) * MTX (2, J) +
     $                            TMP (I, 3) * MTX (3, J) +
     $                            TMP (I, 4) * MTX (4, J)
100   CONTINUE
      RETURN
      END
C
C
C-----------------------------------
      SUBROUTINE C3COPY (MTX1, MTX2)
C
      REAL MTX1 (4, 4), MTX2 (4, 4)
C
C MTX1  (Out) matrix to copy to
C MTX2  (In)  matrix to copy from
C
      INTEGER I, J
C
      DO 100 I = 1, 4
        DO 100 J = 1, 4
          MTX1 (I, J) = MTX2 (I, J)
100   CONTINUE
      RETURN
      END
C
C
C----------------------------
      SUBROUTINE C3UNIT (MTX)
C
      REAL MTX (4, 4)
C
C MTX  (Out) matrix to set to unity
C
      MTX (1, 1) = 1.0
      MTX (1, 2) = 0.0
      MTX (1, 3) = 0.0
      MTX (1, 4) = 0.0
      MTX (2, 1) = 0.0
      MTX (2, 2) = 1.0
      MTX (2, 3) = 0.0
      MTX (2, 4) = 0.0
      MTX (3, 1) = 0.0
      MTX (3, 2) = 0.0
      MTX (3, 3) = 1.0
      MTX (3, 4) = 0.0
      MTX (4, 1) = 0.0
      MTX (4, 2) = 0.0
      MTX (4, 3) = 0.0
      MTX (4, 4) = 1.0
      RETURN
      END
C Package CADM
C
C Compute and handle CA distance matrix.
C
C CADMTX s  compute CA distance matrix
C CAGLMK s  return mask (linear form) for some secondary structure
C CAPLMK s  pass mask (linear form) over CA distance matrix, compute RMSD
CC CAGFMK s  return mask (full matrix form) for some secondary structure
CC CAPFMK s  pass mask (full matrix form) over CA distance matrix, compute RMSD
C
C Uses no separately compiled procedures, but uses same data
C structure as procedures MOLIN and MOLCAP in package MOL.
C
C Some of the ideas and data used in this package are from:
C F.M. Richards and C.E. Kundrot, Identification of Structural Motifs From
C Protein Coordinate Data: Secondary Structure and First-Level Supersecondary
C Structure, Proteins: Structure Function and Genetics (1988) 3, pp 71-84.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   20-May-1988  first attempts
C    1-Jun-1988  linear mask code written
C
C
C--------------------------------------------------------------
      SUBROUTINE CADMTX (DMTX, WIDTH, DNUM, CAPTR, CATOT, AXYZ)
C
      INTEGER WIDTH, DNUM, CATOT
      REAL    DMTX (WIDTH, *), AXYZ (3, *)
      INTEGER CAPTR (CATOT)
C
C DMTX   (Out) CA distance matrix; (i,j) i = forward step, j = from residue
C WIDTH  (In)  first index dimensioning of input CA distance matrix
C DNUM   (In)  number of distances to compute for each CA atom
C CAPTR  (In)  pointer list to CA atoms in coordinate array
C CATOT  (In)  number of CA atoms
C AXYZ   (In)  atom coordinates
C
C Help variables
C
      INTEGER I, J, TOP
      REAL    X, Y, Z
C
      DO 100 J = 1, CATOT - 1
        X = AXYZ (1, CAPTR (J))
        Y = AXYZ (2, CAPTR (J))
        Z = AXYZ (3, CAPTR (J))
        TOP = J + DNUM
        IF (TOP .GT. CATOT) TOP = CATOT
C
        DO 100 I = J + 1, TOP
          DMTX (I - J, J) = SQRT ((AXYZ (1, CAPTR (I)) - X) **2 +
     $                            (AXYZ (2, CAPTR (I)) - Y) **2 +
     $                            (AXYZ (3, CAPTR (I)) - Z) **2)
100   CONTINUE
C
      RETURN
      END
C
C
C-----------------------------------------------------
      SUBROUTINE CAGLMK (LMASK, LMLEN, SSTYPE, ERRCOD)
C
      INTEGER     LMLEN, ERRCOD
      REAL        LMASK (LMLEN)
      CHARACTER*1 SSTYPE
C
C LMASK   (Out) distance matrix mask (linear form)
C LMLEN   (In)  length of linear form mask to output
C SSTYPE  (In)  type of secondary structure mask to output;
C               A = alpha helix, E = extended strand
C ERRCOD  (Out) error code
C     0 = no error
C     1 = unknown secondary structure type
C     2 = too long or undefined length for given secondary structure type
C
C Mask data
C
      INTEGER    MAXAHX, MAXEXT
      PARAMETER (MAXAHX = 16, MAXEXT = 16)
C
      REAL AHELIX (MAXAHX), EXTEND (MAXEXT)
      SAVE AHELIX, EXTEND
      DATA AHELIX
     $/ 3.75,  5.36,  5.02,  6.11,  8.53,  9.75, 10.43, 12.18,
     $ 14.09, 15.15, 16.32, 18.19, 19.77, 20.85, 22.33, 24.13/
      DATA EXTEND
     $/ 3.75,  6.47,  9.89, 12.94, 16.28, 19.40, 22.72, 25.87,
     $ 29.17, 32.34, 35.62, 38.81, 42.09, 45.28, 48.55, 51.74/
C
C Help variable
C
      INTEGER I
C
C Return requested linear mask; check length
C
      IF (SSTYPE .EQ. 'A') THEN
        IF (LMLEN .GT. MAXAHX) THEN
          ERRCOD = 2
        ELSE
          DO 100 I = 1, LMLEN
            LMASK (I) = AHELIX (I)
100       CONTINUE
          ERRCOD = 0
        END IF
C
      ELSE IF (SSTYPE .EQ. 'E') THEN
        IF (LMLEN .GT. MAXEXT) THEN
          ERRCOD = 2
        ELSE
          DO 200 I = 1, LMLEN
            LMASK (I) = EXTEND (I)
200       CONTINUE
          ERRCOD = 0
        END IF
C
      ELSE
        ERRCOD = 1
      END IF
C
      RETURN
      END
C
C
C----------------------------------------------------------------
      SUBROUTINE CAPLMK (RMSD, DMTX, WIDTH, LMASK, ZONLEN, CATOT)
C
      INTEGER WIDTH, ZONLEN, CATOT
      REAL    RMSD (*), DMTX (WIDTH, *), LMASK (*)
C
C RMSD    (Out) list of RMSD values between matrix and mask off-diagonal
C DMTX    (In)  CA distance matrix
C WIDTH   (In)  first index dimensioning of input CA distance matrix
C LMASK   (In)  distance matrix mask (linear form)
C ZONLEN  (In)  number of residues in zone to compare mask; ZONLEN = DNUM + 1
C CATOT   (In)  number of CA atoms
C
C Help variables
C
      REAL    SUM, NUM
      INTEGER I, J, K
C
      NUM = REAL (ZONLEN * (ZONLEN - 1) / 2)
C
      DO 100 I = 1, CATOT - ZONLEN + 1
        SUM = 0.0
C
        DO 200 J = 1, ZONLEN - 1
          DO 200 K = 1, ZONLEN - J
            SUM = SUM + (DMTX (K, J + I - 1) - LMASK (K)) **2
200     CONTINUE
C
        RMSD (I) = SQRT (SUM / NUM)
C
100   CONTINUE
C
      DO 300 I = CATOT - ZONLEN + 2, CATOT
        RMSD (I) = 0.0
300   CONTINUE
C
      RETURN
      END
C Package COLOUR
C
C Colour conversion procedures. RGB is considered the base system.
C
C Reference: J.D. Foley & A. Van Dam, Fundamentals of Interactive
C Computer Graphics, 1982 Addison-Wesley Publishing Company Inc,
C Reading, Massachusetts.
C
C EAS (Evans & Sutherland PS300) is a modified HSV model
C with Red at Hue 0.0 and Value always at 1.0.
C
C  colour        RGB         HSV     EAS
C
C  red      1.0, 0.0, 0.0     0.0   120.0
C  yellow   1.0, 1.0, 0.0    60.0   180.0
C  green    0.0, 1.0, 0.0   120.0   240.0
C  cyan     0.0, 1.0, 1.0   180.0   300.0
C  blue     0.0, 0.0, 1.0   240.0   360.0
C  magenta  1.0, 0.0, 1.0   300.0    60.0
C
C Uses no separately compiled procedures.
C
C RGBHSV s  convert RGB colour to HSV
C RGBEAS s  convert RGB colour to EAS
C HSVRGB s  convert HSV colour to RGB
C EASRGB s  convert EAS colour to RGB
C
C Copyright (C) 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   17-Jul-1990  written, tested
C
C
C---------------------------------
      SUBROUTINE RGBHSV (RGB, HSV)
C
      REAL RGB (3), HSV (3)
C
C RGB  (In)  RGB colour triple, RGB: [0.0, 1.0]
C HSV  (Out) HSV colour triple, H: [0.0, 360.0], SV: [0.0, 1.0]
C
C Help variables
C
      REAL R, G, B, MAXC, MINC, RC, GC, BC
C
C Force interval limits
C
      R = MAX (0.0, MIN (1.0, RGB (1)))
      G = MAX (0.0, MIN (1.0, RGB (2)))
      B = MAX (0.0, MIN (1.0, RGB (3)))
C
      MAXC = MAX (R, G, B)
      MINC = MIN (R, G, B)
C
C Value
C
      HSV (3) = MAXC
C
C Saturation
C
      IF (MAXC .NE. 0.0) THEN
        HSV (2) = (MAXC - MINC) / MAXC
      ELSE
        HSV (2) = 0.0
      END IF
C
C Hue; default blue if achromatic
C
      IF (HSV (2) .EQ. 0.0) THEN
        HSV (1) = 0.0
C
C Chromatic case
C
      ELSE
C
C Distance of colour from red, green, blue
C
        RC = (MAXC - R) / (MAXC - MINC)
        GC = (MAXC - G) / (MAXC - MINC)
        BC = (MAXC - B) / (MAXC - MINC)
C
C Between yellow and magenta
C
        IF (R .EQ. MAXC) THEN
          HSV (1) = BC - GC
C
C Between cyan and yellow
C
        ELSE IF (G .EQ. MAXC) THEN
          HSV (1) = 2.0 + RC - BC
C
C Between magenta and cyan
C
        ELSE
          HSV (1) = 4.0 + GC - RC
        END IF
C
C Convert to degrees, make non-negative
C
        HSV (1) = 60.0 * HSV (1)
        IF (HSV (1) .LT. 0.0) HSV (1) = HSV (1) + 360.0
C
      END IF
C
      RETURN
      END
C
C
C---------------------------------
      SUBROUTINE RGBEAS (RGB, EAS)
C
      REAL RGB (3), EAS (2)
C
C RGB  (In)  RGB colour triple, RGB: [0.0, 1.0]
C EAS  (Out) EAS colour pair, H: [0.0, 360.0], S: [0.0, 1.0]
C
C Help variable
C
      REAL HSV (3)
C
      CALL RGBHSV (RGB, HSV)
C
C Convert from HSV to EAS; red from 0.0 to 120.0
C
      EAS (1) = HSV (1) + 120.0
      IF (EAS (1) .GE. 360.0) EAS (1) = EAS (1) - 360.0
C
      EAS (2) = HSV (2)
C
      RETURN
      END
C
C
C---------------------------------
      SUBROUTINE HSVRGB (HSV, RGB)
C
      REAL HSV (3), RGB (3)
C
C HSV  (In)  HSV colour triple, H: [0.0, 360.0], SV: [0.0, 1.0]
C RGB  (Out) RGB colour triple, RGB: [0.0, 1.0]
C
C Help variables
C
      REAL    H, S, V, F, P, Q, T
      INTEGER I
C
C Force intervals
C
      H = MAX (0.0, MIN (360.0, HSV (1)))
      S = MAX (0.0, MIN (1.0, HSV (2)))
      V = MAX (0.0, MIN (1.0, HSV (3)))
C
C Achromatic case
C
      IF (S .EQ. 0.0) THEN
        RGB (1) = V
        RGB (2) = V
        RGB (3) = V
C
C Chromatic case
C
      ELSE
        IF (H .EQ. 360.0) H = 0.0
C
C Interval [0.0, 6.0], floor integer and fractional part
C
        H = H / 60.0
        I = INT (H)
        F = H - REAL (I)
C
        P = V * (1.0 - S)
        Q = V * (1.0 - S * F)
        T = V * (1.0 - S * (1.0 - F))
C
C According to floor integer
C
        GOTO (1, 2, 3, 4, 5) I
C
C Zero case
C
        RGB (1) = V
        RGB (2) = T
        RGB (3) = P
        RETURN
C
    1   RGB (1) = Q
        RGB (2) = V
        RGB (3) = P
        RETURN
C
    2   RGB (1) = P
        RGB (2) = V
        RGB (3) = T
        RETURN
C
    3   RGB (1) = P
        RGB (2) = Q
        RGB (3) = V
        RETURN
C
    4   RGB (1) = T
        RGB (2) = P
        RGB (3) = V
        RETURN
C
    5   RGB (1) = V
        RGB (2) = P
        RGB (3) = Q
        RETURN
C
      END IF
C
      END
C
C
C---------------------------------
      SUBROUTINE EASRGB (EAS, RGB)
C
      REAL EAS (2), RGB (3)
C
C EAS  (In)  EAS colour pair, H: [0.0, 360.0], S: [0.0, 1.0]
C RGB  (Out) RGB colour triple, RGB: [0.0, 1.0]
C
C Help variable
C
      REAL HSV (3)
C
C Convert from EAS to HSV hue; red from 120.0 to 0.0
C
      HSV (1) = EAS (1) - 120.0
      IF (HSV (1) .LT. 0.0) HSV (1) = HSV (1) + 360.0
C
      HSV (2) = EAS (2)
      HSV (3) = 1.0
C
      CALL HSVRGB (HSV, RGB)
C
      RETURN
      END
C Contour a 2D array of real values.
C
C Coordinates for the output contour level lines are in units of grid
C point distances, placing coordinate (1.0,1.0) at grid point (1,1).
C
C Tests with random-number value matrices indicate that MAXCNT = XG*YG
C will be quite sufficient in most cases.
C
C The execution time is approximately proportional to (NX * NY) ** 1.36.
C
C Uses no separately compiled procedures.
C
C CONT2D s  contour 2D real value array for one contour level
C CNT2DL s  wind up contour lines one by one
C
C Copyright (C) 1987, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  28-Sep-1987  first attempts
C  29-Sep-1987  main code written
C  30-Sep-1987  added SHIFT to handle VALUES = LEVEL case correctly
C  16-Jul-1990  added CNT2DL procedure
C
C---------------------------------------------------------------
      SUBROUTINE CONT2D (VALUES, XG, YG, MAXXG, MAXYG, LEVEL,
     $                   MAXCNT, CNTPTR, CNTCOO, TOTCNT, ERRCOD)
C
      INTEGER XG, YG, MAXXG, MAXYG, MAXCNT, TOTCNT, ERRCOD
      REAL    LEVEL
      REAL    VALUES (MAXXG, MAXYG), CNTCOO (2, MAXCNT)
      INTEGER CNTPTR (2, MAXCNT)
C
C VALUES  (In) real value 2D matrix to be contoured
C XG      (In) number of grid points in first dimension
C YG      (In) number of grid points in second dimension
C MAXXG   (In) number of grid points declared for VALUES in first dim
C MAXYG   (In) number of grid points declared for VALUES in second dim
C LEVEL   (In) value level to contour at
C MAXCNT  (In) maximum number of contour coordinates
C CNTPTR  (Out) pointers from coordinate to prev and next, 0 if none
C CNTCOO  (Out) contour lines coordinates
C TOTCNT  (Out) total number of contour coordinates found
C ERRCOD  (Out) error code
C     0 = no error
C     1 = contour coordinate list overflow, no further contouring
C     2 = internal error; uneven number of crossings in value square
C
      REAL       DELTA, SHIFT
      PARAMETER (DELTA = 1.0E-6, SHIFT = 0.9999)
C
      INTEGER I, J, K, L, BEFORE, LATEST, TOTSQ
      INTEGER SQPTR (4)
      REAL    DIFF
C
      TOTCNT = 0
      LATEST = 1
C
C Boundary: find level-crossing coordinates for Y=1 grid line
C
      DO 100 I = 1, XG - 1
        IF (VALUES(I,1).GE.LEVEL .NEQV. VALUES(I+1,1).GE.LEVEL) THEN
          TOTCNT = TOTCNT + 1
          IF (TOTCNT .GT. MAXCNT) GOTO 900
          DIFF = VALUES(I+1,1) - VALUES(I,1)
          IF (ABS(DIFF) .LT. DELTA) DIFF = DELTA
          DIFF = ABS ((VALUES(I,1) - LEVEL) / DIFF)
          IF (DIFF .GE. SHIFT) DIFF = SHIFT
          CNTCOO(1,TOTCNT) = REAL(I) + DIFF
          CNTCOO(2,TOTCNT) = 1.0
          CNTPTR(1,TOTCNT) = 0
          CNTPTR(2,TOTCNT) = 0
        END IF
100   CONTINUE
C
C Let X dimension run fastest, Y slowest
C
      DO 200 J = 2, YG
C
C Boundary: find level-crossing coords for X=1 at this Y-grid position
C
        IF (VALUES(1,J-1).GE.LEVEL .NEQV. VALUES(1,J).GE.LEVEL) THEN
          TOTCNT = TOTCNT + 1
          IF (TOTCNT .GT. MAXCNT) GOTO 900
          DIFF = VALUES(1,J) - VALUES(1,J-1)
          IF (ABS(DIFF) .LT. DELTA) DIFF = DELTA
          DIFF = ABS ((VALUES(1,J-1) - LEVEL) / DIFF)
          IF (DIFF .GE. SHIFT) DIFF = SHIFT
          CNTCOO(1,TOTCNT) = 1.0
          CNTCOO(2,TOTCNT) = REAL(J-1) + DIFF
          CNTPTR(1,TOTCNT) = 0
          CNTPTR(2,TOTCNT) = 0
          SQPTR(4) = TOTCNT
        ELSE
          SQPTR(4) = 0
        END IF
C
C Update pointer to oldest possible crossing not yet connected to others
C
        BEFORE = LATEST
        LATEST = TOTCNT + 1
C
C For each X,Y value, look at square lower left of X,Y;
C look for level crossings: if 2, draw one line, if 4, draw to lines
C
        DO 200 I = 2, XG
C
C Init square pointer-to-level-crossings array
C
          SQPTR(2) = SQPTR(4)
          IF (SQPTR(2) .EQ. 0) THEN
            TOTSQ = 0
          ELSE
            TOTSQ = 1
          END IF
C
C Look through latest made Y=J-1 grid line crossings, if any for this X
C
          SQPTR (1) = 0
          DO 250 K = BEFORE, TOTCNT
            IF (INT (CNTCOO(1,K)) .EQ. I-1) THEN
              IF (INT (CNTCOO(2,K)) .EQ. J-1) THEN
                IF (K .NE. SQPTR(2)) THEN
                  SQPTR (1) = K
                  TOTSQ = TOTSQ + 1
                  GOTO 300
                END IF
              END IF
            END IF
250       CONTINUE
C
C Check level-crossing in X-direction
C
300       IF (VALUES(I-1,J).GE.LEVEL .NEQV. VALUES(I,J).GE.LEVEL) THEN
            TOTCNT = TOTCNT + 1
            IF (TOTCNT .GT. MAXCNT) GOTO 900
            DIFF = VALUES(I,J) - VALUES(I-1,J)
            IF (ABS(DIFF) .LT. DELTA) DIFF = DELTA
            DIFF = ABS ((VALUES(I-1,J) - LEVEL) / DIFF)
            IF (DIFF .GE. SHIFT) DIFF = SHIFT
            CNTCOO(1,TOTCNT) = REAL(I-1) + DIFF
            CNTCOO(2,TOTCNT) = REAL(J)
            CNTPTR(1,TOTCNT) = 0
            CNTPTR(2,TOTCNT) = 0
            SQPTR(3) = TOTCNT
            TOTSQ = TOTSQ + 1
          ELSE
            SQPTR(3) = 0
          END IF
C
C Check level-crossing in Y-direction
C
          IF (VALUES(I,J-1).GE.LEVEL .NEQV. VALUES(I,J).GE.LEVEL) THEN
            TOTCNT = TOTCNT + 1
            IF (TOTCNT .GT. MAXCNT) GOTO 900
            DIFF = VALUES(I,J) - VALUES(I,J-1)
            IF (ABS(DIFF) .LT. DELTA) DIFF = DELTA
            DIFF = ABS ((VALUES(I,J-1) - LEVEL) /DIFF)
            IF (DIFF .GE. SHIFT) DIFF = SHIFT
            CNTCOO(1,TOTCNT) = REAL(I)
            CNTCOO(2,TOTCNT) = REAL(J-1) + DIFF
            CNTPTR(1,TOTCNT) = 0
            CNTPTR(2,TOTCNT) = 0
            SQPTR(4) = TOTCNT
            TOTSQ = TOTSQ + 1
          ELSE
            SQPTR(4) = 0
          END IF
C
C Connect the level-crossing coordinates in current square
C Two cases: 2 or 4 crossings in square (and, of course, zero)
C
          IF (TOTSQ .EQ. 2) THEN
            IF (SQPTR (1) .NE. 0) THEN
              K = SQPTR (1)
              IF (SQPTR (2) .NE. 0) THEN
                L = SQPTR (2)
              ELSE IF (SQPTR (3) .NE. 0) THEN
                L = SQPTR (3)
              ELSE
                L = SQPTR (4)
              END IF
            ELSE IF (SQPTR (2) .NE. 0) THEN
              K = SQPTR (2)
              IF (SQPTR (3) .NE. 0) THEN
                L = SQPTR (3)
              ELSE
                L = SQPTR (4)
              END IF
            ELSE
              K = SQPTR (3)
              L = SQPTR (4)
            END IF
            IF (CNTPTR (1,K) .EQ. 0) THEN
              CNTPTR (1,K) = L
            ELSE
              CNTPTR (2,K) = L
            END IF
            IF (CNTPTR (1,L) .EQ. 0) THEN
              CNTPTR (1,L) = K
            ELSE
              CNTPTR (2,L) = K
            END IF
C
C When 4: decide which direction the two parallel contour lines go
C by checking if mean value in middle of square is in valley or ridge
C
          ELSE IF (TOTSQ.EQ.4) THEN
            DIFF = (VALUES (I, J) + VALUES (I-1, J)+
     $              VALUES (I, J-1) + VALUES (I-1, J-1)) / 4.0
            IF (DIFF.LT.LEVEL .EQV. VALUES(I,J).LT.LEVEL) THEN
              IF (CNTPTR (1, SQPTR (1)) .EQ. 0) THEN
                CNTPTR (1, SQPTR (1)) = SQPTR (4)
              ELSE
                CNTPTR (2, SQPTR (1)) = SQPTR (4)
              END IF
              IF (CNTPTR (1, SQPTR (4)) .EQ. 0) THEN
                CNTPTR (1, SQPTR (4)) = SQPTR (1)
              ELSE
                CNTPTR (2, SQPTR (4)) = SQPTR (1)
              END IF
              IF (CNTPTR (1, SQPTR (2)) .EQ. 0) THEN
                CNTPTR (1, SQPTR (2)) = SQPTR (3)
              ELSE
                CNTPTR (2, SQPTR (2)) = SQPTR (3)
              END IF
              IF (CNTPTR (1, SQPTR (3)) .EQ. 0) THEN
                CNTPTR (1, SQPTR (3)) = SQPTR (2)
              ELSE
                CNTPTR (2, SQPTR (3)) = SQPTR (2)
              END IF
            ELSE
              IF (CNTPTR (1, SQPTR (1)) .EQ. 0) THEN
                CNTPTR (1, SQPTR (1)) = SQPTR (2)
              ELSE
                CNTPTR (2, SQPTR (1)) = SQPTR (2)
              END IF
              IF (CNTPTR (1, SQPTR (2)) .EQ. 0) THEN
                CNTPTR (1, SQPTR (2)) = SQPTR (1)
              ELSE
                CNTPTR (2, SQPTR (2)) = SQPTR (1)
              END IF
              IF (CNTPTR (1, SQPTR (3)) .EQ. 0) THEN
                CNTPTR (1, SQPTR (3)) = SQPTR (4)
              ELSE
                CNTPTR (2, SQPTR (3)) = SQPTR (4)
              END IF
              IF (CNTPTR (1, SQPTR (4)) .EQ. 0) THEN
                CNTPTR (1, SQPTR (4)) = SQPTR (3)
              ELSE
                CNTPTR (2, SQPTR (4)) = SQPTR (3)
              END IF
            END IF
C
          ELSE IF (TOTSQ.NE.0) THEN
            ERRCOD = 2
            RETURN
          END IF
C
200   CONTINUE
C
      ERRCOD = 0
      RETURN
C
900   TOTCNT = MAXCNT
      ERRCOD = 1
      RETURN
      END

C
C
C--------------------------------------------------------------
      SUBROUTINE CNT2DL (LINPTR, TOTLIN, CNTPTR, TOTCNT, STATE)
C
      INTEGER TOTLIN, TOTCNT, MAXCNT, STATE
      INTEGER LINPTR (*), CNTPTR (2, *)
C
C LINCOO  (Out) pointers to contour line coordinates
C TOTLIN  (Out) total number of coordinates in contour line
C CNTPTR  (InOut) contour coordinate pointers
C TOTCNT  (In)  total contour coordinates
C STATE   (InOut) processing state flag:
C                 0 = nothing processed yet
C                 1 = processing open-ended lines
C                 2 = processing closed-loop lines
C                 3 = everything processed
C
C Pointer to first unprocessed coordinate point
C
      INTEGER FIRST
      SAVE    FIRST
C
C Help variables
C
      INTEGER CURR, NEXT
C
C Go to wind-up procedure according to current state
C
      GOTO (100, 400) STATE
C
C Nothing done yet; init open-ended line processing
C
      FIRST = 1
      STATE = 1
C
C If all open-ended lines processed, then start doing closed-loop lines
C
  100 IF (FIRST .GT. TOTCNT) GOTO 300
C
C Find start of open-ended line
C
      IF (CNTPTR (1,FIRST) .EQ. 0  .EQV.  CNTPTR (2,FIRST) .EQ. 0) THEN
        FIRST = FIRST + 1
        GOTO 100
      END IF
C
C Open-ended line start
C
      TOTLIN = 1
      CURR = FIRST
C
C Set pointer to point
C
200   LINPTR (TOTLIN) = CURR
C
C Next line point; not known which pointer is to next
C
      IF (CNTPTR (1, CURR) .NE. 0) THEN
        NEXT = CNTPTR (1, CURR)
        CNTPTR (1, CURR) = 0
C
      ELSE IF (CNTPTR (2, CURR) .NE. 0) THEN
        NEXT = CNTPTR (2, CURR)
        CNTPTR (2, CURR) = 0
C
C No next point; this open-ended line finished
C
      ELSE
        RETURN
      END IF
C
C Reset pointers for next point
C
      IF (CNTPTR (1, NEXT) .EQ. CURR) THEN
        CNTPTR (1, NEXT) = 0
      ELSE
        CNTPTR (2, NEXT) = 0
      END IF
C
C Loop back for next point
C
      TOTLIN = TOTLIN + 1
      CURR = NEXT
C
      GOTO 200
C
C Init closed-loop line processing
C
300   FIRST = 1
      STATE = 2
C
C If all closed-loop lines processed, then finished
C
  400 IF (FIRST .GT. TOTCNT) THEN
        STATE = 3
        RETURN
      END IF
C
C Closed-loop line; find any point in it
C
      IF (CNTPTR (1, FIRST) .EQ. 0) THEN
        FIRST = FIRST + 1
        GOTO 400
      END IF
C
C Closed-loop line start
C
      TOTLIN = 1
      CURR = FIRST
C
C Break links between first and last points
C
      IF (CNTPTR (1, CNTPTR (2, CURR)) .EQ. CURR) THEN
        CNTPTR (1, CNTPTR (2, CURR)) = 0
      ELSE
        CNTPTR (2, CNTPTR (2, CURR)) = 0
      END IF
      CNTPTR (2, CURR) = 0
C
C Set pointer to point
C
500   LINPTR (TOTLIN) = CURR
C
C Next line point; not known which pointer is to next
C
      IF (CNTPTR (1, CURR) .NE. 0) THEN
        NEXT = CNTPTR (1, CURR)
        CNTPTR (1, CURR) = 0
C
      ELSE IF (CNTPTR (2, CURR) .NE. 0) THEN
        NEXT = CNTPTR (2, CURR)
        CNTPTR (2, CURR) = 0
C
C No next point; close loop by setting first point also as last
C
      ELSE
        TOTLIN = TOTLIN + 1
        LINPTR (TOTLIN) = LINPTR (1)
C
        RETURN
      END IF
C
C Reset pointers for next point
C
      IF (CNTPTR (1, NEXT) .EQ. CURR) THEN
        CNTPTR (1, NEXT) = 0
      ELSE
        CNTPTR (2, NEXT) = 0
      END IF
C
C Loop back for next point
C
      TOTLIN = TOTLIN + 1
      CURR = NEXT
C
      GOTO 500
C
      END
C Package ERROUT
C
C Output to terminal and handle errors of varying severity.
C
C ERRWNG s  output warning message
C ERRMSG s  output error message
C ERRFTL s  output fatal error message and stop execution
C ERRXXX s  output error message and cause program crash
C ERRCHK s  check integer error code, optionally stop execution
C ERRNUM lf return true and output message if wrong number
C ERRTST lf return value of input logical, if true also output message
C
C Uses no separately compiled procedures.
C
C (C) Copyright Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   16-Sep-1987  written
C   14-Oct-1987  ERRNUM added
C    5-Nov-1987  ERRTST added
C
C
C----------------------------
      SUBROUTINE ERRWNG (MSG)
C
      CHARACTER*(*) MSG
C
      WRITE (6, 10) CHAR (7), MSG
10    FORMAT (' Warning: ', A, A)
      RETURN
      END
C
C
C----------------------------
      SUBROUTINE ERRMSG (MSG)
C
      CHARACTER*(*) MSG
C
      WRITE (6, 10) CHAR (7), MSG
10    FORMAT (' Error: ', A, A)
      RETURN
      END
C
C
C----------------------------
      SUBROUTINE ERRFTL (MSG)
C
      CHARACTER*(*) MSG
C
      WRITE (6, 10) CHAR (7), MSG
10    FORMAT (' Fatal error: ', A, A)
      STOP
      END
C
C
C----------------------------
      SUBROUTINE ERRXXX (MSG)
C
      CHARACTER*(*) MSG
C
      INTEGER I
      WRITE (6, 10) CHAR (7), MSG
10    FORMAT (' Crash error: ', A, A)
C
C This code should fool even optimizing compilers.
C
      DO 20 I = 1, -1, -1
        IF (I .LE. 0) WRITE (6, *) 1.0 / REAL (I)
20    CONTINUE
      END
C
C
C--------------------------------------------
      SUBROUTINE ERRCHK (ERRCOD, MSG, STOPEX)
C
      INTEGER       ERRCOD
      CHARACTER*(*) MSG
      LOGICAL       STOPEX
C
      IF (ERRCOD .EQ. 0) THEN
        RETURN
      ELSE IF (STOPEX) THEN
        WRITE (6, 10) CHAR (7), 'Fatal error code: ', ERRCOD, MSG
10      FORMAT (' ', A, A, I3, TR2, A)
        STOP
      ELSE
        WRITE (6, 10) CHAR (7), 'Error code: ', ERRCOD, MSG
      END IF
      RETURN
      END
C
C
C------------------------------------------
      LOGICAL FUNCTION ERRNUM (ERRCOD, MSG)
C
      INTEGER       ERRCOD
      CHARACTER*(*) MSG
C
C ERRCOD  (In) error code value to test
C MSG     (In) error message if error code indicates some error
C
      IF (ERRCOD .NE. 0) THEN
        WRITE (6, 10) CHAR (7), MSG
10      FORMAT (' Error: ', A, A)
        ERRNUM = .TRUE.
      ELSE
        ERRNUM = .FALSE.
      END IF
      RETURN
      END
C
C
C----------------------------------------
      LOGICAL FUNCTION ERRTST (TEST, MSG)
C
      LOGICAL       TEST
      CHARACTER*(*) MSG
C
C TEST  (In) logical test value
C MSG   (In) error message if incorrect value
C
      IF (TEST) THEN
        WRITE (6, 10) CHAR (7), MSG
10      FORMAT (' Error: ', A, A)
      END IF
      ERRTST = TEST
      RETURN
      END
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C GVDEF  s  define variable, type and size (number of elements)
C GVPTR  if return pointer to variable
C GVTYPE if return type of variable
C GVSIZE if return size (number of elements) of variable
C GVNAME cf return name of variable for pointer
C GVTOT  if return total number of variables
C GVMMOD s  modify mode value for variable
C GVMODE if return mode value for variable
C GVIMOD s  modify variable integer value(s)
C GVRMOD s  modify variable real value(s)
C GVLMOD s  modify variable logical value(s)
C GVSMOD s  modify variable string value(s)
C GVIVAL s  return variable integer value(s)
C GVRVAL s  return variable real value(s)
C GVLVAL s  return variable logical value(s)
C GVSVAL s  return variable string value(s)
C GVREM  s  remove variable
C GVDAT1 o  numerical data structure
C GVDAT2 o  character data structure
C GVDINI b  initialize data structure
C
C Uses separately compiled procedure:
C
C BFINDS if  find word slot in ordered list, or slot to insert at
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C   9-Jul-1988  written and debugged
C
C The file genvar.inc, which contains the data structure used by the
C procedures, must be included in the program (using VAX FORTRAN
C INCLUDE, editor or utility program) before compiling.
C
C
C----------------------------------------------------------
      SUBROUTINE GVDEF (PTR, VAR, TYPE, SIZE, MODE, ERRCOD)
C
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C
C MAXVAR  max number of variables
C MAXINT  max number of integer values
C MAXREL  max number of real values
C MAXLOG  max number of logical values
C MAXSTR  max number of string values
C
      INTEGER    MAXVAR, MAXINT, MAXREL, MAXLOG, MAXSTR
      PARAMETER (MAXVAR = 100, MAXINT = 40, MAXREL = 40, MAXLOG = 40,
     $           MAXSTR = 100)
C
C TOTVAR  total allocated variables
C VARNAM  variable name
C VARTYP  variable type
C VARSIZ  variable size (number of elements)
C VARPTR  pointer to first variable value
C VARMOD  variable mode value
C
      INTEGER      TOTVAR
      CHARACTER*20 VARNAM (MAXVAR)
      INTEGER      VARTYP (MAXVAR), VARSIZ (MAXVAR), VARPTR (MAXVAR),
     $             VARMOD (MAXVAR)
C
C TOTINT  total allocated integer values
C TOTREL  total allocated real values
C TOTLOG  total allocated logical values
C TOTSTR  total allocated string values
C INTVAL  integer values
C RELVAL  real values
C LOGVAL  logical values
C STRVAL  string values
C
      INTEGER      TOTINT, TOTREL, TOTLOG, TOTSTR
      INTEGER      INTVAL (MAXINT)
      REAL         RELVAL (MAXREL)
      LOGICAL      LOGVAL (MAXLOG)
      CHARACTER*80 STRVAL (MAXSTR)
C
      COMMON /GVDAT1/ TOTVAR, VARTYP, VARSIZ, VARPTR, VARMOD,
     $                TOTINT, TOTREL, TOTLOG, TOTSTR,
     $                INTVAL, RELVAL, LOGVAL
      COMMON /GVDAT2/ VARNAM, STRVAL
      SAVE   /GVDAT1/, /GVDAT2/
C
C To be sure to initialize
C
      EXTERNAL GVDINI
C
      INTEGER       PTR, TYPE, SIZE, MODE, ERRCOD
      CHARACTER*(*) VAR
C
C PTR     (Out) pointer to variable
C VAR     (In)  variable name
C TYPE    (In)  variable type; 1=integer, 2=real, 3=logical, 4=string
C SIZE    (In)  variable size (number of elements)
C MODE    (In)  variable mode value
C ERRCOD  (Out) error code
C     0 = no error
C     1 = variable name already exists
C     2 = no space for variable name
C     3 = no space for type with given size
C     4 = unknown variable type
C     5 = invalid variable size
C
C Externally defined function
C
      INTEGER BFINDS
C
C Help variable
C
      INTEGER I
C
      ERRCOD = 0
      IF (TOTVAR .GE. MAXVAR) THEN
        ERRCOD = 2
      ELSE IF (TYPE .LT. 1  .OR.  TYPE .GT. 4) THEN
        ERRCOD = 4
      ELSE IF (SIZE .LT. 1) THEN
        ERRCOD = 5
      ELSE IF (TYPE .EQ. 1  .AND.  (TOTINT+SIZE-1) .GE. MAXINT  .OR.
     $         TYPE .EQ. 2  .AND.  (TOTREL+SIZE-1) .GE. MAXREL  .OR.
     $         TYPE .EQ. 3  .AND.  (TOTLOG+SIZE-1) .GE. MAXLOG  .OR.
     $         TYPE .EQ. 4  .AND.  (TOTSTR+SIZE-1) .GE. MAXSTR) THEN
        ERRCOD = 3
C
C Find the variable slot to put into; return with failure if already exists
C
      ELSE
        PTR = - BFINDS (VAR, VARNAM, TOTVAR)
        IF (PTR .LT. 0) ERRCOD = 1
      END IF
      IF (ERRCOD .NE. 0) RETURN
C
C Shift all pointers up and insert the variable into the list
C
      TOTVAR = TOTVAR + 1
      DO 100 I = TOTVAR, PTR + 1, - 1
        VARNAM (I) = VARNAM (I - 1)
        VARTYP (I) = VARTYP (I - 1)
        VARSIZ (I) = VARSIZ (I - 1)
        VARPTR (I) = VARPTR (I - 1)
        VARMOD (I) = VARMOD (I - 1)
100   CONTINUE
C
      VARNAM (PTR) = VAR
      VARTYP (PTR) = TYPE
      VARSIZ (PTR) = SIZE
      VARMOD (PTR) = MODE
C
C Allocate the variable value space
C
      IF (TYPE .EQ. 1) THEN
        VARPTR (PTR) = TOTINT + 1
        TOTINT = TOTINT + SIZE
      ELSE IF (TYPE .EQ. 2) THEN
        VARPTR (PTR) = TOTREL + 1
        TOTREL = TOTREL + SIZE
      ELSE IF (TYPE .EQ. 3) THEN
        VARPTR (PTR) = TOTLOG + 1
        TOTLOG = TOTLOG + SIZE
      ELSE
        VARPTR (PTR) = TOTSTR + 1
        TOTSTR = TOTSTR + SIZE
      END IF
C
      RETURN
      END
C
C
C---------------------------------
      INTEGER FUNCTION GVPTR (VAR)
C
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C
C MAXVAR  max number of variables
C MAXINT  max number of integer values
C MAXREL  max number of real values
C MAXLOG  max number of logical values
C MAXSTR  max number of string values
C
      INTEGER    MAXVAR, MAXINT, MAXREL, MAXLOG, MAXSTR
      PARAMETER (MAXVAR = 100, MAXINT = 40, MAXREL = 40, MAXLOG = 40,
     $           MAXSTR = 100)
C
C TOTVAR  total allocated variables
C VARNAM  variable name
C VARTYP  variable type
C VARSIZ  variable size (number of elements)
C VARPTR  pointer to first variable value
C VARMOD  variable mode value
C
      INTEGER      TOTVAR
      CHARACTER*20 VARNAM (MAXVAR)
      INTEGER      VARTYP (MAXVAR), VARSIZ (MAXVAR), VARPTR (MAXVAR),
     $             VARMOD (MAXVAR)
C
C TOTINT  total allocated integer values
C TOTREL  total allocated real values
C TOTLOG  total allocated logical values
C TOTSTR  total allocated string values
C INTVAL  integer values
C RELVAL  real values
C LOGVAL  logical values
C STRVAL  string values
C
      INTEGER      TOTINT, TOTREL, TOTLOG, TOTSTR
      INTEGER      INTVAL (MAXINT)
      REAL         RELVAL (MAXREL)
      LOGICAL      LOGVAL (MAXLOG)
      CHARACTER*80 STRVAL (MAXSTR)
C
      COMMON /GVDAT1/ TOTVAR, VARTYP, VARSIZ, VARPTR, VARMOD,
     $                TOTINT, TOTREL, TOTLOG, TOTSTR,
     $                INTVAL, RELVAL, LOGVAL
      COMMON /GVDAT2/ VARNAM, STRVAL
      SAVE   /GVDAT1/, /GVDAT2/
C
C To be sure to initialize
C
      EXTERNAL GVDINI
C
      CHARACTER*(*) VAR
C
C VAR    (In)  variable name
C
C Externally defined function
C
      INTEGER BFINDS
C
C Help variable
C
      INTEGER SLOT
C
      SLOT = BFINDS (VAR, VARNAM, TOTVAR)
      IF (SLOT .LT. 0) THEN
        GVPTR = 0
      ELSE
        GVPTR = SLOT
      END IF
      RETURN
      END
C
C
C----------------------------------
      INTEGER FUNCTION GVTYPE (PTR)
C
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C
C MAXVAR  max number of variables
C MAXINT  max number of integer values
C MAXREL  max number of real values
C MAXLOG  max number of logical values
C MAXSTR  max number of string values
C
      INTEGER    MAXVAR, MAXINT, MAXREL, MAXLOG, MAXSTR
      PARAMETER (MAXVAR = 100, MAXINT = 40, MAXREL = 40, MAXLOG = 40,
     $           MAXSTR = 100)
C
C TOTVAR  total allocated variables
C VARNAM  variable name
C VARTYP  variable type
C VARSIZ  variable size (number of elements)
C VARPTR  pointer to first variable value
C VARMOD  variable mode value
C
      INTEGER      TOTVAR
      CHARACTER*20 VARNAM (MAXVAR)
      INTEGER      VARTYP (MAXVAR), VARSIZ (MAXVAR), VARPTR (MAXVAR),
     $             VARMOD (MAXVAR)
C
C TOTINT  total allocated integer values
C TOTREL  total allocated real values
C TOTLOG  total allocated logical values
C TOTSTR  total allocated string values
C INTVAL  integer values
C RELVAL  real values
C LOGVAL  logical values
C STRVAL  string values
C
      INTEGER      TOTINT, TOTREL, TOTLOG, TOTSTR
      INTEGER      INTVAL (MAXINT)
      REAL         RELVAL (MAXREL)
      LOGICAL      LOGVAL (MAXLOG)
      CHARACTER*80 STRVAL (MAXSTR)
C
      COMMON /GVDAT1/ TOTVAR, VARTYP, VARSIZ, VARPTR, VARMOD,
     $                TOTINT, TOTREL, TOTLOG, TOTSTR,
     $                INTVAL, RELVAL, LOGVAL
      COMMON /GVDAT2/ VARNAM, STRVAL
      SAVE   /GVDAT1/, /GVDAT2/
C
C To be sure to initialize
C
      EXTERNAL GVDINI
C
      INTEGER PTR
C
C PTR   (In) pointer to variable
C
      IF (PTR .LT. 1  .OR.  PTR .GT. TOTVAR) THEN
        GVTYPE = 0
      ELSE
        GVTYPE = VARTYP (PTR)
      END IF
      RETURN
      END
C
C
C----------------------------------
      INTEGER FUNCTION GVSIZE (PTR)
C
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C
C MAXVAR  max number of variables
C MAXINT  max number of integer values
C MAXREL  max number of real values
C MAXLOG  max number of logical values
C MAXSTR  max number of string values
C
      INTEGER    MAXVAR, MAXINT, MAXREL, MAXLOG, MAXSTR
      PARAMETER (MAXVAR = 100, MAXINT = 40, MAXREL = 40, MAXLOG = 40,
     $           MAXSTR = 100)
C
C TOTVAR  total allocated variables
C VARNAM  variable name
C VARTYP  variable type
C VARSIZ  variable size (number of elements)
C VARPTR  pointer to first variable value
C VARMOD  variable mode value
C
      INTEGER      TOTVAR
      CHARACTER*20 VARNAM (MAXVAR)
      INTEGER      VARTYP (MAXVAR), VARSIZ (MAXVAR), VARPTR (MAXVAR),
     $             VARMOD (MAXVAR)
C
C TOTINT  total allocated integer values
C TOTREL  total allocated real values
C TOTLOG  total allocated logical values
C TOTSTR  total allocated string values
C INTVAL  integer values
C RELVAL  real values
C LOGVAL  logical values
C STRVAL  string values
C
      INTEGER      TOTINT, TOTREL, TOTLOG, TOTSTR
      INTEGER      INTVAL (MAXINT)
      REAL         RELVAL (MAXREL)
      LOGICAL      LOGVAL (MAXLOG)
      CHARACTER*80 STRVAL (MAXSTR)
C
      COMMON /GVDAT1/ TOTVAR, VARTYP, VARSIZ, VARPTR, VARMOD,
     $                TOTINT, TOTREL, TOTLOG, TOTSTR,
     $                INTVAL, RELVAL, LOGVAL
      COMMON /GVDAT2/ VARNAM, STRVAL
      SAVE   /GVDAT1/, /GVDAT2/
C
C To be sure to initialize
C
      EXTERNAL GVDINI
C
      INTEGER PTR
C
C PTR   (In) pointer to variable
C
      IF (PTR .LT. 1  .OR.  PTR .GT. TOTVAR) THEN
        GVSIZE = 0
      ELSE
        GVSIZE = VARSIZ (PTR)
      END IF
      RETURN
      END
C
C
C---------------------------------------
      CHARACTER*20 FUNCTION GVNAME (PTR)
C
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C
C MAXVAR  max number of variables
C MAXINT  max number of integer values
C MAXREL  max number of real values
C MAXLOG  max number of logical values
C MAXSTR  max number of string values
C
      INTEGER    MAXVAR, MAXINT, MAXREL, MAXLOG, MAXSTR
      PARAMETER (MAXVAR = 100, MAXINT = 40, MAXREL = 40, MAXLOG = 40,
     $           MAXSTR = 100)
C
C TOTVAR  total allocated variables
C VARNAM  variable name
C VARTYP  variable type
C VARSIZ  variable size (number of elements)
C VARPTR  pointer to first variable value
C VARMOD  variable mode value
C
      INTEGER      TOTVAR
      CHARACTER*20 VARNAM (MAXVAR)
      INTEGER      VARTYP (MAXVAR), VARSIZ (MAXVAR), VARPTR (MAXVAR),
     $             VARMOD (MAXVAR)
C
C TOTINT  total allocated integer values
C TOTREL  total allocated real values
C TOTLOG  total allocated logical values
C TOTSTR  total allocated string values
C INTVAL  integer values
C RELVAL  real values
C LOGVAL  logical values
C STRVAL  string values
C
      INTEGER      TOTINT, TOTREL, TOTLOG, TOTSTR
      INTEGER      INTVAL (MAXINT)
      REAL         RELVAL (MAXREL)
      LOGICAL      LOGVAL (MAXLOG)
      CHARACTER*80 STRVAL (MAXSTR)
C
      COMMON /GVDAT1/ TOTVAR, VARTYP, VARSIZ, VARPTR, VARMOD,
     $                TOTINT, TOTREL, TOTLOG, TOTSTR,
     $                INTVAL, RELVAL, LOGVAL
      COMMON /GVDAT2/ VARNAM, STRVAL
      SAVE   /GVDAT1/, /GVDAT2/
C
C To be sure to initialize
C
      EXTERNAL GVDINI
C
      INTEGER PTR
C
C PTR   (In) pointer to variable
C
      IF (PTR .LT. 1  .OR.  PTR .GT. TOTVAR) THEN
        GVNAME = ' '
      ELSE
        GVNAME = VARNAM (PTR)
      END IF
      RETURN
      END
C
C
C------------------------------
      INTEGER FUNCTION GVTOT ()
C
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C
C MAXVAR  max number of variables
C MAXINT  max number of integer values
C MAXREL  max number of real values
C MAXLOG  max number of logical values
C MAXSTR  max number of string values
C
      INTEGER    MAXVAR, MAXINT, MAXREL, MAXLOG, MAXSTR
      PARAMETER (MAXVAR = 100, MAXINT = 40, MAXREL = 40, MAXLOG = 40,
     $           MAXSTR = 100)
C
C TOTVAR  total allocated variables
C VARNAM  variable name
C VARTYP  variable type
C VARSIZ  variable size (number of elements)
C VARPTR  pointer to first variable value
C VARMOD  variable mode value
C
      INTEGER      TOTVAR
      CHARACTER*20 VARNAM (MAXVAR)
      INTEGER      VARTYP (MAXVAR), VARSIZ (MAXVAR), VARPTR (MAXVAR),
     $             VARMOD (MAXVAR)
C
C TOTINT  total allocated integer values
C TOTREL  total allocated real values
C TOTLOG  total allocated logical values
C TOTSTR  total allocated string values
C INTVAL  integer values
C RELVAL  real values
C LOGVAL  logical values
C STRVAL  string values
C
      INTEGER      TOTINT, TOTREL, TOTLOG, TOTSTR
      INTEGER      INTVAL (MAXINT)
      REAL         RELVAL (MAXREL)
      LOGICAL      LOGVAL (MAXLOG)
      CHARACTER*80 STRVAL (MAXSTR)
C
      COMMON /GVDAT1/ TOTVAR, VARTYP, VARSIZ, VARPTR, VARMOD,
     $                TOTINT, TOTREL, TOTLOG, TOTSTR,
     $                INTVAL, RELVAL, LOGVAL
      COMMON /GVDAT2/ VARNAM, STRVAL
      SAVE   /GVDAT1/, /GVDAT2/
C
C To be sure to initialize
C
      EXTERNAL GVDINI
C
      GVTOT = TOTVAR
      RETURN
      END
C
C
C------------------------------------------
      SUBROUTINE GVMMOD (MODE, PTR, ERRCOD)
C
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C
C MAXVAR  max number of variables
C MAXINT  max number of integer values
C MAXREL  max number of real values
C MAXLOG  max number of logical values
C MAXSTR  max number of string values
C
      INTEGER    MAXVAR, MAXINT, MAXREL, MAXLOG, MAXSTR
      PARAMETER (MAXVAR = 100, MAXINT = 40, MAXREL = 40, MAXLOG = 40,
     $           MAXSTR = 100)
C
C TOTVAR  total allocated variables
C VARNAM  variable name
C VARTYP  variable type
C VARSIZ  variable size (number of elements)
C VARPTR  pointer to first variable value
C VARMOD  variable mode value
C
      INTEGER      TOTVAR
      CHARACTER*20 VARNAM (MAXVAR)
      INTEGER      VARTYP (MAXVAR), VARSIZ (MAXVAR), VARPTR (MAXVAR),
     $             VARMOD (MAXVAR)
C
C TOTINT  total allocated integer values
C TOTREL  total allocated real values
C TOTLOG  total allocated logical values
C TOTSTR  total allocated string values
C INTVAL  integer values
C RELVAL  real values
C LOGVAL  logical values
C STRVAL  string values
C
      INTEGER      TOTINT, TOTREL, TOTLOG, TOTSTR
      INTEGER      INTVAL (MAXINT)
      REAL         RELVAL (MAXREL)
      LOGICAL      LOGVAL (MAXLOG)
      CHARACTER*80 STRVAL (MAXSTR)
C
      COMMON /GVDAT1/ TOTVAR, VARTYP, VARSIZ, VARPTR, VARMOD,
     $                TOTINT, TOTREL, TOTLOG, TOTSTR,
     $                INTVAL, RELVAL, LOGVAL
      COMMON /GVDAT2/ VARNAM, STRVAL
      SAVE   /GVDAT1/, /GVDAT2/
C
C To be sure to initialize
C
      EXTERNAL GVDINI
C
      INTEGER MODE, PTR, ERRCOD
C
C MODE    (In)  mode value
C PTR     (In)  pointer to variable
C ERRCOD  (Out) error code
C     0 = no error
C     1 = no such variable
C
      IF (PTR .LT. 1  .OR.  PTR .GT. TOTVAR) THEN
        ERRCOD = 1
      ELSE
        VARMOD (PTR) = MODE
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C------------------------------------------
      INTEGER FUNCTION GVMODE (PTR, ERRCOD)
C
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C
C MAXVAR  max number of variables
C MAXINT  max number of integer values
C MAXREL  max number of real values
C MAXLOG  max number of logical values
C MAXSTR  max number of string values
C
      INTEGER    MAXVAR, MAXINT, MAXREL, MAXLOG, MAXSTR
      PARAMETER (MAXVAR = 100, MAXINT = 40, MAXREL = 40, MAXLOG = 40,
     $           MAXSTR = 100)
C
C TOTVAR  total allocated variables
C VARNAM  variable name
C VARTYP  variable type
C VARSIZ  variable size (number of elements)
C VARPTR  pointer to first variable value
C VARMOD  variable mode value
C
      INTEGER      TOTVAR
      CHARACTER*20 VARNAM (MAXVAR)
      INTEGER      VARTYP (MAXVAR), VARSIZ (MAXVAR), VARPTR (MAXVAR),
     $             VARMOD (MAXVAR)
C
C TOTINT  total allocated integer values
C TOTREL  total allocated real values
C TOTLOG  total allocated logical values
C TOTSTR  total allocated string values
C INTVAL  integer values
C RELVAL  real values
C LOGVAL  logical values
C STRVAL  string values
C
      INTEGER      TOTINT, TOTREL, TOTLOG, TOTSTR
      INTEGER      INTVAL (MAXINT)
      REAL         RELVAL (MAXREL)
      LOGICAL      LOGVAL (MAXLOG)
      CHARACTER*80 STRVAL (MAXSTR)
C
      COMMON /GVDAT1/ TOTVAR, VARTYP, VARSIZ, VARPTR, VARMOD,
     $                TOTINT, TOTREL, TOTLOG, TOTSTR,
     $                INTVAL, RELVAL, LOGVAL
      COMMON /GVDAT2/ VARNAM, STRVAL
      SAVE   /GVDAT1/, /GVDAT2/
C
C To be sure to initialize
C
      EXTERNAL GVDINI
C
      INTEGER PTR, ERRCOD
C
C PTR     (In)  pointer to variable
C ERRCOD  (Out) error code
C     0 = no error
C     1 = no such variable
C
      IF (PTR .LT. 1  .OR.  PTR .GT. TOTVAR) THEN
        GVMODE = 0
        ERRCOD = 1
      ELSE
        GVMODE = VARMOD (PTR)
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C------------------------------------------------
      SUBROUTINE GVIMOD (IVAL, PTR, SIZE, ERRCOD)
C
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C
C MAXVAR  max number of variables
C MAXINT  max number of integer values
C MAXREL  max number of real values
C MAXLOG  max number of logical values
C MAXSTR  max number of string values
C
      INTEGER    MAXVAR, MAXINT, MAXREL, MAXLOG, MAXSTR
      PARAMETER (MAXVAR = 100, MAXINT = 40, MAXREL = 40, MAXLOG = 40,
     $           MAXSTR = 100)
C
C TOTVAR  total allocated variables
C VARNAM  variable name
C VARTYP  variable type
C VARSIZ  variable size (number of elements)
C VARPTR  pointer to first variable value
C VARMOD  variable mode value
C
      INTEGER      TOTVAR
      CHARACTER*20 VARNAM (MAXVAR)
      INTEGER      VARTYP (MAXVAR), VARSIZ (MAXVAR), VARPTR (MAXVAR),
     $             VARMOD (MAXVAR)
C
C TOTINT  total allocated integer values
C TOTREL  total allocated real values
C TOTLOG  total allocated logical values
C TOTSTR  total allocated string values
C INTVAL  integer values
C RELVAL  real values
C LOGVAL  logical values
C STRVAL  string values
C
      INTEGER      TOTINT, TOTREL, TOTLOG, TOTSTR
      INTEGER      INTVAL (MAXINT)
      REAL         RELVAL (MAXREL)
      LOGICAL      LOGVAL (MAXLOG)
      CHARACTER*80 STRVAL (MAXSTR)
C
      COMMON /GVDAT1/ TOTVAR, VARTYP, VARSIZ, VARPTR, VARMOD,
     $                TOTINT, TOTREL, TOTLOG, TOTSTR,
     $                INTVAL, RELVAL, LOGVAL
      COMMON /GVDAT2/ VARNAM, STRVAL
      SAVE   /GVDAT1/, /GVDAT2/
C
C To be sure to initialize
C
      EXTERNAL GVDINI
C
      INTEGER IVAL (*)
      INTEGER PTR, SIZE, ERRCOD
C
C IVAL    (In)  integer value(s)
C PTR     (In)  pointer to variable
C SIZE    (In)  size of variable (number of values)
C ERRCOD  (Out) error code
C     0 = no error
C     1 = no such variable
C     2 = wrong type
C     3 = invalid size, or too large
C
      INTEGER I
C
      IF (PTR .LT. 1  .OR.  PTR .GT. TOTVAR) THEN
        ERRCOD = 1
      ELSE IF (VARTYP (PTR) .NE. 1) THEN
        ERRCOD = 2
      ELSE IF (SIZE .LT. 1  .OR.  SIZE .GT. VARSIZ (PTR)) THEN
        ERRCOD = 3
      ELSE
        DO 100 I = 1, SIZE
          INTVAL (VARPTR (PTR) + I - 1) = IVAL (I)
100     CONTINUE
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C------------------------------------------------
      SUBROUTINE GVRMOD (RVAL, PTR, SIZE, ERRCOD)
C
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C
C MAXVAR  max number of variables
C MAXINT  max number of integer values
C MAXREL  max number of real values
C MAXLOG  max number of logical values
C MAXSTR  max number of string values
C
      INTEGER    MAXVAR, MAXINT, MAXREL, MAXLOG, MAXSTR
      PARAMETER (MAXVAR = 100, MAXINT = 40, MAXREL = 40, MAXLOG = 40,
     $           MAXSTR = 100)
C
C TOTVAR  total allocated variables
C VARNAM  variable name
C VARTYP  variable type
C VARSIZ  variable size (number of elements)
C VARPTR  pointer to first variable value
C VARMOD  variable mode value
C
      INTEGER      TOTVAR
      CHARACTER*20 VARNAM (MAXVAR)
      INTEGER      VARTYP (MAXVAR), VARSIZ (MAXVAR), VARPTR (MAXVAR),
     $             VARMOD (MAXVAR)
C
C TOTINT  total allocated integer values
C TOTREL  total allocated real values
C TOTLOG  total allocated logical values
C TOTSTR  total allocated string values
C INTVAL  integer values
C RELVAL  real values
C LOGVAL  logical values
C STRVAL  string values
C
      INTEGER      TOTINT, TOTREL, TOTLOG, TOTSTR
      INTEGER      INTVAL (MAXINT)
      REAL         RELVAL (MAXREL)
      LOGICAL      LOGVAL (MAXLOG)
      CHARACTER*80 STRVAL (MAXSTR)
C
      COMMON /GVDAT1/ TOTVAR, VARTYP, VARSIZ, VARPTR, VARMOD,
     $                TOTINT, TOTREL, TOTLOG, TOTSTR,
     $                INTVAL, RELVAL, LOGVAL
      COMMON /GVDAT2/ VARNAM, STRVAL
      SAVE   /GVDAT1/, /GVDAT2/
C
C To be sure to initialize
C
      EXTERNAL GVDINI
C
      REAL    RVAL (*)
      INTEGER PTR, SIZE, ERRCOD
C
C RVAL    (In)  real value(s)
C PTR     (In)  pointer to variable
C SIZE    (In)  size of variable (number of values)
C ERRCOD  (Out) error code
C     0 = no error
C     1 = no such variable
C     2 = wrong type
C     3 = invalid size, or too large
C
      INTEGER I
C
      IF (PTR .LT. 1  .OR.  PTR .GT. TOTVAR) THEN
        ERRCOD = 1
      ELSE IF (VARTYP (PTR) .NE. 2) THEN
        ERRCOD = 2
      ELSE IF (SIZE .LT. 1  .OR.  SIZE .GT. VARSIZ (PTR)) THEN
        ERRCOD = 3
      ELSE
        DO 100 I = 1, SIZE
          RELVAL (VARPTR (PTR) + I - 1) = RVAL (I)
100     CONTINUE
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C------------------------------------------------
      SUBROUTINE GVLMOD (LVAL, PTR, SIZE, ERRCOD)
C
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C
C MAXVAR  max number of variables
C MAXINT  max number of integer values
C MAXREL  max number of real values
C MAXLOG  max number of logical values
C MAXSTR  max number of string values
C
      INTEGER    MAXVAR, MAXINT, MAXREL, MAXLOG, MAXSTR
      PARAMETER (MAXVAR = 100, MAXINT = 40, MAXREL = 40, MAXLOG = 40,
     $           MAXSTR = 100)
C
C TOTVAR  total allocated variables
C VARNAM  variable name
C VARTYP  variable type
C VARSIZ  variable size (number of elements)
C VARPTR  pointer to first variable value
C VARMOD  variable mode value
C
      INTEGER      TOTVAR
      CHARACTER*20 VARNAM (MAXVAR)
      INTEGER      VARTYP (MAXVAR), VARSIZ (MAXVAR), VARPTR (MAXVAR),
     $             VARMOD (MAXVAR)
C
C TOTINT  total allocated integer values
C TOTREL  total allocated real values
C TOTLOG  total allocated logical values
C TOTSTR  total allocated string values
C INTVAL  integer values
C RELVAL  real values
C LOGVAL  logical values
C STRVAL  string values
C
      INTEGER      TOTINT, TOTREL, TOTLOG, TOTSTR
      INTEGER      INTVAL (MAXINT)
      REAL         RELVAL (MAXREL)
      LOGICAL      LOGVAL (MAXLOG)
      CHARACTER*80 STRVAL (MAXSTR)
C
      COMMON /GVDAT1/ TOTVAR, VARTYP, VARSIZ, VARPTR, VARMOD,
     $                TOTINT, TOTREL, TOTLOG, TOTSTR,
     $                INTVAL, RELVAL, LOGVAL
      COMMON /GVDAT2/ VARNAM, STRVAL
      SAVE   /GVDAT1/, /GVDAT2/
C
C To be sure to initialize
C
      EXTERNAL GVDINI
C
      LOGICAL LVAL (*)
      INTEGER PTR, SIZE, ERRCOD
C
C LVAL    (In)  logical value(s)
C PTR     (In)  pointer to variable
C SIZE    (In)  size of variable (number of values)
C ERRCOD  (Out) error code
C     0 = no error
C     1 = no such variable
C     2 = wrong type
C     3 = invalid size, or too large
C
      INTEGER I
C
      IF (PTR .LT. 1  .OR.  PTR .GT. TOTVAR) THEN
        ERRCOD = 1
      ELSE IF (VARTYP (PTR) .NE. 3) THEN
        ERRCOD = 2
      ELSE IF (SIZE .LT. 1  .OR.  SIZE .GT. VARSIZ (PTR)) THEN
        ERRCOD = 3
      ELSE
        DO 100 I = 1, SIZE
          LOGVAL (VARPTR (PTR) + I - 1) = LVAL (I)
100     CONTINUE
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C------------------------------------------------
      SUBROUTINE GVSMOD (SVAL, PTR, SIZE, ERRCOD)
C
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C
C MAXVAR  max number of variables
C MAXINT  max number of integer values
C MAXREL  max number of real values
C MAXLOG  max number of logical values
C MAXSTR  max number of string values
C
      INTEGER    MAXVAR, MAXINT, MAXREL, MAXLOG, MAXSTR
      PARAMETER (MAXVAR = 100, MAXINT = 40, MAXREL = 40, MAXLOG = 40,
     $           MAXSTR = 100)
C
C TOTVAR  total allocated variables
C VARNAM  variable name
C VARTYP  variable type
C VARSIZ  variable size (number of elements)
C VARPTR  pointer to first variable value
C VARMOD  variable mode value
C
      INTEGER      TOTVAR
      CHARACTER*20 VARNAM (MAXVAR)
      INTEGER      VARTYP (MAXVAR), VARSIZ (MAXVAR), VARPTR (MAXVAR),
     $             VARMOD (MAXVAR)
C
C TOTINT  total allocated integer values
C TOTREL  total allocated real values
C TOTLOG  total allocated logical values
C TOTSTR  total allocated string values
C INTVAL  integer values
C RELVAL  real values
C LOGVAL  logical values
C STRVAL  string values
C
      INTEGER      TOTINT, TOTREL, TOTLOG, TOTSTR
      INTEGER      INTVAL (MAXINT)
      REAL         RELVAL (MAXREL)
      LOGICAL      LOGVAL (MAXLOG)
      CHARACTER*80 STRVAL (MAXSTR)
C
      COMMON /GVDAT1/ TOTVAR, VARTYP, VARSIZ, VARPTR, VARMOD,
     $                TOTINT, TOTREL, TOTLOG, TOTSTR,
     $                INTVAL, RELVAL, LOGVAL
      COMMON /GVDAT2/ VARNAM, STRVAL
      SAVE   /GVDAT1/, /GVDAT2/
C
C To be sure to initialize
C
      EXTERNAL GVDINI
C
      CHARACTER*(*) SVAL (*)
      INTEGER       PTR, SIZE, ERRCOD
C
C SVAL    (In)  string value(s)
C PTR     (In)  pointer to variable
C SIZE    (In)  size of variable (number of values)
C ERRCOD  (Out) error code
C     0 = no error
C     1 = no such variable
C     2 = wrong type
C     3 = invalid size, or too large
C
      INTEGER I
C
      IF (PTR .LT. 1  .OR.  PTR .GT. TOTVAR) THEN
        ERRCOD = 1
      ELSE IF (VARTYP (PTR) .NE. 4) THEN
        ERRCOD = 2
      ELSE IF (SIZE .LT. 1  .OR.  SIZE .GT. VARSIZ (PTR)) THEN
        ERRCOD = 3
      ELSE
        DO 100 I = 1, SIZE
          STRVAL (VARPTR (PTR) + I - 1) = SVAL (I)
100     CONTINUE
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C------------------------------------------------
      SUBROUTINE GVIVAL (IVAL, PTR, SIZE, ERRCOD)
C
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C
C MAXVAR  max number of variables
C MAXINT  max number of integer values
C MAXREL  max number of real values
C MAXLOG  max number of logical values
C MAXSTR  max number of string values
C
      INTEGER    MAXVAR, MAXINT, MAXREL, MAXLOG, MAXSTR
      PARAMETER (MAXVAR = 100, MAXINT = 40, MAXREL = 40, MAXLOG = 40,
     $           MAXSTR = 100)
C
C TOTVAR  total allocated variables
C VARNAM  variable name
C VARTYP  variable type
C VARSIZ  variable size (number of elements)
C VARPTR  pointer to first variable value
C VARMOD  variable mode value
C
      INTEGER      TOTVAR
      CHARACTER*20 VARNAM (MAXVAR)
      INTEGER      VARTYP (MAXVAR), VARSIZ (MAXVAR), VARPTR (MAXVAR),
     $             VARMOD (MAXVAR)
C
C TOTINT  total allocated integer values
C TOTREL  total allocated real values
C TOTLOG  total allocated logical values
C TOTSTR  total allocated string values
C INTVAL  integer values
C RELVAL  real values
C LOGVAL  logical values
C STRVAL  string values
C
      INTEGER      TOTINT, TOTREL, TOTLOG, TOTSTR
      INTEGER      INTVAL (MAXINT)
      REAL         RELVAL (MAXREL)
      LOGICAL      LOGVAL (MAXLOG)
      CHARACTER*80 STRVAL (MAXSTR)
C
      COMMON /GVDAT1/ TOTVAR, VARTYP, VARSIZ, VARPTR, VARMOD,
     $                TOTINT, TOTREL, TOTLOG, TOTSTR,
     $                INTVAL, RELVAL, LOGVAL
      COMMON /GVDAT2/ VARNAM, STRVAL
      SAVE   /GVDAT1/, /GVDAT2/
C
C To be sure to initialize
C
      EXTERNAL GVDINI
C
      INTEGER IVAL (*)
      INTEGER PTR, SIZE, ERRCOD
C
C IVAL    (Out) integer value(s)
C PTR     (In)  pointer to variable
C SIZE    (In)  size of variable (number of values)
C ERRCOD  (Out) error code
C     0 = no error
C     1 = no such variable
C     2 = wrong type
C     3 = invalid size, or too large
C
      INTEGER I
C
      IF (PTR .LT. 1  .OR.  PTR .GT. TOTVAR) THEN
        ERRCOD = 1
      ELSE IF (VARTYP (PTR) .NE. 1) THEN
        ERRCOD = 2
      ELSE IF (SIZE .LT. 1  .OR.  SIZE .GT. VARSIZ (PTR)) THEN
        ERRCOD = 3
      ELSE
        DO 100 I = 1, SIZE
          IVAL (I) = INTVAL (VARPTR (PTR) + I - 1)
100     CONTINUE
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C------------------------------------------------
      SUBROUTINE GVRVAL (RVAL, PTR, SIZE, ERRCOD)
C
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C
C MAXVAR  max number of variables
C MAXINT  max number of integer values
C MAXREL  max number of real values
C MAXLOG  max number of logical values
C MAXSTR  max number of string values
C
      INTEGER    MAXVAR, MAXINT, MAXREL, MAXLOG, MAXSTR
      PARAMETER (MAXVAR = 100, MAXINT = 40, MAXREL = 40, MAXLOG = 40,
     $           MAXSTR = 100)
C
C TOTVAR  total allocated variables
C VARNAM  variable name
C VARTYP  variable type
C VARSIZ  variable size (number of elements)
C VARPTR  pointer to first variable value
C VARMOD  variable mode value
C
      INTEGER      TOTVAR
      CHARACTER*20 VARNAM (MAXVAR)
      INTEGER      VARTYP (MAXVAR), VARSIZ (MAXVAR), VARPTR (MAXVAR),
     $             VARMOD (MAXVAR)
C
C TOTINT  total allocated integer values
C TOTREL  total allocated real values
C TOTLOG  total allocated logical values
C TOTSTR  total allocated string values
C INTVAL  integer values
C RELVAL  real values
C LOGVAL  logical values
C STRVAL  string values
C
      INTEGER      TOTINT, TOTREL, TOTLOG, TOTSTR
      INTEGER      INTVAL (MAXINT)
      REAL         RELVAL (MAXREL)
      LOGICAL      LOGVAL (MAXLOG)
      CHARACTER*80 STRVAL (MAXSTR)
C
      COMMON /GVDAT1/ TOTVAR, VARTYP, VARSIZ, VARPTR, VARMOD,
     $                TOTINT, TOTREL, TOTLOG, TOTSTR,
     $                INTVAL, RELVAL, LOGVAL
      COMMON /GVDAT2/ VARNAM, STRVAL
      SAVE   /GVDAT1/, /GVDAT2/
C
C To be sure to initialize
C
      EXTERNAL GVDINI
C
      REAL    RVAL (*)
      INTEGER PTR, SIZE, ERRCOD
C
C RVAL    (Out) real value(s)
C PTR     (In)  pointer to variable
C SIZE    (In)  size of variable (number of values)
C ERRCOD  (Out) error code
C     0 = no error
C     1 = no such variable
C     2 = wrong type
C     3 = invalid size, or too large
C
      INTEGER I
C
      IF (PTR .LT. 1  .OR.  PTR .GT. TOTVAR) THEN
        ERRCOD = 1
      ELSE IF (VARTYP (PTR) .NE. 2) THEN
        ERRCOD = 2
      ELSE IF (SIZE .LT. 1  .OR.  SIZE .GT. VARSIZ (PTR)) THEN
        ERRCOD = 3
      ELSE
        DO 100 I = 1, SIZE
          RVAL (I) = RELVAL (VARPTR (PTR) + I - 1)
100     CONTINUE
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C------------------------------------------------
      SUBROUTINE GVLVAL (LVAL, PTR, SIZE, ERRCOD)
C
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C
C MAXVAR  max number of variables
C MAXINT  max number of integer values
C MAXREL  max number of real values
C MAXLOG  max number of logical values
C MAXSTR  max number of string values
C
      INTEGER    MAXVAR, MAXINT, MAXREL, MAXLOG, MAXSTR
      PARAMETER (MAXVAR = 100, MAXINT = 40, MAXREL = 40, MAXLOG = 40,
     $           MAXSTR = 100)
C
C TOTVAR  total allocated variables
C VARNAM  variable name
C VARTYP  variable type
C VARSIZ  variable size (number of elements)
C VARPTR  pointer to first variable value
C VARMOD  variable mode value
C
      INTEGER      TOTVAR
      CHARACTER*20 VARNAM (MAXVAR)
      INTEGER      VARTYP (MAXVAR), VARSIZ (MAXVAR), VARPTR (MAXVAR),
     $             VARMOD (MAXVAR)
C
C TOTINT  total allocated integer values
C TOTREL  total allocated real values
C TOTLOG  total allocated logical values
C TOTSTR  total allocated string values
C INTVAL  integer values
C RELVAL  real values
C LOGVAL  logical values
C STRVAL  string values
C
      INTEGER      TOTINT, TOTREL, TOTLOG, TOTSTR
      INTEGER      INTVAL (MAXINT)
      REAL         RELVAL (MAXREL)
      LOGICAL      LOGVAL (MAXLOG)
      CHARACTER*80 STRVAL (MAXSTR)
C
      COMMON /GVDAT1/ TOTVAR, VARTYP, VARSIZ, VARPTR, VARMOD,
     $                TOTINT, TOTREL, TOTLOG, TOTSTR,
     $                INTVAL, RELVAL, LOGVAL
      COMMON /GVDAT2/ VARNAM, STRVAL
      SAVE   /GVDAT1/, /GVDAT2/
C
C To be sure to initialize
C
      EXTERNAL GVDINI
C
      LOGICAL LVAL (*)
      INTEGER PTR, SIZE, ERRCOD
C
C LVAL    (Out) logical value(s)
C PTR     (In)  pointer to variable
C SIZE    (In)  size of variable (number of values)
C ERRCOD  (Out) error code
C     0 = no error
C     1 = no such variable
C     2 = wrong type
C     3 = invalid size, or too large
C
      INTEGER I
C
      IF (PTR .LT. 1  .OR.  PTR .GT. TOTVAR) THEN
        ERRCOD = 1
      ELSE IF (VARTYP (PTR) .NE. 3) THEN
        ERRCOD = 2
      ELSE IF (SIZE .LT. 1  .OR.  SIZE .GT. VARSIZ (PTR)) THEN
        ERRCOD = 3
      ELSE
        DO 100 I = 1, SIZE
          LVAL (I) = LOGVAL (VARPTR (PTR) + I - 1)
100     CONTINUE
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C------------------------------------------------
      SUBROUTINE GVSVAL (SVAL, PTR, SIZE, ERRCOD)
C
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C
C MAXVAR  max number of variables
C MAXINT  max number of integer values
C MAXREL  max number of real values
C MAXLOG  max number of logical values
C MAXSTR  max number of string values
C
      INTEGER    MAXVAR, MAXINT, MAXREL, MAXLOG, MAXSTR
      PARAMETER (MAXVAR = 100, MAXINT = 40, MAXREL = 40, MAXLOG = 40,
     $           MAXSTR = 100)
C
C TOTVAR  total allocated variables
C VARNAM  variable name
C VARTYP  variable type
C VARSIZ  variable size (number of elements)
C VARPTR  pointer to first variable value
C VARMOD  variable mode value
C
      INTEGER      TOTVAR
      CHARACTER*20 VARNAM (MAXVAR)
      INTEGER      VARTYP (MAXVAR), VARSIZ (MAXVAR), VARPTR (MAXVAR),
     $             VARMOD (MAXVAR)
C
C TOTINT  total allocated integer values
C TOTREL  total allocated real values
C TOTLOG  total allocated logical values
C TOTSTR  total allocated string values
C INTVAL  integer values
C RELVAL  real values
C LOGVAL  logical values
C STRVAL  string values
C
      INTEGER      TOTINT, TOTREL, TOTLOG, TOTSTR
      INTEGER      INTVAL (MAXINT)
      REAL         RELVAL (MAXREL)
      LOGICAL      LOGVAL (MAXLOG)
      CHARACTER*80 STRVAL (MAXSTR)
C
      COMMON /GVDAT1/ TOTVAR, VARTYP, VARSIZ, VARPTR, VARMOD,
     $                TOTINT, TOTREL, TOTLOG, TOTSTR,
     $                INTVAL, RELVAL, LOGVAL
      COMMON /GVDAT2/ VARNAM, STRVAL
      SAVE   /GVDAT1/, /GVDAT2/
C
C To be sure to initialize
C
      EXTERNAL GVDINI
C
      CHARACTER*(*) SVAL (*)
      INTEGER       PTR, SIZE, ERRCOD
C
C SVAL    (Out) string value(s)
C PTR     (In)  pointer to variable
C SIZE    (In)  size of variable (number of values)
C ERRCOD  (Out) error code
C     0 = no error
C     1 = no such variable
C     2 = wrong type
C     3 = invalid size, or too large
C
      INTEGER I
C
      IF (PTR .LT. 1  .OR.  PTR .GT. TOTVAR) THEN
        ERRCOD = 1
      ELSE IF (VARTYP (PTR) .NE. 4) THEN
        ERRCOD = 2
      ELSE IF (SIZE .LT. 1  .OR.  SIZE .GT. VARSIZ (PTR)) THEN
        ERRCOD = 3
      ELSE
        DO 100 I = 1, SIZE
          SVAL (I) = STRVAL (VARPTR (PTR) + I - 1)
100     CONTINUE
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C-----------------------------------
      SUBROUTINE GVREM (PTR, ERRCOD)
C
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C
C MAXVAR  max number of variables
C MAXINT  max number of integer values
C MAXREL  max number of real values
C MAXLOG  max number of logical values
C MAXSTR  max number of string values
C
      INTEGER    MAXVAR, MAXINT, MAXREL, MAXLOG, MAXSTR
      PARAMETER (MAXVAR = 100, MAXINT = 40, MAXREL = 40, MAXLOG = 40,
     $           MAXSTR = 100)
C
C TOTVAR  total allocated variables
C VARNAM  variable name
C VARTYP  variable type
C VARSIZ  variable size (number of elements)
C VARPTR  pointer to first variable value
C VARMOD  variable mode value
C
      INTEGER      TOTVAR
      CHARACTER*20 VARNAM (MAXVAR)
      INTEGER      VARTYP (MAXVAR), VARSIZ (MAXVAR), VARPTR (MAXVAR),
     $             VARMOD (MAXVAR)
C
C TOTINT  total allocated integer values
C TOTREL  total allocated real values
C TOTLOG  total allocated logical values
C TOTSTR  total allocated string values
C INTVAL  integer values
C RELVAL  real values
C LOGVAL  logical values
C STRVAL  string values
C
      INTEGER      TOTINT, TOTREL, TOTLOG, TOTSTR
      INTEGER      INTVAL (MAXINT)
      REAL         RELVAL (MAXREL)
      LOGICAL      LOGVAL (MAXLOG)
      CHARACTER*80 STRVAL (MAXSTR)
C
      COMMON /GVDAT1/ TOTVAR, VARTYP, VARSIZ, VARPTR, VARMOD,
     $                TOTINT, TOTREL, TOTLOG, TOTSTR,
     $                INTVAL, RELVAL, LOGVAL
      COMMON /GVDAT2/ VARNAM, STRVAL
      SAVE   /GVDAT1/, /GVDAT2/
C
C To be sure to initialize
C
      EXTERNAL GVDINI
C
      INTEGER PTR, ERRCOD
C
C PTR     (In)  pointer to variable
C ERRCOD  (Out) eror code
C     0 = no error
C     1 = no such variable
C
      INTEGER I, TYPE, SIZE
C
      IF (PTR .LT. 1  .OR.  PTR .GT. TOTVAR) THEN
        ERRCOD = 1
        RETURN
      END IF
C
      TYPE = VARTYP (PTR)
      SIZE = VARSIZ (PTR)
C
C Deallocate variable values space
C
      IF (TYPE .EQ. 1) THEN
        TOTINT = TOTINT - SIZE
        DO 100 I = VARPTR (PTR), TOTINT
          INTVAL (I) = INTVAL (I + SIZE)
100     CONTINUE
C
      ELSE IF (TYPE .EQ. 2) THEN
        TOTREL = TOTREL - SIZE
        DO 200 I = VARPTR (PTR), TOTREL
          RELVAL (I) = RELVAL (I + SIZE)
200     CONTINUE
C
      ELSE IF (TYPE .EQ. 3) THEN
        TOTLOG = TOTLOG - SIZE
        DO 300 I = VARPTR (PTR), TOTLOG
          LOGVAL (I) = LOGVAL (I + SIZE)
300     CONTINUE
C
      ELSE
        TOTSTR = TOTSTR - SIZE
        DO 400 I = VARPTR (PTR), TOTSTR
          STRVAL (I) = STRVAL (I + SIZE)
400     CONTINUE
      END IF
C
C Modify the affected pointers
C
      DO 500 I = 1, TOTVAR
        IF (VARTYP (I) .EQ. TYPE  .AND.  VARPTR (I) .GT. VARPTR (PTR))
     $    VARPTR (I) = VARPTR (I) - SIZE
500   CONTINUE
C
C Swap down variable pointers
C
      TOTVAR = TOTVAR - 1
      DO 600 I = PTR, TOTVAR
        VARNAM (I) = VARNAM (I + 1)
        VARTYP (I) = VARTYP (I + 1)
        VARSIZ (I) = VARSIZ (I + 1)
        VARPTR (I) = VARPTR (I + 1)
        VARMOD (I) = VARMOD (I + 1)
600   CONTINUE
C
      ERRCOD = 0
      RETURN
      END
C
C
C----------------------
      BLOCK DATA GVDINI
C
C Package GENVAR
C
C General pseudo-dynamic symbolic variable handler.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   8-Jul-1988  first attempts
C
C MAXVAR  max number of variables
C MAXINT  max number of integer values
C MAXREL  max number of real values
C MAXLOG  max number of logical values
C MAXSTR  max number of string values
C
      INTEGER    MAXVAR, MAXINT, MAXREL, MAXLOG, MAXSTR
      PARAMETER (MAXVAR = 100, MAXINT = 40, MAXREL = 40, MAXLOG = 40,
     $           MAXSTR = 100)
C
C TOTVAR  total allocated variables
C VARNAM  variable name
C VARTYP  variable type
C VARSIZ  variable size (number of elements)
C VARPTR  pointer to first variable value
C VARMOD  variable mode value
C
      INTEGER      TOTVAR
      CHARACTER*20 VARNAM (MAXVAR)
      INTEGER      VARTYP (MAXVAR), VARSIZ (MAXVAR), VARPTR (MAXVAR),
     $             VARMOD (MAXVAR)
C
C TOTINT  total allocated integer values
C TOTREL  total allocated real values
C TOTLOG  total allocated logical values
C TOTSTR  total allocated string values
C INTVAL  integer values
C RELVAL  real values
C LOGVAL  logical values
C STRVAL  string values
C
      INTEGER      TOTINT, TOTREL, TOTLOG, TOTSTR
      INTEGER      INTVAL (MAXINT)
      REAL         RELVAL (MAXREL)
      LOGICAL      LOGVAL (MAXLOG)
      CHARACTER*80 STRVAL (MAXSTR)
C
      COMMON /GVDAT1/ TOTVAR, VARTYP, VARSIZ, VARPTR, VARMOD,
     $                TOTINT, TOTREL, TOTLOG, TOTSTR,
     $                INTVAL, RELVAL, LOGVAL
      COMMON /GVDAT2/ VARNAM, STRVAL
      SAVE   /GVDAT1/, /GVDAT2/
C
      DATA TOTVAR /0/, TOTINT /0/, TOTREL /0/, TOTLOG /0/, TOTSTR /0/
C
      END
C Read help text file and output requested parts.
C
C Version 1.0
C
C This procedure uses a number of separately compiled procedures,
C since it can be assumed that it will be used in programs where
C these procedures will be used anyhow.
C
C Note: it is assumed that the calling program has already
C initialized the lexical line analyzer properly.
C
C Questions issued to user by procedure, and their possible answers:
C
C "Topic > "
C     CR  : skip out to end of help file
C     ?   : jump back to start of help file
C     ... : word(s) to output further help text on
C
C "Continue > "
C     CR : output one more screenful of current word help text
C     ?  : jump back to start of help file
C     Q  : quit current help text, and skip to end of help file
C
C "End of help > " and "No help found > "
C     CR  : exit from procedure
C     ?   : jump back to start of help file
C     ... : word(s) to output help on, as if reentering procedure
C
C Uses separately compiled procedures/packages:
C
C STRUTL p  character case control
C STRBLK p  find and eliminate blanks in strings
C LEXANA p  lexical line analyser; find words and delimiters
C TTYIOP p  terminal input/output procedures
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden
C   26-Feb-1988  first attempts
C    2-Mar-1988  first working version
C    8-Mar-1988  introduced found flag
C
C----------------------------------------------------
      SUBROUTINE HLPTXT (LUN, FILNAM, TOPLIN, ERRCOD)
C
      INTEGER       LUN, ERRCOD
      CHARACTER*(*) FILNAM, TOPLIN
C
C LUN     (In)    logical unit number to use
C FILNAM  (In)    name of help text file
C TOPLIN  (InOut) line on topic to output help on; may be modified
C ERRCOD  (Out)   error code
C     0 = no error
C     1 = could not open or read help text file
C     2 = too many topic words
C     3 = input error encountered
C     4 = format error in file; no word or level in title line
C
C Externally defined functions
C
      INTEGER SLNBLK
C
C MAXTOP  max number of topic words
C SCREEN  max number of lines output to screen at one go
C MARKER  title line marker character
C
      INTEGER     MAXTOP, SCREEN
      CHARACTER*1 MARKER
      PARAMETER  (MAXTOP = 9, SCREEN = 20, MARKER = '>')
C
C TOPPOS  positions of topic words in topic line
C TOPNUM  total number of words in topic line
C TOPCUR  current topic word
C
      INTEGER      TOPPOS (2, MAXTOP)
      INTEGER      TOPNUM, TOPCUR
C
C OUTNUM  number of lines output to screen
C OUTPUT  flag when reading text to output to user
C MATCH   previous topic word matched with title word
C FOUND   flag if requested topic found
C
      INTEGER OUTNUM
      LOGICAL OUTPUT, MATCH, FOUND
C
C Help variables
C
      CHARACTER*80 LINE
      INTEGER      DEPTH, LAST
C
C Open the help text file; to check that it exists
C
      OPEN (UNIT=LUN, FILE=FILNAM, STATUS='OLD',
     $      FORM='FORMATTED', READONLY, ERR=900)
C
C Analyze topic line, rewind help text file in case this is a loopback
C
100   CALL LXLINE (TOPLIN, MAXTOP, TOPPOS, TOPNUM, ERRCOD)
      IF (ERRCOD .EQ. 1) GOTO 910
      REWIND (UNIT=LUN)
C
C Upcase for comparison, init current topic word counter and flags
C If no topic, then output help file start text
C
      IF (TOPNUM .EQ. 0) THEN
        TOPCUR = 0
        OUTNUM = 0
        MATCH = .TRUE.
        FOUND = .TRUE.
        OUTPUT = .TRUE.
      ELSE
        CALL SUPCAS (TOPLIN (: TOPPOS (2, TOPNUM)))
        TOPCUR = 1
        MATCH = .FALSE.
        FOUND = .FALSE.
        OUTPUT = .FALSE.
      END IF
C
C Loop until correct depth and word of help tree has been found
C
200   READ (UNIT=LUN, FMT=210, END=800, ERR=900) LINE
210   FORMAT (A)
C
C Line is a title line; get depth of it
C
      IF (LINE (1:1) .EQ. MARKER) THEN
C
        READ (LINE (2:2), 220, ERR=930) DEPTH
220     FORMAT (I1)
        IF (DEPTH .EQ. 0) GOTO 930
C
C Same title line depth as current topic word
C
        IF (DEPTH .EQ. TOPCUR) THEN
C
C Get word from title; check that there is a word
C
          LINE = LINE (3 :)
          CALL SLESHF (LINE)
          LAST = SLNBLK (LINE)
          IF (LAST .EQ. 0) GOTO 930
          CALL SUPCAS (LINE (: LAST))
C
C Test if what we have of the words match
C
          IF (TOPPOS(2,TOPCUR) - TOPPOS(1,TOPCUR) + 1 .LT. LAST) THEN
            MATCH = TOPLIN (TOPPOS(1,TOPCUR) : TOPPOS(2,TOPCUR)) .EQ.
     $              LINE (: TOPPOS(2,TOPCUR) - TOPPOS(1,TOPCUR) + 1)
          ELSE
            MATCH = TOPLIN (TOPPOS(1,TOPCUR) :
     $                      TOPPOS(1,TOPCUR) + LAST - 1) .EQ. LINE
          END IF
C
          IF (MATCH) THEN
C
C Words match at this level; increment and skip back for next match
C
            IF (TOPNUM .GT. TOPCUR) THEN
              MATCH = .TRUE.
              TOPCUR = TOPCUR + 1
C
C Last topic to match; set flags for text output
C
            ELSE
              OUTNUM = 0
              FOUND = .TRUE.
              OUTPUT = .TRUE.
            END IF
C
C Skip this part if current topic word and title word do not match
C
          ELSE
            OUTPUT = .FALSE.
          END IF
C
C Finished if title line depth value is lower than current topic
C
        ELSE IF (DEPTH .LT. TOPCUR) THEN
          GOTO 800
C
C New choice if prev word matched and depth value is higher than prev
C
        ELSE IF (DEPTH .GT. TOPNUM .AND. MATCH) THEN
C
          CALL TTYGTL ('Topic > ', LINE, ERRCOD)
          IF (ERRCOD .NE. 0) GOTO 920
          CALL SLESHF (LINE)
C
C Exit if no more topic given
C
          IF (LINE .EQ. ' ') THEN
            GOTO 800
C
C Start again if '?' given
C
          ELSE IF (LINE .EQ. '?') THEN
            TOPLIN = ' '
            GOTO 100
C
C Add on topic word(s) to topic line; reanalyze the line
C
          ELSE
            LAST = SLNBLK (LINE)
            CALL SUPCAS (LINE (: LAST))
            CALL SAPPND (TOPLIN, ' ' // LINE (: LAST))
            CALL LXLINE (TOPLIN, MAXTOP, TOPPOS, TOPNUM, ERRCOD)
            IF (ERRCOD .EQ. 1) GOTO 910
C
C Start looking at added word and current line
C
            FOUND = .FALSE.
            OUTPUT = .FALSE.
            TOPCUR = TOPCUR + 1
            BACKSPACE (UNIT=LUN)
          END IF
C
        END IF
C
C Output help text if flag set
C
      ELSE IF (OUTPUT) THEN
C
        LAST = SLNBLK (LINE)
        IF (LAST .EQ. 0) THEN
          CALL TTYCR (1)
        ELSE
          CALL TTYPTL (LINE (: LAST))
        END IF
C
C Wait for go-ahead from user when screen full
C
        OUTNUM = OUTNUM + 1
        IF (MOD (OUTNUM, SCREEN) .EQ. 0) THEN
          CALL TTYCR (1)
          CALL TTYGTL ('Continue > ', LINE, ERRCOD)
          IF (ERRCOD .NE. 0) GOTO 920
          CALL TTYCR (1)
C
          IF (LINE .EQ. 'q' .OR. LINE .EQ. 'Q') THEN
            GOTO 800
          ELSE IF (LINE .EQ. '?') THEN
            TOPLIN = ' '
            GOTO 100
          END IF
        END IF
C
      END IF
C
C Loop back for next line in help file
C
      GOTO 200
C
C Finish output when nothing more left, or return again
C
800   IF (FOUND) THEN
        CALL TTYGTL ('End of help > ', TOPLIN, ERRCOD)
      ELSE
        CALL TTYGTL ('No help found > ', TOPLIN, ERRCOD)
      END IF
      IF (ERRCOD .NE. 0) THEN
        GOTO 920
      ELSE IF (TOPLIN .EQ. '?') THEN
        TOPLIN = ' '
        GOTO 100
      ELSE IF (TOPLIN .NE. ' ') THEN
        GOTO 100
      ELSE
        CLOSE (UNIT=LUN)
        ERRCOD = 0
        RETURN
      END IF
C
900   ERRCOD = 1
      RETURN
C
910   CLOSE (UNIT=LUN)
      ERRCOD = 2
      RETURN
C
920   CLOSE (UNIT=LUN)
      ERRCOD = 3
      RETURN
C
930   CLOSE (UNIT=LUN)
      ERRCOD = 4
      RETURN
C
      END
C Package LEXANA
C
C Lexical analysis of text, string or file.
C
C Definitions used in this package:
C
C Separators are characters without any syntactical meaning except
C for separating items. Delimiters are characters that have syntactical
C meaning, as well as delimiting words, such as '=' and ')' in Fortran.
C
C String delimiters enclose a string (which may contain any characters
C except string delimiters). A comment delimiter indicates the start
C of a comment, and the end-of-line is end of comment.
C
C An item is either a word between separators, a delimiter character,
C or a string of characters (string delimiters included). End-of-line
C and end-of-record in files are always separators, and strings may not
C contain such characters.
C
C Warning: results will be unpredictable if LXINIT is not called first.
C
C Note: ascii (tab) = 9, ascii (') = 39
C
C LXINIT s   set separator, delimiter, string and comment character sets
C LXCURR s   return current character sets
C LXLINE s   analyze whole line, find items
C LXFILE s   analyze formatted file, return items one by one
C LXNEXT s   find next item in line
C LXDATA o   character lexical type code table
C
C Uses no separately compiled procedures.
C
C Copyright (C) 1987, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   2-May-1987  main code written
C  27-Jun-1987  get and split actions into separate procedures
C  15-Sep-1987  rearrangement, only parse (not split), tokens added
C  13-Oct-1987  added removal of comment in line
C  21-Apr-1988  added parsing of formatted file
C  30-May-1988  terminology: this is lexical analysis, not parsing
C  18-Jun-1988  LXNEXT added and used in other procedures
C   2-Sep-1988  LXCURR added
C   9-Jan-1990  major rewrite; string, comment delimiter added;
C               LXSPLT and LXREMC removed; interface partly changed
C  26-Jun-1990  modified LXCURR to give 'NONE' when no characters in set
C
C
C----------------------------------------------------
      SUBROUTINE LXINIT (SEPARC, DELIMC, STRC, COMMC)
C
      CHARACTER*(*) SEPARC, DELIMC, STRC, COMMC
C
C SEPARC  (In)  string of separator characters, 'NONE' if none
C DELIMC  (In)  string of delimiter characters, 'NONE' if none
C STRC    (In)  string of string delimiter characters, 'NONE' if none
C COMMC   (In)  string of comment start characters, 'NONE' if none
C
C Character lexical set codes
C
      INTEGER    LXTOK, LXSEP, LXDEL, LXSTR, LXCOM
      PARAMETER (LXTOK = 0, LXSEP = 1, LXDEL = 2, LXSTR = 3, LXCOM = 4)
C
C Character set list
C
      INTEGER LXDSET (0:128)
C
      COMMON /LXDATA/ LXDSET
      SAVE   /LXDATA/
C
C Help variable
C
      INTEGER I
C
C All characters are ordinary token characters by default
C
      DO 10 I = 0, 128
        LXDSET (I) = LXTOK
10    CONTINUE
C
C Load character set codes
C
      IF (SEPARC .NE. 'NONE') THEN
        DO 100 I = 1, LEN (SEPARC)
          LXDSET (ICHAR (SEPARC (I:I))) = LXSEP
100     CONTINUE
      END IF
C
      IF (DELIMC .NE. 'NONE') THEN
        DO 200 I = 1, LEN (DELIMC)
          LXDSET (ICHAR (DELIMC (I:I))) = LXDEL
200     CONTINUE
      END IF
C
      IF (STRC .NE. 'NONE') THEN
        DO 300 I = 1, LEN (STRC)
          LXDSET (ICHAR (STRC (I:I))) = LXSTR
300     CONTINUE
      END IF
C
      IF (COMMC .NE. 'NONE') THEN
        DO 400 I = 1, LEN (COMMC)
          LXDSET (ICHAR (COMMC (I:I))) = LXCOM
400     CONTINUE
      END IF
C
      RETURN
      END

C
C
C---------------------------------------------------------
      SUBROUTINE LXCURR (SEPARC, SEPARN, DELIMC, DELIMN,
     $                   STRC, STRN, COMMC, COMMN, ERRCOD)
C
      CHARACTER*(*) SEPARC, DELIMC, STRC, COMMC
      INTEGER       SEPARN, DELIMN, STRN, COMMN, ERRCOD
C
C SEPARC  (Out) string of separator characters
C SEPARN  (Out) number of separator characters
C DELIMC  (Out) string of delimiter characters
C DELIMN  (Out) number of delimiter characters
C STRC    (Out) string of string delimiter characters
C STRN    (Out) number of string delimiter characters
C COMMC   (Out) string of comment start characters
C COMMN   (Out) number of comment start characters
C ERRCOD  (Out) error code
C     0 = no error
C     1 = some characters did not fit into string
C
C Character lexical set codes
C
      INTEGER    LXTOK, LXSEP, LXDEL, LXSTR, LXCOM
      PARAMETER (LXTOK = 0, LXSEP = 1, LXDEL = 2, LXSTR = 3, LXCOM = 4)
C
C Character set list
C
      INTEGER LXDSET (0:128)
C
      COMMON /LXDATA/ LXDSET
      SAVE   /LXDATA/
C
C Help variable
C
      INTEGER I
C
C Init
C
      SEPARN = 0
      DELIMN = 0
      STRN   = 0
      COMMN  = 0
C
C Loop through characters
C
      DO 10 I = 0, 128
C
        GOTO (100, 200, 300, 400) LXDSET (I)
C
        GOTO 10
C
  100   IF (SEPARN .GE. LEN (SEPARC)) GOTO 900
        SEPARN = SEPARN + 1
        SEPARC (SEPARN:SEPARN) = CHAR (I)
        GOTO 10
C
  200   IF (DELIMN .GE. LEN (DELIMC)) GOTO 900
        DELIMN = DELIMN + 1
        DELIMC (DELIMN:DELIMN) = CHAR (I)
        GOTO 10
C
  300   IF (STRN .GE. LEN (STRC)) GOTO 900
        STRN = STRN + 1
        STRC (STRN:STRN) = CHAR (I)
        GOTO 10
C
  400   IF (COMMN .GE. LEN (COMMC)) GOTO 900
        COMMN = COMMN + 1
        COMMC (COMMN:COMMN) = CHAR (I)
        GOTO 10
C
10    CONTINUE
C
C Check if any empty set
C
      IF (SEPARN .EQ. 0) THEN
        SEPARC = 'NONE'
        SEPARN = 4
      END IF
C
      IF (DELIMN .EQ. 0) THEN
        DELIMC = 'NONE'
        DELIMN = 4
      END IF
C
      IF (STRN .EQ. 0) THEN
        STRC = 'NONE'
        STRN = 4
      END IF
C
      IF (COMMN .EQ. 0) THEN
        COMMC = 'NONE'
        COMMN = 4
      END IF
C
      ERRCOD = 0
      RETURN
C
900   ERRCOD = 1
      RETURN
      END
C
C
C---------------------------------------------------------------------
      SUBROUTINE LXLINE (LINE, MAXITM, ITMPOS, ITMTYP, ITMNUM, ERRCOD)
C
      CHARACTER*(*) LINE
      INTEGER       MAXITM, ITMNUM, ERRCOD
      INTEGER       ITMPOS (2, MAXITM), ITMTYP (MAXITM)
C
C LINE    (In)  line to analyze
C MAXITM  (In)  maximum number of items
C ITMPOS  (Out) first and last character of items
C ITMTYP  (Out) item type codes
C     0 = ordinary token
C     1 = delimiter token
C     2 = string token
C ITMNUM  (Out) number of items
C ERRCOD  (Out) error code
C     0 = no error
C     1 = too many items in text line; the last skipped
C     2 = string not finished at end-of-line
C
C Help variables
C
      INTEGER LINLEN, ITYP
      INTEGER IPOS (2)
C
C Init
C
      LINLEN = LEN (LINE)
      IPOS (2) = 0
      ITMNUM = 0
C
C Get one item, and put its pointers and type into arrays
C
100   CALL LXNEXT (LINE, IPOS, ITYP, LINLEN, ERRCOD)
C
C No more items left in string; just return
C
      IF (ERRCOD .EQ. 1) THEN
        ERRCOD = 0
        RETURN
C
C String not finished
C
      ELSE IF (ERRCOD .EQ. 2) THEN
        RETURN
C
C If no more space in pointers array, return with failure
C
      ELSE IF (ITMNUM .GE. MAXITM) THEN
        ERRCOD = 1
        RETURN
      END IF
C
C Transfer pointers and item type to arrays
C
      ITMNUM = ITMNUM + 1
      ITMPOS (1, ITMNUM) = IPOS (1)
      ITMPOS (2, ITMNUM) = IPOS (2)
      ITMTYP (ITMNUM) = ITYP
C
C Return to get next item
C
      GOTO 100
C
      END
C
C
C--------------------------------------------------------------------
      SUBROUTINE LXFILE (LINE, IPOS, ITYP, LNUM, LINLEN, LUN, ERRCOD)
C
      CHARACTER*(*) LINE
      INTEGER       ITYP, LNUM, LUN, LINLEN, ERRCOD
      INTEGER       IPOS (2)
C
C LINE   (InOut) string to store latest line read from file
C IPOS   (InOut) first and last position of (latest) found item
C                IPOS (2) = 0 if no line read from file
C ITYP   (Out) item type
C     0 = ordinary token
C     1 = delimiter token
C     2 = string token
C LNUM   (Out) number of new lines read from file
C LINLEN (In)  length of string; limits record length readable from file
C LUN    (In)  logical unit to read from; must be opened formatted file
C ERRCOD (Out) error code
C     0 = no error
C     1 = end-of-file reached
C     2 = could not read from file
C     3 = string not finished at end-of-line
C
C Read new line from file if none read earlier
C
      IF (IPOS (2) .EQ. 0) THEN
        READ (LUN, 100, END=900, ERR=910) LINE
100     FORMAT (A)
        LNUM = 1
      ELSE
        LNUM = 0
      END IF
C
C Get next word or delimiter in string, and return if any found
C
200   CALL LXNEXT (LINE, IPOS, ITYP, LINLEN, ERRCOD)
C
C No more item in line; get next
C
      IF (ERRCOD .EQ. 1) THEN
        READ (LUN, 100, END=900, ERR=910) LINE
        LNUM = LNUM + 1
        IPOS (2) = 0
        GOTO 200
C
C String not finished in line
C
      ELSE IF (ERRCOD .EQ. 2) THEN
        ERRCOD = 3
        RETURN
      END IF
C
C Return if no error
C
      RETURN
C
900   ERRCOD = 1
      RETURN
910   ERRCOD = 2
      RETURN
      END
C
C
C---------------------------------------------------------
      SUBROUTINE LXNEXT (LINE, IPOS, ITYP, LINLEN, ERRCOD)
C
      CHARACTER*(*) LINE
      INTEGER       IPOS (2)
      INTEGER       ITYP, LINLEN, ERRCOD
C
C LINE    (In)  string to find next word or delimiter in
C IPOS    (InOut) first and last position of (latest) found item
C                 (0, 0) if new line to start processing
C ITYP    (Out) item type
C     0 = ordinary token
C     1 = delimiter token
C     2 = string token
C LINLEN  (In)  length of string
C ERRCOD  (Out) error code
C     0 = no error
C     1 = no more item in line
C     2 = string not finished at end-of-line
C
C Character lexical set codes
C
      INTEGER    LXTOK, LXSEP, LXDEL, LXSTR, LXCOM
      PARAMETER (LXTOK = 0, LXSEP = 1, LXDEL = 2, LXSTR = 3, LXCOM = 4)
C
C Character set list
C
      INTEGER LXDSET (0:128)
C
      COMMON /LXDATA/ LXDSET
      SAVE   /LXDATA/
C
C Help variable
C
      INTEGER CURC
C
C Find start of next item, if any, in line
C
      CURC = IPOS (2)
  100 CURC = CURC + 1
C
C End-of-line reached, no item returned
C
      IF (CURC .GT. LINLEN) THEN
        ERRCOD = 1
        RETURN
      END IF
C
C Act according to character type; if separator then just skip back
C
      GOTO (100, 200, 300, 400) LXDSET (ICHAR (LINE (CURC:CURC)))
C
C Ordinary token
C
      ITYP = 0
      IPOS (1) = CURC
      ERRCOD = 0
C
50    CURC = CURC + 1
C
C End-of-line reached
C
        IF (CURC .GT. LINLEN) THEN
          IPOS (2) = LINLEN
          RETURN
C
C Ordinary token finished
C
        ELSE IF (LXDSET (ICHAR (LINE (CURC:CURC))) .NE. LXTOK) THEN
          IPOS (2) = CURC - 1
          RETURN
        END IF
C
      GOTO 50
C
C Delimiter character; return it
C
  200 ITYP = 1
      IPOS (1) = CURC
      IPOS (2) = CURC
      ERRCOD = 0
      RETURN
C
C String item
C
  300 ITYP = 2
      IPOS (1) = CURC
C
350   CURC = CURC + 1
C
C End-of-line reached before string finished
C
        IF (CURC .GT. LINLEN) THEN
          IPOS (2) = LINLEN
          ERRCOD = 2
          RETURN
C
C String item finished
C
        ELSE IF (LXDSET (ICHAR (LINE (CURC:CURC))) .EQ. LXSTR) THEN
          IPOS (2) = CURC
          ERRCOD = 0
          RETURN
        END IF
C
      GOTO 350
C
C Comment start character; by definition nothing more on line
C
  400 ERRCOD = 1
      RETURN
C
      END
C Package LQPLAN
C
C Compute a least-square plane from a set of 3D coordinates.
C
C LQPLAN s  calculate least-square plane from a set of points
C LPDIST rf  return distance from point to given plane
C LPRMSD rf  compute root-mean-square deviation of points to given plane
C
C Uses no separately compiled procedures.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   12-May-1988  first attempts
C   13-May-1988  written
C   16-May-1988  debugged
C
C
C-----------------------------------------------------------
      SUBROUTINE LQPLAN (PLANE, CENTER, POINTS, NUM, ERRCOD)
C
      INTEGER NUM, ERRCOD
      REAL    PLANE (4), CENTER (3), POINTS (3, NUM)
C
C PLANE   (Out) plane parameters: xyz of normal, and dist to origin
C CENTER  (Out) center of gravity of points
C POINTS  (In)  coordinates of set of points
C NUM     (In)  total number of points
C ERRCOD  (Out) error code
C      0 = no error
C      1 = too few points
C      2 = ill conditioned; determinant of matrix close to zero
C
C MINDET   minimum absolute value of determinant
C
      REAL       MINDET
      PARAMETER (MINDET = 1.0E-6)
C
C Help variables
C
      INTEGER I, OFFSET
      INTEGER ORD (3)
      REAL    FACTOR
      REAL    XYZ (3), MTX (3, 3)
C
C Skip if too few points input
C
      IF (NUM .LT. 3) THEN
        ERRCOD = 1
        RETURN
      END IF
C
C No offset has been added to any direction
C
      OFFSET = 0
C
C Compute sum of coordinates
C
100   XYZ (1) = 0.0
      XYZ (2) = 0.0
      XYZ (3) = 0.0
      DO 200 I = 1, NUM
        XYZ (1) = XYZ (1) + POINTS (1, I)
        XYZ (2) = XYZ (2) + POINTS (2, I)
        XYZ (3) = XYZ (3) + POINTS (3, I)
200   CONTINUE
C
C Compute center of gravity, if not already done
C
      IF (OFFSET .EQ. 0) THEN
        CENTER (1) = XYZ (1) / REAL (NUM)
        CENTER (2) = XYZ (2) / REAL (NUM)
        CENTER (3) = XYZ (3) / REAL (NUM)
      END IF
C
C Compute matrix elements
C
      MTX (1, 1) = 0.0
      MTX (1, 2) = 0.0
      MTX (1, 3) = 0.0
      MTX (2, 2) = 0.0
      MTX (2, 3) = 0.0
      MTX (3, 3) = 0.0
      DO 300 I = 1, NUM
        MTX (1, 1) = MTX (1, 1) + POINTS (1, I) * POINTS (1, I)
        MTX (1, 2) = MTX (1, 2) + POINTS (1, I) * POINTS (2, I)
        MTX (1, 3) = MTX (1, 3) + POINTS (1, I) * POINTS (3, I)
        MTX (2, 2) = MTX (2, 2) + POINTS (2, I) * POINTS (2, I)
        MTX (2, 3) = MTX (2, 3) + POINTS (2, I) * POINTS (3, I)
        MTX (3, 3) = MTX (3, 3) + POINTS (3, I) * POINTS (3, I)
300   CONTINUE
      MTX (2, 1) = MTX (1, 2)
      MTX (3, 1) = MTX (1, 3)
      MTX (3, 2) = MTX (2, 3)
C
C Check if problem is ill conditioned
C
      IF (ABS (MTX (1,1) * MTX (2,2) * MTX (3,3) +
     $         MTX (1,2) * MTX (2,3) * MTX (3,1) +
     $         MTX (1,3) * MTX (2,1) * MTX (3,2) -
     $         MTX (1,1) * MTX (2,3) * MTX (3,2) -
     $         MTX (1,2) * MTX (2,1) * MTX (3,3) -
     $         MTX (1,3) * MTX (2,2) * MTX (3,1)) .LT. MINDET) THEN
C
C Try adding offset and recompute centre of gravity and matrix
C
        IF (OFFSET .LT. 3) THEN
C
C Remove old offset, if any
C
          IF (OFFSET .GT. 0) THEN
            DO 400 I = 1, NUM
              POINTS (OFFSET, I) = POINTS (OFFSET, I) - 1.0
400         CONTINUE
          END IF
C
C Add offset, try first in x direction, then y, then z
C
          OFFSET = OFFSET + 1
          DO 450 I = 1, NUM
            POINTS (OFFSET, I) = POINTS (OFFSET, I) + 1.0
450       CONTINUE
          GOTO 100
C
C If neither x, y nor z offset works, then really ill conditioned
C
        ELSE
          ERRCOD = 2
          RETURN
        END IF
      END IF
C
C Use Gauss elimination and row pivoting (or whatever it's called)
C
      IF (ABS (MTX (1,1)) .GT. ABS (MTX (2,1))) THEN
        ORD (1) = 1
      ELSE
        ORD (1) = 2
      END IF
      IF (ABS (MTX (3,1)) .GT. ABS (MTX (ORD (1), 1))) ORD (1) = 3
      DO 500 I = 1, 3
        IF (I .NE. ORD (1)) THEN
          FACTOR = MTX (I, 1) / MTX (ORD (1), 1)
          MTX (I, 2) = MTX (I, 2) - FACTOR * MTX (ORD (1), 2)
          MTX (I, 3) = MTX (I, 3) - FACTOR * MTX (ORD (1), 3)
          XYZ (I)    = XYZ (I)    - FACTOR * XYZ (ORD (1))
        END IF
500   CONTINUE
C
      IF (ORD (1) .EQ. 1) THEN
        IF (ABS (MTX (2,2)) .GT. ABS (MTX (3,2))) THEN
          ORD (2) = 2
          ORD (3) = 3
        ELSE
          ORD (2) = 3
          ORD (3) = 2
        END IF
      ELSE IF (ORD (1) .EQ. 2) THEN
        IF (ABS (MTX (1,2)) .GT. ABS (MTX (3,2))) THEN
          ORD (2) = 1
          ORD (3) = 3
        ELSE
          ORD (2) = 3
          ORD (3) = 1
        END IF
      ELSE
        IF (ABS (MTX (1,2)) .GT. ABS (MTX (2,2))) THEN
          ORD (2) = 1
          ORD (3) = 2
        ELSE
          ORD (2) = 2
          ORD (3) = 1
        END IF
      END IF
      FACTOR = MTX (ORD (3), 2) / MTX (ORD (2), 2)
      MTX (ORD (3), 3) = MTX (ORD (3), 3) - FACTOR * MTX (ORD (2), 3)
      XYZ (ORD (3))    = XYZ (ORD (3))    - FACTOR * XYZ (ORD (2))
C
C Solve for the plane coefficients; backwards substitution
C
      PLANE (3) =  XYZ (ORD (3)) / MTX (ORD (3), 3)
      PLANE (2) = (XYZ (ORD (2)) - MTX (ORD (2), 3) * PLANE (3)) /
     $             MTX (ORD (2), 2)
      PLANE (1) = (XYZ (ORD (1)) - MTX (ORD (1), 2) * PLANE (2) -
     $             MTX (ORD (1), 3) * PLANE (3)) / MTX (ORD (1), 1)
      PLANE (4) = 1.0 / SQRT (PLANE(1)**2 + PLANE(2)**2 + PLANE(3)**2)
      PLANE (1) = PLANE (4) * PLANE (1)
      PLANE (2) = PLANE (4) * PLANE (2)
      PLANE (3) = PLANE (4) * PLANE (3)
C
C If offset added, remove it from plane parameter and coordinates
C
      IF (OFFSET .NE. 0) THEN
        PLANE (4) = PLANE (4) - PLANE (OFFSET)
        DO 600 I = 1, NUM
          POINTS (OFFSET, I) = POINTS (OFFSET, I) - 1.0
600     CONTINUE
      END IF
C
      ERRCOD = 0
      RETURN
      END
C
C
C----------------------------------------
      REAL FUNCTION LPDIST (PLANE, POINT)
C
      REAL PLANE (4), POINT (3)
C
C PLANE  (In)  plane parameters
C POINT  (In)  coordinate of point
C
      LPDIST = PLANE (1) * POINT (1) +
     $         PLANE (2) * POINT (2) +
     $         PLANE (3) * POINT (3) - PLANE (4)
      RETURN
      END
C
C
C----------------------------------------------
      REAL FUNCTION LPRMSD (PLANE, POINTS, NUM)
C
      INTEGER NUM
      REAL    PLANE (4), POINTS (3, NUM)
C
C PLANE   (In)  plane parameters
C POINTS  (In)  coordinates of points
C NUM     (In)  number of points
C
      REAL    SUM
      INTEGER I
C
      SUM = 0.0
      DO 100 I = 1, NUM
        SUM = SUM + (PLANE (1) * POINTS (1, I) +
     $               PLANE (2) * POINTS (2, I) +
     $               PLANE (3) * POINTS (3, I) - PLANE (4)) **2
100   CONTINUE
C
      IF (NUM .GT. 1) SUM = SUM / REAL (NUM)
      LPRMSD = SQRT (SUM)
      RETURN
      END
C Package MOL
C
C Copyright (C) 1987 Per Kraulis
C
C Read, write and handle molecule coordinate data
C
C File formats available and their integer codes:
C  1 = PDB, Brookhaven Protein Data Bank
C  2 = MSA, extended MS input atom file format,
C           ABF contains MOLNUM, AOCC contains MSFLAG (input only)
C  3 = DG, Disgeo format (input only)
C  4 = RD, Diamond format, Frodo variant (input only)
C  5 = CDS, Diamond format, MRC variant (input only)
C  6 = WAH, Wayne Hendrickson (PROLSQ) format (input only, orthogonal)
C
C MOLIN  s   read molecule coordinate file
C MOLOUT s   write molecule coordinate file (PDB format only)
C MCFFMT if  file format integer code, inferred from file extension
C FNDRES if  finds pointer to named residue
C FNDATM if  finds pointer to named atom
C MOLCAP s   set up list of pointers to CA atoms
C
C Uses separately compiled procedures:
C   ORTHO  create matrix to orthogonalise coordinates
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  29-Apr-1987  main code for PDB format written
C   2-May-1987  data via arguments, not common block, MCFFMT written
C   4-May-1987  FND functions added, and PTATM changed
C   7-May-1987  MS extended input format added, minor changes
C  15-Jul-1987  changed MSA format according to HYDRA (new MS ?)
C  17-Sep-1987  changed order of AXYZ indices, other minor changes
C  31-May-1988  MOLOUT inserted, MOLCAP added
C   3-Oct-1988  DG format added
C   6-Dec-1990  minor changes
C  30-Jan-1991  added RD, CDS and WAH formats; not well tested
C
C
C--------------------------------------------------------------------
      SUBROUTINE MOLIN (MOLFIL, LOGUNI, FILFMT,
     $                  RMAX, AMAX, RTOT, ATOT, PTATM, PTRES,
     $                  RNAME, RTYPE, ANAME, AXYZ, ABF, AOCC, ERRCOD)
C
      CHARACTER*(*) MOLFIL
      INTEGER       LOGUNI, FILFMT, RMAX, AMAX, RTOT, ATOT, ERRCOD
      INTEGER       PTATM (2, RMAX), PTRES (AMAX)
      CHARACTER*6   RNAME (RMAX)
      CHARACTER*4   RTYPE (RMAX), ANAME (AMAX)
      REAL          AXYZ (3, AMAX), ABF (AMAX), AOCC (AMAX)
C
C MOLFIL  (In) name of molecule coordinate file
C LOGUNI  (In) logical unit number to use
C FILFMT  (In) file format integer code
C RMAX    (In) maximum number of residues to keep
C AMAX    (In) maximum number of atoms to keep
C RTOT    (InOut) last occupied position in residue arrays
C ATOT    (InOut) last occupied position in atom arrays
C PTATM   (Out) pointer from residue list to first and last atom
C PTRES   (Out) pointer from atom list to residue
C RNAME   (Out) residue name (=number)
C RTYPE   (Out) residue type
C ANAME   (Out) atom name
C AXYZ    (Out) atom coordinates (orthogonal, Angstrom)
C ABF     (Out) atom B-factor (isotropic)
C AOCC    (Out) atom occupancy
C ERRCOD  (Out) error codes
C     0 = no error
C     1 = unknown file format
C     2 = file not found
C     3 = file read error; wrong file format
C     4 = atom list full
C     5 = residue list full
C
      CHARACTER*80 INLINE
      CHARACTER*6  NEWRES, OLDRES
      INTEGER      I, J, MSFLAG, MOLNUM, RLAST, ALAST
      REAL         CELL (6), MTX (3, 3), XYZ (3)
C
C Go to reading procedure according to specified format
C
      GOTO (100, 200, 300, 400, 400, 600) FILFMT
      ERRCOD = 1
      RETURN
C
C PDB format
C
100   IF (MOLFIL .NE. ' ') THEN
        OPEN (UNIT=LOGUNI, FILE=MOLFIL, STATUS='OLD', FORM='FORMATTED',
     $        ERR=900)
      ELSE
        OPEN (UNIT=LOGUNI, STATUS='OLD', FORM='FORMATTED', ERR=900)
      END IF
C
C Start of read loop
C
      OLDRES = ' '
110   READ (LOGUNI, 120, END=800, ERR=910) INLINE
120   FORMAT (A)
C
      IF (INLINE (1:6) .EQ. 'ATOM  '  .OR.
     $    INLINE (1:6) .EQ. 'HETATM') THEN
C
C Check for atom list array overflow
C
        IF (ATOT .EQ. AMAX) THEN
          PTATM (2, RTOT) = ATOT
          GOTO 940
        END IF
C
        ATOT = ATOT + 1
        NEWRES = INLINE (22:27)
C
C Set last pointer for previously read residue, if any,
C and first pointer for new, and transfer name
C
        IF (NEWRES .NE. OLDRES) THEN
          IF (RTOT .GT. 0) PTATM (2, RTOT) = ATOT - 1
C
C Check for residue list array overflow
C
          IF (RTOT .EQ. RMAX) GOTO 950
C
          RTOT = RTOT + 1
          RNAME (RTOT) = NEWRES
          OLDRES = NEWRES
          RTYPE (RTOT) = INLINE (18:20)
          PTATM (1, RTOT) = ATOT
        END IF
C
C Transfer values for atom
C
        READ (INLINE,130,ERR=910) ANAME (ATOT), (AXYZ (I, ATOT), I=1,3),
     $                            AOCC (ATOT), ABF (ATOT)
130     FORMAT (T13,A4,T31,3F8.3,2F6.2)
C
C Set pointer from atom to residue
C
        PTRES (ATOT) = RTOT
      END IF
C
C Loop back for next line
C
      GOTO 110
C
C MS extended input format
C
200   IF (MOLFIL .NE. ' ') THEN
        OPEN (UNIT=LOGUNI, FILE=MOLFIL, STATUS='OLD', FORM='FORMATTED',
     $        ERR=900)
      ELSE
        OPEN (UNIT=LOGUNI, STATUS='OLD', FORM='FORMATTED', ERR=900)
      END IF
C
C Start of read loop
C
      OLDRES = ' '
210   READ (LOGUNI, 220, END=800, ERR=910) INLINE
220   FORMAT (A)
C
C Check for atom list array overflow
C
      IF (ATOT .EQ. AMAX) THEN
        PTATM (2, RTOT) = ATOT
        GOTO 940
      END IF
C
      ATOT = ATOT + 1
      NEWRES = ' ' // INLINE (55:59)
C
C Set last pointer for previously read residue, if any,
C and first pointer for new, and transfer name
C
      IF (NEWRES .NE. OLDRES) THEN
        IF (RTOT .GT. 0) PTATM (2, RTOT) = ATOT - 1
C
C Check for residue list array overflow
C
        IF (RTOT .EQ. RMAX) GOTO 950
C
        RTOT = RTOT + 1
        RNAME (RTOT) = NEWRES
        OLDRES = NEWRES
        RTYPE (RTOT) = INLINE (51:54)
        PTATM (1, RTOT) = ATOT
      END IF
C
C Transfer values for atom, convert integer to real
C
      READ (INLINE, 230, ERR=910) (AXYZ (I, ATOT), I=1,3),
     $                             J, MSFLAG, MOLNUM, ANAME (ATOT)
230   FORMAT (3F10.5,3I5,T61,A4)
      AOCC (ATOT) = REAL (MSFLAG)
      ABF (ATOT) = REAL (MOLNUM)
C
C Set pointer from atom to residue
C
      PTRES (ATOT) = RTOT
C
C Loop back for next line
C
      GOTO 210
C
C DG format
C
300   IF (MOLFIL .NE. ' ') THEN
        OPEN (UNIT=LOGUNI, FILE=MOLFIL, STATUS='OLD', FORM='FORMATTED',
     $        ERR=900)
      ELSE
        OPEN (UNIT=LOGUNI, STATUS='OLD', FORM='FORMATTED', ERR=900)
      END IF
C
C Skip past header lines
C
      READ (LOGUNI, 320, END=910, ERR=910)
      READ (LOGUNI, 320, END=910, ERR=910)
      READ (LOGUNI, 320, END=910, ERR=910)
C
C Start of read loop
C
      OLDRES = ' '
310   READ (LOGUNI, 320, END=800, ERR=910) INLINE
320   FORMAT (A)
C
C Check for atom list array overflow
C
      IF (ATOT .EQ. AMAX) THEN
        PTATM (2, RTOT) = ATOT
        GOTO 940
      END IF
C
      ATOT = ATOT + 1
      NEWRES = INLINE (14:17)
C
C Set last pointer for previously read residue, if any,
C and first pointer for new, and transfer name
C
      IF (NEWRES .NE. OLDRES) THEN
        IF (RTOT .GT. 0) PTATM (2, RTOT) = ATOT - 1
C
C Check for residue list array overflow
C
        IF (RTOT .EQ. RMAX) GOTO 950
C
        RTOT = RTOT + 1
        RNAME (RTOT) = NEWRES
        OLDRES = NEWRES
        RTYPE (RTOT) = INLINE (19:22)
        PTATM (1, RTOT) = ATOT
      END IF
C
C Transfer values for atom
C
      READ (INLINE, 330, ERR=910) ANAME (ATOT), (AXYZ (I,ATOT), I=1,3)
330   FORMAT (T7,A4,T24,3F11.4)
      ABF (ATOT) = 0.0
      AOCC (ATOT) = 0.0
C
C Set pointer from atom to residue
C
      PTRES (ATOT) = RTOT
C
C Loop back for next line
C
      GOTO 310
C
C RD format, Frodo or MRC variants
C
400   IF (MOLFIL .NE. ' ') THEN
        OPEN (UNIT=LOGUNI, FILE=MOLFIL, STATUS='OLD', FORM='FORMATTED',
     $        ERR=900)
      ELSE
        OPEN (UNIT=LOGUNI, STATUS='OLD', FORM='FORMATTED', ERR=900)
      END IF
C
C Read unit cell parameters for orthogonalization
C
      READ (LOGUNI, 450, ERR=910) CELL
450   FORMAT (6F10.4)
      CALL ORTHO (CELL, MTX, .TRUE.)
C
C Skip past header lines
C
      READ (LOGUNI, 420, ERR=910)
      READ (LOGUNI, 420, ERR=910)
C
C Start of read loop
C
      OLDRES = ' '
410   READ (LOGUNI, 420, END=800, ERR=910) INLINE
420   FORMAT (A)
C
C No data in record: probably last record
C
      IF (INLINE (1:10) .EQ. ' ') GOTO 410
C
C Check for atom list array overflow
C
      IF (ATOT .EQ. AMAX) THEN
        PTATM (2, RTOT) = ATOT
        GOTO 940
      END IF
C
      ATOT = ATOT + 1
      NEWRES = INLINE (69:72)
C
C Set last pointer for previously read residue, if any,
C and first pointer for new, and transfer name
C
      IF (NEWRES .NE. OLDRES) THEN
        IF (RTOT .GT. 0) PTATM (2, RTOT) = ATOT - 1
C
C Check for residue list array overflow
C
        IF (RTOT .EQ. RMAX) GOTO 950
C
        RTOT = RTOT + 1
        RNAME (RTOT) = NEWRES
        OLDRES = NEWRES
        RTYPE (RTOT) = INLINE (65:68)
        PTATM (1, RTOT) = ATOT
      END IF
C
C Transfer values for atom; different for Frodo or MRC variants
C
      IF (FILFMT .EQ. 4) THEN
        READ (INLINE,430,ERR=910) ANAME (ATOT), (XYZ (I), I=1,3),
     $                            ABF (ATOT), AOCC (ATOT)
430     FORMAT (T76,A4,T1,4F10.5,T56,F8.2)
      ELSE
        READ (INLINE,440,ERR=910) ANAME (ATOT), (XYZ (I), I=1,3),
     $                            ABF (ATOT), AOCC (ATOT)
440     FORMAT (T76,A4,T1,4F10.5,T56,F9.4)
      END IF
C
C Orthogonalize coordinates
C
      AXYZ(1,ATOT) = MTX(1,1)*XYZ(1) + MTX(1,2)*XYZ(2) + MTX(1,3)*XYZ(3)
      AXYZ(2,ATOT) = MTX(2,1)*XYZ(1) + MTX(2,2)*XYZ(2) + MTX(2,3)*XYZ(3)
      AXYZ(3,ATOT) = MTX(3,1)*XYZ(1) + MTX(3,2)*XYZ(2) + MTX(3,3)*XYZ(3)
C
C Set pointer from atom to residue
C
      PTRES (ATOT) = RTOT
C
C Loop back for next line
C
      GOTO 410
C
C Wayne Hendrickson (PROLSQ) format
C
600   IF (MOLFIL .NE. ' ') THEN
        OPEN (UNIT=LOGUNI, FILE=MOLFIL, STATUS='OLD', FORM='FORMATTED',
     $        ERR=900)
      ELSE
        OPEN (UNIT=LOGUNI, STATUS='OLD', FORM='FORMATTED', ERR=900)
      END IF
C
C Start of read loop
C
      OLDRES = ' '
610   READ (LOGUNI, 620, END=800, ERR=910) INLINE
620   FORMAT (A)
C
C Last record
C
      IF (INLINE (8:11) .EQ. ' 999') GOTO 610
C
C Check for atom list array overflow
C
      IF (ATOT .EQ. AMAX) THEN
        PTATM (2, RTOT) = ATOT
        GOTO 940
      END IF
C
      ATOT = ATOT + 1
      NEWRES = INLINE (8:11)
C
C Set last pointer for previously read residue, if any,
C and first pointer for new, and transfer name
C
      IF (NEWRES .NE. OLDRES) THEN
        IF (RTOT .GT. 0) PTATM (2, RTOT) = ATOT - 1
C
C Check for residue list array overflow
C
        IF (RTOT .EQ. RMAX) GOTO 950
C
        RTOT = RTOT + 1
        RNAME (RTOT) = NEWRES
        OLDRES = NEWRES
        RTYPE (RTOT) = INLINE (4:6)
        PTATM (1, RTOT) = ATOT
      END IF
C
C Transfer values for atom
C
      READ (INLINE,630,ERR=910) ANAME (ATOT), (AXYZ (I, ATOT), I=1,3),
     $                          ABF (ATOT), AOCC (ATOT)
630   FORMAT (T12,A4,5F10.5)
C
C Set pointer from atom to residue
C
      PTRES (ATOT) = RTOT
C
C Loop back for next line
C
      GOTO 610
C
C Successful termination, set last pointer
C
800   CLOSE (LOGUNI)
      PTATM (2, RTOT) = ATOT
      ERRCOD = 0
      RETURN
C
C File not found
C
900   ERRCOD = 2
      RETURN
C
C File read error; wrong file format
C
910   CLOSE (LOGUNI)
      ERRCOD = 3
      RETURN
C
C Too many residue or atoms in file
C
940   CLOSE (LOGUNI)
      ERRCOD = 4
      RETURN
C
950   CLOSE (LOGUNI)
      ERRCOD = 5
      RETURN
      END
C
C
C---------------------------------------------------------------------
      SUBROUTINE MOLOUT (MOLFIL, LOGUNI, FILFMT,
     $                   RMAX, AMAX, RTOT, ATOT, PTATM, PTRES,
     $                   RNAME, RTYPE, ANAME, AXYZ, ABF, AOCC, ERRCOD)
C
      CHARACTER*(*) MOLFIL
      INTEGER       LOGUNI, FILFMT, RMAX, AMAX, RTOT, ATOT, ERRCOD
      INTEGER       PTATM (2, RMAX), PTRES (AMAX)
      CHARACTER*6   RNAME (RMAX)
      CHARACTER*4   RTYPE (RMAX), ANAME (AMAX)
      REAL          AXYZ (3, AMAX), ABF (AMAX), AOCC (AMAX)
C
C MOLFIL  (In) name of molecule coordinate file
C LOGUNI  (In) logical unit number to use
C FILFMT  (In) file format integer code
C RMAX    (In) maximum number of residues to keep
C AMAX    (In) maximum number of atoms to keep
C RTOT    (In) last occupied position in residue arrays
C ATOT    (In) last occupied position in atom arrays
C PTATM   (In) pointer from residue list to first and last atom
C PTRES   (In) pointer from atom list to residue
C RNAME   (In) residue name (=number)
C RTYPE   (In) residue type
C ANAME   (In) atom name
C AXYZ    (In) atom coordinates (orthogonal, Angstrom)
C ABF     (In) atom B-factor (isotropic)
C AOCC    (In) atom occupancy
C ERRCOD  (Out) error code
C     0 = no error
C     1 = unknown file format
C     2 = file open error; file exists already or illegal file name
C     3 = error writing file
C
      INTEGER I, J, K, COUNT
C
C Go to write procedure according to specified format
C
      GOTO (100, 200, 300, 400, 400, 600) FILFMT
      ERRCOD = 1
      RETURN
C
C PDB format file
C
100   IF (MOLFIL .NE. ' ') THEN
        OPEN (UNIT=LOGUNI, FILE=MOLFIL, STATUS='NEW', FORM='FORMATTED',
     $        CARRIAGECONTROL='LIST', ERR=900)
      ELSE
        OPEN (UNIT=LOGUNI, STATUS='NEW', FORM='FORMATTED',
     $        CARRIAGECONTROL='LIST', ERR=900)
      END IF
C
      COUNT = 0
      DO 120 I = 1, RTOT
        DO 120 J = PTATM (1, I), PTATM (2, I)
          COUNT = COUNT + 1
          WRITE (LOGUNI, 110, ERR=910)
     $      'ATOM  ', COUNT, ANAME (J), RTYPE (I) (1:3),
     $      RNAME (I), (AXYZ (K, J), K=1,3), AOCC (J), ABF (J)
110       FORMAT (A6,I5,TR1,A4,TR1,A3,TR1,A6,TR3,3F8.3,2F6.2)
120   CONTINUE
C
      WRITE (LOGUNI, '(A)', ERR=910) 'END'
      CLOSE (LOGUNI)
      ERRCOD = 0
      RETURN
C
C MSA, DG and RD (Frodo or MRC variants) formats; cannot output those
C
200   ERRCOD = 1
      RETURN
300   ERRCOD = 1
      RETURN
400   ERRCOD = 1
      RETURN
600   ERRCOD = 1
      RETURN
C
C File open error; file exists already, or illegal file name
C
900   ERRCOD = 2
      RETURN
910   ERRCOD = 3
      RETURN
      END
C
C
C-------------------------------------
      INTEGER FUNCTION MCFFMT (MOLFIL)
C
C Returned values:
C    0 = unknown format
C    1 = PDB, Brookhaven Protein Data Bank format
C    2 = MSA, extended MS atom input file format
C    3 = DG, Disgeo format
C    4 = RD, Diamond format, Frodo variant
C    5 = CDS, Diamond format, MRC variant
C    6 = WAH, Wayne Hendrickson (PROLSQ) formst
C
      CHARACTER*(*) MOLFIL
C
      INTEGER LENGTH
C
      LENGTH = LEN (MOLFIL)
C
      IF (LENGTH .LT. 4) THEN
        MCFFMT = 0
      ELSE IF (MOLFIL (LENGTH - 3 :) .EQ. '.pdb'  .OR.
     $         MOLFIL (LENGTH - 3 :) .EQ. '.PDB') THEN
        MCFFMT = 1
      ELSE IF (MOLFIL (LENGTH - 3 :) .EQ. '.msa'  .OR.
     $         MOLFIL (LENGTH - 3 :) .EQ. '.MSA') THEN
        MCFFMT = 2
      ELSE IF (MOLFIL (LENGTH - 2 :) .EQ. '.dg'  .OR.
     $         MOLFIL (LENGTH - 2 :) .EQ. '.DG') THEN
        MCFFMT = 3
      ELSE IF (MOLFIL (LENGTH - 2 :) .EQ. '.rd'  .OR.
     $         MOLFIL (LENGTH - 2 :) .EQ. '.RD') THEN
        MCFFMT = 4
      ELSE IF (MOLFIL (LENGTH - 3 :) .EQ. '.cds'  .OR.
     $         MOLFIL (LENGTH - 3 :) .EQ. '.CDS') THEN
        MCFFMT = 5
      ELSE IF (MOLFIL (LENGTH - 3 :) .EQ. '.wah'  .OR.
     $         MOLFIL (LENGTH - 3 :) .EQ. '.WAH') THEN
        MCFFMT = 6
      ELSE
        MCFFMT = 0
      END IF
C
      RETURN
      END
C
C
C--------------------------------------------------
      INTEGER FUNCTION FNDRES (RESNAM, RTOT, RNAME)
C
C Returns 0 if not found. Case and position sensitive.
C
      CHARACTER*(*) RESNAM
      INTEGER       RTOT
      CHARACTER*6   RNAME (RTOT)
C
C RESNAM  (In) residue name to find
C RTOT    (In) total number of residues in list
C RNAME   (In) list of residue names
C
      CHARACTER*6 RN
      INTEGER     I
C
      RN = RESNAM
      DO 100 I = 1, RTOT
        IF (RNAME (I) .EQ. RN) THEN
          FNDRES = I
          RETURN
        END IF
100   CONTINUE
C
      FNDRES = 0
      RETURN
      END
C
C
C---------------------------------------------------
      INTEGER FUNCTION FNDATM (ATMNAM, ANAME, PTATM)
C
C Returns 0 if not found. Case and position sensitive.
C
      CHARACTER*(*) ATMNAM
      CHARACTER*4   ANAME (*)
      INTEGER       PTATM (2)
C
C ATMNAM  (In) atom name to find
C ANAME   (In) atom name list
C PTATM   (In) pointers to first and last atom in residue
C
      CHARACTER*4 AN
      INTEGER     I
C
      AN = ATMNAM
      DO 100 I = PTATM (1), PTATM (2)
        IF (ANAME (I) .EQ. AN) THEN
          FNDATM = I
          RETURN
        END IF
100   CONTINUE
C
      FNDATM = 0
      RETURN
      END
C
C
C---------------------------------------------------------------------
      SUBROUTINE MOLCAP (CAPTR, PMAX, PTOT, RTOT, ANAM, PTATM, ERRCOD)
C
      INTEGER     PMAX, PTOT, RTOT, ERRCOD
      INTEGER     CAPTR (PMAX), PTATM (2, *)
      CHARACTER*4 ANAM (*)
C
C CAPTR   (Out) CA pointer list
C PMAX    (In)  max number of CA pointers
C PTOT    (Out) total number of CA pointers set up
C RTOT    (In)  number of residues
C ANAM    (In)  atom names list; test for CA is case but not pos sens
C PTATM   (In)  pointers from residue to first and last atoms
C ERRCOD  (Out) error code
C     0 = no error
C     1 = too many CA pointers found
C
      INTEGER I, J
C
      PTOT = 0
      DO 100 I = 1, RTOT
        DO 100 J = PTATM (1, I), PTATM (2, I)
          IF (ANAM (J) .EQ. 'CA  ' .OR.
     $        ANAM (J) .EQ. ' CA ' .OR.
     $        ANAM (J) .EQ. '  CA') THEN
            IF (PTOT .GE. PMAX) THEN
              ERRCOD = 1
              RETURN
            END IF
C
            PTOT = PTOT + 1
            CAPTR (PTOT) = J
          END IF
100   CONTINUE
C
      ERRCOD = 0
      RETURN
      END
C Return first unused logical unit number, skipping 5 and 6
C
C Uses no separately compiled procedures.
C
C (C) Copyright Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  13-Oct-1987  first attempts
C
C-------------------------------
      INTEGER FUNCTION NXTLUN ()
C
C MAXLUN  maximum logical unit number to test, return 0 if unsuccessful
C
      INTEGER    MAXLUN
      PARAMETER (MAXLUN = 100)
C
      INTEGER I
      LOGICAL USED
C
      DO 100 I = 1, 4
        INQUIRE (UNIT=I, OPENED=USED)
        IF (.NOT. USED) THEN
          NXTLUN = I
          RETURN
        END IF
100   CONTINUE
C
      DO 200 I = 7, MAXLUN
        INQUIRE (UNIT=I, OPENED=USED)
        IF (.NOT. USED) THEN
          NXTLUN = I
          RETURN
        END IF
200   CONTINUE
C
      NXTLUN = 0
      RETURN
      END
C Calculate transforming matrix between fractional unit cell
C and orthogonal real space coordinates. Direction of x-axis is kept.
C
C Uses no separately compiled procedures.
C
C Original code from T. Alwyn Jones.
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  28-Sep-1987  original code somewhat tidied up
C  31-Jan-1991  changed mode argument to logical
C
C
C----------------------------------------
      SUBROUTINE ORTHO (CELL, MTX, TOORT)
C
      REAL    CELL (1:6), MTX (1:3, 1:3)
      LOGICAL TOORT
C
C CELL  (In) unit cell parameters; Angstrom, degrees
C MTX   (Out) transforming matrix
C TOORT (In) from fraction to orthogonal coords, or other way around
C
      INTEGER I
      REAL    CABG (1:3), SABG (1:3), CABGS (1:3), ABCS (1:3)
      REAL    TORAD, VOL, SABGS1
      SAVE    TORAD
      DATA    TORAD /1.7453293E-2/
C
C Calculate cos/sine and star variables of alpha, beta, gamma
C
      DO 100 I = 1, 3
        CABG(I) = COS (TORAD * CELL(I+3))
        SABG(I) = SIN (TORAD * CELL(I+3))
100   CONTINUE
C
      CABGS(1) = (CABG(2) * CABG(3) - CABG(1)) / (SABG(2) * SABG(3))
      CABGS(2) = (CABG(3) * CABG(1) - CABG(2)) / (SABG(3) * SABG(1))
      CABGS(3) = (CABG(1) * CABG(2) - CABG(3)) / (SABG(1) * SABG(2))
      VOL = CELL(1) * CELL(2) * CELL(3) *
     $      SQRT (1.0 +2.0 * CABG(1) * CABG(2) * CABG(3) -
     $            CABG(1) ** 2 - CABG(2) ** 2 - CABG(3) ** 2)
      ABCS(1) = CELL(2) * CELL(3) * SABG(1) / VOL
      ABCS(2) = CELL(1) * CELL(3) * SABG(2) / VOL
      ABCS(3) = CELL(1) * CELL(2) * SABG(3) / VOL
      SABGS1 = SQRT (1.0 - CABGS (1) ** 2)
C
C Calculate transforming matrix
C
      IF (TOORT) THEN
        MTX(1,1) = CELL(1)
        MTX(1,2) = CABG(3) * CELL(2)
        MTX(1,3) = CABG(2) * CELL(3)
        MTX(2,1) = 0.0
        MTX(2,2) = SABG(3) * CELL(2)
        MTX(2,3) = - SABG(2) * CABGS(1) * CELL(3)
        MTX(3,1) = 0.0
        MTX(3,2) = 0.0
        MTX(3,3) = SABG(2) * SABGS1 * CELL(3) 
      ELSE
        MTX(1,1) = 1.0 / CELL(1)
        MTX(1,2) = - CABG(3) / (SABG(3) * CELL(1))
        MTX(1,3) = - (CABG(3) * SABG(2) * CABGS(1) + CABG(2) * SABG(3))
     $             / (SABG(2) * SABGS1 * SABG(3) * CELL(1))
        MTX(2,1) = 0.0
        MTX(2,2) = 1.0 / (SABG(3) * CELL(2))
        MTX(2,3) = CABGS(1) / (SABGS1 * SABG(3) * CELL(2))
        MTX(3,1) = 0.0
        MTX(3,2) = 0.0
        MTX(3,3) = 1.0 / (SABG(2) * SABGS1 * CELL(3))
      END IF
      RETURN
      END
C  Package PCCURV
C
C Parametric cubic 3D curve procedures
C
C Copyright (C) 1991 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   2-Jan-1991  written, Hermite tested
C
C  PCHERM  Hermite curve, given end-points and their tangent vectors
C  PCBEZI  Bezier curve, given end-points and two control points
C  PCBSPL  B-spline curve, given four control points
C
C
C---------------------------------------------------------
      SUBROUTINE PCHERM (POINT, T, COO1, COO2, VEC1, VEC2)
C
      REAL POINT (3), T, COO1 (3), COO2 (3), VEC1 (3), VEC2 (3)
C
C POINT  (Out) computed curve point
C T      (In)  parameter [0.0, 1.0]
C COO1   (In)  start point
C COO2   (In)  end point
C VEC1   (In)  start point tangent vector
C VEC2   (In)  end point tangent vector
C
C Help variables
C
      REAL    T2, T3
      INTEGER XYZ
C
      T2 = T ** 2
      T3 = T ** 3
C
      DO 100 XYZ = 1, 3
        POINT (XYZ) = COO1 (XYZ) * (2.0* T3 - 3.0* T2 + 1.0) +
     $              + COO2 (XYZ) * (- 2.0* T3 + 3.0* T2) +
     $              + VEC1 (XYZ) * (T3 - 2.0* T2 + T) +
     $              + VEC2 (XYZ) * (T3 - T2)
100   CONTINUE
C
      RETURN
      END
C
C
C---------------------------------------------------------
      SUBROUTINE PCBEZI (POINT, T, COO1, COO2, CTR1, CTR2)
C
      REAL POINT (3), T, COO1 (3), COO2 (3), CTR1 (3), CTR2 (3)
C
C POINT  (Out) computed curve point
C T      (In)  parameter [0.0, 1.0]
C COO1   (In)  start point
C COO2   (In)  end point
C CTR1   (In)  first control point
C CTR2   (In)  second control point
C
C Help variables
C
      REAL    T2, T3
      INTEGER XYZ
C
      T2 = T ** 2
      T3 = T ** 3
C
      DO 100 XYZ = 1, 3
        POINT (XYZ) = COO1 (XYZ) * (- T3 + 3.0* T2 - 3.0* T + 1.0)
     $              + COO2 (XYZ) * T3
     $              + CTR1 (XYZ) * (3.0* T3 - 6.0* T2 + 3.0* T)
     $              + CTR2 (XYZ) * (- 3.0* T3 + 3.0* T2)
100   CONTINUE
C
      RETURN
      END
C
C
C---------------------------------------------------------
      SUBROUTINE PCBSPL (POINT, T, COO1, COO2, PREV, NEXT)
C
      REAL POINT (3), T, COO1 (3), COO2 (3), PREV(3), NEXT (3)
C
C POINT  (Out) computed curve point
C T      (In)  parameter [0.0, 1.0]
C COO1   (In)  start control point
C COO2   (In)  end control point
C PREV   (In)  previous control point
C NEXT   (In)  next control point
C
C Help variables
C
      REAL    T2, T3
      INTEGER XYZ
C
      T2 = T ** 2
      T3 = T ** 3
C
      DO 100 XYZ = 1, 3
        POINT (XYZ) = (COO1 (XYZ) * (3.0* T3 - 6.0* T2 + 4.0)
     $              +  COO2 (XYZ) * (- 3.0* T3 + 3.0* T2 + 3.0* T + 1.0)
     $              +  PREV (XYZ) * (- T3 + 3.0* T2 - 3.0* T + 1.0)
     $              +  NEXT (XYZ) * T3) / 6.0
100   CONTINUE
C
      RETURN
      END
C Random number generators
C
C RANDOM df  random number in interval 0.0-1.0
C GAUSS  df  random number from gaussian, mean 0.0, standard deviation 1.0
C
C Copyright (C) 1989 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  10-apr-1989  first attempts
C
C--------------------------------------------
      DOUBLE PRECISION FUNCTION RANDOM (SEED)
C
      DOUBLE PRECISION SEED
C
C SEED   (InOut) random number generator seed
C
      DOUBLE PRECISION MODDIV, NORM, STORE
      PARAMETER       (MODDIV = 2147483648.0D0,
     $                 NORM   = 2147483711.0D0)
C
      DATA STORE /0.31D0/
      SAVE STORE
C
      IF (SEED .LT. 0.0D0) SEED = 314564.0D0
      SEED = SEED + STORE
      SEED = MOD (168069.0D0 * SEED, MODDIV)
      STORE = SEED / NORM
      RANDOM = STORE
      RETURN
      END
C
C---------------------------------------------------
      DOUBLE PRECISION FUNCTION GAUSS (APPROX, SEED)
C
      INTEGER          APPROX
      DOUBLE PRECISION SEED
C
C APPROX  (In)    number of 0.0-1.0 random numbers to use in approximation
C SEED    (InOut) random number seed
C
C Default number of random numbers to use in approximation
C
      INTEGER    DEFLT
      PARAMETER (DEFLT = 6)
C
C Externally defined function
C
      DOUBLE PRECISION RANDOM
C
C Help variables
C
      INTEGER I, TOP
      DOUBLE PRECISION SUM
C
      IF (APPROX .LE. 1) THEN
        TOP = DEFLT
      ELSE
        TOP = APPROX
      END IF
C
      SUM = 0.0D0
      DO 100 I = 1, TOP
        SUM = SUM + RANDOM (SEED)
100   CONTINUE
C
      GAUSS = SUM - DBLE (TOP) / 2.0D0
      RETURN
      END
C Read DSSP file secondary structure assignments.
C
C Residue names input must be right-aligned.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  11-Jan-1988  first attempts
C
C Uses separately compiled packages:
C
C STRBLK  find/remove blanks
C
C
C-----------------------------------------------------------------------
      SUBROUTINE REDDSS (DSSFIL, LOGUNI, RESNAM, TOTRES, RESCOD, ERRCOD)
C
      IMPLICIT NONE
C
      CHARACTER*(*) DSSFIL
      INTEGER       LOGUNI, TOTRES, ERRCOD
      CHARACTER*6   RESNAM (1:*)
      CHARACTER*1   RESCOD (1:*)
C
C DSSFIL  (In)  DSSP file
C LOGUNI  (In)  logical unit number to use
C RESNAM  (In)  residue name list, must be right-aligned
C RESTOT  (In)  total number of residues in list
C RESCOD  (Out) residue secondary structure assigmnent
C ERRCOD  (Out) error code
C     0 = no error
C     1 = file not found
C     2 = file read error
C     3 = residue name in DSSP file not present in input residue list
C
      INTEGER       RES, FIRST
      CHARACTER*6   NAME
      CHARACTER*132 LINE
C
      FIRST = 1
C
C Open and read past initial lines
C
      OPEN (UNIT=LOGUNI, FILE=DSSFIL, STATUS='OLD', FORM='FORMATTED',
     $      ERR=900)
100   READ (UNIT=LOGUNI, FMT=110, ERR=910) LINE
110   FORMAT (A)
      IF (LINE (3:3) .EQ. '#') GOTO 200
      GOTO 100
C
C Read residue line, reformat residue name, find it among input residues
C
200   READ (UNIT=LOGUNI, FMT=110, END=800, ERR=910) LINE
      NAME = LINE (12:12) // LINE (7:11)
      CALL SRIJYR (NAME)
      IF (NAME .EQ. '     !') GOTO 200
C
      DO 300 RES = FIRST, TOTRES
        IF (RESNAM (RES) .EQ. NAME) THEN
          RESCOD (RES) = LINE (17:17)
          FIRST = RES + 1
          GOTO 200
        END IF
300   CONTINUE
      ERRCOD = 3
      RETURN
C
800   CLOSE (UNIT=LOGUNI)
      ERRCOD = 0
      RETURN
C
900   ERRCOD = 1
      RETURN
910   ERRCOD = 2
      RETURN
C
      END
C Package SALIAS
C
C Store and handle string variables and their values.
C
C SALDEF  s   define variable and assign value
C SALMOD  s   modify variable value
C SALDEL  s   delete variable from list
C SALGET  s   get variable value
C SALNUM  if  return number of defined variables
C SALNAM  cf  return variable name for internal number
C SALID1  o   numerical data structure
C SALID2  o   character data structure
C SALINI  b   initialize data structure
C
C The file salias.inc, which contains the data structure used by the
C procedures, must be included (using VAX FORTRAN INCLUDE, editor or
C utility program) in all procedures before compiling.
C
C Uses separately compiled procedure:
C
C BFINDS if  find word slot in ordered list, or slot to insert at
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   4-Jan-1988  written
C  31-May-1988  added new BFINDS
C
C
C-----------------------------------------
      SUBROUTINE SALDEF (VAR, VAL, ERRCOD)
C
C Package SALIAS
C
C Store and handle string variables and values
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program)
C in all procedures before compiling.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   4-Jan-1988  first attempts
C
C MAXVAR  max number of variables in list
C
      INTEGER MAXVAR
      PARAMETER (MAXVAR = 64)
C
C SALTOT  total number of defined variables
C SALVAR  variable names, ordered list
C SALVAL  variable values
C
      INTEGER      SALTOT
      CHARACTER*20 SALVAR (MAXVAR)
      CHARACTER*80 SALVAL (MAXVAR)
C
      COMMON /SALID1/ SALTOT
      COMMON /SALID2/ SALVAR, SALVAL
      SAVE   /SALID1/, /SALID2/
C
      CHARACTER*(*) VAR, VAL
      INTEGER       ERRCOD
C
C VAR     (In)  variable name
C VAL     (In)  variable value
C ERRCOD  (Out) error code
C      0 = no error
C      1 = variable list overflow; new variable not inserted into list
C      2 = variable already exists; value not changed
C
C To ensure that we initialize
C
      EXTERNAL SALINI
C
C Externally defined function
C
      INTEGER BFINDS
C
C Help variables
C
      INTEGER I, SLOT
C
C Check if any space in arrays
C
      IF (SALTOT .GE. MAXVAR) THEN
        ERRCOD = 1
        RETURN
      END IF
C
C Find slot to insert into; return with failure if already there
C
      SLOT = - BFINDS (VAR, SALVAR, SALTOT)
      IF (SLOT .LE. 0) THEN
        ERRCOD = 2
        RETURN
      END IF
C
C Shift up all variables after this slot
C
      SALTOT = SALTOT + 1
      DO 100 I = SALTOT, SLOT + 1, -1
        SALVAR (I) = SALVAR (I - 1)
        SALVAL (I) = SALVAL (I - 1)
100   CONTINUE
C
C Put in new variable and value
C
      SALVAR (SLOT) = VAR
      SALVAL (SLOT) = VAL
C
      ERRCOD = 0
      RETURN
      END
C
C
C-----------------------------------------
      SUBROUTINE SALMOD (VAR, VAL, ERRCOD)
C
C Package SALIAS
C
C Store and handle string variables and values
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program)
C in all procedures before compiling.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   4-Jan-1988  first attempts
C
C MAXVAR  max number of variables in list
C
      INTEGER MAXVAR
      PARAMETER (MAXVAR = 64)
C
C SALTOT  total number of defined variables
C SALVAR  variable names, ordered list
C SALVAL  variable values
C
      INTEGER      SALTOT
      CHARACTER*20 SALVAR (MAXVAR)
      CHARACTER*80 SALVAL (MAXVAR)
C
      COMMON /SALID1/ SALTOT
      COMMON /SALID2/ SALVAR, SALVAL
      SAVE   /SALID1/, /SALID2/
C
      CHARACTER*(*) VAR, VAL
      INTEGER       ERRCOD
C
C VAR     (In)  variable name
C VAL     (In)  variable value
C ERRCOD  (Out) error code
C      0 = no error
C      1 = variable does not exist
C
C To ensure that we initialize
C
      EXTERNAL SALINI
C
C Externally defined function
C
      INTEGER BFINDS
C
C Help variable
C
      INTEGER SLOT
C
      SLOT = BFINDS (VAR, SALVAR, SALTOT)
      IF (SLOT .LE. 0) THEN
        ERRCOD = 1
      ELSE
        SALVAL (SLOT) = VAL
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C------------------------------------
      SUBROUTINE SALDEL (VAR, ERRCOD)
C
C Package SALIAS
C
C Store and handle string variables and values
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program)
C in all procedures before compiling.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   4-Jan-1988  first attempts
C
C MAXVAR  max number of variables in list
C
      INTEGER MAXVAR
      PARAMETER (MAXVAR = 64)
C
C SALTOT  total number of defined variables
C SALVAR  variable names, ordered list
C SALVAL  variable values
C
      INTEGER      SALTOT
      CHARACTER*20 SALVAR (MAXVAR)
      CHARACTER*80 SALVAL (MAXVAR)
C
      COMMON /SALID1/ SALTOT
      COMMON /SALID2/ SALVAR, SALVAL
      SAVE   /SALID1/, /SALID2/
C
      CHARACTER*(*) VAR
      INTEGER       ERRCOD
C
C VAR     (In)  variable name
C ERRCOD  (Out) error code
C      0 = no error
C      1 = no such variable in list
C
C To ensure that we initialize
C
      EXTERNAL SALINI
C
C Externally defined functions
C
      INTEGER BFINDS
C
C Help variables
C
      INTEGER I, SLOT
C
C Get slot number; return with failure if none
C
      SLOT = BFINDS (VAR, SALVAR, SALTOT)
      IF (SLOT .LE. 0) THEN
        ERRCOD = 1
        RETURN
      END IF
C
C Shift down variables and values, overwriting variable to delete
C
      SALTOT = SALTOT - 1
      DO 100 I = SLOT, SALTOT
        SALVAR (I) = SALVAR (I + 1)
        SALVAL (I) = SALVAL (I + 1)
100   CONTINUE
C
      ERRCOD = 0
      RETURN
      END
C
C
C-----------------------------------------
      SUBROUTINE SALGET (VAL, VAR, ERRCOD)
C
C Package SALIAS
C
C Store and handle string variables and values
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program)
C in all procedures before compiling.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   4-Jan-1988  first attempts
C
C MAXVAR  max number of variables in list
C
      INTEGER MAXVAR
      PARAMETER (MAXVAR = 64)
C
C SALTOT  total number of defined variables
C SALVAR  variable names, ordered list
C SALVAL  variable values
C
      INTEGER      SALTOT
      CHARACTER*20 SALVAR (MAXVAR)
      CHARACTER*80 SALVAL (MAXVAR)
C
      COMMON /SALID1/ SALTOT
      COMMON /SALID2/ SALVAR, SALVAL
      SAVE   /SALID1/, /SALID2/
C
      CHARACTER*(*) VAL, VAR
      INTEGER       ERRCOD
C
C VAL     (Out) variable value
C VAR     (In)  variable name
C ERRCOD  (Out) error code
C      0 = no error
C      1 = no such variable in list
C
C To ensure that we initialize
C
      EXTERNAL SALINI
C
C Externally defined function
C
      INTEGER BFINDS
C
C Help variable
C
      INTEGER SLOT
C
      SLOT = BFINDS (VAR, SALVAR, SALTOT)
      IF (SLOT .LE. 0) THEN
        ERRCOD = 1
      ELSE
        VAL = SALVAL (SLOT)
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C-------------------------------
      INTEGER FUNCTION SALNUM ()
C
C Package SALIAS
C
C Store and handle string variables and values
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program)
C in all procedures before compiling.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   4-Jan-1988  first attempts
C
C MAXVAR  max number of variables in list
C
      INTEGER MAXVAR
      PARAMETER (MAXVAR = 64)
C
C SALTOT  total number of defined variables
C SALVAR  variable names, ordered list
C SALVAL  variable values
C
      INTEGER      SALTOT
      CHARACTER*20 SALVAR (MAXVAR)
      CHARACTER*80 SALVAL (MAXVAR)
C
      COMMON /SALID1/ SALTOT
      COMMON /SALID2/ SALVAR, SALVAL
      SAVE   /SALID1/, /SALID2/
C
C To ensure that we initialize
C
      EXTERNAL SALINI
C
      SALNUM = SALTOT
      RETURN
      END
C
C
C---------------------------------------
      CHARACTER*20 FUNCTION SALNAM (NUM)
C
C Package SALIAS
C
C Store and handle string variables and values
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program)
C in all procedures before compiling.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   4-Jan-1988  first attempts
C
C MAXVAR  max number of variables in list
C
      INTEGER MAXVAR
      PARAMETER (MAXVAR = 64)
C
C SALTOT  total number of defined variables
C SALVAR  variable names, ordered list
C SALVAL  variable values
C
      INTEGER      SALTOT
      CHARACTER*20 SALVAR (MAXVAR)
      CHARACTER*80 SALVAL (MAXVAR)
C
      COMMON /SALID1/ SALTOT
      COMMON /SALID2/ SALVAR, SALVAL
      SAVE   /SALID1/, /SALID2/
C
      INTEGER NUM
C
C NUM   (In)  internal number for variable
C
C To ensure that we initialize
C
      EXTERNAL SALINI
C
      IF (NUM .LT. 1 .OR. NUM .GT. SALTOT) THEN
        SALNAM = ' '
      ELSE
        SALNAM = SALVAR (NUM)
      END IF
      RETURN
      END
C
C
C----------------------
      BLOCK DATA SALINI
C
C Package SALIAS
C
C Store and handle string variables and values
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program)
C in all procedures before compiling.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   4-Jan-1988  first attempts
C
C MAXVAR  max number of variables in list
C
      INTEGER MAXVAR
      PARAMETER (MAXVAR = 64)
C
C SALTOT  total number of defined variables
C SALVAR  variable names, ordered list
C SALVAL  variable values
C
      INTEGER      SALTOT
      CHARACTER*20 SALVAR (MAXVAR)
      CHARACTER*80 SALVAL (MAXVAR)
C
      COMMON /SALID1/ SALTOT
      COMMON /SALID2/ SALVAR, SALVAL
      SAVE   /SALID1/, /SALID2/
C
      DATA SALTOT /0/
C
      END
C Compare strings for equivalence, one of which may contain wildcards.
C
C Wildcard characters:
C '*' indicates any string of any length.
C '#' indicates any single character.
C
C Note: the strings 'test' and 'test ' are considered as unequal by
C the algorithm used in this procedure. Trimming of blanks should be
C done before sending the strings to this procedure.
C
C Uses no separately compiled procedures.
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  13-Sep-1987  first attempts
C  16-Sep-1987  written and tested
C
C-----------------------------------
      LOGICAL FUNCTION SCMPWD (S, W)
C
      CHARACTER*(*) S, W
C
C S  (In) character string to compare
C W  (In) character string comparand; may contain wildcards '*' and '#'
C
C Comparison wildcard characters
C
      CHARACTER*1 WLDSTR, WLDCHR
      PARAMETER  (WLDSTR = '*', WLDCHR = '#')
C
C Help variables
C
      INTEGER I, SLEN, SCURR, SCMP, WLEN, WCURR, WCMP
      LOGICAL WLDFLG
C
C Initialize
C
      SLEN = LEN (S)
      WLEN = LEN (W)
      SCURR = 1
      WCURR = 1
      WLDFLG = .FALSE.
C
C If comparand is any-string, set up for comparison beyond that char
C
100   IF (W (WCURR:WCURR) .EQ. WLDSTR) THEN
        WLDFLG = .TRUE.
        WCURR = WCURR + 1
C
C If comparand is any-character, then skip one step if anything left
C
      ELSE IF (W (WCURR:WCURR) .EQ. WLDCHR) THEN
        IF (SCURR. GT. SLEN) THEN
          SCMPWD = .FALSE.
          RETURN
        END IF
        SCURR = SCURR + 1
        WCURR = WCURR + 1
C
C If comparand is a non-wildcard, just test for equality
C
      ELSE
        DO 110 I = WCURR, WLEN
          IF ((W(I:I).EQ.WLDSTR) .OR. (W(I:I).EQ.WLDCHR)) THEN
            WCMP = I - 1
            GOTO 120
          END IF
110     CONTINUE
        WCMP = WLEN
C
C Any-string in comparand, walk to non-wildcard in comparand
C
120     IF (WLDFLG) THEN
          I = INDEX (S (SCURR:SLEN), W (WCURR:WCMP))
          IF (I .EQ. 0) THEN
            SCMPWD = .FALSE.
            RETURN
          END IF
          SCURR = SCURR + I + WCMP - WCURR
          WLDFLG = .FALSE.
C
        ELSE
          SCMP = SCURR + WCMP - WCURR
          IF (SCMP .GT. SLEN) THEN
            SCMPWD = .FALSE.
            RETURN
          ELSE IF (S (SCURR:SCMP) .NE. W (WCURR:WCMP)) THEN
            SCMPWD = .FALSE.
            RETURN
          END IF
          SCURR = SCMP + 1
        END IF
        WCURR = WCMP + 1
      END IF
C
C If any-string comparand, and nothing left to compare, then it fits
C
      IF (WCURR .GT. WLEN) THEN
        IF (WLDFLG) THEN
          SCMPWD = .TRUE.
        ELSE IF (SCURR .LE. SLEN) THEN
          SCMPWD = S (SCURR:SLEN) .EQ. ' '
        ELSE
          SCMPWD = .TRUE.
        END IF
        RETURN
C
C Loop back if anything left to compare
C
      ELSE
        GOTO 100
      END IF
C
      END
C Expand string abbreviation if unique in string list.
C
C Note: leading and trailing blanks in input string are significant.
C
C Uses no separately compiled procedures.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  16-Sep-1987  first attempts
C  29-Oct-1987  lexically ordered input list; binary search
C  24-Mar-1988  unordered list option removed
C   9-Jun-1988  renamed
C  11-Jul-1990  added test for string length in list
C
C-----------------------------------------------------------
      SUBROUTINE SEXPND (EXPAND, STR, SLIST, LSTLEN, ERRCOD)
C
      CHARACTER*(*) EXPAND, STR
      CHARACTER*(*) SLIST (*)
      INTEGER       LSTLEN, ERRCOD
C
C EXPAND  (Out) expanded string
C STR     (In)  string to expand
C SLIST   (In)  list of string to expand into
C LSTLEN  (In)  number of strings in list; array size
C ERRCOD  (Out) error code
C     0 = no error
C     1 = no corresponding string found in list
C     2 = ambiguous; more than one corresponding string found in list
C     3 = input string ord longer than expanded; expansion not possible
C
      INTEGER STRLEN, LO, HI, MIDDLE
C
C Check that expansion is possible and if anything in list
C
      IF (LSTLEN .LT. 1) THEN
        ERRCOD = 1
        RETURN
      END IF
C
      STRLEN = LEN (STR)
      IF (STRLEN .GT. LEN (EXPAND)  .OR.
     $    STRLEN .GT. LEN (SLIST (1))) THEN
        ERRCOD = 3
        RETURN
      END IF
C
C Binary search; compare until middle of interval matches, or 0 interval
C
      LO = 1
      HI = LSTLEN
C
C Simulated repeat-until loop
C
100   MIDDLE = (LO + HI) / 2
C
C A matching string has been found
C
        IF (STR .EQ. SLIST (MIDDLE) (1:STRLEN)) THEN
C
C Check if ambiguous; if so, such strings must be adjacent in list
C
          IF (MIDDLE .GT. 1) THEN
            IF (STR .EQ. SLIST (MIDDLE - 1) (:STRLEN)) THEN
              ERRCOD = 2
              RETURN
            END IF
          END IF
          IF (MIDDLE .LT. LSTLEN) THEN
            IF (STR .EQ. SLIST (MIDDLE + 1) (:STRLEN)) THEN
              ERRCOD = 2
              RETURN
            END IF
          END IF
C
C Non-ambiguous match; expand string and return
C
          EXPAND = SLIST (MIDDLE)
          ERRCOD = 0
          RETURN
C
C No match; set new interval limits, upwards or downwards in list
C
        ELSE IF (LGT (STR, SLIST (MIDDLE) (:STRLEN))) THEN
          LO = MIDDLE + 1
        ELSE
          HI = MIDDLE - 1
        END IF
C
C Return with failure if interval has shrunk to nothing
C
      IF (LO .GT. HI) THEN
        ERRCOD = 1
        RETURN
      END IF
C
C Loop back
C
      GOTO 100
C
      END
C Package SMACRO
C
C Store and handle string macros
C
C SMBEGM s   begin macro definition; end last one; test if macro name unique
C SMADDL s   add one line to latest macro defined
C SMTOTM if  return total number of defined macros
C SMDELE s   delete macro definition
C SMNAME s   return macro name for number
C SMSLOT if  return number of macro
C SMACTI s   set macro number to top active
C SMFIND s   find macro, set to top active; push macro pointer
C SMGETL s   get line from top active macro; pop pointer if last
C SMNACT if  return number of active macros; depth of macro pointer stack
C SMCLER s   skip top or all active macros; clear macro pointer(s)
C SMDAT1 o   numerical data structure
C SMDAT2 o   character data structure
C SMINIT b   initialize data structure
C
C The file SMACRO.INC, which contains the data structure used by the
C procedures, must be included (using VAX FORTRAN INCLUDE, editor or
C utility program) in all procedures before compiling.
C
C Uses separately compiled procedure:
C
C BFINDS if  find word slot in ordered list, or slot to insert at
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C  27-Nov-1987  modify to allow recursive calls
C  13-Jan-1988  ordered internal macro list, more efficient search
C  18-Mar-1988  fixed bug that did not activate a one-line macro
C  31-May-1988  entered new BFINDS
C  11-Apr-1990  added SMSLOT and SMACTI; minor changes
C
C
C--------------------------------------
      SUBROUTINE SMBEGM (MACRO, ERRCOD)
C
C Package SMACRO
C
C Store and handle string macros
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program) into all
C the procedures in SMACRO.FOR before compiling.
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C  27-Nov-1987  modify to allow recursive calls
C
C MAXMAC  max number of macros
C MAXLIN  max number of lines for all macros
C MAXDPT  max depth of recursive macro calls
C
      INTEGER MAXMAC, MAXLIN, MAXDPT
      PARAMETER (MAXMAC = 40, MAXLIN = 200, MAXDPT = 32)
C
C MACCUR  pointer to top of pointer stack
C MACSTK  stack of pointers to next and last line of macro(s) called
C MACTOT  total number of defined macros
C MACPOS  pointers from macro to first and last lines
C MACBEG  pointer to macro currently being entered
C MACNAM  macro names
C MACLIN  macro lines
C
      INTEGER      MACCUR, MACTOT, MACBEG
      INTEGER      MACSTK (2, MAXDPT), MACPOS (2, MAXMAC)
      CHARACTER*20 MACNAM (MAXMAC)
      CHARACTER*80 MACLIN (MAXLIN)
C
      COMMON /SMDAT1/ MACCUR, MACTOT, MACBEG, MACSTK, MACPOS
      COMMON /SMDAT2/ MACNAM, MACLIN
      SAVE   /SMDAT1/, /SMDAT2/
C
      CHARACTER*(*) MACRO
      INTEGER       ERRCOD
C
C MACRO   (In)  macro name to define
C ERRCOD  (Out) error code
C      0 = no error
C      1 = macro name already defined
C      2 = no space for macro name; array overflow
C      3 = no space for macro line; array overflow
C
C To ensure that the data structure is initialized
C
      EXTERNAL SMINIT
C
C Externally defined function
C
      INTEGER BFINDS
C
C Help variables
C
      INTEGER I, SLOT
C
      SLOT = - BFINDS (MACRO, MACNAM, MACTOT)
      IF (SLOT .LE. 0) THEN
        ERRCOD = 1
        RETURN
C
      ELSE IF (MACTOT .GE. MAXMAC) THEN
        ERRCOD = 2
        RETURN
      END IF
C
C Shift up macros and pointers in lists
C
      MACTOT = MACTOT + 1
      DO 100 I = MACTOT, SLOT + 1, -1
        MACNAM (I) = MACNAM (I - 1)
        MACPOS (1, I) = MACPOS (1, I - 1)
        MACPOS (2, I) = MACPOS (2, I - 1)
100   CONTINUE
C
C Put in macro name and initialize pointers for this macro
C
      MACBEG = SLOT
      MACNAM (MACBEG) = MACRO
C
      IF (MACBEG .EQ. 1) THEN
        MACPOS (1, MACBEG) = 1
        MACPOS (2, MACBEG) = 0
        ERRCOD = 0
C
      ELSE IF (MACPOS (2, MACBEG - 1) .LT. MAXLIN) THEN
        MACPOS (1, MACBEG) = MACPOS (2, MACBEG - 1) + 1
        MACPOS (2, MACBEG) = MACPOS (2, MACBEG - 1)
        ERRCOD = 0
C
      ELSE
        ERRCOD = 3
      END IF
C
      RETURN
      END
C
C
C---------------------------------------
      SUBROUTINE SMADDL (SMLINE, ERRCOD)
C
C Package SMACRO
C
C Store and handle string macros
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program) into all
C the procedures in SMACRO.FOR before compiling.
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C  27-Nov-1987  modify to allow recursive calls
C
C MAXMAC  max number of macros
C MAXLIN  max number of lines for all macros
C MAXDPT  max depth of recursive macro calls
C
      INTEGER MAXMAC, MAXLIN, MAXDPT
      PARAMETER (MAXMAC = 40, MAXLIN = 200, MAXDPT = 32)
C
C MACCUR  pointer to top of pointer stack
C MACSTK  stack of pointers to next and last line of macro(s) called
C MACTOT  total number of defined macros
C MACPOS  pointers from macro to first and last lines
C MACBEG  pointer to macro currently being entered
C MACNAM  macro names
C MACLIN  macro lines
C
      INTEGER      MACCUR, MACTOT, MACBEG
      INTEGER      MACSTK (2, MAXDPT), MACPOS (2, MAXMAC)
      CHARACTER*20 MACNAM (MAXMAC)
      CHARACTER*80 MACLIN (MAXLIN)
C
      COMMON /SMDAT1/ MACCUR, MACTOT, MACBEG, MACSTK, MACPOS
      COMMON /SMDAT2/ MACNAM, MACLIN
      SAVE   /SMDAT1/, /SMDAT2/
C
      CHARACTER*(*) SMLINE
      INTEGER       ERRCOD
C
C SMLINE  (In)  line to add to macro definition
C ERRCOD  (Out) error code
C      0 = no error
C      1 = no macro definition has been started
C      2 = no space for macro line; array overflow
C
C Help variable
C
      INTEGER I
C
      IF (MACBEG .EQ. 0) THEN
        ERRCOD = 1
        RETURN
C
      ELSE IF (MACPOS (2, MACTOT) .GE. MAXLIN) THEN
        ERRCOD = 2
        RETURN
      END IF
C
C Shift up all affected line pointers one step
C
      IF (MACBEG .LT. MACTOT) THEN
        DO 100 I = MACPOS (2, MACTOT), MACPOS (1, MACBEG + 1), -1
          MACLIN (I + 1) = MACLIN (I)
100     CONTINUE
      END IF
C
      DO 200 I = MACBEG + 1, MACTOT
        MACPOS (1, I) = MACPOS (1, I) + 1
        MACPOS (2, I) = MACPOS (2, I) + 1
200   CONTINUE
C
      DO 300 I = 1, MACCUR
        IF (MACSTK (1, I) .GT. MACPOS (2, MACBEG)) THEN
          MACSTK (1, I) = MACSTK (1, I) + 1
          MACSTK (2, I) = MACSTK (2, I) + 1
        END IF
300   CONTINUE
C
      MACPOS (2, MACBEG) = MACPOS (2, MACBEG) + 1
      MACLIN (MACPOS (2, MACBEG)) = SMLINE
C
      ERRCOD = 0
      RETURN
      END
C
C
C-------------------------------
      INTEGER FUNCTION SMTOTM ()
C
C Package SMACRO
C
C Store and handle string macros
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program) into all
C the procedures in SMACRO.FOR before compiling.
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C  27-Nov-1987  modify to allow recursive calls
C
C MAXMAC  max number of macros
C MAXLIN  max number of lines for all macros
C MAXDPT  max depth of recursive macro calls
C
      INTEGER MAXMAC, MAXLIN, MAXDPT
      PARAMETER (MAXMAC = 40, MAXLIN = 200, MAXDPT = 32)
C
C MACCUR  pointer to top of pointer stack
C MACSTK  stack of pointers to next and last line of macro(s) called
C MACTOT  total number of defined macros
C MACPOS  pointers from macro to first and last lines
C MACBEG  pointer to macro currently being entered
C MACNAM  macro names
C MACLIN  macro lines
C
      INTEGER      MACCUR, MACTOT, MACBEG
      INTEGER      MACSTK (2, MAXDPT), MACPOS (2, MAXMAC)
      CHARACTER*20 MACNAM (MAXMAC)
      CHARACTER*80 MACLIN (MAXLIN)
C
      COMMON /SMDAT1/ MACCUR, MACTOT, MACBEG, MACSTK, MACPOS
      COMMON /SMDAT2/ MACNAM, MACLIN
      SAVE   /SMDAT1/, /SMDAT2/
C
      SMTOTM = MACTOT
      RETURN
      END
C
C
C--------------------------------------
      SUBROUTINE SMDELE (MACRO, ERRCOD)
C
C Package SMACRO
C
C Store and handle string macros
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program) into all
C the procedures in SMACRO.FOR before compiling.
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C  27-Nov-1987  modify to allow recursive calls
C
C MAXMAC  max number of macros
C MAXLIN  max number of lines for all macros
C MAXDPT  max depth of recursive macro calls
C
      INTEGER MAXMAC, MAXLIN, MAXDPT
      PARAMETER (MAXMAC = 40, MAXLIN = 200, MAXDPT = 32)
C
C MACCUR  pointer to top of pointer stack
C MACSTK  stack of pointers to next and last line of macro(s) called
C MACTOT  total number of defined macros
C MACPOS  pointers from macro to first and last lines
C MACBEG  pointer to macro currently being entered
C MACNAM  macro names
C MACLIN  macro lines
C
      INTEGER      MACCUR, MACTOT, MACBEG
      INTEGER      MACSTK (2, MAXDPT), MACPOS (2, MAXMAC)
      CHARACTER*20 MACNAM (MAXMAC)
      CHARACTER*80 MACLIN (MAXLIN)
C
      COMMON /SMDAT1/ MACCUR, MACTOT, MACBEG, MACSTK, MACPOS
      COMMON /SMDAT2/ MACNAM, MACLIN
      SAVE   /SMDAT1/, /SMDAT2/
C
      CHARACTER*(*) MACRO
      INTEGER       ERRCOD
C
C MACRO   (In)  macro to delete
C ERRCOD  (Out) error code
C      0 = no error
C      1 = no such macro to delete
C      2 = cannot delete macro while executing macro(s); stack not empty
C
C Externally defined function
C
      INTEGER BFINDS
C
C Help variables
C
      INTEGER I, SLOT, FREE
C
      SLOT = BFINDS (MACRO, MACNAM, MACTOT)
      IF (SLOT .LE. 0) THEN
        ERRCOD = 1
        RETURN
C
      ELSE IF (MACCUR .NE. 0) THEN
        ERRCOD = 2
        RETURN
      END IF
C
C Shift down lines, macros and pointers, if macro to delete is not last
C
      IF (SLOT .LT. MACTOT) THEN
        FREE = MACPOS (1, SLOT)
        DO 100 I = MACPOS (1, SLOT + 1), MACPOS (2, MACTOT)
          MACLIN (FREE) = MACLIN (I)
          FREE = FREE + 1
100     CONTINUE
C
        FREE = MACPOS (2, SLOT) - MACPOS (1, SLOT) + 1
        DO 200 I = SLOT, MACTOT - 1
          MACNAM (I) = MACNAM (I + 1)
          MACPOS (1, I) = MACPOS (1, I + 1) - FREE
          MACPOS (2, I) = MACPOS (2, I + 1) - FREE
200     CONTINUE
      END IF
C
      MACTOT = MACTOT - 1
C
      ERRCOD = 0
      RETURN
      END
C
C
C----------------------------------------------
      SUBROUTINE SMNAME (MACRO, MACNUM, ERRCOD)
C
C Package SMACRO
C
C Store and handle string macros
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program) into all
C the procedures in SMACRO.FOR before compiling.
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C  27-Nov-1987  modify to allow recursive calls
C
C MAXMAC  max number of macros
C MAXLIN  max number of lines for all macros
C MAXDPT  max depth of recursive macro calls
C
      INTEGER MAXMAC, MAXLIN, MAXDPT
      PARAMETER (MAXMAC = 40, MAXLIN = 200, MAXDPT = 32)
C
C MACCUR  pointer to top of pointer stack
C MACSTK  stack of pointers to next and last line of macro(s) called
C MACTOT  total number of defined macros
C MACPOS  pointers from macro to first and last lines
C MACBEG  pointer to macro currently being entered
C MACNAM  macro names
C MACLIN  macro lines
C
      INTEGER      MACCUR, MACTOT, MACBEG
      INTEGER      MACSTK (2, MAXDPT), MACPOS (2, MAXMAC)
      CHARACTER*20 MACNAM (MAXMAC)
      CHARACTER*80 MACLIN (MAXLIN)
C
      COMMON /SMDAT1/ MACCUR, MACTOT, MACBEG, MACSTK, MACPOS
      COMMON /SMDAT2/ MACNAM, MACLIN
      SAVE   /SMDAT1/, /SMDAT2/
C
      CHARACTER*(*) MACRO
      INTEGER       MACNUM, ERRCOD
C
C MACRO   (Out) macro name
C MACNUM  (In)  number of macro
C ERRCOD  (Out) error code
C      0 = no error
C      1 = macro number out of range
C
      IF (MACNUM.GT.0 .AND. MACNUM.LE.MACTOT) THEN
        MACRO = MACNAM (MACNUM)
        ERRCOD = 0
C
      ELSE
        ERRCOD = 1
      END IF
C
      RETURN
      END
C
C
C------------------------------------
      INTEGER FUNCTION SMSLOT (MACRO)
C
C Package SMACRO
C
C Store and handle string macros
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program) into all
C the procedures in SMACRO.FOR before compiling.
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C  27-Nov-1987  modify to allow recursive calls
C
C MAXMAC  max number of macros
C MAXLIN  max number of lines for all macros
C MAXDPT  max depth of recursive macro calls
C
      INTEGER MAXMAC, MAXLIN, MAXDPT
      PARAMETER (MAXMAC = 40, MAXLIN = 200, MAXDPT = 32)
C
C MACCUR  pointer to top of pointer stack
C MACSTK  stack of pointers to next and last line of macro(s) called
C MACTOT  total number of defined macros
C MACPOS  pointers from macro to first and last lines
C MACBEG  pointer to macro currently being entered
C MACNAM  macro names
C MACLIN  macro lines
C
      INTEGER      MACCUR, MACTOT, MACBEG
      INTEGER      MACSTK (2, MAXDPT), MACPOS (2, MAXMAC)
      CHARACTER*20 MACNAM (MAXMAC)
      CHARACTER*80 MACLIN (MAXLIN)
C
      COMMON /SMDAT1/ MACCUR, MACTOT, MACBEG, MACSTK, MACPOS
      COMMON /SMDAT2/ MACNAM, MACLIN
      SAVE   /SMDAT1/, /SMDAT2/
C
      CHARACTER*(*) MACRO
C
C MACRO   (In)  macro to find number
C
C Externally defined function
C
      INTEGER BFINDS
C
      SMSLOT = MAX (0, BFINDS (MACRO, MACNAM, MACTOT))
      RETURN
      END
C
C
C------------------------------------
      SUBROUTINE SMACTI (NUM, ERRCOD)
C
C Package SMACRO
C
C Store and handle string macros
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program) into all
C the procedures in SMACRO.FOR before compiling.
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C  27-Nov-1987  modify to allow recursive calls
C
C MAXMAC  max number of macros
C MAXLIN  max number of lines for all macros
C MAXDPT  max depth of recursive macro calls
C
      INTEGER MAXMAC, MAXLIN, MAXDPT
      PARAMETER (MAXMAC = 40, MAXLIN = 200, MAXDPT = 32)
C
C MACCUR  pointer to top of pointer stack
C MACSTK  stack of pointers to next and last line of macro(s) called
C MACTOT  total number of defined macros
C MACPOS  pointers from macro to first and last lines
C MACBEG  pointer to macro currently being entered
C MACNAM  macro names
C MACLIN  macro lines
C
      INTEGER      MACCUR, MACTOT, MACBEG
      INTEGER      MACSTK (2, MAXDPT), MACPOS (2, MAXMAC)
      CHARACTER*20 MACNAM (MAXMAC)
      CHARACTER*80 MACLIN (MAXLIN)
C
      COMMON /SMDAT1/ MACCUR, MACTOT, MACBEG, MACSTK, MACPOS
      COMMON /SMDAT2/ MACNAM, MACLIN
      SAVE   /SMDAT1/, /SMDAT2/
C
      INTEGER NUM, ERRCOD
C
C NUM     (In)  macro number
C ERRCOD  (Out) error code
C     0 = no error
C     1 = could not go deeper in recursive calls; stack overflow
C
      IF (MACCUR .GE. MAXDPT) THEN
        ERRCOD = 1
        RETURN
      END IF
C
C Don't bother if no lines in macro
C
      IF (MACPOS (1, NUM) .LE. MACPOS (2, NUM)) THEN
        MACCUR = MACCUR + 1
        MACSTK (1, MACCUR) = MACPOS (1, NUM)
        MACSTK (2, MACCUR) = MACPOS (2, NUM)
      END IF
C
      ERRCOD = 0
      RETURN
      END
C
C
C--------------------------------------
      SUBROUTINE SMFIND (MACRO, ERRCOD)
C
      CHARACTER*(*) MACRO
      INTEGER       ERRCOD
C
C MACRO   (In)  macro to find and activate
C ERRCOD  (Out) error code
C      0 = no error
C      1 = could not find macro
C      2 = could not go deeper in recursive calls; stack overflow
C
C Externally defined function
C
      INTEGER SMSLOT
C
C Help variable
C
      INTEGER SLOT
C
C Find macro in list
C
      SLOT = SMSLOT (MACRO)
      IF (SLOT .EQ. 0) THEN
        ERRCOD = 1
C
      ELSE
        CALL SMACTI (SLOT, ERRCOD)
        IF (ERRCOD .NE. 0) ERRCOD = 2
      END IF
C
      RETURN
      END
C
C
C---------------------------------------
      SUBROUTINE SMGETL (SMLINE, ERRCOD)
C
C Package SMACRO
C
C Store and handle string macros
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program) into all
C the procedures in SMACRO.FOR before compiling.
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C  27-Nov-1987  modify to allow recursive calls
C
C MAXMAC  max number of macros
C MAXLIN  max number of lines for all macros
C MAXDPT  max depth of recursive macro calls
C
      INTEGER MAXMAC, MAXLIN, MAXDPT
      PARAMETER (MAXMAC = 40, MAXLIN = 200, MAXDPT = 32)
C
C MACCUR  pointer to top of pointer stack
C MACSTK  stack of pointers to next and last line of macro(s) called
C MACTOT  total number of defined macros
C MACPOS  pointers from macro to first and last lines
C MACBEG  pointer to macro currently being entered
C MACNAM  macro names
C MACLIN  macro lines
C
      INTEGER      MACCUR, MACTOT, MACBEG
      INTEGER      MACSTK (2, MAXDPT), MACPOS (2, MAXMAC)
      CHARACTER*20 MACNAM (MAXMAC)
      CHARACTER*80 MACLIN (MAXLIN)
C
      COMMON /SMDAT1/ MACCUR, MACTOT, MACBEG, MACSTK, MACPOS
      COMMON /SMDAT2/ MACNAM, MACLIN
      SAVE   /SMDAT1/, /SMDAT2/
C
      CHARACTER*(*) SMLINE
      INTEGER       ERRCOD
C
C SMLINE  (Out) macro line
C ERRCOD  (Out) error code
C      0 = no error
C      1 = no current macro
C
      IF (MACCUR .EQ. 0) THEN
        ERRCOD = 1
        RETURN
      END IF
C
C Position to next line; if none then pop stack
C
      SMLINE = MACLIN (MACSTK (1, MACCUR))
      IF (MACSTK (1, MACCUR) .LT. MACSTK (2, MACCUR)) THEN
        MACSTK (1, MACCUR) = MACSTK (1, MACCUR) + 1
      ELSE
        MACCUR = MACCUR - 1
      END IF
C
      ERRCOD = 0
      RETURN
      END
C
C
C-------------------------------
      INTEGER FUNCTION SMNACT ()
C
C Package SMACRO
C
C Store and handle string macros
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program) into all
C the procedures in SMACRO.FOR before compiling.
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C  27-Nov-1987  modify to allow recursive calls
C
C MAXMAC  max number of macros
C MAXLIN  max number of lines for all macros
C MAXDPT  max depth of recursive macro calls
C
      INTEGER MAXMAC, MAXLIN, MAXDPT
      PARAMETER (MAXMAC = 40, MAXLIN = 200, MAXDPT = 32)
C
C MACCUR  pointer to top of pointer stack
C MACSTK  stack of pointers to next and last line of macro(s) called
C MACTOT  total number of defined macros
C MACPOS  pointers from macro to first and last lines
C MACBEG  pointer to macro currently being entered
C MACNAM  macro names
C MACLIN  macro lines
C
      INTEGER      MACCUR, MACTOT, MACBEG
      INTEGER      MACSTK (2, MAXDPT), MACPOS (2, MAXMAC)
      CHARACTER*20 MACNAM (MAXMAC)
      CHARACTER*80 MACLIN (MAXLIN)
C
      COMMON /SMDAT1/ MACCUR, MACTOT, MACBEG, MACSTK, MACPOS
      COMMON /SMDAT2/ MACNAM, MACLIN
      SAVE   /SMDAT1/, /SMDAT2/
C
      SMNACT = MACCUR
      RETURN
      END
C
C
C------------------------------------
      SUBROUTINE SMCLER (ALL, ERRCOD)
C
C Package SMACRO
C
C Store and handle string macros
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program) into all
C the procedures in SMACRO.FOR before compiling.
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C  27-Nov-1987  modify to allow recursive calls
C
C MAXMAC  max number of macros
C MAXLIN  max number of lines for all macros
C MAXDPT  max depth of recursive macro calls
C
      INTEGER MAXMAC, MAXLIN, MAXDPT
      PARAMETER (MAXMAC = 40, MAXLIN = 200, MAXDPT = 32)
C
C MACCUR  pointer to top of pointer stack
C MACSTK  stack of pointers to next and last line of macro(s) called
C MACTOT  total number of defined macros
C MACPOS  pointers from macro to first and last lines
C MACBEG  pointer to macro currently being entered
C MACNAM  macro names
C MACLIN  macro lines
C
      INTEGER      MACCUR, MACTOT, MACBEG
      INTEGER      MACSTK (2, MAXDPT), MACPOS (2, MAXMAC)
      CHARACTER*20 MACNAM (MAXMAC)
      CHARACTER*80 MACLIN (MAXLIN)
C
      COMMON /SMDAT1/ MACCUR, MACTOT, MACBEG, MACSTK, MACPOS
      COMMON /SMDAT2/ MACNAM, MACLIN
      SAVE   /SMDAT1/, /SMDAT2/
C
      LOGICAL ALL
      INTEGER ERRCOD
C
C ALL     (In)  skip all active macros (.TRUE.), or only top (.FALSE.)
C ERRCOD  (Out) error code
C      0 = no error
C      1 = no active macro
C
      IF (MACCUR .EQ. 0) THEN
        ERRCOD = 1
C
      ELSE
        IF (ALL) THEN
          MACCUR = 0
        ELSE
          MACCUR = MACCUR - 1
        END IF
        ERRCOD = 0
      END IF
C
      RETURN
      END
C
C
C----------------------
      BLOCK DATA SMINIT
C
C Package SMACRO
C
C Store and handle string macros
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program) into all
C the procedures in SMACRO.FOR before compiling.
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C  27-Nov-1987  modify to allow recursive calls
C
C MAXMAC  max number of macros
C MAXLIN  max number of lines for all macros
C MAXDPT  max depth of recursive macro calls
C
      INTEGER MAXMAC, MAXLIN, MAXDPT
      PARAMETER (MAXMAC = 40, MAXLIN = 200, MAXDPT = 32)
C
C MACCUR  pointer to top of pointer stack
C MACSTK  stack of pointers to next and last line of macro(s) called
C MACTOT  total number of defined macros
C MACPOS  pointers from macro to first and last lines
C MACBEG  pointer to macro currently being entered
C MACNAM  macro names
C MACLIN  macro lines
C
      INTEGER      MACCUR, MACTOT, MACBEG
      INTEGER      MACSTK (2, MAXDPT), MACPOS (2, MAXMAC)
      CHARACTER*20 MACNAM (MAXMAC)
      CHARACTER*80 MACLIN (MAXLIN)
C
      COMMON /SMDAT1/ MACCUR, MACTOT, MACBEG, MACSTK, MACPOS
      COMMON /SMDAT2/ MACNAM, MACLIN
      SAVE   /SMDAT1/, /SMDAT2/
C
      DATA MACTOT /0/, MACCUR /0/, MACBEG /0/
C
      END
C Package SNCNVT
C
C Conversion of character string from/to integer/real/fix data.
C
C SITOCH s  write integer value into string
C SRTOCH s  write float real value into string
C SFTOCH s  write fix real value into string
C SCHTOI s  convert string into integer value
C SCHTOR s  convert string into real value
C
C Uses no separately compiled procedures.
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   9-Sep-1987  written, tested
C  19-Jun-1990  added P format code to SRTOCH
C
C--------------------------------------
      SUBROUTINE SITOCH (CH, I, ERRCOD)
C
      CHARACTER*(*) CH
      INTEGER       I, ERRCOD
C
C CH      (Out) string to write integer value in
C I       (In)  integer value to write
C ERRCOD  (Out) error code
C      0 = no error
C      1 = string equivalent of integer could not fit into given string
C
      CHARACTER*5 FMTLIN
      SAVE FMTLIN
      DATA FMTLIN /'(I  )'/
C
      WRITE (FMTLIN (3:4), 10) LEN (CH)
10    FORMAT (I2)
      WRITE (CH, FMTLIN, ERR=90) I
      ERRCOD = 0
      RETURN
90    ERRCOD = 1
      RETURN
      END
C
C-------------------------------------------
      SUBROUTINE SRTOCH (CH, R, DEC, ERRCOD)
C
      CHARACTER*(*) CH
      REAL          R
      INTEGER       DEC, ERRCOD
C
C CH      (Out) string to write floating real value in
C R       (In)  floating real value to write
C DEC     (In)  number of decimals to use
C ERRCOD  (Out) error code
C      0 = no error
C      1 = string equivalent of real could not fit into given string
C      2 = illegal number of decimals
C
      CHARACTER*10 FMTLIN
      SAVE FMTLIN
      DATA FMTLIN /'(1PE  .  )'/
C
      IF (DEC .LT. 1 .OR. DEC .GT. 99) THEN
        ERRCOD = 2
        RETURN
      END IF
C
      WRITE (FMTLIN (5:6), 10) LEN (CH)
10    FORMAT (I2)
      WRITE (FMTLIN (8:9), 10) DEC
      WRITE (CH, FMTLIN, ERR=90) R
      ERRCOD = 0
      RETURN
C
90    ERRCOD = 1
      RETURN
      END
C
C-------------------------------------------
      SUBROUTINE SFTOCH (CH, F, DEC, ERRCOD)
C
      CHARACTER*(*) CH
      REAL          F
      INTEGER       DEC, ERRCOD
C
C CH      (Out) string to write fix real value in
C F       (In)  fix real value to write
C DEC     (In)  number of decimals to use
C ERRCOD  (Out) error code
C      0 = no error
C      1 = string equivalent of real could not fit into given string
C      2 = illegal number of decimals
C
      CHARACTER*8 FMTLIN
      SAVE FMTLIN
      DATA FMTLIN /'(F  .  )'/
C
      IF (DEC .LT. 0 .OR. DEC .GT. 99) THEN
        ERRCOD = 2
        RETURN
      END IF
C
      WRITE (FMTLIN (3:4), 10) LEN (CH)
10    FORMAT (I2)
      WRITE (FMTLIN (6:7), 10) DEC
      WRITE (CH, FMTLIN, ERR=90) F
      ERRCOD = 0
      RETURN
C
90    ERRCOD = 1
      RETURN
      END
C
C--------------------------------------
      SUBROUTINE SCHTOI (I, CH, ERRCOD)
C
      INTEGER       I, ERRCOD
      CHARACTER*(*) CH
C
C I       (Out) integer value to read
C CH      (In)  string to read integer value from
C ERRCOD  (Out) error code
C      0 = no error
C      1 = could not read integer value string
C
      CHARACTER*8 FMTLIN
      SAVE FMTLIN
      DATA FMTLIN /'(BN,I  )'/
C
      WRITE (FMTLIN (6:7), 10) LEN (CH)
10    FORMAT (I2)
      READ (CH, FMTLIN, ERR=90) I
      ERRCOD = 0
      RETURN
C
90    ERRCOD = 1
      RETURN
      END
C
C--------------------------------------
      SUBROUTINE SCHTOR (R, CH, ERRCOD)
C
      REAL          R
      CHARACTER*(*) CH
      INTEGER       ERRCOD
C
C R       (Out) real value to read
C CH      (In)  string to read real value from
C ERRCOD  (Out) error code
C      0 = no error
C      1 = could not read real value from string
C
      CHARACTER*10 FMTLIN
      SAVE FMTLIN
      DATA FMTLIN /'(BN,F  .0)'/
C
      WRITE (FMTLIN (6:7), 10) LEN (CH)
10    FORMAT (I2)
      READ (CH, FMTLIN, ERR=90) R
      ERRCOD = 0
      RETURN
C
90    ERRCOD = 1
      RETURN
      END
C Package SORT
C
C Sort arrays of values into ascending order, using the Quicksort
C algorithm, and find values in sorted arrays using binary search.
C
C Reference: Niklaus Wirth, Algorithms + Data Structures = Programs,
C 1976 Prentice-Hall Inc, Englewood Cliffs, New Jersey.
C
C Uses no separately compiled procedures.
C
C QSORTI s   sort array of integers
C QSORTS s   sort array of strings using lexical comparison function
C QSORTP s   sort array of pointers; comparison function passed as arg
C BFINDI if  search integer array, return negative for slot to insert in
C BFINDS if  search string array, return negative for slot to insert in
C INSRTI s   insert integer in given slot in array
C INSRTS s   insert string in given slot in array
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   30-May-1988  first attempts
C    8-Jun-1988  found and corrected bug involving comparand pointer
C   11-May-1989  declared COMPAR in QSORTP as EXTERNAL
C   25-Jun-1990  added QSORTI, BFINDI and INSRTI
C   13-Jul-1990  TOT argument into INSRT procedures not incremented
C
C
C-----------------------------------------------------------
      SUBROUTINE QSORTI (IARRAY, TOT, STACK, MAXSTK, ERRCOD)
C
      INTEGER TOT, MAXSTK, ERRCOD
      INTEGER IARRAY (TOT), STACK (MAXSTK)
C
C IARRAY  (InOut) array of integers to sort
C TOT     (In)  total number of integers in array
C STACK   (Scr) recursion stack
C MAXSTK  (In)  max size of recursion stack; should be = 2 * (2)log TOT
C ERRCOD  (Out) error code
C     0 = no error
C     1 = overflow in recursion stack
C
C Help variables
C
      INTEGER S, L, R, I, J, X, ISWAP
C
C Just skip if nothing to sort
C
      ERRCOD = 0
      IF (TOT .LE. 1) RETURN
C
C Initialize stack
C
      S = 2
      IF (S .GT. MAXSTK) GOTO 900
      STACK (S - 1) = 1
      STACK (S)     = TOT
C
C Simulated repeat-until loop 1; pop stack until empty
C
100   L = STACK (S - 1)
      R = STACK (S)
      S = S - 2
C
C Simulated repeat-until loop 2; split interval until nothing left
C
200     I = L
        J = R
C
C Choose (arbitrarily) slot in middle of interval as comparand
C
        X = (L + R) / 2
C
C Simulated repeat-until loop 3; partition until nothing left
C
300       IF (IARRAY (I) .LT. IARRAY (X)) THEN
            I = I + 1
            GOTO 300
          END IF
310       IF (IARRAY (X) .LT. IARRAY (J)) THEN
            J = J - 1
            GOTO 310
          END IF
C
C Swap array values if on correct side of each other in interval;
C beware of what happens to the pointer to the comparand slot
C
          IF (I .LE. J) THEN
            ISWAP      = IARRAY (I)
            IARRAY (I) = IARRAY (J)
            IARRAY (J) = ISWAP
            IF (X .EQ. I) THEN
              X = J
            ELSE IF (X .EQ. J) THEN
              X = I
            END IF
            I = I + 1
            J = J - 1
          END IF
C
C End of simulated repeat-until loop 3
C
          IF (I .LE. J) GOTO 300
C
C Recursion; process shorter interval first, and push longer to stack
C
        IF (J - L .LT. R - I) THEN
          IF (I .LT. R) THEN
            S = S + 2
            IF (S .GT. MAXSTK) GOTO 900
            STACK (S - 1) = I
            STACK (S)     = R
          END IF
          R = J
C
        ELSE
          IF (L .LT. J) THEN
            S = S + 2
            IF (S .GT. MAXSTK) GOTO 900
            STACK (S - 1) = L
            STACK (S)     = J
          END IF
          L = I
        END IF
C
C End of simulated repeat-until loop 2
C
        IF (L .LT. R) GOTO 200
C
C End of simulated repeat-until loop 1
C
      IF (S .GT. 0) GOTO 100
C
      RETURN
C
900   ERRCOD = 1
      RETURN
      END
C
C
C------------------------------------------------------------------
      SUBROUTINE QSORTS (SARRAY, TOT, SSWAP, STACK, MAXSTK, ERRCOD)
C
      INTEGER       TOT, MAXSTK, ERRCOD
      CHARACTER*(*) SARRAY (TOT)
      CHARACTER*(*) SSWAP
      INTEGER       STACK (MAXSTK)
C
C SARRAY  (InOut) array of strings to sort
C TOT     (In)  total number of strings in array
C SSWAP   (Scr) swap string variable
C STACK   (Scr) recursion stack
C MAXSTK  (In)  max size of recursion stack; should be = 2 * (2)log TOT
C ERRCOD  (Out) error code
C     0 = no error
C     1 = swap string too short; must Len (SSWAP) >= Len (SARRAY(1))
C     2 = overflow in recursion stack
C
C Help variables
C
      INTEGER S, L, R, I, J, X
C
C Check length of swap string variable
C
      IF (LEN (SSWAP) .LT. LEN (SARRAY (1))) THEN
        ERRCOD = 1
        RETURN
      END IF
C
C Just skip if nothing to sort
C
      ERRCOD = 0
      IF (TOT .LE. 1) RETURN
C
C Initialize stack
C
      S = 2
      IF (S .GT. MAXSTK) GOTO 900
      STACK (S - 1) = 1
      STACK (S)     = TOT
C
C Simulated repeat-until loop 1; pop stack until empty
C
100   L = STACK (S - 1)
      R = STACK (S)
      S = S - 2
C
C Simulated repeat-until loop 2; split interval until nothing left
C
200     I = L
        J = R
C
C Choose (arbitrarily) slot in middle of interval as comparand
C
        X = (L + R) / 2
C
C Simulated repeat-until loop 3; partition until nothing left
C
300       IF (LLT (SARRAY (I), SARRAY (X))) THEN
            I = I + 1
            GOTO 300
          END IF
310       IF (LLT (SARRAY (X), SARRAY (J))) THEN
            J = J - 1
            GOTO 310
          END IF
C
C Swap array values if on correct side of each other in interval;
C beware of what happens to the pointer to the comparand slot
C
          IF (I .LE. J) THEN
            SSWAP      = SARRAY (I)
            SARRAY (I) = SARRAY (J)
            SARRAY (J) = SSWAP
            IF (X .EQ. I) THEN
              X = J
            ELSE IF (X .EQ. J) THEN
              X = I
            END IF
            I = I + 1
            J = J - 1
          END IF
C
C End of simulated repeat-until loop 3
C
          IF (I .LE. J) GOTO 300
C
C Recursion; process shorter interval first, and push longer to stack
C
        IF (J - L .LT. R - I) THEN
          IF (I .LT. R) THEN
            S = S + 2
            IF (S .GT. MAXSTK) GOTO 900
            STACK (S - 1) = I
            STACK (S)     = R
          END IF
          R = J
C
        ELSE
          IF (L .LT. J) THEN
            S = S + 2
            IF (S .GT. MAXSTK) GOTO 900
            STACK (S - 1) = L
            STACK (S)     = J
          END IF
          L = I
        END IF
C
C End of simulated repeat-until loop 2
C
        IF (L .LT. R) GOTO 200
C
C End of simulated repeat-until loop 1
C
      IF (S .GT. 0) GOTO 100
C
      RETURN
C
900   ERRCOD = 2
      RETURN
      END
C
C
C-------------------------------------------------------------------
      SUBROUTINE QSORTP (PARRAY, TOT, STACK, MAXSTK, COMPAR, ERRCOD)
C
      INTEGER TOT, MAXSTK, COMPAR, ERRCOD
      INTEGER PARRAY (TOT), STACK (MAXSTK)
      EXTERNAL COMPAR
C
C PARRAY  (InOut) array of pointers to sort
C TOT     (In)  total number of pointers in array
C STACK   (Scr) recursion stack
C MAXSTK  (In)  max size of recursion stack; should be = 2 * (2)Log TOT
C COMPAR  (Func) comparison function: INTEGER FUNCTION COMPAR (P1, P2),
C                where P1 and P2 are integer pointers from PARRAY;
C                return -1 if Obj(P1) < Obj(P2), 0 if Obj(P1) = Obj(P2),
C                and 1 if Obj(P1) > Obj(P2). Must be declared EXTERNAL
C                in calling procedure.
C ERRCOD  (Out) error code
C     0 = no error
C     1 = overflow in recursion stack
C
C Help variables
C
      INTEGER S, L, R, I, J, X, PSWAP
C
C Just skip if nothing to sort
C
      ERRCOD = 0
      IF (TOT .LE. 1) RETURN
C
C Initialize stack
C
      S = 2
      IF (S .GT. MAXSTK) GOTO 900
      STACK (S - 1) = 1
      STACK (S)     = TOT
C
C Simulated repeat-until loop 1; pop stack until empty
C
100   L = STACK (S - 1)
      R = STACK (S)
      S = S - 2
C
C Simulated repeat-until loop 2; split interval until nothing left
C
200     I = L
        J = R
C
C Choose (arbitrarily) value in middle of interval as comparand
C
        X = (L + R) / 2
C
C Simulated repeat-until loop 3; partition until nothing left
C
300       IF (COMPAR (PARRAY (I), PARRAY (X)) .LT. 0) THEN
            I = I + 1
            GOTO 300
          END IF
310       IF (COMPAR (PARRAY (X), PARRAY (J)) .LT. 0) THEN
            J = J - 1
            GOTO 310
          END IF
C
C Swap array values if on correct side of each other in interval;
C beware of what happens to the pointer to the comparand slot
C
          IF (I .LE. J) THEN
            PSWAP      = PARRAY (I)
            PARRAY (I) = PARRAY (J)
            PARRAY (J) = PSWAP
            IF (X .EQ. I) THEN
              X = J
            ELSE IF (X .EQ. J) THEN
              X = I
            END IF
            I = I + 1
            J = J - 1
          END IF
C
C End of simulated repeat-until loop 3
C
          IF (I .LE. J) GOTO 300
C
C Recursion; process shorter interval first, and push interval to stack
C
        IF (J - L .LT. R - I) THEN
          IF (I .LT. R) THEN
            S = S + 2
            IF (S .GT. MAXSTK) GOTO 900
            STACK (S - 1) = I
            STACK (S)     = R
          END IF
          R = J
C
        ELSE
          IF (L .LT. J) THEN
            S = S + 2
            IF (S .GT. MAXSTK) GOTO 900
            STACK (S - 1) = L
            STACK (S)     = J
          END IF
          L = I
        END IF
C
C End of simulated repeat-until loop 2
C
        IF (L .LT. R) GOTO 200
C
C End of simulated repeat-until loop 1
C
      IF (S .GT. 0) GOTO 100
C
      RETURN
C
900   ERRCOD = 1
      RETURN
      END
C
C
C---------------------------------------------
      INTEGER FUNCTION BFINDI (I, IARRAY, TOT)
C
      INTEGER I, TOT
      INTEGER IARRAY (*)
C
C I      (In)  integer to look for in array
C IARRAY (In)  ordered integer array
C TOT    (In)  total number of integers in array
C    (Return)  slot number if integer is in array,
C              negative slot number to insert at if not in array
C
C Help variables
C
      INTEGER LO, HI, MIDDLE
C
C If nothing in list, return first slot as insertion slot
C
      IF (TOT .LE. 0) THEN
        BFINDI = - 1
C
C Initialize binary search
C
      ELSE
        LO = 1
        HI = TOT
C
C Simulated repeat-until loop
C
100     MIDDLE = (LO + HI) / 2
C
C Integer matches that in middle of current interval; return slot number
C
          IF (I .EQ. IARRAY (MIDDLE)) THEN
            BFINDI = MIDDLE
            RETURN
C
C If no match, set new interval limits
C
          ELSE IF (I .GT. IARRAY (MIDDLE)) THEN
            LO = MIDDLE + 1
          ELSE
            HI = MIDDLE - 1
          END IF
C
C Loop back if interval still non-zero
C
        IF (HI .GE. LO) GOTO 100
C
C Integer is not in array; return negative slot number to insert at
C
        BFINDI = - LO
C
      END IF
C
      RETURN
      END
C
C
C-----------------------------------------------
      INTEGER FUNCTION BFINDS (STR, SARRAY, TOT)
C
      CHARACTER*(*) STR
      INTEGER       TOT
      CHARACTER*(*) SARRAY (*)
C
C STR    (In)  string to look for in string array
C SARRAY (In)  lexically ordered string array
C TOT    (In)  total number of strings in array
C    (Return)  slot number if string is in array,
C              negative slot number to insert at if not in array
C
C Help variables
C
      INTEGER LO, HI, MIDDLE
C
C If nothing in list, return first slot as insertion slot
C
      IF (TOT .LE. 0) THEN
        BFINDS = - 1
C
C Initialize binary search
C
      ELSE
        LO = 1
        HI = TOT
C
C Simulated repeat-until loop
C
100     MIDDLE = (LO + HI) / 2
C
C String matches that in middle of current interval; return slot number
C
          IF (STR .EQ. SARRAY (MIDDLE)) THEN
            BFINDS = MIDDLE
            RETURN
C
C If no match, set new interval limits
C
          ELSE IF (LGT (STR, SARRAY (MIDDLE))) THEN
            LO = MIDDLE + 1
          ELSE
            HI = MIDDLE - 1
          END IF
C
C Loop back if interval still non-zero
C
        IF (HI .GE. LO) GOTO 100
C
C String is not in array; return negative slot number to insert at
C
        BFINDS = - LO
C
      END IF
C
      RETURN
      END
C
C
C---------------------------------------------
      SUBROUTINE INSRTI (I, SLOT, IARRAY, TOT)
C
      INTEGER I, SLOT, TOT
      INTEGER IARRAY (*)
C
C I       (In) integer to insert into array
C SLOT    (In) slot number in array to insert at
C IARRAY  (In) integer array; must have space for one more
C TOT     (In) total number of integers in input array
C
C Help variable
C
      INTEGER ITEM
C
      DO 100 ITEM = TOT, SLOT, -1
        IARRAY (ITEM + 1) = IARRAY (ITEM)
100   CONTINUE
C
      IARRAY (SLOT) = I
C
      RETURN
      END
C
C
C---------------------------------------------
      SUBROUTINE INSRTS (S, SLOT, SARRAY, TOT)
C
      CHARACTER*(*) S
      INTEGER       SLOT, TOT
      CHARACTER*(*) SARRAY (*)
C
C S      (In) string to insert into array
C SLOT   (In) slot number in array to insert at
C SARRAY (In) string array; must have space for one more
C TOT    (In) total number of strings in input array
C
C Help variable
C
      INTEGER ITEM
C
      DO 100 ITEM = TOT, SLOT, -1
        SARRAY (ITEM + 1) = SARRAY (ITEM)
100   CONTINUE
C
      SARRAY (SLOT) = S
C
      RETURN
      END
C Package SQUEUE
C
C First-in, first-out string queue store and handle
C
C SQUPUT s   put string in queue
C SQUGET s   get string from queue
C SQUCLR s   clear queue
C SQUNUM if  return number of items in queue
C SQUDT1 o   numerical data structure
C SQUDT2 o   character data structure
C SQUDIN b   initialize data structure
C
C The file SQUEUE.INC, which contains the data structure used by the
C procedures, must be inserted into each procedure (using VAX FORTRAN
C INCLUDE, editor or utility program) before compiling.
C
C Uses no separately compiled procedures.
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  18-Nov-1987  first attempts
C
C
C-------------------------------------
      SUBROUTINE SQUPUT (LINE, ERRCOD)
C
C Package SQUEUE
C
C First-in, first-out string queue store and handle
C
C This file must be inserted into each procedure (using VAX FORTRAN
C INCLUDE, editor or utility program) before compiling.
C
C (C) Copyright Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  18-Nov-1987  first attempts
C
C MAXSQU  maximum number of strings in queue
C
      INTEGER    MAXSQU
      PARAMETER (MAXSQU = 20)
C
C SQDFST  current first string slot to output
C SQDNXT  current next string slot to input
C SQDLIN  queue of string lines, circular
C
      INTEGER      SQDFST, SQDNXT
      CHARACTER*80 SQDLIN (MAXSQU)
C
      COMMON /SQUDT1/ SQDFST, SQDNXT
      COMMON /SQUDT2/ SQDLIN
      SAVE   /SQUDT1/, /SQUDT2/
C
      CHARACTER*(*) LINE
      INTEGER       ERRCOD
C
C LINE    (In)  string to put into queue
C ERRCOD  (Out) error code
C      0 = no error
C      1 = queue full; cannot put string into it
C
C To ensure that we initialize
C
      EXTERNAL SQUDIN
C
      IF ((SQDFST - SQDNXT .EQ. 1) .OR.
     $    (SQDNXT.EQ.MAXSQU .AND. SQDFST.EQ.1)) THEN
        ERRCOD = 1
      ELSE
        SQDLIN (SQDNXT) = LINE
        SQDNXT = SQDNXT + 1
        IF (SQDNXT .GT. MAXSQU) SQDNXT = 1
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C-------------------------------------
      SUBROUTINE SQUGET (LINE, ERRCOD)
C
C Package SQUEUE
C
C First-in, first-out string queue store and handle
C
C This file must be inserted into each procedure (using VAX FORTRAN
C INCLUDE, editor or utility program) before compiling.
C
C (C) Copyright Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  18-Nov-1987  first attempts
C
C MAXSQU  maximum number of strings in queue
C
      INTEGER    MAXSQU
      PARAMETER (MAXSQU = 20)
C
C SQDFST  current first string slot to output
C SQDNXT  current next string slot to input
C SQDLIN  queue of string lines, circular
C
      INTEGER      SQDFST, SQDNXT
      CHARACTER*80 SQDLIN (MAXSQU)
C
      COMMON /SQUDT1/ SQDFST, SQDNXT
      COMMON /SQUDT2/ SQDLIN
      SAVE   /SQUDT1/, /SQUDT2/
C
      CHARACTER*(*) LINE
      INTEGER       ERRCOD
C
C LINE    (Out) string from queue
C ERRCOD  (Out) error code
C      0 = no error
C      1 = queue empty; no string to get
C
C To ensure that we initialize
C
      EXTERNAL SQUDIN
C
      IF (SQDFST .EQ. SQDNXT) THEN
        ERRCOD = 1
      ELSE
        LINE = SQDLIN (SQDFST)
        SQDFST = SQDFST + 1
        IF (SQDFST .GT. MAXSQU) SQDFST = 1
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C----------------------
      SUBROUTINE SQUCLR
C
C Package SQUEUE
C
C First-in, first-out string queue store and handle
C
C This file must be inserted into each procedure (using VAX FORTRAN
C INCLUDE, editor or utility program) before compiling.
C
C (C) Copyright Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  18-Nov-1987  first attempts
C
C MAXSQU  maximum number of strings in queue
C
      INTEGER    MAXSQU
      PARAMETER (MAXSQU = 20)
C
C SQDFST  current first string slot to output
C SQDNXT  current next string slot to input
C SQDLIN  queue of string lines, circular
C
      INTEGER      SQDFST, SQDNXT
      CHARACTER*80 SQDLIN (MAXSQU)
C
      COMMON /SQUDT1/ SQDFST, SQDNXT
      COMMON /SQUDT2/ SQDLIN
      SAVE   /SQUDT1/, /SQUDT2/
C
C To ensure that we initialize
C
      EXTERNAL SQUDIN
C
      SQDFST = SQDNXT
      RETURN
      END
C
C
C-------------------------------
      INTEGER FUNCTION SQUNUM ()
C
C Package SQUEUE
C
C First-in, first-out string queue store and handle
C
C This file must be inserted into each procedure (using VAX FORTRAN
C INCLUDE, editor or utility program) before compiling.
C
C (C) Copyright Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  18-Nov-1987  first attempts
C
C MAXSQU  maximum number of strings in queue
C
      INTEGER    MAXSQU
      PARAMETER (MAXSQU = 20)
C
C SQDFST  current first string slot to output
C SQDNXT  current next string slot to input
C SQDLIN  queue of string lines, circular
C
      INTEGER      SQDFST, SQDNXT
      CHARACTER*80 SQDLIN (MAXSQU)
C
      COMMON /SQUDT1/ SQDFST, SQDNXT
      COMMON /SQUDT2/ SQDLIN
      SAVE   /SQUDT1/, /SQUDT2/
C
C To ensure that we initialize
C
      EXTERNAL SQUDIN
C
      IF (SQDNXT .GE. SQDFST) THEN
        SQUNUM = SQDNXT - SQDFST
      ELSE
        SQUNUM = SQDNXT + MAXSQU - SQDFST
      END IF
      RETURN
      END
C
C
C----------------------
      BLOCK DATA SQUDIN
C
C Package SQUEUE
C
C First-in, first-out string queue store and handle
C
C This file must be inserted into each procedure (using VAX FORTRAN
C INCLUDE, editor or utility program) before compiling.
C
C (C) Copyright Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  18-Nov-1987  first attempts
C
C MAXSQU  maximum number of strings in queue
C
      INTEGER    MAXSQU
      PARAMETER (MAXSQU = 20)
C
C SQDFST  current first string slot to output
C SQDNXT  current next string slot to input
C SQDLIN  queue of string lines, circular
C
      INTEGER      SQDFST, SQDNXT
      CHARACTER*80 SQDLIN (MAXSQU)
C
      COMMON /SQUDT1/ SQDFST, SQDNXT
      COMMON /SQUDT2/ SQDLIN
      SAVE   /SQUDT1/, /SQUDT2/
C
      DATA SQDFST /1/, SQDNXT /1/
C
      END
C Package SSTACK
C
C Store and handle stack of strings
C
C SSTPSH s  push string onto stack
C SSTPOP s  pop string from stack
C SSTCLR s  clear stack
C SSTTOT if current number of items on stack
C SSTSWP s  swap the two top items on the stack
C SSTINV s  invert the items on the stack
C SSTDT1 o  numerical data structure
C SSTDT2 o  character data structure
C SSTDIN b  initialize data structure
C
C The file sstack.inc, which contains the data structure used by the
C procedures, must be included in each procedure (using VAX FORTRAN
C INCLUDE, editor or utility program) before compiling.
C
C Uses no separately compiled procedures.
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C
C
C-------------------------------------
      SUBROUTINE SSTPSH (LINE, ERRCOD)
C
C Package SSTACK
C
C Store and handle stack of strings
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program) into all
C the procedures in CMACRO.FOR before compiling.
C
C (C) Copyright Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C
C MAXSST  maximum number of items on stack
C
      INTEGER    MAXSST
      PARAMETER (MAXSST = 32)
C
C SSTCUR  current top of stack
C SSTLIN  stack string items
C
      INTEGER      SSTCUR
      CHARACTER*80 SSTLIN (MAXSST)
C
      COMMON /SSTDT1/ SSTCUR
      COMMON /SSTDT2/ SSTLIN
      SAVE   /SSTDT1/, /SSTDT2/
C
C To ensure that we initialize
C
      EXTERNAL SSTDIN
C
      CHARACTER*(*) LINE
      INTEGER       ERRCOD
C
C LINE   (In)  string to push onto stack
C ERRCOD (Out) error code
C      0 = no error
C      1 = stack full; cannot push string onto it
C
      IF (SSTCUR .GE. MAXSST) THEN
        ERRCOD = 1
      ELSE
        SSTCUR = SSTCUR + 1
        SSTLIN (SSTCUR) = LINE
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C-------------------------------------
      SUBROUTINE SSTPOP (LINE, ERRCOD)
C
C Package SSTACK
C
C Store and handle stack of strings
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program) into all
C the procedures in CMACRO.FOR before compiling.
C
C (C) Copyright Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C
C MAXSST  maximum number of items on stack
C
      INTEGER    MAXSST
      PARAMETER (MAXSST = 32)
C
C SSTCUR  current top of stack
C SSTLIN  stack string items
C
      INTEGER      SSTCUR
      CHARACTER*80 SSTLIN (MAXSST)
C
      COMMON /SSTDT1/ SSTCUR
      COMMON /SSTDT2/ SSTLIN
      SAVE   /SSTDT1/, /SSTDT2/
C
C To ensure that we initialize
C
      EXTERNAL SSTDIN
C
      CHARACTER*(*) LINE
      INTEGER       ERRCOD
C
C LINE    (Out) string popped from stacked
C ERRCOD  (Out) error code
C      0 = no error
C      1 = stack empty; no string to pop
C
      IF (SSTCUR .EQ. 0) THEN
        ERRCOD = 1
      ELSE
        LINE = SSTLIN (SSTCUR)
        SSTCUR = SSTCUR - 1
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C----------------------
      SUBROUTINE SSTCLR
C
C Package SSTACK
C
C Store and handle stack of strings
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program) into all
C the procedures in CMACRO.FOR before compiling.
C
C (C) Copyright Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C
C MAXSST  maximum number of items on stack
C
      INTEGER    MAXSST
      PARAMETER (MAXSST = 32)
C
C SSTCUR  current top of stack
C SSTLIN  stack string items
C
      INTEGER      SSTCUR
      CHARACTER*80 SSTLIN (MAXSST)
C
      COMMON /SSTDT1/ SSTCUR
      COMMON /SSTDT2/ SSTLIN
      SAVE   /SSTDT1/, /SSTDT2/
C
C To ensure that we initialize
C
      EXTERNAL SSTDIN
C
      SSTCUR = 0
      RETURN
      END
C
C
C-------------------------------
      INTEGER FUNCTION SSTTOT ()
C
C Package SSTACK
C
C Store and handle stack of strings
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program) into all
C the procedures in CMACRO.FOR before compiling.
C
C (C) Copyright Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C
C MAXSST  maximum number of items on stack
C
      INTEGER    MAXSST
      PARAMETER (MAXSST = 32)
C
C SSTCUR  current top of stack
C SSTLIN  stack string items
C
      INTEGER      SSTCUR
      CHARACTER*80 SSTLIN (MAXSST)
C
      COMMON /SSTDT1/ SSTCUR
      COMMON /SSTDT2/ SSTLIN
      SAVE   /SSTDT1/, /SSTDT2/
C
C To ensure that we initialize
C
      EXTERNAL SSTDIN
C
      SSTTOT = SSTCUR
      RETURN
      END
C
C
C-------------------------------
      SUBROUTINE SSTSWP (ERRCOD)
C
C Package SSTACK
C
C Store and handle stack of strings
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program) into all
C the procedures in CMACRO.FOR before compiling.
C
C (C) Copyright Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C
C MAXSST  maximum number of items on stack
C
      INTEGER    MAXSST
      PARAMETER (MAXSST = 32)
C
C SSTCUR  current top of stack
C SSTLIN  stack string items
C
      INTEGER      SSTCUR
      CHARACTER*80 SSTLIN (MAXSST)
C
      COMMON /SSTDT1/ SSTCUR
      COMMON /SSTDT2/ SSTLIN
      SAVE   /SSTDT1/, /SSTDT2/
C
C To ensure that we initialize
C
      EXTERNAL SSTDIN
C
      INTEGER ERRCOD
C
C ERRCOD  (Out) error code
C      0 = no error
C      1 = could not swap items; less than 2 items on stack
C
      CHARACTER*80 TMP
C
      IF (SSTCUR .LT. 2) THEN
        ERRCOD = 1
      ELSE
        TMP = SSTLIN (SSTCUR)
        SSTLIN (SSTCUR) = SSTLIN (SSTCUR - 1)
        SSTLIN (SSTCUR - 1) = TMP
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C----------------------
      SUBROUTINE SSTINV
C
C Package SSTACK
C
C Store and handle stack of strings
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program) into all
C the procedures in CMACRO.FOR before compiling.
C
C (C) Copyright Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C
C MAXSST  maximum number of items on stack
C
      INTEGER    MAXSST
      PARAMETER (MAXSST = 32)
C
C SSTCUR  current top of stack
C SSTLIN  stack string items
C
      INTEGER      SSTCUR
      CHARACTER*80 SSTLIN (MAXSST)
C
      COMMON /SSTDT1/ SSTCUR
      COMMON /SSTDT2/ SSTLIN
      SAVE   /SSTDT1/, /SSTDT2/
C
C To ensure that we initialize
C
      EXTERNAL SSTDIN
C
      INTEGER      I, LAST
      CHARACTER*80 TMP
C
      LAST = SSTCUR
      DO 100 I = 1, SSTCUR / 2
        TMP = SSTLIN (LAST)
        SSTLIN (LAST) = SSTLIN (I)
        SSTLIN (I) = TMP
        LAST = LAST - 1
100   CONTINUE
      RETURN
      END
C
C
C----------------------
      BLOCK DATA SSTDIN
C
C Package SSTACK
C
C Store and handle stack of strings
C
C Data structure for the procedures. This file must be included
C (using VAX FORTRAN INCLUDE, editor or utility program) into all
C the procedures in CMACRO.FOR before compiling.
C
C (C) Copyright Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   7-Nov-1987  first attempts
C
C MAXSST  maximum number of items on stack
C
      INTEGER    MAXSST
      PARAMETER (MAXSST = 32)
C
C SSTCUR  current top of stack
C SSTLIN  stack string items
C
      INTEGER      SSTCUR
      CHARACTER*80 SSTLIN (MAXSST)
C
      COMMON /SSTDT1/ SSTCUR
      COMMON /SSTDT2/ SSTLIN
      SAVE   /SSTDT1/, /SSTDT2/
C
      DATA SSTCUR /0/
C
      END
C Package STRBLK
C
C Finding and eliminating blanks in string.
C
C SFNBLK if  position of first nonblank character
C SLNBLK if  position of last nonblank character
C SLEJYR s   left justify, remove blanks
C SRIJYR s   right justify, remove blanks
C SLESHF s   left shift, leave blanks after first nonblank untouched
C SRISHF s   right shift, leave blanks before last nonblank untouched
C SAPPND s   append second string to first, starting after last nonblank
C
C Uses no separately compiled procedures.
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C    2-May-1987  main code written
C   16-Sep-1987  package created by splitting up STRUTL
C    3-Oct-1987  procedures SLESHF, SRISHF and SAPPND added
C   21-Oct-1990  fixed bug in SRISHF; slight internal modifications
C
C
C----------------------------------
      INTEGER FUNCTION SFNBLK (STR)
C
      CHARACTER*(*) STR
C
      INTEGER POS
C
      DO 100 POS = 1, LEN (STR)
        IF (STR (POS : POS) .NE. ' ') THEN
          SFNBLK = POS
          RETURN
        END IF
100   CONTINUE
C
      SFNBLK = 0
      RETURN
      END
C
C
C----------------------------------
      INTEGER FUNCTION SLNBLK (STR)
C
      CHARACTER*(*) STR
C
      INTEGER POS
C
      DO 100 POS = LEN (STR), 1, -1
        IF (STR (POS : POS) .NE. ' ') THEN
          SLNBLK = POS
          RETURN
        END IF
100   CONTINUE

      SLNBLK = 0
      RETURN
      END
C
C
C----------------------------
      SUBROUTINE SLEJYR (STR)
C
      CHARACTER*(*) STR
C
      INTEGER POS, CURR
C
      CURR = 0
      DO 100 POS = 1, LEN (STR)
        IF (STR (POS : POS) .NE. ' ') THEN
          CURR = CURR + 1
          STR (CURR : CURR) = STR (POS : POS)
          IF (POS .NE. CURR) STR (POS : POS) = ' '
        END IF
100   CONTINUE
C
      RETURN
      END
C
C
C----------------------------
      SUBROUTINE SRIJYR (STR)
C
      CHARACTER*(*) STR
C
      INTEGER POS, CURR
C
      CURR = LEN (STR) + 1
      DO 100 POS = LEN (STR), 1, -1
        IF (STR (POS : POS) .NE. ' ') THEN
          CURR = CURR - 1
          STR (CURR : CURR) = STR (POS : POS)
          IF (POS .NE. CURR) STR (POS : POS) = ' '
        END IF
100   CONTINUE
C
      RETURN
      END
C
C
C----------------------------
      SUBROUTINE SLESHF (STR)
C
      CHARACTER*(*) STR
C
      INTEGER SFNBLK
C
      INTEGER POS
C
      POS = SFNBLK (STR)
      IF (POS .LE. 1) RETURN
      STR = STR (POS :)
      RETURN
      END
C
C
C----------------------------
      SUBROUTINE SRISHF (STR)
C
      CHARACTER *(*) STR
C
      INTEGER SLNBLK
C
      INTEGER POS, STRLEN
C
      STRLEN = LEN (STR)
      POS = SLNBLK (STR)
      IF (POS .EQ. 0) RETURN
      IF (POS .EQ. STRLEN) RETURN
      STR (STRLEN - POS + 1 :) = STR (: POS)
      STR (1 : STRLEN - POS) = ' '
      RETURN
      END
C
C
C-----------------------------------
      SUBROUTINE SAPPND (STR1, STR2)
C
      CHARACTER*(*) STR1, STR2
C
      INTEGER SLNBLK
C
      INTEGER POS
C
      POS = SLNBLK (STR1)
      IF (POS .LT. LEN (STR1)) STR1 (POS + 1 :) = STR2
      RETURN
      END
C Package STRUTL
C
C Simple string utility procedures. Assumes ASCII character code.
C
C SUPCAS s   all characters into upcase
C SLOCAS s   all characters into lowcase
C SCHCHR s   change all occurrences of a specified character
C SMINDX if  position of first of specified set of characters, 0 if none
C SRINDX if  position of last occurrence of string
C SDIGIT lf  TRUE if character is digit, otherwise FALSE
C SALPHA lf  TRUE if character is alphabetical, otherwise FALSE
C
C Uses no separately compiled procedures.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C    2-May-1987  main code written
C    3-Jun-1987  SMINDX added
C   16-Sep-1987  package split up; see STRBLK, SNCNVT
C   15-Mar-1988  SDIGIT and SALPHA added
C   15-Jul-1989  SRINDX added
C
C
C----------------------------
      SUBROUTINE SUPCAS (STR)
C
      CHARACTER*(*) STR
C
      INTEGER I, ASCII
C
      DO 100 I = 1, LEN (STR)
        ASCII = ICHAR (STR (I:I))
        IF (ASCII.GE.97 .AND. ASCII.LE.122) STR (I:I) = CHAR (ASCII-32)
100   CONTINUE
      RETURN
      END
C
C
C----------------------------
      SUBROUTINE SLOCAS (STR)
C
      CHARACTER*(*) STR
C
      INTEGER I, ASCII
C
      DO 100 I = 1, LEN (STR)
        ASCII = ICHAR (STR (I:I))
        IF (ASCII.GE.65 .AND. ASCII.LE.90) STR (I:I) = CHAR (ASCII+32)
100   CONTINUE
      RETURN
      END
C
C
C----------------------------------------
      SUBROUTINE SCHCHR (STR, OLDC, NEWC)
C
      CHARACTER*(*) STR
      CHARACTER*1   OLDC, NEWC
C
      INTEGER I
C
      DO 100 I = 1, LEN (STR)
        IF (STR (I:I) .EQ. OLDC) STR (I:I) = NEWC
100   CONTINUE
      RETURN
      END
C
C
C--------------------------------------
      INTEGER FUNCTION SMINDX (STR, CH)
C
      CHARACTER*(*) STR, CH
C
      INTEGER I, POS, MINPOS
C
      SMINDX = 0
      MINPOS = LEN (STR) + 1
      DO 100 I = 1, LEN (CH)
        POS = INDEX (STR, CH(I:I))
        IF (POS .NE. 0) THEN
          IF (POS .LT. MINPOS) THEN
            SMINDX = POS
            MINPOS = POS
          END IF
        END IF
100   CONTINUE
      RETURN
      END
C
C
C--------------------------------------
      INTEGER FUNCTION SRINDX (STR, CH)
C
      CHARACTER*(*) STR, CH
C
      INTEGER I, LENSTR, LENCH
C
      LENSTR = LEN (STR)
      LENCH  = LEN (CH)
      DO 100 I = LENSTR - LENCH + 1, 1, -1
        IF (STR (I : I + LENCH - 1) .EQ. CH) THEN
          SRINDX = I
          RETURN
        END IF
100   CONTINUE
      SRINDX = 0
      RETURN
      END
C
C
C---------------------------------
      LOGICAL FUNCTION SDIGIT (CH)
C
      CHARACTER*1 CH
C
      INTEGER ASCII
C
      ASCII = ICHAR (CH)
      SDIGIT = ASCII.GE.48 .AND. ASCII.LE.57
      RETURN
      END
C
C
C---------------------------------
      LOGICAL FUNCTION SALPHA (CH)
C
      CHARACTER*1 CH
C
      INTEGER ASCII
C
      ASCII = ICHAR (CH)
      SALPHA = (ASCII.GE.65 .AND. ASCII.LE.90) .OR.
     $         (ASCII.GE.97 .AND. ASCII.LE.122)
      RETURN
      END
C Package SYNTAX
C
C Read syntax definition for a language and then parse source code
C in that language; a table-driven one-symbol-lookahead without
C backtracking top-down parser.
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   18-May-1988  first attempts
C    9-Jun-1988  meta-syntax defined
C   21-Jun-1988  main code written
C   30-Jun-1988  code for rules 1 and 2 written; debugged
C    7-Jul-1988  bug-fix in syntax check; check only nodes of same kind
C   26-Nov-1988  introduced basic symbols and current syntax node
C    8-May-1989  removed tty output, introduced error msg output
C   25-May-1989  included possibility for reserved word check
C   13-Jan-1990  changes following LEXANA rewrite
C    4-Jul-1990  removed minor bug relating to RSVW in SXITEM
C   13-Sep-1990  changes to error messages when overflow
C
C Procedures to handle syntax definition:
C
C SXSINI s   initialize syntax definition; define special character sets
C SXPROD s   enter one line of production code; parsed internally
C SXSCHK s   check syntax; prod finished, symbols defined, rules 1 and 2
C SXSERR s   meta-syntax error message
C SXEPOS s   position of erroneous item in production line
C SXMARK s   create error marker string
C
C Procedures to parse language code:
C
C SXLINI s   initialize language code parsing
C SXPARS s   input one language item and parse it
C SXITEM cf  current syntax node identifier
C SXTYPE if  current syntax node type;
C            0=none, 1=delimiter token, 2=literal token, 3=integer,
C            4=real, 5=identifier, 6=word, 7=string
C SXINT  if  current syntax node integer value
C SXREAL rf  current syntax node real value
C SXDPTH if  current depth of symbol stack, after popping or not
C SXPSYM cf  production identifier symbol from symbol stack
C SXLERR s   syntax error message
C
C Help procedures:
C
C SXPBEG s   begin production
C SXPEND s   end production
C SXESYM s   enter symbol
C SXETOK s   enter token
C SXRBEG s   begin repeat
C SXREND s   end repeat
C SXOBEG s   begin option
C SXOEND s   end option
C SXCBEG s   begin choice
C SXCEND s   end choice
C SXPSET s   set pointers for new syntax node
C SXPFIX s   set pointers according to any left-over flags
C SXALTL s   make list of all alternatives to given syntax node
C SXBSID if  return identifier basic symbol code
C SXISID lf  is string an identifier?
C SXISIN lf  is string an integer?
C SXISRE lf  is string a real?
C SXISRW lf  is string a reserved word?
C SXDAT2 o   data structure for numerical data
C SXDAT2 o   data structure for character data
C
C Uses separately compiled packages:
C
C LEXANA p  lexical line analyser; find words and delimiters
C SNCNVT p  conversion of strings from/to integer/real/fix values
C
C This package follows to a large degree the concepts for
C language definition and parsing described in chapter 5 of
C "Algorithms + Data Structures = Programs" by Niklaus Wirth (1976).
C However, the terminology is slightly different, and the meta-syntax
C notation is different from BNF (Backus-Naur Form).
C
C The file SYNTAX.INC, which contains the data structure used by the
C procedures, must be included in the procedures (using VAX FORTRAN
C INCLUDE, editor or utility program) before compiling.
C
C The procedure SXISID relies on the ASCII character code.
C
C Meta-syntax:
C
C  sym             symbol (= non-terminal symbol)
C  "tok"           token (= terminal symbol)
C  sym1=sym2;      production
C  sym1 sym2       sequence
C  {sym}           repeat (0 or more times)
C  <sym1 sym2>     choice
C  [sym]           option
C  sym.x           basic symbol
C
C Separators in meta-syntax are blank, tab and newline. Delimiters
C are brackets ([]), braces ({}), less-than (<), greater-than (>),
C equals (=) and semicolon (;). String delimiter is double-quote ("),
C and comment character is exclamation-mark (!).
C
C Basic symbol declarations (informal definitions):
C
C .i  integer    = [<"+" "-">] digit {digit};
C .r  real       = integer "." {digit} [ <"E" "e"> integer ];
C .d  identifier = letter { <letter digit "_" "."> };
C .w  word       = string of non-special characters
C .s  string     = string of characters enclosed by string delimiters
C
C The basic symbol declaration is part of the symbol identifier.
C
C Note that if the check for reserved words is not used, then the use
C of basic symbols may give syntax definitions that violate rules 1 or 2
C without the check procedure SXSCHK being able to detect this.
C The basic symbols identifier and word are especially dangerous.
C The syntax should be defined so that at forks in the syntax
C graph the alternatives are tested in the following order:
C token, symbol, integer, real, string, identifier, word.
C
C The syntax definition must be started by calling the procedure SXSINI
C first. This initializes the internal data structure, and defines the
C special character sets in the language: separator, delimiter, string
C delimiter and comment start characters. The character sets must be
C disjunct. An empty set is denoted by the string 'NONE'. Newline is
C always considered a separator, and is also comment end character.
C Note that the definition of the characters "+" and "-" as special
C characters will complicate recognition of reals and integers.
C
C
C-------------------------------------------------------------
      SUBROUTINE SXSINI (SEPAR, DELIM, STRING, COMMNT, ERRCOD)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      CHARACTER*(*) SEPAR, DELIM, STRING, COMMNT
      INTEGER       ERRCOD
C
C SEPAR   (In)  separator character set
C DELIM   (In)  delimiter character set
C STRING  (In)  string delimiter character set
C COMMNT  (In)  comment start character set
C ERRCOD  (Out) error code
C
C Help variable
C
      INTEGER I, J, K
C
C Special character sets
C
      CHRSET (1) = SEPAR
      CHRSET (2) = DELIM
      CHRSET (3) = STRING
      CHRSET (4) = COMMNT
C
      IF (SEPAR .EQ. 'NONE') THEN
        CHRTOT (1) = 0
      ELSE
        CHRTOT (1) = LEN (SEPAR)
      END IF
      IF (DELIM .EQ. 'NONE') THEN
        CHRTOT (2) = 0
      ELSE
        CHRTOT (2) = LEN (DELIM)
      END IF
      IF (STRING .EQ. 'NONE') THEN
        CHRTOT (3) = 0
      ELSE
        CHRTOT (3) = LEN (STRING)
      END IF
      IF (COMMNT .EQ. 'NONE') THEN
        CHRTOT (4) = 0
      ELSE
        CHRTOT (4) = LEN (COMMNT)
      END IF
C
C Check non-empty separator and delimiter sets
C
      IF (CHRTOT (1) .EQ. 0  .AND.  CHRTOT (2) .EQ. 0) THEN
        ERRCOD = 2
        RETURN
      END IF
C
C Check character set sizes and disjunction
C
      DO 100 I = 1, 4
C
        IF (CHRTOT (I) .GT. 20) THEN
          ERRCOD = 3
          RETURN
C
        ELSE IF (CHRTOT (I) .GT. 0) THEN
C
          DO 200 J = I+1, 4
            DO 200 K = 1, CHRTOT (J)
              IF (INDEX (CHRSET (I) (: CHRTOT (I)),
     $                   CHRSET (J) (K:K)) .NE. 0) THEN
                ERRCOD = 4
                RETURN
              END IF
200       CONTINUE
C
        END IF
C
100   CONTINUE
C
C Reset counters, pointers and flags
C
      SYMTOT = 0
      NODTOT = 0
      REPNOD = 0
      OPTNOD = 0
      CHONOD = 0
      FIXREP = 0
      FIXOPT = 0
      FIXCHO = 0
      INPRO = .FALSE.
      INREP = .FALSE.
      INOPT = .FALSE.
      INCHO = .FALSE.
      ERRPTR = 0
C
C Initialize meta-syntax lexical analyzer
C
      CALL LXINIT (' ' // CHAR (9), '=;{}[]<>', '"', '!')
C
      ERRCOD = 0
      RETURN
      END
C
C
C-------------------------------------
      SUBROUTINE SXPROD (PROD, ERRCOD)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      CHARACTER*(*) PROD
      INTEGER       ERRCOD
C
C PROD    (In)  line of syntax definition code
C ERRCOD  (Out) error code
C
C Help variables
C
      INTEGER LINLEN
C
C Init line analysis
C
      ITMPOS (2) = 0
      LINLEN = LEN (PROD)
C
C Get next item from line
C
100   CALL LXNEXT (PROD, ITMPOS, ITMTYP, LINLEN, ERRCOD)
C
C No more in current line
C
      IF (ERRCOD .EQ. 1) THEN
        ERRCOD = 0
        RETURN
C
C String not finished at end-of-line
C
      ELSE IF (ERRCOD .EQ. 2) THEN
        ERRCOD = 1
        RETURN
      END IF
C
C Jump according to meta-syntax lexical type
C
      GOTO (1000, 2000) ITMTYP
C
C Meta-syntax ordinary token
C
C If not already in production, then must be start of one
C
      IF (.NOT. INPRO) THEN
C
        CALL SXPBEG (PROD (ITMPOS(1):ITMPOS(2)), ERRCOD)
        IF (ERRCOD .NE. 0) RETURN
C
C Next item in production line must be "="
C
        CALL LXNEXT (PROD, ITMPOS, ITMTYP, LINLEN, ERRCOD)
        IF (ERRCOD .NE. 0) THEN
          ERRCOD = 9
          RETURN
        ELSE IF (PROD (ITMPOS(1):ITMPOS(2)) .NE. '=') THEN
          ERRCOD = 9
          RETURN
        END IF
C
C Must otherwise be symbol identifier
C
      ELSE
        CALL SXESYM (PROD (ITMPOS(1):ITMPOS(2)), ERRCOD)
        IF (ERRCOD .NE. 0) RETURN
      END IF
C
      GOTO 100
C
C Production line item is a meta-syntax delimiter character
C
C Begin repeat construct
C
 1000 IF (PROD (ITMPOS(1):ITMPOS(2)) .EQ. '{') THEN
        CALL SXRBEG (ERRCOD)
        IF (ERRCOD .NE. 0) RETURN
C
C End repeat construct
C
      ELSE IF (PROD (ITMPOS(1):ITMPOS(2)) .EQ. '}') THEN
        CALL SXREND (ERRCOD)
        IF (ERRCOD .NE. 0) RETURN
C
C Begin option construct
C
      ELSE IF (PROD (ITMPOS(1):ITMPOS(2)) .EQ. '[') THEN
        CALL SXOBEG (ERRCOD)
        IF (ERRCOD .NE. 0) RETURN
C
C End option construct
C
      ELSE IF (PROD (ITMPOS(1):ITMPOS(2)) .EQ. ']') THEN
        CALL SXOEND (ERRCOD)
        IF (ERRCOD .NE. 0) RETURN
C
C Begin choice construct
C
      ELSE IF (PROD (ITMPOS(1):ITMPOS(2)) .EQ. '<') THEN
        CALL SXCBEG (ERRCOD)
        IF (ERRCOD .NE. 0) RETURN
C
C End choice construct
C
      ELSE IF (PROD (ITMPOS(1):ITMPOS(2)) .EQ. '>') THEN
        CALL SXCEND (ERRCOD)
        IF (ERRCOD .NE. 0) RETURN
C
C End of production
C
      ELSE IF (PROD (ITMPOS(1):ITMPOS(2)) .EQ. ';') THEN
        CALL SXPEND (ERRCOD)
        IF (ERRCOD .NE. 0) RETURN
C
C Only "=" left; an attempt to start a production while already in one
C
      ELSE
        ERRCOD = 6
        RETURN
      END IF
C
      GOTO 100
C
C String; meta-syntax literal token; check if empty
C
 2000 IF (ITMPOS (1) + 1 .EQ. ITMPOS (2)) THEN
          ERRCOD = 5
          RETURN
C
      ELSE
        CALL SXETOK (PROD (ITMPOS(1)+1 : ITMPOS(2)-1), ERRCOD)
        IF (ERRCOD .NE. 0) RETURN
      END IF
C
      GOTO 100
C
      END
C
C
C---------------------------------------
      SUBROUTINE SXSCHK (RESRVD, ERRCOD)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      LOGICAL RESRVD
      INTEGER ERRCOD
C
C RESRVD  (In)  switch reserved word check on
C ERRCOD  (Out) error code
C
C Help variables
C
      INTEGER NODE, SYM, TOTLIS, TMP, R2TOT, R2CUR, RSV
      INTEGER R2LIS (MAXSTK)
      LOGICAL NULL
C
C Last production must be finished
C
      IF (INPRO) THEN
        ERRCOD = 15
        RETURN
      END IF
C
C Set pointers from syntax node symbols
C
      DO 100 NODE = 1, NODTOT
        IF (NODTYP (NODE) .NE. 0) GOTO 100
C
        DO 110 SYM = 1, SYMTOT
          IF (NODVAL (NODE) .EQ. SYMVAL (SYM)) THEN
            NODSYM (NODE) = SYM
            GOTO 100
          END IF
110     CONTINUE
C
C If we get here, then a syntax node symbol is undefined
C
        ERRPTR = NODE
        ERRCOD = 17
        RETURN
C
100   CONTINUE
C
C Rule 1; every branch emanating from a fork point in the
C syntax graph must lead toward a distinct symbol
C
      DO 200 NODE = 1, NODTOT
        TOTLIS = 0
        CALL SXALTL (NODE, TOTLIS, NULL, ERRCOD)
        IF (ERRCOD .NE. 0) RETURN
200   CONTINUE
C
C Rule 2; for a syntax graph that has a null path, its following
C symbols must be disjunct from its initial symbols
C
      DO 300 SYM = 1, SYMTOT
C
C Find all initial nodes of production symbol; skip if no null path
C
        TOTLIS = 0
        CALL SXALTL (SYMNOD (SYM), TOTLIS, NULL, ERRCOD)
        IF (.NOT. NULL) GOTO 300
C
C Init the list of symbols, the following nodes of which to check
C
        R2CUR = 1
        R2TOT = 1
        R2LIS (R2CUR) = SYM
C
C Check all following nodes of symbol syntax nodes
C
310     DO 320 NODE = 1, NODTOT
          IF (NODTYP (NODE) .NE. 0) GOTO 320
          IF (NODSYM (NODE) .NE. R2LIS (R2CUR)) GOTO 320
C
C If following nodes not disjunct from initial, then return with error
C
          TMP = TOTLIS
          CALL SXALTL (NODSUC (NODE), TMP, NULL, ERRCOD)
          IF (ERRCOD .EQ. 18) ERRCOD = 19
          IF (ERRCOD .NE. 0) THEN
            CURDEP = SYM
            RETURN
          END IF
C
C If there was a null path, then the following nodes of the production
C symbol that this node was in must be checked against the initial
C
          IF (.NOT. NULL) GOTO 320
C
          DO 330 TMP = SYMTOT, 1, -1
C
C Find the production identifier symbol and put into list
C
            IF (SYMNOD (TMP) .GT. NODE) GOTO 330
C
            IF (R2TOT .GE. MAXSTK) THEN
              ERRCOD = 20
              RETURN
            END IF
            R2TOT = R2TOT + 1
            R2LIS (R2TOT) = TMP
            GOTO 320
C
330       CONTINUE
C
320     CONTINUE
C
C If any more symbols in list to check, then loop back
C
        R2CUR = R2CUR + 1
        IF (R2CUR .LE. R2TOT) GOTO 310
C
300   CONTINUE
C
C If reserved word check is switched on, then prepare ordered list
C
      IF (RESRVD) THEN
        RSVTOT = 0
        DO 400 NODE = 1, NODTOT
C
C Skip if not literal token
C
          IF (NODTYP (NODE) .NE. 2) GOTO 400
C
C Insert literal token in ordered reserved word list
C
          DO 410 RSV = 1, RSVTOT
C
C Try next if not lexically greater than; skip if already in list
C
            IF (NODVAL (NODE) .GT. RSVWRD (RSV)) GOTO 410
            IF (NODVAL (NODE) .EQ. RSVWRD (RSV)) GOTO 400
C
C Too many reserved words
C
            IF (RSVTOT .GE. MAXRSV) THEN
              ERRCOD = 21
              RETURN
            END IF
C
C Insert after having switched others down; skip to next node
C
            DO 420 TMP = RSVTOT, RSV, -1
              RSVWRD (TMP + 1) = RSVWRD (TMP)
420         CONTINUE
            RSVWRD (RSV) = NODVAL (NODE)
            RSVTOT = RSVTOT + 1
            GOTO 400
C
410       CONTINUE
C
C Insert at end of list
C
          RSVTOT = RSVTOT + 1
          RSVWRD (RSVTOT) = NODVAL (NODE)
c
400     CONTINUE
C
C Reserved word check not used
C
      ELSE
        RSVTOT = 0
      END IF
C
      RETURN
      END
C
C
C------------------------------------
      SUBROUTINE SXSERR (MSG, ERRCOD)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      CHARACTER*(*) MSG
      INTEGER       ERRCOD
C
C MSG     (Out) error message
C ERRCOD  (In)  syntax definition error code
C
C Error code message array
C
      CHARACTER*62 ERRMSG (16)
      SAVE ERRMSG
      DATA ERRMSG
     $/'invalid token: too long, or not finished on production line',
     $ 'separator and delimiter character sets cannot both be empty',
     $ 'too many special characters in set',
     $ 'special character sets not disjunct',
     $ 'invalid token; empty',
     $ 'cannot start new production; previous production not finished',
     $ 'no space for production identifier symbol; MAXSYM in SYNTAX',
     $ 'symbol identifier invalid, or already defined',
     $ '"=" missing after production identifier symbol',
     $ 'could not enter item; not in production',
     $ 'could not enter item; no space for node; MAXNOD in SYNTAX',
     $ 'repeat, option or choice cannot be nested within a production',
     $ 'not in repeat, option or choice construct',
     $ 'cannot end production while in repeat, option or choice',
     $ 'check: production not finished',
     $ 'invalid token: contains special characters'/
C
      IF (ERRCOD .LE. 0) THEN
        MSG = ' '
C
      ELSE IF (ERRCOD .LE. 16) THEN
        MSG = ERRMSG (ERRCOD)
C
      ELSE IF (ERRCOD .EQ. 17) THEN
        MSG = 'undefined symbol: ' // NODVAL (ERRPTR)
C
      ELSE IF (ERRCOD .EQ. 18) THEN
        IF (CURDEP .EQ. 0) THEN
          MSG = 'rule 1; P:' // SYMVAL (1) //
     $          ', S: ' // NODVAL (ERRPTR)
        ELSE
          MSG = 'rule 1; P: ' // NODVAL (SYMSTK (CURDEP)) //
     $          ', S: ' // NODVAL (ERRPTR)
        END IF
C
      ELSE IF (ERRCOD .EQ. 19) THEN
        MSG = 'rule 2; P: ' // SYMVAL (CURDEP) //
     $        ', S : ' // NODVAL (ERRPTR)
C
      ELSE IF (ERRCOD .EQ. 20) THEN
        MSG = 'list used for rules check full; MAXSTK in SYNTAX'
C
      ELSE IF (ERRCOD .EQ. 21) THEN
        MSG = 'too many reserved words: MAXRSV in SYNTAX'
      ELSE
        MSG = 'unknown syntax definition error'
      END IF
C
      RETURN
      END
C
C
C-----------------------------
      SUBROUTINE SXEPOS (EPOS)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      INTEGER EPOS (2)
C
C EPOS  (Out) position of erroneous item
C 
      EPOS (1) = ITMPOS (1)
      EPOS (2) = ITMPOS (2)
C
      RETURN
      END
C
C
C---------------------------------------------------
      SUBROUTINE SXMARK (LINE, IPOS, OFFSET, MARKER)
C
      CHARACTER*(*) LINE
      INTEGER       IPOS (2)
      INTEGER       OFFSET
      CHARACTER*1   MARKER
C
C LINE    (Out) line with error marker
C IPOS    (In)  positions to put markers between
C OFFSET  (In)  offset of position (to allow for prompt length)
C MARKER  (In)  marker character
C
      INTEGER I
C
      LINE = ' '
      DO 100 I = IPOS (1) + OFFSET, IPOS (2) + OFFSET
        LINE (I:I) = MARKER
100   CONTINUE
C
      RETURN
      END
C
C
C-------------------------------
      SUBROUTINE SXLINI (ERRCOD)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      INTEGER ERRCOD
C
C ERRCOD  (Out) error code
C
C Help variable
C
      INTEGER I
C
C Check if any syntax defined
C
      IF (SYMTOT .EQ. 0) THEN
        ERRCOD = 1
        RETURN
      END IF
C
C Mark empty character sets
C
      DO 100 I = 1, 4
        IF (CHRSET (I) .EQ. 'NONE') CHRTOT (I) = 4
100   CONTINUE
C
C Init lexical analyzer for language
C
      CALL LXINIT (CHRSET (1) (:CHRTOT (1)), CHRSET (2) (:CHRTOT (2)),
     $             CHRSET (3) (:CHRTOT (3)), CHRSET (4) (:CHRTOT (4)))
C
C Reset empty character sets
C
      DO 200 I = 1, 4
        IF (CHRSET (I) .EQ. 'NONE') CHRTOT (I) = 0
200   CONTINUE
C
C Push start symbol to symbol stack, and set flags
C
      CURDEP = 1
      SYMSTK (CURDEP) = 1
      BCKSTK (CURDEP) = .FALSE.
      CURNOD = 0
C
      ERRCOD = 0
      RETURN
      END
C
C
C---------------------------------------------
      SUBROUTINE SXPARS (ITEM, LXTYPE, ERRCOD)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      CHARACTER*(*) ITEM
      INTEGER       LXTYPE, ERRCOD
C
C ITEM    (In)  language item to parse
C LXTYPE  (In)  language item type from lexical analysis
C ERRCOD  (Out) error code
C
C Externally defined functions
C
      LOGICAL SXISID, SXISIN, SXISRE, SXISRW
C
C Help variables
C
      INTEGER NXTNOD
      LOGICAL MATCH, RSVW
C
C Not reserved word by default
C
      RSVW = .FALSE.
C
C Find the next node; no current node implies being at start of symbol
C
100   IF (CURNOD .EQ. 0) THEN
        NXTNOD = SYMNOD (SYMSTK (CURDEP))
      ELSE
        NXTNOD = NODSUC (CURNOD)
        BCKSTK (CURDEP) = .FALSE.
      END IF
C
C If at the end of symbol production, then pop the stack
C
200   IF (NXTNOD .EQ. -1) THEN
C
C If stack is empty, then item is superfluous
C
        IF (CURDEP .LE. 1) THEN
          ERRCOD = 3
          RETURN
        END IF
        CURDEP = CURDEP - 1
C
C Set symbol syntax node at top of node stack to be current node
C
        CURNOD = NODSTK (CURDEP)
        GOTO 100
C
C No alternative node
C
      ELSE IF (NXTNOD .EQ. 0) THEN
C
C Try to backtrack to an alternative in the symbol stack
C
        IF (BCKSTK (CURDEP)) THEN
          CURDEP = CURDEP - 1
          NXTNOD = NODALT (NODSTK (CURDEP))
          GOTO 200
C
C If backtracking not possible, then illegal item
C
        ELSE IF (RSVW) THEN
          ERRCOD = 5
        ELSE
          ERRCOD = 4
        END IF
        RETURN
C
      END IF
C
C Does item match next node? Test according to expected node type
C
      GOTO (1000, 2000, 3000, 4000, 5000, 6000, 7000) NODTYP (NXTNOD)
C
C Node type 0: node is a symbol; save node pointer
C
      IF (CURDEP .GE. MAXSTK) THEN
        ERRCOD = 2
        RETURN
      END IF
      NODSTK (CURDEP) = NXTNOD
C
C Push new symbol to stack; at start of symbol; loop back for next node
C
      CURDEP = CURDEP + 1
      SYMSTK (CURDEP) = NODSYM (NXTNOD)
      BCKSTK (CURDEP) = .TRUE.
      CURNOD = 0
      GOTO 100
C
C Delimiter token
C
 1000 MATCH = LXTYPE .EQ. LXDEL  .AND.  ITEM .EQ. NODVAL (NXTNOD) (1:1)
      GOTO 8000
C
C Literal token
C
 2000 MATCH = LXTYPE .EQ. LXTOK  .AND.  ITEM .EQ. NODVAL (NXTNOD)
      GOTO 8000
C
C Integer value
C
 3000 MATCH = LXTYPE .EQ. LXTOK  .AND.  SXISIN (ITEM)
      GOTO 8000
C
C Real value
C
 4000 MATCH = LXTYPE .EQ. LXTOK  .AND.  SXISRE (ITEM)
      GOTO 8000
C
C Identifier; check reserved words
C
 5000 RSVW = SXISRW (ITEM)
      MATCH = LXTYPE .EQ. LXTOK  .AND. SXISID (ITEM)  .AND. .NOT. RSVW
      GOTO 8000
C
C Word; anything matches except delimiter, string and reserved word
C
 6000 RSVW = SXISRW (ITEM)
      MATCH = LXTYPE .EQ. LXTOK  .AND.  .NOT. RSVW
      GOTO 8000
C
C String value token
C
 7000 MATCH = LXTYPE .EQ. LXSTR
C
C If item matched, then set current node pointer and return
C
8000  IF (MATCH) THEN
        CURNOD = NXTNOD
        ERRCOD = 0
        RETURN
C
C If no match, then set alternative node and loop back for new test
C
      ELSE
        NXTNOD = NODALT (NXTNOD)
        GOTO 200
      END IF
      END
C
C
C------------------------------------
      CHARACTER*20 FUNCTION SXITEM ()
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
C (Return)  current syntax node identifier
C
      IF (CURNOD .EQ. 0) THEN
        SXITEM = ' '
      ELSE
        SXITEM = NODVAL (CURNOD)
      END IF
      RETURN
      END
C
C
C-------------------------------
      INTEGER FUNCTION SXTYPE ()
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
C (Return)  0 = no current item
C           1 = delimiter token
C           2 = literal token
C           3 = integer
C           4 = real
C           5 = identifier
C           6 = word
C           7 = string
C
      IF (CURNOD .EQ. 0) THEN
        SXTYPE = 0
      ELSE
        SXTYPE = NODTYP (CURNOD)
      END IF
      RETURN
      END
C
C
C------------------------------
      INTEGER FUNCTION SXINT ()
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      SXINT = CURINT
      RETURN
      END
C
C
C----------------------------
      REAL FUNCTION SXREAL ()
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      SXREAL = CURREL
      RETURN
      END
C
C
C----------------------------------
      INTEGER FUNCTION SXDPTH (POP)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      LOGICAL POP
C
C POP  (In)  return current depth as is, or after having popped
C            as much as possible from current symbol stack
C
C Help variables
C
      INTEGER DEPTH, NODE1, NODE2
C
C Pop symbol stack as far as possible
C
      IF (POP) THEN
        DEPTH = CURDEP
        NODE1 = CURNOD
C
C Stack empty
C
        IF (DEPTH .EQ. 0) THEN
          SXDPTH = DEPTH
          RETURN
        END IF
C
C If successor of current node is end-of-production, then pop stack
C
100     NODE2 = NODSUC (NODE1)
C
200     IF (NODE2 .EQ. -1) THEN
          DEPTH = DEPTH - 1
          IF (DEPTH .EQ. 0) THEN
            SXDPTH = DEPTH
            RETURN
          END IF
          NODE1 = NODSTK (DEPTH)
          GOTO 100
C
C Loop through all alternatives to successor node
C
        ELSE IF (NODE2 .NE. 0) THEN
          NODE2 = NODALT (NODE2)
          GOTO 200
C
        ELSE
          SXDPTH = DEPTH
          RETURN
        END IF
C
C Return current depth as is
C
      ELSE
        SXDPTH = CURDEP
        RETURN
      END IF
      END
C
C
C-----------------------------------------
      CHARACTER*20 FUNCTION SXPSYM (STACK)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      INTEGER STACK
C
C STACK  (In)  number of symbol stack slot
C
      IF (STACK .LT. 1  .OR.  STACK .GT. CURDEP) THEN
        SXPSYM = ' '
      ELSE
        SXPSYM = SYMVAL (SYMSTK (STACK))
      END IF
      RETURN
      END
C
C
C------------------------------------
      SUBROUTINE SXLERR (MSG, ERRCOD)
C
      CHARACTER*(*) MSG
      INTEGER       ERRCOD
C
C MSG     (Out) error message
C ERRCOD  (In)  error code
C
C Error code message array
C
      CHARACTER*50 ERRMSG (5)
      SAVE ERRMSG
      DATA ERRMSG
     $/'no syntax defined for language',
     $ 'too complicated language construct; MAXSTK in SYNTAX',
     $ 'superfluous item',
     $ 'item does not match syntax',
     $ 'identifier or word is reserved word'/
C
      IF (ERRCOD .LE. 0) THEN
        MSG = ' '
C
      ELSE IF (ERRCOD .LE. 5) THEN
        MSG = ERRMSG (ERRCOD)
C
      ELSE
        MSG = 'unknown language syntax error'
      END IF
C
      RETURN
      END
C
C
C---------------------------------------
      SUBROUTINE SXPBEG (PRODID, ERRCOD)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      CHARACTER*(*) PRODID
      INTEGER       ERRCOD
C
C PRODID  (In)  production identifier
C ERRCOD  (Out) error code
C
C Externally defined functions
C
      LOGICAL SXISID
      INTEGER SXBSID
C
C Help variable
C
      INTEGER I
C
C Already in production
C
      IF (INPRO) THEN
        ERRCOD = 6
        RETURN
C
C Too many symbols
C
      ELSE IF (SYMTOT .GE. MAXSYM) THEN
        ERRCOD = 7
        RETURN
C
C Check that production identifier symbol really is identifier
C
      ELSE IF (.NOT. SXISID (PRODID)) THEN
        ERRCOD = 8
        RETURN
C
C Check that production identifier symbol is not a basic symbol
C
      ELSE IF (SXBSID (PRODID) .NE. 0) THEN
        ERRCOD = 8
        RETURN
      END IF
C
C Or that it has been already defined
C
      DO 100 I = 1, SYMTOT
        IF (SYMVAL (I) .EQ. PRODID) THEN
          ERRCOD = 8
          RETURN
        END IF
100   CONTINUE
C
C Enter symbol into table, set production flag and syntax node pointer
C
      SYMTOT = SYMTOT + 1
      SYMVAL (SYMTOT) = PRODID
      INPRO = .TRUE.
      CURNOD = 0
C
      ERRCOD = 0
      RETURN
      END
C
C
C-------------------------------
      SUBROUTINE SXPEND (ERRCOD)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      INTEGER ERRCOD
C
C ERRCOD  (Out) error code
C
C Not in production
C
      IF (.NOT. INPRO) THEN
        ERRCOD = 10
C
C Unfinished construct
C
      ELSE IF (INREP .OR. INOPT .OR. INCHO) THEN
        ERRCOD = 14
C
C Set end pointer
C
      ELSE
        CALL SXPSET (-1)
        INPRO = .FALSE.
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C---------------------------------------
      SUBROUTINE SXESYM (SYMBOL, ERRCOD)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      CHARACTER*(*) SYMBOL
      INTEGER       ERRCOD
C
C SYMBOL  (In)  symbol to enter in syntax node
C ERRCOD  (Out) error code
C
C Externally defined functions
C
      LOGICAL SXISID
      INTEGER SXBSID
C
C Help variable
C
      INTEGER BASIC
C
C Too many syntax nodes
C
      IF (NODTOT .GE. MAXNOD) THEN
        ERRCOD = 11
        RETURN
C
C Check that symbol identifier really is identifier
C
      ELSE IF (.NOT. SXISID (SYMBOL)) THEN
        ERRCOD = 8
        RETURN
      END IF
C
C Put symbol in syntax node; get basic symbol code, if any
C
      NODTOT = NODTOT + 1
      NODVAL (NODTOT) = SYMBOL
      NODTYP (NODTOT) = SXBSID (SYMBOL)
C
C Set and update pointers
C
      CALL SXPSET (NODTOT)
      ERRCOD = 0
      RETURN
      END
C
C
C--------------------------------------
      SUBROUTINE SXETOK (TOKEN, ERRCOD)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      CHARACTER*(*) TOKEN
      INTEGER       ERRCOD
C
C TOKEN   (In)  token to enter in syntax node
C ERRCOD  (Out) error code
C
C Help variables
C
      INTEGER I, J
      LOGICAL DELIMF
C
C Not in production
C
      IF (.NOT. INPRO) THEN
        ERRCOD = 10
        RETURN
C
C Token too long
C
      ELSE IF (LEN (TOKEN) .GT. LEN (NODVAL (1))) THEN
        ERRCOD = 1
        RETURN
C
C Too many syntax nodes
C
      ELSE IF (NODTOT .GE. MAXNOD) THEN
        ERRCOD = 11
        RETURN
      END IF
C
C Check presence of special characters
C
      DELIMF = .FALSE.
      DO 100 I = 1, 4
        DO 100 J = 1, CHRTOT (I)
          IF (INDEX (TOKEN, CHRSET (I) (J:J)) .NE. 0) THEN
C
C One and only one delimiter character allowed
C
            DELIMF = I .EQ. 2  .AND.  LEN (TOKEN) .EQ. 1
            IF (.NOT. DELIMF) THEN
              ERRCOD = 16
              RETURN
            END IF
          END IF
100   CONTINUE
C
C Put token in syntax node
C
      NODTOT = NODTOT + 1
      NODVAL (NODTOT) = TOKEN
      IF (DELIMF) THEN
        NODTYP (NODTOT) = 1
      ELSE
        NODTYP (NODTOT) = 2
      END IF
C
C Set and update pointers
C
      CALL SXPSET (NODTOT)
      ERRCOD = 0
      RETURN
      END
C
C
C-------------------------------
      SUBROUTINE SXRBEG (ERRCOD)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      INTEGER ERRCOD
C
C ERRCOD  (Out) error code
C
C Not in production
C
      IF (.NOT. INPRO) THEN
        ERRCOD = 10
C
C Already in construct
C
      ELSE IF (INREP .OR. INOPT .OR. INCHO) THEN
        ERRCOD = 12
C
      ELSE
        INREP = .TRUE.
        REPNOD = 0
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C-------------------------------
      SUBROUTINE SXREND (ERRCOD)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      INTEGER ERRCOD
C
C ERRCOD  (Out) error code
C
C Not in production
C
      IF (.NOT. INPRO) THEN
        ERRCOD = 10
C
C Not in construct
C
      ELSE IF (.NOT. INREP) THEN
        ERRCOD = 13
C
      ELSE
        FIXREP = REPNOD
        REPNOD = 0
        INREP = .FALSE.
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C-------------------------------
      SUBROUTINE SXOBEG (ERRCOD)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      INTEGER ERRCOD
C
C ERRCOD  (Out) error code
C
C Not in production
C
      IF (.NOT. INPRO) THEN
        ERRCOD = 10
C
C Already in construct
C
      ELSE IF (INREP .OR. INOPT .OR. INCHO) THEN
        ERRCOD = 12
C
      ELSE
        INOPT = .TRUE.
        OPTNOD = 0
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C-------------------------------
      SUBROUTINE SXOEND (ERRCOD)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      INTEGER ERRCOD
C
C ERRCOD  (Out) error code
C
C Not in production
C
      IF (.NOT. INPRO) THEN
        ERRCOD = 10
C
C Not in construct
C
      ELSE IF (.NOT. INOPT) THEN
        ERRCOD = 13
C
      ELSE
        FIXOPT = OPTNOD
        OPTNOD = 0
        INOPT = .FALSE.
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C-------------------------------
      SUBROUTINE SXCBEG (ERRCOD)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      INTEGER ERRCOD
C
C ERRCOD  (Out) error code
C
C Not in production
C
      IF (.NOT. INPRO) THEN
        ERRCOD = 10
C
C Already in construct
C
      ELSE IF (INREP .OR. INOPT .OR. INCHO) THEN
        ERRCOD = 12
C
      ELSE
        INCHO = .TRUE.
        CHONOD = 0
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C-------------------------------
      SUBROUTINE SXCEND (ERRCOD)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      INTEGER ERRCOD
C
C ERRCOD  (Out) error code
C
C Not in production
C
      IF (.NOT. INPRO) THEN
        ERRCOD = 10
C
C Not in construct
C
      ELSE IF (.NOT. INCHO) THEN
        ERRCOD = 13
C
      ELSE
        FIXCHO = CHONOD
        CHONOD = 0
        INCHO = .FALSE.
        ERRCOD = 0
      END IF
      RETURN
      END
C
C
C-----------------------------
      SUBROUTINE SXPSET (NODE)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      INTEGER NODE
C
C NODE  (In) pointer to new syntax node
C
C Set new syntax node pointers to null
C
      IF (NODE .GT. 0) NODSUC (NODE) = 0
      IF (NODE .GT. 0) NODALT (NODE) = 0
C
C Set pointer from current node to new node
C
      IF (INREP) THEN
        IF (CURNOD .EQ. 0) THEN
          SYMNOD (SYMTOT) = NODE
          REPNOD = NODE
        ELSE IF (REPNOD .EQ. 0) THEN
          CALL SXPFIX (NODE)
          REPNOD = NODE
        ELSE
          NODSUC (CURNOD) = NODE
        END IF
C
      ELSE IF (INOPT) THEN
        IF (CURNOD .EQ. 0) THEN
          SYMNOD (SYMTOT) = NODE
          OPTNOD = NODE
        ELSE IF (OPTNOD .EQ. 0) THEN
          CALL SXPFIX (NODE)
          OPTNOD = NODE
        ELSE
          NODSUC (CURNOD) = NODE
        END IF
C
      ELSE IF (INCHO) THEN
        IF (CURNOD .EQ. 0) THEN
          SYMNOD (SYMTOT) = NODE
          CHONOD = NODE
        ELSE IF (CHONOD .EQ. 0) THEN
          CALL SXPFIX (NODE)
          CHONOD = NODE
        ELSE
          NODALT (CURNOD) = NODE
        END IF
C
      ELSE
        IF (CURNOD .EQ. 0) THEN
          SYMNOD (SYMTOT) = NODE
        ELSE
          CALL SXPFIX (NODE)
        END IF
      END IF
C
C Update current node
C
      CURNOD = NODE
      RETURN
      END
C
C
C-----------------------------
      SUBROUTINE SXPFIX (NODE)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      INTEGER NODE
C
C NODE  (In) pointer to new syntax node
C
C Fix pointers from nodes to new node according to previous construct
C
      IF (FIXREP .NE. 0) THEN
        NODSUC (CURNOD) = FIXREP
        NODALT (FIXREP) = NODE
        FIXREP = 0
C
      ELSE IF (FIXOPT .NE. 0) THEN
        NODALT (FIXOPT) = NODE
        NODSUC (CURNOD) = NODE
        FIXOPT = 0
C
      ELSE IF (FIXCHO .NE. 0) THEN
100     NODSUC (FIXCHO) = NODE
        FIXCHO = NODALT (FIXCHO)
        IF (FIXCHO .NE. 0) GOTO 100
C
      ELSE
        NODSUC (CURNOD) = NODE
      END IF
C
      RETURN
      END
C
C
C---------------------------------------------------
      SUBROUTINE SXALTL (NODE, TOTLIS, NULL, ERRCOD)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      INTEGER NODE, TOTLIS, ERRCOD
      LOGICAL NULL
C
C NODE    (In)  syntax node alternatives of which to list
C TOTLIS  (InOut) total number of pointers in list
C NULL    (Out) a null path from the syntax node has been found
C ERRCOD  (Out) error code
C
C Help variables
C
      INTEGER I, NODE2
C
C Init
C
      CURDEP = 0
      ERRCOD = 0
      NODE2 = NODE
      NULL = .FALSE.
C
C If no next alternative, then pop stack to get it
C
100   IF (NODE2 .LE. 0) THEN
C
C Set flag if null path found
C
        IF (NODE2 .EQ. -1) NULL = .TRUE.
C
C If stack empty, then all alternatives found; return
C
        IF (CURDEP .LE. 0) RETURN
C
        NODE2 = NODALT (SYMSTK (CURDEP))
        CURDEP = CURDEP - 1
        GOTO 100
      END IF
C
C Check through list of alternatives found so far; compare literal
C only with literal (delimiter and literal tokens are never equal)
C
      DO 110 I = 1, TOTLIS
        IF (NODVAL (NODE2) .NE. NODVAL (NODSTK (I))) GOTO 110
        IF (NODTYP(NODE2).EQ.1 .AND. NODTYP(NODSTK(I)).NE.1) GOTO 110
        IF (NODTYP(NODE2).EQ.2 .AND. NODTYP(NODSTK(I)).NE.2) GOTO 110
        ERRPTR = NODE2
        ERRCOD = 18
        RETURN
110   CONTINUE
C
C Add this node to list
C
      IF (TOTLIS .GE. MAXSTK) THEN
        ERRCOD = 20
        RETURN
      END IF
      TOTLIS = TOTLIS + 1
      NODSTK (TOTLIS) = NODE2
C
C If this node is a symbol, push onto stack and go to its first node
C
      IF (NODTYP (NODE2) .EQ. 0) THEN
        IF (CURDEP .GE. MAXSTK) THEN
          ERRCOD = 20
          RETURN
        END IF
        CURDEP = CURDEP + 1
        SYMSTK (CURDEP) = NODE2
        NODE2 = SYMNOD (NODSYM (NODE2))
C
C Just go to next alternative node
C
      ELSE
        NODE2 = NODALT (NODE2)
      END IF
C
      GOTO 100
C
      END
C
C
C---------------------------------
      INTEGER FUNCTION SXBSID (ID)
C
      CHARACTER*(*) ID
C
C ID   (In) symbol identifier
C (return)  basic symbol code
C       0 = not basic symbol
C       3 = integer basic symbol
C       4 = real basic symbol
C       5 = identifier basic symbol
C       6 = word basic symbol
C       7 = string basic symbol
C               
      INTEGER FIRST
C
      FIRST = LEN (ID) - 1
      IF (FIRST .LT. 1) THEN
        SXBSID = 0
      ELSE IF (ID (FIRST :) .EQ. '.i') THEN
        SXBSID = 3
      ELSE IF (ID (FIRST :) .EQ. '.r') THEN
        SXBSID = 4
      ELSE IF (ID (FIRST :) .EQ. '.d') THEN
        SXBSID = 5
      ELSE IF (ID (FIRST :) .EQ. '.w') THEN
        SXBSID = 6
      ELSE IF (ID (FIRST :) .EQ. '.s') THEN
        SXBSID = 7
      ELSE
        SXBSID = 0
      END IF
      RETURN
      END
C
C
C----------------------------------
      LOGICAL FUNCTION SXISID (STR)
C
      CHARACTER*(*) STR
C
C STR    (In) string to check if identifier
C
      INTEGER I, ASCII
C
C This procedure relies on the ASCII character code.
C
C Table of reject values for (most of) the range of printable characters
C
      LOGICAL REJECT (46:122)
      SAVE    REJECT
      DATA    REJECT /.FALSE., .TRUE., 10*.FALSE., 7*.TRUE.,
     $                26*.FALSE., 4*.TRUE., .FALSE., .TRUE., 26*.FALSE./
C
C First character must be alphabetical
C
      ASCII = ICHAR (STR (1:1))
      IF (ASCII.LT.65 .OR. ASCII.GT.122) GOTO 900
      IF (REJECT (ASCII)) GOTO 900
C
C Following characters may be alphanumerical, underscore or dot
C
      DO 100 I = 2, LEN (STR)
        ASCII = ICHAR (STR (I:I))
        IF (ASCII.LT.46 .OR. ASCII.GT.122) GOTO 900
        IF (REJECT (ASCII)) GOTO 900
100   CONTINUE
C
      SXISID = .TRUE.
      RETURN
C
900   SXISID = .FALSE.
      RETURN
      END
C
C
C----------------------------------
      LOGICAL FUNCTION SXISIN (STR)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      CHARACTER*(*) STR
C
C STR    (In)  character string to check and convert
C
      INTEGER ERRCOD
C
      IF (STR .EQ. '-'  .OR.  STR .EQ. '+') THEN
        SXISIN = .FALSE.
      ELSE
        CALL SCHTOI (CURINT, STR, ERRCOD)
        SXISIN = ERRCOD .EQ. 0
      END IF
      RETURN
      END
C
C
C----------------------------------
      LOGICAL FUNCTION SXISRE (STR)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      CHARACTER*(*) STR
C
C STR    (In)  character string to check and convert
C
      INTEGER ERRCOD
C
      IF (STR .EQ. '-'  .OR.  STR .EQ. '+') THEN
        SXISRE = .FALSE.
      ELSE IF (INDEX (STR, '.') .EQ. 0) THEN
        SXISRE = .FALSE.
      ELSE
        CALL SCHTOR (CURREL, STR, ERRCOD)
        SXISRE = ERRCOD .EQ. 0
      END IF
      RETURN
      END
C
C
C-----------------------------------
      LOGICAL FUNCTION SXISRW (ITEM)
C
C Package SYNTAX
C
C Copyright (C) 1988, 1990 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C   15-Jun-1988  first attempts
C    8-May-1989  modified for error message procedure
C   25-May-1989  reserved word check introduced
C   12-Jan-1990  changes following LEXANA rewrite
C
C MAXSYM  max number of symbols
C MAXNOD  max number of syntax nodes
C MAXSTK  max depth of symbol stack
C MAXRSV  max number of reserved words
C
      INTEGER    MAXSYM, MAXNOD, MAXSTK, MAXRSV
      PARAMETER (MAXSYM = 100, MAXNOD = 300, MAXSTK = 100, MAXRSV = 100)
C
C LXTOK   lexical analysis ordinary token
C LXDEL   lexical analysis delimiter value
C LXSTR   lexical analysis string value
C
      INTEGER    LXTOK, LXDEL, LXSTR
      PARAMETER (LXTOK = 0, LXDEL = 1, LXSTR = 2)
C
C SYMTOT  number of symbols in table
C SYMVAL  table of symbols
C SYMNOD  pointer from symbol to first syntax node
C
      INTEGER      SYMTOT
      CHARACTER*20 SYMVAL (MAXSYM)
      INTEGER      SYMNOD (MAXSYM)
C
C NODTOT  number of syntax nodes
C NODVAL  syntax node value
C NODTYP  type of syntax node value
C NODSYM  pointer from node to symbol or token slot
C NODALT  pointer to alternative node
C NODSUC  pointer to successor node
C
      CHARACTER*20 NODVAL (MAXNOD)
      INTEGER      NODTOT
      INTEGER      NODTYP (MAXNOD), NODSYM (MAXNOD), NODALT (MAXNOD),
     $             NODSUC (MAXNOD)
C
C CHRSET  separator, delimiter, string delimiter, comment character sets
C CHRTOT  number of characters in sets
C
      CHARACTER*20 CHRSET (4)
      INTEGER      CHRTOT (4)
C
C RSVTOT  number of reserved words
C RSVWRD  reserved words
C
      INTEGER      RSVTOT
      CHARACTER*20 RSVWRD (MAXRSV)
C
C CURNOD  current node
C INPRO   currently in production definition
C INREP   currently in repeat construct
C INOPT   currently in option construct
C INCHO   currently in choice construct
C REPNOD  first syntax node in repeat construct
C OPTNOD  first syntax node in option construct
C CHONOD  first syntax node in choice construct
C FIXREP  first syntax node to fix repeat pointers with next node
C FIXOPT  first syntax node to fix option pointers with next node
C FIXCHO  first syntax node to fix choice pointers with next node
C
      LOGICAL INPRO, INREP, INOPT, INCHO
      INTEGER CURNOD, REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO
C
C ITMPOS  production line item position
C ITMTYP  production line item type
C SYMSTK  production stack; pointers to active symbol productions
C NODSTK  node stack; pointers to active syntax nodes
C BCKSTK  backtracking flag stack; searching for match among alternatives
C CURDEP  current depth of stack
C CURINT  integer value of current item
C CURREL  real value of current item
C ERRPTR  pointer to erroneous production identifier symbol
C
      INTEGER ITMPOS (2), SYMSTK (MAXSTK), NODSTK (MAXSTK)
      LOGICAL BCKSTK (MAXSTK)
      INTEGER ITMTYP, CURDEP, CURINT, ERRPTR
      REAL    CURREL
C
      COMMON /SXDAT1/ SYMTOT, SYMNOD, NODTOT, NODTYP, NODSYM,
     $                NODALT, NODSUC, CHRTOT,
     $                CURNOD, INPRO, INREP, INOPT, INCHO,
     $                REPNOD, OPTNOD, CHONOD, FIXREP, FIXOPT, FIXCHO,
     $                ITMPOS, SYMSTK, NODSTK, BCKSTK, ITMTYP, CURDEP,
     $                CURINT, CURREL, ERRPTR, RSVTOT
      COMMON /SXDAT2/ SYMVAL, NODVAL, CHRSET, RSVWRD
      SAVE   /SXDAT1/, /SXDAT2/
C
      CHARACTER*(*) ITEM
C
C ITEM  (In)  item to check if reserved word
C
C Help variables
C
      INTEGER LO, HI, MIDDLE
C
C Default: not reserved word
C
      SXISRW = .FALSE.
C
C Binary search in reserved word list
C
      IF (RSVTOT .GT. 0) THEN
        LO = 1
        HI = RSVTOT
C
C Simulated repeat-until loop
C
100     MIDDLE = (LO + HI) / 2
C
C If match in middle of current list interval, then reserved word
C
          IF (ITEM .EQ. RSVWRD (MIDDLE)) THEN
            SXISRW = .TRUE.
            RETURN
C
C Set new interval limits
C
          ELSE IF (ITEM .GT. RSVWRD (MIDDLE)) THEN
            LO = MIDDLE + 1
          ELSE
            HI = MIDDLE - 1
          END IF
C
C Loop back if interval still non-zero
C
        IF (HI .GE. LO) GOTO 100
C
      END IF
C
      RETURN
      END
C Package TTYIOP
C
C Terminal input/output procedures
C
C Some of these procedures use the '$' format descriptor
C
C TTYGTL s  output prompt and get line
C TTYGTI s  output prompt and get integer value 
C TTYGTR s  output prompt and get real value
C TTYCR  s  output N empty lines
C TTYPTL s  output line
C TTYPTI s  output line and integer value
C TTYPTF s  output line and fix real value
C TTYPTR s  output line and floating real value
C
C Uses no separately compiled procedures.
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden
C  16-Sep-1987  written
C   7-Oct-1987  modified to handle empty input line properly
C
C
C---------------------------------------------
      SUBROUTINE TTYGTL (PROMPT, LINE, ERRCOD)
C
      CHARACTER*(*) PROMPT, LINE
      INTEGER       ERRCOD
C
C ERRCOD  (Out) error code
C     0 = no error
C     1 = could not read line
C     2 = end-of-file encountered
C
      WRITE (6, 10) PROMPT
10    FORMAT (' ',A,$)
      READ (5, 20, ERR=90, END=95) LINE
20    FORMAT (A)
      ERRCOD = 0
      RETURN
C
90    ERRCOD = 1
      RETURN
95    ERRCOD = 2
      RETURN
      END
C
C
C---------------------------------------------
      SUBROUTINE TTYGTI (PROMPT, IVAL, ERRCOD)
C
      CHARACTER*(*) PROMPT
      INTEGER       IVAL, ERRCOD
C
C ERRCOD  (Out) error code
C     0 = no error
C     1 = could not read integer value
C     2 = end-of-file encountered
C
      CHARACTER*80 INLINE
C
      WRITE (6, 10) PROMPT
10    FORMAT (' ',A,$)
      READ (5, 20, ERR=90, END=95) INLINE
20    FORMAT (A)
      IF (INLINE.EQ.' ') GOTO 90
      READ (INLINE, 30, ERR=90) IVAL
30    FORMAT (BN, I80)
      ERRCOD = 0
      RETURN
C
90    ERRCOD = 1
      RETURN
95    ERRCOD =2
      RETURN
      END
C
C
C---------------------------------------------
      SUBROUTINE TTYGTR (PROMPT, RVAL, ERRCOD)
C
      CHARACTER*(*) PROMPT
      REAL          RVAL
      INTEGER       ERRCOD
C
C ERRCOD  (Out) error code
C     0 = no error
C     1 = could not read real value
C     2 = end-of-file encountered
C
      CHARACTER*80 INLINE
C
      WRITE (6, 10) PROMPT
10    FORMAT (' ',A,$)
      READ (5, 20, ERR=90, END=95) INLINE
20    FORMAT (A)
      IF (INLINE.EQ.' ') GOTO 90
      READ (INLINE, 30, ERR=90) RVAL
30    FORMAT (BN, F80.0)
      ERRCOD = 0
      RETURN
C
90    ERRCOD = 1
      RETURN
95    ERRCOD = 2
      RETURN
      END
C
C
C-------------------------
      SUBROUTINE TTYCR (N)
C
      INTEGER N
C
      INTEGER I
C
      DO 10 I = 1, N
        WRITE (6, *)
10    CONTINUE
      RETURN
      END
C
C
C-----------------------------
      SUBROUTINE TTYPTL (LINE)
C
      CHARACTER*(*) LINE
C
      WRITE (6, 10) LINE
10    FORMAT (' ',A)
      RETURN
      END
C
C
C------------------------------------------------
      SUBROUTINE TTYPTI (LINE, IVAL, POS, ERRCOD)
C
      CHARACTER*(*) LINE
      INTEGER       IVAL, POS, ERRCOD
C
C LINE    (In) line to output
C IVAL    (In) integer value to output
C POS     (In) number of positions to use for integer value
C ERRCOD  (Out) error code
C     0 = no error
C     1 = integer value position overflow
C
      CHARACTER*11 FMTLIN
      SAVE FMTLIN
      DATA FMTLIN /'('' '',A,I  )'/
C
      WRITE (FMTLIN (9:10), 10, ERR=90) POS
10    FORMAT (I2)
      WRITE (6, FMTLIN, ERR=90) LINE, IVAL
      ERRCOD = 0
      RETURN
C
90    ERRCOD = 1
      RETURN
      END
C
C
C-----------------------------------------------------
      SUBROUTINE TTYPTF (LINE, FVAL, POS, DEC, ERRCOD)
C
      CHARACTER*(*) LINE
      REAL          FVAL
      INTEGER       POS, DEC, ERRCOD
C
C LINE    (In) line to output
C FVAL    (In) fix real value to output
C POS     (In) number of positions to use for fix real value
C DEC     (In) number of decimals to output
C ERRCOD  (Out) error code
C     0 = no error
C     1 = fix real value position overflow
C
      CHARACTER*14 FMTLIN
      SAVE FMTLIN
      DATA FMTLIN /'('' '',A,F  .  )'/
C
      WRITE (FMTLIN (9:10), 10, ERR=90) POS
10    FORMAT (I2)
      WRITE (FMTLIN (12:13), 10, ERR=90) DEC
      WRITE (6, FMTLIN, ERR=90) LINE, FVAL
      ERRCOD = 0
      RETURN
C
90    ERRCOD = 1
      RETURN
      END
C
C
C-----------------------------------------------------
      SUBROUTINE TTYPTR (LINE, RVAL, POS, DEC, ERRCOD)
C
      CHARACTER*(*) LINE
      REAL          RVAL
      INTEGER       POS, DEC, ERRCOD
C
C LINE    (In) line to output
C RVAL    (In) floating real value to output
C POS     (In) number of positions to use for floating real value
C DEC     (In) number of decimals to output
C ERRCOD  (Out) error code
C     0 = no error
C     1 = floating real value position overflow
C
      CHARACTER*14 FMTLIN
      SAVE FMTLIN
      DATA FMTLIN /'('' '',A,E  .  )'/
C
      WRITE (FMTLIN (9:10), 10, ERR=90) POS
10    FORMAT (I2)
      WRITE (FMTLIN (12:13), 10, ERR=90) DEC
      WRITE (6, FMTLIN, ERR=90) LINE, RVAL
      ERRCOD = 0
      RETURN
C
90    ERRCOD = 1
      RETURN
      END
      SUBROUTINE U3BEST(W,X,Y,N,MODE,RMS,U,T,IER)
C
C**** CALCULATES BEST ROTATION & TRANSLATION BETWEEN TWO VECTOR SETS
C**** SUCH THAT U*X+T IS THE BEST APPROXIMATION TO Y.
C**** THIS VERSION OF THE ALGORITHM IS OPTIMIZED FOR THREE-DIMENSIONAL
C**** REAL VECTOR SPACE.
C**** USE OF THIS ROUTINE IS RESTRICTED TO NON-PROFIT ACADEMIC
C**** APPLICATIONS.
C**** PLEASE REPORT ERRORS TO
C**** PROGRAMMER:  W.KABSCH  MAX-PLANCK-INSTITUTE FOR MEDICAL RESEARCH
C                            JAHNSTRASSE 29, 6900 HEIDELBERG, FRG.
C**** REFERENCES:  W.KABSCH  ACTA CRYST.(1978).A34,827-828
C                  W.KABSCH  ACTA CRYST.(1976).A32,922-923
C
C  W     - W(M) IS WEIGHT FOR ATOM PAIR  # M                     (GIVEN)
C  X     - X(I,M) ARE COORDINATES OF ATOM # M IN SET X           (GIVEN)
C  Y     - Y(I,M) ARE COORDINATES OF ATOM # M IN SET Y           (GIVEN)
C  N     - N IS NUMBER OF ATOM PAIRS                             (GIVEN)
C  MODE  - 0:CALCULATE RMS ONLY                                  (GIVEN)
C          1:CALCULATE RMS,U,T   (TAKES LONGER)
C  RMS   - SUM OF W*(UX+T-Y)**2 OVER ALL ATOM PAIRS             (RESULT)
C  U     - U(I,J) IS   ROTATION  MATRIX FOR BEST SUPERPOSITION  (RESULT)
C  T     - T(I)   IS TRANSLATION VECTOR FOR BEST SUPERPOSITION  (RESULT)
C  IER   - 0:NO ERROR; -1:N WAS <2; -2:ILLEGAL WEIGHTS W(M)     (RESULT)
C
C-----------------------------------------------------------------------
      DIMENSION  W(1),X(3,1),Y(3,1),U(3,3),T(3),IP(9)
      DOUBLE PRECISION R(3,3),XC(3),YC(3),WC,A(3,3),B(3,3),E0,
     1 E(3),E1,E2,E3,D,SPUR,DET,COF,H,G,CTH,STH,SQRTH,SQRT3,P,
     2 RR(6),RR1,RR2,RR3,RR4,RR5,RR6,SS(6),SS1,SS2,SS3,SS4,SS5,SS6
      EQUIVALENCE (RR1,RR(1)),(RR2,RR(2)),(RR3,RR(3)),
     1            (RR4,RR(4)),(RR5,RR(5)),(RR6,RR(6)),
     2            (SS1,SS(1)),(SS2,SS(2)),(SS3,SS(3)),
     3            (SS4,SS(4)),(SS5,SS(5)),(SS6,SS(6)),
     4            (E1,E(1)),(E2,E(2)),(E3,E(3))
      DATA SQRT3/1.73205080756888D+00/
      DATA IP/1,2,4,  2,3,5,  4,5,6/
      IER=-1
      IF (N.LT.2)RETURN
      IER=-2
      WC=0.0D+00
      DO 1 I=1,3
      XC(I)=0.0D+00
1     YC(I)=0.0D+00
      DO 2 M=1,N
      IF (W(M).LT.0.0)RETURN
      WC=WC+W(M)
      DO 2 I=1,3
      XC(I)=XC(I)+W(M)*X(I,M)
2     YC(I)=YC(I)+W(M)*Y(I,M)
      IF (WC.LE.0.0D+00)RETURN
      DO 3 I=1,3
      XC(I)=XC(I)/WC
      YC(I)=YC(I)/WC
      DO 3 J=1,3
3     R(I,J)=0.0D+00
      E0=0.0D+00
      DO 4 M=1,N
      DO 4 I=1,3
      E0=E0+W(M)*((X(I,M)-XC(I))**2+(Y(I,M)-YC(I))**2)
      D=W(M)*(Y(I,M)-YC(I))
      DO 4 J=1,3
4     R(I,J)=R(I,J)+D*(X(J,M)-XC(J))
C**** CALCULATE DETERMINANT OF R(I,J)
      DET=R(1,1)*(R(2,2)*R(3,3)-R(2,3)*R(3,2))
     1   -R(1,2)*(R(2,1)*R(3,3)-R(2,3)*R(3,1))
     2   +R(1,3)*(R(2,1)*R(3,2)-R(2,2)*R(3,1))
      SIGMA=DET
C**** FORM UPPER TRIANGLE OF TRANSPOSED(R)*R
      M=0
      DO 5 J=1,3
      DO 5 I=1,J
      M=M+1
5     RR(M)=R(1,I)*R(1,J)+R(2,I)*R(2,J)+R(3,I)*R(3,J)
C***************** EIGENVALUES *****************************************
C**** FORM CHARACTERISTIC CUBIC  X**3-3*SPUR*X**2+3*COF*X-DET=0
      SPUR=(RR1+RR3+RR6)/3.0D+00
      COF=(RR3*RR6-RR5*RR5+RR1*RR6-RR4*RR4+RR1*RR3-RR2*RR2)/3.0D+00
      DET=DET*DET
C**** REDUCE CUBIC TO STANDARD FORM Y**3-3HY+2G=0 BY PUTTING X=Y-SPUR
      D=SPUR*SPUR
      H=D-COF
      G=SPUR*(COF*1.5D+00-D)-DET*0.5D+00
C**** SOLVE CUBIC. ROOTS ARE E1,E2,E3 IN DECREASING ORDER
      IF (H.LE.D*1.0D-9)GO TO 8
      SQRTH=DSQRT(H)
      D=-G/(H*SQRTH)
      IF (D.GT. 0.9999999D+00)GO TO 6
      IF (D.LT.-0.9999999D+00)GO TO 7
C.....HANDLE CASE OF THREE DISTINCT ROOTS
      D=DACOS(D)/3.0D+00
      CTH=SQRTH*DCOS(D)
      STH=SQRTH*SQRT3*DSIN(D)
      E1=SPUR+CTH+CTH
      E2=SPUR-CTH+STH
      E3=SPUR-CTH-STH
      IF (E3.LT.0.0D+00)E3=0.0D+00
      IF (MODE.EQ.0)GO TO 35
      M1=3
      M2=1
      M3=2
      GO TO 10
C.....HANDLE SPECIAL CASE OF TWO IDENTICAL ROOTS
6     E1=SPUR+SQRTH+SQRTH
      E2=SPUR-SQRTH
      E3=E2
      IF (MODE.EQ.0)GO TO 35
      M=1
      M1=3
      M2=1
      M3=2
      GO TO 20
7     E1=SPUR+SQRTH
      E2=E1
      E3=SPUR-SQRTH-SQRTH
      IF (E3.LT.0.0D+00)E3=0.0D+00
      IF (MODE.EQ.0)GO TO 35
      M=3
      M1=1
      M2=2
      M3=3
      GO TO 20
C.....HANDLE SPECIAL CASE OF 3 IDENTICAL ROOTS
8     E1=SPUR
      E2=SPUR
      E3=SPUR
      IF (MODE)26,35,26
C**************** EIGENVECTORS *****************************************
C.....EIGENVECTORS IN CASE OF THREE DISTINCT ROOTS
10    DO 15 L=1,2
      D=E(L)
      SS1=(D-RR3)*(D-RR6)-RR5*RR5
      SS2=(D-RR6)*RR2+RR4*RR5
      SS3=(D-RR1)*(D-RR6)-RR4*RR4
      SS4=(D-RR3)*RR4+RR2*RR5
      SS5=(D-RR1)*RR5+RR2*RR4
      SS6=(D-RR1)*(D-RR3)-RR2*RR2
      J=1
      IF (DABS(SS1).GE.DABS(SS3))GO TO 12
      J=2
      IF (DABS(SS3).GE.DABS(SS6))GO TO 13
11    J=3
      GO TO 13
12    IF (DABS(SS1).LT.DABS(SS6))GO TO 11
13    D=0.0D+00
      J=3*(J-1)
      DO 14 I=1,3
      K=IP(I+J)
      A(I,L)=SS(K)
14    D=D+SS(K)*SS(K)
      D=DSQRT(D)
      DO 15 I=1,3
15    A(I,L)=A(I,L)/D
16    A(1,M1)=A(2,M2)*A(3,M3)-A(2,M3)*A(3,M2)
      A(2,M1)=A(3,M2)*A(1,M3)-A(3,M3)*A(1,M2)
      A(3,M1)=A(1,M2)*A(2,M3)-A(1,M3)*A(2,M2)
      GO TO 30
C.....EIGENVECTORS IN CASE OF TWO DISTINCT ROOTS
20    P=0.0D+00
cccccccccccccccc      H=SPUR+G
      H=e2
      DO 21 I=1,3
      K=(I*I+I)/2
      D=DABS(RR(K)-H)
      IF (D.LT.P)GO TO 21
      J=I
      P=D
21    CONTINUE
      P=0.0D+00
      D=0.0D+00
      L=3*(J-1)
      DO 23 I=1,3
      K=IP(I+L)
      A(I,2)=1.0D+00
      IF (I.NE.J)GO TO 22
      A(I,M)=RR(K)-H
      GO TO 23
22    A(I,M)=RR(K)
      P=P-A(I,M)
23    D=D+A(I,M)**2
      A(J,2)=P/A(J,M)
      D=DSQRT(D)
      P=DSQRT(A(1,2)**2+A(2,2)**2+A(3,2)**2)
      DO 24 I=1,3
      A(I,2)=A(I,2)/P
24    A(I,M)=A(I,M)/D
      GO TO 16
C.....EIGENVECTORS IN CASE OF THREE IDENTICAL ROOTS
26    DO 27 I=1,3
      DO 27 J=1,3
      D=0.0D+00
      IF (I.EQ.J)D=1.0D+00
27    A(I,J)=D
C**** CALCULATE ROTATION MATRIX
30    DO 32 L=1,2
      D=0.0D+00
      DO 31 I=1,3
      B(I,L)=R(I,1)*A(1,L)+R(I,2)*A(2,L)+R(I,3)*A(3,L)
31    D=D+B(I,L)**2
      D=DSQRT(D)
      DO 32 I=1,3
32    B(I,L)=B(I,L)/D
      B(1,3)=B(2,1)*B(3,2)-B(2,2)*B(3,1)
      B(2,3)=B(3,1)*B(1,2)-B(3,2)*B(1,1)
      B(3,3)=B(1,1)*B(2,2)-B(1,2)*B(2,1)
      DO 33 I=1,3
      DO 33 J=1,3
33    U(I,J)=B(I,1)*A(J,1)+B(I,2)*A(J,2)+B(I,3)*A(J,3)
C**** CALCULATE TRANSLATION VECTOR
      DO 34 I=1,3
34    T(I)=YC(I)-U(I,1)*XC(1)-U(I,2)*XC(2)-U(I,3)*XC(3)
C**** CALCULATE RMS ERROR
35    D=DSQRT(E3)
      IF (SIGMA.LT.0.0)D=-D
      D=D+DSQRT(E2)+DSQRT(E1)
      RMS=ABS(E0-D-D)
C**** NORMAL EXIT
      IER=0
      RETURN
      END
C
C
C
      SUBROUTINE U3BSTU(X,Y,N,MODE,RMS,U,T,IER)
C
C**** CALCULATES BEST ROTATION & TRANSLATION BETWEEN TWO VECTOR SETS
C**** SUCH THAT U*X+T IS THE BEST APPROXIMATION TO Y.
C**** THIS VERSION OF THE ALGORITHM IS OPTIMIZED FOR THREE-DIMENSIONAL
C**** REAL VECTOR SPACE.
C**** USE OF THIS ROUTINE IS RESTRICTED TO NON-PROFIT ACADEMIC
C**** APPLICATIONS.
C**** PLEASE REPORT ERRORS TO
C**** PROGRAMMER:  W.KABSCH  MAX-PLANCK-INSTITUTE FOR MEDICAL RESEARCH
C                            JAHNSTRASSE 29, 6900 HEIDELBERG, FRG.
C**** REFERENCES:  W.KABSCH  ACTA CRYST.(1978).A34,827-828
C                  W.KABSCH  ACTA CRYST.(1976).A32,922-923
C
C>>>> THIS IS A VERSION OF U3BEST WITHOUT SEPARATE WEIGHTS FOR EACH
C>>>> ATOM PAIR. CHANGED BY PER KRAULIS, MOL BIOL UPPSALA.
C
C  X     - X(I,M) ARE COORDINATES OF ATOM # M IN SET X           (GIVEN)
C  Y     - Y(I,M) ARE COORDINATES OF ATOM # M IN SET Y           (GIVEN)
C  N     - N IS NUMBER OF ATOM PAIRS                             (GIVEN)
C  MODE  - 0:CALCULATE RMS ONLY                                  (GIVEN)
C          1:CALCULATE RMS,U,T   (TAKES LONGER)
C  RMS   - SUM OF (UX+T-Y)**2 OVER ALL ATOM PAIRS               (RESULT)
C  U     - U(I,J) IS   ROTATION  MATRIX FOR BEST SUPERPOSITION  (RESULT)
C  T     - T(I)   IS TRANSLATION VECTOR FOR BEST SUPERPOSITION  (RESULT)
C  IER   - 0:NO ERROR; -1:N WAS <2;                             (RESULT)
C
C-----------------------------------------------------------------------
      DIMENSION  X(3,1),Y(3,1),U(3,3),T(3),IP(9)
      DOUBLE PRECISION R(3,3),XC(3),YC(3),A(3,3),B(3,3),E0,
     1 E(3),E1,E2,E3,D,SPUR,DET,COF,H,G,CTH,STH,SQRTH,SQRT3,P,
     2 RR(6),RR1,RR2,RR3,RR4,RR5,RR6,SS(6),SS1,SS2,SS3,SS4,SS5,SS6
      EQUIVALENCE (RR1,RR(1)),(RR2,RR(2)),(RR3,RR(3)),
     1            (RR4,RR(4)),(RR5,RR(5)),(RR6,RR(6)),
     2            (SS1,SS(1)),(SS2,SS(2)),(SS3,SS(3)),
     3            (SS4,SS(4)),(SS5,SS(5)),(SS6,SS(6)),
     4            (E1,E(1)),(E2,E(2)),(E3,E(3))
      DATA SQRT3/1.73205080756888D+00/
      DATA IP/1,2,4,  2,3,5,  4,5,6/
      IER=-1
      IF (N.LT.2)RETURN
      DO 1 I=1,3
      XC(I)=0.0D+00
1     YC(I)=0.0D+00
      DO 2 M=1,N
      DO 2 I=1,3
      XC(I)=XC(I)+X(I,M)
2     YC(I)=YC(I)+Y(I,M)
      E0=N
      DO 3 I=1,3
      XC(I)=XC(I)/E0
      YC(I)=YC(I)/E0
      DO 3 J=1,3
3     R(I,J)=0.0D+00
      E0=0.0D+00
      DO 4 M=1,N
      DO 4 I=1,3
      E0=E0+((X(I,M)-XC(I))**2+(Y(I,M)-YC(I))**2)
      D=Y(I,M)-YC(I)
      DO 4 J=1,3
4     R(I,J)=R(I,J)+D*(X(J,M)-XC(J))
C**** CALCULATE DETERMINANT OF R(I,J)
      DET=R(1,1)*(R(2,2)*R(3,3)-R(2,3)*R(3,2))
     1   -R(1,2)*(R(2,1)*R(3,3)-R(2,3)*R(3,1))
     2   +R(1,3)*(R(2,1)*R(3,2)-R(2,2)*R(3,1))
      SIGMA=DET
C**** FORM UPPER TRIANGLE OF TRANSPOSED(R)*R
      M=0
      DO 5 J=1,3
      DO 5 I=1,J
      M=M+1
5     RR(M)=R(1,I)*R(1,J)+R(2,I)*R(2,J)+R(3,I)*R(3,J)
C***************** EIGENVALUES *****************************************
C**** FORM CHARACTERISTIC CUBIC  X**3-3*SPUR*X**2+3*COF*X-DET=0
      SPUR=(RR1+RR3+RR6)/3.0D+00
      COF=(RR3*RR6-RR5*RR5+RR1*RR6-RR4*RR4+RR1*RR3-RR2*RR2)/3.0D+00
      DET=DET*DET
C**** REDUCE CUBIC TO STANDARD FORM Y**3-3HY+2G=0 BY PUTTING X=Y-SPUR
      D=SPUR*SPUR
      H=D-COF
      G=SPUR*(COF*1.5D+00-D)-DET*0.5D+00
C**** SOLVE CUBIC. ROOTS ARE E1,E2,E3 IN DECREASING ORDER
      IF (H.LE.D*1.0D-9)GO TO 8
      SQRTH=DSQRT(H)
      D=-G/(H*SQRTH)
      IF (D.GT. 0.9999999D+00)GO TO 6
      IF (D.LT.-0.9999999D+00)GO TO 7
C.....HANDLE CASE OF THREE DISTINCT ROOTS
      D=DACOS(D)/3.0D+00
      CTH=SQRTH*DCOS(D)
      STH=SQRTH*SQRT3*DSIN(D)
      E1=SPUR+CTH+CTH
      E2=SPUR-CTH+STH
      E3=SPUR-CTH-STH
      IF (E3.LT.0.0D+00)E3=0.0D+00
      IF (MODE.EQ.0)GO TO 35
      M1=3
      M2=1
      M3=2
      GO TO 10
C.....HANDLE SPECIAL CASE OF TWO IDENTICAL ROOTS
6     E1=SPUR+SQRTH+SQRTH
      E2=SPUR-SQRTH
      E3=E2
      IF (MODE.EQ.0)GO TO 35
      M=1
      M1=3
      M2=1
      M3=2
      GO TO 20
7     E1=SPUR+SQRTH
      E2=E1
      E3=SPUR-SQRTH-SQRTH
      IF (E3.LT.0.0D+00)E3=0.0D+00
      IF (MODE.EQ.0)GO TO 35
      M=3
      M1=1
      M2=2
      M3=3
      GO TO 20
C.....HANDLE SPECIAL CASE OF 3 IDENTICAL ROOTS
8     E1=SPUR
      E2=SPUR
      E3=SPUR
      IF (MODE)26,35,26
C**************** EIGENVECTORS *****************************************
C.....EIGENVECTORS IN CASE OF THREE DISTINCT ROOTS
10    DO 15 L=1,2
      D=E(L)
      SS1=(D-RR3)*(D-RR6)-RR5*RR5
      SS2=(D-RR6)*RR2+RR4*RR5
      SS3=(D-RR1)*(D-RR6)-RR4*RR4
      SS4=(D-RR3)*RR4+RR2*RR5
      SS5=(D-RR1)*RR5+RR2*RR4
      SS6=(D-RR1)*(D-RR3)-RR2*RR2
      J=1
      IF (DABS(SS1).GE.DABS(SS3))GO TO 12
      J=2
      IF (DABS(SS3).GE.DABS(SS6))GO TO 13
11    J=3
      GO TO 13
12    IF (DABS(SS1).LT.DABS(SS6))GO TO 11
13    D=0.0D+00
      J=3*(J-1)
      DO 14 I=1,3
      K=IP(I+J)
      A(I,L)=SS(K)
14    D=D+SS(K)*SS(K)
      D=DSQRT(D)
      DO 15 I=1,3
15    A(I,L)=A(I,L)/D
16    A(1,M1)=A(2,M2)*A(3,M3)-A(2,M3)*A(3,M2)
      A(2,M1)=A(3,M2)*A(1,M3)-A(3,M3)*A(1,M2)
      A(3,M1)=A(1,M2)*A(2,M3)-A(1,M3)*A(2,M2)
      GO TO 30
C.....EIGENVECTORS IN CASE OF TWO DISTINCT ROOTS
20    P=0.0D+00
cccccccccccccccc      H=SPUR+G
      H=e2
      DO 21 I=1,3
      K=(I*I+I)/2
      D=DABS(RR(K)-H)
      IF (D.LT.P)GO TO 21
      J=I
      P=D
21    CONTINUE
      P=0.0D+00
      D=0.0D+00
      L=3*(J-1)
      DO 23 I=1,3
      K=IP(I+L)
      A(I,2)=1.0D+00
      IF (I.NE.J)GO TO 22
      A(I,M)=RR(K)-H
      GO TO 23
22    A(I,M)=RR(K)
      P=P-A(I,M)
23    D=D+A(I,M)**2
      A(J,2)=P/A(J,M)
      D=DSQRT(D)
      P=DSQRT(A(1,2)**2+A(2,2)**2+A(3,2)**2)
      DO 24 I=1,3
      A(I,2)=A(I,2)/P
24    A(I,M)=A(I,M)/D
      GO TO 16
C.....EIGENVECTORS IN CASE OF THREE IDENTICAL ROOTS
26    DO 27 I=1,3
      DO 27 J=1,3
      D=0.0D+00
      IF (I.EQ.J)D=1.0D+00
27    A(I,J)=D
C**** CALCULATE ROTATION MATRIX
30    DO 32 L=1,2
      D=0.0D+00
      DO 31 I=1,3
      B(I,L)=R(I,1)*A(1,L)+R(I,2)*A(2,L)+R(I,3)*A(3,L)
31    D=D+B(I,L)**2
      D=DSQRT(D)
      DO 32 I=1,3
32    B(I,L)=B(I,L)/D
      B(1,3)=B(2,1)*B(3,2)-B(2,2)*B(3,1)
      B(2,3)=B(3,1)*B(1,2)-B(3,2)*B(1,1)
      B(3,3)=B(1,1)*B(2,2)-B(1,2)*B(2,1)
      DO 33 I=1,3
      DO 33 J=1,3
33    U(I,J)=B(I,1)*A(J,1)+B(I,2)*A(J,2)+B(I,3)*A(J,3)
C**** CALCULATE TRANSLATION VECTOR
      DO 34 I=1,3
34    T(I)=YC(I)-U(I,1)*XC(1)-U(I,2)*XC(2)-U(I,3)*XC(3)
C**** CALCULATE RMS ERROR
35    D=DSQRT(E3)
      IF (SIGMA.LT.0.0)D=-D
      D=D+DSQRT(E2)+DSQRT(E1)
      RMS=ABS(E0-D-D)
C**** NORMAL EXIT
      IER=0
      RETURN
      END
C Package U3VECP
C
C Basic 3D vector list procedures.
C
C U3INIT s  initialize vector values
C U3COPY s  copy values from one vector to another
C U3ADD  s  add vectors
C U3SUBT s  subtract vectors
C U3SCAL s  scale vectors
C U3LEN  s  length of vectors
C U3DIFF s  distance between coordinates
C U3DOT  s  dot product of vectors
C U3NORM s  normalize vectors
C U3ROT  s  multiply vectors by matrix
C U3RMSD rf compute root-mean-square deviation of vector lists, as is
C
C Uses no separately compiled procedures.
C
C Copyright (C) 1987 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  17-Sep-1987  first attempts
C  21-Sep-1987  written
C  23-Jan-1988  U3RMSD and U3ROT included, minor changes
C
C
C-----------------------------------------
      SUBROUTINE U3INIT (U, R1, R2, R3, N)
C
C Operation:  U(N) = (R1, R2, R3)
C
      INTEGER N
      REAL    U (3, N)
      REAL    R1, R2, R3
C
      INTEGER I
C
      DO 100 I = 1, N
        U (1, I) = R1
        U (2, I) = R2
        U (3, I) = R3
100   CONTINUE
      RETURN
      END
C
C
C----------------------------------
      SUBROUTINE U3COPY (U1, U2, N)
C
C Operation:  U1(N) = U2(N)
C
      INTEGER N
      REAL    U1 (3, N), U2 (3, N)
C
      INTEGER I
C
      DO 100 I = 1, N
        U1 (1, I) = U2 (1, I)
        U1 (2, I) = U2 (2, I)
        U1 (3, I) = U2 (3, I)
100   CONTINUE
      RETURN
      END
C
C
C-------------------------------------
      SUBROUTINE U3ADD (U1, U2, U3, N)
C
C Operation:  U1(N) = U2(N) + U3(N)
C
      INTEGER N
      REAL    U1 (3, N), U2 (3, N), U3 (3, N)
C
      INTEGER I
C
      DO 100 I = 1, N
        U1 (1, I) = U2 (1, I) + U3 (1, I)
        U1 (2, I) = U2 (2, I) + U3 (2, I)
        U1 (3, I) = U2 (3, I) + U3 (3, I)
100   CONTINUE
      RETURN
      END
C
C
C--------------------------------------
      SUBROUTINE U3SUBT (U1, U2, U3, N)
C
C Operation:  U1(N) = U2(N) - U3(N)
C
      INTEGER N
      REAL    U1 (3, N), U2 (3, N), U3 (3, N)
C
      INTEGER I
C
      DO 100 I = 1, N
        U1 (1, I) = U2 (1, I) - U3 (1, I)
        U1 (2, I) = U2 (2, I) - U3 (2, I)
        U1 (3, I) = U2 (3, I) - U3 (3, I)
100   CONTINUE
      RETURN
      END
C
C
C-------------------------------------
      SUBROUTINE U3SCAL (U1, R, U2, N)
C
C Operation:  U1(N) = R * U2(N)
C
      INTEGER N
      REAL    R
      REAL    U1 (3, N), U2 (3, N)
C
      INTEGER I
C
      DO 100 I = 1, N
        U1 (1, I) = R * U2 (1, I)
        U1 (2, I) = R * U2 (2, I)
        U1 (3, I) = R * U2 (3, I)
100   CONTINUE
      RETURN
      END
C
C
C-------------------------------
      SUBROUTINE U3LEN (V, U, N)
C
C Operation:  V(N) = Length (U(N))
C
      INTEGER N
      REAL    V (N), U (3, N)
C
      INTEGER I
C
      DO 100 I = 1, N
        V(I) = SQRT (U (1, I) **2 + U (2, I) **2 + U (3, I) **2)
100   CONTINUE
      RETURN
      END
C
C
C-------------------------------------
      SUBROUTINE U3DIFF (V, U1, U2, N)
C
C Operation:  V(N) = Length (U1(N) - U2(N))
C
      INTEGER N
      REAL    V (N), U1 (3, N), U2 (3, N)
C
      INTEGER I
C
      DO 100 I = 1, N
        V (I) = SQRT ((U1 (1,I) - U2 (1,I)) **2 +
     $                (U1 (2,I) - U2 (2,I)) **2 +
     $                (U1 (3,I) - U2 (3,I)) **2)
100   CONTINUE
      RETURN
      END
C
C
C------------------------------------
      SUBROUTINE U3DOT (V, U1, U2, N)
C
C Operation:  V(N) = U1(N) <dot> U2(N)
C
      INTEGER N
      REAL    V (N), U1 (3, N), U2 (3, N)
C
      INTEGER I
C
      DO 100 I = 1, N
        V (I) = U1 (1, I) * U2 (1, I) +
     $          U1 (2, I) * U2 (2, I) +
     $          U1 (3, I) * U2 (3, I)
100   CONTINUE
      RETURN
      END
C
C
C----------------------------------
      SUBROUTINE U3NORM (U1, U2, N)
C
C Operation:  U1(N) = U2(N) / Length (U2(N))
C
      INTEGER N
      REAL    U1 (3, N), U2 (3, N)
C
      INTEGER I
      REAL    L
C
      DO 100 I = 1, N
        L = SQRT (U2 (1, I) **2 + U2 (2, I) **2 + U2 (3, I) **2)
        U1 (1, I) = U2 (1, I) / L
        U1 (2, I) = U2 (2, I) / L
        U1 (3, I) = U2 (3, I) / L
100   CONTINUE
      RETURN
      END
C
C
C------------------------------------
      SUBROUTINE U3ROT (U1, M, U2, N)
C
C Operation: U1(N) = M * U2(N)
C
      INTEGER N
      REAL    U1 (3, N), M (3, 3), U2 (3, N)
C
      INTEGER I
      REAL    V (3)
C
      DO 100 I = 1, N
        V (1) = U2 (1, I)
        V (2) = U2 (2, I)
        V (3) = U2 (3, I)
        U1 (1, I) = M(1,1) * V(1) + M(1,2) * V(2) + M(1,3) * V(3)
        U1 (2, I) = M(2,1) * V(1) + M(2,2) * V(2) + M(2,3) * V(3)
        U1 (3, I) = M(3,1) * V(1) + M(3,2) * V(2) + M(3,3) * V(3)
100   CONTINUE
      RETURN
      END
C
C
C-------------------------------------
      REAL FUNCTION U3RMSD (U1, U2, N)
C
      INTEGER N
      REAL    U1 (3, N), U2 (3, N)
C
      REAL    S
      INTEGER I
C
      S = 0.0
      DO 100 I = 1, N
        S = S + (U1 (1, I) - U2 (1, I)) **2 +
     $          (U1 (2, I) - U2 (2, I)) **2 +
     $          (U1 (3, I) - U2 (3, I)) **2
100   CONTINUE
      IF (N .GT. 1) S = S / REAL (N)
      U3RMSD = SQRT (S)
      RETURN
      END
C Package V3VECP
C
C Basic 3D vector procedures.
C
C V3INIT s  initialize vector value
C V3COPY s  copy values from one vector to another
C V3ADD  s  add vectors
C V3SUBT s  subtract vectors
C V3SCAL s  scale vector
C V3LEN  rf length of vector
C V3DIFF rf distance between coordinates
C V3DOT  rf dot product of vectors
C V3CROS s  cross product of vectors
C V3ANG  rf angle between vectors, in radians
C V3ANGD rf angle between vectors, in degrees
C V3TOR  rf torsion angle between vectors, in radians
C V3TORD rf torsion angle between vectors, in degrees
C V3NORM s  normalize vector
C V3ROT  s  multiply vector by matrix
C
C Uses no separately compiled procedures.
C
C Copyright (C) 1988 Per Kraulis
C
C Per Kraulis, Dept Molecular Biology, Uppsala University Sweden.
C  18-May-1988  copied from U3VECP, modified, added procedures
C
C
C--------------------------------------
      SUBROUTINE V3INIT (V, R1, R2, R3)
C
C Operation:  V = (R1, R2, R3)
C
      REAL V (3)
      REAL R1, R2, R3
C
      V (1) = R1
      V (2) = R2
      V (3) = R3
      RETURN
      END
C
C
C-------------------------------
      SUBROUTINE V3COPY (V1, V2)
C
C Operation:  V1 = V2
C
      REAL V1 (3), V2 (3)
C
      V1 (1) = V2 (1)
      V1 (2) = V2 (2)
      V1 (3) = V2 (3)
      RETURN
      END
C
C
C----------------------------------
      SUBROUTINE V3ADD (V1, V2, V3)
C
C Operation:  V1 = V2 + V3
C
      REAL V1 (3), V2 (3), V3 (3)
C
      V1 (1) = V2 (1) + V3 (1)
      V1 (2) = V2 (2) + V3 (2)
      V1 (3) = V2 (3) + V3 (3)
      RETURN
      END
C
C
C-----------------------------------
      SUBROUTINE V3SUBT (V1, V2, V3)
C
C Operation:  V1 = V2 - V3
C
      REAL V1 (3), V2 (3), V3 (3)
C
      V1 (1) = V2 (1) - V3 (1)
      V1 (2) = V2 (2) - V3 (2)
      V1 (3) = V2 (3) - V3 (3)
      RETURN
      END
C
C
C----------------------------------
      SUBROUTINE V3SCAL (V1, R, V2)
C
C Operation:  V1 = R * V2
C
      REAL R
      REAL V1 (3), V2 (3)
C
      V1 (1) = R * V2 (1)
      V1 (2) = R * V2 (2)
      V1 (3) = R * V2 (3)
      RETURN
      END
C
C
C----------------------------
      REAL FUNCTION V3LEN (V)
C
C Operation:  return Length (V)
C
      REAL V (3)
C
      V3LEN = SQRT (V (1) **2 + V (2) **2 + V (3) **2)
      RETURN
      END
C
C
C----------------------------------
      REAL FUNCTION V3DIFF (V1, V2)
C
C Operation:  return Length (V1 - V2)
C
      REAL V1 (3), V2 (3)
C
      V3DIFF = SQRT ((V1 (1) - V2 (1)) **2 +
     $               (V1 (2) - V2 (2)) **2 +
     $               (V1 (3) - V2 (3)) **2)
      RETURN
      END
C
C
C---------------------------------
      REAL FUNCTION V3DOT (V1, V2)
C
C Operation:  return V1 <dot> V2
C
      REAL V1 (3), V2 (3)
C
      V3DOT = V1 (1) * V2 (1) + V1 (2) * V2 (2) + V1 (3) * V2 (3)
      RETURN
      END
C
C
C-----------------------------------
      SUBROUTINE V3CROS (V1, V2, V3)
C
C Operation:  V1 = V2 x V3
C
      REAL V1 (3), V2 (3), V3 (3)
C
      REAL T (3)
C
      T (1) = V2 (2) * V3 (3) - V2 (3) * V3 (2)
      T (2) = V2 (3) * V3 (1) - V2 (1) * V3 (3)
      T (3) = V2 (1) * V3 (2) - V2 (2) * V3 (1)
      V1 (1) = T (1)
      V1 (2) = T (2)
      V1 (3) = T (3)
      RETURN
      END
C
C
C-------------------------------------
      REAL FUNCTION V3ANG (V1, V2, V3)
C
C Operation :  return Angle ((V1 - V2), (V3 - V2))  [radians]
C
      REAL V1 (3), V2 (3), V3 (3)
C
      REAL T, S1, S3
      REAL R1 (3), R3 (3)
C
      R1 (1) = V1 (1) - V2 (1)
      R1 (2) = V1 (2) - V2 (2)
      R1 (3) = V1 (3) - V2 (3)
      R3 (1) = V3 (1) - V2 (1)
      R3 (2) = V3 (2) - V2 (2)
      R3 (3) = V3 (3) - V2 (3)
      T = R1 (1) * R3 (1) + R1 (2) * R3 (2) + R1 (3) * R3 (3)
      S1 = SQRT (R1 (1) **2 + R1 (2) **2 + R1 (3) **2)
      S3 = SQRT (R3 (1) **2 + R3 (2) **2 + R3 (3) **2)
      T = T / (S1 * S3)
      IF (ABS (T) .GT. 1.0) T = SIGN (1.0, T)
      V3ANG = ACOS (T)
      RETURN
      END
C
C
C--------------------------------------
      REAL FUNCTION V3ANGD (V1, V2, V3)
C
C Operation :  return Angle ((V1 - V2), (V3 - V2))  [degrees]
C
      REAL       D
      PARAMETER (D = 180.0 / 3.1415926536)
C
      REAL V1 (3), V2 (3), V3 (3)
C
      REAL T, S1, S3
      REAL R1 (3), R3 (3)
C
      R1 (1) = V1 (1) - V2 (1)
      R1 (2) = V1 (2) - V2 (2)
      R1 (3) = V1 (3) - V2 (3)
      R3 (1) = V3 (1) - V2 (1)
      R3 (2) = V3 (2) - V2 (2)
      R3 (3) = V3 (3) - V2 (3)
      T = R1 (1) * R3 (1) + R1 (2) * R3 (2) + R1 (3) * R3 (3)
      S1 = SQRT (R1 (1) **2 + R1 (2) **2 + R1 (3) **2)
      S3 = SQRT (R3 (1) **2 + R3 (2) **2 + R3 (3) **2)
      T = T / (S1 * S3)
      IF (ABS (T) .GT. 1.0) T = SIGN (1.0, T)
      V3ANGD = D * ACOS (T)
      RETURN
      END
C
C
C-----------------------------------------
      REAL FUNCTION V3TOR (V1, V2, V3, V4)
C
C Operation :  return Torsion_angle (V1, V2, V3, V4)  [radians]
C
      REAL V1 (3), V2 (3), V3 (3), V4 (3)
C
      REAL T, S2, S3
      REAL R21 (3), R23 (3), R43 (3), X2 (3), X3 (3)
C
      R21 (1) = V1 (1) - V2 (1)
      R21 (2) = V1 (2) - V2 (2)
      R21 (3) = V1 (3) - V2 (3)
      R23 (1) = V3 (1) - V2 (1)
      R23 (2) = V3 (2) - V2 (2)
      R23 (3) = V3 (3) - V2 (3)
      X2 (1) = R21 (2) * R23 (3) - R21 (3) * R23 (2)
      X2 (2) = R21 (3) * R23 (1) - R21 (1) * R23 (3)
      X2 (3) = R21 (1) * R23 (2) - R21 (2) * R23 (1)
      R43 (1) = V3 (1) - V4 (1)
      R43 (2) = V3 (2) - V4 (2)
      R43 (3) = V3 (3) - V4 (3)
      X3 (1) = R23 (2) * R43 (3) - R23 (3) * R43 (2)
      X3 (2) = R23 (3) * R43 (1) - R23 (1) * R43 (3)
      X3 (3) = R23 (1) * R43 (2) - R23 (2) * R43 (1)
      T = X2 (1) * X3 (1) + X2 (2) * X3 (2) + X2 (3) * X3 (3)
      S2 = SQRT (X2 (1) **2 + X2 (2) **2 + X2 (3) **2)
      S3 = SQRT (X3 (1) **2 + X3 (2) **2 + X3 (3) **2)
      T = T / (S2 * S3)
      IF (ABS (T) .GT. 1.0) T = SIGN (1.0, T)
      T = ACOS (T)
      IF (R21(1) * X3(1) + R21(2) * X3(2) + R21(3) * X3(3) .LT. 0.0)
     $  T = -T
      V3TOR = T
      RETURN
      END
C
C
C------------------------------------------
      REAL FUNCTION V3TORD (V1, V2, V3, V4)
C
C Operation :  return Torsion_angle (V1, V2, V3, V4)  [degrees]
C
      REAL       D
      PARAMETER (D = 180.0 / 3.1415926536)
C
      REAL V1 (3), V2 (3), V3 (3), V4 (3)
C
      REAL T, S2, S3
      REAL R21 (3), R23 (3), R43 (3), X2 (3), X3 (3)
C
      R21 (1) = V1 (1) - V2 (1)
      R21 (2) = V1 (2) - V2 (2)
      R21 (3) = V1 (3) - V2 (3)
      R23 (1) = V3 (1) - V2 (1)
      R23 (2) = V3 (2) - V2 (2)
      R23 (3) = V3 (3) - V2 (3)
      X2 (1) = R21 (2) * R23 (3) - R21 (3) * R23 (2)
      X2 (2) = R21 (3) * R23 (1) - R21 (1) * R23 (3)
      X2 (3) = R21 (1) * R23 (2) - R21 (2) * R23 (1)
      R43 (1) = V3 (1) - V4 (1)
      R43 (2) = V3 (2) - V4 (2)
      R43 (3) = V3 (3) - V4 (3)
      X3 (1) = R23 (2) * R43 (3) - R23 (3) * R43 (2)
      X3 (2) = R23 (3) * R43 (1) - R23 (1) * R43 (3)
      X3 (3) = R23 (1) * R43 (2) - R23 (2) * R43 (1)
      T = X2 (1) * X3 (1) + X2 (2) * X3 (2) + X2 (3) * X3 (3)
      S2 = SQRT (X2 (1) **2 + X2 (2) **2 + X2 (3) **2)
      S3 = SQRT (X3 (1) **2 + X3 (2) **2 + X3 (3) **2)
      T = T / (S2 * S3)
      IF (ABS (T) .GT. 1.0) T = SIGN (1.0, T)
      T = D * ACOS (T)
      IF (R21(1) * X3(1) + R21(2) * X3(2) + R21(3) * X3(3) .LT. 0.0)
     $  T = -T
      V3TORD = T
      RETURN
      END
C
C
C-------------------------------
      SUBROUTINE V3NORM (V1, V2)
C
C Operation:  V1 = V2 / Length (V2)
C
      REAL V1 (3), V2 (3)
C
      REAL S
C
      S = SQRT (V2 (1) **2 + V2 (2) **2 + V2 (3) **2)
      V1 (1) = V2 (1) / S
      V1 (2) = V2 (2) / S
      V1 (3) = V2 (3) / S
      RETURN
      END
C
C
C---------------------------------
      SUBROUTINE V3ROT (V1, M, V2)
C
C Operation:  V1 = M * V2
C
      REAL V1 (3), M (3, 3), V2 (3)
C
      REAL T (3)
C
      T (1) = V2 (1)
      T (2) = V2 (2)
      T (3) = V2 (3)
      V1 (1) = M(1,1) * T(1) + M(1,2) * T(2) + M(1,3) * T(3)
      V1 (2) = M(2,1) * T(1) + M(2,2) * T(2) + M(2,3) * T(3)
      V1 (3) = M(3,1) * T(1) + M(3,2) * T(2) + M(3,3) * T(3)
      RETURN
      END
