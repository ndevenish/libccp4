C
C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
C-- FFT81       F77TRNFM.FOR                        13/09/85    JWC
C
C
C**** FOLLOWING ARE ROUTINES USED BY TEN EYCK'S FFT PROGRAMS***
C
C
      SUBROUTINE CMPLFT(X,Y,N,D)
C     ===============================
C
C
C     Complex finite discrete fourier transform
C     transforms one dimension of multi-dimensional data
C     modified by L. F. TEN EYCK from a one-dimensional version written
C     by G. T. SANDE, 1969.
C
C     This program calculates the transform
C               (X(T) + I*Y(T))*(COS(2*PI*T/N) - I*SIN(2*PI*T/N))
C
C     INDEXING -- the arrangement of the multi-dimensional data is
C     specified by the integer array D, the values of which are used as
C     control parameters in do loops.  when it is desired to cover all
C     elements of the data for which the subscript being transformed has
C     the value I0, the following is used.
C
C               I1 = (I0 - 1)*D(2) + 1
C               DO 100 I2 = I1, D(1), D(3)
C               I3 = I2 + D(4) - 1
C               DO 100 I = I2, I3, D(5)
C                  .
C                  .
C           100 CONTINUE
C
C     with this indexing it is possible to use a number of arrangements
C     of the data, including normal fortran complex numbers (d(5) = 2)
C     or separate storage of real and imaginary parts.
C
C
C---- PMAX is the largest prime factor that will be tolerated by this
C     program.
C
C---- TWOGRP is the largest power of two that is treated as a special
C     case.
C
C     .. Scalar Arguments ..
      INTEGER           N
C     ..
C     .. Array Arguments ..
      REAL              X(*),Y(*)
      INTEGER           D(5)
C     ..
C     .. Local Scalars ..
      INTEGER           PMAX,PSYM,TWOGRP
      LOGICAL           ERROR
C     ..
C     .. Local Arrays ..
      INTEGER           FACTOR(15),SYM(15),UNSYM(15)
C     ..
C     .. External Subroutines ..
      EXTERNAL          DIPRP,MDFTKD,SRFP
C     ..
      PMAX = 19
      TWOGRP = 8
C
      IF (N.GT.1) THEN
          CALL SRFP(N,PMAX,TWOGRP,FACTOR,SYM,PSYM,UNSYM,ERROR)
          IF (ERROR) THEN
C
              WRITE (6,FMT=9000) N
              call ccperr(1,' stop in fftlib.for 1000')
          ELSE
C
              CALL MDFTKD(N,FACTOR,D,X,Y)
              CALL DIPRP(N,SYM,PSYM,UNSYM,D,X,Y)
          END IF
      END IF
C
C---- Format statements
C
 9000 FORMAT ('0INVALID NUMBER OF POINTS FOR CMPL FT.  N =',I10,//)
      END
C
C
      SUBROUTINE SRFP(PTS,PMAX,TWOGRP,FACTOR,SYM,PSYM,UNSYM,ERROR)
C     ==================================================================
C
C
C---- Symmetrized reordering factoring programme
C
C
C     .. Scalar Arguments ..
      INTEGER         PMAX,PSYM,PTS,TWOGRP
      LOGICAL         ERROR
C     ..
C     .. Array Arguments ..
      INTEGER         FACTOR(10),SYM(10),UNSYM(10)
C     ..
C     .. Local Scalars ..
      INTEGER         F,J,JJ,N,NEST,P,PTWO,Q,R
C     ..
C     .. Local Arrays ..
      INTEGER         PP(14),QQ(7)
C     ..
      NEST = 14
C
      N = PTS
      PSYM = 1
      F = 2
      P = 0
      Q = 0
   10 CONTINUE
      IF (N.LE.1) THEN
          GO TO 60
      ELSE
          DO 20 J = F,PMAX
              IF (N.EQ. (N/J)*J) GO TO 30
   20         CONTINUE
          GO TO 40
   30     IF (2*P+Q.GE.NEST) THEN
              GO TO 50
          ELSE
              F = J
              N = N/F
              IF (N.EQ. (N/F)*F) THEN
                  N = N/F
                  P = P + 1
                  PP(P) = F
                  PSYM = PSYM*F
              ELSE
                  Q = Q + 1
                  QQ(Q) = F
              END IF
              GO TO 10
          END IF
      END IF
C
   40 CONTINUE
      WRITE (6,FMT=9000) PMAX,PTS
      ERROR = .TRUE.
      GO TO 100
C
   50 CONTINUE
      WRITE (6,FMT=9010) NEST,PTS
      ERROR = .TRUE.
      GO TO 100
C
   60 CONTINUE
      R = 1
      IF (Q.EQ.0) R = 0
      IF (P.GE.1) THEN
          DO 70 J = 1,P
              JJ = P + 1 - J
              SYM(J) = PP(JJ)
              FACTOR(J) = PP(JJ)
              JJ = P + Q + J
              FACTOR(JJ) = PP(J)
              JJ = P + R + J
              SYM(JJ) = PP(J)
   70         CONTINUE
      END IF
      IF (Q.GE.1) THEN
          DO 80 J = 1,Q
              JJ = P + J
              UNSYM(J) = QQ(J)
              FACTOR(JJ) = QQ(J)
   80         CONTINUE
          SYM(P+1) = PTS/PSYM**2
      END IF
      JJ = 2*P + Q
      FACTOR(JJ+1) = 0
      PTWO = 1
      J = 0
   90 CONTINUE
      J = J + 1
      IF (FACTOR(J).NE.0) THEN
          IF (FACTOR(J).EQ.2) THEN
              PTWO = PTWO*2
              FACTOR(J) = 1
              IF (PTWO.LT.TWOGRP) THEN
                  IF (FACTOR(J+1).EQ.2) GO TO 90
              END IF
              FACTOR(J) = PTWO
              PTWO = 1
          END IF
          GO TO 90
      END IF
      IF (P.EQ.0) R = 0
      JJ = 2*P + R
      SYM(JJ+1) = 0
      IF (Q.LE.1) Q = 0
      UNSYM(Q+1) = 0
      ERROR = .FALSE.
C
  100 CONTINUE
C
C---- Format statements
C
 9000 FORMAT ('0LARGEST FACTOR EXCEEDS ',I3,'.  N = ',I6,'.')
 9010 FORMAT ('0FACTOR COUNT EXCEEDS ',I3,'.  N = ',I6,'.')
      END
C
C
      SUBROUTINE MDFTKD(N,FACTOR,DIM,X,Y)
C     ==========================================
C
C
C---- Multi-dimensional complex fourier transform kernel driver
C
C
C     .. Scalar Arguments ..
      INTEGER           N
C     ..
C     .. Array Arguments ..
      REAL              X(*),Y(*)
      INTEGER           DIM(5),FACTOR(10)
C     ..
C     .. Local Scalars ..
      INTEGER           F,M,P,R,S
C     ..
C     .. External Subroutines ..
      EXTERNAL          R2CFTK,R3CFTK,R4CFTK,R5CFTK,R8CFTK,RPCFTK
C     ..
      S = DIM(2)
      F = 0
      M = N
   10 CONTINUE
      F = F + 1
      P = FACTOR(F)
      IF (P.EQ.0) THEN
          RETURN
      ELSE
          M = M/P
          R = M*S
          IF (P.LE.8) THEN
              GO TO (10,20,30,40,50,80,70,60) P
              GO TO 80
C
   20         CONTINUE
              CALL R2CFTK(N,M,X(1),Y(1),X(R+1),Y(R+1),DIM)
              GO TO 10
C
   30         CONTINUE
              CALL R3CFTK(N,M,X(1),Y(1),X(R+1),Y(R+1),X(2*R+1),Y(2*R+1),
     +                    DIM)
              GO TO 10
C
   40         CONTINUE
              CALL R4CFTK(N,M,X(1),Y(1),X(R+1),Y(R+1),X(2*R+1),Y(2*R+1),
     +                    X(3*R+1),Y(3*R+1),DIM)
              GO TO 10
C
   50         CONTINUE
              CALL R5CFTK(N,M,X(1),Y(1),X(R+1),Y(R+1),X(2*R+1),Y(2*R+1),
     +                    X(3*R+1),Y(3*R+1),X(4*R+1),Y(4*R+1),DIM)
              GO TO 10
C
   60         CONTINUE
              CALL R8CFTK(N,M,X(1),Y(1),X(R+1),Y(R+1),X(2*R+1),Y(2*R+1),
     +                    X(3*R+1),Y(3*R+1),X(4*R+1),Y(4*R+1),X(5*R+1),
     +                    Y(5*R+1),X(6*R+1),Y(6*R+1),X(7*R+1),Y(7*R+1),
     +                    DIM)
              GO TO 10
          END IF
C
   70     CONTINUE
          CALL RPCFTK(N,M,P,R,X,Y,DIM)
          GO TO 10
      END IF
C
   80 CONTINUE
      WRITE (6,FMT=9000)
C
C---- Format statements
C
 9000 FORMAT ('0TRANSFER ERROR DETECTED IN MDFTKD',//)
      END
C
C
      SUBROUTINE R2CFTK(N,M,X0,Y0,X1,Y1,DIM)
C     ==============================================
C
C
C---- Radix 2 multi-dimensional complex fourier transform kernel
C
C     .. Scalar Arguments ..
      INTEGER           M,N
C     ..
C     .. Array Arguments ..
      REAL              X0(10),X1(10),Y0(10),Y1(10)
      INTEGER           DIM(5)
C     ..
C     .. Local Scalars ..
      REAL              ANGLE,C,FJM1,FM2,IS,IU,RS,RU,S,TWOPI
      INTEGER           J,K,K0,K1,K2,KK,L,L1,M2,MM2,MOVER2,NS,NT,SEP,
     +                  SIZE
      LOGICAL           FOLD,ZERO
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         COS,REAL,SIN
C     ..
C     .. Data statements ..
      DATA              TWOPI/6.283185/
C     ..
C
      NT = DIM(1)
      SEP = DIM(2)
      L1 = DIM(3)
      SIZE = DIM(4) - 1
      K2 = DIM(5)
      NS = N*SEP
      M2 = M*2
      FM2 = REAL(M2)
      MOVER2 = M/2 + 1
      MM2 = SEP*M2
C
      FJM1 = -1.0
      DO 50 J = 1,MOVER2
          FOLD = J .GT. 1 .AND. 2*J .LT. M + 2
          K0 = (J-1)*SEP + 1
          FJM1 = FJM1 + 1.0
          ANGLE = TWOPI*FJM1/FM2
          ZERO = ANGLE .EQ. 0.0
          IF (.NOT.ZERO) THEN
              C = COS(ANGLE)
              S = SIN(ANGLE)
          END IF
   10     CONTINUE
C
          DO 40 KK = K0,NS,MM2
              DO 30 L = KK,NT,L1
                  K1 = L + SIZE
                  DO 20 K = L,K1,K2
                      RS = X0(K) + X1(K)
                      IS = Y0(K) + Y1(K)
                      RU = X0(K) - X1(K)
                      IU = Y0(K) - Y1(K)
                      X0(K) = RS
                      Y0(K) = IS
                      IF (ZERO) THEN
                          X1(K) = RU
                          Y1(K) = IU
                      ELSE
                          X1(K) = RU*C + IU*S
                          Y1(K) = IU*C - RU*S
                      END IF
   20                 CONTINUE
   30             CONTINUE
   40         CONTINUE
          IF (FOLD) THEN
              FOLD = .FALSE.
              K0 = (M+1-J)*SEP + 1
              C = -C
              GO TO 10
          END IF
   50     CONTINUE
C
      END
C
C
      SUBROUTINE R3CFTK(N,M,X0,Y0,X1,Y1,X2,Y2,DIM)
C     ======================================================
C
C
C---- Radix 3 multi-dimensional complex fourier transform kernel
C
C     .. Scalar Arguments ..
      INTEGER           M,N
C     ..
C     .. Array Arguments ..
      REAL              X0(10),X1(10),X2(10),Y0(10),Y1(10),Y2(10)
      INTEGER           DIM(5)
C     ..
C     .. Local Scalars ..
      REAL              A,ANGLE,B,C1,C2,FJM1,FM3,I0,I1,I2,IA,IB,IS,R0,
     +                  R1,R2,RA,RB,RS,S1,S2,T,TWOPI
      INTEGER           J,K,K0,K1,K2,KK,L,L1,M3,MM3,MOVER2,NS,NT,SEP,
     +                  SIZE
      LOGICAL           FOLD,ZERO
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         COS,REAL,SIN
C     ..
C     .. Data statements ..
      DATA              TWOPI/6.283185/,A/-0.5/,B/0.86602540/
C     ..
C
      NT = DIM(1)
      SEP = DIM(2)
      L1 = DIM(3)
      SIZE = DIM(4) - 1
      K2 = DIM(5)
      NS = N*SEP
      M3 = M*3
      FM3 = REAL(M3)
      MM3 = SEP*M3
      MOVER2 = M/2 + 1
C
      FJM1 = -1.0
      DO 50 J = 1,MOVER2
          FOLD = J .GT. 1 .AND. 2*J .LT. M + 2
          K0 = (J-1)*SEP + 1
          FJM1 = FJM1 + 1.0
          ANGLE = TWOPI*FJM1/FM3
          ZERO = ANGLE .EQ. 0.0
          IF (.NOT.ZERO) THEN
              C1 = COS(ANGLE)
              S1 = SIN(ANGLE)
              C2 = C1*C1 - S1*S1
              S2 = S1*C1 + C1*S1
          END IF
   10     CONTINUE
C
          DO 40 KK = K0,NS,MM3
              DO 30 L = KK,NT,L1
                  K1 = L + SIZE
                  DO 20 K = L,K1,K2
                      R0 = X0(K)
                      I0 = Y0(K)
                      RS = X1(K) + X2(K)
                      IS = Y1(K) + Y2(K)
                      X0(K) = R0 + RS
                      Y0(K) = I0 + IS
                      RA = RS*A + R0
                      IA = IS*A + I0
                      RB = (X1(K)-X2(K))*B
                      IB = (Y1(K)-Y2(K))*B
                      IF (ZERO) THEN
                          X1(K) = RA + IB
                          Y1(K) = IA - RB
                          X2(K) = RA - IB
                          Y2(K) = IA + RB
                      ELSE
                          R1 = RA + IB
                          I1 = IA - RB
                          R2 = RA - IB
                          I2 = IA + RB
                          X1(K) = R1*C1 + I1*S1
                          Y1(K) = I1*C1 - R1*S1
                          X2(K) = R2*C2 + I2*S2
                          Y2(K) = I2*C2 - R2*S2
                      END IF
   20                 CONTINUE
   30             CONTINUE
   40         CONTINUE
          IF (FOLD) THEN
              FOLD = .FALSE.
              K0 = (M+1-J)*SEP + 1
              T = C1*A + S1*B
              S1 = C1*B - S1*A
              C1 = T
              T = C2*A - S2*B
              S2 = -C2*B - S2*A
              C2 = T
              GO TO 10
          END IF
   50     CONTINUE
C
      END
C
C
      SUBROUTINE R4CFTK(N,M,X0,Y0,X1,Y1,X2,Y2,X3,Y3,DIM)
C     ==============================================================
C
C
C---- Radix 4 multi-dimensional complex fourier transform kernel
C
C     .. Scalar Arguments ..
      INTEGER           M,N
C     ..
C     .. Array Arguments ..
      REAL              X0(10),X1(10),X2(10),X3(10),Y0(10),Y1(10),
     +                  Y2(10),Y3(10)
      INTEGER           DIM(5)
C     ..
C     .. Local Scalars ..
      REAL              ANGLE,C1,C2,C3,FJM1,FM4,I1,I2,I3,IS0,IS1,IU0,
     +                  IU1,R1,R2,R3,RS0,RS1,RU0,RU1,S1,S2,S3,T,TWOPI
      INTEGER           J,K,K0,K1,K2,KK,L,L1,M4,MM4,MOVER2,NS,NT,SEP,
     +                  SIZE
      LOGICAL           FOLD,ZERO
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         COS,REAL,SIN
C     ..
C     .. Data statements ..
      DATA              TWOPI/6.283185/
C     ..
C
      NT = DIM(1)
      SEP = DIM(2)
      L1 = DIM(3)
      SIZE = DIM(4) - 1
      K2 = DIM(5)
      NS = N*SEP
      M4 = M*4
      FM4 = REAL(M4)
      MM4 = SEP*M4
      MOVER2 = M/2 + 1
C
      FJM1 = -1.0
      DO 50 J = 1,MOVER2
          FOLD = J .GT. 1 .AND. 2*J .LT. M + 2
          K0 = (J-1)*SEP + 1
          FJM1 = FJM1 + 1.0
          ANGLE = TWOPI*FJM1/FM4
          ZERO = ANGLE .EQ. 0.0
          IF (.NOT.ZERO) THEN
              C1 = COS(ANGLE)
              S1 = SIN(ANGLE)
              C2 = C1*C1 - S1*S1
              S2 = S1*C1 + C1*S1
              C3 = C2*C1 - S2*S1
              S3 = S2*C1 + C2*S1
          END IF
   10     CONTINUE
C
          DO 40 KK = K0,NS,MM4
              DO 30 L = KK,NT,L1
                  K1 = L + SIZE
                  DO 20 K = L,K1,K2
                      RS0 = X0(K) + X2(K)
                      IS0 = Y0(K) + Y2(K)
                      RU0 = X0(K) - X2(K)
                      IU0 = Y0(K) - Y2(K)
                      RS1 = X1(K) + X3(K)
                      IS1 = Y1(K) + Y3(K)
                      RU1 = X1(K) - X3(K)
                      IU1 = Y1(K) - Y3(K)
                      X0(K) = RS0 + RS1
                      Y0(K) = IS0 + IS1
                      IF (ZERO) THEN
                          X2(K) = RU0 + IU1
                          Y2(K) = IU0 - RU1
                          X1(K) = RS0 - RS1
                          Y1(K) = IS0 - IS1
                          X3(K) = RU0 - IU1
                          Y3(K) = IU0 + RU1
                      ELSE
                          R1 = RU0 + IU1
                          I1 = IU0 - RU1
                          R2 = RS0 - RS1
                          I2 = IS0 - IS1
                          R3 = RU0 - IU1
                          I3 = IU0 + RU1
                          X2(K) = R1*C1 + I1*S1
                          Y2(K) = I1*C1 - R1*S1
                          X1(K) = R2*C2 + I2*S2
                          Y1(K) = I2*C2 - R2*S2
                          X3(K) = R3*C3 + I3*S3
                          Y3(K) = I3*C3 - R3*S3
                      END IF
   20                 CONTINUE
   30             CONTINUE
   40         CONTINUE
          IF (FOLD) THEN
              FOLD = .FALSE.
              K0 = (M+1-J)*SEP + 1
              T = C1
              C1 = S1
              S1 = T
              C2 = -C2
              T = C3
              C3 = -S3
              S3 = -T
              GO TO 10
          END IF
   50     CONTINUE
C
      END
C
C
      SUBROUTINE R5CFTK(N,M,X0,Y0,X1,Y1,X2,Y2,X3,Y3,X4,Y4,DIM)
C     =================================================================
C
C
C---- Radix 5 multi-dimensional complex fourier transform kernel
C
C     .. Scalar Arguments ..
      INTEGER           M,N
C     ..
C     .. Array Arguments ..
      REAL              X0(10),X1(10),X2(10),X3(10),X4(10),Y0(10),
     +                  Y1(10),Y2(10),Y3(10),Y4(10)
      INTEGER           DIM(5)
C     ..
C     .. Local Scalars ..
      REAL              A1,A2,ANGLE,B1,B2,C1,C2,C3,C4,FJM1,FM5,I0,I1,I2,
     +                  I3,I4,IA1,IA2,IB1,IB2,IS1,IS2,IU1,IU2,R0,R1,R2,
     +                  R3,R4,RA1,RA2,RB1,RB2,RS1,RS2,RU1,RU2,S1,S2,S3,
     +                  S4,T,TWOPI
      INTEGER           J,K,K0,K1,K2,KK,L,L1,M5,MM5,MOVER2,NS,NT,SEP,
     +                  SIZE
      LOGICAL           FOLD,ZERO
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         COS,REAL,SIN
C     ..
C     .. Data statements ..
      DATA              TWOPI/6.283185/,A1/0.30901699/,B1/0.95105652/,
     +                  A2/-0.80901699/,B2/0.58778525/
C     ..
C
      NT = DIM(1)
      SEP = DIM(2)
      L1 = DIM(3)
      SIZE = DIM(4) - 1
      K2 = DIM(5)
      NS = N*SEP
      M5 = M*5
      FM5 = REAL(M5)
      MM5 = SEP*M5
      MOVER2 = M/2 + 1
C
      FJM1 = -1.0
      DO 50 J = 1,MOVER2
          FOLD = J .GT. 1 .AND. 2*J .LT. M + 2
          K0 = (J-1)*SEP + 1
          FJM1 = FJM1 + 1.0
          ANGLE = TWOPI*FJM1/FM5
          ZERO = ANGLE .EQ. 0.0
          IF (.NOT.ZERO) THEN
              C1 = COS(ANGLE)
              S1 = SIN(ANGLE)
              C2 = C1*C1 - S1*S1
              S2 = S1*C1 + C1*S1
              C3 = C2*C1 - S2*S1
              S3 = S2*C1 + C2*S1
              C4 = C2*C2 - S2*S2
              S4 = S2*C2 + C2*S2
          END IF
   10     CONTINUE
C
          DO 40 KK = K0,NS,MM5
              DO 30 L = KK,NT,L1
                  K1 = L + SIZE
                  DO 20 K = L,K1,K2
                      R0 = X0(K)
                      I0 = Y0(K)
                      RS1 = X1(K) + X4(K)
                      IS1 = Y1(K) + Y4(K)
                      RU1 = X1(K) - X4(K)
                      IU1 = Y1(K) - Y4(K)
                      RS2 = X2(K) + X3(K)
                      IS2 = Y2(K) + Y3(K)
                      RU2 = X2(K) - X3(K)
                      IU2 = Y2(K) - Y3(K)
                      X0(K) = R0 + RS1 + RS2
                      Y0(K) = I0 + IS1 + IS2
                      RA1 = RS1*A1 + R0 + RS2*A2
                      IA1 = IS1*A1 + I0 + IS2*A2
                      RA2 = RS1*A2 + R0 + RS2*A1
                      IA2 = IS1*A2 + I0 + IS2*A1
                      RB1 = RU1*B1 + RU2*B2
                      IB1 = IU1*B1 + IU2*B2
                      RB2 = RU1*B2 - RU2*B1
                      IB2 = IU1*B2 - IU2*B1
                      IF (ZERO) THEN
                          X1(K) = RA1 + IB1
                          Y1(K) = IA1 - RB1
                          X2(K) = RA2 + IB2
                          Y2(K) = IA2 - RB2
                          X3(K) = RA2 - IB2
                          Y3(K) = IA2 + RB2
                          X4(K) = RA1 - IB1
                          Y4(K) = IA1 + RB1
                      ELSE
                          R1 = RA1 + IB1
                          I1 = IA1 - RB1
                          R2 = RA2 + IB2
                          I2 = IA2 - RB2
                          R3 = RA2 - IB2
                          I3 = IA2 + RB2
                          R4 = RA1 - IB1
                          I4 = IA1 + RB1
                          X1(K) = R1*C1 + I1*S1
                          Y1(K) = I1*C1 - R1*S1
                          X2(K) = R2*C2 + I2*S2
                          Y2(K) = I2*C2 - R2*S2
                          X3(K) = R3*C3 + I3*S3
                          Y3(K) = I3*C3 - R3*S3
                          X4(K) = R4*C4 + I4*S4
                          Y4(K) = I4*C4 - R4*S4
                      END IF
   20                 CONTINUE
   30             CONTINUE
   40         CONTINUE
          IF (FOLD) THEN
              FOLD = .FALSE.
              K0 = (M+1-J)*SEP + 1
              T = C1*A1 + S1*B1
              S1 = C1*B1 - S1*A1
              C1 = T
              T = C2*A2 + S2*B2
              S2 = C2*B2 - S2*A2
              C2 = T
              T = C3*A2 - S3*B2
              S3 = -C3*B2 - S3*A2
              C3 = T
              T = C4*A1 - S4*B1
              S4 = -C4*B1 - S4*A1
              C4 = T
              GO TO 10
          END IF
   50     CONTINUE
C
      END
C
C
      SUBROUTINE R8CFTK(N,M,X0,Y0,X1,Y1,X2,Y2,X3,Y3,X4,Y4,X5,Y5,X6,Y6,
     +                  X7,Y7,DIM)
C      ===============================================================
C
C
C---- Radix 8 multi-dimensional complex fourier transform kernel
C
C     .. Scalar Arguments ..
      INTEGER           M,N
C     ..
C     .. Array Arguments ..
      REAL              X0(10),X1(10),X2(10),X3(10),X4(10),X5(10),
     +                  X6(10),X7(10),Y0(10),Y1(10),Y2(10),Y3(10),
     +                  Y4(10),Y5(10),Y6(10),Y7(10)
      INTEGER           DIM(5)
C     ..
C     .. Local Scalars ..
      REAL              ANGLE,C1,C2,C3,C4,C5,C6,C7,E,FJM1,FM8,I1,I2,I3,
     +                  I4,I5,I6,I7,IS0,IS1,IS2,IS3,ISS0,ISS1,ISU0,ISU1,
     +                  IU0,IU1,IU2,IU3,IUS0,IUS1,IUU0,IUU1,R1,R2,R3,R4,
     +                  R5,R6,R7,RS0,RS1,RS2,RS3,RSS0,RSS1,RSU0,RSU1,
     +                  RU0,RU1,RU2,RU3,RUS0,RUS1,RUU0,RUU1,S1,S2,S3,S4,
     +                  S5,S6,S7,T,TWOPI
      INTEGER           J,K,K0,K1,K2,KK,L,L1,M8,MM8,MOVER2,NS,NT,SEP,
     +                  SIZE
      LOGICAL           FOLD,ZERO
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         COS,REAL,SIN
C     ..
C     .. Data statements ..
      DATA              TWOPI/6.283185/,E/0.70710678/
C     ..
C
      NT = DIM(1)
      SEP = DIM(2)
      L1 = DIM(3)
      SIZE = DIM(4) - 1
      K2 = DIM(5)
      NS = N*SEP
      M8 = M*8
      FM8 = REAL(M8)
      MM8 = SEP*M8
      MOVER2 = M/2 + 1
C
      FJM1 = -1.0
      DO 50 J = 1,MOVER2
          FOLD = J .GT. 1 .AND. 2*J .LT. M + 2
          K0 = (J-1)*SEP + 1
          FJM1 = FJM1 + 1.0
          ANGLE = TWOPI*FJM1/FM8
          ZERO = ANGLE .EQ. 0.0
          IF (.NOT.ZERO) THEN
              C1 = COS(ANGLE)
              S1 = SIN(ANGLE)
              C2 = C1*C1 - S1*S1
              S2 = S1*C1 + C1*S1
              C3 = C2*C1 - S2*S1
              S3 = S2*C1 + C2*S1
              C4 = C2*C2 - S2*S2
              S4 = S2*C2 + C2*S2
              C5 = C4*C1 - S4*S1
              S5 = S4*C1 + C4*S1
              C6 = C4*C2 - S4*S2
              S6 = S4*C2 + C4*S2
              C7 = C4*C3 - S4*S3
              S7 = S4*C3 + C4*S3
          END IF
   10     CONTINUE
C
          DO 40 KK = K0,NS,MM8
              DO 30 L = KK,NT,L1
                  K1 = L + SIZE
                  DO 20 K = L,K1,K2
                      RS0 = X0(K) + X4(K)
                      IS0 = Y0(K) + Y4(K)
                      RU0 = X0(K) - X4(K)
                      IU0 = Y0(K) - Y4(K)
                      RS1 = X1(K) + X5(K)
                      IS1 = Y1(K) + Y5(K)
                      RU1 = X1(K) - X5(K)
                      IU1 = Y1(K) - Y5(K)
                      RS2 = X2(K) + X6(K)
                      IS2 = Y2(K) + Y6(K)
                      RU2 = X2(K) - X6(K)
                      IU2 = Y2(K) - Y6(K)
                      RS3 = X3(K) + X7(K)
                      IS3 = Y3(K) + Y7(K)
                      RU3 = X3(K) - X7(K)
                      IU3 = Y3(K) - Y7(K)
                      RSS0 = RS0 + RS2
                      ISS0 = IS0 + IS2
                      RSU0 = RS0 - RS2
                      ISU0 = IS0 - IS2
                      RSS1 = RS1 + RS3
                      ISS1 = IS1 + IS3
                      RSU1 = RS1 - RS3
                      ISU1 = IS1 - IS3
                      RUS0 = RU0 - IU2
                      IUS0 = IU0 + RU2
                      RUU0 = RU0 + IU2
                      IUU0 = IU0 - RU2
                      RUS1 = RU1 - IU3
                      IUS1 = IU1 + RU3
                      RUU1 = RU1 + IU3
                      IUU1 = IU1 - RU3
                      T = (RUS1+IUS1)*E
                      IUS1 = (IUS1-RUS1)*E
                      RUS1 = T
                      T = (RUU1+IUU1)*E
                      IUU1 = (IUU1-RUU1)*E
                      RUU1 = T
                      X0(K) = RSS0 + RSS1
                      Y0(K) = ISS0 + ISS1
                      IF (ZERO) THEN
                          X4(K) = RUU0 + RUU1
                          Y4(K) = IUU0 + IUU1
                          X2(K) = RSU0 + ISU1
                          Y2(K) = ISU0 - RSU1
                          X6(K) = RUS0 + IUS1
                          Y6(K) = IUS0 - RUS1
                          X1(K) = RSS0 - RSS1
                          Y1(K) = ISS0 - ISS1
                          X5(K) = RUU0 - RUU1
                          Y5(K) = IUU0 - IUU1
                          X3(K) = RSU0 - ISU1
                          Y3(K) = ISU0 + RSU1
                          X7(K) = RUS0 - IUS1
                          Y7(K) = IUS0 + RUS1
                      ELSE
                          R1 = RUU0 + RUU1
                          I1 = IUU0 + IUU1
                          R2 = RSU0 + ISU1
                          I2 = ISU0 - RSU1
                          R3 = RUS0 + IUS1
                          I3 = IUS0 - RUS1
                          R4 = RSS0 - RSS1
                          I4 = ISS0 - ISS1
                          R5 = RUU0 - RUU1
                          I5 = IUU0 - IUU1
                          R6 = RSU0 - ISU1
                          I6 = ISU0 + RSU1
                          R7 = RUS0 - IUS1
                          I7 = IUS0 + RUS1
                          X4(K) = R1*C1 + I1*S1
                          Y4(K) = I1*C1 - R1*S1
                          X2(K) = R2*C2 + I2*S2
                          Y2(K) = I2*C2 - R2*S2
                          X6(K) = R3*C3 + I3*S3
                          Y6(K) = I3*C3 - R3*S3
                          X1(K) = R4*C4 + I4*S4
                          Y1(K) = I4*C4 - R4*S4
                          X5(K) = R5*C5 + I5*S5
                          Y5(K) = I5*C5 - R5*S5
                          X3(K) = R6*C6 + I6*S6
                          Y3(K) = I6*C6 - R6*S6
                          X7(K) = R7*C7 + I7*S7
                          Y7(K) = I7*C7 - R7*S7
                      END IF
   20                 CONTINUE
   30             CONTINUE
   40         CONTINUE
          IF (FOLD) THEN
              FOLD = .FALSE.
              K0 = (M+1-J)*SEP + 1
              T = (C1+S1)*E
              S1 = (C1-S1)*E
              C1 = T
              T = S2
              S2 = C2
              C2 = T
              T = (-C3+S3)*E
              S3 = (C3+S3)*E
              C3 = T
              C4 = -C4
              T = - (C5+S5)*E
              S5 = (-C5+S5)*E
              C5 = T
              T = -S6
              S6 = -C6
              C6 = T
              T = (C7-S7)*E
              S7 = - (C7+S7)*E
              C7 = T
              GO TO 10
          END IF
   50     CONTINUE
C
      END
C
C
      SUBROUTINE RPCFTK(N,M,P,R,X,Y,DIM)
C     ==========================================
C
C
C---- Radix prime multi-dimensional complex fourier transform kernel
C
C     .. Scalar Arguments ..
      INTEGER           M,N,P,R
C     ..
C     .. Array Arguments ..
      REAL              X(R,P),Y(R,P)
      INTEGER           DIM(5)
C     ..
C     .. Local Scalars ..
      REAL              ANGLE,FJM1,FMP,FP,FU,IS,IU,RS,RU,T,TWOPI,XT,YT
      INTEGER           J,JJ,K,K0,K1,K2,KK,L,L1,MMP,MOVER2,MP,NS,NT,PM,
     +                  PP,SEP,SIZE,U,V
      LOGICAL           FOLD,ZERO
C     ..
C     .. Local Arrays ..
      REAL              A(18),AA(9,9),B(18),BB(9,9),C(18),IA(9),IB(9),
     +                  RA(9),RB(9),S(18)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         COS,REAL,SIN
C     ..
C     .. Data statements ..
      DATA              TWOPI/6.283185/
C     ..
C
C
      NT = DIM(1)
      SEP = DIM(2)
      L1 = DIM(3)
      SIZE = DIM(4) - 1
      K2 = DIM(5)
      NS = N*SEP
      MOVER2 = M/2 + 1
      MP = M*P
      FMP = REAL(MP)
      MMP = SEP*MP
      PP = P/2
      PM = P - 1
      FP = REAL(P)
      FU = 0.0
      DO 10 U = 1,PP
          FU = FU + 1.0
          ANGLE = TWOPI*FU/FP
          JJ = P - U
          A(U) = COS(ANGLE)
          B(U) = SIN(ANGLE)
          A(JJ) = A(U)
          B(JJ) = -B(U)
   10     CONTINUE
      DO 30 U = 1,PP
          DO 20 V = 1,PP
              JJ = U*V - U*V/P*P
              AA(V,U) = A(JJ)
              BB(V,U) = B(JJ)
   20         CONTINUE
   30     CONTINUE
C
      FJM1 = -1.0
      DO 140 J = 1,MOVER2
          FOLD = J .GT. 1 .AND. 2*J .LT. M + 2
          K0 = (J-1)*SEP + 1
          FJM1 = FJM1 + 1.0
          ANGLE = TWOPI*FJM1/FMP
          ZERO = ANGLE .EQ. 0.0
          IF (.NOT.ZERO) THEN
              C(1) = COS(ANGLE)
              S(1) = SIN(ANGLE)
              DO 40 U = 2,PM
                  C(U) = C(U-1)*C(1) - S(U-1)*S(1)
                  S(U) = S(U-1)*C(1) + C(U-1)*S(1)
   40             CONTINUE
          END IF
   50     CONTINUE
C
          DO 120 KK = K0,NS,MMP
              DO 110 L = KK,NT,L1
                  K1 = L + SIZE
                  DO 100 K = L,K1,K2
                      XT = X(K,1)
                      YT = Y(K,1)
                      RS = X(K,2) + X(K,P)
                      IS = Y(K,2) + Y(K,P)
                      RU = X(K,2) - X(K,P)
                      IU = Y(K,2) - Y(K,P)
                      DO 60 U = 1,PP
                          RA(U) = AA(U,1)*RS + XT
                          IA(U) = AA(U,1)*IS + YT
                          RB(U) = BB(U,1)*RU
                          IB(U) = BB(U,1)*IU
   60                     CONTINUE
                      XT = XT + RS
                      YT = YT + IS
                      DO 80 U = 2,PP
                          JJ = P - U
                          RS = X(K,U+1) + X(K,JJ+1)
                          IS = Y(K,U+1) + Y(K,JJ+1)
                          RU = X(K,U+1) - X(K,JJ+1)
                          IU = Y(K,U+1) - Y(K,JJ+1)
                          XT = XT + RS
                          YT = YT + IS
                          DO 70 V = 1,PP
                              RA(V) = AA(V,U)*RS + RA(V)
                              IA(V) = AA(V,U)*IS + IA(V)
                              RB(V) = BB(V,U)*RU + RB(V)
                              IB(V) = BB(V,U)*IU + IB(V)
   70                         CONTINUE
   80                     CONTINUE
                      X(K,1) = XT
                      Y(K,1) = YT
                      DO 90 U = 1,PP
                          JJ = P - U
                          IF (ZERO) THEN
                              X(K,U+1) = RA(U) + IB(U)
                              Y(K,U+1) = IA(U) - RB(U)
                              X(K,JJ+1) = RA(U) - IB(U)
                              Y(K,JJ+1) = IA(U) + RB(U)
                          ELSE
                              XT = RA(U) + IB(U)
                              YT = IA(U) - RB(U)
                              X(K,U+1) = C(U)*XT + S(U)*YT
                              Y(K,U+1) = C(U)*YT - S(U)*XT
                              XT = RA(U) - IB(U)
                              YT = IA(U) + RB(U)
                              X(K,JJ+1) = C(JJ)*XT + S(JJ)*YT
                              Y(K,JJ+1) = C(JJ)*YT - S(JJ)*XT
                          END IF
   90                     CONTINUE
  100                 CONTINUE
  110             CONTINUE
  120         CONTINUE
          IF (FOLD) THEN
              FOLD = .FALSE.
              K0 = (M+1-J)*SEP + 1
              DO 130 U = 1,PM
                  T = C(U)*A(U) + S(U)*B(U)
                  S(U) = -S(U)*A(U) + C(U)*B(U)
                  C(U) = T
  130             CONTINUE
              GO TO 50
          END IF
  140     CONTINUE
C
      END
C
C
      SUBROUTINE HERMFT(X,Y,N,DIM)
C     ==================================
C
C
C
C---- Hermitian symmetric fourier transform
C
C     Given the unique terms of a hermitian symmetric sequence of length
C     2N this subroutine calculates the 2N real numbers which are its
C     fourier transform.  The even numbered elements of the transform
C     (0, 2, 4, . . ., 2n-2) are returned in X and the odd numbered
C     elements (1, 3, 5, . . ., 2n-1) in Y.
C
C     A finite hermitian sequence of length 2n contains n + 1 unique
C     real numbers and n - 1 unique imaginary numbers.  For convenience
C     the real value for X(n) is stored at Y(0).
C
C
C     .. Scalar Arguments ..
      INTEGER           N
C     ..
C     .. Array Arguments ..
      REAL              X(*),Y(*)
      INTEGER           DIM(5)
C     ..
C     .. Local Scalars ..
      REAL              A,ANGLE,B,C,CO,D,E,F,SI,TWON,TWOPI
      INTEGER           D2,D3,D4,D5,I,I0,I1,I2,J,K,K1,NOVER2,NT
C     ..
C     .. External Subroutines ..
      EXTERNAL          CMPLFT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         COS,REAL,SIN
C     ..
      TWOPI = 6.283185
      TWON = REAL(2*N)
C
      NT = DIM(1)
      D2 = DIM(2)
      D3 = DIM(3)
      D4 = DIM(4) - 1
      D5 = DIM(5)
C
      DO 20 I0 = 1,NT,D3
          I1 = I0 + D4
          DO 10 I = I0,I1,D5
              A = X(I)
              B = Y(I)
              X(I) = A + B
              Y(I) = A - B
   10         CONTINUE
   20     CONTINUE
C
      NOVER2 = N/2 + 1
      IF (NOVER2.GE.2) THEN
          DO 50 I0 = 2,NOVER2
              ANGLE = REAL(I0-1)*TWOPI/TWON
              CO = COS(ANGLE)
              SI = SIN(ANGLE)
              K = (N+2-2*I0)*D2
              K1 = (I0-1)*D2 + 1
              DO 40 I1 = K1,NT,D3
                  I2 = I1 + D4
                  DO 30 I = I1,I2,D5
                      J = I + K
                      A = X(I) + X(J)
                      B = X(I) - X(J)
                      C = Y(I) + Y(J)
                      D = Y(I) - Y(J)
                      E = B*CO + C*SI
                      F = B*SI - C*CO
                      X(I) = A + F
                      X(J) = A - F
                      Y(I) = E + D
                      Y(J) = E - D
   30                 CONTINUE
   40             CONTINUE
   50         CONTINUE
C
          CALL CMPLFT(X,Y,N,DIM)
      END IF
C
C
      END
C
C
      SUBROUTINE INV21(X,Y,N,D)
C      =========================
C
C
C
C---- Inverts fourier transform along a screw
C     diad. the result is scaled by n.
C
C
C     .. Scalar Arguments ..
      INTEGER          N
C     ..
C     .. Array Arguments ..
      REAL             X(*),Y(*)
      INTEGER          D(5)
C     ..
C     .. Local Scalars ..
      REAL             A,B,C,C1,PI,R,S,S1
      INTEGER          D1,D2,D3,D4,D5,I,J,J1,J2,J3,K,KK,L,LL,M,NOVER2
C     ..
C     .. External Subroutines ..
      EXTERNAL         CMPLFT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC        COS,REAL,SIN
C     ..
      PI = 3.141593
C
      D1 = D(1)
      D2 = D(2)
      D3 = D(3)
      D4 = D(4) - 1
      D5 = D(5)
C
      NOVER2 = N/2
      LL = N*D2
      KK = NOVER2*D2
      DO 20 J1 = 1,D1,D3
          J2 = J1 + D4
          DO 10 J = J1,J2,D5
              L = LL + J
              K = KK + J
              X(L) = X(J) + X(K)
              X(K) = X(K) + Y(K)
              Y(L) = 0.0
              Y(K) = 0.0
   10         CONTINUE
   20     CONTINUE
C
      C1 = COS(PI/REAL(N))
      S1 = SIN(PI/REAL(N))
      C = 1.0
      S = 0.0
      DO 50 I = 2,NOVER2
          KK = (N+2-2*I)*D2
          LL = (N+1-I)*D2
          R = C*C1 - S*S1
          S = C*S1 + S*C1
          C = R
          J1 = (I-1)*D2 + 1
          DO 40 J2 = J1,D1,D3
              J3 = J2 + D4
              DO 30 J = J2,J3,D5
                  L = J + LL
                  K = J + KK
                  X(L) = X(L) + X(J) + X(K)
                  X(J) = Y(J)*S + X(J)
                  X(K) = Y(K)*S + X(K)
                  Y(J) = Y(J)*C
                  Y(K) = -Y(K)*C
   30             CONTINUE
   40         CONTINUE
   50     CONTINUE
C
      CALL CMPLFT(X,Y,N,D)
C
      DO 80 I = 1,NOVER2
          KK = (N+1-2*I)*D2
          LL = I*D2 + KK
          J1 = (I-1)*D2 + 1
          DO 70 J2 = J1,D1,D3
              J3 = J2 + D4
              DO 60 J = J2,J3,D5
                  K = J + KK
                  L = J + LL
                  A = X(J) - X(L)
                  B = Y(J) + Y(L)
                  X(J) = X(L)
                  Y(J) = -Y(L)
                  X(L) = X(K) + A
                  Y(L) = Y(K) - B
                  X(K) = A
                  Y(K) = B
   60             CONTINUE
   70         CONTINUE
   80     CONTINUE
C
      M = N - 2
      DO 130 I = 1,M
          K = I
   90     CONTINUE
          J = K
          K = J/2
          IF (2*K.NE.J) K = N - 1 - K
          IF (K-I) 90,130,100
  100     KK = (K-I)*D2
          J1 = I*D2 + 1
          DO 120 J2 = J1,D1,D3
              J3 = J2 + D4
              DO 110 J = J2,J3,D5
                  K = J + KK
                  A = X(K)
                  B = Y(K)
                  X(K) = X(J)
                  Y(K) = Y(J)
                  X(J) = A
                  Y(J) = B
  110             CONTINUE
  120         CONTINUE
  130     CONTINUE
C
C
      END
C
C
      SUBROUTINE REALFT(EVEN,ODD,N,DIM)
C      ======================================
C
C
C
C     REAL FOURIER TRANSFORM
C
C     Given a real sequence of length 2n this subroutine calculates the
C     unique part of the fourier transform.  The fourier transform has
C     n + 1 unique real parts and n - 1 unique imaginary parts.  Since
C     the real part at x(n) is frequently of interest, this subroutine
C     stores it at x(n) rather than in y(0).  Therefore x and y must be
C     of length n + 1 instead of n.  Note that this storage arrangement
C     is different from that employed by the hermitian fourier transform
C     subroutine.
C
C     For convenience the data is presented in two parts, the first
C     containing the even numbered real terms and the second containing
C     the odd numbered terms (numbering starting at 0).  On return the
C     real part of the transform replaces the even terms and the
C     imaginary part of the transform replaces the odd terms.
C
C
C     .. Scalar Arguments ..
      INTEGER           N
C     ..
C     .. Array Arguments ..
      REAL              EVEN(1),ODD(1)
      INTEGER           DIM(5)
C     ..
C     .. Local Scalars ..
      REAL              A,ANGLE,B,C,CO,D,E,F,SI,TWON,TWOPI
      INTEGER           D2,D3,D4,D5,I,I0,I1,I2,J,K,L,NOVER2,NT
C     ..
C     .. External Subroutines ..
      EXTERNAL          CMPLFT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         COS,REAL,SIN
C     ..
      TWOPI = 6.283185
      TWON = REAL(2*N)
C
      CALL CMPLFT(EVEN,ODD,N,DIM)
C
      NT = DIM(1)
      D2 = DIM(2)
      D3 = DIM(3)
      D4 = DIM(4) - 1
      D5 = DIM(5)
      NOVER2 = N/2 + 1
C
      IF (NOVER2.GE.2) THEN
          DO 30 I = 2,NOVER2
              ANGLE = REAL(I-1)*TWOPI/TWON
              CO = COS(ANGLE)
              SI = SIN(ANGLE)
              I0 = (I-1)*D2 + 1
              J = (N+2-2*I)*D2
              DO 20 I1 = I0,NT,D3
                  I2 = I1 + D4
                  DO 10 K = I1,I2,D5
                      L = K + J
                      A = (EVEN(L)+EVEN(K))/2.0
                      C = (EVEN(L)-EVEN(K))/2.0
                      B = (ODD(L)+ODD(K))/2.0
                      D = (ODD(L)-ODD(K))/2.0
                      E = C*SI + B*CO
                      F = C*CO - B*SI
                      EVEN(K) = A + E
                      EVEN(L) = A - E
                      ODD(K) = F - D
                      ODD(L) = F + D
   10                 CONTINUE
   20             CONTINUE
   30         CONTINUE
      END IF
C
      IF (N.GE.1) THEN
          J = N*D2
          DO 50 I1 = 1,NT,D3
              I2 = I1 + D4
              DO 40 K = I1,I2,D5
                  L = K + J
                  EVEN(L) = EVEN(K) - ODD(K)
                  ODD(L) = 0.0
                  EVEN(K) = EVEN(K) + ODD(K)
                  ODD(K) = 0.0
   40             CONTINUE
   50         CONTINUE
      END IF
C
C
      END
C
C
      SUBROUTINE RSYMFT(X,N,DIM)
C      ===============================
C
C
C
C     REAL SYMMETRIC MULTIDIMENSIONAL FOURIER TRANSFORM
C
C     N must be a multiple of 4.  The two unique elements are stored at
C     X(1) and X(n+1).
C
C     Decimation in frequency applied to a real symmetric sequence of
C     length 2n gives a real symmetric sequence of length n, the
C     transform of which gives the even numbered fourier coefficients,
C     and a hermitian symmetric sequence of length n, the transform of
C     which gives the odd numbered fourier coefficients.  The sum of
C     the two sequences is a hermitian symmetric sequence of length n,
C     which may be stored in n/2 complex locations.  The transform of
C     this sequence is n real numbers representing the term by term sum
C     of the even and odd numbered fourier coefficients.  This symmetric
C     sequence may be solved if any of the fourier coefficients are
C     known.  For this purpose x0, which is simply the sum of the
C     original sequence, is computed and saved in x(n+1).
C
C
C     .. Scalar Arguments ..
      INTEGER           N
C     ..
C     .. Array Arguments ..
      REAL              X(*)
      INTEGER           DIM(5)
C     ..
C     .. Local Scalars ..
      REAL              A,ANGLE,B,C,CO,D,SI,TWON,TWOPI
      INTEGER           D1,D2,D3,D4,D5,I,I0,I1,I2,II,J,J0,J1,J2,K,K0,K1,
     +                  K2,L,M,MJ,MK,ML,MM,MOD,N2,NN,NOVER2,NOVER4,TEST,
     +                  TWOD2
C     ..
C     .. External Subroutines ..
      EXTERNAL          HERMFT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         COS,REAL,SIN
C     ..
      IF (N.NE.1) THEN
          NOVER2 = N/2
          NOVER4 = N/4
          IF (4*NOVER4.NE.N) THEN
C
              WRITE (6,FMT=9000) N
              call ccperr(1,' stop in fftlib.for 2000')
          ELSE
              D1 = DIM(1)
              D2 = DIM(2)
              D3 = DIM(3)
              D4 = DIM(4) - 1
              D5 = DIM(5)
              TWOPI = 6.283185
              TWON = REAL(2*N)
              TWOD2 = 2*D2
C
              K0 = N*D2 + 1
              DO 20 K1 = K0,D1,D3
                  K2 = K1 + D4
                  DO 10 K = K1,K2,D5
                      X(K) = X(K)/2.0
   10                 CONTINUE
   20             CONTINUE
C
              DO 50 I = 2,NOVER2
                  ANGLE = REAL(I-1)*TWOPI/TWON
                  CO = COS(ANGLE)
                  SI = SIN(ANGLE)
                  K0 = (I-1)*D2 + 1
                  J0 = (N+2-2*I)*D2
                  J1 = (N+1-I)*D2
                  DO 40 K1 = K0,D1,D3
                      K2 = K1 + D4
                      DO 30 K = K1,K2,D5
                          L = K + J0
                          NN = K + J1
                          A = X(L) + X(K)
                          B = X(L) - X(K)
                          X(K) = A - B*CO
                          X(L) = B*SI
                          X(NN) = X(NN) + A
   30                     CONTINUE
   40                 CONTINUE
   50             CONTINUE
C
              IF (NOVER4.NE.1) THEN
                  J0 = NOVER4 - 1
                  DO 80 I = 1,J0
                      K0 = (NOVER2+I)*D2 + 1
                      J1 = (NOVER2-2*I)*D2
                      DO 70 K1 = K0,D1,D3
                          K2 = K1 + D4
                          DO 60 K = K1,K2,D5
                              L = K + J1
                              A = X(K)
                              X(K) = X(L)
                              X(L) = A
   60                         CONTINUE
   70                     CONTINUE
   80                 CONTINUE
              END IF
C
              J0 = NOVER2*D2
              J1 = N*D2
              DO 100 K1 = 1,D1,D3
                  K2 = K1 + D4
                  DO 90 K = K1,K2,D5
                      I = K + J0
                      L = K + J1
                      X(I) = X(I)*2.0
                      X(L) = X(K) + X(I) + X(L)*2.0
                      X(K) = X(K)*2.0
   90                 CONTINUE
  100             CONTINUE
C
              K = NOVER2*D2 + 1
              CALL HERMFT(X(1),X(K),NOVER2,DIM)
C
C---- Solve the equations for all of the sequences
C
              I0 = 1 - D2
              MK = NOVER2*D2
              MJ = MK + D2
              ML = N*D2 + D2
              MM = ML
              DO 130 II = 1,NOVER4
                  I0 = I0 + D2
                  MJ = MJ - TWOD2
                  ML = ML - TWOD2
                  MM = MM - D2
                  DO 120 I1 = I0,D1,D3
                      I2 = I1 + D4
                      DO 110 I = I1,I2,D5
                          J = I + MJ
                          K = I + MK
                          L = I + ML
                          M = I + MM
                          A = X(I) - X(M)
                          B = X(L) - A
                          C = X(K) - B
                          D = X(J) - C
                          X(I) = X(M)
                          X(J) = A
                          X(K) = B
                          X(L) = C
                          X(M) = D
  110                     CONTINUE
  120                 CONTINUE
  130             CONTINUE
C
C---- The results are now in a scrambled digit reversed order, i.e.
C     x(1), x(5), x(9), ..., x(10), x(6), x(2), ..., x(3), x(7), x(11),
C     ..., x(12), x(8), x(4).  the following section of program follows
C     the permutation cycles and does the necessary interchanges.
C
              IF (NOVER4.NE.1) THEN
                  NN = N - 2
                  DO 170 I = 1,NN
                      K = I
  140                 CONTINUE
C
                      K0 = K/4
                      L = K - K0*4
                      IF (L.NE. (L/2)*2) K0 = NOVER4 - 1 - K0
                      K = L*NOVER4 + K0
                      IF (K.LT.I) GO TO 140
                      IF (K.NE.I) THEN
C
                          K0 = I*D2 + 1
                          J0 = (K-I)*D2
                          DO 160 K1 = K0,D1,D3
                              K2 = K1 + D4
                              DO 150 K = K1,K2,D5
                                  L = K + J0
                                  A = X(K)
                                  X(K) = X(L)
                                  X(L) = A
  150                             CONTINUE
  160                         CONTINUE
                      END IF
  170                 CONTINUE
              END IF
          END IF
      END IF
C
C---- Format statements
C
 9000 FORMAT ('0N NOT A MULTIPLE OF 4 IN R SYM FT.  N =',I10,//)
      END
C
C
      SUBROUTINE SDIAD(X,Y,N,DIM)
C      ===============================
C
C
C
C     This subroutine computes half the fourier synthesis along a screw
C     diad lying along a crystallographic axis given half the fourier
C     coefficients.  That is, it assumes that f(t) = conjg(f(-t)) for t
C     even and f(t) = -conjg(f(-t)) for t odd.  n is the length of the
C     desired half of the transform.  The location x(n+1) is required as
C     a scratch location and therefore a value is also returned in
C     x(n+1) and y(n+1).  The value of the second half of the transform
C     may be generated from the first half by the formula x(n+t) = x(t),
C     y(n+t) = -y(t).  In other words, the last half of the transform is
C     the complex conjugate of the first half.
C
C     The transform is calculated by forming the sum of the even terms
C     and the odd terms in place, using the symmetry relations to
C     obtain the values for negative subscripts.  The transform of the
C     resulting sequence may be separated by using the fact that the
C     transform of the even terms is real, while the prodct of the
C     transform of the odd terms and (cos(pi*t/n) - i*sin(pi*t/n)) is
C     imaginary.  The scratch location is required because the formula
C     for separating the two transforms breaks down when t = n/2.
C
C
C Corrections from A.D.MCLACHLAN 1980, put here sep 1985
C errors in original algorithm for the scratch location which
C assumed f(n)=0
C
C
C
C     .. Scalar Arguments ..
      INTEGER          N
C     ..
C     .. Array Arguments ..
      REAL             X(*),Y(*)
      INTEGER          DIM(5)
C     ..
C     .. Local Scalars ..
      REAL             A,ANGLE,C,S,TWON,TWOPI
      INTEGER          D1,D2,D3,D4,D5,I,J,K,K0,K1,K2,L,M,MM,MN,NN,NOVER2
      LOGICAL          FOLD
C     ..
C     .. External Subroutines ..
      EXTERNAL         CMPLFT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC        COS,REAL,SIN
C     ..
      NOVER2 = N/2
      IF (2*NOVER2.NE.N) THEN
C
          WRITE (6,FMT=9000) N
          call ccperr(1,' stop in fftlib.for 3000')
      ELSE
          TWON = REAL(2*N)
          TWOPI = 6.2831852
          D1 = DIM(1)
          D2 = DIM(2)
          D3 = DIM(3)
          D4 = DIM(4) - 1
          D5 = DIM(5)
C
          S = -1.0
          IF (NOVER2.EQ. (2* (NOVER2/2))) S = -S
          K0 = (N-1)*D2 + 1
          DO 20 K1 = K0,D1,D3
              K2 = K1 + D4
              DO 10 K = K1,K2,D5
                  L = K + D2
                  Y(L) = X(K)*S
   10             CONTINUE
   20         CONTINUE
          S = 1.0
          NN = N - 2
          DO 50 I = 1,NN,2
              S = -S
              MN = (N+1-I)*D2
              K0 = (I-1)*D2 + 1
              DO 40 K1 = K0,D1,D3
                  K2 = K1 + D4
                  DO 30 K = K1,K2,D5
                      J = K + D2
                      L = 2*D2 + K
                      M = K + MN
                      Y(M) = X(J)*S + Y(M)
                      X(K) = X(K) + X(J)
                      X(J) = X(L) - X(J)
                      Y(K) = Y(K) + Y(J)
                      Y(J) = Y(J) - Y(L)
   30                 CONTINUE
   40             CONTINUE
   50         CONTINUE
          K0 = (N-2)*D2 + 1
          DO 70 K1 = K0,D1,D3
              K2 = K1 + D4
              DO 60 K = K1,K2,D5
                  L = K + D2
                  X(K) = X(K) + X(L)
                  Y(K) = Y(K) + Y(L)
                  J = L + D2
                  X(L) = X(J) - X(L)
   60             CONTINUE
   70         CONTINUE
C
C---- Reorder scrambled fourier coefficients
C
          DO 110 I = 1,NN
              K = I
   80         CONTINUE
              K = 2*K
              IF (K.GT.N-1) K = 2*N - 1 - K
              IF (K.LT.I) GO TO 80
              IF (K.NE.I) THEN
                  J = (K-I)*D2
                  K0 = I*D2 + 1
                  DO 100 K1 = K0,D1,D3
                      K2 = K1 + D4
                      DO 90 K = K1,K2,D5
                          L = K + J
                          A = X(K)
                          X(K) = X(L)
                          X(L) = A
                          A = Y(K)
                          Y(K) = Y(L)
                          Y(L) = A
   90                     CONTINUE
  100                 CONTINUE
              END IF
  110         CONTINUE
C
          CALL CMPLFT(X,Y,N,DIM)
C
          M = NOVER2 - 1
          DO 150 I = 1,M
              ANGLE = REAL(I)*TWOPI/TWON
              C = COS(ANGLE)
              S = SIN(ANGLE)
              K0 = I*D2 + 1
              FOLD = .TRUE.
  120         CONTINUE
C
              DO 140 K1 = K0,D1,D3
                  K2 = K1 + D4
                  DO 130 K = K1,K2,D5
                      A = Y(K)/C
                      X(K) = X(K) + S*A
                      Y(K) = A
  130                 CONTINUE
  140             CONTINUE
              IF (FOLD) THEN
C
                  C = -C
                  K0 = (N-I)*D2 + 1
                  FOLD = .FALSE.
                  GO TO 120
              END IF
  150         CONTINUE
C
          M = NOVER2*D2
          K0 = M + 1
          DO 170 K1 = K0,D1,D3
              K2 = K1 + D4
              DO 160 K = K1,K2,D5
                  J = K - M
                  L = K + M
                  A = Y(L)*2.0
                  X(K) = X(K) + A
                  Y(K) = A
                  X(L) = X(J)
                  Y(L) = -Y(J)
  160             CONTINUE
  170         CONTINUE
C
      END IF
C
C---- Format statements
C
 9000 FORMAT ('0SDIAD N ODD.  N =',I10)
      END
C
C
      SUBROUTINE DIPRP(PTS,SYM,PSYM,UNSYM,DIM,X,Y)
C     =================================================
C
C
C---- Double in place reordering programme
C
C
C     .. Scalar Arguments ..
      INTEGER          PSYM,PTS
C     ..
C     .. Array Arguments ..
      REAL             X(10),Y(10)
      INTEGER          DIM(5),SYM(10),UNSYM(10)
C     ..
C     .. Local Scalars ..
      REAL             T
      INTEGER          A,AL,B,BL,BS,C,CL,CS,D,DELTA,DK,DL,DS,E,EL,ES,F,
     +                 FL,FS,G,GL,GS,H,HL,HS,I,IL,IS,J,JJ,JL,JS,K,KK,KL,
     +                 KS,L,LK,LL,LS,M,ML,MODS,MS,MULT,N,NEST,NL,NS,NT,
     +                 P,P0,P1,P2,P3,P4,P5,PUNSYM,SEP,SIZE,TEST
      LOGICAL          ONEMOD
C     ..
C     .. Local Arrays ..
      INTEGER          MODULO(14),S(14),U(14)
C     ..
C     .. Equivalences ..
      EQUIVALENCE      (AL,U(1)), (BS,S(2)), (BL,U(2))
      EQUIVALENCE      (CS,S(3)), (CL,U(3)), (DS,S(4)), (DL,U(4))
      EQUIVALENCE      (ES,S(5)), (EL,U(5)), (FS,S(6)), (FL,U(6))
      EQUIVALENCE      (GS,S(7)), (GL,U(7)), (HS,S(8)), (HL,U(8))
      EQUIVALENCE      (IS,S(9)), (IL,U(9)), (JS,S(10)), (JL,U(10))
      EQUIVALENCE      (KS,S(11)), (KL,U(11)), (LS,S(12)), (LL,U(12))
      EQUIVALENCE      (MS,S(13)), (ML,U(13)), (NS,S(14)), (NL,U(14))
C     ..
      NEST = 14
C
      NT = DIM(1)
      SEP = DIM(2)
      P2 = DIM(3)
      SIZE = DIM(4) - 1
      P4 = DIM(5)
      IF (SYM(1).NE.0) THEN
          DO 10 J = 1,NEST
              U(J) = 1
              S(J) = 1
   10         CONTINUE
          N = PTS
          DO 20 J = 1,NEST
              IF (SYM(J).EQ.0) THEN
                  GO TO 30
              ELSE
                  JJ = NEST + 1 - J
                  U(JJ) = N
                  S(JJ) = N/SYM(J)
                  N = N/SYM(J)
              END IF
   20         CONTINUE
C
   30     JJ = 0
          DO 190 A = 1,AL
              DO 180 B = A,BL,BS
                  DO 170 C = B,CL,CS
                      DO 160 D = C,DL,DS
                          DO 150 E = D,EL,ES
                              DO 140 F = E,FL,FS
                                  DO 130 G = F,GL,GS
                                      DO 120 H = G,HL,HS
                                          DO 110 I = H,IL,IS
                                              DO 100 J = I,JL,JS
                                                 DO 90 K = J,KL,KS
                                                 DO 80 L = K,LL,LS
                                                 DO 70 M = L,ML,MS
                                                 DO 60 N = M,NL,NS
                                                 JJ = JJ + 1
                                                 IF (JJ.LT.N) THEN
                                                 DELTA = (N-JJ)*SEP
                                                 P1 = (JJ-1)*SEP + 1
                                                 DO 50 P0 = P1,NT,P2
                                                 P3 = P0 + SIZE
                                                 DO 40 P = P0,P3,P4
                                                 P5 = P + DELTA
                                                 T = X(P)
                                                 X(P) = X(P5)
                                                 X(P5) = T
                                                 T = Y(P)
                                                 Y(P) = Y(P5)
                                                 Y(P5) = T
   40                                            CONTINUE
   50                                            CONTINUE
                                                 END IF
   60                                            CONTINUE
   70                                            CONTINUE
   80                                            CONTINUE
   90                                            CONTINUE
  100                                            CONTINUE
  110                                         CONTINUE
  120                                     CONTINUE
  130                                 CONTINUE
  140                             CONTINUE
  150                         CONTINUE
  160                     CONTINUE
  170                 CONTINUE
  180             CONTINUE
  190         CONTINUE
      END IF
C
      IF (UNSYM(1).NE.0) THEN
          PUNSYM = PTS/PSYM**2
          MULT = PUNSYM/UNSYM(1)
          TEST = (UNSYM(1)*UNSYM(2)-1)*MULT*PSYM
          LK = MULT
          DK = MULT
          DO 200 K = 2,NEST
              IF (UNSYM(K).EQ.0) THEN
                  GO TO 210
              ELSE
                  LK = UNSYM(K-1)*LK
                  DK = DK/UNSYM(K)
                  U(K) = (LK-DK)*PSYM
                  MODS = K
              END IF
  200         CONTINUE
  210     ONEMOD = MODS .LT. 3
          IF (.NOT.ONEMOD) THEN
              DO 220 J = 3,MODS
                  JJ = MODS + 3 - J
                  MODULO(JJ) = U(J)
  220             CONTINUE
          END IF
          MODULO(2) = U(2)
          JL = (PUNSYM-3)*PSYM
          MS = PUNSYM*PSYM
C
          DO 290 J = PSYM,JL,PSYM
              K = J
  230         CONTINUE
C
              K = K*MULT
              IF (.NOT.ONEMOD) THEN
                  DO 240 I = 3,MODS
                      K = K - (K/MODULO(I))*MODULO(I)
  240                 CONTINUE
              END IF
              IF (K.GE.TEST) THEN
                  K = K - (K/MODULO(2))*MODULO(2) + MODULO(2)
              ELSE
                  K = K - (K/MODULO(2))*MODULO(2)
              END IF
              IF (K.LT.J) GO TO 230
C
              IF (K.NE.J) THEN
                  DELTA = (K-J)*SEP
                  DO 280 L = 1,PSYM
                      DO 270 M = L,PTS,MS
                          P1 = (M+J-1)*SEP + 1
                          DO 260 P0 = P1,NT,P2
                              P3 = P0 + SIZE
                              DO 250 JJ = P0,P3,P4
                                  KK = JJ + DELTA
                                  T = X(JJ)
                                  X(JJ) = X(KK)
                                  X(KK) = T
                                  T = Y(JJ)
                                  Y(JJ) = Y(KK)
                                  Y(KK) = T
  250                             CONTINUE
  260                         CONTINUE
  270                     CONTINUE
  280                 CONTINUE
              END IF
  290         CONTINUE
      END IF
C
      END
C

C
C     ==========================================================
      SUBROUTINE SETGRD(NLAUE,SAMPLE,NXMIN,NYMIN,NZMIN,NX,NY,NZ)
C     ==========================================================
C
C Set up a suitable sampling grid for FFT
C
C Input:
C     NLAUE         Laue-group for FFT/SF calculation
C     SAMPLE        default fineness of sample, ie if = 1.0 (minimum),
C                   try to get sampling as close to minimum as possible
C                   Typically = 1.5 to get sample at traditional
C                   3 * maximum index
C     NXMIN NYMIN NZMIN minimum sampling (true XYZ)
C
C Output:
C     NX,NY,NZ       sampling intervals along X,Y,Z
C
C  The sampling intervals must satisfying the following conditions:
C
C     1) approximately SAMPLE * minimum sampling
C     2) no prime factor .gt. 19
C     3) special restrictions for particular space-groups
C
C      IMPLICIT NONE
C
      INTEGER NLAUE,NXMIN,NYMIN,NZMIN,NX,NY,NZ
      REAL SAMPLE
      EXTERNAL FNDSMP
C
C  This is ALL the point groups.
C PG1 PG1bar PG2 PGm PG2/m PG222 PGmm2 PGmmm 
C PG4 PG4bar PG4/m PG422 PG4mm PG4bar2m PG4/mmm 
C PG3 PG3bar PG32 PG3m PG3barm 
C PG6 PG6bar PG6/m PG622 PG6mm PG6bar2m  PG6/mmm
C PG23 PGm/3bar PG432 PG4bar3m PGm3bar m
C
C  We use:
C PG1 PG1bar PG2  PG2/m PG222  PGmmm 
C PG4 PG4/m PG422 PG4/mmm 
C PG3 PG3bar PG32 PG3bar/m 
C PG6 PG6/m PG622 PG6/mmm
C PG23 PGm/3bar PG432 PGm3barm
C  For grid restrictions we only need to know the laue number.
C Here is the table:
C   3 pg1     1bar      hkl:l>=0  hk0:h>=0  0k0:k>=0   1,2
C   4 pg2    2/m        hkl:k>=0, l>=0  hk0:h>=0       3/b,4/b....
C   6 pg222  mmm        hkl:h>=0, k>=0, l>=0            16 ...
C   7 pg4    4/m        hkl:h>=0, l>=0 with k>=0 if  h=0  and
C   8 pg422 4/mmm       hkl:h>=0, k>=0, l>=0            89..
C   9 pg3     3bar      hkl:h>=0, k>0  00l:l>0         143..
C  10 pg312  3/m        hkl:h>=0, k>=0 with k<=h for all l.
C                           if k = 0  l>=0
C           Space group numbers :   149-151-153
C  11 pg321  3/m        hkl:h>=0, k>=0 with k<=h for all l.
C                           if h = k  l>=0
C           Space group numbers :   150-152-154
C  12 pg6    6/m        hkl:h>=0, k>=0, l>=0 with k=0 if  h=0
C  13 pg622  6/mmm
C  14 pg23   m3
C  15 pg432  m3m
C 
C Tables of restrictions for FFT Laue-groups
C  NRESTR(1,) lauegroup number
C        (2-4,) factors for NX,NY,NZ
      INTEGER MAXLAU
      PARAMETER (MAXLAU=15)
      INTEGER NRESTR(4,MAXLAU), I
      DATA NRESTR/
C          Nsg    NX NY NZ
C     P1 or P1bar - disallowed:
     $     -1,    2, 2, 2,
C     P1 or P1bar - disallowed:
     $     -2,    2, 2, 2,
C     P1 or P1bar :
     $      3,    2, 2, 2,
C     P2 or P2bar :
     $      4,    2, 4, 2,
C     P2/m:
     $      5,    2, 8, 4,
C     P222 or Pmmm:
     $      6,    4, 4, 4,
C     P4   or P4/m:
     $      7,    4, 4, 8,
C     P422 or P4/mmm:
     $      8,    4, 4, 8,
C     P3   or P3bar:
     $      9,    6, 6, 6,
C     P32  or P3/m:
     $     10,    6, 6, 6,
C     P32  or P3/m:
     $     11,    6, 6, 6,
C     P6   or P6/m:
     $     12,    6, 6, 6,
C     P6222   or P6/mmmm:
     $     13,    6, 6, 6,
C     P23:
     $     14,    4, 4, 4,
C     P432 or Pmmm:
     $     15,    8, 8, 8/
C
      DO 1, I=1,MAXLAU
         IF (NLAUE .EQ. NRESTR(1,I)) GO TO 10
 1    CONTINUE
C
C Unrecognized Laue-group
      NX = -1
      RETURN
C
 10   CALL FNDSMP(NXMIN, NRESTR(2,I), SAMPLE, NX)
      CALL FNDSMP(NYMIN, NRESTR(3,I), SAMPLE, NY)
      CALL FNDSMP(NZMIN, NRESTR(4,I), SAMPLE, NZ)
C
      RETURN
      END
C
C
C
C     ================================
      SUBROUTINE SETLIM(LSPGRP,XYZLIM)
C     ================================
C
C Set appropriate box (asymmetric unit) for spacegroup (true spacegroup)
C     LSPGRP. For high symmetry spacegroups, this will be more than
C     one asymmetric unit
C
C On entry:
C     lspgrp    true spacegroup (not FFT spacegroup)
C
C On exit
C     xyzlim(2,3)  minimum, maximum limits on x,y,z (fractions of cell)
C                  if spacegroup not recognized, returns xzylim(1,1) = -1.0
C                  Note that the minimum limits (xyzlim(1,)) will always
C                   = 0.0
C
C      IMPLICIT NONE
C
      INTEGER LSPGRP
      REAL XYZLIM(2,3)
C
      INTEGER I,J
C
      INTEGER NUMSGP
      PARAMETER (NUMSGP=88)
      REAL ONE,HALF,THRD,TWTD,SIXT,QUAR,EIGH,TWLT,ROUND,ROUND2
      REAL ONEL,HALFL,THRDL,SIXTL,QUARL,EIGHL
      PARAMETER (ROUND=0.00001, ROUND2=2.0*ROUND)
      PARAMETER (ONE=1.0+ROUND,HALF=0.5+ROUND,THRD=1./3.+ROUND,
     $     TWTD=2./3.+ROUND,SIXT=1./6.+ROUND,
     $     QUAR=0.25+ROUND,EIGH=0.125+ROUND,TWLT=1./12.+ROUND)
      PARAMETER (ONEL=ONE-ROUND2,HALFL=HALF-ROUND2,THRDL=THRD-ROUND2,
     $     SIXTL=SIXT-ROUND2,QUARL=QUAR-ROUND2,EIGHL=EIGH-ROUND2)
C
      INTEGER NSPGRP(NUMSGP)
      REAL ASULIM(3,NUMSGP)
C
C  asulim contains maximum limit on x,y,z: the box is always assumed to
C     start at 0,0,0
C
C  Space group numbers
      DATA NSPGRP/
     $   1,   2,   3,    4,   5,  10,  16,   17,  18,1018,  19,   20,
     $  21,  22,  23,   24,  47,  65,  69,   71,  75,  76,  77,   78,
     $  79,  80,  83,   87,  89,  90,  91,   92,  93,  94,  95,   96,
     $  97,  98, 123,  139, 143, 144, 145,  146, 147, 148, 149,  150,
     $ 151, 152, 153,  154, 155, 162, 166,  168, 169, 170, 171,  172,
     $ 173, 175, 177,  178, 179, 180, 181,  182, 191, 195, 196,  197,
     $ 198, 199, 200,  202, 204, 207, 208,  209, 210, 211, 212,  213,
     $ 214, 221, 225,  229/
C
      DATA ((ASULIM(II,JJ),II=1,3),JJ=1,76)/
C        1:  P1          2:  P-1         3:  P2            4:  P21
     $ ONEL,ONEL,ONEL, ONEL,HALF,ONEL, HALF,ONEL,ONEL, ONEL,HALFL,ONEL,
C        5:  C2         10:  P2/m       16:  P222         17:  P2221
     $ HALF,HALFL,ONEL, HALF,HALF,ONEL,HALF,HALF,ONEL, HALF,HALF,ONEL,
C       18: P21212    1018: P21212      19: P212121       20:C2221
     $ ONEL,QUAR,ONEL, ONEL,QUAR,ONEL, ONEL,ONEL,QUAR, HALF,QUAR,ONEL,
C       21:  C222       22:  F222       23:  I222         24: I212121
     $ HALF,QUAR,ONEL, QUAR,QUAR,ONEL, HALF,QUAR,ONE, HALF,QUAR,ONEL,
C       47:  Pmmm       65:  Cmmm       69:  Fmmm         71:  Immm
     $ HALF,HALF,HALF, HALF,QUAR,HALF, QUAR,QUAR,HALF, HALF,QUAR,HALF,
C       75:  P4         76:  P41        77:  P42          78:  P43
     $ HALF,HALF,ONEL,ONEL,ONEL,QUARL, HALF,ONEL,HALFL,ONEL,ONEL,QUARL,
C       79:  I4         80:  I41        83:  P4/m         87:  I4/m
     $ HALF,HALF,HALF,HALF,ONEL,QUARL, HALF,HALF,HALF, HALF,HALF,QUAR,
C       89: P422        90: P4212       91: P4122         92: P41212
     $ HALF,HALF,HALF, HALF,HALF,HALF, ONEL,ONEL,EIGH, ONEL,ONEL,EIGH,
C       93: P4222       94: P42212      95: P4322         96: P43212
     $ HALF,ONEL,QUAR, HALF,HALF,HALF, ONEL,ONEL,EIGH, ONEL,ONEL,EIGH,
C       97: I422        98: I4122      123: P4/mmm       139: I4/mmm
     $ HALF,HALF,QUAR, HALF,ONEL,EIGH, HALF,HALF,HALF,  HALF,HALF,QUAR,
C      143:  P3        144:  P31       145: P32          146:  R3
     $ TWTD,TWTD,ONEL,ONEL,ONEL,THRDL,ONEL,ONEL,THRDL, TWTD,TWTD,THRDL,
C      147:  P-3       148:  R-3       149: P312         150:  P321
     $ TWTD,TWTD,HALF, TWTD,TWTD,SIXT, TWTD,TWTD,HALF, TWTD,TWTD,HALF,
C      151: P3112      152: P3121      153: P3212        154: P3221
     $ ONEL, ONEL,SIXT, ONEL,ONEL,SIXT, ONEL,ONEL,SIXT, ONEL,ONEL,SIXT,
C      155: R32        162:  P-31m     166:  R-3m        168:  P6
     $ TWTD,TWTD,SIXT, TWTD,HALF,HALF, TWTD,TWTD,SIXT, TWTD,HALF,ONEL,
C      169:  P61       170:  P65       171:  P62         172:  P64
     $ ONEL,ONEL,SIXTL,ONEL,ONEL,SIXTL,ONEL,ONEL,THRDL,ONEL,ONEL,THRDL,
C      173:  P63       175:  P6/m      177: P622         178: P6122
     $ TWTD,TWTD,HALFL, TWTD,TWTD,HALF,TWTD,HALF,HALF, ONEL,ONEL,TWLT,
C      179: P6522      180: P6222      181: P6422        182: P6322
     $ ONEL,ONEL,TWLT, ONEL,ONEL,SIXT, ONEL,ONEL,SIXT, TWTD,TWTD,QUAR,
C      191: P6/mmm     195: P23        196: F23          197: I23
     $ TWTD,THRD,HALF, ONEL,ONEL,HALF, QUAR,QUAR,ONEL, ONEL,ONEL,HALF,
C      198: P213       199: I213       200: Pm-3         202: Fm-3
     $ HALF,HALF,ONEL, HALF,HALF,HALF, HALF,HALF,HALF, HALF,HALF,QUAR/
      DATA ((ASULIM(II,JJ),II=1,3),JJ=77,88)/
C      204: Im-3       207: P432       208: P4232        209: F432
     $ HALF,HALF,HALF, ONEL,HALF,HALF, HALF,ONEL,QUAR, HALF,HALF,HALF,
C      210: F4132      211: I432       212: P4332        213: P4132
     $ HALF,ONEL,EIGH, HALF,HALF,QUAR, ONEL,ONEL,EIGH, ONEL,ONEL,EIGH,
C      214: I4132      221: Pm-3m      225: Fm-3m        229: Im-3m
     $ HALF,ONEL,EIGH, HALF,HALF,HALF, HALF,QUAR,QUAR, HALF,HALF,QUAR/
C
C
      DO 10, J=1,NUMSGP
         IF (LSPGRP .EQ. NSPGRP(J)) GO TO 20
 10   CONTINUE
C
C Spacegroup not found
      XYZLIM(1,1) = -1.0
      RETURN
C
C
 20   DO 30, I=1,3
         XYZLIM(1,I) = 0.0
         XYZLIM(2,I) = ASULIM(I,J)
 30   CONTINUE
C
      RETURN
C
      END
C
C     =============================================
      SUBROUTINE FNDSMP(MINSMP, NMUL, SAMPLE, NSAMPL)
C     =============================================
C
C----  Find suitable grid sample, approximately = SAMPLE/2 * maximum index,
C     with required factor, & no prime factor .gt. 19
C
C  On entry:
C     MINSMP     minimum sample, approximately 2 * maximum index
C     NMUL       required factor
C     SAMPLE     desired sample factor, ie if = 1.0 (minimum), try to
C                get sample close to MINSMP
C
C  On exit:
C     nsampl     grid sample
C                if MINSMP<=0, nsampl=nmul
C
CC      implicit none
C
C     .. Scalar Arguments ..
      INTEGER MINSMP,NMUL,NSAMPL
      REAL SAMPLE
C     ..
C     .. Local Scalars ..
      REAL R1MAX,R1MIN,R2MAX,R2MIN
      INTEGER N
C     ..
C     .. External Functions ..
      LOGICAL FACTRZ
      EXTERNAL FACTRZ
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT,REAL
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
C
C---- This routine makes 2 attempts at finding a suitable factor:-
C     1) searching downwards from r1max*MINSMP to r1min*MINSMP
C     2) searching upwards from r2min*MINSMP to r2max*MINSMP
C
      DATA R1MIN,R1MAX/1.0, 1.6/
      DATA R2MIN,R2MAX/1.4, 4.0/
C     ..
C
C----  Check MINSMP <= 0, if so set NSAMPL = NMUL
      IF (MINSMP .LE. 0) THEN
         NSAMPL = NMUL
         RETURN
      ENDIF
C
C---- Set search limits
      IF (SAMPLE .GE. 1.0) THEN
         R1MAX = SAMPLE
         R2MIN = MAX(1.0, SAMPLE*0.95)
      ENDIF
C----  Start with multiple of nmul
C
      N = NINT(REAL(MINSMP)*R1MAX/REAL(NMUL))*NMUL
C
C---- Function factrz returns .true.
C     if number has all prime factors .le. 19
C
   10 IF (FACTRZ(N)) THEN
C
C---- OK suitable sample interval found, accept it
C
        NSAMPL = N
        RETURN
      END IF
C
C---- decrement trial value & continue if still in range
C
      N = N - NMUL
      IF (REAL(N)/REAL(MINSMP).GT.R1MIN) GO TO 10
C
C---- Now try 2nd search if 1st unsuccesfull
C
      N = NINT(REAL(MINSMP)*R2MIN/REAL(NMUL))*NMUL
   20 IF (FACTRZ(N)) THEN
C
C---- OK suitable sample interval found, accept it
C
        NSAMPL = N
        RETURN
      END IF
C
C---- increment trial value & continue if still in range
C
      N = N + NMUL
      IF (REAL(N)/REAL(MINSMP).LT.R2MAX) GO TO 20
C
C---- Failed
C
      NSAMPL = -1
      RETURN
C
      END
C
C
C
C     ==========================
      LOGICAL FUNCTION FACTRZ(N)
C     ==========================
C
C---- Returns true if N has all prime factors .le. 19
C
C     .. Parameters ..
      INTEGER NFACT
      PARAMETER (NFACT=8)
C     ..
C     .. Scalar Arguments ..
      INTEGER N
C     ..
C     .. Local Scalars ..
      INTEGER I,NN
C     ..
C     .. Local Arrays ..
      INTEGER IFACT(NFACT)
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPERR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
      DATA IFACT/2,3,5,7,11,13,17,19/
C     ..
C
C
      NN = N
C
C
      DO 20 I = 1,NFACT
   10   IF (MOD(NN,IFACT(I)).EQ.0) THEN
C
C---- factor found, divide & continue if required
C
          IF (IFACT(I).EQ.0) THEN
C
C                *******************************************
            CALL CCPERR(1,' Error divide by Zero in FACTRZ')
C                *******************************************
C
          END IF
C
C
          NN = NN/IFACT(I)
          IF (NN.EQ.1) THEN
C
C----  success
C
            FACTRZ = .TRUE.
            RETURN
          END IF
          GO TO 10
        END IF
   20 CONTINUE
C
C---- Failure
C
      FACTRZ = .FALSE.
      RETURN
      END
