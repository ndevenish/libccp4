C
C  cross.f        dot.f          ea06c.f      ea08c.f     ea09c.f
C  fa01as.f       fa01bs.f       fa01cs.f     fa01ds.f    fm02ad.f
C  icross.f       idot.f         ma21ad.f    match.f
C  matmul.f       matvec.f       mc04b.f      mc10ad.f    minv.f
C  scalev.f       transp.f       unit.f       vdif.f      vset.f
C  vsum.f         zipin.f        zipout.f
C
C External routines used :         (in MODLIB)
C  TRANSP(A,B)     transpose 3 x 3 matrix  A = (B)T
C  MATMUL(A,B,C)   multiply 3 x 3 matrices A = BC
C  MINV(A,B,D)     invert 3 x 3 matrix     A = (B)**-1; D = determinant
C  MOVE(A,B,N)     move N bytes from B to A
C
C     The routines ea06c, ea08c, ea09c, fa01as, fa01bs, fa01cs, fa01ds,
C     fm02ad, ma21ad, mc04b, mc10ad (and possibly others) are from the
C     Harwell Subroutine library.  The conditions on their external use,
C     reproduced from the Harwell manual are:
C     * due acknowledgement is made of the use of subroutines in any
C     research publications resulting from their use.
C     * the subroutines may be modified for use in research applications
C     by external users.  The nature of such modifiactions should be
C     indicated in writing for information to the liaison officer.  At
C     no time however, shall the subroutines or modifications thereof
C     become the property of the external user.
C       The liaison officer for the library's external affairs is listed
C     as: Mr. S. Marlow, Building 8.9, Harwell Laboratory, Didcot, 
C     Oxon OX11 0RA, UK.
C     
      SUBROUTINE CROSS(A,B,C)
C     =======================
C
C
C     compute A = B cross C
C
C     .. Array Arguments ..
      REAL             A(3),B(3),C(3)
C     ..
      A(1) = B(2)*C(3) - C(2)*B(3)
      A(2) = B(3)*C(1) - C(3)*B(1)
      A(3) = B(1)*C(2) - C(1)*B(2)
      END
C
C
      REAL FUNCTION DOT(A,B)
C     ======================
C
C---- Dot product of two vectors
C
C
C
C     .. Array Arguments ..
      REAL              A(3),B(3)
C     ..
      DOT = A(1)*B(1) + A(2)*B(2) + A(3)*B(3)
      END
C
C
      SUBROUTINE EA06C(A,VALUE,VECTOR,M,IA,IV,W)
C     ==========================================
C
C**  18/03/70 LAST LIBRARY UPDATE
C
C   Given a real mXm symmetric matrix A = {aij} this routine finds 
C   all its eigenvalues (lambda)i i=1,2,.....,m and 
C   eigenvectors xj i=1,2,...,m.
C  i.e. finds the non-trivial solutions of Ax=(lambda)x
C
C  Argument list
C  -------------
C
C  SUBROUTINE EA06C(A,VALUE,X,M,IA,IX,W)
C
C  Arguments set by the user -
C
C   A  is a REAL 2-dimensional array with first dimension IA. 
C   The user must store the lower triangle of matrix A into the 
C   lower triangle of the array A. 
C   i.e. put a(ij) into A(I,J) for i>=j and I>=J.
C
C  M  is an INTEGER and should be set to the order m of the matrix
C
C   IA is an INTEGER set to the first dimension of the array A, 
C   i.e. if the allocation for the array A was specified by
C
C            DIMENSION A(100,50)
C
C     then IA would be set to 100
C
C   IX is an INTEGER set to the first dimension of 
C   the 2-dimensional array X
C
C
C  Arguments set by the routine -
C
C   A  the space above the diagonal of the array A is used by 
C   the routine as work space, the lower triangle set by the user 
C   will remain unchanged on return.
C
C   VALUE is a REAL array in which the routine puts the eigenvalues
C   (lambda)i, i=1,2,...,m. These are not necessarily in any order.
C
C   X  is a REAL 2-dimensional array, with first dimension IX, 
C      containing the eigenvectors. The components of the eigenvector 
C      x(i) corresponding to the eigenvalue (lambda)i 
C      (in VALUE(I)) are placed in X(J,I) J=1,2,...,M.
C     The eigenvectors are normalized si that xT(i)x(i)=1 i=1,2,...,m.
C
C  W  is a REAL array used by the routine for work space. It must have
C     dimension at least 5*M.
C
C                                    ------------
C
C
C
C     .. Scalar Arguments ..
      INTEGER          IA,IV,M
C     ..
C     .. Array Arguments ..
      REAL             A(IA,M),VALUE(M),VECTOR(IV,M),W(*)
C     ..
C     .. Local Scalars ..
      REAL             PP
      INTEGER          I,I1,II,K,L,M1
C     ..
C     .. External Subroutines ..
      EXTERNAL         EA08C,MC04B
C     ..
      M1 = M + 1
      W(1) = A(1,1)
      IF (M-2) 30,10,20
   10 W(2) = A(2,2)
      W(4) = A(2,1)
      GO TO 30

   20 CALL MC04B(A,W,W(M1),M,IA,W(M+M1))
   30 CALL EA08C(W,W(M1),VALUE,VECTOR,M,IV,W(M+M1))
      IF (M.GT.2) THEN
          DO 70 L = 1,M
              DO 60 II = 3,M
                  I = M - II + 1
                  IF (W(M1+I).NE.0) THEN
                      PP = 0.0
                      I1 = I + 1
                      DO 40 K = I1,M
                          PP = A(I,K)*VECTOR(K,L) + PP
   40                     CONTINUE
                      PP = PP/ (A(I,I+1)*W(M1+I))
                      DO 50 K = I1,M
                          VECTOR(K,L) = A(I,K)*PP + VECTOR(K,L)
   50                     CONTINUE
                  END IF

   60             CONTINUE
   70         CONTINUE
      END IF

      END
C
C
      SUBROUTINE EA08C(A,B,VALUE,VEC,M,IV,W)
C     =====================================
C
C    19/03/70 LAST LIBRARY UPDATE
C  This uses qr iteration to find the eigenvalues and eigenvectors
C  of the symmetric tridiagonal matrix whose diagonal elements are
C  a(i),i=1,m and off-diagonal elements are b(i),i=2,m.  The array
C  w is used for workspace and must have dimension at least 2*m.
C  we treat vec as if it had dimensions (iv,m).
C
C     .. Scalar Arguments ..
      INTEGER          IV,M
C     ..
C     .. Array Arguments ..
      REAL             A(M),B(M),VALUE(M),VEC(1),W(*)
C     ..
C     .. Local Scalars ..
      REAL             A11,A12,A13,A21,A22,A23,A33,A34,CO,EPS,ROOT,S,SI,
     +                 V1,V2,XAX,XX
      INTEGER          I,II,ITER,J,J1,J2,JI,JK,K,L,N1,N2,N2M1
C     ..
C     .. External Subroutines ..
      EXTERNAL         EA09C
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC        ABS,MIN,SIGN,SQRT
C     ..
C     .. Data statements ..
      DATA             EPS/1.0E-5/,A34/0.0/
C     ..
C
C
      CALL EA09C(A,B,W(M+1),M,W)
C
C---- Set vec to the identity matrix.
C
      DO 20 I = 1,M
          VALUE(I) = A(I)
          W(I) = B(I)
          K = (I-1)*IV + 1
          L = K + M - 1
          DO 10 J = K,L
              VEC(J) = 0.0
   10         CONTINUE
          VEC(K+I-1) = 1.0
   20     CONTINUE
      ITER = 0
      IF (M.NE.1) THEN
          N2 = M
   30     CONTINUE
C
C---- Each qr iteration is performed of rows and columns n1 to n2
C
          DO 40 II = 2,N2
              N1 = 2 + N2 - II
              IF (ABS(W(N1)).LE. (ABS(VALUE(N1-1))+ABS(VALUE(N1)))*
     +            EPS) GO TO 50
   40         CONTINUE
          N1 = 1
   50     IF (N2.NE.N1) THEN
              ROOT = W(M+N2)
              ITER = ITER + 1
              N2M1 = N2 - 1
              A22 = VALUE(N1)
              A12 = A22 - ROOT
              A23 = W(N1+1)
              A13 = A23
              DO 70 I = N1,N2M1
                  A33 = VALUE(I+1)
                  IF (I.NE.N2M1) A34 = W(I+2)
                  S = SIGN(SQRT(A12*A12+A13*A13),A12)
                  SI = A13/S
                  CO = A12/S
                  JK = I*IV + 1
                  J1 = JK - IV
                  J2 = MIN(M,I+ITER) + J1 - 1
                  DO 60 JI = J1,J2
                      V1 = VEC(JI)
                      V2 = VEC(JK)
                      VEC(JI) = V1*CO + V2*SI
                      VEC(JK) = V2*CO - V1*SI
                      JK = JK + 1
   60                 CONTINUE
                  IF (I.NE.N1) W(I) = S
                  A11 = CO*A22 + SI*A23
                  A12 = CO*A23 + SI*A33
                  A13 = SI*A34
                  A21 = CO*A23 - SI*A22
                  A22 = CO*A33 - SI*A23
                  A23 = CO*A34
                  VALUE(I) = A11*CO + A12*SI
                  A12 = -A11*SI + A12*CO
                  W(I+1) = A12
                  A22 = A22*CO - A21*SI
   70             CONTINUE
              VALUE(N2) = A22
              GO TO 30

          ELSE
              N2 = N2 - 1
              IF (N2-1.GT.0) GO TO 30
          END IF
C
C---- Rayleigh quotient
C
          DO 90 J = 1,M
              K = (J-1)*IV
              XX = VEC(K+1)**2
              XAX = A(1)*XX
              DO 80 I = 2,M
                  XX = VEC(K+I)**2 + XX
                  XAX = (B(I)*2.0*VEC(K+I-1)+VEC(K+I)*A(I))*VEC(K+I) +
     +                  XAX
   80             CONTINUE
              VALUE(J) = XAX/XX
   90         CONTINUE
      END IF

      END
C
C
      SUBROUTINE EA09C(A,B,VALUE,M,OFF)
C     =================================
C
C    18/03/70 LAST LIBRARY UPDATE
C
C     .. Scalar Arguments ..
      INTEGER          M
C     ..
C     .. Array Arguments ..
      REAL             A(M),B(M),OFF(M),VALUE(M)
C     ..
C     .. Local Scalars ..
      REAL             A11,A12,A13,A21,A22,A23,A33,A34,BB,CC,CO,EPS,
     +                 ROOT,S,SBB,SI
      INTEGER          I,II,N1,N2,N2M1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC        ABS,SQRT
C     ..
C     .. Data statements ..
      DATA             A34/0.0/,EPS/0.6E-7/
C     ..
C
C
      VALUE(1) = A(1)
      IF (M.NE.1) THEN
          DO 10 I = 2,M
              VALUE(I) = A(I)
              OFF(I) = B(I)
   10         CONTINUE
C
C---- Each qr iteration is performed of rows and columns n1 to n2
C
          N2 = M
   20     CONTINUE
          IF (N2.GT.1) THEN
              DO 30 II = 2,N2
                  N1 = 2 + N2 - II
                  IF (ABS(OFF(N1)).LE. (ABS(VALUE(N1-1))+
     +                ABS(VALUE(N1)))*EPS) GO TO 40
   30             CONTINUE
              N1 = 1
   40         IF (N2.NE.N1) THEN
C
C---- Root  is the eigenvalue of the bottom 2*2 matrix that is nearest
C     to the last matrix element and is used to accelerate the
C     convergence
C
                  BB = (VALUE(N2)-VALUE(N2-1))*0.5
                  CC = OFF(N2)*OFF(N2)
                  SBB = 1.0
                  IF (BB.LT.0.0) SBB = -1.0
                  ROOT = CC/ (SQRT(BB*BB+CC)*SBB+BB) + VALUE(N2)
                  N2M1 = N2 - 1
                  A22 = VALUE(N1)
                  A12 = A22 - ROOT
                  A23 = OFF(N1+1)
                  A13 = A23
                  DO 50 I = N1,N2M1
                      A33 = VALUE(I+1)
                      IF (I.NE.N2M1) A34 = OFF(I+2)
                      S = SQRT(A12*A12+A13*A13)
                      SI = A13/S
                      CO = A12/S
                      IF (I.NE.N1) OFF(I) = S
                      A11 = CO*A22 + SI*A23
                      A12 = CO*A23 + SI*A33
                      A13 = SI*A34
                      A21 = CO*A23 - SI*A22
                      A22 = CO*A33 - SI*A23
                      A23 = CO*A34
                      VALUE(I) = A11*CO + A12*SI
                      A12 = -A11*SI + A12*CO
                      OFF(I+1) = A12
                      A22 = A22*CO - A21*SI
   50                 CONTINUE
                  VALUE(N2) = A22

              ELSE
                  N2 = N2 - 1
              END IF

              GO TO 20

          END IF

      END IF

      END
C
C
C---- Changes put in to make it work on vax (this version has scaling)
C  1. 12 statements for calculation of over/underflow of determinant in ma21
C     replaced by a simple one which will overflow more easily(entry ma21cd)
C  2. changes to mc10ad to replace 370-specific parts....
C     a. u=floati  (6 times)
C     b. new alog16 procedure (twice)
C     c. simpler 16**diag statement (once)
C  3. all double precision replaced by real*8 (not really necessary).
C  4. replace a(n),b(n) by a(1),b(1) in fm02ad to avoid vax array checking.
C
      FUNCTION FA01AS(I)
C     ==================
      DOUBLE PRECISION  G
      COMMON/FA01ES/G
      G= 1431655765.D0
C
C
      G=DMOD(G* 9228907.D0,4294967296.D0)
      IF(I.GE.0)FA01AS=G/4294967296.D0
      IF(I.LT.0)FA01AS=2.D0*G/4294967296.D0-1.D0
      RETURN
      END
C
      SUBROUTINE FA01BS(MAX,NRAND)
C     ============================
C
      NRAND=INT(FA01AS(1)*FLOAT(MAX))+1
      RETURN
      END
C
      SUBROUTINE FA01CS(IL,IR)
C     ========================
C
      DOUBLE PRECISION  G
      COMMON/FA01ES/G
      G= 1431655765.D0
C
      IL=G/65536.D0
      IR=G-65536.D0*FLOAT(IL)
      RETURN
      END
C
      SUBROUTINE FA01DS(IL,IR)
C     ========================
C
      DOUBLE PRECISION  G
      COMMON/FA01ES/G
      G= 1431655765.D0
C
      G=65536.D0*FLOAT(IL)+FLOAT(IR)
      RETURN
      END
C
C---- FM02AD - A routine to compute the inner product of two
C     double precision real vectors accumulating the result
C     double precision.  it can be used as an alternative
C     to the assembler version, but note that it is likely
C     to be significantly slower in execution.
C
      DOUBLE PRECISION  FUNCTION FM02AD(N,A,IA,B,IB)
      DOUBLE PRECISION  R1,A,B
C
C---- The following statement changed from a(n),b(n) to avoid vax dynamic
C     array check failure.
C
      DIMENSION A(1),B(1)
C
C    N       the length of the vectors (if n<= 0  fm02ad = 0)
C    A       the first vector
C    IA      subscript displacement between elements of a
C    B       the second vector
C    IB      subscript displacement between elements of b
C    FM02AD  the result
C
      R1=0D0
      IF(N.LE.0) GO TO 2
      JA=1
      IF(IA.LT.0) JA=1-(N-1)*IA
      JB=1
      IF(IB.LT.0) JB=1-(N-1)*IB
      I=0
    1 I=I+1
      R1=R1+A(JA)*B(JB)
      JA=JA+IA
      JB=JB+IB
      IF(I.LT.N) GO TO 1
    2 FM02AD=R1
      RETURN
C
      END
C
C
      SUBROUTINE ICROSS(A,B,C)
C     ========================
C
C Integer version
C
C
C
C     .. Array Arguments ..
      INTEGER           A(3),B(3),C(3)
C     ..
      A(1) = B(2)*C(3) - C(2)*B(3)
      A(2) = B(3)*C(1) - C(3)*B(1)
      A(3) = B(1)*C(2) - C(1)*B(2)
      END
C
C
      INTEGER FUNCTION IDOT(A,B)
C     ==========================
C
C---- Dot product of two vectors, integer version
C
C
C
C     .. Array Arguments ..
      INTEGER               A(3),B(3)
C     ..
      IDOT = A(1)*B(1) + A(2)*B(2) + A(3)*B(3)
      END
C
C
      SUBROUTINE IMINV3(A,B,D)
C     =======================
C
C    Integer version
C---- Invert a general 3x3 matrix and return determinant in d
C
C     A=(B)-1
C
C
C
C     .. Scalar Arguments ..
      INTEGER          D
C     ..
C     .. Array Arguments ..
      INTEGER          A(3,3),B(3,3)
C     ..
C     .. Local Scalars ..
      INTEGER          I,J
C     ..
C     .. Local Arrays ..
      INTEGER          C(3,3)
C     ..
C     .. External Functions ..
      INTEGER          IDOT
      EXTERNAL         IDOT
C     ..
C     .. External Subroutines ..
      EXTERNAL         ICROSS
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC        ABS
C     ..
      CALL ICROSS(C(1,1),B(1,2),B(1,3))
      CALL ICROSS(C(1,2),B(1,3),B(1,1))
      CALL ICROSS(C(1,3),B(1,1),B(1,2))
      D = IDOT(B(1,1),C(1,1))
C
C---- Test determinant
C
      IF (ABS(D).GT.0) THEN
C
C---- Determinant is non-zero
C
          DO 20 I = 1,3
              DO 10 J = 1,3
                  A(I,J) = C(J,I)/D
   10             CONTINUE
   20         CONTINUE

      ELSE
          D = 0
      END IF

      END
C
C
      SUBROUTINE MATMUL(A,B,C)
C     ========================
C
C---- Multiply 2 3x3 matrices
C
C   A=BC
C
C
C
C     .. Array Arguments ..
      REAL              A(3,3),B(3,3),C(3,3)
C     ..
C     .. Local Scalars ..
      REAL              S
      INTEGER           I,J,K
C     ..
      DO 30 I = 1,3
          DO 20 J = 1,3
              S = 0
              DO 10 K = 1,3
                  S = B(I,K)*C(K,J) + S
   10             CONTINUE
              A(I,J) = S
   20         CONTINUE
   30     CONTINUE
      END
C
C
      SUBROUTINE MATVEC(V,A,B)
C     ========================
C
C---- Post-multiply a 3x3 matrix by a vector
C
C    V=AB
C
C
C
C     .. Array Arguments ..
      REAL              A(3,3),B(3),V(3)
C     ..
C     .. Local Scalars ..
      REAL              S
      INTEGER           I,J
C     ..
      DO 20 I = 1,3
          S = 0
          DO 10 J = 1,3
              S = A(I,J)*B(J) + S
   10         CONTINUE
          V(I) = S
   20     CONTINUE
      END
C
C
      SUBROUTINE MC04B(A,ALPHA,BETA,M,IA,Q)
C     =====================================
C
C
C
C     .. Scalar Arguments ..
      INTEGER          IA,M
C     ..
C     .. Array Arguments ..
      REAL             A(IA,1),ALPHA(1),BETA(1),Q(1)
C     ..
C     .. Local Scalars ..
      REAL             BIGK,H,PP,PP1,QJ
      INTEGER          I,I1,I2,J,J1,KI,KJ,M1,M2
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC        SQRT
C     ..
      ALPHA(1) = A(1,1)
      DO 20 J = 2,M
          J1 = J - 1
          DO 10 I = 1,J1
              A(I,J) = A(J,I)
   10         CONTINUE
          ALPHA(J) = A(J,J)
   20     CONTINUE
      M1 = M - 1
      M2 = M - 2
      DO 110 I = 1,M2
          PP = 0.0
          I1 = I + 1
          DO 30 J = I1,M
              PP = A(I,J)**2 + PP
   30         CONTINUE
          PP1 = SQRT(PP)
          IF (A(I,I+1).LT.0) THEN
              BETA(I+1) = PP1

          ELSE
              BETA(I+1) = -PP1
          END IF

          IF (PP.GT.0) THEN
              H = PP - BETA(I+1)*A(I,I+1)
              A(I,I+1) = A(I,I+1) - BETA(I+1)
              DO 60 KI = I1,M
                  QJ = 0.0
                  DO 40 KJ = I1,KI
                      QJ = A(KJ,KI)*A(I,KJ) + QJ
   40                 CONTINUE
                  IF (KI-M.LT.0) THEN
                      I2 = KI + 1
                      DO 50 KJ = I2,M
                          QJ = A(KI,KJ)*A(I,KJ) + QJ
   50                     CONTINUE
                  END IF

                  Q(KI) = QJ/H
   60             CONTINUE
              BIGK = 0.0
              DO 70 KJ = I1,M
                  BIGK = A(I,KJ)*Q(KJ) + BIGK
   70             CONTINUE
              BIGK = BIGK/ (2.0*H)
              DO 80 KJ = I1,M
                  Q(KJ) = Q(KJ) - A(I,KJ)*BIGK
   80             CONTINUE
              DO 100 KI = I1,M
                  DO 90 KJ = KI,M
                      A(KI,KJ) = A(KI,KJ) - Q(KI)*A(I,KJ) -
     +                           Q(KJ)*A(I,KI)
   90                 CONTINUE
  100             CONTINUE
          END IF

  110     CONTINUE
      DO 120 I = 2,M
          H = ALPHA(I)
          ALPHA(I) = A(I,I)
          A(I,I) = H
  120     CONTINUE
      BETA(M) = A(M-1,M)
      END
C
C
      SUBROUTINE MINV(A,B,D)
C     ======================
C
C---- Invert a general 3x3 matrix and return determinant in d
C
C      A=(B)-1
C
C
C
C     .. Scalar Arguments ..
      REAL            D
C     ..
C     .. Array Arguments ..
      REAL            A(3,3),B(3,3)
C     ..
C     .. Local Scalars ..
      INTEGER         I,J
C     ..
C     .. Local Arrays ..
      REAL            C(3,3)
C     ..
C     .. External Functions ..
      REAL            DOT
      EXTERNAL        DOT
C     ..
C     .. External Subroutines ..
      EXTERNAL        CROSS
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC       ABS
C     ..
      CALL CROSS(C(1,1),B(1,2),B(1,3))
      CALL CROSS(C(1,2),B(1,3),B(1,1))
      CALL CROSS(C(1,3),B(1,1),B(1,2))
      D = DOT(B(1,1),C(1,1))
C
C---- Test determinant
C
      IF (ABS(D).GT.1.0E-30) THEN
C
C---- Determinant is non-zero
C
          DO 20 I = 1,3
              DO 10 J = 1,3
                  A(I,J) = C(J,I)/D
   10             CONTINUE
   20         CONTINUE

      ELSE
          D = 0.0
      END IF

      END
C
C
      SUBROUTINE SCALEV(A,X,B)
C     ========================
C
C---- Scale a vector b with scalar x and put result in a
C
C
C
C     .. Scalar Arguments ..
      REAL              X
C     ..
C     .. Array Arguments ..
      REAL              A(3),B(3)
C     ..
C     .. Local Scalars ..
      INTEGER           I
C     ..
      DO 10 I = 1,3
          A(I) = B(I)*X
   10     CONTINUE
      END
C
C
      SUBROUTINE TRANSP(A,B)
C     ======================
C
C---- Transpose a 3x3 matrix
C
C     A=BT
C
C
C
C     .. Array Arguments ..
      REAL              A(3,3),B(3,3)
C     ..
C     .. Local Scalars ..
      INTEGER           I,J
C     ..
      DO 20 I = 1,3
          DO 10 J = 1,3
              A(I,J) = B(J,I)
   10         CONTINUE
   20     CONTINUE
      END
C
C
      SUBROUTINE UNIT(V)
C     =================
C
C
C
C---- Vector v reduced to unit vector
C
C     .. Array Arguments ..
      REAL            V(3)
C     ..
C     .. Local Scalars ..
      REAL            VMOD
      INTEGER         I
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC       SQRT
C     ..
      VMOD = V(1)**2 + V(2)**2 + V(3)**2
      VMOD = SQRT(VMOD)
      DO 10 I = 1,3
          V(I) = V(I)/VMOD
   10     CONTINUE
      END
C
C
      SUBROUTINE VDIF(A,B,C)
C     =====================
C
C
C---- Subtract two vectors
C
C
C
C     .. Array Arguments ..
      REAL            A(3),B(3),C(3)
C     ..
C     .. Local Scalars ..
      INTEGER         I
C     ..
      DO 10 I = 1,3
          A(I) = B(I) - C(I)
   10     CONTINUE
      END
C
C
      SUBROUTINE VSET(A,B)
C     ====================
C
C
C---- Move a vector from b to a
C
C
C
C     .. Array Arguments ..
      REAL            A(3),B(3)
C     ..
C     .. Local Scalars ..
      INTEGER         I
C     ..
      DO 10 I = 1,3
          A(I) = B(I)
   10     CONTINUE
      END
C
C
      SUBROUTINE VSUM(A,B,C)
C     ======================
C
C
C---- Add two vectors
C
C
C
C     .. Array Arguments ..
      REAL            A(3),B(3),C(3)
C     ..
C     .. Local Scalars ..
      INTEGER         I
C     ..
      DO 10 I = 1,3
          A(I) = B(I) + C(I)
   10     CONTINUE
      END
C
C
      SUBROUTINE ZIPIN(ID,N,BUF)
C     ==========================
C
C
C---- Fast binary read on unit id into array buf of length n
C
C
C
C     .. Scalar Arguments ..
      INTEGER          ID,N
C     ..
C     .. Array Arguments ..
      REAL             BUF(N)
C     ..
      READ (ID) BUF
      END
C
C
      SUBROUTINE ZIPOUT(ID,N,BUF)
C     ===========================
C
C
C---- Fast binary write to unit id of array buf length n
C
C
C
C     .. Scalar Arguments ..
      INTEGER           ID,N
C     ..
C     .. Array Arguments ..
      REAL              BUF(N)
C     ..
      WRITE (ID) BUF
      END
