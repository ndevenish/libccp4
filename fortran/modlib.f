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
      SUBROUTINE CROSS(A,B,C)
C     =======================
C
C
C
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
      SUBROUTINE MA21AD (A,IA,N,B,W,E)
C     ===============================
C
      COMMON /MA21ED/ LP,JSCALE,EA,EB
C
      REAL*8 A(IA,N),B(N),W(N,N),AA,AC,DET,WW,Q(2),PCK
      REAL*8 EPS4,ZERO,ONE,TWO,P5
     +,E,EA,EB,EPSN,XNORM,AXNORM,ENORM,ENORMA,ANORMA,ANORM,ERR,AB,AM
     +,EPS,R(4),RA,FM02AD
      CHARACTER*8 DICT(8 )
C
C---- eps=machine precision,eps4=eps*4
C     I3, I4 previously initialised to 0 but changed because of EQUIVALENCE 
C     stmt. MRO'D 23/10/81
C
      INTEGER*4 I3,I4,I1(4)
      REAL*4 UPCK(4)
      LOGICAL*1 L1,L2,L3(4),L4(4),LQ,LA
      EQUIVALENCE (Q(1),R(1),L1),(RA,LA),(WW,II),(UPCK(1),PCK),
     +            (Q(2),L2),(L3(1),I3),(L4(1),I4)
      DATA EPS4/4.0D-16/,ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/,P5/0.5D0/,
     +     EPS /1.0D-16/
      DATA DICT/' MA21AD ',' MA21BD ',' MA21CD ',' MA21DD ',
     +               ' N IS   ',' PIVOT  ','IS SMALL','IS ZERO '/
C
C---- JSCALE NORMALLY =1, TEMPORARILY SET TO 0
C
      LP = 6
      JSCALE = 1
      EA = 1.0D-16
      EB = 1.0D-16
C
C
C---- Next two lines inserted to initialise I3,I4 by assignment - see above 
C
      I3 = 0
      I4 = 0
      IENT = 1
      GO TO 100
C
      ENTRY MA21BD(A,IA,N,W,E)
C     ========================
C
      IENT = 2
      GO TO 100
C
      ENTRY MA21CD(A,IA,N,DET,IDET,W)
C     ===============================
C
      IENT = 3
      DET = ONE
      IDET = 0
  100 IF(N.LT.1)GO TO 810
      IP = 0
      IF(JSCALE .LE. 0)GO TO 120
C     FIND SCALING FACTORS.
      CALL MC10AD(A,N,IA,W(1,5),W(1,1),IT)
      DO 110 I=1,N
        PCK = W(I,1)
        W(2*I-1,3) = UPCK(1)
        W(2*I,3) = UPCK(2)
  110 CONTINUE
C
C---- Store in w(j,1),j=1,n the maximal elements  of columns after
C     application of scaling.
C
  120 EPSN = ZERO
      DO 140 J=1,N
        AB = ZERO
        DO 130 I=1,N
          AM =  DABS(A(I,J))
          IF(JSCALE.GT.0)AM=AM*W(I,3)
          AB = DMAX1(AB,AM) 
  130   CONTINUE
        W(J,1) = AB
        IF(JSCALE.GT.0)AB=AB*W(J,4)
        EPSN = DMAX1(EPSN,AB)
  140 CONTINUE
      EPSN = EPSN*EPS4
      IF((E.LE.ZERO).OR.(IENT.EQ.3))GO TO 160
C     MAKE COPY OF MATRIX
      DO 1150 I=1,N
        DO 150 J=1,N
          W(I,J+5) = A(I,J)
  150   CONTINUE
 1150 CONTINUE
C
C---- Factorisation of matrix into l*u, where l is a lower unit
C     triangle and u is upper triangle
C
  160 DO 230 L=1,N
        AM = ZERO
        II = L
C
C---- Evaluate  elements in pivotal column.
C
        DO 170 I=L,N
          A(I,L)=A(I,L)-FM02AD(L-1,A(I,1),IA,A(1,L),1)
C
C---- Look for maximum element in pivotal column.
C
          AB =  DABS(A(I,L))
          IF(JSCALE .GT. 0)AB = AB*W(I,3)
          IF(AM .GE. AB)GO TO 170
          AM = AB
          II = I
  170   CONTINUE
C
C---- Test for small or zero pivot.
C
        AB = W(L,1)
        IF(AM .LE. EPS4*AB)IP=-L
        IF(AM.NE.ZERO)GO TO 180
        IF(IENT.EQ.2)GO TO 820
        IP=L
  180   IF(IENT.NE.3)GO TO 190
C
C---- The next 12 statements calculate the determinant of matrix a.
C     to prevent overflows/underflows the exponents of the numbers
C     being multiplied are examined and the excess exponent is stored
C     in idet.
C     for vax next twelve statements replaced- may need attention if ma21cd
C     is ever needed for big matrices.
C      Q(1)= DABS(DET)
C      Q(2)= DABS(A(II,L))
C      L3(4) = L1
C      L4(4) = L2
C      K = IDET+I3+I4-128
C      I3 = K
C      L2 = LQ
C      IF(IABS(K) .GT. 62)K=ISIGN(62,I3)
C      IDET = I3-K
C      I3 = K+64
C      L1 = L3(4)
C      DET =DSIGN(Q(1),DET)*DSIGN(Q(2),A(II,L))
C
        DET=DET*A(II,L)
  190   IF(II .EQ. L)GO TO 220
C
C---- Interchange rows n and ii
C     interchange equilibration factors
C     w(l,1)=ii means interchange between rows l and ii
C
        IF(IENT.EQ.3)DET=-DET
        IF(JSCALE .LE. 0)GO TO 200
        AA = W(L,3)
        W(L,3) = W(II,3)
        W(II,3) = AA
  200   DO 210 I=1,N
          AA = A(L,I)
          A(L,I) = A(II,I)
          A(II,I) = AA
  210   CONTINUE
  220   W(L,1) = WW
        IF( L .EQ. N)GO TO 240
        AA = ONE
        AC = A(L,L)
        IF( DABS(AC) .NE. ZERO)AA = AA/AC
        K= L+1
C
C---- Upper triangle
C
        DO 2230 I=K,N
          A(I,L) = A(I,L)*AA
          A(L,I)=A(L,I)-FM02AD(L-1,A(L,1),IA,A(1,I),1)
 2230   CONTINUE
  230 CONTINUE
C
  240 IF(IENT -2)250,500,720
C
  250 IF(E .LE. ZERO)GO TO 270
      DO 260 I=1,N
        W(I,5) = B(I)
        W(I,2) = ZERO
  260 CONTINUE
      IT = 0
C
C---- Forward substitution
C
  270 DO 280 I=1,N
        WW = W(I,1)
        AA = B(II)
        B(II) = B(I)
        B(I)=AA-FM02AD(I-1,A(I,1),IA,B(1),1)
  280 CONTINUE
      ENORMA = ENORM
      ENORM = ZERO
C
C---- Calculate norms of x,change in solution
C     backward substitution
C
      DO 310 K=1,N
        I=N+1-K
        AA = A(I,I)
        IF(DABS(AA) .EQ. ZERO)GO TO 290
        AC=B(I)-FM02AD(N-I,A(I,I+1),IA,B(I+1),1)
        B(I) = AC/AA
        GO TO 300
  290   IP=I
        B(I) = ZERO
  300   AM=ONE
        IF(JSCALE.GT.0)AM=W(I,4)
        ENORM=DMAX1(ENORM, DABS(B(I))/AM)
  310 CONTINUE
      IF((E .LE. ZERO) .OR. (IP .NE. 0 ))GO TO 720
      IF(IT .EQ. 0)GO TO 320
      IF(ENORM .GT. P5*ENORMA) GO TO 460
      IF(ENORM   -  EPS*XNORM)470,470,330
  320 XNORM=ENORM
      CALL FA01CS(IRANDL,IRANDR)
      CALL FA01DS(21845,21845)
C
C---- Update solution vector x
C
  330 DO 340 I=1,N
        W(I,2) = W(I,2)+B(I)
  340 CONTINUE
      IT = IT+1
C
C---- Compute residual
C
  350 DO 450 J=1,N
        IF(IENT .NE. 2)GO TO 360
        AA = ZERO
        IF(L .EQ. J)AA = ONE
        GO TO 370
  360   AA = W(J,5)
  370   AC=AA-FM02AD(N,W(J,6),N,W(1,2),1)
        AA = ZERO
        IF(EA)400,430,380
C
C---- Make pseudo random changes to elements of a and b
C
  380   DO 390 K=1,N
          AA = AA+FA01AS(-K)*W(J,K+5)*W(K,2)
  390   CONTINUE 
        GO TO 420
  400   DO 410 K=1,N
          AA = AA+FA01AS(-K)*W(K,2)
  410   CONTINUE
  420   AC =AC-DABS(EA)*AA
  430   IF(IENT .EQ. 2)GO TO 440
        AA  = DABS(EB)*FA01AS(-J)
        IF(EB .GE. ZERO)AA=AA*W(J,5)
        AC = AA+AC
        B(J) = AC
        GO TO 450
  440   W(J,5) = AC
  450 CONTINUE
      IF(IENT-2)270,630,270
  460 ENORM = ENORMA
  470 DO 480 I=1,N
        B(I) = W(I,2)
  480 CONTINUE
      E=ENORM
      IF(JSCALE .LE. 0)GO TO 710
C
C---- Set up accuracy estimates for solution vector.
C
      ERR = ZERO
      DO 490 I=1,N
        W(I,2) = ENORM*W(I,4)
        AB = W(I,2)
        ERR = DMAX1(ERR,AB)
  490 CONTINUE
      E=ERR
      GO TO 710
C
C---- Overwrite lu factorisation of a by inverse of permuted a.
C
  500 IF(N .LT. 2)GO TO 520
      DO 5510 I=2,N
        K=I-1
        DO 510 J=1,K
          A(I,J)=-A(I,J)-FM02AD(I-1-J,A(I,J+1),IA,A(J+1,J),1)
  510   CONTINUE
 5510 CONTINUE
  520 DO 540 K=1,N
        I = N+1-K
        ERR = ONE/EPSN
        IF(JSCALE .GT. 0)ERR = ERR*W(I,4)
        DO 530 J=I,N
          W(J,2) = A(I,J)
          A(I,J) = ZERO
  530   CONTINUE
        A(I,I) = ONE
        AA = ONE/W(I,2)
        DO 5540 J=1,N
          AB = ONE
          IF (JSCALE .GT. 0)AB = W(J,3)
          AC=A(I,J)-FM02AD(N-I,W(I+1,2),1,A(I+1,J),1)
          A(I,J) = AC*AA
          IF( DABS(A(I,J)) .GE. ERR*AB)IP=N*I+J
 5540   CONTINUE
  540 CONTINUE
      ANORM = ZERO
      DO 590 K=1,N
        I = N+1-K
        WW = W(I,1)
        IF(II .EQ. I)GO TO 570
        IF(JSCALE .LE. 0)GO TO 550
        AA = W(I,3)
        W(I,3) = W(II,3)
        W(II,3) = AA
  550   DO 560 J=1,N
          AA = A(J,II)
          A(J,II) = A(J,I)
          A(J,I) = AA
  560   CONTINUE
        W(I,1) = W(II,1)
  570   ENORM = ZERO
        DO 580 J=1,N
          AB =  DABS(A(J,II))
          IF(JSCALE.GT.0)AB=AB/W(J,4)
          ENORM = DMAX1(ENORM,AB)
  580   CONTINUE
        W(II,1) = ENORM
        AB = ONE
        IF(JSCALE.GT.0)AB=W(II,3)
        ANORM = DMAX1(ANORM,ENORM/AB)
  590 CONTINUE
      IF((E .LE. ZERO) .OR. (IP .NE. 0 ))GO TO 720
      CALL FA01CS(IRANDL,IRANDR)
      CALL FA01DS(21845,21845)
      AXNORM=ANORM
  600 ANORMA = ANORM
      ANORM = ZERO
      L=0
  610 L=L+1
C
C---- Inverse of a is iteratively refined by columns
C     make copy of appropiate column.
C
      DO 620 I=1,N
        W(I,2) = A(I,L)
  620 CONTINUE
      GO TO 350
C
C---- Calculate the change in appropiate column of inverse of a.
C
  630 ENORM = ZERO
      DO 640 I=1,N
        W(I,2) = ZERO
        W(I,2)=W(I,2)+FM02AD(N,A(I,1),IA,W(1,5),1)
        AM=ONE
        IF(JSCALE.GT.0)AM=W(I,4)
        ENORM=DMAX1(ENORM, DABS(W(I,2))/AM)
  640 CONTINUE
      AB = W(L,1)
      IF(ENORM .GT. P5*AB)GO TO 660
C
C---- Update appropiate column of inverse of a.
C
      DO 650 J=1,N
        A(J,L) = A(J,L)+W(J,2)
  650 CONTINUE
      W(L,1) = ENORM
      AB = ENORM
  660 ERR = ONE
      IF(JSCALE .GT. 0)ERR = W(L,3)
      ANORM = DMAX1(ANORM,AB/ERR)
      IF(L.LT.N)GO TO 610
      IF(ANORM .GT. P5*ANORMA)GO TO 670
      IF(ANORM   -  EPS*AXNORM)680,680,600
  670 ANORM = ANORMA
  680 IF(JSCALE .LE. 0)GO TO 700
C
C----- Set up accuracy estimates for matrix inverse.
C
      ENORM = ZERO
      ERR = ZERO
      DO 690 I=1,N
        AB = ANORM*W(I,3)
        ENORM = DMAX1(ENORM,AB)
        W(I,1) = AB
        AB = W(I,4)
        ERR = DMAX1(ERR,AB)
        W(I,2) = AB
  690 CONTINUE
      E=ENORM*ERR
      GO TO 710
  700 E=ANORM
  710 CALL FA01DS(IRANDL,IRANDR)
      GO TO 850
C
      ENTRY MA21DD (A,IA,N,B,W,E)
      IENT = 4
      IF ( N .LE. 0)GO TO 810
C
C---- Check whether on a previous entry a small pivot was found,if
C     so put on error flag.
C
      IP = 0
      WW = W(N,1)
      IF(II .GT. 0)GO TO 250
      IP = II
      I = -II
      II = N
      W(N,1) = WW
      GO TO 250
C
C---- The remaining instructions handle error diagnostics,etc.
C
  720 IF(IP)730,800,740
  730 II=IP
      W(N,1) = WW
      J=7
      GO TO 770
  740 IF(IP.LE.N)GO TO 760
      I=(IP-1)/N
      J=IP-N*I
      WRITE(LP,750)DICT(IENT),I,J
  750 FORMAT(A8,'HAS FOUND THAT INVERSE ELEMENT (',I4,',',I4,') IS LARGE
     +,RESULTS MAY BE UNRELIABLE')
      GO TO 790
  760 J=8
  770 I=IABS(IP)
      WRITE(LP,780)DICT(IENT),DICT(6),I,DICT(J)
  780 FORMAT(A8,'HAS FOUND THAT',A8,I4,1X,A8,',RESULTS MAY',
     +  ' BE UNRELIABLE')
  790 E=-ONE
      GO TO 850
  800 E=ZERO
      GO TO 850
  810 WRITE(LP,830)DICT(IENT),DICT(5),N
      GO TO 840
  820 WRITE(LP,830)DICT(IENT),DICT(6),L,DICT(8 )
  830 FORMAT(' ERROR RETURN FROM',2A8,I5,1X,A8)
  840 E=-TWO
  850 RETURN
      END
C
C
      INTEGER FUNCTION MATCH(I,J,K)
C     =============================
C
C---- Test k bytes of i against j
C     return match=0 if all equal, =1 if j>i else =-1
C
C
C
C     .. Scalar Arguments ..
      INTEGER                K
C     ..
C     .. Array Arguments ..
      BYTE                   I(K),J(K)
C     ..
C     .. Local Scalars ..
      INTEGER                N
C     ..
      DO 10 N = 1,K
          IF (I(N).NE.J(N)) GO TO 20
   10     CONTINUE
      MATCH = 0
      RETURN

   20 IF (J(N).LT.I(N)) THEN
          MATCH = -1

      ELSE
          MATCH = 1
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
      SUBROUTINE MC10AD(A,N,NN,DIAG,RES,IS)
C     =====================================
C
      DOUBLE PRECISION  A(NN,NN)
      LOGICAL*1 IU,IW(3)
C
C---- Res is used to return scaling factors as integral
C          powers of base, and as workspace
C     is is set to 0 on successful completion, to i if row i has only
C        zero elements, to -i if column i has only zero elements
C     diag is used to hold counts of non-zeros in rows and columns
C      and to return scaling powers
C
      REAL*4 RES(N,4)
      INTEGER*2 DIAG(N,2),JU(2),KU
C
C---- Smin is used in a convergence test on (residual norm)**2
C
      DATA SMIN/.01/
C
C---- Set up constants
C
      UU=ALOG(16.0)
      IS=0
C
C---- Initialise for accumulation of sums and products
C
      DO 2 L=1,2
      DO 2 I=1,N
      RES(I,L)=0.
      RES(I,L+2)=0.
    2 DIAG(I,L)=0
      DO 33 I=1,N
        DO 3 J=1,N
          U=DABS(A(I,J))
          IF(U.EQ.0.)GO TO 3
          U=ALOG(U)/UU
          U=AINT(U+1.0)
C
C---- Count non-zeros in row and column
C
          DIAG(I,1)=DIAG(I,1)+1
          DIAG(J,2)=DIAG(J,2)+1
          RES(I,1)=RES(I,1)+U
          RES(J,3)=RES(J,3)+U
    3   CONTINUE
   33 CONTINUE
C
C---- Compute rhs vectors testing for zero row or column
C
      SSUM=0.
      J=0
      DO 8 I=1,N
        J=J+DIAG(I,1)
          DO 9 L=1,2
            IF(DIAG(I,L).GT.0 )GO TO 153
            DIAG(I,L)=1
            IS=I*(3-2*L)
  153       CONTINUE
            U=FLOATI(DIAG(I,L))
            RES(I,2*L-1)=RES(I,2*L-1)/U
    9     CONTINUE
        SSUM=SSUM+RES(I,3)
    8 CONTINUE
      SM=SMIN*J
C
C---- Sweep to compute initial residual vector
C
      RSUM=0.
      DO 110 I=1,N
        SUM = SSUM
        IF(DIAG(I,1).GE. N)GO TO 109
        SUM=0.
        DO 10 J=1,N
          IF(A(I,J).EQ.0D0)GO TO 10
          SUM=SUM+RES(J,3)
   10   CONTINUE
  109   U=FLOATI(DIAG(I,1))
        RES(I,1)=RES(I,1)-SUM/U
        RSUM=RSUM+RES(I,1)
  110 CONTINUE
C
C---- Initialise iteration
C
      E=0.
      E1=0.
      Q=1.
      S=0.
      DO 11 I=1,N
        U=FLOATI(DIAG(I,1))
        S=S+U*RES(I,1)**2
   11 CONTINUE
      L=2
      IF(S.LE.SM)GO TO 100
C
C---- Iteration step
C
   20 EM=E*E1
C
C---- Sweep through matrix to update residual vector
C
      DO 220 I=1,N
        SUM=RSUM
        IF(DIAG(I,L).GE. N)GO TO 220
        SUM=0.
        DO 22 J=1,N
          IF(L.EQ.2)GO TO 21
          IF(A(I,J))19,22,19
   21     IF(A(J,I))19,22,19
   19     SUM=SUM+RES(J,3-L)
   22   CONTINUE
        RES(I,L)=RES(I,L)+SUM
  220 CONTINUE
      S1=S
      S=0.
      RSUM=0.
      DO 23 I=1,N
        V=-RES(I,L)/Q
        U=FLOATI(DIAG(I,L))
        RES(I,L)=V/U
        RSUM=RSUM+RES(I,L)
        S=S+V*RES(I,L)
   23 CONTINUE
      E1=E
      E=Q*S/S1
      Q1=Q
      Q=1.-E
      M=3-L
      IF(S.GT.SM)GO TO 27
      E=M-1
      M=1
      Q=1.
   27 IF(L.EQ.2)GO TO 25
      QM=Q*Q1
      DO 24 I=1,N
        RES (I,4)=(EM*RES(I,4)+RES(I,2))/QM
        RES(I,3)=RES(I,3)+RES(I,4)
   24 CONTINUE
   25 L=M
      DO 26 I=1,N
        U=FLOATI(DIAG(I,L))
        RES(I,L)=RES(I,L)*U*E
   26 CONTINUE
      IF(S.GT.SM  )GO TO 20
C
C---- Sweep through matrix to get row scaling powers
C
  100 DO 1103 I=1,N
        DO 103 J=1,N
          U=DABS(A(I,J))
          IF(U.EQ.0.)GO TO 103
          U=ALOG(U)/UU
          U=AINT(U+1.0)
          RES(I,1)=RES(I,1)+RES(J,3)-U
  103   CONTINUE
 1103 CONTINUE
C
C--- Convert powers to integers
C
      DO 104 I=1,N
        U=FLOATI(DIAG(I,1))
        V=RES(I,1)/U
        DIAG(I,1)=V+SIGN(0.5,V)
        DIAG(I,2)=-(RES(I,3)+SIGN(0.5,RES(I,3)))
  104 CONTINUE
C
C---- Set scaling factors in res
C
      DO 1105 L=1,2
        DO 105 I=1,N
          RES(I,L)=16.0**DIAG(I,L)
  105   CONTINUE
 1105 CONTINUE
C
      RETURN
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
