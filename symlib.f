C
C   SYMLIB
C
C $Date$
C
C
C    centr.f       centric.f     determ.f     epsln.f
C    epslon.f      invsym.f      keyhkl.f     maxval.f
C    msyget.f      msymlb.f      pgdefn.f     pgmdf.f
C    pgnlau.f      prmvci.f      prmvcr.f     
C    rotfix.f      symfr2.f      symtrn.f     sysab.f
C    xspecials.f   patsgp
C       RDSYMM       ASUSET       ASUPUT       ASUGET	ASUPHP
C       PRTRSM       INASU 
C
C-------------------------------------  Changes  -----
C
C  Added PATSGP                  15/12/92  PRE
C  MSYMLB ignores blank lines in SYMOP file
C
C  Change call to EPSLN           4/10/91  PRE
C
C    SYMMAT opens a file PGIN  and then uses PGIN as a variable name.
C  Disaster on SGI - I have changed the file assignment to PGDATA
C   July 1989  - this file contains the following subroutines
C
C
C  Brief Description.
C  =================
C
C---- SUBROUTINE SYMFR2(ICOL,I1,NS,ROT)
C                translates a character string  into a 4*4 matrix.
C         On exit, ROT(4,4,NS) contains the real-space symmetry
C                  matrices, in standard convention, ie
C
C                      [x']    = [s][x]
C
C                x'(I)=Sum(J=1,3)ROT(I,J,NS)*x(J) + ROT(I,4,NS)
C                 ROT(I,4,NS)    contains the fractional translations
C
C
C---- SUBROUTINE INVSYM(S,ST)
C                Inverts 4*4 matrices - used here to get inverse
C                symmetry operation for generating equivalent h k l.
C                    ie                 [h']    = [h][St]
C
C                   h'(j) =Sum(I=1,3)[ h(i)*St(I,J,NS)]
C
C---- ROTFIX PRMVCI PRMVCR DETERM
C
C        Three subroutines for permuting symmetry operations
C        They do not really belong here but they are widely used
C        invisibly in FFT routines using symmetry operations to
C        permute axes for easier fast fourier calculations.
C
C---- SUBROUTINE ROTFIX
C                Permutes inverse symmetry operations
C
C---- SUBROUTINE PRMVCI(PERM,JV,N,N1)
C                Permute integer vector JV(N,3) by permutation
C                vector KP
C                   KP  is set in a common block
C                   N1 is first dimension of JV
C
C
C---- SUBROUTINE PRMVCR(PERM,AV,N,N1)
C                Permute real vector AV(N,3) by permutation
C                vector KP
C                   KP  is set in a common block
C                   N1 is first dimension of AV
C
C
C---- SUBROUTINE MSYGET(IST,LSPGRP,NSYM,ROT)
C                Get symmetry operations for space-group LSPGRP
C                from library file on stream IST, logical name SYMOP.
C         Returns NSYM           = number of symmetry operations
C                 ROT(4,4,NSYM)  = rotation/translation  matrices
C
C.....  Calls SYMFR2
C
C  Suggested:
C   SUBROUTINE MSYMLB(IST,LSPGRP,NAMSPG,NAMPG,NSYMP,NSYM,RSYM)
C
C Get symmetry operations from library file
C on stream IST, logical name SYMOP.
C  Space group defined by LSPGRP - spacegroup number or
C                         NAMSPG - spacegroup name.
C
C
C Returns
C   LSPGRP      spacegroup number
C   NAMSPG      spacegroup name
C   NAMPG       pointgroup name
C   NSYMP        number of primitive symmetry operations
C   NSYM         number of symmetry operations
C   ROT(4,4,NSYM)  rotation/translation  matrices
C
C.....  Calls SYMFR2
C
C
C---- SYMTRN(NSM,RSM)
C           symmetry translation from matrix back to characters
C
C           This translates the Symmetry matrices into INT TAB
C           character strings
C
C           It gives the real and reciprocal space operations.
C                eg     X,Y,Z        H  , K, L
C                eg     -Y,X-Y, Z   -H-K, H, L  etc
C           That is more complicated than you might think!!
C
C          It then calls subroutines EPSLN and CENTRIC to give the
C          centric zones and the epslin zones for h k l
C          It also sets up the common block needed to test systematic
C           absences.
C
C---- CENTRIC(NSM,RSMT,IPRINT)
C       This is Randy Read's method of defining centric reflections.
C       subroutine centric works out the numbers from Symmetry
C
C         Read number of centric zones, then centric zone cards.
C         Zones are encoded using an idea from a program by bricogne.
C         If h*zone(1) + k*zone(2) + l*zone(3) is equal to 0.0,
C         that reflection is in that zone.  all that is needed is the
C         most general conditions--a reflection is either centric or
C         not. Some care is warranted in setting up these cards.  For
C         example, for the zone 0 k 0, the most obvious test would be
C         1 0 1.  However, if l can take negative values, then
C         reflections of the type h k -h could incorrectly satisfy
C         the test. If the maximum h is < 100, then 1 0 100 would work.
C         So it is necessary to think, for each test, about what
C         other reflections in the data set could spuriously satisfy
C         it.
C
C
C---- CENTR(IH,IC)
C
C         Determine whether a reflection is centric (return ic=1)
C         or not (ic=0).  If none of the zone tests is satisfied,
C         the reflection is non-centric.
C
C---- SUBROUTINE EPSLN(NSM,NSMP,RSMT,IPRINT)
C
C       It works out the epsilon cards from consideration of the
C       Symmetry and a set of standard reflections.
C       This is Randys program description
C
C         Read number of epsilon zones (see rollett p. 126)
C         then epsilon cards. zones defined as for centric zones, but
C         fourth number on each line is the multiplicity corresponding
C         to this zone.  last card should always be 0 0 0 n, where n is
C         appropriate for the lattice (1 for primitive, 2 for face-
C         centred, etc.), so that general reflections get a value
C         for epsilon. be very careful of the order of the zones. cards
C         for reciprocal lattice rows should be given before those for
C         planes, because the first test that is satisfied defines
C         the zone.
C
C       set up tests for
C        h00 0k0 00l hh0 h0h 0kk h,-h0 h0-h 0k-k -h2h0 2h-h0 hhh
C
C
C---- SUBROUTINE EPSLON(IH,EPSI,ISYSAB)
C     Systematic absences flagged with ISYSAB = 1
C
C       Find the zone a reflection falls into, and return the
C       appropriate value for the reflection multiplicity factor.
C       each reflection must have a zone.
C
C---- SUBROUTINE SYSAB(IN,ISYSAB)
C     Systematic absences flagged with ISYSAB = 1
C
C       Test reflections for Systematic absences
C       Only reflns with EPSI gt 1 need be considered
C
C
C---- SUBROUTINE PGDEFN(NAMPG,NSYMP,NSYM,RSYMT,IPRINT)
C
C      This subroutine chooses the primitive set of symmetry operators.
C
C         It re-orders the symmetry operators to give the primitive ones
C      first.
C
C      This subroutine works out the NAMPG read from PGDATA.LIB
C      from the primitive   symmetry operators.
C      That is ; it checks rotation axes, etc etc and recognises these
C      point groups.  (It DOES NOT cope with mirror planes etc)
C
C       MDF stuff:  It now sets up the common block MDFPAR for MDF file
C       mods and  fills in the symmetry info.  See subroutine for
C       details
C
C      COMMON /MDFPAR/MAXR,MAXB,CELL(6),ISLOW,INTER,IFAST,KLASS,ICENTR,
C     +       ISCREW(3),IVERSN
C
C
C---- SUBROUTINE PGMDF(JLASS,JCENTR,JSCREW)
C
C     Use this subroutine to transfer information to and from MDFPAR.
C  If JLASS eq 0   then fill JLASS JCENTR JSCREW from common block.
C  If JLASS gt 0   then fill KLASS ICENTR ISCREW in common block.
C
C
C---- SUBROUTINE PGNLAU(NAMPG,NLAUE,LAUNAM)
C
C       This subroutine returns a laue code number used to choose
C      the unique region of reciprocal space for
C      each point group.  This region is the same as the one chosen
C      in the subroutines TURNIP and TURN3 for most point groups.
C      I am not sure what TURN3 did with the cubic spacegroups.
C
C       The number nlaue is the same as the one set in CADLCF for
C       this purpose.
C
C
C   3 pg1     1bar      hkl:l>=0  hk0:h>=0  0k0:k>=0   1,2
C   4 pg2    2/m        hkl:k>=0, l>=0  hk0:h>=0       3/b,4/b....
C   6 pg222  mmm        hkl:h>=0, k>=0, l>=0            16 ...
C   7 pg4    4/m        hkl:h>=0, l>=0 with k>=0 if  h=0  and
C                                                k>0 if h>0
C   8 pg422 4/mmm       hkl:h>=0, k>=0, l>=0            89..
C   9 pg3     3bar      hkl:h>=0, k>0  00l:l>0         143..
C  10 pg312  3/m        hkl:h>=0, k>=0 with k<=h for all l.
C                           if k = 0  l>=0
C           Space group numbers :   149-151-153
C  11 pg321  3/m        hkl:h>=0, k>=0 with k<=h for all l.
C                           if h = k  l>=0
C           Space group numbers :   150-152-154
C  12 pg6    6/m        hkl:h>=0, k>=0, l>=0 with k>=0 if  h=0
C                       and k> 0 if h>0
C  13 pg622 6/mmm       hkl:h>=0, k>=0, l>=0 with h>=k 177..
C  14 pg23   m3         hkl:h>=0, k>=0, l>=0 with l>=h,  k>=h
C  15 pg432  m3m        hkl:h>=0, k>=0, l>=0  with  k>=l
C
C
C---- DETERM(det,a)  - Mini subroutine to evaluate determinant of matrix
C                      [A].
C
C
C
C  End of Brief Description.
C +++++++++++++++++++++++++
C
C
C     =======================
      SUBROUTINE CENTR(IH,IC)
C     =======================
C
C
C---- Determine whether a reflection is centric (return ic=1)
C     or not (ic=0).  if none of the zone tests is satisfied,
C     the reflection is non-centric.
C
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER IC
C     ..
C     .. Array Arguments ..
      INTEGER IH(3)
C     ..
C     .. Scalars in Common ..
      INTEGER NCENT
C     ..
C     .. Arrays in Common ..
      REAL CPROJ
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
C     .. Common blocks ..
      COMMON /CP/CPROJ(3,20),NCENT
C     ..
C     .. Save statement ..
      SAVE
C     ..
C
C
      IC = 0
      IF (NCENT.NE.0) THEN
C
C
        DO 10 I = 1,NCENT
          IF ((CPROJ(1,I)*IH(1)+CPROJ(2,I)*IH(2)+CPROJ(3,I)*IH(3)).EQ.
     +        0.0) GO TO 20
   10   CONTINUE
C
C
        RETURN
   20   IC = 1
      END IF
C
C
      END
C
C
C     ============================
      SUBROUTINE CENTRIC(NSM,RSM,IPRINT)
C     ============================
C
C
C---- This is Randy Read's method of defining centric reflections.
C     subroutine centric works out the numbers from Symmetry
C
C     read number of centric zones, then centric zone cards.
C     zones are encoded using an idea from a program by bricogne.
C     if h*zone(1) + k*zone(2) + l*zone(3) is equal to 0.0,
C     that reflection is in that zone.  all that is needed is the
C     most general conditions--a reflection is either centric or
C     not.  some care is warranted in setting up these cards.  for
C     example, for the zone 0 k 0, the most obvious test would be
C     1 0 1.  however, if l can take negative values, then
C     reflections of the type h k -h could incorrectly satisfy
C     the test. if the maximum h is < 100, then 1 0 100 would work.
C     so it is necessary to think, for each test, about what
C     other reflections in the data set could spuriously satisfy
C     the test.
C
C---- SIGMAA commons
C
C      COMMON /CP/ CPROJ(3,20),NCENT
C      COMMON /EPS/ EPZONE(4,20),NEZONE
C
C
C
C     .. Scalar Arguments ..
      INTEGER NSM,IPRINT
C     ..
C     .. Array Arguments ..
      REAL RSM(4,4,96)
C     ..
C     .. Scalars in Common ..
      INTEGER NCENT,NEZONE
C     ..
C     .. Arrays in Common ..
      REAL CPROJ,EPZONE
C     ..
C     .. Local Scalars ..
      INTEGER IH,IK,IL,J,NC
      CHARACTER STROUT*400
C     ..
C     .. Local Arrays ..
      REAL CPRJ(3,12)
      INTEGER IHKL(3,12),IN(3)
      CHARACTER REFTYP(12)*7
C     ..
C     .. External Subroutines ..
      EXTERNAL PUTLIN
C     ..
C     .. Common blocks ..
      COMMON /CP/CPROJ(3,20),NCENT
      COMMON /EPS/EPZONE(4,20),NEZONE
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
C
C
C
C---- set up tests for 0kl h0l hk0 hhl hkh hkk h,-hl hk-h hk-k
C      -h 2h l   2h -h l  hkl
C
      DATA REFTYP/'0kl','h0l','hk0','hhl','hkh','hkk','h -hl',' hk-h',
     +     ' hk-k','-h 2h l','2h -h l','hkl'/
      DATA IHKL/0,1,2,1,0,2,1,2,0,1,1,10,1,10,1,10,1,1,1,-1,10,1,10,-1,
     +     10,1,-1,-1,2,10,2,-1,10,1,4,8/
      DATA CPRJ/1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0,1.0,-1.0,0.0,1.0,
     +     0.0,-1.0,0.0,1.0,-1.0,1.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1.0,
     +     2.0,1.0,0.0,1.0,2.0,0.0,0.0,0.0,0.0/
C     ..
C
C
      NCENT = 0
C
C
      DO 30 NC = 1,10
        IN(1) = IHKL(1,NC)
        IN(2) = IHKL(2,NC)
        IN(3) = IHKL(3,NC)
C
C---- Generate symm equivs
C
C---- test whether h' k' l' equals -h -k -l
C
        DO 10 J = 1,NSM
          IH = IN(1)*RSM(1,1,J) + IN(2)*RSM(2,1,J) + IN(3)*RSM(3,1,J)
          IF (IH.EQ.-IN(1)) THEN
            IK = IN(1)*RSM(1,2,J) + IN(2)*RSM(2,2,J) +
     +           IN(3)*RSM(3,2,J)
            IF (IK.EQ.-IN(2)) THEN
              IL = IN(1)*RSM(1,3,J) + IN(2)*RSM(2,3,J) +
     +             IN(3)*RSM(3,3,J)
              IF (IL.EQ.-IN(3)) GO TO 20
            END IF
          END IF
   10   CONTINUE
C
C---- next symm opn
C
        GO TO 30
   20   NCENT = NCENT + 1
        CPROJ(1,NCENT) = CPRJ(1,NC)
        CPROJ(2,NCENT) = CPRJ(2,NC)
        CPROJ(3,NCENT) = CPRJ(3,NC)
C
        IF(IPRINT.GE.1) THEN
        WRITE (STROUT,FMT=6000) NCENT,REFTYP(NC)
 6000 FORMAT ('  Centric Zone ',I3,' Reflections of Type  ',A7)
C
C            **************
        CALL PUTLIN(STROUT,'CURWIN')
C            **************
        END IF
C
   30 CONTINUE
C
C
       END
C
C
C     ========================
      SUBROUTINE DETERM(DET,A)
C     ========================
C
C
C---- Parameters
C     ==========
C
C
C          A (I)     4*4 matrix to be inverted
C          AI (O)    inverse matrix
C          DET       Determinant of A
C
C
C
C---- Get cofactors of 'a' in array 'c'
C
C     .. Scalar Arguments ..
      REAL DET
C     ..
C     .. Array Arguments ..
      REAL A(4,4)
C     ..
C     .. Local Scalars ..
      REAL AM,D
      INTEGER I,I1,II,J,J1,JJ
C     ..
C     .. Local Arrays ..
      REAL AI(4,4),C(4,4),X(3,3)
C     ..
C
C
      DO 40 II = 1,4
        DO 30 JJ = 1,4
          I = 0
C
C
          DO 20 I1 = 1,4
            IF (I1.NE.II) THEN
              I = I + 1
              J = 0
              DO 10 J1 = 1,4
C
C
                IF (J1.NE.JJ) THEN
                  J = J + 1
                  X(I,J) = A(I1,J1)
                END IF
   10         CONTINUE
            END IF
   20     CONTINUE
C
C
          AM = X(1,1)*X(2,2)*X(3,3) - X(1,1)*X(2,3)*X(3,2) +
     +         X(1,2)*X(2,3)*X(3,1) - X(1,2)*X(2,1)*X(3,3) +
     +         X(1,3)*X(2,1)*X(3,2) - X(1,3)*X(2,2)*X(3,1)
          C(II,JJ) = (-1)** (II+JJ)*AM
   30   CONTINUE
   40 CONTINUE
C
C---- Calculate determinant
C
      D = 0
C
C
      DO 50 I = 1,4
        D = A(I,1)*C(I,1) + D
   50 CONTINUE
C
C---- Get inverse matrix
C
      DO 70 I = 1,4
        DO 60 J = 1,4
          AI(I,J) = C(J,I)/D
   60   CONTINUE
   70 CONTINUE
C
C
      DET = D
C
C
      END
C
C     ====================================
      SUBROUTINE EPSLN(NSM,NSMP,RSM,IPRINT)
C     =====================================
C
C
C---- It works out the epsilon cards from consideration of the Symmetry
C      and a set of standard reflections.
C
C    This is Randys program description
C
C     read number of epsilon zones (see rollett p. 126)
C     then epsilon cards. zones defined as for centric zones, but
C     fourth number on each line is the multiplicity corresponding
C     to this zone.  last card should always be 0 0 0 n, where n is
C     appropriate for the lattice (1 for primitive, 2 for face-
C     centred, etc.), so that general reflections get a value
C     for epsilon. be very careful of the order of the zones. cards
C     for reciprocal lattice rows should be given before those for
C     planes, because the first test that is satisfied defines
C     the zone.
C
C
C    set up tests for
C     h00 0k0 00l hh0 h0h 0kk h,-h0 h0-h 0k-k -h2h0 2h-h0 hhh
C
C---- SIGMAA commons
C      COMMON /CP/ CPROJ(3,20),NCENT
C      COMMON /EPS/ EPZONE(4,20),NEZONE
C
C   Set up this common block for use when searching for systematic absences
C
C     ..
C     .. Common blocks ..
      INTEGER NSMT
      REAL RSMM,RSMTT
      COMMON /SYSABS/NSMT,RSMM(4,4,96),RSMTT(4,4,96)
C
C
C     .. Scalar Arguments ..
      INTEGER NSM,NSMP,IPRINT
C     ..
C     .. Array Arguments ..
      REAL RSMT(4,4,96)
      REAL RSM(4,4,96)
C     ..
C     .. Scalars in Common ..
      INTEGER NCENT,NEZONE
C     ..
C     .. Arrays in Common ..
      REAL CPROJ,EPZONE
C     ..
C     .. Local Scalars ..
      INTEGER IH,IK,IL,J,NC,NEPS,ISTERR,IFGERR,LATMUL,N,I
      CHARACTER LINERR*100
      CHARACTER STROUT*400
C     ..
C     .. Local Arrays ..
      REAL EPZNE(3,13)
      INTEGER IHKL(3,13),IN(3)
      CHARACTER REFTYP(13)*7
C     ..
C     .. External Subroutines ..
      EXTERNAL PUTLIN,LERROR,INVSYM
C     ..
C     .. Common blocks ..
      COMMON /CP/CPROJ(3,20),NCENT
      COMMON /EPS/EPZONE(4,20),NEZONE
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
C
C
C
C---- Example h k l for testing possible epsln zones
C
      DATA REFTYP/'h00','0k0','00l','hh0','h0h','0kk','h -h0',' h0-h',
     +     ' 0k-k','-h 2h 0','2h -h 0','hhh','hkl'/
      DATA IHKL/1,0,0, 0,2,0, 0,0,2, 1,1,0, 1,0,1, 0,1,1, 1,-1,0,
     +         1,0,-1, 0,1,-1, -1,2,0, 2,-1,0, 1,1,1, 1,2,3/
      DATA EPZNE/0.0,500.0,1.0, 1.0,0.0,500.0, 1.0,500.0,0.0,
     +           1.0,-1.0,500.0, 1.0,500.0,-1.0, 500.0,1.0,-1.0,
     +           1.0,1.0,500.0,  1.0,500.0, 1.0, 500.0,1.0, 1.0,
     +           2.0,1.0,500.0,  1.0,2.0, 500.0, 500.0,500.0,500.0, 
     +           0.0,0.0,0.0/
C     ..
C
C---- I dont think this test will work for cubic symmetry - no way to
C      uniquely specify hhh  - ejd feb 1988
C
C
      LATMUL = NSM/NSMP
C
      IF(IPRINT.GT.0)THEN
       STROUT = '   ******   EPSILON ZONES -  Reflection Classes'//
     +       ' and their multiplicity ****** '
C
C          ****************
      CALL PUTLIN(STROUT,'CURWIN')
C          ****************
         END IF
C
      NEZONE = 0
      NEPS = 1
      IF (NSM.NE.1) THEN
C
C
        DO 20 NC = 1,12
          IN(1) = IHKL(1,NC)
          IN(2) = IHKL(2,NC)
          IN(3) = IHKL(3,NC)
          NEPS = 1
C
C---- Generate symm equivs
C
C---- test whether h' k' l' equals h k l
C
          DO 10 J = 2,NSMP
            IH = IN(1)*RSM(1,1,J) + IN(2)*RSM(2,1,J) +
     +           IN(3)*RSM(3,1,J)
            IF (IH.EQ.IN(1)) THEN
              IK = IN(1)*RSM(1,2,J) + IN(2)*RSM(2,2,J) +
     +             IN(3)*RSM(3,2,J)
              IF (IK.EQ.IN(2)) THEN
                IL = IN(1)*RSM(1,3,J) + IN(2)*RSM(2,3,J) +
     +               IN(3)*RSM(3,3,J)
C
C---- next symm opn
C
                IF (IL.EQ.IN(3)) NEPS = NEPS + 1
              END IF
            END IF
   10     CONTINUE
C
          IF (NEPS.NE.1) THEN
            NEZONE = NEZONE + 1
            EPZONE(4,NEZONE) = NEPS*LATMUL
            EPZONE(1,NEZONE) = EPZNE(1,NC)
            EPZONE(2,NEZONE) = EPZNE(2,NC)
            EPZONE(3,NEZONE) = EPZNE(3,NC)
C
            IF(IPRINT.GT.0)THEN
            WRITE (STROUT,FMT=6000) NEZONE
            CALL PUTLIN(STROUT,'CURWIN')
            WRITE (STROUT,FMT=6010) REFTYP(NC)
            CALL PUTLIN(STROUT,'CURWIN')
            WRITE (STROUT,FMT=6020)NEPS
            CALL PUTLIN(STROUT,'CURWIN')
            END IF
          END IF
   20   CONTINUE
C
C---- next reflection class
C
      END IF
C
C---- add H K L ezone 0 0 0   1.
C     If hkl already set this will be ignored..
C
      NEZONE = NEZONE + 1
      EPZONE(4,NEZONE) = LATMUL
      EPZONE(1,NEZONE) = EPZNE(1,13)
      EPZONE(2,NEZONE) = EPZNE(2,13)
      EPZONE(3,NEZONE) = EPZNE(3,13)
C
C
            IF(IPRINT.GT.0)THEN
            WRITE (STROUT,FMT=6000) NEZONE
            CALL PUTLIN(STROUT,'CURWIN')
            WRITE (STROUT,FMT=6010) REFTYP(13)
            CALL PUTLIN(STROUT,'CURWIN')
            WRITE (STROUT,FMT=6020)LATMUL
            CALL PUTLIN(STROUT,'CURWIN')
            END IF
C
C
      IF ((NEZONE.GT.20) .OR. (NEZONE.LT.1)) THEN
C
C
        IF (NEZONE.LT.1) THEN
          WRITE (LINERR,FMT='(A,A)')
     +' Have to have at least one EPSILON ZONE CARD ',
     +' **Execution suppressed'
          ISTERR = 2
          IFGERR = 1
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
          CALL CCPERR(1,' STOP in SYMLIB.for 3388')
      END IF
      END IF
C
C
C---- Fill common /sysabs/
C
      NSMT = NSM
C
C
      DO 80 N = 1,NSM
           CALL INVSYM(RSM(1,1,N),RSMT(1,1,N) )
        DO 70 I = 1,4
          DO 60 J = 1,4
            RSMM(J,I,N) = RSM(J,I,N)
            RSMTT(J,I,N) = RSMT(J,I,N)
   60     CONTINUE
   70   CONTINUE
   80 CONTINUE
C
C
C
C---- Format statements
C
 6000 FORMAT (' EPSILON Zone ',I3)
 6010 FORMAT (' Reflections of type ',A7)
 6020 FORMAT (' Multiplicity ',I3)
C
C
      END
C
C
C     ================================
      SUBROUTINE EPSLON(IH,EPSI,ISYSAB)
C     ================================
C
C---- Find the zone a reflection falls into, and return the
C     appropriate value for the reflection multiplicity factor.
C     each reflection must have a zone.
C     Systematic absences flagged with ISYSAB = 1
C
C
C
C
C     .. Scalar Arguments ..
      REAL EPSI
      INTEGER ISYSAB
C     ..
C     .. Array Arguments ..
      INTEGER IH(3)
C     ..
C     .. Scalars in Common ..
      INTEGER NEZONE
C     ..
C     .. Arrays in Common ..
      REAL EPZONE
C     ..
C     .. Local Scalars ..
      REAL TEST
      INTEGER I,J,ISTERR,IFGERR
      CHARACTER LINERR*200
C     ..
C     .. External Subroutines ..
      EXTERNAL SYSAB,LERROR
C     ..
C     .. Common blocks ..
      COMMON /EPS/EPZONE(4,20),NEZONE
C     ..
C     .. Save statement ..
      SAVE
C     ..
C
C
      DO 20 I = 1,NEZONE
        TEST = 0.0
C
C
        DO 10 J = 1,3
          TEST = EPZONE(J,I)*IH(J) + TEST
   10   CONTINUE
C
C
        IF (TEST.EQ.0.0) GO TO 30
   20 CONTINUE
C
C
          WRITE (LINERR,FMT='(A,3I5,A)') 
     + ' NO EPSILON ZONE found for reflection ',IH,
     + ' **EXECUTION SUPPRESSED**'
          ISTERR = 2
          IFGERR = 1
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
                CALL CCPERR(1,' STOP in SYMLIB.for 3377')
   30 EPSI = EPZONE(4,I)
      ISYSAB = 0
C
C                    *********************
      IF (EPSI.GT.1) CALL SYSAB(IH,ISYSAB)
C                    *********************
C
C
      END
C
C
C     =======================
      SUBROUTINE INVSYM(A,AI)
C     =======================
C
C---- subroutine to invert 4*4 matrices for conversion between
C     real space and reciprocal space symmetry operators.
C
C
C---- Parameters
C     ==========
C
C           A (I)   4*4 matrix to be inverted
C          AI (O)   inverse matrix
C
C
C
C---- get cofactors of 'a' in array 'c'
C
C
C     .. Array Arguments ..
      REAL A(4,4),AI(4,4)
C     ..
C     .. Local Scalars ..
      REAL AM,D
      INTEGER I,I1,II,J,J1,JJ
C     ..
C     .. Local Arrays ..
      REAL C(4,4),X(3,3)
C     ..
C
C
      DO 40 II = 1,4
        DO 30 JJ = 1,4
          I = 0
          DO 20 I1 = 1,4
            IF (I1.NE.II) THEN
              I = I + 1
              J = 0
              DO 10 J1 = 1,4
                IF (J1.NE.JJ) THEN
                  J = J + 1
                  X(I,J) = A(I1,J1)
                END IF
   10         CONTINUE
            END IF
   20     CONTINUE
C
C
          AM = X(1,1)*X(2,2)*X(3,3) - X(1,1)*X(2,3)*X(3,2) +
     +         X(1,2)*X(2,3)*X(3,1) - X(1,2)*X(2,1)*X(3,3) +
     +         X(1,3)*X(2,1)*X(3,2) - X(1,3)*X(2,2)*X(3,1)
          C(II,JJ) = (-1)** (II+JJ)*AM
   30   CONTINUE
   40 CONTINUE
C
C---- Calculate determinant
C
      D = 0
C
C
      DO 50 I = 1,4
        D = A(I,1)*C(I,1) + D
   50 CONTINUE
C
C---- Get inverse matrix
C
      DO 70 I = 1,4
        DO 60 J = 1,4
          AI(I,J) = C(J,I)/D
   60   CONTINUE
   70 CONTINUE
C
C
      END
C
C
C     =====================================
      SUBROUTINE MAXVAL(F2MOD,NREF,FKP,JKP)
C     =====================================
C
C
C
C     .. Scalar Arguments ..
      INTEGER NREF
C     ..
C     .. Array Arguments ..
      REAL F2MOD(NREF),FKP(15)
      INTEGER JKP(15)
C     ..
C     .. Local Scalars ..
      REAL FMAX
      INTEGER I,J,JMAX
C     ..
C
C
      DO 20 I = 1,15
        FMAX = 0
        JMAX = 0
C
C
        DO 10 J = 1,NREF
          IF (FMAX.LT.F2MOD(J)) THEN
            JMAX = J
            FMAX = F2MOD(J)
          END IF
   10   CONTINUE
C
C
        FKP(I) = F2MOD(JMAX)
        JKP(I) = JMAX
        F2MOD(JMAX) = 0.0
   20 CONTINUE
C
C
      END
C
C
C     ======================================
      SUBROUTINE MSYGET(IST,LSPGRP,NSYM,ROT)
C     ======================================
C
C----  Get symmetry operations for space-group LSPGRP from library
C      file  on stream IST, logical name SYMOP.
C         Returns NSYM = number of symmetry operations
C                 ROT(4,4,NSYM)  rotation/translation  matrices
C
C
C
C     .. Scalar Arguments ..
      INTEGER IST,LSPGRP,NSYM
C     ..
C     .. Array Arguments ..
      REAL ROT(4,4,*)
C     ..
C     .. Local Scalars ..
      INTEGER I,IFAIL,ISG,LDUM,NLIN,ISTERR,IFGERR
      CHARACTER LINE*80,LINERR*200
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPOPN,SYMFR2,LERROR
C     ..
C
C---- Open symmetry file
C
      IFAIL = 1
      CALL CCPOPN(IST,'SYMOP',5,1,LDUM,IFAIL)
      IF (IFAIL.LT.0) THEN
C
C---- Error conditions
C
          WRITE (LINERR,FMT='(A)')
     + ' **SYMMETRY FILE ERROR**'
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
          WRITE (LINERR,FMT='(A)')
     +    ' **MSYGET: ERROR In opening symop file**'
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
      ELSE
        NSYM = 0
   10   CONTINUE
C
C---- Find correct space-group in file.
C     Each space-group has header line of space-group number,
C     number of lines of symmetry operations
C
        READ (IST,FMT=*,END=30) ISG,NLIN
        IF (ISG.EQ.LSPGRP) THEN
          GO TO 40
        ELSE
C
C---- Skip NLIN lines
C
          DO 20 I = 1,NLIN
            READ (IST,FMT=*)
   20     CONTINUE
C
C
          GO TO 10
        END IF
   30   CONTINUE
           WRITE (LINERR,FMT='(A)')
     + ' **SYMMETRY FILE ERROR**'
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
          WRITE (LINERR,FMT='(A,A,I5,A)')
     + ' **MSYGET: NO SYMMETRY information for SPACE GROUP ',
     + ' Number',LSPGRP,' in SYMOP file**'
C
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
        GO TO 60
C
C---- Space-group found,
C     convert NLIN lines of symmetry operators to matrices
C
   40   CONTINUE
C
C
        DO 50 I = 1,NLIN
          READ (IST,FMT=6000) LINE
C
C---- Convert line to matrices
C
          NSYM = NSYM + 1
C
C              ***********************
          CALL SYMFR2(LINE,1,NSYM,ROT)
C              ***********************
C
   50   CONTINUE
C
C
        REWIND IST
        RETURN
      END IF
C
C
   60 CONTINUE
          WRITE (LINERR,FMT='(A,A)') 
     +  ' **PROGRAM TERMINATED** ',
     +  '**SYMMETRY FILE ERROR**'
          ISTERR = 2
          IFGERR = 1
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
                CALL CCPERR(1,' STOP in SYMLIB.for 3366')
C
C---- Format statements
C
 6000 FORMAT (A)
C
C
      END
C
C
C     =========================================================
      SUBROUTINE MSYMLB(IST,LSPGRP,NAMSPG,NAMPG,NSYMP,NSYM,ROT)
C     =========================================================
C
C---- Get symmetry operations for spacegroup LSPGRP from library file
C     on stream IST, logical name SYMOP.
C
C   In the library file, the header for each entry is
C
C      LSPGRP   NLINS   NLINP   NAMSPG  NAMPG
C
C  where  LSPGRP        spacegroup number
C         NLINS         total number of lines of symmetry operators.
C         NLINP         number of LINES of primitive symmetry operators
C         NAMSPG        spacegroup name
C         NAMPG         name of corresponding pointgroup
C
C On entry:
C   IST         stream number to read file
C   LSPGRP      spacegroup number
C   NAMSPG      spacegroup name: this will be used to find the
C                       spacegroup only if LSPGRP = 0
C
C
C Returns
C   LSPGRP      spacegroup number
C   NAMSPG      spacegroup name
C   NAMPG       pointgroup name
C   NSYMP       number of primitive symmetry operations - only different
C               from NSYM in non-primitive spacegroups
C   NSYM        total number of symmetry operations
C   ROT(4,4,NSYM)  rotation/translation  matrices
C
C
C     .. Parameters ..
      INTEGER NPARSE
      PARAMETER (NPARSE=200)
C     ..
C     .. Scalar Arguments ..
      INTEGER IST,LSPGRP,NSYM,NSYMP
      CHARACTER NAMPG* (*),NAMSPG* (*)
C     ..
C     .. Array Arguments ..
      REAL ROT(4,4,*)
C     ..
C     .. Local Scalars ..
      LOGICAL PRINT
      INTEGER I,IFAIL,ISG,LEND,NLIN,NLINS,NTOK,ISTERR,IFGERR
      CHARACTER KEY*4,LINE*400,LINERR*400
C     ..
C     .. Local Arrays ..
      REAL FVALUE(NPARSE)
      INTEGER IBEG(NPARSE),IDEC(NPARSE),IEND(NPARSE),ITYP(NPARSE)
      CHARACTER CVALUE(NPARSE)*4
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPDPN,CCPUPC,PARSER,SYMFR2,LERROR,LENSTR
      INTEGER LENSTR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
C
C---- Open symmetry file
C
      IFAIL = 1
C
C          ******************************************
      CALL CCPDPN(IST,'SYMOP','READONLY','F',0,IFAIL)
C          ******************************************
C
      IF (IFAIL.LT.0) THEN
C
C---- Error conditions
C
          WRITE (LINERR,FMT='(A,A)')
     +   ' **SYMMETRY FILE ERROR**',
     +   ' **MSYMLB: Error in opening SYMOP FILE**'
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
      ELSE
C
        NTOK = 0
        NSYM = 0
   10   CONTINUE
C
C---- Find correct space-group in file.
C     Each space-group has header line of space-group number,
C     number of line of symmetry operations for non-primitive
C     and primitive cells.
C
        READ (IST,FMT='(A)',END=30) LINE
C
C---- Ignore blank lines
	IF (LENSTR(LINE) .EQ. 0) GO TO 10
C
C            ************
        CALL CCPUPC(LINE)
C            ************
C
        NTOK = NPARSE
C
C            *******************************************************
        CALL PARSER(KEY,LINE,IBEG,IEND,ITYP,FVALUE,CVALUE,IDEC,NTOK,
     +              LEND,PRINT)
C            *******************************************************
C
C---- Fields are space group number,
C                number of lines,
C                number of lines in primitive cell symmetry,
C                spacegroup name
C
        IF (ITYP(1).NE.2 .OR. ITYP(2).NE.2 .OR. ITYP(3).NE.2) THEN
          GO TO 70
        ELSE
          ISG = NINT(FVALUE(1))
          NLIN = NINT(FVALUE(2))
          NLINS = NINT(FVALUE(3))
          IF (LSPGRP.GT.0) THEN
C
C---- Check for spacegroup number given
C
            IF (LSPGRP.EQ.ISG) GO TO 40
C
C---- Check for spacegroup name given
C
          ELSE IF (NAMSPG.EQ.LINE(IBEG(4) :IEND(4))) THEN
            GO TO 40
          END IF
C
C---- Not this one, skip NLIN lines
C
          DO 20 I = 1,NLIN
            READ (IST,FMT=*)
   20     CONTINUE
C
C
          GO TO 10
        END IF
   30   CONTINUE
          WRITE (LINERR,FMT='(A,A,A)')
     +   ' **SYMMETRY FILE ERROR**',
     +   ' **MSYMLB: NO SYMMETRY information for space group in ',
     +   'SYMOP file**'
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
        GO TO 80
C
C---- Space-group found, convert NLIN lines of
C     symmetry operators to matrices
C
   40   LSPGRP = ISG
        NAMSPG = LINE(IBEG(4) :IEND(4))
        NAMPG = LINE(IBEG(5) :IEND(5))
C
        DO 50 I = 1,NLINS
          READ (IST,FMT='(A)') LINE
C
C---- Convert line to matrices
C
          NSYM = NSYM + 1
C
C              ***********************
          CALL SYMFR2(LINE,1,NSYM,ROT)
C              ***********************
C
   50   CONTINUE
C
C
        NSYMP = NSYM
C
        IF (NLIN.GT.NLINS) THEN
C
C
          DO 60 I = NLINS + 1,NLIN
            READ (IST,FMT='(A)') LINE
C
C---- Convert line to matrices
C
            NSYM = NSYM + 1
C
C                ***********************
            CALL SYMFR2(LINE,1,NSYM,ROT)
C                ***********************
C
   60     CONTINUE
        END IF
C
        CLOSE (IST)
        RETURN
C
C
   70   CONTINUE
          WRITE (LINERR,FMT='(A,A,A)')
     +   ' **SYMMETRY FILE ERROR**',
     +   ' **MSYMLB: Error in format of SYMOP FILE**'
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
      END IF
C
C
   80 CONTINUE
          WRITE (LINERR,FMT='(A,A)')
     +  ' **SYMMETRY FILE ERROR** ',
     +  ' **PROGRAM TERMINATED**'
          ISTERR = 2
          IFGERR = 1
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
                CALL CCPERR(1,' STOP in SYMLIB.for 3355')
C
C
      END
C
C
C     =====================================
      SUBROUTINE PGMDF(JLASS,JCENTR,JSCREW)
C     =====================================
C
C---- Use this subroutine to transfer information
C  If JLASS eq 0   then fill JLASS JCENTR JSCREW from common block.
C  If JLASS gt 0   then fill KLASS ICENTR ISCREW in common block.
C
C
C
C     .. Scalar Arguments ..
      INTEGER JCENTR,JLASS
C     ..
C     .. Array Arguments ..
      INTEGER JSCREW(3)
C     ..
C     .. Scalars in Common ..
      INTEGER ICENTR,IFAST,INTER,ISLOW,IVERSN,KLASS,MAXB,MAXR
      CHARACTER STROUT*400
C     ..
C     .. Arrays in Common ..
      REAL CELL
      INTEGER ISCREW
C     ..
C     .. Local Scalars ..
      INTEGER ISCR
C     ..
C     .. Common blocks ..
      COMMON /MDFPAR/MAXR,MAXB,CELL(6),ISLOW,INTER,IFAST,KLASS,ICENTR,
     +     ISCREW(3),IVERSN
      SAVE /MDFPAR/
C     ..
C
C
      IF (JLASS.EQ.0) THEN
       STROUT = ' Filling  JLASS JCENTR JSCREW from common block.'
       CALL PUTLIN(STROUT,'CURWIN')
        JLASS = KLASS
        JCENTR = ICENTR
        JSCREW(1) = ISCREW(1)
        JSCREW(2) = ISCREW(2)
        JSCREW(3) = ISCREW(3)
      END IF
C
      IF (JLASS.GT.0) THEN
        STROUT = ' Filling  KLASS ICENTR ISCREW in common block.'
C
C            ****************
        CALL PUTLIN(STROUT,'CURWIN')
C            ****************
C
        KLASS = JLASS
        ICENTR = JCENTR
        ISCREW(1) = JSCREW(1)
        ISCREW(2) = JSCREW(2)
        ISCREW(3) = JSCREW(3)
C
        STROUT = ' **** ICENTR   gives  axis of centering *****'
C
C            *****************
        CALL BLANK('CURWIN',2)
        CALL PUTLIN(STROUT,'CURWIN')
C            *****************
C
        IF (ICENTR.EQ.0) THEN
         STROUT = '  No centering              (P spacegroups)'
        ELSE IF (ICENTR.EQ.1) THEN
         STROUT = '  Centering around a-axis    (A spacegroups)'
        ELSE IF (ICENTR.EQ.2) THEN
         STROUT = '  Centering around b-axis    (B spacegroups)'
        ELSE IF (ICENTR.EQ.3) THEN
         STROUT = '  Centering around c-axis    (C spacegroups)'
        ELSE IF (ICENTR.EQ.4) THEN
         STROUT = '  Centering on all faces     (F spacegroups)'
        ELSE IF (ICENTR.EQ.5) THEN
         STROUT = '  Body centering             (I spacegroups)'
        ELSE IF (ICENTR.EQ.6) THEN
         STROUT = '  Rhombohedral centering     (R spacegroups with'//
     +      '  hexagonal axes)'//
     +      '      (NOTE: R-spacegroups with rhombohedral axes'//
     +      ' have ICENTR = 0!)'
         END IF
          CALL PUTLIN(STROUT,'CURWIN')
C
C
        ISCR = ISCREW(1) + ISCREW(2) + ISCREW(3)
C
        IF (ISCR.GT.0) WRITE (6,FMT='(//,A)')
     +      ' **** Screw axes are: *****'
        IF (ISCREW(1).GT.0) WRITE (6,FMT='(//,I4,A)') ISCREW(1),
     +      'fold screw axis along A '
        IF (ISCREW(2).GT.0) WRITE (6,FMT='(//,I4,A)') ISCREW(2),
     +      'fold screw axis along B '
        IF (ISCREW(3).GT.0) WRITE (6,FMT='(//,I4,A)') ISCREW(3),
     +      'fold screw axis along C '
C
        WRITE (6,FMT='(//,A)')
     +    ' *** KLASS    : a crystal class name used in MDF files ***'
C
C---- (int. tables)
C
        IF (KLASS.EQ.1) WRITE (6,FMT='(//,A)')
     +      '  TRICLINIC       1_BAR (PG1)       sgs  1 '
        IF (KLASS.EQ.2) WRITE (6,FMT='(//,A)')
     +      '  MONOCLINIC   I  2/M (PG4) B-UNIQUE  sgs  3 -  5'
        IF (KLASS.EQ.3) WRITE (6,FMT='(//,A)')
     +      '  ORTHORHOMBIC    MMM (PG222)         sgs 16 - 24'
        IF (KLASS.EQ.4) WRITE (6,FMT='(//,A)')
     +      '  TETRAGONAL   I  4/M (PG4)          sgs 75 - 80'
        IF (KLASS.EQ.5) WRITE (6,FMT='(//,A)')
     +      '  TETRAGONAL  II  4/MMM (PG422)      sgs 89 - 98 '
        IF (KLASS.EQ.6) WRITE (6,FMT='(//,A)')
     +      '  TRIGONAL     I  3_BAR (PG3)HEXAGONAL AXES sgs  143-146'
        IF (KLASS.EQ.7) WRITE (6,FMT='(//,A)')
     +      '  TRIGONAL    II  3_BAR (??) RHOMBOHEDRAL AXES sgs   146'
        IF (KLASS.EQ.8) WRITE (6,FMT='(//,A)')
     +      ' TRIGONAL   III  3_BAR1M (PG312)       sgs 149,151,153 '
        IF (KLASS.EQ.9) WRITE (6,FMT='(//,A)')
     +      ' TRIGONAL    IV  3_BARM1 (PG321)HEXAGONAL AXES ',
     +      ' sgs 150,152,154,155'
        IF (KLASS.EQ.10) WRITE (6,FMT='(//,A)')
     +      '  TRIGONAL     V  3_BARM1 (??)RHOMBOHEDRAL AXES sgs 155'
        IF (KLASS.EQ.11) WRITE (6,FMT='(//,A)')
     +      '  HEXAGONAL    I  6/M  (PG6)   sgs        168 - 173'
        IF (KLASS.EQ.12) WRITE (6,FMT='(//,A)')
     +      '  HEXAGONAL   II  6/MMM (PG622)    sgs 177 - 182'
        IF (KLASS.EQ.13) WRITE (6,FMT='(//,A)')
     +      '  CUBIC        I  M3_BAR (PG23)    sgs 195 - 199'
        IF (KLASS.EQ.14) WRITE (6,FMT='(//,A)')
     +      '  CUBIC       II  M3_BARM (PG432)  sgs 207 - 214'
        IF (KLASS.EQ.15) WRITE (6,FMT='(//,A)')
     +      '  MONOCLINIC  II  2/M (??)  A UNIQUE  sgs 3 -   5'
        IF (KLASS.EQ.16) WRITE (6,FMT='(//,A)')
     +      '  MONOCLINIC III  2/M (??)  C UNIQUE sgs 3 -   5'
C
      END IF
C
C
      END
C
C
C     ===============================
      SUBROUTINE PRMVCI(PERM,JV,N,N1)
C     ===============================
C
C---- Permute vector JV(N,3) by permutation vector KP
C      N1 is first dimension of JV
C
C
C
C     .. Scalar Arguments ..
      INTEGER N,N1
C     ..
C     .. Array Arguments ..
      REAL PERM(4,4)
      INTEGER JV(N1,3)
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
C     .. Local Arrays ..
      REAL BV(3)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
C
C---- Permute
C
      DO 10 I = 1,3
        BV(I) = PERM(I,1)*JV(N,1) + PERM(I,2)*JV(N,2) +
     +          PERM(I,3)*JV(N,3)
   10 CONTINUE
C
C---- Copy back
C
      DO 20 I = 1,3
        JV(N,I) = NINT(BV(I))
   20 CONTINUE
C
C
      END
C
C
C     ===============================
      SUBROUTINE PRMVCR(PERM,AV,N,N1)
C     ===============================
C
C---- Permute vector AV(N,3) by permutation vector KP
C           N1 is first dimension of AV
C
C
C
C     .. Scalar Arguments ..
      INTEGER N,N1
C     ..
C     .. Array Arguments ..
      REAL AV(N1,3),PERM(4,4)
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
C     .. Local Arrays ..
      REAL BV(3)
C     ..
C
C---- Permute
C
      DO 10 I = 1,3
        BV(I) = PERM(I,1)*AV(N,1) + PERM(I,2)*AV(N,2) +
     +          PERM(I,3)*AV(N,3)
   10 CONTINUE
C
C---- Copy back
C
      DO 20 I = 1,3
        AV(N,I) = BV(I)
   20 CONTINUE
C
C
      END
C
C
C     =================
      SUBROUTINE ROTFIX
C     =================
C
C     .. Scalars in Common ..
      INTEGER NSYM
C     ..
C     .. Arrays in Common ..
      REAL PERM,ROT,ROTT
      INTEGER JJJNK
C     ..
C     .. Local Scalars ..
      INTEGER I,ISYM,J,JX,JY,JZ
C     ..
C     .. Local Arrays ..
      REAL R1(4,4),R2(4,4)
      CHARACTER NAME(3)*1
C     ..
C     .. Common blocks ..
      COMMON /ATSYM/ROT(4,4,96),ROTT(4,4,96),NSYM,PERM(4,4),JJJNK(9)
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
      DATA NAME/'X','Y','Z'/
C     ..
C
C
C
      DO 10 I = 1,3
        IF (PERM(1,I).EQ.1.0) JX = I
        IF (PERM(2,I).EQ.1.0) JY = I
        IF (PERM(3,I).EQ.1.0) JZ = I
   10 CONTINUE
C
C
      WRITE (6,FMT=6000) NAME(JX),NAME(JY),NAME(JZ)
C
C
      DO 60 ISYM = 1,NSYM
        WRITE (6,FMT=6002) ISYM, ((ROT(I,J,ISYM),J=1,3),I=1,3),
     +    (ROT(J,4,ISYM),J=1,3)
        IF (JX.NE.1 .OR. JY.NE.2) THEN
C
C
          DO 30 I = 1,4
            DO 20 J = 1,4
              R2(J,I) = ROTT(J,1,ISYM)*PERM(I,1) +
     +                  ROTT(J,2,ISYM)*PERM(I,2) +
     +                  ROTT(J,3,ISYM)*PERM(I,3) +
     +                  ROTT(J,4,ISYM)*PERM(I,4)
              R1(J,I) = PERM(J,1)*ROT(1,I,ISYM) +
     +                  PERM(J,2)*ROT(2,I,ISYM) +
     +                  PERM(J,3)*ROT(3,I,ISYM) +
     +                  PERM(J,4)*ROT(4,I,ISYM)
   20       CONTINUE
   30     CONTINUE
C
C
          DO 50 J = 1,4
            DO 40 I = 1,4
              ROT(J,I,ISYM) = R1(J,I)
              ROTT(J,I,ISYM) = R2(J,I)
   40       CONTINUE
   50     CONTINUE
C
C
          WRITE (6,FMT=6004) ISYM, ((ROT(I,J,ISYM),J=1,3),I=1,3),
     +      (ROT(J,4,ISYM),J=1,3)
        END IF
   60 CONTINUE
C
C---- Format statements
C
 6000 FORMAT ('  Input X used as ',A2,'    Input Y used as ',A2,'    I',
     +       'nput IZ used as ',A2)
 6002 FORMAT (' Int Tab Symmetry ',I3,4 (5X,3F6.2))
 6004 FORMAT (' Transformed Symmetry ',I3,4 (5X,3F6.2))
C
C
      END
C
C
C     =================================
      SUBROUTINE SYMFR2(ICOL,I1,NS,ROT)
C     =================================
C
C---- Read and interpret symmetry operations
C                                    
C On entry, ICOL contains line of 80 characters , I1 is the first
C column to look at (say after keyword 'SYM')
C
C NS is the number of the first symmetry operation to be read, & returns
C    with the number of the last one read.
C
C On exit, ROT(4,4,NS) contains the real-space symmetry matrices, in
C                     standard convention, ie
C                     x'(I)=Sum(J=1,3)ROT(I,J,NS)*x(J) + ROT(I,4,NS)
C          ROT(I,4,NS)    contains the fractional translations
C
C
C
C     .. Scalar Arguments ..
      INTEGER I1,NS,I11
      CHARACTER ICOL*80
C     ..
C     .. Array Arguments ..
      REAL ROT(4,4,*)
C     ..
C     .. Local Scalars ..
      REAL A,REAL,RECIP,S,T
      INTEGER I,ICOMST,IERR,IFOUND,IMAX,IP,ISL,J,K,NOP,NP,NS1,NSYM
      CHARACTER IBLANK*1,ICH*1,ICOMMA*1,IH*1,IK*1,IL*1,IMINUS*1,IPLUS*1,
     +          IPOINT*1,IS*1,ISLASH*1,ISTAR*1,IX*1,IY*1,IZ*1,JH*1,JK*1,
     +          JL*1,JS*1,JX*1,JY*1,JZ*1
C     ..
C     .. Local Arrays ..
      INTEGER NUM(10)
      CHARACTER INUM(10)*1
C     ..
C     .. Data statements ..
      DATA IS,IX,IY,IZ,IH,IK,IL,IPLUS,IMINUS,ISLASH,IPOINT,ICOMMA,ISTAR,
     +     IBLANK/'S','X','Y','Z','H','K','L','+','-','/','.',',','*',
     +     ' '/
      DATA JS,JX,JY,JZ,JH,JK,JL/'s','x','y','z','h','k','l'/
      DATA NUM/1,2,3,4,5,6,7,8,9,0/
      DATA INUM/'1','2','3','4','5','6','7','8','9','0'/
C     ..
C
C
C
      IMAX = 80
      IERR = 0
C
      NS1 = NS
C
C---- Search for first blank to skip flag sym symtr symmetry
C     or whatever
C
      I11 = I1
      IF (I11.NE.1) THEN
        I11 = 1
        IF (ICOL(I11:I11).EQ.'s' .OR. ICOL(I11:I11).EQ.'S') THEN
   10     CONTINUE
          IF (ICOL(I11:I11).EQ.' ') THEN
            GO TO 20
          ELSE
            I11 = I11 + 1
            IF (I11.LE.80) GO TO 10
          END IF
C
                    CALL CCPERR(1,
     +' error - no space between codeword sym.. and first operator')
        END IF
      END IF
C
C
   20 I = I11 - 1
      NS = NS - 1
   30 CONTINUE
      NS = NS + 1
      REAL = 0.0
      RECIP = 0.0
      NOP = 1
C
C
      DO 50 J = 1,4
        DO 40 K = 1,4
          ROT(J,K,NS) = 0.0
   40   CONTINUE
   50 CONTINUE
C
C
      ROT(4,4,NS) = 1.0
   60 CONTINUE
C
      S = 1.0
C
C---- Set j=4 for translation vector
C
      J = 4
      T = 0.0
      IP = 0
      NP = 0
      ISL = 0
      IFOUND = 0
      ICOMST = 0
   70 CONTINUE
      I = I + 1
C
C
      IF (I.LE.IMAX) THEN
        ICH = ICOL(I:I)
        IF (ICH.EQ.IBLANK) THEN
          GO TO 70
        ELSE IF (ICH.NE.ICOMMA .AND. ICH.NE.ISTAR) THEN
          IFOUND = 1
          IF (ICH.EQ.IX .OR. ICH.EQ.JX .OR. ICH.EQ.IH .OR.
     +        ICH.EQ.JH) THEN
            IF (ICH.EQ.IX .OR. ICH.EQ.JX) REAL = REAL + 1.0
            IF (ICH.EQ.IH .OR. ICH.EQ.JH) RECIP = RECIP + 1.0
            J = 1
            IF (T.EQ.0.0) T = S
            GO TO 70
          ELSE IF (ICH.EQ.IY .OR. ICH.EQ.JY .OR. ICH.EQ.IK .OR.
     +             ICH.EQ.JK) THEN
            IF (ICH.EQ.IY .OR. ICH.EQ.JY) REAL = REAL + 1.0
            IF (ICH.EQ.IK .OR. ICH.EQ.JK) RECIP = RECIP + 1.0
            J = 2
            IF (T.EQ.0.0) T = S
            GO TO 70
          ELSE IF (ICH.EQ.IZ .OR. ICH.EQ.JZ .OR. ICH.EQ.IL .OR.
     +             ICH.EQ.JL) THEN
            IF (ICH.EQ.IZ .OR. ICH.EQ.JZ) REAL = REAL + 1.0
            IF (ICH.EQ.IL .OR. ICH.EQ.JL) RECIP = RECIP + 1.0
            J = 3
            IF (T.EQ.0.0) T = S
            GO TO 70
          ELSE IF (ICH.EQ.IPLUS) THEN
            S = 1.0
            IF (T.EQ.0.0 .AND. J.EQ.4) THEN
              GO TO 70
            ELSE
              GO TO 100
            END IF
          ELSE IF (ICH.EQ.IMINUS) THEN
            S = -1.0
            IF (T.EQ.0.0 .AND. J.EQ.4) THEN
              GO TO 70
            ELSE
              GO TO 100
            END IF
          ELSE IF (ICH.EQ.ISLASH) THEN
            ISL = 1
            GO TO 70
          ELSE IF (ICH.EQ.IPOINT) THEN
            IP = 1
            GO TO 70
          ELSE
            DO 80 K = 1,10
              IF (ICH.EQ.INUM(K)) GO TO 90
   80       CONTINUE
            WRITE (6,FMT=6000)
            WRITE (6,FMT=6002) ICH,ICOL
            IERR = 1
            GO TO 70
   90       A = NUM(K)
            IF (ISL.EQ.1) THEN
              T = T/A
            ELSE IF (IP.EQ.1) THEN
              NP = NP + 1
              T = S*A/10**NP + T
            ELSE
              T = 10.0*T + A*S
            END IF
            GO TO 70
          END IF
        END IF
      END IF
      IF (T.EQ.0.0 .AND. J.EQ.4) THEN
        GO TO 110
      ELSE
        ICOMST = 1
      END IF
  100 ROT(NOP,J,NS) = T
      J = 4
      T = 0.0
      IP = 0
      NP = 0
      ISL = 0
C
      IF (ICOMST.EQ.0) GO TO 70
C
C
      IF (IFOUND.EQ.0 .AND. I.LE.IMAX) THEN
        WRITE (6,FMT=6000)
        WRITE (6,FMT=6006) ICOL
      END IF
C
C
      IF (I.LE.IMAX) THEN
        NOP = NOP + 1
        IF (NOP.LE.3) THEN
          GO TO 60
        ELSE
          GO TO 30
        END IF
      ELSE
        GO TO 120
      END IF
  110 WRITE (6,FMT=6000)
      WRITE (6,FMT=6004) ICOL
      GO TO 140
  120 IF (NOP.NE.1 .OR. IFOUND.NE.0) THEN
        IF (NOP.EQ.3 .AND. IFOUND.EQ.1) THEN
          GO TO 130
        ELSE
          IERR = 1
          WRITE (6,FMT=6000)
          WRITE (6,FMT=6008) ICOL
        END IF
      END IF
      NS = NS - 1
  130 NSYM = NS
      IF (REAL.LT.3.0 .AND. RECIP.LT.3.0) IERR = 1
      IF (RECIP.GE.3.0) NSYM = -NSYM
      IF (IERR.NE.1) RETURN          
  140 WRITE (6,FMT='(A,I4,2F6.1,4(/,4F10.3))') ' NSYM REAL RECIP ROT',
     +  NSYM,REAL,RECIP, ((ROT(I,J,NS),J=1,4),I=1,4)
C
                CALL CCPERR(1,
     +   '**SYMMETRY OPERATOR ERROR**')
C
C---- Format statements
C
 6000 FORMAT (/' **SYMMETRY OPERATOR ERROR**')
 6002 FORMAT (' **INVALID CHARACTER...',A1,' **',/' ',A)
 6004 FORMAT (/' **NO OPERATOR**',/' ',A)
 6006 FORMAT (' **BLANK OPERATOR FIELD**',/' ',A)
 6008 FORMAT (' **LAST GENERAL POSITION IS INCOMPLETE**',/' ',A)
 6010 FORMAT (/' **PROGRAM TERMINATED**')
C
C
      END
C
C
C     ==========================
      SUBROUTINE SYMTRN(NSM,RSM)
C     ==========================
C
C
C---- This translates the Symmetry matrices into INT TAB
C     character strings for each symmetry operation.
C
C     It gives the real and reciprocal space operations.
C   eg     X,Y,Z    H,K,L
C   eg     -Y,X-Y, Z   -H-K, H, L  etc
C   That is more complicated than you might think!!
C
C
C---- Inverse symmetry needed to test systematic absences -
C     copy rsmm rsmtt this common block.
C
C      COMMON /SYSABS/ NSMT,RSMM(4,4,96),RSMTT(4,4,96)
C
C     .. Scalar Arguments ..
      INTEGER NSM
C     ..
C     .. Array Arguments ..
      REAL RSM(4,4,96)
C     ..
C     ..
C     .. Local Scalars ..
      INTEGER I,ICH,IST,ITR,J,K,N,NS,IPRINT
C     ..
C     .. Local Arrays ..
      REAL RSMT(4,4,96)
      CHARACTER AXISCR(3)*1,HKLCR(3)*1,NUMB(9)*1,SYMCHS(96)*80
C     ..
C     .. External Subroutines ..
      EXTERNAL INVSYM
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
C
      DATA IPRINT/0/
      DATA AXISCR/'X','Y','Z'/
      DATA HKLCR/'H','K','L'/
      DATA NUMB/'1','2','3','4','5','6','7','8','9'/
C     ..
C
C
      DO 50 NS = 1,NSM
C
C---- Clear symchs
C
        SYMCHS(NS) = ' '
        ICH = 1
C
C
        DO 20 J = 1,3
C
C---- Ist is flag for first character of operator
C
          IST = 0
C
C
          DO 10 I = 1,4
C
            IF (RSM(J,I,NS).NE.0) THEN
              IF (RSM(J,I,NS).GT.0 .AND. IST.GT.0) THEN
                SYMCHS(NS) (ICH:ICH) = '+'
                ICH = ICH + 1
              END IF
C
              IF (RSM(J,I,NS).LT.0) THEN
                SYMCHS(NS) (ICH:ICH) = '-'
                IST = 1
                ICH = ICH + 1
              END IF
C
              IF (I.NE.4) THEN
                SYMCHS(NS) (ICH:ICH) = AXISCR(I)
                IST = 1
                ICH = ICH + 1
              END IF
C
              IF (I.EQ.4 .AND. RSM(J,4,NS).NE.0) THEN
                ITR = NINT(1.0/RSM(J,4,NS))
                SYMCHS(NS) (ICH:ICH+1) = '1/'
                ICH = ICH + 2
                SYMCHS(NS) (ICH:ICH) = NUMB(ITR)
                ICH = ICH + 1
              END IF
            END IF
   10     CONTINUE
C
C---- ADD COMMA  space
C
          IF (J.NE.3) THEN
            SYMCHS(NS) (ICH:ICH+2) = ',  '
            ICH = ICH + 3
          END IF
   20   CONTINUE
C
C---- H K L   - get inverse symmetry operation
C
C            ********************************
        CALL INVSYM(RSM(1,1,NS),RSMT(1,1,NS))
C            ********************************
C
        ICH = 40
C
C
        DO 40 J = 1,3
          IST = 0
C
C
          DO 30 I = 1,3
            IF (RSMT(I,J,NS).NE.0) THEN
              IF (RSMT(I,J,NS).GT.0 .AND. IST.GT.0) THEN
                SYMCHS(NS) (ICH:ICH) = '+'
                ICH = ICH + 1
              END IF
              IF (RSMT(I,J,NS).LT.0) THEN
                SYMCHS(NS) (ICH:ICH) = '-'
                IST = 1
                ICH = ICH + 1
              END IF
              SYMCHS(NS) (ICH:ICH) = HKLCR(I)
              ICH = ICH + 1
              IST = 1
            END IF
   30     CONTINUE
C
C---- ADD COMMA space
C
          IF (J.NE.3) THEN
            SYMCHS(NS) (ICH:ICH+2) = ',  '
            ICH = ICH + 3
          END IF
   40   CONTINUE
C
C---- write a message
C
        WRITE (6,FMT='(A,I3,A,/,2X,A,4(/,4F6.2,10X,4F6.2))')
     +    ' SYMMETRY ',NS,'   REAL SPACE            RECIPROCAL SPACE',
     +    SYMCHS(NS), ((RSM(I,J,NS),J=1,4), (RSMT(I,K,NS),K=1,4),I=1,4)
   50 CONTINUE
C
C
C
      END
C
C
C     ===========================
      SUBROUTINE SYSAB(IN,ISYSAB)
C     ===========================
C
C
C---- Test reflections for Systematic absences
C     Only reflns with EPSI gt 1 need be considered
C     Systematic absences flagged with ISYSAB = 1
C
C---- Inverse symmetry needed to test systematic absences
C     - copy into this common block.
C      COMMON /SYSABS/ NSM,RSM(4,4,96),RSMT(4,4,96)
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER ISYSAB
C     ..
C     .. Array Arguments ..
      INTEGER IN(3)
C     ..
C     .. Scalars in Common ..
      INTEGER NSM
C     ..
C     .. Arrays in Common ..
      REAL RSM,RSMT
C     ..
C     .. Local Scalars ..
      REAL ERR,PHAS
      INTEGER IH,IK,IL,J
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,NINT
C     ..
C     .. Common blocks ..
      COMMON /SYSABS/NSM,RSM(4,4,96),RSMT(4,4,96)
C     ..
C     .. Save statement ..
      SAVE
C     ..
C
C---- Generate symm equivs
C
C---- test whether h' k' l' equals h k l
C
      ISYSAB = 0
      IF (NSM.NE.1) THEN
C
C
        DO 10 J = 2,NSM
          IH = IN(1)*RSMT(1,1,J) + IN(2)*RSMT(2,1,J) + IN(3)*RSMT(3,1,J)
          IF (IH.EQ.IN(1)) THEN
            IK = IN(1)*RSMT(1,2,J) + IN(2)*RSMT(2,2,J) +
     +           IN(3)*RSMT(3,2,J)
            IF (IK.EQ.IN(2)) THEN
              IL = IN(1)*RSMT(1,3,J) + IN(2)*RSMT(2,3,J) +
     +             IN(3)*RSMT(3,3,J)
              IF (IL.EQ.IN(3)) THEN
C
C---- Test whether this Symmetry equivalent has a different phase to
C     in(1) in(2) in(3)  - If so it is a systematic absence
C     .... Believe me EJD
C
                PHAS = IN(1)*RSM(1,4,J) + IN(2)*RSM(2,4,J) +
     +                 IN(3)*RSM(3,4,J)
                ERR = ABS(PHAS-NINT(PHAS))
                IF (ERR.GT.0.05) ISYSAB = 1
              END IF
            END IF
          END IF
   10   CONTINUE
C
C
CC        IF (ISYSAB.EQ.1) WRITE (6,FMT=6000) IN
      END IF
C
C---- Format statements
C
 6000 FORMAT (/'  REFLECTION',3I4,' SYSTEMATIC ABSENCE')
C
C
      END
C
C
C     ============================================
      SUBROUTINE XSPECIALS(NSM,RSM,XF,YF,ZF,NSPEC)
C     ============================================
C
C
C---- This subroutine finds what coordinates occupy special positions
C     ie have occupancies less than 1.0
C     from consideration of the Symmetry Operations.
C
C
C     .. Array Arguments ..
      REAL RSM(4,4,96)
C     ..
C     .. Scalar Arguments ..
      REAL XF,YF,ZF
      INTEGER NSM,NSPEC
C     ..
C     .. Local Scalars ..
      REAL DELX,DELXX,DELY,DELYY,DELZ,DELZZ,XXF,YYF,ZZF
      INTEGER J
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,MOD
C     ..
C
C
      NSPEC = 1
C
      IF (NSM.NE.1) THEN
C
C---- Generate symm equivs
C
C---- test whether xxf yyf zzf equals xf yf zf  - possibly with a unit
C---- cell translation...
C
        DO 10 J = 2,NSM
          XXF = RSM(1,1,J)*XF + RSM(1,2,J)*YF + RSM(1,3,J)*ZF +
     +          RSM(1,4,J)
C
C---- Is DELX = n - ie - have we generated the same coordinate?
C
          DELX = ABS(XXF-XF) + 0.01
          DELXX = MOD(DELX,1.0)
          IF (DELXX.LE.0.01) THEN
C
            YYF = RSM(2,1,J)*XF + RSM(2,2,J)*YF + RSM(2,3,J)*ZF +
     +            RSM(2,4,J)
C
C---- Check DELY
C
            DELY = ABS(YYF-YF) + 0.01
            DELYY = MOD(DELY,1.0)
            IF (DELYY.LE.0.01) THEN
C
              ZZF = RSM(3,1,J)*XF + RSM(3,2,J)*YF + RSM(3,3,J)*ZF +
     +              RSM(3,4,J)
C
C---- Check DELZ
C
              DELZ = ABS(ZZF-ZF) + 0.01
              DELZZ = MOD(DELZ,1.0)
              IF (DELZZ.LE.0.01) NSPEC = NSPEC + 1
C
            END IF
          END IF
   10   CONTINUE
C
C---- next symm opn
C
C
C---- next reflection class
C
      END IF
C
C
      END
C
C
C     ================================
      SUBROUTINE TURNIP(M1,M2,M3,MOST)
C     ================================
C
C---- TURNIP  -  to convert to unique form of indices with max value of
C            ( l*lpack + k )*lpack+ h
C           using symmetry matrices as read by subroutine symmat.
C
C   21-Apr-1989  
C     changed to exclude reflections with l .eq. 0 .and. h .lt. 0
C     ie for Triclinic space groups, take h .gt. 0 in the hk0 zone
C     Allow indices up to LHPACK by increasing packing number LPACK
C     Andrew Leslie & Phil Evans,  MRC LMB
C
C
C
C
C     .. Parameters ..
      INTEGER LPACK,LHPACK
      PARAMETER (LPACK=1024,LHPACK=LPACK/2)
      INTEGER MPACK
      PARAMETER (MPACK= ((LPACK*LHPACK+LHPACK)*LPACK+LHPACK))
C     ..
C     .. Scalar Arguments ..
      INTEGER M1,M2,M3,MOST
C     ..
C     .. Scalars in Common ..
      INTEGER NISYM
      CHARACTER PGNAME*10
C     ..
C     .. Arrays in Common ..
      INTEGER ISYM
C     ..
C     .. Local Scalars ..
      INTEGER H,IBEST,ITEST,K,L,M,MA,MB
C     ..
C     .. Local Arrays ..
      INTEGER ITABLE(3)
C     ..
C     .. Common blocks ..
      COMMON /SYMM/NISYM,ISYM(9,47)
      COMMON /SYMM1/PGNAME
C     ..
C
C
      MOST = 0
      IBEST = -MPACK
      IF (NISYM.NE.0) THEN
        H = M1
        K = M2
        L = M3
C
C---- Initialize with identity operation only if acceptable
C
        IF (L.NE.0 .OR. H.GE.0) THEN
          IBEST = (LPACK*M3+M2)*LPACK + M1
          MOST = 1
        END IF
C
C
        DO 20 M = 1,NISYM
          MB = 0
C
C
          DO 10 MA = 1,3
            ITABLE(MA) = ISYM(MB+1,M)*H + ISYM(MB+2,M)*K +
     +                   ISYM(MB+3,M)*L
            MB = MB + 3
   10     CONTINUE
C
C---- Test only acceptable indices
C
          IF (ITABLE(3).NE.0 .OR. ITABLE(1).GE.0) THEN
            ITEST = (ITABLE(3)*LPACK+ITABLE(2))*LPACK + ITABLE(1)
            IF (ITEST.GT.IBEST) THEN
              M1 = ITABLE(1)
              M2 = ITABLE(2)
              M3 = ITABLE(3)
              IBEST = ITEST
              MOST = M + 1
            END IF
          END IF
   20   CONTINUE
C
C
        IF (MOST.EQ.0) THEN
                    CALL CCPERR(1,
     +  'TURNIP - no symmetry operation found')
        ELSE
C
        END IF
      END IF
C
C
      END
C
C
C     ==============================
      SUBROUTINE TURN3(M1,M2,M3,MOST)
C     ===============================
C
C  Reduces indices to correct form for Trigonal FFT
C  Asymmetric unit:  h.ge.k.ge.0  for all l but when h=k then l.ge.0
C  (ie max value of h with k.ge.0)
C  Note that ISYM does not contain the identity matrix.
C  Modified form of code written by David Blow on the Nova at IC
C                       Peter Brick     20 Feb 1984
C
C
C
C     .. Parameters ..
      INTEGER LPACK,LHPACK
      PARAMETER (LPACK=1024,LHPACK=LPACK/2)
      INTEGER MPACK
      PARAMETER (MPACK= ((LPACK*LHPACK+LHPACK)*LPACK+LHPACK))
C     ..
C     .. Scalar Arguments ..
      INTEGER M1,M2,M3,MOST
C     ..
C     .. Scalars in Common ..
      INTEGER NISYM
      CHARACTER PGNAME*10
C     ..
C     .. Arrays in Common ..
      INTEGER ISYM
C     ..
C     .. Local Scalars ..
      INTEGER I,IBEST,ITEST,J,K,M,MA,MB
C     ..
C     .. Local Arrays ..
      INTEGER ITABLE(3)
C     ..
C     .. Common blocks ..
      COMMON /SYMM/NISYM,ISYM(9,47)
      COMMON /SYMM1/PGNAME
C     ..
C
C
      MOST = 0
      IBEST = -MPACK
      IF (NISYM.GT.0) THEN
        I = M1
        J = M2
        K = M3
C
        IF (J.GE.0) THEN
          MOST = 1
C
C---- For identity matrix
C
          IBEST = (M1*LPACK+M2)*LPACK + M3
        END IF
C
C
        DO 20 M = 1,NISYM
          MB = 0
C
C
          DO 10 MA = 1,3
            ITABLE(MA) = ISYM(MB+1,M)*I + ISYM(MB+2,M)*J +
     +                   ISYM(MB+3,M)*K
            MB = MB + 3
   10     CONTINUE
C
C---- Gives correct segment
C     namely M1.gt.M2 and M2.ge.0
C
          IF (ITABLE(2).GE.0) THEN
            ITEST = (ITABLE(1)*LPACK+ITABLE(2))*LPACK + ITABLE(3)
            IF (ITEST.GT.IBEST) THEN
              M1 = ITABLE(1)
              M2 = ITABLE(2)
              M3 = ITABLE(3)
              IBEST = ITEST
              MOST = M + 1
            END IF
          END IF
   20   CONTINUE
C
C
        IF (MOST.EQ.0) THEN
                    CALL CCPERR(1,
     +  ' ** ERROR IN TURN3 - MOST = 0 **')
        ELSE
C
        END IF
      END IF
C
C
      END
C     ============================
      LOGICAL FUNCTION HKLEQ(IH,KH)
C     =============================
C
C---- Returns true if indices ih = kh
C
C
C     .. Array Arguments ..
      INTEGER IH(3),KH(3)
C     ..
C
C
      HKLEQ = .FALSE.
C
C
      IF (IH(1).EQ.KH(1) .AND. IH(2).EQ.KH(2) .AND.
     +    IH(3).EQ.KH(3)) HKLEQ = .TRUE.
C
C
      END
C
C
C     ============================
      INTEGER FUNCTION LOOKUP(NSER)
C     ============================
C
C---- The function lookup returns the value nfind (which was input when
C     setting up the function in the subroutine setup) for the large
C     range variable nser
C
C     .. Scalar Arguments ..
      INTEGER NSER
C     ..
C     .. Arrays in Common ..
      INTEGER IT
C     ..
C     .. Local Scalars ..
      INTEGER KPRI,NDX,NSER4
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     ..
C     .. Common blocks ..
      COMMON /LOOK/IT(2,461)
C     ..
C     .. Data statements ..
      DATA KPRI/461/
C     ..
C
C
      NSER4 = NSER
C
C
   10 CONTINUE
C
C
      NDX = MOD(NSER4,KPRI) + 1
      IF (NSER.NE.IT(1,NDX)) THEN
        IF (IT(1,NDX).NE.0) THEN
          NSER4 = NSER4 + 3
          GO TO 10
        END IF
      END IF
C
C
      LOOKUP = IT(2,NDX)
C
C
      END
C
C
C     ==============================
      SUBROUTINE SETUP(NSER,NFIND)
C     ==============================
C
C---- This subroutine sets up a value for the function lookup(nser)
C     when lookup(nser) is later evaluated it will return nfind
C     this function will allow the efficient retrieval of an identifier
C     for a large range variable (such as a crystal number).  the values
C     of the function lookup(nser) are stored in the array it(2, kpri)
C     where kpri is the prime number used to generate the function
C     the array it  lives in the common look which is shared by setup
C     and the function lookup
C
C
C     IT(1, NDX) = NSER,  IT(2, NDX) = NFIND
C
C     .. Scalar Arguments ..
      INTEGER NFIND,NSER
C     ..
C     .. Arrays in Common ..
      INTEGER IT
C     ..
C     .. Local Scalars ..
      INTEGER I,KPRI,NDX,NSER4
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     ..
C     .. Common blocks ..
      COMMON /LOOK/IT(2,461)
C     ..
C     .. Data statements ..
      DATA KPRI/461/
C     ..
C
C
      NSER4 = NSER
   10 CONTINUE
      NDX = MOD(NSER4,KPRI) + 1
      IF (IT(1,NDX).NE.0) THEN
        NSER4 = NSER4 + 3
        GO TO 10
      END IF
C
      IT(1,NDX) = NSER
      IT(2,NDX) = NFIND
      RETURN
C
C     ==============
      ENTRY ZEROIT()
C     =============
C
      DO 20 I = 1,KPRI
        IT(1,I) = 0
        IT(2,I) = 0
   20 CONTINUE
C
C
C
      END
C
C
C     ================================================
      SUBROUTINE SETRSL(A,B,C,ALPHA,BETA,GAMMA)
C     ================================================
C
C---- Routine to calculate coefficients for (sin(theta)/lambda)**2 from
C     h,k,l for general axes
C
C     first calculated the components of input axes in an orthonormal
C     basis, then calculate components of reciprocal axes in same basis
C
C---- Input angles are in degrees
C
C     .. Scalar Arguments ..
      REAL A,ALPHA,B,BETA,C,GAMMA
C     ..
C     .. Scalars in Common ..
      REAL AXST,AYST,AZST,BYST,BZST,COEFHH,COEFHK,COEFHL,COEFKK,COEFKL,
     +     COEFLL,CZST
C     ..
C     .. Local Scalars ..
      REAL AR,AX,BR,BX,BY,CX,CY,CZ,DTORAD,GR,HALF,QMIN,STMAX,TMAX,TWO,
     +     XX,ZERO
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,COS,MAX,SIN,SQRT
C     ..
C     .. Common blocks ..
      COMMON /DSTAR/AXST,AYST,AZST,BYST,BZST,CZST
      COMMON /RECPLT/COEFHH,COEFHK,COEFHL,COEFKK,COEFKL,COEFLL
      SAVE /DSTAR/, /RECPLT/
C     ..
C     .. Data statements ..
      DATA QMIN,ZERO/5.0E-7,0.0/
      DATA DTORAD/0.01745329/
      DATA TWO/2.0/
      DATA HALF/0.5/
C     ..
C
C
C---- dtorad = 3.1415927/180.0
C
C     set up formulae for monoclinic or orthorhombic crystals
C     alpha, beta, gamma set to 90 where appropriate
C
      IF (ALPHA.EQ.90.0) THEN
        AR = 1.57079635
      ELSE
        AR = ALPHA*DTORAD
      END IF
      IF (BETA.EQ.90.0) THEN
        BR = 1.57079635
      ELSE
        BR = BETA*DTORAD
      END IF
      IF (GAMMA.EQ.90.0) THEN
        GR = 1.57079635
      ELSE
        GR = GAMMA*DTORAD
      END IF
C
C---- Put a-axis along x
C
      AX = A
C
C---- put b-axis in x-y plane
C
      BX = COS(GR)*B
      BY = SIN(GR)*B
C
C---- Be sure by is positive
C
      BY = ABS(BY)
C
C---- C falls where it is
C
      CX = COS(BR)*C
      CY = (B*C*COS(AR)-BX*CX)/BY
C
C---- CZ determined by length of c
C
      XX = C*C - CX*CX - CY*CY
      CZ = SQRT(XX)
      TMAX = MAX(AX,BY)
      TMAX = MAX(TMAX,CZ)
      IF (ABS(BX/TMAX).LT.QMIN) BX = ZERO
      IF (ABS(CX/TMAX).LT.QMIN) CX = ZERO
      IF (ABS(CY/TMAX).LT.QMIN) CY = ZERO
C
C
c      WRITE (6,FMT=6000) AX,BX,BY,CX,CY,CZ
C
C---- Now for reciprocal vectors
C
      AXST = HALF/AX
      AYST = -AXST*BX/BY
      AZST = - (AXST*CX+AYST*CY)/CZ
      BYST = HALF/BY
      BZST = -CY*BYST/CZ
      CZST = HALF/CZ
      STMAX = MAX(AXST,BYST)
      STMAX = MAX(STMAX,CZST)
      IF (ABS(AYST/STMAX).LT.QMIN) AYST = ZERO
      IF (ABS(AZST/STMAX).LT.QMIN) AZST = ZERO
      IF (ABS(BZST/STMAX).LT.QMIN) BZST = ZERO
c      WRITE (6,FMT=6002) AXST,AYST,BYST,AZST,BZST,CZST
C
C---- The other three components of reciprocal vectors are zero
C     coefficient of h*h
C
      COEFHH = AXST*AXST + AYST*AYST + AZST*AZST
C
C---- Coefficient of h*k
C
      COEFHK = (AYST*BYST+AZST*BZST)*TWO
C
C---- coefficient of h*l
C
      COEFHL = AZST*CZST*TWO
C
C---- coefficient of k*k
C
      COEFKK = BYST*BYST + BZST*BZST
C
C---- coef of k*l
C
      COEFKL = BZST*CZST*TWO
C
C---- coef of l*l
C
      COEFLL = CZST*CZST
C
C---- Format statements
C
 6000 FORMAT (' Direct Matrix     :',T25,1P,E15.6,2 (12X,'0.0'),/,
     +                               T25,2E15.6,12X,'0.0',/,
     +                               T25,3E15.6,/)
 6002 FORMAT (' Reciprocal Matrix :',T25,1P,E15.6,2 (12X,'0.0'),/,
     +                               T25,2E15.6,12X,'0.0',/,
     +                               T25,3E15.6,/)
C
C
      END
C
C
C     ===========================
      REAL FUNCTION STHLSQ(IH,IK,IL)
C     ============================
C
C---- Calculate (sin(theta)/lambda)**2 from h,k,l; coef's set by call to
C        SETRSL : good for any kind of axes
C
C
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER IH,IK,IL
C     ..
C     .. Scalars in Common ..
      REAL COEFHH,COEFHK,COEFHL,COEFKK,COEFKL,COEFLL
C     ..
C     .. Common blocks ..
      COMMON /RECPLT/COEFHH,COEFHK,COEFHL,COEFKK,COEFKL,COEFLL
      SAVE /RECPLT/
C     ..
C
C
      STHLSQ = IH*IH*COEFHH + IH*IK*COEFHK + IH*IL*COEFHL +
     +         IK*IK*COEFKK + IK*IL*COEFKL + IL*IL*COEFLL
C
C
      END
C
C
C     ==================================
      LOGICAL FUNCTION CENTRC(KHKL,ICENT)
C     ==================================
C
C
C---- returns value true if reflection khkl is centric, false otherwise.
C     general for all point groups - but only for the unique set of
C     indices which conforms to the criterion of maximising the value
C     of        (khkl(3)*256 + khkl(2))*256 + khkl(1)
C
C    as produced by e.g. subroutine turnip in protin and ulysses.
C
C---- in this case the required tests are controlled by 7 flags in
C     icent fo
C
C  0KL  H0L  HK0  HKK  HKH  HHL  H,-2H,L
C     (the last is needed in pg312)
C
C
C
C     .. Array Arguments ..
      INTEGER ICENT(7),KHKL(3)
C     ..
C     .. Local Scalars ..
      INTEGER JJ
C     ..
C
C
      CENTRC = .FALSE.
      IF (ICENT(1).NE.0) THEN
        IF (KHKL(1).EQ.0) GO TO 10
      END IF
      IF (ICENT(2).NE.0) THEN
        IF (KHKL(2).EQ.0) GO TO 10
      END IF
      IF (ICENT(3).NE.0) THEN
        IF (KHKL(3).EQ.0) GO TO 10
      END IF
      IF (ICENT(4).NE.0) THEN
        IF (KHKL(2).EQ.KHKL(3)) GO TO 10
      END IF
      IF (ICENT(5).NE.0) THEN
        IF (KHKL(3).EQ.KHKL(1)) GO TO 10
      END IF
      IF (ICENT(6).NE.0) THEN
        IF (KHKL(1).EQ.KHKL(2)) GO TO 10
      END IF
      IF (ICENT(7).EQ.0) THEN
        GO TO 20
      ELSE
        JJ = -KHKL(1)*2
        IF (KHKL(2).NE.JJ) GO TO 20
      END IF
   10 CENTRC = .TRUE.
   20 RETURN
C
C
      END
C
C
C     =================
      SUBROUTINE SYMMAT
C     =================
C
C---- to read and print the symmetry matrices for point group pg
C     which are stored with successive rows being the 9 elements
C     of the column for the jth matrix in isym.
C
C
C
C
C     .. Scalars in Common ..
      INTEGER NISYM
      CHARACTER PGNAME*10
C     ..
C     .. Arrays in Common ..
      INTEGER ISYM
C     ..
C     .. Local Scalars ..
      INTEGER I,IFAIL,J,JX,K,KT,LDUM,MT,MU,NMAT,NU
      CHARACTER PGIN*10
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPOPN
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MIN
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. Common blocks ..
      COMMON /SYMM/NISYM,ISYM(9,47)
      COMMON /SYMM1/PGNAME
C     ..
C
C
      IFAIL = 1
      CALL CCPOPN(4,'PGDATA',5,1,LDUM,IFAIL)
      IF (IFAIL.NE.-1) THEN
   10   CONTINUE
C
C---- Skip file to correct point group pgname
C
        READ (4,FMT=6000,END=20) PGIN
        LNSTR1 = LENSTR(PGIN)
        LNSTR2 = LENSTR(PGNAME(1:10))
        IF (PGIN(1:LNSTR1).NE.PGNAME(1:LNSTR2)) THEN
          GO TO 10
        ELSE
          GO TO 30
        END IF
C
C---- point-group not found
C
   20   WRITE (6,FMT=6002) PGNAME
        GO TO 110
C
   30   CONTINUE
C
C
        DO 50 I = 1,9
          DO 40 K = 1,47
            ISYM(I,K) = 0
   40     CONTINUE
   50   CONTINUE
C
C
        READ (4,FMT=6010) NMAT
        NISYM = NMAT + NMAT + 1
C
C
        DO 60 K = 1,9,4
          ISYM(K,1) = -1
   60   CONTINUE
C
C
        IF (NISYM.GT.1) THEN
C
C
          DO 80 I = 2,NISYM,2
            READ (4,FMT=6010) (ISYM(J,I),J=1,9)
C
C
            DO 70 J = 1,9
              ISYM(J,I+1) = -ISYM(J,I)
   70       CONTINUE
   80     CONTINUE
C
C
        END IF
        J = NISYM + 1
        WRITE (6,FMT=6012) PGNAME,NMAT,NISYM,J
        WRITE (6,FMT=6004)
C
C
        DO 100 J = 1,NISYM,8
C
C---- print out matrices eight at a time
C
          JX = MIN(NISYM,J+7)
          MT = 1
          KT = 3
C
C---- all the matrices are 3-by-3, print 3 lines
C
          DO 90 I = 1,3
            WRITE (6,FMT=6008) ((ISYM(NU,MU),NU=MT,KT),MU=J,JX)
            MT = MT + 3
            KT = KT + 3
   90     CONTINUE
C
C---- skip two lines on printer
C
          WRITE (6,FMT=6006)
  100   CONTINUE
C
C
        REWIND 4
        CLOSE (UNIT=4)
C
        RETURN
      END IF
  110           CALL CCPERR(1,' STOP in SYMLIB.for 110')
C
C---- Format statements
C
 6000 FORMAT (2X,A)
 6002 FORMAT (//'  POINT-GROUP  ',A,'  NOT FOUND IN LIBRARY !!!!',//)
 6004 FORMAT (//10X,'** SYMMETRY MATRICES USED **')
 6006 FORMAT (//)
 6008 FORMAT (14X,8 (4X,3I3))
 6010 FORMAT (9I2)
 6012 FORMAT (////11X,'Number of Symmetry Matrices read for POINT GROU',
     +       'P ',A,' .....',I5,/11X,'Total Number of Symmetry Matric',
     +       'es..........................',I5,/11X,'Identity Matrix i',
     +       's assumed to be number 1 - Those printed are numbers 2 to'
     +       ,I3)
C
C
      END
C
C
C     ================================
      SUBROUTINE DECSYM(JHKL,HKL,KSYM)
C     ================================
C
C---- subroutine decsym decodes symmetry operations according to ksym
C     which was attached to every reflection by subroutine turnip
C     as in protin and ulysses.
C
C---- uses matrices read into core by symmat, and inverted by syminv
C
C---- Symmetry matrix is stored in core (array isym) as 
C     a 9-element column
C     i.e.the first row of the symmetry matrix m occupies elements 1-3
C     of column m of array symmat,the second occupies elements 4-7 etc.
C     to retrieve the original index from lcf file ,the inverse
C     of symmetry matrix isym is needed .
C
C
C
C
C
C---- if ksym = 1 , use identity
C
C     .. Scalar Arguments ..
      INTEGER KSYM
C     ..
C     .. Array Arguments ..
      REAL HKL(3)
      INTEGER JHKL(3)
C     ..
C     .. Scalars in Common ..
      INTEGER NISYM
      CHARACTER PGNAME*10
C     ..
C     .. Arrays in Common ..
      INTEGER ISYM
C     ..
C     .. Local Scalars ..
      INTEGER I,J,KS
C     ..
C     .. Common blocks ..
      COMMON /SYMM/NISYM,ISYM(9,47)
      COMMON /SYMM1/PGNAME
C     ..
C
C
      IF (KSYM.EQ.1) THEN
C
C---- identity
C
        DO 10 I = 1,3
          HKL(I) = JHKL(I)
   10   CONTINUE
C
C
      ELSE
        KS = KSYM - 1
C
C
        DO 20 I = 1,3
          J = 3*I - 2
          HKL(I) = ISYM(J+1,KS)*JHKL(2) + ISYM(J,KS)*JHKL(1) +
     +             ISYM(J+2,KS)*JHKL(3)
   20   CONTINUE
C
C
      END IF
C
C
      END
C
C
C     =================
      SUBROUTINE SYMINV
C     =================
C
C---- invert point group symmetry matrices in common symm
C
C---- External routine used:
C     =====================
C
C  IMINV(IA,IB,JDET)  invert 3 x 3 matrix ib to ia
C
C        IA = (IB)**-1
C        JDET  IS DETERMINANT
C
C
C
C     .. Scalars in Common ..
      INTEGER NISYM
      CHARACTER PGNAME*10
C     ..
C     .. Arrays in Common ..
      INTEGER ISYM
C     ..
C     .. Local Scalars ..
      INTEGER J,JDET,N
C     ..
C     .. Local Arrays ..
      INTEGER MAT(9)
C     ..
C     .. External Subroutines ..
      EXTERNAL IMINV
C     ..
C     .. Common blocks ..
      COMMON /SYMM/NISYM,ISYM(9,47)
      COMMON /SYMM1/PGNAME
C     ..
C
C
      DO 20 N = 1,NISYM
C
C---- invert into mat
C
        CALL IMINV(MAT,ISYM(1,N),JDET)
        IF (JDET.EQ.0) THEN
          GO TO 30
        ELSE
C
C---- copy back
C
          DO 10 J = 1,9
            ISYM(J,N) = MAT(J)
   10     CONTINUE
C
C
        END IF
   20 CONTINUE
C
C
C
      RETURN
C
C
   30 WRITE (6,FMT=6000) N
                CALL CCPERR(1,' STOP in SYMLIB.for 6000')
C
C---- Format statements
C
 6000 FORMAT (//'  !!!!!! SYMMETRY MATRIX',I4,' IS SINGULAR !!!!!!',//)
C
C
      END
C
C
C     ============================
      SUBROUTINE IMINV(IA,IB,JDET)
C     ============================
C
C
C     .. Scalar Arguments ..
      INTEGER JDET
C     ..
C     .. Array Arguments ..
      INTEGER IA(3,3),IB(3,3)
C     ..
C     .. Local Scalars ..
      INTEGER I,I1,I2,J,J1,J2
C     ..
C
C
      DO 20 J = 1,3
        DO 10 I = 1,3
          I1 = 2 - I/2
          I2 = 3 - I/3
          J1 = 2 - J/2
          J2 = 3 - J/3
          IA(I,J) = (IB(J1,I1)*IB(J2,I2)-IB(J2,I1)*IB(J1,I2))*
     +              (-1)** (I+J)
   10   CONTINUE
   20 CONTINUE
C
C
      JDET = IB(1,1)*IA(1,1) + IB(1,2)*IA(2,1) + IB(1,3)*IA(3,1)
C
C
      DO 40 I = 1,3
        DO 30 J = 1,3
          IA(I,J) = IA(I,J)/JDET
   30   CONTINUE
   40 CONTINUE
C
C
      END
C
C
C
      INTEGER FUNCTION KROT(NS)
C     =========================
C
C---- Apply ns'th symmetry operation to jp to get lp,
C     check if lies in asymmetric unit given by nau
C
C      Returns KROT=0  correct operation
C                  =1  if not
C
C
C
C     .. Scalar Arguments ..
      INTEGER NS
C     ..
C     .. Scalars in Common ..
      REAL RHMAX,RHMEAN,RHMIN
      INTEGER LUNIN,LUNOUT,INMAP,OUTMAP,SYMFIL
      INTEGER IBCD,IX1,IX2,IY1,IY2,IZ1,IZ2,JSEC,JX1,JX2,JZ1,JZ2,LSEC,
     +        LSPGRP,NSEC,NSYM
C     ..
C     .. Arrays in Common ..
      REAL CELL
      INTEGER IUVW,JP,JS,JT,KP,LP,NAU,NXYZ,NXYZ10
C     ..
C     .. Local Scalars ..
      INTEGER I,J,L
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     ..
C     .. Common blocks ..
      COMMON /INPUT/NXYZ(3),IX1,IX2,IY1,IY2,IZ1,IZ2,JSEC,JX1,JX2,JZ1,
     +       JZ2,IUVW(3),NSEC,KP(3),CELL(6),LSPGRP,RHMIN,RHMAX,RHMEAN,
     +       IBCD
      COMMON /POINTS/JP(3),LP(3),LSEC,NXYZ10(3)
      COMMON /SYM/NSYM,NAU(3),JS(3,3,96),JT(3,96)
      COMMON /INOUT/ LUNIN,LUNOUT,INMAP,OUTMAP,SYMFIL
C     ..
C
C---- Error trap
C
      IF (NS.LE.0) THEN
        WRITE (LUNOUT,6000) NS
                  CALL CCPERR(1,' STOP in SYMLIB.for 6600')
      END IF
      KROT = 1
      DO 20 I = 1,3
        L = JT(I,NS)
        DO 10 J = 1,3
          L = JP(J)*JS(I,J,NS) + L
   10   CONTINUE
C
C---- Get into cell
C
        L = NXYZ10(I) + L
        L = MOD(L,NXYZ(I))
        IF (I.EQ.2) THEN
C
C---- For W axis, check same section
C
          IF (L.NE.LSEC) GO TO 30
C
C---- U or V axes
C
        ELSE IF (L.GT.NAU(I)) THEN
          GO TO 30
        END IF
C
C---- OK
C
        LP(I) = L
   20 CONTINUE
C
C---- Successful
C
      KROT = 0
   30 RETURN
C
C---- Format statements
C
6000  FORMAT('   NS in Function KROT is le 0 with a value of ',I6)
C
C
      END
C
      SUBROUTINE PSTOPH (PSIX,PSIY,PSIZ,PHIX,PHIY,PHIZ,AVPHI)
C     =======================================================
C
C***   *****  PSTOPH  *****
C***   Convert PSIX,PSIY,PSIZ (= epsx,epsy,epsz) to PHIX,PHIY,PHIZ ,
C***    using AVPHI
C      All angles in radians
C
      CP1 = COS(PSIX)
      SP1 = SIN(PSIX)
      CP2 = COS(PSIY)
      SP2 = SIN(PSIY)
      CP3 = COS(PSIZ)
      SP3 = SIN(PSIZ)
      CP = COS(AVPHI)
      SP = SIN(AVPHI)
 
C***CALCULATE PHIX
      SPX = SP2*SP + CP2*SP1*CP
      CPX = CP2*CP1
      PHIX = ATAN2(SPX,CPX)
 
C***CALCULATE PHIY
      SPY =-CP2*SP1*SP + SP2*CP
      CPY = CP2*CP1/COS(PHIX)
      PHIY = ATAN2(SPY,CPY)
 
C***CALCULATE PHIZ
      P11 = CP3*CP2*CP + SP*(CP3*SP2*SP1 - SP3*CP1)
      P21 = SP3*CP2*CP+SP*(SP3*SP2*SP1+CP3*CP1)
      SPZ =-SP*P11 + CP*P21
      CPZ = CP*P11 + SP*P21
      PHIZ = ATAN2(SPZ,CPZ)
 
      RETURN
C**   DEBUG SUBCHK
      END
C
C
C     ==============================
      REAL FUNCTION STS3R4(IH,IK,IL)
C     ==============================
C
C
C
C---- calculate (sin(theta)/lambda)**2 from h,k,l; coef's set by call to
C        setrsl : good for any kind of axes
C
C
C     .. Scalar Arguments ..
      REAL IH,IK,IL
C     ..
C     .. Scalars in Common ..
      REAL COEFHH,COEFHK,COEFHL,COEFKK,COEFKL,COEFLL
C     ..
C     .. Common blocks ..
      COMMON /RECPLT/COEFHH,COEFHK,COEFHL,COEFKK,COEFKL,COEFLL
C     ..
C     .. Save statement ..
      SAVE
C     ..
C
C
      STS3R4 = IH*IH*COEFHH + IH*IK*COEFHK + IH*IL*COEFHL +
     +         IK*IK*COEFKK + IK*IL*COEFKL + IL*IL*COEFLL
C
C
      END
C----------------------------------------------------------------
C
c  this file contains bits for symlib
c
c       PGDEFN  new version with extra argument, print flag
c       PGNLAU  new version with extra argument, Laue group name returned
c                 both PGDEFN & PGNLAU are called by ASUSET
c
c  New routines
c       RDSYMM  parse SYMMETRY command, return symmetry operations
c       ASUSET  set up symmetry for ASUPUT & ASUGET, print it
c       ASUPUT  put reflection into asymmetric unit defined in ASUSET
c       ASUGET  recover original indices, ie reverse of ASUPUT
c	ASUPHP  change phase for symmetry related reflection
c  Internal routines:-
c       PRTRSM  print reciprocal space symmetry, called by ASUSET
c       INASU   (function) test if reflection is in asymmetric unit
c               called by ASUPUT                        
C
C
C
      SUBROUTINE PGDEFN(NAMPG,NSYMP,NSYM,RSMT,LPRINT)
C     ==============================================
C
C  Things for MDF files... Draft for Bauke...
C
C     ICENTR   : axis of centering
C                0  =  no centering               (P spacegroups)
C                1  =  centering around a-axis    (A spacegroups)
C                2  =  centering around b-axis    (B spacegroups)
C                3  =  centering around c-axis    (C spacegroups)
C                4  =  centering on all faces     (F spacegroups)
C                5  =  body centering             (I spacegroups)
C                6  =  rhombohedral centering     (R spacegroups with
C                                                    hexagonal axes)
C                      (NOTE: R-spacegroups with rhombohedral axes
C                             have ICENTR = 0!)
C
C     ISCREW(3): type of screw axis for A, B  and C
C                so ISCREW(I) must be 0 (no screw), 2, 3, 4, or 6
C EJD:     I think I may have this wrong for non primitive spacegroups.
C          The routine only looks at the primitive sym ops ....
C
C     KLASS    : a crystal class number of the set below
C     KLASS  crystal system  laue class comments  spacegroup number
C  EJD
C          Our nlaue code number ...
C          We do not allow KLASS 7 10 15 16
C
C                                                 (INT. TABLES)
C       1    TRICLINIC       1_BAR (PG1)                        1
C       2    MONOCLINIC   I  2/M (PG4) B-UNIQUE           3 -   5
C       3    ORTHORHOMBIC    MMM (PG222)                 16 -  24
C       4    TETRAGONAL   I  4/M (PG4)                   75 -  80
C       5    TETRAGONAL  II  4/MMM (PG422)               89 -  98
C       6    TRIGONAL     I  3_BAR (PG3)HEXAGONAL AXES   143 - 146
C       7    TRIGONAL    II  3_BAR (??) RHOMBOHEDRAL AXES      146
C       8    TRIGONAL   III  3_BAR1M (PG312)           149,151,153
C       9    TRIGONAL    IV  3_BARM1 (PG321)HEXAGONAL AXES  
C                                                   150,152,154,155
C      10    TRIGONAL     V  3_BARM1 (??)RHOMBOHEDRAL AXES      155
C      11    HEXAGONAL    I  6/M  (PG6)                   168 - 173
C      12    HEXAGONAL   II  6/MMM (PG622)                177 - 182
C      13    CUBIC        I  M3_BAR (PG23)                195 - 199
C      14    CUBIC       II  M3_BARM (PG432)              207 - 214
C      15    MONOCLINIC  II  2/M (??)  A UNIQUE           3 -   5
C      16    MONOCLINIC III  2/M (??)  C UNIQUE           3 -   5
C
C     In this table only the enantiomorphic spacegroups are
C     included. For the other spacgroups (which contain (glide)
C     mirrors or an inversion center) this routine can still be
C     used, but then isym has no meaning anymore.
C
C
C      COMMON /MDFPAR/MAXR,MAXB,CELL(6),ISLOW,INTER,IFAST,KLASS,ICENTR,
C     +       ISCREW(3),IVERSN
C
C
C LCF CADLCF nlaue number
C   3 pg1     1bar      hkl:l>=0  hk0:h>=0  0k0:k>=0   1,2
C   4 pg2    2/m        hkl:k>=0, l>=0  hk0:h>=0       3/b,4/b....
C   6 pg222  mmm        hkl:h>=0, k>=0, l>=0            16 ...
C                                                k>0 if h>0
C   7 pg4    4/m        hkl:h>=0, l>=0 with k>=0 if  h=0  and
C   8 pg422 4/mmm       hkl:h>=0, k>=0, l>=0            89..
C   9 pg3     3bar      hkl:h>=0, k>0  00l:l>0         143..
C  10 pg312  3/m        hkl:h>=0, k>=0 with k<=h for all l.
C                           if k = 0  l>=0
C           Space group numbers :   149-151-153
C  11 pg321  3/m        hkl:h>=0, k>=0 with k<=h for all l.
C                           if h = k  l>=0
C           Space group numbers :   150-152-154
C  12 pg6    6/m        hkl:h>=0, k>=0, l>=0 with k>=0 if  h=0
C                       and k> 0 if h>0
C  13 pg622 6/mmm       hkl:h>=0, k>=0, l>=0 with h>=k 177..
C  14 pg23   m3         hkl:h>=0, k>=0, l>=0 with l>=h,  k>=h
C  15 pg432  m3m        hkl:h>=0, k>=0, l>=0  with  k>=l  and
C
C
C
C
C
C---- Find unique set of rsmt - these are the reciprocal space symmetry
C                               operators and there will be duplicate
C                               rsmt(3,3) for non primitivs spacegroups.
C
C   Copy  symmetry into RJUNK(i,j,n) to preserve it.
C   Set rsmt(4,4,??) = 0.0 as a flag for a duplicate while checking.
C
C     .. Scalar Arguments ..
      INTEGER NSYM,NSYMP
      CHARACTER NAMPG* (*)
      LOGICAL LPRINT
C     ..
C     .. Array Arguments ..
      REAL RSMT(4,4,96)
C     ..
C     .. Scalars in Common ..
      INTEGER ICENTR,IFAST,INTER,ISLOW,IVERSN,KLASS,MAXB,MAXR,NCENT,
     +        NEZONE
      CHARACTER STROUT*140
C     ..
C     .. Arrays in Common ..
      REAL CELL,CPROJ,EPZONE
      INTEGER ISCREW
C     ..
C     .. Local Scalars ..
      REAL DET,DX,DXY,DXYZ,DXZ,DY,DYZ,DZ
      INTEGER I,IH,IHR,IK,IKR,IL,ILR,IRAXIS,IRMIN,IROT,ISCR,ISM1,ISM2,
     +        ISS,ISYM,J,JMIN,JROT,JSM,JUNIQU,N,NREP,NREPET
C     ..
C     .. Local Arrays ..
      REAL RJUNK(4,4,96)
      INTEGER IN(3),JROTS(96),NORIG(96),NREPP(96),NROT(96),NROTS(96)
C     ..
C     .. External Subroutines ..
      EXTERNAL DETERM
      EXTERNAL PUTLIN
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,NINT
C     ..
C     .. Common blocks ..
      COMMON /CP/CPROJ(3,20),NCENT
      COMMON /EPS/EPZONE(4,20),NEZONE
      COMMON /MDFPAR/MAXR,MAXB,CELL(6),ISLOW,INTER,IFAST,KLASS,ICENTR,
     +       ISCREW(3),IVERSN
C     ..
C     .. Save statement ..
      SAVE
C     ..
C
C
      IF (LPRINT) THEN
        WRITE (STROUT,FMT=6020) NSYM
 6020   FORMAT(' In PGDEFN: Nsym = ',I6)
        CALL PUTLIN(STROUT,'CURWIN')
      ENDIF
C
C
      DO 30 N = 1,NSYM
C---- Clear all repeat counts
        NREPP(N) = 0
        DO 20 J = 1,4
          DO 10 I = 1,4
            RJUNK(I,J,N) = RSMT(I,J,N)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
C
C
      NSYMP = 0
C
C---- Which sym ops belong to primitive set? Check 1 - nsym-1 first.
C
      DO 70 ISM1 = 1,NSYM - 1
C
C---- We have already dealt with this Symm op...
C
        IF (RSMT(4,4,ISM1).NE.0.0) THEN
          NREP = 0
          NSYMP = NSYMP + 1
          NORIG(ISM1) = NSYMP
C
C---- Search all remaining ones for duplicates...
C
          DO 60 ISM2 = ISM1 + 1,NSYM
            DO 50 I = 1,3
              DO 40 J = 1,3
                IF (RSMT(I,J,ISM2).NE.RSMT(I,J,ISM1)) GO TO 60
   40         CONTINUE
   50       CONTINUE
C
C---- This is a duplicate... - count how often there is repetition and
C     record which sym op it is a duplicate of.
C     Modify rsmt(4,4,ism2) to act as a flag.
C
            NREP = NREP + 1
            NREPP(ISM2) = NREP
            NORIG(ISM2) = NORIG(ISM1)
            RSMT(4,4,ISM2) = 0.0
            IF (LPRINT) THEN
              WRITE (STROUT,FMT='(1X,A,I3,A,I3)')
     +     '  Reciprocal space symmetry operator ',ISM2,' same as ',
     +        ISM1
              CALL PUTLIN(STROUT,'CURWIN')
            ENDIF
   60     CONTINUE
        END IF
   70 CONTINUE
C
C---- Check last symmetry operator now.
C
      IF (RSMT(4,4,NSYM).NE.0.0) THEN
        NREP = 0
        NSYMP = NSYMP + 1
        NORIG(NSYM) = NSYMP
      END IF
C
C---- Now rewrite symmetry with all primitives
C     first and define centring.
C
C   If nsym=nsymp   you have a primitive spacegroup.
C
      NREPET = NSYM/NSYMP
      ICENTR = 0
      IF (NREPET.NE.1) THEN
C
C---- If nrepet = 4 you must have FACE centring. (icentr = 4)
C
        IF (NREPET.EQ.4) ICENTR = 4
C
C---- If nrepet = 3 you must have Rhombehedral centring. (icentr = 6)
C
        IF (NREPET.EQ.3) ICENTR = 6
C
C---- If nrepet.eq 2 you may have A B C or I centring.
C
C     Check translation components for appropriate
C     symmetry axes after reordering.
C
        ISYM = 0
C
C
        DO 100 N = 1,NSYM
          ISYM = NREPP(N)*NSYMP + NORIG(N)
C
          DO 90 J = 1,4
            DO 80 I = 1,4
              RSMT(I,J,ISYM) = RJUNK(I,J,N)
   80       CONTINUE
   90     CONTINUE
C
C---- Reset rsmt(4,4,...) = 1.0
C
          RSMT(4,4,N) = 1.0
  100   CONTINUE
      END IF
C
C---- More centring.
C     If nrepet.eq 2 you may have A B C or I centring.
C
      IF (NREPET.EQ.2) THEN
        DX = ABS(RSMT(1,4,1)-RSMT(1,4,NSYMP+1))
        DY = ABS(RSMT(2,4,1)-RSMT(2,4,NSYMP+1))
        DZ = ABS(RSMT(3,4,1)-RSMT(3,4,NSYMP+1))
C
C---- If dx dy and dz are all = 0.5 you must
C     have body centring. (icentr = 5)
C
        DXYZ = DX*DY*DZ
        IF (DXYZ.GT.0.1) THEN
          ICENTR = 5
        ELSE
          DXY = DX*DY
          DXZ = DX*DZ
          DYZ = DY*DZ
C
C---- If dy and dz  = 0.5 and dx=0 you must
C     have A  centring. (icentr = 1)
C
          IF (DYZ.GT.0.2) ICENTR = 1
C
C---- If dx and dz  = 0.5 and dy=0 you must
C     have B  centring. (icentr = 2)
C
          IF (DXZ.GT.0.2) ICENTR = 2
C
C---- If dx and dy  = 0.5 and dz=0 you must
C     have C  centring. (icentr = 3)
C
          IF (DXY.GT.0.2) ICENTR = 3
        END IF
      END IF
C
C---- Find whether 2fold 3 fold etc and define ISCREW(...)
C     use general reflection  - taken as (1,4,8)
C
      IN(1) = 1
      IN(2) = 4
      IN(3) = 8
C
C---- Generate primitive symm equivs and look at rotations.
C
C---- test whether h' k' l' equals  h  k  l
C
C----- Choose rotation axis - either a(3) b(2) c(1)  or
C                             lines 1 1 0(4)  or  1 1 1(5)
C
C      Define ISCREW(i) = 0,2,3,4,6
C
      ISCREW(1) = 0
      ISCREW(2) = 0
      ISCREW(3) = 0
      JSM = 0
C
C
      DO 130 J = 1,NSYMP
C
C---- Is it a rotation at all?
C
C            ***********************
        CALL DETERM(DET,RSMT(1,1,J))
C            ***********************
C
        IF (DET.EQ.1.0) THEN
C
          NROT(J) = 0
          IRAXIS = 0
C
C---- Unique rotation axes must be
C     a  b  c  or the lines 1 1 0   or 1 1 1.
C
C----   a
C
          IF (RSMT(1,1,J).EQ.1.0) IRAXIS = 3
C
C---- b
C
          IF (RSMT(2,2,J).EQ.1.0) IRAXIS = 2
C
C---- c
C
          IF (RSMT(3,3,J).EQ.1.0) IRAXIS = 1
C
C---- 1 1 0
C
          IF (RSMT(2,1,J).EQ.1.0 .AND. RSMT(1,2,J).EQ.1.0 .AND.
     +        RSMT(3,3,J).EQ.-1.0) IRAXIS = 4
C
C---- 1 1 1
C
          IF (RSMT(2,1,J).EQ.1.0 .AND. RSMT(3,2,J).EQ.1.0 .AND.
     +        RSMT(1,3,J).EQ.1.0) IRAXIS = 5
C
C---- 1 1 1
C
          IF (RSMT(3,1,J).EQ.1.0 .AND. RSMT(1,2,J).EQ.1.0 .AND.
     +        RSMT(2,3,J).EQ.1.0) IRAXIS = 5
C
          IHR = IN(1)
          IKR = IN(2)
          ILR = IN(3)
C
C---- Ignore anything else eg -1 1 0  - they are consequences of others
C
          IF (IRAXIS.NE.0) THEN
C
C---- What sort of rotation ? 2fold? 3fold? 4fold? 6fold?
C
            JSM = JSM + 1
            NROT(JSM) = 0
C
C
            DO 110 IROT = 1,6
              IH = RSMT(1,1,J)*IHR + RSMT(2,1,J)*IKR + RSMT(3,1,J)*ILR
              IK = RSMT(1,2,J)*IHR + RSMT(2,2,J)*IKR + RSMT(3,2,J)*ILR
              IL = RSMT(1,3,J)*IHR + RSMT(2,3,J)*IKR + RSMT(3,3,J)*ILR
C
C---- Back to h k l - how many rotations to get here?
C
              IF (IH.EQ.IN(1) .AND. IK.EQ.IN(2) .AND. IL.EQ.IN(3)) THEN
                GO TO 120
              ELSE
C
C---- apply symmetry again....
C
                IHR = IH
                IKR = IK
                ILR = IL
              END IF
  110       CONTINUE
C
C
            GO TO 130
  120       NROT(JSM) = 10*IRAXIS + IROT
C
C---- Check screwiness for a b and c axes -
C     keep the highest order of it.
C   ( a 6 fold axis will also produce a 2 fold and a 3 fold...)
C
            IF (IRAXIS.LE.3) THEN
              ISCR = 0
              JROT = 4 - IRAXIS
C
C---- Test translation component
C
              IF (RSMT(4,JROT,J).NE.0.0) ISCR = NINT(1.0/
     +            ABS(RSMT(4,JROT,J)))
C
C---- If that one was 0 try this "translation component"
C
              IF (ISCR.EQ.0 .AND. RSMT(JROT,4,J).NE.
     +            0.0) ISCR = NINT(1.0/ABS(RSMT(JROT,4,J)))
              IF (ISCR.GT.ISCREW(JROT)) ISCREW(JROT) = ISCR
            END IF
          END IF
        END IF
  130 CONTINUE
C
C---- Sort rotation info - nrot(...) gt 50 means iraxis = 5
C                                       means cubic   etc...
C
      DO 150 I = 1,JSM
        IRMIN = 1000000
        JMIN = 0
C
C
        DO 140 J = 1,JSM
          IF (IRMIN.GT.NROT(J)) THEN
            JMIN = J
            IRMIN = NROT(J)
          END IF
  140   CONTINUE
C
        NROTS(I) = IRMIN
        JROTS(I) = JMIN
        NROT(JMIN) = 1000001
  150 CONTINUE
C
C---- Get rid of  duplications
C
      JUNIQU = 1
C
      DO 160 I = 2,JSM
        IF (NROTS(I).NE.NROTS(JUNIQU)) THEN
          JUNIQU = JUNIQU + 1
          NROTS(JUNIQU) = NROTS(I)
          JROTS(JUNIQU) = JROTS(I)
        END IF
  160 CONTINUE
C
C---- Choose point group
C
C     Rarest first
C
C  Cubic
C  14 pg23   m3        KLASS 13
C  15 pg432  m3m       KLASS 14
C
      IF (NROTS(JUNIQU).GT.50) THEN
        IF (NROTS(2).EQ.12) THEN
          NAMPG = 'PG23'
          KLASS = 13
          IF (NROTS(3).EQ.14) THEN
            NAMPG = 'PG432'
            KLASS = 14
          END IF
        END IF
C
C---- Space groups with 2 fold axes along 1 1 0
C     8 pg422 4/mmm       KLASS 5
C    11 pg321  3/m        KLASS 9
C    13 pg622 6/mmm       KLASS 12
C
      ELSE IF (NROTS(JUNIQU).GT.40) THEN
        IF (NROTS(2).EQ.12 .AND. NROTS(3).EQ.14) THEN
          NAMPG = 'PG422'
          KLASS = 5
        END IF
        IF (NROTS(2).EQ.13) THEN
          NAMPG = '321'
          KLASS = 9
        END IF
        IF (NROTS(2).EQ.12 .AND. NROTS(3).EQ.13) THEN
          NAMPG = 'PG622'
          KLASS = 12
        END IF
C
C--- 6 pg222  mmm     KLASS = 3
C   11 pg312  3/m     KLASS = 8
C   ?? pg2c   2       KLASS = 16    monoclinic C unique.
C
      ELSE IF (NROTS(JUNIQU).GT.30) THEN
        IF (NROTS(2).EQ.13) THEN
          NAMPG = 'PG312'
          KLASS = 8
        END IF
        IF (NROTS(2).EQ.12 .AND. NROTS(3).EQ.22) THEN
          NAMPG = 'PG222'
          KLASS = 3
        END IF
        IF (NROTS(JUNIQU).EQ.12) THEN
          NAMPG = 'PG2C'
          KLASS = 16
        END IF
C
C---- 4 pg2    2/m
C
      ELSE IF (NROTS(JUNIQU).EQ.22) THEN
        NAMPG = 'PG2'
        KLASS = 2
C
C---- 3 pg1     1bar          KLASS 1
C    ?? pg2A    2/m  A unique KLASS 15
C     7 pg4    4/m            KLASS 4
C     9 pg3     3bar          KLASS 6
C    12 pg6    6/m            KLASS 11
C
      ELSE IF (NROTS(JUNIQU).LT.20) THEN
        IF (NROTS(JUNIQU).EQ.11) NAMPG = 'PG1'
        IF (NROTS(JUNIQU).EQ.11) KLASS = 1
        IF (NROTS(JUNIQU).EQ.12) NAMPG = 'PG2A'
        IF (NROTS(JUNIQU).EQ.12) KLASS = 15
        IF (NROTS(JUNIQU).EQ.13) NAMPG = 'PG3'
        IF (NROTS(JUNIQU).EQ.13) KLASS = 6
        IF (NROTS(JUNIQU).EQ.14) NAMPG = 'PG4'
        IF (NROTS(JUNIQU).EQ.14) KLASS = 4
        IF (NROTS(JUNIQU).EQ.16) NAMPG = 'PG6'
        IF (NROTS(JUNIQU).EQ.16) KLASS = 11
      END IF
C
C---- next symm opn
C
      IF (LPRINT) THEN
        WRITE (STROUT,FMT='(A,4X,I4)')
     +       '  Number of primitive symmetry operators',NSYMP
        CALL PUTLIN(STROUT,'CURWIN')
        WRITE (STROUT,FMT='(A,4X,I4)')
     +       '  Number of  symmetry operators         ',NSYM
        CALL PUTLIN(STROUT,'CURWIN')
        WRITE (STROUT,FMT='(A,4X,A)')
     +       '  The point group for these symmetry operators is ',NAMPG
        CALL PUTLIN(STROUT,'CURWIN')
C     
C     
        DO 170 ISS = 1,NSYM
          WRITE (STROUT,FMT='(A,4X,I3)')
     +         ' Resorted symmetry( all primitives first)',ISS
          CALL PUTLIN(STROUT,'CURWIN')
          DO 175 IDO = 1,4
            WRITE (STROUT,FMT='(4(2X,F8.3))') (RSMT(IDO,J,ISS),J=1,4)
            CALL PUTLIN(STROUT,'CURWIN')
 175      CONTINUE
 170    CONTINUE
C     
C     
        WRITE (STROUT,FMT='(A)')
     +       ' **** ICENTR   gives  axis of centering *****'
        CALL PUTLIN(STROUT,'CURWIN')
        IF (ICENTR.EQ.0) THEN
          STROUT = '  No centering              (P spacegroups)'
        ELSE IF (ICENTR.EQ.1) THEN
          STROUT = '  Centering around a-axis    (A spacegroups)'
        ELSE IF (ICENTR.EQ.2) THEN
          STROUT = '  Centering around b-axis    (B spacegroups)'
        ELSE IF (ICENTR.EQ.3) THEN
          STROUT = '  Centering around c-axis    (C spacegroups)'
        ELSE IF (ICENTR.EQ.4) THEN
          STROUT = '  Centering on all faces     (F spacegroups)'
        ELSE IF (ICENTR.EQ.5) THEN
          STROUT = '  Body centering             (I spacegroups)'
        ELSE IF (ICENTR.EQ.6) THEN
          CALL BLANK('CURWIN',2)
          WRITE (STROUT,FMT='(A)') '  Rhombohedral centering'
          CALL PUTLIN(STROUT,'CURWIN')
          WRITE (STROUT,FMT='(A)') 
     .           ' (R spacegroups with hexagonal axes)'
          CALL PUTLIN(STROUT,'CURWIN')
          WRITE (STROUT,FMT='(A)') 
     +         ' (NOTE: R-spacegroups with rhombohedral axes'
          CALL PUTLIN(STROUT,'CURWIN')
          WRITE (STROUT,FMT='(A)')  ' have ICENTR = 0 !)'
          CALL PUTLIN(STROUT,'CURWIN')
          GO TO 9876
        END IF
C     
C     
        CALL BLANK('CURWIN',2)
        CALL PUTLIN(STROUT,'CURWIN')
C
C
 9876   ISCR = ISCREW(1) + ISCREW(2) + ISCREW(3)
        IF (ISCR.GT.0) THEN
          WRITE (STROUT,FMT='(A)') ' **** Screw axes are: *****'
          CALL BLANK('CURWIN',2)
          CALL PUTLIN(STROUT,'CURWIN')
        END IF
C
        IF (ISCREW(1).GT.0) THEN
          WRITE (STROUT,FMT='(I4,A)') ISCREW(1),
     +         '  fold screw axis along A '
          CALL BLANK('CURWIN',2)
          CALL PUTLIN(STROUT,'CURWIN')
        END IF
C     
        IF (ISCREW(2).GT.0) THEN
          WRITE (STROUT,FMT='(I4,A)') ISCREW(2),
     +         ' fold screw axis along B '
          CALL BLANK('CURWIN',2)
          CALL PUTLIN(STROUT,'CURWIN')
        END IF
C     
        IF (ISCREW(3).GT.0) THEN
          WRITE (STROUT,FMT='(I4,A)') ISCREW(3),
     +         ' fold screw axis along C '
          CALL BLANK('CURWIN',2)
          CALL PUTLIN(STROUT,'CURWIN')
        END IF
C     
        WRITE (STROUT,FMT='(A)')
     +   ' *** KLASS    : a crystal class name used in MDF files ***'
        CALL BLANK('CURWIN',3)
        CALL PUTLIN(STROUT,'CURWIN')  
C
C---- (int. tables)
C
        CALL BLANK('CURWIN',2)
        IF (KLASS.EQ.1) THEN
          STROUT = '  TRICLINIC       1_BAR (PG1)       sgs  1 '
        ELSE IF (KLASS.EQ.2) THEN
          STROUT = '  MONOCLINIC   I  2/M (PG4) B-UNIQUE  sgs  3 -  5'
        ELSE IF (KLASS.EQ.3) THEN
          STROUT = '  ORTHORHOMBIC    MMM (PG222)         sgs 16 - 24'
        ELSE IF (KLASS.EQ.4) THEN
          STROUT = '  TETRAGONAL   I  4/M (PG4)          sgs 75 - 80'
        ELSE IF (KLASS.EQ.5) THEN
          STROUT = '  TETRAGONAL  II  4/MMM (PG422)      sgs 89 - 98 '
        ELSE IF (KLASS.EQ.6) THEN
          STROUT = 
     .     '  TRIGONAL I  3_BAR (PG3)HEXAGONAL AXES sgs  143-146'
        ELSE IF (KLASS.EQ.7) THEN
          STROUT = 
     .     '  TRIGONAL II  3_BAR (??) RHOMBOHEDRAL AXES sgs   146'
        ELSE IF (KLASS.EQ.8) THEN
          STROUT = 
     .      ' TRIGONAL III  3_BAR1M (PG312)       sgs 149,151,153 '
        ELSE IF (KLASS.EQ.9) THEN
          STROUT = ' TRIGONAL IV  3_BARM1 (PG321)HEXAGONAL AXES '//
     +         ' sgs 150,152,154,155'
        ELSE IF (KLASS.EQ.10) THEN
          STROUT = '  TRIGONAL V  3_BARM1 (??)RHOMBOHEDRAL AXES sgs 155'
        ELSE IF (KLASS.EQ.11) THEN
          STROUT = '  HEXAGONAL    I  6/M  (PG6)   sgs        168 - 173'
        ELSE IF (KLASS.EQ.12) THEN
          STROUT = '  HEXAGONAL   II  6/MMM (PG622)    sgs 177 - 182'
        ELSE IF (KLASS.EQ.13) THEN
          STROUT ='  CUBIC        I  M3_BAR (PG23)    sgs 195 - 199'
        ELSE IF (KLASS.EQ.14) THEN 
          STROUT = '  CUBIC       II  M3_BARM (PG432)  sgs 207 - 214'
        ELSE IF (KLASS.EQ.15) THEN
          STROUT = '  MONOCLINIC  II  2/M (??)  A UNIQUE  sgs 3 -   5'
        ELSE IF (KLASS.EQ.16) THEN
          STROUT = '  MONOCLINIC III  2/M (??)  C UNIQUE sgs 3 -   5'
        END IF
        CALL PUTLIN(STROUT,'CURWIN')
      ENDIF
C
C---- In this table only the enantiomorphic spacegroups are
C     included. For the other spacgroups (which contain (glide)
C     mirrors or an inversion center) this routine can still be
C     used, but then isym has no meaning anymore.
C
      END
C     
C   
C     
C     ====================================
      SUBROUTINE PGNLAU(NAMPG,NLAUE,LAUNAM)
C     ====================================
C     
C---- Choose Laue group from PG name.
C     
C     On entry:
C     NAMPG      point-group name
C     
C     On exit:
C     NLAUE     Laue group number
C     LAUNAM    Laue group name
C     
C     .. Scalar Arguments ..
      INTEGER NLAUE
      CHARACTER NAMPG*(*), LAUNAM*(*)
C     ..
      INTEGER LPG
      CHARACTER LOCNAM*12
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     
      NLAUE = 0
      LAUNAM = ' '
      LOCNAM = ' '
      LOCNAM = NAMPG
C     Strip off 'PG' if present
      IF (LOCNAM(1:2) .EQ. 'PG') LOCNAM = LOCNAM(3:)
      LPG = LENSTR(LOCNAM)
C     
C     Rarest first
C     
C     Cubic
C     14 pg23   m3
C     15 pg432  m3m
C     
      IF (LOCNAM(1:LPG).EQ.'23') THEN
        NLAUE = 14
        LAUNAM = 'm3'
      ELSE IF (LOCNAM(1:LPG).EQ.'432') THEN
        NLAUE = 15
        LAUNAM = 'm3m'
C     
C---- 8 pg422 4/mmm
C     11 pg321  3/m        hkl:h>=0, k>=0 with k<=h for all l.
C                           if h = k  l>=0
C           Space group numbers :   150-152-154
C     13 pg622 6/mmm
C     
      ELSE IF (LOCNAM(1:LPG).EQ.'422') THEN
        NLAUE = 8
        LAUNAM = '4/mmm'
      ELSE IF (LOCNAM(1:LPG).EQ.'321') THEN
        NLAUE = 11
        LAUNAM = '3/m'
      ELSE IF (LOCNAM(1:LPG).EQ.'622') THEN
        NLAUE = 13
        LAUNAM = '6/mmm'
C     
C---- 6 pg222  mmm        hkl:h>=0, k>=0, l>=0            16 ...
C     10 pg312  3/m        hkl:h>=0, k>=0 with k<=h for all l.
C                           if k = 0  l>=0
C           Space group numbers :   149-151-153
C     
      ELSE IF (LOCNAM(1:LPG).EQ.'312') THEN
        NLAUE = 10
        LAUNAM = '3/m'
      ELSE IF (LOCNAM(1:LPG).EQ.'222') THEN
        NLAUE = 6
        LAUNAM = '222'
C     
C---- 4 pg2    2/m
C     
      ELSE IF (LOCNAM(1:LPG).EQ.'2') THEN
        NLAUE = 4
        LAUNAM = '2/m'
C     
C---- 3 pg1     1bar
C     7 pg4    4/m
C     9  pg3     3bar
C     12 pg6    6/m
C     
      ELSE IF (LOCNAM(1:LPG).EQ.'1') THEN
        NLAUE = 3
        LAUNAM = '-1'
      ELSE IF (LOCNAM(1:LPG).EQ.'3') THEN
        NLAUE = 9
        LAUNAM = '-3'
      ELSE IF (LOCNAM(1:LPG).EQ.'4') THEN
        NLAUE = 7
        LAUNAM = '4/m'
      ELSE IF (LOCNAM(1:LPG).EQ.'6') THEN
        NLAUE = 12
        LAUNAM = '6/m'
      END IF
C     
      IF (NLAUE.EQ.0)           CALL CCPERR(1,
     +  ' You have not defined PG name properly ')
C     
C     
      END
C
C
C
      SUBROUTINE ASUSET(
     .     SPGNAM,NUMSGP,PGNAME,MSYM,RRSYM,MSYMP,MLAUE,LPRINT)
C     ========================================================
C
C  Set up & store symmetry for later use in ASUPUT or ASUGET
C
C  On input:
C    SPGNAM  space-group name (not used)
C    NUMSGP  space-group number (not used)
C    PGNAME  point-group name (if returned from SYMOP.LIB)
C    MSYM    total number of symmetry operations
C    RRSYM(4,4,MSYM) symmetry matrices (real-space)
C
C  On output:
C    PGNAME  point-group name
C    MSYMP   number of primitive symmetry operations
C    MLAUE   Laue group number
C
C
C Arguments:
      INTEGER NUMSGP, MSYM, MSYMP, MLAUE
      REAL    RRSYM(4,4,96) 
ccMSYM)
      CHARACTER*(*) SPGNAM, PGNAME
      LOGICAL LPRINT
C
C Common blocks
C
C Common block /RECSYM/
C   RSYM     real-space symmetry operators
C   RSYMIV   their inverse
C   NSYM     number of symmetry operations
C   NSYMP    number of primitive symmetry operations
C   NLAUE    number of Laue group
      INTEGER MAXSYM
      PARAMETER (MAXSYM=96)
      COMMON /RECSYM/RSYM(4,4,MAXSYM),RSYMIV(4,4,MAXSYM),
     .    NSYM,NSYMP,NLAUE
      INTEGER NSYM,NSYMP,NLAUE
      REAL RSYM,RSYMIV
      SAVE /RECSYM/
C
C Functions
      INTEGER LENSTR
C Locals
      INTEGER I, J, L
      CHARACTER*100 STROUT, NAME*8, LAUNAM*8
C
C Get point group and primitive-only operations
      CALL PGDEFN(PGNAME,MSYMP,MSYM,RRSYM,.FALSE.)
C
C Get Laue group number
      CALL PGNLAU(PGNAME,MLAUE,LAUNAM)
C
C Store in common block
      NSYM = MSYM
      NSYMP = MSYMP
      NLAUE = MLAUE
      DO 10, I=1,NSYM
        CALL CCPMVI(RSYM(1,1,I), RRSYM(1,1,I), 16)
C Invert all symmetry operators
        CALL INVSYM(RSYM(1,1,I), RSYMIV(1,1,I))
 10   CONTINUE
C
C Print
      IF (LPRINT) THEN
        CALL BLANK('CURWIN',1)
        STROUT = '          Reciprocal space symmetry'
        CALL PUTLIN(STROUT,'CURWIN')
        NAME = PGNAME
        L = LENSTR(NAME)
        IF (NAME(1:2) .EQ. 'PG') NAME = NAME(3:L)
        L = LENSTR(NAME)
        I = LENSTR(SPGNAM)
        J = LENSTR(LAUNAM)
        WRITE (STROUT, 6001) 
     .         SPGNAM(1:I),NUMSGP,NAME(1:L),LAUNAM(1:J)
 6001   FORMAT('    Space group: ',A,' (',I3,')',5X,
     .    'Point group: ',A,5X,'Laue group: ',A)
        CALL PUTLIN(STROUT,'CURWIN')
C        
        IF (NLAUE .EQ. 3) THEN
          STROUT = '[-1] hkl:l>=0  hk0:h>=0  0k0:k>=0'
        ELSEIF (NLAUE .EQ. 4) THEN
          STROUT = '[2/m] hkl:k>=0, l>=0  hk0:h>=0'
        ELSEIF (NLAUE .EQ. 6) THEN
          STROUT = '[mmm] hkl:h>=0, k>=0, l>=0'
        ELSEIF (NLAUE .EQ. 7) THEN
          STROUT = '[4/m] hkl:h>=0, l>=0 with k>=0 if h=0'//
     .       ' and k>0 if h>0'
        ELSEIF (NLAUE .EQ. 8) THEN
          STROUT = '[4/mmm] hkl:h>=0, k>=0, l>=0 and h>=k'
        ELSEIF (NLAUE .EQ. 9) THEN
          STROUT = '[-3] hkl:h>=0, k>0  00l:l>0'
        ELSEIF (NLAUE .EQ. 10) THEN
          STROUT = '[312] hkl:h>=0, k>=0 with k<=h '//
     .          'for all l. If h = 0  l>=0'
        ELSEIF (NLAUE .EQ. 11) THEN
          STROUT = '[321] hkl:h>=0, k>=0 with k<=h '//
     .          'for all l. If h = k  l>=0'
        ELSEIF (NLAUE .EQ. 12) THEN
          STROUT = '[6/m] hkl:h>=0, k>=0, l>=0 with k>=0'//
     .          ' if h=0, and k> 0 if h>0'
        ELSEIF (NLAUE .EQ. 13) THEN
          STROUT = '[6/mmm] hkl:h>=0, k>=0, l>=0 with h>=k'
        ELSEIF (NLAUE .EQ. 14) THEN
          STROUT = '[m3] hkl:h>=0, k>=0, l>=0 with l>=h,'//
     .          ' k>=h if l=h, k> h if l>h'
        ELSEIF (NLAUE .EQ. 15) THEN
          STROUT = 
     .          '[m3m] hkl:h>=0, k>=0, l>=0 with k>=l, and l>=h'
        ELSE
          WRITE(STROUT, 6020) ' **** Illegal Laue group : ',NLAUE
 6020     FORMAT(1X,A,I6)
          CALL PUTLIN(STROUT,'CURWIN')
                    CALL CCPERR(1,' STOP in SYMLIB.for 7711')
        ENDIF
C
        STROUT = 'Asymmetric unit: '//STROUT
        CALL PUTLIN(STROUT,'CURWIN')
C
C Print symmetry operations
        CALL PRTRSM(PGNAME, NSYMP, RSYMIV)
C
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE ASUPUT(IHKL,JHKL,ISYM)
C     =================================
C
C Put reflection into asymmetric unit defined by call to ASUSET
C
C On input:
C    IHKL(3)    input indices hkl
C
C On output:
C    JHKL(3)    output indices hkl
C    ISYM       symmetry number for output
C                 odd  numbers are for I+
C                 even numbers are for I-
C               real-space symmetry operation number L = (ISYM-1)/2 + 1
C
C  The real-space symmetry matrices are applied by premultiplying them
C  by a row vector hkl,  ie  (h'k'l') = (hkl)R
C
C
C  Arguments
      INTEGER IHKL(3), JHKL(3), ISYM
C
C Common block /RECSYM/
C   RSYM     real-space symmetry operators
C   RSYMIV   their inverse
C   NSYM     number of symmetry operations
C   NSYMP    number of primitive symmetry operations
C   NLAUE    number of Laue group
      INTEGER MAXSYM
      PARAMETER (MAXSYM=96)
      COMMON /RECSYM/RSYM(4,4,MAXSYM),RSYMIV(4,4,MAXSYM),
     .    NSYM,NSYMP,NLAUE
      INTEGER NSYM,NSYMP,NLAUE
      REAL RSYM,RSYMIV
      SAVE /RECSYM/
C
C Routines
      INTEGER INASU
      EXTERNAL INASU
C Locals
      INTEGER I,L,ISGN
      CHARACTER*100 LINERR
C
C
      DO 10, L=1,NSYMP
C  h' = h R   ie row vector h premultiplies symmetry matrix
        DO 20, I=1,3
          JHKL(I) = IHKL(1)*RSYM(1,I,L) + IHKL(2)*RSYM(2,I,L)
     .            + IHKL(3)*RSYM(3,I,L)
 20     CONTINUE
C Test this index against the asymmetric unit
C   Function INASU returns 0 if outside au, else +-1 depending on sign
C 
        ISGN = INASU(JHKL, NLAUE)
        IF (ISGN .NE. 0) GO TO 100
C Failed, try next symmetry
 10   CONTINUE
C
C Shouldn't get here, can't reduce reflection to asymmetric unit
      WRITE (LINERR,'(A,3I4,A)') 
     .    'ASUPUT: can''t put reflection ',IHKL,
     .     ' into asymmetric unit'
C Fatal error, stop in routine
      CALL LERROR(2,-1,LINERR)
C
C Succesful, multiply by sign
 100  DO 101, I=1,3
        JHKL(I) = JHKL(I)*ISGN
 101  CONTINUE
C 
      IF (ISGN .GT. 0) THEN
C I+, odd ISYM
        ISYM = L*2 - 1
      ELSE
C I-, even ISYM
        ISYM = L*2
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE ASUGET(IHKL,JHKL,ISYM)
C     =================================
C
C Get original indices of reflection from  asymmetric unit,
C   ie reverse operation of ASUPUT
C   symmetry defined by call to ASUSET
C
C On input:
C    IHKL(3)    input unique indices hkl
C    ISYM       symmetry number for output
C                 odd  numbers are for I+
C                 even numbers are for I-
C               real-space symmetry operation number L = (ISYM-1)/2 + 1
C
C On output:
C    JHKL(3)    output original indices hkl
C
C  The real-space symmetry matrices are applied in ASUPUT by 
C premultiplying them by a row vector hkl,  ie  (h'k'l') = (hkl)R
C  So here we calculate (hkl) = (h'k'l') R**-1
C
C
C  Arguments
      INTEGER IHKL(3), JHKL(3), ISYM
C
C Common block /RECSYM/
C   RSYM     real-space symmetry operators
C   RSYMIV   their inverse
C   NSYM     number of symmetry operations
C   NSYMP    number of primitive symmetry operations
C   NLAUE    number of Laue group
      INTEGER MAXSYM
      PARAMETER (MAXSYM=96)
      COMMON /RECSYM/RSYM(4,4,MAXSYM),RSYMIV(4,4,MAXSYM),
     .    NSYM,NSYMP,NLAUE
      SAVE /RECSYM/
      INTEGER NSYM,NSYMP,NLAUE
      REAL RSYM,RSYMIV
C
C Locals
      INTEGER I,J,L,ISGN
      CHARACTER*100 LINERR
C
C L is symmetry operation number
      L = (ISYM+1)/2
      IF (L .LT. 1 .OR. L .GT. NSYMP) THEN
        WRITE (LINERR,6001) ISYM, NSYMP
 6001   FORMAT(' ASUGET: illegal symmetry number ',I5,
     .       ' outside range of ',i5,' symmetry operators')
        CALL LERROR(2,-1,LINERR)
C  Stop in error routine
      ENDIF
C
C  Sign code +-1
      ISGN = MOD(ISYM,2)
      IF (ISGN.EQ.0) ISGN = -1
C
C  h' = h R   ie row vector h premultiplies symmetry matrix
        DO 20, I=1,3
          JHKL(I) = IHKL(1)*RSYMIV(1,I,L) + IHKL(2)*RSYMIV(2,I,L)
     .            + IHKL(3)*RSYMIV(3,I,L)
 20     CONTINUE
C
C  Multiply by sign
        DO 101, I=1,3
          JHKL(I) = JHKL(I)*ISGN
 101    CONTINUE
C 
      RETURN
      END
C
C
C
      INTEGER FUNCTION INASU(IH, NLAUE)
C     =================================
C
C   This used to be FUNCTION XRY173(NLAUE,IH,ISGN)
C***********************************************************************
C***********************************************************************
C*****                         *****************************************
C***** ROUTINE ( XRY173        *****************************************
C*****                         *****************************************
C*****     From XRAY77 or something like it ****************************
C***********************************************************************
C***********************************************************************
C
C  Arguments:
C   NLAUE - code number for this pointgroup
C   IH(3) - indices
C
C Returns:
C   INASU = +1  if  h k l chosen
C   INASU = -1  if -h-k-l chosen
C   INASU =  0   if reflection is out-of-bounds
C
C
C  Reciprocal space zones: please check spacegroup numbers.
C
C
Code:3 pg1     1bar      hkl:l>=0  hk0:h>=0  0k0:k>=0
C           Space group numbers :   1,2
Code:4 pg2    2/m        hkl:k>=0, l>=0  hk0:h>=0       3/b,4/b....
C           Space group numbers :   3 -15 B axis unique.
Code:6 pg222  mmm        hkl:h>=0, k>=0, l>=0            16 ...
C           Space group numbers :   16-74
Code:7 pg4    4/m        hkl:h>=0, l>=0 with k>=0 if  h=0  and
C                                           k> 0 if  h>0.
C           Space group numbers :   75-88
Code:8 pg422 4/mmm       hkl:h>=0, k>=0, l>=0 and h>=k
C           Space group numbers :   89-142
Code:9  pg3     3bar      hkl:h>=0, k>0  00l:l>0
C           Space group numbers :   143-148
Code:  10 pg312  3/m        hkl:h>=0, k>=0 with k<=h for all l.
C                           if k = 0  l>=0
C           Space group numbers :   149-151-153
Code:  11 pg321  3/m        hkl:h>=0, k>=0 with k<=h for all l.
C                           if h = k  l>=0
C           Space group numbers :   150-152-154
Code:12 pg6    6/m        hkl:h>=0, k>=0, l>=0 with k>=0 if  h=0
C                                            and  k> 0 if  h>0.
C           Space group numbers :   168-176
Code:13 pg622 6/mmm       hkl:h>=0, k>=0, l>=0 with h>=k
C           Space group numbers :   177-194
Code:14 pg23   m3         hkl:h>=0, k>=0, l>=0 with l>=h,
C                                                 k>=h if l=h
C                                                 k> h if l>h.
C           Space group numbers :   195-206
Code:15 pg432  m3m        hkl:h>=0, k>=0, l>=0 with k>=l, and l>=h.
C           Space group numbers :   207-232
C
C
C -- TEST FOR HKL IN ASYMMETRIC UNIT
C     INCORPORATED INTO DATCO5 JUNE 70
C
C
C Arguments
      INTEGER IH(3), NLAUE
C Locals
      INTEGER J,K,L,ISGN
C
C----MOVE INDICES INTO J,K, AND L
      J = IH(1)
      K = IH(2)
      L = IH(3)
      ISGN=1
C----GO TO TEST IF INDICES ARE IN DESIRED UNIT
1     GO TO (100,150,200,250,300,350,400,450,550,570,600,650,700,750,
     1  800),NLAUE
 100  CONTINUE
C----1 BAR (ALTERNATE 1)
C     HKL --    H.GE.0
C     0KL --    K.GE.0
C     00L --    L.GE.0
      IF (J) 7,105,5
 105  IF (K) 7,110,5
 110  IF (L) 7,5,5
C
 150  CONTINUE
C----1 BAR (ALTERNATE I)
C     HKL --    K.GE.0
C     H0L --    L.GE.0
C     H00 --    H.GE.0
      IF (K) 7,155,5
 155  IF (L) 7,160,5
 160  IF (J) 7,5,5
C
C
C  Corresponds to Data reduction unique set for  pg1
C   3 pg1     1bar      hkl:l>=0  hk0:h>=0  0k0:k>=0   1,2
 200  CONTINUE
C----1 BAR (ALTERNATE 3)
C     HKL --    L.GE.0
C     HK0 --    H.GE.0
C     0K0 --    K.GE.0
      IF (L) 7,205,5
 205  IF (J) 7,210,5
 210  IF (K) 7,5,5
C
 250  CONTINUE
C----2/M (ALTERNATE 1)
C     HKL --    K.GE.0 AND L.GE.0
C     HK0 --    H.GE.0
      IF (K) 7,255,255
 255  IF (L) 7,260,5
 260  IF (J) 7,5,5
C
C  Corresponds to Data reduction unique set for  pg2
C   4 pg2    2/m        hkl:k>=0, l>=0  hk0:h>=0       3/b,4/b....
 300  CONTINUE
C----2/M (ALTERNATE 2)
C     HKL --    L.GE.0 AND H.GE.0
C     0KL --    K.GE.0
      IF (L) 7,305,305
 305  IF (J) 7,310,5
 310  IF (K) 7,5,5
C
C  Corresponds to Data reduction unique set for  pg222
C   6 pg222  mmm        hkl:h>=0, k>=0, l>=0            16 ...
 350  CONTINUE
C----MMM
C     HKL --    H.GE.0, K.GE.0, AND L.GE.0
      IF (J) 7,355,355
 355  IF (K) 7,361,361
C****NO 3-SICKLIES PLEASE
 361  IF (L) 7,5,5
C
C  Corresponds to Data reduction unique set for  pg4
C   7 pg4    4/m        hkl:h>=0, l>=0 with k>=0 if  h=0  and
C                                           k> 0 if  h>0.
 400  CONTINUE
C----4/M
C     HKL --    H.GE.0, L.GE.0, WITH K.GE.0 IF H.EQ.0 OR
C               K.GE.1 IF H.GT.0
      IF (L) 7,405,405
 405  IF (J) 7,410,415
 410  IF (K) 7,5,5
 415  IF (K) 7,7,5
C
C  Corresponds to Data reduction unique set for  pg422
C   8 pg422 4/mmm       hkl:h>=0, k>=0, l>=0 and h>=k   89..
 450  CONTINUE
C----4/MMM
C     HKL --    H.GE.0, K.GE.0, AND L.GE.0 WITH H.GE.K
      IF (J) 7,455,455
 455  IF (K) 7,460,460
 460  IF (L) 7,465,465
 465  IF (J-K) 7,5,5
C
 500  CONTINUE
C----3 BAR
C      HKL ---   H.LE.0, L.GE.0, WITH K.GE.0 IF H.LE.0 OR
C               K.GE.1 IF H.LT.0
C      HK0  --  K.GT.-H
CC
C  1978 CHANGED TO FIT N.ISAACS
C  INCLUDE 0 0 L,  HK0 K.LT.0 H.GE.-K     HKL H.GE.0 K.LT.0
CC
      IF(J)7,505,505
505   IF(K)510,510,7
510   IF(L)7,515,520
515   IF(K)516,7,7
516   IF(J+K)7,5,5
520   IF(J-K)521,5,521
521   IF(K)5,7,7
C
C  Corresponds to Data reduction unique set for  pg3
C  9 pg3     3bar      hkl:h>=0, k>0  00l:l>0         143..
550   CONTINUE
C   ALTERNATIVE FOR R3
C  H.GE.0 K.GT.0    ALL L
C  H=0 K=0 L.GT.0
      IF(J)7,555,556
555   IF(K)7,557,5
556   IF(K)7,7,5
557   IF(L)7,7,5
C
C
C
C  Corresponds to Data reduction unique set for  pg312 
C  10 pg312        hkl:h>=0, k>=0 with k<=h for all l.
C                           if k = 0  l>=0
 570  CONTINUE
C----3 BAR M
C  H.GE.0 K.GE.0 K.LE.H   ALL L
C  H=K  L.GE.0
      IF(J)7,575,575
575   IF(K)7,577,576
576   IF(J-K)7,5,5
577   IF(L)7,5,5
C
C
C  Corresponds to Data reduction unique set for  pg321
C  11 pg321        hkl:h>=0, k>=0 with k<=h for all l.
C                           if h = k  l>=0
 600  CONTINUE
C----3 BAR M
C  H.GE.0 K.GE.0 K.LE.H   ALL L
C  H=K  L.GE.0
      IF(J)7,605,605
605   IF(K)7,606,606
606   IF(J-K)7,607,5
607   IF(L)7,5,5
C
C  Corresponds to Data reduction unique set for  pg6
C  12 pg6    6/m        hkl:h>=0, k>=0, l>=0 with k>=0 if  h=0
C                                            and  k> 0 if  h>0.
 650  CONTINUE
C----6/M
C     HKL --    H.GE.0, L.GE.0, WITH K.GE.0 IF H.EQ.0 OR
C               K.GE.1 IF H.GT.0
      IF (L) 7,655,655
 655  IF (J) 7,660,665
 660  IF (K) 7,5,5
 665  IF (K) 7,7,5
C
C  Corresponds to Data reduction unique set for  pg622
C  13 pg622 6/mmm       hkl:h>=0, k>=0, l>=0 with h>=k 177..
  700 CONTINUE
C----6/MMM
C     HKL --    H.GE.0, K.GE.0, AND L.GE.0 WITH H.GE.K
      IF (J) 7,705,705
 705  IF (K) 7,710,710
710   IF(J-K)7,715,715
715   IF(L)7,5,5
C
C  Corresponds to Data reduction unique set for  pg23
C  14 pg23   m3         hkl:h>=0, k>=0, l>=0 with l>=h,
C                                                 k>=h if l=h
C                                                 k> h if l>h.
 750  CONTINUE
C----M3
C     HKL --    H.GE.0, K.GE.0, AND L.GE.0 WITH L.GE.H AND WITH
C               K.GE.H IF L.EQ.H OR K.GT.H IF L.GT.H
      IF (J) 7,755,755
 755  IF (K) 7,760,760
 760  IF (L) 7,765,765
 765  IF (L-J) 7,770,775
 770  IF (K-J) 7,5,5
 775  IF (K-J) 7,7,5
C
C  Corresponds to Data reduction unique set for  pg432
C  15 pg432  m3m        hkl:h>=0, k>=0, l>=0 with k>=l, and l>=h.
 800  CONTINUE
C----M3M
C     HKL --    H.GE.0, K.GE.0, AND L.GE.0 WITH
C              K.GE.L AND L.GE.H
      IF (J) 7,805,805
 805  IF (K) 7,810,810
 810  IF (L) 7,815,815
 815  IF (K-L) 7,820,820
 820  IF (L-J) 7,5,5
C
C----REFLECTION IS IN BOUNDS
 5    INASU = ISGN
      RETURN
C     ======
C
C----REFLECTION IS OUT OF BOUNDS
 7    IF (ISGN .EQ. +1) THEN
C     TRY -H -K -L
        ISGN=-1
        J=-J
        K=-K
        L=-L
        GO TO 1
      ENDIF
C Index failed, exit
      INASU = 0
      RETURN
C     ======
C
      END
C
C
C
      SUBROUTINE PRTRSM(PGNAME, NSYMP, RSYMIV)
C     ========================================
C
C Print reciprocal space symmetry operations
C
C PGNAME              point-group name
C NSYMP               number of primitive symmetry operators
C RSYMIV(4,4,NSYMP)   inverse real-space symmetry matrices
C
C  The real-space symmetry matrices are applied by premultiplying them
C  by a row vector hkl,  ie  (h'k'l') = (hkl)R
C
C
C  Arguments
      INTEGER NSYMP
      REAL RSYMIV(4,4,*)
C
      CHARACTER*(*) PGNAME
C
C Functions
      INTEGER LENSTR
      EXTERNAL LENSTR
C
C Locals
      INTEGER I,J,K,L,M,ISYM,ISGN,LP,NLINC,NLMAX
      CHARACTER LINE*80, HKL(3)*1,NAME*8
      DATA   HKL/'h','k','l'/
C
      LINE = ' '
      CALL BLANK('CURWIN',1)
C
      LINE = 'Original indices for reflection hkl with '//
     .     'symmetry number ISYM'
      CALL PUTLIN(LINE,'CURWIN')
C
      NLINC = (NSYMP+3)/4
C
C Loop positive & negative operations
C
      DO 100, ISGN = +1, -1, -2
        IF (ISGN .EQ. +1) THEN
          LINE = '                             Bijvoet positive'
        ELSE
          LINE = '                             Bijvoet negative'
        ENDIF
        CALL BLANK('CURWIN',1)
        CALL PUTLIN(LINE,'CURWIN')
C
        LINE = ' '
        NLMAX = MIN(NSYMP,1+NLINC*3)
        L = 7
        DO 105, K=1,NLMAX,NLINC
          LINE(L:) = 'ISYM'
          L = L+18
 105    CONTINUE
        CALL PUTLIN(LINE,'CURWIN')
C Loop symmetry 4 / line
        DO 110, L = 1,(NSYMP+3)/4
          LINE = ' ISYM'
          NLMAX = MIN(NSYMP,L+NLINC*3)
          DO 120, K = L, NLMAX, NLINC
C   LP is character pointer in LINE
            LP = ((K-L)/NLINC)*18 + 7
C   ISYM is symmetry number, odd for I+, even for I-
            ISYM = K*2
            IF (ISGN .EQ. +1) ISYM = ISYM - 1
            WRITE(LINE(LP:),'(I3)') ISYM
            LP = LP+4
            DO 130, J=1,3
              DO 140, I=1,3
C     Extract matrix element as +-1
                M = NINT(RSYMIV(I,J,K))*ISGN
C     Add +-h,k,l to line if M .ne. 0
                IF (M .LT. 0) THEN
                  LINE(LP+1:LP+2) = '-'//HKL(I)
                  LP = LP+2
                ELSEIF (M .GT. 0) THEN
                  LINE(LP+1:LP+2) = '+'//HKL(I)
                  LP = LP+2
                ENDIF
 140          CONTINUE
C add ','
              LP = LP+1
              IF (J .LT. 3) LINE(LP:LP) = ','
 130        CONTINUE
 120      CONTINUE
          CALL PUTLIN(LINE,'CURWIN')
 110    CONTINUE
 100  CONTINUE
      CALL BLANK('CURWIN',1)
      RETURN
      END
C
C
C     ================================================
      SUBROUTINE ASUPHP(JHKL,LSYM,ISIGN,PHASIN,PHSOUT)
C     ================================================
C
C
C---- Generate phase of symmetry equivalent JHKL from that of IHKL
C
C     On input:
C
C    JHKL(3)    indices hkl generated in ASUPUT
C    LSYM       symmetry number for generating JHKL
C    ISIGN         =  1   for I+
C                  = -1   for I-
C    PHASIN     phase for reflection IHKL(3)
C
C     On output:
C
C    PHSOUT     phase for reflection JHKL(3)
C
C  The real-space symmetry matrices are applied by premultiplying them
C  by a row vector hkl,  ie  (h'k'l') = (hkl)R
C
C Common block /RECSYM/
C   RSYM     real-space symmetry operators
C   RSYMIV   their inverse
C   NSYM     number of symmetry operations
C   NSYMP    number of primitive symmetry operations
C   NLAUE    number of Laue group
C
C
C
C     .. Parameters ..
      INTEGER           MAXSYM
      PARAMETER         (MAXSYM=96)
C     ..
C     .. Scalar Arguments ..
      REAL              PHASIN,PHSOUT
      INTEGER           ISIGN,LSYM
C     ..
C     .. Array Arguments ..
      INTEGER           JHKL(3)
C     ..
C     .. Scalars in Common ..
      INTEGER           NLAUE,NSYM,NSYMP
C     ..
C     .. Arrays in Common ..
      REAL              RSYM,RSYMIV
C     ..
C     .. Local Scalars ..
      REAL              PHASCH
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         MOD
C     ..
C     .. Common blocks ..
      COMMON            /RECSYM/RSYM(4,4,MAXSYM),RSYMIV(4,4,MAXSYM),
     +                  NSYM,NSYMP,NLAUE
C     ..
      SAVE
C
C---- Get symmetry operator and sign from isym
C
      PHSOUT = PHASIN
C
C----  phase = -phase for Friedel equivalent
C
      IF (ISIGN.EQ.-1) PHSOUT = -PHASIN
C
C----  Phase change taken from old CADLCF
C
C      PHASCH=360.*(IHD*rot(1,4,ISYM)+IKD*rot(2,4,ISYM)
C     1 +ILD*Rot(3,4,ISYM))
C
      PHASCH = (JHKL(1)*RSYM(1,4,LSYM)+JHKL(2)*RSYM(2,4,LSYM)+
     +         JHKL(3)*RSYM(3,4,LSYM))*360.0
      PHSOUT = PHSOUT + PHASCH
C
C----  Put phase in range 0 to 360
C
      PHSOUT = MOD(PHSOUT+36000.0,360.0)
      RETURN
      END
C
C
C
      SUBROUTINE PATSGP(SPGNAM, PGNAME, PATNAM, LPATSG)
C     =================================================
C
C Determine Patterson spacegroup from true space-group
C
C On entry:
C     SPGNAM    space-group name. Only used to determine lattice centering
C     PGNAME    point-group name
C
C On exit:
C     PATNAM    name of Patterson spacegroup
C     LPATSG    number of Patterson spacegroup
C
      CHARACTER*(*) SPGNAM, PGNAME, PATNAM
      INTEGER LPATSG
C
      CHARACTER NMPG*8
C
C
C Strip off 'PG' is present
      IF (PGNAME(1:2) .EQ. 'PG') THEN
         NMPG = PGNAME(3:)
      ELSE
         NMPG = PGNAME
      ENDIF
C
C    Patterson space groups
C      P-1     PG1
C      P2/m    PG2
C      C2/m    PG2
C      Pmmm    PG222
C      Cmmm    PG222
C      Fmmm    PG222
C      Immm    PG222
C      P4/m    PG4
C      I4/m    PG4
C      P4/mmm  PG4/mmm
C      I4/mmm  PG4/mmm
C      P-3     PG3
C      R-3     PG3
C      P-31m   PG312
C      P-3m1   PG321
C      R-3m    PG321
C      P6/m    PG622
C      P6/mmm  PG622
C      Pm-3    PG23
C      Fm-3    PG23
C      Im-3    PG23
C      Pm-3m   PG432
C      Fm-3m   PG432
C      Im-3m   PG432
C
      IF (NMPG.EQ.'1'        .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 2
         PATNAM = 'P-1'
      ELSEIF (NMPG.EQ.'2'    .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 10
         PATNAM = 'P2/m'
      ELSEIF (NMPG.EQ.'2'    .AND. SPGNAM(1:1).EQ.'C') THEN
         LPATSG = 12
         PATNAM = 'C2/m'
      ELSEIF (NMPG.EQ.'222'  .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 47
         PATNAM = 'Pmmm'
      ELSEIF (NMPG.EQ.'222'  .AND. SPGNAM(1:1).EQ.'C') THEN
         LPATSG = 65
         PATNAM = 'Cmmm'
      ELSEIF (NMPG.EQ.'222'  .AND. SPGNAM(1:1).EQ.'F') THEN
         LPATSG = 69
         PATNAM = 'Fmmm'
      ELSEIF (NMPG.EQ.'222'  .AND. SPGNAM(1:1).EQ.'I') THEN
         LPATSG = 71
         PATNAM = 'Immm'
      ELSEIF (NMPG.EQ.'4'    .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 83
         PATNAM = 'P4/m'
      ELSEIF (NMPG.EQ.'4'    .AND. SPGNAM(1:1).EQ.'I') THEN
         LPATSG = 87
         PATNAM = 'I4/m'
      ELSEIF (NMPG.EQ.'422'  .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 123
         PATNAM = 'P4/mmm'
      ELSEIF (NMPG.EQ.'422'  .AND. SPGNAM(1:1).EQ.'I') THEN
         LPATSG = 139
         PATNAM = 'I4/mmm'
      ELSEIF (NMPG.EQ.'3'    .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 147
         PATNAM = 'P-3'
      ELSEIF (NMPG.EQ.'3'    .AND. SPGNAM(1:1).EQ.'R') THEN
         LPATSG = 148
         PATNAM = 'R-3'
      ELSEIF (NMPG.EQ.'312'  .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 162
         PATNAM = 'P-31m'
      ELSEIF (NMPG.EQ.'321'  .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 164
         PATNAM = 'P-3m1'
      ELSEIF (NMPG.EQ.'321'  .AND. SPGNAM(1:1).EQ.'R') THEN
         LPATSG = 166
         PATNAM = 'R-3m'
      ELSEIF (NMPG.EQ.'6'    .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 175
         PATNAM = 'P6/m'
      ELSEIF (NMPG.EQ.'622'  .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 191
         PATNAM = 'P6/mmm'
      ELSEIF (NMPG.EQ.'23'   .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 200
         PATNAM = 'Pm-3'
      ELSEIF (NMPG.EQ.'23'   .AND. SPGNAM(1:1).EQ.'F') THEN
         LPATSG = 202
         PATNAM = 'Fm-3'
      ELSEIF (NMPG.EQ.'23'   .AND. SPGNAM(1:1).EQ.'I') THEN
         LPATSG = 204
         PATNAM = 'Im-3'
      ELSEIF (NMPG.EQ.'432'  .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 221
         PATNAM = 'Pm-3m'
      ELSEIF (NMPG.EQ.'432'  .AND. SPGNAM(1:1).EQ.'F') THEN
         LPATSG = 225
         PATNAM = 'Fm-3m'
      ELSEIF (NMPG.EQ.'432'  .AND. SPGNAM(1:1).EQ.'I') THEN
         LPATSG = 229
         PATNAM = 'Im-3m'
      ELSE
         LPATSG = 0
         PATNAM = ' '
      ENDIF
C
      RETURN
      END
