C************ This file contains the following routines ***********
C
C
C      SUBROUTINE MWHBRK(IUNIT,RHMIN,RHMAX,NBXYZ,NBUVW,LBUVW,NBRKS)
C      SUBROUTINE MWRBRK(IUNIT,BRICK)
C      SUBROUTINE MRHBRK(IUNIT,NBXYZ,NNBUVW,NLBUVW,NNBRKS)
C      SUBROUTINE MRDBRK(IUNIT,BRICK,NBRICK,MUVW,IER)
C      SUBROUTINE MWHVEC(IUNIT,NLEV,CONLEV,NBXYZ,NBUVW,LBUVW,NBRKS)
C      SUBROUTINE MWRVEC(IUNIT,VECTOR,PSBITS,NVEC,JVEC)
C      SUBROUTINE MWRDIR(IUNIT,JDIREC)
C      SUBROUTINE MRHVEC(IUNIT,NLEV,CONLEV,NBXYZ,NNBUVW,NLBUVW,NNBRKS)
C      SUBROUTINE MRDVEC(IUNIT,VECTOR,PSBITS,NVEC,NBRICK,LEVEL,IER)
C      SUBROUTINE MBRORG(IBRORG,NBRICK,LBUVW,NBUVW,LORUVW)
C      SUBROUTINE MRTOBY(BBRICK,RBRICK,N,PROD,PLUS)
C      SUBROUTINE MBYTOR(RBRICK,BBRICK,N,PROD,PLUS)
C      SUBROUTINE PAKVEC(PACKV,VECTOR,PSBITS,NVEC,PROD,PLUS)
C      SUBROUTINE UNPVEC(VECTOR,PSBITS,PACKV,NVEC,PROD,PLUS,IBRORG)
C
C
C
C
      SUBROUTINE MWHBRK(IUNIT,RHMIN,RHMAX,NBXYZ,NBUVW,LBUVW,NBRKS)
C     ============================================================
C
C---- Mode 10, bricked map: calculate packing constants PROD and PLUS,
C     and add extra items to header block
C
C NB This subroutine should be called after MWRHDL (or MWRHDR)
C    AND after MSYCPY or MSYPUT, since it needs to know about
C    any symmetry operations in the file
C
C    IUNIT stream number (not used)
C
C    RHMIN, RHMAX    minimum and maximum densities in input map
C
C    NBXYZ(3)        number of grid points per brick along xyz
C
C Returns:
C    NBUVW(3)   number of grid points/brick along uvw
C    LBUVW(3)   number of bricks in map along uvw
C               (allowing for adjacent
C                bricks sharing a common face).
C    NBRKS      total number of bricks in map
C
C    MINPAK,MAXPAK   minimum and maximum packed densities ( note that
C               RHMIN & RHMAX refer to original unpacked densities)
C
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
C     ..
C     .. Scalar Arguments ..
      REAL RHMAX,RHMIN
      INTEGER IUNIT,NBRKS
C     ..
C     .. Array Arguments ..
      INTEGER LBUVW(3),NBUVW(3),NBXYZ(3)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS,PLUS,PROD
      INTEGER ISPG,ITMHDR,ITMSC1,LSKFLG,MODE,NC,NC1,NCHITM,NLAB,NR,NR1,
     +        NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER JUNK,LABELS,LNBXYZ,LSTRM,MAPCRS,NXYZ
C     ..
C     .. Local Scalars ..
      REAL DELPAK,DELRH
      INTEGER I,L,MAXPAK,MINPAK
C     ..
C     .. Local Arrays ..
      INTEGER NUVW(3)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC REAL
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       PROD,PLUS,LNBXYZ(3),JUNK(12),ARMS,NLAB,LABELS(20,10),
     +       NCHITM,ITMHDR,ITMSC1
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (NC,NUVW(1)), (NR,NUVW(2)), (NS,NUVW(3))
C     ..
C     .. Data statements ..
      DATA MINPAK,MAXPAK/-127,127/
C     ..
C
C
C
C---- Check mode 10
C
      IF (MODE.NE.10) THEN
        WRITE (LUNOUT,FMT=6000) MODE
        STOP
      ELSE
C
C---- PROD, PLUS      packing parameters
C                     packed density = density * PROD + PLUS
C
        DELRH = RHMAX - RHMIN
        DELPAK = MAXPAK - MINPAK
        PROD = DELPAK/DELRH
        PLUS = REAL(MAXPAK) - PROD*RHMAX
C
        DO 10 I = 1,3
          LNBXYZ(I) = NBXYZ(I)
   10   CONTINUE
C
C---- Set up number of grid points/brick along uvw (NBUVW),
C     and number of bricks in map along uvw (LBUVW).
C     NUVW is equivalenced to NC,NR,NS, number of
C     grid points in map.
C     Allow for adjacent bricks sharing a common face.
C
        DO 20 I = 1,3
          L = MAPCRS(I)
          NBUVW(I) = NBXYZ(L)
          LBUVW(I) = (NUVW(I)+NBUVW(I)-3)/ (NBUVW(I)-1)
   20   CONTINUE
C
C---- Number of bricks
C
        NBRKS = LBUVW(1)*LBUVW(2)*LBUVW(3)
C
      END IF
C
C---- Format statements
C
 6000 FORMAT (//' MWHBRK: wrong mode, =',I5,', should be 10 !!!!',//)
C
C
      END
C
C
C
      SUBROUTINE MWRBRK(IUNIT,BRICK)
C     ==============================
C
C---- Write out one brick to unit IUNIT, after converting to bytes
C      Mode 10, 1byte/element
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER IUNIT
C     ..
C     .. Array Arguments ..
      REAL BRICK(*)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS,PLUS,PROD
      INTEGER ISPG,ITMHDR,ITMSC1,LSKFLG,MODE,NC,NC1,NCHITM,NLAB,NR,NR1,
     +        NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER JUNK,LABELS,LNBXYZ,LSTRM,MAPCRS,NXYZ
C     ..
C     .. Local Scalars ..
      INTEGER N
C     ..
C     .. External Subroutines ..
      EXTERNAL MRTOBY,QWRITE
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       PROD,PLUS,LNBXYZ(3),JUNK(12),ARMS,NLAB,LABELS(20,10),
     +       NCHITM,ITMHDR,ITMSC1
      COMMON /MSTRM/LSTRM(12)
C     ..
C
C
      N = LNBXYZ(1)*LNBXYZ(2)*LNBXYZ(3)
C
C---- Convert to bytes
C
      CALL MRTOBY(BRICK,BRICK,N,PROD,PLUS)
C
C---- Write out
C
      CALL QWRITE(LSTRM(IUNIT),BRICK,N)
C
C
      END
C
C
C
      SUBROUTINE MRHBRK(IUNIT,NBXYZ,NNBUVW,NLBUVW,NNBRKS)
C     ===================================================
C
C---- For mode 10, bricked map, get brick size,
C     packing parameters, and calculate number of bricks etc
C     ( see common /MBRKS/)
C
C---- This routine must be called before MRDHDR is called
C     for a different input file.
C
C IUNIT       stream number
C Returns:
C NBXYZ(3)    number of grid points/brick along xyz
C NNBUVW(3)   number of grid points/brick along uvw
C NLBUVW(3)   number of bricks in map along uvw
C             (allowing for adjacent
C             bricks sharing a common face).
C NNBRKS      total number of bricks in map
C
C
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
C     ..
C     .. Scalar Arguments ..
      INTEGER IUNIT,NNBRKS
C     ..
C     .. Array Arguments ..
      INTEGER NBXYZ(3),NLBUVW(3),NNBUVW(3)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS,PLUS,PROD
      INTEGER ISPG,LNGDIR,LNGVEC,LSKFLG,MODE,NC,NC1,NLAB,NPBRK,NR,NR1,
     +        NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN,UPLUS,UPROD
      INTEGER ITMHDR,ITMSC1,J1VEC,JSYMBT,JUNK,LABELS,LBUVW,LENDRS,
     +        LNBXYZ,LSTRM,MAPCRS,MODES,NBRKS,NBUVW,NC1S,NCHITM,NCS,
     +        NLEVS,NPLBRK,NR1S,NRS,NS1S,NSS,NXYZ
C     ..
C     .. Local Scalars ..
      INTEGER I,L
C     ..
C     .. Local Arrays ..
      INTEGER NUVW(3)
      CHARACTER CXYZ(3)*1
C     ..
C     .. Common blocks ..
      COMMON /MBRKS/NBUVW(3),LBUVW(3,12),NPBRK,NBRKS(12),NPLBRK(3,12),
     +       UPROD(12),UPLUS(12),NLEVS(12),LENDRS(12),J1VEC(12),LNGVEC,
     +       LNGDIR
      COMMON /MIHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       PROD,PLUS,LNBXYZ(3),JUNK(12),ARMS,NLAB,LABELS(20,10),
     +       NCS(12),NRS(12),NSS(12),MODES(12),NC1S(12),NR1S(12),
     +       NS1S(12),JSYMBT(12),NCHITM(12),ITMHDR(12),ITMSC1(12)
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (NC,NUVW(1)), (NR,NUVW(2)), (NS,NUVW(3))
C     ..
C     .. Data statements ..
      DATA CXYZ/'x','y','z'/
C     ..
C
C---- Check mode 10
C
      IF (MODE.NE.10) THEN
        WRITE (LUNOUT,FMT=6000) MODE
        STOP
      ELSE
C
        UPROD(IUNIT) = PROD
        UPLUS(IUNIT) = PLUS
        DO 10 I = 1,3
          NBXYZ(I) = LNBXYZ(I)
   10   CONTINUE
C
C---- Set up number of grid points/brick along uvw (NBUVW), and
C     number of bricks in map along uvw (LBUVW).
C     NUVW is equivalenced to NC,NR,NS, number of
C     grid points in map.
C     Allow for adjacent bricks sharing a common face.
C
        DO 20 I = 1,3
          L = MAPCRS(I)
          NBUVW(I) = NBXYZ(L)
          NNBUVW(I) = NBUVW(I)
          NLBUVW(I) = (NUVW(I)+NBUVW(I)-3)/ (NBUVW(I)-1)
          LBUVW(I,IUNIT) = NLBUVW(I)
   20   CONTINUE
C
C---- Number of points/brick
C
        NPBRK = NBUVW(1)*NBUVW(2)*NBUVW(3)
C
C---- Number of bricks
C
        NNBRKS = LBUVW(1,IUNIT)*LBUVW(2,IUNIT)*LBUVW(3,IUNIT)
        NBRKS(IUNIT) = NNBRKS
C
C---- Length of last brick in each direction in grid points
C
        DO 30 I = 1,3
          NPLBRK(I,IUNIT) = NUVW(I) - (LBUVW(I,IUNIT)-1)* (NBUVW(I)-1)
   30   CONTINUE
C
        WRITE (LUNOUT,FMT=6002) (CXYZ(MAPCRS(I)),I=1,3),NUVW,NBUVW,
     +    NLBUVW,NNBRKS
      END IF
C
C---- Format statements
C
 6000 FORMAT (//' MRHBRK: wrong mode, =',I5,', should be 10 !!!!',//)
 6002 FORMAT (/31X,'Axis   ',3 (6X,A),/12X,'Number of points in map   ',
     +       3I7,/13X,'Number of points/brick   ',3I7,/19X,'Number of ',
     +       'bricks   ',3I7,//' Total number of bricks = ',I7,//)
C
C
      END
C
C
C
      SUBROUTINE MRDBRK(IUNIT,BRICK,NBRICK,MUVW,IER)
C     ==============================================
C
C---- Mode 10, bricked map: read brick number
C     NBRICK from stream IUNIT.
C     Brick is converted to real before return.
C
C Returns:
C   MUVW(3)   Number of valid points in brick in each direction uvw
C             = brick size except for last plane of bricks in each
C             direction
C   IER         =0 OK
C               =1 brick NBRICK out of range of map
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER IER,IUNIT,NBRICK
C     ..
C     .. Array Arguments ..
      REAL BRICK(*)
      INTEGER MUVW(3)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS,PLUS,PROD
      INTEGER ISPG,LNGDIR,LNGVEC,LSKFLG,MODE,NC,NC1,NLAB,NPBRK,NR,NR1,
     +        NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN,UPLUS,UPROD
      INTEGER ITMHDR,ITMSC1,J1VEC,JSYMBT,JUNK,LABELS,LBUVW,LENDRS,
     +        LNBXYZ,LSTRM,MAPCRS,MODES,NBRKS,NBUVW,NC1S,NCHITM,NCS,
     +        NLEVS,NPLBRK,NR1S,NRS,NS1S,NSS,NXYZ
C     ..
C     .. Local Scalars ..
      INTEGER I,LBU,LBUV,LBV
C     ..
C     .. External Subroutines ..
      EXTERNAL MBYTOR,QREAD,QSEEK
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     ..
C     .. Common blocks ..
      COMMON /MBRKS/NBUVW(3),LBUVW(3,12),NPBRK,NBRKS(12),NPLBRK(3,12),
     +       UPROD(12),UPLUS(12),NLEVS(12),LENDRS(12),J1VEC(12),LNGVEC,
     +       LNGDIR
      COMMON /MIHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       PROD,PLUS,LNBXYZ(3),JUNK(12),ARMS,NLAB,LABELS(20,10),
     +       NCS(12),NRS(12),NSS(12),MODES(12),NC1S(12),NR1S(12),
     +       NS1S(12),JSYMBT(12),NCHITM(12),ITMHDR(12),ITMSC1(12)
      COMMON /MSTRM/LSTRM(12)
C     ..
C
C
      IF (NBRICK.LE.0 .OR. NBRICK.GT.NBRKS(IUNIT)) THEN
        IER = 1
      ELSE
C
C---- Position to start of brick (NPBRK bytes/brick)
C
        CALL QSEEK(LSTRM(IUNIT),NBRICK,ITMSC1(IUNIT),NPBRK)
C
C---- Read brick
C
        CALL QREAD(LSTRM(IUNIT),BRICK,NPBRK,IER)
        IF (IER.EQ.0) THEN
C
C---- Convert bytes to reals using packing parameters PROD & PLUS
C
          CALL MBYTOR(BRICK,BRICK,NPBRK,UPROD(IUNIT),UPLUS(IUNIT))
C
C---- For most bricks, number of valid grid points = brick size
C
          DO 10 I = 1,3
            MUVW(I) = NBUVW(I)
   10     CONTINUE
C
C---- Is this last layer of bricks in any direction ?
C
          LBU = LBUVW(1,IUNIT)
          LBV = LBUVW(2,IUNIT)
          LBUV = LBU*LBV
C
C---- along u
C
          IF (MOD(NBRICK,LBU).EQ.0) MUVW(1) = NPLBRK(1,IUNIT)
C
C---- along v
C
          I = MOD(NBRICK,LBUV)
          IF (I.EQ.0) I = LBUV
          IF ((I-1)/LBU.EQ.LBV-1) MUVW(2) = NPLBRK(2,IUNIT)
C
C---- along w
C
          IF (NBRICK.GT. (LBUV* (LBUVW(3,IUNIT)-1))) MUVW(3) = NPLBRK(3,
     +        IUNIT)
C
        END IF
      END IF
C
C
      END
C
C
C
      SUBROUTINE MWHVEC(IUNIT,NLEV,CONLEV,NBXYZ,NBUVW,LBUVW,NBRKS)
C     ============================================================
C
C---- Mode 11, vector map: calculate packing constants PROD and PLUS,
C     and add extra items to header block
C
C     Mode 12, ridge line map, treated the same
C
C  NB This routine should called after MWRHDL (MWRHDR) AND after
C      MSYCPY or MSYPUT etc, since it needs to know about any symmetry
C      operations in the file.
C
C    IUNIT        stream number (not used)
C
C    NLEV         number of contour levels (maximum 7)
C
C    CONLEV(NLEV) contour levels
C
C    NBXYZ(3)     number of grid points per brick along xyz
C
C Returns:
C    NBUVW(3)     number of grid points/brick along uvw
C    LBUVW(3)     number of bricks in map along uvw
C                 (allowing for adjacent
C                 bricks sharing a common face).
C    NBRKS        total number of bricks in map
C
C    MINPAK,MAXPAK   minimum and maximum packed vector coordinates
C
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
C     ..
C     .. Scalar Arguments ..
      INTEGER IUNIT,NBRKS,NLEV
C     ..
C     .. Array Arguments ..
      REAL CONLEV(NLEV)
      INTEGER LBUVW(3),NBUVW(3),NBXYZ(3)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS,PLUS,PROD
      INTEGER ISPG,ITMHDR,ITMSC1,J1VEC,LENDIR,LNGDIR,LNGVEC,LNLEV,
     +        LSKFLG,LVEC,MODE,NC,NC1,NCHITM,NDIR,NLAB,NR,NR1,NS,NS1,
     +        NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL ACNLEV,CEL,SKWMAT,SKWTRN
      INTEGER JUNK,LABELS,LNBXYZ,LSTRM,MAPCRS,NXYZ
C     ..
C     .. Local Scalars ..
      REAL VMAX
      INTEGER I,L,MAXPAK,MINPAK
C     ..
C     .. Local Arrays ..
      INTEGER NUVW(3)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MAX,MIN,REAL
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       PROD,PLUS,LNBXYZ(3),LNLEV,ACNLEV(7),LENDIR,JUNK(3),ARMS,
     +       NLAB,LABELS(20,10),NCHITM,ITMHDR,ITMSC1
      COMMON /MOVEC/LVEC,J1VEC,LNGVEC,NDIR,LNGDIR
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (NC,NUVW(1)), (NR,NUVW(2)), (NS,NUVW(3))
C     ..
C     .. Data statements ..
      DATA MINPAK,MAXPAK/-127,127/
C     ..
C
C---- Check mode 11 or 12
C
      IF (MODE.NE.11 .AND. MODE.NE.12) THEN
        WRITE (LUNOUT,FMT=6000) MODE
        STOP
      ELSE
C
C---- PROD, PLUS      packing parameters
C        packed vectors are relative to brick origin,
C        so minimum value = 0, maximum = max(NBXYZ(I=1,3)) - 1
C
        PLUS = MINPAK
        VMAX = MAX(NBXYZ(1),NBXYZ(2),NBXYZ(3)) - 1
        PROD = REAL(MAXPAK-MINPAK)/VMAX
C
C
        DO 10 I = 1,3
          LNBXYZ(I) = NBXYZ(I)
   10   CONTINUE
C
C---- Set up number of grid points/brick along uvw (NBUVW),
C     and number of bricks in map along uvw (LBUVW).
C     NUVW is equivalenced to NC,NR,NS, number of
C     grid points in map.
C     Allow for adjacent bricks sharing a common face.
C
        DO 20 I = 1,3
          L = MAPCRS(I)
          NBUVW(I) = NBXYZ(L)
          LBUVW(I) = (NUVW(I)+NBUVW(I)-3)/ (NBUVW(I)-1)
   20   CONTINUE
C
C---- Number of bricks
C
        NBRKS = LBUVW(1)*LBUVW(2)*LBUVW(3)
C
C---- Store contour levels
C
        LNLEV = MIN(NLEV,7)
        DO 30 I = 1,LNLEV
          ACNLEV(I) = CONLEV(I)
   30   CONTINUE
C
C---- Directory length, 2 entries / brick-element
C
        LENDIR = NLEV*NBRKS*2
C
C---- Clear vector total
C
        LVEC = 1
C
C---- Location in file of first vector
C
        J1VEC = ITMSC1 + LENDIR
C
C---- Length of each packed vector
C
        LNGVEC = 1
C
C---- Initialise directory count
C
        NDIR = 1
C
C---- Length of directory entry
C
        LNGDIR = 2
C
      END IF
C
C---- Format statements
C
 6000 FORMAT (//' MWHVEC: wrong mode, =',I5,', should be 11 or 12 !!!!',
     +       //)
C
C
      END
C
C
C
      SUBROUTINE MWRVEC(IUNIT,VECTOR,PSBITS,NVEC,JVEC)
C     ================================================
C
C---- Write out one brick of vectors for one contour level
C     to unit IUNIT, after packing.
C
C   Mode 11 or 12
C
C On entry:
C   VECTOR(3,NVEC)  vectors uvw (grid coordinates)
C                   (this array is destroyed)
C   PSBITS(NVEC)    logical array, for each vector .false.=move,
C                   .true.=draw
C   NVEC            number of vectors
C
C On return:
C   JVEC            vector number in file of first vector in this
C                   brick-element (for directory)
C
C
C
C
C---- Get number in file of first vector, and increment vector count
C
C     .. Scalar Arguments ..
      INTEGER IUNIT,JVEC,NVEC
C     ..
C     .. Array Arguments ..
      REAL VECTOR(3,*)
      LOGICAL PSBITS(*)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS,PLUS,PROD
      INTEGER ISPG,ITMHDR,ITMSC1,J1VEC,LENDIR,LNGDIR,LNGVEC,LNLEV,
     +        LSKFLG,LVEC,MODE,NC,NC1,NCHITM,NDIR,NLAB,NR,NR1,NS,NS1,
     +        NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL ACNLEV,CEL,SKWMAT,SKWTRN
      INTEGER JUNK,LABELS,LNBXYZ,LSTRM,MAPCRS,NXYZ
C     ..
C     .. External Subroutines ..
      EXTERNAL PAKVEC,QSEEK,QWRITE
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       PROD,PLUS,LNBXYZ(3),LNLEV,ACNLEV(7),LENDIR,JUNK(3),ARMS,
     +       NLAB,LABELS(20,10),NCHITM,ITMHDR,ITMSC1
      COMMON /MOVEC/LVEC,J1VEC,LNGVEC,NDIR,LNGDIR
      COMMON /MSTRM/LSTRM(12)
C     ..
C
C
      JVEC = LVEC
      IF (NVEC.GT.0) THEN
C
C---- Position file to this position
C
        CALL QSEEK(LSTRM(IUNIT),LVEC,J1VEC,LNGVEC)
C
C---- Pack vectors back into array VECTOR
C
        CALL PAKVEC(VECTOR,VECTOR,PSBITS,NVEC,PROD,PLUS)
C
C---- Write out
C
        CALL QWRITE(LSTRM(IUNIT),VECTOR,NVEC*LNGVEC)
C
        LVEC = LVEC + NVEC
      END IF
C
C
      END
C
C
C
      SUBROUTINE MWRDIR(IUNIT,JDIREC)
C     ===============================
C
C---- Mode 11 or 12, vector map: write out directory entries for all
C     contour levels for next brick
C
C
C
C---- Position file to next directory entry
C
C     .. Scalar Arguments ..
      INTEGER IUNIT
C     ..
C     .. Array Arguments ..
      INTEGER JDIREC(2,*)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS,PLUS,PROD
      INTEGER ISPG,ITMHDR,ITMSC1,J1VEC,LENDIR,LNGDIR,LNGVEC,LNLEV,
     +        LSKFLG,LVEC,MODE,NC,NC1,NCHITM,NDIR,NLAB,NR,NR1,NS,NS1,
     +        NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL ACNLEV,CEL,SKWMAT,SKWTRN
      INTEGER JUNK,LABELS,LNBXYZ,LSTRM,MAPCRS,NXYZ
C     ..
C     .. External Subroutines ..
      EXTERNAL QSEEK,QWRITE
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       PROD,PLUS,LNBXYZ(3),LNLEV,ACNLEV(7),LENDIR,JUNK(3),ARMS,
     +       NLAB,LABELS(20,10),NCHITM,ITMHDR,ITMSC1
      COMMON /MOVEC/LVEC,J1VEC,LNGVEC,NDIR,LNGDIR
      COMMON /MSTRM/LSTRM(12)
C     ..
C
C
      CALL QSEEK(LSTRM(IUNIT),NDIR,ITMSC1,LNGDIR)
C
C---- Write entries
C
      CALL QWRITE(LSTRM(IUNIT),JDIREC,LNGDIR*LNLEV)
C
C---- Increment directory count
C
      NDIR = NDIR + LNLEV
C
C
      END
C
C
C
      SUBROUTINE MRHVEC(IUNIT,NLEV,CONLEV,NBXYZ,NNBUVW,NLBUVW,NNBRKS)
C     ===============================================================
C
C---- For mode 11 or 12, vector map, get brick size,
C     packing parameters, and calculate number of bricks etc
C     ( see common /MBRKS/)
C
C---- This routine must be called before MRDHDR is called for
C     a different input file.
C
C IUNIT       stream number
C Returns:
C NLEV          number of contour levels
C CONLEV(NLEV)  contour levels
C NBXYZ(3)      number of grid points/brick along xyz
C NNBUVW(3)     number of grid points/brick along uvw
C NLBUVW(3)     number of bricks in map along uvw
C               (allowing for adjacent
C               bricks sharing a common face).
C NNBRKS        total number of bricks in map
C
C
C
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
C     ..
C     .. Scalar Arguments ..
      INTEGER IUNIT,NLEV,NNBRKS
C     ..
C     .. Array Arguments ..
      REAL CONLEV(*)
      INTEGER NBXYZ(3),NLBUVW(3),NNBUVW(3)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS,PLUS,PROD
      INTEGER ISPG,LENDIR,LNGDIR,LNGVEC,LNLEV,LSKFLG,MODE,NC,NC1,NLAB,
     +        NPBRK,NR,NR1,NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL ACNLEV,CEL,SKWMAT,SKWTRN,UPLUS,UPROD
      INTEGER ITMHDR,ITMSC1,J1VEC,JSYMBT,JUNK,LABELS,LBUVW,LENDRS,
     +        LNBXYZ,LSTRM,MAPCRS,MODES,NBRKS,NBUVW,NC1S,NCHITM,NCS,
     +        NLEVS,NPLBRK,NR1S,NRS,NS1S,NSS,NXYZ
C     ..
C     .. Local Scalars ..
      INTEGER I,L
C     ..
C     .. Local Arrays ..
      INTEGER NUVW(3)
      CHARACTER CXYZ(3)*1
C     ..
C     .. Common blocks ..
      COMMON /MBRKS/NBUVW(3),LBUVW(3,12),NPBRK,NBRKS(12),NPLBRK(3,12),
     +       UPROD(12),UPLUS(12),NLEVS(12),LENDRS(12),J1VEC(12),LNGVEC,
     +       LNGDIR
      COMMON /MIHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       PROD,PLUS,LNBXYZ(3),LNLEV,ACNLEV(7),LENDIR,JUNK(3),ARMS,
     +       NLAB,LABELS(20,10),NCS(12),NRS(12),NSS(12),MODES(12),
     +       NC1S(12),NR1S(12),NS1S(12),JSYMBT(12),NCHITM(12),
     +       ITMHDR(12),ITMSC1(12)
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (NC,NUVW(1)), (NR,NUVW(2)), (NS,NUVW(3))
C     ..
C     .. Data statements ..
      DATA CXYZ/'x','y','z'/
C     ..
C
C---- Check mode 11 or 12
C
      IF (MODE.NE.11 .AND. MODE.NE.12) THEN
        WRITE (LUNOUT,FMT=6000) MODE
        STOP
      ELSE
C
        UPROD(IUNIT) = PROD
        UPLUS(IUNIT) = PLUS
        DO 10 I = 1,3
          NBXYZ(I) = LNBXYZ(I)
   10   CONTINUE
C
C---- Set up number of grid points/brick along uvw (NBUVW),
C     and number of bricks in map along uvw (LBUVW).
C     NUVW is equivalenced to NC,NR,NS, number of
C     grid points in map.
C     Allow for adjacent bricks sharing a common face.
C
        DO 20 I = 1,3
          L = MAPCRS(I)
          NBUVW(I) = NBXYZ(L)
          NNBUVW(I) = NBUVW(I)
          NLBUVW(I) = (NUVW(I)+NBUVW(I)-3)/ (NBUVW(I)-1)
          LBUVW(I,IUNIT) = NLBUVW(I)
   20   CONTINUE
C
C---- Number of points/brick
C
        NPBRK = NBUVW(1)*NBUVW(2)*NBUVW(3)
C
C---- Number of bricks
C
        NNBRKS = LBUVW(1,IUNIT)*LBUVW(2,IUNIT)*LBUVW(3,IUNIT)
        NBRKS(IUNIT) = NNBRKS
C
C---- Length of last brick in each direction in grid points
C
        DO 30 I = 1,3
          NPLBRK(I,IUNIT) = NUVW(I) - (LBUVW(I,IUNIT)-1)* (NBUVW(I)-1)
   30   CONTINUE
C
        WRITE (LUNOUT,FMT=6002) (CXYZ(MAPCRS(I)),I=1,3),NUVW,NBUVW,
     +    NLBUVW,NNBRKS
C
C---- Directory length
C
        LENDRS(IUNIT) = LENDIR
C
C---- Store contour levels etc
C
        NLEV = LNLEV
        NLEVS(IUNIT) = NLEV
        DO 40 I = 1,LNLEV
          CONLEV(I) = ACNLEV(I)
   40   CONTINUE
        WRITE (LUNOUT,FMT=6004) NLEV, (CONLEV(I),I=1,NLEV)
C
C---- Length of directory element
C
        LNGDIR = 2
C
C---- Length of vector element
C
        LNGVEC = 1
C
C---- Start of vectors in file
C
        J1VEC(IUNIT) = ITMSC1(IUNIT) + LENDIR
C
      END IF
C
C---- Format statements
C
 6000 FORMAT (//' MRHVEC: wrong mode, =',I5,', should be 11 or 12 !!!!',
     +       //)
 6002 FORMAT (/31X,'Axis   ',3 (6X,A),/12X,'Number of points in map   ',
     +       3I7,/13X,'Number of points/brick   ',3I7,/19X,'Number of ',
     +       'bricks   ',3I7,//' Total number of bricks = ',I7,//)
 6004 FORMAT (/1X,I8,' contour levels: ',7F10.5,/)
C
C
      END
C
C
C
      SUBROUTINE MRDVEC(IUNIT,VECTOR,PSBITS,NVEC,NBRICK,LEVEL,IER)
C     ============================================================
C
C---- Mode 11 or 12, vector map: read vectors for
C     brick number NBRICK, contour level  LEVEL, from stream IUNIT.
C
C---- Reads directory from file to find brick-element,
C     then reads vectors Vectors are unpacked and corrected
C     to be relative to proper origin before return.
C
C Returns:
C   VECTOR(3,NVEC)  vectors uvw in grid coordinates
C   PSBITS(NVEC)    logical flags for vectors, .false. for move,
C                   .true. for draw
C   NVEC            number of vectors
C   IER             =0 OK
C                   =1 brick NBRICK out of range of map
C                   =-1 illegal contour level
C
C
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER IER,IUNIT,LEVEL,NBRICK,NVEC
C     ..
C     .. Array Arguments ..
      REAL VECTOR(3,*)
      LOGICAL PSBITS(*)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS,PLUS,PROD
      INTEGER ISPG,LENDIR,LNGDIR,LNGVEC,LNLEV,LSKFLG,MODE,NC,NC1,NLAB,
     +        NPBRK,NR,NR1,NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL ACNLEV,CEL,SKWMAT,SKWTRN,UPLUS,UPROD
      INTEGER ITMHDR,ITMSC1,J1VEC,JSYMBT,JUNK,LABELS,LBUVW,LENDRS,
     +        LNBXYZ,LSTRM,MAPCRS,MODES,NBRKS,NBUVW,NC1S,NCHITM,NCS,
     +        NLEVS,NPLBRK,NR1S,NRS,NS1S,NSS,NXYZ
C     ..
C     .. Local Arrays ..
      INTEGER IBRORG(3),JDIREC(2),LORUVW(3)
C     ..
C     .. External Subroutines ..
      EXTERNAL MBRORG,QREAD,QSEEK,UNPVEC
C     ..
C     .. Common blocks ..
      COMMON /MBRKS/NBUVW(3),LBUVW(3,12),NPBRK,NBRKS(12),NPLBRK(3,12),
     +       UPROD(12),UPLUS(12),NLEVS(12),LENDRS(12),J1VEC(12),LNGVEC,
     +       LNGDIR
      COMMON /MIHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       PROD,PLUS,LNBXYZ(3),LNLEV,ACNLEV(7),LENDIR,JUNK(3),ARMS,
     +       NLAB,LABELS(20,10),NCS(12),NRS(12),NSS(12),MODES(12),
     +       NC1S(12),NR1S(12),NS1S(12),JSYMBT(12),NCHITM(12),
     +       ITMHDR(12),ITMSC1(12)
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (LORUVW(1),NC1)
C     ..
C
C
      IER = 0
      IF (NBRICK.LE.0 .OR. NBRICK.GT.NBRKS(IUNIT)) THEN
        IER = 1
      ELSE IF (LEVEL.LE.0 .OR. LEVEL.GT.NLEVS(IUNIT)) THEN
        IER = -1
      ELSE
C
C---- Position to directory entry
C
        CALL QSEEK(LSTRM(IUNIT), (NBRICK-1)*NLEVS(IUNIT)+LEVEL,
     +             ITMSC1(IUNIT),LNGDIR)
C
C---- Read directory entry
C
        CALL QREAD(LSTRM(IUNIT),JDIREC,LNGDIR,IER)
        IF (IER.NE.0) IER = 1
C
C---- Read vectors
C
        NVEC = JDIREC(2)
C
C---- If no vectors in brick, return
C
        IF (NVEC.GT.0) THEN
C
C---- Position to vector list
C
          CALL QSEEK(LSTRM(IUNIT),JDIREC(1),J1VEC(IUNIT),LNGVEC)
C
C
          CALL QREAD(LSTRM(IUNIT),VECTOR,NVEC*LNGVEC,IER)
          IF (IER.NE.0) THEN
            IER = 1
          ELSE
C
C---- Get coordinate of brick origin
C
            CALL MBRORG(IBRORG,NBRICK,LBUVW(1,IUNIT),NBUVW,LORUVW)
C
C---- Unpack vectors
C
            CALL UNPVEC(VECTOR,PSBITS,VECTOR,NVEC,UPROD(IUNIT),
     +                  UPLUS(IUNIT),IBRORG)
C
          END IF
        END IF
      END IF
C
C
      END
C
C
C
      SUBROUTINE MBRORG(IBRORG,NBRICK,LBUVW,NBUVW,LORUVW)
C     ===================================================
C
C---- Calculate coordinates of brick origin IBRORG
C     for brick number NBRICK
C
C  LBUVW(3)   number of bricks in map along uvw
C  NBUVW(3)   number of grid points/brick along uvw
C  LORUVW(3)  map origin in grid coordinates
C
C
C---- Get brick coordinates KUVW from brick number
C
C     .. Scalar Arguments ..
      INTEGER NBRICK
C     ..
C     .. Array Arguments ..
      INTEGER IBRORG(3),LBUVW(3),LORUVW(3),NBUVW(3)
C     ..
C     .. Local Scalars ..
      INTEGER I,LBUV,N
C     ..
C     .. Local Arrays ..
      INTEGER KUVW(3)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     ..
C
C
      LBUV = LBUVW(1)*LBUVW(2)
      N = NBRICK - 1
      KUVW(1) = MOD(N,LBUVW(1))
      KUVW(3) = N/LBUV
      KUVW(2) = (N-KUVW(3)*LBUV)/LBUVW(1)
C
C---- Brick origin
C
      DO 10 I = 1,3
        IBRORG(I) = (NBUVW(I)-1)*KUVW(I) + LORUVW(I)
   10 CONTINUE
C
C
      END
C
C
C
      SUBROUTINE MRTOBY(BBRICK,RBRICK,N,PROD,PLUS)
C     ============================================
C
C---- Convert real brick RBRICK to byte brick BBRICK
C      VAX version
C
C
C     .. Scalar Arguments ..
      REAL PLUS,PROD
      INTEGER N
C     ..
C     .. Array Arguments ..
      REAL RBRICK(N)
      BYTE BBRICK(N)
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
C
C
      DO 10 I = 1,N
        BBRICK(I) = NINT(RBRICK(I)*PROD+PLUS)
   10 CONTINUE
C
C
      END
C
C
C
      SUBROUTINE MBYTOR(RBRICK,BBRICK,N,PROD,PLUS)
C     ============================================
C
C---- Convert byte brick to real
C     VAX version
C
C
C---- Work backwards to avoid overwriting
C
C     .. Scalar Arguments ..
      REAL PLUS,PROD
      INTEGER N
C     ..
C     .. Array Arguments ..
      REAL RBRICK(N)
      BYTE BBRICK(N)
C     ..
C     .. Local Scalars ..
      REAL B
      INTEGER I
C     ..
C
C
      DO 10 I = N,1,-1
        B = BBRICK(I)
        RBRICK(I) = (B-PLUS)/PROD
   10 CONTINUE
C
C
      END
C
C
C
      SUBROUTINE PAKVEC(PACKV,VECTOR,PSBITS,NVEC,PROD,PLUS)
C     =====================================================
C
C---- VAX
C
C---- Pack NVEC vectors from VECTOR/PSBITS into
C     PACKV byte array using PROD and PLUS
C
C
C     .. Scalar Arguments ..
      REAL PLUS,PROD
      INTEGER NVEC
C     ..
C     .. Array Arguments ..
      REAL VECTOR(3,NVEC)
      BYTE PACKV(4,NVEC)
      LOGICAL PSBITS(NVEC)
C     ..
C     .. Local Scalars ..
      INTEGER I,J
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
C
C
      DO 20 J = 1,NVEC
C
C---- Pack uvw coordinates
C
        DO 10 I = 1,3
          PACKV(I,J) = NINT(VECTOR(I,J)*PROD+PLUS)
   10   CONTINUE
C
C---- Pack flag
C
        PACKV(4,J) = PSBITS(J)
   20 CONTINUE
C
C
      END
C
C
C
      SUBROUTINE UNPVEC(VECTOR,PSBITS,PACKV,NVEC,PROD,PLUS,IBRORG)
C     ============================================================
C
C---- Unpack vector list and move/draw flag
C
C---- VAX  specific routine
C
C
C---- Float origin
C
C     .. Scalar Arguments ..
      REAL PLUS,PROD
      INTEGER NVEC
C     ..
C     .. Array Arguments ..
      REAL VECTOR(3,NVEC)
      INTEGER IBRORG(3)
      BYTE PACKV(4,NVEC)
      LOGICAL PSBITS(NVEC)
C     ..
C     .. Local Scalars ..
      REAL B
      INTEGER I,J
C     ..
C     .. Local Arrays ..
      REAL ORIG(3)
C     ..
C     .. External Functions ..
      LOGICAL VAXVMS
      EXTERNAL VAXVMS
C     ..
C
C
      DO 10 I = 1,3
        ORIG(I) = IBRORG(I)
   10 CONTINUE
C
C---- Unpack list backwards to avoid overwriting VAX
C
      IF (VAXVMS()) THEN
        DO 30 J = NVEC,1,-1
          PSBITS(J) = PACKV(4,J)
C
C---- Note that unpacking of bytes
C     backwards is dependent on the VAX order of
C     bytes in a word (byte 1 = ls byte)
C
          DO 20 I = 3,1,-1
            B = PACKV(I,J)
            VECTOR(I,J) = (B-PLUS)/PROD + ORIG(I)
   20     CONTINUE
   30   CONTINUE
      ELSE
C
C---- non-vms
C
        DO 50 J = 1,NVEC
          PSBITS(J) = PACKV(4,J)
          DO 40 I = 1,3
            B = PACKV(I,J)
            VECTOR(I,J) = (B-PLUS)/PROD + ORIG(I)
   40     CONTINUE
   50   CONTINUE
C
C
      END IF
C
C
      END
