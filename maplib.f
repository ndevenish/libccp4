C
C---- F77MSUB.FOR                               30/10/86    JWC
C
C
C
C
C---- Version 2.1  Fortran 77 version
C
C        TITLE is passed as character variable
C        Symmetry routines changed to Eleanor Dodson's SYMFR2
C        returns 4 x 4 matrices
C        Calls to QMODE so that subsequent Q... calls count
C        items rather than bytes. This complicates things,
C        since the header is counted in full words, the
C        symmetry operations in characters, and the map
C        in variable items, but it should make it more portabl
C        Phil Evans 2/5/84
C
C---- CCP4 VERSION      JOHN CAMPBELL,  JAN 1985
C
C    October 1985
C            added entry points MRFNAM, MWFNAM, routine MTTCPY  PRE
C    5/9/86  added routine MTTREP  replace output title after
C            MWRHDR/MTTCPY
C   30/10/86 Add missing argument in opening SYMOP using CCPOPN
C            in routine MSYPUT
C   30/9/86  Add rms level of map (from mean) to header, extra arguments
C            to routines MRDHDR and MCLOSE   P.Brick/PRE
C
C   12/8/87  Added subroutine MPOSNW, from EJD
C
C   5/12/89  Added subroutine MCLOSC, like MCLOSE except that 
C            arguments
C            RHMEAN & RHRMS are written out without change  PRE
C
C   24/2/92  new subroutine MRDHDS, like MRDHDR but with soft fail & 
C            print flag. s/r MRDHDR nor calls MRDHDS (Stefan Knight)
C
C   24/6/92  Remove calculation of max,min etc from mspew for modes
C            other than 2 (Peter Brick)
C
C   26/6/92  Only print max,min etc in s/r MCLOSE, MWCLOSE and MCLOSC
C            for mode 2.  Suppress printing of min,max etc in MRFNAM
C            for logical*1 maps (mode 0) (Peter Brick)
C
C
C---- EXTERNAL SUBROUTINES USED:
C     =========================
C
C     Subroutines for writing and reading map files using fixed-length
C     binary direct access routines DISKIO SUBROUTINES
C     (QOPEN,QCLOSE,QREAD,QWRITE,QSEEK,QBACK,QSKIP).
C
C              CCPLIB SUBROUTINES CCPBYT, CCPMDE, CCPMVB,
C              UXSUPPORT.C routines VAXVMS
C              VMSSUPPORT.FOR routines VAXVMS
C
C
C---- Assumes that a minimum of 4 characters may be packed
C     into a word for p
C     The title and symmetry information into the file header
C
C************ This file contains the following routines ***********
C
C
C      SUBROUTINE MWRHDR(IUNIT,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,NV1,NV2,
C      SUBROUTINE MWRHDL(IUNIT,MAPNAM,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,
C      SUBROUTINE MWRSEC(IUNIT,X,MU,MV,IU1,IU2,IV1,IV2)
C      SUBROUTINE MSPEW(IUNIT,X)                               
C      SUBROUTINE MCLOSE(IUNIT,RHMIN,RHMAX,RHMEAN,RHRMS)
C      SUBROUTINE MWCLOSE(IUNIT)
C      SUBROUTINE MCLOSC(IUNIT,RHMIN,RHMAX,RHMEAN,RHRMS)
C      SUBROUTINE MPOSNW(IUNIT,JSEC)
C      SUBROUTINE MRDHDR(IUNIT,MAPNAM,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,
C      SUBROUTINE MRDHDS(IUNIT,MAPNAM,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,
C      SUBROUTINE MPOSN(IUNIT,JSEC)
C      SUBROUTINE MRDLIN(IUNIT,X,IER)
C      SUBROUTINE MGULP(IUNIT,X,IER)
C      SUBROUTINE MGULPR(IUNIT,X,IER)
C      SUBROUTINE MRCLOS(IUNIT)
C      SUBROUTINE MSYPUT(IST,LSPGRP,IUNIT)
C      SUBROUTINE MSYMOP(IUNIT,NSYM,ROT)
C      SUBROUTINE MSYCPY(IN,IOUT)
C      SUBROUTINE MTTCPY(TITLE)
C      SUBROUTINE MTTREP(TITLE,NT)
C      SUBROUTINE MSKPUT(ASKWMT,ASKWTN)
C      SUBROUTINE MODECV(X,BLINE,N,MODE,JB)
C
C
C      INTEGER FUNCTION MSKGET(ASKWMT,ASKWTN)
C      INTEGER FUNCTION NBYTXX(NWORD)
C
C
C
C
C
      SUBROUTINE MWRHDR(IUNIT,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,NV1,NV2,
     +                  CELL,LSPGRP,LMODE)
C     =================================================================
C
C---- Put map header into common block /MOHDR/ and open map file on unit
C     IUNIT with logical name 'MAPOUT'
C
C The subroutines 'MWRHDR' and 'MWRHDL'
C ====================================
C 
C---- These subroutines are used to open an output map file and 
C     set up the header information. The actual header is only 
C     written to the file when the file is closed via the routine
C     MCLOSE.  The  only  difference  between  the  two subroutines 
C     is that MWRHDR does not have a parameter for the  logical  file
C     name for which a name of 'MAPOUT' is assumed.
C
C  Call:  CALL MWRHDR(IUNIT,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,
C        +            NV1,NV2,CELL,LSPGRP,LMODE)
C 
C  Call:  CALL MWRHDL(IUNIT,MAPNAM,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,
C        +            NV1,NV2,CELL,LSPGRP,LMODE)
C
C---- Parameters:
C     ==========
C
C  IUNIT (I)   Map stream number
C 
C  MAPNAM (I)   Logical  file  name  (type  CHARACTER)  
C               e.g.  'MAPOUT'   
C               (This parameter only present for MWRHDL)
C 
C  TITLE (I)   Map title (CHARACTER*80)
C 
C  NSEC (I)   Number of sections in the map
C 
C  IUVW (I)   3 word array with fast, medium, slow axes 
C             (1=X, 2=Y, 3=Z)
C 
C  MXYZ (I)   3 word array with sampling intervals along 
C             whole cell on X, Y, Z
C 
C   NW1 (I)   No. of first section
C 
C   NU1 (I)   Start of section on fast axis (grid units)
C 
C   NU2 (I)   End of section on fast axis
C 
C   NV1 (I)   Start of section on medium axis
C 
C   NV2 (I)   End of section on medium axis
C 
C   CELL (I)   6 word array for cell dimensions 
C              in Angstroms and degrees
C 
C   LSPGRP (I)   Space group number
C 
C   LMODE (I)   Map data mode =0, LOGICAL*1
C                             =1, INTEGER*2
C                             =2, REAL
C                             =3, COMPLEX INTEGER*2
C                             =4, COMPLEX REAL
C                             =5, Treated as mode 0
C                             =10, Bricked byte map
C 
C 
C
C
C
C                                            
C     .. Scalar Arguments ..
      INTEGER IUNIT,LMODE,LSPGRP,NSEC,NU1,NU2,NV1,NV2,NW1
      CHARACTER TITLE* (*)
C     ..
C     .. Array Arguments ..
      REAL CELL(6)
      INTEGER IUVW(3),MXYZ(3)
C     ..
C     .. External Subroutines ..
      EXTERNAL MWRHDL
C     ..
C
C
      CALL MWRHDL(IUNIT,'MAPOUT',TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,NV1,
     +            NV2,CELL,LSPGRP,LMODE)
C
C
      END
C
C
C
      SUBROUTINE MWRHDL(IUNIT,MAPNAM,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,
     +                  NV1,NV2,CELL,LSPGRP,LMODE)
C     ================================================================
C
C--- Put map header into common block /MOHDR/ and open map file on unit
C    IUNIT with logical name MAPNAM
C    NOTE that QOPEN returns an internal channel number to LSTRM(IUNIT)
C         which is used for subsequent reference to this stream
C
C
C     TITLE        80 character title for map (character)
C     NSEC         number of sections in map
C     IUVW(3)      fast,medium,slow axes (1,2,3 for x,y,z)
C     MXYZ(3)      sampling intervals along whole cell on x,y,z
C     NW1          first section number
C     NU1,NU2      limits on fast axis(grid units)
C     NV1,NV2      limits on medium axis
C     CELL(6)      cell dimensions, A and degrees
C     LSPGRP       space-group number
C     LMODE        map data mode =0 logical*1
C                                =1 integer*2
C                                =2 real*4
C                                =3 complex integer*2
C                                =4 complex real*4
C                                =5 reset to mode 0
C                                =10 byte bricked map
C                                =11 bricked vector map
C                                =12 bricked ridge-line map
C
C
C
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
C     ..
C     .. Scalar Arguments ..
      INTEGER IUNIT,LMODE,LSPGRP,NSEC,NU1,NU2,NV1,NV2,NW1
      CHARACTER FNAME* (*),MAPNAM* (*),TITLE* (*)
C     ..
C     .. Array Arguments ..
      REAL CELL(6)
      INTEGER IUVW(3),MXYZ(3)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS
      INTEGER ISPG,ITMHDR,ITMSC1,LSKFLG,MODE,NC,NC1,NCHITM,NLAB,NR,NR1,
     +        NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER JUNK,LABELS,LSTRM,MAPCRS,NXYZ
C     ..
C     .. Local Scalars ..
      INTEGER I,J,KMODE,NBHDR,NCHHDR,NFILSZ
      CHARACTER BLANK*4,FILE*255
C     ..
C     .. Local Arrays ..
      REAL HEADER(256)
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL QMODE,QQINQ,QOPEN,QWRITE
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCHITM,ITMHDR,ITMSC1
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (NC,HEADER(1))
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MOHDR/,FILE
C     ..
C     .. Data statements ..
C
C---- Number of items in header
C
      DATA NBHDR/256/,BLANK/'    '/
C     ..
C
C
C---- This routine is equivalent to old CCP sequential format
C       WRITE(IUNIT) TITLE,NSEC,IUVW,NXYZ
C
C
C---- Check valid IUNIT
C
      IF (IUNIT.LT.0 .OR. IUNIT.GT.12) THEN
C
C---- Error conditions
C
        WRITE (LUNOUT,FMT=6004)
        WRITE (LUNOUT,FMT=6006) IUNIT
        WRITE (LUNOUT,FMT=6008)
        CALL CCPERR(1, '**MAP FILE HANDLING ERROR**')
      ELSE
C
C---- Zero header and clear titles to space
C
        DO 10 I = 1,NBHDR
          HEADER(I) = 0
   10   CONTINUE
        AMIN =  99999999.0
        AMAX = -99999999.0
        AMEAN = 0.0
        ARMS  = 0.0
        DO 30 I = 1,20
          DO 20 J = 1,10
            READ (BLANK,FMT=6002) LABELS(I,J)
   20     CONTINUE
   30   CONTINUE
C
C---- Number of points in map
C
        NC = NU2 - NU1 + 1
        NR = NV2 - NV1 + 1
        NS = NSEC
C
C---- Default mode= real*4
C
        IF (LMODE.LT.0) LMODE = 2
C
C---- Reset mode 5 to mode 0
C
        IF (LMODE.EQ.5) LMODE = 0
        MODE = LMODE
C
C---- Start points
C
        NC1 = NU1
        NR1 = NV1
        NS1 = NW1
C
C---- Sampling
C
        DO 40 I = 1,3
          NXYZ(I) = MXYZ(I)
   40   CONTINUE
C
C---- Cell dimensions
C
        DO 50 I = 1,6
          CEL(I) = CELL(I)
   50   CONTINUE
C
C---- Axis order
C
        DO 60 I = 1,3
          MAPCRS(I) = IUVW(I)
   60   CONTINUE
C
C---- 1 title
C
        NLAB = 1
C
C---- Convert character title to integer variable
C
        READ (TITLE,FMT=6002) (LABELS(I,1),I=1,20)
C
C---- Space-group number
C
        ISPG = LSPGRP
C
C---- Open output file with logical name MAPNAM
C     returns internal channel number to
C     LSTRM(IUNIT) for future reference
C
        CALL QOPEN(LSTRM(IUNIT),MAPNAM,'NEW')
C
C---- Write dummy header to position file
C     (header really written in MCLOSE)
C      First set mode to 2 for header
C
        CALL QMODE(LSTRM(IUNIT),2,NCHHDR)
        CALL QWRITE(LSTRM(IUNIT),HEADER,NBHDR)
C
C---- Then reset mode to real mode,
C     changing modes 10 & 11(12) to 0 & 2
C
        KMODE = MODE
        IF (MODE.EQ.10) KMODE = 0
        IF (MODE.EQ.11 .OR. MODE.EQ.12) KMODE = 2
        CALL QMODE(LSTRM(IUNIT),KMODE,NCHITM)
C
C---- Set length of header in file items
C
        ITMHDR = NBHDR*NCHHDR/NCHITM
C
C---- and set first section position to same, pending symmetry
C
        ITMSC1 = ITMHDR + 1
C
C---- Get and print filename
C
        CALL QQINQ(LSTRM(IUNIT),MAPNAM,FILE(1:LENSTR(FILE)),NFILSZ)
        WRITE (LUNOUT,FMT=6000) IUNIT,FILE(1:LENSTR(FILE)),MAPNAM
C
      END IF
      RETURN
C
C
      ENTRY MWFNAM(FNAME)
C     ===================
C
C---- Returns filename from last file open,
C     must be called after MWRHDR
C
      FNAME = FILE
C
C---- Format statements
C
 6000 FORMAT (/'  File name for output map file on unit',I4,' : ',A,
     +       /'     logical name ',A,/)
 6002 FORMAT (20A4)
 6004 FORMAT (/' **MAP FILE HANDLING ERROR**')
 6006 FORMAT (/' **MWRHDL: UNIT NO. MUST BE 1 TO 12, =',I3,' **')
 6008 FORMAT (/' **PROGRAM TERMINATED**')
C
C
      END
C
C
C
      SUBROUTINE MWRSEC(IUNIT,X,MU,MV,IU1,IU2,IV1,IV2)
C     ================================================
C
C---- Write part of map section X(MU,MV) to stream IUNIT
C
C---- Equivalent to old CCP sequential format
C
C      WRITE(IUNIT) ((X(I,J),I=IU1,IU2),J=IV1,IV2)
C
C---- The subroutine 'MWRSEC'
C
C 
C---- subroutine is used to write a map section as 
C     part of an array  to  the map file.
C 
C  Call:  CALL MWRSEC(IUNIT,X,MU,MV,IU1,IU2,IV1,IV2)
C 
C---- Parameters:
C     ==========
C
C   IUNIT (I)   The map stream number
C 
C     X (I)   The array holding the map section
C 
C    MU (I)   The number of points along the whole fast axis
C 
C    MV (I)   The number of points along the whole medium axis
C 
C   IU1 (I)   The start array index along the fast axis
C 
C   IU2 (I)   The finish array index along the fast axis
C 
C   IV1 (I)   The start array index along the medium axis
C 
C   IV2 (I)   the finish array index along the medium axis
C 
C---- The elements written for a section may be described 
C     in FORTRAN notation  as
C
C            ((X(I,J),I=IU1,IU2),J=IV1,IV2).
C 
C
C
C
C
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
C     ..
C     .. Scalar Arguments ..
      INTEGER IU1,IU2,IUNIT,IV1,IV2,MU,MV
C     ..
C     .. Array Arguments ..
      REAL X(MU,MV)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS
      INTEGER ISPG,ITMHDR,ITMSC1,LSKFLG,MODE,NC,NC1,NCHITM,NLAB,NR,NR1,
     +        NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER JUNK,LABELS,LSTRM,MAPCRS,NXYZ
C     ..
C     .. Local Scalars ..
      INTEGER I,J,NCOLS
C     ..
C     .. External Subroutines ..
      EXTERNAL QWRITE
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCHITM,ITMHDR,ITMSC1
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MOHDR/
C     ..
C
C
      NCOLS = IU2 - IU1 + 1
      IF (MODE.NE.2) THEN
C
C---- Error condition
C
        WRITE (LUNOUT,FMT=6000) MODE
        CALL CCPERR(1, '**MAP FILE HANDLING ERROR**')
      ELSE
C
        DO 10 J = IV1,IV2
          CALL QWRITE(LSTRM(IUNIT),X(IU1,J),NCOLS)
   10   CONTINUE
C
C    Calculate AMEAN ARMS
        DO 20 J = IV1,IV2
        DO 30 I = IU1,IU2
        IF(X(I,J) .GT.AMAX) AMAX = X(I,J)
        IF(X(I,J) .LT.AMIN) AMIN = X(I,J)
        AMEAN = AMEAN + X(I,J)
        ARMS  = ARMS  + X(I,J)*X(I,J)
30      CONTINUE
20      CONTINUE
      END IF
C
C---- Format statements
C
 6000 FORMAT (/' **MAP FILE HANDLING ERROR**',//' **MWRSEC: MODE MUST ',
     +       'BE 2, =',I2,' **',//' **PROGRAM TERMINATED**')
C
C
      END
C
C
C
      SUBROUTINE MSPEW(IUNIT,X)
C     =========================
C
C---- Write whole section of map to stream IUNIT.
C     This routine is only suitable when the whole array is written
C
C   Equivalent to old CCP sequential format
C      WRITE(IUNIT) X
C
C---- The subroutine 'MSPEW'
C 
C---- This subroutine writes the next whole map section. 
C     The routine is used when the section occupies the 
C     complete  array.  The  data  are  written  without translation.
C 
C  Call:  CALL MSPEW(IUNIT,X)
C 
C---- Parameters:
C     ==========
C
C 
C  IUNIT (I)   Map stream number
C 
C     X (I)   Array holding the map section
C 
C
C
C     .. Scalar Arguments ..
      INTEGER IUNIT
C     ..
C     .. Array Arguments ..
      REAL X(*)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS
      INTEGER ISPG,ITMHDR,ITMSC1,LSKFLG,MODE,NC,NC1,NCHITM,NLAB,NR,NR1,
     +        NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER JUNK,LABELS,LSTRM,MAPCRS,NXYZ
C     ..
C     .. Local Scalars ..
      INTEGER N,J
C     ..
C     .. External Subroutines ..
      EXTERNAL QWRITE
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCHITM,ITMHDR,ITMSC1
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MOHDR/
C     ..
C
C
C---- Number of items
C
      N = NC*NR
C
      CALL QWRITE(LSTRM(IUNIT),X,N)
C
C    Calculate AMEAN ARMS - ONLY IF MAP IS REAL*4
C
        IF(MODE.EQ.2) THEN
          DO 20 J = 1,N
            IF(X(J) .GT.AMAX) AMAX = X(J)
            IF(X(J) .LT.AMIN) AMIN = X(J)
            AMEAN = AMEAN + X(J)
            ARMS  = ARMS  + X(J)*X(J)
20        CONTINUE
        ENDIF
C
      END
C
C
C
      SUBROUTINE MCLOSE(IUNIT,RHMIN,RHMAX,RHMEAN,RHRMS)
C     =================================================
C
C
C---- Write out header to map file on stream IUNIT, and close it
C
C---- The subroutine 'MCLOSE'
C     =======================
C 
C---- This subroutine writes the header records to 
C      the output map file and closes the file.
C 
C  Call:  CALL MCLOSE(IUNIT,RHMIN,RHMAX,RHMEAN,RHRMS)
C 
C---- Parameters:
C     ==========
C 
C  IUNIT (I)   The map stream number
C 
C  RHMIN (I)   The minimum density in the map
C 
C  RHMAX (I)   The maximum density in the map
C 
C  RHMEAN (I)   The sum of all the densities in the map 
C               (This will be  divided internally by the 
C                 number of points in the map to give the mean
C                 density which is then stored)
C 
C RHRMS  (I)   The sum of squares of the density values in the map 
C              (This will used internally to calculate the 
C               rms deviation from the mean value which is then stored.)
C
C
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
C     ..
C     .. Scalar Arguments ..
      REAL RHMAX,RHMEAN,RHMIN,RHRMS
      INTEGER IUNIT
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS
      INTEGER ISPG,ITMHDR,ITMSC1,LSKFLG,MODE,NC,NC1,NCHITM,NLAB,NR,NR1,
     +        NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER JUNK,LABELS,LSTRM,MAPCRS,NXYZ
C     ..
C     .. Local Scalars ..
      REAL T
      INTEGER NBHDR,NCHHDR
C     ..
C     .. Local Arrays ..
      REAL HEADER(256)
C     ..
C     .. External Subroutines ..
      EXTERNAL QCLOSE,QMODE,QSEEK,QWRITE           
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCHITM,ITMHDR,ITMSC1
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (NC,HEADER(1))
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MOHDR/
C     ..
C     .. Data statements ..
C
C---- Number of items in header
C
      DATA NBHDR/256/
C     ..
C
C---- Calculate mean
C
      T = NC*NR*NS
      AMEAN = RHMEAN/T
      ARMS = RHRMS/T - AMEAN*AMEAN
      IF (ARMS.GT.0.0) ARMS = SQRT(ARMS)
C
C---- Minimum & maximum
C
      AMIN = RHMIN
      AMAX = RHMAX
      IF(MODE.EQ.2) THEN
        WRITE (LUNOUT,FMT=6000) AMIN,AMAX,AMEAN,ARMS
      ENDIF
C
C---- Write to header, reset mode to 2 first
C
      CALL QMODE(LSTRM(IUNIT),2,NCHHDR)
      CALL QSEEK(LSTRM(IUNIT),1,1,1)
      CALL QWRITE(LSTRM(IUNIT),HEADER,NBHDR)
C
C---- Close file
C
      CALL QCLOSE(LSTRM(IUNIT))
C
C---- Format statements
C
 6000 FORMAT (/'   Minimum density in map  =',F15.5,'   Maximum densit',
     +       'y         =',F15.5,'   Mean density            =',F15.5,
     +       /'   Rms deviation from mean =',F15.5,/)
C
C
      END
C
C
C
C
      SUBROUTINE MWCLOSE(IUNIT)
C     =========================
C
C
C---- Write out header to map file on stream IUNIT, and close it
C
C---- The subroutine 'MWCLOSE'
C     =======================
C 
C---- This subroutine writes the header records to 
C      the output map file and closes the file.
C     The minimum, maximum, mean & rms densities are calculated 
C     from internal sums
C 
C  Call:  CALL MWCLOSE(IUNIT)
C 
C---- Parameters:
C     ==========
C 
C  IUNIT (I)   The map stream number
C 
C
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
C     ..
C     .. Scalar Arguments ..
      REAL RHMAX,RHMEAN,RHMIN,RHRMS
      INTEGER IUNIT
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS
      INTEGER ISPG,ITMHDR,ITMSC1,LSKFLG,MODE,NC,NC1,NCHITM,NLAB,NR,NR1,
     +        NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER JUNK,LABELS,LSTRM,MAPCRS,NXYZ
C     ..
C     .. Local Scalars ..
      REAL T
      INTEGER NBHDR,NCHHDR
C     ..
C     .. Local Arrays ..
      REAL HEADER(256)
C     ..
C     .. External Subroutines ..
      EXTERNAL QCLOSE,QMODE,QSEEK,QWRITE           
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCHITM,ITMHDR,ITMSC1
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (NC,HEADER(1))
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MOHDR/
C     ..
C     .. Data statements ..
C
C---- Number of items in header
C
      DATA NBHDR/256/
C     ..
C
C---- Calculate mean
C
      T = NC*NR*NS
      AMEAN = AMEAN/T
      ARMS = ARMS/T - AMEAN*AMEAN
      IF (ARMS.GT.0.0) ARMS = SQRT(ARMS)
C
C---- Minimum & maximum
C
      IF(MODE.EQ.2) THEN
        WRITE (LUNOUT,FMT=6000) AMIN,AMAX,AMEAN,ARMS
      ENDIF
C
C---- Write to header, reset mode to 2 first
C
      CALL QMODE(LSTRM(IUNIT),2,NCHHDR)
      CALL QSEEK(LSTRM(IUNIT),1,1,1)
      CALL QWRITE(LSTRM(IUNIT),HEADER,NBHDR)
C
C---- Close file
C
      CALL QCLOSE(LSTRM(IUNIT))
C
C---- Format statements
C
 6000 FORMAT (/'   Minimum density in map  =',F15.5,'   Maximum densit',
     +       'y         =',F15.5,'   Mean density            =',F15.5,
     +       /'   Rms deviation from mean =',F15.5,/)
C
C
      END
C
C
C
      SUBROUTINE MCLOSC(IUNIT,RHMIN,RHMAX,RHMEAN,RHRMS)
C     =================================================
C
C
C---- Write out header to map file on stream IUNIT, and close it
C     This routine is identical to MCLOSE except for arguments
C      RHMEAN, RHRMS
C
C---- It is more suitable than MCLOSE when a map file is being copied
C
C 
C---- This subroutine writes the header records to 
C      the output map file and closes the file.
C 
C  Call:  CALL MCLOSC(IUNIT,RHMIN,RHMAX,RHMEAN,RHRMS)
C 
C---- Parameters:
C     ==========
C 
C  IUNIT (I)   The map stream number
C 
C  RHMIN (I)   The minimum density in the map
C 
C  RHMAX (I)   The maximum density in the map
C 
C  RHMEAN (I)   The mean density in the map
C 
C  RHRMS  (I)   The rms deviation from the mean value in the map
C
C
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
C     ..
C     .. Scalar Arguments ..
      REAL RHMAX,RHMEAN,RHMIN,RHRMS
      INTEGER IUNIT
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS
      INTEGER ISPG,ITMHDR,ITMSC1,LSKFLG,MODE,NC,NC1,NCHITM,NLAB,NR,NR1,
     +        NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER JUNK,LABELS,LSTRM,MAPCRS,NXYZ
C     ..
C     .. Local Scalars ..
      REAL T
      INTEGER NBHDR,NCHHDR
C     ..
C     .. Local Arrays ..
      REAL HEADER(256)
C     ..
C     .. External Subroutines ..
      EXTERNAL QCLOSE,QMODE,QSEEK,QWRITE           
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCHITM,ITMHDR,ITMSC1
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (NC,HEADER(1))
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MOHDR/
C     ..
C     .. Data statements ..
C
C---- Number of items in header
C
      DATA NBHDR/256/
C     ..
C
C
      AMEAN = RHMEAN
      ARMS = RHRMS
C
C---- Minimum & maximum
C
      AMIN = RHMIN
      AMAX = RHMAX
      IF(MODE.EQ.2) THEN
        WRITE (LUNOUT,FMT=6000) AMIN,AMAX,AMEAN,ARMS
      ENDIF
C
C---- Write to header, reset mode to 2 first
C
      CALL QMODE(LSTRM(IUNIT),2,NCHHDR)
      CALL QSEEK(LSTRM(IUNIT),1,1,1)
      CALL QWRITE(LSTRM(IUNIT),HEADER,NBHDR)
C
C---- Close file
C
      CALL QCLOSE(LSTRM(IUNIT))
C
C---- Format statements
C
 6000 FORMAT (/'   Minimum density in map  =',F15.5,'   Maximum densit',
     +       'y         =',F15.5,'   Mean density            =',F15.5,
     +       /'   Rms deviation from mean =',F15.5,/)
C
C
      END







C
C
C
      SUBROUTINE MPOSNW(IUNIT,JSEC)
C     ============================
C
C---- Position output map before section JSEC
C
C---- The subroutine 'MPOSNW'
C     ====================== 
C
C---- This subroutine is used to set the position in the map  
C     file  so  that  the next section to be written is section JSEC.
C 
C  Call:  CALL MPOSNW(IUNIT,JSEC)
C 
C---- Parameters:
C     ==========
C
C   IUNIT (I)   Map stream number
C 
C   JSEC (I)   Position the output map before section JSEC
C 
C
C
C     .. Scalar Arguments ..   
      INTEGER IUNIT,JSEC
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS
      INTEGER ISPG,ITMHDR,ITMSC1,LSKFLG,MODE,NC,NC1,NCHITM,NLAB,NR,NR1,
     +        NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER JUNK,LABELS,LSTRM,MAPCRS,NXYZ
C     ..
C     .. Local Scalars ..
      INTEGER LSEC,NBHDR,NREC
C     ..
C     .. External Subroutines ..
      EXTERNAL QSEEK
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCHITM,ITMHDR,ITMSC1
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MOHDR/
C     ..
C     .. Data statements ..
      DATA NBHDR/256/
C     ..
C
C---- Section length in items
C
      LSEC = NC*NR
C
C---- Record number
C
      NREC = JSEC - NS1 + 1
C
      CALL QSEEK(LSTRM(IUNIT),NREC,ITMSC1,LSEC)
C
C
      END
C
C
C
      SUBROUTINE MRDHDR(IUNIT,MAPNAM,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,
     +                  NV1,NV2,CELL,LSPGRP,LMODE,RHMIN,RHMAX,RHMEAN,
     +                  RHRMS)
C     ================================================================
C
C---- Read map header from stream IUNIT, logical name in MAPNAM by call
C     to MRDHDS

C     Map header common
C
C---- Returns:
C
C  TITLE        80 character title for map (character)
C  NSEC         number of sections in map
C  IUVW(3)      fast,medium,slow axes (1,2,3 for x,y,z)
C  MXYZ(3)      sampling intervals along whole cell on x,y,z
C  NW1          first section number
C  NU1,NU2      limits on fast axis(grid units)
C  NV1,NV2      limits on medium axis
C  CELL(6)      cell dimensions, A and degrees
C  LSPGRP       space-group number
C  LMODE        map data mode =0 logical*1
C                             =1 integer*2
C                             =2 real*4
C                             =3 complex integer*2
C                             =4 complex real*4
C                             =5 treated as mode 0
C                             =10 bricked byte map
C  RHMIN,RHMAX  minimum, maximum density
C  RHMEAN       mean density
C  RHRMS        rms deviation from mean density
C
C
C
C
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
C     ..
C     .. Scalar Arguments ..
      REAL RHMAX,RHMEAN,RHMIN,RHRMS
      INTEGER IUNIT,LMODE,LSPGRP,NSEC,NU1,NU2,NV1,NV2,NW1
      CHARACTER MAPNAM* (*),TITLE* (*)
C     ..
C     .. Array Arguments ..
      REAL CELL(6)
      INTEGER IUVW(3),MXYZ(3)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS
      INTEGER ISPG,LSKFLG,MODE,NC,NC1,NLAB,NR,NR1,NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER ITMHDR,ITMSC1,JSYMBT,JUNK,LABELS,LSTRM,MAPCRS,MODES,NC1S,
     +        NCHITM,NCS,NR1S,NRS,NS1S,NSS,NXYZ
C     ..
C     .. Local Scalars ..
      INTEGER I,IER,J,KMODE,NBHDR,NCHHDR,NFILSZ,NW2
      CHARACTER FILE*255

C     ..
C     .. Error and print control ..
      INTEGER IFAIL
      INTEGER IPRINT

C     ..
C     .. Local Arrays ..
      REAL HEADER(256)
      CHARACTER LXYZ(3)*1
C     ..
C     .. External Subroutines ..
      EXTERNAL QMODE,QQINQ,QOPEN,QREAD,QSEEK,LENSTR
      INTEGER LENSTR
C     ..
C     .. Common blocks ..
      COMMON /MIHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCS(12),NRS(12),NSS(12),
     +       MODES(12),NC1S(12),NR1S(12),NS1S(12),JSYMBT(12),NCHITM(12),
     +       ITMHDR(12),ITMSC1(12)
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (NC,HEADER(1))

C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MIHDR/,FILE
C     ..
C     .. Data statements ..
      DATA NBHDR/256/
      DATA LXYZ/'X','Y','Z'/

      IFAIL  = 0
      IPRINT = 1

      CALL MRDHDS(IUNIT,MAPNAM,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,
     +                  NV1,NV2,CELL,LSPGRP,LMODE,RHMIN,RHMAX,RHMEAN,
     +                  RHRMS,IFAIL,IPRINT)

      RETURN
      END
C
C
C
      SUBROUTINE MRDHDS(IUNIT,MAPNAM,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,
     +                  NV1,NV2,CELL,LSPGRP,LMODE,RHMIN,RHMAX,RHMEAN,
     +                  RHRMS,IFAIL,IPRINT)
C     ================================================================
C
C---- Read map header from stream IUNIT, logical name in MAPNAM
C     Map header common
C
C---- Returns:
C
C  TITLE        80 character title for map (character)
C  NSEC         number of sections in map
C  IUVW(3)      fast,medium,slow axes (1,2,3 for x,y,z)
C  MXYZ(3)      sampling intervals along whole cell on x,y,z
C  NW1          first section number
C  NU1,NU2      limits on fast axis(grid units)
C  NV1,NV2      limits on medium axis
C  CELL(6)      cell dimensions, A and degrees
C  LSPGRP       space-group number
C  LMODE        map data mode =0 logical*1
C                             =1 integer*2
C                             =2 real*4
C                             =3 complex integer*2
C                             =4 complex real*4
C                             =5 treated as mode 0
C                             =10 bricked byte map
C  RHMIN,RHMAX  minimum, maximum density
C  RHMEAN       mean density
C  RHRMS        rms deviation from mean density
C  IFAIL (I/O)  ON INPUT:     =0, STOP ON ERROR
C                             =1, RETURN ON ERROR
C               ON OUTPUT:    UNCHANGED IF NO ERROR
C                             =-1, ERROR 
C  IPRINT                     = 0; silent
C                             .ne. 0; print file name, header info etc
C
C
C
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
C     ..
C     .. Scalar Arguments ..
      REAL RHMAX,RHMEAN,RHMIN,RHRMS
      INTEGER IUNIT,LMODE,LSPGRP,NSEC,NU1,NU2,NV1,NV2,NW1
      CHARACTER FNAME* (*),MAPNAM* (*),TITLE* (*)
C     ..
C     .. Array Arguments ..
      REAL CELL(6)
      INTEGER IUVW(3),MXYZ(3)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS
      INTEGER ISPG,LSKFLG,MODE,NC,NC1,NLAB,NR,NR1,NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER ITMHDR,ITMSC1,JSYMBT,JUNK,LABELS,LSTRM,MAPCRS,MODES,NC1S,
     +        NCHITM,NCS,NR1S,NRS,NS1S,NSS,NXYZ
C     ..
C     .. Local Scalars ..
      INTEGER I,IER,J,KMODE,NBHDR,NCHHDR,NFILSZ,NW2
      CHARACTER FILE*255

C     ..
C     .. Error and print control ..
      INTEGER IFAIL
      INTEGER IPRINT

C     ..
C     .. Local Arrays ..
      REAL HEADER(256)
      CHARACTER LXYZ(3)*1
C     ..
C     .. External Subroutines ..
      EXTERNAL QMODE,QQINQ,QOPEN,QREAD,QSEEK,LENSTR
      INTEGER LENSTR
      LOGICAL CCPEXS
C     ..
C     .. Common blocks ..
      COMMON /MIHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCS(12),NRS(12),NSS(12),
     +       MODES(12),NC1S(12),NR1S(12),NS1S(12),JSYMBT(12),NCHITM(12),
     +       ITMHDR(12),ITMSC1(12)
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (NC,HEADER(1))
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MIHDR/,FILE
C     ..
C     .. Data statements ..
      DATA NBHDR/256/
      DATA LXYZ/'X','Y','Z'/
C     ..
C
C
      TITLE = ' '

C     Check file exists
      IF ( .NOT. CCPEXS ( MAPNAM ) ) THEN
        IF ( IFAIL .EQ. 0 ) THEN
          WRITE(LUNOUT,FMT=6100)MAPNAM( :LENSTR(MAPNAM))
          WRITE(LUNOUT,FMT=6014)
          CALL CCPERR(1, '**MAP FILE HANDLING ERROR**')
        ELSE
          IFAIL = -1
          RETURN
        ENDIF
      ENDIF

C
C---- Check valid IUNIT
C

      IF (IUNIT.LT.0 .OR. IUNIT.GT.12) THEN
C
C---- Error conditions
C
        WRITE (LUNOUT,FMT=6008)
        WRITE (LUNOUT,FMT=6010) IUNIT
      ELSE
C
C---- Open file
C
        CALL QOPEN(LSTRM(IUNIT),MAPNAM,'RO')
C
C---- Get and print file name
C
        IF ( IPRINT .NE. 0 ) THEN
          CALL QQINQ(LSTRM(IUNIT),MAPNAM,FILE,NFILSZ)
          IF (NFILSZ.GE.0) WRITE (LUNOUT,FMT=6000) IUNIT,
     +                          FILE(1:LENSTR(FILE)),NFILSZ,MAPNAM
          IF (NFILSZ.LT.0) WRITE (LUNOUT,FMT=6002) IUNIT,
     +                          FILE(1:LENSTR(FILE)),MAPNAM
        ENDIF
C
        CALL QMODE(LSTRM(IUNIT),2,NCHHDR)
        CALL QREAD(LSTRM(IUNIT),HEADER,NBHDR,IER)
        IF (IER.NE.0) THEN
          WRITE (LUNOUT,FMT=6008)
          WRITE (LUNOUT,FMT=6012)
        ELSE
C
C---- Change mode 5 to mode 0
C
          IF (MODE.EQ.5) MODE = 0
C
C---- Set correct mode, changing 10 & 11(12) to 0 & 2
C
          KMODE = MODE
          IF (MODE.EQ.10) KMODE = 0
          IF (MODE.EQ.11 .OR. MODE.EQ.12) KMODE = 2
          CALL QMODE(LSTRM(IUNIT),KMODE,NCHITM(IUNIT))
C
          NU1 = NC1
          NV1 = NR1
          NW1 = NS1
          NSEC = NS
          NU2 = NU1 + NC - 1
          NV2 = NV1 + NR - 1
          NW2 = NW1 + NSEC - 1
C
C---- Write out header information
C
          IF ( IPRINT .NE. 0 ) THEN
            WRITE (LUNOUT,FMT=6003) NC,NR,NS,MODE,NU1,NU2,NV1,NV2,NW1,
     +      NW2,NXYZ,CEL, (LXYZ(MAPCRS(I)),I=1,3)
            IF(MODE.NE.0) WRITE (LUNOUT,FMT=6004) AMIN,AMAX,AMEAN,ARMS
            WRITE (LUNOUT,FMT=6005) ISPG,NLAB,
     +            ((LABELS(I,J),I=1,20),J=1,NLAB)
          ENDIF

C---- Copy header information for return to calling routine
C
C---- Convert integer title to characters
C
          WRITE (TITLE,FMT=6006) (LABELS(I,1),I=1,20)
C
          DO 10 I = 1,3
            IUVW(I) = MAPCRS(I)
   10     CONTINUE
          DO 20 I = 1,3
            MXYZ(I) = NXYZ(I)
   20     CONTINUE
          DO 30 I = 1,6
            CELL(I) = CEL(I)
   30     CONTINUE
          LSPGRP = ISPG
          LMODE = MODE
          MODES(IUNIT) = MODE
          NCS(IUNIT) = NC
          NRS(IUNIT) = NR
          NSS(IUNIT) = NS
          NC1S(IUNIT) = NC1
          NR1S(IUNIT) = NR1
          NS1S(IUNIT) = NS1
          IF (ISPG.EQ.0) NSYMBT = 0
          JSYMBT(IUNIT) = NSYMBT
          RHMIN = AMIN
          RHMAX = AMAX
          RHMEAN = AMEAN
          RHRMS = ARMS
C
C---- Get length of header in items
C
          ITMHDR(IUNIT) = NBHDR*NCHHDR/NCHITM(IUNIT)
C
C---- and position of first section
C
          ITMSC1(IUNIT) = NSYMBT/NCHITM(IUNIT) + ITMHDR(IUNIT) + 1
C
C---- Position to 1st section
C
          CALL QSEEK(LSTRM(IUNIT),1,ITMSC1(IUNIT),1)
C
          RETURN
        END IF
      END IF

      WRITE (LUNOUT,FMT=6014)
      IF ( IFAIL .EQ. 0 ) THEN
        CALL CCPERR(1, '**MAP FILE HANDLING ERROR**')
      ELSE
        IFAIL = -1
        RETURN
      ENDIF

C
C
C
      ENTRY MRFNAM(FNAME)
C     ===================
C
C---- Returns file name from last file open,
C     must be called after MRDHDR
C
      FNAME = FILE
C
C---- Format statements
C
 6000 FORMAT (/'  File name for input map file on unit',I4,' : ',A,/31X,
     +       'file size =',I8,'  ;  logical name  ',A,/)
 6002 FORMAT (/' File name for input map file on unit',I4,' : ',A,/31X,
     +       'Logical name  ',A,/)
 6003 FORMAT (/11X,'Number of columns, rows, sections ',15 ('.'),3I5,
     +       /11X,'Map mode ',40 ('.'),I5,/11X,'Start and stop points ',
     +       'on columns, rows, sections ',6I5,/11X,'Grid sampling on ',
     +       'x, y, z ',24 ('.'),3I5,/11X,'Cell dimensions ',33 ('.'),
     +       6F10.5,/11X,'Fast, medium, slow axes ',25 ('.'),3 (4X,A1))
 6004 FORMAT (11X,'Minimum density ',33 ('.'),F12.5,/11X,'Maximum dens',
     +       'ity ',33 ('.'),F12.5,/11X,'Mean density ',36 ('.'),F12.5,
     +       /11X,'Rms deviation from mean density ',17 ('.'),F12.5)
 6005 FORMAT (11X,'Space-group ',37 ('.'),I5,/11X,'Number of titles ',
     +       32 ('.'),I5,//' Titles :',/10 (11X,20A4,/))
 6006 FORMAT (20A4)
 6008 FORMAT (/' **MAP FILE HANDLING ERROR**')
 6010 FORMAT (/' **MRDHDR: UNIT NO. MUST BE 1 TO 12, =',I3,' **')
 6012 FORMAT (/' **MRDHDR: READ ERROR ON MAP HEADER**')
 6014 FORMAT (/' **PROGRAM TERMINATED**')
 6100 FORMAT (/' **FILE DOES NOT EXIST > ' , A )
C
C
      END
C
C
C
      SUBROUTINE MPOSN(IUNIT,JSEC)
C     ============================
C
C---- Position input map before section JSEC
C
C---- The subroutine 'MPOSN'
C 
C---- This subroutine is used to set the position in the map  
C     file  so  that  the next section to be read is section JSEC.
C 
C Call:  CALL MPOSN(IUNIT,JSEC)
C 
C---- Parameters:
C     ==========
C 
C  IUNIT (I)   Map stream number
C 
C  JSEC (I)   Position the input map before section JSEC
C 
C
C
C     .. Scalar Arguments ..
      INTEGER IUNIT,JSEC
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS
      INTEGER ISPG,LSKFLG,MODE,NC,NC1,NLAB,NR,NR1,NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER ITMHDR,ITMSC1,JSYMBT,JUNK,LABELS,LSTRM,MAPCRS,MODES,NC1S,
     +        NCHITM,NCS,NR1S,NRS,NS1S,NSS,NXYZ
C     ..
C     .. Local Scalars ..
      INTEGER LSEC,NBHDR,NREC
C     ..
C     .. External Subroutines ..
      EXTERNAL QSEEK
C     ..
C     .. Common blocks ..
      COMMON /MIHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCS(12),NRS(12),NSS(12),
     +       MODES(12),NC1S(12),NR1S(12),NS1S(12),JSYMBT(12),NCHITM(12),
     +       ITMHDR(12),ITMSC1(12)
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MIHDR/
C     ..
C     .. Data statements ..
      DATA NBHDR/256/
C     ..
C
C---- Section length in items
C
      LSEC = NCS(IUNIT)*NRS(IUNIT)
C
C---- Record number
C
      NREC = JSEC - NS1S(IUNIT) + 1
C
      CALL QSEEK(LSTRM(IUNIT),NREC,ITMSC1(IUNIT),LSEC)
C
C
      END
C
C
C
      SUBROUTINE MRDLIN(IUNIT,X,IER)
C     =============================
C
C---- Read next line of map from stream IUNIT to array X.
C     Map is returned in same mode as on file
C
C---- Returns IER = 0  OK
C            .ne. 0  error (end of file)
C
C---- The subroutine 'MRDLIN'
C
C---- Read the next line from an input map file. 
C     The data  are  returned  in  the
C     same form as that stored in the map.
C 
C  Call:  CALL MRDLIN(IUNIT,X,IER)
C 
C---- Parameters:
C     ==========
C 
C  IUNIT (I)   Map stream number
C 
C     X (O)   Array to contain the line of data read from the map
C 
C   IER (O)   Error flag =0, OK   non-zero, error or end of file
C 
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER IER,IUNIT
C     ..
C     .. Array Arguments ..
      REAL X(*)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS
      INTEGER ISPG,LSKFLG,MODE,NC,NC1,NLAB,NR,NR1,NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER ITMHDR,ITMSC1,JSYMBT,JUNK,LABELS,LSTRM,MAPCRS,MODES,NC1S,
     +        NCHITM,NCS,NR1S,NRS,NS1S,NSS,NXYZ
C     ..
C     .. Local Scalars ..
      INTEGER MB
C     ..
C     .. External Subroutines ..
      EXTERNAL QREAD
C     ..
C     .. Common blocks ..
      COMMON /MIHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCS(12),NRS(12),NSS(12),
     +       MODES(12),NC1S(12),NR1S(12),NS1S(12),JSYMBT(12),NCHITM(12),
     +       ITMHDR(12),ITMSC1(12)
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MIHDR/
C     ..
C
C
C---- Size of section (elements)
C
      MB = NCS(IUNIT)
      CALL QREAD(LSTRM(IUNIT),X,MB,IER)
C
C
      END
C
C
C
      SUBROUTINE MGULP(IUNIT,X,IER)
C     =============================
C
C---- Read next whole map section from stream IUNIT to array X.
C     Map is returned in same mode as on file
C
C Returns IER = 0  OK
C            .ne. 0  error (end of file)
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER IER,IUNIT
C     ..
C     .. Array Arguments ..
      REAL X(*)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS
      INTEGER ISPG,LSKFLG,MODE,NC,NC1,NLAB,NR,NR1,NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER ITMHDR,ITMSC1,JSYMBT,JUNK,LABELS,LSTRM,MAPCRS,MODES,NC1S,
     +        NCHITM,NCS,NR1S,NRS,NS1S,NSS,NXYZ
C     ..
C     .. Local Scalars ..
      INTEGER MB
C     ..
C     .. External Subroutines ..
      EXTERNAL QREAD
C     ..
C     .. Common blocks ..
      COMMON /MIHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCS(12),NRS(12),NSS(12),
     +       MODES(12),NC1S(12),NR1S(12),NS1S(12),JSYMBT(12),NCHITM(12),
     +       ITMHDR(12),ITMSC1(12)
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MIHDR/
C     ..
C
C---- Size of section (elements)
C
      MB = NCS(IUNIT)*NRS(IUNIT)
      CALL QREAD(LSTRM(IUNIT),X,MB,IER)
C
C
      END
C
C
C
      SUBROUTINE MGULPR(IUNIT,X,IER)
C     =============================
C
C---- Read next whole map section from stream IUNIT to array X.
C     For map modes other than 2, array is converted to real;
C     for complex maps (MODE = 3 or 4) the complex amplitude is
C     returned.
C
C Returns IER = 0  OK
C            .ne. 0  error (end of file)
C
C---- Map header common
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER IER,IUNIT
C     ..
C     .. Array Arguments ..
      REAL X(*)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS
      INTEGER ISPG,LSKFLG,MODE,NC,NC1,NLAB,NR,NR1,NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER ITMHDR,ITMSC1,JSYMBT,JUNK,LABELS,LSTRM,MAPCRS,MODES,NC1S,
     +        NCHITM,NCS,NR1S,NRS,NS1S,NSS,NXYZ
C     ..
C     .. Local Scalars ..
      INTEGER J,JB,M,MODEE,N,NRL
C     ..
C     .. Local Arrays ..
      REAL RLINE(500)
C     ..
C     .. External Functions ..
      INTEGER NBYTXX
      EXTERNAL NBYTXX
C     ..
C     .. External Subroutines ..
      EXTERNAL MODECV,QREAD
C     ..
C     .. Common blocks ..
      COMMON /MIHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCS(12),NRS(12),NSS(12),
     +       MODES(12),NC1S(12),NR1S(12),NS1S(12),JSYMBT(12),NCHITM(12),
     +       ITMHDR(12),ITMSC1(12)
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MIHDR/
C     ..
C
C
      NRL = NBYTXX(500)
C
C---- Size of section (elements)
C
      M = NCS(IUNIT)*NRS(IUNIT)
      MODEE = MODES(IUNIT)
      JB = NCHITM(IUNIT)
      IF (MODEE.NE.2) THEN
C
C---- Conversion required, read section in chunks of NRL characters
C
        J = 1
   10   CONTINUE
C
C---- Number of items N
C
        N = NRL/NCHITM(IUNIT)
        IF (J+N-1.GT.M) N = M - J + 1
C
        CALL QREAD(LSTRM(IUNIT),RLINE,N,IER)
        IF (IER.NE.0) THEN
          RETURN
        ELSE
C
C---- We need to convert N elements from the file in RLINE
C     to real numbers in X. Subroutine MODECV is machine specific
C
          CALL MODECV(X(J),RLINE,N,MODEE,JB)
C
          J = J + N
          IF (J.LT.M) GO TO 10
        END IF
C
      ELSE
C
C---- Mode real, just read
C
        CALL QREAD(LSTRM(IUNIT),X,M,IER)
      END IF
C
C
      END
C
C
C
      SUBROUTINE MRCLOS(IUNIT)
C     ========================
C
C---- Close read file
C
C
C
C     .. Scalar Arguments ..
      INTEGER IUNIT
C     ..
C     .. Arrays in Common ..
      INTEGER LSTRM
C     ..
C     .. External Subroutines ..
      EXTERNAL QCLOSE
C     ..
C     .. Common blocks ..
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Save statement ..
      SAVE /MSTRM/
C     ..
C
C
      CALL QCLOSE(LSTRM(IUNIT))
C
C
      END
C
C
C
      SUBROUTINE MSYPUT(IST,LSPGRP,IUNIT)
C     ===================================
C
C---- Read symmetry operator file from stream IST, find entry for
C     space-group LSPGRP. Copy symmetry operators to map stream
C     IUNIT, leaving space at head of file for NBHDR items of
C     header record. Puts number of characters of symmetry
C     information NSYMBT into header record in com  MOHDR.
C
C
C
C---- Map header common
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
C     ..
C     .. Scalar Arguments ..
      INTEGER IST,IUNIT,LSPGRP
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS
      INTEGER ISPG,ITMHDR,ITMSC1,LSKFLG,MODE,NC,NC1,NCHITM,NLAB,NR,NR1,
     +        NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER JUNK,LABELS,LSTRM,MAPCRS,NXYZ
C     ..
C     .. Local Scalars ..
      INTEGER I,IFAIL,ISG,LDUM,NBHDR,NBLIN,NCLIN,NLIN
C     ..
C     .. Local Arrays ..
      INTEGER JLINE(20)
C     ..
C     .. External Functions ..
      INTEGER NBYTXX
      EXTERNAL NBYTXX
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPOPN,QSEEK,QWRITE
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCHITM,ITMHDR,ITMSC1
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MOHDR/
C     ..
C     .. Data statements ..
      DATA NBHDR/256/
C     ..
C
      NCLIN = NBYTXX(20)
C
C---- Open symmetry file
C
      IFAIL = 1
      CALL CCPOPN(IST,'SYMOP',5,1,LDUM,IFAIL)
      IF (IFAIL.LT.0) THEN
C
C---- Error conditions
C
        WRITE (LUNOUT,FMT=6002)
        WRITE (LUNOUT,FMT=6004)
      ELSE
C
C---- Position map file to before symmetry operators
C
        CALL QSEEK(LSTRM(IUNIT),2,1,ITMHDR)
C
C---- Calculate number of items
C     / line (allowing for number of characters /
C
        NBLIN = (NCLIN+NCHITM-1)/NCHITM
   10   CONTINUE
C
C---- Find correct space-group in file.
C     Each space-group has header line of space-group number,
C     number of line of symmetry operations
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
          GO TO 10
        END IF
   30   WRITE (LUNOUT,FMT=6002)
        WRITE (LUNOUT,FMT=6006) LSPGRP
        GO TO 60
C
C---- Space-group found, copy NLIN lines of symmetry
C     operators (NCLIN characters / line) to output file
C
   40   CONTINUE
        DO 50 I = 1,NLIN
          READ (IST,FMT=6000) JLINE
          CALL QWRITE(LSTRM(IUNIT),JLINE,NBLIN)
   50   CONTINUE
C
C---- Number of characters of symmetry information
C
        NSYMBT = NLIN*NCLIN
C
C---- Position of first section
C
        ITMSC1 = NSYMBT/NCHITM + ITMHDR + 1
C
        REWIND IST
        RETURN
      END IF
   60 WRITE (LUNOUT,FMT=6008)
      CALL CCPERR(1, '**SYMMETRY FILE ERROR** in maplib.for')
C
C---- Format statements
C
 6000 FORMAT (20A4)
 6002 FORMAT (/' **SYMMETRY FILE ERROR**')
 6004 FORMAT (/' **MSYPUT: ERROR IN OPENING SYMOP FILE**')
 6006 FORMAT (/' **MSYPUT: NO SYMMETRY INFORMATION FOR SPACE GROUP NUM',
     +       'BER',I4,' IN SYMOP FILE**')
 6008 FORMAT (/' **PROGRAM TERMINATED**')
C
C
      END
C
C
C
      SUBROUTINE MSYMOP(IUNIT,NSYM,ROT)
C     =================================
C
C---- Read symmetry operations from map file IUNIT
C     (after call to MRDHDR to read header).
C     Process symmetry in lines of length NBLIN characters
C     to convert to matrices and vectors.
C
C  Returns :-
C    NSYM          Number of symmetry operations
C    ROT(4,4,NSYM)  rotation/translation matrices
C
C---- Map header common
C
C
C
C
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
C     ..
C     .. Scalar Arguments ..
      INTEGER IUNIT,NSYM
C     ..
C     .. Array Arguments ..
      REAL ROT(4,4,*)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS
      INTEGER ISPG,LSKFLG,MODE,NC,NC1,NLAB,NR,NR1,NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER ITMHDR,ITMSC1,JSYMBT,JUNK,LABELS,LSTRM,MAPCRS,MODES,NC1S,
     +        NCHITM,NCS,NR1S,NRS,NS1S,NSS,NXYZ
C     ..
C     .. Local Scalars ..
      INTEGER I,IER,ISYMC,J,L,M,N,NBLIN,NILINE,NLIN
      CHARACTER LINE*80
C     ..
C     .. Local Arrays ..
      INTEGER JLINE(20)
C     ..
C     .. External Functions ..
      INTEGER NBYTXX
      EXTERNAL NBYTXX
C     ..
C     .. External Subroutines ..
      EXTERNAL QREAD,QSEEK,SYMFR2
C     ..
C     .. Common blocks ..
      COMMON /MIHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCS(12),NRS(12),NSS(12),
     +       MODES(12),NC1S(12),NR1S(12),NS1S(12),JSYMBT(12),NCHITM(12),
     +       ITMHDR(12),ITMSC1(12)
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MIHDR/
C     ..
C
C
      NBLIN = NBYTXX(20)
      NSYM = 0
C
C---- Exit if no symmetry
C
      IF (JSYMBT(IUNIT).GT.0) THEN
C
C---- Position to start of symmetry block
C
        CALL QSEEK(LSTRM(IUNIT),2,1,ITMHDR(IUNIT))
C
C---- Total number of symmetry characters
C
        ISYMC = JSYMBT(IUNIT)
C
C---- Number of items / line
C
        NILINE = (NCHITM(IUNIT)+NBLIN-1)/NCHITM(IUNIT)
C
C---- Number of 'lines' of symmetry data, taken in groups of NBLIN
C     characters
C
        NLIN = (ISYMC+NBLIN-1)/NBLIN
C
C---- Process lines
C
        DO 20 I = 1,NLIN
          NSYM = NSYM + 1
C
C---- Clear line
C
          LINE = ' '
C
C---- Read line from file
C
          CALL QREAD(LSTRM(IUNIT),JLINE,NILINE,IER)
          IF (IER.NE.0) THEN
            GO TO 30
          ELSE
C
C---- Convert to characters
C
            WRITE (LINE,FMT=6004) JLINE
C
C---- Convert to matrices
C
            N = NSYM
            CALL SYMFR2(LINE,1,NSYM,ROT)
            IF (NSYM.GE.N) THEN
C
C---- Print
C
              WRITE (LUNOUT,FMT=6000) LINE
C
C              DO 10 J = N,NSYM
C                WRITE (LUNOUT,FMT=6002) J, ((ROT(L,M,J),M=1,4),L=1,3)
C   10         CONTINUE
C
            END IF
          END IF
   20   CONTINUE
C
        RETURN
C
C---- Error condition
C
   30   WRITE (LUNOUT,FMT=6006)
        CALL CCPERR(1,'**MAP FILE HANDLING ERROR**')
      END IF
C
C---- Format statements
C
 6000 FORMAT (/' Symmetry operations : ',A)
 6002 FORMAT (/21X,'Symmetry matrix',I5,5X,3F10.5,5X,F10.5,
     +       /2 (46X,3F10.5,5X,F10.5,/))
 6004 FORMAT (20A4)
 6006 FORMAT (/' **MAP FILE HANDLING ERROR**',//' **MSYMOP: ERROR ON R',
     +       'EADING SYMMETRY OPERATIONS FROM MAP FILE**',//' **PROGRA',
     +       'M TERMINATED**')
C
C
      END
C
C
C
      SUBROUTINE MSYCPY(IN,IOUT)
C     ==========================
C
C---- Copy symmetry data from file IN to file IOUT
C     (after calls to MRDHDR & MWRHDR)
C
C---- Headers from input and output files
C
C
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
C     ..
C     .. Scalar Arguments ..
      INTEGER IN,IOUT
C     ..
C     .. Scalars in Common ..
      INTEGER ISGI,ISGO,ITMHDO,ITMS1O,NBTI,NBTO,NCHITO
C     ..
C     .. Arrays in Common ..
      INTEGER ITMHDI,ITMS1I,JUNKI,JUNKI2,JUNKI3,JUNKO,JUNKO2,LSTRM,
     +        NCHITI
C     ..
C     .. Local Scalars ..
      INTEGER I,IER,NBLIN,NIN,NLIN,NOUT
C     ..
C     .. Local Arrays ..
      INTEGER LINE(20)
C     ..
C     .. External Functions ..
      INTEGER NBYTXX
      EXTERNAL NBYTXX
C     ..
C     .. External Subroutines ..
      EXTERNAL QREAD,QSEEK,QWRITE
C     ..
C     .. Common blocks ..
      COMMON /MIHDR/JUNKI(22),ISGI,NBTI,JUNKI2(232),JUNKI3(12,8),
     +       NCHITI(12),ITMHDI(12),ITMS1I(12)
      COMMON /MOHDR/JUNKO(22),ISGO,NBTO,JUNKO2(232),NCHITO,ITMHDO,ITMS1O
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MIHDR/,/MOHDR/
C     ..
C
C
      NBLIN = NBYTXX(20)
C
C---- Exit if no symmetry
C
      IF (NBTI.NE.0) THEN
C
C---- Position both files
C
        CALL QSEEK(LSTRM(IN),2,1,ITMHDI(IN))
        CALL QSEEK(LSTRM(IOUT),2,1,ITMHDO)
C
C---- Copy NBLIN characters at a time
C
        NLIN = (NBTI+NBLIN-1)/NBLIN
C
C---- Number of items / line for each file
C
        NIN = NBLIN/NCHITI(IN)
        NOUT = NBLIN/NCHITO
C
        DO 10 I = 1,NLIN
          CALL QREAD(LSTRM(IN),LINE,NIN,IER)
          IF (IER.NE.0) THEN
            GO TO 20
          ELSE
            CALL QWRITE(LSTRM(IOUT),LINE,NOUT)
          END IF
   10   CONTINUE
        GO TO 30
C
C
   20   WRITE (LUNOUT,FMT=6000)
        CALL CCPERR(1,' stop in maplib 191')
      END IF
C
C---- Item count
C
   30 NBTO = NBTI
C
C---- Position of first section
C
      ITMS1O = NBTO/NCHITO + ITMHDO + 1
C
C---- Format statements
C
 6000 FORMAT (/' !!!! Read error on symmetry !!!!',/)
C
C
      END
C
C
C
      SUBROUTINE MTTCPY(TITLE)
C     ========================
C
C---- Copy all titles from previously opened input and output files
C     adding title to end
C
C
C
C
C---- Copy all existing titles
C
C     .. Scalar Arguments ..
      CHARACTER TITLE* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER NLABI,NLABO
C     ..
C     .. Arrays in Common ..
      INTEGER JUNKI,JUNKI2,JUNKO,JUNKO3,LABELI,LABELO
C     ..
C     .. Local Scalars ..
      INTEGER I,J
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MIN
C     ..
C     .. Common blocks ..
      COMMON /MIHDR/JUNKI(55),NLABI,LABELI(20,10),JUNKI2(12,11)
      COMMON /MOHDR/JUNKO(55),NLABO,LABELO(20,10),JUNKO3(3)
C     ..
C
C
      DO 20 J = 1,NLABI
        DO 10 I = 1,20
          LABELO(I,J) = LABELI(I,J)
   10   CONTINUE
   20 CONTINUE
C
C---- Add new title, if already 10, overwrite last one
C
      NLABO = MIN(10,NLABI+1)
      READ (TITLE,FMT=6000) (LABELO(I,NLABO),I=1,20)
C
C---- Format statements
C
 6000 FORMAT (20A4)
C
C
      END
C
C
C
      SUBROUTINE MTTREP(TITLE,NT)
C     ===========================
C
C---- Replace NT'th title in output file (after MWRHDR)
C
C
C---- Add new title, if already 10, overwrite last one
C
C
C
C     .. Scalar Arguments ..
      INTEGER NT
      CHARACTER TITLE* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER NLABO
C     ..
C     .. Arrays in Common ..
      INTEGER JUNKO,JUNKO3,LABELO
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MAX
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/JUNKO(55),NLABO,LABELO(20,10),JUNKO3(3)
C     ..
C
C
      NLABO = MAX(NLABO,NT)
      READ (TITLE,FMT=6000) (LABELO(I,NT),I=1,20)
C
C---- Format statements
C
 6000 FORMAT (20A4)
C
C
      END
C
C
C
      SUBROUTINE MSKPUT(ASKWMT,ASKWTN)
C     ================================
C
C---- Put skew transformation into output common block
C
C
C
C     .. Array Arguments ..
      REAL ASKWMT(3,3),ASKWTN(3)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS
      INTEGER ISPG,ITMHDR,ITMSC1,LSKFLG,MODE,NC,NC1,NCHITM,NLAB,NR,NR1,
     +        NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER JUNK,LABELS,MAPCRS,NXYZ
C     ..
C     .. Local Scalars ..
      INTEGER I,J
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCHITM,ITMHDR,ITMSC1
C     ..
C     .. Save statement ..
      SAVE /MOHDR/
C     ..
C
C
      LSKFLG = 1
      DO 20 I = 1,3
        DO 10 J = 1,3
          SKWMAT(I,J) = ASKWMT(I,J)
   10   CONTINUE
   20 CONTINUE
      DO 30 I = 1,3
        SKWTRN(I) = ASKWTN(I)
   30 CONTINUE
C
C
      END
C
C
C
      INTEGER FUNCTION MSKGET(ASKWMT,ASKWTN)
C     ======================================
C
C---- Get skew transformation from input common block
C
C
C
C     .. Array Arguments ..
      REAL ASKWMT(3,3),ASKWTN(3)
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS
      INTEGER ISPG,LSKFLG,MODE,NC,NC1,NLAB,NR,NR1,NS,NS1,NSYMBT
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER ITMHDR,ITMSC1,JSYMBT,JUNK,LABELS,MAPCRS,MODES,NC1S,NCHITM,
     +        NCS,NR1S,NRS,NS1S,NSS,NXYZ
C     ..
C     .. Local Scalars ..
      INTEGER I,J
C     ..
C     .. Common blocks ..
      COMMON /MIHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCS(12),NRS(12),NSS(12),
     +       MODES(12),NC1S(12),NR1S(12),NS1S(12),JSYMBT(12),NCHITM(12),
     +       ITMHDR(12),ITMSC1(12)
C     ..
C     .. Save statement ..
      SAVE /MIHDR/
C     ..
C
C
      MSKGET = LSKFLG
      IF (LSKFLG.NE.0) THEN
        DO 20 I = 1,3
          DO 10 J = 1,3
            ASKWMT(I,J) = SKWMAT(I,J)
   10     CONTINUE
   20   CONTINUE
        DO 30 I = 1,3
          ASKWTN(I) = SKWTRN(I)
   30   CONTINUE
      END IF
C
C
      END
C
C
C
      SUBROUTINE MODECV(X,BLINE,N,MODE,JB)
C     ====================================
C
C---- Convert N items from BLINE in mode MODE to reals in X
C     JB = number of bytes/item
C
C
C
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
C     ..
C     .. Scalar Arguments ..
      INTEGER JB,MODE,N
C     ..
C     .. Array Arguments ..
      REAL BLINE(*),X(*)
C     ..
C     .. Local Scalars ..
      REAL A,B,R
      INTEGER I,IFAIL,II,J,K
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPTOI
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     ..
C
C
      J = 1
      IFAIL = 1
C
      DO 10 I = 1,N
        IF (MODE.EQ.0) THEN
C
C---- Single bytes
C
          CALL CCPTOI(BLINE,I,II,1,IFAIL)
          IF (IFAIL.LT.0) THEN
            GO TO 20
          ELSE
            R = II
          END IF
        ELSE IF (MODE.EQ.1) THEN
C
C---- Integer*2
C
          CALL CCPTOI(BLINE,I,II,2,IFAIL)
          IF (IFAIL.LT.0) THEN
            GO TO 20
          ELSE
            R = II
          END IF
C
C---- Complex integer*2 (watch the order of the next 2 assignments)
C
        ELSE IF (MODE.EQ.3) THEN
          K = 2*I - 1
          CALL CCPTOI(BLINE,K,II,2,IFAIL)
          IF (IFAIL.LT.0) THEN
            GO TO 20
          ELSE
            A = II
            K = K + 1
            CALL CCPTOI(BLINE,K,II,2,IFAIL)
            IF (IFAIL.LT.0) THEN
              GO TO 20
            ELSE
              B = II
              R = SQRT(A*A+B*B)
            END IF
          END IF
C
C---- Complex amplitude
C
        ELSE IF (MODE.EQ.4) THEN
          K = 2*I - 1
          A = BLINE(K)
          K = K + 1
          B = BLINE(K)
          R = SQRT(A*A+B*B)
        ELSE IF (MODE.NE.2) THEN
        END IF
C
        X(J) = R
        J = J + 1
   10 CONTINUE
C
      RETURN
C
C---- Error
C
   20 WRITE (LUNOUT,FMT=6000)
      CALL CCPERR(1,'**MAP FILE HANDLING ERROR**')
C
C---- Format statements
C
 6000 FORMAT (/' **MAP FILE HANDLING ERROR**',//' **MODECV: CONVERSION',
     +       ' OF BYTE OR INTEGER*2 TO INTEGER UNAVAILABLE**',//' **PR',
     +       'OGRAM TERMINATED**')
C
C
      END
C
C
C
      INTEGER FUNCTION NBYTXX(NWORD)
C     ==============================
C
C---- Returms the number of machine items in nword words
C     (as defined by the function ccpbyt)
C
C
C     .. Scalar Arguments ..
      INTEGER NWORD
C     ..
C     .. Local Scalars ..
      INTEGER NITM
      LOGICAL BYT
C     ..
C     .. External Functions ..
      LOGICAL CCPBYT
      EXTERNAL CCPBYT
C     ..
C
C
      BYT = CCPBYT(NITM)
      NBYTXX = NWORD*NITM
C
C
      END
