C
CMWCLOSE    ENTRY ccp4_map_write_close_auto
CMRCLOS     ENTRY ccp4_map_read_close
CMCLOSC     ENTRY ccp4_map_write_close_user_mean
CMCLOSE     ENTRY ccp4_map_write_close_user_sum
CMSYCPY     ENTRY ccp4_map_copy_symmetry
CMRFNAM     ENTRY ccp4_map_get_last_read_filename
CMWFNAM     ENTRY ccp4_map_get_last_write_filename
CMRDLIN     ENTRY ccp4_map_read_line_as_mode
CMSYMOP     ENTRY ccp4_map_read_symm_matrix
CMGULP      ENTRY ccp4_map_read_whole_section_as_mode
CMGULPR     ENTRY ccp4_map_read_whole_section_as_real
CMODECV     ENTRY ccp4_map_mode_to_real
CMRDHDR     ENTRY ccp4_map_read_open_header
CMRDHDS     ENTRY ccp4_map_read_open_header_check
CMWRHDR     ENTRY ccp4_map_write_open_header_by_id
CMWRHDL     ENTRY ccp4_map_write_open_header_by_name
CMSKPUT     ENTRY ccp4_map_write_skew_info
CMSYPUT     ENTRY ccp4_map_write_spgname
CMSYWRT     ENTRY ccp4_map_write_symm_matrix
CMPOSN      ENTRY ccp4_map_read_position_section
CMPOSNW     ENTRY ccp4_map_write_position_section
CMTTCPY     ENTRY ccp4_map_copy_title
CMTTREP     ENTRY ccp4_map_write_replace_title
CMSPEW      ENTRY ccp4_map_write_all_section
CMWRSEC     ENTRY ccp4_map_write_part_section
C
C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
C
C---- F77MSUB.FOR                               30/10/86    JWC
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
C        in variable items, but it should make it more portable
C        Phil Evans 2/5/84
C
C---- CCP4 VERSION      JOHN CAMPBELL,  JAN 1985
C
C    October 1985
C            added entry points MRFNAM, MWFNAM, routine MTTCPY  PRE
C    5/9/86  added routine MTTREP  replace output title after
C            MWRHDR/MTTCPY
C   30/10/86 Add missing argument in opening SYMOP using CCPDPN
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
C   11/5/92  Changes to subroutines MRHDRS, MSYMOP, MSYCPY, MCLOSC,
C            MCLOSE to read real, integer & character parts of header
C            seperately. Allows CONVERT stuff to work (David Wild).
C
C   29/5/92  Same changes to MWCLOSE (D.W.)
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
C              unix.m4 routines VAXVMS
C              VMSSUPPORT.FOR routines VAXVMS
C
C
C---- Assumes that a minimum of 4 characters may be packed
C     into a word for p
C     The title and symmetry information into the file header
C
C************ This file contains the following routines ***********
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
C      SUBROUTINE MODECV(X,BLINE,N,MODE)
C      SUBROUTINE MSYWRT(IUNIT,NSYM,ROT)
C
C      INTEGER FUNCTION MSKGET(ASKWMT,ASKWTN)
C      INTEGER FUNCTION NBYTXX(NWORD)
C
C
C_BEGIN_MWRHDR
C
      SUBROUTINE MWRHDR(IUNIT,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,NV1,NV2,
     +                  CELL,LSPGRP,LMODE)
C     =================================================================
C
C
C---- Put map header into common block /MOHDR/ and open map file on unit
C     IUNIT with logical name 'MAPOUT'
C 
C  Call:  CALL MWRHDR(IUNIT,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,
C        +            NV1,NV2,CELL,LSPGRP,LMODE)
C 
C Note on the difference between the subroutines 'MWRHDR' and 'MWRHDL'
C 
C---- These subroutines are used to open an output map file and 
C     set up the header information. The actual header is only 
C     written to the file when the file is closed via the routine
C     MCLOSE.  The  only  difference  between  the  two subroutines 
C     is that MWRHDR does not have a parameter for the  logical  file
C     name for which a name of 'MAPOUT' is assumed.
C
C
C---- Parameters:
C     ==========
C
C  IUNIT (I)   Map stream number
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
C_END_MWRHDR
C
C
C      IMPLICIT NONE
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
      ENTRY ccp4_map_write_open_header_by_id(
     +          IUNIT,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,NV1,NV2,
     +                  CELL,LSPGRP,LMODE)
C
      CALL MWRHDL(IUNIT,'MAPOUT',TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,NV1,
     +            NV2,CELL,LSPGRP,LMODE)
C
      END
C
C
C_BEGIN_MWRHDL
C
      SUBROUTINE MWRHDL(IUNIT,MAPNAM,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,
     +                  NV1,NV2,CELL,LSPGRP,LMODE)
C     ================================================================
C
C
C---- Put map header into common block /MOHDR/ and open map file on unit
C     IUNIT with logical name MAPNAM
C 
C  Call:  CALL MWRHDL(IUNIT,MAPNAM,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,
C        +            NV1,NV2,CELL,LSPGRP,LMODE)
C
C Note on the difference betwenn the subroutines 'MWRHDR' and 'MWRHDL'
C 
C---- These subroutines are used to open an output map file and 
C     set up the header information. The actual header is only 
C     written to the file when the file is closed via the routine
C     MCLOSE.  The  only  difference  between  the  two subroutines 
C     is that MWRHDR does not have a parameter for the  logical  file
C     name for which a name of 'MAPOUT' is assumed.
C
C
C---- Parameters:
C     ==========
C
C  IUNIT (I)   Map stream number
C 
C  MAPNAM (I)   Logical  file  name  (type  CHARACTER)  
C               e.g.  'MAPOUT'   
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
C_END_MWRHDL
C
C      IMPLICIT NONE
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
      DOUBLE PRECISION QOFFST
      PARAMETER (QOFFST = -1.0D+10)
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
      CHARACTER BLANK*4,FILE*255,OUTLIN*100
C     ..
C     .. Local Arrays ..
      REAL HEADER(256)
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL QMODE,QQINQ,QOPEN, CCPERR, QWRITR
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
      DATA NBHDR/256/,BLANK/'    '/, FILE/' '/
C     ..
C
      ENTRY ccp4_map_write_open_header_by_name
     +       (IUNIT,MAPNAM,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,
     +                  NV1,NV2,CELL,LSPGRP,LMODE)
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
        CALL QWRITR(LSTRM(IUNIT),HEADER,NBHDR)
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
C---- Get and print filename, also ensure write does not exceed 132.
C
        CALL QQINQ(LSTRM(IUNIT),MAPNAM,FILE,NFILSZ)
        WRITE (OUTLIN,FMT=6000) IUNIT
        OUTLIN(LENSTR(OUTLIN)+2:) = FILE
        WRITE (LUNOUT,FMT='(/,A)') OUTLIN(1:LENSTR(OUTLIN))
        OUTLIN = '     logical name '
        OUTLIN(LENSTR(OUTLIN)+2:) = MAPNAM
        WRITE (LUNOUT,FMT='(A,/)') OUTLIN(1:LENSTR(OUTLIN))
C
      END IF
      RETURN
C
C_BEGIN_MWFNAM
C
      ENTRY MWFNAM(FNAME)
      ENTRY ccp4_map_get_last_write_filename(FNAME)
C     ===================
C
C---- Returns filename from last file open,
C     must be called after MWRHDR
C 
C  Parameters:
C     FNAME  (O)    filename 
C
C_END_MWFNAM
C
      FNAME = FILE
C
C---- Format statements
C
 6000 FORMAT ('  File name for output map file on unit',I4,' : ')
 6002 FORMAT (20A4)
 6004 FORMAT (/' **MAP FILE HANDLING ERROR**')
 6006 FORMAT (/' **MWRHDL: UNIT NO. MUST BE 1 TO 12, =',I3,' **')
 6008 FORMAT (/' **PROGRAM TERMINATED**')
C
      END
C
C
C_BEGIN_MWRSEC
C
      SUBROUTINE MWRSEC(IUNIT,X,MU,MV,IU1,IU2,IV1,IV2)
C     ================================================
C
C---- Write part of map section X(MU,MV) to stream IUNIT
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
C_END_MWRSEC
C
C      IMPLICIT NONE
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
      DOUBLE PRECISION QOFFST
      PARAMETER (QOFFST = -1.0D+10)
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
      EXTERNAL QWRITR, CCPERR
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCHITM,ITMHDR,ITMSC1
      COMMON /MOHSUM/  SUMRHO, SUMRH2, OFFSTR
      DOUBLE PRECISION SUMRHO, SUMRH2, OFFSTR
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MOHDR/,/MOHSUM/
C     ..
C
      ENTRY ccp4_map_write_part_section(IUNIT,X,MU,MV,IU1,IU2,IV1,IV2)
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
          CALL QWRITR(LSTRM(IUNIT),X(IU1,J),NCOLS)
   10   CONTINUE
C
        IF(MODE.EQ.2) THEN
C If not set yet, set bias for rms deviation calculation to 1st point in map
C This reduces rounding errors
           IF (OFFSTR .LT. QOFFST) THEN
              OFFSTR = X(IU1, IV1)
           ENDIF
C    Calculate AMEAN ARMS
           DO 20 J = IV1,IV2
              DO 30 I = IU1,IU2
                 IF(X(I,J) .GT.AMAX) AMAX = X(I,J)
                 IF(X(I,J) .LT.AMIN) AMIN = X(I,J)
                 SUMRHO = SUMRHO + X(I,J) - OFFSTR
                 SUMRH2  = SUMRH2  + (X(I,J) - OFFSTR)**2
 30           CONTINUE
 20        CONTINUE
        END IF
      END IF
C
C---- Format statements
C
 6000 FORMAT (/' **MWRSEC: MODE MUST BE 2, =',I2)
C
      END
C
C
C_BEGIN_MSPEW
C
      SUBROUTINE MSPEW(IUNIT,X)
C     =========================
C
C---- Write whole section of map to stream IUNIT.
C     This routine is only suitable when the whole array is written
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
C  IUNIT (I)   Map stream number
C 
C     X (I)   Array holding the map section
C 
C_END_MSPEW
C
      DOUBLE PRECISION QOFFST
      PARAMETER (QOFFST = -1.0D+10)
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
      EXTERNAL QWRITR
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(17),ARMS,NLAB,LABELS(20,10),NCHITM,ITMHDR,ITMSC1
      COMMON /MOHSUM/  SUMRHO, SUMRH2, OFFSTR
      DOUBLE PRECISION SUMRHO, SUMRH2, OFFSTR
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MOHDR/,/MOHSUM/
C     ..
      ENTRY ccp4_map_write_all_section (IUNIT,X)
C
C---- Number of items
C
      N = NC*NR
C
      CALL QWRITR(LSTRM(IUNIT),X,N)
C
C    Calculate AMEAN ARMS - ONLY IF MAP IS REAL*4
C
        IF(MODE.EQ.2) THEN
C If not set yet, set bias for rms deviation calculation to 1st point in map
C This reduces rounding errors
           IF (OFFSTR .LT. QOFFST) THEN
              OFFSTR = X(1)
           ENDIF
           DO 20 J = 1,N
              IF(X(J) .GT.AMAX) AMAX = X(J)
              IF(X(J) .LT.AMIN) AMIN = X(J)
              SUMRHO = SUMRHO + X(J) - OFFSTR
              SUMRH2  = SUMRH2  + (X(J) - OFFSTR)**2
 20        CONTINUE
        ENDIF
C
      END
C
C
C
      SUBROUTINE MSTMST(MAPST)
C     ========================
C
C  Set integer MAPST to character string 'MAP '
C
      INTEGER MAPST
C
      CHARACTER*4 MAP
      DATA MAP/'MAP '/
C
      READ (MAP, '(A4)') MAPST
      RETURN
      END
C
C
C_BEGIN_MCLOSE
C
      SUBROUTINE MCLOSE(IUNIT,RHMIN,RHMAX,RHMEAN,RHRMS)
C     =================================================
C
C---- Write out header to map file on stream IUNIT, and close it
C  You should normally use MWCLOSE rather than this routine
C
C---- Added code to write out map/machine stamp to header.  D.Wild 11/5/92
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
C_END_MCLOSE
C
C      IMPLICIT NONE
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
      INTEGER   MACHST
      INTEGER   MAPST
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION T
      INTEGER NBHDR,NCHHDR,PLEVEL
C     ..
C     .. Local Arrays ..
      REAL HEADER(256)
C     ..
C     .. External Subroutines ..
      EXTERNAL QCLOSE,QMODE,QSEEK,QWRITR, QWARCH, MSTMST, QPRLVL
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(15),MAPST,MACHST(1),ARMS,NLAB,LABELS(20,10),NCHITM,
     +       ITMHDR,ITMSC1
      COMMON /MOHSUM/  SUMRHO, SUMRH2, OFFSTR
      DOUBLE PRECISION SUMRHO, SUMRH2, OFFSTR

      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (NC,HEADER(1))
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MOHDR/,/MOHSUM/
C     ..
C     .. Data statements ..
C
C---- Number of items in header
C
      DATA NBHDR/256/
C     ..
      ENTRY ccp4_map_write_close_user_sum(IUNIT,RHMIN,RHMAX,RHMEAN,
     +                                    RHRMS)
C
C---- Calculate mean in double precision
C
      T = NC*NR*NS
      SUMRHO = RHMEAN/T
      AMEAN = SUMRHO
      SUMRH2 = RHRMS/T - SUMRHO*SUMRHO
      IF (SUMRH2.GT.0.0) THEN
         ARMS = SQRT(SUMRH2)
      ELSE
         ARMS = 0.0
      ENDIF
C
C---- Minimum & maximum
C
      AMIN = RHMIN
      AMAX = RHMAX
      CALL QPRLVL(PLEVEL)
      IF(MODE.EQ.2 .AND. PLEVEL.GE.1) THEN
        WRITE (LUNOUT,FMT=6000) AMIN,AMAX,AMEAN,ARMS
      ENDIF
C
C---- write map stamp to word 53
C set MAPST = 'MAP'
      CALL MSTMST(MAPST)
C
C---- Write to header, reset mode to 2 first
C
      CALL QMODE(LSTRM(IUNIT),2,NCHHDR)
      CALL QSEEK(LSTRM(IUNIT),1,1,1)
      CALL QWRITR(LSTRM(IUNIT),HEADER,NBHDR)
C     architecture stamp at the right position:
      CALL QWARCH (LSTRM (IUNIT), 53)
C
C---- Close file
C
      CALL QCLOSE(LSTRM(IUNIT))
C
C---- Format statements
C
 6000 FORMAT (/'   Minimum density in map  =',F15.5,
     $     '   Maximum density         =',F15.5/
     $     '   Mean density            =',F15.5/
     $     '   Rms deviation from mean =',F15.5,/)
C
      END
C
C
C_BEGIN_MWCLOSE
C
      SUBROUTINE MWCLOSE(IUNIT)
C     =========================
C
C---- Write out header to map file on stream IUNIT, and close it
C  This is the recommended routine for closing a map file
C
C     The minimum, maximum, mean & rms densities are calculated 
C     from internal sums
C 
C---- Added code to write out map/machine stamp to header.  D.Wild 29/5/92
C
C---- Parameters:
C     ==========
C 
C  IUNIT (I)   The map stream number
C 
C_END_MWCLOSE
C      IMPLICIT NONE
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
C     ..
C     .. Scalar Arguments ..
      INTEGER IUNIT
C     ..
C     .. Scalars in Common ..
      REAL AMAX,AMEAN,AMIN,ARMS
      INTEGER ISPG,ITMHDR,ITMSC1,LSKFLG,MODE,NC,NC1,NCHITM,NLAB,NR,NR1,
     +        NS,NS1,NSYMBT
      INTEGER MAPST
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER JUNK,LABELS,LSTRM,MAPCRS,NXYZ,MACHST
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION T
      INTEGER NBHDR,NCHHDR,PLEVEL
C     ..
C     .. Local Arrays ..
      REAL HEADER(256)
C     ..
C     .. External Subroutines ..
      EXTERNAL QCLOSE,QMODE,QSEEK,QWRITR, MSTMST, QWARCH, QPRLVL
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(15),MAPST,MACHST(1),ARMS,NLAB,LABELS(20,10),NCHITM,
     +       ITMHDR,ITMSC1
      COMMON /MOHSUM/  SUMRHO, SUMRH2, OFFSTR
      DOUBLE PRECISION SUMRHO, SUMRH2, OFFSTR

      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (NC,HEADER(1))
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MOHDR/,/MOHSUM/
C     ..
C     .. Data statements ..
C
C---- Number of items in header
C
      DATA NBHDR/256/
C     ..
      ENTRY ccp4_map_write_close_auto (IUNIT)
C
C---- Calculate mean
C
      T = NC*NR*NS
      SUMRHO = SUMRHO/T
      AMEAN = SUMRHO + OFFSTR
      SUMRH2 = SUMRH2/T - SUMRHO*SUMRHO
      IF (SUMRH2.GT.0.0) THEN
         ARMS = SQRT(SUMRH2)
      ELSE
         ARMS = 0.0
      ENDIF
C
C---- Minimum & maximum
C
      CALL QPRLVL(PLEVEL)
      IF(MODE.EQ.2 .AND. PLEVEL.GE.1) THEN
        WRITE (LUNOUT,FMT=6000) AMIN,AMAX,AMEAN,ARMS
      ENDIF
C
C---- write map stamp to word 53 
C
C set MAPST = 'MAP'
      CALL MSTMST(MAPST)
C
C---- Write to header, reset mode to 2 first
C
      CALL QMODE(LSTRM(IUNIT),2,NCHHDR)
      CALL QSEEK(LSTRM(IUNIT),1,1,1)
      CALL QWRITR(LSTRM(IUNIT),HEADER,NBHDR)
C     architecture stamp at the right position:
      CALL QWARCH (LSTRM (IUNIT), 53)
C
C---- Close file
C
      CALL QCLOSE(LSTRM(IUNIT))
C
C---- Format statements
C
 6000 FORMAT (/'   Minimum density in map  =',F15.5,
     $     '   Maximum density         =',F15.5/
     $     '   Mean density            =',F15.5/
     $     '   Rms deviation from mean =',F15.5,/)
C
      END
C
C
C_BEGIN_MCLOSC
C
      SUBROUTINE MCLOSC(IUNIT,RHMIN,RHMAX,RHMEAN,RHRMS)
C     =================================================
C
C---- Write out header to map file on stream IUNIT, and close it
C     This routine is identical to MCLOSE except for arguments
C      RHMEAN, RHRMS
C  You should normally use MWCLOSE rather than this routine
C
C---- It is more suitable than MCLOSE when a map file is being copied
C
C---- Added code to write out map/machine stamp to header.  D.Wild 11/5/92
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
C_END_MCLOSC
C
C      IMPLICIT NONE
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
      INTEGER MAPST
C     ..
C     .. Arrays in Common ..
      REAL CEL,SKWMAT,SKWTRN
      INTEGER JUNK,LABELS,LSTRM,MAPCRS,NXYZ,MACHST
C     ..
C     .. Local Scalars ..
      INTEGER NBHDR,NCHHDR,PLEVEL
C     ..
C     .. Local Arrays ..
      REAL HEADER(256)
C     ..
C     .. External Subroutines ..
      EXTERNAL QCLOSE,QMODE,QSEEK,QWARCH,QWRITR,MSTMST,QPRLVL
C     ..
C     .. Common blocks ..
      COMMON /MOHDR/NC,NR,NS,MODE,NC1,NR1,NS1,NXYZ(3),CEL(6),MAPCRS(3),
     +       AMIN,AMAX,AMEAN,ISPG,NSYMBT,LSKFLG,SKWMAT(3,3),SKWTRN(3),
     +       JUNK(15),MAPST,MACHST(1),ARMS,NLAB,LABELS(20,10),NCHITM,
     +       ITMHDR,ITMSC1
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
      ENTRY ccp4_map_write_close_user_mean(IUNIT,RHMIN,RHMAX,RHMEAN,
     +                                     RHRMS)
C
      AMEAN = RHMEAN
      ARMS = RHRMS
C
C---- Minimum & maximum
C
      AMIN = RHMIN
      AMAX = RHMAX
C
C---- write map stamp to word 53 
C
C set MAPST = 'MAP'
      CALL MSTMST(MAPST)
      CALL QPRLVL(PLEVEL)
      IF(MODE.EQ.2 .AND. PLEVEL.GE.1) THEN
        WRITE (LUNOUT,FMT=6000) AMIN,AMAX,AMEAN,ARMS
      ENDIF
C
C---- Write to header, reset mode to 2 first
C
      CALL QMODE(LSTRM(IUNIT),2,NCHHDR)
      CALL QSEEK(LSTRM(IUNIT),1,1,1)
      CALL QWRITR(LSTRM(IUNIT),HEADER,NBHDR)
C     architecture stamp at the right position:
      CALL QWARCH (LSTRM (IUNIT), 53)
C
C---- Close file
C
      CALL QCLOSE(LSTRM(IUNIT))
C
C---- Format statements
C
 6000 FORMAT (/'   Minimum density in map  =',F15.5,
     $     '   Maximum density         =',F15.5/
     $     '   Mean density            =',F15.5/
     $     '   Rms deviation from mean =',F15.5,/)
C
      END
C
C
C_BEGIN_MPOSNW
C
      SUBROUTINE MPOSNW(IUNIT,JSEC)
C     ============================
C
C---- Position output map before section JSEC
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
C_END_MPOSNW
C
C      IMPLICIT NONE
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
      INTEGER LSEC,NREC
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
      ENTRY ccp4_map_write_position_section(IUNIT,JSEC)
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
      END
C
C
C_BEGIN_MRDHDR
C
      SUBROUTINE MRDHDR(IUNIT,MAPNAM,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,
     +                  NV1,NV2,CELL,LSPGRP,LMODE,RHMIN,RHMAX,RHMEAN,
     +                  RHRMS)
C     ================================================================
C
C---- Read map header from stream IUNIT, logical name in MAPNAM
C
C  IUNIT    (I)    stream number
C  MAPNAM   (I)    logical (file) name
C---- Returns:
C 
C  TITLE    (O)    80 character title for map (character)
C  NSEC     (O)    number of sections in map
C  IUVW(3)  (O)    fast,medium,slow axes (1,2,3 for x,y,z)
C  MXYZ(3)  (O)    sampling intervals along whole cell on x,y,z
C  NW1      (O)    first section number
C  NU1,NU2  (O)    limits on fast axis(grid units)
C  NV1,NV2  (O)    limits on medium axis
C  CELL(6)  (O)    cell dimensions, A and degrees
C  LSPGRP   (O)    space-group number
C  LMODE    (O)    map data mode =0 logical*1
C                                =1 integer*2
C                                =2 real*4
C                                =3 complex integer*2
C                                =4 complex real*4
C                                =5 treated as mode 0
C                                =10 bricked byte map
C  RHMIN,RHMAX (O)  minimum, maximum density
C  RHMEAN      (O)  mean density
C  RHRMS       (O)  rms deviation from mean density
C
C_END_MRDHDR
C
C      IMPLICIT NONE
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
C     .. Error and print control ..
      INTEGER IFAIL
      INTEGER IPRINT
C     ..
C     .. External Subroutines ..
      EXTERNAL MRDHDS
C     ..
      ENTRY ccp4_map_read_open_header(IUNIT,MAPNAM,TITLE,NSEC,
     +                  IUVW,MXYZ,NW1,NU1,NU2,
     +                  NV1,NV2,CELL,LSPGRP,LMODE,RHMIN,RHMAX,RHMEAN,
     +                  RHRMS)

      IFAIL  = 0
      IPRINT = 1

      CALL MRDHDS(IUNIT,MAPNAM,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,
     +                  NV1,NV2,CELL,LSPGRP,LMODE,RHMIN,RHMAX,RHMEAN,
     +                  RHRMS,IFAIL,IPRINT)

      RETURN
      END
C
C
C_BEGIN_MRDHDS
C
      SUBROUTINE MRDHDS(IUNIT,MAPNAM,TITLE,NSEC,IUVW,MXYZ,NW1,NU1,NU2,
     +                  NV1,NV2,CELL,LSPGRP,LMODE,RHMIN,RHMAX,RHMEAN,
     +                  RHRMS,IFAIL,IPRINT)
C     ================================================================
C
C---- Read map header from stream IUNIT, logical name in MAPNAM
C     Map header common
C
C---- On entry:
C
C  IUNIT    (I)    stream number
C  MAPNAM   (I)    logical (file) name
C---- Returns:
C 
C  TITLE    (O)    80 character title for map (character)
C  NSEC     (O)    number of sections in map
C  IUVW(3)  (O)    fast,medium,slow axes (1,2,3 for x,y,z)
C  MXYZ(3)  (O)    sampling intervals along whole cell on x,y,z
C  NW1      (O)    first section number
C  NU1,NU2  (O)    limits on fast axis(grid units)
C  NV1,NV2  (O)    limits on medium axis
C  CELL(6)  (O)    cell dimensions, A and degrees
C  LSPGRP   (O)    space-group number
C  LMODE    (O)    map data mode =0 logical*1
C                                =1 integer*2
C                                =2 real*4
C                                =3 complex integer*2
C                                =4 complex real*4
C                                =5 treated as mode 0
C                                =10 bricked byte map
C  RHMIN,RHMAX (O)  minimum, maximum density
C  RHMEAN      (O)  mean density
C  RHRMS       (O)  rms deviation from mean density
C
C  IFAIL (I/O)  On input:     =0, stop on error
C                             =1, return on error
C               On output:    unchanged if no error
C                             =-1, error 
C  IPRINT (I)                 = 0; silent
C                          .ne. 0; print file name, header info etc
C
C_END_MRDHDS
C
C      IMPLICIT NONE
C
C     .. Parameters ..
      INTEGER LUNOUT
      PARAMETER (LUNOUT=6)
C     ..
C     .. Scalar Arguments ..
      REAL RHMAX,RHMEAN,RHMIN,RHRMS
      INTEGER IUNIT,LMODE,LSPGRP,NSEC,NU1,NU2,NV1,NV2,NW1
      CHARACTER FNAME*(*),MAPNAM*(*),TITLE*(*)
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
      INTEGER I,IER,IRESLT,J,KMODE,NBHDR,NITHDR,NCHHDR,NFILSZ,NW2
      CHARACTER FILE*255, OUTLIN*100
C     ..
C     .. Error and print control ..
      INTEGER IFAIL
      INTEGER IPRINT
C     ..
C
Cdw----added to seperate real and integer parts of header
C
      INTEGER IHDR1(10),IHDR2(3),IHDR3(3),IHDR4(17),IHDR5(1),IHDR6(200)
      REAL    RHDR1(6),RHDR2(3),RHDR3(12),RHDR4 (1)
      CHARACTER LXYZ(3)*1
C     ..
C     .. External Subroutines ..
      EXTERNAL QMODE,QQINQ,QOPEN,QSEEK,LENSTR,QRARCH, CCPEXS,
     +     CCPERR, QPRINT, QREADI, QREADR
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
      EQUIVALENCE (NC,IHDR1(1)),
     *            (CEL(1),RHDR1(1)),
     *            (MAPCRS(1),IHDR2(1)),
     *            (AMIN,RHDR2(1)),
     *            (ISPG,IHDR3(1)),
     *            (SKWMAT(1,1),RHDR3(1)),
     *            (JUNK(1),IHDR4(1)),
     *            (ARMS,RHDR4),
     *            (NLAB,IHDR5),
     *            (LABELS(1,1),IHDR6(1))
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MIHDR/,FILE
C     ..
C     .. Data statements ..
      DATA NBHDR/256/
      DATA LXYZ/'X','Y','Z'/
C     ..
      ENTRY ccp4_map_read_open_header_check(IUNIT,MAPNAM,TITLE,
     +                  NSEC,IUVW,MXYZ,NW1,NU1,NU2,
     +                  NV1,NV2,CELL,LSPGRP,LMODE,RHMIN,RHMAX,RHMEAN,
     +                  RHRMS,IFAIL,IPRINT)
C
      TITLE = ' '

C     Check file exists
      IF ( .NOT. CCPEXS ( MAPNAM ) ) THEN
        IF ( IFAIL .EQ. 0 ) THEN
          OUTLIN = ' **FILE DOES NOT EXIST > '
          OUTLIN(LENSTR(OUTLIN)+2:) = MAPNAM
          WRITE (LUNOUT,FMT='(/,A)')OUTLIN(1:LENSTR(OUTLIN))
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
        IF ( IFAIL .EQ. 0 ) THEN
          WRITE (LUNOUT,FMT=6012) IUNIT
          CALL CCPERR(1, '**MAP FILE HANDLING ERROR**')
        ELSE
          IFAIL = -1
          RETURN
        ENDIF
      ENDIF
C     
C---- Open file
C     
      CALL QOPEN(LSTRM(IUNIT),MAPNAM,'RO')
C     set up transparent numbers if necessary:
      CALL QRARCH (LSTRM (IUNIT), 53, IRESLT)
      IF (IRESLT.EQ.0) CALL QPRINT(1,
     +     ' WARNING: no architecture information in file --'//
     +     ' assuming native.')
      CALL QSEEK (LSTRM (IUNIT), 1, 1, 1)
C     
C---- Get and print file name
C     
      IF ( IPRINT .NE. 0 ) THEN
        CALL QQINQ(LSTRM(IUNIT),MAPNAM,FILE,NFILSZ)
        IF (NFILSZ.GE.0) THEN
          WRITE (OUTLIN,FMT=6000) IUNIT
          OUTLIN(LENSTR(OUTLIN)+2:) = FILE
          WRITE (LUNOUT,FMT='(/,A)') OUTLIN(1:LENSTR(OUTLIN))
          WRITE (OUTLIN,FMT=6001) NFILSZ
          OUTLIN(LENSTR(OUTLIN)+2:) = MAPNAM
          WRITE (LUNOUT,FMT='(A,/)') OUTLIN(1:LENSTR(OUTLIN))
        ENDIF
        IF (NFILSZ.LT.0) THEN
          WRITE (OUTLIN,FMT=6002) IUNIT
          OUTLIN(LENSTR(OUTLIN)+2:) = FILE
          WRITE (LUNOUT,FMT='(/,A)') OUTLIN(1:LENSTR(OUTLIN))
          OUTLIN = '                               Logical name '
          OUTLIN(LENSTR(OUTLIN)+2:) = MAPNAM
          WRITE (LUNOUT,FMT='(A,/)') OUTLIN(1:LENSTR(OUTLIN))
        ENDIF
      ENDIF
C     
C     dw---- Read header, modes 2 & 6 in real and integer blocks
C     dw---- Mode 0 for characters
C     dw---- Unfortunately need to call QMODE each time we change
C     
      NITHDR = 10
      CALL QMODE(LSTRM(IUNIT),6,NCHHDR)
      CALL QREADI(LSTRM(IUNIT),IHDR1,NITHDR,IER)
      IF (IER.NE.0) GOTO 99
C     
      NITHDR = 6
      CALL QMODE(LSTRM(IUNIT),2,NCHHDR)
      CALL QREADR(LSTRM(IUNIT),RHDR1,NITHDR,IER)
      IF (IER.NE.0) GOTO 99
C     
      NITHDR = 3
      CALL QMODE(LSTRM(IUNIT),6,NCHHDR)
      CALL QREADI(LSTRM(IUNIT),IHDR2,NITHDR,IER)
      IF (IER.NE.0) GOTO 99
C     
      NITHDR = 3
      CALL QMODE(LSTRM(IUNIT),2,NCHHDR)
      CALL QREADR(LSTRM(IUNIT),RHDR2,NITHDR,IER)
      IF (IER.NE.0) GOTO 99
C     
      NITHDR = 3
      CALL QMODE(LSTRM(IUNIT),6,NCHHDR)
      CALL QREADI(LSTRM(IUNIT),IHDR3,NITHDR,IER)
      IF (IER.NE.0) GOTO 99
C     
      NITHDR = 12
      CALL QMODE(LSTRM(IUNIT),2,NCHHDR)
      CALL QREADR(LSTRM(IUNIT),RHDR3,NITHDR,IER)
      IF (IER.NE.0) GOTO 99
C     
      NITHDR = 17
      CALL QMODE(LSTRM(IUNIT),6,NCHHDR)
      CALL QREADI(LSTRM(IUNIT),IHDR4,NITHDR,IER)
      IF (IER.NE.0) GOTO 99
C     
      NITHDR = 1
      CALL QMODE(LSTRM(IUNIT),2,NCHHDR)
      CALL QREADR(LSTRM(IUNIT),RHDR4,NITHDR,IER)
      IF (IER.NE.0) GOTO 99
C     
      NITHDR = 1
      CALL QMODE(LSTRM(IUNIT),6,NCHHDR)
      CALL QREADI(LSTRM(IUNIT),IHDR5,NITHDR,IER)
      IF (IER.NE.0) GOTO 99
C     
C     dw---- Read labels as bytes
C     
      NITHDR = 800
      CALL QMODE(LSTRM(IUNIT),0,NCHHDR)
      CALL QREADI(LSTRM(IUNIT),IHDR6,NITHDR,IER)
      IF (IER.NE.0) GOTO 99
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
        WRITE (LUNOUT,FMT=6004) NC,NR,NS,MODE,NU1,NU2,NV1,NV2,NW1,
     +       NW2,NXYZ,CEL, (LXYZ(MAPCRS(I)),I=1,3)
        IF(MODE.NE.0) WRITE (LUNOUT,FMT=6006) AMIN,AMAX,AMEAN,ARMS
        WRITE (LUNOUT,FMT=6008) ISPG,NLAB,
     +       ((LABELS(I,J),I=1,20),J=1,NLAB)
      ENDIF
C---- Copy header information for return to calling routine
C     
C---- Convert integer title to characters
C     
      WRITE (TITLE,FMT=6010) (LABELS(I,1),I=1,20)
C     
      DO 10 I = 1,3
        IUVW(I) = MAPCRS(I)
 10   CONTINUE
      DO 20 I = 1,3
        MXYZ(I) = NXYZ(I)
 20   CONTINUE
      DO 30 I = 1,6
        CELL(I) = CEL(I)
 30   CONTINUE
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
C---- Get length of header in items (1024 bytes)
C     
      ITMHDR(IUNIT) = NBHDR*4/NCHITM(IUNIT)
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
C       diskio error:
 99   IF ( IFAIL .EQ. 0 ) THEN
        CALL CCPERR(1, '**MAP FILE HANDLING ERROR**')
      ELSE
        IFAIL = -1
        RETURN
      ENDIF

C
C_BEGIN_MRFNAM
C
      ENTRY MRFNAM(FNAME)
      ENTRY ccp4_map_get_last_read_filename(FNAME)
C     ===================
C
C---- Returns file name from last file open,
C     must be called after MRDHDR
C
C     FNAME (O)   file name of open file
C
C_END_MRFNAM
C
      FNAME = FILE
C
C---- Format statements
C
 6000 FORMAT ('  File name for input map file on unit',I4,' : ')
 6001 FORMAT (31X,'file size =',I8,'  ;  logical name  ')
 6002 FORMAT (' File name for input map file on unit',I4,' : ')
 6004 FORMAT (/11X,'Number of columns, rows, sections ',15 ('.'),3I5,
     +       /11X,'Map mode ',40 ('.'),I5,/11X,'Start and stop points ',
     +       'on columns, rows, sections ',6I5,/11X,'Grid sampling on ',
     +       'x, y, z ',24 ('.'),3I5,/11X,'Cell dimensions ',33 ('.'),
     +       6F10.5,/11X,'Fast, medium, slow axes ',25 ('.'),3 (4X,A1))
 6006 FORMAT (11X,'Minimum density ',33 ('.'),F12.5,/11X,'Maximum dens',
     +       'ity ',33 ('.'),F12.5,/11X,'Mean density ',36 ('.'),F12.5,
     +       /11X,'Rms deviation from mean density ',17 ('.'),F12.5)
 6008 FORMAT (11X,'Space-group ',37 ('.'),I5,/11X,'Number of titles ',
     +       32 ('.'),I5,//' Titles :',/10 (11X,20A4,/))
 6010 FORMAT (20A4)
 6012 FORMAT (/' **MRDHDR: UNIT NO. MUST BE 1 TO 12, =',I3,' **')
C
      END
C
C
C_BEGIN_MPOSN
C
      SUBROUTINE MPOSN(IUNIT,JSEC)
C     ============================
C
C---- This subroutine is used to set the position in the map  
C     file  so  that  the next section to be read is section JSEC.
C 
C---- Parameters:
C     ==========
C 
C  IUNIT (I)   Map stream number
C 
C  JSEC  (I)   Position the input map before section JSEC
C 
C_END_MPOSN
C
C      IMPLICIT NONE
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
      INTEGER LSEC,NREC
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
      ENTRY ccp4_map_read_position_section(IUNIT,JSEC)
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
      END
C
C
C_BEGIN_MRDLIN
C
      SUBROUTINE MRDLIN(IUNIT,X,IER)
C     =============================
C
C---- Read next line of map from stream IUNIT to array X.
C     Map is returned in same mode as on file, ie no data conversion
C     is done (but should be REAL)
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
C_END_MRDLIN
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
      EXTERNAL QREADR
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
      ENTRY ccp4_map_read_line_as_mode(IUNIT,X,IER)
C
C---- Size of section (elements)
C
      MB = NCS(IUNIT)
      CALL QREADR(LSTRM(IUNIT),X,MB,IER)
C
      END
C
C
C_BEGIN_MGULP
C
      SUBROUTINE MGULP(IUNIT,X,IER)
C     =============================
C
C---- Read next whole map section from stream IUNIT to array X.
C     Map is returned in same mode as on file, but should be REAL
C
C  IUNIT (I)   Map stream number
C 
C     X (O)   Array to contain the section of data read from the map
C 
C   IER (O)   Error flag =0, OK   non-zero, error or end of file
C
C_END_MGULP
C
C      IMPLICIT NONE
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
      EXTERNAL QREADR
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
      ENTRY ccp4_map_read_whole_section_as_mode(IUNIT,X,IER)
C
C---- Size of section (elements)
C
      MB = NCS(IUNIT)*NRS(IUNIT)
      CALL QREADR(LSTRM(IUNIT),X,MB,IER)
C
      END
C
C
C_BEGIN_MGULPR
C
      SUBROUTINE MGULPR(IUNIT,X,IER)
C     =============================
C
C---- Read next whole map section from stream IUNIT to array X.
C     For map modes other than 2, array is converted to real;
C     for complex maps (MODE = 3 or 4) the complex amplitude is
C     returned.
C
C  IUNIT (I)   Map stream number
C 
C     X (O)   Array to contain the section of data read from the map
C 
C   IER (O)   Error flag =0, OK   non-zero, error or end of file
C
C_END_MGULPR
C
C      IMPLICIT NONE
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
      EXTERNAL MODECV,QREADR
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
      ENTRY ccp4_map_read_whole_section_as_real(IUNIT,X,IER)
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
        CALL QREADR(LSTRM(IUNIT),RLINE,N,IER)
        IF (IER.NE.0) THEN
          RETURN
        ELSE
C
C---- We need to convert N elements from the file in RLINE
C     to real numbers in X. Subroutine MODECV is machine specific
C
          CALL MODECV(X(J),RLINE,N,MODEE)
C
          J = J + N
          IF (J.LT.M) GO TO 10
        END IF
C
      ELSE
C
C---- Mode real, just read
C
        CALL QREADR(LSTRM(IUNIT),X,M,IER)
      END IF
C
      END
C
C
C_BEGIN_MRCLOS
C
      SUBROUTINE MRCLOS(IUNIT)
C     ========================
C
C---- Close read file
C
C  IUNIT (I)   Map stream number
C
C_END_MRCLOS
C
C      IMPLICIT NONE
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
      ENTRY ccp4_map_read_close(IUNIT)
C
      CALL QCLOSE(LSTRM(IUNIT))
C
      END
C
C
C_BEGIN_MSYPUT
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
C     IST      (I)     Fortran stream number to use to read SYMOP library
C     LSPGRP   (I)     Spacegroup number
C     IUNIT    (I)     Map stream number
C
C_END_MSYPUT
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
      INTEGER I,IFAIL,ISG,LDUM,NBLIN,NCLIN,NLIN
C     ..
C     .. Local Arrays ..
      REAL JLINE(20)
C     ..
C     .. External Functions ..
      INTEGER NBYTXX
      EXTERNAL NBYTXX
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPDPN,QSEEK,QWRITR, CCPERR
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
      ENTRY ccp4_map_write_spgname(IST,LSPGRP,IUNIT)
C
      NCLIN = NBYTXX(20)
C
C---- Open symmetry file
C
      IFAIL = 0
      CALL CCPDPN(IST,'SYMOP','READONLY','F',LDUM,IFAIL)
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
C----   Skip NLIN lines
C       
        DO 20 I = 1,NLIN
          READ (IST,FMT=*)
 20     CONTINUE
        GO TO 10
      END IF
 30   WRITE (LUNOUT,FMT=6006) LSPGRP
 6006 FORMAT (/
     +     ' **MSYPUT: NO SYMMETRY INFORMATION FOR SPACE GROUP NUMBER',
     +     I4,' IN SYMOP FILE**')
      CALL CCPERR(1, '**SYMMETRY FILE ERROR**')
C     
C---- Space-group found, copy NLIN lines of symmetry
C     operators (NCLIN characters / line) to output file
C     
 40   CONTINUE
      DO 50 I = 1,NLIN
        READ (IST,FMT='(20A4)') JLINE
        CALL QWRITR(LSTRM(IUNIT),JLINE,NBLIN)
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
      END
C
C
C_BEGIN_MSYMOP
C
      SUBROUTINE MSYMOP(IUNIT,NSYM,ROT)
C     =================================
C
C---- Read symmetry operations from map file IUNIT
C     (after call to MRDHDR to read header).
C     Process symmetry in lines of length NBLIN characters
C     to convert to matrices and vectors.
C
C     IUNIT         (I)   Map stream number
C     NSYM          (O)   Number of symmetry operations
C     ROT(4,4,NSYM) (O)   rotation/translation matrices
C
C_END_MSYMOP
C
C      IMPLICIT NONE
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
      INTEGER I,IER,ISYMC,KMODE,N,NBLIN,NILINE,NLIN
      CHARACTER LINE*80
C     ..
C     .. External Functions ..
      INTEGER NBYTXX
      EXTERNAL NBYTXX
C     ..
C     .. External Subroutines ..
      EXTERNAL QSEEK,SYMFR2, QMODE, CCPERR, QREADC, QPRINT
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
      ENTRY ccp4_map_read_symm_matrix(IUNIT,NSYM,ROT)
C
      NBLIN = NBYTXX(20)
      NSYM = 0
C
C---- Exit if no symmetry
C
      IF (JSYMBT(IUNIT).LE.0) RETURN
C
C---- Position to start of symmetry block
C
        CALL QSEEK(LSTRM(IUNIT),2,1,ITMHDR(IUNIT))
C
C---- Total number of symmetry characters
C
        ISYMC = JSYMBT(IUNIT)
C
Cdw---- Number of items / line (BYTES)
C
        NILINE = NBLIN
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
          CALL QREADC(LSTRM(IUNIT),LINE(1:NILINE),IER)
          IF (IER.NE.0) CALL CCPERR(1,
     +   '**MSYMOP: ERROR READING SYMMETRY OPERATIONS FROM MAP FILE**')
C
C---- Convert to matrices
C
            N = NSYM
            CALL SYMFR2(LINE,1,NSYM,ROT)
            IF (NSYM.GE.N) THEN
C
C---- Print
C
              CALL QPRINT(1, ' Symmetry operations: '//LINE)
            END IF
   20   CONTINUE
C
Cdw---- Reset Mode, changing modes 10 & 11(12) to 0 & 2
C
        KMODE = MODES(IUNIT)
        IF (MODES(IUNIT).EQ.10) KMODE = 0
        IF (MODES(IUNIT).EQ.11 .OR. MODES(IUNIT).EQ.12) 
     +      KMODE = 2
        CALL QMODE(LSTRM(IUNIT),KMODE,NCHITM(IUNIT))
      END
C
C
C_BEGIN_MSYCPY
C
      SUBROUTINE MSYCPY(IN,IOUT)
C     ==========================
C
C---- Copy symmetry data from file IN to file IOUT
C     (after calls to MRDHDR & MWRHDR)
C
C     IN   (I)   Map stream number for input file
C     IOUT (I)   Map stream number for output file
C
C_END_MSYCPY
C
C      IMPLICIT NONE
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
      INTEGER ITMHDI,ITMS1I,JUNKI,JUNKI2,JUNKI3,JUNKI4,JUNKO,
     +        JUNKO2,LSTRM,NCHITI,MODEI
C     ..
C     .. Local Scalars ..
      INTEGER I,IER,NBLIN,NLIN
      CHARACTER LINE*80
C     ..
C     .. External Functions ..
      INTEGER NBYTXX
      EXTERNAL NBYTXX
C     ..
C     .. External Subroutines ..
      EXTERNAL QSEEK, CCPERR, QREADC, QWRITC
C     ..
C     .. Common blocks ..
      COMMON /MIHDR/JUNKI(22),ISGI,NBTI,JUNKI2(232),JUNKI3(12,3),
     +       MODEI(12),JUNKI4(12,4),NCHITI(12),ITMHDI(12),ITMS1I(12)
      COMMON /MOHDR/JUNKO(22),ISGO,NBTO,JUNKO2(232),NCHITO,ITMHDO,ITMS1O
      COMMON /MSTRM/LSTRM(12)
C     ..
C     .. Save statement ..
      SAVE /MSTRM/,/MIHDR/,/MOHDR/
C     ..
      ENTRY ccp4_map_copy_symmetry(IN,IOUT)
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
        DO 10 I = 1,NLIN
          CALL QREADC(LSTRM(IN),LINE(1:NBLIN),IER)
          IF (IER.NE.0) CALL CCPERR(1,
     +   '**MSYCPY: ERROR READING SYMMETRY OPERATIONS FROM MAP FILE**')
            CALL QWRITC(LSTRM(IOUT),LINE(1:NBLIN))
   10   CONTINUE
C
Cdw---- Rest Mode for input file
C----   changing modes 10 & 11(12) to 0 & 2
C     This probably isn't necessary now, since we didn't firkle with it
C     above.  (Old code did.)
CCC        KMODE = MODEI(IN)
CCC        IF (MODEI(IN).EQ.10) KMODE = 0
CCC        IF (MODEI(IN).EQ.11 .OR. MODEI(IN).EQ.12) KMODE = 2
CCC        CALL QMODE(LSTRM(IN),KMODE,NCHITI(IN))
      END IF
C
C---- Item count
C
      NBTO = NBTI
C
C---- Position of first section
C
      ITMS1O = NBTO/NCHITO + ITMHDO + 1
      END
C
C
C_BEGIN_MTTCPY
C
      SUBROUTINE MTTCPY(TITLE)
C     ========================
C
C---- Copy all titles from previously opened input and output files
C     adding title to end
C
C     TITLE   (I)     new title (character*80)
C
C_END_MTTCPY
C
C      IMPLICIT NONE
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
      SAVE /MIHDR/, /MOHDR/
C
      ENTRY ccp4_map_copy_title(TITLE)
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
      END
C
C
C_BEGIN_MTTREP
C
      SUBROUTINE MTTREP(TITLE,NT)
C     ===========================
C
C---- Replace NT'th title in output file (after MWRHDR)
C---- Add new title, if already 10, overwrite last one
C
C     TITLE    (I)    new title  (character*80)
C     NT       (I)    title number to replace
C
C_END_MTTREP
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
      SAVE /MOHDR/
      ENTRY ccp4_map_write_replace_title(TITLE,NT)
C
      NLABO = MAX(NLABO,NT)
      READ (TITLE,FMT=6000) (LABELO(I,NT),I=1,20)
C
C---- Format statements
C
 6000 FORMAT (20A4)
C
      END
C
C
C_BEGIN_MSKPUT
C
      SUBROUTINE MSKPUT(ASKWMT,ASKWTN)
C     ================================
C
C---- Put skew transformation into output common block
C
C     ASKWMT(3,3)    (I)    skew matrix S (S11, S12, etc)
C     ASKTRN(3)      (I)    skew translation t
C
C  Skew transformation from orthogonal atom frame to orthogonal map frame
C     Xo(map) = S * ( Xo(atoms) - t)
C
C!!! You probably shouldn't use this routine (Phil Evans, 9/93)
C
C_END_MSKPUT
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
      ENTRY ccp4_map_write_skew_info(ASKWMT,ASKWTN)
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
      END
C
C
C_BEGIN_MSKGET
C
      INTEGER FUNCTION MSKGET(ASKWMT,ASKWTN)
C     ======================================
C
C---- Get skew transformation from input common block
C!!! You probably shouldn't use this routine (Phil Evans, 9/93)
C
C     ASKWMT(3,3)    (I)    skew matrix S (S11, S12, etc)
C     ASKTRN(3)      (I)    skew translation t
C
C  Skew transformation from orthogonal atom frame to orthogonal map frame
C     Xo(map) = S * ( Xo(atoms) - t)
C
C_END_MSKGET
C      IMPLICIT NONE
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
      END
C
C
C
      SUBROUTINE MODECV(X,BLINE,N,MODE)
C     =================================
C
C---- Convert N items from BLINE in mode MODE to reals in X
C
C      IMPLICIT NONE
C     ..
C     .. Scalar Arguments ..
      INTEGER MODE,N
C     ..
C     .. Array Arguments ..
      REAL BLINE(*),X(*)
C     ..
C     .. Local Scalars ..
      REAL A,B,R
      INTEGER I,IFAIL,II,J,K
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPTOI, CCPERR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     ..
      ENTRY ccp4_map_mode_to_real(X,BLINE,N,MODE)
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
 20   CALL CCPERR(1, '**MODECV: CONVERSION'//
     +       ' OF BYTE OR INTEGER*2 TO INTEGER UNAVAILABLE**')
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
C      IMPLICIT NONE
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
      BYT = CCPBYT(NITM)
      NBYTXX = NWORD*NITM
C
      END
C
C
C_BEGIN_MSYWRT
C
      SUBROUTINE MSYWRT(IUNIT,NSYM,ROT)
C     ==================================
C
C     Write symmetry operators to map stream IUNIT
C     Note that the symmetry operators are written to the file one per line
C     and may have a different format to those in the SYMOP file
C
C     IUNIT    (I)     Map stream number
C     NSYM     (I)     Number of symmetry operators
C     ROT(4,4,NSYM)  (I)  rotation/translation matrices
C
C_END_MSYWRT
C
C---- Map header common
C
C     .. Parameters ..
      INTEGER LUNOUT, MAXSYM
      PARAMETER (LUNOUT=6, MAXSYM=192)
C     ..
C     .. Arguments ..
      INTEGER IUNIT,NSYM
      REAL ROT(4,4,NSYM)
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
      INTEGER I,IPRINT,NLIN
C     ..
C     .. Local Arrays ..
      CHARACTER*80   SYMOPS(MAXSYM)
C     ..
C     .. External Subroutines ..
      EXTERNAL QSEEK,QWRITC, CCPERR, SYMTR3
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
      ENTRY ccp4_map_write_symm_matrix(IUNIT,NSYM,ROT)
C
      IF (NSYM .LE. 0 .OR. NSYM .GT. MAXSYM) THEN
         WRITE (LUNOUT, '(/A,I8/)')
     $      ' *** Too many or too few symmetry operations: ', NSYM
         CALL CCPERR(1, '*** Illegal number of symmetry operations ***')
      ENDIF
C
C---- Convert symmetry to character strings, one per line (array element)
      IPRINT = 0
      CALL SYMTR3(NSYM, ROT, SYMOPS, IPRINT)
C     
C---- Position map file to before symmetry operators
C     
      CALL QSEEK(LSTRM(IUNIT),2,1,ITMHDR)
C     
C---- Copy NLIN lines of symmetry
C     operators (80 characters / line) to output file
C     
      NLIN = NSYM
      DO 50 I = 1,NLIN
        CALL QWRITC(LSTRM(IUNIT),SYMOPS(I))
 50   CONTINUE
C     
C---- Number of characters of symmetry information
C     
      NSYMBT = NLIN*80
C     
C---- Position of first section
C     
      ITMSC1 = NSYMBT/NCHITM + ITMHDR + 1
C     
      END
