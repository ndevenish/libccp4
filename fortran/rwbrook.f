C**************************************************************************
C
C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
C**************************************************************************
C
C
C
      SUBROUTINE XYZINIT()
C     ====================
C
C_BEGIN_XYZINIT
C
C	This subroutine initialises the common block RBRKAA ready for reading
C and writing coordinate files. Also, the common blocks associated with 
C storing the header information are initialised. It should be called only 
C once from the top of the program.
C
C Parameters:
C
C       NONE
C
C COMMONS:
C
C         /RBRKAA/ FILESOPEN,LOGUNIT(MAXFILESOPEN),UNIT(MAXFILESOPEN),
C                  TYPE(MAXFILESOPEN)
C
C           FILESOPEN       no. of current coordinate files open.
C           LOGUNIT          logical name of file
C           UNIT            if the file is PDB then the unit is the physical
C                           channel opened. If CIF then is related to blocks.
C           TYPE            indicates whether PDB (1,-1) or CIF (2,-2). If
C                           negative then file is output file.
C
C_END_XYZINIT
C
C     .. Parameters ..
      INTEGER MAXFILESOPEN
      PARAMETER (MAXFILESOPEN=10)
C     ..
C     .. Variables in Common ..
      REAL CELL,CELLAS,RF,RO,RR,VOL
      INTEGER FILESOPEN,ITYP,NCODE,TYPE,UNIT
      CHARACTER LOGUNIT*80,BROOK*1,WBROOK*1,WBROOK1*1,BRKSPGRP*11
      LOGICAL IFCRYS,IFHDOUT,IFSCAL,MATRIX
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
C     .. Common Blocks ..
      COMMON /RBRKAA/ FILESOPEN,LOGUNIT(MAXFILESOPEN),
     +                UNIT(MAXFILESOPEN),TYPE(MAXFILESOPEN)
      COMMON /RBRKXX/ IFCRYS,IFSCAL,ITYP,MATRIX,IFHDOUT
      COMMON /RBRKYY/ BROOK(80),WBROOK(80),WBROOK1(80)
      COMMON /RBRKZZ/ CELL(6),RR(3,3,6),VOL,CELLAS(6)
      COMMON /ORTHOG/ RO(4,4),RF(4,4),NCODE
      COMMON /ORTHOGU/ ROU(4,4),RFU(4,4)
      COMMON /RBRKSPGRP/BRKSPGRP
C     ..
C     .. Save ..
      SAVE /RBRKAA/,/RBRKXX/,/RBRKYY/,/RBRKZZ/,/ORTHOG/,/ORTHOGU/
C     ..

      FILESOPEN = 0

      DO 10 I=1,MAXFILESOPEN
        LOGUNIT(I) = ' '
        UNIT(I) = 0
        TYPE(I) = 0
   10 CONTINUE

      DO 20 I=1,6
        CELL(I) = 0.0
        CELLAS(I) = 0.0
   20 CONTINUE

      IFCRYS=.FALSE.
      IFSCAL=.FALSE.
      MATRIX=.FALSE.
      IFHDOUT=.FALSE.
      NCODE=0
      ITYP=0
C
C
      DO 30 I=1,3
        DO 40 J=I+1,4
          RO(I,J)=0.0
          RO(J,I)=0.0
          RF(I,J)=0.0
          RF(J,I)=0.0
          ROU(I,J)=0.0
          ROU(J,I)=0.0
          RFU(I,J)=0.0
          RFU(J,I)=0.0
40      CONTINUE
        RO(I,I)=1.0
        RF(I,I)=1.0
        ROU(I,I)=1.0
        RFU(I,I)=1.0
30    CONTINUE
C
C
      RO(4,4)=1.0
      RF(4,4)=1.0
      ROU(4,4)=1.0
      RFU(4,4)=1.0

      DO 50 I=1,80
        BROOK(I) = ' '
        WBROOK(I) = ' '
        WBROOK1(I) = ' '
50    CONTINUE
      BRKSPGRP = ' '

      RETURN
      END
C
C
C
      SUBROUTINE RBINIT(IUNIT)
C     ========================
C
C_BEGIN_RBINIT
C
C      This routine is obsolete and should be removed.
C
C_END_RBINIT
C
C     .. Parameters ..
      INTEGER MAXFILESOPEN
      PARAMETER (MAXFILESOPEN=10)
C     ..
C     .. Scalar Arguments ..
      INTEGER IUNIT
C     ..
C     .. Local Scalars ..
      INTEGER I,J
C     ..
C     .. Scalars in Common ..
      INTEGER FILESOPEN,NCODE,ITYP
      LOGICAL IFHDOUT,IFCRYS,IFSCAL,MATRIX
C     ..
C     .. Arrays in Common ..
      REAL RF,RO
      INTEGER UNIT,TYPE
      CHARACTER LOGUNIT*80
C     ..
C     .. External Routines ..
      EXTERNAL XYZINIT,XYZREWD
C     ..
C     .. Common Blocks ..
      COMMON /RBRKAA/ FILESOPEN,LOGUNIT(MAXFILESOPEN),
     +                UNIT(MAXFILESOPEN),TYPE(MAXFILESOPEN)
      COMMON /RBRKXX/IFCRYS,IFSCAL,ITYP,MATRIX,IFHDOUT
      COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE
      COMMON /ORTHOGU/ ROU(4,4),RFU(4,4)
C     ..
C     .. Save Statement ..
      SAVE /RBRKAA/,/RBRKXX/,/ORTHOG/
C     ..
C
C---- Make sure that XYZINIT is only called once. However, this is not
C     fool proof
C
      DO 10 I=1,10
        IF (IUNIT .EQ. UNIT(I)) GOTO 20
   10 CONTINUE

      CALL XYZINIT

   20 CALL XYZREWD(IUNIT)
      IFCRYS=.FALSE.
      IFSCAL=.FALSE.
      MATRIX=.FALSE.
      NCODE=0
      ITYP=0
      IBRKFL=0
C
C
      DO 30 I=1,3
        DO 40 J=I+1,4
          RO(I,J)=0.0
          RO(J,I)=0.0
          RF(I,J)=0.0
          RF(J,I)=0.0
          ROU(I,J)=0.0
          ROU(J,I)=0.0
          RFU(I,J)=0.0
          RFU(J,I)=0.0
   40   CONTINUE
        RO(I,I)=1.0
        RF(I,I)=1.0
        ROU(I,I)=1.0
        RFU(I,I)=1.0
   30 CONTINUE
      RO(4,4)=1.0
      RF(4,4)=1.0
      ROU(4,4)=1.0
      RFU(4,4)=1.0


      RETURN
      END
C
C
C
      SUBROUTINE XYZOPEN(LOGNAM,RWSTAT,FILTYP,IUNIT,IFAIL)
C     =====================================================
C
C_BEGIN_XYZOPEN
C
C      Opens a coordinate file for input or output. The channel number can
C be determined automatically or set on input. The header info.
C is also read: cell, orthog. matrix and symmetry.
C
C Parameters:
C
C         LOGNAM (I)   CHARACTER*(*) but maximum of eight? The logical unit 
C                                    to which the file is assigned
C         RWSTAT (I)   CHARACTER*(*) if 'INPUT' then file is readonly
C                                    if 'OUTPUT' then file is an output file.
C         FILTYP (I)   CHARACTER*(*) if 'CIF' then the file type is treated as
C                                    CIF. If 'PDB' then the file type is 
C                                    treated as PDB. If blank then file type is
C                                    automatically determined for input files 
C                                    and for output file the file type will be
C                                    the same as the first file opened or 
C                                    defaulted to PDB.
C         IUNIT  (I/O) INTEGER       If zero then unit is decided else
C		  	             file opened on that unit
C                                    checked against previous data if 
C                                    applicable. NOT used with output files.
C         IFAIL  (I/O) INTEGER       On input    = 0 stop on failure 
C                                                = 1 continue on failure
C
C                                    On output   unchanged if OK
C                                                = -1  if error
C
C
C_END_XYZOPEN
C
C   
      implicit none
C     .. Parameters ..
      INTEGER MAXFILESOPEN,MAXSYM
      PARAMETER (MAXFILESOPEN=10,MAXSYM=96)
C     ..
C     .. Arguments ..
      INTEGER IFAIL,IUNIT,II,JJ,K,ISYM
      REAL AM,BM,RCHK1,RCHK2,FAC
      CHARACTER*(*) FILTYP,LOGNAM,RWSTAT
C     ..
C     .. Variables in Common ..
      REAL CELL,CELLAS,RF,RO,RR,VOL,ROU,RFU
      INTEGER FILESOPEN,ITYP,NCODE,TYPE,UNIT
      CHARACTER*80 LOGUNIT,BROOK*1,WBROOK*1,WBROOK1*1, BRKSPGRP*11
      LOGICAL IFCRYS,IFHDOUT,IFSCAL,MATRIX
c  Check symmetry stuff
C
      integer  nsymchk,lspgrp,nsymppdbs,nsympdbs,ist
      real rsymchk(4,4,maxsym), rsympdbs(4,4,maxsym)
      character SPGRPpdb*11,NAMSPG_CIFS*20,nampg*10
C     ..
C     .. Local Scalars ..
      REAL ERROR,VOLCHK
      INTEGER I,IORTH,IFILTYP,J,LL,ILEN,KLEN
      CHARACTER BROOKA*80,ERRLIN*600,FILNAM*255,IE*2,IRTYPE*4
      CHARACTER LFILTYP*3,LRWSTAT*5, SPGCHK*30
      CHARACTER*40 ORTH(6)
C     ..
C     .. Local Arrays ..
      REAL P(4,4)
      CHARACTER IEC(3)*2
C     ..
C     .. External Functions ..
      INTEGER LENSTR,CCPNUN
      LOGICAL CCPEXS
      EXTERNAL CCPEXS,LENSTR,CCPNUN
C     ..
C     .. External Routines ..
      EXTERNAL CCPDPN,CCPERR,CCPUPC,RBFROR,RBRINV,UGTENV
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     ..
C     .. Common Blocks ..
      COMMON /RBRKAA/ FILESOPEN,LOGUNIT(MAXFILESOPEN),
     +                UNIT(MAXFILESOPEN),TYPE(MAXFILESOPEN)
      COMMON /RBRKXX/IFCRYS,IFSCAL,ITYP,MATRIX,IFHDOUT
      COMMON /RBRKYY/BROOK(80),WBROOK(80),WBROOK1(80)
      COMMON /RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
      COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE
      COMMON /ORTHOGU/ ROU(4,4),RFU(4,4)
      COMMON /RBRKSPGRP/BRKSPGRP
C     ..
C     .. Equivalences ..
      EQUIVALENCE (IRTYPE,BROOK(1)),(IE,BROOK(5)),(BROOKA,BROOK(1))
C     ..
C     .. Save ..
      SAVE /RBRKAA/,/RBRKXX/,/RBRKYY/,/RBRKZZ/,/ORTHOG/
C     ..
C     .. Data Statement ..
      DATA IEC /'E1','E2','E3'/
      DATA ORTH/'A // XO, C* // ZO (STANDARD PDB)',
     *          'B // XO, A* // ZO',
     *          'C // XO, B* // ZO',
     *          'HEX A+B // XO, C* // ZO',
     *          'A* // XO, C // ZO (ROLLETT)',
     *          'A // XO, B* // YO'/
C     ..
      I = 0
      II = 0
      IFILTYP = 1
      LL = 0
      LRWSTAT = RWSTAT
      LFILTYP = FILTYP
      CALL CCPUPC(LFILTYP)
      CALL CCPUPC(LRWSTAT)
C
C---- If too many files opened
C
      IF (FILESOPEN .EQ. MAXFILESOPEN) THEN
        CALL CCPERR(1,' *** ERROR: too many coordinate files open. ***')
      ENDIF
C
C==== If the file is an INPUT file
C
      IF (LRWSTAT(1:5) .EQ. 'INPUT') THEN
C
C---- Check if file exists
C
        IF (.NOT.CCPEXS(LOGNAM)) THEN
          CALL UGTENV(LOGNAM,FILNAM)
          ERRLIN = ' ERROR: '//LOGNAM//':'//FILNAM(1:LENSTR(FILNAM))//
     .                                        ' does not exist'
          IF (IFAIL .EQ. 0) THEN
            CALL CCPERR(1,ERRLIN)
          ELSE
            CALL CCPERR(2,ERRLIN)
            IFAIL = -1
            GOTO 1000
          ENDIF
        ENDIF
C
C---- Determine whether CIF or PDB
C
        IF (LFILTYP(1:1) .EQ. ' ') THEN
          CALL CCPDPN(IUNIT,LOGNAM,'READONLY','F',LL,IFAIL)
c          IF (IFAIL .LT. 0) GOTO 1000
c   10     READ (I,FMT='(A)',END=20) CHAR
c          IF (CHAR .EQ. '_') THEN
c            IFILTYP = 2
c            CLOSE(UNIT=I,STATUS='KEEP')
c            GOTO 30
c          ENDIF
c          GOTO 10
          

   20     FILESOPEN = FILESOPEN + 1
          LOGUNIT(FILESOPEN) = LOGNAM
          UNIT(FILESOPEN) = IUNIT
          TYPE(FILESOPEN) = 1
          REWIND IUNIT
        ENDIF  
C
C---- If known as CIF
C
   30   IF (LFILTYP(1:3).EQ.'CIF' .OR. IFILTYP.EQ.2) THEN
C  Not yet ready
          FILESOPEN = FILESOPEN + 1
          LOGUNIT(FILESOPEN) = LOGNAM
          TYPE(FILESOPEN) = 2
        ENDIF
C
C---- If known as a PDB file
C
        IF (LFILTYP(1:3).EQ.'PDB') THEN
          CALL CCPDPN(IUNIT,LOGNAM,'READONLY','F',LL,IFAIL)
          IF (IFAIL .LT. 0) GOTO 1000
          FILESOPEN = FILESOPEN + 1
          LOGUNIT(FILESOPEN) = LOGNAM
          UNIT(FILESOPEN) = IUNIT
          TYPE(FILESOPEN) = 1
        ENDIF
C
C---- Read PDB header info.
C
        IF (TYPE(FILESOPEN) .EQ. 1) THEN
          NSYMCHK=0
   40     READ(UNIT(FILESOPEN),FMT='(80A1)',END=1000)BROOK
C
C---- Read symmetry operators from REMARK 290 lines:
C---123456789012345678901234567890
C---REMARK 290      SYMOP   SYMMETRY
C---REMARK 290     NNNMMM   OPERATOR
C---REMARK 290       1555   X,Y,Z
C---REMARK 290       2555   1/2-X,-Y,1/2+Z
C---REMARK 290       3555   -X,1/2+Y,1/2-Z
C---REMARK 290       4555   1/2+X,1/2-Y,-Z
          IF(BROOKA(1:10) .EQ.'REMARK 290') then
           IF(BROOKA(19:21).EQ.'555')THEN 
             READ(BROOKA(11:18),'(I8)')NSYMCHK
             CALL  symfr2 (BROOKA,22,nsymchk,rsymchk)
              write(6,'(a,i3,4(/,4f10.3))')' remark 290',nsymchk,
     + ((rsymchk(ii,jj,nsymchk),ii=1,4),jj=1,4)
           ENDIF
          ENDIF
C
C
C---- Escape if ATOM card found
C
          IF (IRTYPE.EQ.'ATOM') THEN
C   Check possible symmetry first..
         if(nsymchk.gt.0 .and.nsympdbs .gt.0) then
           DO isym = 1,nsympdbs
           AM=
     *      Rsymchk(1,1,isym)*(Rsymchk(2,2,isym)*Rsymchk(3,3,isym)
     *                        -Rsymchk(2,3,isym)*Rsymchk(3,2,isym))
     *     +Rsymchk(1,2,isym)*(Rsymchk(2,3,isym)*Rsymchk(3,1,isym)
     *                        -Rsymchk(2,1,isym)*Rsymchk(3,3,isym))
     *     +Rsymchk(1,3,isym)*(Rsymchk(2,1,isym)*Rsymchk(3,2,isym)
     *                        -Rsymchk(2,2,isym)*Rsymchk(3,1,isym))
             IF( abs(AM).GT.0.05) then
               BM = 0
                do i = 1, 4
                  do j = 1,4
                   BM = BM + abs(Rsymchk(i,j,isym) - rsympdbs(i,j,isym))
                  enddo
                enddo
                if (BM .gt.0.1) then
                  WRITE(6,'(a,i4,a,2(/a,4(/,4f10.3)))')
     +            ' Symmetry operators',isym,
     +            ' from symop.lib and remark 290 inputdo not match:',
     +            ' Remark 290',
     +            ((Rsymchk(i,j,isym),i=1,4),j=1,4) ,
     +            ' symop.lib',
     +            ((Rsympdbs(i,j,isym),i=1,4),j=1,4)
                  call ccperr(1,' Problem with symmetry match')
                endif
             endIF
           ENDDO
         endif
            GOTO 1000
C
C---- Cell card found - calculate standard orthogonalising matrix
C     Check if you already have a cell which is inconsistent with 
C     this one
C
          ELSE IF (IRTYPE.EQ.'CRYS') THEN
            ITYP=1
            IFCRYS=.TRUE.
            BRKSPGRP = ' '
            READ(BROOKA,FMT='(6X,3F9.3,3F7.2,1x,a11)')CELL,BRKSPGRP
C
C Make sure that BRKSPGRP, the Space group name is left-justified
C
            SPGCHK =BROOKA(55:80)
            ILEN = LENSTR(SPGCHK)
             IF (ILEN.LE.1) THEN
               CALL CCPERR(2,' No Space group given on PDB CRYST1 line')
             ELSE
               KLEN = ILEN
C
               DO J = 1,ILEN-1
                 IF (SPGCHK(1:1) .EQ. ' ') THEN
                   SPGCHK = SPGCHK(2:KLEN)
                   KLEN  = KLEN - 1
                 END IF
               END DO
               BRKSPGRP = SPGCHK
             END IF 
C  Read symmetry operators..
             IF (BRKSPGRP.NE.' ') THEN
               IST = CCPNUN()
               call MSYMLB3(IST,LSPGRP,BRKSPGRP,NAMSPG_CIFS,
     +                   NAMPG,NSYMPpdbs,NSYMpdbs,RSYMpdbs)
             END IF
            CALL RBFROR
            IF(NCODE.EQ.0)NCODE=1

            DO 60 I=1,3
              DO 60 J=1,3
                RO(I,J)=RR(I,J,NCODE)
   60       CONTINUE

            RO(4,4)=1.0
            CALL RBRINV(RO,RF)
            MATRIX=.TRUE.

            CALL CCPERR(4,
     +          ' MATRICES DERIVED FROM CRYST1 CARD IN COORDINATE FILE')
            CALL CCPERR(4,' ')
            CALL CCPERR(4,' ')
            CALL CCPERR(4,
     +             '            RF                                  RO')
            CALL CCPERR(4,' ')
            DO 70 I=1,4
              WRITE(ERRLIN,FMT='(1X,4F8.3,5X,4F8.3)')(RF(I,J),J=1,4),
     +                                           (RO(I,K),K=1,4)
              CALL CCPERR(4,ERRLIN)
   70       CONTINUE
            CALL CCPERR(4,' ')
            GO TO 40
C
C---- Scale cards - extract and calculate rotation and trans matrices
C
          ELSE IF (IRTYPE.EQ.'SCAL') THEN
            ITYP=2
            IFSCAL = .TRUE.
            MATRIX=.FALSE.
            DO 80 I=1,3
              IF(IE.NE.IEC(I))GO TO 80
              READ(BROOKA,FMT='(10X,3F10.6,5X,F10.6)')
     +                              P(I,1),P(I,2),P(I,3),P(I,4)
              II = II + 1
              GO TO 90
   80       CONTINUE

   90       IF(II.NE.3)GO TO 40
            MATRIX=.TRUE.
            DO 100 I=1,3
              DO 100 J=1,4
                RF(I,J)=P(I,J)
  100       CONTINUE
C
C---- Find orthogonalisation type
C
            CALL RBRINV(RF,RO)
            VOLCHK = RO(1,1)*(RO(2,2)*RO(3,3) - RO(2,3)*RO(3,2))
     +             + RO(1,2)*(RO(2,3)*RO(3,1) - RO(2,1)*RO(3,3))
     +             + RO(1,3)*(RO(2,1)*RO(3,2) - RO(2,2)*RO(3,1))

            IF (VOL.GT.0.0) THEN
              ERROR = ABS(VOLCHK - VOL) /VOL
              IF (ERROR .GT. 0.02) then
                WRITE (ERRLIN,'(A,F15.4)')
     +          ' Unit cell volume generated from SCALEi cards', VOLCHK
                CALL CCPERR(2,ERRLIN)
                WRITE (ERRLIN,'(A,F15.4)')
     +          ' Percentage error is                        ', ERROR
                CALL CCPERR(2,ERRLIN)
              END IF

              IF (ERROR .GT. 0.1.AND.IFAIL.EQ.0) call ccperr(1,
     +    'Error in rwbrook.f - disagreement between cell and PDB file')
            ELSE
              WRITE (ERRLIN,'(A)')
     +        ' No unit cell volume currently stored'
              CALL CCPERR(2,ERRLIN)
            END IF

            DO 110 IORTH=1,6
              DO 120 I=1,3
                DO 120 J=1,3
                RCHK1 = (RO(I,J)+RR(I,J,IORTH))
                RCHK2 = (RO(I,J)-RR(I,J,IORTH))
                IF(ABS(RCHK1).LT.0.1) GO TO 120
                IF(ABS(RCHK2/RCHK1) .GT.0.01)GO TO 110
  120         CONTINUE
              GO TO 130
  110       CONTINUE
            IORTH=0
  130       NCODE=IORTH
C
C---- Correct inaccuracy of SCALEi input due to FORMAT, replace RF,RO 
C     with RR(...,NCODE) if possible.
C
            IF(NCODE.GT.0) THEN
              DO 140 I = 1,3
                DO 140 J = 1,3
                  RO(I,J) = RR(I,J,NCODE)
  140         CONTINUE
              CALL RBRINV(RO,RF)
            END IF

            CALL CCPERR(4,' ')
            CALL CCPERR(4,
     +        ' MATRICES DERIVED FROM SCALE CARDS IN COORDINATE FILE')
            CALL CCPERR(4,' ')
            CALL CCPERR(4,' ')
            CALL CCPERR(4,
     +          '            RF                                   RO')
            CALL CCPERR(4,' ')
            DO 150 I=1,4
              WRITE(ERRLIN,FMT='(1X,4F8.3,5X,4F8.3)')(RF(I,J),J=1,4),
     +                                           (RO(I,K),K=1,4)
              CALL CCPERR(4,ERRLIN)
  150       CONTINUE
            CALL CCPERR(4,' ')

            IF(NCODE.EQ.0) THEN
              Write(ERRLIN,'(A,I3)') ' Warning - No ORTH code',NCODE
              CALL CCPERR(2,ERRLIN)
            ENDIF
            IF (NCODE .GT. 0) THEN
              CALL CCPERR(4,' ')
              WRITE(ERRLIN,FMT='(A,I3,A)')'  ORTHOGONALISATION CODE: ',
     +            NCODE,  ORTH(NCODE)
              CALL CCPERR(4,ERRLIN)
              CALL CCPERR(4,' ')
            ENDIF

            IF (P(1,4).NE.0 .OR. P(2,4).NE.0 .OR. P(3,4).NE.0) THEN
              CALL CCPERR(4,' ')
              CALL CCPERR(4,' TRANSLATIONS ALSO SPECIFIED')
              CALL CCPERR(4,' ')
            ENDIF
            GO TO 1000
          ELSE
            GOTO 40
          ENDIF
        ENDIF
C
C==== If the file is an OUTPUT file
C
      ELSE

        IFILTYP = 1
        IF (LFILTYP(1:1).EQ.' ' .AND. FILESOPEN.GT.1) THEN
          IF (ABS(TYPE(1)).EQ.1 .OR. ABS(TYPE(1)).EQ.2) THEN
            IFILTYP = ABS(TYPE(1))
          ELSE
            IFILTYP = 1
          ENDIF
        ENDIF

        IF (LFILTYP(1:3) .EQ. 'CIF') IFILTYP = 2 
        IF (LFILTYP(1:3) .EQ. 'PDB') IFILTYP = 1
C
C---- Open output PDB file
C
        IF (IFILTYP .EQ. 1) THEN
          CALL CCPDPN(IUNIT,LOGNAM,'NEW','F',LL,IFAIL)
          IF (IFAIL .LT. 0) GOTO 1000
          FILESOPEN = FILESOPEN + 1
          LOGUNIT(FILESOPEN) = LOGNAM
          UNIT(FILESOPEN) = IUNIT
          TYPE(FILESOPEN) = -1
        ENDIF

        IF (IFILTYP .EQ. 2) THEN
C Not yet ready
          FILESOPEN = FILESOPEN + 1
          LOGUNIT(FILESOPEN) = LOGNAM
          TYPE(FILESOPEN) = -2
        ENDIF

      ENDIF

 1000 IF (TYPE(FILESOPEN) .EQ. 1) REWIND UNIT(FILESOPEN)
C     Generate ROU and RFU for AnisoU stuff
       IF (MATRIX) THEN
         RFU(4,4) = 1.0
         DO I = 1,3
         FAC= SQRT(RF(I,1)*RF(I,1) + RF(I,2)*RF(I,2) +
     +              RF(I,3)*RF(I,3))
         RFU(I,1) = RF(I,1)/FAC
         RFU(I,2) = RF(I,2)/FAC
         RFU(I,3) = RF(I,3)/FAC
         RFU(I,4) = 0.0
         RFU(4,I) = 0.0
         END DO 
         CALL RBRINV(RFU,ROU)
       END IF 
C     If reading in: check SCAL and CRYST1 cards
      IF (IFILTYP.EQ.1 .AND. LRWSTAT(1:5).EQ.'INPUT') THEN
        IF (.NOT.IFCRYS) THEN
          WRITE(ERRLIN,FMT='(A,A)') ' NO CRYST CARDS READ FROM ',LOGNAM
          CALL CCPERR (2,ERRLIN)
        END IF
        IF (.NOT.IFSCAL) THEN
          WRITE(ERRLIN,FMT='(A,A)') ' NO SCALE CARDS READ FROM ',LOGNAM
          CALL CCPERR (2,ERRLIN)
        END IF
      END IF
      RETURN
      END
C
C
C
      SUBROUTINE XYZCLOSE(IUNIT)
C     ==========================
C
C_BEGIN_XYZCLOSE
C
C	This subroutine closes a coordinate file. 
C
C Parameters:
C
C         IUNIT  (I)   INTEGER    Unit number for file
C
C_END_XYZCLOSE
C
C     .. Parameters ..
      INTEGER MAXFILESOPEN
      PARAMETER (MAXFILESOPEN=10)
C     ..
C     .. Arguments ..
      INTEGER IUNIT
C     ..
C     .. Variables in Common ..
      INTEGER FILESOPEN, UNIT, TYPE
      CHARACTER*80 LOGUNIT
C     ..
C     .. Local Scalars ..
      INTEGER I,II
C     ..
C     .. Common Blocks ..
      COMMON /RBRKAA/ FILESOPEN,LOGUNIT(MAXFILESOPEN),
     +                UNIT(MAXFILESOPEN),TYPE(MAXFILESOPEN)
C     ..
C     .. Save ..
      SAVE /RBRKAA/
C     ..
C
      II = 0

      DO 10 I=1,FILESOPEN
        IF (UNIT(I) .EQ. IUNIT) THEN
          II = I
          GOTO  20
        ENDIF
   10 CONTINUE

   20 IF (II .NE. 0) THEN
        IF (TYPE(II) .EQ. -1) CALL WREMARK(IUNIT,'END')
        CLOSE(UNIT=IUNIT,STATUS='KEEP')

        IF (FILESOPEN.NE.1 .AND. II.NE.FILESOPEN) THEN
          LOGUNIT(II) = LOGUNIT(FILESOPEN)
          UNIT(II) = UNIT(FILESOPEN)
          TYPE(II) = TYPE(FILESOPEN)
        ENDIF
        FILESOPEN = FILESOPEN - 1
      ENDIF

      RETURN
      END
C
C
C
      SUBROUTINE XYZADVANCE(IUNIT,IOUT,ITER,*,*)
C     ==========================================
C
C_BEGIN_XYZADVANCE
C
C When IUNIT represents an input coordinate file, this subroutine reads 
C recognised data lines into a buffer BROOK, from which XYZATOM and 
C XYZCOORD can extract useful information. Optionally, if the card is 
C unrecognised (eg REMARK) then the line can be echoed to an output file.
C
C When IUNIT represents an output coordinate file, this subroutine writes 
C out the contents of the buffer WBROOK or WBROOK1. This buffer is filled
C from an input file, or by calls to XYZATOM and XYZCOORD.
C
C Parameters:
C
C      IUNIT  (I) Channel number of the coordinate file
C
C      These arguments are not relevant for output files.
C        IOUT (I) Logical unit number to which non-atom/hetatm/anisou records 
C                 are to be written (may be blank if reading only)
C        ITER (I) FLAG =1, return if 'ter' card found (via return 1)
C                      =0, do not return when 'ter' card found
C      RETURN 1   Return on TER card if ITER=1
C      RETURN 2   Return on end of file.
C
C_END_XYZADVANCE
C
C     .. Paramters ..
      INTEGER MAXFILESOPEN
      PARAMETER (MAXFILESOPEN=10)
C     ..
C     .. Arguments ..
      INTEGER IOUT,ITER,IUNIT
C     ..
C     .. Variables in Common ..
      REAL CELL,CELLAS,RF,RO,RR,VOL
      INTEGER FILESOPEN,NCODE,TYPE,UNIT
      CHARACTER LOGUNIT*80,BROOK*1,WBROOK*1,WBROOK1*1,BRKSPGRP*11
      LOGICAL IFCRYS,IFHDOUT,IFSCAL,MATRIX
C     ..
C     .. Local Variables ..
      INTEGER I,II
      CHARACTER*80 ERRLIN,BROOKA,BROOKB,BROOKC
      CHARACTER*6 ITYPE(7)
C     ..
C     .. External Routines ..
      EXTERNAL CCPERR,WREMARK
C     ..
C     .. Common Blocks ..
      COMMON /RBRKAA/ FILESOPEN,LOGUNIT(MAXFILESOPEN),
     +                UNIT(MAXFILESOPEN),TYPE(MAXFILESOPEN)
      COMMON /RBRKXX/ IFCRYS,IFSCAL,ITYP,MATRIX,IFHDOUT
      COMMON /RBRKYY/ BROOK(80),WBROOK(80),WBROOK1(80)
      COMMON /RBRKZZ/ CELL(6),RR(3,3,6),VOL,CELLAS(6)
      COMMON /ORTHOG/ RO(4,4),RF(4,4),NCODE
      COMMON /ORTHOGU/ ROU(4,4),RFU(4,4)
      COMMON /RBRKSPGRP/BRKSPGRP
C     ..
C     .. Equivalences ..
      EQUIVALENCE (BROOKA,BROOK(1)),(BROOKB,WBROOK1(1)),
     +            (BROOKC,WBROOK(1))
C     ..
C     .. Save Statement ..
      SAVE /RBRKAA/,/RBRKXX/,/RBRKYY/,/RBRKZZ/,/ORTHOG/
C     ..
C     .. Data Statements ..
      DATA ITYPE/'CRYST1','SCALE ','TER   ','ATOM  ','HETATM',
     +           'ANISOU','END   '/
C     ..
      II = 0
      DO 10 I=1,FILESOPEN
        IF (IUNIT .EQ. UNIT(I)) THEN
          II = I
          GOTO 15
        ENDIF
   10 CONTINUE

      ERRLIN = ' ERROR: in XYZADVANCE file has not been opened'
      CALL CCPERR(1,ERRLIN)
C
C==== If input PDB file
C      
   15 IF (TYPE(II) .EQ. 1) THEN
   20   READ (UNIT(II),FMT='(80A1)',END=100) BROOK
        DO 30 I=1,7
          IF (ITYPE(I)(1:4) .EQ. BROOKA(1:4)) THEN
            ITYP = I
            GOTO 40
          ENDIF
   30   CONTINUE
        IF (IOUT.GT.0) CALL WREMARK(IOUT,BROOKA)
        GOTO 20
   40   IF (BROOKA(1:4).EQ.'CRYS' .OR. BROOKA(1:4).EQ.'SCAL') THEN
          GOTO 20
        ELSE IF (BROOKA(1:3) .EQ. 'END') THEN
          ITYP = 0
          RETURN 2
        ELSE IF (BROOKA(1:3) .EQ. 'TER') THEN
          IF (ITER .EQ. 0) THEN
            IF (IOUT .NE. 0) CALL WREMARK(IOUT,BROOKA)
            GOTO 20
          ELSE
            RETURN 1
          ENDIF
        ENDIF
C
C---- Put input buffer into output buffer as some cards maybe just be 
C     written out as is.
C
        IF (ITYP.EQ.4 .OR. ITYP.EQ.5) THEN
          DO 50 I=1,80
            WBROOK(I) = BROOK(I)
            WBROOK1(I) = ' '
   50     CONTINUE
        ELSE IF (ITYP .EQ. 6) THEN
          DO 60 I=1,80
            WBROOK(I) = ' '
            WBROOK1(I) = BROOK(I)
   60     CONTINUE
        ENDIF
C
C==== If output PDB
C
      ELSE IF (TYPE(II) .EQ. -1) THEN
        IF (.NOT.IFHDOUT .AND. IFCRYS) THEN
          WRITE (UNIT(II),1000) CELL,BRKSPGRP
          WRITE (UNIT(II),2000) (I,(RF(I,J),J=1,3),I=1,3)
          IFHDOUT = .TRUE.
        ENDIF

        IF (BROOKC .NE. ' ') THEN
          IF (ITYP .EQ. 0) THEN
            BROOKC(1:6) = 'ATOM  '
          ELSE
            BROOKC(1:6) = ITYPE(ITYP)
          ENDIF
          IF(ITYP.NE.6)WRITE(UNIT(II),FMT='(80A1)') WBROOK
        ENDIF
        IF (WBROOK1(1) .NE. ' ') THEN
          IF (BROOKB .NE. ' ') THEN
          IF (ITYP .EQ. 0) THEN
            BROOKB(1:6) = 'ATOM  '
C          ELSE
C            BROOKB(1:6) = ITYPE(ITYP)
          ENDIF
            DO 70 I=7,27
              WBROOK1(I) = WBROOK(I)
   70       CONTINUE
            DO 80 I=73,80
              WBROOK1(I) = WBROOK(I)
   80       CONTINUE
          ENDIF
          WRITE(UNIT(II),FMT='(80A1)') WBROOK1
        ENDIF
        DO 90 I=1,80
          WBROOK(I) = ' '
          WBROOK1(I) = ' '
   90   CONTINUE
      ENDIF

      RETURN
C
C---- End of file but without having END card
C
  100 CONTINUE
      ITYP = 0
      RETURN 2
 1000 FORMAT('CRYST1',3F9.3,3F7.2,1x,a11)
 2000 FORMAT( 2('SCALE',I1,4X,3F10.6,5X,'   0.00000',/),
     $          'SCALE',I1,4X,3F10.6,5X,'   0.00000')
      END
C
C
C
      SUBROUTINE XYZATOM(IUNIT,ISER,ATNAM,RESNAM,CHNNAM,IRESN,
     *RESNO,INSCOD,ALTCOD,SEGID,IZ,ID)
C     ========================================================
C
C_BEGIN_XYZATOM
C
C	This subroutine reads or writes the atom name, residue name, 
C chain name etc. into the buffer. XYZADVANCE actually advances a line 
C or atom. The character strings have undefined length in order to make 
C change easier. However, these data items will be strictly defined in 
C the working format.
C
C Parameters:
C
C       IUNIT  (I)  Logical unit of the input coordinate file
C        ISER (I/O) Atom serial number
C       ATNAM (I/O) Atom name        (left justified)
C      RESNAM (I/O) Residue name     
C      CHNNAM (I/O) Chain name       
C       IRESN (I/O) Residue number as an integer
C       RESNO  (O)  Residue number as character, NOT used for output file
C      INSCOD (I/O) The insertion code
C      ALTCOD (I/O) The alternate conformation code.
C       SEGID (I/O) Segid is here to complete PDB standard.
C          IZ  (O)  Atomic number (returned as 7 from ambiguous atoms),
C                   NOT used for output file
C          ID (I/O) Atomic ID related to atomic number (element symbol
C                   right justified), plus the ionic state +2, +3 etc..
C
C_END_XYZATOM
C     
C     .. Paramters ..
      INTEGER MAXFILESOPEN
      PARAMETER (MAXFILESOPEN=10)
C     ..
C     .. Arguments ..
      INTEGER IRESN,ISER,IUNIT,IZ
      CHARACTER*(*) RESNO,ATNAM,RESNAM,CHNNAM
      CHARACTER*(*) ID,INSCOD,ALTCOD,SEGID
      CHARACTER*6 ITYPE(7)
C     ..
C     .. Variables in Common ..
      INTEGER FILESOPEN,ITYP,UNIT,TYPE
      CHARACTER LOGUNIT*80,BROOK*1,WBROOK*1,WBROOK1*1
      LOGICAL IFCRYS,IFHDOUT,IFSCAL,MATRIX
C     ..
C     .. Local Scalars ..
      REAL U(6),OCC,X,Y,Z
      INTEGER I,II
      CHARACTER*100 ERRLIN
      CHARACTER BROOKA*80,PDBATN*4,PDBRESN*4,PDBCHN*1,PDBID*4,
     +          PDBRESNO*5,PDBSEGID*4,Cnums(13)*1

C     ..
C     .. External Routines/Functions ..
      EXTERNAL CCPERR,PDBREAD
C     ..
C     .. Common Blocks ..
      COMMON /RBRKAA/ FILESOPEN,LOGUNIT(MAXFILESOPEN),
     +                UNIT(MAXFILESOPEN),TYPE(MAXFILESOPEN)
      COMMON /RBRKXX/ IFCRYS,IFSCAL,ITYP,MATRIX,IFHDOUT
      COMMON /RBRKYY/ BROOK(80),WBROOK(80),WBROOK1(80)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (BROOKA,WBROOK(1))
C
C     .. Data Statements ..
      DATA ITYPE/'CRYST1','SCALE','TER   ','ATOM  ','HETATM',
     +           'ANISOU','END   '/
      DATA Cnums /'0','1','2','3','4','5','6','7','8','9',
     +           '*',"'",'"'/
C     ..
C     .. Save ..
      SAVE /RBRKAA/,/RBRKXX/,/RBRKYY/
C     ..
c
      II = 0

      DO 10 I=1,FILESOPEN
        IF (IUNIT .EQ. UNIT(I)) THEN
          II = I
          GOTO 20
        ENDIF
   10 CONTINUE

      ERRLIN = ' ERROR: in XYZATOM file has not been opened'
      CALL CCPERR(1,ERRLIN)
C
C==== Input PDB file
C
   20 IF (TYPE(II) .EQ. 1) THEN
        ATNAM = ' '
        RESNAM = ' '
        CHNNAM = ' '
        RESNO = ' '
        ID = ' '
        INSCOD = ' '
        ALTCOD = ' '
        SEGID = ' '
        CALL PDBREAD(ISER,PDBATN,PDBRESN,PDBCHN,IRESN,PDBRESNO,
     +               X,Y,Z,OCC,U,IZ,PDBSEGID,PDBID)
        ALTCOD = BROOK(17)
        ATNAM = PDBATN
        RESNAM = PDBRESN
        CHNNAM = PDBCHN
        RESNO = PDBRESNO(1:4)
        INSCOD = PDBRESNO(5:5)
        ID = PDBID
        SEGID = PDBSEGID
C
C==== Output PDB file
C
      ELSE IF (TYPE(II) .EQ. -1) THEN
        IF (ITYP .EQ. 0) THEN
          BROOKA(1:6) = 'ATOM  '
        ELSE
          BROOKA(1:6) = ITYPE(ITYP)
        ENDIF
        IF(BROOKA(1:6) .EQ. '      ')BROOKA(1:6) = 'ATOM  '
        BROOKA(17:17) = ALTCOD(1:1)
c
c----- PDB rule is that if it is a Hydrogen or Deuterium
c      then BROOKA(13:13) can be a digit 0-9
c      all other single char element symbols
c      must have BROOKA(13:13) = ' '
c
c---- BROOKA(12:12) is ALWAYS ' '
c
        BROOKA(12:12) = ' '
c
c
        IF (ID(1:1) .EQ. ' ') THEN
          IF (ATNAM(2:2).eq.'H' .or. 
     +          (ATNAM(2:2).eq.'D' .and. ID(1:2).eq.' D') ) then
             DO 21 LLx=1,13
               IF (ATNAM(1:1).eq.Cnums(LLx))then
                 BROOKA(13:16) = ATNAM(1:4)
                 GO TO 22
               END IF
 21          CONTINUE
           END IF
          BROOKA(13:13) = ' '
          BROOKA(14:16) = ATNAM(1:3)
        ELSE
          BROOKA(13:16) = ATNAM(1:4)
        ENDIF
 22     CONTINUE
c
c
        WRITE(PDBRESNO,FMT='(I5)') ISER
        BROOKA(7:11) = PDBRESNO
        BROOKA(18:20) = RESNAM
        BROOKA(21:21) = ' '
        BROOKA(22:22) = CHNNAM
        WRITE(BROOKA(23:26),FMT='(I4)') IRESN
        BROOKA(27:27) = INSCOD
        BROOKA(28:30) = '   '
        BROOKA(67:72) = '     '
        BROOKA(73:76) = SEGID
        BROOKA(77:80) = ID

      ELSE
C Not yet ready
      ENDIF

      RETURN
      END
C
C
C
      SUBROUTINE XYZCOORD(IUNIT,XFLAG,BFLAG,X,Y,Z,OCC,BISO,U)
C     =======================================================
C
C_BEGIN_XYZCOORD
C
C	This subroutine reads or writes x, y, z, occupancy and b from/to 
C the internal buffer. The buffer is updated from the file by 
C XYZADVANCE. The coordinates can be input/output (to the subroutine) 
C as orthogonal or fractional. 
C
C  PDB files contain anisotropic temperature factors as orthogonal Us.
C The anisotropic temperature factors can be input/output to this routine 
C  as orthogonal or as crystallographic Us. 
C  
C  Shelx defines Uf to calculate temperature factor as:
C T(aniso_Uf) = exp (-2PI**2 ( (h*ast)**2 Uf_11 + (k*bst)**2 Uf_22 + ... 
C                            + 2hk*ast*bst*Uf_12 +..)
C
C   Note:   Uo_ji == Uo_ij and  Uf_ji == Uf_ij.
C
C  [Uo_ij] listed on ANISOU card satisfy  the relationship:
C  [Uo_ij] =   [RFu]-1 [Uf_ij] {[RFu]-1}T   C
C        where [Rfu] is the normalised [Rf] matrix read from the SCALEi cards.
C        see code.   [ROu] ==  [RFu]-1
C
C T(aniso_Uo) = U(11)*H**2 + U(22)*K**2 + 2*U(12)*H*K + ...
C where H,K,L are orthogonal reciprocal lattice indecies. ( EJD: I think????)
C
C Biso     = 8*PI**2 (Uo_11 + Uo_22 + Uo_33) / 3.0
C
C   [Uf(symm_j)] = [Symm_j] [Uf] [Symm_j]T
C
C Parameters:
C
C       IUNIT  (I)  Channel number of the input coordinate file
C       XFLAG  (I)  For input file
C                     ='F' will get fractional coords. 
C                     ='O' will get orthogonal coords.
C                   For output file
C                     ='F' passed coordinates are fractional
C                     ='O' passed coordinates are orthogonal
C       BFLAG  (I)  For input file
C                     ='F' will get fractional us
C                     ='O' will get orthogonal Us.
C                   For output file
C                     ='F' have fractional us
C                     ='O' have othogonal Us
C           X (I/O) Coordinates (orthogonal angstrom coordinates as
C           Y (I/O)     "        stored)
C           Z (I/O)     "
C         OCC (I/O) Occupancy
C        BISO  (O)  Isotropic temperature factor, NOT used for output file.
C        U(6) (I/O) Orthogonal Anisotropic temperature factor, unless only U(1) defined.
C
C_END_XYZCOORD
C     
C     .. Paramters ..
      INTEGER MAXFILESOPEN
      PARAMETER (MAXFILESOPEN=10)
C     ..
C     .. Arguments ..
      REAL U(6),BISO,X,Y,Z
      INTEGER IUNIT
      CHARACTER*1 BFLAG,XFLAG
C     ..
C     .. Variables in Common ..
      INTEGER FILESOPEN,ITYP,UNIT,TYPE
      CHARACTER LOGUNIT*80,BROOK*1,WBROOK*1,WBROOK1*1
      LOGICAL IFCRYS,IFHDOUT,IFSCAL,MATRIX
C     ..
C     .. Local Scalars ..
      REAL EIGHTPI2,XX,YY,ZZ
      INTEGER I,II
      INTEGER IRESN,ISER,IZ
      CHARACTER*100 ERRLIN
      CHARACTER ATNAM*4,RESNAM*4,RESNO*4,ID*4,CHNNAM*1,SEGID*4
      CHARACTER*80 BROOKA,BROOKB
C     ..
C     .. External Routines/Functions ..
      EXTERNAL CCPERR,CCPUPC,CVANISOU,CVFRAC2,PDBREAD
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,NINT
C     ..
C     .. Common Blocks ..
      COMMON /RBRKAA/ FILESOPEN,LOGUNIT(MAXFILESOPEN),
     +                UNIT(MAXFILESOPEN),TYPE(MAXFILESOPEN)
      COMMON /RBRKXX/ IFCRYS,IFSCAL,ITYP,MATRIX,IFHDOUT
      COMMON /RBRKYY/ BROOK(80),WBROOK(80),WBROOK1(80)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (BROOKA,WBROOK(1)),(BROOKB,WBROOK1(1))
C     ..
C     .. Save ..
      SAVE /RBRKAA/,/RBRKXX/,/RBRKYY/
C     ..
C     .. Data Statement ..
      DATA EIGHTPI2 /78.956835/

      II = 0
      DO 10 I=1,FILESOPEN
        IF (IUNIT .EQ. UNIT(I)) THEN
          II = I
          GOTO 20
        ENDIF
   10 CONTINUE

      ERRLIN = ' ERROR: in XYZCOORD has not been opened'
      CALL CCPERR(1,ERRLIN)
C
C==== Input PDB file
C  
   20 IF (TYPE(II) .EQ. 1) THEN
        CALL PDBREAD(ISER,ATNAM,RESNAM,CHNNAM,IRESN,RESNO,
     +               X,Y,Z,OCC,U,IZ,SEGID,ID)
C
C---- Convert x,y,z to fractional if necessary
C
        IF (XFLAG .EQ. 'F' .OR. XFLAG .EQ. 'f') THEN
          IF (MATRIX) THEN
            CALL CVFRAC2(X,Y,Z,XX,YY,ZZ,1)
            X = XX
            Y = YY
            Z = ZZ
          ELSE
            CALL CCPERR(1,'*** Cannot convert to fractional coord. ***')
          ENDIF
        ENDIF
C
C---- Calulate isotropic Uf from Uo, if necessary convert.
C
        IF (U(2).NE.0.0 .AND. U(3).NE.0.0) THEN
          BISO = EIGHTPI2 * (U(1)+U(2)+U(3))/3.0
          IF (BFLAG.EQ.'F' .OR. BFLAG.EQ.'f') CALL CVANISOU(U,1)
C
C---- Go here if no anisotropic B
C
        ELSE
          BISO = U(1)
        ENDIF
C
C==== Output PDB file
C
      ELSE IF (TYPE(II) .EQ. -1) THEN
        IF (XFLAG .EQ. 'F' .OR. XFLAG .EQ. 'f') THEN
          IF (MATRIX) THEN
            CALL CVFRAC2(X,Y,Z,XX,YY,ZZ,0)
          ELSE
            CALL CCPERR(1,
     .      '*** Cannot convert from fract. to ortho. coordinates ***')
          ENDIF
        ELSE
          XX = X
          YY = Y
          ZZ = Z
        ENDIF
C
C---- Check for anisotropic temperature factors
C
        IF (U(2).NE.0.0 .OR. U(3).NE.0.0) THEN
          IF (BFLAG .EQ. 'F' .OR. BFLAG .EQ. 'f') CALL CVANISOU(U,0)
          BISO = EIGHTPI2 * (U(1)+U(2)+U(3))/3.0

          BROOKB(1:6) = 'ANISOU'
          WRITE(BROOKB(29:70),FMT='(6I7)') (NINT(U(I)*1.0E+04),I=1,6)
        ELSE
          BISO = U(1)
        ENDIF

        WRITE(BROOKA(31:66),FMT='(3F8.3,2F6.2)') XX,YY,ZZ,OCC,BISO
          
      ELSE
C Not yet ready
      ENDIF

      RETURN
      END
C
C
C
      SUBROUTINE XYZREWD(IUNIT)
C     =========================
C
C_BEGIN_XYZREWD
C
C	This routine is resets pointer to the begining of the file ie.
C rewind the file.
C
C Parameters:
C
C      IUNIT  (I) INTEGER  Channel number for file.
C
C_END_XYZREWD
C
C     .. Parameters ..
      INTEGER MAXFILESOPEN
      PARAMETER (MAXFILESOPEN=10)
C     ..
C     .. Arguments ..
      INTEGER IUNIT
C     ..
C     .. Variables in Common ..
      INTEGER FILESOPEN,TYPE,UNIT
      CHARACTER*80 LOGUNIT
C     ..
C     .. Local Scalars ..
      INTEGER I,II
      CHARACTER*100 ERRLIN
C     ..
C     .. External Functions/Routines ..
      EXTERNAL CCPERR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     ..
C     .. Common Blocks ..
      COMMON /RBRKAA/ FILESOPEN,LOGUNIT(MAXFILESOPEN),
     +                UNIT(MAXFILESOPEN),TYPE(MAXFILESOPEN)
C     ..
C     .. Save ..
      SAVE /RBRKAA/
C     ..
      II = 0
      DO 10 I=1,FILESOPEN
        IF (IUNIT .EQ. UNIT(I)) THEN
          II = I
          GOTO 20
        ENDIF
   10 CONTINUE

      ERRLIN = ' ERROR: in XYZREWD file has not been opened'
      CALL CCPERR(1,ERRLIN)

   20 IF (ABS(TYPE(II)) .EQ. 1) THEN
        REWIND UNIT(II)
        IF (TYPE(II) .EQ. -1) CALL CCPERR(2,
     +    ' WARNING: you are rewinding an output file!!')
      ENDIF

      RETURN
      END
C
C
C
      SUBROUTINE XYZBKSP(IUNIT)
C     =========================
C
C_BEGIN_XYZBKSP
C
C	This routine is the opposite to XYZADVANCE in that it retreats 
C one atom ie. backspacing.
C
C Parameters:
C
C      IUNIT  (I) INTEGER  Channel number for file.
C
C_END_XYZBKSP
C
C     .. Parameters ..
      INTEGER MAXFILESOPEN
      PARAMETER (MAXFILESOPEN=10)
C     ..
C     .. Arguments ..
      INTEGER IUNIT
C     ..
C     .. Variables in Common ..
      INTEGER FILESOPEN,TYPE,UNIT
      CHARACTER*80 LOGUNIT
C     ..
C     .. Local Scalars ..
      INTEGER I,II
      CHARACTER*100 ERRLIN
C     ..
C     .. External Functions/Routines ..
      EXTERNAL CCPERR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     ..
C     .. Common Blocks ..
      COMMON /RBRKAA/ FILESOPEN,LOGUNIT(MAXFILESOPEN),
     +                UNIT(MAXFILESOPEN),TYPE(MAXFILESOPEN)
C     ..
C     .. Save ..
      SAVE /RBRKAA/
C     ..
      II = 0
      DO 10 I=1,FILESOPEN
        IF (IUNIT .EQ. UNIT(I)) THEN
          II = I
          GOTO 20
        ENDIF
   10 CONTINUE

      ERRLIN = ' ERROR: in XYZBKSP file has not been opened'
      CALL CCPERR(1,ERRLIN)

   20 IF (ABS(TYPE(II)) .EQ. 1) THEN
        BACKSPACE UNIT(II)
        IF (TYPE(II) .EQ. -1) CALL CCPERR(2,
     +    ' WARNING: you are backspacing an output file!!')
      ENDIF

      RETURN
      END
C
C
C
      SUBROUTINE RBROOK(IUNIT,ISER,ATNAM,RESNAM,CHNNAM,IRESN,RESNO,IS,
     *X,Y,Z,OCC,B,IZ,IOUT,MSG1,MSG2,ITER,*,*)
C     ================================================================
C
C_BEGIN_RBROOK
C
C      This subroutine is obsolete and should be removed. May be 
C PROBLEM in that routine returns orthogonal coordinates and not fractional
C ones.
C
C_END_RBROOK
C
C     .. Scalar Arguments ..
      REAL X,Y,Z,OCC,B
      INTEGER IUNIT,ISER,IRESN,IS,IOUT,IZ,MSG1,MSG2,ITER
      CHARACTER*(*) RESNO
      CHARACTER ATNAM*4,RESNAM*4,CHNNAM*1
C     ..
C     .. Local Scalars ..
      CHARACTER*4 ID,SEGID
      CHARACTER*1 INSCOD,ALTCOD
C     ..
C     .. Local Arrays ..
      REAL U(6)
C     ..
C     .. External Routines ..
      EXTERNAL XYZADVANCE,XYZATOM,XYZCOORD
C     ..
      IS = 0

   10 CALL XYZADVANCE(IUNIT,IOUT,ITER,*1000,*2000)
      CALL XYZCOORD(IUNIT,'O','U',X,Y,Z,OCC,B,U)
C
C---- Skip ANISOU card in PDB. Test on X, Y and Z are not strictly necessary
C     as routines can only read PDB currently.
C
      IF (U(2).NE.0.0 .AND. U(3).NE.0.0) THEN
        IF (X.EQ.0.0 .AND. Y.EQ.0.0 .AND. Z.EQ.0.0) GOTO 10
      ENDIF
      CALL XYZATOM(IUNIT,ISER,ATNAM,RESNAM,CHNNAM,IRESN,RESNO,INSCOD,
     +             ALTCOD,SEGID,IZ,ID)


      RETURN
 1000 RETURN 1
 2000 RETURN 2
      END
C
C
C
      SUBROUTINE WBROOK(IUNIT,ISER,ATNAM,RESNAM,CHNNAM,IRESN,IS,
     *X,Y,Z,OCC,B,IZ)
C     ================================================================
C
C_BEGIN_RBROOK
C
C      This subroutine is obsolete and should be removed. May be 
C PROBLEM in that routine does not cater for IS.
C
C_END_RBROOK
C
C     .. Scalar Arguments ..
      REAL X,Y,Z,OCC,B
      INTEGER IUNIT,ISER,IRESN,IS,IZ
      CHARACTER ATNAM*4,RESNAM*4,CHNNAM*1
C     ..
C     .. Local Scalars ..
      CHARACTER*4 ID,SEGID,RESNO
      CHARACTER*1 INSCOD,ALTCOD
C     ..
C     .. Local Arrays ..
      REAL U(6)
C     ..
C     .. External Routines ..
      EXTERNAL XYZADVANCE,XYZATOM,XYZCOORD
C     ..
      SEGID = ' '
      ID = ' '
      INSCOD = ' '
      ALTCOD = ' '
      RESNO = ' '
      DO 10 I=1,6
        U(I) = 0.0
   10 CONTINUE


      CALL XYZADVANCE(IUNIT,0,0,*1000,*1000)
      CALL XYZCOORD(IUNIT,'O','U',X,Y,Z,OCC,B,U)
      CALL XYZATOM(IUNIT,ISER,ATNAM,RESNAM,CHNNAM,IRESN,RESNO,INSCOD,
     +             ALTCOD,SEGID,IZ,ID)
C
C---- This label is here for completeness but is not used (see XYZADVANCE).
C
 1000 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE PDBREAD(ISER,ATNAM,RESNAM,CHNNAM,IRESN,RESNO,
     *X,Y,Z,OCC,U,IZ,SEGID,ID)
C     ========================================================
C
C_BEGIN_PDBREAD
C
C      The subroutine PDBREAD is used to read coordinates from a PDB
C format coordinate file. This routine should not be used stand alone 
C but through XYZADVANCE.
C 
C Parameters
C
C        ISER (O) Atom serial number
C       ATNAM (O) Atom name        (character*4 left justified)
C      RESNAM (O) Residue name     (character*4)
C      CHNNAM (O) Chain name       (character*1)
C       IRESN (O) Residue number as an integer
C       RESNO (O) Residue number   (character*4 or character*5)
C                 If character*5 then the 5th character will be the
C                 insertion code.
C           X (O) Coordinates (orthogonal angstrom coordinates as
C           Y (O)     "        stored)
C           Z (O)     "
C         OCC (O) Occupancy
C        U(6) (O) Temperature factor
C          IZ (O) Atomic number (returned as 7 from ambiguous atoms)
C          ID (O) Atomic ID related to atomic number + ionic state. 
C                 (character*4)
C
C  COMMON BLOCKS
C
C  COMMON /RBRKXX/IFCRYS,IFSCAL,ITYP,MATRIX
C
C      IFCRYS   .TRUE. IF 'CRYST1' CARD READ,  OTHERWISE .FALSE.
C      IFSCAL   .TRUE. IF 'SCALE' CARDS READ, OTHERWISE .FALSE.
C       ITYP    TYPE OF LAST CARD READ =1, 'CRYST1'
C                                      =2, 'SCALE'
C                                      =3, 'TER'
C                                      =4, 'ATOM'
C                                      =5, 'HETATM'
C     MATRIX    .TRUE. IF FRACT/ORTHOG MATRICES CALCULATED
C               .FALSE. IF NOT
C
C  COMMON /RBRKYY/BROOK(80)
C
C      BROOK    CHARACTER*1 ARRAY WHICH IS THE BUFFER FOR PDB FILES
C
C      COMMON/RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
C
C       CELL    CELL DIMENSIONS FROM 'CRYST1' CARD IF READ
C               (CHECK IFCRYS)
C         RR    STANDARD ORTHOGONALISING MATRICES CALCULATED IF THE
C               'CRYST1' CARD WAS READ (CHECK IFCRYS)
C
C  COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE
C
C        RO    ORTHOGONALISING MATRIX (ONLY SET IF 'CRYST1' OR 'SCALE'
C              CARDS PRESENT - CHECK 'MATRIX' FLAG)
C        RF    FRACTIONALISING MATRIX (ONLY SET IF 'CRYST1' OR 'SCALE'
C              CARDS PRESENT - CHECK 'MATRIX' FLAG)
C     NCODE    FLAG INDICATING SETTING FOUND, 0 IF NOT ONE THAT WAS
C              RECOGNISED
C
C_END_PDBREAD
C
C
C     .. Parameters ..
      INTEGER MAXIATM, MAXIHATM
      PARAMETER (MAXIATM=102,MAXIHATM=14)
C     ..
C     .. Arguments ..
      REAL U(6),OCC,X,Y,Z
      INTEGER IRESN,ISER,IZ
      CHARACTER*(*) RESNO
      CHARACTER ATNAM*4,CHNNAM*1,ID*4,RESNAM*4,SEGID*4
C     ..
C     .. Variables in Common ..
      REAL CELL,CELLAS,RF,RO,RR,VOL
      INTEGER ITYP,NCODE
      CHARACTER BROOK*1,WBROOK*1,WBROOK1*1
      LOGICAL IFCRYS,IFSCAL,IFTER,MATRIX,IFHDOUT
C     ..
C     .. Local Scalars ..
      INTEGER I,II,J
      CHARACTER*100 ERRLIN
      CHARACTER*80 BROOKA
      CHARACTER*4 IRTYPE
      CHARACTER*2 IAA,IAT,IE
      CHARACTER*1 ISP
C     ..
C     .. Local Arrays ..
      INTEGER IU(6)
      CHARACTER*40 ORTH(5)
      CHARACTER*2 IATM(MAXIATM),IHATM(MAXIHATM)
C     ..
C     .. External Routines/Functions ..
      INTEGER LENSTR
      EXTERNAL CCPERR,CCPUPC,LENSTR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     ..
C     .. Common Blocks ..
      COMMON /RBRKXX/IFCRYS,IFSCAL,ITYP,MATRIX,IFHDOUT
      COMMON /RBRKYY/BROOK(80),WBROOK(80),WBROOK1(80)
      COMMON/RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
      COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE
      COMMON /ORTHOGU/ ROU(4,4),RFU(4,4)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (IRTYPE,BROOK(1)),(IE,BROOK(5)),(BROOKA,BROOK(1))
C     ..
C     .. Save ..
      SAVE /RBRKXX/,/RBRKYY/,/RBRKZZ/,/ORTHOG/
C     ..
C     .. Data Statement ..
      DATA IATM/' H','HE','LI','BE',' B',' C',' N',' O',' F','NE',
     *          'NA','MG','AL','SI',' P',' S','CL','AR',' K','CA',
     *          'SC','TI',' V','CR','MN','FE','CO','NI','CU','ZN',
     *          'GA','GE','AS','SE','BR','KR','RB','SR',' Y','ZR',
     *          'NB','MO','TC','RU','RH','PD','AG','CD','IN','SN',
     *          'SB','TE',' I','XE','CS','BA','LA','CE','PR','ND',
     *          'PM','SM','EU','GD','TB','DY','HO','ER','TM','YB',
     *          'LU','HF','TA',' W','RE','OS','IR','PT','AU','HG',
     *          'TL','PB','BI','PO','AT','RN','FR','RA','AC','TH',
     *          'PA',' U','NP','PU','AM','CM','BK','CF','ES','FM',
     *          ' D','AN'/
      DATA IHATM/'0H','1H','2H','3H','4H','5H','6H','7H','8H','9H',
     +           'HH','*H',"'H",'"H'/
      DATA IAA/' A'/,ISP/' '/
      DATA ORTH/'A // XO, C* // ZO (STANDARD PDB)',
     *          'B // XO, A* // ZO',
     *          'C // XO, B* // ZO',
     *          'HEX A+B // XO, C* // ZO',
     *          'A* // XO, C // ZO (ROLLETT)'/
C      DATA ITYPE/'CRYS','SCAL','TER ','ATOM','HETA','ANIS','END'/
C
C
      IFTER=.FALSE.
C
C---- Atom/hetatm card processing
C
      IF (IRTYPE.EQ.'ATOM' .OR. IRTYPE.EQ.'HETA' .OR.
     +    IRTYPE.EQ.'ANIS' .OR. IRTYPE.EQ.'TER ') THEN
        IF (IRTYPE.EQ.'TER ') THEN
C
C---- 'ter' card found
C
          ITYP=3
          IFTER=.TRUE.
          GO TO 450
        ENDIF
        IF(BROOK(13).EQ.ISP) then
c
C----   ATNAM (O) Atom name  (character*4 left justified)
c
          ATNAM=BROOK(14)//BROOK(15)//BROOK(16)//' '
c
c---- atnam should never had ALTCODE added to it 
c
         else
          ATNAM=BROOK(13)//BROOK(14)//BROOK(15)//BROOK(16)
         end if
c
c
450     READ(BROOKA,1006)ISER,IRESN
        RESNAM=BROOK(18)//BROOK(19)//BROOK(20)//ISP
        RESNO=BROOK(23)//BROOK(24)//BROOK(25)//BROOK(26)
        IF(LEN(RESNO).GT.4)RESNO(5:5)=BROOK(27)
        CHNNAM=BROOK(22)
        IF(IFTER)GO TO 500
        SEGID = BROOK(73)//BROOK(74)//BROOK(75)//BROOK(76)
C
C---- Find atomic number and ID, ID can be kept in columns 77-80
C
        ID = BROOK(77)//BROOK(78)//BROOK(79)//BROOK(80)
        CALL CCPUPC(ID)
        IAT=BROOK(13)//BROOK(14)
        CALL CCPUPC(IAT)
C
        IF (ID(1:2) .EQ. IATM(6)) THEN
          II = 6
          GOTO 480
        ENDIF
        IF (ID(1:2) .EQ. IATM(7)) THEN
          II = 7
          GOTO 480
        ENDIF
        IF (ID(1:2) .EQ. IATM(8)) THEN
          II = 8
          GOTO 480
        ENDIF
        IF (ID(1:2) .EQ. IATM(1)) THEN
          II = 1
          GOTO 480
        ENDIF

        DO 452 I=1,MAXIATM
          IF (ID(1:2) .EQ. IATM(I)) THEN
            II = I
            GOTO 480
          ENDIF
452     CONTINUE
C
C     If no ID match then make sure it is empty
C
        ID = ' '
C
        IF (IAT.EQ.IATM(6)) THEN
          II = 6
          GO TO 480
        ENDIF
        IF (IAT.EQ.IATM(7)) THEN
          II = 7
          GO TO 480
        ENDIF
        IF (IAT.EQ.IATM(8)) THEN
          II = 8
          GO TO 480
        ENDIF
        IF (IAT.EQ.IATM(1)) THEN
          II = 1
          GO TO 480
        ENDIF
C
        II=1
        DO 454 J=1,MAXIHATM
          IF (IAT.EQ.IHATM(J)) GO TO 480
454     CONTINUE
C
        DO 456 I=1,MAXIATM
          IF (IAT.EQ.IATM(I)) THEN
            II = I
            GO TO 480
          ENDIF
456     CONTINUE
C
        II=0
        IF(IAT.EQ.IAA)II=7
        IF (I .EQ. 0) THEN
          WRITE(ERRLIN,2001)ATNAM,RESNAM,RESNO(1:4)
          CALL CCPERR(4,' ')
          CALL CCPERR(4,ERRLIN)
          CALL CCPERR(4,' ')
        ENDIF
        IF (I .EQ. 7) THEN
          WRITE(ERRLIN,2002)ATNAM,RESNAM,RESNO(1:4)
          CALL CCPERR(4,' ')
          CALL CCPERR(4,ERRLIN)
          CALL CCPERR(4,' ')
        ENDIF
480     IZ=II
        IF (IZ .EQ. 0) THEN
          ID = ' '
        ELSE
C
C---- Keep the ionic state if valid, OR from atom name.
C
          IF (ID(1:1) .EQ. ' ') THEN
            IF (ATNAM(3:3).EQ.'+' .OR. ATNAM(3:3).EQ.'-') 
     +                                       ID(3:4) = ATNAM(3:4)
          ELSE
            IF (ID(3:3).NE.'+' .AND. ID(3:3).NE.'-') ID(3:4) = '  '
          ENDIF
          ID(1:2) = IATM(IZ)
        ENDIF
C
C---- Put elment ID into output buffer.
C
        DO 485 J=1,4
          WBROOK(76+J) = ID(J:J)
          WBROOK1(76+J) = ID(J:J)
485     CONTINUE
         IF (IRTYPE .EQ. 'ATOM'.or.IRTYPE.EQ.'HETA') THEN
C  This is the ONLY flag that you have read a ATOM or HETATOM card..
          DO 40 I=1,6
            U(I) = 0.0
   40     CONTINUE
            IF (IRTYPE.EQ.'ATOM') ITYP=4
            IF (IRTYPE.EQ.'HETA') ITYP=5
          READ(BROOKA,1005)X,Y,Z,OCC,U(1)
C
C---- AnisoU cards
C
        ELSE IF (IRTYPE .EQ. 'ANIS') THEN
C
        READ(BROOKA,1010)IU(1),IU(2),IU(3),IU(4),IU(5),IU(6)
        DO 510 I=1,6
          U(I) = IU(I)/1.0E+04
510     CONTINUE
C  Get rid of this, sometimes useful to know xyz 
C   use values of U(i) to check for ANISOU 
        X = 0.0
        Y = 0.0
        Z = 0.0
        ENDIF
C
        RETURN        
      ENDIF

500   RETURN
C
C---- Format statements
C
1005  FORMAT(30X,3F8.3,2F6.2)
1006  FORMAT(6X,I5,11X,I4)
1010  FORMAT(28X,6I7)
2001  FORMAT(' *UNKNOWN ATOMIC FORMFACTOR ',A4,' IN ',A4,1X,A4,'*')
2002  FORMAT(' *AMBIGUOUS ATOMIC FORMFACTOR ',A4,' IN ',A4,1X,A4,'*')
      END
C
C
C
      SUBROUTINE RBFRAC2(A,B,C,AL,BE,GA,ARGNCODE)
C     ===========================================
C
C_BEGIN_RBFRAC2
C
C
C This subroutine is used to calculate the default transformation
C matrices between orthogonal angstrom and fractional coordinates
C The sensible name is for Phil, as RBFRAC2 was changed from the original.
C
C
C PARAMETERS
C
C    A,B,C,AL,BE,GA (I) (REAL)     CELL PARAMETERS IN ANGSTROMS AND DEGREES
C    ARGNCODE       (I) (INTEGER)  ORTHOGONALISATION CODE 1-6
C
C_END_RBFRAC2
C
C     .. Arguments ..
      REAL A,B,C,AL,BE,GA
      INTEGER ARGNCODE
C     ..
C     .. Variables in Common ..
      REAL CELL,CELLAS,RF,RO,RR,VOL
      INTEGER ITYP,NCODE
      LOGICAL IFCRYS,IFSCAL,MATRIX,IFHDOUT
C     ..
C     .. External Routines ..
      EXTERNAL CCPERR,RBRINV
C     ..
C     .. Common Blocks ..
      COMMON /RBRKXX/IFCRYS,IFSCAL,ITYP,MATRIX,IFHDOUT
      COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE
      COMMON /RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
C     ..
C     .. Save Statement ..
      SAVE /RBRKXX/,/ORTHOG/,/RBRKZZ/
C     ..
      IF (ARGNCODE.NE.0) NCODE = ARGNCODE
C
C---- Calculate matrices
C

      DO 10 I=1,4
      DO 10 J=1,4
      RO(I,J)=0
      RF(I,J)=0
10    CONTINUE
C
C
      RO(4,4)=1.0
      RF(4,4)=1.0
      CELL(1)=A
      CELL(2)=B
      CELL(3)=C
      CELL(4)=AL
      CELL(5)=BE
      CELL(6)=GA
      CALL RBFROR
C
C
      DO 20 I=1,3
      DO 20 J=1,3
      RO(I,J)=RR(I,J,1)
20    CONTINUE
C
C
      CALL RBRINV(RO,RF)
      MATRIX=.TRUE.
      CALL CCPERR(4,' ')
      CALL CCPERR(4,
     +     ' STANDARD PDB COORDINATE SETTING WILL BE ASSUMED')
      CALL CCPERR(4,
     +     ' IF NO SCALE CARDS PRESENT  IN  INPUT  COORDINATE  FILE')
      CALL CCPERR(4,' ')

      RETURN
      END
C
C
C
      SUBROUTINE RBFRAC(A,B,C,AL,BE,GA,MSG)
C     =====================================
C
C_BEGIN_RBFRAC
C
C	This routine is obsolete and should be removed.
C
C_END_RBFRAC
C
C     .. Scalar Arguments ..
      REAL A,B,C,AL,BE,GA
      INTEGER MSG
C     ..
C     .. External Routines ..
      EXTERNAL RBFRAC2
C     ..
      CALL RBFRAC2(A,B,C,AL,BE,GA,1)

      RETURN
      END
C
C
C
      SUBROUTINE RBRORF(ROO,RFF)
C     ==========================
C
C_BEGIN_RBRORF
C
C	This routine is obsolete and should be removed.
C
C      SUBROUTINE RBRORF(ROO,RFF)
C
C     Subroutine to  fill or return RF (fractionalising) and Ro
C     (orthogonalising) matrices. 
C
C PARAMETERS
C
C          ROO (I) (REAL(4,4))  4*4 MATRIX TO BE INVERTED
C          RFF (O) (REAL(4,4))  INVERSE MATRIX
C
C common blocks
C
C
C
      DIMENSION ROO(4,4),RFF(4,4)
C
      LCODE = 0
      CALL RBRORF2(ROO,RFF,LCODE)
      END
C
C
C
      SUBROUTINE RBRORF2(ROO,RFF,LCODE)
C     ==========================
C
C_BEGIN_RBRORF
C
C      SUBROUTINE RBRORF2(ROO,RFF,LCODE)
C
C     Subroutine to  fill or return RF (fractionalising) and Ro
C     (orthogonalising) matrices. 
C
C PARAMETERS
C
C          ROO (I) (REAL(4,4))  4*4 MATRIX TO BE INVERTED
C          RFF (O) (REAL(4,4))  INVERSE MATRIX
C
C common blocks
C
C      COMMON /RBRKXX/IFCRYS,IFSCAL,ITYP,MATRIX
C      COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE
C
C_END_RBRORF

      LOGICAL IFCRYS,IFSCAL,MATRIX,IFHDOUT
      COMMON /RBRKXX/IFCRYS,IFSCAL,ITYP,MATRIX,IFHDOUT
      COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE
      SAVE /ORTHOG/, /RBRKXX/
C
C
      DIMENSION ROO(4,4),RFF(4,4)
C
C---- Get cofactors of 'a' in array 'c'
C
      IF(ROO(1,1) .LE.0.0000000001) THEN 
        DO 40 II=1,4
        DO 30 JJ=1,4
        RFF(II,JJ)=RF(II,JJ)
        ROO(II,JJ)=RO(II,JJ)
30      CONTINUE
40      CONTINUE
        LCODE = NCODE
        RETURN
      END IF
C  FILL...
      IF(ROO(1,1) .GT.0.0000000001) THEN 
        DO 50 II=1,4
        DO 60 JJ=1,4
        RF(II,JJ)=RFF(II,JJ)
        RO(II,JJ)=ROO(II,JJ)
60      CONTINUE
50      CONTINUE
        NCODE = LCODE
        MATRIX = .TRUE.
        RETURN
      END IF
      END
C
C
      SUBROUTINE RBRINV(A,AI)
C     =======================
C
C_BEGIN_RBRINV
C
C      SUBROUTINE RBRINV(A,AI)
C
C
C Subroutine to invert 4*4 matrices for conversion between
C fractional and orthogonal axes. 
C
C
C PARAMETERS
C
C           A (I) (REAL(4,4))  4*4 MATRIX TO BE INVERTED
C          AI (O) (REAL(4,4))  INVERSE MATRIX
C
C_END_RBRINV
C
      REAL A(4,4),AI(4,4),C(4,4),X(3,3)
C
C---- Get cofactors of 'a' in array 'c'
C
      DO 40 II=1,4
      DO 30 JJ=1,4
      I=0
      DO 20 I1=1,4
      IF(I1.EQ.II)GO TO 20
      I=I+1
      J=0
      DO 10 J1=1,4
      IF(J1.EQ.JJ)GO TO 10
      J=J+1
      X(I,J)=A(I1,J1)
10    CONTINUE
20    CONTINUE
      AM=X(1,1)*X(2,2)*X(3,3)-X(1,1)*X(2,3)*X(3,2)+X(1,2)*X(2,3)*X(3,1)
     *  -X(1,2)*X(2,1)*X(3,3)+X(1,3)*X(2,1)*X(3,2)-X(1,3)*X(2,2)*X(3,1)
      C(II,JJ)=(-1)**(II+JJ)*AM
30    CONTINUE
40    CONTINUE
C
C---- Calculate determinant
C
      D=0
      DO 50 I=1,4
      D=D+A(I,1)*C(I,1)
50    CONTINUE
C
C---- Get inverse matrix
C
      DO 70 I=1,4
      DO 60 J=1,4
      AI(I,J)=C(J,I)/D
60    CONTINUE
70    CONTINUE
      RETURN
      END
C
C
C
C
      SUBROUTINE RBFROR
C     =================
C
C_BEGIN_RBFROR
C
C      SUBROUTINE RBFROR
C
C THIS SUBROUTINE CALCULATES MATRICES FOR STANDARD ORTHOGONALISATIONS
c   and cell volume
C
C  this generates the various orthogonalising matrices
C     ' NCODE =1 -  ORTHOG AXES ARE DEFINED TO HAVE'
C                    A PARALLEL TO XO   CSTAR PARALLEL TO ZO'
C     ' NCODE =2 -  ORTHOG AXES ARE DEFINED TO HAVE'
C     '               B PARALLEL TO XO   ASTAR PARALLEL TO ZO'
C     ' NCODE =3 -  ORTHOG AXES ARE DEFINED TO HAVE'
C     '               C PARALLEL TO XO   BSTAR PARALLEL TO ZO'
C     ' NCODE =4 -  ORTHOG AXES ARE DEFINED TO HAVE'
C     '         HEX A+B PARALLEL TO XO   CSTAR PARALLEL TO ZO'
C     ' NCODE =5 -  ORTHOG AXES ARE DEFINED TO HAVE'
C     '           ASTAR PARALLEL TO XO       C PARALLEL TO ZO'
C     ' NCODE =6 -  ORTHOG AXES ARE DEFINED TO HAVE'
C                    A  PARALLEL TO XO   BSTAR PARALLEL TO YO'
C
C   SET UP MATRICES TO ORTHOGONALISE H K L AND X Y Z FOR THIS CELL.
C
C Common Blocks
C
C     .. Scalar Arguments ..
      REAL VOLL
C     ..
C     .. Array Arguments ..
      REAL CEL(6),RRR(3,3,6)
C
C      COMMON/RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
C      COMMON /RBREC/AC(6)
C
C_END_RBFROR
C
      COMMON/RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
      COMMON /RBREC/AC(6)
      SAVE /RBRKZZ/, /RBREC/
C
C---- Initialisations
C
        DO I = 1,6
         CEL(I)=CELL(I)
        END DO
        VOLL = VOL
      CALL RBFRO1(CEL,VOLL,RRR)
C
      RETURN
      END
C
C
C     ===============================
      SUBROUTINE RBFRO1(CEL,VOLL,RRR)
C     ===============================
C
C_BEGIN_RBFRO1
C
C      SUBROUTINE RBFRO1(CEL,VOLL,RRR)
C
C---- This subroutine is a duplicate of rbfror with a different call.
C
C PARAMETERS
C
C   CEL  (I) (REAL(6))     Cell dimensions
C   VOLL (O) (REAL)        Cell volume
C   RRR  (O) (REAL(3,3,6)) Standard orthogonisational matrices
C
C_END_RBFRO1
C
C THIS SUBROUTINE CALCULATES MATRICES FOR STANDARD ORTHOGONALISATIONS
c   and cell volume
C
C  this generates the various orthogonalising matrices
C     ' NCODE =1 -  ORTHOG AXES ARE DEFINED TO HAVE'
C                    A PARALLEL TO XO   CSTAR PARALLEL TO ZO'
C     ' NCODE =2 -  ORTHOG AXES ARE DEFINED TO HAVE'
C     '               B PARALLEL TO XO   ASTAR PARALLEL TO ZO'
C     ' NCODE =3 -  ORTHOG AXES ARE DEFINED TO HAVE'
C     '               C PARALLEL TO XO   BSTAR PARALLEL TO ZO'
C     ' NCODE =4 -  ORTHOG AXES ARE DEFINED TO HAVE'
C     '         HEX A+B PARALLEL TO XO   CSTAR PARALLEL TO ZO'
C     ' NCODE =5 -  ORTHOG AXES ARE DEFINED TO HAVE'
C     '           ASTAR PARALLEL TO XO       C PARALLEL TO ZO'
C     ' NCODE =6 -  ORTHOG AXES ARE DEFINED TO HAVE'
C                    A  PARALLEL TO XO   BSTAR PARALLEL TO YO'
C
C   SET UP MATRICES TO ORTHOGONALISE H K L AND X Y Z FOR THIS CELL.
C
C     .. Scalar Arguments ..
      REAL VOLL
C     ..
C     .. Array Arguments ..
      REAL CEL(6),RRR(3,3,6)
C     ..
C     .. Scalars in Common ..
      REAL VOL
C     ..
C     .. Arrays in Common ..
      REAL AC,CELL,CELLAS,RR
C     ..
C     .. Local Scalars ..
      REAL A,ALPH,ALPHAS,AS,B,BET,BETAS,BS,C,CONV,COSA,COSAS,COSB,COSBS,
     +     COSG,COSGS,CS,FCT,GAMM,GAMMAS,SINA,SINAS,SINB,SINBS,SING,
     +     SINGS,SUM,V
      INTEGER I,J,K,N,NCODE
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ATAN2,COS,SIN,SQRT
C     ..
C     .. Common blocks ..
      COMMON /RBREC/AC(6)
      COMMON /RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
C     ..
C     .. Save statement ..
      SAVE /RBRKZZ/, /RBREC/
C     ..
C
C---- Initialisations
C
      CELDEL = 0.0
      IF (CEL(1).GT.0.0) THEN
        IF (CELL(1).GT.0.0) THEN
          IWARN = 0
          DO 101 I = 1,6
            CELDEL = ABS(CEL(I)-CELL(I))/CEL(I)
            IF (CELDEL.GT.0.01) IWARN = 1
 101      CONTINUE
          IF(IWARN.NE.0) WRITE(6,9876)CELL,CEL
9876      FORMAT(' Inconsistency in Cell Dimensions - replacing old:',
     +      /,' Old cell:',3X,6F10.5,/,' New cell:',3X,6F10.5)
        ENDIF
        DO 10 I = 1,6
          CELL(I) = CEL(I)
          IF (CELL(I).EQ.0.0) call ccperr(1,
     +' **** Incorrect (0.0) CELL element in  subroutine rbfro1?? ****')
   10   CONTINUE
      ENDIF
C
C
      CONV = 3.14159/180.0
      FCT = 8.0*3.14159*3.14159
      ALPH = CELL(4)*CONV
      BET = CELL(5)*CONV
      GAMM = CELL(6)*CONV
      SUM = (ALPH+BET+GAMM)*0.5
      V = SQRT(SIN(SUM-ALPH)*SIN(SUM-BET)*SIN(SUM-GAMM)*SIN(SUM))
      VOL = 2.0*CELL(1)*CELL(2)*CELL(3)*V
      SINA = SIN(ALPH)
      COSA = COS(ALPH)
      SINB = SIN(BET)
      COSB = COS(BET)
      SING = SIN(GAMM)
      COSG = COS(GAMM)
      COSAS = (COSG*COSB-COSA)/ (SINB*SING)
      SINAS = SQRT(1.0-COSAS*COSAS)
      COSBS = (COSA*COSG-COSB)/ (SINA*SING)
      SINBS = SQRT(1.0-COSBS*COSBS)
      COSGS = (COSA*COSB-COSG)/ (SINA*SINB)
      SINGS = SQRT(1.0-COSGS*COSGS)
      A = CELL(1)
      B = CELL(2)
      C = CELL(3)
      AS = B*C*SINA/VOL
      BS = C*A*SINB/VOL
      CS = A*B*SING/VOL
      ALPHAS = ATAN2(SINAS,COSAS)/CONV
      BETAS  = ATAN2(SINBS,COSBS)/CONV
      GAMMAS = ATAN2(SINGS,COSGS)/CONV
      CELLAS(1) = AS
      CELLAS(2) = BS
      CELLAS(3) = CS
      CELLAS(4) = ALPHAS
      CELLAS(5) = BETAS
      CELLAS(6) = GAMMAS
C
C---- Set useful things for calculating dstar
C
      AC(1) = AS*AS
      AC(2) = BS*BS
      AC(3) = CS*CS
      AC(4) = 2.0*BS*CS*COSAS
      AC(5) = 2.0*CS*AS*COSBS
      AC(6) = 2.0*AS*BS*COSGS
C
C---- Zero matrices
C
      DO 40 N = 1,6
        DO 30 I = 1,3
          DO 20 J = 1,3
            RR(I,J,N) = 0.0
   20     CONTINUE
   30   CONTINUE
   40 CONTINUE
C
C---- Calculate matrices
C
C---- XO along a  Zo along c*
C
      NCODE = 1
      RR(1,1,NCODE) = A
      RR(1,2,NCODE) = B*COSG
      RR(1,3,NCODE) = C*COSB
      RR(2,2,NCODE) = B*SING
      RR(2,3,NCODE) = -C*SINB*COSAS
      RR(3,3,NCODE) = C*SINB*SINAS
C
C---- XO along b  Zo along a*
C
      NCODE = 2
      RR(1,1,NCODE) = A*COSG
      RR(1,2,NCODE) = B
      RR(1,3,NCODE) = C*COSA
      RR(2,1,NCODE) = -A*SING*COSBS
      RR(2,3,NCODE) = C*SINA
      RR(3,1,NCODE) = A*SING*SINBS
C
C---- XO along c  Zo along b*
C
      NCODE = 3
      RR(1,1,NCODE) = A*COSB
      RR(1,2,NCODE) = B*COSA
      RR(1,3,NCODE) = C
      RR(2,1,NCODE) = A*SINB
      RR(2,2,NCODE) = -B*SINA*COSGS
      RR(3,2,NCODE) = B*SINA*SINGS
C
C---- trigonal only - XO along a+b  YO alon a-b  Zo along c*
C
      NCODE = 4
      RR(1,1,NCODE) = A/2.0
      RR(1,2,NCODE) = A/2.0
      RR(2,1,NCODE) = -A*SING
      RR(2,2,NCODE) = A*SING
      RR(3,3,NCODE) = C
C
C---- XO along a*   ZO along c
C
      NCODE = 5
      RR(1,1,NCODE) = A*SINB*SINGS
      RR(2,1,NCODE) = -A*SINB*COSGS
      RR(2,2,NCODE) = B*SINA
      RR(3,1,NCODE) = A*COSB
      RR(3,2,NCODE) = B*COSA
      RR(3,3,NCODE) = C
C
C---- Grr*! to  Gerard Bricogne - his setting for P1 in SKEW.
C     XO along a  Yo along b*
C
      NCODE = 6
      RR(1,1,NCODE) = A
      RR(1,2,NCODE) = B*COSG
      RR(1,3,NCODE) = C*COSB
      RR(2,2,NCODE) = B*SING*SINAS
      RR(3,2,NCODE) = -B*SING*COSAS
      RR(3,3,NCODE) = C*SINB
C
C---- copy rr(...) into rrr(...)
C
      DO 11 I=1,6
        CEL(I) = CELL(I)
11    CONTINUE
      DO 70 I = 1,6
        DO 60 J = 1,3
          DO 50 K = 1,3
            RRR(K,J,I) = RR(K,J,I)
   50     CONTINUE
   60   CONTINUE
   70 CONTINUE
C
C
      VOLL = VOL
      IF (CELDEL.GT.0.01) VOLL = -VOL
C
C
      END
C
C
      SUBROUTINE CVFRAC2(X,Y,Z,XX,YY,ZZ,IFLAG)
C     ============================================
C
C_BEGIN_CVFRAC2
C
C      This subroutine is used to convert between the stored  orthogonal  and
C fractional coordinates using the  matrices  set  up  in  the  common  block
C /ORTHOG/ by the subroutine XYZOPEN. If no matrices have been set up then the
C program will stop with an error message.
C                                         
C Call:  CALL CVFRAC2(X,Y,Z,XX,YY,ZZ,IFLAG)
C                                             
C Arguments:
C            
C       X (I)   (REAL)  Input coordinates.
C       Y (I)   (REAL)       "
C       Z (I)   (REAL)       "
C      XX (O)   (REAL)  Output coordinates.
C      YY (O)   (REAL)       "
C      ZZ (O)   (REAL)       "
C   IFLAG (I) (INTEGER)  Flag =0, Convert coordinates from fractional to orthogonal
C                             =1, Convert coordinates from orthogonal to fractional
C
C_END_CVFRAC2
C
      LOGICAL IFCRYS,IFSCAL,MATRIX,IFHDOUT
      COMMON /RBRKXX/IFCRYS,IFSCAL,ITYP,MATRIX,IFHDOUT
      COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE
      SAVE /RBRKXX/,/ORTHOG/
C
C---- Check that matrices set up
C
      IF(.NOT.MATRIX)GO TO 800
C
C---- Perform transformation
C
      IF(IFLAG.NE.0)GO TO 10
      XX=RO(1,1)*X + RO(1,2)*Y +RO(1,3)*Z +RO(1,4)
      YY=RO(2,1)*X + RO(2,2)*Y +RO(2,3)*Z +RO(2,4)
      ZZ=RO(3,1)*X + RO(3,2)*Y +RO(3,3)*Z +RO(3,4)
      RETURN
10    XX=RF(1,1)*X + RF(1,2)*Y +RF(1,3)*Z +RF(1,4)
      YY=RF(2,1)*X + RF(2,2)*Y +RF(2,3)*Z +RF(2,4)
      ZZ=RF(3,1)*X + RF(3,2)*Y +RF(3,3)*Z +RF(3,4)
      RETURN
C
C---- Error condition
C
800   CALL CCPERR(4,' **FRACTIONAL/ORTHOGONAL MATRICES NOT SET UP**')
      call ccperr(1,' No knowledge of input orthogonalisation')
C
C---- Format statements
C
      END
C
C
C
      SUBROUTINE CVFRAC(X,Y,Z,XX,YY,ZZ,IFLAG,MSG)
C     ===========================================
C
C_BEGIN_CVFRAC
C
C	Another silly obsolete routine that really should be deleted.
C MSG value is in fact useless. Library output controlled by CCPERR.
C
C_END_CVFRAC
C
C     .. Scalar Arguments ..
      REAL X,Y,Z,XX,YY,ZZ
      INTEGER IFLAG,MSG
C     ..
C     .. External Routines ..
      EXTERNAL CVFRAC2
C     ..
      CALL CVFRAC2(X,Y,Z,XX,YY,ZZ,IFLAG)

      RETURN
      END
C
C
C
      SUBROUTINE CVANISOB(B,IFLAG)
C     THIS SUBROUTINE SHOULD NOT BE USED
C     SEE CVANISOU
      REAL B(6)
      INTEGER IFLAG
      EXTERNAL CVANISOu, CCPERR
      WRITE (6,*) 'ERR: THIS PROGRAM USES S/R CVANISOB'
      WRITE (6,*) 'ERR: IT SHOULD NOT USE THIS ROUTINE'
      WRITE (6,*) 'ERR: CVANISOU IS CALLED AUTOMATICALLY'
      CALL CCPERR(2, 'CHANGE YOUR CODE')
      CALL CVANISOU(B, IFLAG)
      RETURN
      END
C
C
C
C
      SUBROUTINE CVANISOU(U,IFLAG)
C     ============================
C
C_BEGIN_CVANISOU
C
C      This subroutine is used to convert between crystallographic bs and 
C orthogonal Us or the other way round. The orthogonal matrices are 
C required, if no matrices have been set up then the program will stop 
C with an error message. The temperature factors are defined below;
C
C  PDB files contain anisotropic temperature factors as orthogonal Us.
C The anisotropic temperature factors can be input/output to this routine 
C  as orthogonal or as crystallographic Us. 
C  
C  Shelx defines Uf to calculate temperature factor as:
C T(aniso_Uf) = exp (-2PI**2 ( (h*ast)**2 Uf_11 + (k*bst)**2 Uf_22 + ... 
C                            + 2hk*ast*bst*Uf_12 +..)
C
C   Note:   Uo_ji == Uo_ij and  Uf_ji == Uf_ij.
C
C  [Uo_ij] listed on ANISOU card satisfy  the relationship:
C  [Uo_ij] =   [RFu]-1 [Uf_ij] {[RFu]-1}T   
C
C        where [Rfu] is the normalised [Rf] matrix read from the SCALEi cards.
C        see code.   [ROu] ==  [RFu]-1
C  Hence:
C  [Uf_ij] =   [RFu]   [Uo_ij] {[RFu]  }T   
C
C T(aniso_Uo) = U(11)*H**2 + U(22)*K**2 + 2*U(12)*H*K + ...
C where H,K,L are orthogonal reciprocal lattice indecies. ( EJD: I think????)
C
C Biso     = 8*PI**2 (Uo_11 + Uo_22 + Uo_33) / 3.0
C
C   [Uf(symm_j)] = [Symm_j] [Uf] [Symm_j]T
C
C 
C Arguments:
C            
C    U(6) (I/O  (REAL)  Input coordinates.
C   IFLAG (I) (INTEGER)  Flag =0, Convert coordinates from fract. to orthog.
C                             =1, Convert coordinates from orthog. to fract.
C
C_END_CVANISOU
C
C     .. Arguments ..
      REAL U(6)
      INTEGER IFLAG
C     ..
C     .. Variables in Common ..
      REAL RF,RO
      INTEGER NCODE
      LOGICAL IFCRYS,IFSCAL,MATRIX,IFHDOUT
C     ..
C     .. Local Variables ..
      INTEGER I,J
      REAL TWOPI2
      REAL A(3,3),AT(3,3),TMP(3,3),TMPMAT(3,3)
C     ..
C     .. External Routines ..
      EXTERNAL CCPERR,MATMUL
C     ..
C     .. Common Blocks ..
      COMMON /RBRKXX/IFCRYS,IFSCAL,ITYP,MATRIX,IFHDOUT
      COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE
      COMMON /ORTHOGU/ ROU(4,4),RFU(4,4)
C     ..
C     .. Save Statement ..
      SAVE /RBRKXX/,/ORTHOG/,/ORTHOGU/
C     ..
C     .. Data Statements ...
      DATA TWOPI2 /19.739209/
C     ..
C
C---- Check that matrices set up
C
      IF(.NOT.MATRIX)GO TO 800
C
C---- Perform transformation
C
        TMP(1,1)=U(1)
        TMP(2,2)=U(2)
        TMP(3,3)=U(3)
        TMP(1,2)=U(4)
        TMP(2,1)=U(4)
        TMP(1,3)=U(5)
        TMP(3,1)=U(5)
        TMP(2,3)=U(6)
        TMP(3,2)=U(6)
C
C   IFLAG (I) (INTEGER)  Flag =0, Convert coordinates from fract. to orthog.
C   IFLAG (I) (INTEGER)  Flag =1, Convert coordinates from orthog. to fract.
C
      IF (IFLAG .EQ. 0) THEN
        DO 10 I=1,3
          DO 10 J=1,3
            A(J,I)=ROU(J,I)
            AT(I,J)=ROU(J,I)
   10   CONTINUE
      ELSE
        DO 20 I=1,3
          DO 20 J=1,3
            A(J,I) = RFU(J,I)
            AT(I,J) = RFU(J,I)
   20   CONTINUE
      ENDIF
C
        CALL MATMUL(TMPMAT,TMP,AT)
        CALL MATMUL(TMP,A,TMPMAT)
        U(1) = TMP(1,1)
        U(2) = TMP(2,2)
        U(3) = TMP(3,3)
        U(4) = TMP(1,2)
        U(5) = TMP(1,3)
        U(6) = TMP(2,3)

      RETURN
C
C---- Error condition
C
800   CALL CCPERR(4,' **FRACTIONAL/ORTHOGONAL MATRICES NOT SET UP**')
      call ccperr(1,' No knowledge of input orthogonalisation')
C
C---- Format statements
C
      END
C
C
C
      SUBROUTINE RBCELL(CELLD,CVOL)
C     ============================
C
C_BEGIN_RBCELL
C
C      SUBROUTINE RBCELL(CELLD,CVOL)
C
C Returns cell dimensions and unit cell  volume.
C
C PARAMETERS
C     CELLD (O)  (REAL(6))  cell dimensions
C     CVOL (O)  (REAL)     cell volume
C
C Common blocks
C
C      COMMON/RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
C
C_END_RBCELL
C
      COMMON/RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
      REAL CELL,RR,VOL,CELLAS
      REAL CELLD(6), CVOL
      SAVE /RBRKZZ/
C
      CVOL = VOL
      DO 1 I=1,6
1       CELLD(I) = CELL(I) 
      END
C
C
C
      SUBROUTINE RBRCEL(RCEL,RVOL)
C     ============================
C
C_BEGIN_RBRCEL
C
C      SUBROUTINE RBRCEL(RCEL,RVOL)
C
C THIS SUBROUTINE RETURNS Reciprocal cell dimensions, and reciprocal
C                       unit cell  volume.
C
C PARAMETERS
C     RCEL (O)  (REAL(6)) reciprocal cell dimensions
C     RVOL (O)  (REAL)    reciprocal cell volume
C
C Common blocks
C
C      COMMON/RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
C
C_END_RBRCEL
C
      COMMON/RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
      REAL RCEL(6),RVOL
      REAL CELL,CELLAS,RR,VOL
      SAVE /RBRKZZ/
C
      IF (VOL.EQ.0.0) THEN
        RVOL = 0.0
      ELSE
        RVOL = 1.0/VOL
      ENDIF
      DO 1 I=1,6
1       RCEL(I) = CELLAS(I) 
      RETURN
      END
C
C
C
      SUBROUTINE RBSPGRP(SPGRP)
C     ============================
C
C_BEGIN_RBSPGRP
C
C      SUBROUTINE SUBROUTINE RBSPGRP(SPGRP)
C
C Returns spacegrpup from pdb
C
C PARAMETERS
C     SPGRP (O) (CHARACTER*11) 
C
C Common blocks
C
C      COMMON /RBRKSPGRP/BRKSPGRP
C
C_END_RBSPGRP
C
      CHARACTER SPGRP*(*)
      CHARACTER BRKSPGRP*11
      INTEGER ILEN,KLEN,J
      COMMON /RBRKSPGRP/BRKSPGRP
      SPGRP = BRKSPGRP
C
C Make sure that the returned name is left-justified
C
      ILEN = LENSTR(SPGRP)
      KLEN = ILEN
C
      DO J = 1,ILEN-1
        IF (SPGRP(1:1) .EQ. ' ') THEN
          SPGRP = SPGRP(2:KLEN)
          KLEN  = KLEN - 1
        END IF
      END DO
C
      END
C
C
C
      SUBROUTINE WBSPGRP(SPGRP)
C     =============================
C
C_BEGIN_WBSPGRP
C
C      SUBROUTINE WBSPGRP(SPGRP)
C
C Sets the internal spacegroup of a pdb file
C
C PARAMETERS
C     SPGRP (I) (CHARACTER*11)
C
C Common Blocks
C
C      COMMON /RBRKSPGRP/BRKSPGRP
C
C_END_WBSPGRP
C
      CHARACTER SPGRP*(*)
      CHARACTER BRKSPGRP*11
      COMMON /RBRKSPGRP/BRKSPGRP
      BRKSPGRP = SPGRP
      END
C
C
C
      SUBROUTINE RES3TO1(RESNM3,RESNM1)
C     ================================
C
C_BEGIN_RES3TO1
C
C      SUBROUTINE RES3TO1(RESNM3,RESNM1)
C
C       FIND 3 CHARACTER RESIDUE NAME FROM 1 CHARACTER CODE OR
C       FIND 1 CHARACTER RESIDUE NAME FROM 3 CHARACTER CODE.
C       SUBROUTINE IS CALLED WITH EITHER RESNM3 OR RESNM1 PREVIOUSLY 
C       ASSIGNED, AND THE OTHER IS ASSIGNED  HERE.
C 
C Parameters
C
C   RESNM3 (I/O)  CHAR*4    3 character residue name
C   RESNM1 (I/O)  CHAR*1    1 character residue name
C
C_END_RES3TO1
C
      CHARACTER*4 RESNM3
      CHARACTER*1 RESNM1
      CHARACTER*4 MAACD3(26)
      CHARACTER*1 MAACD1(26)
      DATA NAACID/26/
      DATA MAACD3/'ALA ','ARG ','ASN ','ASP ','CYS ','CYH ','GLN ',
     1 'GLU ','GLY ','HIS ','ILE ','LEU ','LYS ','MET ','PHE ','PRO ',
     2 'SER ','THR ','TRP ','TYR ','VAL ','HEM ','WAT ','SUL ','END ',
     3 'DUM '/
      DATA MAACD1/'A','R','N','D','C','C','Q',
     1 'E','G','H','I','L','K','M','F','P',
     2 'S','T','W','Y','V','X','O','U','Z','Z'/
C
C---- Routine to find one character amino acid name
C
        IF(RESNM3.NE.' ')THEN
          DO 1 I=1,NAACID
          IF(RESNM3.EQ.MAACD3(I)) GO TO 2
1         CONTINUE
        I=NAACID
2       RESNM1=MAACD1(I)
        RETURN
        ENDIF
C
C---- Routine to find three character amino acid name
C
        IF(RESNM1.NE.' ')THEN
          DO 11 I=1,NAACID
          IF(RESNM1.EQ.MAACD1(I)) GO TO 12
11        CONTINUE
        I=NAACID
12      RESNM3=MAACD3(I)
        RETURN
        ENDIF
      END
C
C
C
        SUBROUTINE RBRECIP(IH,IK,IL,S)
C       ==============================
C
C_BEGIN_BRECIP
C
C        SUBROUTINE RBRECIP(IH,IK,IL,S)
C
C---- This subroutine calculates 4SIN**2/L**2
C
C PARAMETERS
C         IH,IK,IL (I) (INTEGER)  reflection indices
C                S (O) (REAL)     4SIN**2/L**2
C
C_END_BRECIP
C
      COMMON /RBREC/AC(6)
      SAVE /RBREC/
C
      S = 
     .(AC(1)*IH*IH+AC(2)*IK*IK+AC(3)*IL*IL
     .+AC(4)*IK*IL+AC(5)*IL*IH+AC(6)*IH*IK)
      RETURN
      END


C     =====================================================
      SUBROUTINE SFREAD2(ID,NG,A,B,C,IWT,IELEC,CU,MO,Ifail)
C     =====================================================
C
C  Inputs: ID     atom identifier
C          This should match an atom type in the atomsf.lib
C          If an atom is identified as NE2+ say, characters are 
C          subtracted from the ID till a match is found, or there are 
C          no characters left. 
C          EG: Routine tests first NE2+, then NE2, then NE, then N.
C            All matching checks UPPER CASE strings.
C
C          NG     num. of gaussian approximations (2 or 5 (default))
C          IFILFF  .TRUE. if want to open the library file assigned
C                 to ATOMSF (default `atomsf.lib')
C
C  Output: A(4)   coefficient for structure factor calculation
C          B(4)   coefficient for structure factor calculation
C          C      coefficient for structure factor calculation
C          IWT    atomic weight
C          IELEC  number of electrons
C          CU(2)  delta F' and delta F'' for Cu
C          MO(2)  delta F' and delta F'' for Mo
C          Ifail  = -1 if atom not found at all
C                 =  0 OK
C                 =  1 for two gaussian case that does not exist
C
C     .. Scalar Arguments ..
      REAL C
      INTEGER IELEC,Ifail,IWT,NG
      CHARACTER ID*4,IDCHK*4
C     ..
C     .. Array Arguments ..
      REAL A(4),B(4),CU(2),MO(2)
C     ..
C     .. Local Scalars ..
      INTEGER NGauss,IOS
      CHARACTER ID2*6,IDIN*6
      LOGICAL OP
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPDPN,CCPERR
C     ..
      Ifail  = -1
       IDCHK = ID
       CALL CCPUPC(IDCHK)
      ID2    = IDCHK//'  '
      NGauss = NG
C
      IF (NGauss.EQ.2) THEN
        ID2(6:6) = '2'
      ELSE
        NGauss = 5
      END IF
C
C---- Check to open file
C
      INQUIRE (UNIT=45, OPENED=OP, IOSTAT=IOS)
      IF (IOS .NE. 0) CALL CCPERR(1,'Error opening ATOMSF file')

      IF (.NOT.OP) THEN
        Ifail  = 1
        CALL CCPDPN(45,'ATOMSF','READONLY','F',0,Ifail)
        IF (Ifail.LT.0) CALL CCPERR(1,'Error opening library file')
      ELSE
        REWIND 45
      END IF
C
C
C---- Search for identifier
        IFAIL = -1
        LID = LENSTR(ID)
       DO 25 NID = LID,1,-1
       REWIND 45
C
   10 CONTINUE
      READ (45,FMT=6002,END=50,ERR=40) IDIN
C
      CALL CCPUPC(IDIN)
      IF (ID2(1:NID).EQ.IDIN(1:NID)) THEN
        Ifail = 1
        IF (NGauss.NE.2 .OR. IDIN(6:6).NE.' ') GO TO 60
      END IF
C
      GO TO 10
C
C---- Error reading library file
C
   40 CALL CCPERR(1,'Error reading library file')
C
C---- No match
C
C---- No match
C
   50 CONTINUE
      CALL CCPERR
     +  (0,' No match for full atom ID - subtract one character ')
        IF(NID.GT.1)ID2 = ID2(1:NID-1)//' '//ID2(NID+1:6)
        IF(NID.GT.1)ID  = ID (1:NID-1)//'    '
  25  CONTINUE
      CALL CCPERR(1,'No match for atom ID')
C
C---- Matched atom
C
   60 READ (45,FMT=6006) IWT,IELEC,C
      READ (45,FMT=6008) A(1),A(2),A(3),A(4)
      READ (45,FMT=6008) B(1),B(2),B(3),B(4)
      READ (45,FMT=6008) CU(1),CU(2),MO(1),MO(2)
      Ifail = 0
C
C---- Format statements
C
 6002 FORMAT (A6)
 6006 FORMAT (2X,I8,2X,I8,2X,F14.6)
 6008 FORMAT (4 (2X,F14.6))
      END
C
C
C
      SUBROUTINE SFREAD(ID,NG,A,B,C,IWT,IELEC,CU,MO,IFAIL,IFILFF)
C     ===========================================================
C
C_BEGIN_SFREAD
C
C	Obsolete routine should be deleted. IFILFF not used.
C
C_END_SFREAD
C
C     .. Scalar Arguments ..
      REAL C
      INTEGER IELEC,Ifail,IWT,NG
      LOGICAL IFILFF
      CHARACTER ID*4
C     ..
C     .. Array Arguments ..
      REAL A(4),B(4),CU(2),MO(2)
C     ..
C     .. External Routines ..
      EXTERNAL SFREAD2
C     ..
      CALL SFREAD2(ID,NG,A,B,C,IWT,IELEC,CU,MO,IFAIL)
 
      RETURN
      END
C
C
C
        SUBROUTINE WBCELL(IUNIT,ARGCELL,ARGNCODE)
C       =========================================
C
C_BEGIN_WBCELL
C
C   This subroutine writes out the cell and orthogonalisation matrices, to 
C the output file. If the input parameters are null then the cell etc. are
C taken from the COMMON blocks.
C
C PARAMETERS
C
C            IUNIT (I) (INTEGER)   Channel number for output file.
C
C       ARGCELL(6) (I) (REAL)      crystallographic cell taken from COMMON
C                                  if cell = 0
C         ARGNCODE (I) (INTEGER)   NCODE number taken from COMMON if NCODE=0
C
C_END_WBCELL
C
C     .. Parameters ..
      INTEGER MAXFILESOPEN
      PARAMETER (MAXFILESOPEN=10)
C     ..
C     .. Agruments ..
      REAL ARGCELL(6)
      INTEGER ARGNCODE,IUNIT
C     ..
C     .. Variables in Common ..
      REAL CELL, RO, RF, RR
      INTEGER FILESOPEN, NCODE, TYPE, UNIT
      CHARACTER*80 LOGUNIT
      CHARACTER BRKSPGRP*11
      LOGICAL IFCRYS,IFSCAL,MATRIX,IFHDOUT
C     ..
C     .. Local Scalars ..
      INTEGER I, II, J
      CHARACTER*80 ERRLIN
C     ..
C     .. External Routines/Functions ..
      EXTERNAL CCPERR,RBFROR,RBRINV
C     ..
C     .. Common Blocks ..
      COMMON /RBRKAA/ FILESOPEN,LOGUNIT(MAXFILESOPEN),
     +                UNIT(MAXFILESOPEN),TYPE(MAXFILESOPEN)
      COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE
      COMMON /RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
      COMMON /RBRKXX/ IFCRYS,IFSCAL,ITYP,MATRIX,IFHDOUT
      COMMON /RBRKSPGRP/BRKSPGRP
C     ..
      SAVE /ORTHOG/,/RBRKAA/,/RBRKXX/,/RBRKZZ/

      II = 0
      DO 10 I=1,FILESOPEN
        IF (IUNIT .EQ. UNIT(I)) THEN
          II = I
          GOTO 20
        ENDIF
   10 CONTINUE

      ERRLIN = ' ERROR: in WBCELL file has not been opened'
      CALL CCPERR(1,ERRLIN)

   20 IF (TYPE(II) .GT. 0) THEN
        ERRLIN = ' ERROR: in WBCELL file is of type input'
        CALL CCPERR(1,ERRLIN)
      ENDIF
      IF (ARGCELL(1) .EQ. 0.0) THEN
        IF (IFCRYS) WRITE (UNIT(II),100) CELL,BRKSPGRP
      ELSE
        WRITE(UNIT(II),100) ARGCELL,BRKSPGRP
      ENDIF

      IF (ARGNCODE .EQ. 0) THEN
        IF (IFCRYS) WRITE (UNIT(II),200) (I,(RF(I,J),J=1,3),I=1,3)
      ELSE
        DO 30 I = 1,6
          CELL(I) = ARGCELL(I)
   30   CONTINUE

        CALL RBFROR
        DO 40 I = 1,3
          DO 40 J = 1,3
            RO(J,I) = RR(J,I,ARGNCODE)
   40   CONTINUE

        RO(4,4) = 1.0     
        DO 50 I=1,3
          RO(I,4) = 0.0
   50   CONTINUE

        CALL RBRINV(RO,RF)
        WRITE (UNIT(II),200) (I,(RF(I,J),J=1,3),I=1,3)
      ENDIF

      IFHDOUT = .TRUE.
      RETURN
C
C---- Format Statements
C
 100  FORMAT('CRYST1',3F9.3,3F7.2,1x,a11)
 200  FORMAT( 2('SCALE',I1,4X,3F10.6,5X,'   0.00000',/),
     $          'SCALE',I1,4X,3F10.6,5X,'   0.00000')
      END
C
C
C
        SUBROUTINE WREMARK(IUNIT,LINE)
C       ==============================
C
C_BEGIN_WREMARK
C
C	This subroutine writes a line to the output file. Its main use is for 
C REMARK statements in PDB.
C
C Parameters:
C
C            IUNIT (I) (CHARACTER*(*))  Channel number
C             LINE (I) (CHARACTER*(*))  line to be written, best
C                                       if declared as *80
C
C_END_WREMARK
C
C     .. Parameters ..
      INTEGER MAXFILESOPEN
      PARAMETER (MAXFILESOPEN=10)
C     ..
C     .. Arguments ..
      INTEGER IUNIT
      CHARACTER*(*) LINE
C     ..
C     .. Variables in Common ..
      INTEGER FILESOPEN,TYPE,UNIT
      CHARACTER*80 LOGUNIT
C     ..
C     .. Locals ..
      INTEGER II
      CHARACTER OUTLIN*80,ERRLIN*80
C     ..
C     .. Common Blocks ..
      COMMON /RBRKAA/ FILESOPEN,LOGUNIT(MAXFILESOPEN),
     +                UNIT(MAXFILESOPEN),TYPE(MAXFILESOPEN)
C     ..
C     .. Save Statement ..
      SAVE /RBRKAA/
C     ..
C
C---- The remark line will be truncated if it > 80 characters
C     this fits with PDB and CIF syntax.
C
      II = 0
      DO 10 I=1,FILESOPEN
        IF (IUNIT .EQ. UNIT(I)) THEN
          II = I
          GOTO 20
        ENDIF
   10 CONTINUE

      ERRLIN = ' ERROR: in WREMARK file has not been opened'
      CALL CCPERR(1,ERRLIN)

   20 OUTLIN = LINE
      WRITE (UNIT(II),FMT='(A)') OUTLIN

      RETURN
      END
C
C<FF>
C
      SUBROUTINE RWBFIN(IUN,IOUT)
C     ===========================
C
C_BEGIN_RWBFIN
C
C	This subroutine copies remaining lines straight from input to 
C output. 
C
C_END_RWBFIN
C
C     .. Scalar Arguments ..
      INTEGER IUN,IOUT
C     ..
C     .. External Routines ..
      EXTERNAL XYZADVANCE

   10 CALL XYZADVANCE(IUN,IOUT,0,*1000,*1000)
      CALL XYZADVANCE(IOUT,0,0,*1000,*1000)
      GOTO 10

 1000 RETURN
      END
C
C
C
      SUBROUTINE RWNOHEAD()
C     =====================
C
C_BEGIN_RWNOHEAD
C
C	This subroutine resets the logical variable IFHDOUT in the RWBROOK
C     common block RBRKXX, and should be called once before either
C     XYZADVANCE or WBCELL in order to prevent those routines from writing
C     headers to an output pdb file.
C     Effectively we are fooling the library that the header has already
C     been written.
C
C_END_RWNOHEAD
C
C     .. Arguments in common ..
      INTEGER ITYP
      LOGICAL IFCRYS,IFHDOUT,IFSCAL,MATRIX
C
C     .. Common blocks ..
      COMMON /RBRKXX/ IFCRYS,IFSCAL,ITYP,MATRIX,IFHDOUT
C
      IFHDOUT = .TRUE.
C
      RETURN
      END
C
C
      SUBROUTINE NUM_EXPECTED_WATERS(RESO,TEMP,FHOH,SEFHOH)
C     =====================================================
C
C_BEGIN_NUM_EXPECTED_WATERS
C
C Arguments:
C
C         RESO    (I)   REAL           Resolution in A.
C         TEMP    (I)   CHARACTER*4    'ROOM' or 'LOWT' for temperature.
C         FHOH    (O)   REAL           Prediction of N_HOH/N_at
C         SEFHOH  (O)   REAL           Standard error of FHOH
C
C     Given the resolution, this routine returns the number of water
C     molecules expected to be determined by PX (and the associated
C     standard error) as a fraction of the number of protein atoms,
C     as estimated by the statistical analysis of Carugo & Bordo, 
C     Acta Cryst D, 55, 479 (1999). Two expressions are given, one
C     for room temperature structures and one for low temperature
C     structures.
C
C_END_NUM_EXPECTED_WATERS
C
      CHARACTER*4 TEMP,TTEMP
      REAL RESO,FHOH,SEFHOH

      TTEMP = TEMP
      CALL CCPUPC(TTEMP)
      IF (TTEMP.EQ.'ROOM') THEN

        FHOH = 0.301 - 0.095*RESO
        SEFHOH = 0.092 * SQRT(0.00114 + 0.005*(RESO - 2.3)**2)

      ELSEIF (TTEMP.EQ.'LOWT') THEN

        FHOH = 0.334 - 0.110*RESO
        SEFHOH = 0.043 * SQRT(0.030 + 0.167*(RESO - 2.2)**2)

      ENDIF

      RETURN
      END
C
C
