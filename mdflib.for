      SUBROUTINE ABORTV (MESSAG)
C
C      SUBROUTINE TO ABORT A JOB
C      VAX-SPECIFIC
C
      EXTERNAL HANDLER
      CHARACTER*(*)  MESSAG
      CALL LIB$ESTABLISH(HANDLER)
      WRITE(*,'(/,'' SEVERE ERROR:  '',A,/,'' JOB ABORTED'',/)')
     .MESSAG
      CALL LIB$STOP
      RETURN
      END
C
C
C
      SUBROUTINE ADDATA(MDFIN,MDFOUT,NKEEP,NADD,DATA,IHKL)
C     ====================================================
C
C
C
C---- Subroutine  to  add new data to  a  master data file.
C     data can be added both to an existing MDF record  or,
C     if  the appropriate hkl record  is not present in the
C     input MDF, new hkl records will be created.
C
C---- NKEEP items from the input MDF will be kept;  columns
C     NKEEP+1  to  NKEEP+NADD  will  be filled with the new data.
C
C---- The calling program should handle reading and writing
C     of  the  master  data file header records by means of
C     routines rdiMDF and wriMDF.
C
C     A  different  way  of  adding  new data to an MDF is
C     provided by subroutine  REPLCE.
C
C
C---- Parameters in calling statement:
C     ================================
C
C       MDFIN  : unit number on which  input MDF resides
C       MDFOUT : unit number on which output MDF resides
C       NKEEP  : number of items in input MDF to be kept
C       NADD   : number of items to add to the MDF (stored in
C                array data(nadd)
C       DATA   : array data(nadd) contains the new data
C       IHKL   : array ihkl(3) contains h, k and l indices
C
C       In addition this subroutine needs parameters stored in the
C       following common blocks:
C
C   1.  COMMON /MDFPAR/ MAXR, MAXB, CELL(6), ISLOW, INTER, IFAST,
C      .                KLASS, ICENTR, ISCREW(3), IVERSN
C
C       MAXR     : length of rbufr array (should be same in
C                  input and output MDF)
C       MAXB     : maximum length of data buffer array on
C                  input MDF
C       CELL     : cell parameters
C       ISLOW    : slowest changing index
C       INTER    : intermediate changing index
C       IFAST    : fastest changing index
C       KLASS    : laue class (see MDF description)
C       ICENTR   : lattice centering parameters
C       ISCREW   : screw axis information
C       IVERSN   : MDF version number
C                  IVERSN = 0 : MDF from before april 15, 1987
C                  IVERSN = 1 : april 15, 1987 MDF version
C
C       Use subroutine RDIMDF to define this common block.
C
C
C       The following common blocks should not (not!) be modified
C       by any other part of the calling program :
C
C   2.  counts of what was done are kept in the common block /MDFCTS/:
C       a call to this subroutine with h=k=l=511 will force the
C       subroutine to copy the remaining records of the input MDF to
C       the output MDF, and print the counts:
C
C       COMMON / MDFCTS / NREAD, NOTMOD, NMODIF, NNEW, NDELET, NSKIP
C
C       NREAD  :  number of hkl records read from input MDF
C       NOTMOD :  number of hkl records written unchanged to MDFout
C       NMODIF :  number of modified hkl records written to MDFout
C       NNEW   :  number of completely new hkl records written
C                 to MDFout
C       NDELET :  number of empty records in MDF (all entries absent)
C                 these are deleted in the output MDF
C       NSKIP  :  number of times no valid data were present in array
C                 data (e.g. because length of this array was specified
C                 to be zero, and, at the same time, for this hkl no
C                 MDF record was present.
C
C
C   3.  information on the last MDF record read (and some other info
C       is kept in common block / MDFrec /.  (after adding data,
C       this subroutine always reads the next MDF record before
C       returning to the calling routine)
C
C       COMMON / MDFREC / RH(MAXRBF), BUFFER(MAXBUF), MDFHKL(3), IPOS2,
C      .                  MDFEND, LAST, FIRST
C
C       RH     : array of length maxrbf containing rbuffr info from
C                input MDF
C       MAXRBF : parameter specifying length of rh (set to = 6)
C       BUFFER : array of length maxbuf containing buffer info from
C                input MDF
C       MAXBUF : parameter specifying length of buffer (set to = 100)
C       MDFHKL : hkl values on input MDF record
C       IPOS2  : hkl packed into a 32 bit word according to the
C                ordering on the input MDF
C       MDFEND : 0/1 specifying that end of file on input MDF has
C                not/already been reached.
C       LAST   : h=511, k=511, l=511 packed into 32 bit word
C                (specifies final call to this subroutine)
C       FIRST  : logical : .true. for first call to this subroutine
C                .false. for subsequent calls.
C
C
C   4.  Parameters needed for fast calculation of the resolution of
C       a reflection are kept in common block / MDFRES /:
C
C       COMMON / MDFRES / A11, A21, A22, A31, A32, A33
C       the resolution can be calculated as follows:
C
C       DSTAR-SQUARED = IH*IH*A11 + IH*IK*A21 + IH*IL*A31
C                     + IK*IK*A22 + IK*IL*A32 + IL*IL*A33
C       SO:       RES = 1.0/SQRT(DSTAR_SQUARED)
C
C
C       The data in this common block are provided by subroutine
C       DCALC (called by ADDATA itself)
C
C
C
C---- Important for programmers:
C     =========================
C
C   1.  To ensure that all hkl records of the input MDF are written
C       to the output MDF,  a final call to this subroutine  should
C       be made with ihkl = (511, 511, 511).
C       In addition,  the routine will then print out the number of
C       hkl records  read,  written  (changed  and  unchanged)  and
C       created or deleted.
C
C   2.  This subroutine makes calls to the following subroutines:
C          -  RDREC
C          -  WRTREC
C          -  DCALC
C          -  PACK
C       These subroutines are available in CFBLIB.
C
C
C---- Version march 19, 1987  (bauke dijkstra)
C     =======================================
C
C
C
C     .. Parameters ..
      INTEGER MAXRBF,MAXBUF
      PARAMETER (MAXRBF=6,MAXBUF=100)
C     ..
C     .. Scalar Arguments ..
      INTEGER MDFIN,MDFOUT,NADD,NKEEP
C     ..
C     .. Array Arguments ..
      REAL DATA(NADD)
      INTEGER IHKL(3)
C     ..
C     .. Scalars in Common ..
      REAL A11,A21,A22,A31,A32,A33
      INTEGER ICENTR,IFAST,INTER,IPOS2,ISLOW,IVERSN,KLASS,LAST,LUNIN,
     +        LUNOUT,MAXB,MAXR,MDFEND,NDELET,NMODIF,NNEW,NOTMOD,NREAD,
     +        NSKIP
      LOGICAL FIRST
C     ..
C     .. Arrays in Common ..
      REAL BUFFER,CELL,RH
      INTEGER ISCREW,MDFHKL
C     ..
C     .. Local Scalars ..
      REAL ABSENT
      INTEGER I,I1,I2,I3,IDEL,IPOS1
C     ..
C     .. Local Arrays ..
      REAL BF(MAXBUF),RR(MAXRBF)
C     ..
C     .. External Subroutines ..
      EXTERNAL DCALC,PACK,RDREC,WRTREC
C     ..
C     .. Common blocks ..
      COMMON /AINOUT/LUNIN,LUNOUT
      COMMON /MDFCTS/NREAD,NOTMOD,NMODIF,NNEW,NDELET,NSKIP
      COMMON /MDFPAR/MAXR,MAXB,CELL(6),ISLOW,INTER,IFAST,KLASS,ICENTR,
     +       ISCREW(3),IVERSN
      COMMON /MDFREC/RH(MAXRBF),BUFFER(MAXBUF),MDFHKL(3),IPOS2,MDFEND,
     +       LAST,FIRST
      COMMON /MDFRES/A11,A21,A22,A31,A32,A33
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
      DATA FIRST/.TRUE./,ABSENT/-1.0E10/
C     ..
C
C---- If first time through calculate constants for rapid calculation
C     of resolution. needed to add new hkl records to an existing MDF
C     also check parameter values...
C
      IF (FIRST) THEN
C
C---- Check parameters...
C
        IF (MAXRBF.LT.MAXR .OR. MAXBUF.LT.MAXB) THEN
          WRITE (LUNOUT,FMT=
     +'(///,'' ERROR in subroutine ADDATA'',/,                  '' Param
     +eter MAXRBF is'',I5,'' but should be at'',                '' least
     +'',I5,/,'' Parameter MAXBUF is'',I5,                      '' but s
     +hould be at least'',I5,/)') MAXRBF,MAXR,MAXBUF,MAXB
          STOP 'Error in subroutine ADDATA'
        ELSE
C
C---- Determine constants for rapid calculation of resolution
C
          CALL DCALC(CELL)
C
C---- Set counters
C
          FIRST = .FALSE.
          CALL PACK(511,511,511,LAST)
          NOTMOD = 0
          NMODIF = 0
          NNEW = 0
          NDELET = 0
          NSKIP = 0
C
C---- And read first MDF record
C
          CALL RDREC(MDFIN,RH,MAXR,MDFHKL,BUFFER,MAXB,MDFEND,IVERSN)
          NREAD = 1
          IF (MDFEND.NE.0) THEN
            NREAD = 0
            MDFHKL(1) = 511
            MDFHKL(2) = 511
            MDFHKL(3) = 511
          END IF
          I1 = MDFHKL(ISLOW)
          I2 = MDFHKL(INTER)
          I3 = MDFHKL(IFAST)
          CALL PACK(I1,I2,I3,IPOS2)
        END IF
      END IF
C
C---- Calculate position number for the new data
C
      I1 = IHKL(ISLOW)
      I2 = IHKL(INTER)
      I3 = IHKL(IFAST)
      CALL PACK(I1,I2,I3,IPOS1)
C
C---- Check whether this is final input reflection
C     final hkl should have indices 511, 511, 511
C     in that case copy input MDF hkl records to MDFout
C     and print counts
C
      IF (IPOS1.NE.LAST) THEN
   10   CONTINUE
C
C---- Compare with position number from MDF
C
        IF ((IPOS2-IPOS1).EQ.0) THEN
          GO TO 50
        ELSE IF ((IPOS2-IPOS1).LE.0) THEN
C
C---- New data ahead of MDF data: write MDF record to output MDF,
C     read a new record, and try again
C
          IF (NKEEP.EQ.0) THEN
            NDELET = NDELET + 1
          ELSE
            CALL WRTREC(MDFOUT,RH,MAXR,MDFHKL,BUFFER,NKEEP,IDEL)
            IF (IDEL.NE.0) THEN
              NDELET = NDELET + 1
            ELSE
              NOTMOD = NOTMOD + 1
            END IF
          END IF
          IF (MDFEND.EQ.0) THEN
            CALL RDREC(MDFIN,RH,MAXR,MDFHKL,BUFFER,MAXB,MDFEND,IVERSN)
            IF (MDFEND.NE.0) THEN
              MDFHKL(1) = 511
              MDFHKL(2) = 511
              MDFHKL(3) = 511
            ELSE
              NREAD = NREAD + 1
            END IF
            I1 = MDFHKL(ISLOW)
            I2 = MDFHKL(INTER)
            I3 = MDFHKL(IFAST)
            CALL PACK(I1,I2,I3,IPOS2)
          END IF
          GO TO 10
        END IF
C
C---- MDF hkl ahead of input hkl
C     create a new record for the MDF
C
C
C---- Fill rh array (here called rr in order not to screw up
C     the existing rh array
C
        DO 20 I = 1,MAXR
          RR(I) = ABSENT
   20   CONTINUE
        RR(1) = IHKL(1)*IHKL(1)*A11 + IHKL(1)*IHKL(2)*A21 +
     +          IHKL(1)*IHKL(3)*A31 + IHKL(2)*IHKL(2)*A22 +
     +          IHKL(2)*IHKL(3)*A32 + IHKL(3)*IHKL(3)*A33
C
C---- Fill in data buffer array, but only if nadd.gt.0
C
        IF ((NADD).LE.0) THEN
          NSKIP = NSKIP + 1
        ELSE
          DO 30 I = 1, (NKEEP+NADD)
            BF(I) = ABSENT
   30     CONTINUE
          DO 40 I = 1,NADD
            BF(NKEEP+I) = DATA(I)
   40     CONTINUE
          CALL WRTREC(MDFOUT,RR,MAXR,IHKL,BF, (NKEEP+NADD),IDEL)
          IF (IDEL.EQ.1) THEN
            NDELET = NDELET + 1
          ELSE
            NNEW = NNEW + 1
          END IF
        END IF
        RETURN
C
C---- Input hkl and MDF hkl are the same:
C     add data to the data buffer and write the new data to the
C     output master data file. read a new record and return  to
C     the calling routine afterwards
C
   50   IF (NADD.GT.0) THEN
          DO 60 I = 1,NADD
            BUFFER(NKEEP+I) = DATA(I)
   60     CONTINUE
        END IF
        IF ((NKEEP+NADD).EQ.0) THEN
          NSKIP = NSKIP + 1
        ELSE
          CALL WRTREC(MDFOUT,RH,MAXR,MDFHKL,BUFFER, (NKEEP+NADD),IDEL)
          IF (IDEL.EQ.1) THEN
            NDELET = NDELET + 1
          ELSE
            NMODIF = NMODIF + 1
          END IF
        END IF
        IF (MDFEND.EQ.0) THEN
          CALL RDREC(MDFIN,RH,MAXR,MDFHKL,BUFFER,MAXB,MDFEND,IVERSN)
          IF (MDFEND.NE.0) THEN
            MDFHKL(1) = 511
            MDFHKL(2) = 511
            MDFHKL(3) = 511
          ELSE
            NREAD = NREAD + 1
          END IF
          I1 = MDFHKL(ISLOW)
          I2 = MDFHKL(INTER)
          I3 = MDFHKL(IFAST)
          CALL PACK(I1,I2,I3,IPOS2)
        END IF
      ELSE IF (MDFEND.NE.0) THEN
        WRITE (LUNOUT,FMT=6000) NREAD,NOTMOD,NMODIF,NNEW,NDELET,NSKIP
      ELSE
   70   CONTINUE
        IF (NKEEP.EQ.0) THEN
          NDELET = NDELET + 1
        ELSE
          CALL WRTREC(MDFOUT,RH,MAXR,MDFHKL,BUFFER,NKEEP,IDEL)
          IF (IDEL.EQ.1) THEN
            NDELET = NDELET + 1
          ELSE
            NOTMOD = NOTMOD + 1
          END IF
        END IF
        CALL RDREC(MDFIN,RH,MAXR,MDFHKL,BUFFER,MAXB,MDFEND,IVERSN)
        IF (MDFEND.EQ.0) THEN
          NREAD = NREAD + 1
          GO TO 70
        END IF
        WRITE (LUNOUT,FMT=6000) NREAD,NOTMOD,NMODIF,NNEW,NDELET,NSKIP
      END IF
C
C---- Format statements
C
 6000 FORMAT (///' Adding data to a MASTER DATA FILE:',//' Number of r',
     +       'ecords read from input MDF',I30,/' Number of hkl records',
     +       ' written unmodified to output MDF',I13,/' Number of hkl ',
     +       'records with new data written to output MDF',I10,/' Numb',
     +       'er of newly created hkl records written to output MDF',
     +       I10,/' Number of (EMPTY) hkl records not written to outpu',
     +       't MDF',I12,/' Number of input hkls with zero length data',
     +       ' BUFFER skipped',I10,///)
C
C
      END
C
C
C
      SUBROUTINE ADDVEC( A, B, AB, NX)
C     ================================
C
C
      DIMENSION A( NX), B( NX), AB( NX)
C
C
      DO 999 I = 1, NX
        AB( I) = A( I) + B( I)
999   CONTINUE
C
C
      RETURN
C
C
      END
C
C
C
      FUNCTION ARCTDG( Y, X)
C     ===============================
C
C
      IF ( X .EQ. 0.0 )  THEN
        IF ( Y .EQ. 0.0 )  A = 0.
        IF ( Y .GT. 0.0 )  A = 90.
        IF ( Y .LT. 0.0 )  A = -90.
      ELSE
        IF ( Y .EQ. 0.0 )  THEN
          IF ( X .GE. 0.0 )  A = 0.
          IF ( X .LT. 0.0 )  A = 180.
        ELSE
          A = ATAN2D( Y, X)
        END IF
      END IF
C
C
      ARCTDG = A
C
C
      RETURN
C
C
      END
      SUBROUTINE ATANQ(S,C,A)
C
C   SUBROUTINE FOR GIVING THE ANGLE IN RADIANS WHEN THE COSINE
C   AND SINE ARE KNOWN.
C   IT DIFFERS FROM ATAN2 ONLY IN THIS RESPECT THAT THE RESULTANT
C   ANGLE HAS A VALUE BETWEEN ZERO AND TWOPI WHILE    
C   ATAN2 RETURNS AN ANGLE BETWEEN -PI AND +PI
C
C
      A = ATAN2(S,C)
      IF(A.LT.0.0) A = A + 8.0*ATAN(1.0)
C
      RETURN
      END
      SUBROUTINE AX3D(H,V,IB,IE,NI,ANG,DL)
      DIMENSION H(8),V(8) 
C FIRST TICKMARK
      HOR=H(IB) 
      VER=V(IB) 
      CALL PLOT(HOR,VER,3)
      CALL PLOT(HOR+DL*COS(ANG),VER+DL*SIN(ANG),2)
      CALL PLOT(HOR,VER,2)
C 
      HD=(H(IE)-H(IB))/NI 
      VD=(V(IE)-V(IB))/NI 
      DO 10 I=1,NI
      HOR=H(IB)+I*HD
      VER=V(IB)+I*VD
      CALL PLOT(HOR,VER,2)
      CALL PLOT(HOR+DL*COS(ANG),VER+DL*SIN(ANG),2)
   10 CALL PLOT(HOR,VER,2)
      RETURN
      END 

      SUBROUTINE AXIS(X0,Y0,A0,N0,S0,T0,C0,D0)
COPYRIGHT (C) CERRITOS COMPUTER SERVICES, INC. 1976 
C 
C     DRAW AN AXIS WITH LABELLED TIC MARKS AT UNIT INTERVALS. 
C     AN OPTIONAL TITLE IS ALSO PRINTED.
C 
C        X0      X COORDINATE OF START OF AXIS
C        Y0      Y COORDINATE OF START OF AXIS
C        A0      CHARACTER STRING TO DESCRIBE AXIS
C        N0      NUMBER OF CHARACTERS IN STRING 
C                - ON CLOCKWISE SIDE OF AXIS (NORMAL FOR X) 
C                + ON COUNTER CLOCKWISE SIDE OF AXIS (NORMAL FOR Y) 
C        S0      LENGTH OF AXIS 
C        T0      ANGLE OF AXIS TO X AXIS OF PAPER 
C                0.0 FOR X-AXIS 
C                90.0 FOR Y-AXIS
C        C0      COORDINATE OF MINIMUM TICK ON AXIS 
C        D0      DISTANCE BETWEEN TICKS 
C  NOTE: C0 AND D0 ARE MOST EASILY SUPPLIED BY "SCALE" ARGUMENT 
C        XTREME(1) AND XTREME(2). 
C 
      REAL A0(1)
      N1=IABS(N0) 
      N2=S0+0.50
      B1=0.5*S0-0.042857*N1 
      B2=0.0857*(N1+1)
      T1=T0*0.017453294 
      T2=T0 
      T3=COS(T1)
      T4=SIN(T1)
      IF(N0.LT.0)GOTO 10
      B3=0.3675 
      B4=0.18 
      T5=0.1
      GOTO 20 
10    B3=-0.4375
      B4=-0.25
      T5=-0.1 
C 
C        DRAW AXIS AND TICK MARKS 
C 
20    T6=T5*T3
      T5=T5*T4
      X1=X0 
      Y1=Y0 
      DO 100 I=1,N2 
      X2=X1-T5
      Y2=Y1+T6
      CALL PLOT(X2,Y2,3)
      CALL PLOT(X1,Y1,2)
      X1=X1+T3
      Y1=Y1+T4
100   CALL PLOT(X1,Y1,2)
      X2=X1-T5
      Y2=Y1+T6
      CALL PLOT(X2,Y2,2)
C 
C     NUMBERS ALONG THE AXIS  (START FROM THE HIGH END AND WORK BACK) 
C 
      D1=D0 
      C1=C0+N2*D1 
      E1=0.0
      IF(D1.EQ.0.0)GOTO 140 
C 
C        BRING D0 (=D1) INTO THE RANGE 0.01 TO 100. 
C 
110   IF(D1.LT.100.0)GOTO 130 
      D1=D1*0.1 
      C1=C1*0.1 
      E1=E1+1.0 
      GOTO 110
120   D1=D1*10.0
      C1=C1*10.0
      E1=E1-1.0 
130   IF(D1.LT.0.01)GOTO 120
140   X2=X1-B4*T4-.0857*T3
      Y2=Y1+B4*T3-.0857*T4
      N2=N2+1 
      DO 150 I=1,N2 
      CALL NUMB(X2,Y2,0.10,C1,T2,2) 
      C1=C1-D1
      X2=X2-T3
150   Y2=Y2-T4
C 
C        LABEL THE AXIS 
C 
      X2=X0+B1*T3-B3*T4 
      Y2=Y0+B1*T4+B3*T3 
      CALL SYMBL(X2,Y2,0.10,A0,T2,N1) 
      IF(E1.EQ.0.0)RETURN 
C 
C        SCALE FACTOR 
C 
      X2=X2+B2*T3 
      Y2=Y2+B2*T4 
      CALL SYMBL(X2,Y2,0.10,5H(X10),T2,5) 
      X2=X2+.48*T3-.07*T4 
      Y2=Y2+.48*T4+.07*T3 
      CALL NUMB(X2,Y2,0.10,E1,T2,-1)
      RETURN
      END 
      SUBROUTINE AXSS(XSZ,YSZ,TH,NX,NY) 
C PLOT 2 AXES FOR FXY-ROUTINES
      COMMON /ZCTAX/NXL,IXLAB(8),XLEFT,XSF,NYL, 
     *              IYLAB(8),YLOW,YSF,QXAX,QYAX 
C LENGTH OF IXLAB,IYLAB MACH. DEP.  *************** 
C------- IN SUBROUTINE : PRESETTING BY INIAX( BY DATA STATEMENTS) 
C------- IN FXY : BY ZINI WHICH OVERRIDES THE INIAX-PRESETTING
      LOGICAL QXAX,QYAX 
C X-AXIS
      XS=ABS(XSZ) 
      XFF=(NX-XLEFT)/XS 
      IF(QXAX) XFF=XSF
      CALL AXIS(0.,0.,IXLAB,-NXL,XS,0.,XLEFT,XFF) 
C Y-AXIS
      YS=ABS(YSZ) 
      YFF=(NY-YLOW)/YS
      IF(QYAX) YFF=YSF
      CALL AXIS(0.,0.,IYLAB,NYL ,YS,TH,YLOW,YFF)
      RETURN
      END 
      SUBROUTINE BIN(LBUFF,LNUM)                                        
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C SIMULATE A FREE-FIELD READ FROM THE PLOTTER                           
C                                                                       
C   LBUFF  = THE INTEGER ARRAY TO RECEIVE THE CONVERTED DATA            
C                                                                       
C   LNUM   = THE NUMBER OF DATA ITEMS TO BE RETURNED                    
C                                                                       
C ******************************************************************    
C                                                                       
C *** COMMON BLOCK                                                      
C                                                                       
      COMMON /HP/ XMINPU, YMINPU, XORG  , YORG  , XOFF  , YOFF    ,     
     +            XFACT , YFACT , XCNTRL, YCNTRL, XYUNIT          ,     
     +            MDEV  , MUNIT , MPENS , MLU   , MCNTRL, MINIT   ,     
     +            MSHAKE, MPTR  , MCODE , MPLOT , MFLUSH, MSIZE   ,     
     +            MSPOOL, MVER  , MOUT(72),                             
     +            RSI1  , RSI2  , RDI1   , RDI2  , RES1  , RES2         
C                                                                       
C     ***** MLU    - CONTAINS THE LOGICAL UNIT NUMBER FOR I/O           
C                                                                       
C *****************************************************************     
C                                                                       
      DIMENSION LBUFF(1),INUM(8),KBUFF(73)                              
      ICNT=LNUM                                                         
      IFACT=0                                                           
      DO 10 I = 1,8                                                     
          INUM(I)=0                                                     
   10   CONTINUE                                                        
C                                                                       
C**** READ FROM THE PLOTTER                                             
C                                                                       
      CALL ZZGET(MLU,KBUFF,73,LAST)                                     
C                                                                       
C**** SAVE THE POINTER TO THE END OF THE INPUT STRING                   
C                                                                       
      NBYTE=LAST-1                                                      
      DO 50 J = 1,NBYTE                                                 
          KBYTE=(NBYTE-J)+1                                             
          ITEST=KBUFF(KBYTE)                                            
C                                                                       
C**** DO WE HAVE COMMA ?                                                
C                                                                       
          IF (ITEST.EQ.44) GO TO 20                                     
C                                                                       
C**** DO WE HAVE A MINUS SIGN ?                                         
C                                                                       
          IF (ITEST.EQ.45) GO TO 30                                     
C                                                                       
C**** DO WE HAVE AN A,B,C,S, OR T ?                                     
C                                                                       
          IF (ITEST.EQ.65) GO TO 40                                     
          IF (ITEST.EQ.66) GO TO 40                                     
          IF (ITEST.EQ.67) GO TO 40                                     
          IF (ITEST.EQ.83) GO TO 40                                     
          IF (ITEST.EQ.84) GO TO 40                                     
C                                                                       
C**** WE HAVE A DIGIT, CONVERT IT                                       
C                                                                       
          INUM(ICNT)=INUM(ICNT)+(ITEST-48)*10**IFACT                    
          IFACT=IFACT+1                                                 
          GO TO 50                                                      
C                                                                       
C**** SET UP FOR NEXT INTEGER                                           
C                                                                       
   20     IFACT=0                                                       
          ICNT=ICNT-1                                                   
          GO TO 50                                                      
C                                                                       
C**** MAKE THE INTEGER WE JUST CONVERTED NEGATIVE                       
C                                                                       
   30     INUM(ICNT)=-INUM(ICNT)                                        
          GO TO 50                                                      
C                                                                       
C**** SAVE THE ASCII A,B,C,S, OR T                                      
C                                                                       
   40     IFACT=0                                                       
          INUM(ICNT)=ITEST                                              
          ICNT=ICNT-1                                                   
   50   CONTINUE                                                        
      DO 60 I = 1,LNUM                                                  
          LBUFF(I)=INUM(I)                                              
   60   CONTINUE                                                        
   70 RETURN                                                            
      END                                                               
      BLOCK DATA
      PARAMETER         (MAXRBF = 6, MAXBUF = 100)
      LOGICAL           FIRST
      COMMON / MDFREC / RH(MAXRBF), BUFFER(MAXBUF), MDFHKL(3), IPOS2,
     .                  MDFEND, LAST, FIRST
      DATA              FIRST /.TRUE./
      END
      BLOCK DATA
      PARAMETER         (MAXRBF = 6, MAXBUF = 100)
      LOGICAL           FIRST
      COMMON / MDFREC / RH(MAXRBF), BUFFER(MAXBUF), MDFHKL(3), IPOS2,
     .                  MDFEND, LAST, FIRST
      DATA              FIRST /.TRUE./
      END
*DECK $FBONDANG
      FUNCTION BONDANG(X1,X2,X3)
C///  VERSION 83/05/18
C       CALCULATES THE ANGLE (IN RADIANS) BETWEEN THE LINES DEFINED BY
C       X1 AND X2 AND BY X2 AND X3.
C\\\
      DIMENSION X1(3),X2(3),X3(3),U(3),V(3)
      DATA ALMOST1/.99999999999999/
C
C       SET UP VECTORS U AND V:  U = X2 > X1   V = X2 > X3
      UL = 0.
      VL = 0.
      DO 10 I = 1,3
      U(I) = X1(I) - X2(I)
      UL = UL + U(I)**2.
      V(I) = X3(I) - X2(I)
      VL = VL + V(I)**2.
 10   CONTINUE
      UL = SQRT(UL)
      VL = SQRT(VL)
      BONDANG = DOT(U(1),V(1)) / (UL*VL)
      IF (ABS(BONDANG).GT.ALMOST1) BONDANG = SIGN(ALMOST1,BONDANG)
      BONDANG = ACOS(BONDANG)
      RETURN
C *  THIS PROGRAM VALID ON FTN4 AND FTN5 **
      END
      SUBROUTINE BOUT(LNUM)                                             
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C WRITE DATA FROM THE HOST BUFFER TO THE PLOTTER                        
C                                                                       
C   LNUM   = THE NUMBER OF BYTES TO BE WRITTEN TO THE PLOTTER           
C                                                                       
C                                                                       
C *****************************************************************     
C                                                                       
C *** COMMON BLOCK                                                      
C                                                                       
      COMMON /HP/ XMINPU, YMINPU, XORG  , YORG  , XOFF  , YOFF    ,     
     +            XFACT , YFACT , XCNTRL, YCNTRL, XYUNIT          ,     
     +            MDEV  , MUNIT , MPENS , MLU   , MCNTRL, MINIT   ,     
     +            MSHAKE, MPTR  , MCODE , MPLOT , MFLUSH, MSIZE   ,     
     +            MSPOOL, MVER  , MOUT(72),                             
     +            RSI1  , RSI2  , RDI1   , RDI2  , RES1  , RES2         
C                                                                       
C     ***** MLU    - CONTAINS THE LOGICAL UNIT NUMBER FOR I/O           
C                                                                       
C     ***** MSHAKE - CONTAINS THE HANDSHAKE METHOD CODE ( 1 = ENQUIRE-  
C                    ACKNOWLEDGE  /  2 = XON-XOFF  /  3 = HP-IB )       
C                    0= NO HANDSHAKE                                    
C                                                                       
C     ***** MFLUSH - CONTAINS THE CODE USED TO SIGNAL THAT AN IMMEDIATE 
C                    TRANSMIT OF THE OUTPUT BUFFER MUST BE MADE ( 0 =   
C                    NO IMMEDIATE TRANSMIT / 999 = TRANSMIT IMMEDIATE ) 
C                                                                       
C     ***** MOUT   - CONTAINS DATA TO BE SENT TO THE PLOTTER            
C                                                                       
C ******************************************************************    
C                                                                       
C *** COMMON BLOCK                                                      
C                                                                       
      COMMON/ZZCOM/MZBITS,MZCHAR,MZWORD,MZJUST,MZCCSW,MZCCTL,MZDCCS,    
     +MZDCCT,MZHISW,MZLUSW,MZLURD,MZLUWR,                               
     +MZTAD(2),MZOTC(2),MZETC(2),MZOT1(2),MZOT2(2),                     
     +MZHM(2),MZBS(2),MZHEC(2),MZHS1(2),MZHS2(2),MZICD(2),MZIRS1(2),    
     +MZIRS2(2),MZMBS(2),MZSCOC(2)                                      
C                                                                       
C*****************************************************************      
C                                                                       
      DIMENSION IN(73),LARAY(1)                                         
C                                                                       
C**** IF NO HANDSHAKE HAS BEEN SET UP YET, OR IF THE PLOTTER            
C**** IS TO BE TURNED ON AGAIN, TRANSMIT THE CONTENTS OF THE            
C**** OUTPUT BUFFER                                                     
C                                                                       
      IF (MFLUSH.EQ.999) GO TO 20                                       
C                                                                       
C**** WHAT HANDSHAKE METHOD IS INVOKED ?                                
C                                                                       
      IF(MSHAKE.NE.0) GO TO 20                                          
C                                                                       
C**** THE ENQUIRE/ACKNOWLEDGE METHOD IS INVOKED                         
C**** TRANSMIT THE HANDSHAKE ENABLE CHARACTER                           
C                                                                       
   10 LARAY(1)=MZHEC(1)                                                 
      CALL ZZPUT(MLU,LARAY,1)                                           
C                                                                       
C**** READ THE HANDSHAKE STRING                                         
C                                                                       
      CALL ZZGET(MLU,IN,73,IEND)                                        
C                                                                       
C**** TRANSMIT THE CONTENTS OF THE OUTPUT BUFFER                        
C                                                                       
   20 CALL ZZPUT(MLU,MOUT,LNUM)                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE BUFF(MODE,IBUFF,XBUFF,INUM)                            
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C THIS ROUTINE PERFORMS I/O TO THE PLOTTER                              
C                                                                       
C   PARAMETERS :                                                        
C                                                                       
C                                                                       
C   MODE  =  INPUT/OUTPUT CODE :                                        
C                                                                       
C        1 = INSERT CHARACTERS INTO THE BUFFER                          
C            AND FLUSH WHEN FULL.                                       
C                                                                       
C            IF <INUM> NEGATIVE, THEN A SEMICOLON                       
C            TERMINATOR WILL FOLLOW THE OUTPUT STRING.                  
C                                                                       
C        2 = INSERT CHARACTERS INTO THE BUFFER                          
C            AND FLUSH THE BUFFER.                                      
C                                                                       
C            IF <INUM> NEGATIVE, THEN A SEMICOLON                       
C            TERMINATOR WILL FOLLOW THE OUTPUT STRING.                  
C                                                                       
C        3 = FLUSH THE BUFFER AND READ DATA FROM                        
C            THE DEVICE.                                                
C                                                                       
C        4 = CONVERT THE NUMBER OR NUMBERS CONTAINED                    
C            IN THE ARRAY <IBUFF> TO ASCII AND INSERT                   
C            THEM INTO THE OUTPUT BUFFER. THE FORM                      
C            OF THE OUTPUT IS -                                         
C                                                                       
C               INT SEP INT SEP INT SEP TERM                            
C                                                                       
C            WHERE :  INT  = SIGNED/UNSIGNED INTEGERS                   
C                                                                       
C                     SEP  = A COMMA OR SEMICOLON                       
C                                                                       
C                     TERM = A SEMICOLON OR COLON                       
C                                                                       
C            IF <INUM> NEGATIVE, THEN SEPARATOR IS                      
C            A SEMICOLON, AND TERMINATOR IS A COLON.                    
C                                                                       
C        5 = SAME AS MODE 4, BUT FORCE FLUSH BUFFER.                    
C            ( AN INQUIRY IS NOT MADE REGARDING                         
C              AVAILABLE BUFFER SPACE )                                 
C                                                                       
C        6 = CONVERT THE FLOATING POINT NUMBER OR                       
C            NUMBERS IN THE ARRAY <XBUFF> TO ASCII                      
C            AND INSERT THEM INTO THE OUTPUT BUFFER.                    
C            THE FORM OF THE OUTPUT IS -                                
C                                                                       
C               DEC SEP DEC TERM                                        
C                                                                       
C            WHERE :  DEC  = SIGNED/UNSIGNED DECIMALS                   
C                                                                       
C                     SEP  = COMMA                                      
C                                                                       
C                     TERM = SEMICOLON                                  
C                                                                       
C        7 = SAME AS MODE 6, BUT FORCE FLUSH THE BUFFER.                
C            ( AN INQUIRY IS NOT MADE REGARDING AVAILABLE               
C              BUFFER SPACE )                                           
C                                                                       
C                                                                       
C                                                                       
C                                                                       
C   IBUFF =  ARRAY CONTAINING DATA TO BE PLACED IN THE BUFFER           
C                                                                       
C   XBUFF =  REAL ARRAY CONTAINING DATA TO BE PLACED IN THE BUFFER      
C                                                                       
C   INUM  =  THE NUMBER OF DATA ELEMENTS IN EITHER THE <IBUFF> OR       
C            <XBUFF> ARRAY.                                             
C                                                                       
C                                                                       
C ******************************************************************    
C                                                                       
C *** COMMON BLOCK                                                      
C                                                                       
      COMMON /HP/ XMINPU, YMINPU, XORG  , YORG  , XOFF  , YOFF    ,     
     +            XFACT , YFACT , XCNTRL, YCNTRL, XYUNIT          ,     
     +            MDEV  , MUNIT , MPENS , MLU   , MCNTRL, MINIT   ,     
     +            MSHAKE, MPTR  , MCODE , MPLOT , MFLUSH, MSIZE   ,     
     +            MSPOOL, MVER  , MOUT(72),                             
     +            RSI1  , RSI2  , RDI1   , RDI2  , RES1  , RES2         
C                                                                       
C     ***** MOUT   - CONTAINS DATA TO BE SENT TO THE PLOTTER            
C                                                                       
C     ***** MPTR   - CONTAINS THE POINTER TO THE NEXT LOCATION IN MOUT  
C                                                                       
C     ***** MPLOT  - CONTAINS THE PARAMETER CONCATENATION CODE USED     
C                    BY SUBROUTINE BUFF ( 0 = DONT CONCATENATE /        
C                    1 = CONCATENATE )                                  
C                                                                       
C     ***** MSIZE  - CONTAINS THE SIZE OF THE OUTPUT ARRAY MOUT         
C                                                                       
C ******************************************************************    
C                                                                       
      DIMENSION IBUFF(1),XBUFF(1),IA(2),MTEMP(20),LBUFF(8),IDIG(20)     
      DIMENSION XTEMP(5)                                                
      DATA IA(1)/80/,IA(2)/65/                                          
C                 P         A                                           
      NUM=IABS(INUM)                                                    
C                                                                       
C**** SELECT I/O FUNCTION                                               
C                                                                       
      GO TO (10,10,90,120,120,140,140), MODE                            
C                                                                       
C**** MODE 1 AND 2 : IS THIS A ( PA ) COMMAND ?                         
C                                                                       
   10 IF (NUM.NE.2) GO TO 40                                            
      DO 20 I = 1,2                                                     
      IF (IA(I).NE.IBUFF(I)) GO TO 40                                   
   20   CONTINUE                                                        
C                                                                       
C**** WAS THE LAST COMMAND TRANSMITTED A ( PA ) ?                       
C                                                                       
      IF (MPLOT.EQ.1) GO TO 30                                          
C                                                                       
C**** SET COMMAND FLAG                                                  
C                                                                       
      MPLOT=1                                                           
      GO TO 50                                                          
C                                                                       
C**** REPLACE SEMICOLON TERMINATOR WITH COMMA ( CONCATENATE VECTORS )   
C                                                                       
   30 MOUT(MPTR-1)=44                                                   
      GO TO 280                                                         
   40 MPLOT=0                                                           
C                                                                       
C**** SPACE AVAILABLE IN THE OUTPUT BUFFER ?                            
C                                                                       
   50 IF (MPTR+NUM.GT.MSIZE) GO TO 90                                   
C                                                                       
C**** PLACE CHARACTERS IN THE OUTPUT BUFFER                             
C                                                                       
   60 DO 70 I = 1,NUM                                                   
          MOUT(MPTR)=IBUFF(I)                                           
          MPTR=MPTR+1                                                   
   70   CONTINUE                                                        
C                                                                       
C**** WAS A TERMINATOR ( ; ) DESIRED ?                                  
C                                                                       
      IF (INUM.GE.0) GO TO 80                                           
C                                                                       
C YES, PROVIDE THE TERMINATOR                                           
C                                                                       
      MOUT(MPTR)=59                                                     
      MPTR=MPTR+1                                                       
C                                                                       
C**** IS THE OUTPUT BUFFER TO BE FLUSHED ?                              
C                                                                       
   80 IF (MODE.NE.2) GO TO 280                                          
C                                                                       
C**** MODE 3 : FLUSH THE OUTPUT BUFFER                                  
C                                                                       
   90 ILAST=MPTR-1                                                      
      CALL BOUT(ILAST)                                                  
      MPTR=1                                                            
      IF (MODE.EQ.2) GO TO 280                                          
      IF (MODE.EQ.3) GO TO 100                                          
      GO TO 60                                                          
C                                                                       
C**** READ DATA FROM THE PLOTTER                                        
C                                                                       
  100 CALL BIN(LBUFF,NUM)                                               
      DO 110 I = 1,NUM                                                  
          IBUFF(I)=LBUFF(I)                                             
  110   CONTINUE                                                        
      GO TO 280                                                         
C                                                                       
C**** MODE 4 AND 5 : ( SET UP INTEGERS FOR FLOATING POINT PROCESS )     
C                                                                       
  120 DO 130 M = 1,NUM                                                  
          XTEMP(M)=FLOAT(IBUFF(M))                                      
  130 CONTINUE                                                          
      GO TO 160                                                         
C                                                                       
C**** MODE 6 AND 7 : ( CONVERT REAL VALUES TO ASCII )                   
C                                                                       
  140 DO 150 M = 1,NUM                                                  
          XTEMP(M) = XBUFF(M)                                           
  150 CONTINUE                                                          
  160 DO 250 M = 1,NUM                                                  
          KPTR=1                                                        
          IFXD=1                                                        
          RNUM=XTEMP(M)                                                 
          IF (RNUM .LT. 1.0) IFXD=3                                     
          R = ABS(RNUM*10.0**IFXD)                                      
      DO 170 J = 1,20                                                   
            IDIG(J) = IFIX(AMOD(R,10.0))                                
            R = AINT(R/10.0)                                            
            LENGTH = J                                                  
      IF (R.EQ.0.0.AND.LENGTH.GE.IFXD+1) GO TO 180                      
  170 CONTINUE                                                          
  180 IF (RNUM.GE.0.0) GO TO 190                                        
      MTEMP(KPTR) = 45                                                  
      KPTR=KPTR+1                                                       
  190 NVAL=LENGTH                                                       
      DO 200 J = 1,LENGTH                                               
C                                                                       
C**** IF THE VALUE TO BE CONVERTED TO ASCII IS NEGATIVE                 
C**** DUE TO ERRORS IN PRECISION, WE REPLACE IT WITH A                  
C**** ZERO                                                              
            IF (IDIG(NVAL) .LT. 0) IDIG(NVAL)=0                         
            MTEMP(KPTR) = 48+IDIG(NVAL)                                 
            KPTR=KPTR+1                                                 
            NVAL=NVAL-1                                                 
      IF (J.NE.LENGTH-IFXD) GO TO 200                                   
C                                                                       
C**** IF THE MODE IS 6 OR 7 AND WE ARE PROCESSING A ( PA )              
C**** COMMAND SEQUENCE, DELETE THE DECIMAL POINT AND                    
C**** THE FRACTIONAL PART                                               
C                                                                       
      IF ((MODE.EQ.6.OR.MODE.EQ.7).AND.MPLOT.EQ.1) GO TO 210            
C                                                                       
C**** IF THE MODE IS 4 OR 5 DELETE THE DECIMAL POINT                    
C**** AND THE FRACTIONAL PART                                           
C                                                                       
      IF (MODE.EQ.4.OR.MODE.EQ.5) GO TO 210                             
            MTEMP(KPTR) = 46                                            
            KPTR=KPTR+1                                                 
  200 CONTINUE                                                          
C                                                                       
C**** SPACE AVAILABLE IN THE OUTPUT BUFFER ?                            
C                                                                       
  210 IF (MPTR+KPTR.LE.MSIZE) GO TO 220                                 
C                                                                       
C**** FLUSH THE OUTPUT BUFFER                                           
C                                                                       
      ILAST=MPTR-1                                                      
      CALL BOUT(ILAST)                                                  
      MPTR=1                                                            
C                                                                       
C**** PLACE CHARACTERS IN THE OUTPUT BUFFER                             
C                                                                       
  220     MLAST=KPTR-1                                                  
C                                                                       
C**** IF THE VALUE IS ZERO OR MINUS ZERO, DELETE SOME OF THE            
C**** FRACTIONAL PART ( I.E. 0.000 BECOMES 0.0 )                        
C                                                                       
          IF (ABS(RNUM) .LE. 0.0) MLAST=KPTR-3                          
      IF (MLAST .LE. 0) MLAST=1                                         
      DO 230 I = 1,MLAST                                                
              MOUT(MPTR)=MTEMP(I)                                       
              MPTR=MPTR+1                                               
  230       CONTINUE                                                    
C                                                                       
C**** PROVIDE APPROPRIATE SEPARATOR OR TERMINATOR                       
C                                                                       
  240     MOUT(MPTR)=44                                                 
          IF (INUM .LT. 0) MOUT(MPTR)=59                                
          IF (M .EQ. NUM .AND. INUM .LT. 0) MOUT(MPTR)=58               
          IF (M .EQ. NUM .AND. INUM .GE. 0) MOUT(MPTR)=59               
          MPTR=MPTR+1                                                   
  250  CONTINUE                                                         
C                                                                       
C**** IF MODE THE MODE IS 5 THEN FLUSH THE BUFFER                       
C                                                                       
      IF (MODE.NE.5.AND.MODE.NE.7) GO TO 280                            
C                                                                       
C**** FLUSH THE OUTPUT BUFFER                                           
C                                                                       
  260 ILAST=MPTR-1                                                      
      CALL BOUT(ILAST)                                                  
      MPTR=1                                                            
C                                                                       
C**** ENSURE THAT A ( PA ) PRECEDES THE NEXT X,Y PAIR                   
C                                                                       
      MPLOT=0                                                           
  280 RETURN                                                            
      END                                                               
C
C
C
      SUBROUTINE BUI3VEC( VEC, X, Y, Z)
C     =======================================
C
C
      DIMENSION VEC( 3)
C
C
      VEC( 1) = X
      VEC( 2) = Y
      VEC( 3) = Z
C
C
      RETURN
C
C
      END
*DECK $FCALCRP
      SUBROUTINE CALCRP(ROT,TH,RP)
C///  VERSION 83/05/18
C
C         CALCULATES ELEMENTS OF MATRIX ROT-PRIME I.E. THE DERIVATIVE OF
C         EACH ELEMENT OF ROT(I,J) VERSUS THETA(K)
C
C         RP(I,J,K) = D(ROT(I,J))/D(THETA(K))  (PARTIAL DERIVATIVE)
C
      DIMENSION ROT(3,3),TH(3),RP(3,3,3)
C\\\
      S1 = SIN(TH(1))
      S2 = SIN(TH(2))
      S3 = SIN(TH(3))
      C1 = COS(TH(1))
      C2 = COS(TH(2))
      C3 = COS(TH(3))
C
      RP(1,1,1)=-ROT(1,2)
      RP(1,2,1)= ROT(1,1)
      RP(1,3,1)= 0.0
      RP(2,1,1)=-ROT(2,2)
      RP(2,2,1)= ROT(2,1)
      RP(2,3,1)= 0.0
      RP(3,1,1)=-ROT(3,2)
      RP(3,2,1)= ROT(3,1)
      RP(3,3,1)= 0.0
      RP(1,1,2)= S1*S2*S3
      RP(1,2,2)=-C1*S2*S3
      RP(1,3,2)= C2*S3
      RP(2,1,2)= S1*S2*C3
      RP(2,2,2)=-C1*S2*C3
      RP(2,3,2)= C2*C3
      RP(3,1,2)= S1*C2
      RP(3,2,2)=-C1*C2
      RP(3,3,2)=-S2
      RP(1,1,3)= ROT(2,1)
      RP(1,2,3)= ROT(2,2)
      RP(1,3,3)= ROT(2,3)
      RP(2,1,3)=-ROT(1,1)
      RP(2,2,3)=-ROT(1,2)
      RP(2,3,3)=-ROT(1,3)
      RP(3,1,3)= 0.0
      RP(3,2,3)= 0.0
      RP(3,3,3)= 0.0
      RETURN
C *  THIS PROGRAM VALID ON FTN4 AND FTN5 **
      END
      SUBROUTINE CANSEE(Z,NMAX,XI,YJ,M,N,P) 
C------------------CALLED BY THREED 
      DIMENSION Z(NMAX,N) 
      INTEGER CUM, CNT, P, POLD 
      REAL I , J , II , JJ
      COMMON /X3HZ39/ANGA,ANGB,HV,D,SL,SM,SN,CX,CY,CZ,QX,QY,QZ,SD 
      IR = XI 
      JC = YJ 
      ZB = Z ( IR , JC )
      IF(XI.EQ.(FLOAT(IR))) GO TO 2 
      ZB=Z(IR,JC)+(XI-FLOAT(IR))*(Z(IR+1,JC)-Z(IR,JC))
      GO TO 4 
    2 IF ( YJ.EQ.(FLOAT(JC)) ) GO TO 4
      ZB=Z(IR,JC)+(YJ-FLOAT(JC))*(Z(IR,JC+1)-Z(IR,JC))
    4 CONTINUE
      XEND = 0.0
      DX = 0.0
      YMULT = 0.0 
      ZMULT = 0.0 
      IF ( XI .EQ. CX ) GO TO 10
      YMULT = (YJ-CY ) / ( XI - CX )
      ZMULT = ( ZB - CZ ) / ( XI - CX ) 
      DX = 1.0
      XEND = M + 1
      IF ( XI .LT. CX ) GO TO 10
      DX = -1.0 
      XEND = 0.0
   10 CONTINUE
      YEND = 0.0
      DY = 0.0
      XMULT = 0.0 
      IF ( YJ .EQ. CY ) GO TO 20
      XMULT = ( XI-CX) / (YJ - CY ) 
      IF ( ZMULT .EQ. 0.0 ) ZMULT=(ZB - CZ ) / ( YJ - CY )
      DY = 1.0
      YEND = N + 1
      IF ( YJ .LT. CY ) GO TO 20
      DY = -1.0 
      YEND = 0.0
   20 CONTINUE
      CUM = 0 
      CNT = 0 
      P = 0 
      XB = XI 
      YB = YJ 
   30 CONTINUE
      II = AINT ( XB )
      JJ = AINT ( YB )
      XSTEP = DX
      YSTEP = DY
      IF ( XB .EQ. II ) GO TO 40
      IF ( DX .LT. 0.0 ) XSTEP = 0.0
      GO TO 45
   40 IF ( YB .EQ. JJ ) GO TO 45
      IF ( DY .LT. 0.0 ) YSTEP = 0.0
   45 CONTINUE
      I = II + XSTEP
      J = JJ + YSTEP
      IF ( I .EQ. XEND ) GO TO 80 
      IF ( J .EQ. YEND ) GO TO 80 
      XB = CX + XMULT * ( J - CY )
      YB = CY + YMULT * (I - CX ) 
      IF ( DX .LT. 0.0 ) GO TO 55 
      IF ( XB .LT. I ) GO TO 60 
   50 XB = I
      GO TO 65
   55 IF ( XB .LT. I ) GO TO 50 
   60 YB = J
   65 CONTINUE
      ZB = CZ + ZMULT * ( XB - CX ) 
      IR = I
      JC = J
      IF ( YB .NE. J ) GO TO 70 
      IDX = I - DX
      ZS = Z(IR,JC)-DX*(XB-I)*(Z(IDX,JC)-Z(IR,JC) ) 
      GO TO 75
   70 JDY = J - DY
      ZS = Z ( IR,JC ) - DY * ( YB - J ) *(Z(IR,JDY ) - Z ( IR,JC ) ) 
   75 CONTINUE
      SGN = 1 
      IF ( ZB .LT. ZS ) SGN = -1
      CUM=CUM+IFIX(SGN) 
      CNT = CNT + 1 
      IF ( IABS ( CUM ) .EQ. CNT ) GO TO 30 
      GO TO 90
   80 CONTINUE
      P = 1 
      IF ( CUM ) 84 , 86 , 90 
   84  P = -1 
      GO TO 90
   86 CONTINUE
      IF ( ZB .LE. CZ ) GO TO 90
      P = -1
   90 CONTINUE
      RETURN
      END 
*DECK $FCELCAL
      SUBROUTINE CELCAL (DIRCEL,RCPCEL,GD,GR,VOLUME)
C///  
C=========================================================================
C
C                         C E L C A L
C
C=========================================================================
C
C      DIMENSION DIRCEL(3,3),RCPCEL(3,3),GD(3,3),GR(3,3)
C
C ------------- INPUT IN DIRCEL(I=1..3,J=1..2)
C      CELL PARAMETERS IN ONE OF THE FOLLOWING FORMATS..
C
C          I= 1, 2, 3; J=1    I= 1, 2, 3;  J= 2
C          A,  B,  C,             ALFA,      BETA,      GAMMA
C     OR   A,  B,  C,         COS(ALFA),  COS(BETA),  COS(GAMMA)
C     OR   A*, B*, C*,            ALFA*,     BETA*,     GAMMA*
C     OR   A*, B*, *C,        COS(ALFA*), COS(BETA*), COS(GAMMA*)
C
C       AXES IN (RECIPROCAL) ANGSTROMS; ANGLES IN DEGREES OR RADIANS.
C
C ------------- UPON EXIT PARAMETERS CONTAIN...
C
C     DIRCEL..A,B,C,ALFA,BETA,GAMMA,COS(ALFA),COS(BETA),COS(GAMMA)
C  RCPCEL..A*,B*,C*,ALFA*,BETA*,GAMMA*,COS(ALFA*),COS(BETA*),COS(GAMMA*)
C     GD..METRIC TENSOR OF DIRECT LATTICE
C     GR..METRIC TENSOR OF RECIPROCAL LATTICE
C     VOLUME...CELL VOLUME
C
C     ANGLES IN DEGREES, DIMENSIONS IN (RECIPROCAL) ANGSTROMS.
C
C -------------
C      ADAPTED FROM 'ORTEP', PETER KROON, GRONINGEN, 29-10-73
C      RESTYLED, LEO LIJK, 27-10-76
C      FORTRAN77 VERSION, 26-4-83 (LJL)
C
C=========================================================================
C      VERSION 10 DECEMBER 1985
C=========================================================================
C\\\
      DIMENSION DIRCEL(3,3),RCPCEL(3,3),GD(3,3),GR(3,3)
      DATA EMIN8 /0.00000001/
      DATA RAD/.17453292519943E-1/
      T = ABS(DIRCEL(1,2)) - 1.0
      DO 1 J = 1,3
        IF (T.LT.0.) THEN
* ------------- COSINES
          DIRCEL(J,3) = DIRCEL(J,2)
          DIRCEL(J,2) = ACOS(DIRCEL(J,3))/RAD
        ELSE
* ------------- ANGLES
          IF (T.GT.3.) THEN
* ------------- ANGLES IN DEGREES.
            DIRCEL(J,3) = COS(DIRCEL(J,2)*RAD)
          ELSE
* ------------- ANGLES IN RADIANS.
            DIRCEL(J,3) = COS(DIRCEL(J,2))
            DIRCEL(J,2) = DIRCEL(J,2)/RAD
          ENDIF
        ENDIF
* ------------- METRIC TENSOR DIAGONAL ELEMENTS
        GD(J,J) = DIRCEL(J,1)**2
1     CONTINUE
* ------------- OFF-DIAGONAL ELEMENTS
      GD(2,1) = DIRCEL(1,1) * DIRCEL(2,1) * DIRCEL(3,3)
      GD(1,2) = GD(2,1)
      GD(3,2) = DIRCEL(2,1) * DIRCEL(3,1) * DIRCEL(1,3)
      GD(2,3) = GD(3,2)
      GD(3,1) = DIRCEL(1,1) * DIRCEL(3,1) * DIRCEL(2,3)
      GD(1,3) = GD(3,1)
* ------------- INVERT GD TO GET GR.
      CALL INVRS(GD,VOLUME,GR)
      IF (VOLUME.EQ.0.) STOP 'DET METRIC TENS 0'
* ------------- RECIPROCAL CELL PARAMETERS
      DO 3 J = 1,3
        RCPCEL(J,1) = SQRT(GR(J,J))
3     CONTINUE
      RCPCEL(3,3) = GR(1,2) / (RCPCEL(1,1)*RCPCEL(2,1))
      RCPCEL(2,3) = GR(1,3) / (RCPCEL(1,1)*RCPCEL(3,1))
      RCPCEL(1,3) = GR(2,3) / (RCPCEL(2,1)*RCPCEL(3,1))
      DO 4 J = 1,3
        RCPCEL(J,2) = ACOS(RCPCEL(J,3))/RAD
4     CONTINUE
* ------------- DIRECT OR RECIPROCAL INPUT ?
      IF (DIRCEL(1,1).LT.1.) THEN
        VOLUME = 1. / VOLUME
        DO 5 J = 1,3
         DO 5 I = 1,3
          T = GD(I,J)
          GD(I,J) = GR(I,J)
          GR(I,J) = T
          T = DIRCEL(I,J)
          DIRCEL(I,J) = RCPCEL(I,J)
          RCPCEL(I,J) = T
5       CONTINUE
      ENDIF
      DO 6 I = 1,3
        DO 6 J = 1,3
          IF (ABS(GD(I,J)).LT.EMIN8) GD(I,J) = 0.
          IF (ABS(GR(I,J)).LT.EMIN8) GR(I,J) = 0.
6     CONTINUE
      VOLUME = SQRT(VOLUME)
      RETURN
      END

      SUBROUTINE CHECK(MASK)
C 
C        CHECK WHETHER POINT (IX,IY) IS HIDDEN. 
C           IF HIDDEN, IP=3 AND MASK IS UNCHANGED.
C           IF NOT HIDDEN, IP=2 AND MASK IS UPDATED.
C 
      DIMENSION MASK(2,1) 
      COMMON /PLTCM/IX,IY,IP,LIMITX,LIMITY
C 
C        CHECK THE POINT AGAINST THE PLOT BORDER
C 
      IP=3
      IF(IX .LT. 1 .OR. IX .GT. LIMITX)GO TO 10 
      IF(IY .LT. 0 .OR. IY .GT. LIMITY)GO TO 10 
C 
C        CHECK AGAINST PREVIOUS SURFACES
C 
      IF(IY .GT. MASK(1,IX) .AND. IY .LT. MASK(2,IX))GO TO 10 
      MASK(1,IX)=MIN0(MASK(1,IX),IY)
      MASK(2,IX)=MAX0(MASK(2,IX),IY)
      IP=2
10    RETURN
      END 
C
C
C
      SUBROUTINE CHEKHKL(HKLIN,KLASS,ICENTR,ISCREW,HKLOUT,ISYM,IERR)
C     ===============================================================
C
C
C
C                         C H E K H K L
C                         =============
C
C
C---- PURPOSE:
C     ========
C
C     MAP H, K, L to the asymmetric unit according to the
C                 crystal class.
C
C
C---- INPUT PARAMETERS USED:
C     =====================
C
C     HKLIN(3) : H, K and L, original values to be mapped to
C                            asymmetric unit
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
C
C     KLASS    : a crystal class number of the set below
C     KLASS  crystal system  laue class comments  spacegroup number
C                                                 (INT. TABLES)
C       1    TRICLINIC       1_BAR                             1
C       2    MONOCLINIC   I  2/M      B-UNIQUE           3 -   5
C       3    ORTHORHOMBIC    MMM                        16 -  24
C       4    TETRAGONAL   I  4/M                        75 -  80
C       5    TETRAGONAL  II  4/MMM                      89 -  98
C       6    TRIGONAL     I  3_BAR    HEXAGONAL AXES   143 - 146
C       7    TRIGONAL    II  3_BAR    RHOMBOHEDRAL AXES      146
C       8    TRIGONAL   III  3_BAR1M                  149,151,153
C       9    TRIGONAL    IV  3_BARM1  HEXAGONAL AXES  150,152,154,155
C      10    TRIGONAL     V  3_BARM1  RHOMBOHEDRAL AXES      155
C      11    HEXAGONAL    I  6/M                       168 - 173
C      12    HEXAGONAL   II  6/MMM                     177 - 182
C      13    CUBIC        I  M3_BAR                    195 - 199
C      14    CUBIC       II  M3_BARM                   207 - 214
C      15    MONOCLINIC  II  2/M      A UNIQUE           3 -   5
C      16    MONOCLINIC III  2/M      C UNIQUE           3 -   5
C
C     In this table only the enantiomorphic spacegroups are
C     included. For the other spacgroups (which contain (glide)
C     mirrors or an inversion center) this routine can still be
C     used, but then isym has no meaning anymore.
C
C
C
C
C---- OUTPUT PARAMETERS USED:
C     =======================
C
C
C     HKLOUT(3): H K L values of the asymmetric unit equivalent
C                to the input HKL.
C
C     ISYM     : Symmetry number (1 or 2)
C                ISYM = 1 if the asymmetric unit can be reached
C                         by applying the normal spacegroup
C                         symmetry transformations
C                ISYM = 2 if an additional inversion is needed
C                this discriminates between bijvoet pairs of
C                the same reflection
C
C     IERR     : error flag
C                IERR = 0   no error has occurred
C                IERR = 1   reflection not allowed
C
C
C---- VERSION JANUARY 1986,    BAUKE DIJKSTRA
C     ====================
C
C
C     .. Scalar Arguments ..
      INTEGER ICENTR,IERR,ISYM,KLASS
C     ..
C     .. Array Arguments ..
      INTEGER HKLIN(3),HKLOUT(3),ISCREW(3)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      INTEGER I,IH,II,IK,IL,IMX
C     ..
C     .. Local Arrays ..
      INTEGER IM1(3),IP1(3)
C     ..
C     .. External Subroutines ..
      EXTERNAL ABORT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,MAX,MOD
C     ..
C     .. Common blocks ..
      COMMON /AINOUT/LUNIN,LUNOUT
C     ..
C     .. Data statements ..
      DATA IP1,IM1/2,3,1,3,1,2/
C     ..
C
C---- Store the input indices in scratch variables :
C
      IH = HKLIN(1)
      IK = HKLIN(2)
      IL = HKLIN(3)
C
C---- Make sure that on reject the actual indices are
C     returned in "HKLOUT" :
C
      DO 10 I = 1,3
        HKLOUT(I) = HKLIN(I)
   10 CONTINUE
C
C---- Check centering
C
      IERR = 0
      ISYM = 1
      IF (IH.EQ.0 .AND. IK.EQ.0 .AND. IL.EQ.0) THEN
        IERR = 1
      ELSE
        IF (ICENTR.NE.0) THEN
          IF (ICENTR.LE.3) THEN
            II = MOD((HKLIN(IP1(ICENTR))+HKLIN(IM1(ICENTR))),2)
            IF (II.NE.0) IERR = 1
          ELSE IF (ICENTR.EQ.4) THEN
            IF (MOD((IH+IK),2).NE.0) IERR = 1
            IF (MOD((IH+IL),2).NE.0) IERR = 1
            IF (MOD((IK+IL),2).NE.0) IERR = 1
          ELSE IF (ICENTR.EQ.5) THEN
            IF (MOD((IH+IK+IL),2).NE.0) IERR = 1
          ELSE IF (ICENTR.EQ.6) THEN
            II = -IH + IK + IL
            IF (MOD(II,3).NE.0) IERR = 1
          END IF
          IF (IERR.EQ.1) RETURN
        END IF
C
C---- Check screw-axes
C
        DO 20 I = 1,3
          IF (ISCREW(I).NE.0) THEN
            IF (HKLIN(IM1(I)).EQ.0 .AND. HKLIN(IP1(I)).EQ.0) THEN
              IF (MOD(HKLIN(I),ISCREW(I)).NE.0) GO TO 30
            END IF
          END IF
   20   CONTINUE
C
C---- Convert to asymmetric unit
C
        IF (KLASS.EQ.1) THEN
C
C
C
C---- TRICLINIC:
C                   equivalent reflections h, k, l
C                   asymmetric unit:  h .ge. 0
C                                     if (h.eq.0) k .ge. 0
C                                     if (h.eq.0 .and. k.eq.0) l .ge. 0
C
          IF (IH.LT.0) THEN
            HKLOUT(1) = -IH
            HKLOUT(2) = -IK
            HKLOUT(3) = -IL
            ISYM = 2
          ELSE IF (IH.EQ.0) THEN
            IF (IK.LT.0) THEN
              HKLOUT(2) = -IK
              HKLOUT(3) = -IL
              ISYM = 2
            ELSE IF (IK.EQ.0) THEN
              IF (IL.LT.0) THEN
                HKLOUT(3) = -IL
                ISYM = 2
              END IF
            END IF
          END IF
C
C
        ELSE IF (KLASS.EQ.2) THEN
C
C---- MONOCLINIC I  (B UNIQUE):
C                   equivalent reflections h, k, l   and  -h, k, -l
C                   asymmetric unit:  h and k  both positive
C                                     if (h.eq.0) l .ge. 0
C
          IF (IK.LT.0) THEN
            IH = -IH
            IK = -IK
            IL = -IL
            ISYM = 2
          END IF
          IF (IH.LT.0) THEN
            IH = -IH
            IL = -IL
          ELSE IF (IH.EQ.0 .AND. IL.LT.0) THEN
            IL = -IL
          END IF
          HKLOUT(1) = IH
          HKLOUT(2) = IK
          HKLOUT(3) = IL
C
C
        ELSE IF (KLASS.EQ.15) THEN
C
C---- MONOCLINIC II (A UNIQUE):
C                   equivalent reflections h, k, l   and  h, -k, -l
C                   asymmetric unit:  h and k  both positive
C                                     if (k.eq.0) l .ge. 0
C
          IF (IH.LT.0) THEN
            IH = -IH
            IK = -IK
            IL = -IL
            ISYM = 2
          END IF
          IF (IK.LT.0) THEN
            IK = -IK
            IL = -IL
          ELSE IF (IK.EQ.0 .AND. IL.LT.0) THEN
            IL = -IL
          END IF
          HKLOUT(1) = IH
          HKLOUT(2) = IK
          HKLOUT(3) = IL
C
C
        ELSE IF (KLASS.EQ.16) THEN
C
C---- MONOCLINIC III (C UNIQUE):
C                   equivalent reflections h, k, l   and  -h, -k, l
C                   asymmetric unit:  h and l  both positive
C                                     if (h.eq.0) k .ge. 0
C
          IF (IL.LT.0) THEN
            IH = -IH
            IK = -IK
            IL = -IL
            ISYM = 2
          END IF
          IF (IH.LT.0) THEN
            IH = -IH
            IK = -IK
          ELSE IF (IH.EQ.0 .AND. IK.LT.0) THEN
            IK = -IK
          END IF
          HKLOUT(1) = IH
          HKLOUT(2) = IK
          HKLOUT(3) = IL
C
C
        ELSE IF (KLASS.EQ.3) THEN
C
C---- ORTHORHOMBIC:
C                   equivalent reflections   h, k,  l;    -h, -k,  l
C                                           -h, k, -l;     h, -k, -l
C                   asymmetric unit:         h, k  and l  all positive
C
          IF (IH.LT.0) THEN
            IH = -IH
            IK = -IK
          END IF
          IF (IK.LT.0) THEN
            IK = -IK
            IL = -IL
          END IF
          IF (IL.LT.0) THEN
            ISYM = 2
            IL = -IL
            IF (IH.EQ.0) ISYM = 1
            IF (IK.EQ.0) ISYM = 1
          END IF
          HKLOUT(1) = IH
          HKLOUT(2) = IK
          HKLOUT(3) = IL
C
C
        ELSE IF (KLASS.EQ.4) THEN
C
C---- TETRAGONAL I (LAUE CLASS 4/M):
C                   equivalent reflections    h, k, l;   -h, -k, l
C                                            -k, h, l;    k, -h, l
C                   asymmetric unit:  h, k and l  all positive
C                                     if (h .ne. 0) k .ne. 0
C
          IF (IL.LT.0) THEN
            ISYM = 2
            IH = -IH
            IK = -IK
            IL = -IL
          END IF
          IF (IH.LT.0) THEN
            IH = -IH
            IK = -IK
          END IF
          IF (IK.LE.0) THEN
            II = IH
            IH = -IK
            IK = II
          END IF
          HKLOUT(1) = IH
          HKLOUT(2) = IK
          HKLOUT(3) = IL
C
C
        ELSE IF (KLASS.EQ.5) THEN
C
C---- TETRAGONAL II (LAUE CLASS 4/MMM):
C                   equivalent reflections    h, k, l;   -h, -k, l
C                                            -k, h, l;    k, -h, l
C                                            -h, k,-l;    h, -k,-l
C                                             k, h,-l;   -k, -h,-l
C                   asymmetric unit:  h, k and l  all positive
C                                     k .le. h
C
C
          IF (IH.LT.0) THEN
            IH = -IH
            IK = -IK
          END IF
          IF (IK.LE.0) THEN
            II = IH
            IH = -IK
            IK = II
          END IF
          IF (IK.GT.IH) THEN
            II = IH
            IH = IK
            IK = II
            IL = -IL
          END IF
          IF (IH.EQ.IK) IL = ABS(IL)
          IF (IL.LT.0) THEN
            IF (IH.NE.0) ISYM = 2
            IL = -IL
          END IF
          HKLOUT(1) = IH
          HKLOUT(2) = IK
          HKLOUT(3) = IL
C
C
        ELSE IF (KLASS.EQ.6) THEN
C
C---- TRIGONAL I:
C                   equivalent reflections:     h,   k  , l
C                                               k,-(h+k), l
C                                            -(h+k), h  , l
C                   asymmetric unit:
C                                            h and l both positive
C                                            k .lt. 0 only if  h+k gt 0
C
          IF (IL.LT.0) THEN
            IH = -IH
            IK = -IK
            IL = -IL
            ISYM = 2
          END IF
          IF (IH.LT.0) THEN
            II = IH + IK
            IF (II.LT.0) THEN
              IK = IH
              IH = -II
            ELSE
              IH = IK
              IK = -II
            END IF
          END IF
          IF (IK.LT.0) THEN
            II = IH + IK
            IF (II.LE.0) THEN
              IK = IH
              IH = -II
            END IF
          END IF
          HKLOUT(1) = IH
          HKLOUT(2) = IK
          HKLOUT(3) = IL
C
C
        ELSE IF (KLASS.EQ.7) THEN
C
C---- TRIGONAL II (SPACEGROUP 146 WITH RHOMBOHEDRAL AXES):
C                   equivalent reflections:     h,   k,   l
C                                               k,   l,   h
C                                               l,   h,   k
C                   asymmetric unit:
C                                            h .ge. 0
C                                            h .ge. abs(k)
C                                            h .ge. abs(l)
C                                            h .ne. l if (h .ne. k)
C
          IMX = MAX(ABS(IH),ABS(IK),ABS(IL))
          IF (IMX.EQ.ABS(IK)) THEN
            IK = IL
            IL = IH
            IH = IMX
          ELSE IF (IMX.EQ.ABS(IL)) THEN
            IL = IK
            IK = IH
            IH = IMX
          END IF
          IF (IH.EQ.IL) THEN
            IL = IK
            IK = IH
          END IF
          IF (IH.LT.0) THEN
            IH = -IH
            IK = -IK
            IL = -IL
            ISYM = 2
          END IF
          HKLOUT(1) = IH
          HKLOUT(2) = IK
          HKLOUT(3) = IL
C
C
        ELSE IF (KLASS.EQ.8) THEN
C
C---- TRIGONAL III:
C                   equivalent reflections:
C                          h, k, l;   k, -(h+k), l;   -(h+k), h, l;
C                         -k,-h,-l;   -h, (h+k),-l;    (h+k),-k,-l;
C                   asymmetric unit
C                   h and k positive
C                   k .le. h
C                   l positive if h or k = 0
C
          IF (IH.LT.0) THEN
            II = IH + IK
            IH = -IH
            IK = II
            IL = -IL
          END IF
          IF (IK.LT.0) THEN
            II = IH + IK
            IF (II.LT.0) THEN
              IK = IH
              IH = -II
            ELSE
              IH = II
              IK = -IK
              IL = -IL
            END IF
          END IF
          IF (IH.EQ.0) IL = ABS(IL)
          IF (IK.EQ.0) IL = ABS(IL)
          IF (IK.GT.IH) THEN
            II = IH
            IH = IK
            IK = II
            ISYM = 2
          END IF
          HKLOUT(1) = IH
          HKLOUT(2) = IK
          HKLOUT(3) = IL
C
C
        ELSE IF (KLASS.EQ.9) THEN
C
C---- TRIGONAL IV:
C                   equivalent reflections:
C                     h, k, l;   k, -(h+k), l;  -(h+k), h, l
C                     k, h,-l;   h, -(h+k),-l;  -(h+k), k,-l
C                   asymmetric unit:
C                     h and k positive
C                     k .le. h
C                     if (h.eq.k) l positive
C
          IF (IH.LT.0) THEN
            II = IH + IK
            IF (II.LT.0) THEN
              IK = IH
              IH = -II
            ELSE
              IH = IK
              IK = -II
            END IF
          END IF
          IF (IK.LT.0) THEN
            II = IH + IK
            IF (II.LE.0) THEN
              IK = -II
              IL = -IL
            ELSE
              IH = II
              IK = -IK
              ISYM = 2
            END IF
          END IF
          IF (IK.GT.IH) THEN
            II = IH
            IH = IK
            IK = II
            IL = -IL
          END IF
          IF (IH.EQ.IK) IL = ABS(IL)
          HKLOUT(1) = IH
          HKLOUT(2) = IK
          HKLOUT(3) = IL
C
C
        ELSE IF (KLASS.EQ.10) THEN
C
C---- TRIGONAL V (SPACEGROUP 155 WITH RHOMBOHEDRAL AXES):
C                   equivalent reflections:
C                     h,  k,  l;     k,  l,  h;     l,  h,  k
C                    -h, -l, -k;    -k, -h, -l;    -l, -k, -h
C                   asymmetric unit:
C                     h .ge. 0
C                     h .ge. abs(k)
C                     h .ge. abs(l)
C                 abs(k).ge. abs(l)
C
          IMX = MAX(ABS(IH),ABS(IK),ABS(IL))
          IF (IMX.EQ.ABS(IK)) THEN
            IK = IL
            IL = IH
            IH = IMX
          ELSE IF (IMX.EQ.ABS(IL)) THEN
            IL = IK
            IK = IH
            IH = IMX
          END IF
          IF (ABS(IL).GT.ABS(IK)) THEN
            II = IK
            IK = -IL
            IL = -II
            IH = -IH
          ELSE IF (IK.EQ.IL) THEN
            IH = ABS(IH)
            IK = ABS(IK)
            IL = ABS(IL)
          END IF
          IF (IH.LT.0) THEN
            IH = -IH
            IK = -IK
            IL = -IL
            ISYM = 2
          END IF
          HKLOUT(1) = IH
          HKLOUT(2) = IK
          HKLOUT(3) = IL
C
C
        ELSE IF (KLASS.EQ.11) THEN
C
C---- HEXAGONAL I:
C                   equivalent reflections  h,   k,   l;   -h,  -k,   l
C                                           k,-(h+k), l;   -k, (h+k), l
C                                         (h+k),-h,   l; -(h+k), h,   l
C                   asymmetric unit:        h, k  and l  all positive
C                                           k .ne. 0
C
          IF (IH.LT.0) THEN
            IH = -IH
            IK = -IK
          END IF
          IF (IK.LT.0) THEN
            II = IH + IK
            IH = -IK
            IK = II
          END IF
          IF (IK.LT.0) THEN
            II = IH + IK
            IH = -IK
            IK = II
          END IF
          IF (IK.EQ.0) THEN
            IK = IH
            IH = 0
          END IF
          IF (IL.LT.0) THEN
            IL = -IL
            ISYM = 2
          END IF
          HKLOUT(1) = IH
          HKLOUT(2) = IK
          HKLOUT(3) = IL
C
C
        ELSE IF (KLASS.EQ.12) THEN
C
C---- HEXAGONAL II:
C                   equivalent reflections  h,   k,   l;   -h,  -k,   l
C                                           k,-(h+k), l;   -k, (h+k), l
C                                         (h+k),-h,   l; -(h+k), h,   l
C                                           k,   h,  -l;   -k,  -h,  -l
C                                           h,-(h+k),-l;   -h, (h+k),-l
C                                         (h+k),-k,  -l; -(h+k), k,  -l
C                   asymmetric unit:        h, k  and l  all positive
C                                           k .le. h
C
          IF (IH.LT.0) THEN
            IH = -IH
            IK = -IK
          END IF
          IF (IK.LT.0) THEN
            II = IH + IK
            IH = -IK
            IK = II
          END IF
          IF (IK.LT.0) THEN
            II = IH + IK
            IH = -IK
            IK = II
          END IF
          IF (IK.GT.IH) THEN
            II = IH
            IH = IK
            IK = II
            IL = -IL
          END IF
          IF (IH.EQ.IK) IL = ABS(IL)
          IF (IK.EQ.0) IL = ABS(IL)
          IF (IL.LT.0) THEN
            IL = -IL
            ISYM = 2
          END IF
          HKLOUT(1) = IH
          HKLOUT(2) = IK
          HKLOUT(3) = IL
C
C
        ELSE IF (KLASS.EQ.13) THEN
C
C---- CUBIC I:
C                   equivalent reflections   h, k,  l;    -h, -k,  l
C                                           -h, k, -l;     h, -k, -l
C                                            k, l,  h;    -k, -l,  h
C                                           -k, l, -h;     k, -l, -h
C                                            l, h,  k;    -l, -h,  k
C                                           -l, h, -k;     l, -h, -k
C                   asymmetric unit:         h, k  and l  all positive
C                                            h .ge. k
C                                            h .ge. l
C
          IF (IH.LT.0) THEN
            IH = -IH
            IK = -IK
          END IF
          IF (IK.LT.0) THEN
            IK = -IK
            IL = -IL
          END IF
          IF (IH.EQ.0) IL = ABS(IL)
          IF (IK.EQ.0) IL = ABS(IL)
          IF (IL.LT.0) THEN
            IL = -IL
            ISYM = 2
          END IF
          II = MAX(IH,IK,IL)
          IF (II.EQ.IK) THEN
            IK = IL
            IL = IH
            IH = II
          ELSE IF (II.EQ.IL) THEN
            IL = IK
            IK = IH
            IH = II
          END IF
          HKLOUT(1) = IH
          HKLOUT(2) = IK
          HKLOUT(3) = IL
C
C
        ELSE IF (KLASS.EQ.14) THEN
C
C---- CUBIC II:
C                   equivalent reflections   h, k,  l;    -h, -k,  l
C                                           -h, k, -l;     h, -k, -l
C                                            k, l,  h;    -k, -l,  h
C                                           -k, l, -h;     k, -l, -h
C                                            l, h,  k;    -l, -h,  k
C                                           -l, h, -k;     l, -h, -k
C                                            k, h, -l;     h, -l,  k
C                                           -l, k,  h;    -k, -h, -l
C                                           -h, l,  k;     l, -k,  h
C                                           -k, h,  l;    -h, -l, -k
C                                            l, k, -h;     k, -h,  l
C                                            h, l, -k;    -l, -k, -h
C                   asymmetric unit:         h, k  and l  all positive
C                                            h .ge. k
C                                            k .ge. l
C
          IF (IH.LT.0) THEN
            IH = -IH
            IK = -IK
          END IF
          IF (IK.LT.0) THEN
            IK = -IK
            IL = -IL
          END IF
          IF (IH.EQ.0) IL = ABS(IL)
          IF (IK.EQ.0) IL = ABS(IL)
          IF (IL.LT.0) THEN
            II = IK
            IK = -IL
            IL = II
          END IF
          IMX = MAX(IH,IK,IL)
          IF (IMX.EQ.IH) THEN
            IF (IK.LT.IL) THEN
              ISYM = 2
              II = IK
              IK = IL
              IL = II
            END IF
          ELSE IF (IMX.EQ.IK) THEN
            IK = IL
            IL = IH
            IH = IMX
            IF (IK.LT.IL) THEN
              ISYM = 2
              II = IK
              IK = IL
              IL = II
            END IF
          ELSE
            IL = IK
            IK = IH
            IH = IMX
            IF (IK.LT.IL) THEN
              ISYM = 2
              II = IK
              IK = IL
              IL = II
            END IF
          END IF
          HKLOUT(1) = IH
          HKLOUT(2) = IK
          HKLOUT(3) = IL
        ELSE
C
C
          CALL ABORT('ERROR IN SUBR CHKHKL: KLASS.GT.16 OR .LT.1')
        END IF
        RETURN
   30   IERR = 1
      END IF
C
C
      END
      SUBROUTINE CHKHKL (HKLIN,KLASS,ICENTR,ISCREW,HKLOUT,ISYM,IERR)
C///
C=======================================================================
C
C                         C H E K H K L
C
C=======================================================================
C
C     PURPOSE:
C     MAP H, K, L TO THE ASYMMETRIC UNIT ACCORDING TO THE
C     CRYSTAL CLASS.
C
C=======================================================================
C
C     INPUT PARAMETERS USED:
C
C     HKLIN(3) : H, K AND L, ORIGINAL VALUES TO BE MAPPED TO
C                ASYMMETRIC UNIT
C
C     ICENTR   : AXIS OF CENTERING
C                0  =  NO CENTERING               (P SPACEGROUPS)
C                1  =  CENTERING AROUND A-AXIS    (A SPACEGROUPS)
C                2  =  CENTERING AROUND B-AXIS    (B SPACEGROUPS)
C                3  =  CENTERING AROUND C-AXIS    (C SPACEGROUPS)
C                4  =  CENTERING ON ALL FACES     (F SPACEGROUPS)
C                5  =  BODY CENTERING             (I SPACEGROUPS)
C                6  =  RHOMBOHEDRAL CENTERING     (R SPACEGROUPS WITH
C                                                    HEXAGONAL AXES)
C                      (NOTE: R-SPACEGROUPS WITH RHOMBOHEDRAL AXES
C                             HAVE ICENTR = 0!)
C
C     ISCREW(3): TYPE OF SCREW AXIS FOR A, B  AND C
C                SO ISCREW(I) MUST BE 0 (NO SCREW), 2, 3, 4, OR 6
C
C     KLASS    : A CRYSTAL CLASS NUMBER OF THE SET BELOW
C     KLASS  CRYSTAL SYSTEM  LAUE CLASS COMMENTS  SPACEGROUP NUMBER
C                                                 (INT. TABLES)
C       1    TRICLINIC       1_BAR                             1
C       2    MONOCLINIC   I  2/M      B-UNIQUE           3 -   5
C       3    ORTHORHOMBIC    MMM                        16 -  24
C       4    TETRAGONAL   I  4/M                        75 -  80
C       5    TETRAGONAL  II  4/MMM                      89 -  98
C       6    TRIGONAL     I  3_BAR    HEXAGONAL AXES   143 - 146
C       7    TRIGONAL    II  3_BAR    RHOMBOHEDRAL AXES      146
C       8    TRIGONAL   III  3_BAR1M                  149,151,153
C       9    TRIGONAL    IV  3_BARM1  HEXAGONAL AXES  150,152,154,155
C      10    TRIGONAL     V  3_BARM1  RHOMBOHEDRAL AXES      155
C      11    HEXAGONAL    I  6/M                       168 - 173
C      12    HEXAGONAL   II  6/MMM                     177 - 182
C      13    CUBIC        I  M3_BAR                    195 - 199
C      14    CUBIC       II  M3_BARM                   207 - 214
C      15    MONOCLINIC  II  2/M      A UNIQUE           3 -   5
C      16    MONOCLINIC III  2/M      C UNIQUE           3 -   5
C
C     IN THIS TABLE ONLY THE ENANTIOMORPHIC SPACEGROUPS ARE
C     INCLUDED. FOR THE OTHER SPACGROUPS (WHICH CONTAIN (GLIDE)
C     MIRRORS OR AN INVERSION CENTER) THIS ROUTINE CAN STILL BE
C     USED, BUT THEN ISYM HAS NO MEANING ANYMORE.
C
C
C     OUTPUT PARAMETERS USED:
C
C     HKLOUT(3): H K L VALUES OF THE ASYMMETRIC UNIT EQUIVALENT
C                TO THE INPUT HKL.
C
C     ISYM     : SYMMETRY NUMBER (1 OR 2)
C                ISYM = 1 IF THE ASYMMETRIC UNIT CAN BE REACHED
C                         BY APPLYING THE NORMAL SPACEGROUP
C                         SYMMETRY TRANSFORMATIONS
C                ISYM = 2 IF AN ADDITIONAL INVERSION IS NEEDED
C                THIS DISCRIMINATES BETWEEN BIJVOET PAIRS OF
C                THE SAME REFLECTION
C
C     IERR     : ERROR FLAG
C                IERR = 0   NO ERROR HAS OCCURRED
C                IERR = 1   REFLECTION NOT ALLOWED
C
C
C=======================================================================
C     VERSION JANUARY 1986,    BAUKE DIJKSTRA
C=======================================================================
C\\\
      INTEGER ISCREW(3), HKLIN(3), HKLOUT(3), IP1(3), IM1(3)
      DATA    IP1, IM1   / 2, 3, 1, 3, 1, 2 /
C
C     STORE THE INPUT INDICES IN SCRATCH VARIABLES :
C
      IH = HKLIN(1)
      IK = HKLIN(2)
      IL = HKLIN(3)
C
C     MAKE SURE THAT ON REJECT THE ACTUAL INDICES ARE
C     RETURNED IN "HKLOUT" :
C
      DO 5 I=1,3
    5 HKLOUT(I) = HKLIN(I)
C
C  CHECK CENTERING
C
      IERR = 0
      ISYM = 1
      IF(IH.EQ.0.AND.IK.EQ.0.AND.IL.EQ.0) THEN
            IERR = 1
            RETURN
      ENDIF
      IF(ICENTR.NE.0) THEN
            IF(ICENTR.LE.3) THEN
                  II = MOD((HKLIN(IP1(ICENTR))+HKLIN(IM1(ICENTR))),2)
                  IF(II.NE.0) IERR = 1
            ELSE
                  IF(ICENTR.EQ.4) THEN
                        IF(MOD((IH+IK),2).NE.0) IERR = 1
                        IF(MOD((IH+IL),2).NE.0) IERR = 1
                        IF(MOD((IK+IL),2).NE.0) IERR = 1
                  ELSE
                        IF(ICENTR.EQ.5) THEN
                              IF(MOD((IH+IK+IL),2).NE.0) IERR = 1
                        ELSE
                              IF(ICENTR.EQ.6) THEN
                                    II = -IH + IK + IL
                                    IF(MOD(II,3).NE.0) IERR = 1
                              ENDIF
                        ENDIF
                  ENDIF
            ENDIF
            IF(IERR.EQ.1) RETURN
      ENDIF
C
C  CHECK SCREW-AXES
C
      DO 10 I = 1,3
            IF (ISCREW(I).NE.0) THEN
                  IF (HKLIN(IM1(I)).EQ.0 .AND. HKLIN(IP1(I)).EQ.0) THEN
                        IF (MOD(HKLIN(I),ISCREW(I)).NE.0) THEN
                              IERR = 1
                              RETURN
                        ENDIF
                  ENDIF
            ENDIF
   10 CONTINUE
C
C  CONVERT TO ASYMMETRIC UNIT
C
      GOTO(100,110,140,150,160,170,180,190,200,210,220,230,240,250,
     .120,130) KLASS
      CALL ABORT('ERROR IN SUBR CHKHKL: KLASS.GT.16 OR .LT.1')
C
C=======================================================================
C
  100 CONTINUE
C
C     TRICLINIC:
C                   EQUIVALENT REFLECTIONS H, K, L
C                   ASYMMETRIC UNIT:  H .GE. 0
C                                     IF (H.EQ.0) K .GE. 0
C                                     IF (H.EQ.0 .AND. K.EQ.0) L .GE. 0
C
      IF(IH.LT.0) THEN
            HKLOUT(1) = -IH
            HKLOUT(2) = -IK
            HKLOUT(3) = -IL
            ISYM = 2
      ELSE IF(IH.EQ.0) THEN
            IF(IK.LT.0) THEN
                  HKLOUT(2) = -IK
                  HKLOUT(3) = -IL
                  ISYM = 2
            ELSE IF(IK.EQ.0) THEN
                  IF(IL.LT.0) THEN
                        HKLOUT(3) = -IL
                        ISYM = 2
                  ENDIF
            ENDIF
      ENDIF
      RETURN
C
C=======================================================================
C
  110 CONTINUE
C
C     MONOCLINIC I  (B UNIQUE):
C                   EQUIVALENT REFLECTIONS H, K, L   AND  -H, K, -L
C                   ASYMMETRIC UNIT:  H AND K  BOTH POSITIVE
C                                     IF (H.EQ.0) L .GE. 0
C
      IF(IK.LT.0) THEN
            IH = -IH
            IK = -IK
            IL = -IL
            ISYM = 2
      ENDIF
      IF(IH.LT.0) THEN
            IH = -IH
            IL = -IL
      ELSE
            IF(IH.EQ.0.AND.IL.LT.0) IL = -IL
      ENDIF
      HKLOUT(1) = IH
      HKLOUT(2) = IK
      HKLOUT(3) = IL
      RETURN
C
C=======================================================================
C
  120 CONTINUE
C
C     MONOCLINIC II (A UNIQUE):
C                   EQUIVALENT REFLECTIONS H, K, L   AND  H, -K, -L
C                   ASYMMETRIC UNIT:  H AND K  BOTH POSITIVE
C                                     IF (K.EQ.0) L .GE. 0
C
      IF(IH.LT.0) THEN
            IH = -IH
            IK = -IK
            IL = -IL
            ISYM = 2
      ENDIF
      IF(IK.LT.0) THEN
            IK = -IK
            IL = -IL
      ELSE
            IF(IK.EQ.0.AND.IL.LT.0) IL = -IL
      ENDIF
      HKLOUT(1) = IH
      HKLOUT(2) = IK
      HKLOUT(3) = IL
      RETURN
C
C=======================================================================
C
  130 CONTINUE
C
C     MONOCLINIC III (C UNIQUE):
C                   EQUIVALENT REFLECTIONS H, K, L   AND  -H, -K, L
C                   ASYMMETRIC UNIT:  H AND L  BOTH POSITIVE
C                                     IF (H.EQ.0) K .GE. 0
C
      IF(IL.LT.0) THEN
            IH = -IH
            IK = -IK
            IL = -IL
            ISYM = 2
      ENDIF
      IF(IH.LT.0) THEN
            IH = -IH
            IK = -IK
      ELSE
            IF(IH.EQ.0.AND.IK.LT.0) IK = -IK
      ENDIF
      HKLOUT(1) = IH
      HKLOUT(2) = IK
      HKLOUT(3) = IL
      RETURN
C
C=======================================================================
C
  140 CONTINUE
C
C     ORTHORHOMBIC:
C                   EQUIVALENT REFLECTIONS   H, K,  L;    -H, -K,  L
C                                           -H, K, -L;     H, -K, -L
C                   ASYMMETRIC UNIT:         H, K  AND L  ALL POSITIVE
C
      IF(IH.LT.0) THEN
            IH = -IH
            IK = -IK
      ENDIF
      IF(IK.LT.0) THEN
            IK = -IK
            IL = -IL
      ENDIF
      IF(IL.LT.0) THEN
            ISYM = 2
            IL = -IL
            IF(IH.EQ.0) ISYM = 1
            IF(IK.EQ.0) ISYM = 1
      ENDIF
      HKLOUT(1) = IH
      HKLOUT(2) = IK
      HKLOUT(3) = IL
      RETURN
C
C=======================================================================
C
  150 CONTINUE
C
C     TETRAGONAL I (LAUE CLASS 4/M):
C                   EQUIVALENT REFLECTIONS    H, K, L;   -H, -K, L
C                                            -K, H, L;    K, -H, L
C                   ASYMMETRIC UNIT:  H, K AND L  ALL POSITIVE
C                                     IF (H .NE. 0) K .NE. 0
C
      IF(IL.LT.0) THEN
            ISYM = 2
            IH = -IH
            IK = -IK
            IL = -IL
      ENDIF
      IF(IH.LT.0) THEN
            IH = -IH
            IK = -IK
      ENDIF
      IF(IK.LE.0) THEN
            II = IH
            IH = -IK
            IK = II
      ENDIF
      HKLOUT(1) = IH
      HKLOUT(2) = IK
      HKLOUT(3) = IL
      RETURN
C
C=======================================================================
C
  160 CONTINUE
C
C     TETRAGONAL II (LAUE CLASS 4/MMM):
C                   EQUIVALENT REFLECTIONS    H, K, L;   -H, -K, L
C                                            -K, H, L;    K, -H, L
C                                            -H, K,-L;    H, -K,-L
C                                             K, H,-L;   -K, -H,-L
C                   ASYMMETRIC UNIT:  H, K AND L  ALL POSITIVE
C                                     K .LE. H
C
C
      IF(IH.LT.0) THEN
            IH = -IH
            IK = -IK
      ENDIF
      IF(IK.LE.0) THEN
            II = IH
            IH = -IK
            IK = II
      ENDIF
      IF(IK.GT.IH) THEN
            II = IH
            IH = IK
            IK = II
            IL = -IL
      ENDIF
      IF(IH.EQ.IK) IL = IABS(IL)
      IF(IL.LT.0) THEN
            IF(IH.NE.0) ISYM = 2
            IL = -IL
      ENDIF
      HKLOUT(1) = IH
      HKLOUT(2) = IK
      HKLOUT(3) = IL
      RETURN
C
C=======================================================================
C
  170 CONTINUE
C
C     TRIGONAL I:
C                   EQUIVALENT REFLECTIONS:     H,   K  , L
C                                               K,-(H+K), L
C                                            -(H+K), H  , L
C                   ASYMMETRIC UNIT:
C                                            H AND L BOTH POSITIVE
C                                            K .LT. 0 ONLY IF  H+K GT 0
C
C
      IF(IL.LT.0) THEN
            IH = -IH
            IK = -IK
            IL = -IL
            ISYM = 2
      ENDIF
      IF(IH.LT.0) THEN
            II = IH + IK
            IF(II.LT.0) THEN
                  IK =  IH
                  IH = -II
            ELSE
                  IH =  IK
                  IK = -II
            ENDIF
      ENDIF
      IF(IK.LT.0) THEN
            II = IH + IK
            IF(II.LE.0) THEN
                  IK =  IH
                  IH = -II
            ENDIF
      ENDIF
      HKLOUT(1) = IH
      HKLOUT(2) = IK
      HKLOUT(3) = IL
      RETURN
C
C=======================================================================
C
  180 CONTINUE
C     TRIGONAL II (SPACEGROUP 146 WITH RHOMBOHEDRAL AXES):
C                   EQUIVALENT REFLECTIONS:     H,   K,   L
C                                               K,   L,   H
C                                               L,   H,   K
C                   ASYMMETRIC UNIT:
C                                            H .GE. 0
C                                            H .GE. ABS(K)
C                                            H .GE. ABS(L)
C                                            H .NE. L IF (H .NE. K)
C
      IMX = MAX0(IABS(IH), IABS(IK), IABS(IL))
      IF(IMX.EQ.IABS(IK)) THEN
            IK = IL
            IL = IH
            IH = IMX
      ELSE IF(IMX.EQ.IABS(IL)) THEN
            IL = IK
            IK = IH
            IH = IMX
      ENDIF
      IF(IH.EQ.IL) THEN
            IL = IK
            IK = IH
      ENDIF
      IF(IH.LT.0) THEN
            IH = -IH
            IK = -IK
            IL = -IL
            ISYM = 2
      ENDIF
      HKLOUT(1) = IH
      HKLOUT(2) = IK
      HKLOUT(3) = IL
      RETURN
C
C=======================================================================
C
  190 CONTINUE
C
C     TRIGONAL III:
C                   EQUIVALENT REFLECTIONS:
C                          H, K, L;   K, -(H+K), L;   -(H+K), H, L;
C                         -K,-H,-L;   -H, (H+K),-L;    (H+K),-K,-L;
C                   ASYMMETRIC UNIT
C                   H AND K POSITIVE
C                   K .LE. H
C                   L POSITIVE IF H OR K = 0
C
      IF(IH.LT.0) THEN
            II = IH + IK
            IH = -IH
            IK =  II
            IL = -IL
      ENDIF
      IF(IK.LT.0) THEN
            II = IH + IK
            IF(II.LT.0) THEN
                  IK =  IH
                  IH = -II
            ELSE
                  IH =  II
                  IK = -IK
                  IL = -IL
            ENDIF
      ENDIF
      IF(IH.EQ.0) IL = IABS(IL)
      IF(IK.EQ.0) IL = IABS(IL)
      IF(IK.GT.IH) THEN
            II = IH
            IH = IK
            IK = II
            ISYM = 2
      ENDIF
      HKLOUT(1) = IH
      HKLOUT(2) = IK
      HKLOUT(3) = IL
      RETURN
C
C=======================================================================
C
  200 CONTINUE
C
C     TRIGONAL IV:
C                   EQUIVALENT REFLECTIONS:
C                     H, K, L;   K, -(H+K), L;  -(H+K), H, L
C                     K, H,-L;   H, -(H+K),-L;  -(H+K), K,-L
C                   ASYMMETRIC UNIT:
C                     H AND K POSITIVE
C                     K .LE. H
C                     IF (H.EQ.K) L POSITIVE
C
      IF(IH.LT.0) THEN
            II = IH + IK
            IF(II.LT.0) THEN
                  IK =  IH
                  IH = -II
            ELSE
                  IH =  IK
                  IK = -II
            ENDIF
      ENDIF
      IF(IK.LT.0) THEN
            II = IH + IK
            IF(II.LE.0) THEN
                  IK = -II
                  IL = -IL
            ELSE
                  IH =  II
                  IK = -IK
                  ISYM = 2
            ENDIF
      ENDIF
      IF(IK.GT.IH) THEN
            II =  IH
            IH =  IK
            IK =  II
            IL = -IL
      ENDIF
      IF(IH.EQ.IK) IL = IABS(IL)
      HKLOUT(1) = IH
      HKLOUT(2) = IK
      HKLOUT(3) = IL
      RETURN
C
C=======================================================================
C
  210 CONTINUE
C
C     TRIGONAL V (SPACEGROUP 155 WITH RHOMBOHEDRAL AXES):
C                   EQUIVALENT REFLECTIONS:
C                     H,  K,  L;     K,  L,  H;     L,  H,  K
C                    -H, -L, -K;    -K, -H, -L;    -L, -K, -H
C                   ASYMMETRIC UNIT:
C                     H .GE. 0
C                     H .GE. ABS(K)
C                     H .GE. ABS(L)
C                 ABS(K).GE. ABS(L)
C
      IMX = MAX0(IABS(IH), IABS(IK), IABS(IL))
      IF(IMX.EQ.IABS(IK)) THEN
            IK = IL
            IL = IH
            IH = IMX
      ELSE IF(IMX.EQ.IABS(IL)) THEN
            IL = IK
            IK = IH
            IH = IMX
      ENDIF
      IF(IABS(IL).GT.IABS(IK)) THEN
            II =  IK
            IK = -IL
            IL = -II
            IH = -IH
      ELSE IF(IK.EQ.IL) THEN
            IH = IABS(IH)
            IK = IABS(IK)
            IL = IABS(IL)
      ENDIF
      IF(IH.LT.0) THEN
            IH = -IH
            IK = -IK
            IL = -IL
            ISYM = 2
      ENDIF
      HKLOUT(1) = IH
      HKLOUT(2) = IK
      HKLOUT(3) = IL
      RETURN
C
C=======================================================================
C
  220 CONTINUE
C
C     HEXAGONAL I:
C                   EQUIVALENT REFLECTIONS  H,   K,   L;   -H,  -K,   L
C                                           K,-(H+K), L;   -K, (H+K), L
C                                         (H+K),-H,   L; -(H+K), H,   L
C                   ASYMMETRIC UNIT:        H, K  AND L  ALL POSITIVE
C                                           K .NE. 0
C
      IF(IH.LT.0) THEN
            IH = -IH
            IK = -IK
      ENDIF
      IF(IK.LT.0) THEN
            II = IH + IK
            IH = -IK
            IK =  II
      ENDIF
      IF(IK.LT.0) THEN
            II = IH + IK
            IH = -IK
            IK =  II
      ENDIF
      IF(IK.EQ.0) THEN
            IK =  IH
            IH =   0
      ENDIF
      IF(IL.LT.0) THEN
            IL = -IL
            ISYM = 2
      ENDIF
      HKLOUT(1) = IH
      HKLOUT(2) = IK
      HKLOUT(3) = IL
      RETURN
C
C=======================================================================
C
  230 CONTINUE
C
C     HEXAGONAL II:
C                   EQUIVALENT REFLECTIONS  H,   K,   L;   -H,  -K,   L
C                                           K,-(H+K), L;   -K, (H+K), L
C                                         (H+K),-H,   L; -(H+K), H,   L
C                                           K,   H,  -L;   -K,  -H,  -L
C                                           H,-(H+K),-L;   -H, (H+K),-L
C                                         (H+K),-K,  -L; -(H+K), K,  -L
C                   ASYMMETRIC UNIT:        H, K  AND L  ALL POSITIVE
C                                           K .LE. H
C
      IF(IH.LT.0) THEN
            IH = -IH
            IK = -IK
      ENDIF
      IF(IK.LT.0) THEN
            II = IH + IK
            IH = -IK
            IK =  II
      ENDIF
      IF(IK.LT.0) THEN
            II = IH + IK
            IH = -IK
            IK =  II
      ENDIF
      IF(IK.GT.IH) THEN
            II =  IH
            IH =  IK
            IK =  II
            IL = -IL
      ENDIF
      IF(IH.EQ.IK) IL = IABS(IL)
      IF(IK.EQ. 0) IL = IABS(IL)
      IF(IL.LT.0) THEN
            IL = -IL
            ISYM = 2
      ENDIF
      HKLOUT(1) = IH
      HKLOUT(2) = IK
      HKLOUT(3) = IL
      RETURN
C
C=======================================================================
C
  240 CONTINUE
C
C     CUBIC I:
C                   EQUIVALENT REFLECTIONS   H, K,  L;    -H, -K,  L
C                                           -H, K, -L;     H, -K, -L
C                                            K, L,  H;    -K, -L,  H
C                                           -K, L, -H;     K, -L, -H
C                                            L, H,  K;    -L, -H,  K
C                                           -L, H, -K;     L, -H, -K
C                   ASYMMETRIC UNIT:         H, K  AND L  ALL POSITIVE
C                                            H .GE. K
C                                            H .GE. L
C
      IF(IH.LT.0) THEN
            IH = -IH
            IK = -IK
      ENDIF
      IF(IK.LT.0) THEN
            IK = -IK
            IL = -IL
      ENDIF
      IF(IH.EQ.0) IL = IABS(IL)
      IF(IK.EQ.0) IL = IABS(IL)
      IF(IL.LT.0) THEN
            IL = -IL
            ISYM = 2
      ENDIF
      II = MAX0(IH, IK, IL)
      IF(II.EQ.IK) THEN
            IK = IL
            IL = IH
            IH = II
      ELSE
            IF(II.EQ.IL) THEN
                  IL = IK
                  IK = IH
                  IH = II
            ENDIF
      ENDIF
      HKLOUT(1) = IH
      HKLOUT(2) = IK
      HKLOUT(3) = IL
      RETURN
C
C=======================================================================
C
  250 CONTINUE
C
C     CUBIC II:
C                   EQUIVALENT REFLECTIONS   H, K,  L;    -H, -K,  L
C                                           -H, K, -L;     H, -K, -L
C                                            K, L,  H;    -K, -L,  H
C                                           -K, L, -H;     K, -L, -H
C                                            L, H,  K;    -L, -H,  K
C                                           -L, H, -K;     L, -H, -K
C                                            K, H, -L;     H, -L,  K
C                                           -L, K,  H;    -K, -H, -L
C                                           -H, L,  K;     L, -K,  H
C                                           -K, H,  L;    -H, -L, -K
C                                            L, K, -H;     K, -H,  L
C                                            H, L, -K;    -L, -K, -H
C                   ASYMMETRIC UNIT:         H, K  AND L  ALL POSITIVE
C                                            H .GE. K
C                                            K .GE. L
C
      IF(IH.LT.0) THEN
            IH = -IH
            IK = -IK
      ENDIF
      IF(IK.LT.0) THEN
            IK = -IK
            IL = -IL
      ENDIF
      IF(IH.EQ.0) IL = IABS(IL)
      IF(IK.EQ.0) IL = IABS(IL)
      IF(IL.LT.0) THEN
            II =  IK
            IK = -IL
            IL =  II
      ENDIF
      IMX = MAX0(IH, IK, IL)
      IF(IMX.EQ.IH) THEN
            IF(IK.LT.IL) THEN
                  ISYM = 2
                  II = IK
                  IK = IL
                  IL = II
            ENDIF
      ELSE
            IF(IMX.EQ.IK) THEN
                  IK = IL
                  IL = IH
                  IH = IMX
                  IF(IK.LT.IL) THEN
                        ISYM = 2
                        II = IK
                        IK = IL
                        IL = II
                  ENDIF
            ELSE
                  IL = IK
                  IK = IH
                  IH = IMX
                  IF(IK.LT.IL) THEN
                        ISYM = 2
                        II = IK
                        IK = IL
                        IL = II
                  ENDIF
            ENDIF
      ENDIF
      HKLOUT(1) = IH
      HKLOUT(2) = IK
      HKLOUT(3) = IL
      RETURN
      END
*DECK $FCLCDIR
      SUBROUTINE CLCDIR(CAPPA,PSI,PHI,ANGLE)
C///  VERSION 83/05/18
C
C          CALCULATES FROM PSI AND PHI THE DIRECTION ANGLES OF THE
C          ROTATION AXIS WITH TEH POSITIVE X,Y AND Z-AXES.
C          PSI AND PHI ARE BROUGHT BY "KAPRANG" IN THE RANGE
C          0<PHI<PI AND 0<PSI<PI. THIS LEADS TO DIRECTION ANGLES IN THE
C          RANGE: 0<ANG1<PI,0<ANG2<PI AND PI/2<ANG3<PI.
C          BY USING AX(ANG1,ANG2,ANG3)=AX(PI-ANG1,PI-ANG2,PI-ANG3) THE
C          DIRECTION ANGLES ARE BROUGHT WITHIN THE RANGE 0<ANG1<PI/2,
C          0<ANG2<PI AND 0<ANG3<PI.
C
C         W.HOL    JULY 1974
C
C\\\
C
      DIMENSION ANGLE(3)
      DATA PI /.31415926535898E+01/
      DATA HPI/.15707963267949E+01/
C
C
      ANGLE(2)= PSI
      ANGLE(1)=ACOS(SIN(PSI)*COS(PHI))
      RHO = ACOS(SIN(PSI)*SIN(PHI))
      ANGLE(3)=PI - RHO
C
      IF(ANGLE(1).GT.HPI)GOTO 100
      GOTO 200
  100 DO 150 I=1,3
  150 ANGLE(I)= PI - ANGLE(I)
  200 CONTINUE
C
      RETURN
C *  THIS PROGRAM VALID ON FTN4 AND FTN5 **
      END
*DECK $FCLCROT
      SUBROUTINE CLCROT(TH,ROT)
C///  VERSION 83/05/18
C
C         CALCULATES ELEMENTS OF ROTATION MATRIX FROM EULERIAN ANGLES.
C         REFERENCE : ROSSMANN & BLOW, ACTA CRYST.15(1962),PP.24-31
C                                      TABLE 1(A)
C
      DIMENSION ROT(3,3),TH(3)
C\\\
      S1=SIN(TH(1))
      S2=SIN(TH(2))
      S3=SIN(TH(3))
      C1=COS(TH(1))
      C2=COS(TH(2))
      C3=COS(TH(3))
C
      ROT(1,1)=-S1*C2*S3+C1*C3
      ROT(1,2)= C1*C2*S3+S1*C3
      ROT(1,3)= S2*S3
      ROT(2,1)=-S1*C2*C3-C1*S3
      ROT(2,2)= C1*C2*C3-S1*S3
      ROT(2,3)= S2*C3
      ROT(3,1)= S1*S2
      ROT(3,2)=-C1*S2
      ROT(3,3)= C2
C
      RETURN
C *  THIS PROGRAM VALID ON FTN4 AND FTN5 **
      END
*DECK $FCLCTHET
      SUBROUTINE CLCTHET(CAPPA,PSI,PHI,TH)
C///  VERSION 83/05/18
C
C         CALCULATES EULERIAN ANGLES FROM ROTATION ANGLE KAPPA AND THE
C         SPHERICAL POLAR COORDINATES  PSI AND PHI.
C         REFERENCE : ROSSMANN & BLOW, ACTA CRYST.15(1962),PP.24-31
C
      DIMENSION TH(3)
C\\\
      SK=SIN(CAPPA)
      CK=COS(CAPPA)
      SPH=SIN(PHI)
      CPH=COS(PHI)
      SPS = SIN(PSI)
      CPS = COS(PSI)
C
C         CALC COS(TH2)RESULTING INA THETA2 BETWEEN 0 AND +PI.
      CT2=CK+SPS*SPS*SPH*SPH*(1.0-CK)
      TH(2)=ACOS(CT2)
      ST2=  SIN(TH(2))
C
C         CALCULATE THETA3
      CT3ST2=-SPS*CPS*SPH*(1.0-CK)-SPS*CPH*SK
      CT3=CT3ST2/ST2
      ST3ST2=-SPS*SPS*SPH*CPH*(1.0-CK)+CPS*SK
      ST3=ST3ST2/ST2
      TH(3)=ATAN2(ST3,CT3)
C
C         CALCULATE THETA1
      CT1ST2=+SPS*CPS*SPH*(1.0-CK)-SPS*CPH*SK
      CT1=CT1ST2/ST2
      ST1ST2=-SPS*SPS*CPH*SPH*(1.0-CK)-CPS*SK
      ST1=ST1ST2/ST2
      TH(1)=ATAN2(ST1,CT1)
      CALL EULRANG(TH)
      RETURN
C *  THIS PROGRAM VALID ON FTN4 AND FTN5 **
      END
      SUBROUTINE CLEAR  ( NAME , N )
C///
C=======================================================================
C     PURPOSE :
C-----------------------------------------------------------------------
C
C             THIS ROUTINE SET THE FIRST N ELEMENTS OF ARRAY " NAME "
C         TO ZERO
C
C=======================================================================
C.......................................................................
C
C          THIS ROUTINE IS WRITTEN FOR CYBER 170/760 IN FTN5 BY
C
C                        W I L    G A Y K E M A
C
C                        DEPARTMENT  OF  CHEMICAL  PHYSICS
C                        STATE  UNIVERSITY  GRONINGEN
C                        NIJENBORGH  16
C                        GRONINGEN
C                        THE  NETHERLANDS
C
C
C          VERSION 82/11/12-WG
C
C.......................................................................
C=======================================================================
C     USAGE :
C-----------------------------------------------------------------------
C
C         CALL  CLEAR  ( NAME , N )
C         =========================
C
C             ARGUMENTS:
C                 NAME - FIRST ELEMENT OF VARIABLE TO SET TO ZERO
C                 N    - AN INTEGER CONSTANT OR VARIABLE WHICH
C                           SPECIFIES THE NUMBER OF VARIABLES
C                           TO SET TO ZERO
C
C=======================================================================
C\\\
      DIMENSION   NAME(N)
C
      DO 10 I = 1, N
          NAME(I) = 0
   10 CONTINUE
C
C
      RETURN
C
      END
      SUBROUTINE CMODE (DEL,RMIN,RMAX,STEP,CENTER,RM,MODE)
C
C
C.....FINDS THE BEST FORMAT TO PLOT MARKS ALONG THE AXES.
C
C
      CHARACTER*10 MINMRK, MAXMRK
      COMMON /SCALE/ XMIN, YMIN, XSCALE, YSCALE, DELTAX, DELTAY,
     + XPOS, YPOS, XM, YM, TZ, DEBUG, DASHL, ISPEN, ISYMB, SSIZE
C
C
C.....CODES:  MODE=1 > E8.1
C             MODE=2 > I8
C             MODE=3 > F8.1
C             MODE=4 > F8.2
C             MODE=5 > F8.3
C
      MODE=1
      IF (RMIN.LT.10000.0.OR.RMAX.GT.10000.0) MODE=1
      IF (DEL.GT.0.002) MODE=5
      IF (DEL.GT.0.02)  MODE=4
      IF (DEL.GT.0.2)   MODE=3
      IF (DEL.GT.2.0)   MODE=2
      CENTER=0.0
C
C.....CHECK FOR POSSIBLE OVERFLOWS
   10 IF (MODE.EQ.1) THEN
          WRITE (MINMRK,'(E8.2)') RMIN
          WRITE (MAXMRK,'(E8.2)') RMAX
          CENTER=4.0*TZ
      ELSEIF (MODE.EQ.2) THEN
          WRITE (MINMRK,'(I10)') NINT(RMIN)
          WRITE (MAXMRK,'(I10)') NINT(RMAX)
      ELSEIF (MODE.EQ.3) THEN
          WRITE (MINMRK,'(F10.1)') RMIN
          WRITE (MAXMRK,'(F10.1)') RMAX
          CENTER=6.5*TZ
      ELSEIF (MODE.EQ.4) THEN
          WRITE (MINMRK,'(F10.2)') RMIN
          WRITE (MAXMRK,'(F10.2)') RMAX
          CENTER=5.5*TZ
      ELSEIF (MODE.EQ.5) THEN
          WRITE (MINMRK,'(F10.3)') RMIN
          WRITE (MAXMRK,'(F10.3)') RMAX
          CENTER=4.5*TZ
      ENDIF
      IF (MINMRK(2:2).NE.' '.OR.MAXMRK(2:2).NE.' ') THEN
          WRITE (7,'('' MODE='',I4)')MODE
          IF (MODE.EQ.1) GOTO 15
          MODE=1
          WRITE (7,'('' MINMRK='',A10,'' MAXMRK='',A10)')MINMRK,MAXMRK
          GOTO 10
      ENDIF
   15 CONTINUE
C
C.....TEST LENGTH FORMAT
      DO 20, I=3,10
          IF (MINMRK(I:I).EQ.' '.AND.MAXMRK(I:I).EQ.' ') GOTO 20
          RM=REAL(11-I)*TZ
          GOTO 30
   20 CONTINUE
   30 IF (CENTER.EQ.0.0) CENTER=8*TZ-RM/2.0
      WRITE (7,*) 'CENTER=',CENTER,'I=',I
      RETURN
      END
      SUBROUTINE CNCHAR(IN,NCHAR,IO)
C COUNT NR. OF CHARS BEFORE ; 
C  INPUT CHARS IN IN(..) ARE TRANSFERRED TO IO(..) WITHOUT ;
C********ICHAR(<MNCHAR>)  ************* 
C******IN/IO(<LTXT>)****
CKRAAKHELDER********************* 
C :KRAAK*** CHRIN ND CHROUT SAME AS IN KOMPLOT **** 
CKRAAKHELDER****************************
      DIMENSION IN(1),IO(1),ICHAR(80) 
      DATA ISCL /1H;/ 
      DATA IBLNK /1H /
      DATA ISL /1H//
C 
C*******
      MNCHAR=80 
C*****************************
      LTXT=8
      NCHW=10 
C 
C 
      IF(IN(1).NE.0  )       GO TO 5
      NCHAR=0 
                             RETURN 
    5 CONTINUE
C UNPACK TEXT 
      IC=0
      IND=0 
      DO 20 K=1,LTXT
      IND=IND+1 
      DO 10 L=1,NCHW
      IC=IC+1 
      IF(IC.GT.MNCHAR)       GO TO 30 
      CALL CHROUT(IN(IND) ,L,ICHAR(IC) )
   10 CONTINUE
   20 CONTINUE
   30 CONTINUE
CBEGIN / : SAME AS BEFORE , // : ONE SLASH
      IF(ICHAR(1).NE.ISL)    GO TO 35 
C / 
      IF(ICHAR(2).NE.ISL)    RETURN 
C //
      NCHAR=1 
      CALL CHRIN(IO(1),1,ISL) 
                             RETURN 
   35 CONTINUE
CEND   / : SAME AS BEFORE , // : ONE SLASH
C COUNT NR. CHAR BEFORE ; 
      L=0 
      DO 40 K=1,MNCHAR
      IF(ICHAR(K).EQ.ISCL)   GO TO 50 
      L=L+1 
   40 CONTINUE
   50 CONTINUE
      NCHAR=L 
      IF(NCHAR.LT.MNCHAR)    GO TO 80 
C IF NO ; COUNT TRAILING BLANKS 
      L=MNCHAR
      DO 60 K=1,MNCHAR
      IF(ICHAR(L).NE.IBLNK)  GO TO 70 
      L=L-1 
   60 CONTINUE
   70 CONTINUE
      NCHAR=L 
      IF(NCHAR.EQ.0)         RETURN 
   80 CONTINUE
C PACK TEXT IN IO 
      IC=0
      DO 100 K=1,LTXT 
      DO 90  L=1,NCHW 
      IC=IC+1 
      IF(IC.GT.NCHAR)        RETURN 
      CALL CHRIN(IO(K),L,ICHAR(IC) )
   90 CONTINUE
  100 CONTINUE
      RETURN
      END 
*DECK $FCONTUR
      SUBROUTINE CONTUR(AM,M,N,CLZ,DEL,CL1,IT,HT,R,TH,M1,N1)
C///  VERSION 83/05/18
C
C         AM IS A MATRIX TO BE CONTOURED
C
      DIMENSION AM(M1,N1)
C
C         AM(1,1) IS TAKEN TO BE (0,0) SO AM MUST BE FILLED TO
C                 AM(M+1,N+1), WHERE M AND N ARE THE X AND Y AXIS MAXS
C                 ON THE PLOT ALSO
C         CLZ IS THE START CONTOUR LEVEL
C         DEL IS THE INCREMENT LEVEL
C         CL1 IS THE LEVEL ABOVE WHICH NO LEVELS ARE PLOTTED
C         IT=1      CONTINUOUS LINE
C           =2      DASHED LINE
C           =-1,-2  SUPERPOSITION ON PREVIOUS PLOT
C         HT HEIGHT OF CONTOUR - MAX OF 10 IN.
C         R  RATIO  OF ONE UNIT IN X TO ONE UNIT IN Y
C         TH DEGREES BETWEEN AXES    10<THETA<170
C\\\
      DATA RAD /.1745329251994E-01/
      NCONTUR = NCONTUR+1
      PRINT 999,NCONTUR
  999 FORMAT(' CONTUR HAS BEEN CALLED ',I5,'  TIMES')
      IZX=0
10    IF(IT)60,90,15
15    W=R*HT*M/N
      IF(W-48.)20,20,88
20    THE=TH * RAD
      IF(TH-10.)89,22,22
22    IF(TH-170.)24,24,89
24    IF(TH-90.)28,28,26
   26 YMOVE=-COS(THE)*HT
      CALL PLOT(YMOVE,0.,-3)
   28 CALL PLOT(0.,0.,-3)
C     THE NUMBERS PLOTTED ALONG THE AXES ARE MULTIPLES OF DX AND DY
C     RESPECTIVELY.
      READ(5,1,END=10000)YSTAP,XSTAP,FAC
C     YSTAP AND XSTAP ARE THE STEP SIZES ALONG THEX AND Y DIRECTION
C     FAC IS THE FACTOR BY WHICH THE AXES WITH THEIR ASSOCIATED NUMBERS
C      WILL BE ENLARGED IN THE PLOTS
    1 FORMAT(BZ,3F10.4)
      DY=(N*YSTAP)/(HT/FAC)
      DX=(M*XSTAP)/(W/FAC)
      HTX=HT/FAC
      HTZ=W/FAC
      IF(FAC-4.)2,3,2
    3 HTX=HTX-1.
C     'FACTOR' IS THE CALCOMP SUBROUTINE THAT ENLARGES THE PLOT
C     LINEARLY BY A FACTOR OF FAC.
    2 CALL FACTOR(FAC)
      CALL AXIS(0,0,'X',0,HTX,TH,0,DY)
      CALL AXIS(0,0,'Z',-1,HTZ,0,0,DX)
C     AFTER PLOTTING OF THE AXES THE PLOT IS REDUCED TO ITS NORMAL SIZE.
      CALL FACTOR(1.0)
60    D=IABS(IT)-1
   62 CALL SCAN (AM,M+1,N+1,CL0,D,W/HT,THE,HT,IZX)
64    CL0=CL0+DEL
      IF (CL0-CL1)62,62,90
88    WRITE(3,92)W
      GO TO 90
89    WRITE(3,91)TH
90    RETURN
91    FORMAT(' THETA BAD',F8.3)
92    FORMAT(' PLOT TOO LONG',F8.3)
10000 CALL ABORT ('  INPUT MISSING FOR  *CONTUR*')
      END
C
C
C
      SUBROUTINE COPYVECTOR( A, B, NX)
C     =================================
C
C
      DIMENSION A( NX), B( NX)
C
C
      DO 999 I = 1, NX
        B( I) = A( I)
999   CONTINUE
C
C
      RETURN
C
C
      END
C
C
C
      SUBROUTINE CROSSP( A, B, AB)
C     ==================================
C
C
      DIMENSION A( 3), B( 3), AB( 3)
C
C
      AB( 1) = A( 2) * B( 3) - A( 3) * B( 2)
      AB( 2) = A( 3) * B( 1) - A( 1) * B( 3)
      AB( 3) = A( 1) * B( 2) - A( 2) * B( 1)
C
C
      RETURN
C
C
      END
*DECK $FCRVFIT
      SUBROUTINE CRVFIT(MD, L, X, Y, M, N, U, V)
C///  VERSION 83/05/18
C
C     THIS  ROUTINE  IS  COPIED  FROM  ACCULIB
C
C     THE  SOURCE  CODE  WAS  70125
C
C SMOOTH CURVE FITTING
C THIS ROUTINE FITS A SMOOTH CURVE TO A GIVEN SET OF IN-
C PUT DATA POINTS IN AN X-Y PLANE. IT INTERPOLATES POINTS
C IN EACH INTERVAL BETWEEN A PAIR OF DATA POINTS AND GENER-
C ATES A SET OF OUTPUT POINTS CONSISTING OF THE INPUT DATA
C POINTS AND THE INTERPOLATED POINTS. IT CAN PROCES EITHER
C A SINGLE-VALUED FUNCTION OR A MULTIPLE-VALUED FUNCTION.
C THE INPUT PARAMETERS ARE : 
C     MD = MODE OF THE CURVE (MUST BE 1 OR 2)
C        = 1 FOR A SINGLE-VALUED FUNCTION
C        = 2 FOR A MULTIPLE-VALUED FUNCTION
C     L  = NUMBER OF INPUT DATA POINTS
C          (MUST BE 2 OR GREATER)
C     X  = ARRAY OF DIMENSION L STORING THE ABSCISSAS OF
C          INPUT DATA POINTS (IN ASCENDING OR DESCENDING
C          ORDER FOR MD = 1)
C     Y  = ARRAY OF DIMENSION L STORING THE ORDINATES OF
C          INPUT DATA POINTS
C     M  = NUMBER OF SUBINTERVALS BETWEEN EACH PAIR OF
C          INPUT DATA POINTS (MUST BE 2 OR GREATER)
C     N  = NUMBER OF OUTPUT POINTS
C        = (L-1)*M+1
C THE OUTPUT PARAMETERS ARE : 
C     U  = ARRAY OF DIMENSION N WHERE THE ABSCISSAS OF
C          OUTPUT POINTS ARE TO BE DISPLAYED
C     V  = ARRAY OF DIMENSION N WHERE THE ORDINATES OF
C          OUTPUT POINTS ARE TO BE DISPLAYED
C\\\
C
C DECLARATION STATEMENTS
      DIMENSION X(L), Y(L), U(N), V(N)
      EQUIVALENCE (M1,B1), (M2,B2), (M3,B3), (M4,B4), (X2,P0), (Y2,Q0),
     1 (T2,Q1)
      REAL M1, M2, M3, M4
      EQUIVALENCE (W2,Q2), (W3,Q3), (A1,P2), (B1,P3), (A2,DZ), (SW,R,Z)
C PRELIMINARY PROCESSING
      MD0 = MD
      MDM1 = MD0 - 1
      L0 = L
      LM1 = L0 - 1
      M0 = M
      MM1 = M0 - 1
      N0 = N
      IF (MD0.LE.0) GO TO 320
      IF (MD0.GE.3) GO TO 320
      IF (LM1.LE.0) GO TO 330
      IF (MM1.LE.0) GO TO 340
      IF (N0.NE.LM1*M0+1) GO TO 350
      GO TO (10, 60), MD0
      CALL ABORT('ERROR IN GO TO STATEMENT - MD0')
   10 I = 2
      IF (X(1)-X(2)) 20, 360, 40
   20 DO 30 I=3,L0
         IF (X(I-1)-X(I)) 30, 360, 370
   30 CONTINUE
      GO TO 80
   40 DO 50 I=3,L0
         IF (X(I-1)-X(I)) 370, 360, 50
   50 CONTINUE
      GO TO 80
   60 DO 70 I=2,L0
         IF (X(I-1).NE.X(I)) GO TO 70
         IF (Y(I-1).EQ.Y(I)) GO TO 380
   70 CONTINUE
   80 K = N0 + M0
      I = L0 + 1
      DO 90 J=1,L0
         K = K - M0
         I = I - 1
         U(K) = X(I)
         V(K) = Y(I)
   90 CONTINUE
      RM = M0
      RM = 1.0/RM
C MAIN DO-LOOP
      K5 = M0 + 1
      DO 310 I=1,L0
C ROUTINES TO PICK UP NECESSARY X AND Y VALUES AND
C          TO ESTIMATE THEM IF NECESSARY
         IF (I.GT.1) GO TO 130
         X3 = U(1)
         Y3 = V(1)
         X4 = U(M0+1)
         Y4 = V(M0+1)
         A3 = X4 - X3
         B3 = Y4 - Y3
         IF (MDM1.EQ.0) M3 = B3/A3
         IF (L0.NE.2) GO TO 140
         A4 = A3
         B4 = B3
  100    GO TO (120, 110), MD0
      CALL ABORT('ERROR IN GO TO STATEMENT - MD0')
  110    A2 = A3 + A3 - A4
         A1 = A2 + A2 - A3
  120    B2 = B3 + B3 - B4
         B1 = B2 + B2 - B3
         GO TO (180, 210), MD0
      CALL ABORT('ERROR IN GO TO STATEMENT - MD0')
  130    X2 = X3
         Y2 = Y3
         X3 = X4
         Y3 = Y4
         X4 = X5
         Y4 = Y5
         A1 = A2
         B1 = B2
         A2 = A3
         B2 = B3
         A3 = A4
         B3 = B4
         IF (I.GE.LM1) GO TO 150
  140    K5 = K5 + M0
         X5 = U(K5)
         Y5 = V(K5)
         A4 = X5 - X4
         B4 = Y5 - Y4
         IF (MDM1.EQ.0) M4 = B4/A4
         GO TO 160
  150    IF (MDM1.NE.0) A4 = A3 + A3 - A2
         B4 = B3 + B3 - B2
  160    IF (I.EQ.1) GO TO 100
         GO TO (170, 200), MD0
      CALL ABORT('ERROR IN GO TO STATEMENT - MD0')
C NUMERICAL DIFFERENTIATION
  170    T2 = T3
  180    W2 = ABS(M4-M3)
         W3 = ABS(M2-M1)
         SW = W2 + W3
         IF (SW.NE.0.0) GO TO 190
         W2 = 0.5
         W3 = 0.5
         SW = 1.0
  190    T3 = (W2*M2+W3*M3)/SW
         IF (I-1) 310, 310, 240
  200    COS2 = COS3
         SIN2 = SIN3
  210    W2 = ABS(A3*B4-A4*B3)
         W3 = ABS(A1*B2-A2*B1)
         IF (W2+W3.NE.0.0) GO TO 220
         W2 = SQRT(A3*A3+B3*B3)
         W3 = SQRT(A2*A2+B2*B2)
  220    COS3 = W2*A2 + W3*A3
         SIN3 = W2*B2 + W3*B3
         R = COS3*COS3 + SIN3*SIN3
         IF (R.EQ.0.0) GO TO 230
         R = SQRT(R)
         COS3 = COS3/R
         SIN3 = SIN3/R
  230    IF (I-1) 310, 310, 250
C DETERMINATION OF THE COEFFICIENTS
  240    Q2 = (2.0*(M2-T2)+M2-T3)/A2
         Q3 = (-M2-M2+T2+T3)/(A2*A2)
         GO TO 260
  250    R = SQRT(A2*A2+B2*B2)
         P1 = R*COS2
         P2 = 3.0*A2 - R*(COS2+COS2+COS3)
         P3 = A2 - P1 - P2
         Q1 = R*SIN2
         Q2 = 3.0*B2 - R*(SIN2+SIN2+SIN3)
         Q3 = B2 - Q1 - Q2
         GO TO 280
C COMPUTATION OF THE POLYNOMIALS
  260    DZ = A2*RM
         Z = 0.0
         DO 270 J=1,MM1
            K = K + 1
            Z = Z + DZ
            U(K) = P0 + Z
            V(K) = Q0 + Z*(Q1+Z*(Q2+Z*Q3))
  270    CONTINUE
         GO TO 300
  280    Z = 0.0
         DO 290 J=1,MM1
            K = K + 1
            Z = Z + RM
            U(K) = P0 + Z*(P1+Z*(P2+Z*P3))
            V(K) = Q0 + Z*(Q1+Z*(Q2+Z*Q3))
  290    CONTINUE
  300    K = K + 1
  310 CONTINUE
      RETURN
C ERROR EXIT
  320 PRINT 99999
      GO TO 400
  330 PRINT 99998
      GO TO 400
  340 PRINT 99997
      GO TO 400
  350 PRINT 99996
      GO TO 400
  360 PRINT 99995
      GO TO 390
  370 PRINT 99994
      GO TO 390
  380 PRINT 99993
  390 PRINT 99992, I, X(I), Y(I)
  400 PRINT 99991, MD0, L0, M0, N0
      RETURN
C FORMAT STATEMENTS
99999 FORMAT (1X/31H  ***   MD OUT OF PROPER RANGE./)
99998 FORMAT (1X/22H  ***   L = 1 OR LESS./)
99997 FORMAT (1X/22H  ***   M = 1 OR LESS./)
99996 FORMAT (1X/25H  ***   IMPROPER N VALUE./)
99995 FORMAT (1X/27H  ***   IDENTICAL X VALUES./)
99994 FORMAT (1X/33H  ***   X VALUES OUT OF SEQUENCE./)
99993 FORMAT (1X/33H  ***   IDENTICAL X AND Y VALUES./)
99992 FORMAT (7H   I  =, I4, 10X, 6HX(I) =, E12.3, 10X, 6HY(I) =, E12.3)
99991 FORMAT (7H   MD =, I4, 8X, 3HL =, I5, 8X, 3HM =, I5, 8X, 3HN =,
     1 I5/36H ERROR DETECTED IN ROUTINE    CRVFIT)
      END

      SUBROUTINE DASH(X,XT,Y,YT,N,J,L,ALINE2,GAP2)
C 
C        DRAW A DASHED LINE.                   L. WEISSMAN  SEPT 1980 
C 
C        X       ARRAY OF UNSCALED ABCISSA VALUES 
C        XT      XMIN AND DX PROVIDED BY "SCALE"
C        Y       ARRAY OF UNSCALED ORDINATE VALUES
C        YT      YMIN AND DY PROVIDED BY "SCALE"
C        N       NUMBER OF POINTS IN THE ARRAY
C        J       >0, SYMBOL AND LINE CONNECTING (PRINT SYMBOL EVERY 
C                    J'TH POINT)
C                =0, LINE ONLY
C                <0, PRINT SYMBOL ONLY AT EVERY J'TH POINT
C        L       NUMBER OF SYMBOL ,SEE SYMBOL ROUTINE FOR LIST
C                0-17 ARE CENTERED SPECIAL SYMBOLS
C                32-127 ARE THE NORMAL ASCII CODES
C        ALINE2  LENGTH OF LINE SEGMENT TO BE DRAWN (PLOT UNITS)
C        GAP2    LENGTH OF GAPS BETWEEN LINE SEGMENTS (PLOT UNITS)
C 
      REAL X(N),Y(N),XT(2),YT(2)
C 
      XMIN=XT(1)
      YMIN=YT(1)
      DX=XT(2)
      DY=YT(2)
      IF(N-1)100,50,1 
C 
C        DRAW THE DASHED LINE 
C        NOTE: (X1,Y1) IS THE PREVIOUSLY PLOTTED POINT
C              (X2,Y2) IS THE NEXT POINT INDICATED BY ARRAYS X,Y
C              (X3,Y3) IS THE NEXT POINT INDICATED BY ALINE OR GAP
C 
1     IF(J .LT. 0)GO TO 50
      I=1 
      X2=(X(1)-XMIN)/DX 
      Y2=(Y(1)-YMIN)/DY 
      CALL PLOT(X2,Y2,3)
      ALINE=ALINE2
      IF(ALINE .LE. 0.0)ALINE=0.15
      GAP=GAP2
      IF(GAP .LE. 0.0)GAP=0.075 
      IP=2
      D12=0.0 
      D13=ALINE 
C 
C        WHICH IS CLOSER, (X2,Y2) OR (X3,Y3)? 
C 
10    IF(D13 .LT. D12)GO TO 40
C 
C        (X2,Y2) IS CLOSER. 
C 
      CALL PLOT(X2,Y2,IP) 
      D13=D13-D12 
      X1=X2 
      Y1=Y2 
20    I=I+1 
      IF(I .GT. N)GO TO 50
      X2=(X(I)-XMIN)/DX 
      Y2=(Y(I)-YMIN)/DY 
30    D12=SQRT((X2-X1)**2+(Y2-Y1)**2) 
      IF(D12 .LT. .0001)GO TO 20
      T=D13/D12 
      X3=X1+T*(X2-X1) 
      Y3=Y1+T*(Y2-Y1) 
      GO TO 10
C 
C        (X3,Y3) IS CLOSER. 
C 
40    CALL PLOT(X3,Y3,IP) 
      D13=ALINE 
      IF(IP .EQ. 2)D13=GAP
      IP=5-IP 
      X1=X3 
      Y1=Y3 
      GO TO 30
C 
C        PLOT SYMBOLS 
C 
50    IF(J .EQ. 0)GO TO 100 
      AL=L
      IF(AL.LT.0. .OR. AL.GT.127.)AL=63.
      JJ=IABS(J)
      DO 60 I=1,N,JJ
      X1=(X(I)-XMIN)/DX 
      Y1=(Y(I)-YMIN)/DY 
      CALL PLOT(X1,Y1,3)
      CALL SYMBL(X1,Y1,0.14,AL,0.0,-1)
60    CONTINUE
100   RETURN
      END 
*DECK $FDASHIT
      SUBROUTINE DASHIT(X1,Y1,X2,Y2,IDASH)
C///  VERSION 83/05/18
C
C     DASH A STRAIGHT LINE BETWEEN (X1,Y1) AND (X2,Y2)
C     THERE ARE IDASH DASHES PER CM.
C     2/3 OF EACH DASH IS ACTUALLY DRAWN, 1/3 IS SKIPPED.
C     IDASH = 0 MEANS A SOLID LINE.
C     LEO LIJK   02-08-78
C\\\
C
      CALL PLOT(X1,Y1,3)
      IF(IDASH.GT.0) GOTO 1
      CALL PLOT(X2,Y2,2)
      RETURN
1     X=SQRT((X1-X2)**2.+(Y1-Y2)**2.)
      X = FLOAT(IDASH)*X
      NDASH = X
      IF(NDASH.LE.0) RETURN
      DDX = (X2-X1)/X
      DDY = (Y2-Y1)/X
      DDDX = DDX/3.
      DDDY = DDY/3.
      DDX =  DDX - DDDX
      DDY = DDY - DDDY
      X=X1
      Y=Y1
      DO 2 I = 1,NDASH
      X=X+DDX
      Y=Y+DDY
      CALL PLOT(X,Y,2)
      X=X+DDDX
      Y=Y+DDDY
      CALL PLOT(X,Y,3)
2     CONTINUE
      RETURN
C *  THIS PROGRAM VALID ON FTN4 AND FTN5 **
      END
C
C
C
      SUBROUTINE DCALC(CELL)
C     =======================
C
C
C---- The resolution can be calculated as follows:
C
C     DSTAR-SQUARED = IH*IH*A11 + IH*IK*A21 + IH*IL*A31
C                   + IK*IK*A22 + IK*IL*A32 + IL*IL*A33
C     SO:       RES = 1.0/SQRT(DSTAR_SQUARED)
C
C     in this subroutine the coefficients a11, a21, a31, a22, a32, a33
C     are calculated
C
C
C
C     .. Array Arguments ..
      REAL CELL(6)
C     ..
C     .. Scalars in Common ..
      REAL A11,A21,A22,A31,A32,A33
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      REAL AST,BST,CA,CAST,CB,CBST,CG,CGST,CST,RAD,S,SA,SAST,SB,SBST,SG,
     +     SGST
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ATAN,COS,SIN,SQRT
C     ..
C     .. Common blocks ..
      COMMON /AINOUT/LUNIN,LUNOUT
      COMMON /MDFRES/A11,A21,A22,A31,A32,A33
C     ..
C
C
      RAD = ATAN(1.0)/45.0
      S = CELL(4)*RAD
      CA = COS(S)
      SA = SIN(S)
      S = CELL(5)*RAD
      CB = COS(S)
      SB = SIN(S)
      S = CELL(6)*RAD
      CG = COS(S)
      SG = SIN(S)
      CAST = (CB*CG-CA)/ (SB*SG)
      CBST = (CG*CA-CB)/ (SG*SA)
      CGST = (CA*CB-CG)/ (SA*SB)
      SAST = SQRT(1.0-CAST*CAST)
      SBST = SQRT(1.0-CBST*CBST)
      SGST = SQRT(1.0-CGST*CGST)
      AST = 1.0/ (CELL(1)*SB*SGST)
      BST = 1.0/ (CELL(2)*SG*SAST)
      CST = 1.0/ (CELL(3)*SA*SBST)
      A11 = AST*AST
      A21 = 2.0*AST*BST*CGST
      A22 = BST*BST
      A31 = 2.0*AST*CST*CBST
      A32 = 2.0*BST*CST*CAST
      A33 = CST*CST
C
C
      END
      SUBROUTINE DECENT (DELTA,SIZE,NS,STEP,DIV,MODE)
C
C
C.....CALCULATE DECENT SUBDIVISIONS FOR THE AXES: 
C
      DEL=DELTA    
      FAC = 1
      IF (DEL.EQ.0.0) THEN 
          STEP = 0.0
          GOTO 90 
      ENDIF 
      NS = NINT(SIZE/DIV) 
      DEL = DEL / NS
      DEL = ABS(DEL)
   70 IF (DEL.LT.1) THEN 
          DEL = DEL * 10 
          FAC = FAC / 10
          GOTO 70
      ENDIF 
   80 IF (DEL.GT.10) THEN
          DEL = DEL / 10
          FAC = FAC * 10
          GOTO 80 
      ENDIF 
      STEP = (NINT(DEL))*FAC*SIZE/(DELTA)
      NS=INT(SIZE/STEP+0.00001)
   90 RETURN
      END
      SUBROUTINE DERRB(ROT,TH,RP) 
C///  VERSION 86/11/08 
C 
C        CALCULATES ELEMENTS OF MATRIX ROT-PRIME I.E. THE DERIVATIVE
C        OF EACH ELEMENT OF ROT(I,J) VERSUS THETA(K) 
C 
C         RP(I,J,K) = D(ROT(I,J))/D(THETA(K))  (PARTIAL DERIVATIVE) 
C
C\\\
      DIMENSION  ROT(3,3),TH(3),RP(3,3,3) 
      CALL DTR(TH)
      S1 = SIN(TH(1))
      S2 = SIN(TH(2))
      S3 = SIN(TH(3))
      C1 = COS(TH(1))
      C2 = COS(TH(2))
      C3 = COS(TH(3))
      CALL RTD(TH)
C
      RP(1,1,1)=-ROT(2,1)
      RP(2,1,1)= ROT(1,1)
      RP(3,1,1)= 0.0
      RP(1,2,1)=-ROT(2,2)
      RP(2,2,1)= ROT(1,2)
      RP(3,2,1)= 0.0
      RP(1,3,1)=-ROT(2,3)
      RP(2,3,1)= ROT(1,3)
      RP(3,3,1)= 0.0
      RP(1,1,2)= S1*S2*S3
      RP(2,1,2)=-C1*S2*S3
      RP(3,1,2)= C2*S3
      RP(1,2,2)= S1*S2*C3
      RP(2,2,2)=-C1*S2*C3
      RP(3,2,2)= C2*C3
      RP(1,3,2)= S1*C2
      RP(2,3,2)=-C1*C2
      RP(3,3,2)=-S2
      RP(1,1,3)= ROT(1,2)
      RP(2,1,3)= ROT(2,2)
      RP(3,1,3)= ROT(3,2)
      RP(1,2,3)=-ROT(1,1)
      RP(2,2,3)=-ROT(2,1)
      RP(3,2,3)=-ROT(3,1)
      RP(1,3,3)= 0.0
      RP(2,3,3)= 0.0
      RP(3,3,3)= 0.0
      RETURN
      END
C
C
C
      FUNCTION DET( A)
C     ================
C
C
      DIMENSION A( 3, 3)
C
C
      DET = A(1,1)*( A(2,2)*A(3,3) - A(2,3)*A(3,2)) +
     +      A(1,2)*( A(2,3)*A(3,1) - A(2,1)*A(3,3)) +
     +      A(1,3)*( A(2,1)*A(3,2) - A(2,2)*A(3,1))
C
C
      RETURN
C
C
      END
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
*DECK $FDIHED
      FUNCTION DIHED(X1,X2,X3,X4)
C///  VERSION 83/05/18
C
C       CALCULATES THE DIHEDRAL ANGLE (IN RADIANS) BETWEEN THE PLANES
C       FIXED BY X1, X2 AND X3 AND BY X2, X3 AND X4.
C       FORMULA USED FOR THE ANGLE TAU: 
C
C                              (U*V).(V*W)
C                 TAU = ARCCOS -----------
C                              !U*V! !V*W!
C
C       REFERENCE: U.SHMUELI, ACTA CRYST. VOL. A30 (1974) 848-9
C
      DIMENSION X1(3),X2(3),X3(3),X4(3),U(3),V(3),W(3),UV(3),VW(3)
C\\\
      DATA ALMOST1/0.99999999999999/
C
C       SET UP VECTORS U, V, W:  U = X1 > X2, V = X2 > X3, W = X3 > X4
      DO 10 I=1,3
      U(I) = X2(I) - X1(I)
      V(I) = X3(I) - X2(I)
      W(I) = X4(I) - X3(I)
 10   CONTINUE
C
C       CALCULATE TAU
      CALL VECPR(U,V,UV,UVL)
      CALL VECPR(V,W,VW,VWL)
      ARG = DOT(UV(1),VW(1))/(UVL*VWL)
      IF(ABS(ARG).GT.ALMOST1) ARG = SIGN(ALMOST1,ARG)
      TAU = ACOS(ARG)
C
C       CALCULATE SIGN OF TAU AS THE SIGN OF THE TRIPLE-PRODUCT (U*V).W
      DIHED = SIGN (TAU,DOT(UV(1),W(1)))
      RETURN
C *  THIS PROGRAM VALID ON FTN4 AND FTN5 **
      END

      SUBROUTINE DMPRIN(A,N,M,MODE,W,LU) 
C 
C        Print out a double precision matrix 
C 
C        A:  The matrix
C        N:  # rows
C        M:  # columns 
C        MODE: Storage mode. 0=general (column order), 1=symmetric
C              (upper triangle in column order), 2=diagonal 
C        W:  A work vector of length M 
C        LU: Logical unit for printing
C 
C        L. Weissman
C        Department of Biology, Gilmer Hall
C        University of Virginia
C        McCormick Road
C        Charlottesville, VA  22901
C
      DOUBLE PRECISION A(*),W(*)
C 
      WRITE(LU,5)MODE 
5     FORMAT(/10X,'---  Begin MPRINT  --- MODE=',I2)
      IF(N)120,120,10 
10    IF(M)120,120,15 
15    DO 100 I=1,N
      DO 90 J=1,M 
      W(J)=0.0D0
      IF(MODE-1)70,40,20
C 
C        Diagonal storage 
C 
20    IF(I-J)90,30,90 
30    K=I 
      GO TO 80
C 
C        Symmetric storage
C 
40    IF(I-J)50,60,60 
50    K=I+(J*J-J)/2 
      GO TO 80
60    K=J+(I*I-I)/2 
      GO TO 80
C 
C        General storage
C 
70    K=N*(J-1)+I 
80    W(J)=A(K) 
90    CONTINUE
100   WRITE(LU,110)I,(W(J),J=1,M) 
110   FORMAT(1X,I2,10(1PD12.4)/(3X,10(1PD12.4)))
120   WRITE(LU,130) 
130   FORMAT(10X,'---  End MPRINT  ---'/) 
      RETURN
      END 
*DECK $FDOT
      FUNCTION DOT(X,Y)
C///  VERSION 83/05/18
C
C       RETURNS THE SCALAR PRODUCT OF 2 VECTORS
C
      DIMENSION X(3),Y(3)
C\\\
      DOT = 0.
      DO 10 I=1,3
      DOT = DOT + X(I)*Y(I)
 10   CONTINUE
      RETURN
C *  THIS PROGRAM VALID ON FTN4 AND FTN5 **
      END
C
C
C
      FUNCTION DOTPRD( A, B, NX)
C     ==============================
C
C
      DIMENSION A( NX), B( NX)
C
C
      SUM = 0.
C
C
      DO 999 I = 1, NX
        SUM = SUM + A( I) * B( I)
999   CONTINUE
C
C
      DOTPRD = SUM
C
C
      RETURN
C
C
      END
      SUBROUTINE DPINIT(LIO,LMODEL,LVER,LPSIZE,LCHARS,LUNIT,LFILE)      
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C INITIALIZE THE SOFTWARE AND THE PLOTTER                               
C                                                                       
C   PARAMETERS :                                                        
C                                                                       
C   LIO      = HANDSHAKE METHOD DESIRED : 3 = NO HANDSHAKING            
C                                         0 = ENQUIRE / ACKNOWLEDGE     
C                                         1 = XON / XOFF                
C                                         2 = HP-IB                     
C                                                                       
C                                                                       
C              NOTE : IF THE VALUE OF -LIO- IS NEGATIVE, THE INITIAL-   
C                     IZATION OF SYSTEM CONFIGURATION PARAMETERS WILL   
C                     BE SUPPRESSED.                                    
C                                                                       
C                                                                       
C   LMODEL   = MODEL NUMBER OF PLOTTER                                  
C                                                                       
C   LVER     = VERSION OF PLOTTER   (A,B,C,S OR T)                      
C                      65,66,67,83,84                                   
C                                                                       
C   LPSIZE   = PAPER SIZE  (DEPENDS ON MODEL)                           
C                       0 = STANDARD                                    
C                       1 = FIRST ALTERNATE                             
C                       2 ETC.  (WHATEVER IS SUPPORTED)                 
C                                                                       
C   LCHARS   = THE STANDARD CHARACTER SET DESIRED                       
C                                                                       
C                                                                       
C   LUNIT    = UNIT SYSTEM DESIRED : 0 = INCHES                         
C                                    1 = METRIC                         
C                                                                       
C   LFILE    = LOGICAL UNIT OF THE PLOT FILE                            
C                                                                       
C                                                                       
C *****************************************************************     
C                                                                       
C *** COMMON BLOCK                                                      
C                                                                       
      COMMON /HP/ XMINPU, YMINPU, XORG  , YORG  , XOFF  , YOFF    ,     
     +            XFACT , YFACT , XCNTRL, YCNTRL, XYUNIT          ,     
     +            MDEV  , MUNIT , MPENS , MLU   , MCNTRL, MINIT   ,     
     +            MSHAKE, MPTR  , MCODE , MPLOT , MFLUSH, MSIZE   ,     
     +            MSPOOL, MVER  , MOUT(72),                             
     +            RSI1  , RSI2  , RDI1   , RDI2  , RES1  , RES2         
C                                                                       
C                                                                       
C *****************************************************************     
C                                                                       
C *** COMMON BLOCK                                                      
C                                                                       
      COMMON/ZZCOM/MZBITS,MZCHAR,MZWORD,MZJUST,MZCCSW,MZCCTL,MZDCCS,    
     +MZDCCT,MZHISW,MZLUSW,MZLURD,MZLUWR,                               
     +MZTAD(2),MZOTC(2),MZETC(2),MZOT1(2),MZOT2(2),                     
     +MZHM(2),MZBS(2),MZHEC(2),MZHS1(2),MZHS2(2),MZICD(2),MZIRS1(2),    
     +MZIRS2(2),MZMBS(2),MZSCOC(2)                                      
C                                                                       
C *****************************************************************     
      MIO=MOD(IABS(LIO),10) + 10                                        
      MDEV=LMODEL                                                       
      MVER=LVER                                                         
C                                                                       
CC**** NOW STORE PAPER SIZE IN MZDCCT FOR USE BY HPINIT                 
C                                                                       
      MZDCCT=LPSIZE                                                     
      IF (MVER.NE.65.AND.MVER.NE.66.AND.MVER.NE.67                      
     +              .AND.MVER.NE.83.AND.MVER.NE.84) MVER=65             
      CALL HPINIT(MIO,0,LCHARS,LUNIT,LFILE)                             
      RETURN                                                            
      END                                                               
*DECK $FDSQINV
      FUNCTION DSQINV(IH, GR)
C///  VERSION 83/05/18
*     CALCULATE  (D*)**2.
*     INPUT.. H,K,L AND THE RECIPROCAL METRIC TENSOR [GR].
*
*      APRIL 1983 (LJL)
C\\\
      DIMENSION IH(3), GR(3,3)
      DSQINV = 0.
      DO 1 I = 1,3
        DO 1 J = 1,3
1         DSQINV = DSQINV + FLOAT(IH(I)*IH(J)) * GR(I,J)
      RETURN
      END
      SUBROUTINE DTR(A)
C
C   SUBROUTINE FOR CONVERTING ANGLES FROM DEGREES INTO RADIANS.
C
      DIMENSION      A(3)
C
      DEGTOR = ATAN(1.0)/45.0
      DO 100 I = 1,3
  100 A(I) = A(I)*DEGTOR
C
      RETURN 
      END

      SUBROUTINE DUMP(IBUF,N,NOUT)
C 
C        Dump a buffer - octal and ASCII format
C
C        N:    # Bytes of "IBUF" to dump 
C        NOUT: Logical unit for output
C
C        Unprintable characters will appear as a pound sign (#)
C 
C        L. Weissman
C        Department of Biology, Gilmer Hall
C        University of Virginia
C        McCormick Road
C        Charlottesville, VA  22901
C
      INTEGER*2 IBUF(*),LINE(8),LINE2(8)
      DATA NCALL/0/ 
C
      J=N 
      IF(N)10,70,20 
10    J=-(N-1)/2
20    NLINES=(J+7)/8
      NCALL=NCALL+1 
      WRITE(NOUT,25)NCALL 
25    FORMAT('  Dump block# ',I6) 
      DO 50 I=1,NLINES
C 
C        Clear the output lines
C
      DO 30 I1=1,8
      LINE(I1)=0
30    LINE2(I1)=0
      J1=(I-1)*8+1
      J2=MIN0(J1+7,J) 
C 
C        Fill output line
C
      I2=0
      DO 40 I1=J1,J2
      I2=I2+1 
      LINE(I2)=IBUF(I1) 
      ILEFT=IAND(IBUF(I1),'177400'O)
      IRIGHT=IAND(IBUF(I1),'377'O)
      IF(ILEFT .GT. '77000'O .OR. ILEFT .LT. '20000'O)
     1   ILEFT='21400'O
      IF(IRIGHT .GT. '176'O .OR. IRIGHT .LT. 32)IRIGHT=35
40    LINE2(I2)=IOR(IRIGHT,ILEFT) 
50    WRITE(NOUT,60)LINE,LINE2
60    FORMAT(1X,8O7,4H  ..,8A2,2H..)
70    RETURN
      END 
      SUBROUTINE ENDPLT
      CALL NEWPEN (0)
      CALL PLOT (0.0, 0.0, 999)
      RETURN
      END
*DECK $FEULRANG
      SUBROUTINE EULRANG(TH)
C///  VERSION 83/05/18
C
C         REFERENCE : ROSSMANN & BLOW, ACTA CRYST.15(1962),PP.24-31
C         EULERIAN ANGLES SHOULD BE: 
C         0<TH1<PI,0<TH2<2PI,0<TH3<2PI. INORDER TOARRIVE AT THIS USE
C         ROT(TH1,TH2,TH3)=ROT(TH1+PI,-TH2,TH3+PI) AND
C         ROT(TH1,TH2,TH3)=ROT(TH1+K*2PI,TH2+L*2PI,TH3+M*2PI).
C\\\
      REAL TH(3)
      DATA PI/0.31415926535898E+01/
      DATA TPI/0.62831853071796E+01/
C
      DO 5 I=1,3
    5 TH(I)= AMOD(TH(I),TPI)
C
      IF(TH(1).LT.-PI)GOTO 10
      GOTO 20
   10 TH(1)=TH(1)+TPI
      GO TO 60
   20 IF(TH(1).LT.0)GOTO 30
      GOTO 40
   30 TH(1)=TH(1)+PI
      TH(2) = -TH(2)
      TH(3)=TH(3)+PI
      GO TO 60
   40 IF(TH(1).GT.PI)GOTO 50
      GOTO 60
   50 TH(1)=TH(1)-PI
      TH(2) = -TH(2)
      TH(3)= TH(3)+PI
   60 CONTINUE
C
      IF ( TH(2) .LT.0.0) TH(2) = TH(2) + TPI
      IF ( TH(2) .GT. TPI) TH(2) = TH(2) -TPI
C
      IF (TH(3).LT.0)TH(3)=TH(3)+TPI
      IF (TH(3).GT.TPI)TH(3)=TH(3)-TPI
C
      RETURN
C *  THIS PROGRAM VALID ON FTN4 AND FTN5 **
      END
C
C
C
      SUBROUTINE EXTMAT( ASET, NX, NY, NSET, ISET, AMAT)
C     ===========================================================
C
C
      DIMENSION ASET( NX, NY, NSET), AMAT( NX, NY)
C
C
      DO 999 I = 1, NX
        DO 998 J = 1, NY
          AMAT( I, J) = ASET( I, J, ISET)
998     CONTINUE
999     CONTINUE
C
C
      RETURN
C
C
      END
C
C
C
      SUBROUTINE EXTVEC( VSET, NX, NSET, ISET, VEC)
C     ======================================================
C
C
      DIMENSION VSET( NX, NSET), VEC( NX)
C
C
      DO 999 I = 1, NX
        VEC( I) = VSET( I, ISET)
999   CONTINUE
C
C
      RETURN
C
C
      END
      SUBROUTINE FACTOR(FCT)                                            
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C CHANGE THE SIZE OF THE ENTIRE PLOT                                    
C                                                                       
C   PARAMETERS :                                                        
C                                                                       
C   FCT = THE CURRENT PLOTTING FACTOR                                   
C                                                                       
C                                                                       
C *******************************************************************   
C                                                                       
C *** COMMON BLOCK                                                      
C                                                                       
      COMMON /HP/ XMINPU, YMINPU, XORG  , YORG  , XOFF  , YOFF    ,     
     +            XFACT , YFACT , XCNTRL, YCNTRL, XYUNIT          ,     
     +            MDEV  , MUNIT , MPENS , MLU   , MCNTRL, MINIT   ,     
     +            MSHAKE, MPTR  , MCODE , MPLOT , MFLUSH, MSIZE   ,     
     +            MSPOOL, MVER  , MOUT(72),                             
     +            RSI1  , RSI2  , RDI1   , RDI2  , RES1  , RES2         
C                                                                       
C     ***** XYUNIT - CONTAINS THE CURRENT NUMBER OF PLOTTER UNITS PER   
C                    INCH OR CENTIMETER                                 
C                                                                       
C     ***** MUNIT  - CONTAINS THE CODE FOR THE UNIT SYSTEM ENABLED      
C                                                                       
C *****************************************************************     
C                                                                       
C                                                                       
C**** CHECK FOR A VALID PLOT SIZE FACTOR                                
C                                                                       
      FCTR=FCT                                                          
      IF (FCTR .LE. 0.0 .OR. FCTR .GT. 32767.0) FCTR=1.0                
C                                                                       
C**** COMPUTE PLOTTER UNITS/INCH IF UNIT SYSTEM IS INCHES               
C                                                                       
      IF (MUNIT .EQ. 0) XYUNIT=FCTR*1016.0                              
C                                                                       
C**** COMPUTE PLOTTER UNITS/CENTIMETER IF UNIT SYSTEM IS METRIC         
C                                                                       
      IF (MUNIT .EQ. 1) XYUNIT=FCTR*400.0                               
      RETURN                                                            
      END                                                               

      SUBROUTINE FACTR(SC)
COPYRIGHT (C) CERRITOS COMPUTER SERVICES, INC. 1976 
C 
C     RESET THE SCALE FACTOR TO SC*CURRENT SCALE FACTOR.
C     SC=1.0 FORCES THE SCALE FACTOR BACK TO 1.0
C 
      COMMON/PXCOM/LU,MP,MB(128),RE(2),OX,OY,SF,PX,PY 
      IF(SC.GT.0.)SF=SC*SF
      IF(SC.EQ.1.)SF=1. 
      RETURN
      END 

      SUBROUTINE FNAME(N,NDIGIT,FILPRE,FILEXT,FILNAM)
C
C        Construct a file name of the form:
C           FILNAM = FILPRE + N + FILEXT
C
C     Parameter type contents
C       N         I  The sequence number to be inserted in the file name
C       NDIGIT    I  The number of digits of N to be converted
C       FILPRE    C  The file prefix string
C       FILEXT    C  The file extension string
C       FILNAM    C  The resulting file name string
C
C        L. Weissman Nov 1984
C
      CHARACTER*(*) FILPRE,FILEXT,FILNAM
      CHARACTER C,CHAR
      LOGICAL LLE
C
C        Determine string lengths
C
      L1=LEN(FILPRE)
      L2=LEN(FILEXT)
      L3=LEN(FILNAM)
      FILNAM=' '
C
C        Move the prefix to the file name
C
      L=0
      IF(L1 .LT. 1)GO TO 20
      DO 10 I=1,L1
      C=FILPRE(I:I)
      IF(LLE(C,' '))GO TO 20
      IF(L .GE. L3)GO TO 60
      L=L+1
10    FILNAM(L:L)=C
C
C        Move N to the file name
C
20    IF(NDIGIT .LT. 1)GO TO 40
      J=10**NDIGIT
      NN=MOD(IABS(N),J)
      DO 30 I=1,NDIGIT
      J=J/10
      C=CHAR(NN/J + 48)
      NN=MOD(NN,J)
      IF(L .GE. L3)GO TO 60
      L=L+1
30    FILNAM(L:L)=C
C
C        Move the prefix to the file name
C
40    IF(L2 .LT. 1)GO TO 60
      DO 50 I=1,L2
      C=FILEXT(I:I)
      IF(LLE(C,' '))GO TO 60
      IF(L .GE. L3)GO TO 60
      L=L+1
50    FILNAM(L:L)=C
60    RETURN
      END
      FUNCTION FNIP(N, A, XN, X)
      DIMENSION XN(N), A(1)
C N IS DEGREE OF THE POLYNOMIAL.
      I = N + 1
      S = A(I)
   10 I = I - 1
      IF (I.LT.1) GO TO 20
      S = S*(X-XN(I)) + A(I)
      GO TO 10
   20 FNIP = S
      RETURN
C *  THIS PROGRAM VALID ON FTN4 AND FTN5 **
      END
      SUBROUTINE FPINIT(LIO,LMON,LLONG,LDEV,LFILE)                      
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C INITIALIZE THE SOFTWARE AND THE PLOTTER                               
C                                                                       
C   PARAMETERS :                                                        
C                                                                       
C   LIO      = HANDSHAKE METHOD DESIRED : 3 = NO HANDSHAKING            
C                                         0 = ENQUIRE / ACKNOWLEDGE     
C                                         1 = XON / XOFF                
C                                         2 = HP-IB                     
C                                                                       
C              NOTE : IF THE VALUE OF -LIO- IS NEGATIVE, THE INITIAL-   
C                     IZATION OF SYSTEM CONFIGURATION PARAMETERS WILL   
C                     BE SUPPRESSED.                                    
C                                                                       
C                                                                       
C   LMON     = MONITOR MODE 0= OFF, 1= ON                               
C           PAPER PARAM IF IN DEFERRED MODE                             
C                                                                       
C                                                                       
C   LLONG    = LONG-AXIS PLOT  0= NORMAL, 1= LONG PLOT                  
C                                                                       
C                                                                       
C   LDEV     = LOGICAL UNIT OF PLOTTER (OR NEW PLOT FILE)               
C                                                                       
C                                                                       
C   LFILE    = LOGICAL UNIT OF THE PLOT FILE                            
C                                                                       
C                                                                       
C *****************************************************************     
C                                                                       
C *** COMMON BLOCK                                                      
C                                                                       
      COMMON /HP/ XMINPU, YMINPU, XORG  , YORG  , XOFF  , YOFF    ,     
     +            XFACT , YFACT , XCNTRL, YCNTRL, XYUNIT          ,     
     +            MDEV  , MUNIT , MPENS , MLU   , MCNTRL, MINIT   ,     
     +            MSHAKE, MPTR  , MCODE , MPLOT , MFLUSH, MSIZE   ,     
     +            MSPOOL, MVER  , MOUT(72),                             
     +            RSI1  , RSI2  , RDI1   , RDI2  , RES1  , RES2         
C                                                                       
C                                                                       
C *****************************************************************     
C                                                                       
C *** COMMON BLOCK                                                      
C                                                                       
      COMMON/ZZCOM/MZBITS,MZCHAR,MZWORD,MZJUST,MZCCSW,MZCCTL,MZDCCS,    
     +MZDCCT,MZHISW,MZLUSW,MZLURD,MZLUWR,                               
     +MZTAD(2),MZOTC(2),MZETC(2),MZOT1(2),MZOT2(2),                     
     +MZHM(2),MZBS(2),MZHEC(2),MZHS1(2),MZHS2(2),MZICD(2),MZIRS1(2),    
     +MZIRS2(2),MZMBS(2),MZSCOC(2)                                      
C                                                                       
C *****************************************************************     
      DIMENSION LBUFF(73),IA(8),XBUFF(4)                                
      DATA IA(1)/70/, IA(2)/82/, IA(3)/79/, IA(4)/80/                   
C                F          R         O          P                      
      DATA IA(5)/73/, IA(6)/80/, IA(7)/83/, IA(8)/67/                   
C                I          P           S         C                     
C                                                                       
C**** MAKE SURE USER ISN'T TRYING TO FOOL US WITH LIO=10                
C                                                                       
      MIO=MOD(IABS(LIO),10)                                             
C                                                                       
C**** NOW INITIALIZE SYSTEM AND GET MDEV AND MVER #'S                   
C**** IF A 7586B BUT IT HAS NO ROLL PAPER LOADED, HPINIT WILL           
C**** CHANGE IT TO A 7585B AS FAR AS WE ARE CONCERNED.                  
C                                                                       
      IF(LLONG.LT.0) GO TO 10                                           
      CALL HPINIT(MIO,LMON,0,0,LDEV)                                    
      GO TO 20                                                          
  10  CALL DPINIT(MIO,7586,66,0,0,0,LDEV)                               
  20  CONTINUE                                                          
C                                                                       
C**** ALTER FILE NUMBER IF NEEDED                                       
C**** THIS IS DETERMINED AT INSTALLATION                                
C                                                                       
      MFILE=LFILE                                                       
      IF (MZLUSW.EQ.2) MFILE=MFILE+MZLURD                               
C                                                                       
C**** LLONG IS WHETHER OR NOT A LONG-AXIS PLOT                          
C                                                                       
      LONG=LLONG                                                        
C                                                                       
C**** MAKE LONG=0 IF NOT ROLL FEED -- 7586B                             
C                                                                       
      IF (MDEV.NE.7586) LONG=0                                          
C                                                                       
C**** INITIALIZE NUMBER OF READ-WRITE ITERATIONS                        
C                                                                       
      NREADS=1                                                          
C                                                                       
C**** IS THIS A LONG-AXIS PLOT?                                         
C                                                                       
      IF (LONG.EQ.0) GO TO 100                                          
C                                                                       
C**** YES, THEN READ FILE ONCE TO GET BOUNDRIES,                        
C**** SET UP PLOTTER,                                                   
C**** AND SET NREADS TO PROPER NUMBER OF FRAMES TO USE                  
C                                                                       
C                                                                       
      XMAX=-10000.0                                                     
      YMAX=-10000.0                                                     
      XMIN=10000.0                                                      
      YMIN=10000.0                                                      
      IPSTAT=0                                                          
C                                                                       
C**** NOW REWIND FILE                                                   
C**** MAKE SURE THAT LFILE IS OFFSET BY MZLURD                          
C                                                                       
      REWIND MFILE                                                      
 1010 CONTINUE                                                          
C                                                                       
C**** NOW GET A RECORD                                                  
C                                                                       
      LAST = -1                                                         
C  THIS IS FLAG FOR ZZGET                                               
      CALL ZZGET(LFILE,LBUFF,73,LAST)                                   
      ICNT=1                                                            
C                                                                       
C****    REMOVE TRAILING BLANKS                                         
C                                                                       
 1160 CONTINUE                                                          
      IF (LBUFF(LAST-1).NE.32) GO TO 1165                               
      LAST=LAST-1                                                       
      GO TO 1160                                                        
C                                                                       
C****    DETERMINE IF END OF FILE  AND BRANCH                           
C                                                                       
 1165 CONTINUE                                                          
      IF (LAST.NE.4) GO TO 1175                                         
      IF (LBUFF(1).NE.80) GO TO 1175                                    
      IF (LBUFF(2).NE.85) GO TO 1175                                    
      IF (LBUFF(3).NE.59) GO TO 1175                                    
      GO TO 1200                                                        
C                                                                       
C**** NOW, IF IN PA STATE, THEN GO TO DECODE SECTION                    
C                                                                       
 1175 CONTINUE                                                          
      IF (IPSTAT.NE.0) GO TO 1030                                       
 1020 CONTINUE                                                          
      IF (ICNT.GE.LAST-1) GO TO 1010                                    
      IF (LBUFF(ICNT).EQ.80) GO TO 1022                                 
      ICNT=ICNT+1                                                       
      GO TO 1020                                                        
 1022 CONTINUE                                                          
      IF (LBUFF(ICNT+1).EQ.65) GO TO 1025                               
      ICNT=ICNT+1                                                       
      GO TO 1020                                                        
C                                                                       
C**** PA COMMAND HAS BEEN FOUND, NOW LOOK FOR X-COORD                   
C                                                                       
 1025 IPSTAT=1                                                          
      IXSTAT=1                                                          
      ICNT=ICNT+2                                                       
      IF (ICNT.GE.LAST-1) GO TO 1010                                    
C                                                                       
C**** HERE WE ARE IN PA COMMAND NOW DECODE COORD.                       
C                                                                       
 1030 CONTINUE                                                          
      WORK=0.0                                                          
      MINUS=0                                                           
C     USED FOR NEGATIVE CONTANTS                                        
C                                                                       
C****    IS FIRST CHAR A MINUS SIGN?                                    
C                                                                       
      IF(LBUFF(ICNT).NE.45) GO TO 1040                                  
C                                                                       
C**** YES, NOW SET FLAG                                                 
C                                                                       
      MINUS=1                                                           
      ICNT=ICNT+1                                                       
 1040 CONTINUE                                                          
      IF(LBUFF(ICNT).LT.48.OR.LBUFF(ICNT).GT.57) GO TO 1045             
C                                                                       
C**** FOUND DIGIT, NOW UPDATE WORK                                      
C                                                                       
      WORK=WORK*10.0+FLOAT(LBUFF(ICNT))-48.0                            
      ICNT=ICNT+1                                                       
      GO TO 1040                                                        
 1045 CONTINUE                                                          
C                                                                       
C**** CHECK FOR MINUS SIGN                                              
C                                                                       
      IF (MINUS.EQ.1) WORK= -WORK                                       
C                                                                       
C**** IS THIS AN X-COORD                                                
C                                                                       
      IF (IXSTAT.NE.1) GO TO 1047                                       
C                                                                       
C**** YES, NOW CHECK AGAINST MAX AND MIN                                
C                                                                       
      IXSTAT=0                                                          
      IF(WORK.GT.XMAX) XMAX=WORK                                        
      IF(WORK.LT.XMIN) XMIN=WORK                                        
      GO TO 1049                                                        
C                                                                       
C**** THIS IS Y-COORD, NOW CHECK IT FOR MIN AND MAX                     
C                                                                       
 1047 CONTINUE                                                          
      IXSTAT=1                                                          
      IF(WORK.GT.YMAX) YMAX=WORK                                        
      IF(WORK.LT.YMIN) YMIN=WORK                                        
C                                                                       
C**** NOW SEE IF END OF PA COMMAND                                      
C**** THAT IS, HAVE WE REACHED A SEMI-COLON                             
C                                                                       
 1049 CONTINUE                                                          
      IF (LBUFF(ICNT).EQ.59) GO TO 1050                                 
      IF (ICNT.GE.LAST-1) GO TO 1010                                    
      ICNT=ICNT+1                                                       
      GO TO 1030                                                        
 1050 CONTINUE                                                          
      IPSTAT=0                                                          
      GO TO 1020                                                        
C                                                                       
C**** NOW, USE NUMBERS TO SETUP 7586B FOR LONG PLOT                     
C                                                                       
 1200 CONTINUE                                                          
C                                                                       
C**** OBTAIN THE PLOTTER X,Y MIN-MAX VALUES                             
C                                                                       
      P1X=XMINPU                                                        
      P1Y=YMINPU                                                        
      P2X=-P1X                                                          
      P2Y=-P1Y                                                          
      XTOT=XMAX-XMIN                                                    
      YTOT=YMAX-YMIN                                                    
C                                                                       
C**** NOW DETERMINE NUMBER OF FRAMES NEEDED                             
C                                                                       
      NREADS=INT(XTOT/(P2X-P1X)+1.0)                                    
C                                                                       
C**** NOW X-FORM P1 AND P2 FOR NEW PLOT                                 
C                                                                       
      P1X=P2X-XTOT                                                      
C     P1Y=P1Y                                                           
C     P2X=P2X                                                           
      P2Y=P1Y+YTOT                                                      
C                                                                       
C**** NOW OUTPUT THEM TO PLOTTER                                        
C                                                                       
      CALL BUFF(1,IA(5),XBUFF(1),2)                                     
      XBUFF(1)=P1X                                                      
      XBUFF(2)=P1Y                                                      
      XBUFF(3)=P2X                                                      
      XBUFF(4)=P2Y                                                      
      CALL BUFF(6,LBUFF(1),XBUFF(1),4)                                  
C                                                                       
C****  NOW SCALE WITH SC COMMAND                                        
C                                                                       
      CALL BUFF(1,IA(7),XBUFF(1),2)                                     
      XBUFF(1)=XMIN                                                     
      XBUFF(2)=XMAX                                                     
      XBUFF(3)=YMIN                                                     
      XBUFF(4)=YMAX                                                     
      CALL BUFF(6,LBUFF(1),XBUFF(1),4)                                  
C                                                                       
C**** NOW CONTINUE WITH PLOTTING                                        
C                                                                       
  100 CONTINUE                                                          
C                                                                       
C**** NOW  REWIND AND READ AND WRITE FILE                               
C                                                                       
  125 CONTINUE                                                          
      REWIND MFILE                                                      
C                                                                       
C**** GET RECORD                                                        
C                                                                       
  150 CONTINUE                                                          
      LAST = -1                                                         
C  THIS IS FLAG FOR ZZGET                                               
      CALL ZZGET(LFILE,LBUFF,73,LAST)                                   
C                                                                       
C**** DELETE TRAILING BLANKS                                            
C                                                                       
  160 CONTINUE                                                          
      IF (LBUFF(LAST-1).NE.32) GO TO 165                                
      LAST=LAST-1                                                       
      GO TO 160                                                         
C                                                                       
C**** DETERMINE IF END OF FILE ( LINE= PU; ), TO GO TO NEXT SECTION     
C                                                                       
C**** IF (LBUFF =  PU;  ) GO TO 200                                     
C                                                                       
  165 CONTINUE                                                          
      IF (LAST.NE.4) GO TO 175                                          
      IF (LBUFF(1).NE.80) GO TO 175                                     
      IF (LBUFF(2).NE.85) GO TO 175                                     
      IF (LBUFF(3).NE.59) GO TO 175                                     
      GO TO 200                                                         
C                                                                       
C**** NOW OUTPUT THE RECORD                                             
C                                                                       
  175 CONTINUE                                                          
      CALL BUFF(1,LBUFF(1),XBUFF(1),LAST-1)                             
      GO TO 150                                                         
C                                                                       
C**** NOW DO FRAME IF LONG PLOT                                         
C                                                                       
  200 CONTINUE                                                          
      NREADS=NREADS-1                                                   
      IF (NREADS.LE.0) GO TO 300                                        
C                                                                       
C**** DO FRAME ADVANCE     FR;                                          
C                                                                       
      CALL BUFF(2,IA(1),XBUFF(1),-2)                                    
      GO TO 125                                                         
C                                                                       
C**** FINISH UP                                                         
C                                                                       
  300 CONTINUE                                                          
      CALL PLOT(0.0,0.0,999)                                            
      RETURN                                                            
      END                                                               
      SUBROUTINE FRAM (IPEN,XSIZE,YSIZE,XMAX,YMAX,RECIP,FCT) 
C 
C 
C.....PLOTS A FRAME 
C 
C 
      CHARACTER*8 TITLE
      INTEGER LARAY(4)
      EQUIVALENCE (LARAY(1),TITLE) 
      LOGICAL RECIP, DEBUG, NEWX, NEWY
      COMMON /SCALE/ XMIN, YMIN, XSCALE, YSCALE, DELTAX, DELTAY,
     + XPOS, YPOS, XM, YM, TZ, DEBUG, DASHL, ISPEN, ISYMB, SSIZE
      COMMON /STEPS/ SMARKX, SMARKY, OFFSET
      COMMON /OLDIES/ OLDXMN, OLDXMX, OLDYMN, OLDYMX
C 
C 
C 
C.....INITIALIZE PLOTTER
      IF (DELTAX.EQ.0.0) THEN
          IF (.NOT.DEBUG) CALL HPINIT(0,0,1,1,6) 
          IF (.NOT.DEBUG) CALL NEWPEN (ABS(IPEN))
          IF (.NOT.DEBUG) CALL FACTOR (FCT)
C.....NEW ORIGIN
          IF (.NOT.DEBUG) CALL PLOT (3.0,3.0,-3)
      ENDIF
C 
C 
C.....FRAME PLOTTING
      IF (.NOT.DEBUG) CALL NEWPEN(ABS(IPEN))
      IF (.NOT.DEBUG) CALL PLOT (XSIZE,0.0,2) 
      IF (.NOT.DEBUG) CALL PLOT (XSIZE,YSIZE,2) 
      IF (.NOT.DEBUG) CALL PLOT (0.0,YSIZE,2) 
      IF (.NOT.DEBUG) CALL PLOT (0.0,0.0,2) 
C 
C 
C.....TEST IF SCALE NUMBERS ARE REQUIRED
      IF (DELTAX.NE.0.0.AND.XMIN.EQ.0.0.AND.XMAX.EQ.0.0) THEN
          NEWX=.FALSE.
          XMIN=OLDXMN
          XMAX=OLDXMX
      ELSE
          NEWX=.TRUE.
          OLDXMX=XMAX
          OLDXMN=XMIN
      ENDIF
C
C
C.....SCHALING X-AXIS 
      IF (RECIP) THEN
        NSX=NINT(XSIZE/2.0) 
        DELTAX=(1/XMAX)**2 - (1/XMIN)**2
        XSCALE=XSIZE/DELTAX 
        STEPX=DELTAX/NSX
C 
C 
C.....PLOT DIVISIONS X-AXIS PROPORTIONAL TO (1/X)**2
        XMARK=0.0
        DO 10, I=1,NSX
            XP=REAL(I)/REAL(NSX) * XSIZE 
            IF (.NOT.DEBUG) CALL PLOT (XP,0.0,3)
            IF (.NOT.DEBUG) CALL PLOT (XP,0.5*TZ,2)
            XMARK=SQRT(1/( (1/XMIN)**2 + I*STEPX )) 
            IF (XMARK.LE.100) THEN
                WRITE (TITLE,'(F5.2)') XMARK
            ELSE
                IMARK=NINT(XMARK)
                WRITE (TITLE,'(I5)') XMARK
            ENDIF
            IF (.NOT.DEBUG.AND.NEWX) CALL SYMBOL (XP-TZ*2.5,-1.5*TZ,
     +                                 TZ,LARAY,0.0,5)
   10   CONTINUE
      ELSE
C 
C.....PLOT LINEAR DIVISIONS X-AXIS
        DELTAX=XMAX-XMIN
        XSCALE=XSIZE/DELTAX 
        IF (SMARKX.NE.0.0) THEN
           NSX   = INT(DELTAX/SMARKX)
           STEPX = SMARKX * XSIZE / DELTAX
        ELSE
          CALL DECENT (DELTAX,XSIZE,NSX,STEPX,2.0)
          SMARKX=DELTAX/XSIZE * STEPX
        ENDIF
        DEL=SMARKX
        RMIN=XMIN
        RMAX=XMAX
        CALL CMODE (DEL,RMIN,RMAX,STEPX,CENTER,RM,MODE)
C 
        DO 15, I=0,NSX
            XP=I*STEPX
            IF (.NOT.DEBUG) CALL PLOT (XP,0.0,3)
            IF (.NOT.DEBUG) CALL PLOT (XP,0.5*TZ,2)
            XMARK=XMIN + I*SMARKX
            IF (MODE.EQ.1) WRITE (TITLE,'(E8.1)') XMARK
            IF (MODE.EQ.2) THEN
                IMARK=NINT(XMARK)
                WRITE (TITLE,'(I8)') IMARK
            ENDIF
            IF (MODE.EQ.3) WRITE (TITLE,'(F8.1)') XMARK
            IF (MODE.EQ.4) WRITE (TITLE,'(F8.2)') XMARK
            IF (MODE.EQ.5) WRITE (TITLE,'(F8.3)') XMARK
            IF (.NOT.DEBUG) THEN
C
C.....POSITION FIRST X MARK
                IF (I.EQ.0) THEN
                    RMDUM=RM
   12               IF (TITLE(1:1).EQ.' ') THEN
                       RMDUM=RMDUM-TZ
                       TITLE(1:7)=TITLE(2:8)
                       TITLE(8:8)=' '
                       GOTO 12
                    ENDIF   
                    IF (.NOT.DEBUG.AND.NEWX) CALL SYMBOL
     +              (XP,-1.5*TZ,TZ,LARAY,0.0,8)
                    LASTX=XP+RMDUM+TZ
                ELSE
                    IF (LASTX.LE.XP-CENTER+8*TZ-RM) THEN
                      IF (.NOT.DEBUG.AND.NEWX) CALL SYMBOL 
     +                   (XP-CENTER,-1.5*TZ,TZ,LARAY,0.0,8)
                      LASTX=XP-CENTER+9*TZ
                    ENDIF
                ENDIF
            ENDIF 
   15   CONTINUE
      ENDIF
C
C 
C.....TEST IF SCALE NUMBERS ARE REQUIRED
      IF (DELTAY.NE.0.0.AND.YMIN.EQ.0.0.AND.YMAX.EQ.0.0) THEN
          NEWY=.FALSE.
          YMIN=OLDYMN
          YMAX=OLDYMX
      ELSE
          NEWY=.TRUE.
          OLDYMN=YMIN
          OLDYMX=YMAX
      ENDIF
C
C 
C.....SCHALING Y-AXIS 
      DELTAY=YMAX-YMIN
      YSCALE=YSIZE/DELTAY
      IF (SMARKY.NE.0.0) THEN
         NSY   = INT(DELTAY/SMARKY)
         STEPY = SMARKY * YSIZE / DELTAY
      ELSE
        CALL DECENT (DELTAY,YSIZE,NSY,STEPY,1.0,MODE)
        SMARKY=DELTAY/YSIZE * STEPY
      ENDIF
      DEL=SMARKY
      RMIN=YMIN
      RMAX=YMAX
      CALL CMODE (DEL,RMIN,RMAX,STEPY,CENTER,RM,MODE)
C 
C.....PLOT DIVISIONS Y-AXIS
      DO 20, I=0,NSY
          YP=I*STEPY
          IF (.NOT.DEBUG) CALL PLOT (0.0,YP,3)
          IF (.NOT.DEBUG) CALL PLOT (0.5*TZ,YP,2)
          IF (OFFSET.NE.0.0) GOTO 20
          YMARK=YMIN + I*SMARKY
          IF (MODE.EQ.1) WRITE (TITLE,'(E8.1)') YMARK
          IF (MODE.EQ.2) THEN
              IMARK=NINT(YMARK)
              WRITE (TITLE,'(I8)') IMARK
          ENDIF
          IF (MODE.EQ.3) WRITE (TITLE,'(F8.1)') YMARK
          IF (MODE.EQ.4) WRITE (TITLE,'(F8.2)') YMARK
          IF (MODE.EQ.5) WRITE (TITLE,'(F8.3)') YMARK
          IF (.NOT.DEBUG.AND.NEWY) CALL SYMBOL (-8.0*TZ,
     +                      YP-TZ/2.0,TZ,LARAY,0.0,8)
   20 CONTINUE
C
C.....FIND POSITIONS FOR TITLES FOR AXES
      XM=-RM-0.6*TZ
      YM=-2.0*TZ
C
C.....FIND POSITION FOR TITLE BLOCK
      IF (XPOS.EQ.0.AND.YPOS.EQ.0) THEN
       XPOS=XSIZE/3.0
       YPOS=YSIZE-3.0*TZ
      ENDIF
      RETURN
      END 

      SUBROUTINE FRAME(VERTEX,MASK) 
C 
C        ROUTINE TO PLOT A FRAME ON THE PROJECTION OF A 
C        3-DIMENSIONAL FIGURE AS DRAWN BY PLOT3D. 
C        INPUT PARAMETERS:
C           VERTEX  ARRAY CONTAINING THE COORDINATES OF THE 
C                   VERTICES OF THIS FIGURE AS RETURNED FROM
C                   PLOT3D ON THE LAST CALL.
C           MASK    ARRAY CONTAINING THE MASK FOR THIS FIGURE 
C                   AS RETURNED BY PLOT3D ON THE LAST CALL. 
C        THE VERTICES OF THE FRAME ARE NUMBERED (1-4) IN THE SAME 
C        ORDER AS THEIR COORDINATES APPEAR IN VERTEX. 
C        THE MASK ARRAY IS ALTERED BY THIS ROUTINE, 
C        BUT THE PLOTTER ORIGIN IS NOT MOVED. 
C 
      DIMENSION VERTEX(1),MASK(1),ARRAY(14) 
      I=2.0*VERTEX(17)
      I=MAX0(I,2) 
      I=MIN0(I,8) 
C 
C        THE VERTICES WHICH MAY BE HIDDEN ARE DRAWN BY PLOT3D 
C 
      ARRAY(1)=VERTEX(I-1)
      ARRAY(8)=VERTEX(I)
      ARRAY(2)=VERTEX(I+7)
      ARRAY(9)=VERTEX(I+8)
      ARRAY(4)=ARRAY(2) 
      ARRAY(11)=ARRAY(9)
      ARRAY(6)=ARRAY(2) 
      ARRAY(13)=ARRAY(9)
      ARRAY(7)=ARRAY(1) 
      ARRAY(14)=ARRAY(8)
      I=I-2 
      IF(I .EQ. 0)I=8 
      ARRAY(3)=VERTEX(I+7)
      ARRAY(10)=VERTEX(I+8) 
      I=I+4 
      IF(I .GT. 8)I=I-8 
      ARRAY(5)=VERTEX(I+7)
      ARRAY(12)=VERTEX(I+8) 
      CALL PLT3D(110,ARRAY,ARRAY(8),0.0,1.0,1.0,0.0,
     1   2,7,0.0,0.0,0.0,0.0,0.0,MASK,VERTEX) 
C 
C        THE REMAINING VERTICES ARE DRAWN BY CALLS TO PLOT. 
C 
      CALL PLOT(VERTEX(I-1),VERTEX(I),3)
      I=I-2 
      DO 10 J=1,3 
      I=I+2 
      IF(I .EQ. 10)I=2
      CALL PLOT(VERTEX(I+7),VERTEX(I+8),2)
10    CONTINUE
      CALL PLOT(VERTEX(I-1),VERTEX(I),2)
      I=I-2 
      IF(I .EQ. 0)I=8 
      CALL PLOT(VERTEX(I-1),VERTEX(I),3)
      CALL PLOT(VERTEX(I+7),VERTEX(I+8),2)
      RETURN
      END 

      SUBROUTINE FREFRM(LINE,NDIM1,RVARS,NDIM2,N)
C
C     This subroutine is used to convert an ascii character string
C     containing numeric fields separated by delimiters to floating
C     point numbers.
C
C     Parameters-
C        LINE:  An array containing the ascii characters
C        NDIM1: The number of characters in LINE
C        RVARS: The returned variables.
C        NDIM2: The maximum number of returned variables.
C        N:     The actual number of floating point fields found.
C               This number will not exceed NDIM2 or the dimension
C               of local arrays JBEG and JFIN.
C               If set to -1, an input parameter was in error.
C
C     Routines needed:
C        PARSE    Break the input line into fields
C        GETNUM   Interpret the fields as numeric values
C
C     Notes:
C        1) Delimiters are of the following types:
C           a) "Hard" delimiters are comma(,),semicolon(;),colon(:),
C               slash(/)
C           b) "Soft" delimiters are blank and tab
C           c) "Terminators" are exclamation point(!),pound(#) and null
C        2) Any type of delimiter separates fields, but only multiple
C           hard delimiters are interpreted as defaulted fields.
C           Characters beyond a terminator are ignored.
C        3) LINE,NDIM1,NDIM2 are not changed by this routine.
C        4) Unspecified values of RVARS are not changed in this version.
C
C        L. Weissman
C        Molecular Biology Institute
C        405 Hilgard Avenue
C        Los Angeles, California 90024
C        U.S.A.
C
      BYTE LINE(*)
      DIMENSION JBEG(30),JFIN(30),RVARS(*)
C
C        Error return if either NDIM1 or NDIM2 <= 0
C
      N=-1
      IF(MIN0(NDIM1,NDIM2) .LE. 0)GO TO 20
      N=0
C
C        Zero RVARS. If this code is commented, this version uses
C        pre-loaded values of RVARS.
C
C--   DO 5 I=1,NDIM2
C--5  RVARS(I)=0.0
C
C        Parse out the fields of the input line
C
      NDIMJ=MIN0(NDIM2,30)
      CALL PARSE(LINE,NDIM1,JBEG,JFIN,NDIMJ,N)
C
C        Interpret the fields as numeric values
C
      IF(N .LT. 1)GO TO 20
      DO 10 J=1,N
      CALL GETNUM(LINE,JBEG(J),JFIN(J),RVARS(J),IERR)
10    CONTINUE
20    RETURN
      END
C
C
C      
      SUBROUTINE GENTRIGTAB
C     ======================
C
C
      COMMON /TRIGTAB/ AMULTIPLIER, COSTAB( 0:9000), SINTAB( 0:9000)
C
C
      MAXARG = 90. * AMULTIPLIER
C
C
      DO 999 I = 0, MAXARG
        ARG = FLOAT( I) / AMULTIPLIER
        COSTAB( I) = COSD( ARG)
        SINTAB( I) = SIND( ARG)
999     CONTINUE
C
C
      RETURN
C
C
      END
C
C
C
      FUNCTION GETCOSD( ARG )
C     ========================
C
C
      COMMON /TRIGTAB/ AMULTIPLIER, COSTAB( 0:9000), SINTAB( 0:9000)
C
C
      A = ARG
      IF ( A .LT. 0. )  A = -A
10    CONTINUE
C
C
      IF ( A .GE. 360. ) THEN
        A = A - 360.
      GOTO 10
      END IF
C
C
      IF ( A .GT. 180. )  A = 360. - A
C
C
      IF ( A .GT. 90. )  THEN
        A = 180. - A
        ASIGN = -1.
      ELSE
        ASIGN = 1.
      END IF
C
C
      IA = INT( A * AMULTIPLIER + .5 )
      GETCOSD = ASIGN * COSTAB( IA )
C
C
      RETURN
C
C
      END

      SUBROUTINE GETNUM(BUFFER,IST,IFIN,RESULT,IERR)
C
C        Extract a floating point number from a string.
C
C        Numbers may be entered with a decimal point and sign.
C        Base 10 exponentiation in the FORTRAN convention is also
C        permitted. Examples:
C                 1.73e-2       read as 0.0173
C                   -E2         read as -100.0
C
C     Parameters:
C        BUFFER   contains the string to be processed
C        IST      The starting byte for the scan.
C        IFIN     The finishing byte for the scan.
C        RESULT   the returned numeric result (floating point)
C        IERR     zero if no error occurred.
C                 negative if an error occurred.
C
C     Local variables:
C        F        fractional part
C        M        multiplier for fractional part (base 10 log)
C        S        sign of fractional part
C        E        exponent
C        ES       sign of exponent
C        MODE     -1 if accumulating left of the decimal point
C                  0 if accumulating right of the decimal point
C                 +1 if accumulating the exponent
C
C     Notes:
C        1) If a severe error occurs or if NDIM is not positive,
C           RESULT is returned unchanged. Minor errors, such as
C           uninterpretable characters, are ignored.
C        2) This version does not check for overflows.
C
C        L. Weissman
C        Molecular Biology Institute
C        405 Hilgard Avenue
C        Los Angeles, California 90024
C        U.S.A.
C
      INTEGER E,ES
      BYTE BUFFER(*),MASK,ZERO,NINE,DOT,PLUS,MINUS,E1,E2,D1,D2,C
      DATA MASK,ZERO,NINE,DOT,PLUS,MINUS, E1, E2, D1, D2/
     1      127, '0', '9','.','+',   '-','E','e','D','d'/
C
C        Consistency check
C
      IERR=-1
      IF(MIN0(IST,IFIN,IFIN-IST+1) .LT. 0)GO TO 170
      IERR=0
C
C        Starting values of local variables
C
      MODE=-1
      F=0.0
      M=0
      S=1.0
      E=0
      ES=1
C
C        Begin character loop
C
      DO 160 I=IST,IFIN
      C=IBYTE(BUFFER,I)
C
C        Process digits
C
      IF(C .LT. ZERO .OR. C .GT. NINE)GO TO 40
      C=C-ZERO
      IF(MODE)20,10,30
10    M=M-1
20    F=10.0*F+C
      GO TO 160
30    E=10*E+C
      GO TO 160
C
C        Process "+"
C
40    IF(C .NE. PLUS)GO TO 70
      IF(MODE)50,50,60
50    S=1.0
      GO TO 160
60    ES=1
      GO TO 160
C
C        Process "-"
C
70    IF(C .NE. MINUS)GO TO 100
      IF(MODE)80,80,90
80    S=-1.0
      GO TO 160
90    ES=-1
      GO TO 160
C
C        Process E,e,D,d
C
100   IF(C .NE. E1 .AND. C .NE. E2 .AND.
     1   C .NE. D1 .AND. C .NE. D2)GO TO 130
      IF(MODE)110,110,120
110   MODE=1
      IF(F .EQ. 0.0)F=1.0
      GO TO 160
120   IERR=-1
      GO TO 170
C
C        Process "."
C
130   IF(C .NE. DOT)GO TO 180
      IF(MODE)140,150,150
140   MODE=0
      GO TO 160
150   IERR=-2
      GO TO 170
160   CONTINUE
C
C        End of character loop. Form the result.
C
      RESULT=S*F*(10.0**(M+ES*E))
170   RETURN
180   IERR=-3
      RETURN
      END
C
C
C
      FUNCTION GETSIND( ARG )
C     ========================
C
C
      COMMON /TRIGTAB/ AMULTIPLIER, COSTAB( 0:9000), SINTAB( 0:9000)
C
C
      A = ARG
C
C
      IF ( A .LT. 0. )  THEN
        A = -A
        ASIGN = -1.
      ELSE
        ASIGN = 1.
      END IF
C
10     CONTINUE
C
      IF ( A .GE. 360. ) THEN
        A = A - 360.
        GOTO 10
        END IF
C
C
      IF ( A .GT. 180. )  THEN
        A = 360. - A
        ASIGN = -1. * ASIGN
      END IF
C
C
      IF ( A .GT. 90. )  A = 180. - A
      IA = INT( A * AMULTIPLIER + .5 )
      GETSIND = ASIGN * SINTAB( IA )
C
C
      RETURN
C
C
      END

      SUBROUTINE GRAPH(X,Y,N) 
C 
C        ROUTINE PRINTER PLOT USING CALCOMP CALLS 
C 
C     L. WEISSMAN                                  SEPT 1980
C     DEPARTMENT OF PHYSICS, UNIV. OF VIRGINIA
C     MCCORMICK ROAD
C     CHARLOTTESVILLE, VA. 22901
C 
      REAL X(1),Y(1),XT(2),YT(2)
      COMMON /PXCOM/LU
      IF(N .LT. 2)GO TO 10
      IF(LU .LE. 0)CALL PLOTS(0)
      CALL PLOT(0.75,0.75,-3) 
      CALL SCALE(X,XT,8.0,N)
      CALL SCALE(Y,YT,8.0,N)
      J=0 
      IF(N .LE. 20)J=1
      CALL LINE(X,XT,Y,YT,N,J,5)
      CALL AXIS(0.0,0.0,1HX,-1,8.0,0.0,XT(1),XT(2)) 
      CALL AXIS(0.0,0.0,1HY,+1,8.0,90.0,YT(1),YT(2))
      CALL PLOT(0.0,0.0,10) 
      RETURN
C 
C        END OF PLOTTING
C 
10    CALL PLOT(0.,0.,999)
      RETURN
      END 
      SUBROUTINE GRDNT(Z,MAXNX,NXX,NYY,YSIZE) 
C 
C FXY ROUTINE 
C PLOT FIRST DERIVATIVE (VECTOR FIELD) OF A FUNCTION DEFINED AT A GRID
C ABS(YSIZE)= HEIGHT IN Y-DIRECTION (> 2.0 CM)
C XSIZE=FLOAT(IABS(NXX))/FLOAT(IABS(NYY))*ABS(YSIZE)
C YSIZE > 0.0 : AXES , TO BE CHANGED WITH NUMTXT
C YSIZE < 0.0 : NO AXES 
C NX=IABS(NXX) : NR. GRIDPOINTS IN X-DIRECTION
C NXX>0.0 : USED AS A SUBROUTINE
C NXX < 0.0 : USED IN FXY ( SOME TESTS DELETED )
C 
      DIMENSION Z(MAXNX,1)
      LOGICAL QAXES,QFXY
C 
      NAME=4HGRDT
C 
      QAXES=YSIZE.GT.0.0
      QFXY=NXX.LT.0.0 
C 
C 
C ARROW DEFINITION
      AN=.39
      FP=-.2
      FILL=.45
C 
      NX=IABS(NXX)
      NY=IABS(NYY)
      NX1=NX-1
      NY1=NY-1
      RNX=NX
      RNY=NY
C 
      IF(QFXY)               GO TO 20 
CBEGIN  USED AS SUBROUTINE
      IF(.NOT.(NX.LT.3.OR.NY.LT.3.OR.NX.GT.200))    GO TO 10
      PRINT 9000,NAME 
 9000 FORMAT(7H ***** ,A10,12H ERROR *****) 
      PRINT 9010
 9010 FORMAT(14H NX/NY ILLEGAL) 
                             RETURN 
   10 CONTINUE
      CALL TESTZ(Z,MAXNX,NX,NY,IERR,NAME) 
      IF(IERR.GT.0)          RETURN 
CEND    USED AS SUBROUTINE
   20 CONTINUE
C 
      YS=ABS(YSIZE) 
      XS=RNX/RNY*YS 
C 
CBEGIN TEST HEIGHT/YSIZE
      IF(QFXY)               GO TO 30 
      IF(YS.GT.2.0)          GO TO 25 
      PRINT 9000,NAME 
      PRINT 9020
 9020 FORMAT(18H ABS(HEIGHT) < 2.0) 
   25 CONTINUE
   30 CONTINUE
CEND   TEST HEIGHT/YSIZE
C 
CBEGIN COMPUTE VM=LARGEST VECTOR COMPONENT
      VM=0.0
      DO 40 IY=2,NY1
      DO 40 IX=2,NX1
      VX=ABS(Z(IX+1,IY)-Z(IX-1,IY)) 
      VY=ABS(Z(IX,IY+1)-Z(IX,IY-1)) 
      IF(VX.GT.VM) VM=VX
      IF(VY.GT.VM) VM=VY
   40 CONTINUE
C 
C-------- VM>0.0 BECAUSE OF TEST IN TESTZ ----- 
CEND   COMPUTE VM=LARGEST VECTOR COMPONENT
C 
CBEGIN AXES 
      IF(.NOT.QAXES)         GO TO 60 
CC SCALING+ TEXTS TO BE CHANGED WITH NUMTXT 
CKRAAK** INIAX PRESETS /ZCTAX/ HELDER 
C 
CKRAAK INIAX,NUMTXT AND AXSS USE SOME NON-ANSI FEATURES 
CKRAAK ( NUMTXT CALLS CNCHAR TO DO CHARACTER MANIPULATION ) 
CKRAAK* TO AVOID POSSIBLE PROBLEMS DELETE THE INIAX AND 
CKRAAK*** AXSS CALLS ANS PLOT THE AXES SYSTEM WITH YOUR 
CKRAAK*** OWN   AXIS ROUTINE HELDER******** 
CKRAAK*** THIS COMMENT APPLIES ALSO TO RLSURF AND VCNW (VCNTR)
C 
      CALL INIAX
      ANGLE=90.0
      CALL AXSS(XS,YS,ANGLE,NX,NY)
   60 CONTINUE
CEND   AXES 
C 
      DX=XS/(RNX-1.)
      DY=YS/(RNY-1.0) 
      DX2=FILL*DX 
      DY2=FILL*DY 
      FX=DX2/VM 
      FY=DY2/VM 
      XB=DX 
      YB=DY 
C ARROW 
C ASSUME DX=DY
      AS=FP*DX2 
C 
      MM=-1 
      DO 200 IY=2,NY1 
      Y=YB+(IY-2)*DY
      MM=-MM
      DO 200 IX=2,NX1 
      IF(MM.EQ.-1)           GO TO 100
C MM=1 FORWARDS 
      F=IX-2
      II=IX 
                             GO TO 110
  100 CONTINUE
C MM=-1 BACKWARDS 
      F=NX1-IX
      II=NX1-IX+2 
  110 CONTINUE
      X=XB+F*DX 
      VX =Z(II+1,IY)-Z(II-1,IY) 
      VY=Z(II,IY+1)-Z(II,IY-1)
      DXX=VX*FX 
      DYY=VY*FY 
      IF(DXX.EQ.0.0.AND. DYY.EQ.0.0)   GO TO 200
      X1=X-DXX
      Y1=Y-DYY
C 
      CALL PLOT(X1,Y1,3)
      X2=X+DXX
      Y2=Y+DYY
      CALL PLOT(X2,Y2,2)
C ARROW POINT 
      IF(DXX.NE.0.0)        GO TO 120 
C DXX=0 
      ANG=1.57
      IF(DYY.LT.0.0) ANG=-ANG 
                             GO TO 130
  120 ANG=ATAN2(DYY,DXX)
  130 CONTINUE
      A=ANG+AN
      XX=X2+AS*COS(A) 
      YY=Y2+AS*SIN(A) 
      CALL PLOT(XX,YY,2)
      CALL PLOT(X2,Y2,3)
      A=ANG-AN
      XX=X2+AS*COS(A) 
      YY=Y2+AS*SIN(A) 
      CALL PLOT(XX,YY,2)
  200 CONTINUE
C 
      RETURN
      END 
      FUNCTION   HANDLER (SIGARGS, MECHARGS)
      INTEGER*4 SIGARGS(4),MECHARGS(5),HANDLER
      SIGARGS(1)   = 1
      SIGARGS(2)  =  4
      RETURN
      END

      SUBROUTINE HELPR(NAME,NHELP,NWHERE)
C 
C        Dump the contents of the appropriate help file at
C        the terminal 
C
C        NAME is the name of the HELP file, with no imbedded blanks
C        NHELP is a logical unit number available temporarily to this
C        routine.
C        NWHERE is the logical unit for writing the output.
C 
C        L. Weissman
C        Department of Biology, Gilmer Hall
C        University of Virginia
C        McCormick Road
C        Charlottesville, VA  22901
C
      BYTE LINE(80),NAME(*)
      INTEGER BLANK,NULL
      DATA BLANK,NULL/32,0/
C
C        Process the help file name and open it
C
      DO 10 I=1,80
      J=IBYTE(NAME,I)
      IF(J .EQ. NULL .OR. J .EQ. BLANK)GO TO 20
      CALL MOVBYT(NAME,I,LINE,I)
10    CONTINUE
      I=80
20    CALL MOVBYT(0,1,LINE,I)
      OPEN(NHELP,FILE=LINE,READONLY,STATUS='OLD',ERR=60)
C
C        Read the help file
C
30    READ(NHELP,40,END=50)J,(LINE(I),I=1,J)
40    FORMAT(Q,80A1)
      IF(J .GT. 0)WRITE(NWHERE,45)(LINE(I),I=1,J)
45    FORMAT(1X,80A1)
      GO TO 30
50    CLOSE(NHELP)
      RETURN
60    WRITE(NWHERE,70)
70    FORMAT(' *** ERROR *** HELP file not found')
      RETURN
      END
      SUBROUTINE HPINIT(LIO,LMON,LCHARS,LUNIT,LDEV)                     
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C INITIALIZE THE SOFTWARE AND THE PLOTTER                               
C                                                                       
C   PARAMETERS :                                                        
C                                                                       
C   LIO      = HANDSHAKE METHOD DESIRED : 0 = ENQUIRE / ACKNOWLEDGE     
C                                         1 = XON / XOFF                
C                                         2 = HP-IB                     
C                                         3 = NO HANDSHAKE              
C                                                                       
C              NOTE : IF THE VALUE OF -LIO- IS NEGATIVE, THE INITIAL-   
C                     IZATION OF SYSTEM CONFIGURATION PARAMETERS WILL   
C                     BE SUPPRESSED.                                    
C                                                                       
C                                                                       
C   LMON     = MONITOR MODE CONTROL : 0 = NO MONITOR MODE               
C                                     1 = PARSE MONITOR MODE            
C                                     2 = RECEIVE MONITOR MODE          
C                                                                       
C   LCHARS   = THE STANDARD CHARACTER SET DESIRED                       
C                                                                       
C                                                                       
C   LUNIT    = UNIT SYSTEM DESIRED : 0 = INCHES                         
C                                    1 = METRIC                         
C                                                                       
C   LDEV     = LOGICAL UNIT OF THE PLOTTER                              
C                                                                       
C                                                                       
C *****************************************************************     
C                                                                       
C *** COMMON BLOCK                                                      
C                                                                       
      COMMON /HP/ XMINPU, YMINPU, XORG  , YORG  , XOFF  , YOFF    ,     
     +            XFACT , YFACT , XCNTRL, YCNTRL, XYUNIT          ,     
     +            MDEV  , MUNIT , MPENS , MLU   , MCNTRL, MINIT   ,     
     +            MSHAKE, MPTR  , MCODE , MPLOT , MFLUSH, MSIZE   ,     
     +            MSPOOL, MVER  , MOUT(72),                             
     +            RSI1  , RSI2  , RDI1   , RDI2  , RES1  , RES2         
C                                                                       
C     **** XMINPU - CONTAINS THE MINIMUM NUMBER OF PLOTTER UNITS IN X   
C                                                                       
C     **** YMINPU - CONTAINS THE MINIMUM NUMBER OF PLOTTER UNITS IN Y   
C                                                                       
C     ***** XORG   - CONTAINS THE CURRENT X ORIGIN IN PLOTTER UNITS     
C                                                                       
C     ***** YORG   - CONTAINS THE CURRENT Y ORIGIN IN PLOTTER UNITS     
C                                                                       
C     ***** XOFF   - CONTAINS THE CURRENT X OFFSET IN PLOTTER UNITS     
C                                                                       
C     ***** YOFF   - CONTAINS THE CURRENT Y OFFSET IN PLOTTER UNITS     
C                                                                       
C     ***** XFACT  - CONTAINS THE CURRENT X SCALE FACTOR                
C                                                                       
C     ***** YFACT  - CONTAINS THE CURRENT Y SCALE FACTOR                
C                                                                       
C     ***** XYUNIT - CONTAINS THE CURRENT NUMBER OF PLOTTER UNITS PER   
C                    INCH OR CENTIMETER                                 
C                                                                       
C     ***** MDEV   - CONTAINS DEVICE TYPE ( 7220,7470,7475,             
C                     7580,7585, 9872 )                                 
C                                                                       
C     ***** MUNIT  - CONTAINS THE CODE FOR THE UNIT SYSTEM ENABLED      
C                    ( 0 = INCHES / 1 = METRIC )                        
C                                                                       
C     ***** MPENS  - CONTAINS THE NUMBER OF PENS AVAILABLE              
C                                                                       
C     ***** MLU    - CONTAINS THE LOGICAL UNIT NUMBER FOR I/O           
C                                                                       
C                                                                       
C     ***** MINIT  - CONTAINS THE CODE FOR SOFTWARE SYSTEM INITIALIZE   
C                    ( 0 = NOT INITIALIZED / 1 = INITIALIZED )          
C                                                                       
C     *** MSHAKE - CONTAINS THE HANDSHAKE METHOD CODE ( 0 = ENQUIRE -   
C                    ACKNOWLEDGE  /  1 = XON-XOFF  /  2 = HP-IB )       
C            AND 3 = NO HANDSHAKEING - SPOOLING ONLY                    
C                                                                       
C     **** MPTR   - CONTAINS THE POINTER TO THE NEXT LOCATION IN MOUT   
C                                                                       
C     ***** MCODE  - CONTAINS THE PEN STATUS CODE ( 0 = PEN IS UP /     
C                    1 = PEN IS DOWN )                                  
C                                                                       
C     ***** MPLOT  - CONTAINS THE PARAMETER CONCATENATION CODE USED     
C                    BY SUBROUTINE BUFF ( 0 = DONT CONCATENATE /        
C                    1 = CONCATENATE )                                  
C                                                                       
C     ***** MFLUSH - CONTAINS THE CODE USED TO SIGNAL THAT AN           
C                    IMMEDIATE TRANSMIT OF THE OUTPUT BUFFER MUST BE    
C                    MADE ( 0 = NO IMMEDIATE TRANSMIT / 999 =           
C                    IMMEDIATE TRANSMIT).                               
C                                                                       
C     ***** MSIZE  - CONTAINS THE SIZE OF THE OUTPUT ARRAY MOUT         
C                                                                       
C     ***** MOUT   - CONTAINS DATA TO BE SENT TO THE PLOTTER            
C                                                                       
C    ***** RSI1,RSI2  -- KEEP TRACK OF VALUES SO SYMBOL                 
C                        WONT KEEP REPEATING COMMAND  SI                
C                                                                       
C    ***** RDI1,RDI2  -- SAME AS ABOVE BUT FOR DI COMMAND               
C                                                                       
C    ***** RES1,RES2  -- SAME AS ABOVE BUT FOR ES COMMAND               
C                                                                       
C *****************************************************************     
C                                                                       
C *** COMMON BLOCK                                                      
C                                                                       
      COMMON/ZZCOM/MZBITS,MZCHAR,MZWORD,MZJUST,MZCCSW,MZCCTL,MZDCCS,    
     +MZDCCT,MZHISW,MZLUSW,MZLURD,MZLUWR,                               
     +MZTAD(2),MZOTC(2),MZETC(2),MZOT1(2),MZOT2(2),                     
     +MZHM(2),MZBS(2),MZHEC(2),MZHS1(2),MZHS2(2),MZICD(2),MZIRS1(2),    
     +MZIRS2(2),MZMBS(2),MZSCOC(2)                                      
C                                                                       
C *****************************************************************     
C                                                                       
      DIMENSION IA(22),IB(10),IC(2),IN(8),IBUFF(5),XBUFF(1)             
      DATA IA( 1)/67/,IA( 2)/65/,IA( 3)/67/,IA( 4)/83/                  
C                  C          A          C          S                   
      DATA IA( 5)/73/,IA( 6)/78/,IA( 7)/73/,IA( 8)/80/                  
C                  I          N          I          P                   
      DATA IA( 9)/73/,IA(10)/87/,IA(11)/79/,IA(12)/73/                  
C                  I          W          O          I                   
      DATA IA(13)/79/,IA(14)/80/,IA(15)/83/,IA(16)/67/                  
C                  O          P          S          C                   
      DATA IA(17)/83/,IA(18)/80/,IA(19)/79/,IA(20)/79/                  
C                  S          P          O          O                   
      DATA IA(21)/65/,IA(22)/70/                                        
C             A          F                                              
      DATA IB( 1)/72/,IB( 2)/73/,IB( 3)/74/,IB( 4)/75/,IB( 5)/77/       
C                  H          I          J          K          M        
      DATA IB( 6)/78/,IB( 7)/89/,IB( 8)/64/,IB(9)/67/,IB(10)/84/        
C                  N          Y          @         C          T         
      DATA IC( 1)/27/,IC( 2)/46/                                        
C                 ESC         .                                         
C                                                                       
C**** NOTE THAT THE INITIALIZATION OF CONFIGURATION PARAMETERS          
C**** CAN BE SUPPRESSED BY PASSING THE PARAMETER -LIO- AS A             
C**** NEGATIVE VALUE.  THIS IS A DIAGNOSTIC AID WHICH CAN BE            
C**** USED TO ESTABLISH CONFIGURATION PARAMETERS AT RUN-TIME.           
C                                                                       
C                                                                       
C THIS SECTION HAS BEEN ADDED TO ALLOW FOR DEFERRED PLOTTING.           
C IF TENS DIGIT OF LIO = 1 THEN SPOOLING IS PUT INTO EFFECT.            
C                                                                       
      MSPOOL=0                                                          
      IOCODE=MOD(IABS(LIO),10)                                          
C                                                                       
C MAKE SURE IOCODE IN RANGE                                             
C                                                                       
      IF(IOCODE.LT.0.OR.IOCODE.GT.3) IOCODE=0                           
C                                                                       
C CHECK FOR SPOOLING                                                    
C                                                                       
      IF(LIO.GE.10.AND.LIO.LT.20) MSPOOL=1                              
C                                                                       
C IF USER ASKED FOR NO HANDSHAKE BUT NOT SPOOLING, ASSUME ENQ/ACK       
C                                                                       
      IF (MSPOOL.EQ.0.AND.IOCODE.EQ.3) IOCODE=0                         
C SHOULD USE DEFAULT HANDSHAKEING PARAM FROM INSTALL.                   
C                                                                       
C**** SAVE PAPER SIZE IF SPOOLING                                       
C                                                                       
      IF (MSPOOL.EQ.1) IPSIZE=MZDCCT                                    
      IF (LIO.LT.0) GO TO 10                                            
C                                                                       
C**** INITIALIZE THE SYSTEM CONFIGURATION PARAMETERS                    
C**** WHICH WILL BE PLACED IN THE COMMON BLOCK -ZZCOM-                  
C                                                                       
      CALL ZZINIT                                                       
C                                                                       
C**** INITIALIZE THE ISPP VARIABLES                                     
C                                                                       
   10 MUNIT=LUNIT                                                       
      MLU=LDEV                                                          
      MPENS=8                                                           
      MPTR=1                                                            
      MCODE=0                                                           
      MPLOT=0                                                           
C                                                                       
C**** IF DEFERED PLOT, RESET CCONTROL                                   
C                                                                       
      IF (MSPOOL.NE.1) GOTO 12                                          
        MZCCSW=MZDCCS                                                   
        MZCCTL=MZDCCT                                                   
  12  CONTINUE                                                          
C                                                                       
C IF SPOOLING IS DONE THEN MDEV IS SET THRU SUBROUTINE                  
C DPINIT. OTHERWISE THE SOFTWARE INTERROGATES THE PLOTTER               
C FOR IT'S IDENTITY. EITHER WAY IT IS NOT NECCESARY TO                  
C INITIALIZE MDEV TO 0 AND IN THE CASE OF SPOOLING IT                   
C WOULDN'T BE DESIRED. THE VERSION OF THE PLOTTER MUST BE               
C SET TO EITHER A,B,C,S, OR T.                                          
C                                                                       
C     MDEV=0                                                            
      IF(MSPOOL.EQ.1) IN(2)=MVER                                        
C                                                                       
C IF MDEV = 0  THEN USE DEFAULT PLOTTER VALUE FROM INSTALATION          
      MINIT=0                                                           
      MSHAKE=IOCODE                                                     
      IOTYPE=IOCODE+1                                                   
      MFLUSH=0                                                          
      MCNTRL=0                                                          
      MSIZE=71                                                          
      XMINPU=0.0                                                        
      YMINPU=0.0                                                        
      XORG=0.0                                                          
      YORG=0.0                                                          
      XOFF=0.0                                                          
      YOFF=0.0                                                          
      XFACT=1.0                                                         
      YFACT=1.0                                                         
      XYUNIT=1016.0                                                     
C                                                                       
C**** THESE ARE FOR SI,DI AND ES COMMANDS                               
C                                                                       
      RSI1=0.0                                                          
      RSI2=0.0                                                          
      RDI1=0.0                                                          
      RDI2=0.0                                                          
      RES1=0.0                                                          
      RES2=0.0                                                          
C                                                                       
C**** THESE ARE TO KEEP TRACK OF PEN POSITION FOR SPOOLING              
C                                                                       
      XCNTRL=0.0                                                        
      YCNTRL=0.0                                                        
C                                                                       
C**** IS THE HP-IB HANDSHAKE DESIRED ?                                  
C                                                                       
      IF (IOCODE.EQ.2) GO TO 20                                         
C                                                                       
C IF NO HANDSHAKEING WANTED, BYPASS ALL INIT STUFF                      
C                                                                       
      IF (IOCODE.EQ.3) GO TO 70                                         
C                                                                       
C**** SET UP I/O IMMEDIATE TRANSMIT CODE TO ENSURE THAT ALL OUTPUT      
C**** IS IMMEDIATELY TRANSMITTED UNTIL THE PROPER HANDSHAKE METHOD      
C**** HAS BEEN INVOKED                                                  
C                                                                       
      MFLUSH=999                                                        
C                                                                       
C**** ATTEMPT TO TURN ON THE PLOTTER ( ESC.Y )                          
C**** THIS RESOLVES THE PROBLEM WITH TERMINALS THAT                     
C**** MUST SEE AN UPPER-CASE CHARACTER FOLLOWING AN                     
C**** ESCAPE CHARACTER SEQUENCE                                         
C                                                                       
      CALL BUFF(1,IC(1),XBUFF(1),2)                                     
      CALL BUFF(1,IB(7),XBUFF(1),1)                                     
C                                                                       
C                                                                       
C**** SET PLOTTER CONFIGURATION ( ESC.@ P1;P2: )                        
C                                                                       
C                                                                       
C      THE PLOTTER CONFIGURATION PARAMETERS ARE AS FOLLOWS :            
C                                                                       
C      P1 = MAXIMUM BUFFER SIZE                                         
C                                                                       
C      P2 = CONFIGURATION OPTIONS       ENABLE DATA TRANSMISSION        
C                                                                       
      CALL BUFF(1,IC(1),XBUFF(1),2)                                     
      CALL BUFF(1,IB(8),XBUFF(1),1)                                     
      IBUFF(1)=MZMBS(IOTYPE)                                            
      IBUFF(2)=MZSCOC(IOTYPE)                                           
C                                                                       
C**** IS MONITOR MODE DESIRED ?                                         
C                                                                       
      IF (LMON.EQ.0) GO TO 90                                           
C                                                                       
C**** SET UP FOR MONITOR MODE ( P2 = 10 )                               
C                                                                       
      IBUFF(2)=IBUFF(2)+8                                               
C                                                                       
C  SEE IF RECEIVE MODE WANTED                                           
C                                                                       
      IF (LMON.NE.2) GO TO 90                                           
C                                                                       
C  SET UP FOR RECEIVE MODE (P2 = 14)                                    
C                                                                       
      IBUFF(2)= IBUFF(2) + 4                                            
C                                                                       
  90  CALL BUFF(5,IBUFF(1),XBUFF(1),-2)                                 
C                                                                       
C**** IF SPOOLING, DONT SEND ABORT COMMANDS                             
C                                                                       
      IF (MSPOOL.NE.0) GO TO 15                                         
C                                                                       
C**** ABORT DEVICE CONTROL ( ESC.J )                                    
C                                                                       
      CALL BUFF(1,IC(1),XBUFF(1),2)                                     
      CALL BUFF(1,IB(3),XBUFF(1),1)                                     
C                                                                       
C**** ABORT GRAPHIC ( ESC.K )                                           
C                                                                       
      CALL BUFF(1,IC(1),XBUFF(1),2)                                     
      CALL BUFF(1,IB(4),XBUFF(1),1)                                     
  15  CONTINUE                                                          
C                                                                       
C**** INITIALIZE THE PLOTTER ( IN; )                                    
C                                                                       
   20 CALL BUFF(2,IA(5),XBUFF(1),-2)                                    
C                                                                       
C**** IS THE HP-IB HANDSHAKE DESIRED ?                                  
C                                                                       
      IF (IOCODE.EQ.2) GO TO 30                                         
C                                                                       
C**** SET OUTPUT MODE ( ESC.M P1;P2;P3;P4: )                            
C                                                                       
C     THE OUTPUT MODE PARAMETERS CONDITION THE PLOTTER FOR OUTPUT       
C     AS FOLLOWS :                                                      
C                                                                       
C      P1 = TURN-AROUND DELAY                                           
C                                                                       
C      P2 = OUTPUT TRIGGER                                              
C                                                                       
C      P3 = ECHO TERMINATOR                                             
C                                                                       
C      P4 = OUTPUT TERMINATOR                                           
C                                                                       
      CALL BUFF(1,IC(1),XBUFF(1),2)                                     
      CALL BUFF(1,IB(5),XBUFF(1),1)                                     
      IBUFF(1)=MZTAD(IOTYPE)                                            
      IBUFF(2)=MZOTC(IOTYPE)                                            
      IBUFF(3)=MZETC(IOTYPE)                                            
      IBUFF(4)=MZOT1(IOTYPE)                                            
      IBUFF(5)=MZOT2(IOTYPE)                                            
      CALL BUFF(4,IBUFF(1),XBUFF(1),-5)                                 
C                                                                       
C**** SET EXTENDED HANDSHAKE ( ESC.N P1;P2: )                           
C                                                                       
C                                                                       
C     THE EXTENDED HANDSHAKE PARAMETERS ARE AS FOLLOWS :                
C                                                                       
C      P1 = BETWEEN-CHARACTERS DELAY                                    
C                                                                       
C      P2 = IMMEDIATE RESPONSE CHARACTER                                
C                                                                       
      CALL BUFF(1,IC(1),XBUFF(1),2)                                     
      CALL BUFF(1,IB(6),XBUFF(1),1)                                     
      IBUFF(1)=MZICD(IOTYPE)                                            
      IBUFF(2)=MZIRS1(IOTYPE)                                           
      IBUFF(3)=MZIRS2(IOTYPE)                                           
      CALL BUFF(4,IBUFF(1),XBUFF(1),-3)                                 
C                                                                       
C**** OBTAIN THE PLOTTER TYPE ( OI; )                                   
C**** ONE OF THE FOLLOWING PLOTTER TYPES SHOULD BE RETURNED :           
C****                                                                   
C****      RS-232 C                     HP-IB                           
C****      --------                     -----                           
C****                                                                   
C****     7220 A,B,C,S,T             9872 A,B,C,S,T                     
C****                                                                   
C****     7580 A,B                   7580 A,B                           
C****                                                                   
C****     7585 A,B                   7585 A,B                           
C****                                                                   
C****     7470 A                     7470 A                             
C****                                                                   
C****     7475 A                     7475 A                             
C****                                                                   
C****     7586 B                     7586 B                             
C****                                                                   
C                                                                       
C                                                                       
C IS SPOOLING IN EFFECT? IF SO THEN WE CAN'T READ FROM THE PLOTTER.     
C                                                                       
   30 IF (MSPOOL.EQ.1) GOTO 35                                          
C                                                                       
      CALL BUFF(1,IA(11),XBUFF(1),-2)                                   
      CALL BUFF(3,IN(1),XBUFF(1),2)                                     
      MDEV=IN(1)                                                        
C                                                                       
C THIS IS THE CONTINUE LINE FOR AVOIDING THE READ FROM THE PLOTTER.     
C                                                                       
   35 CONTINUE                                                          
      MVER=IN(2)                                                        
C                                                                       
C                                                                       
C**** IS THE HP-IB HANDSHAKE DESIRED ?                                  
C                                                                       
      IF (IOCODE.EQ.2) GO TO 70                                         
C                                                                       
C**** SELECT DESIRED HANDSHAKE MODE                                     
C                                                                       
   40 IF (MZHM(IOTYPE).EQ.2) GO TO 60                                   
C                                                                       
C**** SETUP ENQUIRE / ACKNOWLEDGE HANDSHAKE METHOD                      
C                                                                       
C**** SET HANDSHAKE MODE 1 ( ESC.H P1;P2;P3: )                          
C                                                                       
C                                                                       
C     THE HANDSHAKE MODE 1 PARAMETERS ARE AS FOLLOWS :                  
C                                                                       
C      P1 = BLOCK SIZE                                                  
C                                                                       
C      P2 = HANDSHAKE ENABLE CHARACTER                                  
C                                                                       
C      P3 = HANDSHAKE STRING                                            
C                                                                       
C                                                                       
      CALL BUFF(1,IC(1),XBUFF(1),2)                                     
      CALL BUFF(1,IB(1),XBUFF(1),1)                                     
      IBUFF(1)=MZBS(IOTYPE)                                             
      IBUFF(2)=MZHEC(IOTYPE)                                            
      IBUFF(3)=MZHS1(IOTYPE)                                            
      IBUFF(4)=MZHS2(IOTYPE)                                            
      CALL BUFF(5,IBUFF(1),XBUFF(1),-4)                                 
C                                                                       
C**** SET HANDSHAKE CODE                                                
C                                                                       
      MSHAKE=0                                                          
C                                                                       
C*** THIS IS SO THAT BOUT WILL OUTPUT CORRECT CHARACTER                 
C*** FOR HANDSHAKEING, BECAUSE IT ASSUMES PRIMARY HANDSHAKE             
C*** IS SOFTWARE CHECKING                                               
C                                                                       
      MZHEC(1)=MZHEC(IOTYPE)                                            
C                                                                       
      MFLUSH=0                                                          
      GO TO 70                                                          
C                                                                       
C**** SETUP XON / XOFF HANDSHAKE METHOD                                 
C                                                                       
C                                                                       
C**** SET HANDSHAKE MODE 2 ( ESC.I P1;P2;P3: )                          
C                                                                       
C     THE HANDSHAKE MODE 2 PARAMETERS ARE AS FOLLOWS :                  
C                                                                       
C     P1 = BLOCK SIZE ( NUMBER OF BYTES REMAINING TO TRIGGER XOFF )     
C                                                                       
C     P2 = HANDSHAKE ENABLE CHARACTER                                   
C                                                                       
C     P3 = HANDSHAKE STRING                                             
C                                                                       
  60  CALL BUFF(1,IC(1),XBUFF(1),2)                                     
      CALL BUFF(1,IB(2),XBUFF(1),1)                                     
      IBUFF(1)=MZBS(IOTYPE)                                             
      IBUFF(2)=MZHEC(IOTYPE)                                            
      IBUFF(3)=MZHS1(IOTYPE)                                            
      IBUFF(4)=MZHS2(IOTYPE)                                            
      CALL BUFF(5,IBUFF(1),XBUFF(1),-4)                                 
C                                                                       
C**** SET HANDSHAKE CODE                                                
C                                                                       
      MSHAKE=1                                                          
      MFLUSH=0                                                          
C                                                                       
C                                                                       
C**** IS THE PLOTTER A 7220 OR A 9872 ?                                 
C                                                                       
   70 IF (MDEV.NE.7220.AND.MDEV.NE.9872) GO TO 80                       
C                                                                       
C**** YES, IS IT THE 8 PEN MODEL ( C OR T ) ?                           
C                                                                       
      IF (MVER.EQ.IB(9).OR.MVER.EQ.IB(10)) GO TO 80                     
C                                                                       
C**** NO,WE HAVE AN 4 PEN MODEL 7220 OR 9872                            
C                                                                       
      MPENS=4                                                           
C                                                                       
C****                                                                   
C                                                                       
   80 CONTINUE                                                          
C                                                                       
C**** IF THE PLOTTER IS A 7470 SET UP FOR 2 PENS                        
C                                                                       
      IF (MDEV .EQ. 7470) MPENS=2                                       
C                                                                       
C**** IF THE PLOTTER IS A 7475, SET UP FOR 6 PENS                       
C                                                                       
      IF (MDEV .EQ. 7475) MPENS=6                                       
C                                                                       
C IF SPOOLING IS IN EFFECT THEN WE MUST DEFINE THE PLOTTING AREA        
C WITHOUT INTERROGATING THE PLOTTER.                                    
C                                                                       
      IF (MSPOOL.EQ.0) GOTO 111                                         
      CALL XXORG(MDEV,IPSIZE,IBUFF)                                     
C                                                                       
C**** SKIP IF NO HANDSHAKE                                              
C                                                                       
      IF (IOCODE.EQ.3) GO TO 115                                        
      GOTO 112                                                          
111   CONTINUE                                                          
C                                                                       
C                                                                       
C**** OBTAIN THE PLOTTER X,Y MIN-MAX VALUES ( OP; )                     
C                                                                       
      CALL BUFF(1,IA(13),XBUFF(1),-2)                                   
      CALL BUFF(3,IBUFF(1),XBUFF(1),4)                                  
C                                                                       
C**** IF 7586B THEN CHECK FOR ROLL FEED OR SHEET FEED                   
C                                                                       
      IF (MDEV.NE.7586.AND.MDEV.NE.7550) GO TO 115                      
C                                                                       
C**** OUT PUT EXTENDED STATUS FOR CHECK ESC.O                           
C                                                                       
      CALL BUFF(1,IC(1),XBUFF(1),2)                                     
      CALL BUFF(1,IA(11),XBUFF(1),1)                                    
      CALL BUFF(3,IN(1),XBUFF(1),1)                                     
      ISTAT=IN(1)                                                       
C                                                                       
C**** IF NOT ROLL FEED THEN TURN INTO 7585                              
C                                                                       
      IF (MOD(ISTAT,2).EQ.0.AND.MDEV.EQ.7586) MDEV=7585                 
C                                                                       
C*** IF NO ROLL FEED(7586) OR SHEET FEED(7550) THEN SKIP                
C                                                                       
      IF (MOD(ISTAT,2).EQ.0) GO TO 116                                  
      IF (MOD(ISTAT,4).LT.2) GO TO 116                                  
      CALL BUFF(2,IA(21),XBUFF(1),-2)                                   
  116 CONTINUE                                                          
      GO TO 115                                                         
C                                                                       
C CONTINUE LINE TO AVOID THE READ FROM THE PLOTTER.                     
C                                                                       
  112 CONTINUE                                                          
C                                                                       
C**** IF 7586B, PUT AF COMMAND IN FILE                                  
C                                                                       
      IF(MDEV.NE.7586) GO TO 115                                        
      CALL BUFF(1,IA(21),XBUFF(1),-2)                                   
C                                                                       
  115 CONTINUE                                                          
C                                                                       
C**** OBTAIN THE PLOTTER MINIMUM PLOTTER UNITS                          
C                                                                       
      XMINPU=IBUFF(1)                                                   
      YMINPU=IBUFF(2)                                                   
C                                                                       
C**** INITIALIZE THE CURRENT ORIGIN                                     
C                                                                       
  120 XORG=XMINPU                                                       
      YORG=YMINPU                                                       
C                                                                       
C**** SET UP THE UNITS SYSTEM                                           
C                                                                       
      IF (MUNIT .EQ. 0) XYUNIT=1016.0                                   
      IF (MUNIT .EQ. 1) XYUNIT=400.0                                    
C                                                                       
C                                                                       
C**** DESIGNATE THE STANDARD CHARACTER SET ( CS )                       
C                                                                       
      IBUFF(1)=LCHARS                                                   
      CALL BUFF(1,IA(3),XBUFF(1),2)                                     
      CALL BUFF(4,IBUFF(1),XBUFF(1),1)                                  
C                                                                       
C**** IS THE PLOTTER A 7580 OR 7585 OR 7586?                            
C                                                                       
      IF(MDEV.NE.7580.AND.MDEV.NE.7585.AND.MDEV.NE.7586                 
     +  .AND.MDEV.NE.7550)                                              
     +  GO TO 160                                                       
C                                                                       
C**** YES, DESIGNATE SET 5 AS THE ALTERNATE SET FOR                     
C**** GENERATING CENTERED SYMBOLS ( CA5; )                              
C                                                                       
      IBUFF(1)=5                                                        
      CALL BUFF(1,IA(1),XBUFF(1),2)                                     
      CALL BUFF(4,IBUFF(1),XBUFF(1),1)                                  
C                                                                       
C**** SELECT PEN #1 ( SP1; )                                            
C                                                                       
  160 CALL BUFF(1,IA(17),XBUFF(1),2)                                    
      IBUFF(1)=1                                                        
      CALL BUFF(5,IBUFF(1),XBUFF(1),1)                                  
C                                                                       
C**** SET THE SYSTEM INITIALIZED CODE                                   
C                                                                       
      MINIT=1                                                           
      RETURN                                                            
      END                                                               

      FUNCTION IBYTE(STR,NPOS)
C 
C        Returns numerical value of byte in position "NPOS" in string
C        "STR". IBYTE will be in the range 0-255, or -1 if NPOS < 1.
C 
C        L. Weissman
C        Department of Biology, Gilmer Hall
C        University of Virginia
C        McCormick Road
C        Charlottesville, VA  22901
C
      BYTE STR(*)
C
      IBYTE=-1
      IF(NPOS .LT. 1)GO TO 10
      IBYTE=STR(NPOS)
      IF(IBYTE .LT. 0)IBYTE=256+IBYTE
10    RETURN
      END

      LOGICAL FUNCTION IFBIT(IA,I)
C 
C        Returns true if I'th bit of array IA is set to 1, else 
C        false. 
C 
C        L. Weissman
C        Department of Biology, Gilmer Hall
C        University of Virginia
C        McCormick Road
C        Charlottesville, VA  22901
C
      BYTE IA(*),BIT1(8)
      DATA BIT1/1,2,4,8,16,32,64,'200'O/
C
      IFBIT=.FALSE. 
      IF(I)30,30,10 
10    J=(I-1)/8
      K=I-J*8
      L=IA(J+1) .AND. BIT1(K)
      IF(L)20,30,20
20    IFBIT=.TRUE.
30    RETURN
      END 
C
C
C
      FUNCTION INCMOD3( I, INC)
C     =========================
C
C
      INCMOD3 = I + INC
      IF ( INCMOD3 .GT. 3) INCMOD3 = INCMOD3 - 3
C
C
      RETURN
C
C
      END
      SUBROUTINE INIAX
C PRESET /ZCTAX/
      COMMON /ZCTAX/NXL,IXLAB(8),XLEFT,XSF,NYL, 
     *              IYLAB(8),YLOW,YSF,QXAX,QYAX 
C LENGTH OF IXLAB,IYLAB MACH. DEP.  *************** 
C------- IN SUBROUTINE : PRESETTING BY INIAX( BY DATA STATEMENTS) 
C------- IN FXY : BY ZINI WHICH OVERRIDES THE INIAX-PRESETTING
      LOGICAL QXAX,QYAX 
      DATA NXL /1/
      DATA NYL /1/
      DATA IXLAB(1) /1HX/ 
      DATA IYLAB(1) /1HY/ 
      DATA XLEFT /1.0/
      DATA YLOW  /1.0/
      DATA QXAX /.FALSE./ 
      DATA QYAX /.FALSE./ 
      RETURN
      END 
      SUBROUTINE INTPOL (X,Y,OLDX,OLDY,XSMAX,YSMAX,YES)
C
C
C.....FIND POINT WERE CURVE HITS FRAME
      LOGICAL YES
      XDUM=X
      YDUM=Y
      IF (X.LT.0.0) THEN
          A=(Y-OLDY)/(X-OLDX)
          B=Y-A*X
          X=0.0
          Y=A*X+B
      ENDIF
      IF (Y.LT.0.0) THEN
          A=(Y-OLDY)/(X-OLDX)
          B=Y-A*X
          Y=0.0
          X=(Y-B)/A
      ENDIF
      IF (Y.GT.YSMAX) THEN
          A=(Y-OLDY)/(X-OLDX)
          B=Y-A*X
          Y=YSMAX
          X=(Y-B)/A
      ENDIF
      IF (X.GT.XSMAX) THEN
          A=(Y-OLDY)/(X-OLDX)
          B=Y-A*X
          X=XSMAX
          Y=A*X+B
      ENDIF
      IF(X.LT.0.0.OR.X.GT.XSMAX.OR.Y.LT.0.0.OR.Y.GT.YSMAX)THEN
          YES=.FALSE.
          X=XDUM
          Y=YDUM
      ELSE
          YES=.TRUE.
      ENDIF
      RETURN
      END
C
C
C
      SUBROUTINE INVRS(X,DETX,XINV)
C     ===============================
C
C
C
C---- Calculates the invers [xinv] and the determinant 'detx' of a
C      3x3 matrix [x];  [xinv] may be the same array as [x].
C      if detx = 0, [xinv] will be left undefined.
C
C---- Slightly modified     27/10/76     LJL
C
C
C     .. Scalar Arguments ..
      REAL DETX
C     ..
C     .. Array Arguments ..
      REAL X(9),XINV(9)
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
C     .. Local Arrays ..
      REAL XX(9),ZZ(9)
C     ..
C
C
      DO 10 I = 1,9
        XX(I) = X(I)
   10 CONTINUE
      ZZ(1) = XX(5)*XX(9) - XX(6)*XX(8)
      ZZ(2) = XX(3)*XX(8) - XX(2)*XX(9)
      ZZ(3) = XX(2)*XX(6) - XX(3)*XX(5)
      ZZ(4) = XX(6)*XX(7) - XX(4)*XX(9)
      ZZ(5) = XX(1)*XX(9) - XX(3)*XX(7)
      ZZ(6) = XX(3)*XX(4) - XX(1)*XX(6)
      ZZ(7) = XX(4)*XX(8) - XX(5)*XX(7)
      ZZ(8) = XX(2)*XX(7) - XX(1)*XX(8)
      ZZ(9) = XX(1)*XX(5) - XX(2)*XX(4)
      DETX = ZZ(1)*XX(1) + ZZ(2)*XX(4) + ZZ(3)*XX(7)
      IF (DETX.NE.0.0) THEN
        DO 20 I = 1,9
          XINV(I) = ZZ(I)/DETX
   20   CONTINUE
      END IF
C
C
      END
      SUBROUTINE KAPPSPH(TH,CAPPA,PSI,PHI)
C///  VERSION 83/05/18
      DIMENSION TH(3)
C
C         CALCULATES FROM THETA'S THE CORRESPONDING VALUES FOR THE
C         ROTATIONAL ANGLE KAPPA AND THE SPHERICAL POLAR COORDINATES
C         PS AND PHI
C         REF ROSSMANN AND BLOW,OP.CIT.,P.28,FORMULA 11,WITH CORRECTIONS
C         AS GIVEN IN PROGRAM DESCRIPTION.
C
C\\\
      S1=  SIN(TH(1))
      S2 = SIN(TH(2))
      S3 = SIN(TH(3))
      C1 = COS(TH(1))
      C2 = COS(TH(2))
      C3 = COS(TH(3))
C
      ARG = COS(0.5 * TH(2)) * COS(0.5 * (TH(1)+TH(3)))
      CAPPA= ACOS(ARG)
      CAPPA=2.0*CAPPA
      SK=SIN(CAPPA)
C
      ARG = (S2* S3 - S1*S2)/(2.0*SK)
      PSI= ACOS(ARG)
      SPS=SIN(PSI)
C
      DIV = -2.0*SPS*SK
      ARG  = -(C1*C2*S3+S1*C3 + S1*C2*C3 + C1*S3)/DIV
      ARG2 = (S2*C3 + C1*S2)/DIV
      PHI= ATAN2(ARG ,ARG2)
C
C
      CALL KAPRANG(CAPPA,PSI,PHI)
      RETURN
      END
      SUBROUTINE KAPRANG(CAPPA,PSI,PHI)
C         KAPRANG BRINGS POLAR ROT. ANGLES IN RANGE:
C         0< KAPPA<TPI;0<PSI<PI; 0<PHI<PI , USING:
C         ROT(KAP,PS,PH)=ROT(KAP,-PSI,PHI-PI), AND
C         ROT(KAP,PS,PH)=ROT(-KAP,PI-PSI,PI+PHI),WHICH COMBINED GIVE:
C         ROT(KAP,PS,PH)=ROT(-KAP,PSI-PI,PHI)
C         REF:ROSSMANN AND BLOW ACTA(1962)15,24  PAGE 30.
C
C         JUNE 1974
C
      DATA PI/0.31415926535898E+01/
      DATA TPI/0.62831853071796E+01/
      CAPPA= AMOD(CAPPA,TPI)
      PSI  = AMOD(PSI,TPI)
      PHI  = AMOD(PHI,TPI)
      IF(PHI.LT.0.0) GOTO 10
      GOTO 20
   10 PHI=PHI + TPI
   20 IF(PHI .GT. PI)GOTO 30
      GOTO 40
   30 PHI= PHI - PI
      PSI= -PSI
   40 IF(PSI.LT. 0.0) GOTO 50
      GOTO 60
   50 PSI= PSI+ TPI
   60 IF(PSI.GT.PI) GOTO 70
      GOTO 80
   70 PSI=PSI-PI
      CAPPA=-CAPPA
   80 IF(CAPPA.LT. 0.0) CAPPA= CAPPA+TPI
      RETURN
      END

      FUNCTION LENG(STR,NDIM) 
C 
C        Return the length in bytes "LENG" of the string "STR".
C        "NDIM" is the number of bytes in the input string.
C        LENG is returned as the position of the last non-blank
C        before the scan is terminated by NDIM or a null. If NDIM
C        is invalid, LENG returns as -1.
C 
C        L. Weissman
C        Department of Biology, Gilmer Hall
C        University of Virginia
C        McCormick Road
C        Charlottesville, VA  22901
C
      INTEGER STR(*),BLANK,NULL
      DATA BLANK,NULL/32,0/ 
C 
      LENG=-1
      IF(NDIM .LE. 0)GO TO 20 
C
      LENG=0
      DO 10 I=1,NDIM
      K=IBYTE(STR,I)
      IF(K .EQ. NULL)GO TO 20
10    IF(K .NE. BLANK)LENG=I
20    RETURN
      END 

      SUBROUTINE LINE(X,XT,Y,YT,N,J,L)
COPYRIGHT (C) CERRITOS COMPUTER SERVICES, INC. 1976 
C 
C        X       ARRAY OF UNSCALED ABCISSA VALUES 
C        XT      XMIN AND DX PROVIDED BY "SCALE"
C        Y       ARRAY OF UNSCALED ORDINATE VALUES
C        YT      YMIN AND DY PROVIDED BY "SCALE"
C        N       NUMBER OF POINTS IN THE ARRAY
C        J       >0, SYMBOL AND LINE CONNECTING 
C                =0, LINE ONLY
C                <0, SYMBOL ONLY
C                IF |J|>1 PRINT THE SYMBOL ONLY AT EVERY J'TH POINT,
C                STARTING WITH THE FIRST. 
C        L       NUMBER OF SYMBOL ,SEE SYMBOL ROUTINE FOR LIST
C                0-17 ARE CENTERED SPECIAL SYMBOLS
C                32-127 ARE THE NORMAL ASCII CODES
C 
      REAL X(N),Y(N),XT(2),YT(2)
      IF(N.LT.1)GOTO 110
      AL=L
      IF(AL.LT.0. .OR. AL.GT.127.)AL=63.
      XMIN=XT(1)
      DX=XT(2)
      YMIN=YT(1)
      DY=YT(2)
      I3=3
      IF(J.GE.0)I3=2
      I4=3
      DO 100 I1=1,N 
      X1=(X(I1)-XMIN)/DX
      Y1=(Y(I1)-YMIN)/DY
      CALL PLOT(X1,Y1,I4) 
      I4=I3 
      IF(J .EQ. 0)GO TO 100 
      IF(MOD(I1-1,J) .EQ. 0)CALL SYMBL(X1,Y1,0.14,AL,0.0,-1)
100   CONTINUE
110   RETURN
      END 
      SUBROUTINE LINE2 (IPEN,NPNT,IPEN2,TITLE,RECIP,IFILE,INDX,INDY) 
C 
C 
C.....PLOTS THE DATA
C 
C 
      CHARACTER*80 TITLE, DUM 
      DIMENSION DATA(100)
      INTEGER LARAY(40)
      EQUIVALENCE (LARAY(1),DUM)
      LOGICAL RECIP, DEBUG, LARGEX, START, YES
      COMMON /SCALE/ XMIN, YMIN, XSCALE, YSCALE, DELTAX, DELTAY,
     + XPOS, YPOS, XM, YM, TZ, DEBUG, DASHL, ISPEN, ISYMB, SSIZE
      COMMON /STEPS/ SMARKX, SMARKY, OFFSET
      COMMON /LAST/ RLASTX, RLASTY
C 
C 
C.....GENERATE INDICES FOR FILE READING
      IF (INDX.EQ.0) THEN
         INDX = 1
         INDY = 2
      ENDIF
      MAXIND = MAX(INDX,INDY)
      DUM=TITLE
      LARGEX=.FALSE.
      START=.TRUE.
      OLD=.FALSE.
      WRITE (7,'('' TITLE: '',A80)') TITLE
C
C.....PLOT SAMPLE LINE IN TITLE BLOCK
      IF (.NOT.DEBUG) CALL NEWPEN(ABS(IPEN))
      IF (TITLE.EQ.'             ') GOTO 5 
      X=XPOS
      Y=YPOS
      IF (ISPEN.NE.0) THEN
         IF(.NOT.DEBUG)CALL NEWPEN(ABS(ISPEN))
         IF(.NOT.DEBUG)CALL SYMBOL(X-0.5,Y+TZ/2.0,SSIZE,ISYMB,0.0,-1)
         IF(.NOT.DEBUG)CALL SYMBOL(X+1.0,Y+TZ/2.0,SSIZE,ISYMB,0.0,-1)
         IF(.NOT.DEBUG)CALL NEWPEN(ABS(IPEN))
      ENDIF
      IF (IPEN.GT.0) THEN 
          IF (.NOT.DEBUG) CALL PLOT (X-0.5,Y+TZ/2.0,3)
          IF (.NOT.DEBUG) CALL PLOT (X+1.0,Y+TZ/2.0,2)
      ELSEIF (IPEN.LT.O) THEN
          IF (.NOT.DEBUG) CALL PLDASH (X-0.5,Y+TZ/2.0,0.0)
          IF (.NOT.DEBUG) CALL PLDASH (X+1.0,Y+TZ/2.0,DASHL) 
      ENDIF 
      IF (ISPEN.NE.0.OR.IPEN.NE.0) THEN
          IF (.NOT.DEBUG) CALL NEWPEN (ABS(IPEN2))
          IF (.NOT.DEBUG) CALL SYMBOL (X+1.5,Y,TZ,LARAY,0.0,80) 
          IF (.NOT.DEBUG) CALL NEWPEN (ABS(IPEN))
      ENDIF
C
C.....UPDATE PLOT POSITION IN TITLE BLOCK
      YPOS=YPOS-2.5*TZ 
C 
C 
C.....THE ACTUAL DATA PLOTTING
C.....
    5 DO 10, I=1, NPNT
          IF (IFILE.EQ.0) THEN
              READ (*,*) X, Y 
          ELSE
              READ (IFILE,*,END=100,ERR=100) (DATA(J),J=1,MAXIND)
              X = DATA(INDX)
              Y = DATA(INDY)
          ENDIF
          WRITE (7,*) X, Y
          Y=Y+OFFS
          IF (RECIP) THEN
              X=((1/X)**2 - (1/XMIN)**2) * XSCALE
          ELSE
              X=(X-XMIN) * XSCALE
          ENDIF
          Y=(Y - YMIN) * YSCALE
          XDUM=X
          YDUM=Y
          CALL XYTEST (X,Y,OLDX,OLDY,START,YES,LARGEX,OLD,IPEN)
          IF (.NOT.YES) GOTO 9
          WRITE (7,'(''PLOTTED: X='',F5.2,''; Y='',F5.2)') X, Y
          RLASTX=X
          RLASTY=Y
          IF (IPEN.GT.0) THEN 
              IF (.NOT.DEBUG) CALL PLOT (X,Y,2) 
          ELSEIF (IPEN.LT.0) THEN
              IF (.NOT.DEBUG) CALL PLDASH (X,Y,DASHL)
          ENDIF
          IF (.NOT.DEBUG.AND.ISPEN.NE.0.AND.X.EQ.XDUM.AND.Y.EQ.YDUM)
     +    THEN
              CALL NEWPEN (ABS(ISPEN))
              CALL SYMBOL (X,Y,SSIZE,ISYMB,0.0,-1)
              IF (IPEN.NE.0.AND.ABS(IPEN).NE.ABS(ISPEN)) 
     +        CALL NEWPEN (ABS(IPEN))
          ENDIF
    9     OLDX = X
          OLDY = Y
   10 CONTINUE
  100 RETURN
      END 
      SUBROUTINE LINPLT(IBUF,NDIM,IOP) 
C 
C        Plot on the printronix printer in graphics mode.
C
C     Parameters: 
C        IOP:  -N   Initialize the plot with output LU=|N|
C              +N   Plot a line buffer N times (repeats)
C               0   Terminate graphics mode
C        IBUF: An integer array containing non-zero (dot) or zero (no 
C              dot). Max length is 792 words.
C        NDIM: The number of words to plot in IBUF. If <1, a clear
C              line is plotted. 
C 
C     Note:
C        The logical unit must be opened with CARRIAGECONTROL=
C        'FORTRAN' (under F77) else the print format must be
C        changed.
C
C     L. Weissman                                   March 1980 (HP)
C     Department of physics, univ. of virginia      Aug 1982 (PDP)
C     McCormick road
C     Charlottesville, VA 22903
C 
      INTEGER IBUF(1)
      BYTE BIT(6),OCTAL5,LINE(132)
      DATA OCTAL5,BIT/5,1,2,4,8,16,32/
C
      IF(IOP)5,60,10
C
C        Initialize the plot
C
5     LU=-IOP
      GO TO 60
C
C        Form the line to be printed
C
10    K=0
      LAST=1
      DO 30 I=1,132
      LINE(I)=64
      DO 20 J=1,6
      K=K+1
      IF(K .GT. NDIM)GO TO 35
      IF(IBUF(K) .EQ. 0)GO TO 20
      LAST=I
      LINE(I)=LINE(I) .OR. BIT(J)
20    CONTINUE
30    IF(LINE(I) .GT. 126)LINE(I)=63
C
C        Print the line
C
35    DO 40 I=1,IOP
40    WRITE(LU,50)OCTAL5,(LINE(J),J=1,LAST)
50    FORMAT(1X,133A1)
C
C        Leave graphics mode
C
60    RETURN
      END

      SUBROUTINE LOCCMD(BUF,JBEG,JFIN,CMDLST,CMDLEN,NDIMC,NCMD)
C
C        Locate a command from among those in a discrete list.
C        Command characters are checked only until a unique match
C        is found.
C        The case of alphabetic characters is ignored.
C
C     Parameters:
C        BUF     Buffer containing characters of the unknown command
C        JBEG    First byte of BUF to be checked
C        JFIN    Last byte of BUF to be checked
C        CMDLST  The list of known commands to be searched for a match.
C        CMDLEN  The number of bytes per command.
C        NDIMC   The number of commands in CMDLST
C        NCMD    Contains one of the following on return:
C                >0  The number of the command matched.
C                 0  No match found.
C                -1  Invalid input parameter. No other action taken.
C                -2  Command not uniquely interpreted.
C
C     Notes:
C        1) Only the value of NCMD is changed on return
C        2) The limit on the number of commands is set by the internal
C           variable MAXACT, the dimension of array ACTIVE.
C        3) LCMASK is used to map upper case alphabetics to lower case
C
C        L. Weissman
C        Molecular Biology Institute
C        405 Hilgard Avenue
C        Los Angeles, California 90024
C        U.S.A.
C
      LOGICAL ACTIVE(30)
      INTEGER CMDLEN
      BYTE BUF(*),CMDLST(CMDLEN,NDIMC),TARGET,TEMP,LCMASK,A,Z
      DATA LCMASK,MAXACT,A,Z/32,30,'A','Z'/
C
C        Check input for dimensioning errors
C
      NCMD=-1
      IF(MIN0(CMDLEN,NDIMC,1+JFIN-JBEG) .LT. 1)GO TO 70
      IF(NDIMC .GT. MAXACT)GO TO 70
C
C        Initialize
C
      DO 10 I=1,NDIMC
10    ACTIVE(I)=.TRUE.
      NACT=NDIMC
C
C        Loop over target characters
C
      I=0
      DO 30 J=JBEG,JFIN
      I=I+1
      IF(I .GT. CMDLEN)GO TO 40
C
C        Map to lower case for comparisons
C
      TARGET=BUF(J)
      IF(TARGET .GE. A .AND. TARGET .LE. Z)TARGET=TARGET .OR. LCMASK
C
C        Loop over active commands
C
      DO 20 ICMD=1,NDIMC
      IF(.NOT. ACTIVE(ICMD))GO TO 20
C
C        Compare target with known command (mapped to lower case)
C
      TEMP=IBYTE(CMDLST,(ICMD-1)*CMDLEN+I)
      IF(TEMP .GE. A .AND. TEMP .LE. Z)TEMP=TEMP .OR. LCMASK
      IF(TEMP .EQ. TARGET)GO TO 20
C
C        No match. Deactivate command.
C
      ACTIVE(ICMD)=.FALSE.
      NACT=NACT-1
20    CONTINUE
C
C        At this point, is the interpretation still ambiguous?
C
      IF(NACT .LT. 2)GO TO 50
C
C        Yes, so get another character from the target.
C
30    CONTINUE
C
C        Still ambiguous and we ran out of target characters.
C
40    NCMD=-2
      GO TO 70
C
C        Unambiguous command.
C
50    DO 60 NCMD=1,NDIMC
      IF(ACTIVE(NCMD))GO TO 70
60    CONTINUE
      NCMD=0
70    RETURN
      END

      FUNCTION LOCST(STR,NSTR,TARGET,NTARG,INIT)
C 
C        Locate a target string "TARGET" in a string "STR".
C        NSTR:  Number of bytes in "STR" 
C        NTARG: Number of bytes in "TARGET" 
C        INIT:  Initial position in "STR" to start the search. 
C               If out of range, 1 is assumed.
C        LOCST: On return, indicates the first byte of the first match.
C               0 = no match found.
C              -1 = invalid input parameter.
C 
C        L. Weissman
C        Department of Biology, Gilmer Hall
C        University of Virginia
C        McCormick Road
C        Charlottesville, VA  22901
C
      IMPLICIT INTEGER (A-Z)
      INTEGER STR(*),TARGET(*)
C 
C        Test the dimensions 
C
      LOCST=-1
      IF(MIN0(NSTR,NTARG) .LT. 1)GO TO 30 
      LOCST=0
C 
C        Test target length
C
      JMIN=1
      IF(INIT .GT. 0 .AND. INIT .LE. NSTR)JMIN=INIT 
      IF(NSTR .LT. NTARG+JMIN-1)GO TO 30
C 
C        Calculate maximum loop index for first position of search 
C
      JMAX=1+NSTR-NTARG 
      DO 20 J=JMIN,JMAX 
      JLESS1=J-1
C 
C        Loop over the target and test for a match.
C
      DO 10 K=1,NTARG 
      IF(IBYTE(STR,JLESS1+K) .NE. IBYTE(TARGET,K))GO TO 20
10    CONTINUE
      LOCST=J 
      GO TO 30
20    CONTINUE
30    RETURN
      END 
	BYTE FUNCTION LOWER(C)
C
C	   Return lower case value of "C".
C	   "C" is not changed by the call.
C	   L. Weissman  July 1984
C
	BYTE C,A,Z
	DATA A,Z/'A','Z'/
	LOWER=C
	IF(C .GE. A .AND. C .LE. Z)LOWER = C + 32
	RETURN
	END
C
C
C
      SUBROUTINE MATCOPY( SOURCE, DEST, IROWS, ICOLS)
C     ===============================================
C
C
      DIMENSION SOURCE( IROWS, ICOLS), DEST( IROWS, ICOLS)
C
C
      DO 999 I = 1, IROWS
        DO 998 J = 1, ICOLS
          DEST( I, J) = SOURCE( I, J)
998      CONTINUE
999      CONTINUE
C
C
      RETURN
C
C
      END
      SUBROUTINE MATDCS (M,A,ANGLE,IPRNT)
C///
C
C     SUBROUTINE FOR CONVERTING A ROTATION MATRIX INTO THE
C     THREE  EULERIAN  ANGLES  WHICH SPECIFY THE DIRECTION
C     COSINES.  FIRST THE POLAR ROTATION ANGLES KAPPA, PHI
C     AND PSI ARE CALCULTATED; THEN FROM  PSI AND  PHI THE
C     DIRECTION  ANGLES  OF  THE  ROTATION  AXIS  WITH THE
C     POSITIVE X,Y AND Z-AXES ARE CALCULATED.  PSI AND PHI
C     ARE BROUGHT IN THE RANGE 0<PHI<PI AND 0<PSI<PI. THIS
C     LEADS TO DIRECTION ANGLES IN THE  RANGE:  0<ANG1<PI,
C     0<ANG2<PI AND PI/2<ANG3<PI.  BY USING
C     AX(ANG1, ANG2, ANG3) = AX(PI-ANG1, PI-ANG2, PI-ANG3)
C     THE  DIRECTION  ANGLES ARE BROUGHT WITHIN THE  RANGE
C     0<ANG1<PI/2,  0<ANG2<PI  AND  0<ANG3<PI. 
C
C\\\
      REAL     ANGLE(3), M(3,3), A(3), EQUI(3)
      PI     = 4.0*ATAN(1.0)
      HPI    = 0.5*PI
C
C     FIRST CALCULATE POLAR ROTATION ANGLES
C     KAPPA, PHI AND PSI
C
      CALL MATKPP(M,A,IPRNT)
      CALL DTR(A)
      ANGLE(2)= A(3)
      ANGLE(1)= ACOS(SIN(A(3))*COS(A(2)))
      RHO     = ACOS(SIN(A(3))*SIN(A(2)))
      ANGLE(3)= PI - RHO
C
      IF(ANGLE(1).GT.HPI) THEN
            DO 150 I=1,3
  150       ANGLE(I)= PI - ANGLE(I)
      ENDIF
      CALL RTD(A)
      CALL RTD(ANGLE)
C
C   PRINT RESULTS
C
      IF(IPRNT.NE.0) WRITE(*,250)
  250 FORMAT(/4X,'ANGLES SPECIFYING DIRECTION COSINES:')
      IF(IPRNT.NE.0) WRITE(*,300) ANGLE
  300 FORMAT(4X,'ANG1,ANG2,ANG3  =',3F9.3)
      RETURN
      END
      SUBROUTINE MATFST(M,A,IPRNT)
C
C   SUBROUTINE FOR CONVERTING A ROTATION MATRIX INTO THE
C   THREE EULERIAN ROTATION ANGLES AS USED BY TONY CROWTHER'S
C   FANTASTIC FAST ROTATION FUNCTION.
C
      REAL             M(3,3), A(3), EQUI(3)
C
      DEL    = 1.0E-5
      UMD    = 1.0-DEL
      BETZER = 0
      BETPI  = 0
      PI     = 4.0*ATAN(1.0)
      TWOPI  = 2.0*PI
C
C   FIRST CALCULATE THE EASY ONE : BETA FROM M(3,3)
C
      IF(M(3,3).GT.UMD) THEN
          A(2) = 0.0
          CB   = 1.0
          SB   = 0.0
      ELSE IF(M(3,3).LT.-UMD) THEN
          A(2) = PI
          CB   =-1.0
          SB   = 0.0
      ELSE
          A(2) = ACOS(M(3,3))
          CB   = M(3,3)
          SB   = SIN(A(2))
      ENDIF
C
C   CHECK IF BETA EQUAL ZERO
C   IF SO THEN ONLY ALFA+GAMMA HAS A PHYSICAL MEANING
C   ALFA GIVEN A VALUE AND GAMMA SET TO ZERO
C
      IF(ABS(SB).LT.DEL.AND.CB.GT.0.0) THEN
          CALL ATANQ(M(2,1),M(1,1),A(1))
          A(3)  = 0.0
          BETZER=   1
          GO TO 5000
      ENDIF
C
C   CHECK IF BETA EQUAL 180 DEGREES
C   IF SO THEN ONLY ALFA-GAMMA A PHYSICAL MEANING
C   ALFA GIVEN A VALUE AND GAMMA SET TO ZERO
C
      IF(ABS(SB).LT.DEL.AND.CB.LT.0.0) THEN
          CALL ATANQ(-M(1,2),M(2,2),A(1))
          A(3)  = 0.0
          BETPI = 1
          GO TO 5000
      ENDIF
C
C   NOW LEFT WITH THE GENERAL CASE
C
      SA = M(2,3)/SB
      CA = M(1,3)/SB
      CALL ATANQ(SA,CA,A(1))
      SG = M(3,2)/SB 
      CG =-M(3,1)/SB
      CALL ATANQ(SG,CG,A(3))
C
C   PRINT RESULTS
C
 5000 IF(IPRNT.NE.0) WRITE(*,5010)
 5010 FORMAT(/4X,'FAST ROT FUNCTION EULERIAN ANGLES:')
      IF(BETZER.EQ.1.AND.IPRNT.NE.0) WRITE(*,5020)
      IF(BETPI .EQ.1.AND.IPRNT.NE.0) WRITE(*,5030)
 5020 FORMAT(4X,'SPECIAL CASE:BETA IS ZERO',
     .       1X,'ONLY (ALFA+GAMMA) PHYSICAL MEANING',
     .       /4X,'ARBITRARILY ALFA GIVEN A VALUE',
     .       1X,'AND GAMMA SET TO ZERO')
 5030 FORMAT(4X,'SPECIAL CASE:BETA IS 180 DEGREES',
     .       1X,'ONLY (ALFA-GAMMA) PHYSICAL MEANING',
     .       /4X,'ARBITRARILY ALFA GIVEN A VALUE',
     .       1X,'AND GAMMA SET TO ZERO')
C
C   CALCULATE EQUIVALENT ANGLES
C
      EQUI(1) = PI+A(1)
      IF(EQUI(1).GT.TWOPI) EQUI(1) = EQUI(1)-TWOPI
      EQUI(2) = TWOPI-A(2)
      EQUI(3) = PI+A(3)
      IF(EQUI(3).GT.TWOPI) EQUI(3) = EQUI(3)-TWOPI
      CALL RTD(A)
      CALL RTD(EQUI)
      IF(IPRNT.NE.0) WRITE(*,5050)A,EQUI
 5050 FORMAT(4X,'ALFA,BETA,GAMMA =',3F9.3,' OR ',3F9.3)
      RETURN
      END
C
C
C
      SUBROUTINE MATINV( A, AI)
C     =========================
C
C
      DIMENSION A( 3, 3), AI( 3, 3)
C
C
      D = DET( A)
C
C
      DO 999 I = 1, 3
        I1 = INCMOD3( I, 1)
        I2 = INCMOD3( I, 2)
        DO 998 J = 1, 3
          J1 = INCMOD3( J, 1)
          J2 = INCMOD3( J, 2)
          AI( J, I) = ( A( I1, J1)*A( I2, J2) - 
     +    A( I1, J2)*A( I2, J1))/D
998    CONTINUE
999    CONTINUE
C
C
      RETURN
C
C
      END
      SUBROUTINE MATKPP (M,K,IPRNT)
C
C   SUBROUTINE FOR CONVERTING A ROTATION MATRIX INTO THE POLAR 
C   ROTATION ANGLES AS DEFINED IN THE FAMOUS PAPER BY ROSSMANN
C   AND BLOW ON THE ROTATION FUNCTION.
C         ROSSMANN, M.G. AND BLOW, D.M. (1962)
C         ACTA CRYSTALLOGR. 15, 24-31 
C
      REAL         M(3,3), K(3), EQUI(3)
C
      DEL   = 1.0E-5
      UMD   = 1.0-DEL
      IAXUP = 0
      IAXDWN= 0
      KAPZER= 0
      KAPPI = 0
      PI    = 4.0*ATAN(1.0)
      TWOPI = 2.0*PI
C
C   FIRST CALCULATE KAPPA FROM TRACE
C
      TRACE = M(1,1) + M(2,2) + M(3,3)
      CK    = 0.5*(TRACE-1.0)
      IF(CK.GT.UMD) THEN
          K(1) = 0.0
          CK   = 1.0
          SK   = 0.0
      ELSE IF(CK.LT.-UMD) THEN
          K(1) = PI
          CK   =-1.0
          SK   = 0.0
      ELSE
          K(1) = ACOS(CK)
          SK   = SIN(K(1))
      ENDIF
C
C   CHECK IF KAPPA IS ZERO
C   IF THIS IS INDEED THE CASE THEN WE HAVE NO ROTATION,
C   BUT IDENTITY OPERATION.
C
      IF(ABS(SK).LT.DEL.AND.CK.GT.0.0) THEN
          K(2)  = 0.0
          K(3)  = 0.0
          KAPZER=   1
          GO TO 5000
      ENDIF
C
C   CHECK IF KAPPA IS 180 DEGREES
C   IF SO,THEN WE HAVE TO FOLLOW A DIFFERENT COURSE FOR FINDING
C   PHI AND PSI THAN IN GENERAL CASE BECAUSE SIN(KAPPA) IS ZERO.
C   MOREOVER PSI IS LIMITED BETWEEN 0 AND 90 DEGREES BECAUSE OF
C   SYMMETRY CONSIDERATIONS I.E. ABS OF COS(PSI) IS SUFFICIENT.
C
      IF(ABS(SK).LT.DEL.AND.CK.LT.0.0) THEN
          CPSSQ = 0.5*(M(2,2)+1.0)
          IF (CPSSQ.LT.0.0) CPSSQ = 0.0
          CPSI  = SQRT(CPSSQ)
          IF(CPSI.GT.UMD) THEN
              K(3) = 0.0
              CPSI = 1.0
              SPSI = 0.0
          ELSE
              K(3) = ACOS(CPSI) 
              SPSI = SIN(K(3))
          ENDIF
C
C   CHECK IF,FOR KAPPA=180, PSI IS ZERO
C
          IF(SPSI.LT.DEL) THEN
              K(2)  = 0.0
              IAXUP =   1
              GO TO 5000
          ENDIF
C
C   CHECK IF,FOR KAPPA=180, PSI IS 90
C   IF SO,THEN PHI ONLY UNIQUE BETWEEN 0 AND 180
C
          IF(CPSI.LT.DEL) THEN
              CTWPH = +M(1,1)
              STWPH = -M(3,1)
              CALL ATANQ(STWPH,CTWPH,TWOPHI)
              K(2)  = 0.5*TWOPHI
              GO TO 5000
          ENDIF
C
C   NOW GENERAL CASE FOR KAPPA=180
C
          F    = 2.0*SPSI*CPSI
          CPHI = +M(1,2)/F
          SPHI = -M(3,2)/F
          CALL ATANQ(SPHI,CPHI,K(2))
          GO TO 5000
      ENDIF
C
C   NOW WE CAN CONSIDER THE VARIOUS CASES WITH KAPPA UNEQUAL 0
C   AND KAPPA UNEQUAL 180
C
C   FIRST CALCULATE PSI
C
      CPSI = (M(3,1)-M(1,3))/(-2.0*SK)
      IF(CPSI.GT.UMD) THEN
          K(3) = 0.0
          CPSI = 1.0
          SPSI = 0.0
      ELSE IF(CPSI.LT.-UMD)THEN
          K(3) = PI
          CPSI =-1.0
          SPSI = 0.0
      ELSE
          K(3) = ACOS(CPSI)
          SPSI = SIN(K(3))
      ENDIF
C
C   CHECK IF PSI IS ZERO BECAUSE IN THAT CASE PHI HAS NO          
C   PHYSICAL MEANING AND IS SET TO ZERO
C
      IF(ABS(SPSI).LT.DEL.AND.CPSI.GT.0.0) THEN
          K(2)  = 0.0
          IAXUP =   1
          GO TO 5000
      ENDIF
C
C   CHECK IF PSI IS 180 DEGREES BECAUSE THEN PHI HAS NO
C   PHYSICAL MEANING AND IS SET TO ZERO
C
      IF(ABS(SPSI).LT.DEL.AND.CPSI.LT.0.0) THEN
          K(2)   = 0.0
          IAXDWN =   1
          GO TO 5000
      ENDIF
C
C   NOW LEFT WITH THE ENTIRE GENERAL CASE
C   AND ONLY PHI STILL TO BE CALCULATED
C
      SPHI = (M(2,1)-M(1,2))/(-2.0*SPSI*SK)
      CPHI = (M(3,2)-M(2,3))/(+2.0*SPSI*SK)
      CALL ATANQ(SPHI,CPHI,K(2))
      GO TO 5000
C
C-------------------------------------------------------------------------
C   THE THREE ROTATION ANGLES SHOULD BE KNOWN
C   PRINT RESULTS WITH WARNINGS FOR SPECIAL CASES
C
 5000 IF(IPRNT.NE.0) WRITE(*,5010)
 5010 FORMAT(/4X,'POLAR ROTATION ANGLES:')
      IF(KAPZER.EQ.1.AND.IPRNT.NE.0) WRITE(*,5020)
      IF(KAPPI .EQ.1.AND.IPRNT.NE.0) WRITE(*,5030)
      IF(IAXUP .EQ.1.AND.IPRNT.NE.0) WRITE(*,5040)
      IF(IAXDWN.EQ.1.AND.IPRNT.NE.0) WRITE(*,5050)
 5020 FORMAT(4X,'KAPPA=0 I.E. NO ROTATION')
 5030 FORMAT(4X,'KAPPA=180 THEN PSI BETWEEN 0 AND 90')
 5040 FORMAT(4X,'PSI =0 I.E. ROTATION AXIS ALONG Y-AXIS',
     .       1X,';PHI MEANINGLESS, SET TO ZERO')
 5050 FORMAT(4X,'PSI=180 I.E. ROTATION AXIS ALONG Y-AXIS',
     .       1X,';PHI MEANINGLESS,SET TO ZERO')
C
C   CALCULATE EQUIVALENT ROTATION ANGLES
C
      EQUI(1) = TWOPI-K(1)
      EQUI(2) = PI + K(2)
      IF(EQUI(2).GT.TWOPI) EQUI(2) = EQUI(2)-TWOPI
      EQUI(3) = PI-K(3)
      CALL RTD(K)
      CALL RTD(EQUI)
      IF(IPRNT.NE.0) WRITE(*,5100)K,EQUI
 5100 FORMAT(4X,'KAPPA,PHI,PSI   =',3F9.3,' OR ',3F9.3)
      RETURN
      END
      SUBROUTINE MATMPY(A,B,C,L,M,N)
C///
* **   MULTIPLY A L*M MATRIX WITH A M*N MATRIX
*
*      [C] = [A] * [B];
*
*      THIS SR STEMS FROM THE PROGRAM  'EULER'
C\\\
      DIMENSION A(L,M),B(M,N),C(L,N)
      DO 1 I = 1,L
        DO 1 J = 1,N
          C(I,J) = 0.
          DO 1 K = 1,M
            C(I,J) = C(I,J) + A(I,K)*B(K,J)
1     CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE MATMULT( A, B, AB)
C     =============================
C
C
      DIMENSION A( 3, 3), B( 3, 3), AB( 3, 3)
C
C
      DO 999 I = 1, 3
        DO 998 J = 1, 3
          SUM = 0.
          DO 997 K = 1, 3
            SUM = SUM + A( I, K) * B( K, J)
997       CONTINUE
          AB( I, J) = SUM
998   CONTINUE
999   CONTINUE
C
C
      RETURN
C
C
      END
      SUBROUTINE MATRB(M,TH,IPRNT)
C
C   SUBROUTINE FOR CONVERTING A ROTATION MATRIX INTO THE THREE
C   EULERIAN ROTATION ANGLES AS DEFINED IN THE FAMOUS PAPER BY
C   ROSSMANN AND BLOW ON THE ROTATION FUNCTION.
C         ROSSMANN, M.G. AND BLOW, D.M. (1962)
C         ACTA CRYSTALLOGR. 15, 24-31 
C
      INTEGER      TH2Z, TH2PI
      REAL         M(3,3), TH(3), EQUI(3)
C
      DEL   = 1.0E-5
      UMD   = 1.0-DEL
      TH2Z  = 0
      TH2PI = 0
      PI    = 4.0*ATAN(1.0)
      TWOPI = 2.0*PI
C
C   START WITH EASIEST MATRIX ELEMENT
C
      IF(M(3,3).GT.UMD) THEN
          TH(2) = 0.0
          STH2  = 0.0
          CTH2  = 1.0
      ELSE IF(M(3,3).LT.-UMD) THEN
          TH(2) = PI
          STH2  = 0.0
          CTH2  =-1.0
      ELSE
          TH(2) = ACOS(M(3,3))
          CTH2  = M(3,3)
          STH2  = SIN(TH(2))
      ENDIF
C
C   CHECK IF THETA-2 IS ZERO  DEGREES
C
      IF(ABS(STH2).LT.DEL.AND.CTH2.GT.0.0) THEN
          CALL ATANQ(M(2,1),M(1,1),TH(1))
          TH(3) = 0.0 
          TH2Z  =   1
          GO TO 5000
      ENDIF
C
C   CHECK IF THETA-2 IS 180 DEGREES
C   IF SO THEN ONLY ALFA-GAMMA HAS PHYSICAL MEANING
C   ARBITRARILY ALFA=ALFA-GAMMA AND GAMMA SET TO ZERO.
C
      IF(ABS(STH2).LT.DEL.AND.CTH2.LT.0.0) THEN
          CALL ATANQ(M(2,1),M(1,1),TH(1))
          TH(3) = 0.0
          TH2PI =   1
          GO TO 5000
      ENDIF
C
C   NOW LEFT WITH GENERAL CASE
C
      STH3 = M(3,1)/STH2
      CTH3 = M(3,2)/STH2
      CALL ATANQ(STH3,CTH3,TH(3))
      STH1 = M(1,3)/STH2
      CTH1 =-M(2,3)/STH2
      CALL ATANQ(STH1,CTH1,TH(1))
C
C   HERE FOR PRINTING RESULTS
C
 5000 IF(IPRNT.NE.0) WRITE(*,5020)
 5020 FORMAT(/4X,'ROSSMANN AND BLOW EULERIAN ANGLES:')
      IF(IPRNT.NE.0.AND.TH2Z.EQ.1) WRITE(*,5040)
 5040 FORMAT(4X,'SPECIAL CASE:THETA-2 (=THETA) IS ZERO',
     .       1X,'ONLY THETA1+THETA2 PHYSICAL MEANING',
     .       /4X,'ARBITRARILY THETA3(=PSI) SET TO ZERO',
     .       1X,'AND THETA1(=PHI) GIVEN A VALUE')
      IF(IPRNT.NE.0.AND.TH2PI.EQ.1) WRITE(*,5060)
 5060 FORMAT(4X,'SPECIAL CASE:THETA2(=THETA) IS 180',
     .       1X,'ONLY (THETA1-THETA3) PHYSICAL MEANING',
     .       /4X,'ARBITRARILY THETA3(=PSI) SET TO ZERO',
     .       1X,'AND THETA1(=PHI) GIVEN A VALUE')
C
C   CALCULATE SET OF ANGLES WHICH IS ENTIRELY EQUIVALENT
C
      EQUI(1) = TH(1) + PI
      IF(EQUI(1).GT.TWOPI) EQUI(1) = EQUI(1)-TWOPI
      EQUI(2) = TWOPI-TH(2)
      EQUI(3) = TH(3)+PI
      IF(EQUI(3).GT.TWOPI) EQUI(3) = EQUI(3)-TWOPI
      CALL RTD(TH)
      CALL RTD(EQUI)
      IF(IPRNT.NE.0) WRITE(*,5100)TH,EQUI
 5100 FORMAT(4X,'THETA 1,2,3 RESP=',3F9.3,' OR ',3F9.3)
      RETURN
      END
*DECK $FMAXHKL 
      SUBROUTINE MAXHKL(A,B,C,ALPHA,BETA,GAMMA,RESMAX)
C///
C
C     SUBROUTINE TO CALCULATE MAXIMUM H, K AND L VALUES FOR 
C     A GIVEN MAXIMUM RESOLUTION.  IN ADDITION THIS ROUTINE
C     CALCULATES THE PARAMETERS NEEDED FOR RAPID CALCULATION
C     OF THE RESOLUTION OF A GIVEN REFLECTION.
C
C     INPUT PARAMETERS: 
C     A, B, C, ALPHA, BETA, GAMMA : UNIT CELL PARAMETERS
C     RESMAX                      : MAXIMUM RESOLUTION 
C                                   (E.G. 1.5 ANGSTROM)
C
C     OUTPUT PARAMETERS: 
C     (THESE ARE TRANSFERRED THROUGH COMMON BLOCKS)
C     IN COMMON / HKLMAX /        : MAXH, MAXK, MAXL
C                                   MAXIMUM H, K AND L VALUES
C     IN COMMON / RESPAR /        : A11, A21, A22, A31, A32, A33
C                                   PARAMETERS TO CALCULATE THE
C                                   RESOLUTION OF A REFLECTION
C
C     THE RESOLUTION CAN BE CALCULATED AS FOLLOWS: 
C
C     DSTAR-SQUARED = IH*IH*A11 + IH*IK*A21 + IH*IL*A31
C                   + IK*IK*A22 + IK*IL*A32 + IL*IL*A33
C     SO:       RES = 1.0/SQRT(DSTAR_SQUARED)
C
C
C     VERSION NOVEMBER, 1985     BAUKE DIJKSTRA
C\\\
      COMMON / RESPAR / A11, A21, A22, A31, A32, A33
      COMMON / HKLMAX / MAXH, MAXK, MAXL
      RAD  =  ATAN(1.0)/45.
      S = ALPHA*RAD
      CA = COS(S)
      SA = SIN(S)
      S = BETA *RAD
      CB = COS(S)
      SB = SIN(S)
      S = GAMMA*RAD
      CG = COS(S)
      SG = SIN(S)
      CAST = (CB*CG - CA)/(SB*SG)
      CBST = (CG*CA - CB)/(SG*SA)
      CGST = (CA*CB - CG)/(SA*SB)
      SAST = SQRT(1.0 - CAST*CAST)
      SBST = SQRT(1.0 - CBST*CBST)
      SGST = SQRT(1.0 - CGST*CGST)
      AST = 1.0/(A*SB*SGST)
      BST = 1.0/(B*SG*SAST)
      CST = 1.0/(C*SA*SBST)
C
C     CALCULATE MATRIX FOR D* TESTS
C
      A11 = AST*AST
      A21 = 2.0*AST*BST*CGST
      A22 = BST*BST
      A31 = 2.0*AST*CST*CBST
      A32 = 2.0*BST*CST*CAST
      A33 = CST*CST
C
C     CALCULATE LIMITS ON H, K, AND L
C
      MAXH = INT(SAST/(AST*RESMAX*SQRT(SAST**2 + (SBST*CG)**2
     .     + (SGST*CB)**2 + 2.0*CAST*(2.0*CBST*CGST + SBST*CB*SGST*CG)
     .     - 2.0*(CGST**2 + CBST**2))))
      MAXK = INT(SBST/(BST*RESMAX*SQRT(SBST**2 + (SAST*CG)**2
     .     + (SGST*CA)**2 + 2.0*CBST*(2.0*CAST*CGST + SAST*CA*SGST*CG)
     .     - 2.0*(CAST**2 + CGST**2))))
      MAXL = INT(SGST/(CST*RESMAX*SQRT(SGST**2 + (SAST*CB)**2
     .     + (SBST*CA)**2 + 2.0*CGST*(2.0*CAST*CBST + SAST*CA*SBST*CB)
     .     - 2.0*(CAST**2 + CBST**2))))
      RETURN
      END
      SUBROUTINE MCFSYM (MCF, NSYM, SM, TS)
C///
C=======================================================================
C
C                         M C F S Y M
C
C     SUBROUTINE TO READ THE SPACEGROUP SYMMETRY INFORMATION FROM
C     THE HEADER RECORDS OF AN  MCF  (MASTER COORDINATE FILE)
C     THE IDENTITY OPERATION IS GENERATED BY THIS SUBROUTINE
C
C=======================================================================
C
C     INPUT PARAMETERS:
C     MCF    : UNIT NUMBER ON WHICH MASTER COORDINATE FILE RESIDES
C
C     OUTPUT PARAMETERS:
C     NSYM   : NUMBER OF SYMMETRY OPERATIONS
C              (INCLUDING IDENTITY OPERATION)
C     SM     : ROTATION ART OF SYMMETRY TRANSFORMATIONS
C              DIMENSION SM(3,3,96)
C     TS     : TRANSLATION PART OF SYMMETRY OPERATION
C              DIMENSION TS(3,96)
C
C
C      NO OTHER SUBROUTINES OR COMMON BLOCKS ARE REQUIRED
C
C=======================================================================
C     VERSION 23 MARCH 1987     BAUKE DIJKSTRA
C=======================================================================
C\\\
      CHARACTER       CARD*80
      DIMENSION       SM(3,3,96), TS(3,96)
C
C     READ INPUT MCF
C     ONLY THE SYMMETRY CARDS ARE NEEDED (NO ATOM CARDS REQUIRED)
C     BUT FIRST INITIALIZE THE SYMMETRY TRANSFORMATIONS
C
      DO 100 K=1,96
      DO 100 I=1,3
  100 TS(I,K)=0.0
      DO 110 K=1,96
      DO 110 J=1,3
      DO 110 I=1,3
  110 SM(I,J,K)=0.0
C
C      UNIT TRANSFORMATION
C
      SM(1,1,1)=1.0
      SM(2,2,1)=1.0
      SM(3,3,1)=1.0
      NSYM=1
C
      WRITE (*,'(/,'' HEADER RECORDS FROM MCF; ONLY SYMMETRY '',
     .''INFORMATION ON MCF WILL BE USED'',/)')
      REWIND MCF
  120 READ(MCF,130,END=150) CARD
  130 FORMAT(A80)
      IF(CARD(1:6).EQ.'ATOM  ') GOTO 150
      WRITE (*,'(1X,A80)') CARD
      IF(CARD(1:5).EQ.'SYMTR') THEN
            READ (CARD,'(5X,I1,I4)') I, ISYM
            ISYM1 = ISYM + 1
            READ (CARD,'(10X,3F10.5,5X,F10.5)')
     .      (SM(I,J,ISYM1),J=1,3), TS(I,ISYM1)
            IF (I.EQ.3) NSYM = NSYM + 1
      ENDIF
      GOTO 120
C
C     ALL INFORMATION OBTAINED FROM MCF
C
  150 CONTINUE
      WRITE (*,'(///,'' NUMBER OF EQUIVALENT POSITIONS IS'',I5,//)')
     .NSYM
      DO 160 K=1,NSYM
      WRITE (*,'(//,'' SYMMETRY OPERATION'',I5)') K
      DO 160 I=1,3
  160 WRITE (*,'(3F10.5,5X,F10.5)') (SM(I,J,K),J=1,3), TS(I,K)
      WRITE (*,'(///)')
      RETURN
      END
*DECK $FMDFHDR
      SUBROUTINE MDFHDR(MDF,MAXR,IBMAX,CELL,IND,KL)
C///
C
C       SUBROUTINE TO READ THE HEADER RECORDS OF AN MDF
C
C     PARAMETERS TO BE TRANSFERRED: 
C
C     1. MDF      : UNIT NUMBER ON WHICH THE MDF RESIDES
C     2. MAXR     : LENGTH OF RBUFR ARRAY
C     3. IBMAX    : LENGTH OF DATA BUFFER ARRAY
C     4. CELL     : CELL(6) CONTAINS CELL DIMENSIONS
C     5. IND      : IND(3) CONTAINS ISLOW, MEDIUM, IFAST
C     6. KL       : KL(5)  CONTAINS KLASS, ICENTR, ISCREW(3)
C
C     OF THESE PARAMETERS ONLY MDF HAS TO BE GIVEN A VALUE
C     BY THE CALLING PROGRAM.
C     MDFHDR WILL READ THE OTHER PARAMETERS FROM THE MDF
C
C\\\
      PARAMETER     (MAXBUF=128, MAXRBF=6)
      DIMENSION     CELL(6), IND(3), KL(5)
      CHARACTER*80  INFO(20), RINFO(MAXRBF), TITLE(MAXBUF)
C
C     NOW START READING AND PRINTING THE INTRODUCTORY RECORDS
C     ON THE INPUT MASTER DATA FILE ON TAPE1.
C
      REWIND  MDF
      WRITE(*,'(///,'' HEADER RECORDS ON MDF: '',/)')
C
C     GENERAL INFORMATION
C
      READ (MDF,END=100) INFO
      DO 5 I = 1, 20
    5 IF(INFO(I)(1:5).NE.'     ') WRITE(*,10) INFO(I)
   10 FORMAT(1X,A80)
      WRITE(*,'(/)')
C
C     BUFFER LENGTHS, SLOW MEDIUM AND FAST CHANGING INDICES
C
      READ (MDF,END=100)   MAXR,IBMAX,ISLOW,MEDIUM,IFAST
      WRITE(*,20) IBMAX,MAXR,ISLOW,MEDIUM,IFAST
   20 FORMAT(/4X,'MAXIMUM LENGTH OF BUFFER ARRAY ON MDF IS:',I5,
     .       /4X,'MAXIMUM LENGTH OF RBUFR  ARRAY ON MDF IS:',I5,
     .       /4X,'ISLOW MEDIUM AND IFAST ON MDF ARE       :',3I5)
      IND(1) = ISLOW
      IND(2) = MEDIUM
      IND(3) = IFAST
      IF(IBMAX.GT.MAXBUF)THEN
            WRITE(*,30)
            STOP 'MAXBUF TOO SMALL'
      ENDIF
      IF(MAXR.GT.MAXRBF)THEN
            WRITE(*,40)
            STOP 'MAXRBF TOO SMALL'
      ENDIF
   30 FORMAT(/,4X,'IBMAX ON MDF LARGER THAN PARAMETER MAXBUF',
     .' IN SUBROUTINE MDFHDR:  STOP!')
   40 FORMAT(/,4X,'MAXR  ON MDF LARGER THAN PARAMETER MAXRBF',
     .' IN SUBROUTINE MDFHDR:  STOP!')
      WRITE(*,'(/)')
C
C     RBUFR ARRAY
C
      READ (MDF,END=100) (RINFO(I),I=1,1,MAXR)
      WRITE(*,50) (RINFO(I),I=1,MAXR)
   50 FORMAT(1X,A80)
      WRITE(*,'(/)')
C
C     TITLE FOR DATA BUFFER ARRAY
C
      READ (MDF,END=100) (TITLE(I),I=1,IBMAX)
      WRITE(*,60) (I,TITLE(I),I=1,IBMAX)
   60 FORMAT(/4X,'LINE NR ',I3,' OF TITLE IS    ',A80)
      WRITE(*,'(/)')
C
C     CELL DIMENSIONS, KLASS, CENTERING, SCREW AXIS INFO
C
      READ (MDF,END=100) CELL,KL
      WRITE(*,70) CELL,KL
   70 FORMAT(/,4X,'CELL DIMENSIONS ON MDF ARE : ',6F10.3,
     .       /,4X,'KLASS ICENTR AND ISCREW ARE: ',5I5)
      WRITE(*,'(/)')
C
C     ALL INTRO RECORDS NOW READ
C     RETURN TO CALLING PROGRAM
C
      RETURN
  100 WRITE(*,'('' PREMATURE END OF INFORMATION ON MDF'',
     .'' IN SUBROUTINE MDFHDR:  STOP!'')')
      STOP ' EOI ON MDF IN MDFHDR'
      END
      SUBROUTINE MINV(A,N,D,L,M)
C 
C     ..................................................................
C 
C        SUBROUTINE MINV
C 
C        PURPOSE
C           INVERT A MATRIX 
C 
C        USAGE
C           CALL MINV(A,N,D,L,M)
C 
C        DESCRIPTION OF PARAMETERS
C           A - INPUT MATRIX, DESTROYED IN COMPUTATION AND REPLACED BY
C               RESULTANT INVERSE.
C           N - ORDER OF MATRIX A 
C           D - RESULTANT DETERMINANT 
C           L - WORK VECTOR OF LENGTH N 
C           M - WORK VECTOR OF LENGTH N 
C 
C        REMARKS
C           MATRIX A MUST BE A GENERAL MATRIX 
C 
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C 
C        METHOD 
C           THE STANDARD GAUSS-JORDAN METHOD IS USED. THE DETERMINANT 
C           IS ALSO CALCULATED. A DETERMINANT OF ZERO INDICATES THAT
C           THE MATRIX IS SINGULAR. 
C 
C        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
C        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
C        STATEMENT WHICH FOLLOWS. 
C 
C     DOUBLE PRECISION A,D,BIGA,HOLD
C 
C        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
C        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
C        ROUTINE. 
C 
C        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO
C        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  ABS IN STATEMENT
C        10 MUST BE CHANGED TO DABS.
C 
C        ...............................................................
C 
      DIMENSION A(1),L(1),M(1)
C 
C        SEARCH FOR LARGEST ELEMENT 
C 
      D=1.0 
      NK=-N 
      DO 80 K=1,N 
      NK=NK+N 
      L(K)=K
      M(K)=K
      KK=NK+K 
      BIGA=A(KK)
      DO 20 J=K,N 
      IZ=N*(J-1)
      DO 20 I=K,N 
      IJ=IZ+I 
10    IF( ABS(BIGA)- ABS(A(IJ))) 15,20,20 
15    BIGA=A(IJ)
      L(K)=I
      M(K)=J
20    CONTINUE
C 
C   INTERCHANGE ROWS
C 
      J=L(K)
      IF(J-K) 35,35,25
25    KI=K-N
      DO 30 I=1,N 
      KI=KI+N 
      HOLD=-A(KI) 
      JI=KI-K+J 
      A(KI)=A(JI) 
30    A(JI) =HOLD 
C 
C   INTERCHANGE COLUMNS 
C 
35    I=M(K)
      IF(I-K) 45,45,38
38    JP=N*(I-1)
      DO 40 J=1,N 
      JK=NK+J 
      JI=JP+J 
      HOLD=-A(JK) 
      A(JK)=A(JI) 
40    A(JI) =HOLD 
C 
C   DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS 
C   CONTAINED IN BIGA)
C 
45    IF(BIGA) 48,46,48 
46    D=0.0 
      RETURN
48    DO 55 I=1,N 
      IF(I-K) 50,55,50
50    IK=NK+I 
      A(IK)=A(IK)/(-BIGA) 
55    CONTINUE
C 
C   REDUCE MATRIX 
C 
      DO 65 I=1,N 
      IK=NK+I 
      HOLD=A(IK)
      IJ=I-N
      DO 65 J=1,N 
      IJ=IJ+N 
      IF(I-K) 60,65,60
60    IF(J-K) 62,65,62
62    KJ=IJ-I+K 
      A(IJ)=HOLD*A(KJ)+A(IJ)
65    CONTINUE
C 
C   DIVIDE ROW BY PIVOT 
C 
      KJ=K-N
      DO 75 J=1,N 
      KJ=KJ+N 
      IF(J-K) 70,75,70
70    A(KJ)=A(KJ)/BIGA
75    CONTINUE
C 
C   PRODUCT OF PIVOTS 
C 
      D=D*BIGA
C 
C   REPLACE PIVOT BY RECIPROCAL 
C 
      A(KK)=1.0/BIGA
80    CONTINUE
C 
C   FINAL ROW AND COLUMN INTERCHANGE
C 
      K=N 
100   K=(K-1) 
      IF(K) 150,150,105 
105   I=L(K)
      IF(I-K) 120,120,108 
108   JQ=N*(K-1)
      JR=N*(I-1)
      DO 110 J=1,N
      JK=JQ+J 
      HOLD=A(JK)
      JI=JR+J 
      A(JK)=-A(JI)
110   A(JI) =HOLD 
120   J=M(K)
      IF(J-K) 100,100,125 
125   KI=K-N
      DO 130 I=1,N
      KI=KI+N 
      HOLD=A(KI)
      JI=KI-K+J 
      A(KI)=-A(JI)
130   A(JI) =HOLD 
      GO TO 100 
150   RETURN
      END 
*DECK $FMNXIND
      SUBROUTINE MNXIND (CELL, KLASS, RESMAX)
C///
C
C     SUBROUTINE TO CALCULATE MINIMUM AND MAXIMUM H, K AND L
C     VALUES FOR A GIVEN MAXIMUM RESOLUTION AND LAUE  CLASS.
C
C     INPUT PARAMETERS:
C     CELL (6)                    : UNIT CELL PARAMETERS
C                                   (IN ANGSTROMS AND DEGREES)
C     KLASS                       : LAUE CLASS (SEE MDF DESCR.)
C     RESMAX                      : MAXIMUM RESOLUTION
C                                   (E.G. 1.5 ANGSTROM)
C
C     OUTPUT PARAMETERS (TRANSFERRED THROUGH COMMON BLOCKS) :
C
C     IN COMMON / HKLMNX /        : MINH, MAXH, MINK, MAXK, MINL, MAXL
C
C     VERSION MARCH 19, 1987        BAUKE DIJKSTRA
C\\\
      DIMENSION         CELL(6)
      COMMON / HKLMNX / MINH, MAXH, MINK, MAXK, MINL, MAXL
      RAD  =  ATAN(1.0)/45.
      S  = CELL(4)*RAD
      CA = COS(S)
      SA = SIN(S)
      S  = CELL(5) *RAD
      CB = COS(S)
      SB = SIN(S)
      S  = CELL(6)*RAD
      CG = COS(S)
      SG = SIN(S)
      CAST = (CB*CG - CA)/(SB*SG)
      CBST = (CG*CA - CB)/(SG*SA)
      CGST = (CA*CB - CG)/(SA*SB)
      SAST = SQRT(1.0 - CAST*CAST)
      SBST = SQRT(1.0 - CBST*CBST)
      SGST = SQRT(1.0 - CGST*CGST)
      AST = 1.0/(CELL(1)*SB*SGST)
      BST = 1.0/(CELL(2)*SG*SAST)
      CST = 1.0/(CELL(3)*SA*SBST)
C
C     CALCULATE LIMITS ON H, K, AND L
C
      MAXH = INT(SAST/(AST*RESMAX*SQRT(SAST**2 + (SBST*CG)**2
     .     + (SGST*CB)**2 + 2.0*CAST*(2.0*CBST*CGST + SBST*CB*SGST*CG)
     .     - 2.0*(CGST**2 + CBST**2))))
      MAXK = INT(SBST/(BST*RESMAX*SQRT(SBST**2 + (SAST*CG)**2
     .     + (SGST*CA)**2 + 2.0*CBST*(2.0*CAST*CGST + SAST*CA*SGST*CG)
     .     - 2.0*(CAST**2 + CGST**2))))
      MAXL = INT(SGST/(CST*RESMAX*SQRT(SGST**2 + (SAST*CB)**2
     .     + (SBST*CA)**2 + 2.0*CGST*(2.0*CAST*CBST + SAST*CA*SBST*CB)
     .     - 2.0*(CAST**2 + CBST**2))))
C
C     MINIMUM VALUES FOR H, K AND L ARE A FUNCTION OF THE
C     SPACEGROUP CLASS
C
      MINH = 0
      MINK = 0
      MINL = 0
      IF(KLASS.EQ.1) THEN
            MINK = -MAXK
            MINL = -MAXL
      ELSE IF(KLASS.EQ. 2) THEN
            MINL = -MAXL
      ELSE IF(KLASS.EQ. 6) THEN
            MINK = -MAXK
      ELSE IF(KLASS.EQ. 7) THEN
            MINK = -MAXK
            MINL = -MAXL
      ELSE IF(KLASS.EQ. 8) THEN
            MINL = -MAXL
      ELSE IF(KLASS.EQ. 9) THEN
            MINL = -MAXL
      ELSE IF(KLASS.EQ.10) THEN
            MINK = -MAXK
            MINL = -MAXL
      ELSE IF(KLASS.EQ.15) THEN
            MINL = -MAXL
      ELSE IF(KLASS.EQ.16) THEN
            MINK = -MAXK
      ENDIF
      RETURN
      END
C-------------------------------------------------------------------
C
C     STRLIB: String handling library for FORTRAN programs
C
C     Version for VAX/VMS     L. Weissman    March 1984
C
C     Notes:
C        1) Byte-oriented routines are coded in terms of the two
C           fundamental routines IBYTE (numeric value of a byte) and
C           MOVBYT (move a byte).
C        2) Bit-oriented routines are SETBIT (set a bit) and IFBIT
C           (test a bit).
C
C-------------------------------------------------------------------
C
      SUBROUTINE MOVBYT(S1,I,S2,J)
C 
C        Move a byte from position I of string S1
C        to position J of string S2. Nothing happens if I,J < 1.
C 
C        L. Weissman
C        Department of Biology, Gilmer Hall
C        University of Virginia
C        McCormick Road
C        Charlottesville, VA  22901
C
      BYTE S1(*),S2(*)
      IF(MIN0(I,J) .LT. 1)RETURN
      S2(J)=S1(I)
      RETURN
      END

      SUBROUTINE MPRIN(A,N,M,MODE,W,LU) 
C 
C        Print out a matrix 
C 
C        A:  The matrix
C        N:  # rows
C        M:  # columns 
C        MODE: storage mode. 0=general (column order), 1=symmetric
C              (upper triangle in column order), 2=diagonal 
C        W:  A work vector of length M 
C        LU: Logical unit for printing
C 
C        L. Weissman
C        Department of Biology, Gilmer Hall
C        University of Virginia
C        McCormick Road
C        Charlottesville, VA  22901
C
      REAL A(*),W(*)
C 
      WRITE(LU,5)MODE 
5     FORMAT(/10X,'---  Begin MPRINT  --- MODE=',I2)
      IF(N)120,120,10 
10    IF(M)120,120,15 
15    DO 100 I=1,N
      DO 90 J=1,M 
      W(J)=0.0
      IF(MODE-1)70,40,20
C 
C        Diagonal storage 
C 
20    IF(I-J)90,30,90 
30    K=I 
      GO TO 80
C 
C        Symmetric storage
C 
40    IF(I-J)50,60,60 
50    K=I+(J*J-J)/2 
      GO TO 80
60    K=J+(I*I-I)/2 
      GO TO 80
C 
C        General storage
C 
70    K=N*(J-1)+I 
80    W(J)=A(K) 
90    CONTINUE
100   WRITE(LU,110)I,(W(J),J=1,M) 
110   FORMAT(1X,I2,10(1PE12.4)/(3X,10(1PE12.4)))
120   WRITE(LU,130) 
130   FORMAT(10X,'---  End MPRINT  ---'/) 
      RETURN
      END 
      SUBROUTINE NEWPEN(IPEN)                                           
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C SELECTS THE SPECIFIED PEN                                             
C                                                                       
C   PARAMETERS :                                                        
C                                                                       
C   IPEN = PEN NUMBER TO BE SELECTED                                    
C                                                                       
C *****************************************************************     
C                                                                       
C *** COMMON BLOCK                                                      
C                                                                       
      COMMON /HP/ XMINPU, YMINPU, XORG  , YORG  , XOFF  , YOFF    ,     
     +            XFACT , YFACT , XCNTRL, YCNTRL, XYUNIT          ,     
     +            MDEV  , MUNIT , MPENS , MLU   , MCNTRL, MINIT   ,     
     +            MSHAKE, MPTR  , MCODE , MPLOT , MFLUSH, MSIZE   ,     
     +            MSPOOL, MVER  , MOUT(72),                             
     +            RSI1  , RSI2  , RDI1   , RDI2  , RES1  , RES2         
C                                                                       
C     ***** MPENS  - CONTAINS THE NUMBER OF PENS AVAILABLE              
C                                                                       
C *****************************************************************     
C                                                                       
      DIMENSION IA(4),IBUFF(1),XBUFF(1)                                 
      DATA IA(1)/83/,IA(2)/80/,IA(3)/80/,IA(4)/85/                      
C                 S         P        P        U                         
C                                                                       
C**** IF PEN IS DOWN THEN ISSUE PU COMMAND                              
C                                                                       
      IF (MCODE.EQ.0) GO TO 20                                          
      CALL BUFF(1,IA(3),XBUFF(1),-2)                                    
      MCODE=0                                                           
C                                                                       
  20  CONTINUE                                                          
C                                                                       
C**** ISSUE PEN SELECT COMMAND ( SP )                                   
C                                                                       
      CALL BUFF(1,IA(1),XBUFF(1),2)                                     
C                                                                       
C**** DETERMINE HOW THE PEN WILL BE SELECTED                            
C                                                                       
      IBUFF(1)=MOD(IABS(IPEN)-1,MPENS)+1                                
C                                                                       
C**** ISSUE PEN NUMBER PARAMETER                                        
C                                                                       
      CALL BUFF(4,IBUFF(1),XBUFF(1),1)                                  
      RETURN                                                            
      END                                                               

      SUBROUTINE NUMB(X,Y,HGHT,ZZ,T,N)
C 
C     PRINT A FLOATING POINT NUMBER WITH FORMAT CONTROL 
C 
C        X,Y     COORDINATES OF THE LOWER LEFT CORNER OF THE FIRST
C                DIGIT OF OUTPUT.  X,Y IS IN FLT. PT. INCHES
C        HGHT    HEIGHT OF THE PLOTTED NUMBER IN FLT. PT. 
C        ZZ      FLT. PT. NUMBER TO BE PLOTTED
C        T       ORIENTATION ANGLE FOR THE NUMBER 
C        N       NUMBER OF DECIMAL DIGITS FOR OUTPUT. 
C                  0  TRUNCATE AND PLOT AS INTEGER FOLLOWED BY "."
C                 +N  N DIGITS RIGHT OF DECIMAL PRINTED 
C                 -N  N-1 DIGITS LEFT OF DECIMAL ARE TRUNCATED
C                 -1  TRUNCATE AND PRINT AS AN INTEGER
C 
      INTEGER IBUF(11)
      XT=X
      YT=Y
      Z=ZZ*1.000005 
      IF(ABS(Z) .GT. 1.E10)GO TO 50 
C 
C        Z IS PRINTABLE IN F-TYPE FORMAT
C 
	ENCODE (20,30,IBUF)Z
30    FORMAT(F20.7) 
	J=MIN0(N+13,20)
      DO 40 I=1,J 
	ASC=IBYTE(IBUF,I)
      IF(ASC .LT. 33.)GO TO 40 
      CALL SYMBL(XT,YT,HGHT,ASC,T,-1)
      CALL WHERE(XT,YT,ZT)
40    CONTINUE
      GO TO 60
C 
C        Z IS NOT PRINTABLE (|Z|>1.E+10)
C 
50    CALL SYMBL(XT,YT,HGHT,13H?.?????????? ,N+2) 
60    RETURN
      END 
      SUBROUTINE NUMBER (X,Y,HGT,FPN,THETA,ND)                          
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C DISPLAYS A FLOATING POINT NUMBER IN FORTRAN F-TYPE FORMAT             
C                                                                       
C   PARAMETERS :                                                        
C                                                                       
C   X,Y     = COORDINATES OF THE FIRST CHARACTER TO BE PRODUCED         
C                                                                       
C   HGT     = THE HEIGHT OF THE CHARACTER TO BE PLOTTED                 
C                                                                       
C   FPN     = THE FLOATING POINT NUMBER TO BE CONVERTED AND PLOTTED     
C                                                                       
C   THETA   = THE ANGLE AT WHICH THE NUMBER IS TO BE PLOTTED            
C                                                                       
C   ND      = THE PRECISION OF THE NUMBER CONVERSION                    
C                                                                       
C *****************************************************************     
C                                                                       
      DIMENSION IARAY(20),JARAY(20)                                     
      DATA MAXN/7/,MINUS/45/,IDECPT/46/,NULL/48/                        
C                                                                       
C**** INITIALIZE THE ARRAY WHICH WILL RECEIVE ASCII BYTES               
C                                                                       
      DO 10 I = 1,20                                                    
          IARAY(I)=0                                                    
   10   CONTINUE                                                        
C                                                                       
C**** SET UP LOCAL VARIABLES AND CONSTANTS                              
C                                                                       
      XP=X                                                              
      YP=Y                                                              
C                                                                       
C**** CHECK FOR VALID X,Y                                               
C                                                                       
      XLOW=-32767.0                                                     
      XHIGH=32767.0                                                     
      YLOW=XLOW                                                         
      YHIGH=XHIGH                                                       
      IF (XP .LT. XLOW) XP=XLOW                                         
      IF (XP .GT. XHIGH) XP=XHIGH                                       
      IF (YP .LT. YLOW) YP=YLOW                                         
      IF (YP .GT. YHIGH) YP=YHIGH                                       
      H=HGT                                                             
C                                                                       
C**** CHECK FOR VALID CHARACTER HEIGHT                                  
C                                                                       
      IF (H .GT. 32767.0) H=32767.0                                     
      FPV=FPN                                                           
C                                                                       
C**** CREATE MODULO 360 ANGLE                                           
C                                                                       
      TH=THETA-(AINT(THETA/360.0)*360.0)                                
      N=ND                                                              
      JPTR=1                                                            
      IF (N-MAXN) 30,30,20                                              
   20 N=MAXN                                                            
   30 IF (N+MAXN) 40,50,50                                              
   40 N=-MAXN                                                           
C                                                                       
C**** INSERT MINUS SIGN IF NEGATIVE NUMBER.                             
C                                                                       
   50 IF (FPV) 60,70,70                                                 
   60 JARAY(JPTR)=MINUS                                                 
      JPTR=JPTR+1                                                       
C                                                                       
C**** MN LOCATES EXPONENT VALUE FOR ROUNDING OF THE NUMBER.             
C                                                                       
   70 MN=-N                                                             
      IF (N) 80,90,90                                                   
   80 MN=MN-1                                                           
C                                                                       
C**** ROUND INPUT NUMBER, SET POSITIVE, DETERMINE CHARACTERISTIC +1.    
C                                                                       
   90 FPV=ABS(FPV)+(0.5*10.**MN)                                        
      I=ALOG(FPV)*0.43429448+1.0                                        
      ILP=I                                                             
      IF (N+1) 100,110,110                                              
  100 ILP=ILP+N+1                                                       
C                                                                       
C**** IF NUMBER < 1.0, INSERT A ZERO TO LEFT OF DECIMAL POINT.          
C                                                                       
  110 IF (ILP) 120,120,130                                              
  120 JARAY(JPTR)=NULL                                                  
      JPTR=JPTR+1                                                       
      GO TO 150                                                         
C                                                                       
C**** PROCESS DIGITS TO LEFT OF DECIMAL POINT.                          
C                                                                       
  130 DO 140 J = 1,ILP                                                  
          K=FPV*10.**(J-I)                                              
          K1=K+48                                                       
          JARAY(JPTR)=K1                                                
          JPTR=JPTR+1                                                   
          FPV=FPV-(FLOAT(K)*10.**(I-J))                                 
  140   CONTINUE                                                        
  150 IF (N) 190,160,160                                                
C                                                                       
C**** INSERT DECIMAL POINT.                                             
C                                                                       
  160 JARAY(JPTR)=IDECPT                                                
      JPTR=JPTR+1                                                       
      IF (N) 190,190,170                                                
C                                                                       
C**** PROCESS DIGITS TO RIGHT OF DECIMAL POINT.                         
C                                                                       
  170 DO 180 J = 1,N                                                    
          K=FPV*10.0                                                    
          K1=K+48                                                       
          JARAY(JPTR)=K1                                                
          JPTR=JPTR+1                                                   
          FPV=FPV*10.-FLOAT(K)                                          
  180   CONTINUE                                                        
  190 JPTR=JPTR-1                                                       
      CALL ZZPACK(JARAY,IARAY,JPTR)                                     
C                                                                       
C**** PLOT THE CHARACTER STRING                                         
C                                                                       
  220 CALL SYMBOL(XP,YP,H,IARAY,TH,JPTR)                                
      RETURN                                                            
      END                                                               
      SUBROUTINE NUMTXT(XL,XXSF,IXL,YL,YYSF,IYL)
C DEFINE /ZCTAX/ FOR FXY-ROUTINES 
C INITIAL PRESETTING BY INIAX 
      DIMENSION IXL(8),IYL(8) 
      COMMON /ZCTAX/NXL,IXLAB(8),XLEFT,XSF,NYL, 
     *              IYLAB(8),YLOW,YSF,QXAX,QYAX 
C LENGTH OF IXLAB,IYLAB MACH. DEP.  *************** 
C------- IN SUBROUTINE : PRESETTING BY INIAX( BY DATA STATEMENTS) 
C------- IN FXY : BY ZINI WHICH OVERRIDES THE INIAX-PRESETTING
      LOGICAL QXAX,QYAX 
CBEGIN X-AXIS 
      IF(XL.NE.999.)         GO TO 10 
C BACK TO DEFAULT 
      QXAX=.FALSE.
      XLEFT=1.0 
                             GO TO 20 
   10 CONTINUE
      QXAX=.TRUE. 
      XLEFT=XL
      XSF=XXSF
   20 CONTINUE
      CALL CNCHAR(IXL,NXL,IXLAB)
CEND   X-AXIS 
CBEGIN Y-AXIS 
      IF(YL.NE.999.)         GO TO 30 
C BACK TO DEFAULT 
      QYAX=.FALSE.
      YLOW =1.0 
                             GO TO 40 
   30 CONTINUE
      QYAX=.TRUE. 
      YLOW=YL 
      YSF=YYSF
   40 CONTINUE
      CALL CNCHAR(IYL,NYL,IYLAB)
CEND   Y-AXIS 
      RETURN
      END 
      SUBROUTINE OFFSET(XSET,XFCT,YSET,YFCT)                            
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C SETS SPECIAL OFFSETS AND SCALE FACTORS                                
C                                                                       
C   PARAMETERS :                                                        
C                                                                       
C   XSET  = X OFFSET FACTOR                                             
C                                                                       
C   XFCT  = X SCALE FACTOR                                              
C                                                                       
C   YSET  = Y OFFSET FACTOR                                             
C                                                                       
C   YFCT  = Y SCALE FACTOR                                              
C                                                                       
C *****************************************************************     
C                                                                       
C *** COMMON BLOCK                                                      
C                                                                       
      COMMON /HP/ XMINPU, YMINPU, XORG  , YORG  , XOFF  , YOFF    ,     
     +            XFACT , YFACT , XCNTRL, YCNTRL, XYUNIT          ,     
     +            MDEV  , MUNIT , MPENS , MLU   , MCNTRL, MINIT   ,     
     +            MSHAKE, MPTR  , MCODE , MPLOT , MFLUSH, MSIZE   ,     
     +            MSPOOL, MVER  , MOUT(72),                             
     +            RSI1  , RSI2  , RDI1   , RDI2  , RES1  , RES2         
C                                                                       
C     ***** XOFF   - CONTAINS THE CURRENT X OFFSET IN PLOTTER UNITS     
C                                                                       
C     ***** YOFF   - CONTAINS THE CURRENT Y OFFSET IN PLOTTER UNITS     
C                                                                       
C     ***** XFACT  - CONTAINS THE CURRENT X SCALE FACTOR                
C                                                                       
C     ***** YFACT  - CONTAINS THE CURRENT Y SCALE FACTOR                
C                                                                       
C *****************************************************************     
C                                                                       
C**** ASSIGN NEW X OFFSET                                               
C                                                                       
      XOFF=XSET                                                         
C                                                                       
C**** CHECK FOR VALID X OFFSET                                          
C                                                                       
      XLOW=-32767.0                                                     
      XHIGH=32767.0                                                     
      IF (XOFF .LT. XLOW) XOFF=XLOW                                     
      IF (XOFF .GT. XHIGH) XOFF=XHIGH                                   
C                                                                       
C**** ASSIGN NEW X SCALE FACTOR                                         
C                                                                       
      XFACT=XFCT                                                        
C                                                                       
C**** CHECK FOR VALID X SCALE FACTOR                                    
C                                                                       
      IF (XFACT .LE. 0.0) XFACT=1.0                                     
      IF (XFACT .GT. 32767.0) XFACT=32767.0                             
C                                                                       
C**** ASSIGN NEW Y OFFSET                                               
C                                                                       
      YOFF=YSET                                                         
C                                                                       
C**** CHECK FOR VALID Y OFFSET                                          
C                                                                       
      YLOW=XLOW                                                         
      YHIGH=XHIGH                                                       
      IF (YOFF .LT. YLOW) YOFF=YLOW                                     
      IF (YOFF .GT. YHIGH) YOFF=YHIGH                                   
C                                                                       
C**** ASSIGN NEW Y SCALE FACTOR                                         
C                                                                       
      YFACT=YFCT                                                        
C                                                                       
C**** CHECK FOR VALID Y SCALE FACTOR                                    
C                                                                       
      IF (YFACT .LE. 0.0) YFACT=1.0                                     
      IF (YFACT .GT. 32767.0) YFACT=32767.0                             
      RETURN                                                            
      END                                                               
*DECK $FOPFILE
        SUBROUTINE OPFILE(NFIL,ISTAT,IFORM,QUESTN)
C///
C       SUBROUTINE TO OPEN A FILE
C
C       INPUT PARAMETERS:
C       NFIL     = UNIT NUMBER
C       ISTAT    = STATUS PARAMETER
C       IFORM    = FORMAT PARAMETER
C       QUESTN   = CHARACTER STRING TO BE PRINTED WHEN
C                  ASKING FOR THE NAME OF THE FILE
C
C
C      SUBROUTINE HISTORY
C      ==================
C    RECEIVED FROM BAUKE DIJKSTRA - VAX VERSION - DEC 1985.
C
C=========================================================================
C      VERSION 23 DECEMBER 1985
C=========================================================================
C\\\
        CHARACTER*80  FILNAM,QUESTN,ISTAT,IFORM
        WRITE(*,'(1X,A)') QUESTN
        READ (*,'(A)')    FILNAM
        OPEN(UNIT=NFIL,FILE=FILNAM,STATUS=ISTAT,FORM=IFORM,ERR=100)
        INQUIRE(UNIT=NFIL,NAME=FILNAM)
        WRITE(*,'('' FILE OPENED: '',A,/)') FILNAM
        RETURN
  100   WRITE(*,'('' FILE COULD NOT BE OPENED: ERROR?:'',A)')
     .  FILNAM
        CALL ABORT('ERROR IN SUBROUTINE OPFILE')
        END
C
C
C
      SUBROUTINE PACK(IH,IK,IL,IHKLI)
C     ===============================
C
C
C---- Packs ih,ik,il into one word  -  uses 30 bits of that word
C     should therefore work on all computers with word length of
C     32 bits and more.
C     Limitation is that maximum absolute value of any one index
C     is 511 .  this means  2.0  angstrom resolution for a  1000
C     angstrom axis, which is sufficient in most cases.
C
C
C     .. Scalar Arguments ..
      INTEGER IH,IHKLI,IK,IL
C     ..
C     .. Scalars in Common ..
      INTEGER IFAC,IFAC2,IPLUS,LUNIN,LUNOUT
C     ..
C     .. External Subroutines ..
      EXTERNAL ABORT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     ..
C     .. Common blocks ..
      COMMON /AINOUT/LUNIN,LUNOUT
      COMMON /PCKFCT/IPLUS,IFAC,IFAC2
C     ..
C     .. Data statements ..
      DATA IPLUS,IFAC,IFAC2/512,1024,1048576/
C     ..
C
C
      IF (ABS(IH).LT.512) THEN
        IF (ABS(IK).LT.512) THEN
          IF (ABS(IL).LT.512) THEN
C
            IHKLI = (IH+IPLUS)*IFAC2 + (IK+IPLUS)*IFAC + (IL+IPLUS)
            RETURN
          END IF
        END IF
      END IF
C
      WRITE (LUNOUT,FMT=6000) IH,IK,IL
      CALL ABORT('ERROR in PACK: IH,IK or IL out of bounds')
C
C---- Format statements
C
 6000 FORMAT (' ERROR in indices: IABS(IH, IK, IL) TOO LARGE: ',3I6)
C
C
      END

      SUBROUTINE PARSE(BUF,NDIMB,JBEG,JFIN,NDIMJ,NJ)
C
C        Parse out fields from a command line
C
C     Parameters:
C        BUF    Input buffer
C        NDIMB  Number of bytes in BUF
C        JBEG   Integer buffer to accept pointers to start of field
C        JFIN   Integer buffer to accept pointers to end of field
C        NDIMJ  Dimension of arrays JBEG and JFIN
C        NJ     Number of fields found. If -1, invalid parameters
C               detected and no parsing is performed
C
C     Notes:
C        1) Delimiters are of the following types:
C           a) "Hard" delimiters are comma(,),semicolon(;),colon(:),
C               slash(/)
C           b) "Soft" delimiters are blank and tab
C           c) "Terminators" are exclamation point(!),pound(#) and null
C        2) Any type of delimiter separates fields, but only multiple
C           hard delimiters are interpreted as defaulted fields.
C        3) Defaulted fields, those with no characters other than
C           white space between delimiters, are returned with pointers
C           set to zero in JBEG and JFIN.
C        4) JBEG, JFIN, and NJ are altered by this routine.
C        5) IPREV preserves the type of previous character:
C                 -1   not a delimiter
C                  0   soft delimiter
C                 +1   hard delimiter
C        6) Refer to LW notebook XVII p.106
C
C        L. Weissman
C        Molecular Biology Institute
C        405 Hilgard Avenue
C        Los Angeles, California 90024
C        U.S.A.
C
      BYTE BUF(*),DELIM(6),TERM(3),T
      INTEGER JBEG(*),JFIN(*)
      DATA NDELIM,NSOFT,DELIM/6,2,' ',9,',',';',':','/'/
      DATA NTERM,TERM/3,'!','#',0/
C
C        Check input parameters for error return
C
      NJ=-1
      IF(MIN0(NDIMB,NDIMJ) .LT. 1)GO TO 60
C
C        Initialize
C
      DO 10 J=1,NDIMJ
      JBEG(J)=0
10    JFIN(J)=0
      IPREV=1
      NJ=0
C
C        Loop over characters in input buffer
C
      DO 80 J=1,NDIMB
      T=BUF(J)
C
C        Look for a terminator
C
      DO 20 ITERM=1,NTERM
      IF(T .EQ. TERM(ITERM))GO TO 90
20    CONTINUE
C
C        Look for a delimiter
C
      DO 30 IDELIM=1,NDELIM
      IF(T .EQ. DELIM(IDELIM))GO TO 50
30    CONTINUE
C
C        Not a delimiter
C
      IF(IPREV .LT. 0)GO TO 40
      IF(NJ .EQ. NDIMJ)GO TO 90
      NJ=NJ+1
      JBEG(NJ)=J
40    JFIN(NJ)=J
      IPREV=-1
      GO TO 80
C
C        A delimiter found
C
50    ICUR=0
      IF(IDELIM .GT. NSOFT)ICUR=1
      IF(IPREV .LT. 1 .OR. ICUR .LT. 1)GO TO 60
      IF(NJ .EQ. NDIMJ)GO TO 90
      NJ=NJ+1
60    IF(ICUR .EQ. 0 .AND. IPREV .LT. 1)GO TO 70
      IPREV=1
      GO TO 80
70    IPREV=0
80    CONTINUE
C
C        End of loop over buffer
C
90    RETURN
      END
C     =======================================
      SUBROUTINE PGDEFN(NAMPG,NSYMP,NSYM,RSMT)
C     ========================================
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
C   7 pg4    4/m        hkl:h>=0, l>=0 with k>=0 if  h=0  and
C   8 pg422 4/mmm       hkl:h>=0, k>=0, l>=0            89..
C  10 pg3     3bar      hkl:h>=0, k>0  00l:l>0         143..
C  11 pg312  3/m        hkl:h>=0, k>=0 with k<=h if l>=0  and
C  11 pg321  3/m        hkl:h>=0, k>=0 with k<=h if l>=0  and
C  12 pg6    6/m        hkl:h>=0, k>=0, l>=0 with k=0 if  h=0
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
C     ..
C     .. Array Arguments ..
      REAL RSMT(4,4,96)
C     ..
C     .. Scalars in Common ..
      INTEGER ICENTR,IFAST,INTER,ISLOW,IVERSN,KLASS,MAXB,MAXR,NCENT,
     +        NEZONE
      CHARACTER INXWIN*6,STROUT*400
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
      CHARACTER*1 ABC(3)
C     ..
C     .. External Subroutines ..
      EXTERNAL PUTLIN
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,NINT
C     ..
C     .. Common blocks ..
      COMMON /OUTSTR/STROUT,INXWIN
      COMMON /CP/CPROJ(3,20),NCENT
      COMMON /EPS/EPZONE(4,20),NEZONE
      COMMON /MDFPAR/MAXR,MAXB,CELL(6),ISLOW,INTER,IFAST,KLASS,ICENTR,
     +       ISCREW(3),IVERSN
      DATA   ABC / 'A', 'B', 'C' /
C     ..
C     .. Save statement ..
      SAVE
C     ..
C
C
      DO 30 N = 1,NSYM
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
            WRITE (STROUT,FMT='(1X,A,I3,A,I3)')
     +        '  Reciprocal space symmetry operator ',ISM2,' same as ',
     +        ISM1
            CALL PUTLIN('CURWIN')
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
C---- If nrepet = 3 you must have Rhombehedral centring. (icentr = 3)
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
          IF (RSMT(4,4,N).NE.0) THEN
            ISYM = NORIG(N)
          ELSE
            ISYM = NREPP(N)*NSYMP + NORIG(N)
          END IF
C
          DO 90 J = 1,4
            DO 80 I = 1,4
              RSMT(I,J,ISYM) = RJUNK(I,J,N)
   80       CONTINUE
   90     CONTINUE
C
C---- Reset rsmt(4,4,...) = 1.0
C
          RSMT(4,4,ISYM) = 1.0
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
      DO 130 J = 1,NSYMP
C
          NROT(J) = 0
          IRAXIS  = 0
C
C---- Unique rotation axes must be
C     a  b  c  or the lines 1 1 0   or 1 1 1.
C
C---- a
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
     +        RSMT(1,3,J).EQ.1.0)  IRAXIS = 5
C
C---- 1 1 1
C
          IF (RSMT(3,1,J).EQ.1.0 .AND. RSMT(1,2,J).EQ.1.0 .AND.
     +        RSMT(2,3,J).EQ.1.0)  IRAXIS = 5
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
          NAMPG = ' PG23'
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
          NAMPG = 'PG321'
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
          NAMPG = ' PG2C'
          KLASS = 16
        END IF
C
C---- 4 pg2    2/m
C
      ELSE IF (NROTS(JUNIQU).EQ.22) THEN
        NAMPG = '  PG2'
        KLASS = 2
C
C---- 3 pg1     1bar          KLASS 1
C    ?? pg2A    2/m  A unique KLASS 15
C     7 pg4    4/m            KLASS 4
C    10 pg3     3bar          KLASS 6
C    12 pg6    6/m            KLASS 11
C
      ELSE IF (NROTS(JUNIQU).LT.20) THEN
        IF (NROTS(JUNIQU).EQ.11) NAMPG = '  PG1'
        IF (NROTS(JUNIQU).EQ.11) KLASS = 1
        IF (NROTS(JUNIQU).EQ.12) NAMPG = ' PG2A'
        IF (NROTS(JUNIQU).EQ.12) KLASS = 15
        IF (NROTS(JUNIQU).EQ.13) NAMPG = '  PG3'
        IF (NROTS(JUNIQU).EQ.13) KLASS = 6
        IF (NROTS(JUNIQU).EQ.14) NAMPG = '  PG4'
        IF (NROTS(JUNIQU).EQ.14) KLASS = 4
        IF (NROTS(JUNIQU).EQ.16) NAMPG = '  PG6'
        IF (NROTS(JUNIQU).EQ.16) KLASS = 11
      END IF
C
C---- next symm opn
C
      WRITE (STROUT,FMT='(A,5X,I4)')
     +  '  Number of symmetry operators                    ',NSYM
      CALL PUTLIN('CURWIN')
      WRITE (STROUT,FMT='(A,5X,I4)')
     +  '  Number of primitive symmetry operators          ',NSYMP
      CALL PUTLIN('CURWIN')
      WRITE (STROUT,FMT='(A,4X,A5)')
     +  '  The point group for these symmetry operators is ',NAMPG(1:5)
      CALL PUTLIN('CURWIN')
C
      CALL BLANK('CURWIN',1)
      IF (ICENTR.EQ.0) THEN
        STROUT = '  No centering                     (P spacegroups)'
      ELSE IF (ICENTR.EQ.1) THEN
        STROUT = '  Centering around a-axis          (A spacegroups)'
      ELSE IF (ICENTR.EQ.2) THEN
        STROUT = '  Centering around b-axis          (B spacegroups)'
      ELSE IF (ICENTR.EQ.3) THEN
        STROUT = '  Centering around c-axis          (C spacegroups)'
      ELSE IF (ICENTR.EQ.4) THEN
        STROUT = '  Centering on all faces           (F spacegroups)'
      ELSE IF (ICENTR.EQ.5) THEN
        STROUT = '  Body centering                   (I spacegroups)'
      ELSE IF (ICENTR.EQ.6) THEN
        WRITE (STROUT,FMT='(A,A,A,A)')
     +           '  Rhombohedral centering       (R spacegroups with',
     +           '  hexagonal axes)',
     +           '  (NOTE: R-spacegroups with rhombohedral axes have',
     +           '  ICENTR = 0!)'
      END IF
      CALL PUTLIN('CURWIN')
C
C
      ISCR = ISCREW(1) + ISCREW(2) + ISCREW(3)
      IF (ISCR.GT.0) THEN
        CALL BLANK('CURWIN',2)
        WRITE (STROUT,FMT='(A)') '  Screw axes are : '
        CALL PUTLIN('CURWIN')
        DO 170, I = 1,3
          IF (ISCREW(I).GT.0) THEN
            WRITE (STROUT,FMT='(A,I4,A,A1)') '  There is a ', 
     +      ISCREW(I), '-fold screw axis along ', ABC(I)
            CALL PUTLIN('CURWIN')
          ENDIF
  170   CONTINUE
      END IF
C
C
      WRITE (STROUT,FMT='(A)')
     +  ' *** KLASS    : a crystal class name used in MDF files ***'
      CALL BLANK('CURWIN',1)
      CALL PUTLIN('CURWIN')  
C
C---- (int. tables)
C
      CALL BLANK('CURWIN',2)
      IF (KLASS.EQ.1) THEN
       STROUT = '  TRICLINIC       1_BAR (PG1)       sgs  1 '
      ELSE IF (KLASS.EQ.2) THEN
       STROUT = '  MONOCLINIC   I  2/M (PG4) B-UNIQUE  sgs  3 -  5'
      ELSE IF (KLASS.EQ.3) THEN
       STROUT = '  Orthorhombic    MMM (PG222)         sgs 16 - 24'
      ELSE IF (KLASS.EQ.4) THEN
       STROUT = '  TETRAGONAL   I  4/M (PG4)          sgs 75 - 80'
      ELSE IF (KLASS.EQ.5) THEN
       STROUT = '  TETRAGONAL  II  4/MMM (PG422)      sgs 89 - 98 '
      ELSE IF (KLASS.EQ.6) THEN
       STROUT = '  TRIGONAL I  3_BAR (PG3)HEXAGONAL AXES sgs  143-146'
      ELSE IF (KLASS.EQ.7) THEN
       STROUT = '  TRIGONAL II  3_BAR (??) RHOMBOHEDRAL AXES sgs   146'
      ELSE IF (KLASS.EQ.8) THEN
       STROUT = ' Trigonal III  3_BAR1M (PG312)       sgs 149,151,153 '
      ELSE IF (KLASS.EQ.9) THEN
       STROUT = ' Trigonal IV  3_BARM1 (PG321)HEXAGONAL AXES '//
     +    ' sgs 150,152,154,155'
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
      CALL PUTLIN('CURWIN')
C
C---- In this table only the enantiomorphic spacegroups are
C     included. For the other spacgroups (which contain (glide)
C     mirrors or an inversion center) this routine can still be
C     used, but then isym has no meaning anymore.
C
      END
      SUBROUTINE PLACE(AM,NMAX,IRC,X,Y) 
      DIMENSION AM(NMAX,1),IRC(1),X(1),Y(1) 
      COMMON /LL/ LL
      COMMON /CNTR/ IISYM,NNNN,QSTART,QSUP,QPLOT,QKMPLT,QLAB
      COMMON /KONTUR/ MT,NT,NI,IX,IY,IDX,IDY,ISS,IT,IV,NP,N ,JT,PY,CV,
     1 IPT(3,3),INX(8),INY(8),DL,RA,THE,NTP,RAT,H,HTT 
      LOGICAL QSTART,QSUP,QPLOT,QKMPLT,QLAB 
      COMMON /PLFCT/ XFCT,YFCT,X1,Y1,X2,Y2,XXFCT,XB,YB
      LOGICAL TIND
      PY=0.0                                                               00070
C JTB COUNTS NR. OP POINTS OF 1 CONTOURLINE 
      JTB=0                                                                00190
C N INCREMENTED IN XALC 
      N=0                                                                  00110
      RS=SIN(THE)                                                          00080
      RC=COS(THE)                                                          00090
      JT=0
      XM=MT-1                                                              00120
      XT=NT-1                                                              00130
      IXO=IX                                                               00140
      IYO=IY                                                               00150
      ISX=IDX+2                                                            00160
      ISY=IDY+2                                                            00170
      IS=IPT(ISX,ISY)                                                      00180
      ISO=IS                                                               00200
C****** 
      IF(ISO.GT.8) ISO=ISO-8
      IT=0
    5 CALL PLALC(AM,NMAX,IRC) 
      IF (IT+JT.LE.1)        GO TO 49 
C 
      XS=X1 
      YS=Y1 
      X1=X2 
      Y1=Y2 
      X2=XS 
      Y2=YS 
C 
   49 IS=IS+1                                                              00320
      JT=IT                                                                00330
    9 IF(IS.GE.9) IS=IS-8 
      IDX=INX(IS) 
      IDY=INY(IS)                                                          00370
      IX2=IX+IDX                                                           00380
      IY2=IY+IDY                                                           00390
      JTB=JTB+1                                                            00400
C ******* TEST ON MAX. NO. POINTS REMOVED *********** 
   51 IF (ISS)10,10,20                                                     00470
   20 IF (IX-IXO)12,21,12                                                  00480
   21 IF (IY-IYO)12,22,12                                                  00490
   22 IF (IS-ISO)12,23,12                                                  00500
   23 CALL PLALC(AM,NMAX,IRC) 
      GO TO 73                                                             00520
   10 IF (IX2)13,50,13                                                     00530
   13 IF (IX2-MT)19,19,50                                                  00540
   19 IF (IY2)11,50,11                                                     00550
   11 IF (IY2-NT)12,12,50                                                  00560
   12 IF(TIND(IX2,IY2,1))    GO TO 73 
      IF (CV-AM(IX2,IY2))206,206,5
  206 IF (IDX**2+IDY**2-1)213,6,213                                        00580
  213 DCP=(AM(IX,IY)+AM(IX2,IY)+AM(IX,IY2)+AM(IX2,IY2))/4.0 
      IF (DCP-CV)5,217,217                                                 00600
  217 IF (INX(IS-1))214,215,214                                            00610
  214 IX=IX+IDX                                                            00620
      IDX=-IDX                                                             00630
      PY=2.0                                                               00640
      CALL PLALC(AM,NMAX,IRC) 
      IX=IX+IDX                                                            00660
      GO TO 6                                                              00670
  215 IY=IY+IDY                                                            00680
      IDY=-IDY                                                             00690
      PY=2.0                                                               00700
      CALL PLALC(AM,NMAX,IRC) 
      IY=IY+IDY                                                            00720
    6 IF(TIND(IX-1,IY,3))    GO TO 16 
      IF (AM(IX-1,IY)-CV)306,16,16
  306 NP=NP+1                                                              00740
      IRC(NP)=100*IX+IY                                                    00750
   16 IS=IS+5                                                              00760
      IX=IX2                                                               00770
      IY=IY2                                                               00780
      GO TO 9                                                              00790
50    CONTINUE                                                             00800
   65 IF(TIND(IX-1,IY,4))    GO TO 73 
      IF (AM(IX-1,IY)-CV)307,73,73
  307 NP=NP+1                                                              00820
      IRC(NP)=100*IX+IY                                                    00830
   73 CONTINUE
C 
CBEGIN PLOT LAST 2 POINTS 
      IF(N.GE.3)             GO TO 74 
CBEGIN N=1,2
      IF(.NOT.QLAB)          RETURN 
C******** NO LINE ( DIFFERENCE WITH CCNTR ) 
      X2=X1 
      Y2=Y1 
                             GO TO 3000 
CEND    N=1,2 
   74 CONTINUE
C 
C 
      IF(DL.NE.0.)           GO TO 2010 
      CALL PLOT(X1,Y1,2)
      CALL   PLOT(X2,Y2,2)
      IF(QLAB)               GO TO 3000 
                             RETURN 
 2010 CONTINUE
C DL>0.0 DASHED LINE
      DASH=0.1*DL 
      CALL PLDASH(X1,Y1,DASH) 
      CALL PLDASH(X2,Y2,DASH) 
      IF(QLAB)               GO TO 3000 
                             RETURN 
CEND   PLOT LAST 2 POINTS 
C 
 3000 CONTINUE
C PLOT CONTOUR LEVEL LABEL
      ALFA=0. 
      HTNUM=0.2 
C**** PLOT CHAR. RIGHT-JUSTIFIED IN IISYM 
      CALL SYMBOL(X2,Y2,HTNUM,IISYM,ALFA,0) 
                             RETURN 
C 
      END                                                                  01010
C 
      SUBROUTINE PLALC(AM,NMAX,IRC) 
      DIMENSION AM(NMAX,1),IRC(1) 
      COMMON /PLFCT/ XFCT,YFCT,X1,Y1,X2,Y2,XXFCT,XB,YB
      COMMON /KONTUR/ MT,NT,NI,IX,IY,IDX,IDY,ISS,IT,IV,NP,N ,JT,PY,CV,
     1 IPT(3,3),INX(8),INY(8),DL,RA,THE,NTP,RAT,H,HTT 
C COMPUTE COORDINATES OF NEW CONTOUR POINT AND PLOT THE SECONT POINT BEFORE 
      IT=0
      N=N+1                                                                00070
      IF (IDX**2+IDY**2.GT.1)      GO TO 20 
      IF (IDX)10,2,10 
C IDX=0,ABS(IDY)=1  : 
    2 X   =IX 
      Z=IY                                                                 00110
      IY2=IY+IDY                                                           00120
      DY=IDY                                                               00130
      Y   =((AM(IX,IY)-CV)/(AM(IX,IY)-AM(IX,IY2)))*DY+Z 
      GO TO 1000
C ABS(IDX)=1,IDY=0 :  
   10 Y   =IY 
      W=IX                                                                 00170
      DX=IDX                                                               00180
      IX2=IX+IDX                                                           00190
      X   =((AM(IX,IY)-CV)/(AM(IX,IY)-AM(IX2,IY)))*DX+W 
      GO TO 1000
C ABS(IDX)=1,ABS(IDY)=1 : 
   20 IX2=IX+IDX                                                           00220
      IY2=IY+IDY                                                           00230
      W=IX                                                                 00240
      Z=IY                                                                 00250
      DX=IDX                                                               00260
      DY=IDY                                                               00270
      DCP=(AM(IX,IY)+AM(IX2,IY)+AM(IX,IY2)+AM(IX2,IY2))/4.0                00280
      IF(PY.EQ.2.0)          GO TO 21 
      IF(DCP.GT.CV)          GO TO 25 
C PY=2. AND DCP.LE.CV  :  
   21 AL=AM(IX,IY)-DCP                                                     00310
      IF(AL.EQ.0.0)          GO TO 50 
      V=.5*(AL+DCP-CV)/AL 
                             GO TO 51 
   50 V=0.5 
   51 CONTINUE
      X   =V*DX+W 
      Y   =V*DY+Z 
      PY=0.0                                                               00350
      GO TO 1000
   25 IT=1                                                                 00370
      AL=AM(IX2,IY2)-DCP                                                   00380
      V=.5*(AL+DCP-CV)/AL 
      X   =-V*DX+W+DX 
      Y   =-V*DY+Z+DY 
1000  CONTINUE
C 
CBEGIN PLOT SECOND POINT BEFORE 
C 
      IF(N.GT.1)             GO TO 1010 
C N=1 
      Y1=(Y-YB)*YFCT
      X1=(X-XB)*XFCT+Y1*XXFCT 
                             RETURN 
 1010 IF(N.GT.2)             GO TO 1020 
C N=2 
      Y2=(Y-YB)*YFCT
      X2=(X-XB)*XFCT+Y2*XXFCT 
                             RETURN 
 1020 IF(N.GT.3)             GO TO 1030 
C N=3 
      IF(DL.EQ.0.0) CALL PLOT(X1,Y1,3)
      IF(DL.GT.0.0) CALL PLDASH(X1,Y1,0.0)
                             GO TO 1040 
 1030 CONTINUE
C N>3 
CBEGIN
      IF(DL.NE.0.)           GO TO 1035 
C DL=0. : IT=1  CONT LINE 
      CALL PLOT(X1,Y1,2)
                             GO TO 1040 
 1035 CONTINUE
      CALL PLDASH(X1,Y1,0.1*DL) 
CEND
 1040 CONTINUE
      X1=X2 
      Y1=Y2 
      Y2=(Y-YB)*YFCT
      X2=(X-XB)*XFCT+Y2*XXFCT 
C 
CEND   PLOT SECOND POINT BEFORE 
C 
      RETURN                                                               00420
      END 
C 
      SUBROUTINE
     *          PLCAN(AM,M,N,NMAX,CL,D,R,TH,HT,IZX,IRC,X,Y) 
      DIMENSION AM(NMAX,1),IRC(1),X(1),Y(1) 
      COMMON /CNTR/ IISYM,NNNN,QSTART,QSUP,QPLOT,QKMPLT,QLAB
C************* NQ INSTEAF OF N ************** 
      COMMON /KONTUR/ MT,NT,NI,IX,IY,IDX,IDY,ISS,IT,IV,NP,NQ,JT,PY,CV,
     1 IPT(3,3),INX(8),INY(8),DL,RA,THE,NTP,RAT,H,HTT 
      COMMON /PLFCT/ XFCT,YFCT,X1,Y1,X2,Y2,XXFCT,XB,YB
  
      NP=0                                                                 00070
C------------------SEE XRACE FOR SIGN OF HT 
      H=ABS(HT) 
      HTT=HT
      DL=D                                                                 00080
      RA=R                                                                 00090
      THE=TH                                                               00100
      MT=M                                                                 00110
      NT=N                                                                 00120
C 
C /PLFCT/ USED IN PLALC 
C XB,YB PRESET IN PLCNTR
      XFCT=H*RA/(MT-XB) 
      XXFCT=COS(THE)/SIN(THE) 
      YFCT=H*SIN(THE)/(NT-YB) 
C 
      CV=CL                                                                00130
      IF(IZX.NE.0)           GO TO 3
CBEGIN             IZX=0
C IPT : 
C 812 
C 7 3 
C 654 
      IPT(1,1)=8
      IPT(1,2)=1                                                           00160
      IPT(1,3)=2                                                           00170
      IPT(2,1)=7                                                           00180
      IPT(2,3)=3                                                           00190
      IPT(3,1)=6                                                           00200
      IPT(3,2)=5                                                           00210
      IPT(3,3)=4                                                           00220
      IZX=2                                                                00230
103   INX(1)=-1                                                            00240
      INX(2)=-1                                                            00250
      INX(3)=0                                                             00260
      INX(4)=1                                                             00270
      INX(5)=1                                                             00280
      INX(6)=1                                                             00290
      INX(7)=0                                                             00300
      INX(8)=-1                                                            00310
      INY(1)=0                                                             00320
      INY(2)=1                                                             00330
      INY(3)=1                                                             00340
      INY(4)=1                                                             00350
      INY(5)=0                                                             00360
      INY(6)=-1                                                            00370
      INY(7)=-1                                                            00380
      INY(8)=-1                                                            00390
CEND               IZX=0
    3 CONTINUE
C******************* 800 *****************
C********** 
      JMAX=NNNN 
      JMAX=NNNN/2 
      DO 58 J=1,JMAX
   58 IRC(J) = 0
CBEGIN CONTOURLINE STARTS AT BORDER 
      ISS=0                                                                00450
      MT1=MT-1
      DO 110 I=1,MT1                                                       00470
C CV=1 , AM(1,1)=1 , AM(1,2)=2 , AM(2,1)=2
C LEVEL CV NOT FOUND
C IF NEXT TEST IS CHANGED FOR I=1   (LT=LE) THEN TIME LIMIT 
      IF(.NOT.(AM(I,1).LT.CV.AND.AM(I+1,1).GE.CV))      GO TO 110 
C TEST AM(I+1,1)<CV.=AM(I+1,1) NOT INCLUDED , OTHERWISE THE LEVEL IS
C 2 TIMES FOUND 
      IX=I+1
      IY=1                                                                 00510
      IDX=-1                                                               00520
      IDY=0                                                                00530
      CALL PLACE(AM,NMAX,IRC,X,Y) 
  110 CONTINUE                                                             00550
      NT1=NT-1                                                             00560
      DO 20 I=1,NT1                                                        00570
      IF (AM(MT,I)-CV)15,20,20                                             00580
   15 IF (AM(MT,I+1)-CV)20,17,17                                           00590
   17 IX=MT                                                                00600
      IY=I+1                                                               00610
      IDX=0                                                                00620
      IDY=-1                                                               00630
      CALL PLACE(AM,NMAX,IRC,X,Y) 
   20 CONTINUE                                                             00650
   22 DO 30 I=1,MT1                                                        00660
      MT2=MT+1-I                                                           00670
      IF (AM(MT2,NT)-CV)25,30,30                                           00680
   25 IF (AM(MT2-1,NT)-CV)30,27,27                                         00690
   27 IX=MT2-1                                                             00700
      IY=NT                                                                00710
      IDX=1                                                                00720
      IDY=0                                                                00730
      CALL PLACE(AM,NMAX,IRC,X,Y) 
   30 CONTINUE                                                             00750
      DO 40 I=1,NT1                                                        00760
      NT2=NT+1-I                                                           00770
      IF (AM(1,NT2)-CV)35,40,40                                            00780
35    IF (AM(1,NT2-1)-CV)40,37,37                                          00790
   37 IX=1                                                                 00800
      IY=NT2-1                                                             00810
      IDX=0                                                                00820
      IDY=1                                                                00830
      CALL PLACE(AM,NMAX,IRC,X,Y) 
   40 CONTINUE                                                             00850
CEND   CONTOURLINE STARTS AT BORDER 
CBEGIN CONTOURLINE STARTS INSIDE
      ISS=1                                                                00860
      NT1=NT-1                                                             00870
      MT1=MT-1                                                             00880
      DO 10 J=2,NT1                                                        00890
      DO 10 I=1,MT1                                                        00900
      IF (AM(I,J).GE.CV)     GO TO 10 
      IF (AM(I+1,J).LT.CV)    GO TO 10
      ICM=100*(I+1)+J 
      IF(NP .EQ.0)           GO TO 11 
      DO 9 ID=1,NP
      IF (IRC(ID).EQ.ICM)    GO TO 10 
    9 CONTINUE                                                             00970
   11 IX=I+1                                                               00980
      IY=J                                                                 00990
      IDX=-1                                                               01000
      IDY=0                                                                01010
      CALL PLACE(AM,NMAX,IRC,X,Y) 
   10 CONTINUE
CEND   CONTOURLINE STARTS INSIDE
      RETURN
      END 
      SUBROUTINE PLDASH(XX,YY,DL) 
C DL=0.0 INITIALIZE 
C DL >0.0 DASHED LINE DASHLENGTH=DL 
      DIMENSION PTRN(2) 
      PTRN(1)=DL
      PTRN(2)=DL
      IF(DL.EQ.0.) CALL PLPTRN(XX,YY,0,PTRN)
      IF(DL.GT.0.) CALL PLPTRN(XX,YY,2,PTRN)
      RETURN
      END 

      SUBROUTINE PLOT(XA,YA,IA) 
C 
COPYRIGHT (C) CERRITOS COMPUTER SERVICES, INC. 1976 
C 
C     CAUSES "PEN" MOVEMENTS TO BE RECORDED IN THE INTERMEDIATE 
C     FILE. ALSO CONTROLS PAGINATION AND ORIGIN OF THE PLOT.
C 
C XA    X COORDINATE RELATIVE TO CURRENT ORIGIN 
C YA    Y COORDINATE RELATIVE TO CURRENT ORIGIN 
C IA    PEN CONTROL 
C       +999 END OF PLOTS 
C       +11  END OF PLOTS 
C       +10  EJECT PAGE 
C       +3   MOVE TO (XA,YA) PEN UP 
C       +2   MOVE TO (XA,YA) PEN DOWN 
C       -2   MOVE TO (XA,YA) PEN DOWN SET ORIGIN TO (XA,YA) 
C       -3   MOVE TO (XA,YA) PEN UP   SET ORIGIN TO (XA,YA) 
C            ANY OTHER VALUE OF IA IS TREATED AS A NOP
C 
      INTEGER IC0(6)
      REAL AV(2)
      COMMON/PXCOM/LU,MP,MB(128),RE(2),OX,OY,SF,PX,PY 
      EQUIVALENCE(AV(1),AV1),(AV(2),AV2)
      DATA NC0/6/,IC0/2,3,-2,-3,10,11/
      IE=0
      I0=IA 
      IF(I0.EQ.999)I0=11
      AV1=SF*XA+OX
      AV2=SF*YA+OY
      K=0 
      DO 30 I1=1,NC0
30    IF(I0.EQ.IC0(I1))K=I1 
      IF(K .EQ. 0)GO TO 800 
      GO TO (100,100,100,100,700,600),K 
C 
C        MOVE TO (XA,YA)
C 
100   PX=XA 
      PY=YA 
      IF(IA.GT.0)GOTO 700 
C 
C        RESET ORIGIN 
C 
      PX=0.0
      PY=0.0
      OX=AV1
      OY=AV2
      GOTO 700
C 
C        END ALL PLOTTING 
C 
600   IE=1
C 
C        PLACE THE INSTRUCTION IN THE OUTPUT BUFFER 
C 
700   MP=3+MP 
      IF(MP.LT.128)GOTO 710 
      WRITE(LU)MB 
      MP=3
710   MB(MP)=I0 
      DO 720 I0=1,2 
      I1=MP-I0
      T=AV(I0)/RE(I0) 
      IF(T.LT.0.0)T=0.0 
      IF(T.GT.32767.0)T=32767.0 
      MB(I1)=T
720   CONTINUE
C 
C        FORCE OUT THE BUFFER (IA=11 OR 999)
C 
      IF(IE.NE.1)GOTO 800 
      WRITE(LU)MB 
	CLOSE(LU)
800   RETURN
      END 

      SUBROUTINE PLOTS(IA)
C 
C     THIS IS THE FIRST PXPLOT ROUTINE CALLED AND MAY BE CALLED ONLY
C     ONCE. IT INITIALIZES COMMON AND SETS UP THE TEMPORARY I/O SPACE.
C 
COPYRIGHT (C) CERRITOS COMPUTER SERVICES, INC. 1976 
C 
C	IA: LOGICAL UNIT FOR OUTPUT.
C	   IA>0 LOGICAL UNIT IS ASSUMED ALREADY OPEN FOR I/O
C	   IA<0 DEFAULT FILE 'PXTEMP.TMP' WILL BE OPENED ON LU=-IA
C	   IA=0 ILLEGAL
C 
C     COMMON VARIABLES: 
C        LU      LOGICAL UNIT OF INTERMEDIATE FILE
C        MP      POINTS TO CURRENT POSITION IN ARRAY MP 
C        MB(128) OUTPUT BUFFER OF 1 RECORD
C        RE(2)   RESOLUTION  SIZE OF 1 DOT IN INCHES (VERT,HORIZ) 
C        SF      CURRENT SCALE FACTOR 
C        PX,PY   CURRENT POSITION 
C 
C 
      REAL AR(2)
      COMMON/PXCOM/LU,MP,MB(128),RE(2),OX,OY,SF,PX,PY 
	DATA AR/72.0,60.0/
C
      OX=0.0
      OY=0.0
      SF=1.0
      PX=0.0
      PY=0.0
	RE(1)=1.0/AR(1)
	RE(2)=1.0/AR(2)
      MP=0
C
C	   HOW TO ASSIGN LOGICAL UNIT?
C
	IF(IA)10,20,30
C
C	   OPEN DEFAULT FILE
C
10	LU=-IA
	OPEN(UNIT=LU,FILE='PXTEMP.TMP',FORM='UNFORMATTED',
	1   STATUS='NEW',ERR=20)
	GO TO 40
C
C	   BAD SPECIFICATION
C
20	TYPE 25
25	FORMAT(' *** ERROR *** BAD LU IN "PLOTS"')
	CALL EXIT
C
C	   FILE ALREADY OPEN
C
30	LU=IA
40	RETURN
	END
      SUBROUTINE PLPTRN(XX,YY,NP,PTRN)
      DIMENSION PTRN(NP)
      COMMON /PAR/   XB,YB,NA,DL
C PLPTRN MOVES PEN IN  DOWN/UP WAY TO (XX,YY) 
C PTRN(1)= LENGTH (CM) OF PENDOWN SEGMENT 
C PTRN(2)= LENGTH (CM) OF PENUP   SEGMENT 
C PTRN(3)= LENGTH (CM) OF PENDOWN SEGMENT 
C ETC.
C NP NOT ODD
C INITIALIZE WITH NP=0 , SEE CODE 
C COMMON /PAR/ TO SAVE 'OWN' VAR. 
      IF(NP.GT.0)            GO TO 10 
C INITIALIZE
      CALL PLOT(XX,YY,3)
      NA=1
      DL=0. 
                             GO TO 1000 
   10 CONTINUE
      DX=XX-XB
      DY=YY-YB
      DIST=SQRT(DX*DX+DY*DY)
      IF(DIST.EQ.0.)         GO TO 1000 
      D=DIST
      X=XB
      Y=YB
   20 CONTINUE
      STEP=PTRN(NA)-DL
      IF(D.LT.STEP)          GO TO 30 
C D>=STEP 
      H=STEP/DIST 
      X=X+DX*H
      Y=Y+DY*H
      IPEN=2
      IF(NA/2*2.EQ.NA)       IPEN=3 
      CALL PLOT(X,Y,IPEN) 
      D=D-STEP
      NA=NA+1 
C CYCLIC
      IF(NA.EQ.NP+1)         NA=1 
      DL=0. 
                             GO TO 20 
   30 CONTINUE
      STEP=D
      H=STEP/DIST 
      X=X+DX*H
      Y=Y+DY*H
      IPEN=2
      IF(NA/2*2.EQ.NA)       IPEN=3 
      CALL PLOT(X,Y,IPEN) 
      DL=DL+STEP
C 
 1000 XB=XX 
      YB=YY 
      RETURN
      END 

      SUBROUTINE PLT3D(IVXYZ,XDATA,YDATA,ZDATA,XSCALE,YSCALE,ZSCALE,
     1   NLINE,NPNTS,PHI,THETA,XREF,YREF,XLENTH,MASK,VERTEX)
C 
C     ALGORITHM 483: CACM, VOL 17, 520 (1974).
C     "MASKED THREE-DIMENSIONAL PLOT PROGRAM WITH ROTATIONS"
C     AUTHOR: STEVEN L. WATKINS 
C     RECODED BY L. WEISSMAN     JULY 1981
C 
C        DOCUMENTATION FOR THIS ROUTINE IS IN FILE @PLT3D 
C 
      INTEGER HIGH,OLDHI,OLDLOW 
      DIMENSION XDATA(1),YDATA(1),ZDATA(1),MASK(2,1),VERTEX(1)
      COMMON /PLTCM/IX,IY,IP,LIMITX,LIMITY
      DATA JVXYZ,SPHI,STHETA,ZMIN/0,-1.E38,-1.E38,1.E38/
      DATA IYLO,IYHI,PX,PY/32767,0,72.0,70.0/ 
      DATA DEGRAD/1.7453293E-2/ 
C 
C        STATEMENT FUNCTION DEFINING ROUNDING TO INTEGER
C 
      IROUN(ARG)=IFIX(ARG+SIGN(0.5,ARG))
C 
C        DETERMINE WHETHER INITIALIZATION IS NEEDED 
C 
      IF(NLINE-1)10,20,40 
C 
C        SPECIAL INITIALIZATION TO MAKE FRONT SURFACE SOLID 
C 
10    IYLO=0
      IYHI=0
      GO TO 250 
C 
C        INITIALIZE MASK, SET BORDER LIMITS, RESET MASK DEFAULTS
C 
20    LIMITX=IROUN(XLENTH*PX) 
      LIMITY=IROUN(14.0*PY) 
      DO 30 K=1,LIMITX
      MASK(1,K)=IYLO
30    MASK(2,K)=IYHI
      IYLO=32767
      IYHI=0
      ZMIN=1.E38
C 
C        SET INDICATORS FOR TYPES OF INPUT DATA AND SAVING VERTICES 
C 
40    IF(JVXYZ .EQ. IVXYZ)GO TO 50
      JVXYZ=IVXYZ 
      INDZ=1
      INDY=1
      INDX=1
      INDV=1
      IF(MOD(JVXYZ/1000,10) .GT. 0)INDV=2 
      IF(MOD(JVXYZ/100 ,10) .GT. 0)INDX=2 
      IF(MOD(JVXYZ/10  ,10) .GT. 0)INDY=2 
      IF(MOD(JVXYZ     ,10) .GT. 0)INDZ=2 
C 
C        ROTATION INITIALIZATION
C        DETERMINE IF INITIALIZATION IS REQUIRED
C 
50    IF(PHI .EQ. SPHI .AND. THETA .EQ. STHETA)GO TO 60 
C 
C        COMPUTE ROTATION FACTORS 
C 
      SPHI=SIN(DEGRAD*PHI)
      CPHI=COS(DEGRAD*PHI)
      STHETA=SIN(DEGRAD*THETA)
      CTHETA=COS(DEGRAD*THETA)
      A11=CPHI
      A13=-SPHI 
      A21=STHETA*SPHI 
      A22=CTHETA
      A23=STHETA*CPHI 
      A31=SPHI*CTHETA 
      A32=-STHETA 
      A33=CPHI*CTHETA 
      SPHI=PHI
      STHETA=THETA
C 
C        BEGIN LOOP TO PROCESS THE LINE 
C 
60    DO 240 K=1,NPNTS
C 
C        CALCULATE COORDINATES OF THE POINT 
C 
      GO TO (70,80 ),INDX 
70    X=XDATA(1)+(K-1)*XSCALE
      GO TO 90
80    X=XDATA(K)*XSCALE 
90    GO TO (100,110),INDY
100   Y=YDATA(1)+(K-1)*YSCALE
      GO TO 120 
110   Y=YDATA(K)*YSCALE 
120   GO TO (130,140),INDZ
130   Z=ZDATA(1)+(K-1)*ZSCALE
      GO TO 150 
140   Z=ZDATA(K)*ZSCALE 
C 
C        DATA ROTATION
C 
150   XXX=A11*X + A13*Z + XREF
      XNEW=XXX
      IX=IROUN(XNEW*PX) 
      YYY=A21*X + A23*Z + YREF
      YNEW=YYY+A22*Y
      IY=IROUN(YNEW*PY) 
      IF(K .GT. 1)GO TO 160 
C 
C        THIS CODE ONLY FOR THE FIRST POINT IN THE LINE.
C 
      CALL CHECK(MASK)
      IF(IP .EQ. 2)CALL PLOT(XNEW,YNEW,3) 
C 
C        STORE VERTICES IF REQUESTED
C 
      IF(INDV .EQ. 1)GO TO 230
      ZNEW=A31*X + A32*Y + A33*Z
      IF(ZNEW .GE. ZMIN)GO TO 155 
      ZMIN=ZNEW 
      VERTEX(17)=1.0
      IF(NLINE .GT. 1)VERTEX(17)=4.0
155   VERTEX(7)=XNEW
      VERTEX(8)=YNEW
      VERTEX(15)=XXX
      VERTEX(16)=YYY
      IF(NLINE .GT. 1)GO TO 230 
      VERTEX(1)=XNEW
      VERTEX(2)=YNEW
      VERTEX(9)=XXX 
      VERTEX(10)=YYY
      GO TO 230 
C 
C        SET LOOP PARAMETERS FOR DRAWING THE LINE 
C 
160   I=MAX0(IABS(IX-JX),IABS(IY-JY)) 
      IF(I .LT. 1)GO TO 230 
      DX=(XNEW-XOLD)/I
      DY=(YNEW-YOLD)/I
      XX=XOLD 
      YY=YOLD 
C 
C        LOOP TO DRAW THE LINE
C 
      DO 190 J=1,I
      XX=XX+DX
      YY=YY+DY
      IX=IROUN(XX*PX) 
      IY=IROUN(YY*PY) 
      CALL CHECK(MASK)
C 
C        ACTION IS BASED ON PEN STATUS: 
C 
C          IPOLD   IP   ACTION
C            2      2   NONE
C            2      3   DRAW LINE TO PREVIOUS POINT 
C            3      2   MOVE TO CURRENT POINT 
C            3      3   NONE
C 
      IF(IP-IPOLD)170,185,175 
170   CALL PLOT(XX,YY,IPOLD)
      GO TO 180 
175   CALL PLOT(XPREV,YPREV,IPOLD)
180   IPOLD=IP
185   XPREV=XX
      YPREV=YY
190   CONTINUE
C 
C        FORCE OUT LAST POINT, IF NECESSARY, AND RESET FOR NEXT POINT 
C 
      IF(IP .EQ. 2)CALL PLOT(XNEW,YNEW,IP)
230   XOLD=XNEW 
      YOLD=YNEW 
      XPREV=XNEW
      YPREV=YNEW
      IPOLD=IP
      JX=IX 
      JY=IY 
240   CONTINUE
C 
C        STORE VERTICES IF REQUESTED
C 
      IF(INDV .EQ. 1)GO TO 250
      ZNEW=A31*X + A32*Y + A33*Z
      IF(ZNEW .GE. ZMIN)GO TO 245 
      ZMIN=ZNEW 
      VERTEX(17)=2.0
      IF(NLINE .GT. 1)VERTEX(17)=3.0
245   VERTEX(5)=XNEW
      VERTEX(6)=YNEW
      VERTEX(13)=XXX
      VERTEX(14)=YYY
      IF(NLINE .NE. 1)GO TO 250 
      VERTEX(3)=XNEW
      VERTEX(4)=YNEW
      VERTEX(11)=XXX
      VERTEX(12)=YYY
C 
C        RETURN TO CALLING PROGRAM
C 
250   RETURN
      END 

      SUBROUTINE PROMPT(INSTR,NIN,OUTSTR,NOUT,LU1,LU2) 
C 
C        Write a prompt on the user terminal, returning with the
C        response string.
C 
C     INSTR:  The prompt string. Will be followed by " * " 
C     NIN:    Number of bytes in INSTR. 
C     OUTSTR: The returned answer to the prompt.
C     NOUT:   The number of bytes in OUTSTR.
C     LU1:    LU to write the prompt
C     LU2:    LU to read the prompt 
C 
C        L. Weissman
C        Department of Biology, Gilmer Hall
C        University of Virginia
C        McCormick Road
C        Charlottesville, VA  22901
C
      IMPLICIT INTEGER (A-Z)
      BYTE INSTR(*),OUTSTR(*),IP(3)
      DATA IP/'$',' ','*'/
C
      J=MIN0(LENG(INSTR,NIN),60)
      IF(J .GT. 0)WRITE(LU1,10)IP(1),(INSTR(I),I=1,J),IP(2),IP(3),IP(2)
      IF(J .LT. 1)WRITE(LU1,10)IP,IP(2)
10    FORMAT(80A1)
C
C        Read the response
C
      READ(LU2,20)NOUT,(OUTSTR(I),I=1,NOUT)
20    FORMAT(Q,80A1)
      RETURN
      END
C
C
C
      FUNCTION RANGAU(  NRANDOM01, IRANSEED )
C     ==================================================
C
C
      SUM = 0.
C
C
      DO 999 I = 1, NRANDOM01
        SUM = SUM + RAN( IRANSEED ) -.5
999    CONTINUE
C
C
      RANGAU = SUM
C
C
      RETURN
C
C
      END
      SUBROUTINE RBMAT (E,ROT)
C
C   SUBROUTINE TO CALCULATE THE ROTATION MATRIX GIVEN
C   THE ROSSMANN AND BLOW EULERIAN ANGLES.
C
      DIMENSION ROT(3,3), E(3)
C
      CALL DTR(E)
C
      C1=COS(E(1))
      C2=COS(E(2))
      C3=COS(E(3))
      S1=SIN(E(1))
      S2=SIN(E(2))
      S3=SIN(E(3))
C
      ROT(1,1)=-S1*C2*S3  +  C1*C3
      ROT(2,1)=+C1*C2*S3  +  S1*C3
      ROT(3,1)=+S2*S3
      ROT(1,2)=-S1*C2*C3  -  C1*S3
      ROT(2,2)=+C1*C2*C3  -  S1*S3
      ROT(3,2)=+S2*C3
      ROT(1,3)=+S1*S2
      ROT(2,3)=-C1*S2
      ROT(3,3)=+C2
C
      CALL RTD(E)
      RETURN
      END
C
C
C
      SUBROUTINE RDIMDF(MDFIN,IPRINT,RBFINF,MAXRBF,BUFINF,MAXBUF)
C     ===========================================================
C
C
C---- Subroutine for reading introductory records from an
C     MDF.  most information is stored in  common blocks,
C     except for the title information in rbfinf and bufinf.
C
C
C---- Input parameters:
C     ================
C
C     MDFIN    : unit number on which input MDF resides
C     IPRINT   : flag for printing the MDF header information:
C                IPRINT = 0  header info is not printed
C                IPRINT = 1  header info will be printed
C     RBFINF   : character array which is to be filled with
C                the rbuffer information
C     MAXRBF   : maximum length which the rbfinf array can
C                have (should be defined in calling routine)
C     BUFINF   : character array which is to be filled with
C                the data-buffer information
C     MAXBUF   : maximum length which the bufinf array can
C                have (should be defined in calling routine)
C
C
C---- Output parameters:
C     =================
C
C
C     The character arrays with variable lengths "RBFINF" and "BUFINF"
C     are transferred through the calling statement.  so,  array space
C     should  be  declared  for  these  arrays in the calling routine.
C     all the other parameters are transferred through  common blocks:
C
C     CHARACTER*80    TITLE, RBFINF(MAXRBF), BUFINF(MAXBUF)
C     COMMON /MDFPAR/ MAXR, MAXB, CELL(6), ISLOW, INTER, IFAST,
C    .                KLASS, ICENTR, ISCREW(3), IVERSN
C     COMMON /MDFTTL/ TITLE(20)
C     COMMON /MDFSYM/ NSYM, SYMROT(3,3,96), SYMTRL(3,96)
C
C
C     MAXR     : length of rbufr array
C     MAXB     : length of data buffer array
C     CELL     : cell parameters
C     ISLOW    : slowest changing index
C     INTER    : intermediate changing index
C     IFAST    : fastest changing index
C     KLASS    : laue class (see MDF description)
C     ICENTR   : lattice centering parameters
C     ISCREW   : screw axis information
C     IVERSN   : MDF version number
C                IVERSN = 0 : MDF from before april 15, 1987
C                IVERSN = 1 : april 15, 1987 MDF version
C     TITLE    : general information in MDF
C     RBFINF   : title lines for rbufr contents
C     BUFINF   : title lines for data buffer contents
C     NSYM     : number of symmetry related positions in
C                your spacegroup (including identity
C                transformation as the first symmetry operation)
C     SYMROT   : rotation part of symmetry transformation
C     SYMTRL   : translation part of symmetry transformation
C
C
C
C---- Important notes :
C     =================
C
C     1. use of symmetry operations:
C        XOLD     : starting fractional coordinates
C        XNEW     : coordinates after symmetry operation
C        ISYM     : symmetry operation considered
C
C     XNEW(I) = SYMROT(I,1,ISYM)*XOLD(1) + SYMROT(I,2,ISYM)*XOLD(2)
C                + SYMROT(I,3,ISYM)*XOLD(3) + SYMTRL(I,ISYM)
C
C     2. The symmetry operations are read (and written) as follows:
C
C            READ (MDFIN) NSYM
C            DO 140 ISYM = 1,NSYM
C        140 READ (MDFOUT) ((SYMROT(I,J,ISYM),I=1,3),J=1,3),
C           .               (SYMTRL(I,ISYM),I=1,3)
C
C     3. This subroutine makes no calls to any other subroutine.
C
C
C---- Version march 19, 1987  (bauke dijkstra, randy read)
C     ====================================================
C
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER IPRINT,MAXBUF,MAXRBF,MDFIN
C     ..
C     .. Array Arguments ..
      CHARACTER BUFINF(MAXBUF)*80,RBFINF(MAXRBF)*80
C     ..
C     .. Scalars in Common ..
      INTEGER ICENTR,IFAST,INTER,ISLOW,IVERSN,KLASS,LUNIN,LUNOUT,MAXB,
     +        MAXR,NSYM
C     ..
C     .. Arrays in Common ..
      REAL CELL,SYMROT,SYMTRL
      INTEGER ISCREW
      CHARACTER TITLE*80
C     ..
C     .. Local Scalars ..
      INTEGER I,IERROR,ISYM,J
C     ..
C     .. Local Arrays ..
      CHARACTER CENTR(7)*1,HKL(3)*1,CCLASS(16)*10
C     ..
C     .. Common blocks ..
      COMMON /AINOUT/LUNIN,LUNOUT
      COMMON /MDFPAR/MAXR,MAXB,CELL(6),ISLOW,INTER,IFAST,KLASS,ICENTR,
     +       ISCREW(3),IVERSN
      COMMON /MDFSYM/NSYM,SYMROT(3,3,96),SYMTRL(3,96)
      COMMON /MDFTTL/TITLE(20)
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
      DATA ISLOW,INTER,IFAST/1,2,3/
      DATA HKL/'H','K','L'/
      DATA CENTR/'P','A','B','C','F','I','R'/
      DATA (CCLASS(I),I=1,16)/'TRICLINIC ','MONOCLIN-B','O-RHOMBIC ',
     +     'TETRAGNL-1','TETRAGNL-2','TRIGONAL-1','TRIGONAL-2',
     +     'TRIGONAL-3','TRIGONAL-4','TRIGONAL-5','HEXAGONL-1',
     +     'HEXAGONL-2','CUBIC-1   ','CUBIC-2   ','MONOCLIN-A',
     +     'MONOCLIN-C'/
C     ..
C
C---- Fill title-records with blanks
C
      DO 10 I = 1,20
        TITLE(I) = ' '
   10 CONTINUE
      DO 20 I = 1,MAXRBF
        RBFINF(I) = ' '
   20 CONTINUE
      DO 30 I = 1,MAXBUF
        BUFINF(I) = ' '
   30 CONTINUE
C
C---- Fill spacegroup symmetry operations with zero's
C
      DO 60 ISYM = 1,96
        DO 50 J = 1,3
          SYMTRL(J,ISYM) = 0.0
          DO 40 I = 1,3
            SYMROT(I,J,ISYM) = 0.0
   40     CONTINUE
   50   CONTINUE
   60 CONTINUE
C
C---- Now start reading and printing the introductory records
C     on the input master data file.
C
      REWIND MDFIN
C
C---- RECORD # 1: title with general information
C     ==========
C
      READ (MDFIN,END=140,ERR=130) TITLE
      IF (IPRINT.EQ.1) THEN
        WRITE (LUNOUT,FMT='(///,'' Header records on input MDF: '',/)')
        DO 70 I = 1,20
          IF (TITLE(I).NE.'  ') WRITE (LUNOUT,FMT='(1X,A80)') TITLE(I)
   70   CONTINUE
        WRITE (LUNOUT,FMT='(/)')
      END IF
C
C---- RECORD # 2 : buffer lengths, slow medium and fast changing
C     ==========   indices, MDF version number
C
C
      IVERSN = 0
      IERROR = 0
      READ (MDFIN,END=80,ERR=80) MAXR,MAXB,ISLOW,INTER,IFAST,
     +  IVERSN
      GO TO 90
   80 IERROR = 1
      WRITE (LUNOUT,FMT=
     +'(//,                                               '' ***** Error
     + Reading the MDF line with the '',                  ''MDF Version
     +Number'',/,'' ***** Check next items carefully!'')')
   90 IF (IPRINT.EQ.1 .OR. IERROR.EQ.1) THEN
        WRITE (LUNOUT,FMT='(//,4X,''Crystal-INFO from MDF:'')')
        WRITE (LUNOUT,FMT=6000) MAXR,MAXB,HKL(ISLOW),HKL(INTER),
     +    HKL(IFAST),IVERSN
      END IF
      IF (IVERSN.EQ.0) THEN
        WRITE (LUNOUT,FMT=
     +'(///,'' ***** Your MDF dates from before April '',       ''15, 19
     +87'',/,'' ***** PLEASE, UPGRADE your MDF!'',//)')
      ELSE IF (IVERSN.EQ.1) THEN
        IF (IPRINT.EQ.1) WRITE (LUNOUT,
     +      FMT='(''    MDF Version APRIL 15, 1987'',//)')
      END IF
C
C---- Check if maxr and maxb are within program limits
C
      IF (MAXB.GT.MAXBUF) THEN
        WRITE (LUNOUT,FMT=
     +'(/,''    MAXB on MDF larger than '',                      ''MAXBU
     +F in subroutine RDIMDF :  STOP!'',//,''    MAXB = '',      I5,'',
     +BUT MAXBUF = '',I5,//)') MAXB,MAXBUF
        WRITE (LUNOUT,FMT=
     +'(/,''    Change MAXBUF in "CALL RDIMDF" '',              ''Statem
     +ent in calling routine!'')')
        STOP 'MAXBUF TOO SMALL'
      ELSE IF (MAXR.GT.MAXRBF) THEN
        WRITE (LUNOUT,FMT=
     +'(/,''    MAXR on MDF larger than '',                      ''MAXRB
     +F in subroutine RDIMDF :  STOP!'',//,''    MAXR = '',      I5,'',
     +But MAXRBF = '',I5,//)') MAXR,MAXRBF
        WRITE (LUNOUT,FMT=
     +'(/,''    Change MAXRBF in "CALL RDIMDF" '',              ''Statem
     +ent in calling routine!'')')
        STOP 'MAXRBF TOO SMALL'
      ELSE
C
C---- RECORD # 3 : rbfinf array
C     ==========
C
        READ (MDFIN,END=140,ERR=130) (RBFINF(I),I=1,MAXR)
        IF (IPRINT.EQ.1) THEN
          WRITE (LUNOUT,FMT='(/,/,4X,''Contents of RBUFFR-INFO:'')')
          DO 100 I = 1,MAXR
            IF (RBFINF(I).NE.' ') WRITE (LUNOUT,FMT=6002) I,RBFINF(I)
  100     CONTINUE
          WRITE (LUNOUT,FMT='(/)')
        END IF
C
C---- RECORD # 4 : title for data buffer array
C     ==========
C
        READ (MDFIN,END=140,ERR=130) (BUFINF(I),I=1,MAXB)
        IF (IPRINT.EQ.1) THEN
          WRITE (LUNOUT,FMT='(/,/,4X,''Contents of BUFFER-INFO:'')')
          DO 110 I = 1,MAXB
            IF (BUFINF(I).NE.' ') WRITE (LUNOUT,FMT=6002) I,BUFINF(I)
  110     CONTINUE
          WRITE (LUNOUT,FMT='(/)')
        END IF
C
C---- RECORD # 5 : cell dimensions, klass, centering, screw axis info
C     ==========
C
        READ (MDFIN,END=140,ERR=130) CELL,KLASS,ICENTR,ISCREW
        IF (IPRINT.EQ.1) THEN
          WRITE (LUNOUT,FMT=6004) CELL
          WRITE (LUNOUT,FMT=6006) CCLASS(KLASS),CENTR(ICENTR+1),ISCREW
        END IF
C
C---- RECORD # 6 : symmetry information
C     ===========
C
C     Only if MDF has a version number = 1 or higher
C     for before april 15, 1987 MDF's include identity operation
C
        IF (IVERSN.EQ.0) THEN
          NSYM = 1
          SYMROT(1,1,1) = 1.0
          SYMROT(2,2,1) = 1.0
          SYMROT(3,3,1) = 1.0
          RETURN
        ELSE
          READ (MDFIN,END=140,ERR=130) NSYM
          IF (NSYM.LT.1 .OR. NSYM.GT.96) THEN
            WRITE (LUNOUT,FMT=
     +'(//,'' ILLEGAL Number of SYMMETRY OPERATIONS'',//,       '' ROUTI
     +NE RDIMDF is dimensioned for 96 SYMMETRY'',               '' Opera
     +tions at a MAXIMUM'',/,'' and minimally there'',          '' shoul
     +d be 1 SYMMETRY Operation (the identity'',                '' trans
     +formation'',//,'' Number of symmetry operations'',        '' on th
     +e input MDF is '',I5,///)') NSYM
            STOP 'WRONG NUMBER OF SYMMETRY OPERATIONS IN MDF'
          ELSE
C
C---- RECORD # 7 : symmetry operations
C     ==========
C
            DO 120 ISYM = 1,NSYM
              READ (MDFIN) ((SYMROT(I,J,ISYM),I=1,3),J=1,3),
     +          (SYMTRL(I,ISYM),I=1,3)
              IF (IPRINT.EQ.1) WRITE (LUNOUT,FMT=
     +'(/,'' Symmetry operation'',             I5,//,3(3F10.5,5X,F10.5,/
     +))') ISYM, ((SYMROT(I,J,ISYM),J=1,3),SYMTRL(I,ISYM),I=1,3)
  120       CONTINUE
C
C---- All intro records now read
C     return to calling program
C
          END IF
          RETURN
        END IF
      END IF
  130 WRITE (LUNOUT,FMT=
     +'(/,'' ERROR while reading the header records on the '',
     +''MASTER DATA FILE'',/,'' SOMETHING VERY WRONG!'',///)')
      STOP 'ERROR ON MDF IN RDIMDF'
  140 WRITE (LUNOUT,FMT=
     +'('' PREMATURE END OF INFORMATION ON MDF'',          '' IN SUBROUT
     +INE RDIMDF:  STOP!'')')
      STOP ' EOI ON MDF IN RDIMDF'
C
C---- Format statements
C
 6000 FORMAT (/4X,'LENGTH OF RBUFFR-ARRAY   ',I10,/4X,'LENGTH OF BUFFE',
     +       'R ARRAY   ',I10,/4X,'SLOW CHANGING INDEX',15X,1A,/4X,'ME',
     +       'DIUM CHANGING INDEX',13X,1A,/4X,'FAST CHANGING INDEX',15X,
     +       1A,/4X,'MDF VERSION NUMBER       ',I10,//)
 6002 FORMAT (/'    LINE NR',I4,2X,A80)
 6004 FORMAT (/'    A-AXIS IN ANGSTROMS      ',F8.3,/'    B-AXIS IN AN',
     +       'GSTROMS      ',F8.3,/'    C-AXIS IN ANGSTROMS      ',F8.3,
     +       /'    ALPHA  IN DEGREES        ',F8.3,/'    BETA   IN DEG',
     +       'REES        ',F8.3,/'    GAMMA  IN DEGREES        ',F8.3,
     +       /)
 6006 FORMAT (/'    CRYSTAL CLASS            ',A10,/'    CRYSTAL LATTI',
     +       'CE          ',A1,/'    THERE IS A ',I1,'-FOLD SCREW AXIS',
     +       ' PARALLEL TO THE A-AXIS',/'    THERE IS A ',I1,'-FOLD SC',
     +       'REW AXIS PARALLEL TO THE B-AXIS',/'    THERE IS A',I2,
     +       '-FOLD SCREW AXIS PARALLEL TO THE C-AXIS',//)
C
C
      END
      SUBROUTINE RDMFFZ (X,N,NX,NZ,MFF)
C
C        THIS SUBROUTINE,  MODIFIED FROM TEN EYCK'S READ Z,  READS IN A
C        MAP AS Z-SECTIONS IN WHICH Y VARIES MOST RAPIDLY AND X  VARIES
C        MOST SLOWLY.  THIS TYPE OF MAP IS E.G. PRODUCED BY THE  OUTPUT 
C        ROUTINE PRT ZYX (MODIFIED FROM TEN EYCK'S PRINT Z),  OR BY THE
C        PROGRAM  EDCALC.   THIS IS TO SPEED UP THE READING OF THE MAP, 
C        SINCE THE INPUT CAN NOW PROCEED IN ARRAY NOTATION ;    THIS IS 
C        FOLLOWED BY A REARRANGEMENT OF THE DENSITIES  ( STARTING  FROM
C        THE END OF THE ARRAY )  TO ALLOW FOR  THE SCRATCH  LOCATION OF 
C        THE Y-TRANSFORM ( SUBROUTINE 'GULP YX')
C
C        THE SCRATCH FILE IS IN DIRECT ACCESS.
C
C        G. BRICOGNE, APRIL 1977.
C
C        THE INPUT FILE IS ASSUMED TO BE IN THE GRONINGEN MFF FORMAT.
C        JUNE, 1985        BAUKE DIJKSTRA
C        THE HEADER RECORDS ARE READ IN THE MAIN PROGRAM. IN THIS 
C        ROUTINE THE SECTIONS ARE READ ONLY.
C
C
      INTEGER    N,NX,NZ,MFF
      REAL       X(N,NX,NZ)
      NY  =  N-2
      DO 200 K = 1,NZ
  200 CALL   GULP YX (X(1,1,K),N,NX,X(1,1,K),NY,MFF)
      RETURN
      END
C
C
C
      SUBROUTINE RDREC(MDFIN,RBUFFR,MAXR,IHKL,BUFRIN,MAXIB,MDFEND,
     +                 IVERSN)
C     ===========================================================
C
C
C
C
C---- Subroutine to read a reflection record from a MDF
C     file in a general way, to ensure that all records
C     are properly read, and that all buffers are set to
C     "absent" before reading
C
C
C---- MEANING OF THE PARAMETERS:
C     =========================
C
C      Input parameters for this subroutine:
C
C      - MDFIN          unit number on which the input MDF resides
C
C      - MAXR           maximum length of rbuffr array
C
C      - MAXIB          maximum length of bufrin array
C
C      - IVERSN         MDF version number
C                       iversn = 1 : april 15, 1987 version
C
C        MAXR, MAXIB and IVERSN should have been read from the MDF
C        header records (e.g. by subroutine rdiMDF).  this is fine
C        for a program that just reads an MDF.   for programs that
C        also produce an MDF,  you could set maxib to the  overall
C        maximum for both the input and output MDFs.   then bufrin
C        will  be  filled  to its maximum length with absents, and
C        you  won't  have  to  worry about  accidentally occurring
C        undefined values in it later.
C
C
C---- OUTPUT PARAMETERS FROM THIS SUBROUTINE:
C     ======================================
C
C      - MDFEND         flag for end of file on MDF
C                       = 0 : end of file not yet reached
C                       = 1 : end of file reached
C
C      - IHKL           array ihkl(3) contains ih, ik, il
C
C      - RBUFFR         array rbuffr(maxr) contains the data
C                       present in the rbuffr
C
C      - BUFRIN         array bufrin(maxib) contains the data
C                       present in the buffer
C
C
C---- IMPORTANT NOTE :
C     ================
C
C     1. This subroutine makes no calls to any other subroutine.
C        and does not use information stored in common blocks.
C
C
C---- VERSION MARCH 19, 1987  (BAUKE DIJKSTRA, RANDY READ)
C     ====================================================
C
C
C     .. Scalar Arguments ..
      INTEGER IVERSN,MAXIB,MAXR,MDFEND,MDFIN
C     ..
C     .. Array Arguments ..
      REAL BUFRIN(MAXIB),RBUFFR(MAXR)
      INTEGER IHKL(3)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      REAL ABSENT
      INTEGER IB,IPOS,J
C     ..
C     .. Common blocks ..
      COMMON /AINOUT/LUNIN,LUNOUT
C     ..
C     .. Data statements ..
      DATA ABSENT/-1.0E10/
C     ..
C
C---- Clear the output bufrin record
C
      DO 10 J = 1,MAXIB
        BUFRIN(J) = ABSENT
   10 CONTINUE
      MDFEND = 0
C
C---- Read a new record
C
      IF (IVERSN.EQ.0) THEN
        READ (MDFIN,END=30,ERR=20) IPOS,IB, (RBUFFR(J),J=1,MAXR),
     +    IHKL, (BUFRIN(J),J=1,IB)
        RETURN
      ELSE
        READ (MDFIN,END=30,ERR=20) IB, (RBUFFR(J),J=1,MAXR),IHKL,
     +     (BUFRIN(J),J=1,IB)
        RETURN
      END IF
   20 WRITE (LUNOUT,FMT=6000) MDFIN
      STOP ' ERROR READING MDF!'
   30 MDFEND = 1
      DO 40 J = 1,MAXR
        RBUFFR(J) = ABSENT
   40 CONTINUE
C
C---- Format statements
C
 6000 FORMAT ('0***ERROR ENCOUNTERED READING MDF ON UNIT',I3,' ***')
C
C
      END
C
C
C
      SUBROUTINE REPLCE(MDFIN,MDFOUT,NDATA,IPSNRS,DATA,IHKL)
C     ======================================================
C
C
C
C---- Subroutine  to add new data to  a  master data file.
C     data can be added both to an existing MDF record or,
C     if the appropriate hkl record  is not present in the
C     input MDF, new hkl records will be created.
C
C     This  subroutine  puts  the  new  data in the column
C     numbers specified in array ipsnrs.
C
C     The calling program should handle reading and writing
C     of  the  master  data file header records by means of
C     routines RDIMDF and WRIMDF.
C
C     A  different  way  of  adding  new data to an MDF is
C     provided by subroutine ADDATA.
C
C
C
C---- Parameters in calling statement:
C     ================================
C
C       MDFIN  : unit number on which  input MDF resides
C       MDFOUT : unit number on which output MDF resides
C       IPSNRS : array ipsnrs contains column numbers for new data
C       NDATA  : number of items to add to the MDF
C       DATA   : array data(ndata) contains the new data
C       IHKL   : array ihkl(3) contains h, k and l indices
C
C       In addition this subroutine needs parameters stored in the
C       following common blocks:
C
C   1.  COMMON /MDFPAR/ MAXR, MAXB, CELL(6), ISLOW, INTER, IFAST,
C      .                KLASS, ICENTR, ISCREW(3), IVERSN
C
C       MAXR     : length of rbufr array
C       MAXB     : length of data buffer array
C       CELL     : cell parameters
C       ISLOW    : slowest changing index
C       INTER    : intermediate changing index
C       IFAST    : fastest changing index
C       KLASS    : laue class (see MDF description)
C       ICENTR   : centering parameters
C       ISCREW   : screw axis information
C       IVERSN   : MDF version number
C                  IVERSN = 0 : MDF from before april 15, 1987
C                  IVERSN = 1 : april 15, 1987 MDF version
C
C       Use subroutine RDIMDF to define this common block.
C
C
C       The data in the following common blocks should not (not!)
C       be modified by any other part in the calling program!
C
C   2.  Counts of what was done are kept in the common block /MDFcts/:
C       a call to this subroutine with h=k=l=511 will force the
C       subroutine to copy the remaining records of the input MDF to
C       the output MDF, and print the counts:
C
C       COMMON / MDFCTS / NREAD, NOTMOD, NMODIF, NNEW, NDELET, NSKIP
C
C       NREAD  :  number of hkl records read from input MDF
C       NOTMOD :  number of hkl records written unchanged to MDFout
C       NMODIF :  number of modified hkl records written to MDFout
C       NNEW   :  number of completely new hkl records written
C                 to MDFout
C       NDELET :  number of empty records in MDF (all entries absent)
C                 these are deleted in the output MDF
C       NSKIP  :  number of times no valid data were present in array
C                 data (e.g. because length of this array was specified
C                 to be zero, and, at the same time, for this hkl no
C                 MDF record was present.
C
C
C   3.  Information on the last MDF record read (and some other info
C       is kept in common block / MDFREC /.  (after adding data,
C       This subroutine always reads the next MDF record before
C       returning to the calling routine)
C
C       COMMON / MDFREC / RH(MAXRBF), BUFFER(MAXBUF), MDFHKL(3), IPOS2,
C      .                  MDFEND, LAST, FIRST
C
C       RH     : array of length maxrbf containing rbuffr info from
C                input MDF
C       MAXRBF : parameter specifying length of rh (set to = 6)
C       BUFFER : array of length maxbuf containing buffer info from
C                input MDF
C       MAXBUF : parameter specifying length of buffer (set to = 100)
C       MDFHKL : hkl values on input MDF record
C       IPOS2  : hkl packed into a 32 bit word according to the
C                ordering on the input MDF
C       MDFEND : 0/1 specifying that end of file on input MDF has
C                not/already been reached.
C       LAST   : h=511, k=511, l=511 packed into 32 bit word
C                (specifies final call to this subroutine)
C       FIRST  : logical : .true. for first call to this subroutine
C                .false. for subsequent calls.
C
C
C   4.  Parameters needed for fast calculation of the resolution of
C       a reflection are kept in common block / MDFres /:
C
C       COMMON / MDFRES / A11, A21, A22, A31, A32, A33
C       The resolution can be calculated as follows:
C
C       DSTAR-SQUARED = IH*IH*A11 + IH*IK*A21 + IH*IL*A31
C                     + IK*IK*A22 + IK*IL*A32 + IL*IL*A33
C       SO:       RES = 1.0/SQRT(DSTAR_SQUARED)
C
C
C       The data in this common block are provided by subroutine
C       DCALC (called by REPLCE itself)
C
C
C
C---- Important for programmers:
C     ==========================
C
C   1.  To ensure that all hkl records of the input MDF are written
C       to the output MDF,  a final call to this subroutine  should
C       be made with ihkl = (511, 511, 511).
C       In addition,  the routine will then print out the number of
C       hkl records  read,  written  (changed  and  unchanged)  and
C       created or deleted.
C
C   2.  This subroutine makes calls to the following subroutines:
C          -  RDREC
C          -  WRTREC
C          -  DCALC
C          -  PACK
C       These subroutines are available in CFBLIB.
C
C
C---- Version march 19, 1987  (herman schreuder)
C     ==========================================
C
C
C     .. Parameters ..
      INTEGER MAXRBF,MAXBUF
      PARAMETER (MAXRBF=6,MAXBUF=100)
C     ..
C     .. Scalar Arguments ..
      INTEGER MDFIN,MDFOUT,NDATA
C     ..
C     .. Array Arguments ..
      REAL DATA(NDATA)
      INTEGER IHKL(3),IPSNRS(NDATA)
C     ..
C     .. Scalars in Common ..
      REAL A11,A21,A22,A31,A32,A33
      INTEGER ICENTR,IFAST,INTER,IPOS2,ISLOW,IVERSN,KLASS,LAST,LUNIN,
     +        LUNOUT,MAXB,MAXR,MDFEND,NDELET,NMODIF,NNEW,NOTMOD,NREAD,
     +        NSKIP
      LOGICAL FIRST
C     ..
C     .. Arrays in Common ..
      REAL BUFFER,CELL,RH
      INTEGER ISCREW,MDFHKL
C     ..
C     .. Local Scalars ..
      REAL ABSENT
      INTEGER I,I1,I2,I3,IDEL,IPOS1
C     ..
C     .. Local Arrays ..
      REAL BF(MAXBUF),RR(MAXRBF)
C     ..
C     .. External Subroutines ..
      EXTERNAL DCALC,PACK,RDREC,WRTREC
C     ..
C     .. Common blocks ..
      COMMON /AINOUT/LUNIN,LUNOUT
      COMMON /MDFCTS/NREAD,NOTMOD,NMODIF,NNEW,NDELET,NSKIP
      COMMON /MDFPAR/MAXR,MAXB,CELL(6),ISLOW,INTER,IFAST,KLASS,ICENTR,
     +       ISCREW(3),IVERSN
      COMMON /MDFREC/RH(MAXRBF),BUFFER(MAXBUF),MDFHKL(3),IPOS2,MDFEND,
     +       LAST,FIRST
      COMMON /MDFRES/A11,A21,A22,A31,A32,A33
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
      DATA FIRST/.TRUE./,ABSENT/-1.0E10/
C     ..
C
C---- NDATA should not be = 0.....
C
      IF (NDATA.LE.0) THEN
        WRITE (LUNOUT,FMT=
     +'(//,'' ERROR IN SUBROUTINE REPLCE: NUMBER OF '',        '' DATA I
     +TEMS TO ADD TO MDF IS .LE. 0'',//)')
        STOP 'ERROR IN SUBROUTINE REPLCE'
      ELSE
C
C---- If first time through calculate constants for rapid calculation
C     of resolution. needed to add new hkl records to an existing MDF
C     also check parameter values...
C
        IF (FIRST) THEN
C
C---- Check parameters...
C
          IF (MAXRBF.LT.MAXR .OR. MAXBUF.LT.MAXB) THEN
            WRITE (LUNOUT,FMT=
     +'(///,'' ERROR IN SUBROUTINE REPLCE'',/,                       ''
     +PARAMETER MAXRBF IS'',I5,'' BUT SHOULD BE AT'',                ''
     +LEAST'',I5,/,'' PARAMETER MAXBUF IS'',I5,                      ''
     +BUT SHOULD BE AT LEAST'',I5,/)') MAXRBF,MAXR,MAXBUF,MAXB
            STOP 'ERROR IN SUBROUTINE REPLCE'
          ELSE
C
C---- Determine constants for rapid calculation of resolution
C
            CALL DCALC(CELL)
C
C---- Set counters
C
            FIRST = .FALSE.
            CALL PACK(511,511,511,LAST)
            NOTMOD = 0
            NMODIF = 0
            NNEW = 0
            NDELET = 0
            NSKIP = 0
C
C---- And read first MDF record
C
            CALL RDREC(MDFIN,RH,MAXR,MDFHKL,BUFFER,MAXB,MDFEND,IVERSN)
            NREAD = 1
            DO 10 I = 1,NDATA
              BUFFER(IPSNRS(I)) = ABSENT
   10       CONTINUE
            IF (MDFEND.NE.0) THEN
              NREAD = 0
              MDFHKL(1) = 511
              MDFHKL(2) = 511
              MDFHKL(3) = 511
            END IF
            I1 = MDFHKL(ISLOW)
            I2 = MDFHKL(INTER)
            I3 = MDFHKL(IFAST)
            CALL PACK(I1,I2,I3,IPOS2)
          END IF
        END IF
C
C---- Calculate position number for the new data
C
        I1 = IHKL(ISLOW)
        I2 = IHKL(INTER)
        I3 = IHKL(IFAST)
        CALL PACK(I1,I2,I3,IPOS1)
C
C---- Check whether this is final input reflection
C     final hkl should have indices 511, 511, 511
C     in that case copy input MDF hkl records to MDFout
C     and print counts
C
        IF (IPOS1.NE.LAST) THEN
   20     CONTINUE
C
C---- Compare with position number from MDF
C
          IF ((IPOS2-IPOS1).EQ.0) THEN
            GO TO 70
          ELSE IF ((IPOS2-IPOS1).LE.0) THEN
C
C---- New data ahead of MDF data: write MDF record to output MDF,
C     read a new record, and try again
C
            CALL WRTREC(MDFOUT,RH,MAXR,MDFHKL,BUFFER,MAXB,IDEL)
            IF (IDEL.NE.0) THEN
              NDELET = NDELET + 1
            ELSE
              NOTMOD = NOTMOD + 1
            END IF
            IF (MDFEND.EQ.0) THEN
              CALL RDREC(MDFIN,RH,MAXR,MDFHKL,BUFFER,MAXB,MDFEND,IVERSN)
              DO 30 I = 1,NDATA
                BUFFER(IPSNRS(I)) = ABSENT
   30         CONTINUE
              IF (MDFEND.NE.0) THEN
                MDFHKL(1) = 511
                MDFHKL(2) = 511
                MDFHKL(3) = 511
              ELSE
                NREAD = NREAD + 1
              END IF
              I1 = MDFHKL(ISLOW)
              I2 = MDFHKL(INTER)
              I3 = MDFHKL(IFAST)
              CALL PACK(I1,I2,I3,IPOS2)
            END IF
            GO TO 20
          END IF
C
C---- MDF hkl ahead of input hkl
C     create a new record for the MDF
C
C
C---- Fill rh array (here called rr in order not to screw up
C     the existing rh array
C
          DO 40 I = 1,MAXR
            RR(I) = ABSENT
   40     CONTINUE
          RR(1) = IHKL(1)*IHKL(1)*A11 + IHKL(1)*IHKL(2)*A21 +
     +            IHKL(1)*IHKL(3)*A31 + IHKL(2)*IHKL(2)*A22 +
     +            IHKL(2)*IHKL(3)*A32 + IHKL(3)*IHKL(3)*A33
C
C---- Fill in data buffer array, but only if ndata.gt.0
C
          IF ((NDATA).LE.0) THEN
            NSKIP = NSKIP + 1
          ELSE
            DO 50 I = 1,MAXB
              BF(I) = ABSENT
   50       CONTINUE
            DO 60 I = 1,NDATA
              BF(IPSNRS(I)) = DATA(I)
   60       CONTINUE
            CALL WRTREC(MDFOUT,RR,MAXR,IHKL,BF,MAXB,IDEL)
            IF (IDEL.EQ.1) THEN
              NDELET = NDELET + 1
            ELSE
              NNEW = NNEW + 1
            END IF
          END IF
          RETURN
C
C---- Input hkl and MDF hkl are the same:
C     add data to the data buffer and write the new data to the
C     output master data file. read a new record and return  to
C     the calling routine afterwards
C
   70     IF (NDATA.GT.0) THEN
            DO 80 I = 1,NDATA
              BUFFER(IPSNRS(I)) = DATA(I)
   80       CONTINUE
          END IF
          CALL WRTREC(MDFOUT,RH,MAXR,MDFHKL,BUFFER,MAXB,IDEL)
          IF (IDEL.EQ.1) THEN
            NDELET = NDELET + 1
          ELSE
            NMODIF = NMODIF + 1
          END IF
          IF (MDFEND.EQ.0) THEN
            CALL RDREC(MDFIN,RH,MAXR,MDFHKL,BUFFER,MAXB,MDFEND,IVERSN)
            DO 90 I = 1,NDATA
              BUFFER(IPSNRS(I)) = ABSENT
   90       CONTINUE
            IF (MDFEND.NE.0) THEN
              MDFHKL(1) = 511
              MDFHKL(2) = 511
              MDFHKL(3) = 511
            ELSE
              NREAD = NREAD + 1
            END IF
            I1 = MDFHKL(ISLOW)
            I2 = MDFHKL(INTER)
            I3 = MDFHKL(IFAST)
            CALL PACK(I1,I2,I3,IPOS2)
          END IF
        ELSE IF (MDFEND.NE.0) THEN
          WRITE (LUNOUT,FMT=6000) NREAD,NOTMOD,NMODIF,NNEW,NDELET,NSKIP
        ELSE
  100     CONTINUE
          CALL WRTREC(MDFOUT,RH,MAXR,MDFHKL,BUFFER,MAXB,IDEL)
          IF (IDEL.EQ.1) THEN
            NDELET = NDELET + 1
          ELSE
            NOTMOD = NOTMOD + 1
          END IF
          CALL RDREC(MDFIN,RH,MAXR,MDFHKL,BUFFER,MAXB,MDFEND,IVERSN)
          DO 110 I = 1,NDATA
            BUFFER(IPSNRS(I)) = ABSENT
  110     CONTINUE
          IF (MDFEND.EQ.0) THEN
            NREAD = NREAD + 1
            GO TO 100
          END IF
          WRITE (LUNOUT,FMT=6000) NREAD,NOTMOD,NMODIF,NNEW,NDELET,NSKIP
        END IF
      END IF
C
C---- Format statements
C
 6000 FORMAT (///' ADDING DATA TO A MASTER DATA FILE:',//' NUMBER OF R',
     +       'ECORDS READ FROM INPUT MDF',I30,/' NUMBER OF HKL RECORDS',
     +       ' WRITTEN UNMODIFIED TO OUTPUT MDF',I13,/' NUMBER OF HKL ',
     +       'RECORDS WITH NEW DATA WRITTEN TO OUTPUT MDF',I10,/' NUMB',
     +       'ER OF NEWLY CREATED HKL RECORDS WRITTEN TO OUTPUT MDF',
     +       I10,/' NUMBER OF (EMPTY) HKL RECORDS NOT WRITTEN TO OUTPU',
     +       'T MDF',I12,/' NUMBER OF INPUT HKLS WITH ZERO LENGTH DATA',
     +       ' BUFFER SKIPPED',I10,///)
C
C
      END
      SUBROUTINE RLIS(X1,X2,Y1,Y2,Y3,Y4,XI,YI)
      H=(Y3-Y1)/(Y2+Y3-Y4-Y1) 
      XI=X1+H*(X2-X1) 
      YI=Y1+H*(Y2-Y1) 
      RETURN
      END 
      SUBROUTINE RLSEE
      COMMON /RLS/ X1,X2,Y1,Y2,YH1,YH2
C 
      IF(Y1.GT.YH1) IL=1
      IF(Y1.LE.YH1) IL=2
C 
      IF(Y2.GT.YH2) IR=1
      IF(Y2.LE.YH2) IR=2
C 
      IF(IL.EQ.2.AND.IR.EQ.2)   RETURN
      IF(.NOT.(IL.EQ.1.AND.IR.EQ.1))   GO TO 10 
      CALL RLSEG(X1,Y1,X2,Y2) 
                             RETURN 
   10 IF(.NOT.(IL.EQ.1.AND.IR.EQ.2))   GO TO 20 
      CALL RLIS(X1,X2,YH1,YH2,Y1,Y2,XS,YS)
      CALL RLSEG(X1,Y1,XS,YS) 
                             RETURN 
   20 CONTINUE
C IL=2,IR=1 
      CALL RLIS(X1,X2,YH1,YH2,Y1,Y2,XS,YS)
      CALL RLSEG(XS,YS,X2,Y2) 
      RETURN
      END 
      SUBROUTINE RLSEG(X1,Y1,X2,Y2) 
C PLOT SEGMENT FROM (X1,Y1) TO (X2,Y2)
C 'OWN' VAR. SAVED IN /ORLSEG/
      COMMON  / ORLSEG/ XL,YL 
C INI FOR X1=999. 
      IF(X1.NE.999.)         GO TO 10 
      CALL PLOT(X2,Y2,3)
                             GO TO 20 
   10 CONTINUE
      IF(X1.NE.XL.OR.Y1.NE.YL) CALL PLOT(X1,Y1,3) 
      CALL PLOT(X2,Y2,2)
   20 CONTINUE
      XL=X2 
      YL=Y2 
      RETURN
      END 
C 
C 
      SUBROUTINE RLSURF(Z,MAXNX,NX,NY,XX,YY,YFAC) 
C PLOT RULED SURFACE PLOT 
C ABS(XX) = LENGTH X-AXIS 
C XX>0 : AXIS 
C XX<0 : NO AXES
C ABS(YY)= LENGTH OF Y-AXIS 
C YY>0 : USED AS SUBROUTINE 
C YY< 0 : USED IN FXY 
      DIMENSION Z(MAXNX,1),HU(200)
C /ZCTAX/ ALSO USED IN FXY
      COMMON /ZCTAX/NXL,IXLAB(8),XLEFT,XSF,NYL, 
     *              IYLAB(8),YLOW,YSF,QXAX,QYAX 
C LENGTH OF IXLAB,IYLAB MACH. DEP.  *************** 
C------- IN SUBROUTINE : PRESETTING BY INIAX( BY DATA STATEMENTS) 
C------- IN FXY : BY ZINI WHICH OVERRIDES THE INIAX-PRESETTING
      COMMON /RLS/ X1,X2,Y1,Y2,YH1,YH2
      XS=ABS(XX)
      YS=ABS(YY)
C TESTS 
      IF(YFAC.LE.0..OR.YFAC.GE.1.)     GO TO 1000 
      IF(NX.LT.1.OR.NX.GT.200)         GO TO 1000 
CBEGIN TEST Z 
      IF(YY.LT.0.0)          GO TO 2
C Z VALUES ALL EQUAL TO EACH OTHER ?
C NOT INF/INDEF/UNNORM. GRIDVALUES ?
      CALL TESTZ(Z,MAXNX,NX,NY,IERR,'RLSURF') 
      IF(IERR.GT.0)          RETURN 
    2 CONTINUE
CEND   TEST Z 
C 
      CALL ZMNMX(Z    ,MAXNX,NX,NY,ZMN,ZMX) 
C 
      SX=XS/(NX-1)
      SY=YS*(1.-YFAC)/(NY-1)
      H=YS*YFAC/(ZMX-ZMN) 
      YB=0. 
C 
      CALL PLOT(0.,0.,-3) 
      CALL PLOT(XS,0.,2)
      CALL PLOT(XS,YS,2)
      CALL PLOT(0.,YS,2)
      CALL PLOT(0.,0.,2)
C 
      IF(XX.LT.0.0)          GO TO 15 
C BEGIN PLOT AXIS 
C 
      CALL INIAX
      CALL AXSS(XS,(1.-YFAC)*YS,90.,NX,NY)
C 
C END   PLOT AXIS 
   15 CONTINUE
C PRESET HORIZON
      DO 20 I=1,NX
   20 HU(I)=0.
C 
C INI RLSEG 
      CALL RLSEG(999.,0.,0.,0.) 
C 
      DO 100 J=1,NY 
      X=0.
      STX=SX
      I2=1
      IS=1
      IF(MOD(J,2).EQ.0)      GO TO 25 
      X=XS
      I2=NX 
      STX=-SX 
      IS=-1 
   25 CONTINUE
C 
      X2=X
      YH2=HU(I2)
      Y2=(Z(I2,J)-ZMN)*H+YB 
      DO 30 II=2,NX 
      I1=I2 
      I2=I1+IS
C PRESET /RLS/
      X1=X2 
      X2=X1+STX 
      Y1=Y2 
      Y2=(Z(I2,J)-ZMN)*H+YB 
      YH1=YH2 
      YH2=HU(I2)
C 
      CALL RLSEE
C UPDATE HORIZON
      IF(Y1.GT.YH1)  HU(I1)=Y1
   30 CONTINUE
      IF(Y2.GT.YH2) HU(I2)=Y2 
      YB=YB+SY
  100 CONTINUE
C 
      CALL PLOT(XS+2.,0.,-3)
      RETURN
 1000 PRINT *,' RLSURF WITH ILLEGAL NX/NY/YFAC '
      RETURN
      END 
      SUBROUTINE ROTMAT( U, ANG, R)
C     =============================
C
C
      DIMENSION U( 3), R( 3, 3), K( 3, 3), V( 3)
      DATA K / 0, +3, -2,  -3, 0, +1,  +2, -1, 0/
C
C
      DTOR = 3.14159265/180.
      S = SIN( ANG * DTOR)
      C = COS( ANG * DTOR)
      UMAG = SQRT( U(1)*U(1)+U(2)*U(2)+U(3)*U(3))
C
C
      DO 999 I = 1, 3
        V( I) = U( I)/ UMAG   
999   CONTINUE
C
C
      DO 998 I = 1, 3
        DO 997 J = 1, 3
          R( I, J) = V( I) * V( J) * ( 1. - C)
C
C
          IF( I .EQ. J ) THEN
            R( I, J) = R( I, J) + C
          ELSE
            R( I, J) = R( I, J)+
     + ISIGN(1, K( I, J))*S*V( IABS( K( I, J)))
          END IF
C
C
997      CONTINUE
998      CONTINUE
C
C
      RETURN
C
C
      END

      SUBROUTINE RSC(IER) 
C 
C     THIS ROUTINE SCHEDULES PROGRAM "RSC" AND TEMPORARILY SUSPENDS 
C     THE CALLING PROGRAM UNTIL IS COMPLETE. THE RETURN PARAMETERS
C     FROM RSC ARE EXAMINED TO DETERMINE IF AN ERROR HAS OCCURED. 
C     IER=0  NO ERROR OCCURED.   IER=1  RSC TERMINATED ABNORMALLY.
C 
C     NOTES:
C        1) THE PEN MOTION FILE MUST BE DEASSIGNED PRIOR TO CALLING 
C           THIS ROUTINE USING THE COMMAND: CALL PLOT(0.,0.,999). 
C        2) THE PEN MOTION FILE MUST BE REINITIALIZED WITH THE COMMAND: 
C           CALL PLOTS(0) TO CONTINUE PLOTTING. 
C 
C        L. WEISSMAN   AUG. 1982
C 
	IER=1
	RETURN
      END 
      SUBROUTINE RTD(A)
C
C   SUBROUTINE FOR CONVERTING ANGLES FROM RADIANS TO DEGREES.
C
      DIMENSION      A(3)
C
      RTODEG = 45.0/ATAN(1.0)
      DO 100 I=1,3
  100 A(I)=A(I)*RTODEG
C
      RETURN
      END
      SUBROUTINE RTTRNS(R,T,X)
C///
C
C **  CALCULATES  X = [R]*X + T
C                 -       -   -
C     ENTERED AS COMMON LITTLE SR INTO THE LIBRARY:  01/04/77
      DIMENSION R(3,3), T(3), X(3), Y(3)
C\\\
      DO 100 I = 1,3
      Y(I) = X(I)
 100  CONTINUE
      DO 200 I = 1,3
      X(I) = T(I)
      DO 200 J = 1,3
      X(I) = X(I) + R(I,J)*Y(J)
 200  CONTINUE
      RETURN
      END

      SUBROUTINE SCALE (X,XTREME,S,N) 
COPYRIGHT (C) CERRITOS COMPUTER SERVICES, INC. 1976 
C 
C        X       ARRAY OF DATA TO BE SCANNED FOR MAXIMUM AND MINIMUM
C                VALUES.
C        XTREME  XMIN AND XMAX-XMIN RETURNED IN FIRST 2 LOCATIONS.
C                THESE WILL GIVE AXES WHICH ARE LABELLED CLEANLY. 
C        S       LENGTH(") OVER WHICH THIS DATA IS TO BE PLOTTED. 
C        N       NUMBER OF DATA POINTS IN THE ARRAY X.
C 
      REAL X(1),XM(5),XTREME(2) 
      DATA XM/2.0,4.0,5.0,8.0,10.0/ 
      NP=N
C 
C        FIND XMIN AND XMAX 
C 
      XMAX=X(1) 
      XMIN=XMAX 
      DO 100 I=1,NP 
      XI=X(I) 
      XMAX=AMAX1(XMAX,XI) 
100   XMIN=AMIN1(XMIN,XI) 
      IF(S.LE.0.0)GOTO 210
      DX=(XMAX-XMIN)/S
      IF(DX.LE.0.0)GOTO 210 
C 
C        FIND RANGE IN DECADES OF DATA
C 
      IDX=ALOG10(DX) 
      XI=10.0**IDX
      IF(XMIN)110,140,120 
110   XMIN=IFIX(XMIN-1.0) 
      XI=XI-0.99
120   XMIN=IFIX(XMIN/XI)
130   XMIN=XMIN*XI
140   XTREME(1)=XMIN
      T=((XMAX-XMIN)/S) 
      IF(T.LE.0.0)GOTO 210
      DX=ALOG10(T) 
      IDX=DX
      XMAX=1.0
      DX=10.0**(DX-IDX) 
C 
C        MAKE SURE VALUES ALONG AN AXIS GIVE REASONABLE APPEARANCE
C 
150   IF(DX-1.0)160,200,170 
160   DX=10.0*DX
      IDX=IDX-1 
      GOTO 150
170   DO 180 I=1,5
      XI=XM(I)
      IF(DX.LE.XM(I))GOTO 190 
180   CONTINUE
190   XMAX=XI 
200   XTREME(2)=XMAX*10.0**IDX
      RETURN
210   XTREME(2)=1.0 
      XTREME(1)=XMIN-0.5
      RETURN
      END 
C
C
C
      SUBROUTINE SCALEVECTOR( SCALAR, VECTOR, SCALEDVECTOR, NX)
C     ===========================================================
C
C
      DIMENSION VECTOR( NX), SCALEDVECTOR( NX)
C
C
      DO 999 I = 1, NX
        SCALEDVECTOR( I) = SCALAR * VECTOR( I)
999   CONTINUE
C
C
      RETURN
C
C
      END      
      SUBROUTINE SCAN(AM,M,N,CL,D,R,TH,HT,IZX)
C///  VERSION 83/05/19
C
C         SUBROUTINE OF  * C O N T U R *
C\\\
      DIMENSION AM(M,N)    ,IRC(1200),X(8000),Y(8000)
      DIMENSION IPT(3,3),INX(8),INY(8)
      COMMON MT,NT,NI,IX,IY,IDX,IDY,ISS,IT,IV,NP,NQ,JT,
     1PY,IRC,CV,X,Y,IPT,INX,INY,DL,RA,THE,NTP,RAT,H
      NP=0
      NSCAN=NSCAN+1
      PRINT 999,NSCAN
  999 FORMAT(' SCAN HAS BEEN CALLED ',I5,'  TIMES')
      N1=N
      H=HT
      DL=D
      RA=R
      THE=TH
      MT=M
      NT=N
      CV=CL
      IF (IZX)3,1,3
    1 IPT(1,1)=8
      IPT(1,2)=1
      IPT(1,3)=2
      IPT(2,1)=7
      IPT(2,3)=3
      IPT(3,1)=6
      IPT(3,2)=5
      IPT(3,3)=4
      IZX=2
103   INX(1)=-1
      INX(2)=-1
      INX(3)=0
      INX(4)=1
      INX(5)=1
      INX(6)=1
      INX(7)=0
      INX(8)=-1
      INY(1)=0
      INY(2)=1
      INY(3)=1
      INY(4)=1
      INY(5)=0
      INY(6)=-1
      INY(7)=-1
      INY(8)=-1
    3 CONTINUE
      DO 58 J=1,800
   58 IRC(J) = 0
      ISS=0
    2 MT1=MT-1
      DO 110 I=1,MT1
      IF (AM(I,1)-CV)55,110,110
   55 IF (AM(I+1,1)-CV)110,57,57
   57 IX=I+1
      IY=1
      IDX=-1
      IDY=0
      CALL TRACE(AM,M,N1)
  110 CONTINUE
      NT1=NT-1
      DO 20 I=1,NT1
      IF (AM(MT,I)-CV)15,20,20
   15 IF (AM(MT,I+1)-CV)20,17,17
   17 IX=MT
      IY=I+1
      IDX=0
      IDY=-1
      CALL TRACE(AM,M,N1)
   20 CONTINUE
   22 DO 30 I=1,MT1
      MT2=MT+1-I
      IF (AM(MT2,NT)-CV)25,30,30
   25 IF (AM(MT2-1,NT)-CV)30,27,27
   27 IX=MT2-1
      IY=NT
      IDX=1
      IDY=0
      CALL TRACE(AM,M,N1)
   30 CONTINUE
      DO 40 I=1,NT1
      NT2=NT+1-I
      IF (AM(1,NT2)-CV)35,40,40
35    IF (AM(1,NT2-1)-CV)40,37,37
   37 IX=1
      IY=NT2-1
      IDX=0
      IDY=1
      CALL TRACE(AM,M,N1)
   40 CONTINUE
      ISS=1
      NT1=NT-1
      MT1=MT-1
      DO 10 J=2,NT1
      DO 10 I=1,MT1
      IF (AM(I,J)-CV)5,10,10
    5 IF (AM(I+1,J)-CV)10,7,7
    7 ICM=100*(I+1)+J
      IF (NP)12,11,12
   12 DO 9 ID=1,NP
      IF (IRC(ID)-ICM)9,10,9
    9 CONTINUE
   11 IX=I+1
      IY=J
      IDX=-1
      IDY=0
      CALL TRACE(AM,M,N1)
   10 CONTINUE
      RETURN
      END

      SUBROUTINE SETBIT(IA,I,IB)
C
C        Set or clear the I'th bit of array IA to 1 (IB=1) or 0 (IB=0)
C
C        L. Weissman
C        Department of Biology, Gilmer Hall
C        University of Virginia
C        McCormick Road
C        Charlottesville, VA  22901
C
      BYTE IA(*),BIT0(8),BIT1(8)
      DATA BIT0/'376'O,'375'O,'373'O,'367'O,
     1         '357'O,'337'O,'277'O,'177'O/
      DATA BIT1/1,2,4,8,16,32,64,'200'O/
C
      IF(I)40,40,10
10    J=(I-1)/8
      K=I-8*J
      J=J+1
      IF(IB)20,30,20
C
C        Set the bit
C
20    IA(J)=IA(J) .OR. BIT1(K)
      GO TO 40
C
C        Clear the bit
C
30    IA(J)=IA(J) .AND. BIT0(K)
40    RETURN
      END
      SUBROUTINE SFCFIT(LX, LY, X, Y, Z, MX, MY, NU, NV, U, V, W)
C///
C
C     THIS  ROUTINE  IS  COPIED  FROM  ACCULIB
C
C     THE  SOURCE CODE  WAS  70119
C
C SMOOTH SURFACE FITTING
C THIS SUBROUTINE FITS A SMOOTH SURFACE OF A SINGLE-VALUED
C BIVARIATE FUNCTION Z = Z(X,Y) TO A SET OF INPUT DATA
C POINTS GIVEN AT INPUT GRID POINTS IN AN X-Y PLANE. IT
C GENERATES A SET OF OUTPUT GRID POINTS BY EQUALLY DIVIDING
C THE X AND Y COORDINATES IN EACH INTERVAL BETWEEN A PAIR
C OF INPUT GRID POINTS, INTERPOLATES THE Z VALUE FOR THE
C X AND Y VALUES OF EACH OUTPUT GRID POINT, AND GENERATES
C A SET OF OUTPUT POINTS CONSISTING OF INPUT DATA POINTS
C AND THE INTERPOLATED POINTS.
C THE METHOD IS BASED ON A PIECE-WISE FUNCTION COMPOSED OF
C A SET OF BICUBIC POLYNOMIALS IN X AND Y.  EACH POLYNOMIAL
C IS APPLICABLE TO A RECTANGLE OF THE INPUT GRID IN THE X-Y
C PLANE.  EACH POLYNOMIAL IS DETERMINED LOCALLY.
C THE INPUT PARAMETERS ARE
C LX = NUMBER OF INPUT GRID POINTS IN THE X COORDINATE
C       (MUST BE 2 OR GREATER)
C LY = NUMBER OF INPUT GRID POINTS IN THE Y COORDINATE
C       (MUST BE 2 OR GREATER)
C X  = ARRAY OF DIMENSION LX STORING THE X COORDINATES
C       OF INPUT GRID POINTS (IN ASCENDING OR DESCENDING
C       ORDER)
C Y   = ARRAY OF DIMENSION LY STORING THE Y COORDINATES
C       OF INPUT GRID POINTS (IN ASCENDING OR DECENDING
C       ORDER)
C Z   = DOUBLY-DIMENSIONED ARRAY OF DIMENSION (LX,LY)
C       STORING THE VALUES OF THE FUNCTION AT INPUT
C       GRID POINTS
C MX  = NUMBER OF SUBINTERVALS BETWEEN EACH PAIR OF
C       INPUT GRID POINTS IN THE X COORDINATE
C       (MUST BE 2 OR GREATER)
C MY  = NUMBER OF SUBINTERVALS BETWEEN EACH PAIR OF
C       INPUT GRID POINTS IN THE Y COORDINATE
C       (MUST BE 2 OR GREATER)
C NU  = NUMBER OF OUTPUT GRID POINTS IN THE X COORDINATE
C     = (LX-1)*MX+1
C NV  = NUMBER OF OUTPUT GRID POINTS IN THE Y COORDINATE
C     = (LY-1)*MY+1
C THE OUTPUT PARAMETERS ARE
C U   = ARRAY OF DIMENSION NU WHERE THE X COORDINATES OF
C       OUTPUT POINTS ARE TO BE DISPLAYED
C V   = ARRAY OF DIMENSION NV WHERE THE Y COORDINATES OF
C       OUTPUT POINTS ARE TO BE DISPLAYED
C W   = DOUBLY-DIMENSIONED ARRAY OF DIMENSION (NU,NV)
C       WHERE THE Z COORDINATES OF OUTPUT POINTS ARE TO
C       BE DISPLAYED
C SOME VARIABLES INTERNALLY USED ARE
C ZA  = DIVIDED DIFFERENCE OF Z WITH RESPECT TO X
C ZB  = DIVIDED DIFFERENCE OF Z WITH RESPECT TO Y
C ZAB = SECOND ORDER DIVIDED DIFFERENCE OF Z WITH
C       RESPECT TO X AND Y
C ZX  = PARTIAL DERIVATIVE OF Z WITH RESPECT TO X
C ZY  = PARTIAL DERIVATIVE OF Z WITH RESPECT TO Y
C ZXY = SECOND ORDER PARTIAL DERIVATIVE OF Z WITH
C       RESPECT TO X AND Y
C\\\
C DECLARATION STATEMENTS
      DIMENSION X(LX), Y(LY), Z(LX,LY), U(NU), V(NV), W(NU,NV)
      DIMENSION ZA(4,2), ZB(5), ZAB(2,3), ZX(2), ZY(2), ZXY(2)
      REAL ZZZ112(8)
      EQUIVALENCE (ZZZ112(1),ZA(1,1))
      REAL ZZZ113(6)
      EQUIVALENCE (ZZZ113(1),ZAB(1,1))
      EQUIVALENCE (Z3A2,ZA(1,1)), (Z3A3,ZA(2,1)), (Z3A4,ZA(3,1)),
     1       (Z3A5,ZA(4,1)), (Z4A2,ZZZ112(5)), (Z4A3,ZZZ112(6)), (Z4A4,Z
     2ZZ112(7)),           (Z4A5,ZZZ112(8)), (Z4B1,ZB(1)), (Z4B2,ZB(2)),
     3 (Z4B3,ZB(3)),           (Z4B4,ZB(4)), (Z4B5,ZB(5)), (ZA3B2,ZAB(1,
     41)), (ZA4B2,ZAB(2,1)),       (ZA3B3,ZZZ113(3)), (ZA4B3,ZZZ113(4)),
     5 (ZA3B4,ZZZ113(5)), (ZA4B4,ZZZ113(6)),   (ZX43,ZX(1)), (ZX44,ZX(2)
     6), (ZY43,ZY(1)), (ZY44,ZY(2)),           (ZXY43,ZXY(1)), (ZXY44,ZX
     7Y(2)), (P00,Z33), (P01,ZY33),            (P10,ZX33), (P11,ZXY33)
      EQUIVALENCE (IXM1,JX), (IXML,JY), (DU,DV,DX,DY),
     1 (FMX,RMX,FMY,RMY,SW,E), (W2,WY2,A,Q0), (W3,WY3,B,Q1),
     2 (WX2,C,Q2), (WX3,D,Q3), (Z3A2,P02), (Z4A2,P03), (Z4B1,P12),
     3 (Z4B2,P13), (Z4B4,P20), (Z4B5,P21), (ZA3B2,P22), (ZA3B4,P23)
C PRELIMINARY PROCESSING
C SETTING OF SOME INPUT PARAMETERS TO LOCAL VARIABLES
      LX0 = LX
      LXM1 = LX0 - 1
      LXM2 = LXM1 - 1
      LY0 = LY
      LYM1 = LY0 - 1
      LYM2 = LYM1 - 1
      MX0 = MX
      MXP1 = MX0 + 1
      MXM1 = MX0 - 1
      MY0 = MY
      MYP1 = MY0 + 1
      MYM1 = MY0 - 1
      NU0 = NU
      NV0 = NV
C ERROR CHECK
      IF (LXM2.LT.0) GO TO 400
      IF (LYM2.LT.0) GO TO 410
      IF (MXM1.LE.0) GO TO 420
      IF (MYM1.LE.0) GO TO 430
      IF (NU0.NE.LXM1*MX0+1) GO TO 440
      IF (NV0.NE.LYM1*MY0+1) GO TO 450
      IX = 2
      IF (X(1)-X(2)) 10, 460, 30
   10 DO 20 IX=3,LX0
         IF (X(IX-1)-X(IX)) 20, 460, 470
   20 CONTINUE
      GO TO 50
   30 DO 40 IX=3,LX0
         IF (X(IX-1)-X(IX)) 470, 460, 40
   40 CONTINUE
   50 IY = 2
      IF (Y(1)-Y(2)) 60, 490, 80
   60 DO 70 IY=3,LY0
         IF (Y(IY-1)-Y(IY)) 70, 490, 500
   70 CONTINUE
      GO TO 100
   80 DO 90 IY=3,LY0
         IF (Y(IY-1)-Y(IY)) 500, 490, 90
   90 CONTINUE
C COMPUTATION OF THE U ARRAY
  100 FMX = MX0
      RMX = 1.0/FMX
      KU = 1
      X4 = X(1)
      U(1) = X4
      DO 120 IX=2,LX0
         X3 = X4
         X4 = X(IX)
         DU = (X4-X3)*RMX
         DO 110 JX=1,MXM1
            KU = KU + 1
            U(KU) = U(KU-1) + DU
  110    CONTINUE
         KU = KU + 1
         U(KU) = X4
  120 CONTINUE
C COMPUTATION OF THE V ARRAY
      FMY = MY0
      RMY = 1.0/FMY
      KV = 1
      Y4 = Y(1)
      V(1) = Y4
      DO 140 IY=2,LY0
         Y3 = Y4
         Y4 = Y(IY)
         DV = (Y4-Y3)*RMY
         DO 130 JY=1,MYM1
            KV = KV + 1
            V(KV) = V(KV-1) + DV
  130    CONTINUE
         KV = KV + 1
         V(KV) = Y4
  140 CONTINUE
C MAIN DO-LOOPS
      JYMX = MY0
      KV0 = 0
      DO 390 IY=2,LY0
         IYM2 = IY - 2
         IYM3 = IYM2 - 1
         IYML = IY - LY0
         IYML1 = IYML + 1
         IX6 = 0
         IF (IYML.EQ.0) JYMX = MYP1
         JXMX = MX0
         KU0 = 0
         DO 380 IX=1,LX0
            IXM1 = IX - 1
            IXML = IX - LX0
            IF (IXML.EQ.0) JXMX = MXP1
C ROUTINES TO PICK UP NECESSARY X, Y, AND Z VALUES, TO
C COMPUTE THE ZA, ZB, AND ZAB VALUES, AND TO ESTIMATE THEM
C WHEN NECESSARY
C PRELIMINARY WHEN IX.EQ.1
            IF (IXM1.NE.0) GO TO 150
            Y3 = Y(IY-1)
            Y4 = Y(IY)
            B3 = 1.0/(Y4-Y3)
            B3SQ = B3*B3
            IF (IYM2.GT.0) B2 = 1.0/(Y3-Y(IY-2))
            IF (IYM3.GT.0) B1 = 1.0/(Y(IY-2)-Y(IY-3))
            IF (IYML.LT.0) B4 = 1.0/(Y(IY+1)-Y4)
            IF (IYML1.LT.0) B5 = 1.0/(Y(IY+2)-Y(IY+1))
            GO TO 180
C TO SAVE THE OLD VALUES
  150       Z3A2 = Z3A3
            Z4A2 = Z4A3
            X3 = X4
            Z33 = Z43
            Z3B3 = Z4B3
            A3 = A4
            A3SQ = A3*A3
            Z3A3 = Z3A4
            Z4A3 = Z4A4
            ZA3B2 = ZA4B2
            ZA3B3 = ZA4B3
            ZA3B4 = ZA4B4
  160       X4 = X5
            Z43 = Z53
            Z4B1 = Z5B1
            Z4B2 = Z5B2
            Z4B3 = Z5B3
            Z4B4 = Z5B4
            Z4B5 = Z5B5
            A4 = A5
            Z3A4 = Z3A5
            Z4A4 = Z4A5
            ZA4B2 = ZA5B2
            ZA4B3 = ZA5B3
            ZA4B4 = ZA5B4
  170       X5 = X6
            Z53 = Z63
            Z54 = Z64
            Z5B1 = Z6B1
            Z5B2 = Z6B2
            Z5B3 = Z6B3
            Z5B4 = Z6B4
            Z5B5 = Z6B5
C TO COMPUTE THE ZA, ZB, AND ZAB VALUES AND
C TO ESTIMATE THE ZB VALUES
C WHEN (IY.LE.3).OR.(IY.GE.LY-1)
  180       IX6 = IX6 + 1
            IF (IX6.GT.LX0) GO TO 260
            X6 = X(IX6)
            Z63 = Z(IX6,IY-1)
            Z64 = Z(IX6,IY)
            Z6B3 = (Z64-Z63)*B3
            IF (LYM2.EQ.0) GO TO 200
            IF (IYM2.EQ.0) GO TO 190
            Z62 = Z(IX6,IY-2)
            Z6B2 = (Z63-Z62)*B2
            IF (IYML.NE.0) GO TO 190
            Z6B4 = Z6B3 + Z6B3 - Z6B2
            GO TO 210
  190       Z65 = Z(IX6,IY+1)
            Z6B4 = (Z65-Z64)*B4
            IF (IYM2.NE.0) GO TO 210
            Z6B2 = Z6B3 + Z6B3 - Z6B4
            GO TO 210
  200       Z6B2 = Z6B3
            Z6B4 = Z6B3
  210       IF (IYM3.LE.0) GO TO 220
            Z6B1 = (Z62-Z(IX6,IY-3))*B1
            GO TO 230
  220       Z6B1 = Z6B2 + Z6B2 - Z6B3
  230       IF (IYML1.GE.0) GO TO 240
            Z6B5 = (Z(IX6,IY+2)-Z65)*B5
            GO TO 250
  240       Z6B5 = Z6B4 + Z6B4 - Z6B3
  250       IF (IX6.EQ.1) GO TO 170
            A5 = 1.0/(X6-X5)
            Z3A5 = (Z63-Z53)*A5
            Z4A5 = (Z64-Z54)*A5
            ZA5B2 = (Z6B2-Z5B2)*A5
            ZA5B3 = (Z6B3-Z5B3)*A5
            ZA5B4 = (Z6B4-Z5B4)*A5
            IF (IX6.EQ.2) GO TO 160
            GO TO 280
C TO ESTIMATE THE ZA AND ZAB VALUES
C WHEN (IX.GE.LX-1).AND.(LX.GT.2)
  260       IF (LXM2.EQ.0) GO TO 270
            Z3A5 = Z3A4 + Z3A4 - Z3A3
            Z4A5 = Z4A4 + Z4A4 - Z4A3
            IF (IXML.EQ.0) GO TO 290
            ZA5B2 = ZA4B2 + ZA4B2 - ZA3B2
            ZA5B3 = ZA4B3 + ZA4B3 - ZA3B3
            ZA5B4 = ZA4B4 + ZA4B4 - ZA3B4
            GO TO 290
C TO ESTIMATE THE ZA AND ZAB VALUES
C WHEN (IX.GE.LX-1).AND.(LX.EQ.2)
  270       Z3A5 = Z3A4
            Z4A5 = Z4A4
            IF (IXML.EQ.0) GO TO 290
            ZA5B2 = ZA4B2
            ZA5B3 = ZA4B3
            ZA5B4 = ZA4B4
C TO ESTIMATE THE ZA AND ZAB VALUES
C WHEN IX.EQ.1
  280       IF (IXM1.NE.0) GO TO 290
            Z3A3 = Z3A4 + Z3A4 - Z3A5
            Z3A2 = Z3A3 + Z3A3 - Z3A4
            Z4A3 = Z4A4 + Z4A4 - Z4A5
            Z4A2 = Z4A3 + Z4A3 - Z4A4
            ZA3B2 = ZA4B2 + ZA4B2 - ZA5B2
            ZA3B3 = ZA4B3 + ZA4B3 - ZA5B3
            ZA3B4 = ZA4B4 + ZA4B4 - ZA5B4
            GO TO 300
C NUMERICAL DIFFERENTIATION   ---   TO DETERMINE PARTIAL
C DERIVATIVES ZX, ZY, AND ZXY AS WEIGHTED MEANS OF DIVIDED
C DIFFERENCES ZA, ZB, AND ZAB, RESPECTIVELY
C TO SAVE THE OLD VALUES WHEN IX.NE.1
  290       ZX33 = ZX43
            ZX34 = ZX44
            ZY33 = ZY43
            ZY34 = ZY44
            ZXY33 = ZXY43
            ZXY34 = ZXY44
C NEW COMPUTATION
  300       DO 350 JY=1,2
               W2 = ABS(ZA(4,JY)-ZA(3,JY))
               W3 = ABS(ZA(2,JY)-ZA(1,JY))
               SW = W2 + W3
               IF (SW.EQ.0.0) GO TO 310
               WX2 = W2/SW
               WX3 = W3/SW
               GO TO 320
  310          WX2 = 0.5
               WX3 = 0.5
  320          ZX(JY) = WX2*ZA(2,JY) + WX3*ZA(3,JY)
               W2 = ABS(ZB(JY+3)-ZB(JY+2))
               W3 = ABS(ZB(JY+1)-ZB(JY))
               SW = W2 + W3
               IF (SW.EQ.0.0) GO TO 330
               WY2 = W2/SW
               WY3 = W3/SW
               GO TO 340
  330          WY2 = 0.5
               WY3 = 0.5
  340          ZY(JY) = WY2*ZB(JY+1) + WY3*ZB(JY+2)
               ZXY(JY) = WY2*(WX2*ZAB(1,JY)+WX3*ZAB(2,JY)) +
     1          WY3*(WX2*ZAB(1,JY+1)+WX3*ZAB(2,JY+1))
  350       CONTINUE
            IF (IXM1.EQ.0) GO TO 380
C DETERMINATION OF THE COEFFICIENTS OF THE POLYNOMIAL
            ZX3B3 = (ZX34-ZX33)*B3
            ZX4B3 = (ZX44-ZX43)*B3
            ZY3A3 = (ZY43-ZY33)*A3
            ZY4A3 = (ZY44-ZY34)*A3
            A = ZA3B3 - ZX3B3 - ZY3A3 + ZXY33
            B = ZX4B3 - ZX3B3 - ZXY43 + ZXY33
            C = ZY4A3 - ZY3A3 - ZXY34 + ZXY33
            D = ZXY44 - ZXY43 - ZXY34 + ZXY33
            E = A + A - B - C
            P02 = (2.0*(Z3B3-ZY33)+Z3B3-ZY34)*B3
            P03 = (-2.0*Z3B3+ZY34+ZY33)*B3SQ
            P12 = (2.0*(ZX3B3-ZXY33)+ZX3B3-ZXY34)*B3
            P13 = (-2.0*ZX3B3+ZXY34+ZXY33)*B3SQ
            P20 = (2.0*(Z3A3-ZX33)+Z3A3-ZX43)*A3
            P21 = (2.0*(ZY3A3-ZXY33)+ZY3A3-ZXY43)*A3
            P22 = (3.0*(A+E)+D)*A3*B3
            P23 = (-3.0*E-B-D)*A3*B3SQ
            P30 = (-2.0*Z3A3+ZX43+ZX33)*A3SQ
            P31 = (-2.0*ZY3A3+ZXY43+ZXY33)*A3SQ
            P32 = (-3.0*E-C-D)*B3*A3SQ
            P33 = (D+E+E)*A3SQ*B3SQ
C COMPUTATION OF THE POLYNOMIAL
            DO 370 JY=1,JYMX
               KV = KV0 + JY
               DY = V(KV) - Y3
               Q0 = P00 + DY*(P01+DY*(P02+DY*P03))
               Q1 = P10 + DY*(P11+DY*(P12+DY*P13))
               Q2 = P20 + DY*(P21+DY*(P22+DY*P23))
               Q3 = P30 + DY*(P31+DY*(P32+DY*P33))
               DO 360 JX=1,JXMX
                  KU = KU0 + JX
                  DX = U(KU) - X3
                  W(KU,KV) = Q0 + DX*(Q1+DX*(Q2+DX*Q3))
  360          CONTINUE
  370       CONTINUE
            KU0 = KU0 + MX0
  380    CONTINUE
         KV0 = KV0 + MY0
  390 CONTINUE
C NORMAL EXIT
      RETURN
C ERROR EXIT
  400 PRINT 99999
      GO TO 520
  410 PRINT 99998
      GO TO 520
  420 PRINT 99997
      GO TO 520
  430 PRINT 99996
      GO TO 520
  440 PRINT 99995
      GO TO 520
  450 PRINT 99994
      GO TO 520
  460 PRINT 99993
      GO TO 480
  470 PRINT 99992
  480 PRINT 99991, IX, X(IX)
      GO TO 520
  490 PRINT 99990
      GO TO 510
  500 PRINT 99989
  510 PRINT 99988, IY, Y(IY)
  520 PRINT 99987, LX0, MX0, NU0, LY0, MY0, NV0
      RETURN
C FORMAT STATEMENTS
99999 FORMAT (1X/23H  ***   LX = 1 OR LESS./)
99998 FORMAT (1X/23H  ***   LY = 1 OR LESS./)
99997 FORMAT (1X/23H  ***   MX = 1 OR LESS./)
99996 FORMAT (1X/23H  ***   MY = 1 OR LESS./)
99995 FORMAT (1X/26H  ***   IMPROPER NU VALUE./)
99994 FORMAT (1X/26H  ***   IMPROPER NV VALUE./)
99993 FORMAT (1X/27H  ***   IDENTICAL X VALUES./)
99992 FORMAT (1X/33H  ***   X VALUES OUT OF SEQUENCE./)
99991 FORMAT (7H   IX =, I6, 10X, 7HX(IX) =, E12.3)
99990 FORMAT (1X/27H  ***   IDENRICAL Y VALUES./)
99989 FORMAT (1X/33H  ***   Y VALUES OUT OF SEQUENCE./)
99988 FORMAT (7H   IY =, I6, 10X, 7HY(IY) =, E12.3)
99987 FORMAT (7H   LX =, I6, 10X, 4HMX =, I6, 10X, 4HNU =, I6/7H   LY =,
     1 I6, 10X, 4HMY =, I6, 10X, 4HNV =, I6/6H ERROR, 14H DETECTED IN R,
     2 16HOUTINE    SFCFIT)
      END
      SUBROUTINE SORTAG (A, N, TAG)
C///
C
C       SUBROUTINE SORTAG
C
C        DECK USED - CDC 6500, IBM360/67
C
C        SORTAG
C
C        PURPOSE
C
C           TO SORT A VECTOR INTO INCREASING ORDER FROM A(1) TO A(N),
C           A MAY BE TYPE REAL OR TYPE INTEGER.  VECTOR TAG IS PER-
C           MUTED THE SAME AS VECTOR A.
C
C        USAGE
C
C           CALL SORTAG(A,N,TAG)
C
C        DESCRIPTION OF PARAMETERS
C
C           A - THE NAME OF THE N-VECTOR TO BE SORTED.
C               IF A IS TYPE REAL THEN EACH OF ITS COMPONENTS MUST BE
C               NORMALIZED FORM.
C           N - THE NUMBER OF ELEMENTS IN THE VECTOR TO BE SORTED.
C           TAG - THE NAME OF THE N-VECTOR CONTAINING THE TAG FIELDS.
C
C        REMARKS
C
C           THE PROCEDURE REQUIRES TWO ADDITIONAL ARRAYS IU(K) AND
C           IL(K) WHICH PERMIT SORTING UP TO 2**(K+1)-1 ELEMENTS.
C           THESE ARRAYS ARE SUPPLIED BY THE SUBROUTINE WITH K = 16.
C           THIS ALLOWS SORTING A MAXIMUM OF 131071 ELEMENTS.
C
C        EXAMPLE
C
C           IF N RANDOM NUMBERS, UPON GENERATION, WERE ASSIGNED
C           CONSECUTIVE TAGS OF 1 THROUGH N, AND THE ARRAY OF RANDOM
C           NUMBERS SORTED, THE TAG ARRAY COULD THEN BE USED TO
C           DETERMINE THAT THE RANDOM NUMBER NOW IN POSITION A(I) WAS
C           ORIGINALLY IN POSITION A(J) BECAUSE TAG(I) = J.
C
C        METHOD
C
C           'AN EFFICIENT ALGORITHM FOR SORTING WITH MINIMAL STORAGE'
C           BY RICHARD C. SINGLETON.  PREPARED FOR INSTITUTE RESEARCH
C           AND DEVELOPMENT.  STANFORD RESEARCH INSTITUTE PROJECT
C           387531-132 SEPTEMBER 1968.
C
C     ..................................................................
C
C      RECEIVED FROM BAUKE DIJKSTR - DEC 1985 - VAX VERSION.
C
C=========================================================================
C      VERSION 23 DECEMBER 1985
C=========================================================================
C\\\
C
      DIMENSION A(N),IU(16),IL(16),TAG(N)
      INTEGER A,T,TT,TAG,TG
      M=1
      I=1
      J=N
    5 IF(I .GE. J) GO TO 70
   10 K=I
      IJ=(J+I)/2
      T=A(IJ)
      IF(A(I) .LE. T) GO TO 20
      A(IJ)=A(I)
      A(I)=T
      T=A(IJ)
      TG=TAG(IJ)
      TAG(IJ)=TAG(I)
      TAG(I)=TG
   20 L=J
      IF(A(J) .GE. T) GO TO 40
      A(IJ)=A(J)
      A(J)=T
      T=A(IJ)
      TG=TAG(IJ)
      TAG(IJ)=TAG(J)
      TAG(J)=TG
      IF(A(I) .LE. T) GO TO 40
      A(IJ)=A(I)
      A(I)=T
      T=A(IJ)
      TG=TAG(IJ)
      TAG(IJ)=TAG(I)
      TAG(I)=TG
      GO TO 40
   30 A(L)=A(K)
      A(K)=TT
      TG=TAG(L)
      TAG(L)=TAG(K)
      TAG(K)=TG
   40 L=L-1
      IF(A(L) .GT. T) GO TO 40
      TT=A(L)
   50 K=K+1
      IF(A(K) .LT. T) GO TO 50
      IF(K .LE. L) GO TO 30
      IF(L-I .LE. J-K) GO TO 60
      IL(M)=I
      IU(M)=L
      I=K
      M=M+1
      GO TO 80
   60 IL(M)=K
      IU(M)=J
      J=L
      M=M+1
      GO TO 80
   70 M=M-1
      IF(M.EQ.0) RETURN
      I=IL(M)
      J=IU(M)
80    IF(J-I.GE.1) GOTO 10
      IF(I.EQ.1) GOTO 5
      I=I-1
   90 I=I+1
      IF(I .EQ. J) GO TO 70
      T=A(I+1)
      IF(A(I) .LE. T) GO TO 90
      TG=TAG(I+1)
      K=I
  100 A(K+1)=A(K)
      TAG(K+1)=TAG(K)
      K=K-1
      IF(T .LT. A(K)) GO TO 100
      A(K+1)=T
      TAG(K+1)=TG
      GO TO 90
      END
C
C
C
      SUBROUTINE SRTAG(A,N,TAG)
C     ============================
C
C
C
C       SUBROUTINE SRTAG
C
C        DECK USED - CDC 6500, IBM360/67
C
C
C---- PURPOSE
C     =======
C
C     To sort a vector into increasing order from a(1) to a(n),
C     a may be type real or type integer.  vector tag is per-
C     muted the same as vector a.
C
C
C---- USAGE
C     =====
C
C           CALL SRTAG(A,N,TAG)
C
C
C---- DESCRIPTION OF PARAMETERS
C     =========================
C
C           A - the name of the N-vector to be sorted.
C               if A is type real then each of its components must be
C               normalized form.
C
C           N - the number of elements in the vector to be sorted.
C           TAG - the name of the n-vector containing the tag fields.
C
C
C---- REMARKS
C     =======
C
C           The procedure requires two additional arrays iu(k) and
C           il(k) which permit sorting up to 2**(k+1)-1 elements.
C           these arrays are supplied by the subroutine with k = 16.
C           this allows sorting a maximum of 131071 elements.
C           to allow sorting of more elements, the value of k has to
C           be increased (to a max of 32 for 64 bit computers).
C           eg: k = 17 allows sorting of 262143 elements
C
C
C---- EXAMPLE
C     =======
C
C           If N random numbers, upon generation, were assigned
C           consecutive tags of 1 through n, and the array of random
C           numbers sorted, the tag array could then be used to
C           determine that the random number now in position a(i) was
C           originally in position a(j) because tag(i) = j.
C
C
C---- METHOD
C     ======
C
C           'an efficient algorithm for sorting with minimal storage'
C           by richard c. singleton.  prepared for institute research
C           and development.  stanford research institute project
C           387531-132 september 1968.
C
C
C---- RECEIVED FROM BAUKE DIJKSTR - DEC 1985 - VAX VERSION.
C     ====================================================
C
C      VERSION 89-11-29
C
C
C
C     .. Scalar Arguments ..
      INTEGER N
C     ..
C     .. Array Arguments ..
      INTEGER A(N),TAG(N)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      INTEGER I,IJ,J,K,L,M,T,TG,TT
C     ..
C     .. Local Arrays ..
      INTEGER IL(16),IU(16)
C     ..
C     .. Common blocks ..
      COMMON /AINOUT/LUNIN,LUNOUT
C     ..
C
C
      M = 1
      I = 1
      J = N
   10 IF (I.GE.J) GO TO 80
   20 K = I
      IJ = (J+I)/2
      T = A(IJ)
      IF (A(I).LE.T) GO TO 30
      A(IJ) = A(I)
      A(I) = T
      T = A(IJ)
      TG = TAG(IJ)
      TAG(IJ) = TAG(I)
      TAG(I) = TG
   30 L = J
      IF (A(J).GE.T) GO TO 50
      A(IJ) = A(J)
      A(J) = T
      T = A(IJ)
      TG = TAG(IJ)
      TAG(IJ) = TAG(J)
      TAG(J) = TG
      IF (A(I).LE.T) GO TO 50
      A(IJ) = A(I)
      A(I) = T
      T = A(IJ)
      TG = TAG(IJ)
      TAG(IJ) = TAG(I)
      TAG(I) = TG
      GO TO 50
   40 A(L) = A(K)
      A(K) = TT
      TG = TAG(L)
      TAG(L) = TAG(K)
      TAG(K) = TG
   50 L = L - 1
      IF (A(L).GT.T) GO TO 50
      TT = A(L)
   60 K = K + 1
      IF (A(K).LT.T) GO TO 60
      IF (K.LE.L) GO TO 40
      IF (L-I.LE.J-K) GO TO 70
      IL(M) = I
      IU(M) = L
      I = K
      M = M + 1
      GO TO 90
   70 IL(M) = K
      IU(M) = J
      J = L
      M = M + 1
      GO TO 90
   80 M = M - 1
      IF (M.EQ.0) RETURN
      I = IL(M)
      J = IU(M)
   90 IF (J-I.GE.1) GO TO 20
      IF (I.EQ.1) GO TO 10
      I = I - 1
  100 I = I + 1
      IF (I.EQ.J) GO TO 80
      T = A(I+1)
      IF (A(I).LE.T) GO TO 100
      TG = TAG(I+1)
      K = I
  110 A(K+1) = A(K)
      TAG(K+1) = TAG(K)
      K = K - 1
      IF (T.LT.A(K)) GO TO 110
      A(K+1) = T
      TAG(K+1) = TG
      GO TO 100
C
C
      END

      SUBROUTINE SUBST(INSTR,OUTSTR,I,J)
C 
C        Transfer a substring of the input string "INSTR" to the
C        output string "OUTSTR". Characters from positions "I"
C        to "J" are transferred.
C        Nothing happens if I,J < 1 or I > J.
C 
C        L. Weissman
C        Department of Biology, Gilmer Hall
C        University of Virginia
C        McCormick Road
C        Charlottesville, VA  22901
C
      BYTE INSTR(*),OUTSTR(*)
C 
      IF(MIN0(I,J,1+J-I) .LT. 1)GO TO 20
      K1=0
      DO 10 K=I,J
      K1=K1+1
      CALL MOVBYT(INSTR,K,OUTSTR,K1)
10    CONTINUE
20    RETURN
      END 
      SUBROUTINE SYM4
C           -----------
C           !  SYM4  !
C           -----------
      INCLUDE 'CORELS.DIM'
C-----------------------------------------------------------------------
C	
C     SYM SPECIFIC FOR SPACE GROUP NO.4, P21 WITH B AXIS UNIQUE
C	WRITTEN BY A.G.W. LESLIE, 20TH MAY 1983
C	******* NB  PIMULT IS DEFINED AS -TWOPI IE NEGATIVE 
C
C-----------------------------------------------------------------------
      INTEGER*2 SF,LIMROT,IFATN,NPG
      INTEGER   ADDSUB,ERATOM,PRTFLG
      COMMON /MISC/ DVZERO,ERRD,ERRF,ERRT,IB,IC,IC2,ICORR
     1  ,IG,ISPARS,IW,JV,JP,MAN,NAG,NATB,NATG,NBLKS,ERATOM,ADDSUB
     2  ,NC,NCPBLK,NDWT,NG,NF,NGPBL1,NGPBL2,NQ,NQ1,NRST,NV
     3  ,SCLSHF,WTARG,WTDIS,WTSF,ERRR,NCYCCG,NO,NM,MM,PRTFLG,COSLIM
      COMMON /PI/PIMULT,DEGTRD,RADTDG,TWOPI,SYMULT,ISYM
      PARAMETER MTIMES=2**10,KTIMES=MTIMES/4,ALTIME=MTIMES+KTIMES+4
      PARAMETER ADDON=FLOAT(2*MTIMES*MTIMES)+0.5
      COMMON/SINS/STAB(ALTIME)
      COMMON /W/ RHO,A,B,H(4),IPT,IP,IQ,IATOM
      COMMON /CDD2/  XYZ(3, XNATM)
      COMMON /CDD4/  SF( XNATM)
      COMMON /CEE1/  LIMROT(4, XNP)
      COMMON /CEE3/  P( XNP)
      COMMON /CHH2/  NPG(3, XNG)
      COMMON /CHX1/  IFATN( XNG1)
      COMMON /CNN1/  DADX(4, XNAG)
      COMMON /CNN2/  DBDX(4, XNAG)
      COMMON /ZZ35/  FTAB(400, XFI)
      COMMON /ZZ43/  FTAT( XFI)
      COMMON /ZZ44/  TFHP(3, XFI)
      J=0
      NTH=NPG(2,IG)
      NATZ=IFATN(IG)
      IT=IP+3
      SYMPIQ = SYMULT*P(IQ)
      DO 70 II=1,NTH
         NATB=LIMROT(1,(II+IT)) + NATZ
         NATG=LIMROT(2,(II+IT)) + NATZ
         BETA=P(IT+II)
         TFI=EXP(-BETA*RHO)*SYMPIQ
         DO 20 ISCAT=1,NF
            FTAT(ISCAT)=TFI*FTAB(IPT,ISCAT)
            DO 10 IH=1,3
               TFHP(IH,ISCAT)=FTAT(ISCAT)*H(IH)*PIMULT
   10       CONTINUE
   20    CONTINUE
        	DO 60 I=NATB,NATG
		SFI=SF(I)		
	        J=J+1
        	IF(SFI.EQ.0) GO TO 60
		ITHX=DBLE(H(1)*XYZ(1,I))+DBLE(H(3)*XYZ(3,I))+ADDON
		ITHX=ITHX.AND.(MTIMES-1) 
		SHL=STAB(ITHX+1)
		CHL=STAB(ITHX+1+KTIMES)
		ITHX=DBLE(H(2)*XYZ(2,I))+ADDON
		ITHX=ITHX.AND.(MTIMES-1) 
		SK=STAB(ITHX+1)
		CK=STAB(ITHX+1+KTIMES)
		SHLSK=SHL*SK
		SHLCK=SHL*CK
		CHLSK=CHL*SK
		CHLCK=CHL*CK
		FTATSF=FTAT(SFI)
		TFHP1=TFHP(1,SFI)
		TFHP2=TFHP(2,SFI)
		TFHP3=TFHP(3,SFI)
C		K ODD OR EVEN ?
		IF (JMOD(JINT(H(2)),2).NE.0) GO TO 30
C		K EVEN TERMS
C		STRUCTURE FACTOR TERMS
		FTACOS=FTATSF*CHLCK
		FTASIN=FTATSF*CHLSK
C		SKIP DERIVATIVES IF NOT REQUIRED
		IF (WTSF.NE.1) GO TO 50
C		DERIVATIVES WRT POSITIONAL PARAMETERS
		DADX(1,J)= TFHP1*SHLCK
		DADX(2,J)= TFHP2*CHLSK
		DADX(3,J)= TFHP3*SHLCK
		DBDX(1,J)= TFHP1*SHLSK
		DBDX(2,J)=-TFHP2*CHLCK
		DBDX(3,J)= TFHP3*SHLSK
		GO TO 35
C		K ODD TERMS
C		STRUCTURE FACTOR TERMS
   30		FTACOS=-FTATSF*SHLSK
		FTASIN= FTATSF*SHLCK
C		SKIP DERIVATIVES IF NOT REQUIRED
		IF (WTSF.NE.1) GO TO 50
C		DERIVATIVES WRT POSITIONAL PARAMETERS
		DADX(1,J)= TFHP1*CHLSK
		DADX(2,J)= TFHP2*SHLCK
		DADX(3,J)= TFHP3*CHLSK
		DBDX(1,J)=-TFHP1*CHLCK
		DBDX(2,J)= TFHP2*SHLSK
		DBDX(3,J)=-TFHP3*CHLCK
C		DERIVATIVES WRT THERMAL PARAMETERS
   35		DADX(4,J)=-RHO*FTACOS
		DBDX(4,J)=-RHO*FTASIN
   50	    A=A+FTACOS
            B=B+FTASIN
   60    CONTINUE
   70 CONTINUE
      RETURN
      END

      SUBROUTINE SYMBL(XA,YA,HI,ASC,TH,NC)
C
C     Print a character string or a single character by its
C     ASCII value (0.0 to 127.0). If 0.0<ASC<19.0 a special
C     centered symbol is printed.
C
C     Parameters:
C        XA   Starting x-coord of lower left of string
C        YA   Starting y-coord of lower left of string
C        HI   Height of characters in string
C        ASC  Text string or single ascii value (non-character type)
C        TH   Angle of orientation relative to horizontal (deg)
C        NC   Number of characters
C             0 = no action
C            -1 = single symbol with value ASC
C            -2 = draw a line to XA,YA before drawing a single symbol
C            +N = draw a string of N characters
C
C     The encoding scheme is as follows:
C        1) Let JCH be the ASCII value of a character.
C        2) Let I3=ISYM(JCH+1)+1. Then I3 points to the first byte of
C           ISTB containing the character formation instructions.
C        3) Byte I3 of ISTB codes for the first pen movement as follows:
C
C       Bits          Meaning
C        7(msb)       First byte only:
C                       0 = No descender, 1 = descenders
C                     Subsequent bytes:
C                       0 = More codes to follow
C                       1 = This code completes the character
C        6            0 = Pen down; 1 = Pen up for this segment
C        5-3          IY value for the pen movement
C        2-0(lsb)     IX value for the pen movement
C
C        Note: the width of the symbol is 6/7 times it height
C      Subroutines required:
C        PLOT  - PXLIB plot routine
C        IBYTE - Returns numerical value of a byte in range 0-255
C
      REAL ASC(1)
      LOGICAL NODROP
      INTEGER*2 ISYM(128),ISTB(450),IS1(128),IS129(128),IS257(128),
     1   IS385(41)
      EQUIVALENCE (ISTB(1),IS1),(ISTB(129),IS129),(ISTB(257),IS257),
     1   (ISTB(385),IS385)
      DATA ISYM/
     1   0,  8, 20, 26, 31, 36, 43, 50, 55, 62, 68, 82, 91, 98,101,104,
     2 111,118,125,134,143,143,143,143,143,143,143,143,143,143,143,143,
     3 143,145,155,160,169,180,193,205,209,214,219,226,231,235,238,241,
     4 243,254,260,269,280,286,296,307,313,331,341,346,353,357,362,366,
     5 375,387,394,406,415,423,430,435,445,451,458,464,469,473,478,484,
     6 494,501,513,522,535,540,547,551,557,565,573,580,585,588,592,598,
     7 604,607,618,628,635,646,656,662,676,682,690,698,704,710,720,727,
     8 737,748,760,765,773,779,787,791,797,801,811,816,824,829,837,842/
      DATA IS1/
     1  10842,  2088, 11276, -9686, 10842,  8233,  2320,  5131,
     2  11044, -9686, 10842,  3080, -9686,  2666,  7256, 18650,
     3  26668, -9716, 10842,  2584, 10780, 23258, 10776,  6684,
     4  -9718, 11336,  3112, 26842,  2092, 22796, -9701, 10330,
     5   6764, -9718, 11354,  8547, 24872,  2065,  4945, 21260,
     6  -9693, 11336,  3176, 10826,  7256, 23258, 11304,  3080,
     7  -9702, 10826, 22746, -9700,  7256, 11368,  3144, 24794,
     8  20516, 18708, -9685, 10842,  9057,  4945, 19162, 25106,
     1  22570, 23321, -9700,  2129,  3155, 11363, 10337, 16602,
     2  16838,  2562,   265, 12626,  4658, 25030, 29489,-14813,
     3  12609,   883,  9312,  4180, 18630,  5131,  6427, 10528,
     4  29228,-14846, 11336, 10345, 12592, 17449,  2819,  1036,
     5  21702,   258,  4104, 11043, 10546,  1049, 25030, 12842,
     6  29638,  4385,-14845,  9073,   275, 19142, 25642, 24592,
     7 -14828, 10826,  7256,-16186,  4361, 22726,-14820,   321,
     8  13510, 18630, 12584, 11315,   780,  2049,-14804,   833/
      DATA IS129/
     1  12866,-14807, 12648, 11315,  2084,  1024, 28870, 11316,
     2   6938,  3092,   259,-14840, 13123,  4120,-14828,   328,
     3   3075,  8988, 12320,-14796,  7000,  3092,   259,  8200,
     4  13362, 28870, 11316,   273, 16838,  3075,  6932,  8217,
     5  12584, 11315,  6948,  4185,   264,   710, 11284, 12595,
     6   8232,  7193, 16838, 20737,-14831, 16832,  4618,  8802,
     7  17350, 13080, 20678, 25620,-14816,  7233,-14799,   578,
     8   9298, 13100, 10289, 17350,  2049, 12584, 11315,  4628,
     1  11290,  8390,  9266, 20484,-14828, 13104,  9260,  6171,
     2   5211,   780,-14848, 13164, 10289,   264,  3075,   966,
     3  11276, 12339,   369, 12486, 23348, 16408,-14844, 13360,
     4   6234, 27846, 12595,  2088,  1025,  4884, 12486,  7256,
     5   1140, 16838, 16899, 28978,-14797,   328,  3075,-14796,
     6  29744,  1048, 12486,  1088, 12486, 13338,-14844, 26672,
     7  29708,-14844,   328,  3075, 13100, 10289,-14840, 13104,
     8   9260,  6171, 18630,   769, 11276, 12595,  2088,  1106/
      DATA IS257/
     1  12486, 11315,  6948, 23064,-14844,   328,  3075,  6932,
     2   8217, 12584, 11315, 28870, 29236,-14846,  2160,   769,
     3  13324, 28870, 13314, 28870,  6657, 13315,  2246, 13356,
     4  10352,  1036, 28870,  6696, 29698,  6700, 28870, 11316,
     5      8,-14844, 12402,   512, 17604,-14800, 12802,-15312,
     6  10840, 27164,-14838,  6218, 22570,-14820,  7024, 18630,
     7   8472,  5154,   258, 25608,-14844, 20528,  8994,  3100,
     8    515,-14832,  8548,  2072,  1025, 29894, 21508,  8482,
     1   2072,   513,-14828,  5200,  8988,  6177,   264,-14845,
     2  10818, 22835,-14821, 16832,  3075, 25652, 12594,  6184,
     3   4625,-14812, 24624,  7203,-14844,   833,  6722, 27161,
     4 -14806, 16832,  2818, 29475,-14797, 27696, 23048,-14844,
     5    833, 12866,-14799, 22560,  6689, 23042,  7203,-14844,
     6  22560,  8993,  1052, 18630,  8472,  7203,   780,  2049,
     7 -16186, 24624, 13106,  7212,  4627,-14816, 17600, 25652,
     8  12594,  6184,  4625,-14812, 20512,  9250,   966,  4876/
      DATA IS385/
     1   6161,  9249, 25030, 29219,   778, 24774,   264,  3075,
     2   1124, 24774,  9218, 24774,  4609,  9219,  9414,  1120,
     3 -16186,   833, 13324,  6256,  5137, 24774,    36,-14844,
     4   2370,  6161, 10529,-15054,  4674, 12898, 16838,  4618,
     5   8731, 12586, 20677,  6937,-14812, 13360,     4, 28724, -14844/
C
10    X=XA
      Y=YA
      H=TH*0.0174532925
      SI=SIN(H)
      CO=COS(H)
      H=HI/7.0
      IP=3
C
C        Move to (XA,YA) with pen down if NC=-2
C
      IF(NC.EQ.-2)IP=2
      CALL PLOT(XA,YA,IP)
      IF(NC)20,30,40
C
C        NC negative: draw a single character.
C
20    NC0=0
      JCH=ASC(1)+0.1
      GO TO 60
30    RETURN
C
C        NC positive: draw a string.
C
40    NC0=NC
      I0=0
50    NC0=NC0-1
      IF(NC0.LT.0)GO TO 30
      I0=1+I0
C
C        Extract 1 byte from the string for drawing
C
      JCH=IBYTE(ASC,I0)
60    IF(JCH .LT. 0 .OR. JCH .GT. 127)JCH=32
      I3=ISYM(1+JCH)
C
C        Descenders?
C
      NODROP=.TRUE.
      I1=IBYTE(ISTB,I3+1)
      IF(I1 .LT. 128)GO TO 70
      NODROP=.FALSE.
      I3=1+I3
      I1=I1-128
      IP=3
      DX=+2.0*H*SI
      DY=-2.0*H*CO
      X=X+DX
      Y=Y+DY
      CALL PLOT(X,Y,IP)
      GO TO 90
C
C        Special symbol: move down and left to get it centered.
C
70    IF(JCH.GE.32)GO TO 80
      IP=3
      I1=0
      X=X+H*(-2.*CO+3.*SI)
      Y=Y+H*(-3.*CO-2.*SI)
      XW=X
      YW=Y
      GO TO 100
C
C        Extract the code from the character table
C
80    IP=2
      I3=1+I3
      I1=IBYTE(ISTB,I3)
      IF(I1.GE.64)IP=3
90    I2=MOD(I1,64)
      IY=I2/8
      IX=I2-8*IY
      X1=IX
      Y1=IY
      XW=X+H*(X1*CO-Y1*SI)
      YW=Y+H*(Y1*CO+X1*SI)
100   CALL PLOT(XW,YW,IP)
      IF(I1.LT.128)GO TO 80
      X=XW
      Y=YW
      IF(NODROP)GO TO 50
      X=X-DX
      Y=Y-DY
      CALL PLOT(X,Y,3)
      GO TO 50
      END
      SUBROUTINE SYMBOL(X,Y,HEIGHT,LARAY,ANGLE,ICTL)                    
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C DRAWS A LABEL CHARACTER STRING OR SPECIAL CENTERED SYMBOL AT          
C A SPECIFIED LOCATION, SIZE AND ANGLE.                                 
C                                                                       
C   PARAMETERS :                                                        
C                                                                       
C   X,Y       = THE COORDINATES OF THE FIRST CHARACTER TO BE PRODUCED   
C                                                                       
C   HEIGHT    = THE HEIGHT OF THE CHARACTER TO BE PLOTTED               
C                                                                       
C   LARAY     = THE ARRAY CONTAINING CHARACTERS TO BE PRODUCED          
C                                                                       
C   ANGLE     = THE ANGLE AT WHICH THE CHARACTERS ARE TO BE PLOTTED     
C                                                                       
C   ICTL > 0   THE NUMBER OF CHARACTERS IN 'LARAY' TO BE DRAWN          
C                                                                       
C   ICTL = 0   DRAW ONLY THE SINGLE CHARACTER WHICH IS RIGHT-JUSTIFIED  
C              IN LARAY(1).  THIS IS THE ONLY WAY TO REFERENCE SOME OF  
C              THE CHARACTERS WHEN THE HOST COMPUTER STORES CHARACTERS  
C              INTERNALLY IN FEWER THAN EIGHT BITS.                     
C                                                                       
C   ICTL < 0   DRAW THE SPECIAL SYMBOL REPRESENTED BY                   
C              THE INTEGER IN LARAY(1).                                 
C                                                                       
C              IF ICTL = -1, THE X,Y POINT IS PLOTTED WITH              
C              THE PEN UP.  THE SPECIAL SYMBOLS ARE DRAWN WITH          
C              NO CONNECTING LINES.                                     
C                                                                       
C              IF ICTL = -2, THE X,Y POINT IS PLOTTED WITH              
C              THE PEN DOWN.  THE SPECIAL SYMBOLS ARE DRAWN             
C              WITH CONNECTING LINES.                                   
C                                                                       
C *******************************************************************   
C                                                                       
C *** COMMON BLOCK                                                      
C                                                                       
      COMMON /HP/ XMINPU, YMINPU, XORG  , YORG  , XOFF  , YOFF    ,     
     +            XFACT , YFACT , XCNTRL, YCNTRL, XYUNIT          ,     
     +            MDEV  , MUNIT , MPENS , MLU   , MCNTRL, MINIT   ,     
     +            MSHAKE, MPTR  , MCODE , MPLOT , MFLUSH, MSIZE   ,     
     +            MSPOOL, MVER  , MOUT(72),                             
     +            RSI1  , RSI2  , RDI1   , RDI2  , RES1  , RES2         
C                                                                       
C     ***** XYUNIT - CONTAINS THE CURRENT NUMBER OF PLOTTER UNITS PER   
C                    INCH OR CENTIMETER                                 
C                                                                       
C     ***** MDEV   - CONTAINS DEVICE TYPE ( 7220,7470,7475,             
C                     7580,7585, 9872 )                                 
C                                                                       
C                                                                       
C     ***** MSIZE  - CONTAINS THE SIZE OF THE OUTPUT ARRAY MOUT         
C                                                                       
C                                                                       
C ********************************************************************  
      LOGICAL CENTER                                                    
      DIMENSION LARAY(1),IA(12),IB(2),IBUFF(2),XBUFF(2),KBUFF(72)       
      DIMENSION JARAY(68),IWORK(1)                                      
      DIMENSION NOF(68),NSYM(16)                                        
      DATA IA( 1)/83/,IA( 2)/73/,IA( 3)/83/,IA( 4)/83/                  
C                  S          I          S          S                   
      DATA IA( 5)/83/,IA( 6)/65/,IA( 7)/76/,IA( 8)/66/                  
C                  S          A          L          B                   
      DATA IA( 9)/68/,IA(10)/73/,IA(11)/69/,IA(12)/83/                  
C                  D          I          E          S                   
      DATA IB( 1)/3/,IB( 2)/59/                                         
C                ETX         ;                                          
C                                                                       
C STROKE TABLE DATA FOR CENTERED SYMBOLS                                
C                                                                       
      DATA NSYM( 1)/2025/,NSYM( 2)/2534/,NSYM( 3)/3437/                 
      DATA NSYM( 4)/4751/,NSYM( 5)/5357/,NSYM( 6)/4347/                 
      DATA NSYM( 7)/1620/,NSYM( 8)/5659/,NSYM( 9)/6068/                 
      DATA NSYM(10)/1216/,NSYM(11)/0112/,NSYM(12)/4757/                 
      DATA NSYM(13)/5760/,NSYM(14)/4748/,NSYM(15)/3442/                 
      DATA NSYM(16)/5151/                                               
      DATA NOF( 1)/44/,NOF( 2)/24/,NOF( 3)/15/,NOF( 4)/24/              
      DATA NOF( 5)/22/,NOF( 6)/11/,NOF( 7)/22/,NOF( 8)/42/              
      DATA NOF( 9)/51/,NOF(10)/42/,NOF(11)/44/,NOF(12)/55/              
      DATA NOF(13)/33/,NOF(14)/15/,NOF(15)/33/,NOF(16)/31/              
      DATA NOF(17)/35/,NOF(18)/13/,NOF(19)/53/,NOF(20)/35/              
      DATA NOF(21)/15/,NOF(22)/11/,NOF(23)/51/,NOF(24)/55/              
      DATA NOF(25)/35/,NOF(26)/25/,NOF(27)/14/,NOF(28)/12/              
      DATA NOF(29)/21/,NOF(30)/41/,NOF(31)/52/,NOF(32)/54/              
      DATA NOF(33)/45/,NOF(34)/35/,NOF(35)/12/,NOF(36)/52/              
      DATA NOF(37)/35/,NOF(38)/34/,NOF(39)/14/,NOF(40)/31/              
      DATA NOF(41)/54/,NOF(42)/34/,NOF(43)/35/,NOF(44)/13/              
      DATA NOF(45)/31/,NOF(46)/53/,NOF(47)/35/,NOF(48)/31/              
      DATA NOF(49)/33/,NOF(50)/13/,NOF(51)/53/,NOF(52)/33/              
      DATA NOF(53)/15/,NOF(54)/51/,NOF(55)/33/,NOF(56)/11/              
      DATA NOF(57)/55/,NOF(58)/15/,NOF(59)/51/,NOF(60)/11/              
      DATA NOF(61)/51/,NOF(62)/11/,NOF(63)/55/,NOF(64)/15/              
      DATA NOF(65)/55/,NOF(66)/33/,NOF(67)/23/,NOF(68)/43/              
      CENTER=.FALSE.                                                    
      XP=X                                                              
      YP=Y                                                              
C                                                                       
C**** CHECK FOR VALID X,Y                                               
C                                                                       
      XLOW=-32767.0                                                     
      XHIGH=32767.0                                                     
      YLOW=XLOW                                                         
      YHIGH=XHIGH                                                       
      IF (XP .LT.XLOW) XP=XLOW                                          
      IF (XP .GT. XHIGH) XP=XHIGH                                       
      IF (YP .LT. YLOW) YP=YLOW                                         
      IF (YP .GT. YHIGH) YP=YHIGH                                       
C                                                                       
C**** CHECK FOR VALID CHARACTER HEIGHT                                  
C                                                                       
      SIZE=HEIGHT                                                       
C                                                                       
C**** IF NOT VALID, DEFAULT TO 0.14 ( SAME AS LABEL SIZE USED           
C**** BY THE AXIS SUBROUTINE )                                          
C                                                                       
      IF (SIZE .LE. 0.0) SIZE=0.14                                      
      IF (SIZE .GT. 32767.0) SIZE=32767.0                               
C                                                                       
C**** CREATE MODULO 360 ANGLE                                           
C                                                                       
      ANGL=ANGLE-(AINT(ANGLE/360.0)*360.0)                              
C                                                                       
C**** SET CHARACTER WIDTH,HEIGHT ( SI )                                 
C                                                                       
C                                                                       
C**** IS THIS A SPECIAL CENTERED SYMBOL FOR THE 7580 ?                  
C                                                                       
      IF (ICTL.LT.0 .AND.                                               
     +(MDEV.EQ.7580.OR.MDEV.EQ.7585.OR.MDEV.EQ.7586.OR.MDEV.EQ.7550))   
     + GO TO 10                                                         
C                                                                       
C**** NO CENTERED SYMBOL                                                
C                                                                       
      XBUFF(2)=(SIZE*XYUNIT)*0.0025                                     
      XBUFF(1)=XBUFF(2)*.6                                              
      GO TO 20                                                          
C                                                                       
C**** CENTERED SYMBOL AND THE DEVICE IS A 7580                          
C                                                                       
   10 XBUFF(1)=(SIZE*XYUNIT)*0.0025                                     
      XBUFF(2)=XBUFF(1)*1.333                                           
C                                                                       
C**** IF NEW PARAMS EQUAL OLD, THEN NO NEED TO SEND THEM                
C                                                                       
  20  IF (XBUFF(1).EQ.RSI1.AND.XBUFF(2).EQ.RSI2) GO TO 25               
      CALL BUFF(1,IA(1),XBUFF(1),2)                                     
      CALL BUFF(6,IBUFF(1),XBUFF(1),2)                                  
      RSI1=XBUFF(1)                                                     
      RSI2=XBUFF(2)                                                     
C                                                                       
C**** IS THE PLOTTER A 7580 ?                                           
C                                                                       
  25  CONTINUE                                                          
      IF(MDEV.NE.7580.AND.MDEV.NE.7585.AND.MDEV.NE.7586.AND.            
     +   MDEV.NE.7550) GO TO 30                                         
C                                                                       
C**** YES, SET MORE SPACE BETWEEN CHARACTERS ( ES )                     
C                                                                       
      XBUFF(1)=0.10                                                     
      XBUFF(2)=0.0                                                      
C                                                                       
C**** NO NEED TO SEND ES IF PARAMS ARE CURRENT                          
C                                                                       
      IF (XBUFF(1).EQ.RES1.AND.XBUFF(2).EQ.RES2) GO TO 30               
      RES1=XBUFF(1)                                                     
      RES2=XBUFF(2)                                                     
      CALL BUFF(1,IA(11),XBUFF(1),2)                                    
      CALL BUFF(6,IBUFF(1),XBUFF(1),2)                                  
C                                                                       
C**** SET ROTATION ANGLE ( DI )                                         
C                                                                       
  30  CONTINUE                                                          
      XBUFF(1)=COS(ANGL*(71.0/4068.0))*100.0                            
      XBUFF(2)=SIN(ANGL*(71.0/4068.0))*100.0                            
      IF (XBUFF(1).EQ.RDI1.AND.XBUFF(2).EQ.RDI2) GO TO 35               
      RDI1=XBUFF(1)                                                     
      RDI2=XBUFF(2)                                                     
      CALL BUFF(1,IA(9),XBUFF(1),2)                                     
      CALL BUFF(6,IBUFF(1),XBUFF(1),2)                                  
C----------------------------------------                               
C DETERMINE HEIGHT AND WIDTH OF CELL                                    
C                                                                       
  35  CONTINUE                                                          
      RUN=XBUFF(1)/100.0                                                
      RISE=XBUFF(2)/100.0                                               
C                                                                       
C----------------------------------------                               
C                                                                       
C**** SPECIAL CENTERED SYMBOL ?                                         
C                                                                       
      IF (ICTL.LT.0) GO TO 140                                          
C                                                                       
C**** TEST FOR CONTIGUOUS LABEL                                         
C                                                                       
      IF (XP.NE.999.0.AND.YP.NE.999.0) GO TO 50                         
      IF (XP.EQ.999.0.AND.YP.EQ.999.0) GO TO 60                         
C                                                                       
C**** OBTAIN CURRENT POSITION                                           
C                                                                       
      CALL WHERE(XNOW,YNOW,DF)                                          
      IF (XP.EQ.999.0) GO TO 40                                         
C                                                                       
C**** SET UP FOR CONTIGUOUS LABEL IN Y                                  
C                                                                       
      YP=YNOW                                                           
      GO TO 50                                                          
C                                                                       
C SET UP FOR CONTIGUOUS LABEL IN X                                      
C                                                                       
   40 XP=XNOW                                                           
C                                                                       
C**** MOVE TO X,Y WITH PEN UP.                                          
C                                                                       
   50 IPEN=3                                                            
      CALL PLOT(XP,YP,IPEN)                                             
   60 IF (ICTL.EQ.0) GO TO 70                                           
      INUM=ICTL                                                         
      GO TO 90                                                          
C                                                                       
C**** PROCESS SINGLE-CHARACTER LABEL.                                   
C                                                                       
   70 ITEMP=LARAY(1)                                                    
      CALL ZZLOW(ITEMP,JTEMP)                                           
      IWORK(1)=JTEMP                                                    
      CALL ZZPACK(IWORK,LARAY,1)                                        
   80 INUM=1                                                            
C                                                                       
C**** SET UP TO PROCESS THE LABEL STRING                                
C                                                                       
   90 KPTR=1                                                            
      NPTR=1                                                            
      LOOP=(INUM/(MSIZE-2))+1                                           
C----------------------------------------                               
C THIS IS FOR POSITION TRACKING                                         
C                                                                       
C # OF CHARS AND LINES                                                  
C                                                                       
      NLINE=0                                                           
      NSPACE=ICTL                                                       
C                                                                       
C----------------------------------------                               
C                                                                       
C**** PREPARE THE LABEL STRING                                          
C                                                                       
      DO 120 I = 1,LOOP                                                 
          LSIZE=34                                                      
          IF (LOOP .EQ. I .AND. INUM .EQ. 1) LSIZE=1                    
          IF (LOOP .EQ. I .AND. INUM .GT. 1) LSIZE=(INUM/2)+1           
C                                                                       
C**** PLACE LABEL MODE COMMAND IN THE WORK AREA ( LB )                  
C                                                                       
          KBUFF(KPTR)=IA(7)                                             
          KBUFF(KPTR+1)=IA(8)                                           
          KPTR=KPTR+2                                                   
C                                                                       
C**** UNPACK THE LABEL STRING                                           
C                                                                       
      NSIZE=LSIZE*2                                                     
      DO 100 K = 1,NSIZE                                                
             CALL ZZEXTR(LARAY(NPTR),NSIZE,K,LCHAR)                     
             JARAY(K)=LCHAR                                             
  100      CONTINUE                                                     
C                                                                       
C**** PLACE THE UNPACKED STRING IN THE WORK AREA                        
C                                                                       
          LEN=LSIZE*2                                                   
          IF (LOOP .EQ. I) LEN=INUM                                     
      DO 110 J = 1,LEN                                                  
              KBUFF(KPTR)=JARAY(J)                                      
              KPTR=KPTR+1                                               
C----------------------------------------                               
C KEEP TRACK OF SPACES, BACKSPACES,LINFEED,INV LINEFEED                 
C                                                                       
      JCH=JARAY(J)                                                      
      IF (JCH.GT.31) GO TO 105                                          
        IF (JCH.NE.8.AND.JCH.NE.13) NSPACE=NSPACE-1                     
        IF (JCH.EQ.8) NSPACE=NSPACE-2                                   
        IF (JCH.EQ.10) NLINE=NLINE-1                                    
        IF (JCH.EQ.11) NLINE=NLINE + 1                                  
        IF (JCH.EQ.13) NSPACE=ICTL-NSPACE                               
  105 CONTINUE                                                          
C                                                                       
C----------------------------------------                               
  110       CONTINUE                                                    
C                                                                       
C**** PLACE LABEL TERMINATOR ( ETX; ) IN THE WORK AREA                  
C                                                                       
          KBUFF(KPTR)=IB(1)                                             
          KBUFF(KPTR+1)=IB(2)                                           
          KPTR=KPTR+1                                                   
C                                                                       
C**** PLACE THE PREPARED LABEL STRING IN THE OUTPUT BUFFER              
C                                                                       
          CALL BUFF(1,KBUFF(1),XBUFF(1),KPTR)                           
          KPTR=1                                                        
          NPTR=NPTR+34                                                  
          INUM=INUM-(MSIZE-3)                                           
  120   CONTINUE                                                        
      IF (.NOT.CENTER) GO TO 130                                        
C                                                                       
C**** IF SPECIAL SYMBOL INVOKE STANDARD FONT ( SS; )                    
C                                                                       
      CALL BUFF(1,IA(3),XBUFF(1),-2)                                    
C                                                                       
C**** IF SINGLE CHARACTER OR CENTERED SYMBOL                            
C**** RESTORE ORIGINAL CONTENTS OF CHARACTER                            
C**** ARRAY                                                             
C                                                                       
  130 IF(ICTL .LE. 0) LARAY(1)=ITEMP                                    
      GO TO 220                                                         
C                                                                       
C**** IS THE DEVICE A 7220,7470,7475, OR 9872 ?                         
C                                                                       
  140 IF (MDEV.NE.7580.AND.MDEV.NE.7585.AND.MDEV.NE.7586.AND.           
     +    MDEV.NE.7550) GO TO 160                                       
C                                                                       
C**** IF SPECIAL SYMBOL INVOKE ALTERNATE FONT AND MOVE TO X,Y ( SA; )   
C                                                                       
  150 CALL BUFF(1,IA(5),XBUFF(1),-2)                                    
  160 IPEN=3                                                            
C                                                                       
C**** TEST FOR CONTIGUOUS LABEL                                         
C                                                                       
      IF (XP.NE.999.0.AND.YP.NE.999.0) GO TO 180                        
      IF (XP.EQ.999.0.AND.YP.EQ.999.0) GO TO 200                        
C                                                                       
C**** OBTAIN CURRENT POSITION                                           
C                                                                       
      CALL WHERE(XNOW,YNOW,DF)                                          
      IF (XP.EQ.999.0) GO TO 170                                        
C                                                                       
C**** SET UP FOR CONTIGUOUS SYMBOL IN Y                                 
C                                                                       
      YP=YNOW                                                           
      GO TO 180                                                         
C                                                                       
C**** SET UP FOR CONTIGUOUS SYMBOL IN X                                 
C                                                                       
  170 XP=XNOW                                                           
  180 IF(ICTL .LT. (-1)) IPEN=2                                         
      CALL PLOT(XP,YP,IPEN)                                             
C                                                                       
C**** IS THE DEVICE A 7220,7470,7475,OR 9872 ?                          
C                                                                       
      IF (MDEV.NE.7580.AND.MDEV.NE.7585.AND.MDEV.NE.7586                
     +  .AND.MDEV.NE.7550) GO TO 200                                    
  190 CENTER=.TRUE.                                                     
C                                                                       
C**** ADJUST ARRAY INTEGER VALUE TO SELECT THE SPECIAL SYMBOLS          
C                                                                       
      ITEMP=LARAY(1)                                                    
      KTEMP=LARAY(1)+65                                                 
C                                                                       
C**** PREPARE THE VALUE JUST AS WE WOULD A SINGLE CHARACTER             
C                                                                       
      CALL ZZLOW(KTEMP,JTEMP)                                           
      IWORK(1)=JTEMP                                                    
      CALL ZZPACK(IWORK,LARAY,1)                                        
      GO TO 80                                                          
C                                                                       
C**** DEVICE IS  7220,7470,7475, 0R 9872 - THE CENTERED SYMBOLS         
C**** MUST BE PRODUCED USING THE STROKE TABLE                           
C                                                                       
  200 COSTH=COS(ANGL*(71.0/4068.0))*100.0                               
      SINTH=SIN(ANGL*(71.0/4068.0))*100.0                               
      XAX=SIZE*COSTH*0.0025                                             
      YAY=SIZE*SINTH*0.0025                                             
      K=LARAY(1)                                                        
      K=NSYM(K+1)                                                       
      M=K/100                                                           
      N=K-M*100                                                         
      DO 210 I = M,N                                                    
          J=NOF(I)                                                      
          XN=J/10-3                                                     
          YN=MOD(J,10)-3                                                
          CALL PLOT(XP+XAX*XN-YAY*YN,YP+XAX*YN+YAY*XN,2)                
  210   CONTINUE                                                        
      CALL PLOT(XP,YP,2)                                                
  220 CONTINUE                                                          
C-----------------------------------------------                        
C CALCULATE NEW POSITION                                                
C                                                                       
      IF(ICTL.LT.0) GO TO 250                                           
      XCELL=FLOAT(NSPACE)*SIZE*XYUNIT*0.6*1.5                           
      YCELL=FLOAT(NLINE)*SIZE*XYUNIT*2.0                                
      IF(MDEV.EQ.7580.OR.MDEV.EQ.7585.OR.MDEV.EQ.7586.OR.MDEV.EQ.7550)  
     + XCELL=XCELL*1.10                                                 
      XRUN=(XCELL+YCELL)*RUN                                            
      YRISE=(XCELL+YCELL)*RISE                                          
      XCNTRL=XCNTRL+XRUN                                                
      YCNTRL=YCNTRL+YRISE                                               
  250 CONTINUE                                                          
C                                                                       
C-----------------------------------------------                        
      RETURN                                                            
      END                                                               
      SUBROUTINE SYMCHK
C///
C=======================================================================
C
C               S U B R O U T I N E    S Y M C H K
C
C     SUBROUTINE TO CHECK THAT SYMMETRY OPERATIONS FORM A CLOSED GROUP
C     AND A UNIQUE SET, AND THAT THE FIRST OPERATION IS THE IDENTITY.
C
C=======================================================================
C
C     A) CHECK THAT THE FIRST OPERATION IS THE IDENTITY.
C
C     B) CHECK WHETHER ANY OPERATIONS ARE GIVEN REDUNDANTLY.
C
C     C) FOR A GROUP, OPERATION OF ANY OPERATOR ON THE WHOLE GROUP
C        SHOULD REGENERATE THE GROUP.  VERIFY THIS.
C
C     NOTES:  NSYM SHOULD BE AT LEAST ONE, BUT WE'LL CHECK THIS ANYWAY.
C             TESTS FOR IDENTITY OF SYMMETRY OPERATORS ALLOW FOR UNIT
C             CELL TRANSLATIONS.
C             BECAUSE WE CAN'T ANTICIPATE ALL UNUSUAL THINGS SOMEONE
C             MIGHT WANT TO DO WITH SYMMETRIES, FAILURES OF ANY OF THE
C             CHECKS ONLY RESULT IN WARNING MESSAGES, NOT A HALT.
C
C     REQUIRED LIBRARY SUBROUTINE :  XMAT33
C     ADDITIONAL ATTACHED ROUTINES:  SYMCOM, TRNSFM
C
C=======================================================================
C
C     VERSION 19 MARCH 1987   R J READ
C
C=======================================================================
C\\\
      COMMON /MDFSYM/ NSYM, SYMROT(3,3,96), SYMTRL(3,96)
      REAL ROTID(3,3), TRLID(3), ROT12(3,3), TRL12(3)
      LOGICAL SAME,GROUP,FOUND(96)
      DATA ROTID/1.0,0.0,0.0, 0.0,1.0,0.0, 0.0,0.0,1.0/
      DATA TRLID/3*0.0/
C
C     CHECK NSYM.
C
      IF ( (NSYM .LT. 1) .OR. (NSYM .GT. 96) ) THEN
        WRITE(*,10) NSYM
10      FORMAT(/,' ***ERROR*** NSYM OF',I3,' IS OUT OF RANGE')
        STOP ' BAD VALUE OF NSYM'
      ENDIF
C
C     CHECK THAT FIRST OPERATION IS IDENTITY.
C
      CALL SYMCOM(ROTID,TRLID,SYMROT(1,1,1),SYMTRL(1,1),SAME)
      IF (.NOT. SAME) WRITE(*,20)
20    FORMAT(/,1X,70('*'),//,
     $      ' ***WARNING*** FIRST OPERATOR IS NOT IDENTITY',//,
     $      1X,70('*'),/)
C
C     CHECK FOR REDUNDANCY AND GROUP CLOSURE.  ONLY CHECK REDUNDANCY
C     WHEN NS2>NS1.  FOR EACH OPERATION NS1, SET FOUND(NSGEN) TO .TRUE.
C     WHEN AN OPERATION NS2 APPLIED TO IT GENERATES OPERATION NSGEN.
C
      DO 100 NS1=1,NSYM
        DO 30 NSGEN=1,NSYM
          FOUND(NSGEN) = .FALSE.
30      CONTINUE
        DO 70 NS2=1,NSYM
          IF (NS2 .GT. NS1) THEN
            CALL SYMCOM(SYMROT(1,1,NS1),SYMTRL(1,NS1),
     $                  SYMROT(1,1,NS2),SYMTRL(1,NS2),SAME)
            IF (SAME) WRITE(*,40) NS1,NS2
40          FORMAT(/,1X,70('*'),//,' ***WARNING*** SYMM OPS',I3,' AND',
     $             I3,' ARE REDUNDANT',//,1X,70('*'),/)
          ENDIF
          CALL XMAT33(SYMROT(1,1,NS2),SYMROT(1,1,NS1),ROT12)
          CALL TRNSFM(SYMROT(1,1,NS2),SYMTRL(1,NS1),SYMTRL(1,NS2),TRL12)
          GROUP = .FALSE.
          DO 50 NSGEN=1,NSYM
            CALL SYMCOM(ROT12,            TRL12,
     $                  SYMROT(1,1,NSGEN),SYMTRL(1,NSGEN),SAME)
            IF (SAME) THEN
              FOUND(NSGEN) = .TRUE.
              GROUP = .TRUE.
            ENDIF
50        CONTINUE
          IF (.NOT. GROUP)
     $        WRITE(*,60) NS1,NS2,((ROT12(I,J),J=1,3),TRL12(I),I=1,3)
60        FORMAT(/,1X,70('*'),//,' ***WARNING*** SYMM OPS',I3,' AND',
     $           I3,' GENERATE THE FOLLOWING NON-GROUP OPERATION:',/,
     $           3(/,5X,3F5.0,5X,F10.6),//,1X,70('*'),/)
70      CONTINUE
        DO 90 NSGEN=1,NSYM
          IF (.NOT. FOUND(NSGEN)) WRITE(*,80) NS1,NSGEN
80        FORMAT(/,1X,70('*'),//,' ***WARNING*** FOR SYMM OP',I3,
     $           ', NO OTHER OPERATION GENERATES SYMM OP',I3,//,
     $           1X,70('*'),/)
90      CONTINUE
100   CONTINUE
      WRITE(*,110)
110   FORMAT(/,' CHECKING OF SYMMETRY OPERATIONS IS COMPLETE',/)
      RETURN
      END
      SUBROUTINE SYMCOM(ROT1,TRL1,ROT2,TRL2,SAME)
      REAL ROT1(3,3),TRL1(3),ROT2(3,3),TRL2(3)
      LOGICAL SAME
      SAME = .TRUE.
      DO 10 I=1,3
        CHK = ABS(TRL1(I)-TRL2(I))
        CHK = ABS(CHK - FLOAT(NINT(CHK)))
        IF (CHK .GT. 0.0001) SAME = .FALSE.
        DO 10 J=1,3
          CHK = ABS(ROT1(I,J)-ROT2(I,J))
          IF (CHK .GT. 0.0001) SAME = .FALSE.
10    CONTINUE
      RETURN
      END
      SUBROUTINE SYMFRE
C///
C=======================================================================
C
C               S U B R O U T I N E    S Y M F R E
C
C     A SUBROUTINE TO READ FREE-FORMAT SYMMETRY OPERATIONS.
C
C=======================================================================
C
C     SYMFRE WAS TAKEN FROM PLUTO (W. D. S. MOTHERWELL), AND ADAPTED
C     BY MASAO FUJINAGA, THEN BY RANDY READ.
C     IT READS NSYM, THEN NSYM FREE-FORMAT SYMMETRY OPERATIONS,
C     AND PUTS THEM INTO /MDFSYM/.  SYMMETRY CARDS ARE 80 CHARACTERS
C     LONG, THE FIRST 4 CHARACTERS MUST BE 'SYMM' AND THE OPERATION IS
C     EXPECTED AFTER THE FIRST BLANK.  OPERATIONS SHOULD BE GIVEN
C     AS IN THE INTERNATIONAL TABLES; TRANSLATIONS ARE GIVEN AS
C     FRACTIONS OF A CELL EDGE, EITHER AS RATIONAL NUMBERS (WITH
C     A SLASH) OR DECIMAL NUMBERS (WITH A POINT).  BLANKS ARE IGNORED.
C     HERE IS AN EXAMPLE FOR SPACE GROUP P3(2)21, ILLUSTRATING THE
C     FLEXIBILITY OF THE ROUTINE.
C
C     'NSYM' 6
C     SYMM X,Y,Z
C     SYMMETRY          -Y,    + X - Y,   2  / 3 + Z
C     SYMM Y-X, -X, Z+1/3
C     SYMM Y, X, -Z
C     SYMM -X, -X+Y, 0.6666667-Z
C     SYMM X-Y, -Y, -Z+0.3333333
C
C=======================================================================
C\\\
C
C     IF, FOR SOME REASON, YOU WANT TO CHANGE IMAX, REMEMBER FORMAT 10.
C
      PARAMETER (IMAX=80)
      CHARACTER*1 COL(IMAX),INUM(10),CHECK(4),ICH
      CHARACTER*1 IX,IY,IZ,IPLUS,IMINUS,ISLASH,IPOINT,ICOMMA,IBLANK
      CHARACTER*4 CARDID
      INTEGER NUM(10)
      COMMON /MDFSYM/ NSYM, SYMROT(3,3,96), SYMTRL(3,96)
      DATA IX,IY,IZ,IPLUS,IMINUS,ISLASH,IPOINT,ICOMMA,IBLANK / 'X' ,
     1 'Y' , 'Z' , '+' , '-' , '/' , '.' , ',' , ' ' /
      DATA NUM/1,2,3,4,5,6,7,8,9,0/
      DATA INUM/'1','2','3','4','5','6','7','8','9','0'/
      DATA CHECK/'S','Y','M','M'/
      READ(*,*,END=250,ERR=250) CARDID,NSYM
      IF (CARDID .EQ. 'NSYM') THEN
        WRITE(*,4) NSYM
4       FORMAT('0',I5,' FREE-FORMAT SYMMETRY OPERATIONS EXPECTED')
        IF (NSYM .LE. 0) THEN
          WRITE(*,5)
5         FORMAT('0***ERROR*** NSYM MUST BE AT LEAST 1')
          STOP ' NSYM TOO SMALL'
        ENDIF
        IF (NSYM .GT. 96) THEN
          WRITE(*,6)
6         FORMAT('0***ERROR*** TOO MANY SYMMETRIES')
          STOP ' NSYM TOO BIG'
        ENDIF
      ELSE
        WRITE(*,8)
8       FORMAT('0***ERROR*** NSYM CARD NOT FOUND')
        STOP ' MISSING NSYM CARD'
      ENDIF
      DO 200 NS=1,NSYM
        READ(*,10,END=250,ERR=250) COL
10      FORMAT(80A1)
C
C       CHECK FOR SYMM AT START OF CARD, AND INITIALIZE
C
        DO 15 J=1,4
          IF (COL(J) .EQ. CHECK(J)) GO TO 15
          WRITE(*,13) COL
13        FORMAT('0***ERROR*** EXPECTING SYMM CARD, NOT',/,3X,80A1)
          STOP ' WHERE IS MY SYMM CARD?'
15      CONTINUE
        WRITE(*,17) COL
17      FORMAT('0FREE-FORMAT SYMMETRY OPERATION:',/,/,1X,80A1)
        DO 20 J=1,3
          SYMTRL(J,NS) = 0.0
          DO 20 K=1,3
            SYMROT(J,K,NS) = 0.0
20      CONTINUE
        I = 4
25      CONTINUE
          I = I+1
          IF (I .GT. IMAX) THEN
            WRITE(*,27)
27          FORMAT('0***ERROR*** NO SYMMETRY OPERATION IN SYMM CARD')
            STOP ' NO SYMMETRY OPERATION IN SYMM CARD'
          ENDIF
        IF (COL(I) .NE. IBLANK) GO TO 25
C
C       NOW I POINTS TO FIRST BLANK IN CARD.
C
        NOP = 1
C
C       TOP OF LOOP FOR NEW FIELD IN SYMMETRY OPERATION.
C       FIELDS ARE SEPARATED BY COMMAS.
C
30      CONTINUE
          S = 1.0
          ST = 1.0
          T = 0.0
          IP = 0
          NP = 0
          ISL = 0
          IFOUND = 0
          IXYZ = 0
C
C         TOP OF LOOP FOR NEW CHARACTER IN COL
C
50        CONTINUE
            I=I+1
            IF (I.GT.IMAX)  GO TO 180
            ICH = COL(I)
            IF (ICH.EQ.IBLANK)  GO TO 50
            IF (ICH.EQ.ICOMMA) GO TO 180
            IFOUND=1
            IF (ICH.EQ.IX) GO TO 100
            IF (ICH.EQ.IY)  GO TO 110
            IF (ICH.EQ.IZ)  GO TO 120
            IF (ICH.EQ.IPLUS) GO TO 140
            IF (ICH.EQ.IMINUS)  GO TO 150
            IF (ICH.EQ.ISLASH)  GO TO 160
            IF (ICH.EQ.IPOINT)  GO TO 170
            DO 60 K=1,10
              IF (ICH.EQ.INUM(K))  GO TO 70
60          CONTINUE
            WRITE(*,65) ICH
65          FORMAT('0***ERROR*** INVALID CHARACTER...',A1)
            STOP ' INVALID CHARACTER IN SYMMETRY OPERATOR'
C
70          A = NUM(K)
            ST = S
            IF (ISL.EQ.1)  GO TO 80
            IF (IP.EQ.1)  GO TO 90
            T=10.0*T + A
            GO TO 50
80          T = T/A
            GO TO 50
90          NP=NP+1
            T = T + A/10**NP
            GO TO 50
100         J=1
            GO TO 130
110         J=2
            GO TO 130
120         J=3
130         SYMROT(NOP,J,NS) = S
            IXYZ = 1
            S=1.0
            GO TO 50
140         S=1.0
            GO TO 50
150         S=-1.0
            GO TO 50
160         ISL=1
            GO TO 50
170         IP=1
            GO TO 50
180       CONTINUE
C
C         END OF CARD OR END OF FIELD.  DO SOME CHECKS.
C
          T=T*ST
          SYMTRL(NOP,NS)=T
          IF (IFOUND.EQ.0 .AND. I.LE.IMAX) THEN
            WRITE(*,185)
185         FORMAT('0***ERROR*** BLANK OPERATOR FIELD')
            STOP ' BLANK OPERATOR FIELD'
          ENDIF
          IF (IXYZ .EQ. 0) THEN
            WRITE(*,187)
187         FORMAT('0***ERROR*** NO X, Y OR Z IN OPERATOR FIELD')
            STOP ' NO X, Y OR Z!'
          ENDIF
          IF ( (I.GT.IMAX) .AND. (NOP .NE. 3) ) THEN
            WRITE(*,190)
190         FORMAT('0***ERROR*** SYMMETRY OPERATION IS INCOMPLETE')
            STOP ' SYMMETRY OPERATION INCOMPLETE'
          ENDIF
          NOP=NOP+1
        IF (NOP.LE.3) GO TO 30
C
C       WRITE OUT SYMMETRY OPERATION, THEN DO THE NEXT
C
        WRITE(*,195) ((SYMROT(J,K,NS),K=1,3),SYMTRL(J,NS),J=1,3)
195     FORMAT('0MATRIX/VECTOR FORM:',/,3(/,3F5.0,5X,F10.6))
200   CONTINUE
      RETURN
C
250   CONTINUE
      WRITE(*,300)
300   FORMAT('0***ERROR*** READ ERROR OR UNEXPECTED EOF IN SYMFRE')
      STOP ' READ ERROR OR UNEXPECTED EOF'
      END

C     ========================================
      SUBROUTINE SYMTR3(NSM,RSM,SYMCHS,IPRINT)
C     ========================================
C
C
C
C---- SYMTR3(NSM,RSM)
C           symmetry translation from matrix back to characters
C
C           This translates the Symmetry matrices into INT TAB
C           character strings
C
C           It gives the real space operations.
C                eg     X,Y,Z
C                eg     -Y,X-Y, Z
C           That is more complicated than you might think!!
C
C
C---- Arguments :
C
C     NSM       INTEGER         Number of Symmetry operations
C
C     RSM       REAL            Array of dimension (4,4,at_least_NSM)
C                               containing symmetry operations on input
C
C     SYMCHS    CHARACTER*80    Array of dimension at_least_NSM
C                               containing int tab char
C                               strings on output
C
C     IPRINT    INTEGER         Print flag
C                               =0 No printing
C                               =1 Print the int tab strings
C
C
C     .. Scalar Arguments ..
      INTEGER IPRINT,NSM
C     ..
C     .. Array Arguments ..
      REAL RSM(4,4,*)
      CHARACTER SYMCHS(*)*80
C     ..
C     .. Scalars in Common ..
      CHARACTER INXWIN*6,STROUT*400
C     ..
C     .. Local Scalars ..
      INTEGER ICH,IST,ITR,JCOUNT,JDO10,JDO20,JDO30,JDO40
C     ..
C     .. Local Arrays ..
      CHARACTER AXISCR(3)*1,HKLCR(3)*1,NUMB(9)*1
      INTEGER   NPNTR1(10), NPNTR2(10)
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL PUTLIN
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
C     .. Common blocks ..
      COMMON /OUTSTR/STROUT,INXWIN
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
C
      DATA AXISCR    /'X', 'Y', 'Z'/
      DATA HKLCR     /'H', 'K', 'L'/
      DATA NUMB      /'1', '2', '3', '4', '5', '6', '7', '8', '9'/
      DATA NPNTR1    / 0,   1,   1,   1,   0,   1,   0,   2,   3,   5/
      DATA NPNTR2    / 0,   6,   4,   3,   0,   2,   0,   3,   4,   6/
C     ..
C
      DO 40 JDO40 = 1,NSM
C
C---- Clear symchs
C
        SYMCHS(JDO40) = ' '
        ICH = 1
C
C
        DO 20 JDO20 = 1,3
C
C---- Ist is flag for first character of operator
C
          IST = 0
C
C
          DO 10 JDO10 = 1,4
C
            IF (RSM(JDO20,JDO10,JDO40).NE.0) THEN
C              IF (RSM(JDO20,JDO10,JDO40).GT.0.5)
C     .            RSM(JDO20,JDO10,JDO40) = RSM(JDO20,JDO10,JDO40) - 1.0
              IF (RSM(JDO20,JDO10,JDO40).GT.0 .AND. IST.GT.0) THEN
                SYMCHS(JDO40) (ICH:ICH) = '+'
                ICH = ICH + 1
              END IF
C
              IF (RSM(JDO20,JDO10,JDO40).LT.0) THEN
                SYMCHS(JDO40) (ICH:ICH) = '-'
                IST = 1
                ICH = ICH + 1
              END IF
C
              IF (JDO10.NE.4) THEN
                SYMCHS(JDO40) (ICH:ICH) = AXISCR(JDO10)
                IST = 1
                ICH = ICH + 1
              END IF
C
              IF (JDO10.EQ.4 .AND. RSM(JDO20,4,JDO40).NE.0) THEN
C
                ITR = NINT(ABS(12.0*RSM(JDO20,4,JDO40)))
                I1  = NPNTR1 (ITR)
                I2  = NPNTR2 (ITR)
                SYMCHS(JDO40) (ICH:ICH+2) = NUMB(I1) // '/' //NUMB(I2)
C                SYMCHS(JDO40) (ICH:ICH+1) = '1/'
C                ICH = ICH + 2
C                SYMCHS(JDO40) (ICH:ICH) = NUMB(ITR)
                ICH = ICH + 3
              END IF
            END IF
   10     CONTINUE
C
C---- ADD COMMA  space
C
          IF (JDO20.NE.3) THEN
            SYMCHS(JDO40) (ICH:ICH+2) = ',  '
            ICH = ICH + 3
          END IF
   20   CONTINUE
C
C---- write a message if required
C
        IF (IPRINT.EQ.1) THEN
          WRITE (STROUT,FMT='(A,I3,5X,A)') 'SYMMETRY',JDO40,
     +      SYMCHS(JDO40) (1:LENSTR(SYMCHS(JDO40)))
          CALL PUTLIN('CURWIN')
C
C
          DO 30 JDO30 = 1,4
            WRITE (STROUT,FMT='(4F6.2)') (RSM(JDO30,JCOUNT,JDO40),
     +        JCOUNT=1,4)
            CALL PUTLIN('CURWIN')
   30     CONTINUE
C
C
        END IF
   40 CONTINUE
C
C
      END
      SUBROUTINE TAR(ZV,NL,IERR,INAME)
C******* TEST ARRARRAY ****** ONLY CYBER *****
      DIMENSION ZV(NL)
      IERR=0
C----- EMPTY FOR NON-CYBER ---
      RETURN
      END 
C 
      SUBROUTINE TESTZ(Z,MAXNX,NX,NY,IERR,INAME)
C********* ONLY CYBER *************************** 
      DIMENSION Z(MAXNX,1)
C IERR UPON EXIT
C IERR=0 : O.K. 
C IERR=1 :  AT LEAST 1 GRIDVALUE INFINITE/INDEFINITE/UNNORMALIZED 
C IERR=2 : ALL GV.  = 
C 
      IERR=0
C----- EMPTY FOR NON-CYBER -------
      RETURN
      END 
      SUBROUTINE THREED(A,NMAX,NI,NJ,HIT,ANGLA,ANGLB,XS,YS) 
C 
C THE 3D FIGURE IS SCALES IN A BOX OF ABS(NI)*ABS(NJ)*ABS(HITE) 
C 
C HITE > 0. :PENSHIFT AFTER PLOTTING
C HITE < 0. : NO PENSHIFT 
C HITE'=HITE+10000.  : 3-D AXIS WITH TICKMARKS
C             PARAMETERS IN /CAX3D/ 
C 
C THE BOX IS PROJECTED WITHOUT PERSPECTIVE FROM DIRECTION ANGLA,ANGLB 
C 
C THE PROJECTION IS SCALED ( WITHOUT DISTORTION) TO FILL AS MUCH
C AS POSSIBLE OF THE RECTANGLE ABS(XS)*ABS(YS)
C 
C XS < 0 : NO LINES IN X-DIRECTION
C YS < 0 : NO LINES IN Y-DIRECTION
C 
      COMMON /COMTRD/ XSS,YSS,CEND
      DIMENSION H(10),V(10),X(2),Y(2),Z(2),XP(8),A(NMAX,NJ) 
      COMMON /X3HZ39/ANGA,ANGB,HV,D,SL,SM,SN,CX,CY,CZ,QX,QY,QZ,SD 
      COMMON /CAX3D/ QAX3D,QSCERR,ZZ1,ZZ2,NIX,NIY,NIZ 
C 
      LOGICAL QAX3D,QSCERR
C 
      QAX3D=.FALSE. 
      IF(ABS(HIT).LT.10000.) GO TO 2
CBEGIN AXES SEE XRTHO 
C AXIS INFORMATION IN /CAX3D/ 
      QAX3D=.TRUE.
      HITE=SIGN(ABS(HIT)-10000.,HIT)
                             GO TO 3
CEND   AXES SEE XRTHO 
    2 HITE=HIT
    3 CONTINUE
C 
      PI=3.141593 
      ANGA=ANGLA*PI/180.0 
      ANGB=ANGLB*PI/180.0 
      IF(.NOT.(XS.LT.0. .AND. YS.LT.0.))    GO TO 4 
      PRINT *,' THREED : YOU HAVE SUPPRESSED CURVES IN BOTH DIRECTIONS' 
                             RETURN 
    4 CONTINUE
      N=IABS(NI)
      M=IABS(NJ)
      IF(.NOT.(N.EQ.0 .OR. M.EQ.0))   GO TO 6 
      PRINT *,' THREED : BOTH DIRECTIONS HAVE DIMENSION 0'
                             RETURN 
    6 CONTINUE
C Z VALUES INF/INDEF/UNNORM ? 
C Z VALUES ALL EQUAL TO EACH OTHER ?
      CALL TESTZ(A,NMAX,N,M,IERR,'THREED')
      IF(IERR.GT.0)          RETURN 
C 
      K=3 
      IF(XS.LT.0.0) K=2 
      IF(YS.LT.0.0) K=1 
      XSS=ABS(XS) 
C 
      HV=XSS
C 
      YSS=ABS(YS) 
C 
C SEE XRTHO FOR CEND
      CEND=1./16. 
C 
      SL=-COS(ANGA)*COS(ANGB) 
      SM=-SIN(ANGA)*COS(ANGB) 
      SN=-SIN(ANGB) 
      IF ( ABS(SN).NE.1.0) GO TO 10 
      PRINT 20
   20 FORMAT('1*********************YOU ARE ATTEMPTING TO LOOK
     1STRAIGHT DOWN ( OR UP ) AT THE SURFACE ' )
                             RETURN 
   10 CONTINUE
      SD = 1.0 / SQRT( 1.0 - SN ** 2 )
      X(1)=1. 
      X(2) = N
      Y(1)=1. 
      Y(2) = M
      T=ABS(HITE) 
      D = FLOAT(M**2 + N**2)  +  T**2 
      D = SQRT ( D )
      CALL XINMAX(A,NMAX,N,M,Z) 
C 
      IF(.NOT.QAX3D)         GO TO 26 
CBEGIN 3D AXIS , SEE /CAX3D/
C QSCERR IS FLAG FOR INTERACTIVE TEKTRONIX VERSION
C DEE STRD
      QSCERR=.FALSE.
C ZZ1=ZZ2 : Z-AXIS COMPLETELY FILLED
      IF(ZZ1.EQ.ZZ2)         GO TO 26 
      IF(.NOT.( Z(1).LT.ZZ1 .OR. Z(2) .GT.ZZ2))  GO TO 25 
      QSCERR=.TRUE. 
      PRINT *,' 3D AXES :   Z-VALUES OUTSIDE (Z1,Z2) '
                             RETURN 
   25 CONTINUE
      Z(1)=ZZ1
      Z(2)=ZZ2
CEND   3D AXIS , SEE /CAX3D/
   26 CONTINUE
C 
C 
C******* PERSP. PROJECTION ************************ 
C   PERSP >=1.
      PERSP=10. 
      SCALE=PERSP*D 
C 
      CX = -SL * SCALE
      S = T / (Z(2 ) - Z( 1 ) ) 
      CY = -SM * SCALE
      CZ =-SN * SCALE 
      QX = CX + D * SL
      QY = CY + D * SM
      QZ = CZ + D * SN
      DO 30 I = 1,N 
      DO 30 J = 1,M 
      A(I,J)=(A(I,J)-Z(1) )*S 
   30 CONTINUE
C SEE RESTORE AT END
      ZSAVE=Z(1)
      Z(1) = 0.0
      Z(2) = T
      CALL XUBE (X,Y,Z,XP,H,V)
      DO 2130 I=1, 8
      H(I)=((XP(I)-QX)*SM-(H(I)-QY)*SL)*SD
      V(I)=(V(I)-QZ)*SD 
      CALL XINMAX(H,10,8,1,H(9))
      CALL XINMAX(V,10,8,1,V(9))
 2130 CONTINUE
C    FIX PLOT-ORIGIN
      IF( HITE.GT.0. ) CALL PLOT( 0., 0., -3 )
C 
      IF (ANGB.GE.0.0) GO TO 2140 
      T = V(9)
      V(9)=V(10)
      V(10)=T 
 2140 CALL XRTHO(X,Y,A,NMAX,N,M,H,V,K)
      IF(HITE.GT.0.) CALL PLOT(HV+2.0,0.0,-3) 
C    RESTORE A
      DO 2160 I=1,N 
      DO 2160 J=1,M 
      A(I,J)=A(I,J)/S+ZSAVE 
2160  CONTINUE
C 
      RETURN
      END 
      LOGICAL FUNCTION TIND(NX,NY,I)
      COMMON /KONTUR/ MT,NT,NI,IX,IY,IDX,IDY,ISS,IT,IV,NP,NQ,JT,PY,CV,
     1 IPT(3,3),INX(8),INY(8),DL,RA,THE,NTP,RAT,H,HTT 
      LOGICAL Q 
      Q=NX.LT.1.OR.NX.GT.MT.OR.NY.LT.1.OR.NY.GT.NT
      TIND=Q
      RETURN
      IF(Q) PRINT 100,I,IX,IY,CV
  100 FORMAT(' OVERSCHRYDING',3I5,F10.2)
      END 
C
C
C
      SUBROUTINE TRAFO(Y,S,X,U)
C     =========================
C
C
C
C---- Compute
C
C               Y = [S]*X + U
C
C
C    Y, X and U are 3-dimensional vectors
C    [S] is a 3*3 matrix
C
C     Y may be the same as X
C     Y may be the same as U
C
C
C
C     .. Array Arguments ..
      REAL S(3,3),U(3),X(3),Y(3)
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
C     .. Local Arrays ..
      REAL Z(3)
C     ..
C
C
      DO 10 I = 1,3
        Z(I) = X(I)
   10 CONTINUE
      DO 20 I = 1,3
        Y(I) = S(I,1)*Z(1) + S(I,2)*Z(2) + S(I,3)*Z(3) + U(I)
   20 CONTINUE
C
C
      END
*DECK $FTRICART
      SUBROUTINE TRICART(A1,A2,A3,ALPHA,BETA,GAMMA,TRANSF)
C///  VERSION 83/05/19
C
C     CALCULATES THE MATRIX THAT TRANSFORMS COORDINATES RELATIVE
C     TO THE TRICLINIC AXES A1, A2 AND A3 TO A CARTESIAN SET OF AXES.
C     A2(CART) RUNS ALONG A2, A1(CART) LIES IN THE PLANE OF A1 AND A2
C     AND A3(CART) RUNS ALONG A1 X A2.
C                             --   --
C     ALPHA, BETA AND GAMMA MUST BE GIVEN IN RADIANS.
C     LIT.: M.G.ROSSMANN & D.M.BLOW, ACTA CRYST.(1962) VOL.15,24
C           FORMULA (9).
C
      DIMENSION TRANSF(3,3)
C\\\
      C1=COS(ALPHA)
      C2=COS(BETA)
      C3=COS(GAMMA)
      S1=SIN(ALPHA)
      S3=SIN(GAMMA)
C
      CW=(C2-C1*C3)/(S1*S3)
      SW=SIN(ACOS(CW))
C
      TRANSF(1,1)=A1*S3*SW
      TRANSF(1,2)=0.0
      TRANSF(1,3)=0.0
      TRANSF(2,1)=A1*C3
      TRANSF(2,2)=A2
      TRANSF(2,3)=A3*C1
      TRANSF(3,1)=A1*S3*CW
      TRANSF(3,2)=0.0
      TRANSF(3,3)=A3*S1
      RETURN
C *  THIS PROGRAM VALID ON FTN4 AND FTN5 **
      END
      SUBROUTINE TRNSFM(RMAT,XOLD,T,XNEW)
C///
C=======================================================================
C
C                           T R N S F M
C
C     SUBROUTINE TO APPLY A SYMMETRY OPERATION ON FRACTIONAL COORDINATES
C
C=======================================================================
C
C     INPUT PARAMETERS:
C     RMAT (3,3)     :  ROTATIONAL PART OF SYMMETRY OPERATION
C     T    (3)       :  TRANSLATIONAL PART OF SYMMETRY OPERATION
C     XOLD (3)       :  INPUT FRACTIONAL COORDINATES
C
C     OUTPUT PARAMETERS:
C     XNEW (3)       :  OUTPUT (NEW) FRACTIONAL COORDINATES
C
C=======================================================================
C     VERSION 23 MARCH 1987    BAUKE DIJKSTRA
C=======================================================================
C\\\
      REAL RMAT(3,3),XOLD(3),T(3),XNEW(3)
      DO 10 I=1,3
        XNEW(I) = T(I)
        DO 10 J=1,3
          XNEW(I) = XNEW(I) + RMAT(I,J)*XOLD(J)
10    CONTINUE
      RETURN
      END

      SUBROUTINE UFREAD(LU,RVARS,N)
C
C        Unformatted read using routine FREFRM
C
C        LU     Logical unit for read
C        RVARS  Real array for returning variables, loaded with
C               defaults.
C        N      Maximum number of variables to be returned. Unchanged
C               by the call.
C
C        L.W. July 1984
C
      BYTE LINE(80)
      DIMENSION RVARS(*)
C
      READ(LU,10,END=30)LINE
10    FORMAT(80A1)
      CALL FREFRM(LINE,80,RVARS,N,J)
      RETURN
30    WRITE(6,40)LU
40    FORMAT(' End-of-file on logical unit',I4)
      CALL EXIT
      END
      SUBROUTINE UNITCL (A,CELL,CLSTAR)
C///
C=======================================================================
C
C                         U N I T C L
C
C=======================================================================
C
C     PURPOSE: 
C     GET CELL DIMENSIONS FROM AMATRIX
C
C=======================================================================
C
C     INPUT PARAMETERS USED: 
C
C     A(3,3)    THE A-MATRIX AS DEFINED BY M.G. ROSSMANN
C
C=======================================================================
C
C     OUTPUT PARAMETERS OBTAINED:
C
C     CELL(6)   A,B,C (IN ANGSTROM) AND ALPHA, BETA AND GAMMA (IN
C               DEGREES)
C
C     CLSTAR(6) THE SAME SIX CELL DIMENSIONS, BUT NOW IN RECIPROCAL
C               SPACE
C=======================================================================
C     
C     OTHER ASSOCIATED PROGRAMS (PRESENT IN THE MIF HANDLING PACKAGE)
C     ANALYS    TO OBTAIN CELL DIMENSIONS AND ORIENTATION FROM A-MATRIX
C     ROTANA    TO ANALYS ROTATION MATRIX AS DESCRIBED BELOW
C     MATALG    TO CREATE PERFECT A-MATRIX AS DESCRIBED BELOW
C     CONTACT GERT ABOUT THESE
C
C=======================================================================
C
C     VERSION APRIL   1987     GERT VRIEND
C
C=======================================================================
C
C     PRINCIPLE/THEORY
C     ----------------
C
C    THE FOLLOWING DEFINITIONS ARE USED: 
C    *    : RECIPROCAL SPACE INDICATOR
C    #    : DOT-PRODUCT
C    \P\  : ABSOLUTE VALUE OR LENGTH OF P
C    .    : SCALAR MULTIPLICATION
C    @    : MATRIX MULTIPLICATION
C    X,Y,Z: PRINCIPAL AXES IN REAL SPACE (ORTHONORMAL)
C    V    : VOLUME OF CELL IN REAL SPACE
C    V*   : VOLUME OF CELL IN RECIPROCAL SPACE
C
C    THE A-MATRIX IS DEFINED AS: 
C
C        (A*.COS(A*,X)  B*.COS(B*,X)  C*.COS(C*,X))
C  [A] = (A*.COS(A*,Y)  B*.COS(B*,Y)  C*.COS(C*,Y))
C        (A*.COS(A*,Z)  B*.COS(B*,Z)  C*.COS(C*,Z))
C
C  FOR ANY TWO PRINCIPLE AXES THE DOT-PRODUCT RULE HOLDS, REGARDLESS
C  OF THE AXES-SYSTEM OR SPACE IN WHICH THEY ARE DEFINED (AS LONG AS
C  IT IS CARTESIAN):
C
C  EG. A#B   = A1.B1+A2*B2+A3.B3       = \A\.\B\.COS(GAMMA)
C  OR  A*#B* = A*1.B*1+A*2.B*2+A*3.B*3 = \A*\.\B*\.COS(GAMMA*)
C
C      AFTER DEFINING A*1'=A*.COS(A*,X), A*2'=A*.COS(A*,Y) ETC THE 
C      A-MATRIX BECOMES
C            (A*1'  B*1'  C*1')
C      [A] = (A*2'  B*2'  C*2')
C            (A*3'  B*3'  C*3')
C
C      ALSO FOR THE VECTORS A*', B*' AND C*' WHICH MAKE UP THIS MATRIX,
C      THE DOT-PRODUCT RULES HOLD.
C      IT IS ALSO IMPORTANT TO REALIZE THE ANGLES BETWEEN THE COLUMN
C      VECTORS OF THE TWO AFOREMENTIONED A-MATRICES ARE STILL THE SAME IN BOTH
C      CASES.
C      THEREFORE: 
C
C      COS(GAMMA*)=(A*1'.B*1'+A*2'.B*2'+A*3'+B*3')/(\A*\.\B*\)
C      WITH \A*\=SQRT(A*1'.A*1'+A*2'.A*2'+A*3'.A*3') ETC.
C
C  AFTER APPLYING THESE RULES SEVERAL TIMES TO THE A-MATRIX, ALPHA*, BETA* AND
C  GAMMA* ARE KNOWN. THE MAGNITUDES OF A*, B* AND C* COME OFCOURSE FROM
C  APPLICATION OF PYTHAGORAS LAW ON THE COLUMN VECTORS. (PYTHAGORAS LAW
C  HOLDS BECAUSE THE X,Y,Z AXES SYSTEM IS ORTHONORMAL).
C
C  V* CAN NOW EASILY BE CALCULATED FROM: 
C
C  V*=A*.B*.C*.SQRT(1-COS(ALPHA*)**2-COS(BETA*)**2-COS(GAMMA*)**2+2.COS(
C                   ALPHA*).COS(BETA*).COS(GAMMA*))
C
C  TRIGONIOMETRICAL RELATION WILL NOW YIELD THE SIN'S OF ALPHA*, BETA* AND
C  GAMMA*.
C
C  A, B AND C ARE NOW GOTTEN FROM: 
C
C       B*.C*.SIN(ALPHA*)
C  A = -------------------           ETC.
C            V*
C
C  ALPHA, BETA, GAMMA ARE FINALLY GOTTEN FROM: 
C
C                COS(BETA*).COS(GAMMA*)-COS(ALPHA*)
C  COS(ALPHA) = ------------------------------------        ETC.
C                     SIN(BETA*).SIN(GAMMA*)
C
C  THE SECOND STEP IN THE PROCESS OF A-MATRIX ANALYSES IS THE DETERMINATION
C  OF THE ROTATIONAL PART.
C
C  BE AWARE THAT UNITCL DOES NOT DO THIS ROTATIONAL ANALYSES
C
C  WE SHOULD REALIZE THAT 
C
C  [A] = [ROT] @ [A-PERF]
C  
C  IN WHICH [A]      IS THE A-MATRIX TO BE ANALYSED
C           [A-PERF] IS THE A-MATRIX FOR THE CRYSTAL IN THE STANDARD (=PERFECT)
C                    ORIENTATION.
C           [ROT]    A PURE ROTATION MATRIX.
C
C  THIS CAN BE REWRITTEN AS
C
C  [A] @ [A-PERF]INVERSE = [ROT] @ [A-PERF] @ [A-PERF]INVERSE
C                       OR: 
C  [A] @ [A-PERF]INVERSE = [ROT]
C
C  THE ONLY THING LEFT IS TO CONVERT THE ROTATION MATRIX [ROT] INTO 3 
C  EULERIAN ROTATION ANGLES.
C
C  LET US USE ROSSMANNS DEFINITIONS FOR THE ROTATIONS. THIS MEANS DEFINING: 
C
C                (  1    0     0  )  ( COSY  0 -SINY )  (  COSZ SINZ  0 )
C  [X],[Y],[Z] = (  0  COSX -SINX ), (   0   1    0  ), ( -SINZ COSZ  0 )
C                (  0  SINX  COSX )  ( SINY  0  COSY )  (    0    0   1 )
C
C  IN WHICH [X], [Y] AND [Z] STAND FOR THE THREE INDENDENT ROTATIONS AROUND
C  THE X, Y AND Z AXIS RESPECTIVELY.
C  IF WE THEN ALSO USE ROSSMANN'S ORDER OF MULTIPLYING THESE 3 ROTATION
C  MATRICES, WE GET: 
C
C                        (  CY.CZ           CY.SZ          -SY    )
C  [ROT] = [X]@[Y]@[Z] = ( -CX.SZ-SX.SY.CZ  CX.CZ-SX.SY.SZ -SX.CY )
C                        ( -SX.SZ+CX.SY.CZ  SX.CZ+CX.SY.SZ  CX.CY )
C
C  IN WHICH CX STANDS FOR THE COSINE OF THE ANGLE OF ROTATION AROUND THE
C  X-AXIS ETC.
C
C  BY STARTING WITH THE ROTATION AROUND THE Y-AXIS (THE SIN OF THAT ROTATION
C  IS EASILY DETERMINED FROM THE UPPER RIGHT TERM OF THE ROT-MATRIX) THE
C  3 ROTATIONS OF WHICH [ROT] IS BUILD UP, ARE DETERMINED STRAIGTH FORWARDLY.
C
C\\\ 
      DIMENSION A(3,3),CELL(6),CLSTAR(6),RECSQ(6)
C----
C---- INITIALIZE
C----
      DTR=ATAN(1.0)/45.0
C----
C---- DETERMINE THE ELEMENTS OF THE DOT-PRODUCT TERMS
C----
      M = 1
      DO 20 I = 1, 3
         RECSQ(I) = 0.0
         RECSQ(I+3) = 0.0
         M = M+1
         IF (M.GT.3) M = 1
         DO 10 J = 1, 3
            RECSQ(I) = RECSQ(I)+A(J,I)*A(J,I)
            RECSQ(I+3) = RECSQ(I+3)+2.0*A(J,I)*A(J,M)
   10    CONTINUE
   20 CONTINUE
C----
C---- GET THE RECIPROCAL SIN'S AND COSINES'S FROM THE DOT-PRODUCT TERMS
C----
      CAST = RECSQ(5)/(2.0*SQRT(RECSQ(2)*RECSQ(3)))
      CBST = RECSQ(6)/(2.0*SQRT(RECSQ(3)*RECSQ(1)))
      CGST = RECSQ(4)/(2.0*SQRT(RECSQ(1)*RECSQ(2)))
      SAST = SIN(ACOS(CAST))
      SBST = SIN(ACOS(CBST))
      SGST = SIN(ACOS(CGST))
C----
C---- DETERMINE THE VOLUME OF THE RECIPROCAL CELL
C----
      VST = SQRT(RECSQ(1)*RECSQ(2)*RECSQ(3)*(1.0-CAST**2-CBST**2-CGST**2
     *   +2.0*CAST*CBST*CGST))
C----
C---- DETERMINE THE REAL SPACE CELL DIMENSIONS (ORDER: A,B,C,ALPHA,BETA,GAMMA)
C----
      CELL(1) = (SQRT(RECSQ(2)*RECSQ(3))*SAST)/VST
      CELL(2) = (SQRT(RECSQ(1)*RECSQ(3))*SBST)/VST
      CELL(3) = (SQRT(RECSQ(1)*RECSQ(2))*SGST)/VST
      CELL(4) = (ACOS((CBST*CGST-CAST)/(SBST*SGST)))
      CELL(5) = (ACOS((CAST*CGST-CBST)/(SAST*SGST)))
      CELL(6) = (ACOS((CAST*CBST-CGST)/(SAST*SBST)))
C----
C---- DETERMINE (ACTUALLY, ADMINISTRATE) THE RECIPROCAL CELL DIMENSIONS
C----
      DO 30 I=1,3
         CLSTAR(I)=SQRT(RECSQ(I))
   30 CONTINUE
      CLSTAR(4)=ACOS(CAST)
      CLSTAR(5)=ACOS(CBST)
      CLSTAR(6)=ACOS(CGST)
 
      RETURN
      END
C
C
C
      SUBROUTINE UNPCK(IH,IK,IL,IPK)
C     ==============================
C
C---- Unpack the integer word ipk into ih, ik and il
C
C     .. Scalar Arguments ..
      INTEGER IH,IK,IL,IPK
C     ..
C     .. Local Scalars ..
      INTEGER ITMP
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     ..
C
C
      ITMP = IPK
      IL = MOD(ITMP,1024) - 512
      ITMP = ITMP/1024
      IK = MOD(ITMP,1024) - 512
      ITMP = ITMP/1024
      IH = MOD(ITMP,1024) - 512
C
C
      END
	BYTE FUNCTION UPPER(C)
C
C	   Return upper case value of "C".
C	   "C" is not changed by the call.
C	   L. Weissman  July 1984
C
	BYTE C,A,Z
	DATA A,Z/'a','z'/
	UPPER=C
	IF(C .GE. A .AND. C .LE. Z)UPPER = C - 32
	RETURN
	END
      SUBROUTINE VCNW (AM,MAXNX,N,M,NL,ZMIN,DZ,LAB, 
     * XSZ,YSZ,TH,IT,IWRK)
C VCNTR WITHOUT DYNAMICAL WORKSPACE-ALLOCATION
C 
C IWRK IS WORKARRAY WITH LENGTH <=.4*N*M
C PARAMETERS-CORRESPONDENCE WITH DOCUMENTATION :  
C MAXNX=IMAXNX
C N=INX 
C M=IMY 
C NL=INL
C ZMIN=ZMIN 
C DZ=DZ 
C LAB=ILAB
C XSZ=XSIZE 
C YSZ=YSIZE 
C TH=THETA
C IT=ILNTYP 
C IWRK=EXTRA WORKARRAY
C 
C CALLING SUBPROGRAMS : 
C PLCAN 
C PLALC 
C PLACE 
C TIND
C PLDASH
C PLPTRN
C (CALCOMP PLOTROUTINES)
C PLOT
C FACTOR
C AXIS
C SYMBOL
C 
      COMMON /PLFCT/ XFCT,YFCT,X1,Y1,X2,Y2,XXFCT,XB,YB
      DIMENSION AM(MAXNX ,1),       ZMIN(1),LAB(1)
      DIMENSION IWRK(1) 
      COMMON /CNTR/ IISYM,NNNN,QSTART,QSUP,QPLOT,QKMPLT,QLAB
      COMMON /LL/ LL
      LOGICAL QSKIP 
      LOGICAL QSTART,QSUP,QPLOT,QKMPLT,QLAB,QPL 
      LOGICAL QWRK
C 
CKRAAK**** LENGTH WORKARRAY HELDER**
       NNNN=.4*N*M
      QSUP=.FALSE.
      IL=IABS(IT) 
C XB,YB IN /PLFCT/
      XB=0. 
      YB=0. 
C 
      GO TO 1 
C 
C----------- NON-ANSI-----
      ENTRY SVCNW(AM,MAXNX,N,M,NL,ZMIN,DZ,LAB,                      XSZ,
     +YSZ,TH,IT,IWRK) 
      QSUP=.TRUE. 
      XB=0. 
      YB=0. 
      IL=IABS(IT) 
      IF(IL.LT.10001)        GO TO 987
C (1,1) IN LOG. ORIGIN OF PLOTTER-SOFTWARE
      IL=IL-10000 
      XB=1. 
      YB=1. 
  987 CONTINUE
C 
    1 CONTINUE
      NX=IABS(N)
      NY=IABS(M)
C TEST ARRAY AM 
C TO BE COMPATIBLE WITH PRCNTR :  
      XSIZE=ABS(XSZ)
      YSIZE=ABS(YSZ)
C IT/ILINE
      LL=1
      IF(IL.LT.1001)         GO TO 2
C LL<0 : NO PRINT STAT(TEKTONIX)
      LL=-1 
      IL=IL-1000
    2 CONTINUE
      IF(IL.LT.1.OR.IL.GT.10)       GO TO 1000
      D=IL-1
C QLAB=FALSE : NO LEVEL INDICATION AT  PLOTTER
      QLAB=IT.GT.0
C TH
      IF(TH.LE.10. .OR.TH.GT.170.)     GO TO 1000 
C 
C 
      THE=TH*3.141593/180.
      W=XSIZE 
      HT=YSIZE
      IF(QSUP)               GO TO 60 
CBEGIN-------------INITIALIZE 
      CALL PLOT(0.,0.,-3) 
      IF(TH.GT.90.) CALL PLOT(-COS(THE)*HT,0.,-3) 
C                  RUG:FAC=2. , ACCU : FAC=1. 
      FAC=1.0 
      CALL FACTOR(FAC)
C----------- NON-ANSI-----
      CALL AXIS(0.,0.,'X',-1,W/FAC,0.,XB,(NX-XB)*FAC/XSIZE) 
C----------- NON-ANSI-----
      CALL AXIS(0.,0.,'Y',1,HT/FAC,TH,YB,(NY-YB)*FAC/YSIZE) 
C XB,YN 
      CALL FACTOR(1.) 
   50 CONTINUE
CEND  -------------INITIALIZE 
   60 CONTINUE
C 
C 
C----------- NON-ANSI-----
      IF(LL.GT.0)  PRINT 300
C----------- NON-ANSI-----
  300 FORMAT(//,' ******(S) VCNTR ******')
C 
      ZZZ=999.
      IF(DZ.EQ.ZZZ)          GO TO 500
CBEGIN-------------CL0,DEL
      IF(DZ.NE.0.)           GO TO 400
C DZ=0. : ATOMATIC SCALING
C*********** MDEP.
C BIG=LARGEST REAL
      BIG=1.E38
      ZMN=BIG 
      ZMX=-BIG
      DO 320 I=1,NX 
      DO 310 J=1,NY 
      IF(AM(I,J).LT.ZMN)ZMN=AM(I,J) 
      IF(AM(I,J).GT.ZMX)ZMX=AM(I,J) 
  310 CONTINUE
  320 CONTINUE
      IF(ZMX.NE.ZMN)         GO TO 330
C ZMX=ZMN 
C----------- NON-ANSI-----
      PRINT 900,ZMX 
C----------- NON-ANSI-----
  900 FORMAT(//,' ***** (S)VCNTR ERROR : ALL Z-VALUES =',E10.2) 
                             RETURN 
  330 CONTINUE
      DEL=(ZMX-ZMN)/FLOAT(NL) 
      CL0=ZMN-0.5*DEL 
                             GO TO 500
C DZ.NE.0 AND DZ.NE.ZZZ : USER DEFINED EQUIDISTANT LEVELS 
  400 DEL=DZ
      CL0=ZMIN(1)-DEL 
CEND  -------------CL0,DEL
  500 CONTINUE
C 
      IF(NL.GT.0) IISYM=IABS(LAB(1))-1
      ICNT=0
      NLEV=IABS(NL) 
      IZX=0 
C 
      DO 600 I=1,NLEV 
      IF(NL.GT.0) IISYM=IISYM+1 
      IF(NL.LT.0) IISYM=LAB(I)
      IF(NL.LT.0.AND.I.EQ.1) IISYM=IABS(IISYM)
C 
      IF(DZ.EQ.ZZZ) CL0=ZMIN(I) 
      IF(DZ.NE.ZZZ) CL0=CL0+DEL 
C 
      IF(LL.LT.0)     GO TO 190 
      ICNT=ICNT+1 
      IF(QLAB)
C----------- NON-ANSI-----
     *           PRINT 200,ICNT,IISYM,CL0 
C----------- NON-ANSI-----
  200 FORMAT(' LEVEL NR. ',I5,' ( ',I1,' ) =',E22.14) 
  190 CONTINUE
      CALL PLCAN(AM,NX,NY,MAXNX,CL0,D,W/HT,THE,HT,IZX,IWRK,XX,YY) 
  600 CONTINUE
                             RETURN 
CEND               LEVEL LOOOP
C ERROR EXIT
C----------- NON-ANSI-----
 1000 PRINT 1010,TH,IT
C----------- NON-ANSI-----
 1010 FORMAT(' ***** (S) VCNTR WITH ILLEGAL THETA/ILINE :',F10.4,I10) 
C 
                             RETURN 
      END 
      SUBROUTINE VECFLD(VX,VY,MAXNX,NX,NY,HEIGHT) 
C VECFLD PLOTS VECTORFIELD DEFINED AT RECTANGULAR GRID
C VX= ARRAY WITH X-COMPONENTS OF VECTOR 
C VY=ARRAY WITH Y-COMPONENTS
C HEIGHT= PLOTHEIGHT
C HEIGHT>0 : BORDER 
C HEIGHT<0 : NO BORDER
C XSIZE=FLOAT(NX)/FLOAT(NY)*ABS(HEIGHT) 
C 
      DIMENSION VX(MAXNX,1),VY(MAXNX,1) 
C ARROW DEFINITION
      AN=.39
      FP=-.2
      FILL=.45
C COPY PARAMETERS 
      NXX=NX
      NYY=NY
      HT=HEIGHT 
C 
      RNX=NXX 
      RNY=NYY 
C 
CBEGIN TEST HEIGHT
      IF(HT.GE.2.0)                GO TO 10 
      CALL VFERR
      PRINT *,' ABS(HEIGHT) < 2.0'
      RETURN
   10 CONTINUE
CEND   TEST HEIGHT
C 
CBEGIN COMPUTE VM=LARGEST COMPONENT OF VECTOR 
      CALL ZMNMX(VX,MAXNX,NXX,NYY,VXMIN,VXMAX)
      CALL ZMNMX(VY,MAXNX,NXX,NYY,VYMIN,VYMAX)
      VXM=AMAX1(ABS(VXMIN),ABS(VXMAX))
      VYM=AMAX1(ABS(VYMIN),ABS(VYMAX))
      VM=AMAX1(VXM,VYM) 
      IF(VM.GT.0.0)            GO TO 20 
      CALL VFERR
      PRINT *,' ALL VECTOR COMPONENTS=0.0'
      RETURN
   20 CONTINUE
CEND COMPUTE VM=LARGETS VECTOR COMPONENT
C 
      YS=ABS(HT)
      XS=RNX/RNY*YS 
      DX=XS/RNX 
      DY=YS/RNY 
      DX2=FILL*DX 
      DY2=FILL*DY 
      FX=DX2/VM 
      FY=DY2/VM 
C 
CBEGIN BORDER 
      IF(HT.LT.0.0)               GO TO 30
      CALL PLOT(XS,0.,2)
      CALL PLOT(XS,YS,2)
      CALL PLOT(0.,YS,2)
      CALL PLOT(0.,0.,2)
   30 CONTINUE
CEND    BORDER
C 
      XB=DX2
      YB=DY2
C 
C ARROW 
      AS=FP*DX2 
C 
      MM=-1 
      DO 200 IY=1,NYY 
      Y=YB+(IY-1)*DY
      MM=-1*MM
C 
      DO 100 IX=1,NXX 
      IF(MM.EQ.-1)        GO TO 40
C MM=1 , FORWARDS 
      F=IX-1
      II=IX 
                                  GO TO 50
   40 CONTINUE
C MM=-1 , BACKWARDS 
      F=NXX-IX
      II=NXX-IX+1 
C 
   50 CONTINUE
      X=XB+F*DX 
      DXX=VX(II,IY)*FX
      DYY=VY(II,IY)*FY
      X1=X-DXX
      Y1=Y-DYY
C 
      CALL PLOT(X1,Y1,3)
      X2=X+DXX
      Y2=Y+DYY
      CALL PLOT(X2,Y2,2)
      IF(DXX.EQ.0.0.AND. DYY.EQ.0.0)   GO TO 100
C ARROW POINT 
      IF(DXX.NE.0.0)        GO TO 55
C DXX=0 
      ANG=1.57
      IF(DYY.LT.0.0) ANG=-ANG 
                          GO TO 60
   55 ANG=ATAN2(DYY,DXX)
   60 CONTINUE
      A=ANG+AN
      XX=X2+AS*COS(A) 
      YY=Y2+AS*SIN(A) 
      CALL PLOT(XX,YY,2)
      CALL PLOT(X2,Y2,3)
      A=ANG-AN
      XX=X2+AS*COS(A) 
      YY=Y2+AS*SIN(A) 
      CALL PLOT(XX,YY,2)
  100 CONTINUE
  200 CONTINUE
C 
      RETURN
      END 
*DECK $FVECPR
      SUBROUTINE VECPR(X,Y,Z,ZL)
C///
*      X, Y AND Z ARE 3-ELEMENT VECTORS IN A CARTESIAN SYSTEM.
*
*       RETURNS  Z = X * Y  AND THE LENGTH OF Z
*                -   -   -
*      Z MAY BE THE SAME AS X AND/OR Y.
C\\\
*      LEO LIJK, GRONINGEN, OCT. 1974
*      FORTRAN-77 VERSION, 26-4-83 (LJL)
*
      DIMENSION X(3),Y(3),Z(3),ZZ(3)
      ZZ(1) = X(2)*Y(3) - X(3)*Y(2)
      ZZ(2) = X(3)*Y(1) - X(1)*Y(3)
      ZZ(3) = X(1)*Y(2) - X(2)*Y(1)
      ZL = SQRT(ZZ(1)*ZZ(1) + ZZ(2)*ZZ(2) + ZZ(3)*ZZ(3))
      DO 1 I = 1,3
1       Z(I) = ZZ(I)
      RETURN
      END
C
C
C
      SUBROUTINE VECTORDIFF( A, B, AMINUSB, NX)
C     ============================================
C
C
      DIMENSION A( NX), B( NX), AMINUSB( NX)
C
C
      DO 999 I = 1, NX
        AMINUSB( I) = A( I) - B( I)
999   CONTINUE
C
C
      RETURN
C
C
      END
      SUBROUTINE VFERR
      PRINT *,' ***** VECFLD ERROR *****' 
      RETURN
      END 

      SUBROUTINE WHERE(XP,YP,SC)
C 
C     RETURNS THE CURRENT POSITION AND SCALE FACTOR 
C 
COPYRIGHT (C) CERRITOS COMPUTER SERVICES, INC. 1976 
C 
C XP    PRESENT X COORDINATE
C YP    PRESENT Y COORDINATE
C SC    PRESENT SCALE FACTOR
C 
      COMMON/PXCOM/LU,MP,MB(128),RE(2),OX,OY,SF,PX,PY 
      XP=PX 
      YP=PY 
      SC=SF 
      RETURN
      END 
C
C
C
      SUBROUTINE WRIMDF(MDFOUT,IPRINT,RBFINF,MAXRBF,BUFINF,MAXBUF)
C     =============================================================
C
C
C
C---- Subroutine for writing introductory records to an MDF.
C     all information should have been stored in  common blocks
C
C
C---- Input parameters:
C     ================
C
C     MDFOUT   : unit number on which output MDF resides
C     IPRINT   : flag for printing the MDF header information:
C                IPRINT = 0  header info is not printed
C                IPRINT = 1  header info will be printed
C     RBFINF   : character array which is filled with the
C                rbuffer information
C     MAXRBF   : maximum length which the rbfinf array can
C                have (should be defined in calling routine)
C     BUFINF   : character array which is filled with the
C                data-buffer information
C     MAXBUF   : maximum length which the bufinf array can
C                have (should be defined in calling routine)
C
C
C---- Other input parameters:
C     =======================
C
C     The character arrays with variable lengths "RBFINF" and "BUFINF"
C     are transferred through the calling statement.  so,  array space
C     should  be  declared  for  these  arrays in the calling routine.
C     all the other parameters are transferred through  common blocks:
C
C     CHARACTER*80    TITLE, RBFINF(MAXRBF), BUFINF(MAXBUF)
C     COMMON /MDFPAR/ MAXR, MAXB, CELL(6), ISLOW, INTER, IFAST,
C    .                KLASS, ICENTR, ISCREW(3), IVERSN
C     COMMON /MDFTTL/ TITLE(20)
C     COMMON /MDFSYM/ NSYM, SYMROT(3,3,96), SYMTRL(3,96)
C
C
C     MAXR     : length of rbufr array
C     MAXB     : length of data buffer array
C     CELL     : cell parameters
C     ISLOW    : slowest changing index
C     INTER    : intermediate changing index
C     IFAST    : fastest changing index
C     KLASS    : laue class (see MDF description)
C     ICENTR   : lattice centering parameters
C     ISCREW   : screw axis information
C     IVERSN   : MDF version number: should be newest version:
C                IVERSN = 1 : april 15, 1987 MDF version
C     TITLE    : general information in MDF
C     RBFINF   : title lines for rbufr contents
C     BUFINF   : title lines for data buffer contents
C     NSYM     : number of symmetry related positions in
C                your spacegroup (including identity
C                transformation as the first symmetry operation)
C     SYMROT   : rotation part of symmetry transformation
C     SYMTRL   : translation part of symmetry transformation
C
C
C
C---- Important notes :
C     ================
C
C     1. Use of symmetry operations
C        XOLD     : starting fractional coordinates
C        XNEW     : coordinates after symmetry operation
C        ISYM     : symmetry operation considered
C
C    XNEW(I) = SYMROT(I,1,ISYM)*XOLD(1) + SYMROT(I,2,ISYM)*XOLD(2)
C                + SYMROT(I,3,ISYM)*XOLD(3) + SYMTRL(I,ISYM)
C
C     2. The symmetry operations are written (and read) as follows:
C
C            WRITE (MDFOUT) NSYM
C            DO 130 ISYM = 1,NSYM
C        130 WRITE (MDFOUT) ((SYMROT(I,J,ISYM),I=1,3),J=1,3),
C           .             (SYMTRL(I,ISYM),I=1,3)
C
C     3. This subroutine makes no calls to any other subroutine.
C
C     4. You should make sure that before calling this subroutine
C        the value of  MAXB  (the  maximum  length which the data
C        buffer can have) has been updated.  afterwards it should
C        be reset to the value of the input MDF to ensure correct
C        reading of the input MDF records.  (maxb  is  stored  in
C        COMMON BLOCK / MDFPAR /).
C
C
C---- Version april 15, 1987  (bauke dijkstra, randy read)
C     ====================================================
C
C
C
C     .. Scalar Arguments ..
      INTEGER IPRINT,MAXBUF,MAXRBF,MDFOUT
C     ..
C     .. Array Arguments ..
      CHARACTER BUFINF(MAXBUF)*80,RBFINF(MAXRBF)*80
C     ..
C     .. Scalars in Common ..
      INTEGER ICENTR,IFAST,INTER,ISLOW,IVERSN,KLASS,LUNIN,LUNOUT,MAXB,
     +        MAXR,NSYM
C     ..
C     .. Arrays in Common ..
      REAL CELL,SYMROT,SYMTRL
      INTEGER ISCREW
      CHARACTER TITLE*80
C     ..
C     .. Local Scalars ..
      INTEGER I,ISYM,J
C     ..
C     .. Local Arrays ..
      CHARACTER CENTR(7)*1,HKL(3)*1,CCLASS(16)*10
C     ..
C     .. Common blocks ..
      COMMON /AINOUT/LUNIN,LUNOUT
      COMMON /MDFPAR/MAXR,MAXB,CELL(6),ISLOW,INTER,IFAST,KLASS,ICENTR,
     +       ISCREW(3),IVERSN
      COMMON /MDFSYM/NSYM,SYMROT(3,3,96),SYMTRL(3,96)
      COMMON /MDFTTL/TITLE(20)
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
C
      DATA ISLOW,INTER,IFAST/1,2,3/
      DATA HKL/'H','K','L'/
      DATA CENTR/'P','A','B','C','F','I','R'/
      DATA (CCLASS(I),I=1,16)/'TRICLINIC ','MONOCLIN-B','O-RHOMBIC ',
     +     'TETRAGNL-1','TETRAGNL-2','TRIGONAL-1','TRIGONAL-2',
     +     'TRIGONAL-3','TRIGONAL-4','TRIGONAL-5','HEXAGONL-1',
     +     'HEXAGONL-2','CUBIC-1   ','CUBIC-2   ','MONOCLIN-A',
     +     'MONOCLIN-C'/
C     ..
C
C---- Now start writing and printing the introductory records
C     on the output master data file.
C
      REWIND MDFOUT
C
C---- RECORD # 1 : general information
C     ==========
C
      WRITE (MDFOUT,ERR=50) TITLE
      IF (IPRINT.EQ.1) THEN
        WRITE (LUNOUT,FMT='(///,'' HEADER RECORDS ON OUTPUT MDF: '',/)')
        DO 10 I = 1,20
          IF (TITLE(I).NE.'  ') WRITE (LUNOUT,FMT='(1X,A80)') TITLE(I)
   10   CONTINUE
        WRITE (LUNOUT,FMT='(/)')
      END IF
C
C---- RECORD # 2 : buffer lengths, slow medium and fast changing
C     ==========     indices, MDF version number
C
      IF (IVERSN.NE.1) THEN
        WRITE (LUNOUT,FMT=
     +'(///,70(''?''),//,'' WRONG VERSION NUMBER FOR '',
     +      '' THE OUTPUT MDF:'',/,'' THIS MDF DOES NOT CONFORM TO '',
     +      ''THE APRIL 15, 1987 MDF VERSION!'',//,70(''?''),//)')
        STOP 'WRONG VERSION FOR OUTPUT MDF'
      ELSE
        WRITE (MDFOUT,ERR=50) MAXR,MAXB,ISLOW,INTER,IFAST,IVERSN
        IF (IPRINT.EQ.1) THEN
          WRITE (LUNOUT,FMT='(//,4X,''CRYSTAL-INFO FROM MDF:'')')
          WRITE (LUNOUT,FMT=6000) MAXR,MAXB,HKL(ISLOW),HKL(INTER),
     +      HKL(IFAST),IVERSN
        END IF
C
C---- Check if maxr and maxb are within program limits
C
        IF (MAXB.GT.MAXBUF) THEN
          WRITE (LUNOUT,FMT=
     +'(/,''    MAXB ON MDF LARGER THAN PARAMETER '',           ''MAXBUF
     + IN SUBROUTINE WRIMDF :  STOP!'')')
          STOP 'MAXBUF TOO SMALL'
        ELSE IF (MAXR.GT.MAXRBF) THEN
          WRITE (LUNOUT,FMT=
     +'(/,''    MAXR ON MDF LARGER THAN PARAMETER '',            ''MAXRB
     +F IN SUBROUTINE WRIMDF :  STOP!'')')
          STOP 'MAXRBF TOO SMALL'
        ELSE
C
C---- RECORD # 3 : rbfinf array
C     ==========
C
          WRITE (MDFOUT,ERR=50) (RBFINF(I),I=1,MAXR)
          IF (IPRINT.EQ.1) THEN
            WRITE (LUNOUT,FMT='(/,/,4X,''CONTENTS OF RBUFFR-INFO:'')')
            DO 20 I = 1,MAXR
              IF (RBFINF(I).NE.' ') WRITE (LUNOUT,FMT=6002) I,RBFINF(I)
   20       CONTINUE
            WRITE (LUNOUT,FMT='(/)')
          END IF
C
C---- RECORD # 4 : title for data buffer array
C     ==========
C
          WRITE (MDFOUT,ERR=50) (BUFINF(I),I=1,MAXB)
          IF (IPRINT.EQ.1) THEN
            WRITE (LUNOUT,FMT='(/,/,4X,''CONTENTS OF BUFFER-INFO:'')')
            DO 30 I = 1,MAXB
              IF (BUFINF(I).NE.' ') WRITE (LUNOUT,FMT=6002) I,BUFINF(I)
   30       CONTINUE
            WRITE (LUNOUT,FMT='(/)')
          END IF
C
C---- RECORD 5 : cell dimensions, klass, centering, screw axis info
C     ========
C
          WRITE (MDFOUT,ERR=50) CELL,KLASS,ICENTR,ISCREW
          IF (IPRINT.EQ.1) THEN
            WRITE (LUNOUT,FMT=6004) CELL
            WRITE (LUNOUT,FMT=6006) CCLASS(KLASS),CENTR(ICENTR+1),
     +        ISCREW
          END IF
C
C---- RECORD # 6 : write symmetry information
C     ==========
C
          IF (IVERSN.EQ.0) THEN
            RETURN
          ELSE
            WRITE (MDFOUT,ERR=50) NSYM
C
C---- RECORD # 7 : symmetry operations
C     ==========
C
            DO 40 ISYM = 1,NSYM
              WRITE (MDFOUT,ERR=50) ((SYMROT(I,J,ISYM),I=1,3),J=1,3),
     +           (SYMTRL(I,ISYM),I=1,3)
              IF (IPRINT.EQ.1) WRITE (LUNOUT,FMT=
     +'(/,'' SYMMETRY OPERATION'',
     +      I5,//,3(3F10.5,5X,F10.5,/))') ISYM,
     +            ((SYMROT(I,J,ISYM),J=1,3),SYMTRL(I,ISYM),I=1,3)
   40       CONTINUE
C
C---- All intro records now written
C     return to calling program
C
            RETURN
          END IF
        END IF
      END IF
   50 WRITE (LUNOUT,FMT=
     +'(/,'' ERROR WHILE WRITING THE HEADER RECORDS ON THE '',
     +''MASTER DATA FILE'',/,'' SOMETHING VERY WRONG!'',///)')
      STOP 'ERROR ON MDF IN WRIMDF'
C
C---- Format statements
C
 6000 FORMAT (/4X,'LENGTH OF RBUFFR-ARRAY   ',I10,/4X,'LENGTH OF BUFFE',
     +       'R ARRAY   ',I10,/4X,'SLOW CHANGING INDEX',15X,1A,/4X,'ME',
     +       'DIUM CHANGING INDEX',13X,1A,/4X,'FAST CHANGING INDEX',15X,
     +       1A,/4X,'MDF-VERSION NUMBER       ',I10,/4X,'VERSION APRIL',
     +       ' 15, 1987',//)
 6002 FORMAT (/'    LINE NR',I4,2X,A80)
 6004 FORMAT (/'    A-AXIS IN ANGSTROMS      ',F8.3,/'    B-AXIS IN AN',
     +       'GSTROMS      ',F8.3,/'    C-AXIS IN ANGSTROMS      ',F8.3,
     +       /'    ALPHA  IN DEGREES        ',F8.3,/'    BETA   IN DEG',
     +       'REES        ',F8.3,/'    GAMMA  IN DEGREES        ',F8.3,
     +       /)
 6006 FORMAT (/'    CRYSTAL CLASS            ',A10,/'    CRYSTAL LATTI',
     +       'CE          ',A1,/'    THERE IS A ',I1,'-FOLD SCREW AXIS',
     +       ' PARALLEL TO THE A-AXIS',/'    THERE IS A ',I1,'-FOLD SC',
     +       'REW AXIS PARALLEL TO THE B-AXIS',/'    THERE IS A',I2,
     +       '-FOLD SCREW AXIS PARALLEL TO THE C-AXIS',//)
C
C
      END
C
C
C
      SUBROUTINE WRTREC(MDFOUT,RBUFFR,MAXR,IHKL,BUFOUT,MAXIB,IDEL)
C     =============================================================
C
C
C
C---- Subroutine to write a reflection record to an MDF
C     file in a general way, with removal of trailing absent
C     data buffer elements
C
C
C---- Meaning of the parameters:
C     ==========================
C
C      Input parameters for this subroutine:
C
C      - MDFOUT         unit number on which the output MDF resides
C
C      - RBUFFR         array rbuffr(maxr) contains the data
C                       present in the rbuffr
C
C      - MAXR           length of rbuffr array
C
C      - IHKL           array ihkl(3) contains ih, ik, il
C
C      - BUFOUT         array bufout(maxib) contains the data
C                       present in the buffer.  all of the elements
C                       of bufout to maxib must be defined, even
C                       if as absent.
C
C      - MAXIB          length of data bufout array
C
C      - IDEL           = 0 : nothing wrong
C                       = 1 : hkl record not written to output MDF
C                             (deleted)
C
C
C---- Important notes :
C     ================
C
C     1. This subroutine makes no calls to any other subroutine.
C
C     2. The maximum values of maxr and maxib should have been
C        written to the MDF header records previously (e.g. by
C        subroutine WRIMDF)
C
C
C---- Version april 15, 1987  (randy read, bauke dijkstra)
C     ====================================================
C
C
C     .. Scalar Arguments ..
      INTEGER IDEL,MAXIB,MAXR,MDFOUT
C     ..
C     .. Array Arguments ..
      REAL BUFOUT(MAXIB),RBUFFR(MAXR)
      INTEGER IHKL(3)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      REAL ABSENT
      INTEGER I,LB
C     ..
C     .. Common blocks ..
      COMMON /AINOUT/LUNIN,LUNOUT
C     ..
C     .. Data statements ..
      DATA ABSENT/-1.0E10/
C     ..
C
C
      IDEL = 0
      IF (MAXIB.LE.0) THEN
        IDEL = 1
      ELSE
C
C---- BUFOUT(LB) should not be absent.  if absent, reduce lb by 1
C     and try again (unless lb is already 0).
C     if all entries in bufout are absent, delete record
C
        LB = MAXIB
   10   CONTINUE
        IF (BUFOUT(LB).GT.ABSENT) THEN
          GO TO 20
        ELSE
          LB = LB - 1
          IF (LB.GT.0) GO TO 10
        END IF
        IDEL = 1
        RETURN
   20   WRITE (MDFOUT) LB, (RBUFFR(I),I=1,MAXR),IHKL, (BUFOUT(I),I=1,LB)
      END IF
C
C
      END
      SUBROUTINE XINMAX(A,NMAX,N,M,Z) 
C------------------CALLED BY THREED 
C Z(1)<=A(I,J)<=Z(2)
      DIMENSION Z(2),A(NMAX,M)
C 
 1050 U = A( 1, 1 ) 
 1060 V = U 
C 
 1080 DO 1190 J = 1, M
C 
 1100 DO 1180 I = 1, N
C 
 1120 IF (U-A(I , J ) ) 1150, 1150, 1130
 1130 U = A ( I, J )
C 
 1150 IF (A(I , J ) - V ) 1180, 1180, 1160
 1160 V = A ( I , J ) 
C 
 1180 CONTINUE
 1190 CONTINUE
 1210 Z(1) = U
 1220 Z(2) = V
 1230 RETURN
      END 
      SUBROUTINE XMAT33(A,B,C)
C///  VERSION 83/07/28 (LJL (INS.))
* ------------- COMPUTE âCÕ = âAÕ * âBÕ         A,B,C ARE 3*3 MATRICES
*      âCÕ MAY BE THE SAME AS EITHER âAÕ OR âBÕ.
C\\\
      DIMENSION A(3,3),B(3,3),C(3,3), A1(3,3), B1(3,3)
      DO 1 N=1,3
        DO 1 M=1,3
          A1(N,M) = A(N,M)
          B1(N,M) = B(N,M)
1         C(N,M) = 0.
      DO 30 N=1,3
        DO 20 M=1,3
          DO 10 J=1,3
10          C(N,M) = C(N,M) + A1(N,J)*B1(J,M)
20      CONTINUE
30    CONTINUE
      RETURN
      END
      SUBROUTINE XOTATE (X, Y, Z, XP, YP, ZP )
C------------------CALLED BY THREED 
      COMMON /X3HZ39/ANGA,ANGB,HV,D,SL,SM,SN,CX,CY,CZ,QX,QY,QZ,SD 
      X=X 
      Y=Y 
      Z=Z 
      SK=D/( (X-CX)*SL+(Y-CY)*SM+(Z-CZ)*SN) 
      XP=CX+SK*(X-CX) 
      YP=CY+SK*(Y-CY) 
      ZP=CZ+SK*(Z-CZ) 
      RETURN
      END 
      SUBROUTINE XRTHO(X,Y,A,NMAX,N,M,H,V,K)
C------------------CALLED BY THREED 
  030 DIMENSION X(2),Y(2),H(10),V(10),A(NMAX,M) 
      COMMON /X3HZ39/ANGA,ANGB,HV,D,SL,SM,SN,CX,CY,CZ,QX,QY,QZ,SD 
      COMMON /CAX3D/ QAX3D,QSCERR,ZZ1,ZZ2,NIX,NIY,NIZ 
C 
      COMMON /COMTRD/ XSS,YSS,CEND
      INTEGER UP , DOWN, PEN, P, Q
      INTEGER P1, PO
      LOGICAL QAX3D 
C 
C 
C 
C*****
      END=CEND
C     CAN USE 1/32 OR 1/64 FOR FINER INTERPOLATION
C 
C 
      UP = 3
      DOWN = 2
C 
C SCALE IN XSIZE*YSIZE WITHOUT DISTORTION 
      RH=H(10)-H(9) 
      RV=V(10)-V(9) 
C 
      SS=AMIN1(XSS/ABS(RH),YSS/ABS(RV)) 
      SH=SIGN(SS,RH)
      SV=SIGN(SS,RV)
      IF(.NOT.QAX3D)         GO TO 10 
CBEGIN AXES 
      DO 1 I=1,8
      H(I)=(H(I)-H(9))*SH 
    1 V(I)=(V(I)-V(9))*SV 
CKRAAK**** DL=LENGTH TICKMARKS IN CM HELDER******** 
      DL=.14
      ANG=ATAN((V(5)-V(1))/(H(5)-H(1))) +3.14 
      NI=NIZ
      CALL AX3D(H,V,1,2,NI,ANG,DL)
      ANG=1.57
      NI=NIX
      CALL AX3D(H,V,1,5,NI,ANG,DL)
      NI=NIY
      CALL AX3D(H,V,5,7,NI,ANG,DL)
CEND   AXES 
   10 CONTINUE
C 
      MM = M
      NN = N
  080 IF ( K - 1 ) 100, 120, 100
C 
  100 IF ( K - 3 ) 1110, 120, 1110
C 
  120 CONTINUE
      L = 0 
      LD = 1
      DD = 0.5 * FLOAT(LD)
C 
  140 DO 1060 J = 1, M
      Q = 0 
      YJ = J
  160 DO 1030 I = 1, NN 
C 
      L = L + LD
      XI = L
      CALL CANSEE(A,NMAX,XI,YJ,N,M,P) 
      PEN = UP
      IF ( P ) 510, 520, 530
  510 CONTINUE
C     IF ( Q ) 540, 550, 170
      IF ( Q ) 540, 550, 540
  520 CONTINUE
      IF ( Q ) 610, 1020, 610 
  530 CONTINUE
C     IF ( Q ) 170, 550, 540
      IF ( Q ) 540, 550, 540
  540 CONTINUE
      PEN = DOWN
      GO TO 170 
  550 CONTINUE
      IF ( I . EQ. 1 ) GO TO 170
      DI = DD 
      TO = L - LD 
      T = TO + DI 
      P1 = Q
  560 IF ( ABS( DI ) .LT. END ) GO TO 570 
      CALL CANSEE(A,NMAX,T ,YJ,N,M,PO)
      DI = DI * 0.5 
      IF ( PO .EQ.  0 ) GO TO 565 
      TO = T
      P1 = PO 
      T = T - DI
      GO TO 560 
  565 T = T + DI
      GO TO 560 
  570 CONTINUE
      T = TO
      IF ( P1 * P ) 170, 170, 580 
  580 CONTINUE
  590 CONTINUE
      II=L-LD 
      ZP=A(II,J)+(T-FLOAT(II))*(A(L,J)-A(II,J))/FLOAT(LD) 
      CALL XOTATE(T,YJ,ZP,XP,HH,VV) 
      HH = ( ( XP-QX)*SM- (HH - QY )*SL ) * SD
      VV = ( VV - QZ ) * SD 
      HH = ( HH - H(9) ) * SH 
      VV=( VV - V(9) ) * SV 
      CALL PLOT ( HH, VV , PEN )
  600 PEN = 5 - PEN 
      GO TO 170 
  610 CONTINUE
      PEN = DOWN
      DI = DD 
      TO = L - LD 
      T = TO + DI 
      P1 = Q
  620 IF ( ABS(DI ) .LT. END ) GO TO 630
      CALL CANSEE(A,NMAX,T ,YJ,N,M,PO)
      DI = DI * 0.5 
      IF ( PO . EQ. 0 ) GO TO 625 
      TO = T
      P1 = PO 
      T = T + DI
      GO TO 620 
  625 T = T - DI
      GO TO 620 
  630 CONTINUE
      T = TO
      IF ( P1 * Q ) 600,600,590 
  170 CALL XOTATE ( XI, YJ, A ( L, J ) , XP, HH, VV)
      VV = ( VV - QZ ) * SD 
      HH = ( ( XP-QX)*SM- ( HH - QY ) * SL ) * SD 
  190 HH = ( HH - H(9) ) * SH 
  200 VV = ( VV - V(9) ) * SV 
      CALL PLOT ( HH, VV, PEN ) 
 1020 Q = P 
 1030 CONTINUE
C 
C 
      L = L + LD
      LD = -LD
      DD = -DD
C 
 1060 CONTINUE
C 
C 
 1090 IF ( K - 3 ) 2060, 1110, 2060 
C 
 1110 CONTINUE
C 
      L = 0 
      LD = 1
      DD = 0.5*FLOAT(LD)
 1140 DO 2040 I = 1, N
      XI = I
      Q = 0 
 1160 DO 2020 J = 1, MM 
      L = L + LD
      YJ = L
      CALL CANSEE(A,NMAX,XI,YJ,N,M,P )
      PEN = UP
      IF ( P ) 1510, 1520, 1530 
 1510 CONTINUE
C     IF ( Q ) 1540, 1550, 1170 
      IF ( Q ) 1540, 1550, 1540 
 1520 CONTINUE
      IF ( Q ) 1610, 2010, 1610 
 1530 CONTINUE
C     IF ( Q ) 1170, 1550, 1540 
      IF ( Q ) 1540, 1550, 1540 
 1540 CONTINUE
      PEN = DOWN
      GO TO 1170
 1550 CONTINUE
      IF ( J .EQ. 1 ) GO TO 1170
      DI = DD 
      TO = L - LD 
      T = TO + DI 
      P1 = Q
 1560 IF ( ABS(DI ) .LT. END ) GO TO 1570 
      CALL CANSEE(A,NMAX,XI,T ,N,M,PO)
      DI = DI * 0.5 
      IF ( PO .EQ. 0 ) GO TO 1565 
      TO = T
      P1 = PO 
      T = T - DI
      GO TO 1560
 1565 T = T + DI
      GO TO 1560
 1570 CONTINUE
      T = TO
      IF ( P1 * P ) 1170, 1170, 1580
 1580 CONTINUE
 1590 CONTINUE
      II=L-LD 
      ZP=A(I,II)+(T-FLOAT(II))*(A(I,L)-A(I,II))/FLOAT(LD) 
      CALL XOTATE ( XI , T , ZP , XP , HH , VV )
      HH = ( ( XP-QX)*SM- (HH - QY )*SL ) * SD
      VV = ( VV - QZ ) * SD 
      HH = ( HH - H(9) ) * SH 
      VV = ( VV - V(9) ) * SV 
      CALL PLOT ( HH , VV , PEN ) 
 1600 PEN = 5 - PEN 
      GO TO 1170
 1610 CONTINUE
      PEN = DOWN
      DI = DD 
      TO = L - LD 
      T = TO + DI 
      P1 = Q
 1620 IF ( ABS ( DI ) .LT. END ) GO TO 1630 
      CALL CANSEE(A,NMAX,XI,T ,N,M,PO)
      DI = DI * 0.5 
      IF ( PO .EQ. 0 ) GO TO 1625 
      TO = T
      P1 = PO 
      T = T + DI
      GO TO 1620
 1625 T = T - DI
      GO TO 1620
 1630 CONTINUE
      T = TO
      IF ( P1 * Q ) 1600 , 1600 , 1590
 1170 CALL XOTATE ( XI, YJ, A ( I, L ), XP, HH, VV )
      HH = ( ( XP-QX)*SM- (HH - QY ) *SL ) * SD 
      VV = ( VV - QZ ) * SD 
 1180 HH = ( HH - H(9) ) * SH 
 1190 VV = ( VV - V(9) ) * SV 
      CALL PLOT( HH , VV , PEN )
 2010 Q = P 
 2020 CONTINUE
C 
      L = L + LD
      LD = - LD 
      DD = -DD
 2040 CONTINUE
C 
 2060 CONTINUE
C 
 2130 RETURN
      END 
      SUBROUTINE XTITLE (IPEN,TITLE,XSIZE)
C 
C 
C.....PLOTS A TEXT TO THE X-AXIS
C 
C 
      LOGICAL DEBUG
      COMMON /SCALE/ XMIN, YMIN, XSCALE, YSCALE, DELTAX, DELTAY,
     + XPOS, YPOS, XM, YM, TZ, DEBUG, DASHL, ISPEN, ISYMB, SSIZE
C 
C 
      CHARACTER*80 TITLE,DUM
      INTEGER LARAY(40)
      EQUIVALENCE (LARAY(1),DUM)
      DUM=TITLE
C 
C 
C.....FIND LENGTH TITLE 
      DO 10, I=80,1,-1
          IF (TITLE(I:I).NE.' ') THEN 
              ILEN=I
              GOTO 20 
          ENDIF 
   10 CONTINUE
   20 PHYSLN=ILEN*TZ
      IF (PHYSLN.GT.XSIZE) THEN 
          XPP=0.0 
      ELSE
          XPP=(XSIZE-PHYSLN)/2.0
      ENDIF 
C 
C 
      IF (.NOT.DEBUG) CALL NEWPEN (ABS(IPEN))
      IF (.NOT.DEBUG) CALL SYMBOL (XPP,YM-1.5*TZ,TZ,LARAY,0.0,ILEN) 
      RETURN
      END 
      SUBROUTINE XUBE ( X, Y, Z, XP, H, V ) 
C------------------CALLED BY THREED 
  030 DIMENSION X(2), Y(2), Z(2), H(10),V(10),XP(8) 
C 
  050 L = 0 
  070 DO 180 I = 1, 2 
C 
  090 DO 170 J = 1, 2 
C 
  110 DO 160 K = 1, 2 
C 
  130 L = L + 1 
  140 CALL XOTATE (X(I), Y(J), Z(K), XP(L),    H(L) , V(L) )
  160 CONTINUE
  170 CONTINUE
  180 CONTINUE
  190 RETURN
      END 
      SUBROUTINE XXORG(IDEV,IPSIZE,IBUFF)                               
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C THIS SUBROUTINE PROVIDES THE DEFAULT P1 SCALING POINT                 
C IN PLOTTER UNITS THAT WOULD NORMALLY BE RETRIEVED FROM THE            
C PLOTTER. THIS IS NECCESSARY FOR SPOOLING APPLICATIONS WHEN            
C THESE VALUES CAN NOT BE READ.                                         
C                                                                       
C IDEV = PLOTTER TYPE                                                   
C        (DEVICES SUPPORTED - 7220,9872,7470,7475,7550,7580,7585)       
C                                                                       
C                                                                       
C                                                                       
C IDEV  ISIZE  PAPER SIZE                          TABLE INDEX          
C ----  -----  ----------                          -----------          
C                                                                       
C         0       A0                                    1               
C         1       A1                                    2               
C 7585    2       A2   METRIC                           3               
C         3       A3                                    4               
C  OR     4       A4                                    5               
C        10       E                                     6               
C 7586   11       D                                     7               
C        12       C    ENGLISH                          8               
C        13       B                                     9               
C        14       A                                    10               
C                                                                       
C         1       A1                                   21               
C         2       A2   METRIC                          22               
C         3       A3                                   23               
C 7580    4       A4                                   24               
C        11       D                                    25               
C        12       C                                    26               
C        13       B    ENGLISH                         27               
C        14       A                                    28               
C                                                                       
C FOR EXPANDED MODE, ADD 100 TO ISIZE           11-20,29-36             
C                                                                       
C         3       A3   METRIC                          37               
C 7550    4       A4                                   38               
C        13       B                                    39               
C        14       A    ENGLISH                         40               
C                                                                       
C         3       A3   METRIC                          41               
C 7475    4       A4                                   42               
C        13       B                                    43               
C        14       A    ENGLISH                         44               
C                                                                       
C 7470  4 OR 14   A4 AND A    (METRIC AND ENGLISH)     45               
C                                                                       
C 9872   13       B    ENGLISH                         46               
C  OR   1003       METRIC ROLL PAPER                    47              
C 7220  1013       ENGLISH ROLL PAPER                   48              
C                                                                       
C                                                                       
      DIMENSION IBUFF(2), IX(48),IY(48)                                 
C                                                                       
C*** P1 DATA                                                            
C                                                                       
C  7585 AND 7586                                                        
      DATA       IX(1), IX(2), IX(3), IX(4), IX(5)                      
     +        / -22190,-15120, -6875, -6780, -2700 /                    
      DATA       IX(6), IX(7), IX(8), IX(9),IX(10)                      
     +        / -20840,-15710, -7090, -7100, -2790 /                    
      DATA      IX(11),IX(12),IX(13),IX(14),IX(15)                      
     +        / -22590,-15520, -7270, -7180, -3090 /                    
      DATA      IX(16),IX(17),IX(18),IX(19),IX(20)                      
     +        / -21250,-16110, -7485, -7500, -3190 /                    
C  7580                                                                 
      DATA      IX(21),IX(22),IX(23),IX(24)                             
     +        / -15120,-10280, -6780, -2700 /                           
      DATA      IX(25),IX(26),IX(27),IX(28)                             
     +        / -15710, -9640, -7100, -2790 /                           
      DATA      IX(29),IX(30),IX(31),IX(32)                             
     +        / -15520,-10680, -7180, -3090 /                           
      DATA      IX(33),IX(34),IX(35),IX(36)                             
     +        / -16110,-10040, -7500, -3190 /                           
C  7550                                                                 
      DATA      IX(37),IX(38),IX(39),IX(40)                             
     +        /    380,   430,   620,    80 /                           
C  7475                                                                 
      DATA      IX(41),IX(42),IX(43),IX(44)                             
     +        /    170,   603,   522,   250 /                           
C  7470                                                                 
      DATA      IX(45)                                                  
     +        /    250 /                                                
C  9872 AND 7220                                                        
      DATA      IX(46),IX(47),IX(48)                                    
     +        /    520,   520,   520 /                                  
C                                                                       
C  7585 AND 7586                                                        
      DATA       IY(1), IY(2), IY(3), IY(4), IY(5)                      
     +        / -15740,-10760,-10785, -4830, -4830 /                    
      DATA       IY(6), IY(7), IY(8), IY(9),IY(10)                      
     +        / -16180,-10060,-10075, -4500, -4500 /                    
      DATA      IY(11),IY(12),IY(13),IY(14),IY(15)                      
     +        / -16145,-11160,-11180, -5240, -5230 /                    
      DATA      IY(16),IY(17),IY(18),IY(19),IY(20)                      
     +        / -16580,-10460,-10480, -4900, -4900 /                    
C  7580                                                                 
      DATA      IY(21),IY(22),IY(23),IY(24)                             
     +        / -10760, -7260, -4830, -4830 /                           
      DATA      IY(25),IY(26),IY(27),IY(28)                             
     +        / -10060, -7530, -4500, -4500 /                           
      DATA      IY(29),IY(30),IY(31),IY(32)                             
     +        / -11160, -7660, -5240, -5230 /                           
      DATA      IY(33),IY(34),IY(35),IY(36)                             
     +        / -10460, -7930, -4900, -4900 /                           
C  7550                                                                 
      DATA      IY(37),IY(38),IY(39),IY(40)                             
     +        /    430,   200,    80,   320 /                           
C  7475                                                                 
      DATA      IY(41),IY(42),IY(43),IY(44)                             
     +        /    602,   529,   259,   596 /                           
C  7470                                                                 
      DATA      IY(45)                                                  
     +        /    279 /                                                
C  9872 AND 7220                                                        
      DATA      IY(46),IY(47),IY(48)                                    
     +        /    380,  1140,  1020 /                                  
C                                                                       
      ISIZE=IPSIZE+1                                                    
      IF (ISIZE.GT.100) ISIZE = ISIZE - 100                             
C                                                                       
      IF (IDEV.NE.7585.AND.IDEV.NE.7586) GO TO 10                       
C 7585 OR 7586                                                          
      IF (ISIZE.GT.10) ISIZE = ISIZE - 5                                
      IF (IPSIZE.GE.100) ISIZE = ISIZE + 10                             
      GO TO 5                                                           
  10  CONTINUE                                                          
      IF (IDEV.NE.7580) GO TO 20                                        
C 7580                                                                  
      IF (ISIZE.GT.10) ISIZE = ISIZE - 6                                
      IF (IPSIZE.GE.100) ISIZE = ISIZE + 8                              
      ISIZE = ISIZE + 19                                                
      GO TO 5                                                           
C                                                                       
  20  CONTINUE                                                          
      IF (IDEV.NE.7550) GO TO 30                                        
C 7550                                                                  
      IF (ISIZE.GT.10) ISIZE = ISIZE - 8                                
      ISIZE = ISIZE + 33                                                
      GO TO 5                                                           
C                                                                       
  30  CONTINUE                                                          
      IF (IDEV.NE.7475) GO TO 40                                        
C 7475                                                                  
      IF (ISIZE.GT.10) ISIZE = ISIZE - 8                                
      ISIZE = ISIZE + 37                                                
      GO TO 5                                                           
C                                                                       
  40  CONTINUE                                                          
      IF (IDEV.NE.7470) GO TO 50                                        
C 7470                                                                  
      ISIZE = 45                                                        
      GO TO 5                                                           
C                                                                       
  50  CONTINUE                                                          
      ISIZE = 46                                                        
      IF (IPSIZE.EQ.1003) ISIZE = 47                                    
      IF (IPSIZE.EQ.1013) ISIZE = 48                                    
C                                                                       
C                                                                       
5     CONTINUE                                                          
C                                                                       
      IF (ISIZE.LT.1.OR.ISIZE.GT.48) ISIZE=1                            
C                                                                       
      IBUFF(1)= IX(ISIZE)                                               
      IBUFF(2)= IY(ISIZE)                                               
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE XYTEST (X,Y,OLDX,OLDY,START,YES,LARGEX,OLD,IPEN)
C
C
C.....CHECKS IF DATA POINT LIES WITHIN FRAME
      LOGICAL START, YES, LARGEX, OLD, DEBUG
      COMMON /SCALE/ XMIN, YMIN, XSCALE, YSCALE, DELTAX, DELTAY,
     + XPOS, YPOS, XM, YM, TZ, DEBUG, DASHL, ISPEN, ISYMB, SSIZE
C
C
C
      XSMAX=DELTAX*XSCALE
      YSMAX=DELTAY*YSCALE
      IF (START) THEN
          IF (X.GE.0.0.AND.X.LE.XSMAX.AND.
     +        Y.GE.0.0.AND.Y.LE.YSMAX) THEN
              IF (OLD) THEN
                 XDUM=X
                 YDUM=Y
                 CALL INTPOL (X,Y,OLDX,OLDY,XSMAX,YSMAX,YES)
                 IF (IPEN.GE.0) THEN
                     IF (.NOT.DEBUG) CALL PLOT (X,Y,3)
                 ELSE
                     IF (.NOT.DEBUG) CALL PLDASH(X,Y,0.0)
                 ENDIF
                 X=XDUM
                 Y=YDUM
                 YES=.TRUE.
              ELSE
                 IF (IPEN.GE.0) THEN
                     IF (.NOT.DEBUG) CALL PLOT (X,Y,3)
                 ELSE
                     IF (.NOT.DEBUG) CALL PLDASH(X,Y,0.0)
                 ENDIF
                 YES=.TRUE.
                 OLD=.TRUE. 
              ENDIF
              START=.FALSE.
              RETURN
          ENDIF
          YES=.FALSE.
          RETURN
      ENDIF
      IF (X.LT.0) THEN
          YES=.FALSE.
          RETURN
      ENDIF
      IF (X.GT.XSMAX) THEN
          IF (LARGEX) THEN
              YES=.FALSE.
              RETURN
          ELSE
              LARGEX=.TRUE.
          ENDIF
      ENDIF
      CALL INTPOL (X,Y,OLDX,OLDY,XSMAX,YSMAX,YES)
      RETURN
      END
      SUBROUTINE YAXIS (XSIZE,YSIZE,YMAX)
C 
C
C.....PLOTS A SECOND Y-AXIS AT THE RIGHT SIDE OF THE PLOT
C 
      LOGICAL DEBUG
      CHARACTER*8 TITLE
      INTEGER LARAY(4)
      EQUIVALENCE (LARAY(1),TITLE) 
      COMMON /SCALE/ XMIN, YMIN, XSCALE, YSCALE, DELTAX, DELTAY,
     + XPOS, YPOS, XM, YM, TZ, DEBUG, DASHL, ISPEN, ISYMB, SSIZE
      COMMON /STEPS/ SMARKX, SMARKY, OFFSET
C
C.....SCHALING Y-AXIS 
      DELTAY=YMAX-YMIN
      YSCALE=YSIZE/DELTAY
      IF (SMARKY.NE.0.0) THEN
         NSY   = INT(DELTAY/SMARKY)
         STEPY = SMARKY * YSIZE / DELTAY
      ELSE
        CALL DECENT (DELTAY,YSIZE,NSY,STEPY,1.0,MODE)
        SMARKY=DELTAY/YSIZE * STEPY
      ENDIF
      DEL=SMARKY
      RMIN=YMIN
      RMAX=YMAX
      CALL CMODE   (DEL,RMIN,RMAX,STEPY,CENTER,RM,MODE)
C 
C.....PLOT DIVISIONS Y-AXIS
      YAXPOS=XSIZE - 8*TZ + RM + 0.5*TZ
      DO 20, I=0,NSY
          YP=I*STEPY
          IF (.NOT.DEBUG) CALL PLOT (XSIZE,YP,3)
          IF (.NOT.DEBUG) CALL PLOT (XSIZE-0.5*TZ,YP,2)
          YMARK=YMIN + I*SMARKY
          IF (MODE.EQ.1) WRITE (TITLE,'(E8.1)') YMARK
          IF (MODE.EQ.2) THEN
              IMARK=NINT(YMARK)
              WRITE (TITLE,'(I8)') IMARK
          ENDIF
          IF (MODE.EQ.3) WRITE (TITLE,'(F8.1)') YMARK
          IF (MODE.EQ.4) WRITE (TITLE,'(F8.2)') YMARK
          IF (MODE.EQ.5) WRITE (TITLE,'(F8.3)') YMARK
          IF (.NOT.DEBUG) CALL SYMBOL (YAXPOS,YP-TZ/2.0,TZ,
     +                                 LARAY,0.0,8)
   20 CONTINUE
C
C.....FIND POSITIONS FOR TITLES FOR AXES
      XM=YAXPOS+10.0*TZ
      RETURN
      END
      SUBROUTINE YTITLE (IPEN,TITLE,YSIZE)
C 
C 
C.....PLOTS A TITLE TO THE Y-AYIS 
C 
C 
      LOGICAL DEBUG
      COMMON /SCALE/ XMIN, YMIN, XSCALE, YSCALE, DELTAX, DELTAY,
     + XPOS, YPOS, XM, YM, TZ, DEBUG, DASHL, ISPEN, ISYMB, SSIZE
C 
C 
      CHARACTER*80 TITLE,DUM
      INTEGER LARAY(40)
      EQUIVALENCE (LARAY(1),DUM)
      DUM=TITLE
C 
C 
C.....FIND LENGTH TITLE 
      DO 10, I=80,1,-1
          IF (TITLE(I:I).NE.' ') THEN 
              ILEN=I
              GOTO 20 
          ENDIF 
   10 CONTINUE
   20 PHYSLN=ILEN*TZ
      IF (PHYSLN.GT.YSIZE) THEN 
          YPP=0.0 
      ELSE
          YPP=(YSIZE-PHYSLN)/2.0
      ENDIF 
C 
C 
      IF (.NOT.DEBUG) CALL NEWPEN (ABS(IPEN))
      IF (.NOT.DEBUG) CALL SYMBOL (XM,YPP,TZ,LARAY,90.0,ILEN) 
      RETURN
      END 
      SUBROUTINE ZMNMX(Z,MAXNX,NX,NY,ZMN,ZMX) 
C COMPUTE ZMN+ZMX OF ARRAY Z
      DIMENSION Z(MAXNX,1)
C**************** D VAX *********** 
      ZMN=1.0E38
      ZMX=-ZMN
      DO 10 I=1,NX
      DO 10 J=1,NY
      IF(Z(I,J).GT.ZMX) ZMX=Z(I,J)
      IF(Z(I,J).LT.ZMN) ZMN=Z(I,J)
   10 CONTINUE
      RETURN
      END 
      SUBROUTINE ZPRMDS(A)
      DIMENSION A(20,20),P(10,10),H(4)
      DATA H / 1.5,2.0,1.0,0.5 /
C      CONSTRUCT PYRAMID IN P --- 
      NN=10 
       NMIN=0 
       NMAX=NN+1
       KMAX=NN/2
       KSTEP=1
       Z=-1.
       DO 200 K=1,KMAX,KSTEP
           Z=Z+1. 
           NMI=NMIN+K 
           NMA=NMAX-K 
           DO 200 I=NMI,NMA 
           DO 200 J=NMI,NMA 
               P(I,J)=Z 
200    CONTINUE 
C      END PYRAMID ---
C 
C      FOUR PYRAMIDS IN A USING THE PYRAMID IN P AND THE HEIGHTS IN H 
      M=0 
      DO 500 K=1,2
      DO 500 L=1,2
         M=M+1
         LX=(K-1)*NN
         DO 400 I=1,NN
            LX=LX+1 
            LY=(L-1)*NN 
            DO 400 J=1,NN 
               LY=LY+1
               A(LX,LY)=P(I,J)*H(M) 
400      CONTINUE 
500   CONTINUE
C 
      RETURN
      END 
      SUBROUTINE ZZEXTR(ISOURC,ISSIZE,IPTR,IDEST)                       
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C                                                                       
C                                                                       
C     EXTRACT ASCII CODES FROM HOLLERITH DATA                           
C                                                                       
C     *****   THIS ROUTINE IS MACHINE-DEPENDENT   *****                 
C                                                                       
C     ISOURC = SOURCE ARRAY CONTAINING ONE OR MORE CHARACTERS IN        
C                 THE HOLLERITH FORMAT OF THE HOST PROCESSOR            
C                                                                       
C     ISSIZE = SIZE OF -ISOURC-                                         
C                                                                       
C     IPTR   = POINTER DEFINING WHICH CHARACTER IS TO BE                
C                 EXTRACTED. (IPTR=1 DESIGNATES THE LEFTMOST            
C                 CHARACTER.)                                           
C                                                                       
C     IDEST  = DESTINATION INTEGER TO BE LOADED WITH THE ASCII          
C                 CODE FOR THE DESIRED CHARACTER                        
C                                                                       
C                                                                       
C     ***************************************************************   
C     *                                                             *   
C     *   GIVEN AN ARRAY CONTAINING HOLLERITH DATA, THIS ROUTINE    *   
C     *   EXTRACTS THE CHARACTER SPECIFIED BY -IPTR- AND RETURNS    *   
C     *   ITS ASCII CODE.                                           *   
C     *                                                             *   
C     ***************************************************************   
C                                                                       
C                                                                       
C-------------------------------------------------------------          
C                                                                       
C     ILEFT  -- THE BYTE FROM WHICH THE LEFTMOST CHARACTER OF           
C                 AN INTEGER IS EXTRACTED                               
C                                                                       
C     IBIAS  -- NUMBER OF UNUSED BITS AT LOW-ORDER END OF INTEGER       
C                                                                       
      DIMENSION ISOURC(ISSIZE)                                          
C                                                                       
      COMMON/ZZCOM/MZBITS,MZCHAR,MZWORD,MZJUST,MZCCSW,MZCCTL,MZDCCS,    
     *MZDCCT,MZHISW,MZLUSW,MZLURD,MZLUWR,                               
     *MZTAD(2),MZOTC(2),MZETC(2),MZOT1(2),MZOT2(2),                     
     *MZHM(2),MZBS(2),MZHEC(2),MZHS1(2),MZHS2(2),MZICD(2),MZIRS1(2),    
     *MZIRS2(2),MZMBS(2),MZSCOC(2)                                      
C                                                                       
      MCJUST=MZJUST                                                     
      IF (MCJUST.NE.1) MCJUST=-1                                        
      ILEFT=1                                                           
      IBIAS=MZWORD-MZBITS*MZCHAR                                        
      IF (MCJUST.EQ.1) GO TO 100                                        
          ILEFT=MZCHAR                                                  
          IBIAS=0                                                       
  100 CONTINUE                                                          
      MAGIC=2**(MZWORD-2)                                               
C                                                                       
      IWORD=(IPTR-1)/MZCHAR+1                                           
      ICHAR=MOD(IPTR-1,MZCHAR)+1                                        
C                                                                       
      ICHAR=ILEFT+MCJUST*(ICHAR-1)                                      
      ISHIFT=IBIAS+MZBITS*(MZCHAR-ICHAR)                                
      IDEST=ISOURC(IWORD)/2**ISHIFT                                     
      IF ((ISOURC(IWORD) .LT. 0).AND.(MOD(ISOURC(IWORD),                
     1     2**ISHIFT) .NE. 0)) IDEST=IDEST-1                            
      IF (IDEST.LT.0) IDEST=(IDEST+MAGIC)+MAGIC                         
      IDEST=MOD(IDEST,128)                                              
      RETURN                                                            
      END                                                               
      SUBROUTINE ZZGET(LU,IARRY,ISIZE,IEND)                             
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C                                                                       
C                                                                       
C     GETS A RECORD FROM THE PLOTTER AND CONVERTS FROM                  
C         HOLLERITH TO ASCII CODES                                      
C                                                                       
C     *****   THIS ROUTINE IS MACHINE-DEPENDENT   *****                 
C                                                                       
C     LU     -- UNIT NUMBER                                             
C                                                                       
C     IARRY  -- BUFFER                                                  
C                                                                       
C     IEND   -- LOCATION OF THE END-OF-STRING FLAG                      
C                                                                       
C     ISIZE  -- SIZE OF -IARRY-.  (2.LE.ISIZE .AND. ISIZE.LE.73)        
C                                                                       
C     ***************************************************************   
C     *                                                             *   
C     *   THIS ROUTINE READS A RECORD FROM THE PLOTTER INTO THE     *   
C     *   BUFFER PROVIDED BY THE CALLING PROGRAM, ONE CHARACTER     *   
C     *   PER ARRAY ELEMENT.  EACH CHARACTER IS THEN CONVERTED TO   *   
C     *   ITS EQUIVALENT ASCII CODE.  AN END-OF-STRING FLAG (-1)    *   
C     *   IS STORED IMMEDIATELY AFTER THE RIGHTMOST CHARACTER.      *   
C     *   THE LOCATION OF THIS FLAG IS ALSO RETURNED.               *   
C     *                                                             *   
C     *   WHEN THIS ROUTINE EXITS, EACH ELEMENT OF -IARRY- (UP TO   *   
C     *   THE END-OF-STRING) CONTAINS THE ASCII CODE FOR ONE        *   
C     *   CHARACTER.                                                *   
C     *                                                             *   
C     ***************************************************************   
C                                                                       
      DIMENSION IARRY(ISIZE)                                            
C                                                                       
      COMMON/ZZCOM/MZBITS,MZCHAR,MZWORD,MZJUST,MZCCSW,MZCCTL,MZDCCS,    
     *MZDCCT,MZHISW,MZLUSW,MZLURD,MZLUWR,                               
     *MZTAD(2),MZOTC(2),MZETC(2),MZOT1(2),MZOT2(2),                     
     *MZHM(2),MZBS(2),MZHEC(2),MZHS1(2),MZHS2(2),MZICD(2),MZIRS1(2),    
     *MZIRS2(2),MZMBS(2),MZSCOC(2)                                      
C                                                                       
      DIMENSION ISPACE(1)                                               
C                                                                       
 9001 FORMAT (72A1)                                                     
      IFLAG=0                                                           
      IF (IEND.LT.0) IFLAG=1                                            
      KSIZE= IABS(ISIZE)                                                
C                                                                       
C     FILL BUFFER WITH SPACES                                           
C                                                                       
      ISPACE(1)=32                                                      
      DO 100 I=1,KSIZE                                                  
          CALL ZZPACK(ISPACE,IARRY(I),1)                                
  100 CONTINUE                                                          
C                                                                       
      IUNIT=LU                                                          
      IF (MZLUSW.EQ.2) IUNIT=IUNIT+MZLURD                               
      IWORK=KSIZE-1                                                     
      READ (IUNIT,9001) (IARRY(I),I=1,IWORK)                            
C                                                                       
C     CONVERT TO ASCII CODES                                            
C                                                                       
      I=0                                                               
  200 I=I+1                                                             
          CALL ZZEXTR(IARRY(I),1,1,IWORK)                               
          IARRY(I)=IWORK                                                
          IF (IWORK.GE.33 .AND. IWORK.LE.126) GO TO 200                 
          IF (IFLAG.EQ.1.AND.I.LT.KSIZE) GO TO 200                      
      IARRY(I)=-1                                                       
      IEND=I                                                            
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE ZZHOST(IFUNC,IP,IPSIZ,JP1,JP1SIZ,JP2,JP2SIZ)           
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C                                                                       
C                                                                       
C     STORE OR RETRIEVE CONFIGURATION PARAMETERS                        
C                                                                       
C     IFUNC  = FUNCTION DESIRED                                         
C                                                                       
C                 1 = PUT VALUES INTO COMMON                            
C                                                                       
C                 2 = RETRIEVE VALUES FROM COMMON                       
C                                                                       
C     IP     = ARRAY CONTAINING GENERAL CONFIGURATION PARAMETERS        
C                                                                       
C     IPSIZ  = SIZE OF -IP-                                             
C                                                                       
C     JP1    = ARRAY OF PARAMETERS FOR PRIMARY HANDSHAKE                
C                                                                       
C     JP1SIZ = SIZE OF -JP1-                                            
C                                                                       
C     JP2    = ARRAY OF PARAMETERS FOR SECONDARY HANDSHAKE              
C                                                                       
C     JP2SIZ = SIZE OF -JP2-                                            
C                                                                       
C                                                                       
C     ***************************************************************   
C     *                                                             *   
C     *   THIS ROUTINE STORES THE LOCAL CONFIGURATION PARAMETERS    *   
C     *   INTO COMMON BLOCK -ZZCOM-.  PROVISION IS ALSO MADE FOR    *   
C     *   RETRIEVING DATA FROM -ZZCOM-.  (THIS IS A DIAGNOSTIC      *   
C     *   AID.)  ARRAY SIZES ARE VARIABLE IN ORDER TO PROMOTE       *   
C     *   COMPATIBILITY WITH FUTURE RELEASES.                       *   
C     *                                                             *   
C     *   NOTE THAT THE CONFIGURATION PARAMETERS ARE ACTUALLY       *   
C     *   SPECIFIED IN SUBROUTINE -ZZINIT-.                         *   
C     *                                                             *   
C     ***************************************************************   
C                                                                       
C                                                                       
C--------------------------------------------------------------------   
C                                                                       
C                                                                       
C     PORTABILITY CONSIDERATIONS FOR THIS SOFTWARE PACKAGE.             
C                                                                       
C     SOME ALGORITHMS IN THIS PACKAGE ARE MACHINE-DEPENDENT BECAUSE     
C     THEY PERFORM FUNCTIONS WHICH CANNOT BE DONE IN A STANDARD         
C     FASHION WITHIN THE FRAMEWORK OF ANSI FORTRAN.  SUCH CODE HAS      
C     BEEN CONCENTRATED INTO A SET OF MACHINE-DEPENDENT SUBPROGRAMS,    
C     WHICH HAVE NAMES BEGINNING WITH -ZZ-.                             
C                                                                       
C     IF CERTAIN ASSUMPTIONS CONCERNING THE HOST PROCESSOR ARE          
C     VALID (SEE BELOW), THE MACHINE-DEPENDENT ROUTINES WILL FUNCTION   
C     PROPERLY, AND THE ONLY CHANGES NEEDED WHEN THE PACKAGE IS         
C     INSTALLED WILL BE IN SUBROUTINE -ZZINIT-.  THE FUNCTION OF        
C     -ZZINIT- IS TO SPECIFY VARIOUS CONFIGURATION PARAMETERS WHICH     
C     DESCRIBE THE HOST PROCESSOR AND THE HANDSHAKE PROTOCOLS FOR       
C     COMMUNICATING WITH THE GRAPHICS DEVICE.  THESE PARAMETERS ARE     
C     STORED IN COMMON BLOCK -ZZCOM-.                                   
C                                                                       
C     TWO HANDSHAKE PROTOCOLS CAN BE DEFINED.  THE PRIMARY PROTOCOL     
C     IS AN ENQUIRE-ACKNOWLEDGE HANDSHAKE MANAGED BY THIS SOFTWARE      
C     PACKAGE.  THE SECONDARY PROTOCOL IS TYPICALLY AN XON/XOFF         
C     HANDSHAKE MANAGED BY THE TERMINAL DRIVER.  BOTH PROTOCOLS         
C     MAY BE DEFINED IN -ZZINIT- AT INSTALLATION TIME.  THE             
C     SELECTION BETWEEN THE TWO CAN BE MADE BY THE USER PROGRAM         
C     AT RUN-TIME.                                                      
C                                                                       
C                                                                       
C--------------------------------------------------------------------   
C                                                                       
C                                                                       
C     THE FOLLOWING ASSUMPTIONS ARE MADE CONCERNING THE HOST            
C     PROCESSOR.                                                        
C                                                                       
C     A.  ASSUMPTIONS CONCERNING INTERNAL REPRESENTATION.               
C                                                                       
C         1.  CHARACTERS ARE REPRESENTED INTERNALLY IN ASCII.           
C                                                                       
C         2.  THE NUMBER OF BITS PER CHARACTER IS A CONSTANT            
C             (MZBITS).                                                 
C                                                                       
C         3.  MZBITS.GE.7                                               
C                                                                       
C         4.  THE NUMBER OF CHARACTERS PER INTEGER IS A CONSTANT        
C             (MZCHAR).                                                 
C                                                                       
C         5.  MZCHAR.GE.1                                               
C                                                                       
C         6.  THE NUMBER OF BITS PER INTEGER IS A CONSTANT              
C             (MZWORD).                                                 
C                                                                       
C         7.  MZWORD.GE.16                                              
C                                                                       
C         8.  (MZBITS*MZCHAR).LE.MZWORD                                 
C                                                                       
C         9.  INTEGERS ARE REPRESENTED INTERNALLY IN TWOS-COMPLEMENT    
C             FORM.                                                     
C                                                                       
C        10.  IN HOLLERITH FORMAT, ONE OR MORE CHARACTERS ARE STORED    
C             IN EACH INTEGER, STARTING WITH EITHER THE HIGH-ORDER      
C             OR THE LOW-ORDER POSITION, AS SPECIFIED BY THE VALUE      
C             OF -MZJUST-.                                              
C                                                                       
C     B.  ASSUMPTIONS CONCERNING INPUT/OUTPUT.                          
C                                                                       
C         1.  THE TERMINAL DRIVER WILL NOT ADD ANY CHARACTERS TO        
C             THE STRING BEING SENT TO THE PLOTTER OTHER THAN THE       
C             FOLLOWING.                                                
C                                                                       
C             A.  A CARRIAGE RETURN AT THE BEGINNING OR END OF THE      
C                 STRING.                                               
C                                                                       
C             B.  ONE OR MORE NUL CHARACTERS ANYWHERE.                  
C                                                                       
C         2.  THE TERMINAL DRIVER WILL NOT DELETE OR CHANGE ANY         
C             CHARACTERS IN THE STRING BEING WRITTEN TO THE PLOTTER.    
C             NOTE--THE -ZZPUT- ROUTINE CAN SET THE HIGH-ORDER BIT      
C             OF ALL ASCII CONTROL CHARACTERS BEING SENT IN AN          
C             ATTEMPT TO GUARANTEE THIS.  SEE DESCRIPTION OF            
C             VARIABLE -MZHISW-.                                        
C                                                                       
C         3.  WHEN DATA IS BEING READ FROM THE PLOTTER, THE TERMINAL    
C             DRIVER WILL NOT ADD, DELETE, OR CHANGE ANY CHARACTERS.    
C                                                                       
C         4.  WHEN THE STRING BEING READ IS SHORTER THAN THE FORMAT     
C             SPECIFIED (72A1), THE REMAINDER OF THE INPUT BUFFER       
C             WILL BE FILLED WITH SPACES OR WILL BE LEFT UNCHANGED.     
C                                                                       
C     THESE ASSUMPTIONS ARE VALID FOR A WIDE VARIETY OF PROCESSORS.     
C     FOR THESE SYSTEMS, THE ONLY CHANGE NEEDED TO THIS PACKAGE         
C     SHOULD BE TO SUPPLY THE APPROPRIATE CONFIGURATION PARAMETERS IN   
C     SUBROUTINE -ZZINIT-.                                              
C                                                                       
C     IF ANY OF THE ASSUMPTIONS ABOVE ARE NOT VALID, ONE OR MORE        
C     OF THE MACHINE-DEPENDENT ROUTINES MAY HAVE TO BE MODIFIED.        
C     ALL MACHINE-DEPENDENT ROUTINES HAVE NAMES BEGINNING WITH -ZZ-.    
C                                                                       
C                                                                       
C--------------------------------------------------------------------   
C                                                                       
C                                                                       
      DIMENSION IP(IPSIZ),JP1(JP1SIZ),JP2(JP2SIZ)                       
C                                                                       
      COMMON/ZZCOM/MZBITS,MZCHAR,MZWORD,MZJUST,MZCCSW,MZCCTL,MZDCCS,    
     *MZDCCT,MZHISW,MZLUSW,MZLURD,MZLUWR,                               
     *MZTAD(2),MZOTC(2),MZETC(2),MZOT1(2),MZOT2(2),                     
     *MZHM(2),MZBS(2),MZHEC(2),MZHS1(2),MZHS2(2),MZICD(2),MZIRS1(2),    
     *MZIRS2(2),MZMBS(2),MZSCOC(2)                                      
C                                                                       
C                                                                       
C     MZBITS = NUMBER OF BITS PER CHARACTER                             
C                                                                       
C     MZCHAR = NUMBER OF CHARACTERS PER INTEGER                         
C                                                                       
C     MZWORD = NUMBER OF BITS PER INTEGER                               
C                                                                       
C     MZJUST = JUSTIFICATION OF HOLLERITH DATA                          
C                                                                       
C                 1 = LEFTMOST CHARACTER IS IN HIGH-ORDER (MOST         
C                     SIGNIFICANT) PORTION OF INTEGER                   
C                                                                       
C                 2 = LEFTMOST CHARACTER IS IN LOW-ORDER (LEAST         
C                     SIGNIFICANT) PORTION OF INTEGER                   
C                                                                       
C     MZCCSW = CARRIAGE CONTROL SWITCH                                  
C                                                                       
C                 1 = USE 1H+ AS CARRIAGE CONTROL CHARACTER FOR         
C                     EACH OUTPUT STRING WRITTEN TO THE PLOTTER         
C                                                                       
C                 2 = SEND  CARRIAGE CONTROL CHARACTER SPECIFIED BY     
C                     -MZCCTL-                                          
C                                                                       
C                 3 = DO NOT SEND A CARRIAGE CONTROL CHARACTER          
C                                                                       
C     MZCCTL = CARRIAGE CONTROL CHARACTER.  IF (MZCCSW.EQ.2), THIS      
C                 INTEGER WILL BE SENT AT THE BEGINNING OF EACH         
C                 OUTPUT STRING, USING A1 FORMAT.  NOT USED IF          
C                 (MZCCSW.NE.2).                                        
C                                                                       
C     MZDCCS = DEFERRED CARRAIGE CONTROL SWITCH                         
C                 SAME AS MZCCSW BUT USED WHEN IN DEFERRED PLOTTING     
C                                                                       
C     MZDCCT = DEFERRED CARRIAGE CONTROL CHARACTER.                     
C                 SAME AS MZCCTL BUT USED WHEN IN DEFERRED PLOTTING     
C                                                                       
C     MZHISW = SET-HIGH-ORDER-BIT SWITCH                                
C                                                                       
C                 1 = SET HIGH-ORDER BIT OF ALL CONTROL CHARACTERS      
C                     BEING SENT TO THE PLOTTER                         
C                                                                       
C                 2 = DONT SET HIGH-ORDER BIT                           
C                                                                       
C     MZLUSW = LOGICAL-UNIT-ADJUST SWITCH                               
C                                                                       
C                 1 = NORMAL (NO ADJUSTMENT PERFORMED)                  
C                                                                       
C                 2 = ADJUST UNIT NUMBER ON READS AND WRITES BY         
C                     ADDING THE VALUES IN -MZLURD- AND -MZLUWR-        
C                                                                       
C     MZLURD = VALUE TO BE ADDED TO UNIT NUMBER FOR ALL READS.          
C                 USED ONLY IF (MZLUSW.EQ.2).                           
C                                                                       
C     MZLUWR = VALUE TO BE ADDED TO UNIT NUMBER FOR ALL WRITES.         
C                 USED ONLY IF (MZLUSW.EQ.2).                           
C                                                                       
C     MZTAD  = TURN-AROUND DELAY, MILLISECONDS (0..9999)                
C                                                                       
C     MZOTC  = OUTPUT TRIGGER CHARACTER, ASCII EQUIVALENT (0..126)      
C                                                                       
C     MZETC  = ECHO-TERMINATE CHARACTER, ASCII EQUIVALENT (0..126)      
C                                                                       
C     MZOT1  = OUTPUT TERMINATOR, 1ST CHARACTER,                        
C                 ASCII EQUIVALENT (0..127)                             
C                                                                       
C     MZOT2  = OUTPUT TERMINATOR, 2ND CHARACTER,                        
C                 ASCII EQUIVALENT (0..127)                             
C                                                                       
C     MZHM   = HANDSHAKE MODE (1..2)                                    
C                                                                       
C     MZBS   = BLOCK SIZE, BYTES (0..9999)                              
C                                                                       
C     MZHEC  = HANDSHAKE ENABLE CHARACTER, ASCII EQUIVALENT (0..126)    
C                                                                       
C     MZHS1  = HANDSHAKE STRING, 1ST CHARACTER,                         
C                 ASCII EQUIVALENT (0..127)                             
C                                                                       
C     MZHS2  = HANDSHAKE STRING, 2ND CHARACTER,                         
C                 ASCII EQUIVALENT (0..127)                             
C                                                                       
C     MZICD  = INTER-CHARACTER DELAY, MILLISECONDS (0..9999)            
C                                                                       
C     MZIRS1 = IMMEDIATE RESPONSE STRING, 1ST CHARACTER,                
C                 ASCII EQUIVALENT (0..127)                             
C                                                                       
C     MZIRS2 = IMMEDIATE RESPONSE STRING, 2ND CHARACTER,                
C                 ASCII EQUIVALENT (0..127)                             
C                                                                       
C     MZMBS  = MAXIMUM BUFFER SIZE, BYTES (0..9999)                     
C                                                                       
C     MZSCOC = SET CONFIGURATION OPTIONS CHARACTER,                     
C                 ASCII EQUIVALENT (0..7)                               
C                                                                       
      DIMENSION IZ(12),JZ(2,15)                                         
C                                                                       
      EQUIVALENCE (IZ(1),MZBITS)                                        
      EQUIVALENCE (JZ(1,1),MZTAD(1))                                    
C                                                                       
      ITOP=MIN0(IPSIZ,12)                                               
      J1TOP=MIN0(JP1SIZ,15)                                             
      J2TOP=MIN0(JP2SIZ,15)                                             
C                                                                       
      GO TO (1000,2000),IFUNC                                           
 1000     CONTINUE                                                      
              DO 1090 N=1,ITOP                                          
                  IZ(N)=IP(N)                                           
 1090         CONTINUE                                                  
              DO 1190 N=1,J1TOP                                         
                  JZ(1,N)=JP1(N)                                        
 1190         CONTINUE                                                  
              DO 1290 N=1,J2TOP                                         
                  JZ(2,N)=JP2(N)                                        
 1290         CONTINUE                                                  
              GO TO 2999                                                
 2000     CONTINUE                                                      
              DO 2090 N=1,ITOP                                          
                  IP(N)=IZ(N)                                           
 2090         CONTINUE                                                  
              DO 2190 N=1,J1TOP                                         
                  JP1(N)=JZ(1,N)                                        
 2190         CONTINUE                                                  
              DO 2290 N=1,J2TOP                                         
                  JP2(N)=JZ(2,N)                                        
 2290         CONTINUE                                                  
 2999 CONTINUE                                                          
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE ZZINIT                                                 
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C                                                                       
C                                                                       
C     INITIALIZE CONFIGURATION PARAMETERS                               
C                                                                       
C     ***************************************************************   
C     *                                                             *   
C     *   THIS SUBROUTINE SPECIFIES THE LOCAL CONFIGURATION         *   
C     *   PARAMETERS USED BY OTHER ROUTINES IN THE PACKAGE.  FOR    *   
C     *   MANY PROCESSORS, THIS IS THE ONLY SUBPROGRAM WHICH WILL   *   
C     *   REQUIRE CHANGES.                                          *   
C     *                                                             *   
C     *   SEE SUBROUTINE -ZZHOST- FOR RELATED INFORMATION.  SEE     *   
C     *   ALSO THE OPERATING AND PROGRAMMING MANUAL FOR THE         *   
C     *   GRAPHICS DEVICE BEING USED.                               *   
C     *                                                             *   
C     ***************************************************************   
C                                                                       
      DIMENSION L(14),L1(15),L2(15)                                     
C                                                                       
C     THE FOLLOWING EQUIVALENCE STATEMENTS PROVIDE MNEMONIC NAMES       
C     FOR THE CONFIGURATION PARAMETERS                                  
C                                                                       
      EQUIVALENCE (L(1),LBITS)                                          
      EQUIVALENCE (L(2),LCHAR)                                          
      EQUIVALENCE (L(3),LWORD)                                          
      EQUIVALENCE (L(4),LJUST)                                          
      EQUIVALENCE (L(5),LCCSW)                                          
      EQUIVALENCE (L(6),LCCTL)                                          
      EQUIVALENCE (L(7),LDCCS)                                          
      EQUIVALENCE (L(8),LDCCT)                                          
      EQUIVALENCE (L(9),LHISW)                                          
      EQUIVALENCE (L(10),LLUSW)                                         
      EQUIVALENCE (L(11),LLURD)                                         
      EQUIVALENCE (L(12),LLUWR)                                         
C                                                                       
      EQUIVALENCE (L1(1),L1TAD)                                         
      EQUIVALENCE (L1(2),L1OTC)                                         
      EQUIVALENCE (L1(3),L1ETC)                                         
      EQUIVALENCE (L1(4),L1OT1)                                         
      EQUIVALENCE (L1(5),L1OT2)                                         
      EQUIVALENCE (L1(6),L1HM)                                          
      EQUIVALENCE (L1(7),L1BS)                                          
      EQUIVALENCE (L1(8),L1HEC)                                         
      EQUIVALENCE (L1(9),L1HS1)                                         
      EQUIVALENCE (L1(10),L1HS2)                                        
      EQUIVALENCE (L1(11),L1ICD)                                        
      EQUIVALENCE (L1(12),L1IRS1)                                       
      EQUIVALENCE (L1(13),L1IRS2)                                       
      EQUIVALENCE (L1(14),L1MBS)                                        
      EQUIVALENCE (L1(15),L1SCOC)                                       
C                                                                       
      EQUIVALENCE (L2(1),L2TAD)                                         
      EQUIVALENCE (L2(2),L2OTC)                                         
      EQUIVALENCE (L2(3),L2ETC)                                         
      EQUIVALENCE (L2(4),L2OT1)                                         
      EQUIVALENCE (L2(5),L2OT2)                                         
      EQUIVALENCE (L2(6),L2HM)                                          
      EQUIVALENCE (L2(7),L2BS)                                          
      EQUIVALENCE (L2(8),L2HEC)                                         
      EQUIVALENCE (L2(9),L2HS1)                                         
      EQUIVALENCE (L2(10),L2HS2)                                        
      EQUIVALENCE (L2(11),L2ICD)                                        
      EQUIVALENCE (L2(12),L2IRS1)                                       
      EQUIVALENCE (L2(13),L2IRS2)                                       
      EQUIVALENCE (L2(14),L2MBS)                                        
      EQUIVALENCE (L2(15),L2SCOC)                                       
C                                                                       
C     CONFIGURATION PARAMETERS CORRESPOND TO THE ENTITIES IN            
C     COMMON BLOCK -ZZCOM-.  SEE SUBROUTINE -ZZHOST- FOR MORE           
C     DETAIL.                                                           
C                                                                       
C         LBITS  = NUMBER OF BITS PER CHARACTER                         
C         LCHAR  = NUMBER OF CHARACTERS PER INTEGER                     
C         LWORD  = NUMBER OF BITS PER INTEGER                           
C         LJUST  = JUSTIFICATION OF HOLLERITH DATA                      
C         LCCSW  = CARRIAGE CONTROL SWITCH                              
C         LCCTL  = CARRIAGE CONTROL CHARACTER                           
C         LDCCS  = DEFERRED CARRIAGE CONTROL SWITCH                     
C         LDCCT  = DEFERRED CARRIAGE CONTROL CHARACTER                  
C         LHISW  = SET-HIGH-ORDER-BIT SWITCH                            
C         LLUSW  = LOGICAL-UNIT-ADJUST SWITCH                           
C         LLURD  = VALUE TO BE ADDED TO UNIT NUMBER FOR ALL READS       
C         LLUWR  = VALUE TO BE ADDED TO UNIT NUMBER FOR ALL WRITES      
C                                                                       
C         L1.... = CONFIGURATION PARAMETERS FOR PRIMARY HANDSHAKE       
C         L2.... = CONFIGURATION PARAMETERS FOR SECONDARY HANDSHAKE     
C                                                                       
C         ..TAD  = TURN-AROUND DELAY                                    
C         ..OTC  = OUTPUT TRIGGER CHARACTER                             
C         ..ETC  = ECHO-TERMINATE CHARACTER                             
C         ..OT1  = OUTPUT TERMINATOR, 1ST CHARACTER                     
C         ..OT2  = OUTPUT TERMINATOR, 2ND CHARACTER                     
C         ..HM   = HANDSHAKE MODE                                       
C         ..BS   = BLOCK SIZE                                           
C         ..HEC  = HANDSHAKE ENABLE CHARACTER                           
C         ..HS1  = HANDSHAKE STRING, 1ST CHARACTER                      
C         ..HS2  = HANDSHAKE STRING, 2ND CHARACTER                      
C         ..ICD  = INTER-CHARACTER DELAY                                
C         ..IRS1 = IMMEDIATE RESPONSE STRING, 1ST CHARACTER             
C         ..IRS2 = IMMEDIATE RESPONSE STRING, 2ND CHARACTER             
C         ..MBS  = MAXIMUM BUFFER SIZE                                  
C         ..SCOC = SET CONFIGURATION OPTIONS CHARACTER                  
C                                                                       
C     ***************************************************************   
C     *                                                             *   
C     *   INSTALLATION NOTE.  THE PERSON INSTALLING THIS PACKAGE    *   
C     *   MUST PROVIDE THE APPROPRIATE VALUES IN THE STATEMENTS     *   
C     *   BELOW.                                                    *   
C     *                                                             *   
C     ***************************************************************   
C                                                                       
      LBITS=8                                                           
      LCHAR=4                                                           
      LWORD=32                                                          
      LJUST=2                                                           
      LCCSW=1                                                           
      LCCTL=0                                                           
      LDCCS=1                                                           
      LDCCT=0                                                           
      LHISW=2                                                           
      LLUSW=1                                                           
      LLURD=0                                                           
      LLUWR=0                                                           
C                                                                       
      L1TAD=50                                                           
      L1OTC=10                                                          
      L1ETC=10                                                          
      L1OT1=13                                                          
      L1OT2=0                                                           
      L1HM=2                                                            
      L1BS=80                                                           
      L1HEC=0                                                           
      L1HS1=17                                                          
      L1HS2=0                                                           
      L1ICD=0                                                           
      L1IRS1=19                                                         
      L1IRS2=0                                                          
      L1MBS=9999                                                        
      L1SCOC=2                                                          
C                                                                       
      L2TAD=0                                                           
      L2OTC=17                                                          
      L2ETC=10                                                          
      L2OT1=13                                                          
      L2OT2=0                                                           
      L2HM=1                                                            
      L2BS=80                                                           
      L2HEC=20                                                          
      L2HS1=57                                                          
      L2HS2=0                                                           
      L2ICD=0                                                           
      L2IRS1=0                                                          
      L2IRS2=0                                                          
      L2MBS=9999                                                        
      L2SCOC=2                                                          
C                                                                       
      CALL ZZHOST(1,L,14,L1,15,L2,15)                                   
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE ZZLOW(ISOURC,IDEST)                                    
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C                                                                       
C                                                                       
C     RETURNS THE ASCII CODE OF THE CHARACTER STORED IN THE             
C     LOW-ORDER POSITION OF AN INTEGER.                                 
C                                                                       
C     *****   THIS ROUTINE IS MACHINE-DEPENDENT   *****                 
C                                                                       
C     ISOURC -- SOURCE INTEGER                                          
C                                                                       
C     IDEST  -- DESTINATION INTEGER                                     
C                                                                       
C                                                                       
C     ***************************************************************   
C     *                                                             *   
C     *   THE ASCII CODE OF THE CHARACTER CONTAINED IN THE LEAST    *   
C     *   SIGNIFICANT BYTE OF -ISOURC- IS RETURNED IN -IDEST-.      *   
C     *                                                             *   
C     ***************************************************************   
C                                                                       
C                                                                       
      COMMON/ZZCOM/MZBITS,MZCHAR,MZWORD,MZJUST,MZCCSW,MZCCTL,MZDCCS,    
     *MZDCCT,MZHISW,MZLUSW,MZLURD,MZLUWR,                               
     *MZTAD(2),MZOTC(2),MZETC(2),MZOT1(2),MZOT2(2),                     
     *MZHM(2),MZBS(2),MZHEC(2),MZHS1(2),MZHS2(2),MZICD(2),MZIRS1(2),    
     *MZIRS2(2),MZMBS(2),MZSCOC(2)                                      
C                                                                       
      MAGIC=2**(MZWORD-2)                                               
      IWORK=ISOURC                                                      
C                                                                       
C     CLEAR HIGH-ORDER BIT                                              
C                                                                       
      IF (IWORK.LT.0) IWORK=(IWORK+MAGIC)+MAGIC                         
C                                                                       
C     EXTRACT LOW-ORDER CHARACTER                                       
C                                                                       
      IDEST=MOD(IWORK,128)                                              
      RETURN                                                            
      END                                                               
      SUBROUTINE ZZPACK(ISOURC,IDEST,NCHARS)                            
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C                                                                       
C                                                                       
C     PACKS CHARACTERS--CONVERTS FROM ASCII CODES TO HOLLERITH DATA.    
C                                                                       
C     *****   THIS ROUTINE IS MACHINE-DEPENDENT   *****                 
C                                                                       
C     ISOURC -- SOURCE ARRAY.  EACH ELEMENT CONTAINS THE ASCII          
C                 CODE FOR ONE CHARACTER.                               
C                                                                       
C     IDEST  -- DESTINATION ARRAY, RETURNED TO CALLING PGM.             
C                 WILL CONTAIN THE HOLLERITH REPRESENTATION OF          
C                 THE CHARACTER STRING SPECIFIED BY ISOURC.             
C                                                                       
C     NCHARS -- NUMBER OF CHARACTERS IN THE STRING.                     
C                                                                       
C                                                                       
C     ***************************************************************   
C     *                                                             *   
C     *   THIS ROUTINE CONVERTS A STRING FROM ASCII CODES, STORED   *   
C     *   ONE CHARACTER PER ARRAY ELEMENT, TO THE APPROPRIATE       *   
C     *   REPRESENTATION FOR HOLLERITH DATA IN THE HOST PROCESSOR.  *   
C     *                                                             *   
C     ***************************************************************   
C                                                                       
C                                                                       
C-------------------------------------------------------------          
C                                                                       
C     IS     -- POINTS TO AN ELEMENT OF ISOURC                          
C                                                                       
C     ID     -- POINTS TO AN ELEMENT OF IDEST                           
C                                                                       
C     IDB    -- POINTS TO A BYTE WITHIN AN ELEMENT OF IDEST             
C                 (1 = MOST SIGNIFICANT BYTE)                           
C                                                                       
C     ILEFT  -- THE BYTE INTO WHICH THE LEFTMOST CHARACTER              
C                 WILL BE INSERTED                                      
C                                                                       
C     IBIAS  -- NUMBER OF UNUSED BITS AT LOW-ORDER END OF INTEGER       
C                                                                       
      DIMENSION ISOURC(NCHARS),IDEST(NCHARS)                            
C                                                                       
      COMMON/ZZCOM/MZBITS,MZCHAR,MZWORD,MZJUST,MZCCSW,MZCCTL,MZDCCS,    
     *MZDCCT,MZHISW,MZLUSW,MZLURD,MZLUWR,                               
     *MZTAD(2),MZOTC(2),MZETC(2),MZOT1(2),MZOT2(2),                     
     *MZHM(2),MZBS(2),MZHEC(2),MZHS1(2),MZHS2(2),MZICD(2),MZIRS1(2),    
     *MZIRS2(2),MZMBS(2),MZSCOC(2)                                      
C                                                                       
      MCJUST=MZJUST                                                     
      IF (MCJUST.NE.1) MCJUST=-1                                        
C                                                                       
      ILEFT=1                                                           
      IBIAS=MZWORD-MZBITS*MZCHAR                                        
      IF (MCJUST.EQ.1) GO TO 10                                         
          ILEFT=MZCHAR                                                  
          IBIAS=0                                                       
   10 CONTINUE                                                          
      MAGIC=2**(MZWORD-2)                                               
C                                                                       
      IS=0                                                              
      ID=0                                                              
      IDB=-1                                                            
   30 IF (IS.GE.NCHARS) GO TO 100                                       
          IS=IS+1                                                       
          IDB=IDB+MCJUST                                                
          IF (IDB.GE.1 .AND. IDB.LE.MZCHAR) GO TO 50                    
              IDB=ILEFT                                                 
              ID=ID+1                                                   
              IDEST(ID)=0                                               
   50     CONTINUE                                                      
          IASCII=MOD(IABS(ISOURC(IS)),128)                              
          ISHIFT=MZBITS*(MZCHAR-IDB)+IBIAS                              
          IPOWER=2**ISHIFT                                              
          IF (IASCII.GE.64 .AND. 7+ISHIFT.EQ.MZWORD) GO TO 60           
              IDEST(ID)=IDEST(ID)+IPOWER*IASCII                         
              GO TO 70                                                  
   60     CONTINUE                                                      
C                                                                       
C             HIGH-ORDER BIT MUST BE SET WITHOUT CAUSING INTEGER        
C             OVERFLOW                                                  
C                                                                       
              IDEST(ID)=IDEST(ID)+IPOWER*(IASCII-64)                    
              IDEST(ID)=(IDEST(ID)-MAGIC)-MAGIC                         
   70     CONTINUE                                                      
          GO TO 30                                                      
  100 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE ZZPUT(IDEVOT,IARAY,NCHARS)                             
C ****************************************************************      
C *                                                              *      
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1981.  ALL RIGHTS      *      
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *      
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *      
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *      
C *                                                              *      
C ****************************************************************      
C                                                                       
C                                                                       
C                                                                       
C     WRITES ASCII CHARACTERS TO OUTPUT DEVICE                          
C                                                                       
C     *****   THIS ROUTINE IS MACHINE-DEPENDENT   *****                 
C                                                                       
C     IDEVOT -- OUTPUT DEVICE                                           
C                                                                       
C     IARAY  -- ARRAY CONTAINING ASCII CODES FOR THE CHARACTERS         
C                 TO BE WRITTEN.  EACH ELEMENT CONTAINS THE ASCII       
C                 CODE FOR ONE CHARACTER.                               
C                                                                       
C     NCHARS -- NUMBER OF CHARACTERS IN THE STRING.  (NOT .GT. 71)      
C                                                                       
C                                                                       
C     ***************************************************************   
C     *                                                             *   
C     *   THIS ROUTINE WRITES A RECORD TO THE PLOTTER.  THE CALLING *   
C     *   PROGRAM PROVIDES DATA TO THIS ROUTINE IN THE FORM OF AN   *   
C     *   ARRAY OF ASCII CODES.                                     *   
C     *                                                             *   
C     ***************************************************************   
C                                                                       
C                                                                       
      DIMENSION IARAY(NCHARS)                                           
C                                                                       
      COMMON/ZZCOM/MZBITS,MZCHAR,MZWORD,MZJUST,MZCCSW,MZCCTL,MZDCCS,    
     *MZDCCT,MZHISW,MZLUSW,MZLURD,MZLUWR,                               
     *MZTAD(2),MZOTC(2),MZETC(2),MZOT1(2),MZOT2(2),                     
     *MZHM(2),MZBS(2),MZHEC(2),MZHS1(2),MZHS2(2),MZICD(2),MZIRS1(2),    
     *MZIRS2(2),MZMBS(2),MZSCOC(2)                                      
C                                                                       
      DIMENSION IBUF(72)                                                
      DIMENSION IWORK(1)                                                
C                                                                       
      LOGICAL LFAKE                                                     
C                                                                       
C                                                                       
 9001 FORMAT (1H+,72A1)                                                 
 9002 FORMAT (A1,72A1)                                                  
 9003 FORMAT (72A1)                                                     
C                                                                       
C                                                                       
      MAGIC=2**(MZWORD-2)                                               
C                                                                       
      DO 30 I=1,NCHARS                                                  
          IBUF(I)=MOD(IABS(IARAY(I)),128)                               
   30 CONTINUE                                                          
C                                                                       
      ITOP=NCHARS                                                       
      IF (IBUF(ITOP).NE.32) GO TO 40                                    
C                                                                       
C         THE RIGHTMOST CHARACTER TO BE TRANSMITTED IS A SPACE.         
C         SOME FILE SYSTEMS, SPOOLERS, OR DRIVERS MAY DISCARD           
C         TRAILING SPACES.  SPACES, HOWEVER, ARE SIGNIFICANT IN         
C         THE BINARY FORMAT USED TO COMMAND THE PLOTTER.  A NUL         
C         IS THEREFORE APPENDED TO THE OUTPUT STRING TO PREVENT         
C         TRUNCATION.                                                   
C                                                                       
          ITOP=ITOP+1                                                   
          IBUF(ITOP)=0                                                  
   40 CONTINUE                                                          
C                                                                       
      DO 100 I=1,ITOP                                                   
          IWORK(1)=IBUF(I)                                              
C                                                                       
C         CHECK FOR CONTROL CHARACTERS WHICH MIGHT HAVE                 
C         SPECIAL SIGNIFICANCE TO THE TERMINAL DRIVER ON                
C         THE HOST SYSTEM.  DRIVER MAY BE FAKED OUT BY                  
C         SETTING THE HIGH-ORDER BIT LATER IN THIS ROUTINE.             
C                                                                       
          LFAKE=MZHISW.EQ.1 .AND. MZBITS.GE.8                           
     *            .AND. (IWORK(1).LT.32 .OR. IWORK(1).EQ.127)           
C                                                                       
          CALL ZZPACK(IWORK,IBUF(I),1)                                  
          IF (.NOT.LFAKE) GO TO 80                                      
C                                                                       
C             SET EIGHTH BIT                                            
C                                                                       
              IF (MZJUST.EQ.1) GO TO 50                                 
C                                                                       
C                 CHARACTER IS IN LOW-ORDER POSITION                    
C                                                                       
                  IBUF(I)=IBUF(I)+128                                   
                  GO TO 70                                              
   50         CONTINUE                                                  
C                                                                       
C                 CHARACTER IS IN HIGH-ORDER POSITION                   
C                                                                       
                  IF (MZBITS.EQ.8) IBUF(I)=(IBUF(I)-MAGIC)-MAGIC        
                  IF (MZBITS.NE.8) IBUF(I)=                             
     *                    IBUF(I)+2**(MZWORD-MZBITS+7)                  
   70         CONTINUE                                                  
   80     CONTINUE                                                      
  100 CONTINUE                                                          
      IUNIT=IDEVOT                                                      
      IF (MZLUSW.EQ.2) IUNIT=IUNIT+MZLUWR                               
C                                                                       
      GO TO (210,220,230),MZCCSW                                        
  210     CONTINUE                                                      
              WRITE (IUNIT,9001) (IBUF(I),I=1,ITOP)                     
              GO TO 290                                                 
  220     CONTINUE                                                      
              WRITE (IUNIT,9002) MZCCTL,(IBUF(I),I=1,ITOP)              
              GO TO 290                                                 
  230     CONTINUE                                                      
              WRITE (IUNIT,9003) (IBUF(I),I=1,ITOP)                     
  290 CONTINUE                                                          
C                                                                       
      RETURN                                                            
      END                                                               
