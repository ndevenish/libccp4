C
C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
      SUBROUTINE HSymTrn(Nsym,Rsm,Symchs)
C     ==========================================
C
C---- This translates the Symmetry matrices into INT TAB
C     character strings for each symmetry operation and prints the real
C     and reciprocal space operators on standard output.
C
C     It gives the real and reciprocal space operations.
C   eg     X,Y,Z    H,K,L
C   eg     -Y,X-Y, Z   -H-K, H, L  etc
C   That is more complicated than you might think!!
C
C---- Inverse symmetry needed to test systematic absences -
C     copy Rsmm Rsmtt this common block.
C
C      COMMON /SYSabs/ NsymT,RsmM(4,4,MaxSym),RsmTT(4,4,MaxSym)
C
C
          implicit none
C
C     .. Parameters ..
      INTEGER MaxSymmetry
      PARAMETER (MaxSymmetry=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER Nsym
C     ..
C     .. Array Arguments ..
      REAL Rsm(4,4,*)
      CHARACTER Symchs(MaxSymmetry)*80
C     ..
C     .. Local Scalars ..
      INTEGER I,ICH,IST,J,NS,TempSym
C     ..
C     .. Local Arrays ..
      REAL RsmT(4,4,MaxSymmetry)
      CHARACTER HKLCR(3)*1
C     ..
C     .. External Subroutines ..
      EXTERNAL HTrnSym
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC LEN
C     ..
C     .. Data statements ..
C
      DATA HKLCR/'H','K','L'/
C     ..
C
C
      TempSym = Nsym
      CALL HTrnSym(Nsym,Rsm,Symchs)
C
C
      DO 30 NS = 1,Nsym
C
C---- H K L   - get inverse symmetry operation
C
        CALL INVSYM(Rsm(1,1,NS),RsmT(1,1,NS))
C
C
        ICH = 40
C
C
        DO 20 J = 1,3
          IST = 0
          DO 10 I = 1,3
            IF (RsmT(I,J,NS) .ne. 0) THEN
              IF (RsmT(I,J,NS) .gt. 0  .and.  IST .gt. 0) THEN
                IF (ICH .gt. LEN(Symchs(1))) THEN
                  write (6,fmt=6000)
 6000             format ('SYMTR: character array too short')
                  STOP
                END IF
                Symchs(NS) (ICH:ICH) = '+'
                ICH = ICH + 1
              END IF
C
C
              IF (RsmT(I,J,NS) .lt. 0) THEN
                IF (ICH .gt. LEN(Symchs(1))) THEN
                  write (6,fmt=6002)
 6002             format ('SYMTR: character array too short')
                  STOP
                END IF
                Symchs(NS) (ICH:ICH) = '-'
                IST = 1
                ICH = ICH + 1
              END IF
C
C
              IF (ICH .gt. LEN(Symchs(1))) THEN
                write (6,fmt=6004)
 6004           format ('SYMTR: character array too short')
                STOP
              END IF
              Symchs(NS) (ICH:ICH) = HKLCR(I)
              ICH = ICH + 1
              IST = 1
            END IF
   10     CONTINUE
C
C---- ADD COMMA space
C
          IF (J .ne. 3) THEN
            IF (ICH+2 .gt. LEN(Symchs(1))) THEN
              write (6,fmt=6006)
 6006         format ('SYMTR: character array too short')
              STOP
            END IF
            Symchs(NS) (ICH:ICH+2) = ',  '
            ICH = ICH + 3
          END IF
   20   CONTINUE
   30 CONTINUE
C
C
      END
      SUBROUTINE HTrnSym(Nsym,Rsm,Symchs)
C     ========================================
C
C---- HTrnSym(Nsym,Rsm)
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
C---- Arguments :
C
C Nsym (I) INTEGER   Number of Symmetry operations
C
C Rsm  (I) REAL      Array of dimension (4,4,at least Nsym)
C                    coNTaining symmetry operations on input
C
C Symchs (O) CHARACTER*(*)   Array of dimension at least Nsym
C                            coNTaining int tab char strings on output
C
         implicit none
C
C     .. Parameters ..
      INTEGER MaxSymmetry
      PARAMETER (MaxSymmetry=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER Nsym
C     ..
C     .. Array Arguments ..
      REAL Rsm(4,4,*)
      CHARACTER Symchs(MaxSymmetry)*80
C     ..
C     .. Local Scalars ..
      REAL PPP,RRR
      INTEGER I1,I2,ICH,IST,ITR,Jdo10,Jdo20,Jdo40
C     ..
C     .. Local Arrays ..
      INTEGER NPNTR1(10),NPNTR2(10)
      CHARACTER AXISCR(3)*1,NUMB(9)*1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,INT,LEN,nint,REAL
C     ..
C     .. Data statements ..
C
      DATA AXISCR/'X','Y','Z'/
      DATA NUMB/'1','2','3','4','5','6','7','8','9'/
      DATA NPNTR1/0,1,1,1,0,1,0,2,3,5/
      DATA NPNTR2/0,6,4,3,0,2,0,3,4,6/
C     ..
C
      DO 40 Jdo40 = 1,Nsym
C
C---- Clear Symchs
C
        Symchs(Jdo40) = ' '
        ICH = 1
C
        DO 30 Jdo20 = 1,3
C
C---- Ist is flag for first character of operator
C
          IST = 0
C
          DO 20 Jdo10 = 1,4
            IF (Rsm(Jdo20,Jdo10,Jdo40) .ne. 0.0) THEN
              IF (Rsm(Jdo20,Jdo10,Jdo40) .gt. 0.0  .and.  
     +                IST .gt. 0) THEN
                IF (ICH .gt. LEN(Symchs(1))) THEN
                  write (6,fmt=6000)
 6000             format ('HTrnSym: character array too short')
                  STOP
                END IF
                Symchs(Jdo40) (ICH:ICH) = '+'
                ICH = ICH + 1
              END IF
C
              IF (Rsm(Jdo20,Jdo10,Jdo40) .lt. 0.0) THEN
                IF (ICH .gt. LEN(Symchs(1))) THEN
                  write (6,fmt=6002)
 6002             format ('HTrnSym: character array too short')
                  STOP
                END IF
                Symchs(Jdo40) (ICH:ICH) = '-'
                IST = 1
                ICH = ICH + 1
              END IF
C
              IF (Jdo10 .ne. 4) THEN
                IF (ICH .gt. LEN(Symchs(1))) THEN
                  write (6,fmt=6004)
 6004             format ('HTrnSym: character array too short')
                  STOP
                END IF
                Symchs(Jdo40) (ICH:ICH) = AXISCR(Jdo10)
                IST = 1
                ICH = ICH + 1
              END IF
C
C
              IF (Jdo10 .eq. 4  .and.  
     +            Rsm(Jdo20,4,Jdo40) .ne. 0) THEN
                ITR = abs(INT(Rsm(Jdo20,4,Jdo40)))
                RRR = REAL(ITR)
                IF (RRR .gt. abs(Rsm(Jdo20,4,Jdo40))-0.0001  .and. 
     +              RRR .lt. abs(Rsm(Jdo20,4,Jdo40))+0.0001) THEN
                  write (Symchs(Jdo40) (ICH:ICH+2),fmt=6006) ITR
 6006             format (i2,' ')
                  GO TO 10
                END IF
C
C
                RRR = abs(Rsm(Jdo20,4,Jdo40))
                PPP = RRR
                IF (RRR .gt. 1.0) THEN
                  PPP = RRR - REAL(ITR)
                  ITR = abs(INT(RRR))
                  write (Symchs(Jdo40) (ICH:ICH+2),fmt=6008) ITR
 6008             format (i2,' ')
                  ICH = ICH + 3
                END IF
C
C
                ITR = nint(abs(PPP)*12.0)
                I1 = NPNTR1(ITR)
                I2 = NPNTR2(ITR)
                IF (ICH+2 .gt. LEN(Symchs(1))) THEN
                  write (6,fmt=6010)
 6010             format ('HTrnSym: character array too short')
                  STOP
                END IF
                Symchs(Jdo40) (ICH:ICH+2) = NUMB(I1)//'/'//NUMB(I2)
   10           ICH = ICH + 3
              END IF
            END IF
   20     CONTINUE
C
C---- ADD COMMA  space
C
          IF (Jdo20 .ne. 3) THEN
            IF (ICH+2 .gt. LEN(Symchs(1))) THEN
              write (6,fmt=6012)
 6012         format ('HTrnSym: character array too short')
              STOP
            END IF
            Symchs(Jdo40) (ICH:ICH+2) = ',  '
            ICH = ICH + 3
          END IF
   30   CONTINUE
   40 CONTINUE
      END
C
C     =========================================================
      SUBROUTINE Hatom_type_scat(AtName,a1,a2,a3,a4,b1,b2,b3,b4,c)
C     =========================================================
C
C    _atom_type.symbol
C    _atom_type.scat_Cromer_Mann_a1
C    _atom_type.scat_Cromer_Mann_a2
C    _atom_type.scat_Cromer_Mann_a3
C    _atom_type.scat_Cromer_Mann_a4
C    _atom_type.scat_Cromer_Mann_b1
C    _atom_type.scat_Cromer_Mann_b2
C    _atom_type.scat_Cromer_Mann_b3
C    _atom_type.scat_Cromer_Mann_b4
C    _atom_type.scat_Cromer_Mann_c
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      CHARACTER AtName*(*)
      REAL a1,a2,a3,a4,b1,b2,b3,b4,c
C     ..
C     .. Local Scalars ..
      INTEGER Jdo
      CHARACTER IDwork*6
      LOGICAL First
C     ..
C     .. External Functions ..
      INTEGER lenstr
      EXTERNAL lenstr
C     ..
      DATA First /.true./
C
C
      IF ( .not. Harvest) RETURN
C
C    
        IF (First) THEN
          First = .false.
        CALL ccif_output_fmt('_atom_type.symbol',
     +                       '-',8,0,'z',ccifStatus)
        CALL ccif_output_fmt('_atom_type.scat_Cromer_Mann_a1',
     +                       ' ',9,4,'f',ccifStatus)
        CALL ccif_output_fmt('_atom_type.scat_Cromer_Mann_a2',
     +                       ' ',9,4,'f',ccifStatus)
        CALL ccif_output_fmt('_atom_type.scat_Cromer_Mann_a3',
     +                       ' ',9,4,'f',ccifStatus)
        CALL ccif_output_fmt('_atom_type.scat_Cromer_Mann_a4',
     +                       ' ',9,4,'f',ccifStatus)
        CALL ccif_output_fmt('_atom_type.scat_Cromer_Mann_b1',
     +                       ' ',9,4,'f',ccifStatus)
        CALL ccif_output_fmt('_atom_type.scat_Cromer_Mann_b2',
     +                       ' ',9,4,'f',ccifStatus)
        CALL ccif_output_fmt('_atom_type.scat_Cromer_Mann_b3',
     +                       ' ',9,4,'f',ccifStatus)
        CALL ccif_output_fmt('_atom_type.scat_Cromer_Mann_b4',
     +                       ' ',9,4,'f',ccifStatus)
        CALL ccif_output_fmt('_atom_type.scat_Cromer_Mann_c',
     +                       ' ',9,4,'f',ccifStatus)
      END IF
C
C

      CALL ccif_setup_context('_atom_type.symbol',CurrCategory,
     +                         ccifBlockID,ccifContext,
     +                         ccifStatus,'loop')
C
C
        ccifStatus = AppendRow
        CALL ccif_put_char('_atom_type.symbol',AtName,
     +                           ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real('_atom_type.scat_Cromer_Mann_a1',
     +                     a1,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real('_atom_type.scat_Cromer_Mann_a2',
     +                     a2,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real('_atom_type.scat_Cromer_Mann_a3',
     +                     a3,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real('_atom_type.scat_Cromer_Mann_a4',
     +                     a4,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real('_atom_type.scat_Cromer_Mann_b1',
     +                     b1,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real('_atom_type.scat_Cromer_Mann_b2',
     +                     b2,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real('_atom_type.scat_Cromer_Mann_b3',
     +                     b3,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real('_atom_type.scat_Cromer_Mann_b4',
     +                     b4,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real('_atom_type.scat_Cromer_Mann_c',
     +                     c,ccifContext,ccifStatus)
C
C
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
      SUBROUTINE HCell(Cell)
C     ============================
C
C
C
      include 'harvest.inc'
C
C
C     .. Array Arguments ..
      REAL Cell(6)
C     ..
C     .. External Subroutines ..
      EXTERNAL ccif_output_fmt,
     +          ccif_put_real,
     +           ccif_release_context,
     +            ccif_setup_context
C     ..
C
C
      IF ( .not. Harvest) RETURN
      IF (Package(1:4) .eq. 'CCP4') THEN
        Itest = Cell(1)
        IF (Itest .eq. IvalueNotDet ) RETURN
      END IF
C
C---- do _Cell
C
      CALL ccif_setup_context('cell',CurrCategory,ccifBlockID,
     +                        ccifContext,ccifStatus,' ')
C
C
      CALL ccif_output_fmt('_cell.length_a',' ',8,3,'f',ccifStatus)
      CALL ccif_output_fmt('_cell.length_b',' ',8,3,'f',ccifStatus)
      CALL ccif_output_fmt('_cell.length_c',' ',8,3,'f',ccifStatus)
      CALL ccif_output_fmt('_cell.angle_alpha',' ',8,3,'f',ccifStatus)
      CALL ccif_output_fmt('_cell.angle_beta',' ',8,3,'f',ccifStatus)
      CALL ccif_output_fmt('_cell.angle_gamma',' ',8,3,'f',ccifStatus)
C
C
      CALL ccif_put_real('_cell.length_a',Cell(1),ccifContext,
     +                   ccifStatus)
      CALL ccif_put_real('_cell.length_b',Cell(2),ccifContext,
     +                   ccifStatus)
      CALL ccif_put_real('_cell.length_c',Cell(3),ccifContext,
     +                   ccifStatus)
      CALL ccif_put_real('_cell.angle_alpha',Cell(4),ccifContext,
     +                   ccifStatus)
      CALL ccif_put_real('_cell.angle_beta',Cell(5),ccifContext,
     +                   ccifStatus)
      CALL ccif_put_real('_cell.angle_gamma',Cell(6),ccifContext,
     +                   ccifStatus)
C
C
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C     ====================================
      SUBROUTINE Hcell_percent_solvent(Fraction)
C     ====================================
C
C    _item_type.code               float
C  fraction of unit cell occupied by solvent 
C---- Now check if Wilson plot information been input. If not
C     guess the number of residues assuming 50% solvent in the cell
C     and assigning a average volume of 157A**3 to each residue.
C     Rough but not totally unreasonable - 
C           based on 5+1.2+1.5+8 atoms per residue;
C     Volume per atom ~ 10A**3
C
C
      include 'harvest.inc'
C
C     .. Scalar Arguments ..
      REAL Fraction
C     ..
C     .. External Subroutines ..
      EXTERNAL ccif_put_real,
     +          ccif_release_context,
     +           ccif_setup_context
C     ..
C
      IF ( .not. Harvest) RETURN
C
C
C
      CALL ccif_setup_context(
     +     '_exptl_crystal.percent_solvent',
     +                        CurrCategory,ccifBlockID,ccifContext,
     +                        ccifStatus,' ')
      CALL ccif_output_fmt(
     +     '_exptl_crystal.percent_solvent',
     +         ' ',8,1,'f',ccifStatus)
      ccifStatus = 1
      CALL ccif_put_real(
     +     '_exptl_crystal.percent_solvent',
     +                    Fraction,ccifContext,ccifStatus)
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
c     ============================
      subroutine hciftime(ciftime)
c     ============================
ccFrom GERARD@XRAY.BMC.UU.SE Thu Sep 24 00:25:25 1998
c
      implicit none
c
      character ciftime*(*)
c
      integer gmt_hour,gmt_minutes,localdaymonth,
     +        localhours,localminutes,localmonth,localseconds,
     +        localyear,nhours,nminutes,stime,diff
c
      character gmt_diff*1
c
      integer gmtarray(9),tarray(9)
      integer time
c
      intrinsic abs
c
code ...
c
c ... check if the argument can hold 25 characters
c     (better to return an error flag, of course ;-)
c
      if (len(ciftime) .lt. 25) then
        print *,'error --- hciftime: string too short'
        ciftime = char(0)
        return
      end if
c
      stime = time()
      call gmtime(stime,gmtarray)
      call ltime(stime,tarray)
c
      nminutes = gmtarray(2)
      nhours = gmtarray(3)
      localseconds = tarray(1)
      localminutes = tarray(2)
      localhours = tarray(3)
      localdaymonth = tarray(4)
      localmonth = tarray(5) + 1
      localyear = tarray(6) + 1900
c
c ... calculate time difference in minutes (some time zones
c     differ by N hours + 30 minutes from gmt)
c
      diff = (60*localhours + localminutes) -
     +       (60*nhours + nminutes)
c
c ... allow for different dates to avoid Kim's midnight bug
c     (fudge by simply checking if the day of the month is
c     identical or not; should be okay)
c
      if (diff .lt. 0 .and. tarray(4) .ne. gmtarray(4)) then
        diff = diff + 24*60
      else if (diff .gt. 0 .and. tarray(4) .ne. gmtarray(4)) then
        diff = diff - 24*60
      end if
c
c ... get hour differences by taking INT(minutes)/60
c     since INT(-1.5) would be -2, use ABS and adjust sign
c
      gmt_hour = abs(diff) / 60
      if (diff .lt. 0) gmt_hour = - gmt_hour
      gmt_minutes = diff - 60*gmt_hour
      if (gmt_hour .lt. 0 .or. gmt_minutes .lt. 0) then
        gmt_diff = '-'
      else
        gmt_diff = '+'
      end if
c
      write (ciftime,fmt=6000) localyear,localmonth,localdaymonth,
     +  localhours,localminutes,localseconds,gmt_diff,abs(gmt_hour),
     +  abs(gmt_minutes)
c
c ... NOTE: "i4" in the following format makes that this routine
c           is not Year-10,000-compliant !!!
c
 6000 format (i4,'-',i2.2,'-',i2.2,'T',i2.2,':',i2.2,':',i2.2,a1,i2.2,
     +       ':',i2.2)
c
      return
      end

C
C
C     =======================
      SUBROUTINE Hclose
C     =======================
C
      include 'harvest.inc'
ccx      include 'harderiv.inc'
      include 'harshell.inc'
C
C     .. External Subroutines ..
      EXTERNAL ccif_close_cif
C     ..
C
C
      IF ( .not. Harvest) RETURN
C
C
      IF (rom_context .ne. -1)
     +  CALL ccif_release_context(rom_context)
cc      IF (restraints_context .ne. -1)
cc     +  CALL ccif_release_context(restraints_context)
C
C
      IF (NRshells .gt. 0) THEN
      CALL ccif_setup_context('_reflns_shell.d_res_high',
     +                        CurrCategory,ccifBlockID,
     +                        ccifContext,ccifStatus,'loop')
C
C
      DO 10 Jdo=1,NRshells
      ccifStatus = AppendRow
      CALL ccif_put_real('_reflns_shell.d_res_low',
     +      Res(1,Jdo),ccifContext,ccifStatus)
C
C
      ccifStatus = KeepContext
      CALL ccif_put_real('_reflns_shell.d_res_high',
     +      Res(2,Jdo),ccifContext,ccifStatus)
C
C
      ccifStatus = KeepContext
      CALL ccif_put_int('_reflns_shell.number_measured_all',
     +                  Nreflns(1,Jdo),ccifContext,
     +                  ccifStatus)
C
C
      ccifStatus = KeepContext
      IF ( Nreflns(2,Jdo) .ne. IvalueNotDet)
     + CALL ccif_put_int('_reflns_shell.number_unique_all',
     +                  Nreflns(2,Jdo),ccifContext,
     +                  ccifStatus)
C
C
      ccifStatus = KeepContext
      IF ( Nreflns(3,Jdo) .lt. IValueNotDet - 1)
     + CALL ccif_put_int('_reflns_shell.number_centric_all',
     +                  Nreflns(3,Jdo),ccifContext,
     +                  ccifStatus)
C
C
      ccifStatus = KeepContext
      IF ( Nreflns(4,Jdo) .lt. IValueNotDet - 1 )
     + CALL ccif_put_int('_reflns_shell.number_anomalous_all',
     +                  Nreflns(4,Jdo),ccifContext,
     +                  ccifStatus)
C
C
      ccifStatus = KeepContext
      CALL ccif_put_real('_reflns_shell.Rmerge_I_all',
     +      Rs(1,Jdo),ccifContext,ccifStatus)
C
C
      ccifStatus = KeepContext
      IF (Rs(2,Jdo) .lt. ValueNotDet -1.0 .and. 
     +    Rs(2,Jdo) .gt. 0.01)
     + CALL ccif_put_real(
     +      '_reflns_shell.Rmerge_I_all_cumulative',
     +      Rs(2,Jdo),ccifContext,ccifStatus)
C
C
      ccifStatus = KeepContext
      IF (Rs(3,Jdo) .lt. ValueNotDet -1.0 .and. 
     +    Rs(3,Jdo) .gt. 0.01)
     + CALL ccif_put_real(
     +      '_reflns_shell.Rmerge_I_anomalous_all',
     +      Rs(3,Jdo),ccifContext,ccifStatus)
C
C
      ccifStatus = KeepContext
      IF (Rs(4,Jdo) .lt. ValueNotDet -1.0 .and. 
     +    Rs(4,Jdo) .gt. 0.01)
     + CALL ccif_put_real('_reflns_shell.PCV',
     +      Rs(4,Jdo),ccifContext,ccifStatus)
C
C
      ccifStatus = KeepContext
      IF (Rs(5,Jdo) .lt. ValueNotDet -1.0 .and. 
     +    Rs(5,Jdo) .gt. 0.01)
     + CALL ccif_put_real('_reflns_shell.PCV_mean',
     +      Rs(5,Jdo),ccifContext,ccifStatus)
C
C
      ccifStatus = KeepContext
      IF (Rs(6,Jdo) .lt. ValueNotDet -1.0 .and. 
     +    Rs(6,Jdo) .gt. 0.01)
     + CALL ccif_put_real('_reflns_shell.Rmeas',
     +      Rs(6,Jdo),ccifContext,ccifStatus)
C
C
      ccifStatus = KeepContext
      IF (Rs(7,Jdo) .lt. ValueNotDet -1.0 .and. 
     +    Rs(7,Jdo) .gt. 0.01)
     + CALL ccif_put_real('_reflns_shell.Rmeas_mean',
     +      Rs(7,Jdo),ccifContext,ccifStatus)
C
C
      ccifStatus = KeepContext
      IF (Rmult(Jdo) .lt. ValueNotDet -1.0 .and. 
     +    Rmult(Jdo) .gt. 0.01)
     + CALL ccif_put_real('_reflns_shell.multiplicity',
     +      Rmult(Jdo),ccifContext,ccifStatus)
C
C
      ccifStatus = KeepContext
      IF (Poss(1,Jdo) .lt. ValueNotDet -1.0 .and. 
     +    Poss(1,Jdo) .gt. 0.01)
     + CALL ccif_put_real('_reflns_shell.percent_possible_all',
     +      Poss(1,Jdo),ccifContext,ccifStatus)
C
C
      ccifStatus = KeepContext
      IF (Poss(2,Jdo) .lt. ValueNotDet -1.0 .and. 
     +    Poss(2,Jdo) .gt. 0.01)
     + CALL ccif_put_real(
     +      '_reflns_shell.cum_percent_possible_all',
     +      Poss(2,Jdo),ccifContext,ccifStatus)
C
C
      ccifStatus = KeepContext
      IF (Poss(3,Jdo) .lt. ValueNotDet -1.0 .and. 
     +    Poss(3,Jdo) .gt. 0.01)
     + CALL ccif_put_real(
     +      '_reflns_shell.anom_diff_percent_meas',
     +      Poss(3,Jdo),ccifContext,ccifStatus)
C
C
      ccifStatus = KeepContext
      IF (sds(1,Jdo) .lt. ValueNotDet -1.0 .and. 
     +    sds(1,Jdo) .gt. 0.01)
     + CALL ccif_put_real('_reflns_shell.meanI_over_sigI_all',
     +      sds(1,Jdo),ccifContext,ccifStatus)
C
C
      ccifStatus = KeepContext
      IF (sds(2,Jdo) .lt. ValueNotDet -1.0 .and. 
     +    sds(2,Jdo) .gt. 0.01)
     + CALL ccif_put_real(
     +      '_reflns_shell.meanI_over_sd_all',
     +      sds(2,Jdo),ccifContext,ccifStatus)
C
C
      ccifStatus = KeepContext
      IF (Nfpb(Jdo) .lt. ValueNotDet -1.0 .and. 
     +    Nfpb(Jdo) .gt. 0) THEN
      CALL ccif_put_int(
     +      '_reflns_shell.num_fract_bias_in_mean',
     +      Nfpb(Jdo),ccifContext,ccifStatus)
      ccifStatus = KeepContext
      CALL ccif_put_real(
     +      '_reflns_shell.mean_fract_bias',
     +      FPB(Jdo),ccifContext,ccifStatus)
      END IF
C
C
 10   CONTINUE
      CALL ccif_release_context(ccifContext)
      END IF
C
C
      ccifStatus = 1
      IF (EnvRet .eq. 0) THEN
        CALL ccif_close_cif('DEPOSITFILE',ccifStatus)
      ELSE
        IF (lenstr(DEPOSITFNAME).gt.1)
     +CALL ccif_close_cif(DEPOSITFNAME(1:Lenstr(DEPOSITFNAME)),
     +                      ccifStatus)
      END IF
C
C
      RETURN
      END
C
C
C     ===============================================
      SUBROUTINE Hdata_reduction_method(Method,Nlines)
C     ================================================
C
C
C  _reflns.data_reduction_method <text>
C
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      INTEGER Nlines
      CHARACTER Method(Nlines)*80
C     ..
      CHARACTER Cwork*800
      CHARACTER Awork(MaxLines)*80
C     ..
C     .. External Subroutines ..
      INTEGER Lenstr
      EXTERNAL ccif_output_fmt,
     +          ccif_put_real,
     +           ccif_release_context,
     +            ccif_setup_context
C     ..
      EQUIVALENCE (Awork,Cwork)
C
C
      IF ( .not. Harvest) RETURN
C
C
      IF (Nlines .ge. 1) THEN
      CALL ccif_setup_context('_reflns.data_reduction_method',
     +                         CurrCategory,ccifBlockID,
     +                         ccifContext,ccifStatus,' ')
C
C
        IF (NHlines .le. MaxLines) THEN
        DO 10 Jdo =1 ,NHlines
          Awork(Jdo) = Method(Jdo)
 10     CONTINUE
         ccifStatus = 1
         CALL ccif_put_text('_reflns.data_reduction_method',
     +   NHlines,Cwork,10,ccifContext,ccifStatus,'NEW')
        GO TO 20
        ELSE
        KK = 0 
        MM = 0
 30     CONTINUE
        KK = KK + 10
        MM = MM + 1
C
C
        IF (KK .le. NHlines) THEN
        LL = 0        
        DO 40 Jdo =KK-9 ,KK
          LL = LL + 1
          Awork(LL) = Method(Jdo)
 40     CONTINUE
C
C
         IF (MM .eq. 1) THEN
         ccifStatus = 1
         CALL ccif_put_text('_reflns.data_reduction_method',
     +   10,Cwork,10,ccifContext,ccifStatus,'NEW')
         ELSE
         ccifStatus = 1
         CALL ccif_put_text('_reflns.data_reduction_method',
     +   10,Cwork,10,ccifContext,ccifStatus,'  ')
         END IF
C
C
        GO TO 30
        ELSE
C
C---- last bit of lines
C
        KK = KK - 9
        LL = 0        
        DO 50 Jdo =KK ,NHlines
          LL = LL + 1
          Awork(LL) = Method(Jdo)
 50     CONTINUE
        KK = NHlines - KK + 1
         ccifStatus = 1
         CALL ccif_put_text('_reflns.data_reduction_method',
     +   KK,Cwork,10,ccifContext,ccifStatus,' ')
        GO TO 20
        END IF
        END IF
C
C
 20     CONTINUE
C
C
      CALL ccif_release_context(ccifContext)
C
C
      END IF
C
C
      RETURN
      END
      SUBROUTINE Hdensity_Matthews(Fraction)
C     ====================================
C
C The density of the crystal, expressed as the ratio of the
C volume of the asymmetric unit to the molecular mass of a
C monomer of the structure, in units of angstroms^3^ per dalton.
C  Ref: Matthews, B. W. (1960). J. Mol. Biol., 33, 491-???.
C    _item_type.code               float
C
C
C
      include 'harvest.inc'
C
C     .. Scalar Arguments ..
      REAL Fraction
C     ..
C     .. External Subroutines ..
      EXTERNAL ccif_put_real,
     +          ccif_release_context,
     +           ccif_setup_context
C     ..
C
C
      IF ( .not. Harvest) RETURN
C
C
C
C---- do _exptl_crystal.density_Matthews
C
      CALL ccif_setup_context('_exptl_crystal.density_Matthews',
     +                        CurrCategory,ccifBlockID,ccifContext,
     +                        ccifStatus,' ')
      ccifStatus = 1
      CALL ccif_put_real('_exptl_crystal.density_Matthews',Fraction,
     +                   ccifContext,ccifStatus)
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C     ============================================================
      SUBROUTINE Hinitialise(Pkage,
     +                       ProgName,ProgVersion,
     +                       ProjectName,DataSetName,
     +                       UseCWD,Private,
     +                       IVALND,VALND,RowLimit)
C     ============================================================
C
      include 'harvest.inc'
C
C     .. Scalar Arguments ..
      REAL VALND
      INTEGER IVALND,RowLimit
      LOGICAL Private,UseCWD
      CHARACTER DataSetName* (*),
     +           ProjectName* (*),
     +            ProgVersion* (*),
     +             ProgName* (*),
     +              Pkage* (*)
C     ..
C     .. Local Scalars ..
      INTEGER chmodRet,Jdo,mkdirPT
      CHARACTER ciftime*50,Buffer*256,EnvWork*256,
     +          FileName*256,mkdirMode*3,chmodMode*3
C     ..
C     .. External Functions ..
      LOGICAL VAXVMS
      INTEGER Lenstr
      EXTERNAL Lenstr,VAXVMS
C     ..
C     .. External Subroutines ..
      EXTERNAL ccif_block_by_name,
     +          ccif_init,
     +           ccif_new_cif,
     +            ccif_put_char,
     +             ccif_release_context,
     +              ccif_setup_context,
     +               ccif_set_line_limit,
     +                fdate,cchmod,cmkdir,
     +                 ustenv,ugtenv
C     ..
C     .. Data statements ..
      DATA Harvest/.false./
      DATA MirDerSiteID/0/, rom_context/-1/,restraints_context/-1/
C     ..
c
C
      call Hgetlimits(IValueNotDet,ValueNotDet)
      IVALND = IValueNotDet
      VALND  = ValueNotDet
      SoftwareName = ProgName
      SoftwareVersion = ProgVersion
C
C
      IF (ProjectName(1:1) .eq. ' ') THEN
        WRITE (6,FMT=6000)
 6000   FORMAT (
     +' Harvest: NO ProjectName given - no deposit file created')
        Harvest = .false.
        RETURN
      END IF
C
C
      IF (DataSetName(1:1) .eq. ' ') THEN
        WRITE (6,FMT=6002)
 6002   FORMAT (
     +' Harvest: NO DataSetName given - no deposit file created')
        Harvest = .false.
        RETURN
      END IF
C
C
      PName = ProjectName
      DName = DataSetName
      Package = Pkage
C mode for mkdir call
      MKDIRMODE = '700'
C mode for chmod call
      CHMODMODE = '755'

      CALL Hciftime(ciftime)
C
C
      IF (UseCWD) THEN
        IF ( .not. VAXVMS()) THEN
          FileName = 
     +           './'//DataSetName(1:Lenstr(DataSetName))//'.'//
     +               ProgName(1:Lenstr(ProgName))
        ELSE
          FileName = DataSetName(1:Lenstr(DataSetName))//'.'//
     +               ProgName(1:Lenstr(ProgName))
        END IF
      ELSE
        Buffer = ' '
C
        CALL ugtenv('HARVESTHOME',Buffer)
C
C    If HARVESTHOME not set, default to HOME
        IF (Buffer .eq. ' ') THEN
          IF (VAXVMS()) THEN
            CALL ugtenv('SYS$LOGIN',Buffer)
          ELSE
            CALL ugtenv('HOME',Buffer)
          END IF
C
          IF (Buffer .eq. ' ') THEN
            WRITE (6,FMT=6004)
 6004       FORMAT (
     +' Harvest: No HARVESTHOME or HOME environment variable found - ',
     +               'no deposit file created')
            Harvest = .false.
            RETURN
          END IF
        END IF
C
C---- see if $HARVESTHOME/DepositFiles directory exists
C
        IF (VAXVMS()) THEN
          FileName = Buffer(1:Lenstr(Buffer))//':[DepositFiles]'
        ELSE
          FileName = Buffer(1:Lenstr(Buffer))//'/DepositFiles'
        END IF
C
        CALL cmkdir(FileName(1:Lenstr(FileName)),mkdirMode,mkdirPT)
        IF (mkdirPT .lt. 0) THEN
            WRITE (6,FMT=6006)
 6006       FORMAT (
     +' Harvest: Cant mkdir HARVESTHOME/DepositFiles - ',
     +           'no deposit file created')
            Harvest = .false.
            RETURN
        END IF
C
C--- world read for directory?
C
        IF (.NOT. Private) THEN
            CALL cchmod(FileName(1:Lenstr(FileName)),chmodMode,chmodRet)
            IF (chmodRet .ne. 0) THEN
              WRITE (6,FMT=6008)
 6008         FORMAT (
     +' Harvest: Cant chmod HARVESTHOME/DepositFiles to world',
     +               ' read - no deposit file created')
              Harvest = .false.
              RETURN
            END IF
        END IF
C
C---- now look for ProjectName sub-directory
C
        IF (VAXVMS()) THEN
          FileName = FileName(1:Lenstr(FileName)-1)//'.'//
     +               ProjectName(1:Lenstr(ProjectName))//']'
        ELSE
          FileName = FileName(1:Lenstr(FileName))//'/'//
     +               ProjectName(1:Lenstr(ProjectName))
        END IF
C
        CALL cmkdir(FileName(1:Lenstr(FileName)),mkdirMode,mkdirPT)
        IF (mkdirPT .lt. 0) THEN
            WRITE (6,FMT=6010) ProjectName(1:Lenstr(ProjectName))
 6010       FORMAT (
     +' Harvest: Cant mkdir HARVESTHOME/DepositFiles/',a,
     +           ' - no deposit file created')
            Harvest = .false.
            RETURN
        END IF
C
C--- world read for directory?
C
        IF (.NOT. Private) THEN
            CALL cchmod(FileName(1:Lenstr(FileName)),chmodMode,chmodRet)
            IF (chmodRet .ne. 0) THEN
              WRITE (6,FMT=6012) FileName(1:Lenstr(FileName))
 6012         FORMAT (
     +' Harvest: Cant chmod HARVESTHOME/DepositFiles/',a,' to',
     +               ' world read - no deposit file created')
              Harvest = .false.
              RETURN
            END IF
        END IF
C
C---- now create FileName for deposit information
C
        IF (VAXVMS()) THEN
          FileName = FileName(1:Lenstr(FileName))//
     +               DataSetName(1:Lenstr(DataSetName))//'.'//
     +               ProgName(1:Lenstr(ProgName))
        ELSE
          FileName = FileName(1:Lenstr(FileName))//'/'//
     +               DataSetName(1:Lenstr(DataSetName))//'.'//
     +               ProgName(1:Lenstr(ProgName))
        END IF
      END IF
C
C---- put the environment variable DEPOSITFILE
C
      DEPOSITFNAME = FileName(1:Lenstr(FileName))
C
C
      EnvWork = 'DEPOSITFILE='//FileName(1:Lenstr(FileName))
      CALL USTENV(EnvWork,EnvRet)
C
C
c     ustenv returns non-zero if it was unable to obtain enough space via
c     malloc for an expanded environment, otherwise zero.

c      IF (EnvRet .ne. 0) THEN
c        WRITE (6,FMT=6014) EnvWork(1:Lenstr(EnvWork)),EnvRet
c 6014   FORMAT (
c     +' Harvest: Cant ustenv for DEPOSITFILE  - ',
c     +            'no deposit file created',/,a,/,2x,i10)
c        Harvest = .false.
c        RETURN
c      END IF
C
C
      Harvest = .true.
C
C---- now call ccif routines
C     1. where is   mmcifdic.lib   use logical name MMCIFDIC
C
      CALL ccif_init('MMCIFDIC')
C
C
        IF (RowLimit .gt. 132) RowLimit= 132
        IF (RowLimit .lt. 80)  RowLimit= 80
      IF (EnvRet .eq. 0) THEN
        CALL ccif_new_cif(' ','DEPOSITFILE',ccifBlocks)
        CALL ccif_set_line_limit('DEPOSITFILE',RowLimit)
      ELSE
        CALL ccif_new_cif(' ',FileName(1:Lenstr(FileName)),
     +                    ccifBlocks)
        CALL ccif_set_line_limit(FileName(1:Lenstr(FileName)),
     +                    RowLimit)
      END IF
C
C        
C
C---- do   _data
C
      CurrBlock = 'data_'//
     +               ProjectName(1:Lenstr(ProjectName))//'['//
     +               DataSetName(1:Lenstr(DataSetName))//']'
C
C
      IF (EnvRet .eq. 0) THEN
      CALL ccif_block_by_name('DEPOSITFILE',
     +                        CurrBlock(1:Lenstr(CurrBlock)),
     +                        ccifBlockID,ccifStatus,'NEW')
      ELSE
      CALL ccif_block_by_name(FileName(1:Lenstr(FileName)),
     +                        CurrBlock(1:Lenstr(CurrBlock)),
     +                        ccifBlockID,ccifStatus,'NEW')
      END IF
C
C
      IF (ccifStatus .eq. -1) THEN
        WRITE (6,FMT=6016)
 6016   FORMAT (
     +' Harvest: Internal CCIF error - no deposit file created')
        Harvest = .false.
        RETURN
      END IF
C
C----  do   _entry.id
C
      CALL ccif_setup_context('ENTRY',CurrCategory,ccifBlockID,
     +                        ccifContext,ccifStatus,' ')
      CALL ccif_put_char('_entry.id',
     +                   ProjectName(1:Lenstr(ProjectName)),
     +                   ccifContext,ccifStatus)
      CALL ccif_release_context(ccifContext)



      CALL ccif_setup_context('DIFFRN',CurrCategory,ccifBlockID,
     +                        ccifContext,ccifStatus,' ')
      CALL ccif_put_char('_diffrn.id',
     +     DataSetName(1:Lenstr(DataSetName)),ccifContext,
     +                   ccifStatus)
      CALL ccif_release_context(ccifContext)

C
C---- do   _audit.creation_date
C
      CALL ccif_setup_context('AUDIT',CurrCategory,ccifBlockID,
     +                        ccifContext,ccifStatus,' ')
      CALL ccif_put_char('_audit.creation_date',
     +     ciftime(1:Lenstr(ciftime)),ccifContext,
     +                   ccifStatus)
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C     ===============================================
      SUBROUTINE Hmerge_reject_criterion(Rcriteria,Nlines)
C     ================================================
C
C
C  _reflns.merge_reject_criterion
C
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      INTEGER Nlines
      CHARACTER Rcriteria(Nlines)*80
C     ..
      CHARACTER Cwork*800
      CHARACTER Awork(MaxLines)*80
C     ..
C     .. External Subroutines ..
      INTEGER Lenstr
      EXTERNAL ccif_output_fmt,
     +          ccif_put_real,
     +           ccif_release_context,
     +            ccif_setup_context
C     ..
      EQUIVALENCE (Awork,Cwork)
C
C
      IF ( .not. Harvest) RETURN
C
C
      IF (Nlines .ge. 1) THEN
      CALL ccif_setup_context('_reflns.merge_reject_criterion',
     +                         CurrCategory,ccifBlockID,
     +                         ccifContext,ccifStatus,' ')
C
C
        IF (NHlines .le. MaxLines) THEN
        DO 10 Jdo =1 ,NHlines
          Awork(Jdo) = Rcriteria(Jdo)
 10     CONTINUE
         ccifStatus = 1
        CALL ccif_put_text('_reflns.merge_reject_criterion',
     +   NHlines,Cwork,10,ccifContext,ccifStatus,'NEW')
        GO TO 20
        ELSE
        KK = 0 
        MM = 0
 30     CONTINUE
        KK = KK + 10
        MM = MM + 1
C
C
        IF (KK .le. NHlines) THEN
        LL = 0        
        DO 40 Jdo =KK-9 ,KK
          LL = LL + 1
          Awork(LL) = Rcriteria(Jdo)
 40     CONTINUE
C
C
         IF (MM .eq. 1) THEN
         ccifStatus = 1
         CALL ccif_put_text('_reflns.merge_reject_criterion',
     +   10,Cwork,10,ccifContext,ccifStatus,'NEW')
         ELSE
         ccifStatus = 1
         CALL ccif_put_text('_reflns.merge_reject_criterion',
     +   10,Cwork,10,ccifContext,ccifStatus,'  ')
         END IF
C
C
        GO TO 30
        ELSE
C
C---- last bit of lines
C
        KK = KK - 9
        LL = 0        
        DO 50 Jdo =KK ,NHlines
          LL = LL + 1
          Awork(LL) = Rcriteria(Jdo)
 50     CONTINUE
        KK = NHlines - KK + 1
         ccifStatus = 1
         CALL ccif_put_text('_reflns.merge_reject_criterion',
     +   KK,Cwork,10,ccifContext,ccifStatus,' ')
        GO TO 20
        END IF
        END IF
C
C
 20     CONTINUE
C
C
        CALL ccif_release_context(ccifContext)
C
C
        END IF
C
C
      RETURN
      END
C
C
C     =====================================================
      SUBROUTINE Hoverall_observations(Ntotal,R1,R2)
C     =====================================================
C
C
C     _reflns.overall_d_resolution_high
C     _reflns.overall_d_resolution_low
C     _reflns.overall_num_observations
C
C
      include 'harvest.inc'
C
C
C     .. Array Arguments ..
      REAL R1,R2
      INTEGER Ntotal
C     ..
C     .. External Subroutines ..
      INTEGER Lenstr
      EXTERNAL ccif_output_fmt,
     +          ccif_put_real,
     +           ccif_release_context,
     +            ccif_setup_context
C     ..
C
C
      IF ( .not. Harvest) RETURN
C
C
      CALL ccif_setup_context('_reflns.overall_d_res_high',
     +                         CurrCategory,ccifBlockID,
     +                         ccifContext,ccifStatus,' ')
C
C
      CALL ccif_output_fmt('_reflns.overall_d_res_high',
     +             ' ',6,2,'f',ccifStatus)
      CALL ccif_output_fmt('_reflns.overall_d_res_low',
     +             ' ',6,2,'f',ccifStatus)
      CALL ccif_output_fmt('_reflns.overall_number_observations',
     +    ' ',10,0,'d',ccifStatus)
C
C
       Rlow = R1
       Rhigh = R2
      IF (R1 .lt. R2) THEN
        Rlow = R2
        Rhigh = R1
      END IF
C
C
      CALL ccif_put_real('_reflns.overall_d_res_low',
     +                   Rlow,ccifContext,ccifStatus)
      CALL ccif_put_real('_reflns.overall_d_res_high',
     +                   Rhigh,ccifContext,ccifStatus)
C
C
      IF (Ntotal .ne. IvalueNotDet) 
     + CALL ccif_put_int('_reflns.overall_number_observations',
     +                   Ntotal,ccifContext,ccifStatus)
C
C
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
      SUBROUTINE Hparse(line,Ibeg,Iend,Ityp,Fvalue,Cvalue,Idec,N)
C     ==========================================================
C
C     Free format read routine.  This is reaLLy a scanner, not a parser.
C     It scans the line into N tokens which are separated by delimiters
C     and updates the information arrays for each, as below.  The
C     default delimiters are space, tab, comma and equals; they may be
C     changed using PARSDL.  Adjacent commas delimit `null' fields (the
C     same as empty strings).  strings may be unquoted or single- or
C     double-quoted if they don't coNTain delimiters, but must be
C     surrounded by delimiters to be recognised.  This allows literal
C     quotes to be read, e.g. "ab"c" will be recognised as the token `ab"c'.
C     An unquoted `!' or `#' in line introduces a trailing comment,
C     which is ignored.
C
C---- Arguments:
C
C   line  (I)     CHARACTER*(*)  string to be parsed
C
C   N     (I/O)   INTEGER        UsuaLLy <0, when abs(N) is the maximum
C                                number of fields to interpret and should
C                                be <= the array dimensions.  If N>0 it
C                                is the number of tokens read so far,
C                                intended for continuation lines with PARSER.
C                                Returns number of fields scanned or 0 if
C                                line is blank or just coNTains a comment
C
C  For I=1,N :
C
C   Ibeg(I)   (O) INTEGER(*)     1st column number in field
C
C   Iend(I)   (O) INTEGER(*)     last column number in field
C
C   Ityp(I)   (O) INTEGER(*)     =0  null field
C                                =1  character string
C                                =2  number
C
C    Fvalue(I) (O) REAL(*)        Value of number.  Use nint(Fvalue(I)) to
C                                extract an integer.
C
C   Cvalue(I) (O) CHARACTER(*)*4 Character string (1st 4 characters)
C                                for numbers as well as strings
C
C     Items in Fvalue and Cvalue are left unchanged for null fields
C
C   Idec(I)   (O) INTEGER(*)     Number of 'digits'
C                           for string, number of characters (=4 if  .gt.  4)
C                                for integer, number of digits
C                                for real number,
C                                (number of digits before point+1)*100
C                                +number of digits after point
C
C     This routine is truly horrible and really ought to be re-written
C     in an understandable form with an outer loop over tokens rather
C     than characters...
C
C     .. Parameters ..
      INTEGER Maxdelim
      PARAMETER (Maxdelim=20)
C     ..
C     .. Scalar Arguments ..
      INTEGER N
      CHARACTER line* (*)
C     ..
C     .. Array Arguments ..
      REAL Fvalue(*)
      INTEGER Ibeg(*),Idec(*),Iend(*),Ityp(*)
      CHARACTER Cvalue(*)*4
C     ..
C     .. Local Scalars ..
      REAL F10,SIGN,SIGN0,VALUE,VALUE0
      INTEGER I,IDOT,INTLEN,J,L,LENG,LINLEN,NCHK,NDDELM,NDELM,NDIGS,
     +        NDONE,NDSDLM,NITEM,NPLACE,NSPDLM,OPER
      LOGICAL COMMNT,NULL,NUMBER,OPRATR,QUOTE,TOKEN,TQUOTE
      CHARACTER BLANK*1,DBLQT*1,ICOMM1*1,ICOMM2*1,LETQT*1,OLDQUT*1,
     +          TAB*1,lineRR*1500
C     ..
C     .. Local Arrays ..
      INTEGER ISGN(2)
      CHARACTER DDELIM(Maxdelim)*1,DELIM(Maxdelim)*1,DIGS(18)*1
C     ..
C     .. External Functions ..
      INTEGER Lenstr
      EXTERNAL Lenstr
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC CHAR
C     ..
C     .. Save statement ..
      SAVE DELIM,NDELM,NSPDLM,DDELIM,NDDELM,NDSDLM
C     ..
C     .. Data statements ..
C Delimiters
C---- DELIM  array of NDELM delimiters
C---- DDELIM  default array of NDDELM delimiters
C---- NSPDLM (NDSDLM default) is number of special delimiters which
C            cannot delimit a null field these are
C            at the beginning of the delimiter array
C
C-- Note that delimiters may be changed 
C   by a call to PARSDL (entry point)
C
      DATA LETQT,DBLQT/'''','"'/,BLANK/' '/,ICOMM1,ICOMM2/'#','!'/
      DATA DIGS/'0','1','2','3','4','5','6','7','8','9','+','-','*','/',
     +     'E','.','e',' '/
      DATA ISGN/1,-1/
      DATA NDIGS/17/
      DATA DDELIM/' ',' ','=',',',16*' '/
      DATA NDDELM/4/,NDSDLM/3/,NDELM/-1/
C     ..
C
C---- Setup delimiters if not done
C
      IF (NDELM.LT.0) THEN
        NDELM = NDDELM
        NSPDLM = NDSDLM
        DO 10 I = 1,Maxdelim
          DELIM(I) = DDELIM(I)
   10   CONTINUE
C       Set tab (assumes ASCII)
        TAB = CHAR(9)
        DELIM(2) = TAB
      END IF
C
C---- First call  - N = - NITEM ; NDONE = 0
C
      IF (N.LT.0) THEN
        NITEM = -N
        NDONE = 0
C
C---- Continuation line: N = number already read
C
      ELSE IF (N.GE.0) THEN
        NDONE = N
        NITEM = N
      END IF
C
      N = 1
      TOKEN = .FALSE.
      VALUE = 0.0
      OPRATR = .TRUE.
      IDOT = 0
      INTLEN = 0
      SIGN = 1.0
      OPER = 0
      OLDQUT = BLANK
      QUOTE = .FALSE.
      TQUOTE = .FALSE.
      NUMBER = .FALSE.
      COMMNT = .FALSE.
C
      LINLEN = Lenstr(line)
      IF (LINLEN .le. 0) THEN
        N = 0
        RETURN
      END IF
C
C---- Main loop over character Buffer.  The loop goes one past the end,
C     but we're careful not to index this character.
C
      DO 60 I = 1,LINLEN + 1
C
C----     check for comment character (not in string)
C
        IF ( .not. QUOTE .and. (line(I:I) .eq. ICOMM1 .or. line(I:
     +      I) .eq. ICOMM2)) THEN
          COMMNT = .TRUE.
C           special case; comment line:
          IF (N .eq. 1) THEN
            N = 0
            RETURN
          END IF
        END IF
C
C---- Look for quotation marks
C
        IF (I .le. LINLEN) THEN
          IF (line(I:I) .eq. LETQT  .or.  line(I:I) .eq. DBLQT) THEN
C             1st quote must come at beginning of string, otherwise
C             treat as normal
            IF (OLDQUT .eq. BLANK .and.  .not. TOKEN) THEN
C               Start of quoted string
              OLDQUT = LETQT
              QUOTE = .TRUE.
            ELSE IF (OLDQUT .eq. LETQT) THEN
C               End of quoted string
              OLDQUT = BLANK
              QUOTE = .FALSE.
            END IF
            GO TO 60
          END IF
        ELSE
          QUOTE = .FALSE.
        END IF
C
C---- Check for delimiting characters
C
        IF (I .le. LINLEN) THEN
          DO 20 J = 1,NDELM
            IF (line(I:I) .eq. DELIM(J)) GO TO 30
   20     CONTINUE
        END IF
        J = NDELM + 1
   30   CONTINUE
C
        IF (( .not. QUOTE.and. 
     +       (J .le. NDELM .or. I .gt. LINLEN))  .or. 
     +      COMMNT) THEN
C
C---- Have found a delimiter
C
          NULL = .FALSE.
          IF ( .not. TOKEN .and.  
     +         .not. COMMNT .and. J .gt. NSPDLM) THEN
C
C---- Allow delimiters other than
C     <space> & <tab> to delimit null fields
C
            Ibeg(N) = I
            Ityp(N) = 0
            Iend(N) = I
            NULL = .TRUE.
          END IF
          IF (TOKEN) THEN
C
C---- End of token
C
            Iend(N) = I - 1
C               Exclude quote from token
            IF (TQUOTE .and. OLDQUT .eq. BLANK) Iend(N) = I - 2
C
C---- Store first 4 characters in Cvalue for all types
C
            LENG = Iend(N) - Ibeg(N) + 1
            IF (LENG .gt. 4) LENG = 4
            L = Ibeg(N)
            Cvalue(N) = line(L:L+LENG-1)
C
C---- Token is a number
C
            IF (NUMBER) THEN
              Ityp(N) = 2
              Fvalue(N) = VALUE*SIGN
              IF (OPER .eq. 1) THEN
C
C---- unary +
C
                Fvalue(N) = Fvalue(N) + SIGN0*VALUE0
              ELSE IF (OPER .eq. 2) THEN
C
C----  unary -
C
                Fvalue(N) = Fvalue(N) - SIGN0*VALUE0
              ELSE IF (OPER .eq. 5) THEN
C
C---- exponent
C
                Fvalue(N) = SIGN0*VALUE0*10.0**Fvalue(N)
              END IF
              IF (IDOT .eq. 1) THEN
                Idec(N) = 100*INTLEN + NPLACE
              ELSE
                Idec(N) = INTLEN
              END IF
            ELSE
C
C---- Token is Alphameric
C
              Ityp(N) = 1
              Idec(N) = LENG
            END IF
          END IF
          IF (TOKEN  .or.  NULL) THEN
            N = N + 1
            NCHK = N + NDONE
            TOKEN = .FALSE.
            VALUE = 0.0
            OPRATR = .TRUE.
            IDOT = 0
            INTLEN = 0
            SIGN = 1.0
            OPER = 0
            TQUOTE = .FALSE.
            NUMBER = .FALSE.
C
C---- Check number of items.
C
            IF (NCHK .gt. NITEM) GO TO 80
          END IF
C
C----  there's nothing else to do with a comment
C
          IF (COMMNT) GO TO 70
C
C---- If delimiter was "+" or "-", also treat it as part of the
C     next token
C
          IF (DELIM(J) .eq. '+'  .or.  DELIM(J) .eq. '-') THEN
            J = NDELM + 1
            GO TO 30
          END IF
          GO TO 60
        END IF
C
C---- Not a delimiter so must be a token -- suspect numeric token
C
        IF ( .not. TQUOTE .and. 
     +      ( .not. TOKEN .or. NUMBER)) THEN
          IF ( .not. QUOTE) THEN
            DO 40 J = 1,NDIGS
              IF (line(I:I) .eq. DIGS(J)) GO TO 50
   40       CONTINUE
            J = NDIGS + 1
   50       CONTINUE
C
C----  Change "e" to "E"
C
            IF (J .eq. 17) J = 15
            IF (J .le. NDIGS) THEN
C
C---- May be number
C
              NUMBER = .TRUE.
              IF (J .le. 10) THEN
C
C----  Have a digit 0-9
C
                IF (IDOT .eq. 0) THEN
                  INTLEN = INTLEN + 1
                  VALUE = (J-1) + VALUE*10
                END IF
                IF (IDOT .eq. 1) THEN
C
C----  Before decimal point
C
                  VALUE = (J-1)*F10 + VALUE
                  F10 = F10*0.1
C
C----  After decimal point
C
                  NPLACE = NPLACE + 1
                END IF
                OPRATR = .FALSE.
              ELSE IF (OPRATR .and. 
     +            (J .eq. 11 .or. J .eq. 12)) THEN
C
C---- Find + or - as signs not operators
C
                OPRATR = .FALSE.
C
C---- Set sign of number
C
                SIGN = ISGN(J-10)
              ELSE IF (J .eq. 15) THEN
C
C---- Do not allow 2 operators
C
                IF (OPRATR) NUMBER = .FALSE.
                VALUE0 = VALUE
                SIGN0 = SIGN
                OPER = J - 10
                VALUE = 0.0
                SIGN = 1.0
                IDOT = 0
                OPRATR = .TRUE.
C
C---- Find a decimal point
C       decimal point
C
              ELSE IF (J .eq. 16) THEN
                IDOT = IDOT + 1
                NPLACE = 0
                F10 = 0.1
C
C----  A valid number has one point
C
                IF (IDOT .eq. 2) NUMBER = .FALSE.
                OPRATR = .FALSE.
              END IF
            ELSE
C
C---- Token is not number
C
              NUMBER = .FALSE.
            END IF
          END IF
C
C---- Start a new token
C
          IF ( .not. TOKEN) THEN
C
C---- Of any type
C
            TOKEN = .TRUE.
            Ibeg(N) = I
C
C---- Start quoted string
C
            IF (QUOTE) THEN
              TQUOTE = .TRUE.
              NUMBER = .FALSE.
            END IF
          END IF
        END IF
   60 CONTINUE
   70 N = N - 1
      RETURN
   80 CONTINUE
C
C
      write (lineRR,fmt='(A,I4,A)') '  ***** WARNING - MORE THAN ',
     +  NITEM,' ITEMS IN THIS line - IGNORING THE REST****'
        CALL Putlin('     ','ERRWIN')
        CALL Putlin('***  Warning','ERRWIN')
        CALL Putlin(lineRR,'ERRWIN')
        CALL Putlin('      ','ERRWIN')
        CALL Putlin('     ','ERRWIN')
        CALL Putlin('***  Warning','ERRWIN')
        CALL Putlin(line(1:Lenstr(line)),'ERRWIN')
        CALL Putlin('      ','ERRWIN')
      N = N - 1
      RETURN
      END
      SUBROUTINE Hphasing_mir_der_p1(NumDer,DerID,NumSitesDer,
     +                               Criteria,Resomin,Resomax)
C     ===========================================================
C
C  Additions to Category
C
C    _PHASING_MIR_DER
C  These come at start up
C        _phasing_MIR_der.id
C        _phasing_MIR_der.number_of_sites
C        _phasing_MIR_der.d_res_high
C        _phasing_MIR_der.d_res_low
C        _phasing_MIR_der.Reflns_Criteria
C
C  additions    - These come serially after last cycle
C        _phasing_MIR_der.Power_centric
C        _phasing_MIR_der.Power_acentric
C        _phasing_MIR_der.R_cullis_centric
C        _phasing_MIR_der.R_cullis_acentric
C        _phasing_MIR_der.R_cullis_anomalous
C        _phasing_MIR_der.Reflns_acentric
C        _phasing_MIR_der.Reflns_anomalous
C        _phasing_MIR_der.Reflns_centric
C
C
C
      include 'harvest.inc'
      include 'harderiv.inc'
C
C     .. Array Arguments ..
      REAL Resomin(2,*),Resomax(2,*),Criteria(*)
      INTEGER NumSitesDer(*)
      CHARACTER DerID(*)*80
C     ..
C     .. Scalar Arguments ..
      INTEGER NumDer
C     ..
C     .. Local Scalars ..
      INTEGER Jdo
C     ..
C
C
      ND = -999
      DerPointer = 0
C
      IF ( .not. Harvest) RETURN
C
C
      ND = NumDer
      DO 10 Jdo = 1,NumDer
        SS(Jdo) = DerID(Jdo)(1:15)
        R1(1,Jdo) = Resomin(1,Jdo)
        IF (R1(1,Jdo) .lt. 0.55) R1(1,Jdo) = 1.0/sqrt(R1(1,Jdo))
        R1(2,Jdo) = Resomax(1,Jdo)
        IF (R1(2,Jdo) .lt. 0.55) R1(2,Jdo) = 1.0/sqrt(R1(2,Jdo))
        NS(Jdo) = NumSitesDer(Jdo)

        IF (Criteria(Jdo) .lt. valueNotDet -1.0  .and.
     +      Criteria(Jdo) .gt. 0.01) THEN
              WRITE (CC(Jdo),6000) Criteria(Jdo)
        ELSE
              CC(Jdo) = '****'
        END IF

 6000   FORMAT('FPH >',f3.1,' SIGFPH')
   10 CONTINUE
C
C
      RETURN
      END
      SUBROUTINE Hphasing_mir_der_p2(Power,RCullis,Reflns)
C     ================================================
C
C  Additions to Category
C
C    _PHASING_MIR_DER
C  These come at start up
C        _phasing_MIR_der.id
C        _phasing_MIR_der.number_of_sites
C        _phasing_MIR_der.d_res_high
C        _phasing_MIR_der.d_res_low
C        _phasing_MIR_der.Reflns_Criteria
C
C  additions    - These come serially after last cycle
C        _phasing_MIR_der.Power_centric
C        _phasing_MIR_der.Power_acentric
C        _phasing_MIR_der.R_cullis_centric
C        _phasing_MIR_der.R_cullis_acentric
C        _phasing_MIR_der.R_cullis_anomalous
C        _phasing_MIR_der.Reflns_acentric
C        _phasing_MIR_der.Reflns_anomalous
C        _phasing_MIR_der.Reflns_centric
C
C
      include 'harvest.inc'
      include 'harderiv.inc'
C     ..
C     .. Array Arguments ..
      REAL Power(2),RCullis(3)
      INTEGER Reflns(3)
C     ..
C     .. Local Scalars ..
      REAL Res1,Res2
C     ..
C     .. External Subroutines ..
      EXTERNAL ccif_output_fmt,
     +          ccif_put_char,
     +           ccif_put_int,
     +            ccif_put_real,
     +             ccif_release_context,
     +              ccif_setup_context
C     ..
C     .. External Functions ..
      INTEGER Lenstr
      EXTERNAL Lenstr
C     ..
C
      IF ( .not. Harvest) RETURN
C
C---- cant call this routine without first calling
C      SUBROUTINE Hphasing_mir_der_p1(NumDer,DerID,NumSitesDer,
C
      IF (ND .le. 0) RETURN
C
C
      CALL ccif_setup_context('_phasing_MIR_der.id',CurrCategory,
     +                        ccifBlockID,ccifContext,ccifStatus,'loop')
C
C
      IF (DerPointer .le. 0) THEN
        CALL ccif_output_fmt('_phasing_MIR_der.id','-',15,0,'z',
     +                       ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_der.d_res_low',' ',6,2,'f',
     +                       ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_der.d_res_high',' ',6,2,'f',
     +                       ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_der.number_of_sites',' ',3,0,
     +                       'd',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_der.Reflns_Criteria','-',15,
     +                       0,'z',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_der.Power_centric',
     +                       ' ',5,2,'f',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_der.Power_acentric',
     +                       ' ',5,2,'f',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_der.R_cullis_centric',
     +                       ' ',5,2,'f',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_der.R_cullis_acentric',
     +                       ' ',5,2,'f',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_der.R_cullis_anomalous',
     +                       ' ',5,2,'f',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_der.Reflns_centric',
     +                       ' ',9,0,'d',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_der.Reflns_acentric',
     +                        ' ',9,0,'d',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_der.Reflns_anomalous',
     +                       ' ',9,0,'d',ccifStatus)
      END IF
C
C
      DerPointer = DerPointer + 1
      ccifStatus = AppendRow
      IF (lenstr(SS(DerPointer)).gt.0)
     + CALL ccif_put_char('_phasing_MIR_der.id',
     +      SS(DerPointer) (1:Lenstr(SS(DerPointer))),
     +      ccifContext,ccifStatus)
C
C
      IF (R1(1,DerPointer) .gt. R1(2,DerPointer)) THEN
        Res1 = R1(1,DerPointer)
        Res2 = R1(2,DerPointer)
      ELSE
        Res2 = R1(1,DerPointer)
        Res1 = R1(2,DerPointer)
      END IF
C
C
      ccifStatus = KeepContext
      CALL ccif_put_real('_phasing_MIR_der.d_res_low',Res1,
     +                   ccifContext,ccifStatus)
      ccifStatus = KeepContext
      CALL ccif_put_real('_phasing_MIR_der.d_res_high',Res2,
     +                    ccifContext,ccifStatus)
      ccifStatus = KeepContext
      CALL ccif_put_int('_phasing_MIR_der.number_of_sites',
     +                  NS(DerPointer),ccifContext,
     +                  ccifStatus)
      ccifStatus = KeepContext
      IF (CC(DerPointer)(1:4) .ne. '****' .and.
     +    lenstr(CC(DerPointer)).gt.0) 
     + CALL ccif_put_char('_phasing_MIR_der.Reflns_Criteria',
     +                   CC(DerPointer) (1:
     +                   Lenstr(CC(DerPointer))),ccifContext,
     +                   ccifStatus)
C
C
      IF (Reflns(1) .lt. IvalueNotDet -1 .and. 
     +    Reflns(1) .gt. 0) THEN
          ccifStatus = KeepContext
          CALL ccif_put_int(
     +                  '_phasing_MIR_der.Reflns_centric',
     +                      Reflns(1),ccifContext,ccifStatus)
         ccifStatus = KeepContext
      IF (Package(1:4) .eq. 'CCP4') THEN
      IF (Power(1) .lt. valueNotDet -1.0 .and. 
     +    Power(1) .ne. 0.0) 
     +    CALL ccif_put_real(
     +            '_phasing_MIR_der.Power_centric',
     +                       Power(1),ccifContext,ccifStatus)
          ccifStatus = KeepContext
      IF (RCullis(1) .lt. valueNotDet -1.0 .and. 
     +    RCullis(1) .ne. 0.0) CALL ccif_put_real(
     +                '_phasing_MIR_der.R_cullis_centric',
     +                       RCullis(1),ccifContext,ccifStatus)
      END IF
      END IF
C
C
      IF (Reflns(2) .lt. IvalueNotDet -1 .and. 
     +    Reflns(2) .gt. 0) THEN
          ccifStatus = KeepContext
          CALL ccif_put_int(
     +                   '_phasing_MIR_der.Reflns_acentric',
     +                      Reflns(2),ccifContext,ccifStatus)
      ccifStatus = KeepContext
      IF (Package(1:4) .eq. 'CCP4') THEN
      IF (Power(2) .lt. valueNotDet -1.0 .and. 
     +    Power(2) .ne. 0.0) 
     +    CALL ccif_put_real(
     +                '_phasing_MIR_der.Power_acentric',
     +                       Power(2),ccifContext,ccifStatus)
      ccifStatus = KeepContext
      IF (RCullis(2) .lt. valueNotDet -1.0 .and. 
     +    RCullis(2) .ne. 0.0) CALL ccif_put_real(
     +               '_phasing_MIR_der.R_cullis_acentric',
     +                       RCullis(2),ccifContext,ccifStatus)
      END IF
      END IF
C
C
      IF (Reflns(3) .lt. IvalueNotDet -1 .and. 
     +    Reflns(3) .gt. 0) THEN
          ccifStatus = KeepContext
          CALL ccif_put_int(
     +                '_phasing_MIR_der.Reflns_anomalous',
     +                      Reflns(3),ccifContext,ccifStatus)
      ccifStatus = KeepContext
      IF (Package(1:4) .eq. 'CCP4') THEN
      IF (RCullis(3) .lt. valueNotDet -1.0 .and. 
     +    RCullis(3).ne. 0.0) CALL ccif_put_real(
     +              '_phasing_MIR_der.R_cullis_anomalous',
     +                       RCullis(3),ccifContext,ccifStatus)
      END IF
      END IF
C
C
      CALL ccif_release_context(ccifContext)
      RETURN
      END
C
C
C     =============================================================
      SUBROUTINE Hphasing_mir_der_site(DerID,NumDerSites,B,Atype,
     +                                  X,Y,Z,Occ,OccEsd,anom,
     +                                  AnomEsd)
C     =============================================================
C
C
C
      include 'harvest.inc'
C
C     ..
C     .. Array Arguments ..
      REAL anom(*),AnomEsd(*),B(*),Occ(*),OccEsd(*),X(*),Y(*),Z(*)
      CHARACTER Atype(*)*4
C     ..
C     .. Scalar Arguments ..
      INTEGER NumDerSites
      CHARACTER DerID*15
C     ..
C     .. External Subroutines ..
      EXTERNAL ccif_output_fmt,
     +          ccif_put_char,
     +           ccif_put_real,
     +            ccif_put_real_esd,
     +             ccif_release_context,
     +              ccif_setup_context
C     ..
C     .. Local Scalars ..
      INTEGER Jdo
      CHARACTER IDwork*6
C     ..
C     .. External Functions ..
      INTEGER Lenstr
      EXTERNAL Lenstr
C     ..
C
C
      IF ( .not. Harvest) RETURN
C
C
      CALL ccif_setup_context('_phasing_MIR_der_site.der_id',
     +                        CurrCategory,ccifBlockID,ccifContext,
     +                        ccifStatus,'loop')
C
C
      IF (MirDerSiteID .le. 0) THEN
        CALL ccif_output_fmt('_phasing_MIR_der_site.der_id','-',15,0,
     +                       'z',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_der_site.id','-',6,0,'z',
     +                       ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_der_site.atom_type_symbol',
     +                       '_',4,0,'z',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_der_site.fract_x',' ',10,3,
     +                       'f',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_der_site.fract_y',' ',10,3,
     +                       'f',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_der_site.fract_z',' ',10,3,
     +                       'f',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_der_site.B_iso',' ',8,3,'f',
     +                       ccifStatus)
        CALL ccif_output_fmt(
     +       '_phasing_MIR_der_site.Occupancy_iso',' ',
     +                       8,3,'f',ccifStatus)
        CALL ccif_output_fmt(
     +       '_phasing_MIR_der_site.Occupancy_iso_esd',
     +                       ' ',8,3,'f',ccifStatus)
        CALL ccif_output_fmt(
     +       '_phasing_MIR_der_site.Occupancy_anom',' ',
     +                       8,3,'f',ccifStatus)
        CALL ccif_output_fmt(
     +       '_phasing_MIR_der_site.Occupancy_anom_esd',
     +                       ' ',8,3,'f',ccifStatus)
      END IF
C
C
      DO 10 Jdo = 1,NumDerSites
        ccifStatus = AppendRow
        IF (lenstr(DerID).gt.0)
     +   CALL ccif_put_char('_phasing_MIR_der_site.der_id',
     +                     DerID(1:Lenstr(DerID)),ccifContext,
     +                     ccifStatus)
        ccifStatus = KeepContext
        MirDerSiteID = MirDerSiteID + 1
        WRITE (IDwork,FMT=6000) MirDerSiteID
 6000   FORMAT (i6)
        CALL ccif_put_char('_phasing_MIR_der_site.id',IDwork,
     +                     ccifContext,ccifStatus)
        ccifStatus = KeepContext
        IF (lenstr(Atype(Jdo)).gt.0)
     +   CALL ccif_put_char('_phasing_MIR_der_site.atom_type_symbol',
     +                     Atype(Jdo) (1:Lenstr(Atype(Jdo))),
     +                     ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real('_phasing_MIR_der_site.fract_x',X(Jdo),
     +                     ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real('_phasing_MIR_der_site.fract_y',Y(Jdo),
     +                     ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real('_phasing_MIR_der_site.fract_z',Z(Jdo),
     +                     ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real('_phasing_MIR_der_site.B_iso',B(Jdo),
     +                     ccifContext,ccifStatus)
        ccifStatus = KeepContext
C
C
      IF (Package(1:4) .eq. 'CCP4') THEN
        IF (OccEsd(Jdo) .lt. valueNotDet -1.0) THEN
          CALL ccif_put_real_esd(
     +      '_phasing_MIR_der_site.Occupancy_iso',
     +                           Occ(Jdo),OccEsd(Jdo),ccifContext,
     +                           ccifStatus,' ')
        ELSE
          CALL ccif_put_real(
     +      '_phasing_MIR_der_site.Occupancy_iso',
     +                       Occ(Jdo),ccifContext,ccifStatus)
        END IF
      END IF
C
C---- Need both value not defined and 'finite' function
C     for calculated values
C
        IF (Package(1:4) .eq. 'CCP4') THEN
         IF (AnomESD(Jdo) .lt. valueNotDet -1.0) THEN
          CALL ccif_put_real_esd(
     +      '_phasing_MIR_der_site.Occupancy_anom',
     +                           anom(Jdo),AnomEsd(Jdo),ccifContext,
     +                           ccifStatus,' ')
         ELSE
          CALL ccif_put_real(
     +      '_phasing_MIR_der_site.Occupancy_anom',
     +                       anom(Jdo),ccifContext,ccifStatus)
         END IF
        END IF
   10 CONTINUE
C
C
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C     =============================================================
      SUBROUTINE Hphasing_mir_native(R1,R2,SigmaNat,
     +                            fomT,fomC,fomA,
     +                            Mt,Mc,Ma)
C     =============================================================
C
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      REAL R1,R2,fomT,fomC,fomA,SigmaNat
      INTEGER Mt,Mc,Ma
C     ..
C     .. External Subroutines ..
      EXTERNAL ccif_output_fmt,
     +          ccif_put_char,
     +           ccif_put_real,
     +            ccif_put_real_esd,
     +             ccif_release_context,
     +              ccif_setup_context
C     ..
C     .. Local Scalars ..
      INTEGER Jdo
      CHARACTER Criteria*15
C     ..
C     .. External Functions ..
      INTEGER Lenstr
      EXTERNAL Lenstr
C     ..
C
C
      IF ( .not. Harvest) RETURN
      IF (Package(1:4) .eq. 'CCP4') THEN
       IF (R1 .eq. valueNotDet) RETURN
       IF (R2 .eq. valueNotDet) RETURN
      END IF
C             
C                   
C
      CALL ccif_setup_context('_phasing_MIR.entry_id',
     +                        CurrCategory,ccifBlockID,ccifContext,
     +                        ccifStatus,' ')
C
C
        CALL ccif_output_fmt(
     +           '_phasing_MIR.d_res_high',
     +           ' ',6,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +           '_phasing_MIR.d_res_low',
     +           ' ',6,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +           '_phasing_MIR.fom',
     +           ' ',8,3,'f',ccifStatus)
        CALL ccif_output_fmt(
     +           '_phasing_MIR.fom_centric',
     +           ' ',8,3,'f',ccifStatus)
        CALL ccif_output_fmt(
     +           '_phasing_MIR.fom_acentric',
     +           ' ',8,3,'f',ccifStatus)
        CALL ccif_output_fmt(
     +           '_phasing_MIR.reflns',
     +           ' ',8,0,'d',ccifStatus)
        CALL ccif_output_fmt(
     +           '_phasing_MIR.reflns_centric',
     +           ' ',8,0,'d',ccifStatus)
        CALL ccif_output_fmt(
     +           '_phasing_MIR.reflns_acentric',
     +           ' ',8,0,'d',ccifStatus)
C
C
        IF (R1 .lt. 0.55) R1 = 1.0/sqrt(R1)
        IF (R2 .lt. 0.55) R2 = 1.0/sqrt(R2)
      IF (R1 .gt. R2) THEN
        TMP = R1
        R1 = R2
        R2 = TMP
      END IF
      IF (SigmaNat .lt. valueNotDet -1.0)
     + WRITE (Criteria,6000)  SigmaNat
 6000   FORMAT('FP >',f3.1,' SIGFP')
C
C
      IF (lenstr(DName).gt.0)
     + CALL ccif_put_char('_phasing_MIR.entry_id',
     +                   DName(1:Lenstr(DName)),
     +                   ccifContext,ccifStatus)
      CALL ccif_put_real('_phasing_MIR.d_res_high',
     +                   R1,ccifContext,ccifStatus)
      CALL ccif_put_real('_phasing_MIR.d_res_low',
     +                   R2,ccifContext,ccifStatus)

      IF (SigmaNat .lt. valueNotDet -1.0 .and.
     +    SigmaNat .gt. 0.01)
     + CALL ccif_put_char('_phasing_MIR.reflns_criteria',
     +                   Criteria,ccifContext,ccifStatus)
C
C
      IF (Mt .ne. 0 .and. Mt .lt. IvalueNotDet -1) THEN
         CALL ccif_put_int('_phasing_MIR.reflns',
     +                     Mt,ccifContext,ccifStatus)
         CALL ccif_put_real('_phasing_MIR.fom',
     +                     fomT,ccifContext,ccifStatus)
      END IF
C
C
      IF (Mc .ne. 0 .and. Mc .lt. IvalueNotDet -1) THEN
          CALL ccif_put_int('_phasing_MIR.reflns_centric',
     +                      Mc,ccifContext,ccifStatus)
          CALL ccif_put_real('_phasing_MIR.fom_centric',
     +                      fomC,ccifContext,ccifStatus)
      END IF
C
C
      IF (Ma .ne. 0 .and. Ma .lt. IvalueNotDet -1) THEN
          CALL ccif_put_int('_phasing_MIR.reflns_acentric',
     +                       Ma,ccifContext,ccifStatus)
          CALL ccif_put_real('_phasing_MIR.fom_acentric',
     +                       fomA,ccifContext,ccifStatus)
      END IF
C
C
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C     =============================================================
      SUBROUTINE Hphasing_mir_native_shell(R1,R2,KRa,fomA,
     +                                     KRc,fomC,KRo,fomO)
C     =============================================================
C
C    _PHASING_MIR_NATIVE_SHELL
C            _phasing_MIR_shell.d_res_high
C            _phasing_MIR_shell.d_res_low
C            _phasing_MIR_shell.fom
C            _phasing_MIR_shell.fom_centric
C            _phasing_MIR_shell.fom_acentric
C            _phasing_MIR_shell.reflns
C            _phasing_MIR_shell.reflns_centric
C            _phasing_MIR_shell.reflns_acentric
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      REAL R1,R2,fomA,fomC,fomO
      INTEGER  KRa,KRc,KRo
C     ..
C     .. Scalar Arguments ..
      INTEGER NumShells
C     ..
C     .. External Subroutines ..
      EXTERNAL ccif_output_fmt,
     +          ccif_put_char,
     +           ccif_put_real,
     +            ccif_put_real_esd,
     +             ccif_release_context,
     +              ccif_setup_context
C     ..
C     .. Local Scalars ..
      INTEGER Jdo
      CHARACTER IDwork*6
      LOGICAL First
C     ..
C     .. External Functions ..
      INTEGER Lenstr
      EXTERNAL Lenstr
C     ..
      DATA First /.true./
C
C
      IF ( .not. Harvest) RETURN
      IF (Package(1:4) .eq. 'CCP4') THEN
        IF (R1 .eq. valueNotDet) RETURN
        IF (R2 .eq. valueNotDet) RETURN
      END IF
C
C
      CALL ccif_setup_context('_phasing_MIR_shell.d_res_high',
     +                        CurrCategory,ccifBlockID,ccifContext,
     +                        ccifStatus,'loop')
C
C
        IF (First) THEN
          First = .false.
        CALL ccif_output_fmt('_phasing_MIR_shell.d_res_high',
     +                       ' ',6,2,'f',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_shell.d_res_low',
     +                       ' ',6,2,'f',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_shell.fom',
     +                       ' ',8,3,'f',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_shell.fom_centric',
     +                       ' ',8,3,'f',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_shell.fom_acentric',
     +                       ' ',8,3,'f',ccifStatus)
        CALL ccif_output_fmt('_phasing_MIR_shell.reflns',
     +                       ' ',8,0,'d',ccifStatus)
        CALL ccif_output_fmt(
     +         '_phasing_MIR_shell.reflns_centric',
     +         ' ',8,0,'d',ccifStatus)
        CALL ccif_output_fmt(
     +         '_phasing_MIR_shell.reflns_acentric',
     +         ' ',8,0,'d',ccifStatus)
       END IF
C
C
      IF (R1 .gt. R2) THEN
        TMP = R1
        R1 = R2
        R2 = TMP
      END IF
C
C
        ccifStatus = AppendRow
        CALL ccif_put_real('_phasing_MIR_shell.d_res_high',
     +                     R1,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real('_phasing_MIR_shell.d_res_low',
     +                     R2,ccifContext,ccifStatus)
C
C
      IF (KRo .lt. IvalueNotDet -1 .and. KRo .gt. 0) THEN
        ccifStatus = KeepContext
        CALL ccif_put_int('_phasing_MIR_shell.reflns',
     +                    KRo,ccifContext,ccifStatus)
        ccifStatus = KeepContext
      IF (Package(1:4) .eq. 'CCP4') THEN
      IF (fomO .lt. valueNotDet -1.0) CALL ccif_put_real(
     +     '_phasing_MIR_shell.fom',fomO,
     +     ccifContext,ccifStatus)
      END IF
      END IF
C
C
      IF (KRc .lt. IvalueNotDet -1 .and. KRc .gt. 0) THEN
        ccifStatus = KeepContext
          CALL ccif_put_int(
     +         '_phasing_MIR_shell.reflns_centric',
     +         KRc,ccifContext,ccifStatus)
        ccifStatus = KeepContext
      IF (Package(1:4) .eq. 'CCP4') THEN
      IF (fomC .lt. valueNotDet -1.0) CALL ccif_put_real(
     +        '_phasing_MIR_shell.fom_centric',
     +        fomC,ccifContext,ccifStatus)
      END IF
      END IF
C
C
      IF (KRa .lt. IvalueNotDet -1 .and. KRa .gt. 0) THEN
           ccifStatus = KeepContext
           CALL ccif_put_int(
     +     '_phasing_MIR_shell.reflns_acentric',
     +      KRa,ccifContext,ccifStatus)
        ccifStatus = KeepContext
      IF (Package(1:4) .eq. 'CCP4') THEN
      IF (fomA .lt. valueNotDet -1.0) CALL ccif_put_real(
     +     '_phasing_MIR_shell.fom_acentric',
     +     fomA,ccifContext,ccifStatus)
      END IF
      END IF
C
C
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C        ====================================================
         SUBROUTINE Hrefine_NparamNrestNconstr(NPARMR,NRESTR,NCONSTR)
C        ====================================================
C
C _refine.ls_number_parameters       NPARMR
C _refine.ls_number_restraints       NRESTR
C _refine.ls_number_constraints      NCONSTR
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      INTEGER NPARMR,NRESTR,NCONSTR
C     ..
C     .. External Subroutines ..
      INTEGER Lenstr
      EXTERNAL ccif_output_fmt,
     +          ccif_put_real,
     +           ccif_release_context,
     +            ccif_setup_context
C     ..
C
C
      IF ( .not. Harvest) RETURN
C
C
C     
      CALL ccif_setup_context('REFINE',
     +                         CurrCategory,ccifBlockID,
     +                         ccifContext,ccifStatus,' ')
C
C
      ccifStatus = 1
      IF (NPARMR .gt. 0 .and. NPARMR .lt. IvalueNotDet -1)
     + CALL ccif_put_int('_refine.ls_number_parameters',
     +       NPARMR,ccifContext,ccifStatus)
      ccifStatus = 1
      IF (NRESTR .gt. 0 .and. NRESTR .lt. IvalueNotDet -1)
     + CALL ccif_put_int('_refine.ls_number_restraints',
     +      NRESTR,ccifContext,ccifStatus)
      ccifStatus = 1
      IF (NCONSTR .gt. 0 .and. NCONSTR .lt. IvalueNotDet -1)
     + CALL ccif_put_int('_refine.ls_number_constraints',
     +      NCONSTR,ccifContext,ccifStatus)
C
C
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C         ================================================
          SUBROUTINE Hrefine_corr_esu(Corr,FreeCorr,
     +                                DPI,FreeESU,
     +                                Good,GoodFree,
     +                                ESUml,bESU)
C         ================================================
C
C
C
C         _refine.Correlation_coeff_Fo_to_Fc  
C         _refine.Correlation_coeff_Fo_to_Fc_Free 
C         _refine.goodness_of_fit_work
C         _refine.goodness_of_fit_FreeR
C         _refine.Overall_ESU_ML
C         _refine.Overall_ESU_B
C         _refine.Overall_ESU_R_Cruickshanks_DPI  
C         _refine.Overall_ESU_Rfree
C
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      REAL Corr,FreeCorr,DPI,FreeESU,
     +     Good,GoodFree,ESUml,bESU
C     ..
C     .. External Subroutines ..
      INTEGER Lenstr
      EXTERNAL ccif_output_fmt,
     +          ccif_put_real,
     +           ccif_release_context,
     +            ccif_setup_context
C     ..
C
C
      IF ( .not. Harvest) RETURN
C
C
      CALL ccif_setup_context('REFINE',
     +                         CurrCategory,ccifBlockID,
     +                         ccifContext,ccifStatus,' ')
C
C
      ccifStatus = 1
      IF (Corr .lt. valueNotDet -1.0)
     + CALL ccif_put_real(
     +            '_refine.Correlation_coeff_Fo_to_Fc',
     +                   Corr,ccifContext,ccifStatus)
C
C
      ccifStatus = 1
      IF (FreeCorr .lt. valueNotDet -1.0)
     + CALL ccif_put_real(
     +            '_refine.Correlation_coeff_Fo_to_Fc_Free',
     +                   FreeCorr,ccifContext,ccifStatus)
C
C
      ccifStatus = 1
      IF (Good .lt. valueNotDet -1.0)
     + CALL ccif_put_real(
     +       '_refine.ls_goodness_of_fit_work',
     +                   Good,ccifContext,ccifStatus)
C
C
      ccifStatus = 1
      IF (GoodFree .lt. valueNotDet -1.0)
     + CALL ccif_put_real(
     +       '_refine.ls_goodness_of_fit_FreeR',
     +                   GoodFree,ccifContext,ccifStatus)
C
C
      ccifStatus = 1
      IF (ESUml .lt. valueNotDet -1.0)
     + CALL ccif_put_real(
     +       '_refine.Overall_ESU_ML',
     +                   ESUml,ccifContext,ccifStatus)
C
C
      ccifStatus = 1
      IF (bESU .lt. valueNotDet -1.0)
     + CALL ccif_put_real(
     +       '_refine.Overall_ESU_B',
     +                   bESU,ccifContext,ccifStatus)
C
C
      ccifStatus = 1
      IF (DPI .lt. valueNotDet -1.0)
     + CALL ccif_put_real(
     +      '_refine.Overall_ESU_R_Cruickshank_DPI',
     +                   DPI,ccifContext,ccifStatus)
C
C
      ccifStatus = 1
      IF (FreeESU .lt. valueNotDet -1.0)
     + CALL ccif_put_real(
     +      '_refine.Overall_ESU_R_free',
     +                   FreeESU,ccifContext,ccifStatus)
C
C
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C     ==========================================
      SUBROUTINE Hrefine_details(NHlines,Hlines)
C     ==========================================
C
C _refine.ls_weighting_details
C                                 Sigdel model of Konnert-Hendrickson:
C                                  Sigdel =
C                                  Afsig +  Bfsig*(sin(theta)/lambda-1/6)
C                                  Afsig = 22.0, Bfsig = 150.0
C                                    at the beginning of refinement.
C                                  Afsig = 16.0, Bfsig =  60.0
C                                    at the end of refinement.
C _refine.ls_weighting_scheme
C    _item_enumeration.detail      sigma  'based on measured e.s.d.'s'
C                                  unit   'unit or no weights applied'
C                                  calc   'calculated weights applied'
C                                  sparse     'selected elements only'
C
C    _refine.details
C
C
C
      include 'harvest.inc'
      include 'harderiv.inc'
C
C     .. Array Arguments ..
      INTEGER NHlines
      CHARACTER Hlines(NHlines)*80
C     ..
      CHARACTER Cwork*800
      CHARACTER Awork(MaxLines)*80
C     ..
C     .. External Subroutines ..
      INTEGER Lenstr
      EXTERNAL ccif_output_fmt,
     +          ccif_put_real,
     +           ccif_release_context,
     +            ccif_setup_context
C     ..
      EQUIVALENCE (Awork,Cwork)
C
C
      IF ( .not. Harvest) RETURN
C
C
      IF (NHlines .ge. 1) THEN
      CALL ccif_setup_context('_refine.details',
     +                         CurrCategory,ccifBlockID,
     +                         ccifContext,ccifStatus,' ')
C
C
        IF (NHlines .le. MaxLines) THEN
        DO 10 Jdo =1 ,NHlines
          Awork(Jdo) = Hlines(Jdo)
 10     CONTINUE
         ccifStatus = 1
         CALL ccif_put_text('_refine.details',
     +   NHlines,Cwork,10,ccifContext,ccifStatus,'NEW')
        GO TO 20
        ELSE
C
C--- more than 10 lines
C
        KK = 0 
        MM = 0
 30     CONTINUE
        KK = KK + 10
        MM = MM + 1
C
C
        IF (KK .le. NHlines) THEN
        LL = 0        
        DO 40 Jdo =KK-9 ,KK
          LL = LL + 1
          Awork(LL) = Hlines(Jdo)
 40     CONTINUE
C
C
         IF (MM .eq. 1) THEN
         ccifStatus = 1
         CALL ccif_put_text('_refine.details',
     +   10,Cwork,10,ccifContext,ccifStatus,'NEW')
         ELSE
         ccifStatus = 1
         CALL ccif_put_text('_refine.details',
     +   10,Cwork,10,ccifContext,ccifStatus,'  ')
         END IF
C
C
        GO TO 30
        ELSE
C
C---- last bit of lines
C
        KK = KK - 9
        LL = 0        
        DO 50 Jdo =KK ,NHlines
          LL = LL + 1
          Awork(LL) = Hlines(Jdo)
 50     CONTINUE
        KK = NHlines - KK + 1
         ccifStatus = 1
         CALL ccif_put_text('_refine.details',
     +   KK,Cwork,10,ccifContext,ccifStatus,' ')
        GO TO 20
        END IF
        END IF
C
C
 20     CONTINUE
C
C
      CALL ccif_release_context(ccifContext)
C
C
      END IF
C
C
      RETURN
      END
C
C     =========================================================
      SUBROUTINE Hrefine_fnmin(Nval,Nterms,Vterms,Cterms,Wterms)
C     =========================================================
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      REAL Vterms(*),Wterms(*)
      INTEGER Nterms(*)
      CHARACTER Cterms(Nval)*80
C     ..
C     .. Scalar Arguments ..
      INTEGER Nval
C     ..
C     .. External Subroutines ..
      EXTERNAL ccif_put_char,
     +          ccif_put_int,
     +           ccif_release_context,
     +            ccif_setup_context
C     ..
C     .. External Functions ..
      INTEGER Lenstr
      EXTERNAL Lenstr
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC nint
C     ..
C
      IF (.not. Harvest ) RETURN
      IF (Nval .le. 0) RETURN
C
C
      CALL ccif_setup_context('REFINE_FUNCT_MINIMISED',
     +                        CurrCategory,
     +                        ccifBlockID,
     +                        ccifContext,ccifStatus,'loop')
C
C
        CALL ccif_output_fmt(
     +       '_refine_funct_minimised.Residual',
     +                       ' ',16,5,'g',ccifStatus)
        CALL ccif_output_fmt(
     +       '_refine_funct_minimised.number_terms',
     +                       ' ',16,0,'d',ccifStatus)
        CALL ccif_output_fmt(
     +       '_refine_funct_minimised.type',
     +                       '-',40,0,'z',ccifStatus)
        CALL ccif_output_fmt(
     +       '_refine_funct_minimised.weight',
     +                       '-',6,3,'f',ccifStatus)
C
C
      Ntot = 0
      Rtot = 0.0
C
C
c Hrefine_fnmin(Nval,Nterms,Vterms,Cterms,Wterms)
      DO 90 Jdo = 1,Nval
        IF (Nterms(Jdo) .gt. 0 ) THEN
        ccifStatus = AppendRow
        IF (lenstr(Cterms(Jdo)).gt.0)
     +   CALL ccif_put_char('_refine_funct_minimised.type',
     +                     Cterms(Jdo)(1:Lenstr(Cterms(Jdo))),
     +                     ccifContext,ccifStatus)
        ccifStatus = KeepContext
          CALL ccif_put_int('_refine_funct_minimised.number_terms',
     +                        Nterms(Jdo),
     +                        ccifContext,ccifStatus)
        Ntot = Ntot + Nterms(Jdo)
        ccifStatus = KeepContext
          CALL ccif_put_real('_refine_funct_minimised.Residual',
     +                        Vterms(Jdo),
     +                        ccifContext,ccifStatus)
        Rtot = Rtot + Vterms(Jdo)
        IF (Wterms(Jdo).gt.0.0) then
        ccifStatus = KeepContext
          CALL ccif_put_real('_refine_funct_minimised.weight',
     +                        Wterms(Jdo),
     +                        ccifContext,ccifStatus)
         end if
        END IF
   90 CONTINUE
C
C
        ccifStatus = AppendRow
        CALL ccif_put_char('_refine_funct_minimised.type',
     +                     'Total_Function',
     +                     ccifContext,ccifStatus)
        ccifStatus = KeepContext
          CALL ccif_put_int('_refine_funct_minimised.number_terms',
     +                        Ntot,
     +                        ccifContext,ccifStatus)
        ccifStatus = KeepContext
          CALL ccif_put_real('_refine_funct_minimised.Residual',
     +                        Rtot,
     +                        ccifContext,ccifStatus)
C
C
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C        ===================================
         SUBROUTINE Hrefine_fom(fom,Freefom)
C        ===================================
C
C  _refine.ls_overall_FOM_work_Rset       
C  _refine.ls_overall_FOM_free_Rset  
C
C Fom(<cos(DelPhi)>
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      REAL fom,Freefom
C     ..
C     .. External Subroutines ..
      INTEGER Lenstr
      EXTERNAL ccif_output_fmt,
     +          ccif_put_real,
     +           ccif_release_context,
     +            ccif_setup_context
C     ..
C
C
      IF ( .not. Harvest) RETURN
C
C
C     
      CALL ccif_setup_context(
     +     '_refine.overall_FOM_free_R_set',
     +                         CurrCategory,ccifBlockID,
     +                         ccifContext,ccifStatus,' ')
C
C
      ccifStatus = 1
      IF (Freefom .lt. valueNotDet -1.0)
     + CALL ccif_put_real(
     +      '_refine.overall_FOM_free_R_set',
     +                   Freefom,ccifContext,ccifStatus)
      ccifStatus = 1
      IF (fom .lt. valueNotDet -1.0)
     + CALL ccif_put_real(
     +      '_refine.overall_FOM_work_R_set',
     +                   fom,ccifContext,ccifStatus)
C
C
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C       =========================================
        SUBROUTINE Hrefine_ls_matrix_type(STRING)
C       =========================================
C
C
C   _refine.ls_matrix_type
C    _item_enumeration.detail      full       'full'
C                                  fullcycle
C                                 'full with fixed elements per cycle'
C                                  atomblock  'block diagonal per atom'
C                                  userblock  'user-defined blocks'
C                                  diagonal   'diagonal elements only'
C       '  Method of minimisation  : Sparse Matrix '
C       '  Method of minimisation  : Conjugate Gradient'
C       '  Method of minimisation  : Conjugate Direction'
C
C
      include 'harvest.inc'
C
C
C     .. Scalar Arguments ..
      CHARACTER String*(*)
C     ..
      INTEGER Lenstr
C     ..
C
C
      IF ( .not. Harvest) RETURN
C
C
C
      IF (Lenstr(String) .gt. 1) THEN
      CALL ccif_setup_context('REFINE',
     +                        CurrCategory,ccifBlockID,ccifContext,
     +                        ccifStatus,' ')
      ccifStatus = 1
      CALL ccif_put_char('_refine.ls_matrix_type',
     +                   String(1:Lenstr(String)),
     +                   ccifContext,ccifStatus)
      CALL ccif_release_context(ccifContext)
      END IF
C
C
      RETURN
      END
C
C
C
C       =================================================
        SUBROUTINE Hrefine_ls_overall_reso(R1,R2)
C       =================================================
C
C
C     _refine.ls_d_res_high
C     _refine.ls_d_res_low
C
      include 'harvest.inc'
C
C
C     .. Array Arguments ..
      REAL R1,R2
C     ..
C     .. External Subroutines ..
      INTEGER Lenstr
      EXTERNAL ccif_output_fmt,
     +          ccif_put_real,
     +           ccif_release_context,
     +            ccif_setup_context
C     ..
C
C
      IF ( .not. Harvest) RETURN
C
C
      CALL ccif_setup_context('REFINE',
     +                         CurrCategory,ccifBlockID,
     +                         ccifContext,ccifStatus,' ')
C
C
      CALL ccif_output_fmt('_refine.ls_d_res_high',
     +    ' ',6,3,'f',ccifStatus)
      CALL ccif_output_fmt('_refine.ls_d_res_low',
     +    ' ',6,3,'f',ccifStatus)
C
C
      ccifStatus = 1
      CALL ccif_put_real('_refine.ls_d_res_low',
     +                   R1,ccifContext,ccifStatus)
      ccifStatus = 1
      CALL ccif_put_real('_refine.ls_d_res_high',
     +                   R2,ccifContext,ccifStatus)
C
C
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C     ============================
      SUBROUTINE Hrefine_obscriteria(Criteria)
C     ============================
C
C
C   _reflns.observed_criterion  
C
      include 'harvest.inc'
C
C
C
C     .. Array Arguments ..
      REAL Criteria
C     ..
      CHARACTER SigmaLine*80
C     .. External Subroutines ..
      INTEGER Lenstr
      EXTERNAL ccif_output_fmt,
     +          ccif_put_real,
     +           ccif_release_context,
     +            ccif_setup_context
C     ..
C
C
      IF ( .not. Harvest) RETURN


      IF (Criteria .gt. 0.1) THEN
C
C
      CALL ccif_setup_context('REFLNS',
     +                         CurrCategory,ccifBlockID,
     +                         ccifContext,ccifStatus,' ')
C
C
       IF (lenstr(SigmaLine).gt.0) THEN
       WRITE(SigmaLine,6000) Criteria
 6000  FORMAT('Fobs >=  ',F5.2,'  *standard deviations')
       ccifStatus = 1
       CALL ccif_put_char('_reflns.observed_criterion',
     +   SigmaLine(1:Lenstr(SigmaLine)),ccifContext,ccifStatus)
        CALL ccif_release_context(ccifContext)
       END IF
      END IF
C
C
      RETURN
      END
C
C
C       ==================================================
        SUBROUTINE Hrefine_restraints(Rtype,Num,rmsd,sigd,
     +                                criteria,Nreject)
C       ==================================================
C
C
C    _refine_ls_restr.type
C    _refine_ls_restr.dev_ideal_target
C    _refine_ls_restr.dev_ideal
C    _refine_ls_restr.number
C    _refine_ls_restr.criterion
C    _refine_ls_restr.rejects
C
C    _refine_ls_restr.type   line
C  The type of the parameter being restrained.
C   An explicit set of data values are provided for
C  programs Protin/ Prolsq (beginning with p_) and X-plor (beginning with
C  x_).  As computer programs will evolve, these data values are given as
C  examples, and not as an enumeration list. Computer programs converting
C  a data block to a refinement table will expect the exact form of the
C  data values given here to be used.
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      CHARACTER Rtype*(*),criteria*(*)
      INTEGER   Num,Nreject
      REAL rmsd,sigd
C     ..
C     .. Local Scalars ..
      INTEGER Jdo
      CHARACTER IDwork*6
      LOGICAL First
C     ..
C     .. External Functions ..
      INTEGER Lenstr
      EXTERNAL Lenstr
C     ..
      DATA First /.true./
C
C
      IF ( .not. Harvest) RETURN
C
C    
        IF (First) THEN
          First = .false.
        CALL ccif_output_fmt('_refine_ls_restr.type',
     +                       '-',61,0,'z',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_restr.dev_ideal_target',
     +                       '-',6,3,'f',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_restr.dev_ideal',
     +                       '-',6,3,'f',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_restr.number',
     +                       '-',8,0,'d',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_restr.criterion',
     +                       '-',20,0,'z',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_restr.rejects',
     +                       '-',8,0,'d',ccifStatus)
ccx        CALL ccif_output_fmt('_refine_ls_restr.ebi_rmsdiffs',
ccx     +                       '-',6,3,'f',ccifStatus)
      END IF
C
C
         CALL ccif_setup_context('REFINE_LS_RESTR',
     +                         CurrCategory,
     +                         ccifBlockID,ccifContext,
     +                         ccifStatus,'loop')
C
C
        ccifStatus = AppendRow
        IF (lenstr(RType).gt.0)
     +   CALL ccif_put_char('_refine_ls_restr.type',
     +                     RType(1:Lenstr(RType)),
     +                     ccifContext,ccifStatus)
        ccifStatus = KeepContext
        IF (Num .lt. IvalueNotDet -1 .and. Num .gt. 0)
     +  CALL ccif_put_int('_refine_ls_restr.number',
     +                     Num,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        IF (rmsd .lt. valueNotDet -1.0)
     +  CALL ccif_put_real('_refine_ls_restr.dev_ideal',
     +                     rmsd,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        IF (sigd .lt. valueNotDet -1.0)
     +  CALL ccif_put_real('_refine_ls_restr.dev_ideal_target',
     +                     sigd,ccifContext,ccifStatus)
C
C
        IF (Nreject .lt. IvalueNotDet -1 .and. 
     +      Nreject .gt. 0) THEN
        ccifStatus = KeepContext
          CALL ccif_put_int('_refine_ls_restr.rejects',
     +                     Nreject,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        IF (lenstr(criteria).gt.1) 
     +   CALL ccif_put_char('_refine_ls_restr.criterion',
     +                     criteria(1:Lenstr(criteria)),
     +                     ccifContext,ccifStatus)
         END IF
C
C
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C       ==========================================================
        SUBROUTINE Hrefine_rfacts(Nall,Nobs,Nmiss,Nwork,Nfree,
     +                            NfreeMiss,PercentObs,PercentFree,
     +                            Rall,Robs,Rwork,Rfree,
     +                            Wall,Wobs,Wwork,Wfree)
C       ==========================================================
C
C
C---- harvest
C
C       _refine.ls_number_reflns_all
C       _refine.ls_number_reflns_obs
C       _refine.ls_number_reflns_missing      ****
C       _refine.ls_number_reflns_R_work
C       _refine.ls_number_reflns_R_free
C       _refine_ls_number_reflns_free_missing ****
C       _refine.ls_percent_reflns_obs
C       _refine.ls_percent_reflns_R_free
C       _refine.ls_R_factor_all
C       _refine.ls_R_factor_obs
C       _refine.ls_R_factor_R_work
C       _refine.ls_R_factor_R_free
C       _refine.ls_wR_factor_all
C       _refine.ls_wR_factor_obs
C       _refine.ls_wR_factor_R_work
C       _refine.ls_wR_factor_R_free
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      INTEGER Nall,Nobs,Nmiss,Nwork,Nfree,
     +                            NfreeMiss
      REAL PercentObs,PercentFree,
     +                            Rall,Robs,Rwork,Rfree,
     +                            Wall,Wobs,Wwork,Wfree
C     ..
C     .. External Subroutines ..
      INTEGER Lenstr
      EXTERNAL ccif_output_fmt,
     +          ccif_put_real,
     +           ccif_release_context,
     +            ccif_setup_context
C     ..
C
C
      IF ( .not. Harvest) RETURN
C
C
      CALL ccif_setup_context('REFINE',
     +                         CurrCategory,ccifBlockID,
     +                         ccifContext,ccifStatus,' ')
C
C
      CALL ccif_output_fmt('_refine.ls_number_reflns_all',
     +    ' ',8,0,'d',ccifStatus)
      CALL ccif_output_fmt('_refine.ls_number_reflns_obs',
     +    ' ',8,0,'d',ccifStatus)
      CALL ccif_output_fmt('_refine.ls_number_reflns_R_work',
     +    ' ',8,0,'d',ccifStatus)
      CALL ccif_output_fmt('_refine.ls_number_reflns_R_free',
     +    ' ',6,0,'d',ccifStatus)
C
C
      CALL ccif_output_fmt('_refine.ls_R_factor_all',
     +             ' ',3,3,'f',ccifStatus)
      CALL ccif_output_fmt('_refine.ls_R_factor_obs',
     +             ' ',3,3,'f',ccifStatus)
      CALL ccif_output_fmt('_refine.ls_R_factor_R_work',
     +             ' ',3,3,'f',ccifStatus)
      CALL ccif_output_fmt('_refine.ls_R_factor_R_free',
     +             ' ',3,3,'f',ccifStatus)
C
C
      CALL ccif_output_fmt('_refine.ls_wR_factor_all',
     +             ' ',3,3,'f',ccifStatus)
      CALL ccif_output_fmt('_refine.ls_wR_factor_obs',
     +             ' ',3,3,'f',ccifStatus)
      CALL ccif_output_fmt('_refine.ls_wR_factor_R_work',
     +             ' ',3,3,'f',ccifStatus)
      CALL ccif_output_fmt('_refine.ls_wR_factor_R_free',
     +             ' ',3,3,'f',ccifStatus)
C
C
      CALL ccif_output_fmt('_refine.ls_percent_reflns_obs',
     +             ' ',3,3,'f',ccifStatus)
      CALL ccif_output_fmt('_refine.ls_percent_reflns_R_free',
     +             ' ',3,3,'f',ccifStatus)
C
C
      ccifStatus = 1
      IF (Nall .lt. IvalueNotDet -1 .and. Nall .gt. 0)
     + CALL ccif_put_int('_refine.ls_number_reflns_all',
     +                   Nall,ccifContext,ccifStatus)
      ccifStatus = 1
      IF (Nobs .lt. IvalueNotDet -1 .and. Nobs .gt. 0)
     + CALL ccif_put_int('_refine.ls_number_reflns_obs',
     +                   Nobs,ccifContext,ccifStatus)
      ccifStatus = 1
      IF (Nwork .lt. IvalueNotDet -1 .and. Nwork .gt. 0)
     + CALL ccif_put_int('_refine.ls_number_reflns_R_work',
     +                   Nwork,ccifContext,ccifStatus)
      ccifStatus = 1
      IF (Nfree .lt. IvalueNotDet -1 .and. Nfree .gt. 0)
     + CALL ccif_put_int('_refine.ls_number_reflns_R_free',
     +                   Nfree,ccifContext,ccifStatus)
C
C
      ccifStatus = 1
      IF (Rall .lt. valueNotDet -1.0)
     + CALL ccif_put_real('_refine.ls_R_factor_all',
     +                   Rall,ccifContext,ccifStatus)
      ccifStatus = 1
      IF (Robs .lt. valueNotDet -1.0)
     + CALL ccif_put_real('_refine.ls_R_factor_obs',
     +                   Robs,ccifContext,ccifStatus)
      ccifStatus = 1
      IF (Rwork .lt. valueNotDet -1.0)
     + CALL ccif_put_real('_refine.ls_R_factor_R_work',
     +                   Rwork,ccifContext,ccifStatus)
      ccifStatus = 1
      IF (Rfree .lt. valueNotDet -1.0)
     + CALL ccif_put_real('_refine.ls_R_factor_R_free',
     +                   Rfree,ccifContext,ccifStatus)
C
C 
      ccifStatus = 1
      IF (Wall .lt. valueNotDet -1.0)
     + CALL ccif_put_real('_refine.ls_wR_factor_all',
     +                   Wall,ccifContext,ccifStatus)
      ccifStatus = 1
      IF (Wobs .lt. valueNotDet -1.0)
     + CALL ccif_put_real('_refine.ls_wR_factor_obs',
     +                   Wobs,ccifContext,ccifStatus)
      ccifStatus = 1
      IF (Wwork .lt. valueNotDet -1.0)
     + CALL ccif_put_real('_refine.ls_wR_factor_R_work',
     +                   Wwork,ccifContext,ccifStatus)
      ccifStatus = 1
      IF (Wfree .lt. valueNotDet -1.0)
     + CALL ccif_put_real('_refine.ls_wR_factor_R_free',
     +                   Wfree,ccifContext,ccifStatus)
C
C
      ccifStatus = 1
      IF (PercentObs .lt. valueNotDet -1.0) THEN
         CALL ccif_put_real('_refine.ls_percent_reflns_obs',
     +                   PercentObs,ccifContext,ccifStatus)
      ELSE IF (Nmiss .lt. IvalueNotDet -1 .and.
     +         Nwork .lt. IvalueNotDet -1) THEN
        IF (Nmiss + Nwork .gt. 0) THEN
        PP = 100.0 * (Nwork / (Nmiss + Nwork ))
        CALL ccif_put_real('_refine.ls_percent_reflns_obs',
     +                      PP,ccifContext,ccifStatus)
        END IF
      END IF
C
C
      ccifStatus = 1
      IF (PercentFree .lt. valueNotDet -1.0) THEN
        CALL ccif_put_real('_refine.ls_percent_reflns_R_free',
     +                   PercentFree,ccifContext,ccifStatus)
      ELSE IF (NfreeMiss .lt. IvalueNotDet -1 .and.
     +         Nfree     .lt. IvalueNotDet -1) THEN
        IF (Nfree + NfreeMiss .gt. 0) THEN
        PP = 100.0 * (Nfree / (Nfree + NfreeMiss))
        CALL ccif_put_real('_refine.ls_percent_reflns_R_free',
     +                      PP,ccifContext,ccifStatus)
        END IF
      END IF
C
C
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C       ==========================================================
        SUBROUTINE Hrefine_rg(RDhigh,RDlow,RGall,RGwork,RGfree)
C       ==========================================================
C
C
C---- harvest
C
c       _refine_analyze.RG_d_res_high
c       _refine_analyze.RG_d_res_low
C       _refine_analyze.RG_all 
C       _refine_analyze.RG_work   HVRG
C       _refine_analyze.RG_free   HVGFREE
C       _refine_analyze.RG_work_free_ratio
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      REAL RGall,RGwork,RGfree,RDhigh,RDlow
C     ..
C     .. External Subroutines ..
      INTEGER Lenstr
      EXTERNAL ccif_output_fmt,
     +          ccif_put_real,
     +           ccif_release_context,
     +            ccif_setup_context
C     ..
C
C
      IF ( .not. Harvest) RETURN
C
C
      CALL ccif_setup_context('REFINE_ANALYZE',
     +                         CurrCategory,ccifBlockID,
     +                         ccifContext,ccifStatus,' ')
C
C
      CALL ccif_output_fmt('_refine_analyze.RG_d_res_high',
     +             ' ',3,3,'f',ccifStatus)
      CALL ccif_output_fmt('_refine_analyze.RG_d_res_low',
     +             ' ',3,3,'f',ccifStatus)
      CALL ccif_output_fmt('_refine_analyze.RG_all',
     +             ' ',3,3,'f',ccifStatus)
      CALL ccif_output_fmt('_refine_analyze.RG_work',
     +             ' ',3,3,'f',ccifStatus)
      CALL ccif_output_fmt('_refine_analyze.RG_free',
     +             ' ',3,3,'f',ccifStatus)
ccx      CALL ccif_output_fmt('_refine_analyze.RG_free_work_ratio',
ccx     +             ' ',3,3,'f',ccifStatus)
C
C
      NN = 0
      ccifStatus = 1
      IF (RDhigh .lt. valueNotDet -1.0)
     + CALL ccif_put_real('_refine_analyze.RG_d_res_high',
     +                   RDhigh,ccifContext,ccifStatus)
      ccifStatus = 1
      IF (RDlow .lt. valueNotDet -1.0)
     + CALL ccif_put_real('_refine_analyze.RG_d_res_low',
     +                   RDlow,ccifContext,ccifStatus)
      ccifStatus = 1
      IF (RGall .lt. valueNotDet -1.0)
     + CALL ccif_put_real('_refine_analyze.RG_all',
     +                   RGall,ccifContext,ccifStatus)
      ccifStatus = 1
      IF (RGwork .lt. valueNotDet -1.0) THEN
         CALL ccif_put_real('_refine_analyze.RG_work',
     +                   RGwork,ccifContext,ccifStatus)
       NN = NN + 1
      END IF
C
C
      ccifStatus = 1
      IF (RGfree .lt. valueNotDet -1.0) THEN
         CALL ccif_put_real('_refine_analyze.RG_free',
     +                   RGfree,ccifContext,ccifStatus)
       NN = NN + 1
       END IF
C
C       _refine.ls_RG_work_free_ratio
C
ccx       IF (NN .eq. 2) THEN
ccx       ccifStatus = 1
ccx       Rrat = RGfree/RGwork
ccx         CALL ccif_put_real('_refine_analyze.RG_free_work_ratio',
ccx     +                   Rrat,ccifContext,ccifStatus)
ccx       END IF
C
C 
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C       ==================================================
        SUBROUTINE Hrefine_rmsobs2dict(Rtype,Clow,Chigh,Usigma)
C       ==================================================
C
cloop_
c_refine_ls_restr_type.type
c_refine_ls_restr_type.distance_cutoff_low
c_refine_ls_restr_type.distance_cutoff_high
c_refine_ls_restr_type.U_sigma_weights
c 'RESTRAIN_Distances < 2.12'                              .     2.12       .
c 'RESTRAIN_Distances 2.12 < D < 2.625'                    2.12  2.625      .
c 'RESTRAIN_Distances > 2.625'                             2.625 .          .
c 'RESTRAIN_Peptide Planes'                                .     .          .
c 'RESTRAIN_Ring and other planes'                         .     .          .
c 'RESTRAIN_r.m.s. diffs for Uiso atoms at dist 1.2-1.4'   1.2   1.4        1.800
c 'RESTRAIN_r.m.s. diffs for Uiso atoms at dist 1.4-1.6'   1.4   1.6        1.800
c 'RESTRAIN_r.m.s. diffs for Uiso atoms at dist 1.8-2.0'   1.8   2.0        1.800
c 'RESTRAIN_r.m.s. diffs for Uiso atoms at dist 2.0-2.2'   2.0   2.2        1.800
c 'RESTRAIN_r.m.s. diffs for Uiso atoms at dist 2.2-2.4'   2.2   2.4        1.800
c 'RESTRAIN_r.m.s. diffs for Uiso atoms at dist >2.4'      2.4   .          1.800
c;
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      CHARACTER Rtype*(*)
      REAL Clow,Chigh,Usigma
C     ..
C     .. Local Scalars ..
      INTEGER Jdo
      CHARACTER IDwork*6
      LOGICAL First
C     ..
C     .. External Functions ..
      INTEGER Lenstr
      EXTERNAL Lenstr
C     ..
      DATA First /.true./
C
C
      IF ( .not. Harvest) RETURN
C
C    
        IF (First) THEN
          First = .false.
        CALL ccif_output_fmt('_refine_ls_restr_type.type',
     +                       '-',61,0,'z',ccifStatus)
        CALL ccif_output_fmt(
     +       '_refine_ls_restr_type.distance_cutoff_low',
     +                       '-',6,3,'f',ccifStatus)
        CALL ccif_output_fmt(
     +       '_refine_ls_restr_type.distance_cutoff_high',
     +                       '-',6,3,'f',ccifStatus)
        CALL ccif_output_fmt(
     +       '_refine_ls_restr_type.U_sigma_weights',
     +                       '-',6,3,'f',ccifStatus)
      END IF
C
C
         CALL ccif_setup_context('REFINE_LS_RESTR_TYPE',
     +                         CurrCategory,
     +                         ccifBlockID,ccifContext,
     +                         ccifStatus,'loop')
C
C
        ccifStatus = AppendRow
        IF (lenstr(RType).gt.0)
     +   CALL ccif_put_char('_refine_ls_restr_type.type',
     +                     RType(1:Lenstr(RType)),
     +                     ccifContext,ccifStatus)
        ccifStatus = KeepContext
        IF (Clow .gt. 0.0)
     +  CALL ccif_put_real(
     +       '_refine_ls_restr_type.distance_cutoff_low',
     +                     Clow,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        IF (Chigh .gt.0.0)
     +  CALL ccif_put_real(
     +       '_refine_ls_restr_type.distance_cutoff_high',
     +                     Chigh,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        IF (Usigma .lt. valueNotDet -1.0)
     +  CALL ccif_put_real(
     +       '_refine_ls_restr_type.U_sigma_wghts',
     +                     Usigma,ccifContext,ccifStatus)
        CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C      ========================================================
       SUBROUTINE Hrefine_shell(Rlow,Rhigh,
     +                          NrefAll,NrefObs,Nmiss,NrefWork,
     +                          PercentObs,RfacAll,RfacObs,
     +                          Nfree,Rfree,Rwork,
     +                          WgtRfac,WgtRobs,WgtRwork,
     +                          WgtUsed,Wexpress)
C      ========================================================
C
C
C    _refine_ls_shell.d_res_high                 = Rhigh
C    _refine_ls_shell.d_res_low                  = Rlow
C    _refine_ls_shell.number_reflns_all          = Nref if NO Rfree
C                                                = Nref + Nfree
C    _refine_ls_shell.number_reflns_obs          ** refmac doesnt use 
C                                                   an omit/reject/exclude 
C                                                   on SigF
C    _refine_ls_shell.number_reflns_R_work       = Nref
C    _refine_ls_shell.percent_reflns_obs         = 100.0 * Nref/(Nmiss+Nref)
C
C    _refine_ls_shell.R_factor_all               ** if Rfree Not present
C                                                = Rfac if no Rfree
C    _refine_ls_shell.R_factor_obs               ** not in refmac 
C    _refine_ls_shell.number_reflns_R_free       = Nfree
C    _refine_ls_shell.R_factor_R_free            = Rfree
C    _refine_ls_shell.R_factor_R_work            = Rfac if Rfree
C    _refine_ls_shell.wR_factor_all              = WgtRfac if no Rfree
C    _refine_ls_shell.wR_factor_obs              ** not in refmac 
C    _refine_ls_shell.wR_factor_R_work           = WgtRfac if Rfree
C
C**** NEW TO mmcif
C      _refine_ls_shell.weight_used              Awgt
C      _refine_ls_shell.weight_exp               Asig
C
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      REAL Rlow,Rhigh,PercentObs,RfacObs,RfacAll,
     +     Rfree,Rwork,WgtRfac,WgtRobs,WgtRwork,
     +     Wgtused,Wexpress
      INTEGER  NrefAll,NrefObs,Nmiss,NrefWork,
     +         Nfree
C     ..
C     .. Local Scalars ..
      INTEGER Jdo
      CHARACTER IDwork*6
      LOGICAL First
C     ..
C     .. External Functions ..
      INTEGER Lenstr
      EXTERNAL Lenstr
C     ..
      DATA First /.true./
C
C
      IF ( .not. Harvest) RETURN
C
C    
      CALL ccif_setup_context('REFINE_LS_SHELL',
     +                        CurrCategory,ccifBlockID,ccifContext,
     +                        ccifStatus,'loop')
C
C
        IF (First) THEN
          First = .false.
        CALL ccif_output_fmt('_refine_ls_shell.d_res_high',
     +                       ' ',6,2,'f',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_shell.d_res_low',
     +                       ' ',6,2,'f',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_shell.number_reflns_all',
     +                       ' ',8,0,'d',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_shell.number_reflns_obs',
     +                       ' ',8,0,'d',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_shell.number_reflns_R_work',
     +                       ' ',8,0,'d',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_shell.percent_reflns_obs',
     +                       ' ',8,0,'d',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_shell.R_factor_obs',
     +                       ' ',2,3,'f',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_shell.R_factor_all',
     +                       ' ',2,3,'f',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_shell.R_factor_R_work',
     +                       ' ',2,3,'f',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_shell.number_reflns_R_free',
     +                       ' ',6,0,'d',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_shell.R_factor_R_free',
     +                       ' ',2,3,'f',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_shell.wR_factor_all',
     +                       ' ',2,3,'f',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_shell.wR_factor_obs',
     +                       ' ',2,3,'f',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_shell.wR_factor_R_work',
     +                       ' ',2,3,'f',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_shell.weight_used',
     +                       ' ',3,3,'f',ccifStatus)
        CALL ccif_output_fmt('_refine_ls_shell.weight_exp',
     +                       ' ',3,3,'f',ccifStatus)
       END IF
C
C
        ccifStatus = AppendRow
        CALL ccif_put_real('_refine_ls_shell.d_res_high',
     +                     Rhigh,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real('_refine_ls_shell.d_res_low',
     +                     Rlow,ccifContext,ccifStatus)
C
C
        IF (Nfree .lt. IValueNotDet -1) THEN
         NN = NrefWork + Nfree
        ELSE
         NN = NrefAll
        END IF
        ccifStatus = KeepContext
        IF (NN .lt. IvalueNotDet -1)
     +  CALL ccif_put_int('_refine_ls_shell.number_reflns_all',
     +                     NN,ccifContext,ccifStatus)
C
C
        IF (NrefObs .lt. IValueNotDet -1) THEN
          ccifStatus = KeepContext
          CALL ccif_put_int('_refine_ls_shell.number_reflns_obs',
     +                       NrefObs,ccifContext,ccifStatus)
        END IF
C
C
        IF (NrefWork .lt. IValueNotDet -1) THEN
          ccifStatus = KeepContext
          CALL ccif_put_int('_refine_ls_shell.number_reflns_R_work',
     +                       NrefWork,ccifContext,ccifStatus)
        END IF
C
C
        IF (PercentObs .lt. valueNotDet -1.0) THEN
          NPP = nint(PercentObs)
          ccifStatus = KeepContext
          CALL ccif_put_int('_refine_ls_shell.percent_reflns_obs',
     +                       NPP,ccifContext,ccifStatus)
        ELSE IF (Nmiss .lt. IvalueNotDet -1 .and. 
     +           NrefWork .lt. IvalueNotDet -1) THEN
          PP = 100.0 * REAL( NrefWork/(Nmiss + NrefWork) )
          ccifStatus = KeepContext
          CALL ccif_put_real('_refine_ls_shell.percent_reflns_obs',
     +                       PP,ccifContext,ccifStatus)
        END IF
C
C
        IF (RfacObs .lt. valueNotDet -1.0) THEN
          ccifStatus = KeepContext
          CALL ccif_put_real('_refine_ls_shell.R_factor_obs',
     +                       RfacObs,ccifContext,ccifStatus)
        END IF
C
C
        IF (RfacAll .lt. valueNotDet -1.0) THEN
          ccifStatus = KeepContext
          CALL ccif_put_real('_refine_ls_shell.R_factor_all',
     +                       RfacAll,ccifContext,ccifStatus)
        END IF
C
C
        IF (Rwork .lt. valueNotDet -1.0) THEN
          ccifStatus = KeepContext
          CALL ccif_put_real('_refine_ls_shell.R_factor_R_work',
     +                       Rwork,ccifContext,ccifStatus)
        END IF
C
C
        IF (Nfree .lt. IvalueNotDet -1) THEN
          ccifStatus = KeepContext
          CALL ccif_put_int('_refine_ls_shell.number_reflns_R_free',
     +                       Nfree,ccifContext,ccifStatus)
        END IF
C
C
        IF (Rfree .lt. valueNotDet -1.0) THEN
          ccifStatus = KeepContext
          CALL ccif_put_real('_refine_ls_shell.R_factor_R_free',
     +                       Rfree,ccifContext,ccifStatus)
        END IF
C
C
        IF (WgtRwork .lt. valueNotDet -1.0) THEN
          ccifStatus = KeepContext
          CALL ccif_put_real('_refine_ls_shell.wR_factor_R_work',
     +                       WgtRwork,ccifContext,ccifStatus)
        END IF
     +                          
C
C
        IF (WgtRfac .lt. valueNotDet -1.0) THEN
          ccifStatus = KeepContext
          CALL ccif_put_real('_refine_ls_shell.wR_factor_all',
     +                       WgtRfac,ccifContext,ccifStatus)
        END IF
C
C
        IF (WgtRobs .lt. valueNotDet -1.0) THEN
          ccifStatus = KeepContext
          CALL ccif_put_real('_refine_ls_shell.wR_factor_obs',
     +                       WgtRobs,ccifContext,ccifStatus)
        END IF
C
C
        IF (WgtUsed .lt. valueNotDet -1.0) THEN
          ccifStatus = KeepContext
          CALL ccif_put_real('_refine_ls_shell.weight_used',
     +                       WgtUsed,ccifContext,ccifStatus)
        END IF
C
C
        IF (Wexpress .lt. valueNotDet -1.0) THEN
          ccifStatus = KeepContext
          CALL ccif_put_real('_refine_ls_shell.weight_exp',
     +                       Wexpress,ccifContext,ccifStatus)
        END IF
C
C


      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C     ==========================================
      SUBROUTINE Hrefine_solvent_model(NHlines,Hlines)
C     ==========================================
C
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      INTEGER NHlines
      CHARACTER Hlines(NHlines)*80
C     ..
      CHARACTER Cwork*800
      CHARACTER Awork(MaxLines)*80
C     ..
C     .. External Subroutines ..
      INTEGER Lenstr
      EXTERNAL ccif_output_fmt,
     +          ccif_put_real,
     +           ccif_release_context,
     +            ccif_setup_context
C     ..
      EQUIVALENCE (Awork,Cwork)
C
C
      IF ( .not. Harvest) RETURN
C
C
      IF (NHlines .ge. 1) THEN
      CALL ccif_setup_context('REFINE',
     +                         CurrCategory,ccifBlockID,
     +                         ccifContext,ccifStatus,' ')
C
C
        IF (NHlines .le. MaxLines) THEN
        DO 10 Jdo =1 ,NHlines
          Awork(Jdo) = Hlines(Jdo)
 10     CONTINUE
         ccifStatus = 1
         CALL ccif_put_text('_refine.solvent_model_details',
     +   NHlines,Cwork,10,ccifContext,ccifStatus,'NEW')
        GO TO 20
        ELSE
        KK = 0 
        MM = 0
 30     CONTINUE
        KK = KK + 10
        MM = MM + 1
C
C
        IF (KK .le. NHlines) THEN
        LL = 0        
        DO 40 Jdo =KK-9 ,KK
          LL = LL + 1
          Awork(LL) = Hlines(Jdo)
 40     CONTINUE
C
C
         IF (MM .eq. 1) THEN
         ccifStatus = 1
         CALL ccif_put_text('_refine.solvent_model_details',
     +   10,Cwork,10,ccifContext,ccifStatus,'NEW')
         ELSE
         ccifStatus = 1
         CALL ccif_put_text('_refine.solvent_model_details',
     +   10,Cwork,10,ccifContext,ccifStatus,'  ')
         END IF
C
C
        GO TO 30
        ELSE
C
C--- last bit of lines
C
        KK = KK - 9
        LL = 0        
        DO 50 Jdo =KK ,NHlines
          LL = LL + 1
          Awork(LL) = Hlines(Jdo)
 50     CONTINUE
        KK = NHLines - KK + 1
         ccifStatus = 1
         CALL ccif_put_text('_refine.solvent_model_details',
     +   KK,Cwork,10,ccifContext,ccifStatus,' ')
        GO TO 20
        END IF
        END IF
C
C
 20     CONTINUE
C
C
      CALL ccif_release_context(ccifContext)
C
C
      END IF
C
C
      RETURN
      END
C
C
C     ============================================================
      SUBROUTINE Hrefine_wghtScheme(WghtScheme)
C     ============================================================
C
C
      include 'harvest.inc'
C
C     .. Scalar Arguments ..
      REAL VALND
      INTEGER IVND,RowLimit
      LOGICAL Private,UseCWD
      CHARACTER WghtScheme* (*)
C     ..
C     .. External Functions ..
      INTEGER Lenstr
C     ..
C
C
      IF (.not. Harvest) RETURN
C
C
      IF (Lenstr(WghtScheme) .gt. 1) THEN
      CALL ccif_setup_context('REFINE',
     +                        CurrCategory,ccifBlockID,
     +                        ccifContext,ccifStatus,' ')
C
C
      ccifStatus = 1
      CALL ccif_put_char('_refine.ls_weighting_scheme',
     +       WghtScheme(1:Lenstr(WghtScheme)),
     +       ccifContext,ccifStatus)

      CALL ccif_release_context(ccifContext)
      END IF
C
C
      RETURN
      END
C
C
C     ==========================================
      SUBROUTINE Hrefine_wght_details(NHlines,Hlines)
C     ==========================================
C
C    _refine.ls_weighting_details
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      INTEGER NHlines
      CHARACTER Hlines(NHlines)*80
C     ..
      CHARACTER Cwork*800
      CHARACTER Awork(MaxLines)*80
C     ..
C     .. External Subroutines ..
      INTEGER Lenstr
      EXTERNAL ccif_output_fmt,
     +          ccif_put_real,
     +           ccif_release_context,
     +            ccif_setup_context
C     ..
      EQUIVALENCE (Awork,Cwork)
C
C
      IF ( .not. Harvest) RETURN
C
C
      IF (NHlines .ge. 1) THEN
      CALL ccif_setup_context('_refine.ls_weighting_details',
     +                         CurrCategory,ccifBlockID,
     +                         ccifContext,ccifStatus,' ')
C
C
        IF (NHlines .le. MaxLines) THEN
        DO 10 Jdo =1 ,NHlines
          Awork(Jdo) = Hlines(Jdo)
 10     CONTINUE
         ccifStatus = 1
         CALL ccif_put_text('_refine.ls_weighting_details',
     +   NHlines,Cwork,10,ccifContext,ccifStatus,'NEW')
        GO TO 20
        ELSE
        KK = 0 
        MM = 0
 30     CONTINUE
        KK = KK + 10
        MM = MM + 1
C
C
        IF (KK .le. NHlines) THEN
        LL = 0        
        DO 40 Jdo =KK-9 ,KK
          LL = LL + 1
          Awork(LL) = Hlines(Jdo)
 40     CONTINUE
C
C
         IF (MM .eq. 1) THEN
         ccifStatus = 1
         CALL ccif_put_text('_refine.ls_weighting_details',
     +   10,Cwork,10,ccifContext,ccifStatus,'NEW')
         ELSE
         ccifStatus = 1
         CALL ccif_put_text('_refine.ls_weighting_details',
     +   10,Cwork,10,ccifContext,ccifStatus,'  ')
         END IF
C
C
        GO TO 30
        ELSE
        KK = KK - 9
        LL = 0        
        DO 50 Jdo =KK ,NHlines
          LL = LL + 1
          Awork(LL) = Hlines(Jdo)
 50     CONTINUE
        KK = NHlines - KK + 1
         ccifStatus = 1
         CALL ccif_put_text('_refine.ls_weighting_details',
     +   KK,Cwork,10,ccifContext,ccifStatus,' ')
        GO TO 20
        END IF
        END IF
 20     CONTINUE
C
C
      CALL ccif_release_context(ccifContext)
C
C
      END IF
C
C
      RETURN
      END
      SUBROUTINE Hrefln_sys_abs(IH,IK,IL,F,SF)
C     =============================================================
C
C    _REFLN_SYS_ABS
C  Data items in the REFLN_SYS_ABS category record details about the
C  reflection data found to be systematic absent during data reduction
C  The REFLN data items refer to individual reflections and must
C  be included in loop lists.
C    _category.id                  refln_sys_abs
C    _category.mandatory_code      no
C     loop_
C    _category_key.name          '_refln_sys_abs.h'
C                                '_refln_sys_abs.k'
C                                '_refln_sys_abs.l'
C
C   _refln_sys_abs.h
C Miller index h of the reflection. The values of the Miller
C indices in the REFLN_SYS_ABS category must correspond to the cell
C defined by cell lengths and cell angles in the CELL category.
C    _item.name                  '_refln_sys_abs.h'
C    _item.category_id             refln_sys_abs
C    _item.mandatory_code          yes
C     loop_
C    _item_dependent.dependent_name
C                                '_refln_sys_abs.k'
C                                '_refln_sys_abs.l'
C    _item_sub_category.id         miller_index
C    _item_type.code               int
C
C    _refln_sys_abs.k
C    _refln_sys_abs.l
C
C    _refln_sys_abs.I
C  The measured value of the Intensity in arbitrary units.
C    _item.name                  '_refln_sys_abs.I'
C    _item.category_id             refln_sys_abs
C    _item.mandatory_code          no
C     loop_
C    _item_related.related_name
C    _item_related.function_code '_refln_sys_abs.sigmaI'
C                                  associated_esd
C    _item_type.code               float
C    _item_type_conditions.code    esd
C    _item_units.code              arbitrary
C
C    _refln_sys_abs.sigmaI
C The standard uncertainty (e.s.d.) of _refln_sys_abs.I, in
C  arbitrary units.
C    _item.name                  '_refln_sys_abs.sigmaI'
C    _item.category_id             refln_sys_abs
C    _item.mandatory_code          no
C     loop_
C    _item_related.related_name
C    _item_related.function_code '_refln_sys_abs.I'
C                                  associated_value
C    _item_type.code               float
C    _item_units.code              arbitrary
C
C    _refln_sys_abs.I_over_sigmaI
C   Measure I/sigma(I) for a systematic absent reflection
C    _item.name                  '_refln_sys_abs.I_over_sigmaI'
C    _item.category_id             refln_sys_abs
C    _item.mandatory_code          no
C    _item_type.code               float
C
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      REAL F,SF
      INTEGER IH,IK,IL
C     ..
C     .. External Subroutines ..
      EXTERNAL ccif_output_fmt,
     +          ccif_put_char,
     +           ccif_put_real,
     +            ccif_put_real_esd,
     +             ccif_release_context,
     +              ccif_setup_context
C     ..
C     .. Local Scalars ..
      LOGICAL First
C     ..
C     .. External Functions ..
      INTEGER Lenstr
      EXTERNAL Lenstr
C     ..
      SAVE First
      DATA First/.true./
C
C
      IF ( .not. Harvest) RETURN
C
C
      CALL ccif_setup_context('_refln_sys_abs.index_h',
     +                        CurrCategory,ccifBlockID,ccifContext,
     +                        ccifStatus,'loop')
C
C
      IF (First) THEN
        First = .false.
        CALL ccif_output_fmt('_refln_sys_abs.index_h',' ',8,0,
     +                       'd',ccifStatus)
        CALL ccif_output_fmt('_refln_sys_abs.index_k',' ',8,0,
     +                       'd',ccifStatus)
        CALL ccif_output_fmt('_refln_sys_abs.index_l',' ',8,0,
     +                       'd',ccifStatus)
        CALL ccif_output_fmt('_refln_sys_abs.I',' ',12,2,
     +                       'f',ccifStatus)
        CALL ccif_output_fmt('_refln_sys_abs.sigmaI',' ',12,2,
     +                       'f',ccifStatus)
        CALL ccif_output_fmt('_refln_sys_abs.I_over_sigmaI',
     +                       ' ',8,1,'f',ccifStatus)
      END IF
C
C
        ccifStatus = AppendRow
        CALL ccif_put_int('_refln_sys_abs.index_h',IH,ccifContext,
     +                     ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_int('_refln_sys_abs.index_k',IK,ccifContext,
     +                     ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_int('_refln_sys_abs.index_l',IL,ccifContext,
     +                     ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real_esd('_refln_sys_abs.I',F,SF,
     +                          ccifContext, ccifStatus, ' ')
        ccifStatus = KeepContext
        CALL ccif_put_real('_refln_sys_abs.I_over_sigmaI',
     +                      F/SF,ccifContext,ccifStatus)
C
C
        CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C     ============================
      SUBROUTINE Hreflns(WilsonB,R1,R2,Criteria,
     +                         AMI,AMF,Nref)
C     ============================
C
C
C---- harvest
C
C   _reflns.B_iso_Wilson_estimate    
C   _reflns.entry_id                 Projectname
C   _reflns.data_set_id               Lead_acetate_derivative
C   _reflns.d_resolution_high        
C   _reflns.d_resolution_low         
C   _reflns.observed_criterion  
C   _reflns.mean<I_over_sigI>_obs_all    
C   _reflns.mean<F_over_sigF>_obs_all    
C   _reflns.number_obs                   
C
C
C
      include 'harvest.inc'
C
C
C     .. Array Arguments ..
      REAL WilsonB,R1,R2,AMI,AMF
      INTEGER Nref
      CHARACTER Criteria* (*)
C     ..
C     .. External Subroutines ..
      INTEGER Lenstr
      EXTERNAL ccif_output_fmt,
     +          ccif_put_real,
     +           ccif_release_context,
     +            ccif_setup_context
C     ..
C
C
      IF ( .not. Harvest) RETURN
C
C
      CALL ccif_setup_context('_reflns.entry_id',
     +                         CurrCategory,ccifBlockID,
     +                         ccifContext,ccifStatus,' ')
C
C
      CALL ccif_output_fmt('_reflns.d_resolution_high',
     +             ' ',6,2,'f',ccifStatus)
      CALL ccif_output_fmt('_reflns.d_resolution_low',
     +             ' ',6,2,'f',ccifStatus)
      CALL ccif_output_fmt('_reflns.B_iso_Wilson_estimate',
     +    ' ',12,5,'f',ccifStatus)
      CALL ccif_output_fmt('_reflns.number_obs',
     +    ' ',10,0,'d',ccifStatus)
      CALL ccif_output_fmt('_reflns.mean<I_over_sigI>_obs_all',
     +    ' ',8,2,'f',ccifStatus)
      CALL ccif_output_fmt('_reflns.mean<F_over_sigF>_obs_all',
     +    ' ',8,2,'f',ccifStatus)
C
C
      IF (lenstr(PName).gt.0)
     + CALL ccif_put_char('_reflns.entry_id',
     +   PName(1:Lenstr(PName)),ccifContext,ccifStatus)
ccx      CALL ccif_put_char('_diffrn.id',
ccx     +   DName(1:Lenstr(DName)),ccifContext,ccifStatus)
C
C
      IF (R1 .gt. R2) THEN
        TMP = R1
        R1 = R2
        R2 = TMP
      END IF
C
C
      CALL ccif_put_real('_reflns.d_resolution_high',
     +                   R1,ccifContext,ccifStatus)
      CALL ccif_put_real('_reflns.d_resolution_low',
     +                   R2,ccifContext,ccifStatus)
C
C 
      IF (Lenstr(Criteria) .gt. 1) 
     + CALL ccif_put_char('_reflns.observed_criterion',
     +   Criteria(1:Lenstr(Criteria)),ccifContext,ccifStatus)
      IF (WilsonB .lt. valueNotDet -1.0) THEN
      IF (WilsonB .gt. 0.0)
     + CALL ccif_put_real('_reflns.B_iso_Wilson_estimate',
     +                   WilsonB,ccifContext,ccifStatus)
       END IF
       IF (AMI .lt. valueNotDet -1.0)
     +  CALL ccif_put_real(
     +       '_reflns.mean<I_over_sigI>_obs_all',
     +                   AMI,ccifContext,ccifStatus)
       IF (AMF .lt. valueNotDet -1.0)
     + CALL ccif_put_real(
     +      '_reflns.mean<F_over_sigF>_obs_all',
     +                   AMF,ccifContext,ccifStatus)
      IF (Nref .lt. IvalueNotDet -1 .and. Nref .gt. 0)
     + CALL ccif_put_int('_reflns.number_obs',
     +                   Nref,ccifContext,ccifStatus)
C
C
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C     =============================================================
      SUBROUTINE Hreflns_intensity_shell(Z,ACT,ACO,CT,CO)
C     =============================================================
C
C   loop_
C   _EBI_reflns_intensity_shell.Z
C   _EBI_reflns_intensity_shell.NZ_acentric_theory
C   _EBI_reflns_intensity_shell.NZ_acentric_observed
C   _EBI_reflns_intensity_shell.NZ_centric_theory
C   _EBI_reflns_intensity_shell.NZ_centric_observed
C
C Distributions of Observed Intensity Magnitudes
C
C Tables below give percentage of terms for which I.le.Z 
C where Z is defined as I/<I> for the range of 4*((Sintheta/Lamda)**2)
C
C Z values in tables :
C 0.1  0.2  0.3  0.4  0.5  0.6  0.7  0.8  0.9  1.0
C
C ACENTRIC WILSON Distribution : 1N(Z) :
C THEORETICAL Distribution 
C 9.5 18.1 25.9 33.0 39.3 45.1 50.3 55.1 59.3 63.2
C
C Observed distribution in ranges of 4*((Sintheta/Lamda)**2)
C-----
C CENTRIC WILSON Distribution : 1Bar N(Z) 
C THEORETICAL Distribution 
C 24.8 34.5 41.6 47.3 52.1 56.1 59.7 62.9 65.7 68.3
C
C Observed distribution in ranges of 4*((Sintheta/Lamda)**2)
C
C Cumulative intensity distribution:
C         Z   N(Z)Atheor  N(Z)Acen  N(Z)Ctheor  N(Z)Cen
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      REAL Z,ACT,ACO,CT,CO
C     ..
C     .. Scalar Arguments ..
      INTEGER NumShells
C     ..
C     .. External Subroutines ..
      EXTERNAL ccif_output_fmt,
     +          ccif_put_char,
     +           ccif_put_real,
     +            ccif_put_real_esd,
     +             ccif_release_context,
     +              ccif_setup_context
C     ..
C     .. Local Scalars ..
      INTEGER Jdo
      CHARACTER IDwork*6
      LOGICAL First
C     ..
C     .. External Functions ..
      INTEGER Lenstr
      EXTERNAL Lenstr
C     ..
      DATA First /.true./
C
C
      IF ( .not. Harvest) RETURN
C
C

      CALL ccif_setup_context('_EBI_reflns_intensity_shell.Z',
     +                        CurrCategory,ccifBlockID,ccifContext,
     +                        ccifStatus,'loop')
C
C
        IF (First) THEN
          First = .false.
        CALL ccif_output_fmt('_EBI_reflns_intensity_shell.Z',
     +                       ' ',4,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +     '_EBI_reflns_intensity_shell.NZ_acentric_theory',
     +     ' ',8,1,'f',ccifStatus)
        CALL ccif_output_fmt(
     +     '_EBI_reflns_intensity_shell.NZ_acentric_observed',
     +     ' ',8,1,'f',ccifStatus)
        CALL ccif_output_fmt(
     +     '_EBI_reflns_intensity_shell.NZ_centric_theory',
     +     ' ',8,1,'f',ccifStatus)
        CALL ccif_output_fmt(
     +     '_EBI_reflns_intensity_shell.NZ_centric_observed',
     +     ' ',8,1,'f',ccifStatus)
       END IF
C
C
        ccifStatus = AppendRow
        CALL ccif_put_real('_EBI_reflns_intensity_shell.Z',
     +                     Z,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real(
     +   '_EBI_reflns_intensity_shell.NZ_acentric_theory',
     +   ACT,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real(
     +     '_EBI_reflns_intensity_shell.NZ_acentric_observed',
     +   ACO,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real(
     +   '_EBI_reflns_intensity_shell.NZ_centric_theory',
     +   CT,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real(
     +   '_EBI_reflns_intensity_shell.NZ_centric_observed',
     +   CO,ccifContext,ccifStatus)
C
C
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C     =============================================================
      SUBROUTINE  Hreflns_overall_merge_p1(R1,R2,
     +                    nmeas,nuniq,ncent,nano,
     +                    fsigi,Rfac,ranom,sdIsignal,fpbias,Ntb)
C     =============================================================
C
C                                               fo example from scala.f
C    _diffrn_reflns.d_res_high               
C    _diffrn_reflns.d_res_low
C    _diffrn_reflns.meanI_over_sigI_all                 I/sigma fsigi
C    _diffrn_reflns.number_measured_all                 nmeas
C    _diffrn_reflns.number_unique_all                   nuniq
C    _diffrn_reflns.number_centric_all                  ncent
C    _diffrn_reflns.number_anomalous_all                nano
C    _diffrn_reflns.Rmerge_I_all                        Rfac   aa
C    _diffrn_reflns.Rmerge_I_anomalous_all              Ranom  ranom
C    _diffrn_reflns.meanI_over_sd_all                  <I>/sd  signal
C    _diffrn_reflns.mean_fract_bias                    fpbias
C    _diffrn_reflns.num_fract_bias_in_mean             ntb
C
C
C
C
      include 'harvest.inc'
C
C
C     .. Array Arguments ..
      INTEGER  nmeas,nuniq,ncent,nano,Ntb
      REAL     R1,R2,fsigi,Rfac,ranom,sdIsignal,fpbias
C     ..
C     .. External Subroutines ..
      EXTERNAL ccif_output_fmt,
     +          ccif_put_char,
     +           ccif_put_real,
     +            ccif_put_real_esd,
     +             ccif_release_context,
     +              ccif_setup_context
C     ..
C     .. External Functions ..
      INTEGER Lenstr
      EXTERNAL Lenstr
C     ..
C
C
      IF ( .not. Harvest) RETURN
C
C    
        CALL ccif_output_fmt('_diffrn_reflns.d_res_high',
     +                       ' ',6,2,'f',ccifStatus)
        CALL ccif_output_fmt('_diffrn_reflns.d_res_low',
     +                       ' ',6,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +         '_diffrn_reflns.number_measured_all',
     +                       ' ',8,0,'d',ccifStatus)
        CALL ccif_output_fmt(
     +         '_diffrn_reflns.number_unique_all',
     +                       ' ',8,0,'d',ccifStatus)
        CALL ccif_output_fmt(
     +         '_diffrn_reflns.number_centric_all',
     +                       ' ',8,0,'d',ccifStatus)
        CALL ccif_output_fmt(
     +         '_diffrn_reflns.number_anomalous_all',
     +                       ' ',8,0,'d',ccifStatus)
        CALL ccif_output_fmt('_diffrn_reflns.Rmerge_I_all',
     +                       ' ',9,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +        '_diffrn_reflns.Rmerge_I_anomalous_all',
     +                       ' ',9,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +        '_diffrn_reflns.meanI_over_sd_all',
     +                       ' ',9,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +        '_diffrn_reflns.meanI_over_sigI_all',
     +                       ' ',9,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +   '_diffrn_reflns.num_fract_bias_in_mean',
     +                       ' ',7,0,'d',ccifStatus)
        CALL ccif_output_fmt(
     +    '_diffrn_reflns.mean_fract_bias',
     +                       ' ',7,2,'f',ccifStatus)
C
C
      Rlow = R1
      Rhigh = R2
      IF (R1 .lt. R2) THEN
        Rlow = R2
        Rhigh = R1
      END IF
C
C
      IF (rom_context .eq. -1)
     + CALL ccif_setup_context(
     +     '_diffrn_reflns.d_res_high',
     +      CurrCategory,ccifBlockID,
     +      rom_context,ccifStatus,' ')
C
C
      CALL ccif_put_real('_diffrn_reflns.d_res_low',
     +      Rlow,rom_context,ccifStatus)
C
C
      CALL ccif_put_real('_diffrn_reflns.d_res_high',
     +      Rhigh,rom_context,ccifStatus)
C
C
      IF (Nmeas .lt. IvalueNotDet -1 .and. Nmeas .gt. 0)
     + CALL ccif_put_int(
     +    '_diffrn_reflns.number_measured_all',
     +      Nmeas,rom_context,ccifStatus)
C
C
      IF (Nuniq .lt. IvalueNotDet -1 .and. Nuniq .gt. 0)
     + CALL ccif_put_int(
     +       '_diffrn_reflns.number_unique_all',
     +        Nuniq,rom_context,ccifStatus)
C
C
      IF ( Ncent .lt. IValueNotDet -1 .and. Ncent .gt. 0) 
     + CALL ccif_put_int(
     +     '_diffrn_reflns.number_centric_all',
     +       Ncent,rom_context,ccifStatus)
C
C
      IF ( Nano .lt. IValueNotDet -1 .and. Nano .gt. 0) 
     + CALL ccif_put_int(
     +      '_diffrn_reflns.number_anomalous_all',
     +       Nano,rom_context,ccifStatus)
C
C
      CALL ccif_put_real(
     +      '_diffrn_reflns.Rmerge_I_all',
     +      Rfac,rom_context,ccifStatus)
C
C
      IF (Ranom .lt. ValueNotDet -1.0 .and. Ranom .gt. 0.01)
     + CALL ccif_put_real(
     +     '_diffrn_reflns.Rmerge_I_anomalous_all',
     +      Ranom,rom_context,ccifStatus)
C
C
      IF (fsigi .lt. ValueNotDet -1.0 .and. fsigi .gt. 0.001)
     + CALL ccif_put_real(
     +      '_diffrn_reflns.meanI_over_sigI_all',
     +      fsigi,rom_context,ccifStatus)
C
C
      IF (sdIsignal .lt. ValueNotDet -1.0 .and. 
     +    sdIsignal .gt. 0.001)
     + CALL ccif_put_real(
     +      '_diffrn_reflns.meanI_over_sd_all',
     +      sdIsignal,rom_context,ccifStatus)
C
C
C
C
      IF (Ntb .lt. IValueNotDet -1 .and. Ntb .gt. 0) THEN
         CALL ccif_put_int(
     +   '_diffrn_reflns.num_fract_bias_in_mean',
     +      Ntb,rom_context,ccifStatus)
         CALL ccif_put_real(
     +   '_diffrn_reflns.mean_fract_bias',
     +      fpbias,rom_context,ccifStatus)
      END IF
C
C
      RETURN
      END
C
C
C     =============================================================
      SUBROUTINE  Hreflns_overall_merge_p2(complt,Rmult,pcv,pcvo,
     +                                     rmeas,rmeaso,anomfrc)
C     =============================================================
C
C
C    _diffrn_reflns.percent_possible_all       complt
C    _diffrn_reflns.multiplicity               tt
C    _diffrn_reflns.PCV                        PCV
C    _diffrn_reflns.PCV_mean                   pcvo
C    _diffrn_reflns.Rmeas                      rmeas
C    _diffrn_reflns.Rmeas_mean                 rmeaso
C    _diffrn_reflns.anom_diff_percent_meas     anomfrc
C
C
C
C
      include 'harvest.inc'
C
C
C     .. Array Arguments ..
      REAL complt,Rmult,pcv,pcvo,rmeas,rmeaso,anomfrc
C     ..
C     .. External Subroutines ..
      EXTERNAL ccif_output_fmt,
     +          ccif_put_char,
     +           ccif_put_real,
     +            ccif_put_real_esd,
     +             ccif_release_context,
     +              ccif_setup_context
C     ..
C     .. External Functions ..
      INTEGER Lenstr
      EXTERNAL Lenstr
C     ..
C
C
      IF ( .not. Harvest) RETURN
C
C
        CALL ccif_output_fmt(
     +        '_diffrn_reflns.percent_possible_all',
     +                       ' ',4,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +         '_diffrn_reflns.multiplicity',
     +                       ' ',4,2,'f',ccifStatus)
        CALL ccif_output_fmt('_diffrn_reflns.PCV',
     +                       ' ',6,2,'f',ccifStatus)
        CALL ccif_output_fmt('_diffrn_reflns.PCV_mean',
     +                       ' ',6,2,'f',ccifStatus)
        CALL ccif_output_fmt('_diffrn_reflns.Rmeas',
     +                       ' ',6,2,'f',ccifStatus)
        CALL ccif_output_fmt('_diffrn_reflns.Rmeas_mean',
     +                       ' ',6,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +       '_diffrn_reflns.anom_diff_percent_meas',
     +                       ' ',6,2,'f',ccifStatus)
C
C
      IF (rom_context .eq. -1)
     + CALL ccif_setup_context('_diffrn_reflns.d_res_high',
     +                        CurrCategory,ccifBlockID,
     +                        rom_context,ccifStatus,' ')
C
C
      IF (pcv .lt. ValueNotDet -1.0 .and. pcv .gt. 0.01)
     + CALL ccif_put_real('_diffrn_reflns.PCV',
     +      pcv,rom_context,ccifStatus)
C
C
      IF (pcvo .lt. ValueNotDet -1.0 .and. pcvo .gt. 0.01)
     + CALL ccif_put_real('_diffrn_reflns.PCV_mean',
     +      pcvo,rom_context,ccifStatus)
C
C
      IF (Rmeas .lt. ValueNotDet -1.0 .and. Rmeas .gt. 0.01)
     + CALL ccif_put_real('_diffrn_reflns.Rmeas',
     +      Rmeas,rom_context,ccifStatus)
C
C
      IF (Rmeaso .lt. ValueNotDet -1.0 .and. Rmeaso .gt. 0.01)
     + CALL ccif_put_real('_diffrn_reflns.Rmeas_mean',
     +      Rmeaso,rom_context,ccifStatus)
C
C
      IF (Rmult .lt. ValueNotDet -1.0 .and. Rmult .gt. 0.01)
     + CALL ccif_put_real('_diffrn_reflns.multiplicity',
     +      Rmult,rom_context,ccifStatus)
C
C
      IF (complt .lt. ValueNotDet -1.0 .and. complt .gt. 0.01)
     + CALL ccif_put_real(
     +      '_diffrn_reflns.percent_possible_all',
     +      complt,rom_context,ccifStatus)
C
C
      IF (anomfrc .lt. ValueNotDet -1.0 .and. anomfrc .gt. 0.01)
     + CALL ccif_put_real(
     +      '_diffrn_reflns.anom_diff_percent_meas',
     +      anomfrc,rom_context,ccifStatus)
C
C
      RETURN
      END
C
C
C     =============================================================
      SUBROUTINE Hreflns_overall_merge_p3(aihmin,aihmax,
     +                              t1f,nssf,t2f,t3f,
     +                              t1p,nssp,t2p,t3p)
C     =============================================================
C
C    _diffrn_reflns.min_intensity
C    _diffrn_reflns.max_intensity
C    _diffrn_reflns.Intensity_rms_fully_recorded
C    _diffrn_reflns.num_fully_measured
C    _diffrn_reflns.mean_scatter_over_sd_full
C    _diffrn_reflns.sigma_scatter_over_sd_full
C    _diffrn_reflns.Intensity_rms_partially_recorded
C    _diffrn_reflns.num_partials_measured
C    _diffrn_reflns.mean_scatter_over_sd_part
C    _diffrn_reflns.sigma_scatter_over_sd_part
C
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      INTEGER nssf, nssp
      REAL aihmin,aihmax,t1f,t2f,t3f,t1p,t2p,t3p
C     ..
C     .. External Subroutines ..
      EXTERNAL ccif_output_fmt,
     +          ccif_put_char,
     +           ccif_put_real,
     +            ccif_put_real_esd,
     +             ccif_release_context,
     +              ccif_setup_context
C     ..
C     .. External Functions ..
      INTEGER Lenstr
      EXTERNAL Lenstr
C     ..
C
      IF ( .not. Harvest) RETURN
C
C    
        CALL ccif_output_fmt('_diffrn_reflns.min_intensity',
     +                       ' ',6,2,'f',ccifStatus)
        CALL ccif_output_fmt('_diffrn_reflns.max_intensity',
     +                       ' ',12,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +  '_diffrn_reflns.Intensity_rms_fully_recorded',
     +                       ' ',8,1,'f',ccifStatus)
        CALL ccif_output_fmt(
     +   '_diffrn_reflns.num_fully_measured',
     +                       ' ',8,0,'d',ccifStatus)
        CALL ccif_output_fmt(
     +    '_diffrn_reflns.mean_scatter_over_sd_full',
     +                       ' ',8,1,'f',ccifStatus)
        CALL ccif_output_fmt(
     +    '_diffrn_reflns.sigma_scatter_over_sd_full',
     +                       ' ',8,1,'f',ccifStatus)
        CALL ccif_output_fmt(
     + '_diffrn_reflns.Intensity_rms_partially_recorded',
     +                       ' ',8,1,'f',ccifStatus)
        CALL ccif_output_fmt(
     +    '_diffrn_reflns.num_partials_measured',
     +                       ' ',8,0,'d',ccifStatus)
        CALL ccif_output_fmt(
     +   '_diffrn_reflns.mean_scatter_over_sd_part',
     +                       ' ',8,1,'f',ccifStatus)
        CALL ccif_output_fmt(
     +    '_diffrn_reflns.sigma_scatter_over_sd_part',
     +                       ' ',8,1,'f',ccifStatus)
C
C
      IF (rom_context .eq. -1)
     + CALL ccif_setup_context(
     +  '_diffrn_reflns.d_res_high',
     +        CurrCategory,ccifBlockID,
     +          rom_context,ccifStatus,' ')
C
C
      CALL ccif_put_real('_diffrn_reflns.min_intensity',
     +      aihmin,rom_context,ccifStatus)
      CALL ccif_put_real('_diffrn_reflns.max_intensity',
     +      aihmax,rom_context,ccifStatus)
C
C
      CALL ccif_put_int(
     +   '_diffrn_reflns.num_fully_measured',
     +       Nssf,rom_context,ccifStatus)
      CALL ccif_put_real(
     +     '_diffrn_reflns.Intensity_rms_fully_recorded',
     +      t1f,rom_context,ccifStatus)
      CALL ccif_put_real(
     +     '_diffrn_reflns.mean_scatter_over_sd_full',
     +      t2f,rom_context,ccifStatus)
      CALL ccif_put_real(
     +     '_diffrn_reflns.sigma_scatter_over_sd_full',
     +      t3f,rom_context,ccifStatus)
C
C
      IF (Nssp .lt. IValueNotDet -1 .and. Nssp .gt. 0)
     +  CALL ccif_put_int(
     +    '_diffrn_reflns.num_partials_measured',
     +       Nssp,rom_context,ccifStatus)
      IF (t1p .lt. ValueNotDet -1.0 .and. t1p .gt. 0.001)
     + CALL ccif_put_real(
     + '_diffrn_reflns.Intensity_rms_partially_recorded',
     +      t1p,rom_context,ccifStatus)
      IF (t2p .lt. ValueNotDet -1.0 .and. t2p .gt. 0.001)
     + CALL ccif_put_real(
     + '_diffrn_reflns.mean_scatter_over_sd_part',
     +      t2p,rom_context,ccifStatus)
      IF (t3p .lt. ValueNotDet -1.0 .and. t3p .gt. 0.001)
     + CALL ccif_put_real(
     + '_diffrn_reflns.sigma_scatter_over_sd_part',
     +      t3p,rom_context,ccifStatus)
C
C
      RETURN
      END
      SUBROUTINE Hreflns_scaling_shell(R1,R2,Nref,AI,AF)
C     =============================================================
C
C    loop_
C    _EBI_tmp_reflns_scaling_shell.d_res_high
C    _EBI_tmp_reflns_scaling_shell.d_res_low
C    _EBI_tmp_reflns_scaling_shell.num_reflns_observed
C    _EBI_tmp_reflns_scaling_shell.mean<I_over_sigI>_obs
C    _EBI_tmp_reflns_scaling_shell.mean<F_over_sigF>_obs
C
C
C
      include 'harvest.inc'
C
C     .. Array Arguments ..
      REAL R1,R2,AI,AF
      INTEGER Nref
C     ..
C     .. External Subroutines ..
      EXTERNAL ccif_output_fmt,
     +          ccif_put_char,
     +           ccif_put_real,
     +            ccif_put_real_esd,
     +             ccif_release_context,
     +              ccif_setup_context
C     ..
C     .. Local Scalars ..
      INTEGER Jdo
      CHARACTER IDwork*6
      LOGICAL First
C     ..
C     .. External Functions ..
      INTEGER Lenstr
      EXTERNAL Lenstr
C     ..
      DATA First /.true./
C
C
      IF ( .not. Harvest) RETURN
C
C    
      CALL ccif_setup_context(
     +      '_EBI_tmp_reflns_scaling_shell.d_res_high',
     +                        CurrCategory,ccifBlockID,ccifContext,
     +                        ccifStatus,'loop')
C
C
        IF (First) THEN
          First = .false.
        CALL ccif_output_fmt(
     +        '_EBI_tmp_reflns_scaling_shell.d_res_high',
     +                       ' ',6,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +        '_EBI_tmp_reflns_scaling_shell.d_res_low',
     +                       ' ',6,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +         '_EBI_tmp_reflns_scaling_shell.num_reflns_observed',
     +                       ' ',8,0,'d',ccifStatus)
        CALL ccif_output_fmt(
     +     '_EBI_tmp_reflns_scaling_shell.mean<I_over_sigI>_obs',
     +                       ' ',9,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +    '_EBI_tmp_reflns_scaling_shell.mean<F_over_sigF>_obs',
     +                       ' ',9,2,'f',ccifStatus)
       END IF
C
C
      IF (R1 .gt. R2) THEN
        TMP = R1
        R1 = R2
        R2 = TMP
      END IF
C
C
        ccifStatus = AppendRow
        CALL ccif_put_real(
     +       '_EBI_tmp_reflns_scaling_shell.d_res_high',
     +                     R1,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_real(
     +       '_EBI_tmp_reflns_scaling_shell.d_res_low',
     +                     R2,ccifContext,ccifStatus)
        ccifStatus = KeepContext
        CALL ccif_put_int(
     +       '_EBI_tmp_reflns_scaling_shell.num_reflns_observed',
     +        Nref,ccifContext,ccifStatus)
        ccifStatus = KeepContext
       CALL ccif_put_real(
     +     '_EBI_tmp_reflns_scaling_shell.mean<I_over_sigI>_obs',
     +      AI,ccifContext,ccifStatus)
        ccifStatus = KeepContext
       CALL ccif_put_real(
     +     '_EBI_tmp_reflns_scaling_shell.mean<F_over_sigF>_obs',
     +      AF,ccifContext,ccifStatus)
C
C
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C     =============================================================
      SUBROUTINE Hreflns_shell_p1(R1,R2,Nmeas,Nuniq,Ncent,
     +                            Nanom,fsigi,Rfac,Rcum,
     +                            Ranom,sdIsignal,fpbias,Npb)
C     =============================================================
C
C                                               fo example from scala.f
C    loop_
C    _reflns_shell.d_res_high                       Dmin      reso(k)
C    _reflns_shell.d_res_low
C    _reflns_shell.number_measured_all              Nmeas     n_m_del_s(k)
C    _reflns_shell.number_unique_all                Nref      n_uniq_s(k)
C    _reflns_shell.number_centric_all           Ncent     n_cent_s(k)
C    _reflns_shell.number_anomalous_all         Nanom     n_anom_s(k)
C    _reflns_shell.Rmerge_I_all                     Rfac      tg
C    _reflns_shell.Rmerge_I_all_cumulative      Rcum      aa
C    _reflns_shell.Rmerge_I_anomalous_all       Ranom     ranom
C    _reflns_shell.meanI_over_sd_all            <I>/sd    signal
C    _reflns_shell.meanI_over_sigI_all              I/sigma   fsigi
C    _reflns_shell.mean_fract_bias              fpbias
C    _reflns_shell.num_fract_bias_in_mean       npb
C
C      _reflns.entry_id    projectname
C
C
C
      include 'harvest.inc'
      include 'harshell.inc'
C
C
C     .. Array Arguments ..
      INTEGER Nmeas,Nuniq,Ncent,Nanom,Npb
      REAL R1,R2,fsigi,Rfac,Rcum,Ranom,sdIsignal,FPbias
C     ..
C     .. External Subroutines ..
      EXTERNAL ccif_output_fmt,
     +          ccif_put_char,
     +           ccif_put_real,
     +            ccif_put_real_esd,
     +             ccif_release_context,
     +              ccif_setup_context
C     ..
C     .. Local Scalars ..
      INTEGER Jdo
      CHARACTER IDwork*6
      LOGICAL First
C     ..
C     .. External Functions ..
      INTEGER Lenstr
      EXTERNAL Lenstr
C     ..
      DATA First /.true./
C
C
      IF ( .not. Harvest) RETURN
C
C    
        IF (First) THEN
          First = .false.
          NRshells = 0
        CALL ccif_output_fmt('_reflns_shell.d_res_high',
     +                       ' ',6,2,'f',ccifStatus)
        CALL ccif_output_fmt('_reflns_shell.d_res_low',
     +                       ' ',6,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +         '_reflns_shell.number_measured_all',
     +                       ' ',7,0,'d',ccifStatus)
        CALL ccif_output_fmt(
     +         '_reflns_shell.number_unique_all',
     +                       ' ',7,0,'d',ccifStatus)
        CALL ccif_output_fmt(
     +         '_reflns_shell.number_centric_all',
     +                       ' ',7,0,'d',ccifStatus)
        CALL ccif_output_fmt(
     +         '_reflns_shell.number_anomalous_all',
     +                       ' ',7,0,'d',ccifStatus)
        CALL ccif_output_fmt('_reflns_shell.Rmerge_I_all',
     +                       ' ',4,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +       '_reflns_shell.Rmerge_I_all_cumulative',
     +                       ' ',4,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +       '_reflns_shell.Rmerge_I_anomalous_all',
     +                       ' ',4,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +       '_reflns_shell.meanI_over_sd_all',
     +                       ' ',5,2,'f',ccifStatus)
        CALL ccif_output_fmt('_reflns_shell.meanI_over_sigI_all',
     +                       ' ',5,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +       '_reflns_shell.num_fract_bias_in_mean',
     +                       ' ',6,0,'d',ccifStatus)
        CALL ccif_output_fmt(
     +       '_reflns_shell.mean_fract_bias',
     +                       ' ',6,2,'f',ccifStatus)
       DO 10 Jdo = 1, MaxRshells
         Res(1,Jdo)     = ValueNotDet
         Res(2,Jdo)     = ValueNotDet
         Nreflns(1,Jdo) = IValueNotDet
         Nreflns(2,Jdo) = IValueNotDet
         Nreflns(3,Jdo) = IValueNotDet
         Nreflns(4,Jdo) = IValueNotDet
         Rs(1,Jdo)      = ValueNotDet
         Rs(2,Jdo)      = ValueNotDet
         Rs(3,Jdo)      = ValueNotDet
         sds(1,Jdo)     = ValueNotDet
         sds(2,Jdo)     = ValueNotDet
         Nfpb(Jdo)      = IValueNotDet
         FPB(Jdo)       = ValueNotDet
 10   CONTINUE
       END IF
C
C
      Rlow = R1
      Rhigh = R2
      IF (R1 .lt. R2) THEN
       Rlow = R2
       Rhigh = R1
      END IF
C
C
        NRshells = NRshells + 1
        IF (NRshells .gt. MaxRshells) THEN
            Harvest = .false.
            WRITE (6,6000)
 6000       FORMAT(' Harvest: Max Reflns Shells exceeded,',
     +               ' no harvest file written ********')
            RETURN
        END IF
C
C
      Res(1,NRshells) = Rlow
      Res(2,NRshells) = Rhigh
      Nreflns(1,NRshells) = Nmeas
      Nreflns(2,NRshells) = Nuniq
      Nreflns(3,NRshells) = Ncent
      Nreflns(4,NRshells) = Nanom
      Rs(1,NRshells) = Rfac
      Rs(2,NRshells) = Rcum
      Rs(3,NRshells) = Ranom
      sds(1,NRshells) = fsigi
      sds(2,NRshells) = sdIsignal
      Nfpb(NRshells)  = Npb
      FPB(NRshells)   = fpbias
C
C
      RETURN
      END
C
C
C     =============================================================
      SUBROUTINE Hreflns_shell_p2(R1,R2,complt,ccmplt,amltpl,
     +                            pcvo,pcv,
     +                            rmeas,rmeaso,anomfrc)
C     =============================================================
C
C
C---- harvest
C
C    _reflns_shell.percent_possible_all            %poss      complt
C    _reflns_shell.cum_percent_possible_all    Cm%poss    ccmplt
C    _reflns_shell.multiplicity                Mlplcty    amltpl
C    _reflns_shell.PCV_mean                    PCV0       pcvo
C    _reflns_shell.PCV                         PCV        pcv
C    _reflns_shell.Rmeas                       Rmeas      rmeas
C    _reflns_shell.Rmeas_mean                  Rmeas0     rmeaso
C    _reflns_shell.anom_diff_percent_meas      AnomFrc    anomfrc
C
      include 'harvest.inc'
      include 'harshell.inc'
C
C     .. Array Arguments ..
      REAL complt,ccmplt,amltpl,pcvo,pcv,
     +     rmeas,rmeaso,anomfrc,R1,R2
C     ..
C     .. External Subroutines ..
      EXTERNAL ccif_output_fmt,
     +          ccif_put_char,
     +           ccif_put_real,
     +            ccif_put_real_esd,
     +             ccif_release_context,
     +              ccif_setup_context
C     ..
C     .. Local Scalars ..
      INTEGER Jdo
      CHARACTER IDwork*6
      LOGICAL First
C     ..
C     .. External Functions ..
      INTEGER Lenstr
      EXTERNAL Lenstr
C     ..
      DATA First /.true./
C
C
      IF ( .not. Harvest) RETURN
C
C    
        IF (First) THEN
          First = .false.
          NRshells = 0
        CALL ccif_output_fmt('_reflns_shell.percent_possible_all',
     +                       ' ',4,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +       '_reflns_shell.cum_percent_possible_all',
     +                       ' ',4,2,'f',ccifStatus)
        CALL ccif_output_fmt('_reflns_shell.multiplicity',
     +                       ' ',4,2,'f',ccifStatus)
        CALL ccif_output_fmt('_reflns_shell.PCV',
     +                       ' ',4,2,'f',ccifStatus)
        CALL ccif_output_fmt('_reflns_shell.PCV_mean',
     +                       ' ',4,2,'f',ccifStatus)
        CALL ccif_output_fmt('_reflns_shell.Rmeas',
     +                       ' ',4,2,'f',ccifStatus)
        CALL ccif_output_fmt('_reflns_shell.Rmeas_mean',
     +                       ' ',4,2,'f',ccifStatus)
        CALL ccif_output_fmt(
     +       '_reflns_shell.anom_diff_percent_meas',
     +                       ' ',4,2,'f',ccifStatus)
      DO 10 Jdo=1,MaxRshells
         rmult(Jdo)  = ValueNotDet
         Rs(4,Jdo)   = ValueNotDet
         Rs(5,Jdo)   = ValueNotDet
         Rs(6,Jdo)   = ValueNotDet
         Rs(7,Jdo)   = ValueNotDet
         Poss(1,Jdo) = ValueNotDet
         Poss(2,Jdo) = ValueNotDet 
         Poss(3,Jdo) = ValueNotDet
 10   CONTINUE
       END IF
C
C
        NRshells = NRshells + 1
        IF (NRshells .gt. MaxRshells) THEN
            Harvest = .false.
            WRITE (6,6000)
 6000       FORMAT(' Harvest: Max Reflns Shells exceeded,',
     +               ' no harvest file written ********')
            RETURN
        END IF
C
C
      Rlow = R1
      Rhigh = R2
      IF (R1 .lt. R2) THEN
       Rlow = R2
       Rhigh = R1
      END IF
C
C
      Res(1,NRshells) = Rlow
      Res(2,NRshells) = Rhigh
      rmult(NRshells) = amltpl
      Rs(4,NRshells) = pcv
      Rs(5,NRshells) = pcvo
      Rs(6,NRshells) = rmeas
      Rs(7,NRshells) = rmeaso
      Poss(1,NRshells) = complt
      Poss(2,NRshells) = ccmplt
      Poss(3,NRshells) = anomfrc
C
C
      RETURN
      END
C
C
C     ============================================================
      SUBROUTINE Hsoftware(SoftwareClass,
     +                     SoftwareAuthor,
     +                     SoftwareEmail,
     +                     SoftwareDescr)
C     ============================================================
C
      include 'harvest.inc'
C
C     .. Scalar Arguments ..
      REAL VALND
      INTEGER IVND,RowLimit
      LOGICAL Private,UseCWD
      CHARACTER SoftwareAuthor* (*),
     +           SoftwareEmail* (*),
     +            SoftwareDescr* (*),
     +             SoftwareClass* (*)
C     ..
C     .. Local Scalars ..
      INTEGER Jdo
C     ..
C     .. External Functions ..
      INTEGER Lenstr
C     ..
C
C
      IF (.not. Harvest) RETURN
C
C
      CALL ccif_setup_context('SOFTWARE',CurrCategory,ccifBlockID,
     +                        ccifContext,ccifStatus,' ')
C
C
      IF (Lenstr(SoftwareClass) .gt. 1)
     +  CALL ccif_put_char('_software.classification',
     +       SoftwareClass(1:Lenstr(SoftwareClass)),
     +       ccifContext,ccifStatus)
      IF (Lenstr(SoftwareAuthor) .gt. 1)
     +  CALL ccif_put_char('_software.contact_author',
     +       SoftwareAuthor(1:Lenstr(SoftwareAuthor)),
     +       ccifContext,ccifStatus)
      IF (Lenstr(SoftwareEmail) .gt. 1)
     +  CALL ccif_put_char('_software.contact_author_email',
     +       SoftwareEmail(1:Lenstr(SoftwareEmail)),
     +       ccifContext,ccifStatus)
      IF (Lenstr(SoftwareDescr) .gt. 1)
     +  CALL ccif_put_char('_software.description',
     +       SoftwareDescr(1:Lenstr(SoftwareDescr)),
     +       ccifContext,ccifStatus)
C
C---- do  _software.name
C         _software.version
C
      IF (lenstr(SoftwareName).gt.0)
     + CALL ccif_put_char('_software.name',
     +                   SoftwareName(1:Lenstr(SoftwareName)),
     +                   ccifContext,ccifStatus)
      IF (lenstr(SoftwareVersion).gt.0)
     + CALL ccif_put_char('_software.version',
     + SoftwareVersion(1:Lenstr(SoftwareVersion)),ccifContext,
     +                   ccifStatus)
C
C
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
      END
C
C
C     =========================================================
      SUBROUTINE HSymmetry(IntTabNum,SpaceGrpNam,NumEquiv,
     +                           RFsymm)
C     =========================================================
C
C Space-group number from International Tables for Crystallography,
C    Vol. A (1987).
C    _item.name                  '_Symmetry.Int_Tables_number'
C    _item_type.code               int
C
C Hermann-Mauguin space-group symbol. Note that the H-M symbol does
C not necessarily contain complete information about the Symmetry
C and the space-group origin. If used always supply the FULL symbol
C from International Tables for Crystallography, Vol. A (1987) and
C indicate the origin and the setting if it is not implicit. If
C there is any doubt that the equivalent positions can be uniquely
C deduced from this symbol specify the _Symmetry_equiv.pos_as_xyz
C or _Symmetry.space_group_name_Hall data items as well. Leave
C spaces between symbols referring to different axes.
C
C    _item.name                  '_Symmetry.space_group_name_H-M'
C    _item_type.code               line
C    _item_examples.case          'P 1 21/m 1'
C                                 'P 2/n 2/n 2/n (origin at -1)'
C                                 'R -3 2/m'
C
C_Symmetry_EQUIV    _category.description
C    _Symmetry_equiv.id
C    _Symmetry_equiv.pos_as_xyz
C      1  '+x,+y,+z'
C      2  '-x,-y,z'
C      3  '1/2+x,1/2-y,-z'
C      4  '1/2-x,1/2+y,-z'
C
C  The value of _Symmetry_equiv.id must uniquely identify
C  a record in the Symmetry_EQUIV category.
C  Note that this item need not be a number; it can be any unique
C  identifier.
C    _item.name                  '_Symmetry_equiv.id'
C    _item.mandatory_code          yes
C    _item_type.code               code
C
C Symmetry equivalent position in the 'xyz' representation. Except
C for the space group P1, these data are repeated in a loop.
C The format of the data item is as per International Tables for
C Crystallography, Vol. A. (1987). All equivalent positions should
C be entered, including those for lattice centring and a centre of
C Symmetry, if present.
C
C    _item.name                  '_Symmetry_equiv.pos_as_xyz'
C    _item.mandatory_code          no
C    _item_type.code               line
C    _item_examples.case         '-y+x,-y,1/3+z'
C
C
      include 'harvest.inc'
      include 'harsymm.inc'
C
C     .. Array Arguments ..
      REAL RFsymm(4,4,*)
C     ..
C     .. Scalar Arguments ..
      INTEGER IntTabNum,NumEquiv
      CHARACTER SpaceGrpNam*10
C     ..
C     .. Local Scalars ..
      INTEGER I,Ifail,ISG,IST,Jdo,KNO,Nline,NlineP,Nsymm,NsymmP,Ntok
      LOGICAL LnonStnd,Standard
      CHARACTER IDwork*6,Buffer*80,line*80,SGname*80
C     ..
C     .. External Subroutines ..
      EXTERNAL ccif_put_char,
     +          ccif_put_int,
     +           ccif_release_context,
     +            ccif_setup_context,
     +             Ccpupc,
     +              ugtenv,
     +               ccpdpn,
     +                Hparse
C     ..
C     .. External Functions ..
      INTEGER Lenstr
      EXTERNAL Lenstr
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC nint
C     ..
C     .. Data statements ..
      DATA KnownNonStnd/1003,3004,1004,2005,1005,1018,1020,1021,
     +     1022,1023,1059,1094,1197/
      DATA NonStnd/'P 1 1 2 Dyad along Z','I 1 21 1','P 1 1  21',
     +     'A 2  Origin on screw along b',
     +     'C 21 Origin on screw at 1/4X',
     +     'P 21 21 2a Origin on 21 21 shift 1/4,1/4,0',
     +     'C 2 2 21a  C-cent, shift 1/4X','C 2 2 2a   Origin on 21 21',
     +     'F 2 2 2a   As 1018 with F-cent, shift 1/4X',
     +     'I 2 2 2a   As 1018 with Origin shift 1/4,1/4,1/4',
     +     'P m m n2',
     +     'P 4 2 21 2a As P21212a Origin on 21 21 ie Shift 1/4,1/4,1/4'
     +     ,'I 23a    Expansion of 1023 which is an expansion of 1018'/
      DATA (SGstndName(Jdo),Jdo=1,61)/'P 1','P -1','P 1 2 1',
     +     'P 1 21 1','C 1 2 1','P 1 M 1','P 1 C 1','C 1 M 1','C 1 C 1',
     +     'P 1 2/M 1','P 1 21/M 1','C 2/M','P 2/C','P 21/C','C 2/C',
     +     'P 2 2 2','P 2 2 21','P 21 21 2','P 21 21 21','C 2 2 21',
     +     'C 2 2 2','F 2 2 2','I 2 2 2','I 21 21 21','P M M 2',
     +     'P M C 21','P C C 2','P M A 2','P C A 21','P N C 2',
     +     'P M N 21','P B A 2','P N A 21','P N N 2','C M M 2',
     +     'C M C 21','C C C 2','A M M 2','A B M 2','A M A 2','A B A 2',
     +     'F M M 2','F D D 2','I M M 2','I B A 2','I M A 2','P M M M',
     +     'P N N N','P C C M','P B A N','P M M A','P N N A','P M N A',
     +     'P C C A','P B A M','P C C N','P B C M','P N N M','P M M N',
     +     'P B C N','P B C A'/
      DATA (SGstndName(Jdo),Jdo=62,115)/'P N M A','C M C M',
     +     'C M C A','C M M M','C C C M','C M M A','C C C A','F M M M',
     +     'F D D D','I M M M','I B A M','I B C A','I M M A','P 4',
     +     'P 41','P 42','P 43','I 4','I 41','P -4','I-4','P 4 /M',
     +     'P 42 /M','P 4 /N','P 42 /N','I 4 /M','I 41 /A','P 4 2 2',
     +     'P 4 21 2','P 41 2 2','P 41 21 2','P 42 2 2','P 42 21 2',
     +     'P 43 2 2','P 43 21 2','I 4 2 2','I 41 2 2','P 4 M M',
     +     'P 4 B M','P 42 C M','P 42 N M','P 4 C C','P 4 N C',
     +     'P 42 M C','P 42 B C','I 4 M M','I 4 C M','I 41 M D',
     +     'I 41 C D','P -4 2 M','P -4 2 C','P -4 21 M','P -4 21 C',
     +     'P -4 M 2'/
      DATA (SGstndName(Jdo),Jdo=116,180)/'P -4 C 2','P -4 B 2',
     +     'P -4 N 2','I -4 M 2','I -4 C 2','I -4 2 M','I -4 2 D',
     +     'P 4 /MMM','P 4 /MCC','P 4 /NBM','P 4 /NNC','P 4 /MBM',
     +     'P 4 /MNC','P 4 /NMM','P 4 /NCC','P 42 /MMC','P 42 /MCM',
     +     'P 42 /NBC','P 42 /NNM','P 42 /MBC','P 42 /MNM','P 42 /NMC',
     +     'P 42 /NCM','I 4 /MMM','I 4 /MCM','I 41 /AMD','I 41 /ACD',
     +     'P 3','P 31','P 32','H 3 [R 3]','P -3','H -3 [R -3]',
     +     'P 3 1 2','P 3 2 1','P 31 1 2','P 31 2 1','P 32 1 2',
     +     'P 32 2 1','H 3 2 [R 32]','P 3 M 1','P 3 1 M','P 3 C 1',
     +     'P 3 1 C','R 3 M','R 3 C','P -3 1 M','P -3 1 C','P -3 M 1',
     +     'P -3 C 1','R -3 M','R -3 C','P 6','P 61','P 65','P 62',
     +     'P 64','P 63','P -6','P 6 /M','P 63 /M','P 6 2 2','P 61 2 2',
     +     'P 65 2 2','P 62 2 2'/
      DATA (SGstndName(Jdo),Jdo=181,230)/'P 64 2 2','P 63 2 2',
     +     'P 6 M M','P 6 C C','P 63 C M','P 63 M C','P -6 M 2',
     +     'P -6 C 2','P -6 2 M','P -6 2 C','P 6 /MMM','P 6 /MCC',
     +     'P 63 /MCM','P 63 /MMC','P 2 3','F 2 3','I 2 3','P 21 3',
     +     'I 21 3','P M -3','P N -3','F M -3','F D -3','I M -3',
     +     'P A -3','I A -3','P 4 3 2','P 42 3 2','F 4 3 2','F 41 3 2',
     +     'I 4 3 2','P 43 3 2','P 41 3 2','I 41 3 2','P -4 3 M',
     +     'F 4 -3 M','I -4 3 M','P -4 3 N','F -4 3 C','I -4 3 D',
     +     'P M 3 M','P N 3 N','P M 3 N','P N 3 M','F M 3 M','F M 3 C',
     +     'F D 3 M','F D 3 C','I M 3 M','I A 3 D'/
      DATA Standard/.false./,LnonStnd/.false./
C     ..
C
C
      IF ( .not. Harvest) RETURN
C
C---- see if Standard
C
      Buffer = ' '
      CALL ugtenv('SYMOP',Buffer)
      IF (Buffer(1:1) .eq. ' ') THEN
        WRITE (6,FMT=6000)
 6000   FORMAT (
     +' Harvest: NO SYMOP environment given - no deposit file created')
        Harvest = .false.
        RETURN
      END IF
C
C
      KNO = 0
      IF (IntTabNum .gt. 0 .and. IntTabNum .le. 230) THEN
        Standard = .true.
        GO TO 20
      ELSE
C
C---- check if one of Known NON-Standards
C
        DO 10 Jdo = 1,MaxNonStand
          IF (IntTabNum .eq. KnownNonStnd(Jdo)) THEN
            LnonStnd = .true.
            KNO = Jdo
            GO TO 20
          END IF
   10   CONTINUE
C
C--- the space group number is neither Standard
C                              nor Known non-Standard
C              therefore expecting NumEquiv > 0
C
        IF (NumEquiv .le. 0) THEN
          WRITE (6,FMT=6002)
 6002     FORMAT (
     +' Harvest: NON-stndard Spacegroup BUT - no Equivalent ',
     +               'positions given no deposit file created')
          Harvest = .false.
          RETURN
        END IF
C
C---- handle unKnown non-Standard here
C
            Nsymm = NumEquiv
            CALL HSymTrn(Nsymm,RFsym,PosXyz)
            SGname = SpaceGrpNam
        GO TO 80
C
C
      END IF
C
C
   20 CONTINUE
      IST = 45
      Ifail = 0
      CALL CCPDPN(IST,'SYMOP','READONLY','F',0,IFAIL)
      Nsymm = 0
C
C---- Find correct space-group in file.
C     Each space-group has header line of space-group number,
C     number of line of Symmetry operations for non-primitive
C     and primitive Cells.
C
   30 READ (IST,FMT=6004,ERR=100,END=100) line
 6004 FORMAT (a)
      CALL CCPUPC(line)
      Ntok = -MaxTokens
      CALL Hparse(line,Ibeg,Iend,Ityp,Fvalue,Cvalue,Idec,Ntok)
C
C---- Fields are space group number,
C                number of lines,
C                number of lines in primitive Cell Symmetry,
C                spacegroup name
ccx       NAMPG = line(Ibeg(5) :Iend(5))
C
      IF (Ityp(1) .ne. 2 .or. 
     +     Ityp(2) .ne. 2 .or. 
     +      Ityp(3) .ne. 2) THEN
        WRITE (6,FMT=6006)
 6006   FORMAT ('Harvest: Error in format of SYMOP file: ')
        Harvest = .false.
        RETURN
      END IF
C
C
      ISG = nint(Fvalue(1))
      Nline = nint(Fvalue(2))
      NlineP = nint(Fvalue(3))
C
C---- Check for spacegroup number given
C
      IF (IntTabNum .eq. ISG) GO TO 50
C
C---- Not this one, skip Nline lines
C
      DO 40 Jdo = 1,Nline
        READ (IST,FMT=*)
   40 CONTINUE
C
C----     try again
C
      GO TO 30
   50 CONTINUE
C
C---- Space-group found, convert Nline lines of
C     Symmetry operators to matrices
C
      IF (KNO .gt. 0) THEN
        SGname = NonStnd(KNO)
      ELSE
        SGname = SGstndName(IntTabNum)
      END IF
C
C
      DO 60 I = 1,NlineP
        READ (IST,FMT=6004) line
C
C--- test for case where two positions per line
C     as in     X, Y, Z *  -X,1/2+Y,-Z
C     REMOVED FROM THIS symop.lib !!!!
C
        Nsymm = Nsymm + 1
        EquivPos(Nsymm) = line(1:Lenstr(line))
   60 CONTINUE
C
      NsymmP = Nsymm
      IF (Nline .gt. NlineP) THEN
        DO 70 I = NlineP + 1,Nline
          READ (IST,FMT=6004) line
          Nsymm = Nsymm + 1
          EquivPos(Nsymm) = line(1:Lenstr(line))
   70   CONTINUE
      END IF
C
C
      Close (IST)
   80 CONTINUE
C
C---- do  _Symmetry.Int_Tables_number
C         _Symmetry.space_group_name_H-M
C
      CALL ccif_setup_context('Symmetry',CurrCategory,ccifBlockID,
     +                        ccifContext,ccifStatus,' ')
      CALL ccif_put_int('_Symmetry.Int_Tables_number',IntTabNum,
     +                  ccifContext,ccifStatus)
      CALL ccif_put_char('_Symmetry.space_group_name_H-M',SGname,
     +                   ccifContext,ccifStatus)
      CALL ccif_release_context(ccifContext)
C
C---- do loop    _Symmetry_equiv.pos_as_xyz
C
      CALL ccif_setup_context('Symmetry_EQUIV',CurrCategory,
     +                        ccifBlockID,ccifContext,ccifStatus,'loop')
C
C
      DO 90 Jdo = 1,Nsymm
        WRITE (IDwork,FMT=6008) Jdo
 6008   FORMAT (i6)
        ccifStatus = AppendRow
        CALL ccif_put_char('_Symmetry_equiv.id',IDwork,ccifContext,
     +                     ccifStatus)
        ccifStatus = KeepContext
        IF ( .not. Standard .and.  .not. LnonStnd) THEN
          CALL ccif_put_char('_Symmetry_equiv.pos_as_xyz',
     +                       PosXyz(Jdo) (1:Lenstr(PosXyz(Jdo)))
     +                       ,ccifContext,ccifStatus)
        ELSE
          CALL ccif_put_char('_Symmetry_equiv.pos_as_xyz',
     +                       EquivPos(Jdo) (1:Lenstr(EquivPos(Jdo))),
     +                       ccifContext,ccifStatus)
        END IF
   90 CONTINUE
C
C
      CALL ccif_release_context(ccifContext)
C
C
      RETURN
  100 CONTINUE
      WRITE (6,FMT=6010) IntTabNum
 6010 FORMAT (
     +'Harvest: No Symmetry information for space group number',
     +       i6,' in SYMOP file')
      Harvest = .false.
      RETURN
      END
