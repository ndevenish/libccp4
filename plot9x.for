C
C---- Changed routine names to conform to Fortran 77. All PLT$ --> GS
C     and other names are mapped as shown below. PJD 19th July, 1990
C
C---- Errors
C
C       SUBROUTINE GSSTRD(TEXT,DX,DY)
C       ERROR XCOLD and YCOLD not defined
C
C       SUBROUTINE GSTRIC(GSFIL,LISFIL,IOFLAG)
C       ERROR NSKIP not defined
C
C      SUBROUTINE GSVIEW(FILNAM,NSTART,ISCALE,SCAFAC,LINSIZ,
C     +                    NOCENT,NOPRNT,NINTER,IERR)
C      ERROR NPRNT not defined
C
C
C---- Subroutines in PLOT9X.FOR:
C
C
C     gsbarx.f  gsbftm.f  gsblkd.f  gscetx.f  gscfsz.f
C     gsclpl.f  gsclpt.f  gsclts.f  gscvax.f  gsddrb.f
C     gsdrmv.f  gsdvon.f  gsedtr.f  gsedvc.f  gsedvp.f
C     gsenvr.f  gsewnd.f  gsewsc.f  gsfram.f  gsgch0.f
C     gsgchc.f  gsgchf.f  gsgsym.f  gshair.f  gsinit.f
C     gsintm.f  gsinum.f  gsline.f  gslras.f  gslrsb.f
C     gslvck.f  gsmuct.f  gsoflw.f  gspict.f  gsprtm.f
C     gsquik.f  gsrdtm.f  gsrfnt.f  gsrstm.f  gssftm.f
C     gssltm.f  gsstrc.f  gsstrd.f  gsstyl.f  gssusp.f
C     gssvtm.f  gsswln.f  gstimr.f  gstmpy.f  gstric.f
C     gstril.f  gstsav.f  gstyon.f  gsutrn.f  gsvcln.f
C     gsview.f  gsxytm.f  init6v.f
C
C     axis.f    bopenw.f  chncol.f  crtplt.f  drawpc.f
C     dtext.f   findpc.f  fread.f   frtqrw.f  getscl.f
C     hlptxt.f  init3p.f  iread.f   irot.f    nextpc.f
C     oldtri.f  gscfix.f  gscvlf.f  gscvli.f  gscvsc.f
C     gstxt.f   qzread.f  qzwrit.f  rplot.f   scale.f
C     settbl.f  setvp.f   string.f  symbol.f  zero.f
C
C---- Entry Points in PLOT9X.FOR:
C
C     init6v.f: ENTRY SET640()
C               ENTRY MVT640(IX,IY)
C               ENTRY DWT640(IX,IY)
C               ENTRY PNT640(IX,IY)
C               ENTRY TXT640(BYTXT,NCHARS,ISIZE)
C               ENTRY XHT640(IX,IY,ICODE)
C               ENTRY DTONV6()
C               ENTRY DTOFV6()
C               ENTRY DCMPV6()
C               ENTRY SET100()
C               ENTRY MVT100(IX,IY)
C               ENTRY CLRV10()
C               ENTRY BUFV64(ITERM,NBYTE)
C               ENTRY COODV6(ITERM,IX,IY,NBYTE)
C
C     gsbarx.f: ENTRY GSBARY(CENT,YDIFF,WL,WR,NUM)
C
C     gscetx.f: ENTRY GSCETS(TEXT,SIZ1,SIZ2,NJUST)
C
C     gsddrb.f: ENTRY GSDOTB(XB,YB)
C               ENTRY GSMDRB(XB,YB)
C               ENTRY GSVBRK()
C
C     gsdrmv.f: ENTRY GSDWTO(X,Y)
C               ENTRY GSDWBY(X,Y)
C               ENTRY GSPOIN(X,Y)
C               ENTRY GSMVTO(X,Y)
C               ENTRY GSMVBY(X,Y)
C
C     gsdvon.f: ENTRY GSDVOF()
C
C     gsedvp.f: ENTRY GSEBSZ(DWLIM1,DWLIM2)
C
C     gsenvr.f: ENTRY GSXENV()
C               ENTRY GSDVIC(NN1,NN2)
C               ENTRY GSBSIZ(XX1,YY1)
C               ENTRY GSDVPT(XX1,XX2,YY1,YY2)
C               ENTRY GSWNDB(XX1,XX2,YY1,YY2)
C               ENTRY GSWSCL(NN1,NN2)
C               ENTRY GSDTRN(XX1,YY1)
C
C     gsgch0.f: ENTRY GSGSY0(ISYMB,XCOFF,YCOFF,SIZX,SIZY)
C
C     gsgchc.f: ENTRY GSGCHS(CHAR1,XCOFF,YCOFF,SIZ1,SIZ2,KFONT)
C               ENTRY GSGCHI(NLETT,XCOFF,YCOFF,KFONT)
C               ENTRY GSGCHH(NCHAR,XCOFF,YCOFF,KFONT)
C
C     gsgsym.f: ENTRY GSGSYS(NSYMB,NSET,XCOFF,YCOFF,SIZX,SIZY)
C               ENTRY GSGSYC(CHAR1,NSET,XCOFF,YCOFF,SIZX,SIZY)
C
C     gsinit.f: ENTRY GSTITL(TITL)
C               ENTRY GSPRNT(JPRINT)
C
C     gsintm.f: ENTRY GSONTM(IUTERM)
C               ENTRY GSOFTM(IUTERM)
C               ENTRY GSSCTM()
C               ENTRY GSGRTM()
C               ENTRY GSMVTM(IX,IY)
C               ENTRY GSDWTM(IX,IY)
C               ENTRY GSPTTM(IX,IY)
C               ENTRY GSHRTM(IX,IY,CHKEY)
C               ENTRY GSTXTM(STRING,ISIZE)
C               ENTRY GSTYTM()
C               ENTRY GSMYTM(IX,IY)
C               ENTRY GSCYTM()
C               ENTRY GSDOTM()
C               ENTRY GSERTM()
C               ENTRY GSRVTM()
C               ENTRY GSLSTM(ITYPE)
C               ENTRY GSBLTM()
C
C     gsinum.f: ENTRY GSFNUM(FNUM,NDIGIT,NAFTER,SIZX,SIZY,NJUST)
C               ENTRY GSENUM(ENUM,NDIGIT,NAFTER,SIZX,SIZY,NJUST)
C
C     gsoflw.f: ENTRY GSOFLR(IUNITP,GSNAM)
C               ENTRY GSCFIL(IUNITP)
C               ENTRY GSWHDR()
C               ENTRY GSFLWI(IX,IY)
C               ENTRY GSUHDR()
C               ENTRY GSRHDR(KEOF)
C               ENTRY GSFLRI(IX,IY,KKEOF)
C               ENTRY GSFLP1()
C               ENTRY GSFLBR(NBYTE)
C               ENTRY GSFLSR(NBYTE)
C               ENTRY GSFLWR(IARRAY,NBYTE)
C               ENTRY GSFLRR(IARRAY,NBYTE,KKKEOF)
C
C     gspict.f: ENTRY GSWAIT(NSEC)
C               ENTRY GSENDP()
C               ENTRY GSSTOP()
C
C     gsstrc.f: ENTRY GSSTRS(TEXT,SIZX,SIZY)
C               ENTRY GSSTRH(BYTXT,NLETT)
C               ENTRY GSSTR2(ITEXT2,NLETT)
C               ENTRY GSSTR4(ITEXT4,NLETT)
C
C     gsstrd.f: ENTRY GSSTRU(TEXT,DX,DY)
C               ENTRY GSMIXC(MCOLOR)
C
C     gsstyl.f: ENTRY GSLNWT(LWT)
C               ENTRY GSCOLR(ICOLR)
C               ENTRY GSPAPR(X)
C               ENTRY GSFONT(N)
C               ENTRY GSERAS()
C               ENTRY GSEREV()
C               ENTRY GSERAX()
C
C     gstimr.f: ENTRY GSTIM0(ITPRNT)
C
C     gstsav.f: ENTRY GSTRES(NTRSAV)
C
C     gstyon.f: ENTRY GSTYOF()
C
C     gsutrn.f: ENTRY GSSCLU(A,B)
C               ENTRY GSUROT(A,B)
C               ENTRY GSUMAT(TMAT)
C               ENTRY GSORGD(X,Y)
C               ENTRY GSORGU(X,Y)
C               ENTRY GSSCLC(A,B)
C               ENTRY GSCROT(A,B)
C               ENTRY GSCMAT(TMAT)
C               ENTRY GSORGC(X,Y)
C               ENTRY GSCSPA(A,B)
C               ENTRY GSCSPU(N)
C               ENTRY GSCENC(N)
C               ENTRY GSANCD(X,Y)
C               ENTRY GSANCU(X,Y)
C               ENTRY GSFCUR(X,Y)
C               ENTRY GSPCUR(X,Y)
C               ENTRY GSSCUR(XCGAP)
C               ENTRY GSLNFD(YCDOWN)
C               ENTRY GSTLNK(N)
C               ENTRY GSCMOD(N)
C
C
      SUBROUTINE AXIS(X,Y,LABEL,NSIDE,AXLEN,ANGLE,FVAL,DV)
C     ====================================================
C
C---- Draw axes with labels for graphs
C     A.D. MCLACHLAN JULY 1984. LAST UPDATED 27 JUL 1984.
C     ADAPTED FROM D.A. AGARD PLOT82
C
C    (X,Y) = starting coordinates for axis generation (real)
C    LABEL = character text string for labeling the axis
C    NSIDE = +1 OR -1
C          = + = annotations generated above axis
C                  = - = annotations generated below axis
C    AXLEN = axis length in user units (real)
C            axis is marked every 1.0 user units
C            so user scale needs to be set properly
C    ANGLE = angle in degrees at which axis is drawn (real)
C    FVAL = first annotation value (real)
C    DV = delta annotation value (real)
C
C---- Character height is automatically set to be independent
C     of global scale
C
C---- Remove trailing blanks from label
C     max 40 characters to plot 5mm wide
C
C     .. Scalar Arguments ..
      REAL ANGLE,AXLEN,DV,FVAL,X,Y
      INTEGER NSIDE
      CHARACTER LABEL* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      REAL ADV,ANGFAC,ANL,COSA,DX,DY,EXABS,EXPDV,HGT,PI,QMAX,SIDE,SINA,
     +     SIZE,THETA,USCALY,VAL,WIDL,XX,YY
      INTEGER I,IADV,IEXPDV,IVAL,JDIG,LABLEN,NAFTER,NDIGIT,NJUST,NTIC
C     ..
C     .. Local Arrays ..
      INTEGER NTRSAV(48)
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL GSANCU,GSCENC,GSCROT,GSCSPA,GSDWTO,GSFNUM,
     +         GSFONT,GSINUM,GSMVTO,GSSCLC,GSSTRS,GSTRES,
     +         GSTSAV
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,ATAN2,COS,SIN
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (NTRSAV(11),USCALY)
C     ..
C
      LABLEN = LENSTR(LABEL)
      IF (LABLEN.GT.40) LABLEN = 40
C
      PI = ATAN2(1.0,1.0)*4.0
      ANGFAC = PI/180.0
C
C---- Save current character and user scaling
C
      CALL GSTSAV(NTRSAV)
C
C---- Set up scale for characters
C     this scaling results in  a unit hgt=1.0 mm
C     for letter sizes independent of user scale "uscaly" .
C
      HGT = (1.0/USCALY)
      CALL GSSCLC(HGT,HGT)
      THETA = ANGLE*ANGFAC
      CALL GSCROT(THETA,THETA+PI/2.0)
C
C---- Centred characters with uniform spacing
C
      CALL GSCENC(1)
      CALL GSCSPA(0.0,0.0,1)
C
C---- locate which side of axis to annotate and label
C
      SIDE = +1.0
      IF (NSIDE.LT.0) SIDE = -1.0
C
C---- This section tries to rescale dv into a range from 0.01 to 99.0
C     Determine value of 'dv' exponent
C
      EXPDV = 0.0
      ADV = ABS(DV)
C
C---- Zero delta annotation value?
C
      IF (ADV.NE.0.0) THEN
   10   CONTINUE
C
C---- 'DV' exponent calculation completed?
C     divide by 10.0 till lt. 99.0
C
        IF (ADV.GE.99.0) THEN
          ADV = ADV/10.0
          EXPDV = EXPDV + 1.0
          GO TO 10
        END IF
   20   CONTINUE
C
C---- 'DV' exponent calculation completed?
C
        IF (ADV.LT.0.01) THEN
C
C---- multiply by 10.0 till ge. 0.01
C
          ADV = ADV*10.0
          EXPDV = EXPDV - 1.0
          GO TO 20
        END IF
      END IF
C
C---- compute normalized 'fval' and 'dv' scaled by (10**-expdv)
C
      VAL = (10.0** (-EXPDV))*FVAL
      ADV = (10.0** (-EXPDV))*DV
      EXABS = ABS(EXPDV) + 0.5
      IEXPDV = EXABS
      IF (EXPDV.LT.0) IEXPDV = -IEXPDV
C
C---- check number of digits after decimal point
C
      NTIC = AXLEN + 1.0
      JDIG = 2
      QMAX = (NTIC-1)*ADV + VAL
      IF (QMAX.GT.99.99) JDIG = 1
      IADV = ADV
      IVAL = VAL
      IF ((ADV.EQ.IADV) .AND. (IVAL.EQ.VAL)) JDIG = 0
C
C---- set up positioning constants for numbers
C
      SINA = SIN(THETA)
      COSA = COS(THETA)
C
C---- numbers left justified and 7.0 mm above/below axis size 4mm
C     numbers offset by 4mm to left
C
      DX = -4.0*HGT
      DY = 7.0*SIDE*HGT
      XX = DX*COSA + X - DY*SINA
      YY = DY*COSA + Y + DX*SINA
C
C---- annotate axis in user units 1.0 units at a step
C
      CALL GSFONT(1)
      DO 30 I = 1,NTIC
        CALL GSANCU(XX,YY)
        NJUST = 1
        NAFTER = JDIG
        NDIGIT = JDIG
        SIZE = 4.0
        CALL GSFNUM(VAL,NDIGIT,NAFTER,SIZE,SIZE,NJUST)
        VAL = VAL + ADV
        XX = XX + COSA
        YY = YY + SINA
   30 CONTINUE
      IF (LABLEN.GT.0) THEN
C
C---- label axis with lablen characters and possibly the expdv value
C
        ANL = LABLEN
C
C---- does 'dv' exponent exist?
C
        IF (IEXPDV.NE.0) ANL = LABLEN + 5
C
C---- centre the label letters 5mm high  centred 14mm above/below axis
C
        SIZE = 5.0
        WIDL = 5.0*ANL
        DX = 0.5*AXLEN - 0.5*WIDL*HGT
        DY = 14.0*SIDE*HGT
        XX = DX*COSA + X - DY*SINA
        YY = DY*COSA + Y + DX*SINA
        CALL GSANCU(XX,YY)
        CALL GSSTRS(LABEL,SIZE,SIZE)
C
C---- no 'dv' exponent to plot?
C
        IF (IEXPDV.NE.0) THEN
C
C---- plot exponent label '  *10'
C
          CALL GSSTRS('  *10',SIZE,SIZE)
C
C---- plot value of exponent as superscript 13mm above/below axis size 3mm
C
          XX = (WIDL*COSA-1.0*SINA)*HGT + XX
          YY = (WIDL*SINA+1.0*COSA)*HGT + YY
C
C---- plot -iexpdv label values multiplied by 10.0**-iexpdv give true values
C
          CALL GSANCU(XX,YY)
          NJUST = 1
          NDIGIT = 1
          SIZE = 3.0
          CALL GSINUM(-IEXPDV,NDIGIT,SIZE,SIZE,NJUST)
        END IF
      END IF
C
C---- draw axis and tic marks normally 3mm long
C
      DX = -3.0*SIDE*SINA*HGT
      DY = +3.0*SIDE*COSA*HGT
      XX = X - COSA
      YY = Y - SINA
      CALL GSMVTO(X,Y)
      DO 40 I = 1,NTIC
        XX = XX + COSA
        YY = YY + SINA
        CALL GSDWTO(XX,YY)
        CALL GSMVTO(XX+DX,YY+DY)
        CALL GSDWTO(XX,YY)
   40 CONTINUE
C
C---- draw last bit of axis if required
C
      XX = AXLEN*COSA + X
      YY = AXLEN*SINA + Y
      CALL GSDWTO(XX,YY)
C
C---- restore character scale
C
      CALL GSTRES(NTRSAV)
C
C
      END
C
C
C
      SUBROUTINE GETSCL
C     =================
C
C---- Get scale factors SCALX, SCALY and translations TX, TY for
C     viewport KXMIN,KXMAX,KYMIN,KYMAX and rotation RM.
C     Plotfile limits  are IXMIN,IXMAX,IYMIN,IYMAX.
C
C---- If UNIFORM is .false. , scales on x and y are different
C
C---- Rotate limits
C
C     .. Scalars in Common ..
      REAL DOTMMX,DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWLIMX,DWLIMY,
     +     SCALX,SCALY,TX,TY
      INTEGER ICOLOR,IPICT,IUNIT,IXMAX,IXMIN,IYMAX,IYMIN,KXMAX,KXMIN,
     +        KYMAX,KYMIN,LINWT,LUNIN,LUNOUT,MCNTFL,MDEVIC,MDIREC,
     +        MIXCOLOR,MOUT,MPIC,MSCAFL,NBACK,NPICS,NREC
      LOGICAL AAV,CLEAR,EOF,HTEXT,PAN,PICTURE,ROTATE,TABLE,UNIFORM
      CHARACTER PASWRD*8,TITLEH*80
C     ..
C     .. Arrays in Common ..
      REAL RM,SPARE1,SPARE2
      INTEGER PENS,PFLAGS
C     ..
C     .. Local Scalars ..
      INTEGER PXMAX,PXMIN,PYMAX,PYMIN
C     ..
C     .. Local Arrays ..
      INTEGER PX(4),PY(4)
C     ..
C     .. External Subroutines ..
      EXTERNAL IROT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,MAX,MIN,REAL
C     ..
C     .. Common blocks ..
      COMMON /FLAGS/NBACK,PENS(8),PFLAGS(8),RM(2,2),TX,TY,SCALX,SCALY,
     +       KXMIN,KXMAX,KYMIN,KYMAX,IPICT,AAV,PAN,ROTATE,CLEAR,UNIFORM,
     +       TABLE,PICTURE,HTEXT,EOF
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSFHD/IUNIT,NREC,DOTMMX,DOTMMY,IXMIN,IXMAX,IYMIN,IYMAX,
     +       LINWT,ICOLOR,MIXCOLOR,MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,
     +       MCNTFL,DWLIMX,DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,NPICS,
     +       PASWRD,SPARE1(15),TITLEH,SPARE2(68)
C
C     .. Save Statement ..
C
      SAVE
C
      CALL IROT(PX(1),PY(1),RM,IXMIN,IYMIN)
      CALL IROT(PX(2),PY(2),RM,IXMAX,IYMAX)
      CALL IROT(PX(3),PY(3),RM,IXMIN,IYMAX)
      CALL IROT(PX(4),PY(4),RM,IXMAX,IYMIN)
      PXMIN = MIN(PX(1),PX(2),PX(3),PX(4))
      PXMAX = MAX(PX(1),PX(2),PX(3),PX(4))
      PYMIN = MIN(PY(1),PY(2),PY(3),PY(4))
      PYMAX = MAX(PY(1),PY(2),PY(3),PY(4))
C
C---- Scales
C
      SCALX = ABS((KXMAX-KXMIN)/REAL(PXMAX-PXMIN))
      SCALY = ABS((KYMAX-KYMIN)/REAL(PYMAX-PYMIN))
C
      IF (UNIFORM) THEN
        SCALX = MIN(SCALX,SCALY)
        SCALY = SCALX
      END IF
C
C---- Translation
C
      TX = ((KXMAX-KXMIN)- (PXMAX-PXMIN)*SCALX)*0.5 + KXMIN -
     +     SCALX*PXMIN
      TY = ((KYMAX-KYMIN)- (PYMAX-PYMIN)*SCALY)*0.5 + KYMIN -
     +     SCALY*PYMIN
C
      END
C
C
C
      BLOCK DATA GSBLKD
C     ===================
C
C     A.D. McLachlan JUL 1984. Last updated 27 jul 1984.
C
C     .. Scalars in Common ..
      REAL ANGFAC,CHANGX,CHANGY,CHRSCX,CHRSCY,CHRSPX,CHRSPY,DOTMMX,
     +     DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWBLMX,DWBLMY,DWLIMX,
     +     DWLIMY,PAPLMX,PAPLMY,SCALEX,SCALEY,UBXMAX,UBXMIN,UBYMAX,
     +     UBYMIN,USANGX,USANGY,V64LMX,V64LMY,XCHAR,XCSTRT,YCHAR,
     +     YCSTRT,XFOFF,YFOFF,FACX,FACY
      INTEGER ICENTC,ICOLOR,IDRLVL,IFONT,IPRINT,IUNIT,IXMAX,IXMIN,IYMAX,
     +        IYMIN,KPRINT,LINWT,LUNIN,LUNOUT,MCNTFL,MDEVIC,MDIREC,
     +        MIXCOL,MOUT,MPIC,MSCAFL,NERROR,NPICS,IDXOFF,IDYOFF,
     +        IXOLD,IYOLD,MODOLD
      LOGICAL*4 DEVCON,FONTIN,ICULNK,INITON,LINMOD,UCSPAC
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. Arrays in Common ..
      REAL CHRMAT,CUMAT,USRMAT
C     ..
C     .. Local Scalars ..
      REAL CHORGX,CHORGY,CUORGX,CUORGY,XORIG,YORIG
C     ..
C     .. Local Arrays ..
      INTEGER NCTRAN(24),NSTRAN(24)
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSCHX/CHRMAT(3,3),CHRSCX,CHRSCY,CHANGX,CHANGY,
     +       CHRSPX,CHRSPY,ICENTC,UCSPAC,IFONT,FONTIN,XCHAR,
     +       YCHAR,XCSTRT,YCSTRT,ANGFAC
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
      COMMON /GSUTR/USRMAT(3,3),SCALEX,SCALEY,USANGX,USANGY,
     +       CUMAT(3,3),LINMOD,ICULNK,KPRINT
      COMMON /GSDVT/FACX,FACY,XFOFF,YFOFF,IDXOFF,IDYOFF,
     +                IXOLD,IYOLD,MODOLD
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (XORIG,USRMAT(1,3)), (YORIG,USRMAT(2,3))
      EQUIVALENCE (CUORGX,CUMAT(1,3)), (CUORGY,CUMAT(2,3))
      EQUIVALENCE (NSTRAN(1),USRMAT(1,1))
      EQUIVALENCE (CHORGX,CHRMAT(1,3)), (CHORGY,CHRMAT(2,3))
      EQUIVALENCE (NCTRAN(1),CHRMAT(1,1))
C     ..
C     .. Data statements ..
      DATA LUNIN/5/,LUNOUT/6/
      DATA SCALEX/1.0/,SCALEY/1.0/,KPRINT/1/
      DATA ICULNK/.TRUE./
      DATA FONTIN/.FALSE./,UCSPAC/.FALSE./,IFONT/1/,ICENTC/1/
      DATA MOUT/90/,INITON/.FALSE./,DEVCON/.FALSE./
      DATA IDRLVL/0/,DOTMMX/10.0/,DOTMMY/10.0/
      DATA IXOLD/0/,IYOLD/0/,MODOLD/0/
C
      END
C
C
C
      SUBROUTINE GSCETX(TEXT,NJUST)
C     ==============================
C
C---- Plot centred character string
C
C     A.D.McLachlan JUN 1984. Last updated 16 oct 84
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were NFONTS
C
C---- Plots a character string from the character*(*) variable TEXT
C     Removes leading and trailing blanks or null characters
C     uses the current font
C     uses ICENTC option to centre characters on baseline
C     level or on +0.5 level. characters are of size 1.0*1.5
C     character units with even spacing, or else variable
C      NJUST controls centering (1)left (2)middle (3)right
C
C---- ENTRY GSCETS for variable size text
C
C     .. Scalar Arguments ..
      REAL SIZ1,SIZ2
      INTEGER NJUST
      CHARACTER TEXT* (*)
C     ..
C     .. Scalars in Common ..
      REAL ANGFAC,CHANGX,CHANGY,CHRSCX,CHRSCY,CHRSPX,CHRSPY,SCALEX,
     +     SCALEY,USANGX,USANGY,XCHAR,XCSTRT,YCHAR,YCSTRT
      INTEGER ICENTC,IFONT,KPRINT,LUNIN,LUNOUT
      LOGICAL*4 FONTIN,ICULNK,LINMOD,UCSPAC
C     ..
C     .. Arrays in Common ..
      REAL CHRMAT,CUMAT,USRMAT
      INTEGER*2 IFHT,IFSTRT,IFWID,IFX0,IFY0,LENGF
      CHARACTER*1 NFONTS
C     ..
C     .. Local Scalars ..
      REAL ATXL1,CHORGX,CHORGY,CUORGX,CUORGY,CWID,DSOFFX,DSOFFY,DSTRX,
     +     DSTRY,SIZX,SIZY,WIDSUM,X1,X2,XORIG,Y1,Y2,YORIG
      INTEGER I,ILENG,LETT,LETTER,NFIRST,NLAST,NTXLEN
      CHARACTER BLANKC*1
C     ..
C     .. Local Arrays ..
      INTEGER NCTRAN(24),NSTRAN(24)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSCFSZ,GSSTRS
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ICHAR,INDEX,LEN
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSCHX/CHRMAT(3,3),CHRSCX,CHRSCY,CHANGX,CHANGY,CHRSPX,
     +       CHRSPY,ICENTC,UCSPAC,IFONT,FONTIN,XCHAR,YCHAR,XCSTRT,
     +       YCSTRT,ANGFAC
      COMMON /GSFNT/IFSTRT(150,4),LENGF(150,4),IFX0(150,4),
     +       IFY0(150,4),IFWID(150,4),IFHT(150,4),NFONTS(4,3000,4)
      COMMON /GSUTR/USRMAT(3,3),SCALEX,SCALEY,USANGX,USANGY,
     +       CUMAT(3,3),LINMOD,ICULNK,KPRINT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (XORIG,USRMAT(1,3)), (YORIG,USRMAT(2,3))
      EQUIVALENCE (CUORGX,CUMAT(1,3)), (CUORGY,CUMAT(2,3))
      EQUIVALENCE (NSTRAN(1),USRMAT(1,1))
      EQUIVALENCE (CHORGX,CHRMAT(1,3)), (CHORGY,CHRMAT(2,3))
      EQUIVALENCE (NCTRAN(1),CHRMAT(1,1))
C     ..
C     .. Data statements ..
      DATA BLANKC/' '/
C
      SIZX = 1.0
      SIZY = 1.0
      GO TO 10
C
      ENTRY GSCETS(TEXT,SIZ1,SIZ2,NJUST)
C     ===================================
C
      SIZX = SIZ1
      SIZY = SIZ2
C
C---- Check input njust
C
   10 IF ((NJUST.LT.1) .OR. (NJUST.GT.3)) THEN
        IF (KPRINT.GE.1) WRITE (LUNOUT,FMT=6002) NJUST
        NJUST = 2
      END IF
C
C---- Eliminate illegal characters (ascii set is 32-126)
C
      ILENG = LEN(TEXT)
      DO 20 I = 1,ILENG
        LETT = ICHAR(TEXT(I:I))
        IF ((LETT.LE.31) .OR. (LETT.GE.127)) TEXT(I:I) = BLANKC
   20 CONTINUE
C
C---- Remove trailing and leading blanks
C
      NLAST = LEN(TEXT)
   30 CONTINUE
      IF (INDEX(TEXT(NLAST:NLAST),' ').EQ.1) THEN
        NLAST = NLAST - 1
        GO TO 30
      END IF
      NFIRST = 1
   40 CONTINUE
      IF (INDEX(TEXT(NFIRST:NFIRST),' ').EQ.1) THEN
        NFIRST = NFIRST + 1
        GO TO 40
      END IF
C
C---- Test for blank string
C
      IF (NLAST.GT.0) THEN
C
C---- Check for left justify - no offset
C
        IF (NJUST.EQ.1) THEN
          DSTRX = 0.0
          DSTRY = 0.0
        ELSE
C
C---- calculate space used by non-blank part of string (njust=2 or 3)
C
          NTXLEN = NLAST - NFIRST + 1
          ATXL1 = NTXLEN - 1
C
C---- Letters 1.5 units high spaced by chrspy vertically
C     mean height above baseline for all chars
C
          DSTRY = ATXL1*CHRSPY
C
C---- Width calculation uniform width of 1.0 spaced by chrspx
C
          IF (UCSPAC) THEN
            DSTRX = (CHRSPX+1.0)*ATXL1 + 1.0
          ELSE
C
C---- spacing from font tables
C
            WIDSUM = 0.0
            DO 50 I = NFIRST,NLAST
              LETTER = ICHAR(TEXT(I:I))
              CALL GSCFSZ(LETTER,X1,X2,Y1,Y2,CWID,IFONT)
              WIDSUM = WIDSUM + CWID
   50       CONTINUE
            DSTRX = ATXL1*CHRSPX + WIDSUM
          END IF
C
C---- Doubled offset for right justify
C
          IF (NJUST.EQ.3) THEN
            DSTRX = 2.0*DSTRX
            DSTRY = 2.0*DSTRY
          END IF
        END IF
C
C---- offsets (with icentc offset to be applied later, now allowed for)
C
        DSOFFX = -0.5*DSTRX
        IF (ICENTC.EQ.1) DSOFFX = DSOFFX + 0.5
        DSOFFY = -0.5*DSTRY
C
C---- move character cursor by offset
C
        XCSTRT = DSOFFX*SIZX + XCSTRT
        YCSTRT = DSOFFY*SIZY + YCSTRT
C
C---- draw leaving cursor at end of string
C
        CALL GSSTRS(TEXT(NFIRST:NLAST),SIZX,SIZY)
      ELSE IF (KPRINT.GE.1) THEN
        WRITE (LUNOUT,FMT=6000)
      END IF
C
C---- Format statements
C
 6000 FORMAT (2X,'!!!GSCETX ERROR: ',
     +        'PLOTTING BLANK OR  ZERO LENGTH STRING ')
 6002 FORMAT (2X,'!!!GSCETX ERROR: ',
     +        'NJUST=',I5,' OUT OF RANGE 1-3 RESET AS 2=CENTRAL')
C
      END
C
C
C
      SUBROUTINE GSCFIX(REEL,IFIXI)
C     ===============================
C
C---- Greatest integer function
C
C     .. Scalar Arguments ..
      REAL REEL
      INTEGER IFIXI
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INT,REAL
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
      IFIXI = INT(REEL)
      IF ((REEL-REAL(IFIXI)).LT.0.0) IFIXI = IFIXI - 1
C
      END
C
C
C
      SUBROUTINE GSCFSZ(NCHAR,XLEFT,XRIGHT,YLOW,YHIGH,CWID,KFONT)
C     =============================================================
C
C---- Finds the size of characters in the current font
C
C     A.D. McLachlan JUL 1984. Last updated 27 JUL 1984
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were NFONTS
C
C     The size is measured in character units in a
C     character box 1.0 units wide, 1.0 high above the line
C     and 0.5 low below the line
C
C     XLEFT,XRIGHT,YLOW,YHIGH are drawing limits
C
C     CWID is the character width used for spacing with
C           proportional spacing
C
C     .. Parameters ..
      REAL FACT21
      PARAMETER (FACT21=1.0/21.0)
C     ..
C     .. Scalar Arguments ..
      REAL CWID,XLEFT,XRIGHT,YHIGH,YLOW
      INTEGER KFONT,NCHAR
C     ..
C     .. Scalars in Common ..
      REAL ANGFAC,CHANGX,CHANGY,CHRSCX,CHRSCY,CHRSPX,CHRSPY,SCALEX,
     +     SCALEY,USANGX,USANGY,XCHAR,XCSTRT,YCHAR,YCSTRT
      INTEGER ICENTC,IFONT,KPRINT,LUNIN,LUNOUT
      LOGICAL*4 FONTIN,ICULNK,LINMOD,UCSPAC
C     ..
C     .. Arrays in Common ..
      REAL CHRMAT,CUMAT,USRMAT
      INTEGER*2 IFHT,IFSTRT,IFWID,IFX0,IFY0,LENGF
      CHARACTER*1 NFONTS
C     ..
C     .. Local Scalars ..
      REAL AW,CHORGX,CHORGY,CUORGX,CUORGY,RED,X1,X2,XORIG,Y1,Y2,YORIG
      INTEGER LETTER
C     ..
C     .. Local Arrays ..
      INTEGER NCTRAN(24),NSTRAN(24)
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSCHX/CHRMAT(3,3),CHRSCX,CHRSCY,CHANGX,CHANGY,CHRSPX,
     +       CHRSPY,ICENTC,UCSPAC,IFONT,FONTIN,XCHAR,YCHAR,XCSTRT,
     +       YCSTRT,ANGFAC
      COMMON /GSFNT/IFSTRT(150,4),LENGF(150,4),IFX0(150,4),
     +       IFY0(150,4),IFWID(150,4),IFHT(150,4),NFONTS(4,3000,4)
      COMMON /GSUTR/USRMAT(3,3),SCALEX,SCALEY,USANGX,USANGY,
     +       CUMAT(3,3),LINMOD,ICULNK,KPRINT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (XORIG,USRMAT(1,3)), (YORIG,USRMAT(2,3))
      EQUIVALENCE (CUORGX,CUMAT(1,3)), (CUORGY,CUMAT(2,3))
      EQUIVALENCE (NSTRAN(1),USRMAT(1,1))
      EQUIVALENCE (CHORGX,CHRMAT(1,3)), (CHORGY,CHRMAT(2,3))
      EQUIVALENCE (NCTRAN(1),CHRMAT(1,1))
C
      IF (KFONT.EQ.0) THEN
        XLEFT = 0.0
        XRIGHT = 1.0
        YHIGH = 1.0
        YLOW = 0.0
        CWID = 1.0
      ELSE
C
C---- check font
C
        IF ((KFONT.GE.1) .AND. (KFONT.LE.4)) THEN
C
C---- check letter
C
          LETTER = NCHAR - 31
          IF ((LETTER.GE.1) .AND. (LETTER.LE.146)) THEN
C
C---- for legal letters in fonts (1-4)
C
            X1 = IFX0(LETTER,KFONT) + 1
            Y1 = IFY0(LETTER,KFONT) + 9
            X2 = IFWID(LETTER,KFONT) - 1
            Y2 = IFHT(LETTER,KFONT) + 9
C
C---- some wide or high letters are shrunk down
C
            AW = X2
            IF (AW.GT.21.0) THEN
              RED = (21.0/AW)
              X1 = X1*RED
              X2 = X2*RED
            END IF
            IF (Y2.GT.21.0) THEN
              Y1 = (21.0/Y2)*Y1
              Y2 = 21.0
            END IF
            XLEFT = FACT21*X1
            XRIGHT = FACT21*X2
            YLOW = FACT21*Y1
            YHIGH = FACT21*Y2
C
            CWID = IFWID(LETTER,KFONT)
            IF (CWID.GT.21.0) CWID = 21.0
            IF (CWID.LE.0.0) CWID = 18.0
            CWID = CWID*FACT21
            GO TO 10
          ELSE IF (KPRINT.GE.1) THEN
            WRITE (LUNOUT,FMT=6000) NCHAR
          END IF
        ELSE IF (KPRINT.GE.1) THEN
          WRITE (LUNOUT,FMT=6002) KFONT
        END IF
C
C---- for bad data return maximum sizes
C
        XLEFT = 0.0
        XRIGHT = 1.0
        YHIGH = 1.0
        YLOW = -0.5
        CWID = 1.0
      END IF
   10 IF (KPRINT.GE.3) WRITE (LUNOUT,FMT=6004) NCHAR,KFONT,XLEFT,
     +    XRIGHT,YLOW,YHIGH,CWID
C
C---- Format statements
C
 6000 FORMAT (2X,'!!!GSCFSZ ERROR: ILLEGAL NCHAR=',I5)
 6002 FORMAT (2X,'!!!GSCFSZ ERROR: ILLEGAL KFONT=',I5)
 6004 FORMAT (2X,'GSCFSZ: NCHAR KFONT =',
     +        2I5,/2X,'XLEFT XRIGHT YLOW Y',
     +       'HIGH = ',4F10.4,/2X,'WIDTH = ',F10.4)
C
      END
C
C
C
      SUBROUTINE GSCLPL(X1,Y1,X2,Y2,ACCEPT)
C     =====================================
C
C---- Routine to clip lines in a rectangular window
C
C     A.D. McLachlan JUN 1984 Last updated 10 JUL 1984
C
C     Cohen and Sutherland algorithm from foley & van dam
C
C     .. Scalar Arguments ..
      REAL X1,X2,Y1,Y2
      LOGICAL ACCEPT
C     ..
C     .. Scalars in Common ..
      REAL BXMAX,BXMIN,BYMAX,BYMIN
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      REAL SMALL
      INTEGER NCODE1,NCODE2,NO4,NSWAP
      LOGICAL DONE,INSIDE,REJECT
C     ..
C     .. Local Arrays ..
      LOGICAL*1 NOCODE(4),WCODEA(4),WCODEB(4)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSCLPT,GSCLTS,GSSWLN
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSCLP/BXMIN,BXMAX,BYMIN,BYMAX
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (WCODEA(1),NCODE1), (WCODEB(1),NCODE2)
      EQUIVALENCE (NOCODE(1),NO4)
C     ..
C     .. Data statements ..
      DATA NOCODE/4*.FALSE./
      DATA SMALL/1.0E-20/
C
      NSWAP = 1
      ACCEPT = .FALSE.
      REJECT = .FALSE.
      DONE = .FALSE.
   10 CONTINUE
C
C---- start of line clipping loop "level 1" here
C
C---- start loop now
C     find position codes for both ends of line
C     bytes 1=left 2=right 3=below 4=above
C
      CALL GSCLPT(X2,Y2,NCODE2,INSIDE)
      CALL GSCLPT(X1,Y1,NCODE1,INSIDE)
C
C---- test for trivial cases (both ends inside or both share same excess)
C
      CALL GSCLTS(REJECT,ACCEPT,NCODE1,NCODE2)
C
C---- enter "if1" "level 2"
C
      IF (REJECT .OR. ACCEPT) THEN
        DONE = .TRUE.
      ELSE
C
C---- "else2" "level 2"
C
C
C---- subdivide line since at least one endpoint is outside
C    -if (x1,y1) is inside swap ends so that (x1,y1) is now outside
C
        IF (INSIDE) CALL GSSWLN(X1,Y1,X2,Y2,NCODE1,NCODE2,NSWAP)
C
C---- replace (x1,y1) by an end point on the boundary
C    -use y=y1+slope*(x-x1) or
C    -    x=x1+(1.0/slope)*(y-y1)
C    -check for divides by zero when lines are horizontal or vertical
C     "if1" "level 3"
C    -split line on left (y1 is to left)
C
        IF (WCODEA(1)) THEN
          IF (ABS(X2-X1).GT.SMALL) Y1 = Y1 + (Y2-Y1)*(BXMIN-X1)/(X2-X1)
          X1 = BXMIN
C
C---- "else2" "level 3" split line on right (y1 is to right)
C
        ELSE IF (WCODEA(2)) THEN
          IF (ABS(X2-X1).GT.SMALL) Y1 = Y1 + (Y2-Y1)*(BXMAX-X1)/(X2-X1)
          X1 = BXMAX
C
C---- "else3" "level 3" split line at bottom (y1 is below)
C
        ELSE IF (WCODEA(3)) THEN
          IF (ABS(Y2-Y1).GT.SMALL) X1 = X1 + (X2-X1)*(BYMIN-Y1)/(Y2-Y1)
          Y1 = BYMIN
C
C---- "else4" "level 3" split line at top (y1 is above)
C
        ELSE IF (WCODEA(4)) THEN
          IF (ABS(Y2-Y1).GT.SMALL) X1 = X1 + (X2-X1)*(BYMAX-Y1)/(Y2-Y1)
          Y1 = BYMAX
C
C---- "endif" "level 3"
C
        END IF
C
C---- "endif" "level 2"
C
      END IF
C
C---- "end loop" "level 1"
C
      IF (.NOT.DONE) GO TO 10
C
C---- swap back line ends if required
C
      IF (NSWAP.EQ.-1) 
     +      CALL GSSWLN(X1,Y1,X2,Y2,NCODE1,NCODE2,NSWAP)
C
      END
C
C
C
      SUBROUTINE GSCLPT(X,Y,NCODE,INSIDE)
C     ====================================
C
C     A.D. McLachlan JUN 1984 Last updated 10 JUL 1984
C
C---- To test if a point is inside rectangular window
C     BYTE codes 1=left 2=right 3=below 4=above
C
C     .. Scalar Arguments ..
      REAL X,Y
      INTEGER NCODE
      LOGICAL*1 INSIDE
C     ..
C     .. Scalars in Common ..
      REAL BXMAX,BXMIN,BYMAX,BYMIN
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      INTEGER NO4,NWCODE
C     ..
C     .. Local Arrays ..
      LOGICAL*1 NOCODE(4),WCODE(4)
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSCLP/BXMIN,BXMAX,BYMIN,BYMAX
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (NWCODE,WCODE(1)), (NO4,NOCODE(1))
C     ..
C     .. Data statements ..
      DATA NOCODE/4*.FALSE./
C
      WCODE(1) = (X.LT.BXMIN)
      WCODE(2) = (X.GT.BXMAX)
      WCODE(3) = (Y.LT.BYMIN)
      WCODE(4) = (Y.GT.BYMAX)
      NCODE = NWCODE
      INSIDE = (NCODE.EQ.NO4)
C
      END
C
C
C
      SUBROUTINE GSCLTS(REJECT,ACCEPT,NCODE1,NCODE2)
C     ===============================================
C
C     A.D. McLachlan JUN 1984 Last updated 10 JUL 1984
C
C---- To test if line lies wholly in or out of window
C     in cohen & sutherland test
C
C     .. Scalar Arguments ..
      INTEGER NCODE1,NCODE2
      LOGICAL*1 ACCEPT,REJECT
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      INTEGER NCDAND,NO4
C     ..
C     .. Local Arrays ..
      LOGICAL*1 NOCODE(4)
C     ..
C     .. External Functions ..
cc      INTEGER IAND
cc      EXTERNAL IAND
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (NOCODE(1),NO4)
C     ..
C     .. Data statements ..
      DATA NOCODE/4*.FALSE./
C
C---- NCDAND = IAND(NCODE1,NCODE2)
C
      NCDAND = NCODE1 .AND. NCODE2
      REJECT = (NCDAND.NE.NO4)
      ACCEPT = ((NCODE1.EQ.NO4) .AND. (NCODE2.EQ.NO4))
C
      END
C
C
C
      SUBROUTINE GSCVAX(X,Y,LABEL,NSIDE,NTEX,AXLEN,GSCALE,
     +                    ANGLE,FVAL,DV)
C     ======================================================
C
C---- Draw axes with labels for graphs
C
C     A.D. McLachlan SEPT 1984. Last updated 12 NOV 1984.
C     adapted from d.a. agard plot82
C
C    (X,Y) = starting coordinates for axis generation (real)
C    LABEL = character text string for labeling the axis
C    NSIDE = +1 or -1
C          = + = annotations generated above axis
C          = - = annotations generated below axis
C    NTEX  = 1,0 do, do not plot labels and tic numbers along axis
C    AXLEN = axis length in graph units (real)
C            axis is marked every 1.0 graph units
C    GSCALE= value of 1 graph unit in user units (expected to be mm)
C            no value numbers drawn if gscale.lt.10.0
C    ANGLE = angle in degrees at which axis is drawn (real)
C    FVAL  = first annotation value (real)
C    DV    = delta annotation value (real)
C
C---- Character height is automatically set to be independent
C     of global scale unless it is too large to go on axes
C     character fount is not set here
C
C---- remove trailing blanks from label
C     max 40 characters to plot 5mm wide
C
C     .. Scalar Arguments ..
      REAL ANGLE,AXLEN,DV,FVAL,GSCALE,X,Y
      INTEGER NSIDE,NTEX
      CHARACTER LABEL* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      REAL ADV,ANGFAC,ANL,COSA,DX,DY,EXABS,EXPDV,HGT,PI,QMAX,SIDE,SINA,
     +     SIZE,SIZL,SPACL,SZMAX,SZMIN,THETA,VAL,WIDL,XX,YY
      INTEGER I,IADV,IEXPDV,IVAL,JDIG,LABLEN,NAFTER,NDIGIT,NJUST,NTIC
C     ..
C     .. Local Arrays ..
      INTEGER NTRSAV(48)
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL GSANCU,GSCENC,GSCROT,GSCSPU,
     +         GSDWTO,GSFNUM,GSINUM,
     +         GSMVTO,GSORGC,GSSCLC,
     +         GSSTRS,GSTLNK,GSTRES,GSTSAV
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,ATAN2,COS,SIN
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
      LABLEN = LENSTR(LABEL)
      IF (LABLEN.GT.40) LABLEN = 40
C
      PI = ATAN2(1.0,1.0)*4.0
      ANGFAC = PI/180.0
C
C---- save current character and user scaling
C
      CALL GSTSAV(NTRSAV)
C
C---- decouple character scaling from user scale
C     set up scale for characters
C     this scaling results in  a unit hgt=1.0 mm
C     for letter sizes independent of user scale "uscaly" .
C
      HGT = 1.0
      CALL GSTLNK(0)
      CALL GSORGC(0.0,0.0)
      CALL GSSCLC(HGT,HGT)
      THETA = ANGLE*ANGFAC
      CALL GSCROT(THETA,THETA+PI/2.0)
C
C---- centred characters with uniform spacing
C
      CALL GSCENC(1)
      CALL GSCSPU(1)
C
C---- choose which side of axis to annotate and label
C
      SIDE = +1.0
      IF (NSIDE.LT.0) SIDE = -1.0
C
C---- this section tries to rescale dv into a range from 0.01 to 99.0
C     determine value of 'dv' exponent
C
      EXPDV = 0.0
      ADV = ABS(DV)
C
C---- zero delta annotation value?
C
      IF (ADV.NE.0.0) THEN
   10   CONTINUE
C
C---- 'dv' exponent calculation completed?
C     divide by 10.0 till lt. 99.0
C
        IF (ADV.GE.99.0) THEN
          ADV = ADV/10.0
          EXPDV = EXPDV + 1.0
          GO TO 10
        END IF
   20   CONTINUE
C
C---- 'dv' exponent calculation completed?
C
        IF (ADV.LT.0.01) THEN
C
C---- multiply by 10.0 till ge. 0.01
C
          ADV = ADV*10.0
          EXPDV = EXPDV - 1.0
          GO TO 20
        END IF
      END IF
C
C---- compute normalized 'fval' and 'dv' scaled by (10**-expdv)
C
      VAL = (10.0** (-EXPDV))*FVAL
      ADV = (10.0** (-EXPDV))*DV
      EXABS = ABS(EXPDV) + 0.5
      IEXPDV = EXABS
      IF (EXPDV.LT.0) IEXPDV = -IEXPDV
C
C---- check number of digits after decimal point
C
      NTIC = AXLEN + 1.0
      JDIG = 2
      QMAX = (NTIC-1)*ADV + VAL
      IF (QMAX.GT.99.99) JDIG = 1
      IADV = ADV
      IVAL = VAL
      IF ((ADV.EQ.IADV) .AND. (IVAL.EQ.VAL)) JDIG = 0
C
C---- set up positioning constants for numbers
C
      SINA = SIN(THETA)
      COSA = COS(THETA)
C
C---- numbers left justified and 7.0 mm above/below axis size 4mm
C     numbers offset by 4mm to left
C
      DX = -4.0*HGT
      DY = 7.0*SIDE*HGT
      XX = DX*COSA + X - DY*SINA
      YY = DY*COSA + Y + DX*SINA
C
C---- annotate axis in graph units 1.0 units at a step
C
      NJUST = 1
      NAFTER = JDIG
      NDIGIT = JDIG
      SIZE = 4.0
C
C---- adjust size if necessary to allow 5 digits
C     and a space to each segment
C     but do not go below 2.0mm
C     no values drawn if gscale.lt.10.0
C
      SZMAX = GSCALE/6.0
      SZMIN = 2.0
      IF (SIZE.GT.SZMAX) SIZE = SZMAX
      IF (SIZE.LT.SZMIN) SIZE = SZMIN
C
      IF (GSCALE.GE.10.0) THEN
        IF (NTEX.NE.0) THEN
          DO 30 I = 1,NTIC
            CALL GSANCU(XX,YY)
            CALL GSFNUM(VAL,NDIGIT,NAFTER,SIZE,SIZE,NJUST)
            VAL = VAL + ADV
            XX = COSA*GSCALE + XX
            YY = SINA*GSCALE + YY
   30     CONTINUE
        END IF
      END IF
      IF (NTEX.NE.0) THEN
        IF (LABLEN.GT.0) THEN
C
C---- label axis with lablen characters and possibly the expdv value
C
          ANL = LABLEN
C
C---- does 'dv' exponent exist?
C
          IF (IEXPDV.NE.0) ANL = LABLEN + 5
C
C---- centre the label
C     letters 5mm high  centred 14mm above/below axis
C
          SIZE = 5.0
C
C---- shrink letters if too big to fit allowed space
C     allow space of 80% axis length
C
          SPACL = 0.8*AXLEN*GSCALE
          SIZL = SPACL/ANL
          IF (SIZL.LT.5.0) SIZE = SIZL
          IF (SIZE.LT.2.0) SIZE = 2.0
          WIDL = SIZE*ANL
          DX = (AXLEN*GSCALE)*0.5 - 0.5*WIDL*HGT
          DY = 14.0*SIDE*HGT
          XX = DX*COSA + X - DY*SINA
          YY = DY*COSA + Y + DX*SINA
          CALL GSANCU(XX,YY)
          CALL GSSTRS(LABEL,SIZE,SIZE)
C
C---- no 'dv' exponent to plot?
C

          IF ((GSCALE.GE.10.0) .AND. (IEXPDV.NE.0)) THEN
C
C---- plot exponent label '  *10'
C
            CALL GSSTRS('  *10',SIZE,SIZE)
C
C---- plot value of exponent as superscript
C     13mm above/below axis size 3mm
C
            XX = (WIDL*COSA-1.0*SINA)*HGT + XX
            YY = (WIDL*SINA+1.0*COSA)*HGT + YY
C
C---- plot -iexpdv
C     label values multiplied by 10.0**-iexpdv give true values
C
            CALL GSANCU(XX,YY)
            NJUST = 1
            NDIGIT = 1
            SIZE = SIZE*0.6
            IF (SIZE.LT.2.0) SIZE = 2.0
            CALL GSINUM(-IEXPDV,NDIGIT,SIZE,SIZE,NJUST)
          END IF
        END IF
      END IF
C
C---- draw axis and tic marks normally 3mm long
C
      DX = -3.0*SIDE*SINA*HGT
      DY = +3.0*SIDE*COSA*HGT
      XX = X - COSA*GSCALE
      YY = Y - SINA*GSCALE
      CALL GSMVTO(X,Y)
      DO 40 I = 1,NTIC
        XX = COSA*GSCALE + XX
        YY = SINA*GSCALE + YY
        CALL GSDWTO(XX,YY)
        CALL GSMVTO(XX+DX,YY+DY)
        CALL GSDWTO(XX,YY)
   40 CONTINUE
C
C---- draw last bit of axis if required
C
      XX = (AXLEN*GSCALE)*COSA + X
      YY = (AXLEN*GSCALE)*SINA + Y
      CALL GSDWTO(XX,YY)
C
C---- restore character scale
C
      CALL GSTRES(NTRSAV)
C
      END
C
C
C
      SUBROUTINE GSCVLF(CARD,XNUM,NFIELDS)
C     =====================================
C
C---- Subroutine to do free-format conversion
C
C     A.D. MCLACHLAN JULY 1984. REVISED 4 SEP 1984.
C
C     .. Scalar Arguments ..
      INTEGER NFIELDS
      CHARACTER CARD* (*)
C     ..
C     .. Array Arguments ..
      REAL XNUM(*)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      INTEGER I,IEND,ISTART,NCHAR,NPOINT
      CHARACTER ASTER*1,BLANK*1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX,LEN
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Data statements ..
      DATA ASTER,BLANK/'*',' '/
C     ..
C
C
      NCHAR = LEN(CARD)
      DO 10 I = 1,20
        XNUM(I) = 0.0
   10 CONTINUE
      NFIELDS = 0
C
C---- Search for starting point
C
      ISTART = 1
   20 CONTINUE
      IF ((CARD(ISTART:ISTART).EQ.BLANK) .AND. (ISTART.LT.NCHAR)) THEN
        ISTART = ISTART + 1
        GO TO 20
      END IF
   30 CONTINUE
C
C---- Decode fields in g format
C
      IF (ISTART.LT.NCHAR) THEN
        NFIELDS = NFIELDS + 1
        NPOINT = INDEX(CARD(ISTART:),BLANK) - 1
        IEND = ISTART + NPOINT - 1
C
C----   6333    FORMAT(G<NPOINT>.0)
C
6333    FORMAT(G10.0)
C
C----   DECODE(NPOINT,6333,CARD(ISTART:IEND)) XNUM(NFIELDS)
C
        READ (CARD(ISTART:IEND),FMT=6333) XNUM(NFIELDS) 
        ISTART = IEND + 2
   40   CONTINUE
C
C---- Skip over repeated blanks
C
        IF ((CARD(ISTART:ISTART).EQ.BLANK) .AND. (ISTART.LT.NCHAR)) THEN
          ISTART = ISTART + 1
          GO TO 40
        ELSE
          GO TO 30
        END IF
      END IF
C
      END
C
C
C
      SUBROUTINE GSCVLI(CHLINE,CHOUT,NCHAR)
C     =======================================
C
C     A.D. MCLACHLAN JUL 1984 . LAST UPDATED 9 OCT 1984
C
C---- Subroutine to scan through input (chline) look for "!"
C     adapted from d.a. agard "curvy"
C     return portion up to ! in chout and character count in nchar
C     if no "!" found then return up to 60 characters
C     and trim trailing blanks
C     if "!" then do not trim
C     note!! both input/output variables are character variables
C
C     .. Scalar Arguments ..
      INTEGER NCHAR
      CHARACTER CHOUT*60,CHLINE* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      INTEGER MAXCHR,NBEFOR
      CHARACTER EXCL*1
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Data statements ..
      DATA EXCL/'!'/
      DATA MAXCHR/60/
C
      NCHAR = 0
C
C---- Search for "!" in first maxchr+1  positions
C
      NBEFOR = INDEX(CHLINE,EXCL) - 1
      IF (NBEFOR.GT.MAXCHR) NBEFOR = MAXCHR
C
C---- when no "!"
C
      IF (NBEFOR.LT.0) THEN
        NBEFOR = MAXCHR
C
C---- trim trailing blanks
C
        NBEFOR = LENSTR(CHLINE)
      END IF
C
C---- When first character is "!" or string is blank return nchar=0
C
      IF (NBEFOR.NE.0) THEN
C
C---- copy into chout
C
        CHOUT = CHLINE(1:NBEFOR)
        NCHAR = NBEFOR
      END IF
C
      END
C
C
C
      SUBROUTINE GSCVSC(AMIN,AMAX,AXLEN,XLOW,DX)
C     ============================================
C
C---- plot scaling routine for data ranging from amin to amax
C
C     A.D. MCLACHLAN JULY 1984. LAST UPDATED 5 SEP 1984
C
C     adapted from d.a. agard "curvy" 1982
C
C     GSCVSC selects the 'nicest?' possible xlow & dx for axis
C     length axlen, using integer part of axlen to draw on
C     this routine uses the greatest integer routine GScfix(r,i)
C
C     .. Scalar Arguments ..
      REAL AMAX,AMIN,AXLEN,DX,XLOW
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      REAL AXL,DECFAC,DEL,DL,DM,POW10,XHIGH,XREM
      INTEGER ILOW,IPOW10,IREM,JD,NDM,NXLEN
C     ..
C     .. Local Arrays ..
      REAL DMC(11)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSCFIX
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC LOG10,REAL
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Data statements ..
      DATA NDM/11/
      DATA DMC/1.0,1.25,1.5,2.0,2.5,3.0,4.0,5.0,6.0,8.0,10.0/
C
      IF (AXLEN.NE.0) THEN
C
C---- The plot axis is of length "axlen" graph units
C     "NXLEN" is the number of complete segments along the axis
C     "(NXLEN+1)" tick marks will be drawn
C
        NXLEN = AXLEN
        AXL = NXLEN
C
C---- "DEL" is the range of "x" covered by one segment
C
        DEL = (AMAX-AMIN)*0.99/AXL
        IF (DEL.GT.0.0) THEN
C
          DL = LOG10(DEL)
          CALL GSCFIX(DL,IPOW10)
          POW10 = IPOW10
          DECFAC = 10.0**POW10
          DM = DEL/DECFAC
C
C---- "DM" is a number between 1.0 and 9.99 got by scaling off
C     "POW10" powers of 10.0 from "del"
C     "DX" is a nice value for the range, just above "dm" in size
C          select nice dx
C
          JD = 1
   10     CONTINUE
C
C---- Search for demarcation value just above dm
C
          IF (DM.GT.DMC(JD)) THEN
            JD = JD + 1
            GO TO 10
          END IF
   20     CONTINUE
C
C---- Compute nice xlow
C     "XLOW" is a multiple of "dx" just below the observed "amin"
C            and is the value of "x" to align with the origin
C            tick mark
C    "XHIGH" is the value of "x" to plot at the last tick mark
C            and we should have "xhigh" .ge. "amax"
C
          DX = DMC(JD)*DECFAC
          CALL GSCFIX((AMIN/DX),ILOW)
          XLOW = REAL(ILOW)*DX
          XHIGH = REAL(ILOW+NXLEN)*DX
          XREM = XHIGH - AMAX
          CALL GSCFIX((XREM/DX),IREM)
C
C---- Must increase dx & redo xlow?
C
          IF (XREM.LT. (-DEL*0.01)) THEN
C
C---- increase dx. if at end of range go to next power of 10.0
C
            JD = JD + 1
            IF (JD.GT.NDM) THEN
              JD = 2
              DECFAC = DECFAC*10.0
            END IF
            GO TO 20
          END IF
C
C---- Centre plot on axis
C
          XLOW = XLOW - REAL(IREM/2)*DX
          IF ((AMIN.LE.0.5*DX) .AND. (AMIN.GE.-0.1*DX)) XLOW = 0.0
          RETURN
        END IF
      END IF
      WRITE (LUNOUT,FMT=6000) AMIN,AMAX,AXLEN
      XLOW = 0.0
      DX = 1.0
C
C---- Format statements
C
 6000 FORMAT (2X,'!!!GSCVSC ARGUMENT ERROR:',/2X,'AMIN=',E14.5,' AMA',
     +       'X= ',E14.5,' AXLEN= ',F10.4)
C
      END
C
C
C
      SUBROUTINE GSDDRB(XB,YB)
C     =========================
C
C     GSDDRB draw to point         \   convert pen movements on the
C     GSMDRB move to point          |  drawing board into instructions
C     GSDOTB draw a point or dot   /   to the plotfile or plot device
C
C     A.D. McLachlan JUN 1984.  Last updated 28 SEP 1984
C
C     .. Scalar Arguments ..
      REAL XB,YB
C     ..
C     .. Scalars in Common ..
      REAL DOTMMX,DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWBLMX,DWBLMY,
     +     DWLIMX,DWLIMY,FACX,FACY,PAPLMX,PAPLMY,UBXMAX,UBXMIN,UBYMAX,
     +     UBYMIN,V64LMX,V64LMY,XFOFF,YFOFF
      INTEGER ICOLOR,IDRLVL,IDXOFF,IDYOFF,IPRINT,IUNIT,IXMAX,IXMIN,
     +        IXOLD,IYMAX,IYMIN,IYOLD,LINWT,LUNIN,LUNOUT,MCNTFL,MDEVIC,
     +        MDIREC,MIXCOL,MODOLD,MOUT,MPIC,MSCAFL,NERROR,NPICS
      LOGICAL*4 DEVCON,INITON
      CHARACTER FILNAM*80,TITLE*80,SRNAME*4
C     ..
C     .. Local Scalars ..
      INTEGER IDOT,IEND,IERAS,ILWT,IPAP,IPEN,IX,IY,MODE
C     ..
C     .. External Subroutines ..
      EXTERNAL GSDWTM,GSFLWI,GSLVCK,GSMVTM,GSPTTM
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSDVT/FACX,FACY,XFOFF,YFOFF,IDXOFF,IDYOFF,
     +                IXOLD,IYOLD,MODOLD
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Data statements ..
C
C---- plotfile command codes
C
      DATA IEND/-1/,IDOT/-2/,ILWT/-3/,IPEN/-4/,IPAP/-5/,IERAS/-6/
C
C---- draw to point
C
      MODE = 1
      SRNAME = 'DDRB'
      GO TO 10
C
      ENTRY GSDOTB(XB,YB)
C     ====================
C
C---- Plot a single dot
C
      MODE = 2
      SRNAME = 'DOTB'
      GO TO 10
C
      ENTRY GSMDRB(XB,YB)
C     ====================
C
C---- Move to point
C
      MODE = 3
      SRNAME = 'MDRB'
      GO TO 10
C
      ENTRY GSVBRK()
C     ================
C
C---- Break the current chaining of plotted points
C
      IXOLD = 0
      IYOLD = 0
      MODOLD = 0
      RETURN
C
C---- check level
C
   10 IF (IDRLVL.NE.3) CALL GSLVCK('GS'//SRNAME)
C
C---- convert coordinates to integers in range 1:32677
C
      IX = NINT(XB*FACX+XFOFF) + IDXOFF
      IY = NINT(YB*FACY+YFOFF) + IDYOFF
C
C---- update plot limits
C
      IF (IX.LT.IXMIN) IXMIN = IX
      IF (IX.GT.IXMAX) IXMAX = IX
      IF (IY.LT.IYMIN) IYMIN = IY
      IF (IY.GT.IYMAX) IYMAX = IY
C
C---- report values
C
      IF (IPRINT.GE.3) WRITE (LUNOUT,FMT=6000) SRNAME,XB,YB,IX,IY
C
C---- test to avoid repeated pen movements if new point is same as old
C     reject one move or repeated draw/dot allow line of zero length 
C     (draw after move)
C
      IF ((IX.EQ.IXOLD) .AND. (IY.EQ.IYOLD)) THEN
        IF ((MODE.EQ.3) .OR. (MODE.EQ.MODOLD)) RETURN
      END IF
C
C---- Log operation in meta-file
C
      IF (MODE.EQ.1) THEN
        CALL GSFLWI(IX,IY)
      ELSE IF (MODE.EQ.2) THEN
        CALL GSFLWI(IDOT,0)
        CALL GSFLWI(IX,IY)
      ELSE IF (MODE.EQ.3) THEN
        CALL GSFLWI(IX,-IY)
      ENDIF
C
C---- Write to Screen in interactive
C
      IF (MDIREC.EQ.1 .AND. MDEVIC.EQ.3) THEN
        IF (MODE.EQ.1) THEN
          CALL GSDWTM(IX,IY)
        ELSE IF (MODE.EQ.2) THEN
          CALL GSPTTM(IX,IY)
        ELSE IF (MODE.EQ.3) THEN
          CALL GSMVTM(IX,IY)
        END IF
      END IF
C
      IXOLD = IX
      IYOLD = IY
      MODOLD = MODE
C
C---- Format statements
C
 6000 FORMAT (2X,'GS',A,' ',2F10.5,2I6)
C
      END
C
C
C
      SUBROUTINE GSDRMV(X,Y,N)
C     =========================
C
C---- Line plotting routine adapted from d.a. agard's plot82
C
C     A.D. McLachlan JUN 1984 Last updated 27 JUL 1984
C
C---- First call must be a moveto to get pen in a defined position
C
C     .. Scalar Arguments ..
      REAL X,Y
      INTEGER N
C     ..
C     .. Scalars in Common ..
      REAL ANGFAC,CHANGX,CHANGY,CHRSCX,CHRSCY,CHRSPX,CHRSPY,DOTMMX,
     +     DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWBLMX,DWBLMY,DWLIMX,
     +     DWLIMY,PAPLMX,PAPLMY,SCALEX,SCALEY,UBXMAX,UBXMIN,UBYMAX,
     +     UBYMIN,USANGX,USANGY,V64LMX,V64LMY,XBNEW,XBOLD,XCHAR,XCSTRT,
     +     XNOW,YBNEW,YBOLD,YCHAR,YCSTRT,YNOW
      INTEGER ICENTC,ICOLOR,IDRLVL,IFONT,IPRINT,IUNIT,IXMAX,IXMIN,IYMAX,
     +        IYMIN,KPRINT,LINWT,LUNIN,LUNOUT,MCNTFL,MDEVIC,MDIREC,
     +        MIXCOL,MOUT,MPIC,MSCAFL,NERROR,NPICS
      LOGICAL*4 DEVCON,FONTIN,ICULNK,INITON,LINMOD,UCSPAC
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. Arrays in Common ..
      REAL CHRMAT,CUMAT,USRMAT
C     ..
C     .. Local Scalars ..
      REAL CHORGX,CHORGY,CUORGX,CUORGY,X1,X2,XORIG,XP,Y1,Y2,YORIG,YP
      INTEGER MODE,NCODE
      LOGICAL*1 ACCEPT,INSIDE,LSTOUT,OLDOUT
C     ..
C     .. Local Arrays ..
      INTEGER NCTRAN(24),NSTRAN(24)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSCLPL,GSCLPT,GSDDRB,GSDOTB,GSMDRB
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSCHX/CHRMAT(3,3),CHRSCX,CHRSCY,CHANGX,CHANGY,
     +       CHRSPX,CHRSPY,ICENTC,UCSPAC,IFONT,FONTIN,XCHAR,
     +       YCHAR,XCSTRT,YCSTRT,ANGFAC
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
      COMMON /GSDWX/XNOW,YNOW,XBNEW,YBNEW,XBOLD,YBOLD
      COMMON /GSUTR/USRMAT(3,3),SCALEX,SCALEY,USANGX,USANGY,
     +       CUMAT(3,3),LINMOD,ICULNK,KPRINT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (XORIG,USRMAT(1,3)), (YORIG,USRMAT(2,3))
      EQUIVALENCE (CUORGX,CUMAT(1,3)), (CUORGY,CUMAT(2,3))
      EQUIVALENCE (NSTRAN(1),USRMAT(1,1))
      EQUIVALENCE (CHORGX,CHRMAT(1,3)), (CHORGY,CHRMAT(2,3))
      EQUIVALENCE (NCTRAN(1),CHRMAT(1,1))
C     ..
C     .. Entry Points ..
      ENTRY GSDWTO(X,Y)
C     ==================
C
      MODE = 1
      IF (KPRINT.GE.3) WRITE (LUNOUT,FMT=6000)
      GO TO 10
C
      ENTRY GSDWBY(X,Y)
C     ==================
C
      MODE = 2
      IF (KPRINT.GE.3) WRITE (LUNOUT,FMT=6002)
      GO TO 10
C
      ENTRY GSPOIN(X,Y)
C     ==================
C
      MODE = 3
      IF (KPRINT.GE.3) WRITE (LUNOUT,FMT=6014)
      GO TO 10
C
      ENTRY GSMVTO(X,Y)
C     ==================
C
      MODE = 4
      IF (KPRINT.GE.3) WRITE (LUNOUT,FMT=6004)
      GO TO 10
C
      ENTRY GSMVBY(X,Y)
C     ==================
C
      MODE = 5
      IF (KPRINT.GE.3) WRITE (LUNOUT,FMT=6006)
C
   10 CONTINUE
C
C---- (xp,yp) is target position for the pen
C
      GO TO (20,30,20,20,30) MODE
   20 XP = X
      YP = Y
      GO TO 40
   30 XP = XNOW + X
      YP = YNOW + Y
C
C---- update the record of old and new requested user coordinates
C
   40 CONTINUE
      XNOW = XP
      YNOW = YP
C
C---- convert target point to drawing board coords
C
C---- use user or character transformation
C
      IF (LINMOD) THEN
        XBNEW = USRMAT(1,1)*XP + USRMAT(1,2)*YP + XORIG
        YBNEW = USRMAT(2,1)*XP + USRMAT(2,2)*YP + YORIG
      ELSE
        XBNEW = CUMAT(1,1)*XP + CUMAT(1,2)*YP + CUORGX + XCHAR
        YBNEW = CUMAT(2,1)*XP + CUMAT(2,2)*YP + CUORGY + YCHAR
      END IF
      X2 = XBNEW
      Y2 = YBNEW
C
C---- update record of requested drawing board coords
C     these are not always the same as the pen positions because of
C     the clipping of the lines. the pen always stays on the board
C     oldout records if previous point was off the board
C
      OLDOUT = LSTOUT
C
C---- check whether new point lies on  board
C
      CALL GSCLPT(XBNEW,YBNEW,NCODE,INSIDE)
      LSTOUT = (.NOT.INSIDE)
C
C---- record error
C
      IF (LSTOUT) NERROR = NERROR + 1
C
C---- for point save new position but only draw it if inside
C
      IF ((MODE.EQ.3) .AND. INSIDE) THEN
        CALL GSDOTB(X2,Y2)
C
C---- for moveto or moveby save new position but only move pen
C     if position is on board
C
      ELSE IF ((MODE.GT.3) .AND. INSIDE) THEN
C
C---- inside
C
        CALL GSMDRB(X2,Y2)
C
C---- outside: no action
C     for drawto or drawby clip the line and change its end points
C
      ELSE IF (MODE.LE.2) THEN
        X1 = XBOLD
        Y1 = YBOLD
        CALL GSCLPL(X1,Y1,X2,Y2,ACCEPT)
C
C---- draw line if accepted
C
        IF (ACCEPT) THEN
C
C---- move pen to clipped old point if last point was outside frame
C
          IF (OLDOUT) CALL GSMDRB(X1,Y1)
          CALL GSDDRB(X2,Y2)
        ELSE
C
C---- error if line removed
C
          NERROR = NERROR + 1
        END IF
      END IF
C
      XBOLD = XBNEW
      YBOLD = YBNEW
C
      IF (KPRINT.GE.3) THEN
        WRITE (LUNOUT,FMT=6008) XBOLD,YBOLD,XBNEW,YBNEW
        WRITE (LUNOUT,FMT=6010) X1,Y1,X2,Y2
        WRITE (LUNOUT,FMT=6012) ACCEPT,INSIDE,LSTOUT,OLDOUT,MODE
      END IF
C
C---- Format statements
C
 6000 FORMAT (1X,'GSDWTO')
 6002 FORMAT (1X,'GSDWBY')
 6004 FORMAT (1X,'GSMVTO')
 6006 FORMAT (1X,'GSMVBY')
 6008 FORMAT (1X,'XBOLD YBOLD   XBNEW YBNEW ',2F10.5,2X,2F10.5)
 6010 FORMAT (1X,'X1    Y1      X2    Y2    ',2F10.5,2X,2F10.5)
 6012 FORMAT (1X,'ACCEPT INSIDE LSTOUT OLDOUT MODE ',4L4,I5)
 6014 FORMAT (1X,'GSPOIN')
C
      END
C
C
C
      SUBROUTINE GSDVON(GSNAM,NOUT)
C     ===============================
C
C---- Switches on plotting device
C
C     A.D. McLachlan JUN 1984. Last updated 4 OCT 1984
C
C     .. Scalar Arguments ..
      INTEGER NOUT
      CHARACTER GSNAM* (*)
C     ..
C     .. Scalars in Common ..
      REAL DOTMMX,DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWBLMX,DWBLMY,
     +     DWLIMX,DWLIMY,PAPLMX,PAPLMY,UBXMAX,UBXMIN,UBYMAX,UBYMIN,
     +     V64LMX,V64LMY
      INTEGER ICOLOR,IDRLVL,IPRINT,IUNIT,IUNITR,IXMAX,IXMIN,IYMAX,IYMIN,
     +        LINWT,LUNIN,LUNOUT,MCNTFL,MDEVIC,MDIREC,MIXCOL,MOUT,MPIC,
     +        MSCAFL,NERROR,NPICS
      LOGICAL*4 DEVCON,INITON
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. Arrays in Common ..
      INTEGER IREC
C     ..
C     .. Local Scalars ..
      INTEGER I,JPIC,KEOF,LEVEL,MREC,NRECL
      CHARACTER CHKEY*1,TMPNAM*40,BLN80*80,TITLEH*80
C     ..
C     .. Local Arrays ..
      REAL*4 AREC(128)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSBLTM,GSCFIL,GSCYTM,GSFLBR,GSFLP1,
     +         GSFLSR,GSFLWR,GSGRTM,GSINTM,GSLVCK,
     +         GSMYTM,GSOFLW,GSOFTM,GSRHDR,
     +         GSSCTM,GSTYTM,GSWHDR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
      COMMON /GSFHD/IUNITR,IREC(128)
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (IREC(1),AREC(1))
      EQUIVALENCE (TITLEH,IREC(41))
C     ..
C     .. Data statements ..
      DATA BLN80/' '/
C
C---- check level
C
      IF ((IDRLVL.EQ.0) .OR. (IDRLVL.GT.2)) 
     +            CALL GSLVCK('GSDVON')
C
C---- open plotfile
C
      DEVCON = .TRUE.
      TMPNAM = GSNAM
      FILNAM = TMPNAM
      IF (INDEX(FILNAM,BLN80).EQ.1) FILNAM = 'PLOTOUT.PLT'
      IF (IPRINT.GE.2) WRITE (LUNOUT,FMT=6000) FILNAM
      TMPNAM = FILNAM
      CALL GSOFLW(IUNIT,TMPNAM)
C
C---- turn on device
C
      IF (MDIREC.EQ.1 .AND. MDEVIC.EQ.3) THEN
        MOUT = NOUT
        IF (MOUT.LE.0) MOUT = 90
        IF (IPRINT.GE.2) WRITE (LUNOUT,FMT=6004) MOUT
        CALL GSINTM(MOUT)
        CALL GSGRTM
      END IF
C
      RETURN
C
      ENTRY GSDVOF()
C     =============
C
C---- add picture count to each file header position file at first record
C
      CALL GSFLP1
      NRECL = 512
      DO 40 JPIC = 1,NPICS
        KEOF = 0
        CALL GSRHDR(KEOF)
        IF (KEOF.NE.1) THEN
          IREC(23) = NPICS
          CALL GSFLBR(NRECL)
          IF (IPRINT.GE.2) WRITE (LUNOUT,FMT=6008) IREC(1),
     +       (AREC(I),I=2,3),(IREC(I),I=4,16), (AREC(I),I=17,22),
     +       IREC(23),TITLEH
          CALL GSFLWR(IREC,NRECL)
          MREC = IREC(1)
          CALL GSFLSR(MREC*4)
        END IF
   40 CONTINUE
C
C---- write a last header for a null picture
C     with no records to terminate the file properly
C
      LEVEL = IDRLVL
      IDRLVL = 3
      CALL GSWHDR
      IDRLVL = LEVEL
      CALL GSCFIL(IUNIT)
C
C---- terminal
C
      IF (MDIREC.EQ.1 .AND. MDEVIC.EQ.3) THEN
C
C---- pause and wait for reply before clearing screen always write this!!
C
        CALL GSBLTM
        CALL GSTYTM
        WRITE (LUNOUT,FMT=6002)
        READ (LUNIN,FMT=6006) CHKEY
C
C---- "return" or "blank" saves the screen
C
        IF (CHKEY.NE.' ') CALL GSSCTM
        CALL GSTYTM
        CALL GSCYTM
        CALL GSMYTM(0,0)
C
C---- close graphics stream
C
        CALL GSOFTM(MOUT)
        IF (IPRINT.GE.2) WRITE (LUNOUT,FMT=6004) MOUT
      END IF
      DEVCON = .FALSE.
      NERROR = 0
C
C---- Format statements
C
 6000 FORMAT (2X,'GSDVON: FILNAM= ',A)
 6002 FORMAT (2X,'Type "RETURN" or "SPACE" to save picture',4X,'any ot',
     +       'her character to clear ')
 6004 FORMAT (2X,'GSDVON/DVOF: MOUT=',I5)
 6006 FORMAT (A,A)
 6008 FORMAT (2X,'Plot Header: ',I5,2F10.4,4I6,/1X,9I5,/1X,6F10.4,/1X,
     +       I5,' TITLE: ',/1X,A)
C
      END
C
C
C
      SUBROUTINE GSEDTR(DTMMX,DTMMY)
C     ===============================
C
C---- Defines drawing board to device transformations
C
C     A.D. McLachlan JUN 1984. Last updated 10 SEP 84.
C
C     (XDRW,YDRW) are in mm. on board
C     (XDEV,YDEV) are in mm. on device
C     (IX,IY)     are dot indices for device
C                     (normally 10 dots/mm if plotfile)
C     (DTMMX,DTMMY) are dots/mm along x,y .
C                   if called value is (0.0,0.0)
C
C---- then use automatic setting
C     negative values forbidden
C
C     Board to device transformation for floating scale is
C
C     XDEV=XDRW*WDFACX + XDOFF
C     YDEV=YDRW*WDFACY + YDOFF
C
C     Board to device transformation for true scale is
C
C     XDEV=XDRW+XDOFF
C     YDEV=YDRW+YDOFF
C
C     OFFSETS (XDOFF,YDOFF) depend on floating or true origin
C     device mm to dot transformation for (ix,iy) in range 1:32767 is
C
C     IX= XDEV*DOTMMX + IDXOFF
C     IY= YDEV*DOTMMY + IDYOFF
C
C     combined transformation is
C
C     IX=(XDRW*FACX+XFOFF)+IDXOFF
C     IY=(YDRW*FACY+YFOFF)+IDYOFF
C
C---- board to device
C
C     .. Scalar Arguments ..
      REAL DTMMX,DTMMY
C     ..
C     .. Scalars in Common ..
      REAL DOTMMX,DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWBLMX,DWBLMY,
     +     DWLIMX,DWLIMY,FACX,FACY,PAPLMX,PAPLMY,UBXMAX,UBXMIN,UBYMAX,
     +     UBYMIN,V64LMX,V64LMY,XFOFF,YFOFF
      INTEGER ICOLOR,IDRLVL,IDXOFF,IDYOFF,IPRINT,IUNIT,IXMAX,IXMIN,
     +        IXOLD,IYMAX,IYMIN,IYOLD,LINWT,LUNIN,LUNOUT,MCNTFL,MDEVIC,
     +        MDIREC,MIXCOL,MODOLD,MOUT,MPIC,MSCAFL,NERROR,NPICS
      LOGICAL*4 DEVCON,INITON
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. Local Scalars ..
      REAL DT100,TX,TY,WDFACX,WDFACY,XDOFF,YDOFF
      INTEGER IBIG,IXBIG,IYBIG,NAUTO
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MAX,MIN
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSDVT/FACX,FACY,XFOFF,YFOFF,IDXOFF,IDYOFF,
     +                IXOLD,IYOLD,MODOLD
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
C
C     .. Save Statement ..
C
      SAVE
C
      IF (MSCAFL.EQ.0) THEN
C
C---- fixed scale
C
        WDFACX = 1.0
        WDFACY = 1.0
        IF (MCNTFL.EQ.0) THEN
C
C---- fixed origin
C
          XDOFF = 0.0
          YDOFF = 0.0
        ELSE
C
C---- floating origin
C
          XDOFF = -UBXMIN
          YDOFF = -UBYMIN
        END IF
      ELSE IF (MCNTFL.EQ.0) THEN
C
C---- floating scale fixed origin maps (0...ubmax) onto (0...dvmax)
C
        TX = DVXMAX/ (UBXMAX+0.0001)
        TY = DVYMAX/ (UBYMAX+0.0001)
        WDFACX = MIN(TX,TY)
        WDFACY = WDFACX
        XDOFF = 0.0
        YDOFF = 0.0
      ELSE
C
C---- floating scale floating origin
C
        TX = (DVXMAX-DVXMIN)/ ((UBXMAX-UBXMIN)+0.0001)
        TY = (DVYMAX-DVYMIN)/ ((UBYMAX-UBYMIN)+0.0001)
        WDFACX = MIN(TX,TY)
        WDFACY = WDFACX
        XDOFF = DVXMIN - UBXMIN*WDFACX
        YDOFF = DVYMIN - UBYMIN*WDFACY
      END IF
C
C---- transformation from device (mm) to integers
C
      NAUTO = 1
      DOTMMX = DTMMX
      DOTMMY = DTMMY
      IF ((DOTMMX.NE.0.0) .OR. (DOTMMY.NE.0.0)) THEN
        NAUTO = 0
C
C---- warn if outside range 100-254 dots per inch
C     constant for 100 dots per inch
C
        DT100 = 100.0/25.4
        IF (((DOTMMX.LT.DT100).OR. (DOTMMX.GT.10.0)) .OR.
     +      ((DOTMMY.LT.DT100).OR. (DOTMMY.GT.10.0))) THEN
          IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6004) DOTMMX,DOTMMY
        END IF
C
C---- check for too fine a grid across device window
C     check for zero dots
C
        IF ((DOTMMX.LE.0.0) .OR. (DOTMMY.LE.0.0)) NAUTO = 1
        IXBIG = DOTMMX*DVXMAX
        IYBIG = DOTMMY*DVYMAX
        IBIG = MAX(IXBIG,IYBIG)
        IF (IBIG.GT.32766) NAUTO = 1
        IF ((NAUTO.EQ.1) .AND. (IPRINT.GE.1)) WRITE (LUNOUT,
     +      FMT=6006) DOTMMX,DOTMMY
      END IF
C
      GO TO (10,10,20) MDEVIC
C
C---- (1,2) undefined plotfile or paper normally at 10.0 dots/mm
C     integers 1 to 32767 (2**15 - 1)
C     this section may override values set by call
C
   10 CONTINUE
      IF (NAUTO.EQ.1) THEN
        DOTMMX = 10.0
        DOTMMY = 10.0
      END IF
      IDXOFF = 1
      IDYOFF = 1
      GO TO 30
C
C---- (3) vt640 plot screen
C
   20 CONTINUE
C
C---- integer memory grid (880*756) = (165.0*139.5)mm
C     user area with margins (96:975*13:768)
C     scales 880/165=5.3333   756/139.5=5.41935
C     physical grid is 3.3333 dots/mm with full screen size
C       1024*(5/8)=640 and 780*(8/13)=480 physical dots (192*144) mm
C     we only use part of this as dots near edge may get lost
C
C---- if interactive then override called values
C
      IF ((NAUTO.EQ.1) .OR. (MDIREC.EQ.1)) THEN
        DOTMMX = 880.0/165.0
        DOTMMY = 756.0/139.5
      END IF
      IDXOFF = 96
      IDYOFF = 13
C
C---- combined transformation
C
   30 FACX = DOTMMX*WDFACX
      FACY = DOTMMY*WDFACY
      XFOFF = DOTMMX*XDOFF
      YFOFF = DOTMMY*YDOFF
C
C---- reset last point data for start of picture
C
      IXOLD = 0
      IYOLD = 0
      MODOLD = 0
C
      IF (IPRINT.GE.2) THEN
        WRITE (LUNOUT,FMT=6000) WDFACX,WDFACY,FACX,FACY,XDOFF,YDOFF,
     +    XFOFF,YFOFF,DOTMMX,DOTMMY,IDXOFF,IDYOFF
        WRITE (LUNOUT,FMT=6002) MSCAFL,MCNTFL,DWLIMX,DWLIMY,DVXMIN,
     +    DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,UBYMAX
      END IF
C
C---- Format statements
C
 6000 FORMAT (2X,'GSEDTR: WDFACX WDFACY FACX  FACY ',
     +        4F10.4,/12X,'XDO',
     +       'FF  YDOFF  XFOFF YFOFF',4F10.4,/10X,'DOTMMX DOTMMY IDXOF',
     +       'F IDYOFF',2F10.4,2I5)
 6002 FORMAT (2X,'GSEDTR: MSCAFL MCNTFL ',2I5,/2X,'DWLIMX DWLIMY ',
     +       2F10.4,/2X,'DVXMIN DVXMAX DVYMIN DVYMAX ',4F10.4,/2X,'UBX',
     +       'MIN UBXMAX UBYMIN UBYMAX ',4F10.4)
 6004 FORMAT (2X,'!!!GSEDTR: WARNING - DOTMMX,DOTMMY=',
     +        2F10.4,/2X,'OU',
     +       'TSIDE EXPECTED RANGE 100/25.4-10.0 ')
 6006 FORMAT (2X,'!!!GSEDTR: WARNING - DOTMMX,DOTMMY=',
     +        2F10.4,/2X,'TO',
     +       'O LARGE OR SMALL FOR DEVICE GRID 1-32766; VALUES IGNORED '
     +       )
C
      END
C
C
C
      SUBROUTINE GSEDVC(NDIREC,NDEVIC,DWLIM1,DWLIM2,
     +                    DVLMX1,DVLMX2,DVLMY1,DVLMY2)
C     ==============================================
C
C     A.D. McLachlan JUN 1984. Last updated 14 JUL 1984
C
C---- Select plot device.
C
C     .. Scalar Arguments ..
      REAL DVLMX1,DVLMX2,DVLMY1,DVLMY2,DWLIM1,DWLIM2
      INTEGER NDEVIC,NDIREC
C     ..
C     .. Scalars in Common ..
      REAL DOTMMX,DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWBLMX,DWBLMY,
     +     DWLIMX,DWLIMY,PAPLMX,PAPLMY,UBXMAX,UBXMIN,UBYMAX,UBYMIN,
     +     V64LMX,V64LMY
      INTEGER ICOLOR,IDRLVL,IPRINT,IUNIT,IXMAX,IXMIN,IYMAX,IYMIN,LINWT,
     +        LUNIN,LUNOUT,MCNTFL,MDEVIC,MDIREC,MIXCOL,MOUT,MPIC,MSCAFL,
     +        NERROR,NPICS
      LOGICAL*4 DEVCON,INITON
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. Local Scalars ..
      REAL DTMXDF,DTMYDF,DWBXDF,DWBXMX,DWBYDF,DWBYMX,PAPXDF,PAPXMX,
     +     PAPYDF,PAPYMX,V64XDF,V64XMX,V64YDF,V64YMX
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Data statements ..
C
C---- data statements for device dimensions etc units millimetres
C     2**15-1=32767 is maximum integer size on drawing board with
C     10 dots/mm standard
C     extreme values and default values set here
C
C---- paper: approx 13*20 inches normal, 13.2*80 max
C
C---- vt640: approx 6.5*5.5 inches
C
C---- paper plotter has 1320 dot positions across page
C     13.2 inches =335.28 mm. normally use only 1296 dots
C     paper plotter 100 dots/inch, but plot file uses 10 dots/mm
C
      DATA DWBXMX/3276.6/,DWBYMX/3276.6/
      DATA DWBXDF/330.0/,DWBYDF/508.0/
      DATA PAPXMX/335.0/,PAPYMX/2032.0/
      DATA PAPXDF/330.0/,PAPYDF/508.0/
      DATA V64XMX/165.0/,V64YMX/139.5/
      DATA V64XDF/165.0/,V64YDF/139.5/
      DATA DTMXDF/10.0/,DTMYDF/10.0/
C
C---- choice of deferred plotfile or direct immediate display
C     MDIREC=0,1
C     MDEVIC = 1 undefined. must have mdirec=0
C            = 2 paper plotter trilog
C            = 3 vt640 screen
C
      MDEVIC = NDEVIC
      MDIREC = NDIREC
C
C---- check control values
C
      IF ((MDEVIC.LT.1) .OR. (MDEVIC.GT.3)) THEN
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6002) MDEVIC
        MDEVIC = 1
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6004)
      END IF
      IF ((MDIREC.NE.0) .AND. (MDIREC.NE.1)) THEN
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6000) MDIREC
        MDIREC = 0
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6006)
      END IF
C
C---- check for interactive device
C
      IF (((MDEVIC.EQ.1).OR. (MDEVIC.EQ.2)) .AND. (MDIREC.NE.0)) THEN
        MDIREC = 0
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6006)
      END IF
C
C---- set the default device controls
C     sizes
C
      DWLIM1 = DWBXDF
      DWLIM2 = DWBYDF
C
C---- copy limits into common block
C
      DWBLMX = DWBXMX
      DWBLMY = DWBYMX
      PAPLMX = PAPXMX
      PAPLMY = PAPYMX
      V64LMX = V64XMX
      V64LMY = V64YMX
      DOTMMX = DTMXDF
      DOTMMY = DTMYDF
      DVLMX1 = 0.0
      DVLMY1 = 0.0
      GO TO (10,30,20) MDEVIC
C
C---- undefined device
C
   10 CONTINUE
      DVLMX2 = DWBXDF
      DVLMY2 = DWBYDF
      GO TO 30
   20 DVLMX2 = V64XDF
      DVLMY2 = V64YDF
      RETURN
C
C---- paper
C
   30 DVLMX2 = PAPXDF
      DVLMY2 = PAPYDF
C
C---- Format statements
C
 6000 FORMAT (2X,'!!!GSEDVC ERROR: MDIREC NOT (0),(1) =',I5)
 6002 FORMAT (2X,'!!!GSEDVC ERROR: ',
     + 'MDEVIC NOT (1)UNDEF (2)PAP (3)VT64',
     +       '0 =',I5)
 6004 FORMAT (2X,'!!!GSEDVC WARN: MDEVIC RESET TO (1)')
 6006 FORMAT (2X,'!!!GSEDVC WARN: MDIREC RESET TO (0)')
C
      END
C
C
C
      SUBROUTINE GSEDVP(DVLMX1,DVLMX2,DVLMY1,DVLMY2,VBXMIN,
     +                    VBXMAX,VBYMIN,VBYMAX)
C     ======================================================
C
C---- Resets plot device and board sizes to override defaults
C     but not exceed set limits
C
C     A.D. McLachlan JUN 1984 Last updated 14 JUL 1984
C
C     .. Scalar Arguments ..
      REAL DVLMX1,DVLMX2,DVLMY1,DVLMY2,DWLIM1,DWLIM2,VBXMAX,VBXMIN,
     +     VBYMAX,VBYMIN
C     ..
C     .. Scalars in Common ..
      REAL DOTMMX,DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWBLMX,DWBLMY,
     +     DWLIMX,DWLIMY,PAPLMX,PAPLMY,UBXMAX,UBXMIN,UBYMAX,UBYMIN,
     +     V64LMX,V64LMY
      INTEGER ICOLOR,IDRLVL,IPRINT,IUNIT,IXMAX,IXMIN,IYMAX,IYMIN,LINWT,
     +        LUNIN,LUNOUT,MCNTFL,MDEVIC,MDIREC,MIXCOL,MOUT,MPIC,MSCAFL,
     +        NERROR,NPICS
      LOGICAL*4 DEVCON,INITON
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. Local Scalars ..
      REAL XBIG,YBIG
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
C
C     .. Save Statement ..
C
      SAVE
C
C---- Main entry here Check device viewport
C
      DVXMIN = DVLMX1
      DVXMAX = DVLMX2
      DVYMIN = DVLMY1
      DVYMAX = DVLMY2
      GO TO (10,20,30) MDEVIC
C
C---- (1) undefined: max device size is board size
C
   10 CONTINUE
      XBIG = DWBLMX
      YBIG = DWBLMY
      GO TO 40
C
C---- (2) paper:
C
   20 CONTINUE
      XBIG = PAPLMX
      YBIG = PAPLMY
      GO TO 40
C
C---- (3) vt640:
C
   30 CONTINUE
      XBIG = V64LMX
      YBIG = V64LMY
   40 IF ((DVXMIN.LT.0.0) .OR. (DVXMIN.GT. (DVXMAX-1.0))) THEN
        DVXMIN = 0.0
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6008) DVLMX1,DVXMIN
      END IF
      IF ((DVXMAX.LT.1.0) .OR. (DVXMAX.GT.XBIG)) THEN
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6004) DVXMAX,XBIG
        DVXMAX = XBIG
      END IF
      IF ((DVYMIN.LT.0.0) .OR. (DVYMIN.GT. (DVYMAX-1.0))) THEN
        DVYMIN = 0.0
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6010) DVLMY1,DVYMIN
      END IF
      IF ((DVYMAX.LT.1.0) .OR. (DVYMAX.GT.YBIG)) THEN
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6006) DVYMAX,YBIG
        DVYMAX = YBIG
      END IF
C
C---- set default drawing board window
C
      VBXMIN = 0.0
      VBXMAX = DWLIMX
      VBYMIN = 0.0
      VBYMAX = DWLIMY
      RETURN
C
      ENTRY GSEBSZ(DWLIM1,DWLIM2)
C     ===========================
C
C---- Check drawing board limits
C
      DWLIMX = DWLIM1
      DWLIMY = DWLIM2
      IF ((DWLIMX.LT.1.0) .OR. (DWLIMX.GT.DWBLMX)) THEN
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6000) DWLIMX,DWBLMX
        DWLIMX = DWBLMX
      END IF
      IF ((DWLIMY.LT.1.0) .OR. (DWLIMY.GT.DWBLMY)) THEN
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6002) DWLIMY,DWBLMY
        DWLIMY = DWBLMY
      END IF
C
C---- Format statements
C
 6000 FORMAT (2X,'!!!GSEBSZ: DWLIMX=',F10.4,
     +     ' OUTSIDE RANGE - RESET TO ',F10.4)
 6002 FORMAT (2X,'!!!GSEBSZ: DWLIMY=',F10.4,
     +    ' OUTSIDE RANGE - RESET TO ',F10.4)
 6004 FORMAT (2X,'!!!GSEDVP: DVXMAX =',F10.4,
     +     ' OUTSIDE RANGE - RESET TO ',F10.4)
 6006 FORMAT (2X,'!!!GSEDVP: DVYMAX=',F10.4,
     +    ' OUTSIDE RANGE - RESET TO ',F10.4)
 6008 FORMAT (2X,'!!!GSEDVP: DVXMIN =',F10.4,
     +   ' OUTSIDE RANGE - RESET TO ',F10.4)
 6010 FORMAT (2X,'!!!GSEDVP: DVYMIN=',F10.4,
     +    ' OUTSIDE RANGE - RESET TO ',F10.4)
C
      END
C
C
C
      SUBROUTINE GSENVR
C     ==================
C
C---- Sets up plotting windows etc
C
C     A.D. McLachlan JUL 1984. Last updated 14 JUL 1984.
C
C**** ENTRY    GSXENV  used to clear old settings to default
C
C**** ENTRIES: GSDVIC,GSBSIZ,GSDVPT,GSWNDB,GSWSCL
C              GSDTRN
C
C     These are used to set the desired values for window and
C     device constants. When GSenvr is called it uses either
C     its own default values or those set by the user to build
C     a reasonable plotting system. Each subroutine checks
C     its input values for errors and supplies a default for
C     the next step.
C
C---- The order of initialisation is:
C        GSEDVC  (DIRECT,DEVICE)
C          GSEBSZ  (DRAWING BOARD SIZE)
C            GSEDVP  (DEVICE VIEWPORT)
C              GSEWND  (DEVICE WINDOW)
C                GSEWSC  (SCALING OPTIONS: FLOAT,FIX)
C                  GSEDTR  (DEVICE TRANSFORMATION)
C
C     Values are remembered, so that a second call to GSenvir
C     sets the same environment as before. also, if any of the
C     settings are altered GSenvr makes minimal changes to
C     the values of other constants.
C
C---- The object of these routines is to ensure that all plots
C     stay within the drawing board and device limits.
C
C---- Main entry check level
C
C     .. Scalar Arguments ..
      REAL XX1,XX2,YY1,YY2
      INTEGER NN1,NN2
C     ..
C     .. Scalars in Common ..
      REAL DOTMMX,DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWBLMX,DWBLMY,
     +     DWLIMX,DWLIMY,PAPLMX,PAPLMY,UBXMAX,UBXMIN,UBYMAX,UBYMIN,
     +     V64LMX,V64LMY
      INTEGER ICOLOR,IDRLVL,IPRINT,IUNIT,IXMAX,IXMIN,IYMAX,IYMIN,LINWT,
     +        LUNIN,LUNOUT,MCNTFL,MDEVIC,MDIREC,MIXCOL,MOUT,MPIC,MSCAFL,
     +        NERROR,NPICS
      LOGICAL*4 DEVCON,INITON
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. Local Scalars ..
      REAL DTMMX,DTMMY,DVLMX1,DVLMX2,DVLMY1,DVLMY2,DWLIM1,DWLIM2,VBXMAX,
     +     VBXMIN,VBYMAX,VBYMIN,X1,X2,X3,X4,Y1,Y2,Y3,Y4
      INTEGER N1,N2,NCNTFL,NDEVIC,NDIREC,NEBSIZ,NEDTRN,NEDVIC,NEDVPT,
     +        NEWNDB,NEWSCL,NSCAFL
C     ..
C     .. External Subroutines ..
      EXTERNAL GSEBSZ,GSEDTR,GSEDVC,GSEDVP,
     +         GSEWND,GSEWSC,GSLVCK
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
C
C     .. Save Statement ..
C
      SAVE
C
      IF ((IDRLVL.EQ.0) .OR. (IDRLVL.EQ.3)) 
     +              CALL GSLVCK('GSENVR')
      N1 = 0
      N2 = 2
      IF (NEDVIC.EQ.1) THEN
        N1 = NDIREC
        N2 = NDEVIC
      END IF
      CALL GSEDVC(N1,N2,X3,Y3,X1,X2,Y1,Y2)
      IF (NEBSIZ.EQ.1) THEN
        X3 = DWLIM1
        Y3 = DWLIM2
      END IF
      CALL GSEBSZ(X3,Y3)
      IF (NEDVPT.EQ.1) THEN
        X1 = DVLMX1
        X2 = DVLMX2
        Y1 = DVLMY1
        Y2 = DVLMY2
      END IF
      CALL GSEDVP(X1,X2,Y1,Y2,X3,X4,Y3,Y4)
      IF (NEWNDB.EQ.1) THEN
        X3 = VBXMIN
        X4 = VBXMAX
        Y3 = VBYMIN
        Y4 = VBYMAX
      END IF
      CALL GSEWND(X3,X4,Y3,Y4)
      N1 = 0
      N2 = 1
      IF (NEWSCL.EQ.1) THEN
        N1 = NSCAFL
        N2 = NCNTFL
      END IF
      CALL GSEWSC(N1,N2)
      X1 = 0.0
      Y1 = 0.0
      IF (NEDTRN.EQ.1) THEN
        X1 = DTMMX
        Y1 = DTMMY
      END IF
      CALL GSEDTR(X1,Y1)
      RETURN
C
      ENTRY GSXENV()
C     =============
C
C---- Check level
C
      IF ((IDRLVL.EQ.0) .OR. (IDRLVL.EQ.3)) 
     +             CALL GSLVCK('GSXENV')
      NEDVIC = 0
      NEBSIZ = 0
      NEDVPT = 0
      NEWNDB = 0
      NEWSCL = 0
      NEDTRN = 0
      RETURN
C
      ENTRY GSDVIC(NN1,NN2)
C     ======================
C
C---- Check level
C
      IF ((IDRLVL.EQ.0) .OR. (IDRLVL.GT.2))
     +         CALL GSLVCK('GSDVIC')
      NDIREC = NN1
      NDEVIC = NN2
      NEDVIC = 1
      RETURN
C
      ENTRY GSBSIZ(XX1,YY1)
C     ======================
C
C---- Check level
C
      IF ((IDRLVL.EQ.0) .OR. (IDRLVL.GT.2)) 
     +      CALL GSLVCK('GSBSIZ')
      DWLIM1 = XX1
      DWLIM2 = YY1
      NEBSIZ = 1
      RETURN
C
      ENTRY GSDVPT(XX1,XX2,YY1,YY2)
C     ==============================
C
C---- Check level
C
      IF ((IDRLVL.EQ.0) .OR. (IDRLVL.GT.2)) 
     +    CALL GSLVCK('GSDVPT')
      DVLMX1 = XX1
      DVLMX2 = XX2
      DVLMY1 = YY1
      DVLMY2 = YY2
      NEDVPT = 1
      RETURN
C
      ENTRY GSWNDB(XX1,XX2,YY1,YY2)
C     =============================
C
C---- Check level
C
      IF ((IDRLVL.EQ.0) .OR. (IDRLVL.GT.2)) 
     +     CALL GSLVCK('GSWNDB')
      VBXMIN = XX1
      VBXMAX = XX2
      VBYMIN = YY1
      VBYMAX = YY2
      NEWNDB = 1
      RETURN
C
      ENTRY GSWSCL(NN1,NN2)
C     ======================
C
C---- Check level
C
      IF ((IDRLVL.EQ.0) .OR. (IDRLVL.GT.2))
     +       CALL GSLVCK('GSWSCL')
      NSCAFL = NN1
      NCNTFL = NN2
      NEWSCL = 1
      RETURN
C
      ENTRY GSDTRN(XX1,YY1)
C     ======================
C
C---- Check level
C
      IF ((IDRLVL.EQ.0) .OR. (IDRLVL.EQ.3))
     +        CALL GSLVCK('GSDTRN')
      DTMMX = XX1
      DTMMY = YY1
      NEDTRN = 1
C
      END
C
C
C
      SUBROUTINE GSEWND(VBXMIN,VBXMAX,VBYMIN,VBYMAX)
C     ==============================================
C
C---- Resets drawing board window on drawing board in mm
C     must not be used till drawing board size is set
C     bounds which go off board are rejected
C
C     A.D. McLachlan JUN 1984. Last updated 14 JUL 1984
C
C     .. Scalar Arguments ..
      REAL VBXMAX,VBXMIN,VBYMAX,VBYMIN
C     ..
C     .. Scalars in Common ..
      REAL DOTMMX,DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWBLMX,DWBLMY,
     +     DWLIMX,DWLIMY,PAPLMX,PAPLMY,UBXMAX,UBXMIN,UBYMAX,UBYMIN,
     +     V64LMX,V64LMY
      INTEGER ICOLOR,IDRLVL,IPRINT,IUNIT,IXMAX,IXMIN,IYMAX,IYMIN,LINWT,
     +        LUNIN,LUNOUT,MCNTFL,MDEVIC,MDIREC,MIXCOL,MOUT,MPIC,MSCAFL,
     +        NERROR,NPICS
      LOGICAL*4 DEVCON,INITON
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
C
C     .. Save Statement ..
C
      SAVE
C
      UBXMIN = VBXMIN
      UBXMAX = VBXMAX
      UBYMIN = VBYMIN
      UBYMAX = VBYMAX
      IF (UBXMIN.LT.UBXMAX) THEN
        IF (UBYMIN.LT.UBYMAX) THEN
          IF ((UBXMIN.GE.0.0) .AND. (UBXMAX.LE.DWLIMX)) THEN
            IF ((UBYMIN.GE.0.0) .AND. (UBYMAX.LE.DWLIMY)) GO TO 10
          END IF
        END IF
      END IF
C
C---- error in requested bounds
C
      IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6000) UBXMIN,UBXMAX,UBYMIN,
     +    UBYMAX,DWLIMX,DWLIMY
      UBXMIN = 0.0
      UBXMAX = DWLIMX
      UBYMIN = 0.0
      UBYMAX = DWLIMY
      IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6002) UBXMIN,UBXMAX,UBYMIN,
     +    UBYMAX
C
C---- set clipping to current drawing board bounds
C
   10 CONTINUE
C
C---- Format statements
C
 6000 FORMAT (2X,'!!!GSEWND: BOUNDS REQUESTED ARE OFF BOARD ',
     +        /2X,'XM',
     +       'IN XMAX YMIN YMAX (MM) = ',4F10.4,/2X,'BOARD DIMENSIONS ',
     +       'ARE ',2F10.4)
 6002 FORMAT (2X,'!!!GSEWND: CORRECTED BOUNDS = ',4F10.4)
C
      END
C
C
C
      SUBROUTINE GSEWSC(NSCAFL,NCNTFL)
C     =================================
C
C---- Resets scaling and window clipping options for device
C
C     A.D. McLachlan JUN 1984. Last updated 10 AUG 1984
C
C     MSCAFL = 0  use true scale
C     MSCAFL = 1  floating scale - largest uniform one to fit device
C     MCNTFL = 0  true origin (0,0)board=(0,0)device
C     MCNTFL = 1  floating origin - get as much as possible on device
C
C---- Deferred plotting option (mdirec=0)
C
C     MSCAFL=1    clip with drawing board bounds
C     MSCAFL=0    clip with drawing board bounds and give
C                 warning message if too big for device
C
C---- Direct plotting option (mdirec=1)
C
C     MSCAFL=1    clip with drawing board bounds
C     MSCAFL=0    clip to common region of drawing board and device
C                 warning if too small
C
C     .. Scalar Arguments ..
      INTEGER NCNTFL,NSCAFL
C     ..
C     .. Scalars in Common ..
      REAL BXMAX,BXMIN,BYMAX,BYMIN,DOTMMX,DOTMMY,DVXMAX,DVXMIN,DVYMAX,
     +     DVYMIN,DWBLMX,DWBLMY,DWLIMX,DWLIMY,PAPLMX,PAPLMY,UBXMAX,
     +     UBXMIN,UBYMAX,UBYMIN,V64LMX,V64LMY
      INTEGER ICOLOR,IDRLVL,IPRINT,IUNIT,IXMAX,IXMIN,IYMAX,IYMIN,LINWT,
     +        LUNIN,LUNOUT,MCNTFL,MDEVIC,MDIREC,MIXCOL,MOUT,MPIC,MSCAFL,
     +        NERROR,NPICS
      LOGICAL*4 DEVCON,INITON
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. Local Scalars ..
      REAL DVWIDX,DVWIDY,UBWIDX,UBWIDY,UBXX,UBYY
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSCLP/BXMIN,BXMAX,BYMIN,BYMAX
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
C
C     .. Save Statement ..
C
      SAVE
C
      MSCAFL = NSCAFL
      MCNTFL = NCNTFL
      IF ((MSCAFL.NE.0) .AND. (MSCAFL.NE.1)) THEN
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6000) MSCAFL
        MSCAFL = 1
      END IF
      IF ((MCNTFL.NE.0) .AND. (MCNTFL.NE.1)) THEN
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6002) MCNTFL
        MCNTFL = 1
      END IF
C
      DVWIDX = DVXMAX - DVXMIN
      DVWIDY = DVYMAX - DVYMIN
      UBWIDX = UBXMAX - UBXMIN
      UBWIDY = UBYMAX - UBYMIN
C
C---- for fixed scale check and trim the window
C
      IF (MSCAFL.EQ.0) THEN
C
C---- fixed scale options
C
        IF (MCNTFL.EQ.0) THEN
C
C---- fixed scale fixed origin
C
          IF (((DVXMIN.GT.UBXMIN).OR. (DVXMAX.LT.UBXMAX)) .OR.
     +        ((DVYMIN.GT.UBYMIN).OR. (DVYMAX.LT.UBYMAX))) THEN
            IF (IPRINT.GE.2) WRITE (LUNOUT,FMT=6004) MSCAFL,MCNTFL,
     +          UBXMIN,UBXMAX,DVXMIN,DVXMAX,UBYMIN,UBYMAX,DVYMIN,DVYMAX
            IF (UBXMAX.GT.DVXMAX) UBXMAX = DVXMAX
            IF (UBYMAX.GT.DVYMAX) UBYMAX = DVYMAX
            IF (UBXMIN.LT.DVXMIN) UBXMIN = DVXMIN
            IF (UBYMIN.LT.DVYMIN) UBYMIN = DVYMIN
            IF (IPRINT.GE.2) WRITE (LUNOUT,FMT=6006) UBXMIN,UBXMAX,
     +          UBYMIN,UBYMAX
          END IF
C
C---- check for vanishing window less than 1.0 mm across
C
          IF ((UBXMIN.GT. (UBXMAX-1.0)) .OR.
     +        (UBYMIN.GT. (UBYMAX-1.0))) THEN
            CALL CCPERR (2, 'Window too small')
          END IF
C
C---- fixed scale floating origin
C
        ELSE IF ((UBWIDX.GT.DVWIDX) .OR. (UBWIDY.GT.DVWIDY)) THEN
          IF (IPRINT.GE.2) WRITE (LUNOUT,FMT=6004) MSCAFL,MCNTFL,
     +        UBXMIN,UBXMAX,DVXMIN,DVXMAX,UBYMIN,UBYMAX,DVYMIN,DVYMAX
C
C---- trim the window
C
          IF (DVWIDX.LT.UBWIDX) UBXMAX = UBXMIN + DVWIDX
          IF (DVWIDY.LT.UBWIDY) UBYMAX = UBYMIN + DVWIDY
          IF (IPRINT.GE.2) WRITE (LUNOUT,FMT=6006) UBXMIN,UBXMAX,
     +        UBYMIN,UBYMAX
        END IF
      ELSE
C
C---- for floating scale the window can be larger than the device
C     for floating scale window
C
        IF (MCNTFL.EQ.0) THEN
C
C---- floating scale fixed origin:  map (0...ubmax) onto (0...dvmax)
C     trim window at lower bounds if necessary
C
          UBXX = (UBXMAX/DVXMAX)*DVXMIN
          UBYY = (UBYMAX/DVYMAX)*DVYMIN
          IF (((UBXMAX.GT.DVXMAX).OR. (UBYMAX.GT.DVYMAX)) .OR.
     +        ((UBXMIN.LT.UBXX).OR. (UBYMIN.LT.UBYY))) THEN
            IF (IPRINT.GE.2) WRITE (LUNOUT,FMT=6004) MSCAFL,MCNTFL,
     +          UBXMIN,UBXMAX,DVXMIN,DVXMAX,UBYMIN,UBYMAX,DVYMIN,DVYMAX
            IF (UBXMIN.LT.UBXX) UBXMIN = UBXX
            IF (UBYMIN.LT.UBYY) UBYMIN = UBYY
            IF (IPRINT.GE.2) WRITE (LUNOUT,FMT=6006) UBXMIN,UBXMAX,
     +          UBYMIN,UBYMAX
          END IF
C
C---- floating scale and origin
C     warn if widths exceed device limits do not trim window
C
        ELSE IF ((DVWIDX.LT.UBWIDX) .OR. (DVWIDY.LT.UBWIDY)) THEN
          IF (IPRINT.GE.2) WRITE (LUNOUT,FMT=6004) MSCAFL,MCNTFL,
     +        UBXMIN,UBXMAX,DVXMIN,DVXMAX,UBYMIN,UBYMAX,DVYMIN,DVYMAX
        END IF
      END IF
C
C---- set clipping window to (edited) value of drawing board window
C
      BXMIN = UBXMIN
      BXMAX = UBXMAX
      BYMIN = UBYMIN
      BYMAX = UBYMAX
C
C---- Format statements
C
 6000 FORMAT (2X,'!!!GSEWSC: MSCAFL NOT(0,1)=',I5,' RESET TO 1')
 6002 FORMAT (2X,'!!!GSEWSC: MCNTFL NOT(0,1)=',I5,' RESET TO 1')
 6004 FORMAT (2X,'!!!GSEWSC - WARN: ',
     +      'DEVICE AREA TOO SMALL FOR DRAWING',
     +       ' WINDOW ',/2X,'MSCAFL MCNTFL = ',2I5,/2X,'XMIN XMAX DVXM',
     +       'IN DVXMAX = ',4F10.4,/2X,'YMIN YMAX DVYMIN DVYMAX = ',
     +       4F10.4)
 6006 FORMAT (2X,'!!!GSEWSC - WARN: WINDOW TRIMMED TO FIT BOARD ',
     +       /2X,'XMIN XMAX YMIN YMAX = ',4F10.4)
C
      END
C
C
C
      SUBROUTINE GSFRAM(FXWID,FYWID,XF0,YF0,SLANT)
C     =============================================
C
C---- Draws a slanted rectangular frame
C
C     A.D. McLachlan AUG 1984. Last updated 2 AUG 1984
C
C     .. Scalar Arguments ..
      REAL FXWID,FYWID,SLANT,XF0,YF0
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Arrays ..
      REAL P(2),Q(2),R(2),S(2)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSLINE
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
      P(1) = XF0 - FXWID - FYWID*SLANT
      P(2) = YF0 - FYWID
      Q(1) = XF0 + FXWID - FYWID*SLANT
      Q(2) = YF0 - FYWID
      R(1) = XF0 + FXWID + FYWID*SLANT
      R(2) = YF0 + FYWID
      S(1) = XF0 - FXWID + FYWID*SLANT
      S(2) = YF0 + FYWID
      CALL GSLINE(P,Q)
      CALL GSLINE(Q,R)
      CALL GSLINE(R,S)
      CALL GSLINE(S,P)
C
      END
C
C
C
      SUBROUTINE GSGCH0(LETTER,XCOFF,YCOFF,SIZX,SIZY)
C     ================================================
C
C---- this routine draws both "letters" and "symbols" in font 0
C
C---- A.D. McLachlan JUN 1984. ADAPTED FROM D.A. AGARD PLOT82.
C     Last updated 27th March 1990 PJD
C
C     LETTER      = ascii number.
C     XCOFF,YCOFF = position offsets of baseline left corner
C                   from character cursor (xcstrt,ycstrt),
C                   in character units are xcoff*sizx,ycoff*sizy.
C     SIZX,SIZY   = symbol width and height in character units.
C
C---- Standard ascii text assignments (characters numbered 0...127)
C     written in columns of 16 characters
C
C         0    16    32    48    64    80    96   112
C
C       0 NUL DLE 32SPA  48 0  64 @  80 P  96    112 p
C       1 SOH DC1 33  !  49 1  65 A  81 Q  97 a  113 q
C       2 STX DC2 34  "  50 2  66 B  82 R  98 b  114 r
C       3 ETX DC3 35  #  51 3  67 C  83 S  99 c  115 s
C       4 EOT DC4 36  $  52 4  68 D  84 T 100 d  116 t
C       5 ENQ NAK 37  %  53 5  69 E  85 U 101 e  117 u
C       6 ACK SYN 38  &  54 6  70 F  86 V 102 f  118 v
C       7 BEL ETB 39  '  55 7  71 G  87 W 103 g  119 w
C       8  BS CAN 40  (  56 8  72 H  88 X 104 h  120 x
C       9  HT  EM 41  )  57 9  73 I  89 Y 105 i  121 y
C      10  LF SUB 42  *  58 :  74 J  90 Z 106 j  122 z
C      11  VT ESC 43  +  59 ;  75 K  91 [ 107 k  123 {
C      12  FF  FS 44  ,  60 <  76 L  92 \ 108 l  124 |
C      13  CR  GS 45  -  61 =  77 M  93 ] 109 m  125 }
C      14  SO  RS 46  .  62 >  78 N  94 ^ 110 n  126 ~
C      15  SI  US 47  /  63 ?  79 O  95 _ 111 o  127 DEL
C
C      NUL  Null                        DLE Data link escape
C      SOH  Start of heading            DC1 Device control 1
C      STX  Start of text               DC2 Device control 2
C      ETX  End of text                 DC3 Device control 3
C      EOT  End of transmission         DC4 Device control 4
C      ENQ  Enquiry                     NAK Negative acknowledge
C      ACK  Acknowledge                 SYN Synchronous idle
C      BEL  Bell                        ETB End transmission block
C       BS  Backspace                   CAN Cancel
C       HT  Horizontal tabulation        EM End of medium
C       LF  Line feed                   SUB Substitute
C       VT  Vertical tab                ESC Escape
C       FF  Form feed                    FS File separator
C       CR  Carriage return              GS Group separator
C       SO  Shift out                    RS Record separator
C       SI  Shift in                     US Unit separator
C       SP  Space                       DEL Delete
C
C--- each letter character is defined on a square grid (0-7)
C    character height is 1.0 character units =7.0 grid units
C    character width is 1.0 character units =7.0 grid units
C    the drawn strokes of each letter lie between (0-4) in ix
C    for text centred in grid we shift by 1.5 units to right
C
C---- icentc switch is ignored. letters start at bottom left init location
C
C     .. Parameters ..
      REAL FACT7,FACT4
      PARAMETER (FACT7=1.0/7.0,FACT4=1.0/4.0)
C     ..
C     .. Scalar Arguments ..
      REAL SIZX,SIZY,XCOFF,YCOFF
      INTEGER ISYMB
      INTEGER*4 LETTER
C     ..
C     .. Scalars in Common ..
      REAL ANGFAC,CHANGX,CHANGY,CHRSCX,CHRSCY,CHRSPX,CHRSPY,SCALEX,
     +     SCALEY,USANGX,USANGY,XCHAR,XCSTRT,YCHAR,YCSTRT
      INTEGER KPRINT,LUNIN,LUNOUT
      INTEGER*4 ICENTC,IFONT
      LOGICAL*4 FONTIN,ICULNK,LINMOD,UCSPAC
C     ..
C     .. Arrays in Common ..
      REAL CHRMAT,CUMAT,USRMAT
C     ..
C     .. Local Scalars ..
      REAL ADX,ADY,AXOFF,CHORGX,CHORGY,CUORGX,CUORGY,FACTX,FACTY,XNEW,
     +     XOLD,XORIG,XSPACE,XSTART,YNEW,YOLD,YORIG,YSPACE,YSTART
      INTEGER IDX,IDY,IPX,ISN,ITABLE,IW,KLETTR,MK1,NENTRY
      LOGICAL ENDCHR,ENDVST,OLDVEC
C     ..
C     .. Local Arrays ..
      INTEGER*2 IPC(463),IPT(127),NCTRAN(24),NSTRAN(24)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSDWTO,GSMVTO
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ICHAR,REAL
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSCHX/CHRMAT(3,3),CHRSCX,CHRSCY,CHANGX,CHANGY,CHRSPX,
     +       CHRSPY,ICENTC,UCSPAC,IFONT,FONTIN,XCHAR,YCHAR,XCSTRT,
     +       YCSTRT,ANGFAC
      COMMON /GSUTR/USRMAT(3,3),SCALEX,SCALEY,USANGX,USANGY,
     +       CUMAT(3,3),LINMOD,ICULNK,KPRINT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (XORIG,USRMAT(1,3)), (YORIG,USRMAT(2,3))
      EQUIVALENCE (CUORGX,CUMAT(1,3)), (CUORGY,CUMAT(2,3))
      EQUIVALENCE (NSTRAN(1),USRMAT(1,1))
      EQUIVALENCE (CHORGX,CHRMAT(1,3)), (CHORGY,CHRMAT(2,3))
      EQUIVALENCE (NCTRAN(1),CHRMAT(1,1))
C     ..
C     .. Data statements ..
      DATA MK1/15/
C
      DATA (IPT(I),I=1,127)/
     + 232,236,242,247,252,255,259,263,267,272,276,279,292,298,302,
     + 307, 46,309,310,312,315,318,321,324,326,329,332,334,336,  2,
     +  17, 59,
     *  24, 89, 73, 28, 53, 18, 85, 12, 38, 34, 14, 50, 48,  7,
     1  49,184,189,192,196,203,205,210,216,218,226, 68, 40, 10, 87,
     2  60, 62, 77, 93, 97,103,107,111,115,118,123,126,129,132,135,
     3 137,140,142,147,151,156,161,167,169,172,174,177,179,182,281,
     4 283,284,286,289,342,344,350,356,355,360,365,369,375,379,384,
     5 389,392,395,396,400,405,410,416,419,424,428,432,435,440,444,
     6 449,451,455,457,461,  1/
C
C---- Character vector strings
C
      DATA (IPC(I),I=1,100)/
     1      8,  8230, 12577,   529,  5380,    61,  8208,  4385,
     2     24,  1094,    74,  4384, 12054, 17412,  8740,    46,
     3  10279,  1344,  5894, 13863,   821,  4097, 18992,  8208,
     4   4385,  5008,    31, 12545, 17218,  5172,  5637,  9798,
     5  12064,-14845,-23005,-31165, 19460, 12576, 12086,  8464,
     6   4642,-24302,  8979,  5156,    27, 17412,    75, 19460,
     7  20224,  8464,  4642, 10513,  5895,  1558, 18311, 16512,
     8  12609, 18480, 18432, 17410,    14,  5894, 17975,  9029,
     9   8354, 12592, 10273,  8722,  4899,  5266,  9508,  7189,
     * -27630,-23518,-15869, 19203,  4144,  1537, 14103, 16966,
     1   8497,  5138, 13605,    76, 13879, 16199,-15869, 19717,
     2   5655,-26840, 13879, 16199,  1536, 14103, 16454,  2883,
     3  13316, 17989,  1847, 12288/
C
      DATA (IPC(I),I=101,200)/
     1  17217,    60, 12353,   272,  5894, 20023,  1792, 17975,
     2  12353,     8,    64, 13316,  1796,    79,  1863, 13316,
     3   2052, 17203, 12353,   272,  5894, 20023,  1792, 17412,
     4  18503, 12304, 10016,  7991,  4097, 16688,    79,-30975,
     5    839, 18452,     7,    72,  1792, 18212,    72,  1792,
     6  20288,   272,  5894, 17975, 12353,    24,  1792, 17975,
     7  13381,    12, 16418, 14150,  1559,  4097, 18736,  1792,
     8  17975, 13381, 13316,    72,  4097, 16688, 13379,  1300,
     9   5894, 20023, 18183, 10279,   263, 12304, 20289,  8199,
     *     79,     7,1 6419,    79,-16376, 20224,  9223, 18336,
     1     44, 18183, 18432,    71,  5894, 17975, 12353,  2320,
     2  10006,  4128,    56,  5894, 17975,   325, 18432,  5894,
     3  17975, 13381, 13332, 16707/
C
      DATA (IPC(I),I=201,300)/
     1   4144,     9,   321, 14391,  4097, 16688, 13379,  1796,
     2     79,  5123, 17204, 12353,   272,  5894, 20023,  1798,
     3   6215,  1300,  5894, 17975, 13381,   788,  4097, 16688,
     4  15427,  4097, 16688, 14150,  1559,  4868, 19507,  9250,
     5      4, 17472, 10788,  9250,   788,  4097, 16688, 13379,
     6  10788,  9250,   546,  8226, 16930,    42,    34,  1058,
     7  17442, 16418,    42,  9250, 16384, 10788,  9250,  8194,
     8   9282,    42, 16930,   548,  9250, 10784, 17442, 16388,
     9     34,    42, 17442, 17412,    34,    64,    42, 17442,
     *   1058,  8226,    42, 17442, 16388, 10752,  9250, 10784,
     1     32, 12039, 18439,  8192,  3879, 10006,-27081, 10279,
     2    835,  5138,    11, 12561,  4915,-32750,-16334,-15308,
     3   3091,-15359,-15869,-16379/
C
      DATA (IPC(I),I=301,400)/
     1  11296, 17408, 16388,-31743, 19520, 11296,  4352, 12054,
     2  18946,  9216,    72,-16383,-15869, 19460, 16898, 12595,
     3     74,-16126, 15612,  6196,-19692,-24027, 14609,  8196,
     4     76,  4609, 10016,    79,  8231,   578,    40,  9216,
     5   2112,-15359, 18436,  8208,  4385,  5008,  9251,-27883,
     6  14850,  9751,  7975,  4674,     1,-16079, 17216,  5172,
     7     11,-32760,  4097, 16688, 13379,  2836,-16312, 12353,
     8    272,  5123, 19252, 12802, 13379,   788,  4097, 18736,
     9  14150,  5671,  1168,    60, 12288, 17729,  5686,   773,
     *  12818,    75,-30975,  5123, 17204,    72,-20463,  8992,
     1   9363,  5413, 11284,  4097, 12576, 14005, 10039, 15910,
     2 -30975,-16381, 19458,-20463, 10016,    31,-23519,-31743,
     3   5123, 17204,    72, 12304/
C
      DATA (IPC(I),I=401,463)/
     1  17217,  5172,   259,    24,-31231,  4611, 17202, 13893,
     2   3350, 16465, 17862,  5686,   773, 12818,    75,-31743,
     3   5123, 19252,  4097, 16688,  4658,  5123, 19252,-19451,
     4   4374, 12320,    73,   260, 12304, 16577,    76,   516,
     5  16928,    76,   260,  8464, 16688,  8644,    44,  4096,
     6 -15307,  5124, 18480, 12288,-14782,   774, 12818,    75,
     7     64,  3140,  8496,  4898,  9508,    62,-23775, 12068,
     8   8464, 13090,  9508,    30,  5123, 19507,     0/
C
      NENTRY = 1
      FACTX = FACT7*SIZX
      FACTY = FACT7*SIZY
      AXOFF = 1.5
      KLETTR = LETTER
      GO TO 10
C
      ENTRY GSGSY0(ISYMB,XCOFF,YCOFF,SIZX,SIZY)
C     =========================================
C
C---- "SYMBOL" plotting
C
C---- This section plots one symbol centred on bottom left corner
C     each symbol is defined on a square grid (0,1,2,3,4) with
C     character height as 4 of these units
C
      NENTRY = 2
      FACTX = FACT4*SIZX
      FACTY = FACT4*SIZY
      AXOFF = 0.0
C
C---- set pointer into table
C
      KLETTR = ISYMB
      IF (KLETTR.LT.1) KLETTR = 1
C
   10 CONTINUE
      ITABLE = IPT(KLETTR)
      LINMOD = .FALSE.
      XSPACE = (CHRSPX+1.0)*SIZX
      YSPACE = CHRSPY*SIZY
C
C---- if a symbol then jump into generator loop
C
      IF (NENTRY.NE.2) THEN
C
C---- if letter = 0 then it is a null char and is ignored
C     recognises ascii 32 to 126 i.e. the full set
C
        IF (KLETTR.EQ.0) THEN
          GO TO 50
        END IF
      END IF
C
C---- draw characters as chains of vectors
C   each byte of the table contains two hexadecimal digits (0...15)
C   which define idx,idy as mod(idigit,8), using the hex '8' bit for
C   marking end of char or end of vector chain
C
C---- set markers for a new letter
C
      XSTART = XCOFF*SIZX + XCSTRT
      YSTART = YCOFF*SIZY + YCSTRT
C
      OLDVEC = .FALSE.
      ENDVST = .FALSE.
      ENDCHR = .FALSE.
   20 CONTINUE
C
C---- use one table entry
C
      IW = IPC(ITABLE)
C
C---- set offset value if iw is negative.
C
      ISN = 0
      IF (IW.LT.0) ISN = -1
C
C---- two vector end points per integer*2 table entry
C
      DO 30 IPX = 1,2
C
C---- get y displacement on grid
C     iw contains 2 vector end points; 1 per 4 bits.
C
        IDY = IW .AND. MK1
        IDY = IDY + ISN
        IW = IW/16
C
C---- get x displacement on grid
C
        IDX = IW .AND. MK1
        IDX = IDX + ISN
        IW = IW/16
C
C---- if hex'08' bit on, end of character
C
        IF (IDY.GE.8) THEN
          IDY = IDY - 8
          ENDCHR = .TRUE.
        END IF
C
C---- Reduce Y value if lower case letters g,j,p,q or y 
C
        IF (KLETTR.EQ.103 .OR. KLETTR.EQ.106 .OR. KLETTR.EQ.112 .OR.
     +      KLETTR.EQ.113 .OR. KLETTR.EQ.121) IDY = IDY - 2
C
C---- if hex'08' bit on, end of vector string, but not character
C
        IF (IDX.GE.8) THEN
          IDX = IDX - 8
          ENDVST = .TRUE.
        END IF
C
C---- calculate vector end point and apply scale
C
        ADX = (REAL(IDX)+AXOFF)*FACTX
        ADY = REAL(IDY)*FACTY
        XNEW = XSTART + ADX
        YNEW = YSTART + ADY
C
C---- plot vector
C
        IF (.NOT.OLDVEC) THEN
C
C--- first time, start new vector
C
          OLDVEC = .TRUE.
        ELSE IF ((XOLD.NE.XNEW) .OR. (YOLD.NE.YNEW)) THEN
          CALL GSMVTO(XOLD,YOLD)
          CALL GSDWTO(XNEW,YNEW)
        END IF
C
C---- test for end of char
C
        IF (ENDCHR) THEN
          GO TO 40
        ELSE
C
C---- test for end of vector chain
C
          IF (ENDVST) THEN
            OLDVEC = .FALSE.
            ENDVST = .FALSE.
          END IF
          XOLD = XNEW
          YOLD = YNEW
        END IF
   30 CONTINUE
C
C---- end of half-integer loop
C
      ITABLE = ITABLE + 1
C
C---- next packed word
C
      GO TO 20
C
C---- move cursor to start point of next letter or symbol
C
   40 CONTINUE
      XCSTRT = XCSTRT + XSPACE
      YCSTRT = YCSTRT + YSPACE
   50 LINMOD = .TRUE.
C
      END
C
C
C
      SUBROUTINE GSGCHC(CHAR1,XCOFF,YCOFF,KFONT)
C     ===========================================
C
C---- Draws a character from the fonts(0,1-4)
C
C     A.D.McLachlan JUN 1984. Last updated 27 JUL 84
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were NCHAR, NFONTS
C
C     CHAR1       = ascii character variable
C     XCOFF,YCOFF = offset of left baseline corner of character
C                   from character cursor is (xcoff,ycoff)
C                   character units
C
C     width and height scale of symbol is 1.0 character units
C     after the call the character cursor is moved by
C     (cwid+chrspx),(chrspy) character units, where cwid=1.0 for
C     uniform spacing and is 0.0-1.0 for non-uniform spacing
C
C          KFONT = 0  for full ASCII character set
C          KFONT = 1  for block letters (default)
C          KFONT = 2  for bold-face italics
C          KFONT = 3  for script
C          KFONT = 4  for greek
C
C     .. Scalar Arguments ..
      REAL SIZ1,SIZ2,XCOFF,YCOFF
      INTEGER KFONT,NLETT
      CHARACTER*1 NCHAR
      CHARACTER CHAR1*1
C     ..
C     .. Scalars in Common ..
      REAL ANGFAC,CHANGX,CHANGY,CHRSCX,CHRSCY,CHRSPX,CHRSPY,SCALEX,
     +     SCALEY,USANGX,USANGY,XCHAR,XCSTRT,YCHAR,YCSTRT
      INTEGER ICENTC,IFONT,KPRINT,LUNIN,LUNOUT
      LOGICAL*4 FONTIN,ICULNK,LINMOD,UCSPAC
C     ..
C     .. Arrays in Common ..
      REAL CHRMAT,CUMAT,USRMAT
      INTEGER*2 IFHT,IFSTRT,IFWID,IFX0,IFY0,LENGF
      CHARACTER*1 NFONTS
C     ..
C     .. Local Scalars ..
      REAL CHORGX,CHORGY,CUORGX,CUORGY,SIZX,SIZY,XORIG,YORIG
      INTEGER LETTER
C     ..
C     .. Local Arrays ..
      INTEGER NCTRAN(24),NSTRAN(24)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSGCH0,GSGCHF
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ICHAR
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSCHX/CHRMAT(3,3),CHRSCX,CHRSCY,CHANGX,CHANGY,CHRSPX,
     +       CHRSPY,ICENTC,UCSPAC,IFONT,FONTIN,XCHAR,YCHAR,XCSTRT,
     +       YCSTRT,ANGFAC
      COMMON /GSFNT/IFSTRT(150,4),LENGF(150,4),IFX0(150,4),
     +       IFY0(150,4),IFWID(150,4),IFHT(150,4),NFONTS(4,3000,4)
      COMMON /GSUTR/USRMAT(3,3),SCALEX,SCALEY,USANGX,USANGY,
     +       CUMAT(3,3),LINMOD,ICULNK,KPRINT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (XORIG,USRMAT(1,3)), (YORIG,USRMAT(2,3))
      EQUIVALENCE (CUORGX,CUMAT(1,3)), (CUORGY,CUMAT(2,3))
      EQUIVALENCE (NSTRAN(1),USRMAT(1,1))
      EQUIVALENCE (CHORGX,CHRMAT(1,3)), (CHORGY,CHRMAT(2,3))
      EQUIVALENCE (NCTRAN(1),CHRMAT(1,1))
C
      SIZX = 1.0
      SIZY = 1.0
      LETTER = ICHAR(CHAR1)
      GO TO 10
C
      ENTRY GSGCHS(CHAR1,XCOFF,YCOFF,SIZ1,SIZ2,KFONT)
C     ================================================
C
      LETTER = ICHAR(CHAR1)
      SIZX = SIZ1
      SIZY = SIZ2
      GO TO 10
C
      ENTRY GSGCHI(NLETT,XCOFF,YCOFF,KFONT)
C     ======================================
C
      SIZX = 1.0
      SIZY = 1.0
      LETTER = NLETT
      GO TO 10
C
      ENTRY GSGCHH(NCHAR,XCOFF,YCOFF,KFONT)
C     ======================================
C
      SIZX = 1.0
      SIZY = 1.0
      LETTER = ICHAR(NCHAR)
C
C---- check font
C
   10 IF (KFONT.EQ.0) THEN
        CALL GSGCH0(LETTER,XCOFF,YCOFF,SIZX,SIZY)
      ELSE
        CALL GSGCHF(LETTER,XCOFF,YCOFF,SIZX,SIZY,KFONT)
      END IF
C
      END
C
C
C
      SUBROUTINE GSGCHF(LETTER,XCOFF,YCOFF,SIZX,SIZY,KFONT)
C     ==========================================
C
C---- Draws a character from the fonts(1-4) by number
C
C     A.D.McLachlan JUN 1984. Last updated 27 JUL 84
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were NFONTS
C
C     HERSHEY FONTS FROM NATIONAL BUREAU OF STANDARDS Via A.M. Lesk
C     Modified by D.A. Agard to be a part of VAX PLOT81 package
C     LETTER      = ascii number of symbol (or some special number)
C     XCOFF,YCOFF = offset of left baseline corner of character
C                   from character cursor is (xcoff*sizx,ycoff*sizy)
C                   character units
C     SIZX,SIZY   = width and height scale of symbol in character
C                   character units
C
C          KFONT = 1  for block letters (default)
C          KFONT = 2  for bold-face italics
C          KFONT = 3  for script
C          KFONT = 4  for greek
C
C     .. Scalar Arguments ..
      REAL SIZX,SIZY,XCOFF,YCOFF
      INTEGER KFONT,LETTER
C     ..
C     .. Scalars in Common ..
      REAL ANGFAC,CHANGX,CHANGY,CHRSCX,CHRSCY,CHRSPX,CHRSPY,SCALEX,
     +     SCALEY,USANGX,USANGY,XCHAR,XCSTRT,YCHAR,YCSTRT
      INTEGER ICENTC,IFONT,KPRINT,LUNIN,LUNOUT
      LOGICAL*4 FONTIN,ICULNK,LINMOD,UCSPAC
C     ..
C     .. Arrays in Common ..
      REAL CHRMAT,CUMAT,USRMAT
      INTEGER*2 IFHT,IFSTRT,IFWID,IFX0,IFY0,LENGF
      CHARACTER*1 NFONTS
C     ..
C     .. Local Scalars ..
      REAL AH,AW,CHFACX,CHFACY,CHORGX,CHORGY,CHSIZH,CHSIZW,CSHIFT,
     +     CUORGX,CUORGY,FACT21,XORIG,XSHIFT,XSTART,XWOFF,XX1,XX2,YORIG,
     +     YSHIFT,YSTART,YY1,YY2
      INTEGER IFNT,ISTART,IXG,IYG,J,J1,J2,KLETTR,NSEG
C     ..
C     .. Local Arrays ..
      INTEGER NCTRAN(24),NSTRAN(24)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSDWTO,GSMVTO
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSCHX/CHRMAT(3,3),CHRSCX,CHRSCY,CHANGX,CHANGY,CHRSPX,
     +       CHRSPY,ICENTC,UCSPAC,IFONT,FONTIN,XCHAR,YCHAR,XCSTRT,
     +       YCSTRT,ANGFAC
      COMMON /GSFNT/IFSTRT(150,4),LENGF(150,4),IFX0(150,4),
     +       IFY0(150,4),IFWID(150,4),IFHT(150,4),NFONTS(4,3000,4)
      COMMON /GSUTR/USRMAT(3,3),SCALEX,SCALEY,USANGX,USANGY,
     +       CUMAT(3,3),LINMOD,ICULNK,KPRINT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (XORIG,USRMAT(1,3)), (YORIG,USRMAT(2,3))
      EQUIVALENCE (CUORGX,CUMAT(1,3)), (CUORGY,CUMAT(2,3))
      EQUIVALENCE (NSTRAN(1),USRMAT(1,1))
      EQUIVALENCE (CHORGX,CHRMAT(1,3)), (CHORGY,CHRMAT(2,3))
      EQUIVALENCE (NCTRAN(1),CHRMAT(1,1))
C     ..
C     .. Data statements ..
C
      DATA FACT21/0.0476190/
C
C---- each character is defined on an integer grid with 21 units for the
C     character height and various widths the scaled character is 1.0 
C     character units  high (charh*21 grid units) the width is a multiple 
C     of 1.0/21.0 character units. in font 1:
C     for capital letters the lowest point is at iy=-8 and top at iy=+13
C     for lower-case letters the top is normally at iy=+6 with descenders
C     down to iy= -15 the [ ] | go from -15 to +17 the letter baseline is 
C     therefore at -8. relative to this baseline letters rise to +21
C     special chars to +25, lower case to +14, descents to -7
C     widths are 20 for numbers (0-9),25 letter "m",27 for "@" is widest.
C     for each font the grid values lie in the range
C       IX=(IFX0+1)...(IFWID-1)
C       IY=(IFY0+1)...(IFHT+1)
C     in this drawing routine any big letters are shrunk to fit in a
C     "standard character box" (1.5) character units high and (1.0)
C     units wide,with the baseline (0.5) units above the bottom of the box.
C     positioning of characters is referred to the left end of the baseline
C     (icentc=0) or the centre of the upper box(icentc=1) 0.5 units
C     above the baseline.
C
      LINMOD = .FALSE.
      IFNT = KFONT
      IF (IFNT.LT.1 .OR. IFNT.GT.4) IFNT = 1
C
C---- set up sizes  and starting point
C
      CHSIZH = FACT21*SIZY
      CHSIZW = FACT21*SIZX
      XSTART = XCOFF*SIZX + XCSTRT
      YSTART = YCOFF*SIZY + YCSTRT
C
      KLETTR = LETTER - 31
      IF ((KLETTR.LT.1) .OR. (KLETTR.GT.146)) KLETTR = 1
C
C---- size constants from table: height and width on (0...21) grid
C
      AH = IFHT(KLETTR,IFNT) + 9
      AW = IFWID(KLETTR,IFNT)
C
C---- max width will never exceed 21.0 units
C
      XWOFF = 0.0
      IF (AW.GT.21.0) THEN
        CHFACX = (21.0/AW)*CHSIZW
      ELSE
        CHFACX = CHSIZW
C
C---- for uniform spacing
C
        IF (UCSPAC) XWOFF = 0.5* (21.0-AW)
      END IF
C
C---- max height will not exceed 21.0 units
C
      IF (AH.LE.21.0) THEN
        CHFACY = CHSIZH
      ELSE
        CHFACY = (21.0/AH)*CHSIZH
      END IF
C
      ISTART = IFSTRT(KLETTR,IFNT)
      NSEG = LENGF(KLETTR,IFNT)
      IF (NSEG.NE.0) THEN
C
C---- grid origin on letter baseline
C
        IXG = IFX0(KLETTR,IFNT)
        IYG = IFY0(KLETTR,IFNT) + 8
C
C---- build letter out of elementary vectors
C
        J1 = ISTART
        J2 = ISTART + NSEG - 1
        DO 10 J = J1,J2
          XX1 = ICHAR(NFONTS(1,J,IFNT)) + IXG
          YY1 = ICHAR(NFONTS(2,J,IFNT)) + IYG
          XX2 = ICHAR(NFONTS(3,J,IFNT)) + IXG
          YY2 = ICHAR(NFONTS(4,J,IFNT)) + IYG
C
C---- scale
C
          XX1 = (XX1+XWOFF)*CHFACX
          YY1 = YY1*CHFACY
          XX2 = (XX2+XWOFF)*CHFACX
          YY2 = YY2*CHFACY
C
C---- place on page
C
          XX1 = XSTART + XX1
          YY1 = YSTART + YY1
          XX2 = XSTART + XX2
          YY2 = YSTART + YY2
C
C---- draw line vector
C
          CALL GSMVTO(XX1,YY1)
          CALL GSDWTO(XX2,YY2)
   10   CONTINUE
      END IF
C
C---- position of next character up to 21.0 grid units to right
C
      IF (UCSPAC) THEN
        CSHIFT = 21.0*CHSIZW
      ELSE
        CSHIFT = AW*CHFACX
C
C---- default cshift as (6.0/7.0) * grid size of 21
C
        IF (CSHIFT.EQ.0.0) CSHIFT = 18.0*CHSIZW
      END IF
      XSHIFT = CHRSPX*SIZX + CSHIFT
      YSHIFT = CHRSPY*SIZY
C
C---- end of letter loop set position for next character operation
C
      XCSTRT = XCSTRT + XSHIFT
      YCSTRT = YCSTRT + YSHIFT
C
      LINMOD = .TRUE.
C
      END
C
C
C
      SUBROUTINE GSGSYM(NSYMB,NSET)
C     ==============================
C
C
C---- Generates a symbol number (nsymb) from set (nset)
C
C     A.D. McLachlan JUL 1984. Last updated JUL 27 1984
C
C---- Three entries here:
C
C      (1) GSGSYM  draws centred symbol of standard size
C                    and leaves character cursor unmoved
C      (2) GSGSYS  draws centred symbol of chosen size and offset
C                    and leaves character cursor unmoved
C      (3) GSGSYC  treats symbol as a special form of letter
C                    and centres it as for letters,at bottom left,
C                    moving the character cursor
C
C     XCOFF,YCOFF  = offsets of symbol centre oor letter corner from
C                    the character cursor are (xcoff*sizx,ycoff*sizy)
C     SIZX,SIZY    = scale for size in character units
C
C---- After drawing the symbol the character cursor moves by the
C       normal letter spacing
C
C   NSET=1,2 only available now !!!
C     32 symbols in set 1 from font 0 tables
C     46 symbols in set 2 from font 1 tables (characters 129-174)
C
C     List of symbols in set 1 and set 2
C     SET 1---                     SET 2---
C   A   1. square + line to centre   A  1. left <  bracket
C   B   2. octagon + line to cent    B  2. right > bracket
C   C   3. plus sign                 C  3. double bar ||
C   D   4. x cross                   D  4. +or-
C   E   5. triang + line to centre   E  5. multiply x
C   F   6. diamond + line to cent    F  6. decimal .
C   G   7. + with top arrow          G  7. divide by
C   H   8. x with top link           H  8. not=
C   I   9. z                         I  9. identically=
C   J  10. y                         j 10. <or=
C   K  11. x with top bottom lnk     k 11. >or=
C   L  12. vertical bar              L 12. varies as
C   M  13. square with ears          M 13. accent /
C   N  14. union jack star           N 14. accent \
C   O  15. crossed square            O 15. accent u
C   P  16. pentagram star            P 16. quote 9
C   Q  17. logical not --|           Q 17. quote 6
C   R  18. horiz bar --              R 18. quote back 9
C   S  19. cap lambda                S 19. quote back 6
C   T  20. ident = triple BAR        T 20. square root
C   U  21. right arrow               U 21. boolean [
C   V  22. low --                    V 22. boolean u
C   W  23. +or-                      W 23. boolean ]
C   X  24. v                         X 24. boolean ^
C   Y  25. square root               Y 25. boolean epsilon
C   Z  26. down arrow                Z 26. right arrow
C   1  27. triangle delta            1 27. up arrow
C   2  28. multiply x                2 28. left arrow
C   3  29. divide by                 3 29. down arrow
C   4  30. us "cent"                 4 30. partial deriv d
C   5  31. vertical high bar         5 31. grad d
C   6  32. underline                 6 32. square root
C   7                                7 33. integral
C   8                                8 34. circuit integral 0/
C   9                                9 35. infinity
C   a                                a 36. double ss sign
C   b                                b 37. dagger sign
C   c                                c 38. double dagger
C   d                                d 39. backward "e"
C   e                                e 40. aleph "n"
C   f                                f 41. ff ligature
C   g                                g 42. fi ligature
C   h                                h 43. fl ligature
C   i                                i 44. ffi ligature
C   j                                j 45. ffl ligature
C   k                                k 46. small "1" (footnote)
C
C
C   Symbols suitable for marking points on graphs are:
C       SET 1  :-   1 2 3 4 5 6 11 13 14 15 27 28=4
C        alias      A B C D E F  K  M  N  O  1  2=D
C       SET 2  :-   5 6 31
C        alias      E F  5
C
C   Option to get symbols through an ascii character code
C       NSYMB(1-26) = LETTERS (A-Z)
C       NSYMB(27-35) = DIGITS(1-9)
C       NSYMB(36-61) = LETTERS (a-z)
C
C---- Standard centred symbol
C
C     .. Scalar Arguments ..
      REAL SIZX,SIZY,XCOFF,YCOFF
      INTEGER NSET,NSYMB
      CHARACTER CHAR1*1
C     .. Scalars in Common ..
      REAL SCALEX,SCALEY,USANGX,USANGY
      INTEGER KPRINT,LUNIN,LUNOUT
      LOGICAL*4 ICULNK,LINMOD
C     ..
C     .. Arrays in Common ..
      REAL CUMAT,USRMAT
C     ..
C     .. Local Scalars ..
      REAL SZX,SZY,XCF,XCUR,YCF,YCUR
      INTEGER ISYMB,KFONT,LETTER,NCHAR,NENTRY
C     ..
C     .. External Subroutines ..
      EXTERNAL GSFCUR,GSGCHF,GSGSY0,GSPCUR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ICHAR
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSUTR/USRMAT(3,3),SCALEX,SCALEY,USANGX,USANGY,
     +       CUMAT(3,3),LINMOD,ICULNK,KPRINT
C
C     .. Save Statement ..
C
      SAVE
C
      NENTRY = 1
      ISYMB = NSYMB
      XCF = 0.0
      YCF = 0.0
      SZX = 1.0
      SZY = 1.0
      GO TO 20
C
      ENTRY GSGSYS(NSYMB,NSET,XCOFF,YCOFF,SIZX,SIZY)
C     ==============================================
C
C---- Centred symbol of chosen with chosen offset
C
      NENTRY = 1
      ISYMB = NSYMB
      GO TO 10
C
      ENTRY GSGSYC(CHAR1,NSET,XCOFF,YCOFF,SIZX,SIZY)
C     ===============================================
C
C---- Interpret character char1 as a special type of letter symbol
C     variable size and offset
C
      NENTRY = 2
      NCHAR = ICHAR(CHAR1)
      IF ((NCHAR.GE.49) .AND. (NCHAR.LE.57)) THEN
C
C---- Digits 1-9 ascii 49-57 aliased to symbols 27-35
C
        ISYMB = NCHAR - 22
      ELSE IF ((NCHAR.GE.65) .AND. (NCHAR.LE.90)) THEN
C
C---- Letters a-z ascii 65-90 aliased to symbols 1-26
C
        ISYMB = NCHAR - 64
      ELSE IF ((NCHAR.GE.97) .AND. (NCHAR.LE.107)) THEN
C
C---- letters A-K ascii 97-107 as symbols 36-46
C
        ISYMB = NCHAR - 61
      ELSE
        ISYMB = NCHAR
      END IF
C
   10 CONTINUE
      XCF = XCOFF
      YCF = YCOFF
      SZX = SIZX
      SZY = SIZY
C
C---- Check for legal input
C
   20 IF ((NSET.LT.1) .OR. (NSET.GT.2)) THEN
        IF (KPRINT.GE.1) WRITE (LUNOUT,FMT=6000) NSET
        NSET = 2
      END IF
      LETTER = ISYMB
      IF (NSET.EQ.2) LETTER = ISYMB + 128
C
C---- All "symbols" are drawn centred in the middle
C     and cursor must not move,so save its position
C     all "special letters" are centred at bottom left
C
      IF (NENTRY.EQ.1) THEN
        XCF = XCF - 0.5
        YCF = YCF - 0.5
        CALL GSFCUR(XCUR,YCUR)
      END IF
C
C---- Draw the character
C
      GO TO (30,40) NSET
   30 CALL GSGSY0 (LETTER,XCF,YCF,SZX,SZY)
      GO TO 50
   40 KFONT = 1
      CALL GSGCHF(LETTER,XCF,YCF,SZX,SZY,KFONT)
C
C---- For "symbols" restore character cursor
C
   50 IF (NENTRY.EQ.1) CALL GSPCUR(XCUR,YCUR)
C
C---- Format statements
C
 6000 FORMAT (2X,'!!!GSGSYM ERROR: NSET=',I5,
     +    ' NOT 1 OR 2 - RESET AS ','2 ')
C
      END
C
C
C
      SUBROUTINE GSHAIR(X,Y,CHKEY)
C     =============================
C
C---- Cross-hair control
C
C     A.D. McLachlan SEP 1984. Last updated 19 SEP 1984
C
C---- Place vt640 cross-hairs in user space, move them
C     with the four cursor keys ^ V < > , and read position
C     in user space when any character key is pressed
C
C---- NOTE: The vt640 manual says that the cross-hair only
C     responds to punctuation keys, but it appears to
C     respond to any character key, upper-  or lower-case
C     after the key is pressed the cross hair vanishes.
C     the terminal cannot be used for typing or graphics
C     till the hair is off
C
C     Terminal allows range ix=(0:1023) and iy=(0:779)
C     sometimes the position reported differs from the
C     position set by 1 unit
C
C     .. Scalar Arguments ..
      REAL X,Y
      CHARACTER CHKEY*1
C     ..
C     .. Scalars in Common ..
      REAL DOTMMX,DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWBLMX,DWBLMY,
     +     DWLIMX,DWLIMY,FACX,FACY,PAPLMX,PAPLMY,SCALEX,SCALEY,UBXMAX,
     +     UBXMIN,UBYMAX,UBYMIN,USANGX,USANGY,V64LMX,V64LMY,XFOFF,YFOFF
      INTEGER ICOLOR,IDRLVL,IDXOFF,IDYOFF,IPRINT,IUNIT,IXMAX,IXMIN,
     +        IXOLD,IYMAX,IYMIN,IYOLD,KPRINT,LINWT,LUNIN,LUNOUT,MCNTFL,
     +        MDEVIC,MDIREC,MIXCOL,MODOLD,MOUT,MPIC,MSCAFL,NERROR,NPICS
      LOGICAL*4 DEVCON,ICULNK,INITON,LINMOD
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. Arrays in Common ..
      REAL CUMAT,USRMAT
C     ..
C     .. Local Scalars ..
      REAL AV,BV,CUORGX,CUORGY,CV,DETU,DV,XB,XF,XORIG,YB,YF,YORIG
      INTEGER IX,IXHIGH,IXLOW,IY,IYHIGH,IYLOW
      INTEGER*4 NCODE
      LOGICAL*1 INSIDE
C     ..
C     .. Local Arrays ..
      INTEGER NSTRAN(24)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSCLPT,GSHRTM,GSTYTM
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSDVT/FACX,FACY,XFOFF,YFOFF,IDXOFF,IDYOFF,
     +       IXOLD,IYOLD,MODOLD
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
      COMMON /GSUTR/USRMAT(3,3),SCALEX,SCALEY,USANGX,USANGY,
     +       CUMAT(3,3),LINMOD,ICULNK,KPRINT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (XORIG,USRMAT(1,3)), (YORIG,USRMAT(2,3))
      EQUIVALENCE (CUORGX,CUMAT(1,3)), (CUORGY,CUMAT(2,3))
      EQUIVALENCE (NSTRAN(1),USRMAT(1,1))
C     ..
C     .. Data statements ..
      DATA IXLOW/96/,IXHIGH/975/,IYLOW/13/,IYHIGH/768/
C
C---- Check device
C
      IF ((MDIREC.NE.0) .AND. (MDEVIC.EQ.3)) THEN
C
C---- Convert coords to drawing board and then to device grid
C
        XB = USRMAT(1,1)*X + USRMAT(1,2)*Y + XORIG
        YB = USRMAT(2,1)*X + USRMAT(2,2)*Y + YORIG
C
C---- Check whether inside window
C
        CALL GSCLPT(XB,YB,NCODE,INSIDE)
        IF (INSIDE) THEN
          IX = NINT(XB*FACX+XFOFF) + IDXOFF
          IY = NINT(YB*FACY+YFOFF) + IDYOFF
        ELSE
          IX = IDXOFF + 1
          IY = IDYOFF + 1
        END IF
C
C---- Check whether inside device viewport for plot84
C
        IF (IX.LT.IXLOW) IX = IXLOW
        IF (IX.GT.IXHIGH) IX = IXHIGH
        IF (IY.LT.IYLOW) IY = IYLOW
        IF (IY.GT.IYHIGH) IY = IYHIGH
C
C---- For printing reset terminal to teletype mode
C
        IF (IPRINT.GE.2) THEN
          CALL GSTYTM
          WRITE (LUNOUT,FMT=6000) X,Y,IX,IY
        END IF
C
C---- Place hair at chosen position steer hair read new position by 
C     pressing a key and terminal returns to graphics letter mode
C
        CALL GSHRTM(IX,IY,CHKEY)
C
C---- Force position to lie in range on screen
C
        IF (IX.LT.IXLOW) IX = IXLOW
        IF (IX.GT.IXHIGH) IX = IXHIGH
        IF (IY.LT.IYLOW) IY = IYLOW
        IF (IY.GT.IYHIGH) IY = IYHIGH
C
C---- Convert to drawing board
C
        XF = IX - IDXOFF
        YF = IY - IDYOFF
        XB = (XF-XFOFF)/FACX
        YB = (YF-YFOFF)/FACY
C
C---- Convert back to user coordinates
C
        DETU = USRMAT(1,1)*USRMAT(2,2) - USRMAT(1,2)*USRMAT(2,1)
        IF (DETU.EQ.0.0) DETU = 1.0E-20
        AV = USRMAT(2,2)*DETU
        BV = -USRMAT(2,1)*DETU
        CV = -USRMAT(1,2)*DETU
        DV = USRMAT(1,1)*DETU
        XB = XB - XORIG
        YB = YB - YORIG
        X = AV*XB + BV*YB
        Y = CV*XB + DV*YB
        IF (IPRINT.GE.2) THEN
          CALL GSTYTM
          WRITE (LUNOUT,FMT=6002) X,Y,IX,IY
          WRITE (LUNOUT,FMT=6004) CHKEY
        END IF
      END IF
C
C---- Format statements
C
 6000 FORMAT (1X,'GSHAIR SET   AT:(X,Y)= ',2F12.4,' (IX,IY)= ',2I6)
 6002 FORMAT (1X,'GSHAIR FOUND AT:(X,Y)= ',2F12.4,' (IX,IY)= ',2I6)
 6004 FORMAT (1X,'CHKEY= ',A)
C
      END
C
C
      SUBROUTINE GSINIT(PLTNAM)
C     ==========================
C
C---- Plot84 graphics program jun 1984 adapted from d.a. agard plot82
C
C     A.D. McLachlan JUN 1984. Last updated 1 OCT 1984
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were NFONTS
C
C---- To plot on plotfile, paper or vt640 screen
C
C     Definitions of common blocks and variables in plot84
C
C     <<GSUTR>>      ((user transformation variables))
C     USRMAT(3,3)   =  user scaling matrix:
C
C     XDRW   =  USRMAT(1,1)*XU+USRMAT(1,2)*YU + XORIG
C     YDRW   =  USRMAT(2,1)*XU+USRMAT(2,2)*YU + YORIG
C     XORIG :=: USRMAT(1,3)
C     YORIG :=: USRMAT(2,3)
C
C     XORIG,YORIG = user origin on drawing board (mm)
C
C     SCALEX,SCALEY =  mm equiv of 1 user unit along local x,y
C     USANGX,USANGY =  angles of local user axes (radians)
C     CUMAT(3,3)    =  character*user scaling matrix
C
C     CUORGX:=:CUMAT(1,3) 
C     CUORGY:=:CUMAT(2,3)
C     CUORGX,CUORGY =  character*user origin value
C
C     LINMOD        =  .TRUE. for line mode
C     ICULNK        =  .TRUE. for user char trans linked
C     NSTRAN(24)    =  array equivalent to (usrmat...iculnk)
C     KPRINT        =  print control, copy of iprint for some routines
C
C     <<GSCHX>>     ((character position variables))
C
C     CHRMAT(3,3)   =  character transformation matrix
C     CHORGX:=:CHRMAT(1,3) CHORGY:=:CHRMAT(2,3)
C     CHORGX,CHORGY =  character origin in char units relative to (xchar,ychar)
C
C     CHRSCX,CHRSCY =  char width and height in character units (default=3.0)
C     CHANGX,CHANGY =  angles of char x and y (radians)
C     CHRSPX,CHRSPY =  character space extra values
C     ICENTC        =  character centering control
C     UCSPAC        =  .TRUE. for uniform spacing
C     IFONT         =  font number
C     FONTIN        =  .TRUE. after font tables read in
C     XCHAR,YCHAR   =  character anchor point
C     XCSTRT,YCSTRT =  character cursor position relative to anchor point
C                      in character units on character grid
C     ANGFAC        =  pi/180.0 degrees to radians
C     NCTRAN(24)    =  array equivalent to (chrmat...angfac)
C
C     <<GSDWX>>      ((pen position variables))
C
C     XNOW,YNOW     =  current user position
C     XBNEW,YBNEW   =  current board position
C     XBOLD,YBOLD   =  old board position
C
C     <<GSDVW>>     ((DEVICE AND WINDOW VARIABLES))
C
C     MDEVIC        =  device (1)undef (2)paper (3) vt640
C     MDIREC        =  (0)plotfile (1) direct
C     MOUT          =  vt640 unit symbolic number
C     MPIC          =  series number of picture
C     MSCAFL,MCNTFL =  floating scale and origin
C     DWLIMX,DWLIMY =  current drawing board size mm
C     DVXMIN,DVXMAX =  device x viewport mm
C     DVYMIN,DVYMAX =  device y viewport mm
C     UBXMIN,UBXMAX =  drawing board x window mm
C     UBYMIN,UBYMAX =  drawing board y window mm
C     FILNAM(*80)   =  plotfile name
C     DEVCON        =  logical*4 .true. when device switched on
C     INITON        =  .true. if GSinit called
C     NERROR        =  no of out of bounds errors
C     DWBLMX,DWBLMY =  drawing board max size mm
C     PAPLMX,PAPLMY =  paper max size mm
C     V64LMX,V64LMY =  vt640 max size mm
C     IUNIT         =  symbolic unit number (plotfile)
C     DOTMMX,DOTMMY =  dots per mm along device x y (default (10.0,10.0))
C     IXMIN,IXMAX   =  actual used x range of plot pixels
C     IYMIN,IYMAX   =  actual used y range of plot pixels
C     LINWT,ICOLOR  =  line-thickness, colour (default 4)
C     MIXCOL        =  (0) monochrome (1) mixed colours
C     NPICS         =  number of pictures in the run
C     IPRINT        =  print control (0) none (1) normal, (2,3) more
C     IDRLVL        =  initiation level (0) no-init, (1) init, (2) trset,
C                  (3) drawing in progress
C     TITLE(*80)    =  plot title
C
C     <<GSDVT>>      ((device mm to pixels integers transf))
C
C     FACX,FACY     =  scale factors
C     XFOFF,YFOFF   =  origin offsets
C     IDXOFF,IDYOFF =  pixel margins
C
C     Transformation is: IX = (XDRW*FACX+XFOFF)+IDXOFF
C                        IY = (YDRW*FACY+YFOFF)+IDYOFF
C
C     IXOLD,IYOLD    = pixel values of previous plotted point
C     MODOLD         = mode of previous point (1=draw,2=point,3=move)
C
C     <<GSCLP>>      ((clipping limits on drawing board))
C
C     BXMIN,BXMAX    = x range
C     BYMIN,BYMAX    = y range
C
C     <<GSFHD>>       ((plot header data)) 512 bytes (some spare)
C
C     IUNITR         = unit number (copy of iunit) not part of header
C     IREC(128)/AREC(128) contains the other data as follows:
C
C     1. NREC       = number of records after header
C     2. DOTMMX     = dots per mm along x
C     3. DOTMMY     = dots per mm along y
C     4. IXMIN      = low pixel x used
C     5. IXMAX      = high pixel x used
C     6. IYMIN      = low y
C     7. IYMAX      = high y
C     8. LINWT      = line thickness
C     9. ICOLOR     = colour
C     10. MIXCOL     = mixed colour
C     11. MDEVIC     = device type
C     12. MDIREC     = direct/deferred
C     13. MOUT       = output unit
C     14. MPIC       = picture number
C     15. MSCAFL     = floating scale
C     16. MCNTFL     = floating origin
C     17. DWLIMX     = drawing board x size
C     18. DWLIMY     = drawing board y size
C     19. DVXMIN     = device x min
C     20. DVXMAX     = device x max
C     21. DVYMIN     = device y min
C     22. DVYMAX     = device y max
C     23. NPICS      = number of pictures in this set
C     24-25. PASWRD  = password (character*8)  to show it is a plot84 file
C     26-40.           spare
C     41-60. TITLEH  = plot title(*80)
C     61-128.          spare
C
C     <<GSFNT>>  text font generation vectors
C
C     IFSTRT         = start array position
C     LENGF          = number of vectors
C     IFX0,IFY0      = origin shift
C     IFWID,IFHT     = width, height constants
C     NFONTS         = vector coords (4 bytes for x1,y1,x2,y2)
C
C     .. Scalar Arguments ..
      INTEGER JPRINT
      CHARACTER PLTNAM*(*), TITL*(*)
C     ..
C     .. Scalars in Common ..
      REAL ANGFAC,BXMAX,BXMIN,BYMAX,BYMIN,CHANGX,CHANGY,CHRSCX,CHRSCY,
     +     CHRSPX,CHRSPY,DOTMMX,DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,
     +     DWBLMX,DWBLMY,DWLIMX,DWLIMY,FACX,FACY,PAPLMX,PAPLMY,SCALEX,
     +     SCALEY,UBXMAX,UBXMIN,UBYMAX,UBYMIN,USANGX,USANGY,V64LMX,
     +     V64LMY,XBNEW,XBOLD,XCHAR,XCSTRT,XFOFF,XNOW,YBNEW,YBOLD,YCHAR,
     +     YCSTRT,YFOFF,YNOW
      INTEGER ICENTC,ICOLOR,IDRLVL,IDXOFF,IDYOFF,IFONT,IPRINT,IUNIT,
     +        IUNITR,IXMAX,IXMIN,IXOLD,IYMAX,IYMIN,IYOLD,KPRINT,LINWT,
     +        LUNIN,LUNOUT,MCNTFL,MDEVIC,MDIREC,MIXCOL,MODOLD,MOUT,MPIC,
     +        MSCAFL,NERROR,NPICS
      LOGICAL*4 DEVCON,FONTIN,ICULNK,INITON,LINMOD,UCSPAC
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. Arrays in Common ..
      REAL CHRMAT,CUMAT,USRMAT
      INTEGER IREC
      INTEGER*2 IFHT,IFSTRT,IFWID,IFX0,IFY0,LENGF
      CHARACTER*1 NFONTS
C     ..
C     .. Local Scalars ..
      REAL CHORGX,CHORGY,CUORGX,CUORGY,PI,XORIG,YORIG
      INTEGER NFLEN,NFSTRT
      CHARACTER PASWRD*8,TITLEH*80
C     ..
C     .. Local Arrays ..
      REAL*4 AREC(128)
      INTEGER NCTRAN(24),NSTRAN(24)
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      LOGICAL CCPONL
      EXTERNAL LENSTR,CCPONL
C     ..
C     .. External Subroutines ..
      EXTERNAL GSLVCK,GSRFNT,GSSTYL,GSXENV
C
C---- Force the linker to include "block data"
C
      EXTERNAL GSBLKD
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ATAN2,INDEX
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSCHX/CHRMAT(3,3),CHRSCX,CHRSCY,CHANGX,CHANGY,
     +       CHRSPX,CHRSPY,ICENTC,UCSPAC,IFONT,FONTIN,XCHAR,
     +       YCHAR,XCSTRT,YCSTRT,ANGFAC
      COMMON /GSCLP/BXMIN,BXMAX,BYMIN,BYMAX
      COMMON /GSDVT/FACX,FACY,XFOFF,YFOFF,IDXOFF,IDYOFF,
     +       IXOLD,IYOLD,MODOLD
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
      COMMON /GSDWX/XNOW,YNOW,XBNEW,YBNEW,XBOLD,YBOLD
      COMMON /GSFHD/IUNITR,IREC(128)
      COMMON /GSFNT/IFSTRT(150,4),LENGF(150,4),IFX0(150,4),
     +       IFY0(150,4),IFWID(150,4),IFHT(150,4),NFONTS(4,3000,4)
      COMMON /GSUTR/USRMAT(3,3),SCALEX,SCALEY,USANGX,USANGY,
     +       CUMAT(3,3),LINMOD,ICULNK,KPRINT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (XORIG,USRMAT(1,3)), (YORIG,USRMAT(2,3))
      EQUIVALENCE (CUORGX,CUMAT(1,3)), (CUORGY,CUMAT(2,3))
      EQUIVALENCE (NSTRAN(1),USRMAT(1,1))
      EQUIVALENCE (CHORGX,CHRMAT(1,3)), (CHORGY,CHRMAT(2,3))
      EQUIVALENCE (NCTRAN(1),CHRMAT(1,1))
      EQUIVALENCE (IREC(1),AREC(1))
      EQUIVALENCE (TITLEH,IREC(41))
      EQUIVALENCE (PASWRD,IREC(24))
C
C---- Check level
C
      IF (IDRLVL.EQ.3) CALL GSLVCK ('GSINIT')
      IDRLVL = 1
      INITON = .TRUE.
C
C---- All angles now stored as radians
C
      PI = ATAN2(1.0,1.0)*4.0
      ANGFAC = PI/180.0
      IPRINT = 1
      KPRINT = 1
      NERROR = 0
      TITLE = ' '
      MPIC = 0
      NPICS = 0
C
C---- Remove leading and trailing blanks from filename
C
      NFLEN = LENSTR(PLTNAM)
      NFSTRT = 1
      IF (NFLEN.GT.0) THEN
   10   CONTINUE
        IF ((INDEX(PLTNAM(NFSTRT:NFSTRT),' ').EQ.1) .AND.
     +      (NFSTRT.LE.NFLEN)) THEN
          NFSTRT = NFSTRT + 1
          GO TO 10
        END IF
      END IF
      IF ((NFLEN.LE.0) .OR. (NFSTRT.GT.NFLEN)) THEN
        IF (IPRINT.GT.0) WRITE (LUNOUT,FMT=6002) PLTNAM
      END IF
C
      FILNAM = PLTNAM(NFSTRT:NFLEN)
      DEVCON = .FALSE.
C
C---- Clear settings for device and windows etc
C
      CALL GSXENV
C
C---- GSPICT will call GSenvr to set windows on first picture
C     GSPICT will clear user transformations
C     clear plot style
C
      CALL GSSTYL
      FONTIN = .FALSE.
C
C---- Read in font first time through
C
      IF (.NOT.FONTIN) CALL GSRFNT
      FONTIN = .TRUE.
      RETURN
C
      ENTRY GSTITL (TITL)
C     =====================
C
      TITLE = TITL
      IF (IPRINT.GT.1) WRITE (LUNOUT,FMT=6000) TITLE
      RETURN
C
      ENTRY GSPRNT (JPRINT)
C     =======================
C
C---- IPRINT controls the output of messages
C    (0) no print
C    (1) normal
C    (2) header and scaling information etc
C    (3) line vector details
C
      IF ((JPRINT.LT.0) .OR. (JPRINT.GT.3)) JPRINT = 1
      IPRINT = JPRINT
      KPRINT = JPRINT
C
C---- Format statements
C
 6000 FORMAT (2X,'GSTITL: ',A)
 6002 FORMAT (1X,' GSINIT WARNING: BLANK OR ZERO-LENGTH FILENAME')
C
      END
C
C
C
      SUBROUTINE GSINTM(IUTERM)
C     ==========================
C
C---- Vt640 driver routines t.s. horsnell
C
C     A.D.McLachlan: CORRECTIONS FOR PLOT84 Last updated 3 FEB 1988.
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were BUFFER, LNTYPE, XHAIR, CONTRL, KEY, ZERO, ONE, BEL, BS, 
C     CR, ENQ, ESC, FF, FS, GS, HT, LF, RS, US, VT
C
C     These are a set of FORTRAN-callable routines for driving the
C     VT640 Tektronix-4010 emulator on the VT100 terminals. Routines are
C     provided for line, point and text drawing, crosshair reading,
C     VT100 cursor positioning and screen clearing.
C     The memory coords (0-1023 in x, 0-780 in y)
C     are automatically converted
C     to the display range (0-639 in x, 0-479 in y).
C     The origin is always the bottom left hand corner of the screen.
C
C---- In these routines a time delay routine GSsusp(milsec) which waits
C     for milsec milliseconds is used to  try to ensure that the changes of
C     terminal mode do not occur too fast for it to keep up. these apply
C     to changes between normal teletype (mode=mtty) and graphics modes.
C     ( vector or alpha)
C
C     CALL GSINTM(ITERM)
C       Clear the display screen and open the FORTRAN unit ITERM as the
C       plot stream for output to terminal. Also open the unit
C       ITERMR=ITERM-1 as the plot stream for input to read from terminal
C
C     CALL GSOPTM(ITERM)
C       Open the FORTRAN unit ITERM as the plot stream, but do not clear
C       the screen. Used when a new job wants to look at what is already
C       on the screen.
C
C     CALL GSOFTM(ITERM)
C       Close terminal graphics FORTRAN stream (do not clear screen)
C
C     CALL GSSCTM
C       Clear screen and leave in graphics vector mode 
C
C     CALL GSGRTM
C       Switch to VT640 mode (from a previously selected VT100 mode)
C
C     CALL GSMVTM(IX,IY)
C     CALL GSDWTM(IX,IY)
C     CALL GSPTTM(IX,IY)
C       Move to the position (IX,IY), draw a vector from the current
C       position to position (IX,IY) or place a point at
C       the position (IX,IY)
C
C     CALL GSTXTM(TEXT,ISIZE)
C       Write the string TEXT with char size ISIZE starting at
C       the current position defined by the last GSMVTM command.
C       The integer ISIZE=1,2,3,4 specifies the size in units of
C       the standard text size which is 7*9 physical dots on
C       the screen.
C
C     CALL GSHRTM(IX,IY,CHKEY)
C       Place the cross-hairs at (IX,IY), display them and then
C       wait for the user to position them. The crosshairs are driven
C       by the 4 cursor keys
C       The coordinates are read when any character key is pressed.
C       The cross hair then disappears and the terminal returns to
C       alphabetical mode. The ASCII key character is returned
C       in CHKEY (CHARACTER*1).
C
C     CALL GSDOTM
C       In VT640 mode, selects the normal drawing mode.
C
C     CALL GSERTM
C       In VT640 mode, rubs out any dots which subsequent draws,
C       points, or texts overwrites.
C
C     CALL GSRVTM
C       In VT640 mode, rubs out any dots which subsequent draws,
C       points, or texts overwrites.
C
C     CALL GSTYTM
C       Return the display to VT100 mode.
C
C     CALL GSMYTM(IX,IY)
C       Position the VT100 cursor to (IX,IY) 0<=IX<=79, 0<=IY<=23
C
C     CALL GSCYTM
C       Clear the VT100 screen, leaving the VT640 picture.
C       (vice-versa is not possible)
C
C     CALL GSLSTM(ITYPE)
C       In VT640 mode select the line type for vector drawing.
C          ITYPE=1-4
C         1 = Normal
C         2 = Dotted
C         3 = Dot-Dash
C         4 = Short Dash
C         5 = Long Dash
C
C     CALL GSBLTM
C       Ring the bell or BEEP of the terminal
C
C     .. Scalar Arguments ..
      INTEGER ISIZE,ITYPE,IUTERM,IX,IY
      CHARACTER CHKEY*1,STRING* (*)
C
C     .. Scalars in Common
      INTEGER MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,NERROR,IUNIT,IXMIN,
     +        IXMAX,IYMIN,IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IDRLVL
      REAL DWLIMX,DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,
     +     UBYMIN,UBYMAX,DOTMMX,DOTMMY,DWBLMX,DWBLMY,PAPLMX,PAPLMY,
     +     V64LMX,V64LMY
      LOGICAL*4 DEVCON,INITON
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. Local Scalars ..
      INTEGER ITERM,ITERMR,JSIZE,MALPHA,MDRAW,MHAIR,MODE,MOVE,MPOINT,
     +        MTTY,MVECT,NCLEAR,IDXOFF,IDYOFF,IXOLD,IYOLD,MODOLD
      REAL FACX,FACY,XFOFF,YFOFF
      CHARACTER*1 CONTRL,KEY,ZERO,ONE
      CHARACTER*1 BEL,BS,CR,ENQ,ESC,FF,FS,GS,HT,LF,RS,US,VT
      CHARACTER OUTLIN*14
C     ..
C     .. Local Arrays ..
      CHARACTER*1 LNTYPE(5),XHAIR(5)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSSUSP,GSXYTM
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC CHAR
C     ..
C     .. Common Blocks ..
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
      COMMON /GSDVT/FACX,FACY,XFOFF,YFOFF,IDXOFF,IDYOFF,
     +                IXOLD,IYOLD,MODOLD
C
C     .. Save Statement ..
C
      SAVE ITERM,ITERMR,MODE,/GSDVT/
C
C     .. Data statements ..
      DATA LNTYPE/' ','a','b','c','d'/
      DATA MODE/5/
      DATA MOVE/1/,MDRAW/2/,MPOINT/3/,MHAIR/4/,MALPHA/5/
      DATA MVECT/0/,MTTY/6/
C     ..
C
C---- Mips Compiler doesn't like backslashes (Escape character to shell)
C
      LNTYPE(1) = CHAR(92)
      ZERO=CHAR(0)
      ONE=CHAR(1)
      BEL=CHAR(7)
      BS=CHAR(8)
      CR=CHAR(13)
      ENQ=CHAR(5)
      ESC=CHAR(27)
      FF=CHAR(12)
      FS=CHAR(28)
      GS=CHAR(29)
      HT=CHAR(9)
      LF=CHAR(10)
      RS=CHAR(30)
      US=CHAR(31)
      VT=CHAR(11)
      NCLEAR = 1
      GO TO 10
C
      ENTRY GSONTM(IUTERM)
C     ====================
C
      NCLEAR = 0
      GO TO 10
C
      ENTRY GSOFTM(IUTERM)
C     =====================
C
C---- Turn off graphics stream
C
      ITERM = IUTERM
      IF (ITERM.NE.6) CLOSE (UNIT=ITERM)
      MODE = MTTY
      RETURN
C
      ENTRY GSSCTM()
C     =============
C
C---- Clear screen and leave in Alpha mode
C
      WRITE (ITERM,FMT=6022) US,ESC,FF
      MODE = MVECT
      RETURN
C
      ENTRY GSGRTM()
C     =============
C
      MODE = MVECT
      RETURN
C
      ENTRY GSMVTM(IX,IY)
C     ====================
C
C---- For move/draw/point try and do as much output in
C     one write statement as possible to reduce i/o calls to vms
C
      CALL GSXYTM(ITERM,IX,IY,ZERO)
      MODE = MOVE
      RETURN
C
      ENTRY GSDWTM(IX,IY)
C     ====================
C
      CALL GSXYTM(ITERM,IX,IY,GS)
      MODE = MDRAW
      RETURN
C
      ENTRY GSPTTM(IX,IY)
C     ====================
C
C---- Changed to avoid using point mode, which fails on some terminals
C     draw a zero-length vector at (ix,iy)
C
      CALL GSXYTM(ITERM,IX,IY,GS)
      MODE = MDRAW
      RETURN
C
      ENTRY GSHRTM(IX,IY,CHKEY)
C     ==========================
C
      MODE = MHAIR
      CALL GSXYTM(ITERM,IX,IY,GS)
C
C---- Position cross-hairs at last moveto point
C   "US" gives alpha mode
C   "ESC,'/f'" loads last value of (ix,iy) into xhair address
C             drive xhair with the 4 cursor keys upper right ^ V < >
C             automatic exit to alpha mode
C
      WRITE (ITERM,FMT=6022) US,ESC,SUB
      MODE = MHAIR
      READ (ITERMR,FMT=6008) XHAIR
C
C---- Decode coordinate  bytes the bit pattern code here is
C
C   bit value            32 16  8  4  2  1
C   bit number      7  6  5  4  3  2  1  0
C
C       HIGH X     P  0  1  X9 X8 X7 X6 X5
C       LOW  X     P  0  1  X4 X3 X2 X1 X0
C
C       HIGH Y     P  0  1  Y9 Y8 Y7 Y6 Y5
C       LOW  Y     P  0  1  Y4 Y3 Y2 Y1 Y0
C
C     IX = 32*HIGHX + LOWX
C     IY = 32*HIGHY + LOWY
C
      KEY = XHAIR(1)
      CHKEY = KEY
      IX=ICHAR(XHAIR(2)).AND.31
      IX=32*IX+(ICHAR(XHAIR(3)).AND.31)
      IY=ICHAR(XHAIR(4)).AND.31
      IY=32*IY+(ICHAR(XHAIR(5)).AND.31)
      MODE = MALPHA
      RETURN
C
      ENTRY GSTXTM(STRING,ISIZE)
C     ===========================
C
      MODE = MALPHA
C
C---- Writes at the position of the blinking cursor
C     set by a previous moveto
C    "US" selects alpha mode
C    "ESC, ISIZE" selects character size
C
      JSIZE = ISIZE - 1
      IF (JSIZE.LT.0) JSIZE = 0
      IF (JSIZE.GT.3) JSIZE = 3
      WRITE (ITERM,FMT=6010) US,ESC,CHAR(JSIZE+56),STRING
      RETURN
C
      ENTRY GSTYTM()
C     ===============
C
      WRITE (ITERM,FMT=6022) US,ESC,'2'
      MODE = MTTY
      RETURN
C
      ENTRY GSMYTM(IX,IY)
C     ====================
C
C      WRITE (OUTLIN,FMT=6014) US,ESC,'2',ESC,'[',IY,';',IX,'f'
C      WRITE (ITERM,FMT=6016) OUTLIN
      MODE = MTTY
      RETURN
C
      ENTRY GSCYTM()
C     =============
C
      WRITE (ITERM,FMT=6007) US,ESC,'2',ESC,'[2J'
      MODE = MTTY
      RETURN
C
      ENTRY GSDOTM()
C     =============
C
C---- Instruction "ESC/0d" sets dots-on level
C
      WRITE (ITERM,FMT=6018) US,ESC,'/0d'
      MODE = MVECT
      RETURN
C
      ENTRY GSERTM()
C     =============
C
C---- Instruction "ESC/1D" puts dots off (erase level)
C
      WRITE (ITERM,FMT=6018) US,ESC,'/1d'
      MODE = MVECT
      RETURN
C
      ENTRY GSRVTM()
C     =============
C
C---- Instruction "ESC/3d" Sets complement level
C
      WRITE (ITERM,FMT=6018) US,ESC,'/2d'
      MODE = MVECT
      RETURN
C
      ENTRY GSLSTM(ITYPE)
C     ====================
C
C---- Set line type
C
      IF ((ITYPE.LT.1) .OR. (ITYPE.GT.5)) ITYPE = 1
      WRITE (ITERM,FMT=6022) US,ESC,LNTYPE(ITYPE)
      MODE = MVECT
      RETURN
C
      ENTRY GSBLTM()
C     =============
C
C---- Sound the "bell" or "beep"
C
      WRITE (ITERM,FMT=6022) BEL
      MODE = MVECT
      RETURN
C
   10 CONTINUE
      ITERM = IUTERM
      ITERMR = IUTERM - 1
      NBFPTR = 0
      IF (MDIREC.EQ.1 .AND. MDEVIC.EQ.3) THEN
        ITERM = 6
        ITERMR = 5
      END IF
C
C---- Only clear the screen if GSintm was called
C
      IF (NCLEAR.NE.0) THEN
        WRITE (ITERM,FMT=6007) US,ESC,'2',ESC,'[2J'
        WRITE (ITERM,FMT=6022) GS,ESC,FF
      ENDIF
      MODE = MALPHA
C
C---- Format statements
C
 6007 FORMAT (7A1)
 6008 FORMAT (5A1)
 6010 FORMAT (3A1,A)
 6014 FORMAT (5A1,I2,A1,I2,A1)
 6016 FORMAT (A)
 6018 FORMAT (A1,A)
 6022 FORMAT (3A1)
C
      END
C
C
C 
      SUBROUTINE GSINUM(INUM,NDIGIT,SIZX,SIZY,NJUST)
C     ===============================================
C
C---- Plot an integer in (i-ndigit) format
C
C     A.D. McLachlan JUL 1984. Last updated 27 JUL 1984
C
C---- For integers adjust ndigit upward to avoid overflows
C
C**** ENTRY GSFNUM for f-ndigit-.-nafter format:  min=f3.0
C**** ENTRY GSENUM for e-ndigit-.-nafter format:  min=e8.1
C
C     Maximum of 20 digits allowed. max of 10 after point
C     each routine adjusts format if necessary to try to
C     avoid overflows keeping nafter as set
C     njust justifying control (1)left (2)centre (3)right
C
C     .. Scalar Arguments ..
      REAL ENUM,FNUM,SIZX,SIZY
      INTEGER INUM,NAFTER,NDIGIT,NJUST
C     ..
C     .. Scalars in Common ..
      REAL SCALEX,SCALEY,USANGX,USANGY
      INTEGER KPRINT,LUNIN,LUNOUT
      LOGICAL*4 ICULNK,LINMOD
C     ..
C     .. Arrays in Common ..
      REAL CUMAT,USRMAT
C     ..
C     .. Local Scalars ..
      REAL CUORGX,CUORGY,FABS,POW10,XORIG,YORIG
      INTEGER MAFTER,MDIGIT,NABS,NEEDED,NENTRY
      CHARACTER AFTER*3,DIGIT*3,BLANK20*20,TEXT*20
C     ..
C     .. Local Arrays ..
      INTEGER NSTRAN(24)
      CHARACTER FMTE(5)*3,FMTF(5)*3,FMTI(3)*3
C     ..
C     .. External Subroutines ..
      EXTERNAL GSCETS
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,LOG10,REAL
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSUTR/USRMAT(3,3),SCALEX,SCALEY,USANGX,USANGY,
     +                CUMAT(3,3),LINMOD,ICULNK,KPRINT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (XORIG,USRMAT(1,3)), (YORIG,USRMAT(2,3))
      EQUIVALENCE (CUORGX,CUMAT(1,3)), (CUORGY,CUMAT(2,3))
      EQUIVALENCE (NSTRAN(1),USRMAT(1,1))
C     ..
C     .. Data statements ..
      DATA BLANK20/' '/
      DATA FMTI/' (I',' 5 ',' ) '/
      DATA FMTF/' (F',' 10',' . ',' 4 ',' ) '/
      DATA FMTE/' (E',' 12',' . ',' 4 ',' ) '/
C
      NENTRY = 1
      GO TO 10
C
      ENTRY GSFNUM(FNUM,NDIGIT,NAFTER,SIZX,SIZY,NJUST)
C     =================================================
C
      NENTRY = 2
      GO TO 10
C
      ENTRY GSENUM(ENUM,NDIGIT,NAFTER,SIZX,SIZY,NJUST)
C     =================================================
C
      NENTRY = 3
C
   10 CONTINUE
C
C---- Check for reasonable data settings
C
      MDIGIT = NDIGIT
      IF (MDIGIT.GT.20) MDIGIT = 20
      IF (NENTRY.NE.1) THEN
        MAFTER = NAFTER
        IF (MAFTER.GT.10) MAFTER = 10
        IF (MAFTER.LT.0) MAFTER = 0
      END IF
C
C---- Integer format: enlarge if too short
C
      IF (NENTRY.EQ.1) THEN
        NABS = ABS(INUM)
        IF (NABS.NE.0) THEN
          NEEDED = LOG10(REAL(NABS)+0.5) + 1.0
          IF (INUM.LT.0) NEEDED = NEEDED + 1
          IF (MDIGIT.LT.NEEDED) MDIGIT = NEEDED
        END IF
        IF (MDIGIT.LT.1) MDIGIT = 1
C
C---- Decimal after point check F decimal format
C
      ELSE IF (NENTRY.EQ.2) THEN
        FABS = ABS(FNUM)
        IF (FABS.NE.0.0) THEN
C
C---- Needed is number of places before decimal point incl sign
C
          POW10 = LOG10(FABS)
          IF (POW10.GE.0.0) THEN
            NEEDED = POW10 + 1.5
          ELSE
C
C---- Number lt.(1.0) needs none before pt - but allow at least one digit after
C
            NEEDED = 0
          END IF
C
C---- Allow for  - sign
C
          IF (FNUM.LT.0.0) NEEDED = NEEDED + 1
C
C---- Increase mdigit to include the point
C
          NEEDED = NEEDED + 1
          IF (POW10.LT.0.0) THEN
C
C---- For number lt.1.0 allow at least one digit after point
C
            IF (MAFTER.LT.1) MAFTER = 1
          END IF
          IF (MDIGIT.LT. (NEEDED+MAFTER)) MDIGIT = NEEDED + MAFTER
C
C---- For fnum=0.0
C
        ELSE IF (MDIGIT.LT. (MAFTER+2)) THEN
          MDIGIT = MAFTER + 2
        END IF
C
C---- E format
C
      ELSE IF (NENTRY.EQ.3) THEN
        IF (MDIGIT.LT. (MAFTER+7)) MDIGIT = MAFTER + 7
      END IF
C
      WRITE (DIGIT,FMT=6000) MDIGIT
      IF (KPRINT.GE.3) WRITE (LUNOUT,FMT=6004) DIGIT
      IF (NENTRY.GT.1) WRITE (AFTER,FMT=6000) MAFTER
      IF ((KPRINT.GE.3) .AND. (NENTRY.GT.1)) WRITE (LUNOUT,
     +    FMT=6004) AFTER
      GO TO (20,30,40) NENTRY
C
C---- Set up internal integer format
C
   20 CONTINUE
      FMTI(2) = DIGIT
      TEXT = BLANK20
      IF (KPRINT.GE.3) WRITE (LUNOUT,FMT=6004) FMTI
      WRITE (TEXT,FMT=FMTI) INUM
      GO TO 50
C
C---- Set up internal decimal format
C
   30 CONTINUE
      FMTF(2) = DIGIT
      FMTF(4) = AFTER
      IF (KPRINT.GE.3) WRITE (LUNOUT,FMT=6004) FMTF
      TEXT = BLANK20
      WRITE (TEXT,FMT=FMTF) FNUM
      GO TO 50
C
C---- Set up internal "e" format
C
   40 CONTINUE
      FMTE(2) = DIGIT
      FMTE(4) = AFTER
      IF (KPRINT.GE.3) WRITE (LUNOUT,FMT=6004) FMTE
      TEXT = BLANK20
      WRITE (TEXT,FMT=FMTE) ENUM
C
C---- Plot the result
C
   50 IF (KPRINT.GE.3) WRITE (LUNOUT,FMT=6002) TEXT(1:MDIGIT)
      CALL GSCETS(TEXT(1:MDIGIT),SIZX,SIZY,NJUST)
C
C---- Format statements
C
 6000 FORMAT (I3)
 6002 FORMAT (1X,A)
 6004 FORMAT (1X,20A3)
C
      END
C
C
C
      SUBROUTINE GSLINE(ZA,ZB)
C     =========================
C
C     .. Array Arguments ..
      REAL ZA(2),ZB(2)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. External Subroutines ..
      EXTERNAL GSDWTO,GSMVTO
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
      CALL GSMVTO(ZA(1),ZA(2))
      CALL GSDWTO(ZB(1),ZB(2))
C
      END
C
C
C
      SUBROUTINE GSLRAS(IX1,IY1,IX2,IY2)
C     ===================================
C
C---- Plot a line as dots on a raster by bresenham's method
C
C     A.D. McLachlan JUN 1984. Last updated 18 JUN 1984
C
C---- Left and bottom refer to position of (x1,y1) along line
C     slow for slope inside range (-1.0,+1.0)
C     up for slope positive
C
C     .. Scalar Arguments ..
      INTEGER IX1,IX2,IY1,IY2
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      INTEGER IEND,IX,IY,MSIGN,ND,NX,NX2,NXY2,NY,NY2
      LOGICAL BOTTOM,LEFT,SLOW,UP
C     ..
C     .. External Subroutines ..
      EXTERNAL GSDOT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
      LEFT = (IX1.LE.IX2)
      BOTTOM = (IY1.LE.IY2)
      UP = (BOTTOM .EQV. LEFT)
      IF (UP) THEN
        MSIGN = 1
      ELSE
        MSIGN = -1
      END IF
      NX = ABS(IX2-IX1)
      NY = ABS(IY2-IY1)
      SLOW = (NX.GE.NY)
      WRITE (LUNOUT,FMT=6000) LEFT,BOTTOM,UP,SLOW
C
C---- Constants
C
      NX2 = 2*NX
      NY2 = 2*NY
      NXY2 = ABS(NX2-NY2)
C
C---- Set first point according to case
C
      IF (SLOW) THEN
C
C---- Slope between +-1 : end is defined by x value at left end
C
        IF (LEFT) THEN
          IX = IX1
          IY = IY1
          IEND = IX2
        ELSE
          IX = IX2
          IY = IY2
          IEND = IX1
        END IF
        ND = NY2 - NX
      ELSE
C
C---- Slope gt range +-1 : end is defined by y value at bottom end
C
        IF (BOTTOM) THEN
          IX = IX1
          IY = IY1
          IEND = IY2
        ELSE
          IX = IX2
          IY = IY2
          IEND = IY1
        END IF
        ND = NX2 - NY
      END IF
      WRITE (LUNOUT,FMT=6002) MSIGN,NX,NY,NXY2,ND
C
C---- First dot
C
      CALL GSDOTB(IX,IY)
C
C---- Draw rest of line
C
      IF (SLOW) THEN
   10   CONTINUE
C
C---- Line of small slope: move along x
C
        IF (IX.LT.IEND) THEN
          IX = IX + 1
          IF (ND.LT.0) THEN
            ND = ND + NY2
          ELSE
            IY = IY + MSIGN
            ND = ND - NXY2
          END IF
          CALL GSDOTB(IX,IY)
          GO TO 10
        END IF
      ELSE
   20   CONTINUE
C
C---- Line of steep slope: move along y
C
        IF (IY.LT.IEND) THEN
          IY = IY + 1
          IF (ND.LT.0) THEN
            ND = ND + NX2
          ELSE
            IX = IX + MSIGN
            ND = ND - NXY2
          END IF
          CALL GSDOTB(IX,IY)
          GO TO 20
        END IF
C
C---- Finished line
C
      END IF
C
C---- Format statements
C
 6000 FORMAT (2X,'LEFT BOTTOM UP SLOW = ',4L4)
 6002 FORMAT (2X,'MSIGN NX NY NXY2 ND = ',5I5)
C
      END
C
C
C
      SUBROUTINE GSLRSB(IX1,IY1,IX2,IY2)
C     ===================================
C
C---- Plot a line or single point as dots on a raster of bricks
C     by bresenham's method
C
C     A.D. McLachlan JUN 1984. Last updated 16 JUL 1984
C     9/2/90 - PJD: Changed definition of several BYTEs to INTEGER.
C     They were NPLOTS, MBIT6
C
C---- Definitions of brick sizes etc for trilog
C     Size of one brick: (48*64) dots, 6 dots/byte in (8*64)=512 bytes
C     Plot area (1536*8192) dots or (256*8192) bytes
C     Indexed dots(0:1536*0:8191) or bytes (0:255*0:8191)
C     Indexed bricks (0:31*0:127) one row of bricks=256*64=16384 bytes
C
C     .. Parameters ..
      INTEGER NBITB,NBITB1
      PARAMETER (NBITB=6,NBITB1=NBITB-1)
      INTEGER NWIDX,NWIDY,NBRIKX,NBRIKY
      PARAMETER (NWIDX=8,NWIDY=64,NBRIKX=32,NBRIKY=128)
      INTEGER NSIZEB,NSIZER
      PARAMETER (NSIZEB=NWIDX*NWIDY,NSIZER=NSIZEB*NBRIKX)
      INTEGER NWIDX1,NWIDY1
      PARAMETER (NWIDX1=NWIDX-1,NWIDY1=NWIDY-1)
      INTEGER NSZBX1
      PARAMETER (NSZBX1=NSIZEB-NWIDX1)
      INTEGER NSZRXB
      PARAMETER (NSZRXB=NSIZER+NWIDX-NSIZEB)
      INTEGER NBYTES,NBYTE1
      PARAMETER (NBYTES=NSIZER*NBRIKY,NBYTE1=NBYTES-1)
C     ..
C     .. Scalar Arguments ..
      INTEGER IX1,IX2,IY1,IY2
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Arrays in Common ..
      INTEGER NPLOTS
C     ..
C     .. Local Scalars ..
      INTEGER IBITX,IBRIKX,IBRIKY,IBX,IEND,IPX,IPY,IX,IY,JBRIK,JBYTE,ND,
     +        NX,NX2,NXY2,NY,NY2
      LOGICAL BOTTOM,LEFT,POINT,SLOW,UP
C     ..
C     .. Local Arrays ..
      INTEGER MBIT6(0:5)
C     ..
C     .. External Functions ..
      INTEGER ISHFT
      EXTERNAL ISHFT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSRAS/NPLOTS(0:NBYTE1)
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Data statements ..
      DATA MBIT6/1,2,4,8,16,32/
C
C---- Left and bottom refer to position of (x1,y1) along line
C     slow for slope inside range (-1.0,+1.0) up for slope positive
C
      LEFT = (IX1.LE.IX2)
      BOTTOM = (IY1.LE.IY2)
      UP = (BOTTOM .EQV. LEFT)
      NX = ABS(IX2-IX1)
      NY = ABS(IY2-IY1)
C
C---- Test for single point
C
      IF ((NX.EQ.0) .AND. (NY.EQ.0)) THEN
        IX = IX1
        IY = IY1
        POINT = .TRUE.
      ELSE
        POINT = .FALSE.
        SLOW = (NX.GE.NY)
C
C---- Constants
C
        NX2 = 2*NX
        NY2 = 2*NY
        NXY2 = ABS(NX2-NY2)
C
C---- Set first point according to case
C
        IF (SLOW) THEN
C
C---- Slope between +-1 : end is defined by x value at left end
C
          IF (LEFT) THEN
            IX = IX1
            IY = IY1
            IEND = IX2
          ELSE
            IX = IX2
            IY = IY2
            IEND = IX1
          END IF
          ND = NY2 - NX
        ELSE
C
C---- Slope gt range +-1 : end is defined by y value at bottom end
C
          IF (BOTTOM) THEN
            IX = IX1
            IY = IY1
            IEND = IY2
          ELSE
            IX = IX2
            IY = IY2
            IEND = IY1
          END IF
          ND = NX2 - NY
        END IF
      END IF
C
C---- First dot coordinates in bit,byte and
C     brick array are ibitx,(ibx,iby),(ipx,ipy),(ibrikx,ibriky)
C
      IBX = IX/NBITB
      IBITX = IX - NBITB*IBX
      IBRIKX = ISHFT(IBX,-3)
      IBRIKY = ISHFT(IY,-6)
      IPX = IBX - ISHFT(IBRIKX,3)
      IPY = IY - ISHFT(IBRIKY,6)
      JBRIK = ISHFT(IBRIKY,5) + IBRIKX
      JBYTE = ISHFT(IPY,3) + IPX + ISHFT(JBRIK,9)
      NPLOTS(JBYTE) = (NPLOTS(JBYTE) .OR. MBIT6(IBITX))
      IF (.NOT.POINT) THEN
C
C---- Draw rest of line
C
        IF (.NOT.SLOW) THEN
C
C---- End of slow lines Fast  line of steep slope: move along y
C
          IF (UP) THEN
   10       CONTINUE
C
C---- Up x line
C
            IF (IY.LT.IEND) THEN
              IY = IY + 1
              IPY = IPY + 1
              IF (IPY.GT.NWIDY1) THEN
                IPY = 0
                JBYTE = JBYTE + NSZRXB
              ELSE
                JBYTE = JBYTE + NWIDX
              END IF
              IF (ND.LT.0) THEN
                ND = ND + NX2
              ELSE
                IX = IX + 1
                IBITX = IBITX + 1
                IF (IBITX.GT.NBITB1) THEN
                  IBITX = 0
                  IPX = IPX + 1
                  IF (IPX.GT.NWIDX1) THEN
                    JBYTE = JBYTE + NSZBX1
                    IPX = 0
                  ELSE
                    JBYTE = JBYTE + 1
                  END IF
                END IF
                ND = ND - NXY2
              END IF
              NPLOTS(JBYTE) = (NPLOTS(JBYTE) .OR. MBIT6(IBITX))
              GO TO 10
            END IF
          ELSE
C
C---- End up line
C
   20       CONTINUE
C
C---- Down x line
C
            IF (IY.LT.IEND) THEN
              IY = IY + 1
              IPY = IPY + 1
              IF (IPY.GT.NWIDY1) THEN
                IPY = 0
                JBYTE = JBYTE + NSZRXB
              ELSE
                JBYTE = JBYTE + NWIDX
              END IF
              IF (ND.LT.0) THEN
                ND = ND + NX2
              ELSE
                IX = IX - 1
                IBITX = IBITX - 1
                IF (IBITX.LT.0) THEN
                  IBITX = NBITB1
                  IPX = IPX - 1
                  IF (IPX.LT.0) THEN
                    JBYTE = JBYTE - NSZBX1
                    IPX = NWIDX1
                  ELSE
                    JBYTE = JBYTE - 1
                  END IF
                END IF
                ND = ND - NXY2
              END IF
              NPLOTS(JBYTE) = (NPLOTS(JBYTE) .OR. MBIT6(IBITX))
              GO TO 20
            END IF
C
C---- End down line
C
          END IF
C
C---- Finished steep line Slow line of small slope: move along x
C
        ELSE IF (UP) THEN
   30     CONTINUE
C
C---- Up y line
C
          IF (IX.LT.IEND) THEN
            IX = IX + 1
            IBITX = IBITX + 1
            IF (IBITX.GT.NBITB1) THEN
              IBITX = 0
              IPX = IPX + 1
              IF (IPX.GT.NWIDX1) THEN
                JBYTE = JBYTE + NSZBX1
                IPX = 0
              ELSE
                JBYTE = JBYTE + 1
              END IF
            END IF
            IF (ND.LT.0) THEN
              ND = ND + NY2
            ELSE
              IPY = IPY + 1
              IF (IPY.GT.NWIDY1) THEN
                IPY = 0
                JBYTE = JBYTE + NSZRXB
              ELSE
                JBYTE = JBYTE + NWIDX
              END IF
              ND = ND - NXY2
            END IF
            NPLOTS(JBYTE) = (NPLOTS(JBYTE) .OR. MBIT6(IBITX))
            GO TO 30
          END IF
        ELSE
C
C---- End up line
C
   40     CONTINUE
C
C---- Down y line
C
          IF (IX.LT.IEND) THEN
            IX = IX + 1
            IBITX = IBITX + 1
            IF (IBITX.GT.NBITB1) THEN
              IBITX = 0
              IPX = IPX + 1
              IF (IPX.GT.NWIDX1) THEN
                JBYTE = JBYTE + NSZBX1
                IPX = 0
              ELSE
                JBYTE = JBYTE + 1
              END IF
            END IF
            IF (ND.LT.0) THEN
              ND = ND + NY2
            ELSE
              IPY = IPY - 1
              IF (IPY.LT.0) THEN
                IPY = NWIDY1
                JBYTE = JBYTE - NSZRXB
              ELSE
                JBYTE = JBYTE - NWIDX
              END IF
              ND = ND - NXY2
            END IF
            NPLOTS(JBYTE) = (NPLOTS(JBYTE) .OR. MBIT6(IBITX))
            GO TO 40
          END IF
C
C---- End of down line
C
        END IF
C
C---- Finished all cases
C
      END IF
C
      END
C
C
C
      SUBROUTINE GSLVCK(SUBNAM)
C     ==========================
C
C---- Checks for misplaced subroutine calls
C
C     A.D. McLachlan JUL 1984. Last updated 11 JUL 1984.
C
C
C     .. Scalar Arguments ..
      CHARACTER SUBNAM* (*)
C     ..
C     .. Scalars in Common ..
      REAL DOTMMX,DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWBLMX,DWBLMY,
     +     DWLIMX,DWLIMY,PAPLMX,PAPLMY,UBXMAX,UBXMIN,UBYMAX,UBYMIN,
     +     V64LMX,V64LMY
      INTEGER ICOLOR,IDRLVL,IPRINT,IUNIT,IXMAX,IXMIN,IYMAX,IYMIN,LINWT,
     +        LUNIN,LUNOUT,MCNTFL,MDEVIC,MDIREC,MIXCOL,MOUT,MPIC,MSCAFL,
     +        NERROR,NPICS
      LOGICAL*4 DEVCON,INITON
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
C
C     .. Save Statement ..
C
      SAVE
C
      WRITE (LUNOUT,FMT=6000) SUBNAM,IDRLVL
      CALL CCPERR (2, 'Plot system not initialised?')
C
C---- Format statements
C
 6000 FORMAT (2X,'!!!GSLVCK - ',/2X,
     +        'ATTEMPT TO CALL ',A,' AT LEVEL ',I5,/2X,
     +        '(0)=NOINIT,(1)=INIT,(2)=','TRSET,(3)=DRAWING ')
C
      END
C
C
C
      SUBROUTINE GSMUCT(USRMAT,CHRMAT,CUMAT,ICULNK,IPRINT)
C     =====================================================
C
C---- Combines character and user transformations
C
C     A.D. McLachlan JULY 1984 Last updated 25 JULY 1984
C
C---- The origin shifts in the usrmat transformation are included
C     in the anchor point setting and so must not be multiplied
C     into the character transformation as well
C
C     .. Scalar Arguments ..
      INTEGER IPRINT
      LOGICAL*4 ICULNK
C     ..
C     .. Array Arguments ..
      REAL CHRMAT(3,3),CUMAT(3,3),USRMAT(3,3)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      REAL XORIG1,YORIG1
      INTEGER I,J
C     ..
C     .. External Subroutines ..
      EXTERNAL GSTMPY
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
      IF (ICULNK) THEN
C
C---- Annul and then restore user origin shift
C
        XORIG1 = USRMAT(1,3)
        YORIG1 = USRMAT(2,3)
        USRMAT(1,3) = 0.0
        USRMAT(2,3) = 0.0
        CALL GSTMPY(USRMAT,CHRMAT,CUMAT)
        USRMAT(1,3) = XORIG1
        USRMAT(2,3) = YORIG1
      ELSE
        DO 20 J = 1,3
          DO 10 I = 1,3
            CUMAT(I,J) = CHRMAT(I,J)
   10     CONTINUE
   20   CONTINUE
      END IF
      IF (IPRINT.GE.2) WRITE (LUNOUT,FMT=6000) ICULNK,CUMAT(1,1),
     +    CUMAT(1,2),CUMAT(1,3),CUMAT(2,1),CUMAT(2,2),CUMAT(2,3)
C
C---- Format statements
C
 6000 FORMAT (2X,'USER*CHARACTER TRANSFORMATION: ICULNK= ',L4,/2X,'CUM',
     +       'AT(1,1),(1,2),CUORGX = ',3F10.4,/2X,'CUMAT(2,1),(2,2),CU',
     +       'ORGY = ',3F10.4)
C
      END
C
C
C
      SUBROUTINE GSOFLW(IUNITP,GSNAM)
C     =================================
C
C---- Set of subroutines for plot84 package to do i/o
C
C     Adapted from d.a. agard plot81
C     A.D. McLachlan JUN 1984 . Last updated 4 OCT 1984
C
C---- Uses mrc random-access diskio routines. the header is reset after 
C     each plot to contain the number of actual plot records and the 
C     correct min/max values used in the plot.
C
C---- Header of 512 bytes in irec and equivalent variables
C
C---- Equivalences to convert integer*4 dot indices into 
C     integer*2 without overflow
C
C     .. Scalar Arguments ..
      INTEGER IUNITP,IX,IY,KEOF,KKEOF,KKKEOF,NBYTE
      CHARACTER GSNAM* (*)
C     ..
C     .. Array Arguments ..
      INTEGER IARRAY(1)
C     ..
C     .. Scalars in Common ..
      REAL DOTMMX,DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWBLMX,DWBLMY,
     +     DWLIMX,DWLIMY,PAPLMX,PAPLMY,UBXMAX,UBXMIN,UBYMAX,UBYMIN,
     +     V64LMX,V64LMY
      INTEGER ICOLOR,IDRLVL,IPRINT,IUNIT,IUNITR,IXMAX,IXMIN,IYMAX,IYMIN,
     +        LINWT,LUNIN,LUNOUT,MCNTFL,MDEVIC,MDIREC,MIXCOL,MOUT,MPIC,
     +        MSCAFL,NERROR,NPICS
      LOGICAL*4 DEVCON,INITON
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. Arrays in Common ..
      INTEGER IREC
C     ..
C     .. Local Scalars ..
      INTEGER I,IELM,IER,MREC,NRECL,NMCITM
      CHARACTER FILE84*8,PASWRD*8,TMPNAM*40,TITLEH*80
C     ..
C     .. Local Arrays ..
      REAL*4 AREC(128)
      INTEGER*2 JXY(2)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSLVCK,QBACK,QCLOSE,QOPEN,QREAD,QSEEK,
     +         QSKIP,QWRITE,QMODE
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
      COMMON /GSFHD/IUNITR,IREC(128)
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (IREC(1),AREC(1))
      EQUIVALENCE (TITLEH,IREC(41))
      EQUIVALENCE (PASWRD,IREC(24))
C     ..
C     .. Data statements ..
      DATA NRECL/512/
      DATA FILE84/'PLOT%%84'/
C
C---- Open plotfile to write check level
C
      IF ((IDRLVL.EQ.0) .OR. (IDRLVL.GT.2)) 
     +             CALL GSLVCK('GSOFLW')
C
C---- Precaution against case that "GSnam" is
C     in fact same as "filnam" but called from another plot routine
C
      IUNITR = IUNITP
      TMPNAM = GSNAM
      FILNAM = TMPNAM
      IF (IPRINT.GE.2) WRITE (LUNOUT,FMT=6002) TMPNAM
      CALL QOPEN(IUNITR,TMPNAM,'NEW')
C
C---- Change to byte handling
C
      CALL QMODE (IUNITR,0,NMCITM)
      IUNIT = IUNITR
      IUNITP = IUNITR
      RETURN
C
      ENTRY GSOFLR(IUNITP,GSNAM)
C     ============================
C
C---- Open plotfile to read check level
C
      IUNITR = IUNITP
      TMPNAM = GSNAM
      FILNAM = TMPNAM
      IF (IPRINT.GE.2) WRITE (LUNOUT,FMT=6002) TMPNAM
      CALL QOPEN(IUNITR,TMPNAM,'READONLY')
C
C---- Change to byte handling
C
      CALL QMODE (IUNITR,0,NMCITM)
      IUNIT = IUNITR
      IUNITP = IUNITR
      RETURN
C
      ENTRY GSCFIL(IUNITP)
C     =====================
C
C---- Close plotfile
C
      IUNITR = IUNITP
      CALL QCLOSE(IUNITR)
      IUNIT = IUNITR
      IUNITP = IUNITR
      RETURN
C
      ENTRY GSWHDR()
C     =============
C
C---- Write out header of 512 bytes check level
C
      IF (IDRLVL.NE.3) CALL GSLVCK('GSWHDR')
      IREC(1) = 0
      AREC(2) = DOTMMX
      AREC(3) = DOTMMY
      IREC(4) = IXMIN
      IREC(5) = IXMAX
      IREC(6) = IYMIN
      IREC(7) = IYMAX
      IREC(8) = LINWT
      IREC(9) = ICOLOR
      IREC(10) = MIXCOL
      IREC(11) = MDEVIC
      IREC(12) = MDIREC
      IREC(13) = MOUT
      IREC(14) = MPIC
      IREC(15) = MSCAFL
      IREC(16) = MCNTFL
      AREC(17) = DWLIMX
      AREC(18) = DWLIMY
      AREC(19) = DVXMIN
      AREC(20) = DVXMAX
      AREC(21) = DVYMIN
      AREC(22) = DVYMAX
      IREC(23) = NPICS
      TITLEH = TITLE
      PASWRD = FILE84
      IUNIT = IUNITR
C
      CALL QWRITE(IUNITR,IREC,NRECL)
      IF (IPRINT.GE.2) WRITE (LUNOUT,FMT=6000) IREC(1),AREC(2),AREC(3),
     +     (IREC(I),I=4,16), (AREC(I),I=17,22),IREC(23),TITLEH,PASWRD
      RETURN
C
      ENTRY GSFLWI(IX,IY)
C     ====================
C
C---- Write out ix,iy to intermediate plot file check level
C
      IF (IDRLVL.NE.3) CALL GSLVCK('GSFLWI')
      IREC(1) = IREC(1) + 1
      JXY(1) = IX
      JXY(2) = IY
      IF (IPRINT.GE.3) WRITE (LUNOUT,FMT=*) JXY(1),JXY(2)
      CALL QWRITE(IUNITR,JXY,4)
      RETURN
C
      ENTRY GSUHDR()
C     ================
C
C---- Update plot header check level
C
      IF (IDRLVL.NE.3) CALL GSLVCK('GSUHDR')
      IF ((IREC(1).GE.2) .OR. (JXY(1).GE.0)) THEN
        IF (IXMIN.GT.IXMAX) IXMIN = IXMAX
        IF (IYMIN.GT.IYMAX) IYMIN = IYMAX
        IREC(4) = IXMIN
        IREC(5) = IXMAX
        IREC(6) = IYMIN
        IREC(7) = IYMAX
        CALL QBACK(IUNITR, (4*IREC(1)+NRECL))
        IF (IPRINT.GE.2) WRITE (LUNOUT,FMT=6000) IREC(1),AREC(2),
     +      AREC(3), (IREC(I),I=4,16), (AREC(I),I=17,22),IREC(23),
     +      TITLEH,PASWRD
        CALL QWRITE(IUNITR,IREC,NRECL)
        CALL QSKIP(IUNITR,IREC(1)*4)
      END IF
      RETURN
C
      ENTRY GSRHDR(KEOF)
C     ===================
C
C---- Read in header and init for buffered read
C     return to statement * on end-of-file or picture without any records
C
C     (note that in the direct-access file the eof mark may
C     not match the end of the last record, so the plot file
C     is terminated by an empty header.
C
      KEOF = 0
      IER = 0
      CALL QREAD(IUNITR,IREC,NRECL,IER)
C
C---- Check for empty or end of file
C
      IF ((IER.EQ.0) .AND. (IREC(1).GE.2)) THEN
C
C---- Check for plot84 password
C
        IF (INDEX(PASWRD,FILE84).NE.1) THEN
          WRITE (LUNOUT,FMT=6004) PASWRD
        ELSE
          RETURN
        END IF
      END IF
      KEOF = 1
      RETURN
C
      ENTRY GSFLRI(IX,IY,KKEOF)
C     ==========================
C
C---- Read in ix,iy  return to statement * on  eof
C
      KKEOF = 0
      IER = 0
      CALL QREAD(IUNITR,JXY,4,IER)
      IF (IER.NE.0) THEN
        KKEOF = 1
      ELSE
        IX = JXY(1)
        IY = JXY(2)
      END IF
      RETURN
C
      ENTRY GSFLP1()
C     =============
C
C---- Reposition file at first record of picture 1
C
      MREC = 1
      IELM = 1
      CALL QSEEK(IUNITR,MREC,IELM,NRECL)
      RETURN
C
      ENTRY GSFLBR(NBYTE)
C     ====================
C
C---- Backspace nbyte bytes
C
      CALL QBACK(IUNITR,NBYTE)
      RETURN
C
      ENTRY GSFLSR(NBYTE)
C     ====================
C
C---- Skip forward nbyte bytes
C
      CALL QSKIP(IUNITR,NBYTE)
      RETURN
C
      ENTRY GSFLWR(IARRAY,NBYTE)
C     ===========================
C
C---- Write an integer array of nbyte bytes
C
      CALL QWRITE(IUNITR,IARRAY,NBYTE)
      RETURN
C
      ENTRY GSFLRR(IARRAY,NBYTE,KKKEOF)
C     ==================================
C
C---- Read an integer array of nbyte bytes
C
      KKKEOF = 0
      IER = 0
      CALL QREAD(IUNITR,IARRAY,NBYTE,IER)
      IF (IER.NE.0) KKKEOF = 1
C
C---- Format statements
C
 6000 FORMAT (2X,'GSW/UHDR - HEADER: ',/2X,'NREC DOTMMX DOTMMY ',
     +        I5,2F10.4,/2X,'IXMIN  IXMAX  IYMIN  IYMAX ',4I6,/2X,
     +        'LINWT  ','ICOLOR MIXCOL MDEVIC MDIREC ',5I5,/2X,
     +        'MOUT   MPIC   MSC','AFL MCNTFL ',4I5,/2X,
     +        'DWLIMX DWLIMY ',2F10.4,/2X,'DVXMIN',
     +       ' DVXMAX DVYMIN DVYMAX ',4F10.4,/2X,'NPICS ',I5,' TITLE: ',
     +       /1X,A,/2X,'PASWRD:',A)
 6002 FORMAT (2X,'GSOFLW -PLOTFILE OPENED: NAME= ',A)
 6004 FORMAT (2X,'GSRHDR -NOT A PLOT84 FILE: PASWRD= ',A)
C
      END
C
C
C
      SUBROUTINE GSPICT
C     ===================
C
C     A.D. McLachlan JUN 1984. Last updated 2 OCT 1986
C
C---- Routine to start a new picture on the plotter
C
C     .. Scalar Arguments ..
      INTEGER NSEC
C     ..
C     .. Scalars in Common ..
      REAL DOTMMX,DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWBLMX,DWBLMY,
     +     DWLIMX,DWLIMY,PAPLMX,PAPLMY,UBXMAX,UBXMIN,UBYMAX,UBYMIN,
     +     V64LMX,V64LMY
      INTEGER ICOLOR,IDRLVL,IPRINT,IUNIT,IUNITR,IXMAX,IXMIN,IYMAX,IYMIN,
     +        LINWT,LUNIN,LUNOUT,MCNTFL,MDEVIC,MDIREC,MIXCOL,MOUT,MPIC,
     +        MSCAFL,NERROR,NPICS
      LOGICAL*4 DEVCON,INITON
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. Arrays in Common ..
      INTEGER IREC
C     ..
C     .. Local Scalars ..
      INTEGER IDOT,IEND,IERAS,ILWT,IPAP,IPEN,ISTOP,IX,IY,MILSEC,NOUT
      CHARACTER CHKEY*1,PASWRD*8,GSNAM*40,TITLEH*80
C     ..
C     .. Local Arrays ..
      REAL*4 AREC(128)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSBLTM,GSCYTM,GSDVOF,GSDVON,
     +         GSENVR,GSFLWI,GSGRTM,GSINTM,
     +         GSLVCK,GSMYTM,GSSCTM,GSSTYL,
     +         GSSUSP,GSTYTM,GSUHDR,GSUTRN,
     +         GSWHDR
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
      COMMON /GSFHD/IUNITR,IREC(128)
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (IREC(1),AREC(1))
      EQUIVALENCE (TITLEH,IREC(41))
      EQUIVALENCE (PASWRD,IREC(24))
C     ..
C     .. Data statements ..
C
C---- Command codes plotfile (only iend used here)
C
      DATA CHKEY/' '/
      DATA IEND/-1/,IDOT/-2/,ILWT/-3/,IPEN/-4/,IPAP/-5/,IERAS/-6/
C
C---- Begin the next picture check level
C
      IF (IDRLVL.LT.1 .OR. IDRLVL.EQ.3) CALL GSLVCK('GSPICT')
C
C---- Set the environment
C
      CALL GSENVR
C
C---- Clear user transformation
C
      CALL GSUTRN
C
C---- Set style defaults for colour etc
C
      CALL GSSTYL
      IF (IPRINT.GE.2) WRITE (LUNOUT,FMT=6012) MDEVIC,MDIREC
C
C---- Switch on device
C
      IF (.NOT.DEVCON) THEN
        NOUT = MOUT
        GSNAM = FILNAM
        CALL GSDVON(GSNAM,NOUT)
      END IF
C
C---- Set level as "drawing on"
C
      IDRLVL = 3
C
      MPIC = MPIC + 1
      NPICS = MPIC
C
C---- Reset Limits to Nonsense Values
C
      IXMAX = 0
      IXMIN = 32767
      IYMAX = 0
      IYMIN = 32767
      CALL GSWHDR
C
C---- Clear out-of-bounds errors
C
      NERROR = 0
C
C---- Clear terminal screen and switch on
C
      IF (MDIREC.EQ.1 .AND. MDEVIC.EQ.3) THEN
        CALL GSCYTM
        CALL GSINTM(MOUT)
        CALL GSGRTM
      END IF
      RETURN
C
      ENTRY GSWAIT(NSEC)
C     ===================
C
C---- Pause between pictures up to 1 minute check level
C
      IF ((IDRLVL.EQ.0) .OR. (IDRLVL.EQ.3)) 
     +                  CALL GSLVCK('GSWAIT')
C
C---- Terminal
C
      IF (MDIREC.EQ.1 .AND. MDEVIC.EQ.3) THEN
        IF (NSEC.GT.60) NSEC = 60
        CALL GSTYTM
C
C---- Timed pause followed by clearance
C
        IF (NSEC.GT.0) THEN
          MILSEC = NSEC*1000
          CALL GSSUSP(MILSEC)
          CALL GSSCTM
          CALL GSTYTM
          CALL GSCYTM
          CALL GSMYTM(0,23)
        ELSE
C
C---- Ask to clear screen or not
C     terminal. write a message at bottom left and wait for character
C     then clear screen if not blank or return ring bell
C
          CALL GSBLTM
          CALL GSTYTM
          CALL GSCYTM
          WRITE (LUNOUT,FMT=6002)
          READ (LUNIN,FMT=6004) CHKEY
          IF (CHKEY.NE.' ') THEN
            CALL GSINTM(MOUT)
            CALL GSTYTM
          ELSE
            CALL GSCYTM
            CALL GSMYTM(0,23)
          END IF
        END IF
        RETURN
      ELSE IF (IPRINT.GE.2) THEN
        WRITE (LUNOUT,FMT=6000) MPIC
        RETURN
      ELSE
        RETURN
      END IF
C
      ENTRY GSENDP()
C     ================
C
C---- End current picture
C
      ISTOP = 0
      IF (IDRLVL.NE.3) CALL GSLVCK('GSENDP')
      GO TO 40
C
      ENTRY GSSTOP()
C     =============
C
C---- Terminate this series of plots
C
      ISTOP = 1
C
C---- Check level and switch off device if picture already complete
C
   40 IF (IDRLVL.EQ.3) THEN
C
C---- Interactive plot do not clear screen
C
        IF (MDIREC.EQ.1 .AND. MDEVIC.EQ.3) THEN
          CALL GSTYTM
          CALL GSCYTM
          CALL GSMYTM(0,23)
        END IF
C
C---- Reset out-of-bounds count
C
        IF ((NERROR.GT.0) .AND. (IPRINT.GE.1)) 
     +    WRITE (LUNOUT,FMT=6008) NERROR
        NERROR = 0
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6006) MPIC
C
C---- Update plotfile header
C
        IX = IEND
        IY = 1
        CALL GSFLWI(IX,IY)
        NPICS = MPIC
        CALL GSUHDR
C
        IF (ISTOP.EQ.0) THEN
          IDRLVL = 1
          RETURN
        END IF
      ENDIF
C
C---- Switch off device
C
      CALL GSDVOF
C
C---- Set level for init required
C
      IDRLVL = 0
C
C---- Format statements
C
 6000 FORMAT (2X,'PLOT: End of picture number ',I5)
 6002 FORMAT (2X,'Type any character to clear screen ',/2X,'""RETURN" ',
     +       'or "SPACE" to save ')
 6004 FORMAT (A,A)
 6006 FORMAT (2X,'END PLOT: Picture number ',I5)
 6008 FORMAT (2X,'Number of out-of-bounds plot errors: ',I8)
 6012 FORMAT (2X,'GSPICT: MDEVIC MDIREC = ',2I5)
C
      END
C
C
C
      SUBROUTINE GSPRTM(ITERM,NRWMIN,NRWMAX)
C     =======================================
C
C---- Vt640 dump to dot matrix printer.
C
C     A.D. McLachlan SEP 1984. Last updated 21 SEP 1984.
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were NBIT0, ZERO
C     and the followig BYTEs to INTEGER: BYTE1, NBYTE, BITS, SIXBIT, 
C     XBIT, SCREEN
C
C     ITERM  = fortran unit number for terminal in graphics package
C     NRWMIN = min line number for bottom of display dump
C     NRWMAX = max line number for top of display dump
C              the whole screen goes from 0 to 479 (bottom to top)
C              in physical dot units.
C
C     This program works by reading lines in groups of three.
C     The given values of nrwmin and nrwmax will therefore be
C     rounded to 3*n-1 and 3*m.
C
C     The 4010 memory coords (0-1023 in x, 0-780 in y) 
C      are NOT USED here.
C     This routine uses the physical dot display range
C           (0-639 in x, 0-479 in y).
C     The origin is always at the bottom left hand corner of the screen.
C
C     .. Scalar Arguments ..
      INTEGER ITERM,NRWMAX,NRWMIN
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      REAL SECTOT,SSTAGE
      INTEGER I,ICOLOR,IOFLAG,ITPRNT,J,K,NBIT6,NBITS,NBXOFF,NBXWID,
     +        NCMAX,NCMIN,NCWID,NROW
      INTEGER BYTE1,NBYTE
      CHARACTER*1 NBIT0,ZERO
C     ..
C     .. Local Arrays ..
      INTEGER BITS(642),SIXBIT(107),XBIT(0:5)
      INTEGER SCREEN(0:127,0:479)
C     ..
C     .. External Subroutines ..
      EXTERNAL CLSTRI,INIT3P,GSBFTM,
     +         GSGRTM,GSRDTM,GSSUSP,
     +         GSTIM0,GSTIMR,TRICOL,TRIPLC
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Data statements ..
      DATA XBIT/1,2,4,6,16,32/
      DATA BYTE1/1/
C
      ZERO = CHAR(0)
      NBIT0 = CHAR(0)
      ITPRNT = 0
      CALL GSTIM0(ITPRNT)
C
C---- Put vt640 in graphics mode and clear buffer of vt640
C
      CALL GSGRTM
D     CALL GSBFTM(ITERM,ZERO)
C
C---- Call up trilog routines
C
      IOFLAG = 1
      CALL INIT3P(2,'SCREEN.LIS',IOFLAG)
      ICOLOR = 4
      CALL TRICOL(ICOLOR)
C
C---- Read from vt640 memory
C     5-bit chars + octal 100 in each byte. loaded to the right.
C     arranged in 128 columns of 5 dots wide. cols numbered (0:127)
C     and 480 rows of one dot high numbered (0:479)
C     origin (0,0) at bottom left of screen
C
C---- Screen limits to read
C---- Set row limits to 3n-1 and 3m
C     read 3 lines at a time
C     if nlines.gt.1 then must read whole rows of 128 cols
C
      IF (NRWMAX.GT.479) NRWMAX = 479
      IF (NRWMAX.LE.0) NRWMAX = 479
      NRWMAX = ((NRWMAX+3)/3)*3 - 1
      IF (NRWMIN.GE.NRWMAX) NRWMIN = 0
      IF (NRWMIN.LT.0) NRWMIN = 0
      NRWMIN = (NRWMIN/3)*3
      CALL GSRDTM(ITERM,NRWMIN,NRWMAX,SCREEN)
C
C---- Translate row by row into trilog code
C
C---- Number of bits read is 5*number of columns i.e. 640
C     number of bits sent to trilog is multiple of 6 which
C     covers this i.e. 642 packed in 107 bytes
C
      NCWID = 128
      NCMIN = 0
      NCMAX = 128
      NBITS = 5*NCWID
      NBIT6 = (NBITS-1)/6 + 1
      NBITS = NBIT6*6
C
C---- Clear translation array
C
      DO 10 I = 1,NBITS
        BITS(I) = ICHAR(NBIT0)
   10 CONTINUE
C
C---- Start rows
C
      DO 60 NROW = NRWMAX,NRWMIN,-1
C
C---- Convert to bit stream
C
        K = 0
        DO 30 I = NCMIN,NCMAX
          NBYTE = SCREEN(I,NROW)
          K = K + 5
          DO 20 J = 0,4
            BITS(K-J) = NBYTE .AND. BYTE1
            NBYTE = NBYTE/2
   20     CONTINUE
   30   CONTINUE
C
C---- Convert to 6-bit chars+octal 100 for trilog
C
        K = 0
        DO 50 I = 1,NBIT6
          NBYTE = 64
          DO 40 J = 0,5
            K = K + 1
            IF (BITS(K).EQ.1) NBYTE = NBYTE .OR. XBIT(J)
   40     CONTINUE
          SIXBIT(I) = NBYTE
   50   CONTINUE
C
C---- Control line for trilog printer
C
        NBXWID = NBIT6
        NBXOFF = 10
        CALL TRIPLC(SIXBIT,NBXWID,NBXOFF)
   60 CONTINUE
C
C---- Finish with trilog
C
      CALL CLSTRI
      CALL GSTIMR(SECTOT,SSTAGE,ITPRNT)
      WRITE (LUNOUT,FMT=6004) SECTOT
C      CALL GSSUSP(1000)
C
C---- Format statements
C
 6004 FORMAT (2X,'GSPRTM: Trilog complete . cpu sec = ',F8.2)
C
      END
C
C
C
      SUBROUTINE GSQUIK
C     ===================
C
C---- Routine to do plotting under keyboard control using PARSER
C
C     A.D. McLachlan JUL 1984. Last updated 28 SEP 1984
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were HCHAR, HTEXT
C     and the followig BYTEs to INTEGER: IARRAY
C
C     NTERM  = 0,1  for plotting off or on terminal
C     NCMFIL = 0,1  for commands interactively or from command file
C     PRINT  = .true. .false. for printing prompts at each command
C
      INTEGER NPARM
      PARAMETER (NPARM=200)
C     
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      REAL      ANGFAC,ANGLE,AXLEN,CHANGX,CHANGY,CHORGX,CHORGY,CHRSCX,
     +          CHRSCY,CHRSPX,CHRSPY,CWID,DTMMX,DTMMY,DV,DVLMX1,DVLMX2,
     +          DVLMY1,DVLMY2,DWLIM1,DWLIM2,DWLMX1,DWLMX2,DWLMY1,DWLMY2,
     +          DX,DY,ENUM,FNUM,FVAL,HGT,PI,PLENG,SCAFAC,SCALEX,SCALEY,
     +          SIZ1,SIZ2,SIZX,SIZY,USANGX,USANGY,VBXMAX,VBXMIN,VBYMAX,
     +          VBYMIN,X,XANCH,XBOLD,XCGAP,XCOFF,XCSTRT,XLEFT,XOLD,
     +          XORIG,XRIGHT,Y,YANCH,YBOLD,YCDOWN,YCOFF,YCSTRT,YHIGH,
     +          YLOW,YOLD,YORIG
      INTEGER   I,ICENTC,ICLINK,ICMODE,ICOLR,IERR,INUM,IPRINT,ISCALE,
     +          ISYMB,IUNITP,IUSPCE,IX,IY,J,JJJ,KEOF,KFONT,KKEOF,KKKEOF,
     +          LETTER,LINSIZ,LWT,MCOLOR,NAFTER,NBYTE,NC,NCHAR,NCHARS,
     +          NCMFIL,NCOUNT,NDEVIC,NDIG,NDIGIT,NDIREC,NFONT,NGROUP,
     +          NIND,NINTER,NJUST,NLENG,NLETT,NMEMB,NOCENT,NOPRNT,
     +          NOUT,NSCAFL,NCNTFL,NSEC,NSET,NSTART,NSYMB,NTERM
      LOGICAL   PRINT
      CHARACTER HCHAR,CHAR1,BLANK75*75,BLANK80*80,FILNAM*80,LISFIL*80,
     +          GSFIL*80,TEXT*80,TITLE*80
C     ..
C     .. Local Arrays ..
      REAL      P(2),Q(2),TMAT(2,2)
      INTEGER*4 ITEXT4(80),NTRSAV(48),IARRAY(128)
      CHARACTER HTEXT(80),KEY5(NPARM)*5
      INTEGER*2 ITEXT2(80)
C
C     .. Parser Stuff ..
      INTEGER   NTOK,IBEG(NPARM),IDEC(NPARM),IEND(NPARM),ITYPE(NPARM)
      LOGICAL   LEND,IECHO
      CHARACTER KEY*4,LINE*80,CVALUE(NPARM)*4
      REAL      FVALUE(NPARM)
C     ..
C     .. External Functions ..
      INTEGER  LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL AXIS,CSTRNG,NUMBER,PARSER,SYMBOL,STRING,
     +GSANCD,GSANCU,GSBSIZ,GSCENC,GSCETS,GSCETX,GSCFIL,
     +GSCFSZ,GSCMAT,GSCMOD,GSCOLR,GSCROT,GSCSPA,GSCSPU,
     +GSDDRB,GSDOTB,GSDTRN,GSDVIC,GSDVOF,GSDVON,GSDVPT,
     +GSDWBY,GSDWTO,GSEBSZ,GSEDTR,GSEDVC,GSEDVP,GSENDP,
     +GSENUM,GSENVR,GSERAS,GSERAX,GSEREV,GSEWND,GSEWSC,
     +GSFCUR,GSFLBR,GSFLP1,GSFLRI,GSFLRR,GSFLSR,GSFLWI,
     +GSFLWR,GSFNUM,GSFONT,GSGCH0,GSGCHC,GSGCHF
      EXTERNAL GSGCHH,GSGCHI,GSGCHS,GSGSY0,GSGSYC,GSGSYM,
     +GSGSYS,GSHAIR,GSINIT,GSINUM,GSLINE,GSLNFD,GSLNWT,
     +GSLVCK,GSMDRB,GSMIXC,GSMVBY,GSMVTO,GSOFLR,GSOFLW,
     +GSORGC,GSORGD,GSORGU,GSPAPR,GSPCUR,GSPICT,GSPOIN,
     +GSPRNT,GSRFNT,GSRHDR,GSSCLC,GSSCLU,GSSCUR,GSSTOP,
     +GSSTR2,GSSTR4,GSSTRC,GSSTRD,GSSTRH,GSSTRS,GSSTRU,
     +GSSTYL,GSTITL,GSTLNK,GSTRES,GSTRIL,GSTSAV,GSTYOF,
     +GSTYON,GSUHDR,GSUMAT,GSUROT,GSUTRN,GSVIEW,GSWAIT,
     +GSWHDR,GSWNDB,GSWSCL,GSXENV
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ATAN2,CHAR,INDEX,MOD,NINT
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C     ..
C     .. Data statements ..
      DATA (KEY5(JJJ),JJJ=1,20)/'INIT','TITL','PRNT','ENVR','XENV',
     +     'DVIC','BSIZ','DVPT','WNDB','WSCL','DTRN','EDVC','EBSZ',
     +     'EDVP','EWND','EWSC','EDTR','RFNT','LVCK','PICT'/
      DATA (KEY5(JJJ),JJJ=21,40)/'WAIT','ENDP','STOP','DVON','DVOF',
     +     'OFLW','OFLR','CFIL','FLWI','FLRI','WHDR','RHDR','UHDR',
     +     'FLP1','FLBR','FLSR','FLWR','FLRR','UTRN','SCLU'/
      DATA (KEY5(JJJ),JJJ=41,60)/'UROT','UMAT','ORGD','ORGU','SCLC',
     +     'CROT','CMAT','ORGC','MUCT','TLNK','CMOD','CSPA','ANCD',
     +     'ANCU','FCUR','PCUR','LNFD','CENC','STYL','LNWT'/
      DATA (KEY5(JJJ),JJJ=61,80)/'COLR','PAPR','FONT','ERAS','ERAX',
     +     'GCH0','GSY0','GCHF','GCHC','GCHI','GCHH','GSYM','INUM',
     +     'FNUM','ENUM','STRC','STRH','STR2','STR4','STRD'/
      DATA (KEY5(JJJ),JJJ=81,100)/'STRU','CFSZ','CLPL','CLPT','CLTS',
     +     'SWLN','DWTO','DWBY','MVTO','MVBY','POIN','DDRB','MDRB',
     +     'DOTB','LRAS','LRSB','TEXT','VIEW','TRIL','QUIT'/
      DATA (KEY5(JJJ),JJJ=101,120)/'VCLN','CETX','SCUR','GSYS','GSYC',
     +     'PROM','CETS','GCHS','STRS','MIXC','NUMB','SYMB','AXIS',
     +     'STNG','CSTN','NOPR','CSPU','TSAV','TRES','EREV'/
      DATA (KEY5(JJJ),JJJ=121,125)/'HAIR','CMFL','LINE','TERM','HELP'/
      DATA BLANK80/' '/,BLANK75/' '/
      DATA IARRAY/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,112*0/
      DATA IECHO/.FALSE./
C
      PI = ATAN2(1.0,1.0)*4.0
      ANGFAC = PI/180.0
C
      NTERM = 0
      NCMFIL = 0
      PRINT = .TRUE.
      NCOUNT = 0
   10 IF (PRINT) CALL NOCRLF ('+>>>>$')
      NTOK=NPARM
      LINE = ' '
      CALL PARSER (KEY,LINE,IBEG,IEND,ITYPE,FVALUE,CVALUE,IDEC,NTOK,
     +             LEND,IECHO)
      NCOUNT = NCOUNT + 1
C
C---- Look for each Keyword
C
      IF (KEY.EQ.'INIT') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.1) THEN 
          FILNAM = CVALUE(2)
        ELSE
          IF (PRINT) CALL NOCRLF ('+FILNAM (.PLT ASSUMED)=$')
          READ (LUNIN,FMT=6306) FILNAM
        ENDIF
C
C---- Remove trailing blanks
C
        NLENG = LENSTR(FILNAM)
C
C---- Construct .PLT file name
C
        IF (NLENG.LT.1) THEN
          FILNAM = 'QUICK.PLT'
        ELSE
          FILNAM = FILNAM(1:NLENG)//'.PLT'
        END IF
        IF (.NOT.PRINT) WRITE (LUNOUT,FMT=6002) FILNAM
        CALL GSINIT(FILNAM)
        IF (NTERM.EQ.1) CALL GSDVIC(1,3)
C
      ELSE IF (KEY.EQ.'TITL') THEN
        IF (NTOK.GT.1) THEN
          TITLE = LINE(IBEG(2):80)
        ELSE
          IF (PRINT) CALL NOCRLF ('+TITLE=$')
          READ (LUNIN,FMT=6306) TITLE
        ENDIF
        IF (INDEX(TITLE,BLANK80).EQ.1) TITLE = 'QUICK'
        CALL GSTITL(TITLE)
C
      ELSE IF (KEY.EQ.'PRNT') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.2) THEN 
          IPRINT = NINT(FVALUE(2))
        ELSE
          IF (PRINT) CALL NOCRLF ('+IPRINT =$')
          READ (LUNIN,FMT=6308) IPRINT
        ENDIF
        CALL GSPRNT(IPRINT)
C
      ELSE IF (KEY.EQ.'ENVR') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6008)
        CALL GSENVR
C
      ELSE IF (KEY.EQ.'XENV') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6010)
        CALL GSXENV
C
      ELSE IF (KEY.EQ.'DVIC') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN 
          NDIREC = NINT(FVALUE(2))
          NDEVIC = NINT(FVALUE(3))
        ELSE
          IF (PRINT) CALL NOCRLF ('+NDIREC NDEVIC =$')
          READ (LUNIN,FMT=*) NDIREC,NDEVIC
        ENDIF
        CALL GSDVIC(NDIREC,NDEVIC)
        IF (NDIREC.EQ.1) THEN
          NTERM = 1
        ELSE
          NTERM = 0
        END IF
C
      ELSE IF (KEY.EQ.'BSIZ') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN 
          DWLIM1 = FVALUE(2)
          DWLIM2 = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+DWLIMX,DWLIMY =$')
          READ (LUNIN,FMT=*) DWLIM1,DWLIM2
        ENDIF
        CALL GSBSIZ(DWLIM1,DWLIM2)
C
      ELSE IF (KEY.EQ.'DVPT') THEN
        IF (NTOK.GT.4 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2
     +      .AND. ITYPE(4).EQ.2 .AND. ITYPE(5).EQ.2) THEN 
          DVLMX1 = FVALUE(2)
          DVLMX2 = FVALUE(3)
          DVLMY1 = FVALUE(4)
          DVLMY2 = FVALUE(5)
        ELSE
          IF (PRINT) CALL NOCRLF ('+DVXMIN DVXMAX DVYMIN DVYMAX =$')
          READ (LUNIN,FMT=*) DVLMX1,DVLMX2,DVLMY1,DVLMY2
        ENDIF
        CALL GSDVPT(DVLMX1,DVLMX2,DVLMY1,DVLMY2)
C
      ELSE IF (KEY.EQ.'WNDB') THEN
        IF (NTOK.GT.4 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2
     +      .AND. ITYPE(4).EQ.2 .AND. ITYPE(5).EQ.2) THEN 
          VBXMIN = FVALUE(2)
          VBXMAX = FVALUE(3)
          VBYMIN = FVALUE(4)
          VBYMAX = FVALUE(5)
        ELSE
          IF (PRINT) CALL NOCRLF ('+VBXMIN VBXMAX VBYMIN VBYMAX =$')
          READ (LUNIN,FMT=*) VBXMIN,VBXMAX,VBYMIN,VBYMAX
        ENDIF
        CALL GSWNDB(VBXMIN,VBXMAX,VBYMIN,VBYMAX)
C
      ELSE IF (KEY.EQ.'WSCL') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          NSCAFL = NINT(FVALUE(2))
          NCNTFL = NINT(FVALUE(3))
        ELSE
          IF (PRINT) CALL NOCRLF ('+NSCAFL,NCNTFL =$')
          READ (LUNIN,FMT=*) NSCAFL,NCNTFL
        ENDIF
        CALL GSWSCL(NSCAFL,NCNTFL)
C
      ELSE IF (KEY.EQ.'DTRN') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          DTMMX = FVALUE(2)
          DTMMY = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+DOTMMX DOTMMY =$')
          READ (LUNIN,FMT=*) DTMMX,DTMMY
        ENDIF
        CALL GSDTRN(DTMMX,DTMMY)
C
      ELSE IF (KEY.EQ.'EDVC') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          NDIREC = NINT(FVALUE(2))
          NDEVIC = NINT(FVALUE(3))
        ELSE
          IF (PRINT) CALL NOCRLF ('+NDIREC NDEVIC =$')
          READ (LUNIN,FMT=*) NDIREC,NDEVIC
        ENDIF
        CALL GSEDVC(NDIREC,NDEVIC,DWLIM1,DWLIM2,DWLMX1,DWLMX2,
     +                DWLMY1,DWLMY2)
        IF (PRINT) WRITE (LUNOUT,FMT=6026) DWLIM1,DWLIM2
        IF (PRINT) WRITE (LUNOUT,FMT=6028) DWLMX1,DWLMX2,DWLMY1,DWLMY2
C
      ELSE IF (KEY.EQ.'EBSZ') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          DWLIM1 = FVALUE(2)
          DWLIM2 = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+DWLIMX DWLIMY =$')
          READ (LUNIN,FMT=*) DWLIM1,DWLIM2
        ENDIF
        CALL GSEBSZ(DWLIM1,DWLIM2)
C
      ELSE IF (KEY.EQ.'EDVP') THEN
        IF (NTOK.GT.4 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2
     +      .AND. ITYPE(4).EQ.2 .AND. ITYPE(5).EQ.2) THEN 
          DVLMX1 = FVALUE(2)
          DVLMX2 = FVALUE(3)
          DVLMY1 = FVALUE(4)
          DVLMY2 = FVALUE(5)
        ELSE
          IF (PRINT) CALL NOCRLF ('+DVXMIN DVXMAX DVYMIN DVYMAX =$')
          READ (LUNIN,FMT=*) DVLMX1,DVLMX2,DVLMY1,DVLMY2
        ENDIF
        CALL GSEDVP(DVLMX1,DVLMX2,DVLMY1,DVLMY2,VBXMIN,VBXMAX,
     +                VBYMIN,VBYMAX)
        IF (PRINT) WRITE (LUNOUT,FMT=6034) VBXMIN,VBXMAX,VBYMIN,VBYMAX
C
      ELSE IF (KEY.EQ.'EWND') THEN
        IF (NTOK.GT.4 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2
     +      .AND. ITYPE(4).EQ.2 .AND. ITYPE(5).EQ.2) THEN 
          VBXMIN = FVALUE(2)
          VBXMAX = FVALUE(3)
          VBYMIN = FVALUE(4)
          VBYMAX = FVALUE(5)
        ELSE
          IF (PRINT) CALL NOCRLF ('+VBXMIN VBXMAX VBYMIN VBYMAX =$')
          READ (LUNIN,FMT=*) VBXMIN,VBXMAX,VBYMIN,VBYMAX
        ENDIF
        CALL GSEWND(VBXMIN,VBXMAX,VBYMIN,VBYMAX)
C
      ELSE IF (KEY.EQ.'EWSC') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          NSCAFL = NINT(FVALUE(2))
          NCNTFL = NINT(FVALUE(3))
        ELSE
          IF (PRINT) CALL NOCRLF ('+NSCAFL NCNTFL =$')
          READ (LUNIN,FMT=*) NSCAFL,NCNTFL
        ENDIF
        CALL GSEWSC(NSCAFL,NCNTFL)
C
      ELSE IF (KEY.EQ.'EDTR') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          DTMMX = FVALUE(2)
          DTMMY = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+DOTMMX DOTMMY =$')
          READ (LUNIN,FMT=*) DTMMX,DTMMY
        ENDIF
        CALL GSEDTR(DTMMX,DTMMY)
C
      ELSE IF (KEY.EQ.'RFNT') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6042)
        CALL GSRFNT
C
      ELSE IF (KEY.EQ.'LVCK') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6044)
        CALL GSLVCK('DUMMY')
C
      ELSE IF (KEY.EQ.'PICT') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6046)
        CALL GSPICT
C
C---- interactive plots use GSTYON to exit vector mode and get instructions
C
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'WAIT') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.2) THEN
          NSEC = NINT(FVALUE(2))
        ELSE
          IF (PRINT) CALL NOCRLF ('+NSEC =$')
          READ (LUNIN,FMT=*) NSEC
        ENDIF
        CALL GSWAIT(NSEC)
C
      ELSE IF (KEY.EQ.'ENDP') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6050)
        CALL GSTYON
        CALL GSENDP
C
      ELSE IF (KEY.EQ.'STOP') THEN
        CALL GSTYON
        CALL GSSTOP
C
      ELSE IF (KEY.EQ.'DVON') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.1) THEN
          FILNAM = CVALUE(2)
        ELSE
          IF (PRINT) CALL NOCRLF ('+FILNAM =$')
          READ (LUNIN,FMT=6306) FILNAM
        ENDIF
        IF (INDEX(FILNAM,BLANK80).EQ.1) FILNAM = 'QUICK.PLT'
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2) THEN
          NOUT = NINT(FVALUE(2))
        ELSE
          IF (PRINT) CALL NOCRLF ('+NOUT =$')
          READ (LUNIN,FMT=6308) NOUT
        ENDIF
        CALL GSDVON(FILNAM,NOUT)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'DVOF') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6058)
        CALL GSDVOF
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'OFLW') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.1) THEN
          FILNAM = CVALUE(2)
        ELSE
          IF (PRINT) CALL NOCRLF ('+FILNAM =$')
          READ (LUNIN,FMT=6306) FILNAM
        ENDIF
        IF (INDEX(FILNAM,BLANK80).EQ.1) FILNAM = 'QUICK.PLT'
        CALL GSOFLW(IUNITP,FILNAM)
        IF (PRINT) WRITE (LUNOUT,FMT=6062) IUNITP
C
      ELSE IF (KEY.EQ.'OFLR') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.1) THEN
          FILNAM = CVALUE(2)
        ELSE
          IF (PRINT) CALL NOCRLF ('+FILNAM =$')
          READ (LUNIN,FMT=6306) FILNAM
        ENDIF
        IF (INDEX(FILNAM,BLANK80).EQ.1) FILNAM = 'QUICK.PLT'
        CALL GSOFLR(IUNITP,FILNAM)
        IF (PRINT) WRITE (LUNOUT,FMT=6066) IUNITP
C
      ELSE IF (KEY.EQ.'CFIL') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.2) THEN
          IUINTP = NINT(FVALUE(2))
        ELSE
          IF (PRINT) CALL NOCRLF ('+IUNITP =$')
          READ (LUNIN,FMT=6308) IUNITP
        ENDIF
        CALL GSCFIL(IUNITP)
C
      ELSE IF (KEY.EQ.'FLWI') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          IX = NINT(FVALUE(2))
          IY = NINT(FVALUE(3))
        ELSE
          IF (PRINT) CALL NOCRLF ('+IX IY =$')
          READ (LUNIN,FMT=*) IX,IY
        ENDIF
        CALL GSFLWI(IX,IY)
C
      ELSE IF (KEY.EQ.'FLRI') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          IX = NINT(FVALUE(2))
          IY = NINT(FVALUE(3))
        ELSE
          IF (PRINT) CALL NOCRLF ('+IX IY =$')
          READ (LUNIN,FMT=*) IX,IY
        ENDIF
        KKEOF = 0
        CALL GSFLRI(IX,IY,KKEOF)
        IF (KKEOF.EQ.1) THEN
          IF (PRINT) WRITE (LUNOUT,FMT=6074)
        END IF
C
      ELSE IF (KEY.EQ.'WHDR') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6076)
        CALL GSWHDR
C
      ELSE IF (KEY.EQ.'RHDR') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6080)
        KEOF = 0
        CALL GSRHDR(KEOF)
        IF (KEOF.EQ.1) THEN
          IF (PRINT) WRITE (LUNOUT,FMT=6078)
        END IF
C
      ELSE IF (KEY.EQ.'UHDR') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6082)
        CALL GSUHDR
C
      ELSE IF (KEY.EQ.'FLP1') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6084)
        CALL GSFLP1
C
      ELSE IF (KEY.EQ.'FLBR') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.2) THEN
          NBYTE = NINT(FVALUE(2))
        ELSE
          IF (PRINT) CALL NOCRLF ('+NBYTE =$')
          READ (LUNIN,FMT=6308) NBYTE
        ENDIF
        CALL GSFLBR(NBYTE)
C
      ELSE IF (KEY.EQ.'FLSR') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.2) THEN
          NBYTE = NINT(FVALUE(2))
        ELSE
          IF(PRINT)CALL NOCRLF('+NBYTE =$')
          READ (LUNIN,FMT=6308) NBYTE
        ENDIF
        CALL GSFLSR(NBYTE)
C
      ELSE IF (KEY.EQ.'FLWR') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.2) THEN
          NBYTE = NINT(FVALUE(2))
        ELSE
         IF(PRINT)CALL NOCRLF('+ARRAY VALUES WRITTEN: NBYTE(128 MAX)=$')
          READ (LUNIN,FMT=6308) NBYTE
        ENDIF
        CALL GSFLWR(IARRAY,NBYTE)
C
      ELSE IF (KEY.EQ.'FLRR') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.2) THEN
          NBYTE = NINT(FVALUE(2))
        ELSE
          IF(PRINT)CALL NOCRLF('+ARRAY VALUES READ: NBYTE(128 MAX)=$')
          READ (LUNIN,FMT=6308) NBYTE
        ENDIF
        KKKEOF = 0
        CALL GSFLRR(IARRAY,NBYTE,KKKEOF)
        IF (KKKEOF.EQ.1) THEN
          IF (PRINT) WRITE (LUNOUT,FMT=6094)
        END IF
C
      ELSE IF (KEY.EQ.'UTRN') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6096)
        CALL GSUTRN
C
      ELSE IF (KEY.EQ.'SCLU') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          SCALEX = FVALUE(2)
          SCALEX = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+SCALEX SCALEY =$')
          READ (LUNIN,FMT=*) SCALEX,SCALEY
        ENDIF
        CALL GSSCLU(SCALEX,SCALEY)
C
      ELSE IF (KEY.EQ.'UROT') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          USANGX = FVALUE(2)
          USANGY = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+USANGX,USANGY =$')
          READ (LUNIN,FMT=*) USANGX,USANGY
        ENDIF
        CALL GSUROT(USANGX*ANGFAC,USANGY*ANGFAC)
C
      ELSE IF (KEY.EQ.'UMAT') THEN
        IF (NTOK.GT.4 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2
     +      .AND. ITYPE(4).EQ.2 .AND. ITYPE(5).EQ.2) THEN
          TMAT(1,1) = FVALUE(2)
          TMAT(1,2) = FVALUE(3)
          TMAT(2,1) = FVALUE(4)
          TMAT(2,2) = FVALUE(5)
        ELSE
          IF (PRINT) CALL NOCRLF ('+UMAT(1,1) (1,2) (2,1) (2,2) =$')
          READ (LUNIN,FMT=*) TMAT(1,1),TMAT(1,2),TMAT(2,1),TMAT(2,2)
        ENDIF
        CALL GSUMAT(TMAT)
C
      ELSE IF (KEY.EQ.'ORGD') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          XORIG = FVALUE(2)
          YORIG = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+XORIGD YORIGD =$')
          READ (LUNIN,FMT=*) XORIG,YORIG
        ENDIF
        CALL GSORGD(XORIG,YORIG)
C
      ELSE IF (KEY.EQ.'ORGU') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          XORIG = FVALUE(2)
          YORIG = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+XORIGU YORIGU =$')
          READ (LUNIN,FMT=*) XORIG,YORIG
        ENDIF
        CALL GSORGU(XORIG,YORIG)
C
      ELSE IF (KEY.EQ.'SCLC') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          CHRSCX = FVALUE(2)
          CHRSCY = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+CHRSCX CHRSCY =$')
          READ (LUNIN,FMT=*) CHRSCX,CHRSCY
        ENDIF
        CALL GSSCLC(CHRSCX,CHRSCY)
C
      ELSE IF (KEY.EQ.'CROT') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          CHANGX = FVALUE(2)
          CHANGY = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+CHANGX CHANGY =$')
          READ (LUNIN,FMT=*) CHANGX,CHANGY
        ENDIF
        CALL GSCROT(CHANGX*ANGFAC,CHANGY*ANGFAC)
C
      ELSE IF (KEY.EQ.'CMAT') THEN
        IF (NTOK.GT.4 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2
     +      .AND. ITYPE(4).EQ.2 .AND. ITYPE(5).EQ.2) THEN
          TMAT(1,1) = FVALUE(2)
          TMAT(1,2) = FVALUE(3)
          TMAT(2,1) = FVALUE(4)
          TMAT(2,2) = FVALUE(5)
        ELSE
          IF (PRINT) CALL NOCRLF ('+CMAT(1,1) (1,2) (2,1) (2,2) =$')
          READ (LUNIN,FMT=*) TMAT(1,1),TMAT(1,2),TMAT(2,1),TMAT(2,2)
        ENDIF
        CALL GSCMAT(TMAT)
C
      ELSE IF (KEY.EQ.'ORGC') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          CHORGX = FVALUE(2)
          CHORGY = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+CHORGX CHORGY =$')
          READ (LUNIN,FMT=*) CHORGX,CHORGY
        ENDIF
        CALL GSORGC(CHORGX,CHORGY)
C
      ELSE IF (KEY.EQ.'MUCT') THEN
C
      ELSE IF (KEY.EQ.'TLNK') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.2) THEN
          ICLINK = NINT(FVALUE(2))
        ELSE
          IF (PRINT) CALL NOCRLF ('+ICLINK (0)(1) =$')
          READ (LUNIN,FMT=6308) ICLINK
        ENDIF
        CALL GSTLNK(ICLINK)
C
      ELSE IF (KEY.EQ.'CMOD') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.2) THEN
          ICMODE = NINT(FVALUE(2))
        ELSE
          IF (PRINT) CALL NOCRLF ('+ICMODE (0)(1) =$')
          READ (LUNIN,FMT=*) ICMODE
        ENDIF
        CALL GSCMOD(ICMODE)
C
      ELSE IF (KEY.EQ.'CPSA') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          CHRSPX = FVALUE(2)
          CHRSPY = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+CHRSPX CHRSPY =$')
          READ (LUNIN,FMT=*) CHRSPX,CHRSPY
        ENDIF
        CALL GSCSPA(CHRSPX,CHRSPY)
C
      ELSE IF (KEY.EQ.'ANCD') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          XANCH = FVALUE(2)
          YANCH = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+XANCHD YANCHD =$')
          READ (LUNIN,FMT=*) XANCH,YANCH
        ENDIF
        CALL GSANCD(XANCH,YANCH)
C
      ELSE IF (KEY.EQ.'ANCU') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          XANCH = FVALUE(2)
          YANCH = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+XANCHU YANCHU =$')
          READ (LUNIN,FMT=*) XANCH,YANCH
        ENDIF
        CALL GSANCU(XANCH,YANCH)
C
      ELSE IF (KEY.EQ.'FCUR') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          XCSTRT = FVALUE(2)
          YCSTRT = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+XCSTRT,YCSTRT =$')
          READ (LUNIN,FMT=*) XCSTRT,YCSTRT
        ENDIF
        CALL GSFCUR(XCSTRT,YCSTRT)
        IF (PRINT) WRITE (LUNOUT,FMT=6128) XCSTRT,YCSTRT
C
      ELSE IF (KEY.EQ.'PCUR') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          XCSTRT = FVALUE(2)
          YCSTRT = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+XCSTRT,YCSTRT =$')
          READ (LUNIN,FMT=*) XCSTRT,YCSTRT
        ENDIF
        CALL GSPCUR(XCSTRT,YCSTRT)
C
      ELSE IF (KEY.EQ.'LNFD') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.2) THEN
          YCDOWN = FVALUE(2)
        ELSE
          IF (PRINT) CALL NOCRLF ('+YCDOWN =$')
          READ (LUNIN,FMT=*) YCDOWN
        ENDIF
        CALL GSLNFD(YCDOWN)
C
      ELSE IF (KEY.EQ.'CENC') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.2) THEN
          ICENTC = NINT(FVALUE(2))
        ELSE
          IF (PRINT) CALL NOCRLF ('+ICENTC (0)(1)=$')
          READ (LUNIN,FMT=6308) ICENTC
        ENDIF
        CALL GSCENC(ICENTC)
C
      ELSE IF (KEY.EQ.'STYL') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6136)
        CALL GSSTYL
C
      ELSE IF (KEY.EQ.'LNWT') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.2) THEN
          LWT = NINT(FVALUE(2))
        ELSE
          IF (PRINT) CALL NOCRLF ('+LWT(1-9) =$')
          READ (LUNIN,FMT=6308) LWT
        ENDIF
        CALL GSLNWT(LWT)
C
      ELSE IF (KEY.EQ.'COLR') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.2) THEN
          ICOLR = NINT(FVALUE(2))
        ELSE
          IF (PRINT) CALL NOCRLF ('+ICOLR =$')
          READ (LUNIN,FMT=6308) ICOLR
        ENDIF
        CALL GSCOLR(ICOLR)
C
      ELSE IF (KEY.EQ.'PAPR') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.2) THEN
          PLENG = FVALUE(2)
        ELSE
          IF (PRINT) CALL NOCRLF ('+PLENG(MM)=$')
          READ (LUNIN,FMT=*) PLENG
        ENDIF
        CALL GSPAPR(PLENG)
C
      ELSE IF (KEY.EQ.'FONT') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.2) THEN
          NFONT = NINT(FVALUE(2))
        ELSE
          IF (PRINT) CALL NOCRLF ('+NFONT (0-4) =$')
          READ (LUNIN,FMT=6308) NFONT
        ENDIF
        CALL GSFONT(NFONT)
C
      ELSE IF (KEY.EQ.'ERAS') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6148)
        CALL GSTYOF
        CALL GSERAS
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'ERAX') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6150)
        CALL GSTYOF
        CALL GSERAX
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'GCH0') THEN
        IF (NTOK.GT.5 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2 .AND.
     +      ITYPE(4).EQ.2 .AND. ITYPE(5).EQ.2 .AND. ITYPE(6).EQ.2) THEN
          LETTER = NINT(FVALUE(2))
          XCOFF = FVALUE(3)
          XCOFF = FVALUE(4)
          SIZX = FVALUE(5)
          SIZX = FVALUE(6)
        ELSE
          IF (PRINT)
     +         CALL NOCRLF('+LETTER(32-96) XCOFF YCOFF SIZX SIZY=$')
          READ (LUNIN,FMT=*) LETTER,XCOFF,YCOFF,SIZX,SIZY
        ENDIF
        CALL GSTYOF
        CALL GSGCH0(LETTER,XCOFF,YCOFF,SIZX,SIZY)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'GSY0') THEN
        IF (NTOK.GT.5 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2 .AND.
     +      ITYPE(4).EQ.2 .AND. ITYPE(5).EQ.2 .AND. ITYPE(6).EQ.2) THEN
          ISYMB = NINT(FVALUE(2))
          XCOFF = FVALUE(3)
          XCOFF = FVALUE(4)
          SIZX = FVALUE(5)
          SIZX = FVALUE(6)
        ELSE
          IF(PRINT) CALL NOCRLF('+ISYMB(1-32) XCOFF YCOFF SIZX SIZY =$')
          READ (LUNIN,FMT=*) ISYMB,XCOFF,YCOFF,SIZX,SIZY
        ENDIF
        CALL GSTYOF
        CALL GSGSY0(ISYMB,XCOFF,YCOFF,SIZX,SIZY)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'GCHF') THEN
        IF (NTOK.GT.6 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2 .AND.
     +      ITYPE(4).EQ.2 .AND. ITYPE(5).EQ.2 .AND. ITYPE(6).EQ.2
     +      .AND. ITYPE(7).EQ.2) THEN
          LETTER = NINT(FVALUE(2))
          XCOFF = FVALUE(3)
          XCOFF = FVALUE(4)
          SIZX = FVALUE(5)
          SIZX = FVALUE(6)
          KFONT = NINT(FVALUE(7))
        ELSE
          IF (PRINT) 
     +      CALL NOCRLF ('+LETTER(1-146) XCOFF YCOFF SIZX SIZY KFONT=$')
          READ (LUNIN,FMT=*) LETTER,XCOFF,YCOFF,SIZX,SIZY,KFONT
        ENDIF
        CALL GSTYOF
        CALL GSGCHF(LETTER,XCOFF,YCOFF,SIZX,SIZY,KFONT)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'GCHC') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.1) THEN
          HCHAR = CVALUE(2)(1:1)
        ELSE
          IF (PRINT) CALL NOCRLF ('+CHAR1 =$')
          READ (LUNIN,FMT=6312) CHAR1
        ENDIF
        IF (NTOK.GT.3 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2
     +      .AND. ITYPE(4).EQ.2 .AND. ITYPE(5).EQ.2) THEN
          XCOFF = FVALUE(3)
          XCOFF = FVALUE(4)
          KFONT = NINT(FVALUE(5))
        ELSE
          IF (PRINT) CALL NOCRLF ('+XCOFF YCOFF KFONT =$')
          READ (LUNIN,FMT=*) XCOFF,YCOFF,KFONT
        ENDIF
        CALL GSTYOF
        CALL GSGCHC(CHAR1,XCOFF,YCOFF,KFONT)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'GCHI') THEN
        IF (NTOK.GT.4 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2 .AND.
     +      ITYPE(4).EQ.2 .AND. ITYPE(5).EQ.2) THEN
          LETTER = NINT(FVALUE(2))
          XCOFF = FVALUE(3)
          XCOFF = FVALUE(4)
          KFONT = NINT(FVALUE(5))
        ELSE
          IF (PRINT) CALL NOCRLF ('+LETTER(NUM) XCOFF YCOFF KFONT =$')
          READ (LUNIN,FMT=*) LETTER,XCOFF,YCOFF,KFONT
        ENDIF
        CALL GSTYOF
        CALL GSGCHI(LETTER,XCOFF,YCOFF,KFONT)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'GCHH') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.1) THEN
          HCHAR = CVALUE(2)(1:1)
        ELSE
          IF (PRINT) CALL NOCRLF ('+HCHAR =$')
          READ (LUNIN,FMT=6312) HCHAR
        ENDIF
        IF (NTOK.GT.3 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2
     +      .AND. ITYPE(4).EQ.2 .AND. ITYPE(5).EQ.2) THEN
          XCOFF = FVALUE(3)
          XCOFF = FVALUE(4)
          KFONT = NINT(FVALUE(5))
        ELSE
          IF (PRINT) CALL NOCRLF ('+XCOFF YCOFF KFONT =$')
          READ (LUNIN,FMT=*) XCOFF,YCOFF,KFONT
        ENDIF
        CALL GSTYOF
        CALL GSGCHH(HCHAR,XCOFF,YCOFF,KFONT)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'GSYM') THEN
        IF (NTOK.GT.3 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          NSYMB = NINT(FVALUE(2))
          NSET  = NINT(FVALUE(3))
        ELSE
          IF (PRINT) CALL NOCRLF ('+NSYMB NSET =$')
          READ (LUNIN,FMT=*) NSYMB,NSET
        ENDIF
        CALL GSTYOF
        CALL GSGSYM(NSYMB,NSET)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'INUM') THEN
        IF (NTOK.GT.5 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2 .AND.
     +      ITYPE(4).EQ.2 .AND. ITYPE(5).EQ.2 .AND. ITYPE(6).EQ.2) THEN
          INUM =   NINT(FVALUE(2))
          NDIGIT = NINT(FVALUE(3))
          SIZX = FVALUE(4)
          SIZY = FVALUE(5)
          NJUST = NINT(FVALUE(6))
        ELSE
          IF (PRINT) CALL NOCRLF ('+INUM NDIGIT SIZX SIZY NJUST =$')
          READ (LUNIN,FMT=*) INUM,NDIGIT,SIZX,SIZY,NJUST
        ENDIF
        CALL GSTYOF
        CALL GSINUM(INUM,NDIGIT,SIZX,SIZY,NJUST)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'FNUM') THEN
        IF (NTOK.GT.6 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2 .AND.
     +      ITYPE(4).EQ.2 .AND. ITYPE(5).EQ.2 .AND. ITYPE(6).EQ.2
     +      .AND. ITYPE(7).EQ.2) THEN
          FNUM =   FVALUE(2)
          NDIGIT = NINT(FVALUE(3))
          NAFTER = NINT(FVALUE(4))
          SIZX = FVALUE(5)
          SIZY = FVALUE(6)
          NJUST = NINT(FVALUE(7))
        ELSE
          IF(PRINT) CALL NOCRLF('+FNUM NDIGIT NAFTER SIZX SIZY NJUST=$')
          READ (LUNIN,FMT=*) FNUM,NDIGIT,NAFTER,SIZX,SIZY,NJUST
        ENDIF
        CALL GSTYOF
        CALL GSFNUM(FNUM,NDIGIT,NAFTER,SIZX,SIZY,NJUST)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'ENUM') THEN
        IF (NTOK.GT.6 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2 .AND.
     +      ITYPE(4).EQ.2 .AND. ITYPE(5).EQ.2 .AND. ITYPE(6).EQ.2
     +      .AND. ITYPE(7).EQ.2) THEN
          ENUM =   FVALUE(2)
          NDIGIT = NINT(FVALUE(3))
          NAFTER = NINT(FVALUE(4))
          SIZX = FVALUE(5)
          SIZY = FVALUE(6)
          NJUST = NINT(FVALUE(7))
        ELSE
          IF(PRINT) CALL NOCRLF('+ENUM NDIGIT NAFTER SIZX SIZY NJUST=$')
          READ (LUNIN,FMT=*) ENUM,NDIGIT,NAFTER,SIZX,SIZY,NJUST
        ENDIF
        CALL GSTYOF
        CALL GSENUM(ENUM,NDIGIT,NAFTER,SIZX,SIZY,NJUST)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'STRC') THEN
        IF (NTOK.GT.2) THEN
          TEXT = LINE(IBEG(2):80)
        ELSE
          IF (PRINT) CALL NOCRLF ('+TEXT STRING =$')
          READ (LUNIN,FMT=6306) TEXT
        ENDIF
        CALL GSTYOF
        CALL GSSTRC(TEXT)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'STRH') THEN
        IF (PRINT) CALL NOCRLF ('+NUMBER OF LETTERS =$')
        READ (LUNIN,FMT=*) NLETT
        IF (PRINT) CALL NOCRLF ('+HTEXT =$')
        READ (LUNIN,FMT=6314) (HTEXT(I),I=1,NLETT)
        CALL GSTYOF
        CALL GSSTRH(HTEXT,NLETT)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'STR2') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.2) THEN
          NLETT = NINT(FVALUE(2))
        ELSE
          IF (PRINT) CALL NOCRLF ('+NUMBER OF LETTERS =$')
          READ (LUNIN,FMT=*) NLETT
        ENDIF
        IF (NTOK.EQ.(NLETT+2)) THEN
          DO 222 II=1,NLETT
            ITEXT2(II) = NINT(FVALUE(II+2))
 222      CONTINUE
        ELSE
          IF (PRINT) CALL NOCRLF ('+ITEXT2 =$')
          READ (LUNIN,FMT=*) (ITEXT2(I),I=1,NLETT)
        ENDIF
        CALL GSTYOF
        CALL GSSTR2(ITEXT2,NLETT)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'STR4') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.2) THEN
          NLETT = NINT(FVALUE(2))
        ELSE
          IF (PRINT) CALL NOCRLF ('+NUMBER OF LETTERS =$')
          READ (LUNIN,FMT=*) NLETT
        ENDIF
        IF (NTOK.EQ.(NLETT+2)) THEN
          DO 224 II=1,NLETT
            ITEXT4(II) = NINT(FVALUE(II+2))
 224     CONTINUE
        ELSE
          IF (PRINT) CALL NOCRLF ('+ITEXT4 =$')
          READ (LUNIN,FMT=*) (ITEXT4(I),I=1,NLETT)
        ENDIF
        CALL GSTYOF
        CALL GSSTR4(ITEXT4,NLETT)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'STRD') THEN
        IF (NTOK.GT.2) THEN
          TEXT = LINE(IBEG(2):80)
        ELSE
          IF (PRINT) CALL NOCRLF ('+TEXT =$')
          READ (LUNIN,FMT=6306) TEXT
        ENDIF
        IF (NTOK.GT.4 .AND. ITYPE(3).EQ.2 .AND.ITYPE(4).EQ.2) THEN
          DX = FVALUE(3)
          DY = FVALUE(4)
        ELSE
          IF (PRINT) CALL NOCRLF ('+DXD DYD =$')
          READ (LUNIN,FMT=*) DX,DY
        ENDIF
        CALL GSTYOF
        CALL GSSTRD(TEXT,DX,DY)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'STRU') THEN
        IF (NTOK.GT.2) THEN
          TEXT = LINE(IBEG(2):80)
        ELSE
          IF (PRINT) CALL NOCRLF ('+TEXT =$')
          READ (LUNIN,FMT=6306) TEXT
        ENDIF
        IF (NTOK.GT.4 .AND. ITYPE(3).EQ.2 .AND.ITYPE(4).EQ.2) THEN
          DX = FVALUE(3)
          DY = FVALUE(4)
        ELSE
          IF (PRINT) CALL NOCRLF ('+DXU DYU =$')
          READ (LUNIN,FMT=*) DX,DY
        ENDIF
        CALL GSTYOF
        CALL GSSTRU(TEXT,DX,DY)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'CFSZ') THEN
        IF (NTOK.GT.4 .AND. ITYPE(3).EQ.2 .AND.ITYPE(4).EQ.2) THEN
          NCHAR = NINT(FVALUE(3))
          KFONT = NINT(FVALUE(4))
        ELSE
          IF (PRINT) CALL NOCRLF ('+NCHAR KFONT =$')
          READ (LUNIN,FMT=*) NCHAR,KFONT
        ENDIF
        CALL GSCFSZ(NCHAR,XLEFT,XRIGHT,YLOW,YHIGH,CWID,KFONT)
        CHAR1 = CHAR(NCHAR)
        IF (PRINT) WRITE (LUNOUT,FMT=6200) CHAR1,XLEFT,XRIGHT,YLOW,
     +      YHIGH,CWID
C
      ELSE IF (KEY.EQ.'CLPL') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6202)
C
      ELSE IF (KEY.EQ.'CLPT') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6204)
C
      ELSE IF (KEY.EQ.'CLTS') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6206)
C
      ELSE IF (KEY.EQ.'SWLN') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6208)
C
      ELSE IF (KEY.EQ.'DWTO') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          X = FVALUE(2)
          Y = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+X Y =$')
          READ (LUNIN,FMT=*) X,Y
        ENDIF
        CALL GSTYOF
C
C---- this trick is necessary to overcome the effect of switching
C     modes and of doing successive plots at the same point
C
        IF (NTERM.EQ.1) THEN
          CALL GSMVTO(XOLD,YOLD)
          XOLD = X
          YOLD = Y
        END IF
        CALL GSDWTO(X,Y)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'DWBY') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          X = FVALUE(2)
          Y = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+X Y =$')
          READ (LUNIN,FMT=*) X,Y
        ENDIF
        CALL GSTYOF
        IF (NTERM.EQ.1) THEN
          CALL GSMVTO(XOLD,YOLD)
          XOLD = XOLD + X
          YOLD = YOLD + Y
        END IF
        CALL GSDWBY(X,Y)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'MVTO') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          X = FVALUE(2)
          Y = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+X Y =$')
          READ (LUNIN,FMT=*) X,Y
        ENDIF
C
C---- For interactive plotting the terminal switching destroys the
C     vector chain so save the move till the draw comes along!
C
        IF (NTERM.EQ.1) THEN
          XOLD = X
          YOLD = Y
        ELSE
          CALL GSMVTO(X,Y)
        END IF
C
      ELSE IF (KEY.EQ.'MVBY') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          X = FVALUE(2)
          Y = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+X Y =$')
          READ (LUNIN,FMT=*) X,Y
        ENDIF
        IF (NTERM.EQ.1) THEN
          XOLD = XOLD + X
          YOLD = YOLD + Y
        ELSE
          CALL GSMVBY(X,Y)
        END IF
C
      ELSE IF (KEY.EQ.'POIN') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          X = FVALUE(2)
          Y = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+X Y =$')
          READ (LUNIN,FMT=*) X,Y
        ENDIF
        CALL GSTYOF
        CALL GSPOIN(X,Y)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'DDRB') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          X = FVALUE(2)
          Y = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+X Y =$')
          READ (LUNIN,FMT=*) X,Y
        ENDIF
        CALL GSTYOF
        IF (NTERM.EQ.1) THEN
          CALL GSMDRB(XBOLD,YBOLD)
          XBOLD = X
          YBOLD = Y
        END IF
        CALL GSDDRB(X,Y)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'MDRB') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          X = FVALUE(2)
          Y = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+X Y =$')
          READ (LUNIN,FMT=*) X,Y
        ENDIF
        IF (NTERM.EQ.1) THEN
          XBOLD = X
          YBOLD = Y
        ELSE
          CALL GSMDRB(X,Y)
        END IF
C
      ELSE IF (KEY.EQ.'DOTB') THEN
        IF (NTOK.GT.2 .AND. ITYPE(2).EQ.2 .AND. ITYPE(3).EQ.2) THEN
          X = FVALUE(2)
          Y = FVALUE(3)
        ELSE
          IF (PRINT) CALL NOCRLF ('+X Y =$')
          READ (LUNIN,FMT=*) X,Y
        ENDIF
        CALL GSTYOF
        CALL GSDOTB(X,Y)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'LRAS') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6226)
C
      ELSE IF (KEY.EQ.'LRSB') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6228)
C
      ELSE IF (KEY.EQ.'TEXT') THEN
        IF (NTOK.GT.1) THEN
          TEXT = LINE(IBEG(2):80)
        ELSE
          IF (PRINT) CALL NOCRLF ('+TEXT STRING =$')
          READ (LUNIN,FMT=6306) TEXT
        ENDIF
C
      ELSE IF (KEY.EQ.'VIEW') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.1) THEN
          GSFIL = CVALUE(2)
        ELSE
          IF (PRINT) CALL NOCRLF ('+GSFIL(.PLT ASSUMED)=$')
          READ (LUNIN,FMT=6306) GSFIL
        ENDIF
C
        NLENG = LENSTR(GSFIL)
C
        IF (NLENG.LT.1) THEN
          GSFIL = 'QUICK.PLT'
        ELSE
          GSFIL = GSFIL(1:NLENG)//'.PLT'
        END IF
        NSTART = 0
        ISCALE = 1
        SCAFAC = 0.5
        LINSIZ = 0
        NOCENT = 0
        NOPRNT = 1                     
        NINTER = 0
        IERR = 0
        CALL GSVIEW(GSFIL,NSTART,ISCALE,SCAFAC,LINSIZ,NOCENT,
     +                NOPRNT,NINTER,IERR)
C
      ELSE IF (KEY.EQ.'TRIL') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.1) THEN
          GSFIL = CVALUE(2)
        ELSE
          IF (PRINT) CALL NOCRLF ('+GSFIL(.PLT ASSUMED) =$')
          READ (LUNIN,FMT=6306) GSFIL
        ENDIF
C
        NLENG = LENSTR(GSFIL)
C
        IF (NLENG.LT.1) THEN
          GSFIL = 'QUICK.PLT'
        ELSE
          GSFIL = GSFIL(1:NLENG)//'.PLT'
        END IF
C
        IF (NLENG.LT.1) THEN
          LISFIL = 'QUICK.LIS'
        ELSE
          LISFIL = GSFIL(1:NLENG)//'.LIS'
        END IF
        CALL GSTRIL(GSFIL,LISFIL)
C
      ELSE IF (KEY.EQ.'VCLN') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6226)
C
      ELSE IF (KEY.EQ.'CETX') THEN
        IF (NTOK.GT.1 .AND. ITYPE(2).EQ.2) THEN
          NJUST = NINT(FVALUE(2))
        ELSE
          IF (PRINT) CALL NOCRLF ('+NJUST(1)(2)(3)=$')
          READ (LUNIN,FMT=*) NJUST
        ENDIF
        IF (NTOK.GT.2) THEN
          TEXT = LINE(IBEG(3):80)
        ELSE
          IF (PRINT) CALL NOCRLF ('+TEXT=$')
          READ (LUNIN,FMT=6306) TEXT
        ENDIF
        CALL GSTYOF
        CALL GSCETX(TEXT,NJUST)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'SCUR') THEN
        IF (PRINT) CALL NOCRLF ('+XCGAP =$')
        READ (LUNIN,FMT=*) XCGAP
        CALL GSSCUR(XCGAP)
C
      ELSE IF (KEY.EQ.'GSYS') THEN
        IF (PRINT) CALL NOCRLF ('+NSYMB NSET XCOFF YCOFF SIZX SIZY=$')
        READ (LUNIN,FMT=*) NSYMB,NSET,XCOFF,YCOFF,SIZX,SIZY
        CALL GSTYOF
        CALL GSGSYS(NSYMB,NSET,XCOFF,YCOFF,SIZX,SIZY)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'GSYC') THEN
        IF (PRINT) CALL NOCRLF ('+CHAR1 NSET =$')
        READ (LUNIN,FMT=6316) CHAR1,NSET
        IF (PRINT) CALL NOCRLF ('+XCOFF YCOFF SIZX SIZY =$')
        READ (LUNIN,FMT=*) XCOFF,YCOFF,SIZX,SIZY
        CALL GSTYOF
        CALL GSGSYC(CHAR1,NSET,XCOFF,YCOFF,SIZX,SIZY)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'PROM') THEN
        PRINT = .TRUE.
C
      ELSE IF (KEY.EQ.'CETS') THEN
        IF (PRINT) CALL NOCRLF ('+TEXT =$')
        READ (LUNIN,FMT=6306) TEXT
        IF (PRINT) CALL NOCRLF ('+SIZX,SIZY,NJUST(1)(2)(3)=$')
        READ (LUNIN,FMT=*) SIZX,SIZY,NJUST
        CALL GSTYOF
        CALL GSCETS(TEXT,SIZX,SIZY,NJUST)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'GCHS') THEN
        IF (PRINT) CALL NOCRLF ('+CHAR1 =$')
        READ (LUNIN,FMT=6306) CHAR1
        IF (PRINT) CALL NOCRLF ('+XCOFF,YCOFF,SIZ1,SIZ2,KFONT=$')
        READ (LUNIN,FMT=*) XCOFF,YCOFF,SIZ1,SIZ2,KFONT
        CALL GSTYOF
        CALL GSGCHS(CHAR1,XCOFF,YCOFF,SIZ1,SIZ2,KFONT)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'STRS') THEN
        IF (PRINT) CALL NOCRLF ('+TEXT=$')
        READ (LUNIN,FMT=6306) TEXT
        IF (PRINT) CALL NOCRLF ('+SIZX SIZY =$')
        READ (LUNIN,FMT=*) SIZX,SIZY
        CALL GSTYOF
        CALL GSSTRS(TEXT,SIZX,SIZY)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'MIXC') THEN
        IF (PRINT) CALL NOCRLF ('+MCOLOR =$')
        READ (LUNIN,FMT=*) MCOLOR
        CALL GSMIXC(MCOLOR)
C
      ELSE IF (KEY.EQ.'NUMB') THEN
        IF (PRINT) CALL NOCRLF ('+X Y HGT FNUM ANGLE NDIG =$')
        READ (LUNIN,FMT=*) X,Y,HGT,FNUM,ANGLE,NDIG
        CALL GSTYOF
        CALL NUMBER(X,Y,HGT,FNUM,ANGLE,NDIG)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'SYMB') THEN
        IF (PRINT) CALL NOCRLF ('+X Y HGT ANGLE NC =$')
        READ (LUNIN,FMT=*) X,Y,HGT,ANGLE,NC
        IF (PRINT) CALL NOCRLF ('+BYTXT =$')
        READ (LUNIN,FMT=6314) (HTEXT(J),J=1,NC)
        CALL GSTYOF
        CALL SYMBOL(X,Y,HGT,HTEXT,ANGLE,NC)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'AXIS') THEN
        IF (PRINT) CALL NOCRLF ('+X Y NCHAR AXLEN ANGLE FVAL DV=$')
        READ (LUNIN,FMT=*) X,Y,NCHAR,AXLEN,ANGLE,FVAL,DV
        IF (PRINT) CALL NOCRLF ('+LABEL FOR AXIS =$')
        READ (LUNIN,FMT=6314) (HTEXT(J),J=1,NCHAR)
        CALL GSTYOF
        CALL AXIS(X,Y,HTEXT,NCHAR,AXLEN,ANGLE,FVAL,DV)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'STNG') THEN
        IF (PRINT) CALL NOCRLF ('+NCHARS =$')
        READ (LUNIN,FMT=*) NCHARS
        IF (PRINT) CALL NOCRLF ('+BYTXT =$')
        READ (LUNIN,FMT=6314) (HTEXT(J),J=1,NCHARS)
        CALL GSTYOF
        CALL STRING(HTEXT,NCHARS)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'CSTN') THEN
        IF (PRINT) CALL NOCRLF ('+NCHARS =$')
        READ (LUNIN,FMT=*) NCHARS
        IF (PRINT) CALL NOCRLF ('+BYTXT =$')
        READ (LUNIN,FMT=6314) (HTEXT(J),J=1,NCHARS)
        CALL GSTYOF
        CALL CSTRNG(HTEXT,NCHARS)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'NOPR') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6282)
        PRINT = .FALSE.
C
      ELSE IF (KEY.EQ.'CSPU') THEN
        IF (PRINT) CALL NOCRLF ('+IUSPCE (1) (0) =$')
        READ (LUNIN,FMT=*) IUSPCE
        CALL GSCSPU(IUSPCE)
C
      ELSE IF (KEY.EQ.'TSAV') THEN
        IF (PRINT) CALL NOCRLF ('+TSAV: Save current scales$')
        CALL GSTSAV(NTRSAV)
C
      ELSE IF (KEY.EQ.'TRES') THEN
        IF (PRINT) CALL NOCRLF ('+TRES: Restore saved scales$')
        CALL GSTRES(NTRSAV)
C
      ELSE IF (KEY.EQ.'EREV') THEN
        IF (PRINT) CALL NOCRLF ('+EREV: Reverse erase mode VT640$')
        CALL GSTYON
        CALL GSEREV
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'HAIR') THEN
        IF (PRINT) CALL NOCRLF ('CROSS HAIR POSITION: X Y =$')
        READ (LUNIN,FMT=*) X,Y
        CALL GSHAIR(X,Y,CHAR1)
        CALL GSTYON
        WRITE (LUNOUT,FMT=6294) X,Y,CHAR1
C
      ELSE IF (KEY.EQ.'CMFL') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6296)
        NCMFIL = 1
        PRINT = .FALSE.
C
      ELSE IF (KEY.EQ.'LINE') THEN
        IF (PRINT) WRITE (LUNOUT,FMT=6298)
        READ (LUNIN,FMT=*) P(1),P(2),Q(1),Q(2)
        CALL GSTYOF
        CALL GSLINE(P,Q)
        XOLD = Q(1)
        YOLD = Q(2)
        CALL GSTYON
C
      ELSE IF (KEY.EQ.'TERM') THEN
        IF (IPRINT) WRITE (LUNOUT,FMT=6300)
        NTERM = 1
      ELSE IF (KEY.EQ.'QUIT') THEN
        GO TO 1390
      ELSE IF (KEY.EQ.'HELP') THEN
        DO 80 II=1,20
          WRITE (LUNOUT,60) (KEY5((II-1)*10+JJ),JJ=1,10)
 60       FORMAT (10A6)
 80     CONTINUE
      ELSE
        WRITE (LUNOUT,6304) KEY
      END IF
      GO TO 10
C
C---- Format statements
C
 6002 FORMAT (2X,'FILE NAME USED =',A)
 6008 FORMAT (1X,'ENVR')
 6010 FORMAT (1X,'XENV')
 6026 FORMAT (2X,'DEFAULT-- DWLIMX DWLIMY =',2F10.4)
 6028 FORMAT (2X,'DEFAULT-- DVXMIN DVXMAX DVYMIN DVYMAX =',/1X,4F10.4)
 6034 FORMAT (2X,'DEFAULT -- VBXMIN VBXMAX VBYMIN VBYMAX =',/1X,4F10.4)
 6042 FORMAT (2X,'FONT READ IN ')
 6044 FORMAT (2X,'LEVEL CHECK ')
 6046 FORMAT (2X,'PICTURE STARTED ')
 6050 FORMAT (2X,'END PICTURE ')
 6058 FORMAT (2X,'PLOTTER SWITCHED OFF ')
 6062 FORMAT (2X,'IUNIT RETURNED AS ',I5)
 6066 FORMAT (2X,'IUNIT RETURNED AS ',I5)
 6074 FORMAT (1X,'*FLRI EOF RETURN')
 6076 FORMAT (2X,'WRITE HEADER ')
 6078 FORMAT (1X,'*RHDR EOF RETURN')
 6080 FORMAT (2X,'READ HEADER ')
 6082 FORMAT (2X,'UPDATE HEADER ')
 6084 FORMAT (2X,'REWIND PLOTFILE ')
 6094 FORMAT ('*FLRR EOF  RETURN')
 6096 FORMAT (2X,'USER TRANSFORMATION ')
 6128 FORMAT (2X,'CURSOR FOUND AT: XCSTRT YCSTRT =',2F10.4)
 6136 FORMAT (1X,'-- GSSTYL CALLED --')
 6148 FORMAT (2X,'ERASE VT640 ON')
 6150 FORMAT (2X,'ERASE VT640 OFF')
 6200 FORMAT (2X,'CHAR=',A,' XL XR YL YH ',4F10.4,/2X,'CWID ',F10.4)
 6202 FORMAT (2X,'-NO TEST FOR GSCLPL-')
 6204 FORMAT (2X,'-NO TEST FOR GSCLPT-')
 6206 FORMAT (2X,'-NO TEST FOR GSCLTS-')
 6208 FORMAT (2X,'-NO TEST FOR GSSWLN-')
 6226 FORMAT (2X,'-NO TEST FOR GSLRAS-')
 6228 FORMAT (2X,'-NO TEST FOR GSLRSB-')
 6282 FORMAT (2X,'NOPR (Prompt off)')
 6294 FORMAT ('$ FOUND AT X Y =',2F10.4,' CHKEY =',A)
 6296 FORMAT (2X,'CMFL: Commands read from file ')
 6298 FORMAT (2X,'LINE X1,Y1,X2,Y2 =',4F10.4)
 6300 FORMAT (2X,'TERMINAL PLOTTING SET ')
 6304 FORMAT (2X,'!!!ERROR: UNKNOWN KEY=<',A4,'> IGNORED')
 6306 FORMAT (A)
 6308 FORMAT (I5)
 6312 FORMAT (A1)
 6314 FORMAT (80A1)
 6316 FORMAT (A1,I5)
C
 1390 END
C
C
C
      SUBROUTINE GSRDTM(ITERM,NRWMIN,NRWMAX,SCREEN)
C     ==============================================
C
C---- Read vt640 screen as coded bytes of 5-bit data.
C
C     A.D. McLachlan SEP 1984. Last updated 21 SEP 1984.
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were ESC, GS, SPARE
C     and the followig BYTEs to INTEGER: SCREEN
C
C     This program works by reading lines in groups of three.
C     The given values of nrwmin and nrwmax will therefore be
C     rounded to 3*n-1 and 3*m.
C     On exit the terminal will be in teletype mode
C
C     ITERM  = fortran unit number for terminal in graphics package
C     NRWMIN = min line number for bottom of display dump
C     NRWMAX = max line number for top of display dump
C              the whole screen goes from 0 to 479 (bottom to top)
C              in physical dot units.
C     SCREEN = byte array for accepting the data. 128 bytes per line.
C
C     The 4010 memory coords (0-1023 in x, 0-780 in y) are here
C     replaced by the physical dot display range
C     (0-639 in x, 0-479 in y).
C     The origin is always at the bottom left hand corner of the screen.
C
C     .. Scalar Arguments ..
      INTEGER ITERM,NRWMAX,NRWMIN
C     ..
C     .. Array Arguments ..
      INTEGER SCREEN(0:127,0:*)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      REAL SECTOT,SSTAGE
      INTEGER I,ITPRNT,J,NCMAX,NCMIN,NCNT1,NCNT8,NCOUNT,NCWID,NCWID1,
     +        NLEN,NLIN1,NLINES,NROW,NROWD,NRWLOW,NSTART
      CHARACTER*1 ESC,GS
      CHARACTER OUTLIN*40
C     ..
C     .. Local Arrays ..
      CHARACTER*1 SPARE(8)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSSUSP,GSTIM0,GSTIMR,GSTYTM
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
      ESC = CHAR(27)
      GS = CHAR(29)
      ITPRNT = 0
      CALL GSTIM0(ITPRNT)
C
C---- routine assumes vt640 is in graphics mode and buffer clear
C
C     read from vt640 memory
C     5-bit chars + octal 100 in each byte. loaded to the right.
C     arranged in 128 columns of 5 dots wide. cols numbered (0:127)
C     and 480 rows of one dot high numbered (0:479)
C     origin (0,0) at bottom left of screen
C     "GS" gives vector mode
C     For memory readback write esc"<XVALUE>;<YVALUE>;<COUNT>C
C
C---- This routine reads up to 512 bytes or 4 rows at a time,
C     as this seems to be the maximum record length that the
C     machine can deal with records with more than 512 bytes
C     arrive without the <cr> terminator.
C
C---- !! The vt640 manual says that sets of zero bytes are sent as a
C      "$" followed by a count of up to 32 in the low six-bits of
C       the output character. but this does not seem to happen
C       when reading more than 9 bytes the byte 6 from the end of
C       the output seems to be lost.
C
C---- screen limits to read
C
C---- set row limits to 3n-1 and 3m
C     read 3 lines at a time
C     if nlines.gt.1 then must read whole rows of 128 cols
C
      IF (NRWMAX.GT.479) NRWMAX = 479
      IF (NRWMAX.LE.0) NRWMAX = 479
      NRWMAX = ((NRWMAX+3)/3)*3 - 1
      IF (NRWMIN.GE.NRWMAX) NRWMIN = 0
      IF (NRWMIN.LT.0) NRWMIN = 0
      NRWMIN = (NRWMIN/3)*3
      NLINES = 3
      NLIN1 = NLINES - 1
      NRWLOW = NRWMIN + NLIN1
      NCMIN = 0
      NCWID = 128
      NCWID1 = NCWID - 1
      NCMAX = NCMIN + NCWID1
      NCOUNT = NCWID*NLINES
      NCNT1 = NCOUNT - 1
      NCNT8 = NCOUNT + 8
C
C---- Begin readout
C     WARNING !! any changes to this part of the program should be
C     made with great caution, as they are likely to prevent it
C     from working correctly
C
      NLEN = 19
      DO 10 NROW = NRWMAX,NRWLOW,-NLINES
        IF (NROW.EQ.NRWLOW) THEN
          GO TO 20
        ELSE
C
C---- internal write to set up character stream
C     no blanks allowed in the line
C
          WRITE (OUTLIN,FMT=6000) GS,ESC,'"',NCMIN,';',NROW,';',NCNT8,
     +      ';c'
          WRITE (ITERM,FMT=6002) OUTLIN(1:NLEN)
C
C---- read 8 spare bytes =40
C     dots to circumvent error in terminal response
C
          NROWD = NROW - NRWMIN
          READ (ITERM,FMT=6004) ((SCREEN((NCMIN+I), (NROWD-J)),I=0,
     +      NCWID1),J=0,NLIN1),SPARE
        END IF
   10 CONTINUE
C
C---- debugging write statements
C      write(ioprnt,23) nrow
C      write(ioprnt,24)
C    1  ((screen((ncmin+i),(nrowd-j)),i=0,ncwid1),j=0,nlin1)
C      write(ioprnt,30)
C    1  ((screen((ncmin+i),(nrowd-j)),i=0,ncwid1),j=0,nlin1)
C
C
C---- special fixup for last row.
C     do not read spare, but reread last 8 bytes
C
   20 CONTINUE
C
C---- last group of rows
C
      WRITE (OUTLIN,FMT=6000) GS,ESC,'"',NCMIN,';',NRWLOW,';',NCOUNT,
     +  ';c'
      WRITE (ITERM,FMT=6002) OUTLIN(1:NLEN)
      NROWD = NRWLOW - NRWMIN
      READ (ITERM,FMT=6004) ((SCREEN((NCMIN+I), (NROWD-J)),I=0,NCWID1),
     +  J=0,NLIN1)
C
C---- last 8 bytes
C
      NCOUNT = 8
      NSTART = NCMAX - 7
      WRITE (OUTLIN,FMT=6000) GS,ESC,'"',NSTART,';',NRWMIN,';',NCOUNT,
     +  ';c'
      WRITE (ITERM,FMT=6002) OUTLIN(1:NLEN)
      READ (ITERM,FMT=6004) (SCREEN((NSTART+I),0),I=0,7)
C      CALL GSSUSP(100)
      CALL GSTYTM
C      CALL GSSUSP(100)
      CALL GSTIMR(SECTOT,SSTAGE,ITPRNT)
      WRITE (LUNOUT,FMT=6010) SECTOT
C
C---- Format statements
C
 6000 FORMAT (2A1,A1,I3.3,A1,I3.3,A1,I6.6,A2)
 6002 FORMAT (A)
 6004 FORMAT (4096A1)
 6010 FORMAT (2X,'GSRDTM: Screen read done. cpu sec = ',F8.2)
C
      END
C
C
C
      SUBROUTINE GSRFNT
C     ==================
C
C     A.D. McLachlan JULY 1984. Last updated 17 SEP 1986
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were NFONTS
C
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Arrays in Common ..
      INTEGER*2 IFHT,IFSTRT,IFWID,IFX0,IFY0,LENGF
      CHARACTER*1 NFONTS
C     ..
C     .. Local Scalars ..
      INTEGER IUNITF
C     ..
c     LOGICAL CCPONL
c     EXTERNAL CCPONL
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSFNT/IFSTRT(150,4),LENGF(150,4),IFX0(150,4),
     +       IFY0(150,4),IFWID(150,4),IFHT(150,4),NFONTS(4,3000,4)
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Data statements ..
      DATA IUNITF/89/
C
      IFAIL = 0
      ITEROP = -IUNITF
      CALL CCPDPN (ITEROP,'PUBLIC_FONT84','READONLY','U',80,IFAIL)
      IF (IFAIL.NE.0) GO TO 10
      READ (IUNITF) IFSTRT,LENGF,IFX0,IFY0,IFWID,IFHT,NFONTS
      CLOSE (UNIT=IUNITF)
      RETURN
   10 WRITE (LUNOUT,FMT=6000)
C
C---- Format statements
C
 6000 FORMAT (2X,'!!!GSRFNT ERROR: UNABLE TO READ FONTS - FILE=',
     +       'PUBLIC_FONT84')
C
      END
C
C
C
      SUBROUTINE GSRSTM(ITERM,FILNAM)
C     ================================
C
C---- fill vt640 screen from a saved disc file in packed byte form.
C
C     A.D. McLachlan SEP 1984. Last updated 22 SEP 1984.
C     9/2/90 - PJD: Changed definition of several BYTEs to INTEGER
C     They were SCREEN
C
C     ITERM  = fortran unit number for terminal in graphics package
C     FILNAM = filename or logical unit number for output
C
C     The screen was saved in rows of 128 bytes packed as 5 dots per bit
C     in the lower bits from left to right. 480 rows use 60*1024 bytes
C     the diskio direct access routines are used for writing the screen
C
C     The 4010 memory coords (0-1023 in x, 0-780 in y) 
C     are NOT USED here.
C     This routine uses the physical dot display range
C      (0-639 in x, 0-479 in y).
C     The origin is always at the bottom left hand corner of the screen.
C
C     .. Scalar Arguments ..
      INTEGER ITERM
      CHARACTER FILNAM* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      REAL SECTOT,SSTAGE
      CHARACTER*1 ZERO
      INTEGER IERR,ITPRNT,IUNIT,NCOL,NCOUNT,NROW,NRWMAX,NRWMIN,NMCITM
C     ..
C     .. Local Arrays ..
      INTEGER SCREEN(0:127,0:3)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSBFTM,GSGRTM,GSSLTM,GSTIM0,GSTIMR,
     +         GSTYTM,QCLOSE,QOPEN,QREAD
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
      ZERO = CHAR(0)
      ITPRNT = 0
      CALL GSTIM0(ITPRNT)
C
C---- put vt640 in graphics mode and clear buffer of vt640
C
      CALL GSGRTM
D     CALL GSBFTM(ITERM,ZERO)
C
C---- open input file
C
      CALL QOPEN(IUNIT,FILNAM,'READONLY')
      CALL QMODE (IUNIT,0,NMCITM)
C
C---- fill into vt640 memory
C     5-bit chars + octal 100 in each byte. loaded to the right.
C     arranged in 128 columns of 5 dots wide. cols numbered (0:127)
C     and 480 rows of one dot high numbered (0:479)
C     origin (0,0) at bottom left of screen
C
      NRWMIN = 0
      NRWMAX = 479
      NCOL = 0
      NCOUNT = 512
C
C---- Start reading rows
C     and put them onto screen from top to bottom
C
      DO 10 NROW = NRWMAX,NRWMIN,-4
        CALL QREAD(IUNIT,SCREEN,512,IERR)
        IF (IERR.EQ.1) THEN
          GO TO 20
        ELSE
          CALL GSSLTM(ITERM, (NROW-3),NROW,SCREEN)
        END IF
   10 CONTINUE
C
C---- close disc file
C
   20 CALL QCLOSE(IUNIT)
      CALL GSTIMR(SECTOT,SSTAGE,ITPRNT)
      CALL GSTYTM
      WRITE (LUNOUT,FMT=6000) SECTOT
C
C---- Format statements
C
 6000 FORMAT (2X,'GSRSTM: Restore complete . cpu sec = ',F8.2)
C
      END
C
C
C
      SUBROUTINE GSSFTM(ITERM,BITS,NROW,NCOL,NCOUNT)
C     ===============================================
C
C---- Routine for filling vt640 screen with dot patterns
C
C     A.D. McLachlan SEP 1984 . Last updated 20 SEP 1984.
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were ZERO, NBIT0, PLUS, HASH, CR, ENQ, ESC
C     FF, FS, GS, HT, LF, RS, SUB, US, VT
C     and the followig BYTEs to INTEGER: BITS, NBYTE, BYTE1 ,XBIT, LINE
C
C     Patterns are specified in bytes as 1,0,0,1,...  one byte per dot
C
C     ITERM  = fortran unit number of terminal graphics stream
C     BITS   = byte array of integers either 0 or 1 (no packing)
C     NCOL   = col to start on screen (0:127) 
C              each col holds 5 "bits" dots
C     NROW   = row to start on screen (0:479)  bottom left is (0,0)
C     NCOUNT = number of locations to load . each to hold 5 "bits" dots
C              one row has 128 locations or 640 dots. 
C              So 5*ncount dots are loaded
C              ncount must not exceed 512 (equivalent to 2560 dots or
C              4 complet lines of screen.
C
C---- write to vt640 memory
C     5-bit chars + octal 100 in each byte. loaded to the right.
C     arranged in 128 columns of 5 dots wide. cols numbered (0:127)
C     and 480 rows of one dot high numbered (0:479)
C     origin (0,0) at bottom left of screen
C     for memory writeout write esc"<XVALUE>;<YVALUE>;<COUNT>;A
C
C---- This routine writes up to 512 bytes or 4 rows at a time.
C     going from left to right and top to bottom on screen.
C     this requires the output of 515 bytes in all 
C     because of the special
C     control codes. so the sys$output stream needs to be opened with
C     a suitably long record length (this program assumes recl=1024).
C
C---- !! the vt640 manual says that sets of zero bytes are sent as a
C      "$" followed by a count of up to 32 in the low six-bits of
C      The output character. but this does not seem to work
C
C---- Put vt640 in graphics mode and clear buffer of vt640
C
C     .. Scalar Arguments ..
      INTEGER ITERM,NCOL,NCOUNT,NROW
C     ..
C     .. Array Arguments ..
      INTEGER BITS(0:*)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      INTEGER I,ICOL,IOPRNT,JB5,KBIT,MCOUNT,NBITS,NBITS1,NCNT1,NLEN,
     +        NPACK,NBYTE,BYTE1
      CHARACTER*1 ZERO,NBIT0,PLUS,HASH,CR,ENQ,ESC,FF,
     +     FS,GS,HT,LF,RS,SUB,US,VT
      CHARACTER OUTLIN*40
C     ..
C     .. Local Arrays ..
      INTEGER XBIT(0:4),LINE(0:1023)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSBFTM,GSGRTM
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Data statements ..
      DATA BYTE1/1/,PLUS/'+'/,HASH/'#'/,XBIT/16,8,4,2,1/
C
      ZERO = CHAR(0)
      NBIT0 = CHAR(0)
      ENQ = CHAR(5)
      ESC = CHAR(27)
      FF = CHAR(12)
      FS = CHAR(28)
      GS = CHAR(29)
      HT = CHAR(9)
      LF = CHAR(10)
      RS = CHAR(30)
      SUB = CHAR(26)
      US = CHAR(31)
      VT = CHAR(11)
C
      CALL GSGRTM
D     CALL GSBFTM(ITERM,ZERO)
C
C---- Translate into packed form
C     number of bits sent is 5*number of columns
C
      NPACK = 1
      MCOUNT = NCOUNT
      IF (MCOUNT.LT.1) MCOUNT = 1
      IF (MCOUNT.GT.512) MCOUNT = 512
      NBITS = 5*MCOUNT
      NBITS1 = NBITS - 1
      NCNT1 = MCOUNT - 1
      IOPRNT = 8
C
C---- Make up output array
C
      KBIT = 0
      DO 20 ICOL = 0,NCNT1
        NBYTE = 64
C
C---- Start on bits
C
        DO 10 JB5 = 0,4
          IF (BITS(KBIT).EQ.1) NBYTE = NBYTE .OR. XBIT(JB5)
          KBIT = KBIT + 1
   10   CONTINUE
        LINE(ICOL) = NBYTE
   20 CONTINUE
C
C---- Send address load instruction
C     internal write to set up character stream
C     no blanks allowed in the line'
C
      WRITE (OUTLIN,FMT=6000) ESC,'"',NCOL,';',NROW,';a'
      NLEN = 11
      WRITE (ITERM,FMT=6002) OUTLIN(1:NLEN)
C
C---- Send line to vt640
C
      NCNT1 = NCOUNT - 1
      WRITE (ITERM,FMT=6004) ESC,PLUS, (LINE(I),I=0,NCNT1),HASH
C
C---- Format statements
C
 6000 FORMAT (2A1,I3.3,A1,I3.3,A2)
 6002 FORMAT (A)
 6004 FORMAT (4096A1)
C
      END
C
C
C
      SUBROUTINE GSSLTM(ITERM,NRWMIN,NRWMAX,SCREEN)
C     ==============================================
C
C---- Routine for restoring vt640 screen with saved packed bytes
C
C    A.D. McLachlan SEP 1984 . Last updated 22 SEP 1984.
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were ESC, HASH, PLUS, ZERO
C     and the followig BYTEs to INTEGER: SCREEN
C
C     ITERM  = fortran unit number of terminal graphics stream
C     NRWMIN = bottom row to fill
C     NRWMAX = top row to fill   (max 4 rows at a time)
C     SCREEN = array of rows of bytes in cols (0:127) and rows (0:479)
C              one row has 128 locations or 640 dots.
C              bytes sent must not exceed 512 
C              (equivalent to 2560 dots or 4 complete lines of screen.
C
C     .. Scalar Arguments ..
      INTEGER ITERM,NRWMAX,NRWMIN
C     ..
C     .. Array Arguments ..
      INTEGER SCREEN(0:127,0:*)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      INTEGER I,J,NCOL,NLEN,NRWLOW,NRWTOP,NSENT,NSENT1
      CHARACTER*1 ESC,HASH,PLUS,ZERO
      CHARACTER OUTLIN*40
C     ..
C     .. External Subroutines ..
      EXTERNAL GSBFTM,GSGRTM
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Data statements ..
      DATA PLUS/'+'/,HASH/'#'/
C
      ZERO = CHAR(0)
      ESC = CHAR(27)
C
C---- Write to vt640 memory
C     5-bit chars + octal 100 in each byte. loaded to the right.
C     arranged in 128 columns of 5 dots wide. cols numbered (0:127)
C     and 480 rows of one dot high numbered (0:479)
C     origin (0,0) at bottom left of screen
C     for memory writeout write esc"<XVALUE>;<YVALUE>;<COUNT>;A
C
C---- This routine writes up to 512 bytes or 4 rows at a time.
C     going from left to right and top to bottom on screen.
C     this requires the output of 515 bytes in all because 
C     of the special
C     control codes. so the sys$output stream needs to be opened with
C     a suitably long record length (this program assumes recl=1024).
C
C---- !! the vt640 manual says that sets of zero bytes are sent as a
C      "$" followed by a count of up to 32 in the low six-bits of
C      the output character. but this does not seem to work
C
C---- Put vt640 in graphics mode and clear buffer of vt640
C
      CALL GSGRTM
D     CALL GSBFTM(ITERM,ZERO)
C
C---- Check limits
C
      NRWTOP = NRWMAX
      NRWLOW = NRWMIN
      IF (NRWTOP.GT.479) NRWTOP = 479
      IF (NRWTOP.LT.0) NRWTOP = 0
      IF (NRWLOW.LT.0) NRWLOW = 0
      IF (NRWLOW.GT.NRWTOP) NRWLOW = NRWTOP
      NSENT = NRWTOP - NRWLOW + 1
      IF (NSENT.GT.4) NSENT = 4
      NSENT1 = NSENT - 1
      NRWLOW = NRWTOP - NSENT1
C
C---- Send address load instruction
C     internal write to set up character stream
C     no blanks allowed in the line
C
      NCOL = 0
      WRITE (OUTLIN,FMT=6000) ESC,'"',NCOL,';',NRWTOP,';a'
      NLEN = 11
      WRITE (ITERM,FMT=6002) OUTLIN(1:NLEN)
C
C---- Send lines to vt640
C
      WRITE (ITERM,FMT=6004) ESC,PLUS,
     +  ((SCREEN(I,J),I=0,127),J=NSENT1,0,-1),HASH
C
C---- Format statements
C
 6000 FORMAT (2A1,I3.3,A1,I3.3,A2)
 6002 FORMAT (A)
 6004 FORMAT (4096A1)
C
      END
C
C
C
      SUBROUTINE GSSTRC(TEXT)
C     ========================
C
C---- Plot char string
C
C     A.D.McLachlan JUN 1984. Last updated 16 OCT 84
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were BYTXT, NFONTS
C
C---- Plots a character string from the character*(*) variable
C    (text) without trailing blanks
C     converts illegal ascii characters to blanks (not 32-126)
C     uses the current font
C     uses icentc option to centre characters on cursor
C     but not the string. characters are of size 1.0*1.5
C     character units
C
C---- ENTRY GSSTRS for non-standard size
C
C---- ENTRIES for integer or 'h' format strings
C             GSSTRH(BYTXT,NLETT)  for bytes
C             GSSTR2(ITEXT2,NLETT) for integer*2
C             GSSTR4(ITEXT4,NLETT) for integer*4
C
C     .. Scalar Arguments ..
      REAL SIZX,SIZY
      INTEGER NLETT
      CHARACTER TEXT* (*)
C     ..
C     .. Array Arguments ..
      INTEGER*4 ITEXT4(1)
      INTEGER*2 ITEXT2(1)
      CHARACTER*1 BYTXT(1)
C     ..
C     .. Scalars in Common ..
      REAL ANGFAC,CHANGX,CHANGY,CHRSCX,CHRSCY,CHRSPX,CHRSPY,SCALEX,
     +     SCALEY,USANGX,USANGY,XCHAR,XCSTRT,YCHAR,YCSTRT
      INTEGER ICENTC,IFONT,KPRINT,LUNIN,LUNOUT
      LOGICAL*4 FONTIN,ICULNK,LINMOD,UCSPAC
C     ..
C     .. Arrays in Common ..
      REAL CHRMAT,CUMAT,USRMAT
      INTEGER*2 IFHT,IFSTRT,IFWID,IFX0,IFY0,LENGF
      CHARACTER*1 NFONTS
C     ..
C     .. Local Scalars ..
      REAL CHORGX,CHORGY,CUORGX,CUORGY,XCOFF,XORIG,YCOFF,YORIG
      INTEGER I,IPLOT,LCHAR,LETTER,NLENG
      CHARACTER BLANKC*1
C     ..
C     .. Local Arrays ..
      INTEGER NCTRAN(24),NSTRAN(24)
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL GSGCHC,GSGCHI,GSGCHS
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ICHAR,LEN
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSCHX/CHRMAT(3,3),CHRSCX,CHRSCY,CHANGX,CHANGY,CHRSPX,
     +       CHRSPY,ICENTC,UCSPAC,IFONT,FONTIN,XCHAR,YCHAR,XCSTRT,
     +       YCSTRT,ANGFAC
      COMMON /GSFNT/IFSTRT(150,4),LENGF(150,4),IFX0(150,4),
     +       IFY0(150,4),IFWID(150,4),IFHT(150,4),NFONTS(4,3000,4)
      COMMON /GSUTR/USRMAT(3,3),SCALEX,SCALEY,USANGX,USANGY,
     +       CUMAT(3,3),LINMOD,ICULNK,KPRINT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (XORIG,USRMAT(1,3)), (YORIG,USRMAT(2,3))
      EQUIVALENCE (CUORGX,CUMAT(1,3)), (CUORGY,CUMAT(2,3))
      EQUIVALENCE (NSTRAN(1),USRMAT(1,1))
      EQUIVALENCE (CHORGX,CHRMAT(1,3)), (CHORGY,CHRMAT(2,3))
      EQUIVALENCE (NCTRAN(1),CHRMAT(1,1))
C
      IPLOT = 1
      GO TO 10
C
C
      ENTRY GSSTRS(TEXT,SIZX,SIZY)
C     =============================
C
      IPLOT = 5
   10 CONTINUE
      NLENG = LEN(TEXT)
C
C---- Replace bad characters by blanks
C
      DO 30 I = 1,NLENG
        LCHAR = ICHAR(TEXT(I:I))
        IF ((LCHAR.GT.31) .AND. (LCHAR.LT.127)) GO TO 20
        TEXT(I:I) = BLANKC
   20   CONTINUE
   30 CONTINUE
C
C---- Remove trailing blanks
C
      NLENG = LENSTR(TEXT)
      GO TO 40
C
      ENTRY GSSTRH(BYTXT,NLETT)
C     =========================
C
      IPLOT = 2
      NLENG = NLETT
      GO TO 40
C
      ENTRY GSSTR2(ITEXT2,NLETT)
C     ===========================
C
      IPLOT = 3
      NLENG = NLETT
      GO TO 40
C
      ENTRY GSSTR4(ITEXT4,NLETT)
C     ===========================
C
      IPLOT = 4
      NLENG = NLETT
      GO TO 40
   40 CONTINUE
C
C---- Check for no letters
C
      IF (NLENG.LE.0) THEN
        IF (KPRINT.GE.1) WRITE (LUNOUT,FMT=6000)
        GO TO 140
      END IF
C
C---- Set offsets
C
      IF (ICENTC.EQ.0) THEN
        XCOFF = 0.0
        YCOFF = 0.0
      ELSE
        XCOFF = -0.5
        YCOFF = -0.5
      END IF
C
C---- Plot the string
C
      DO 130 I = 1,NLENG
        GO TO (60,70,80,90,100) IPLOT
   60   CONTINUE
        CALL GSGCHC(TEXT(I:I),XCOFF,YCOFF,IFONT)
        GO TO 120
   70   CONTINUE
        LETTER = ICHAR(BYTXT(I))
        GO TO 110
   80   CONTINUE
        LETTER = ITEXT2(I)
        GO TO 110
   90   CONTINUE
        LETTER = ITEXT4(I)
        GO TO 110
  100   CONTINUE
        CALL GSGCHS(TEXT(I:I),XCOFF,YCOFF,SIZX,SIZY,IFONT)
        GO TO 120
  110   CONTINUE
        CALL GSGCHI(LETTER,XCOFF,YCOFF,IFONT)
  120   CONTINUE
  130 CONTINUE
  140 CONTINUE
      RETURN
C
C---- Format statements
C
 6000 FORMAT (2X,
     +  '!!!GSSTRC/S ERROR: PLOTTING BLANK OR  ZERO LENGTH',
     +  ' STRING ')
C
      END
C
C
C
      SUBROUTINE GSSTRD(TEXT,DX,DY)
C     ==============================
C
C---- Plot char string
C
C     A.D.McLachlan JUL 1984. Last updated 27 JUL 84
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were NFONTS
C
C---- Plot a character string with drawing board separations (mm)
C
C---- ENTRY GSSTRU does same with user unit separations
C
C   Plot the character*(*) variable (text) without trailing blanks
C   use the current font
C   use icentc option to centre characters on cursor
C   but not the string. characters are of size 1.0*1.5
C   character units
C   this routine moves the anchor point for each character
C   and then restores it. the character cursor is reset to zero
C   for each character and not restored.
C
C     .. Scalar Arguments ..
      REAL DX,DY
      CHARACTER TEXT* (*)
C     ..
C     .. Scalars in Common ..
      REAL ANGFAC,CHANGX,CHANGY,CHRSCX,CHRSCY,CHRSPX,CHRSPY,SCALEX,
     +     SCALEY,USANGX,USANGY,XCHAR,XCSTRT,YCHAR,YCSTRT
      INTEGER ICENTC,IFONT,KPRINT,LUNIN,LUNOUT
      LOGICAL*4 FONTIN,ICULNK,LINMOD,UCSPAC
C     ..
C     .. Arrays in Common ..
      REAL CHRMAT,CUMAT,USRMAT
      INTEGER*2 IFHT,IFSTRT,IFWID,IFX0,IFY0,LENGF
      CHARACTER*1 NFONTS
C     ..
C     .. Local Scalars ..
      REAL CHORGX,CHORGY,CUORGX,CUORGY,DXCHAR,DYCHAR,XCHOLD,XCOFF,XCOLD,
     +     XORIG,YCHOLD,YCOFF,YCOLD,YORIG
      INTEGER I,NLENG
C     ..
C     .. Local Arrays ..
      INTEGER NCTRAN(24),NSTRAN(24)
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL GSGCHC
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSCHX/CHRMAT(3,3),CHRSCX,CHRSCY,CHANGX,CHANGY,CHRSPX,
     +       CHRSPY,ICENTC,UCSPAC,IFONT,FONTIN,XCHAR,YCHAR,XCSTRT,
     +       YCSTRT,ANGFAC
      COMMON /GSFNT/IFSTRT(150,4),LENGF(150,4),IFX0(150,4),
     +       IFY0(150,4),IFWID(150,4),IFHT(150,4),NFONTS(4,3000,4)
      COMMON /GSUTR/USRMAT(3,3),SCALEX,SCALEY,USANGX,USANGY,
     +       CUMAT(3,3),LINMOD,ICULNK,KPRINT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (XORIG,USRMAT(1,3)), (YORIG,USRMAT(2,3))
      EQUIVALENCE (CUORGX,CUMAT(1,3)), (CUORGY,CUMAT(2,3))
      EQUIVALENCE (NSTRAN(1),USRMAT(1,1))
      EQUIVALENCE (CHORGX,CHRMAT(1,3)), (CHORGY,CHRMAT(2,3))
      EQUIVALENCE (NCTRAN(1),CHRMAT(1,1))
C
      DXCHAR = DX
      DYCHAR = DY
      GO TO 10
C
      ENTRY GSSTRU(TEXT,DX,DY)
C     =========================
C
      DXCHAR = USRMAT(1,1)*DX + USRMAT(1,2)*DY
      DYCHAR = USRMAT(2,1)*DX + USRMAT(2,2)*DY
C
C---- Save current anchor point
C
   10 XCHOLD = XCHAR
      YCHOLD = YCHAR
C
C---- Zero character cursor
C
      XCSTRT = 0.0
      YCSTRT = 0.0
C
C---- Remove trailing blanks
C
      NLENG = LENSTR(TEXT)
C
C---- Check for no letters
C
      IF (NLENG.GT.0) THEN
C
C---- Set offsets
C
        IF (ICENTC.EQ.0) THEN
          XCOFF = 0.0
          YCOFF = 0.0
        ELSE
          XCOFF = -0.5
          YCOFF = -0.5
        END IF
C
C---- Plot the string
C
        DO 20 I = 1,NLENG
          CALL GSGCHC(TEXT(I:I),XCOFF,YCOFF,IFONT)
          XCHAR = XCHAR + DXCHAR
          YCHAR = YCHAR + DYCHAR
          XCSTRT = 0.0
          YCSTRT = 0.0
   20   CONTINUE
C
C---- Restore anchor point values
C
C
        XCHAR = XCHOLD
        YCHAR = YCHOLD
      ELSE IF (KPRINT.GE.1) THEN
        WRITE (LUNOUT,FMT=6000)
      END IF
C
C---- Format statements
C
 6000 FORMAT (2X,'!!!GSSTRD/U ERROR: PLOTTING BLANK OR  ZERO LENGTH',
     +       ' STRING ')
C
      END
C
C
C
      SUBROUTINE GSSTYL
C     ==================
C
C---- Set plot style constants
C
C     A.D. McLachlan JUL 1984. Last updated 2 OCT 1986
C
C     .. Scalar Arguments ..
      REAL X
      INTEGER ICOLR,LWT,MCOLOR,N
C     ..
C     .. Scalars in Common ..
      REAL ANGFAC,CHANGX,CHANGY,CHRSCX,CHRSCY,CHRSPX,CHRSPY,DOTMMX,
     +     DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWBLMX,DWBLMY,DWLIMX,
     +     DWLIMY,PAPLMX,PAPLMY,UBXMAX,UBXMIN,UBYMAX,UBYMIN,V64LMX,
     +     V64LMY,XCHAR,XCSTRT,YCHAR,YCSTRT
      INTEGER ICENTC,ICOLOR,IDRLVL,IFONT,IPRINT,IUNIT,IXMAX,IXMIN,IYMAX,
     +        IYMIN,LINWT,LUNIN,LUNOUT,MCNTFL,MDEVIC,MDIREC,MIXCOL,MOUT,
     +        MPIC,MSCAFL,NERROR,NPICS
      LOGICAL*4 DEVCON,FONTIN,INITON,UCSPAC
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. Arrays in Common ..
      REAL CHRMAT
C     ..
C     .. Local Scalars ..
      REAL CHORGX,CHORGY,XPAP
      INTEGER IDOT,IEND,IERAS,ILWT,IPAP,IPEN,IVMODE,IX,IY,NCLCNT
C     ..
C     .. Local Arrays ..
      INTEGER NCTRAN(24)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSDOTM,GSERTM,GSFLWI,GSLVCK,GSRVTM,GSVBRK
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,NINT
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSCHX/CHRMAT(3,3),CHRSCX,CHRSCY,CHANGX,CHANGY,CHRSPX,
     +       CHRSPY,ICENTC,UCSPAC,IFONT,FONTIN,XCHAR,YCHAR,XCSTRT,
     +       YCSTRT,ANGFAC
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (CHORGX,CHRMAT(1,3)), (CHORGY,CHRMAT(2,3))
      EQUIVALENCE (NCTRAN(1),CHRMAT(1,1))
C     ..
C     .. Data statements ..
C
C---- Plotfile command codes
C
      DATA IEND/-1/,IDOT/-2/,ILWT/-3/,IPEN/-4/,IPAP/-5/,IERAS/-6/
C
C---- Check level
C
      IF ((IDRLVL.EQ.0) .OR. (IDRLVL.EQ.3)) 
     +            CALL GSLVCK('GSSTYL')
      LINWT = 1
C
C---- Default colour is black
C
      ICOLOR = 1
      MIXCOL = 0
      NCLCNT = 0
      RETURN
C
      ENTRY GSMIXC(MCOLOR)
C     =====================
C
C---- Set for multiple colour plot
C     should use before picture starts to set header
C
      IF ((MCOLOR.LT.0) .OR. (MCOLOR.GT.1)) THEN
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6000) MCOLOR
        MCOLOR = 0
      END IF
      MIXCOL = MCOLOR
      RETURN
C
      ENTRY GSLNWT(LWT)
C     ==================
C
C---- LWT = line weight for all plotting
C
C---- Allowed to call before picture starts
C
      IF ((LWT.LT.1) .OR. (LWT.GT.9)) THEN
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6002) LWT
        LWT = 1
      END IF
      LINWT = LWT
      IF ((IDRLVL.EQ.3) .AND. (MDIREC.NE.1)) THEN
        IX = ILWT
        IY = LWT
        CALL GSFLWI(IX,IY)
      END IF
      RETURN
C
      ENTRY GSCOLR(ICOLR)
C     ====================
C
C---- Allowed to call before picture starts
C
C   Colours for printer system
C      1 Black (yellow+red+blue)
C      2 Red
C      3 Green (yellow+blue)
C      4 Blue
C      5 Yellow
C      6 Orange (yellow+red)
C      7 Purple (red+blue)
C
C---- For device or initialisation not ready
C
      IF ((IDRLVL.EQ.3) .AND. (MDIREC.NE.1)) THEN
C
C---- Ignore if colour is same as already set and has been called before
C
        IF ((NCLCNT.NE.1) .OR. (ICOLR.NE.ICOLOR)) THEN
          ICOLOR = ICOLR
          IX = IPEN
          IY = ICOLR
          CALL GSFLWI(IX,IY)
          NCLCNT = 1
C
C---- Vector break
C
          CALL GSVBRK
        END IF
      END IF
      RETURN
C
      ENTRY GSPAPR(X)
C     ================
C
C---- Send out x mm paper of empty lines 100/inch
C
C---- Set limit of 500.0 on (x) to avoid waste
C
      XPAP = ABS(X)
      IF (XPAP.GT.500.0) XPAP = 500.0
      IF ((IDRLVL.EQ.3) .AND. (MDIREC.NE.1)) THEN
        IX = IPAP
        IY = NINT(XPAP/0.254)
        CALL GSFLWI(IX,IY)
      END IF
      RETURN
C
      ENTRY GSFONT(N)
C     ===============
C
C---- Set character font
C     N = 0  simple capital only font style
C     N = 1  nice block letters   (default)
C     N = 2  italics
C     N = 3  script
C     N = 4  greek
C
      IF ((N.LT.0) .OR. (N.GT.4)) THEN
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6004) N
        N = 1
      END IF
      IFONT = N
      RETURN
C
      ENTRY GSERAS()
C     =============
C
C---- Terminal rubout mode
C
      IVMODE = 1
      GO TO 10
C
      ENTRY GSEREV()
C     =============
C
C---- Terminal reverse complement mode black<>white
C
      IVMODE = 2
      GO TO 10
C
      ENTRY GSERAX()
C     =============
C
C---- Terminal end of rubout or reverse mode
C
      IVMODE = 0
   10 IF (MDIREC.EQ.0) THEN
        IX = IERAS
        IY = IVMODE
        CALL GSFLWI(IX,IY)
C
C---- Vector break
C
        CALL GSVBRK
      END IF
C
      IF ((MDIREC.EQ.1) .AND. (MDEVIC.EQ.3)) THEN
        IF (IVMODE.EQ.0) CALL GSDOTM
        IF (IVMODE.EQ.1) CALL GSERTM
        IF (IVMODE.EQ.2) CALL GSRVTM
C
C---- Vector break
C
        CALL GSVBRK
      END IF
C
C---- Format statements
C
 6000 FORMAT (2X,'!!!GSMIXC ERROR: MCOLR=',I5,' OUT OF RANGE  (0-1)',
     +        ' RESET AS (0)= MONO')
 6002 FORMAT (2X,'!!!GSLNWT ERROR: LINWT=',I5,' OUT OF RANGE  (1-9)',
     +        ' RESET AS 1=THIN ')
 6004 FORMAT (2X,'!!!GSFONT ERROR: IFONT=',I5,' OUT OF RANGE(0-4)',
     +        ' RESET AS 1=BLOCK ')
C
      END
C
C
C
      SUBROUTINE GSSUSP (MILSEC)
C     ============================
C
C---- Real time pause for a given number of millisecs.
C
C     .. Scalar Arguments ..
      INTEGER MILSEC
C     
C     .. Local Scalars ..
      INTEGER START, FINISH
C
C     .. External Statements ..
C      EXTERNAL  UTIMER
C      INTRINSIC ABS
C
C---- UTIMER reads the system clock and returns the milosecond part
C
C      CALL UTIMER (START)
C100   CALL UTIMER (FINISH)
C      IF (ABS(START-FINISH).LT.MILSEC) GOTO 100
C
      END
C
C
C
      SUBROUTINE GSSVTM(ITERM,FILNAM)
C     ================================
C
C---- Vt640 screen saved on a disc file in packed byte form.
C
C     A.D. McLachlan SEP 1984. Last updated 22 SEP 1984.
C     9/2/90 - PJD: Changed definition of several BYTEs to INTEGER
C     They were SCREEN
C
C     ITERM  = fortran unit number for terminal in graphics package
C     FILNAM = filename or logical unit number for output
C
C---- The screen is saved in rows of 128 bytes packed as 5 dots per bit
C     in the lower bits from left to right. 480 rows use 60*1024 bytes
C     the diskio direct access routines are used for writing the screen
C
C     The 4010 memory coords (0-1023 in x, 0-780 in y) 
C     are NOT USED here.
C     This routine uses the physical dot display range
C     (0-639 in x, 0-479 in y).
C     The origin is always at the bottom left hand corner of the screen.
C
C     .. Scalar Arguments ..
      INTEGER ITERM
      CHARACTER FILNAM* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      REAL SECTOT,SSTAGE
      CHARACTER*1 ZERO
      INTEGER ITPRNT,IUNIT,NROW,NRWMAX,NRWMIN,NMCITM
C     ..
C     .. Local Arrays ..
      INTEGER SCREEN(0:127,0:479)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSBFTM,GSGRTM,GSRDTM,GSTIM0,GSTIMR,
     +         QCLOSE,QOPEN,QWRITE
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
      ZERO = CHAR(0)
      ITPRNT = 0
      CALL GSTIM0(ITPRNT)
C
C---- Open output file
C
      CALL QOPEN(IUNIT,FILNAM,'NEW')
      CALL QMODE (IUNIT,0,NMCITM)
      WRITE (LUNOUT,FMT=6002)
C
C---- Put vt640 in graphics mode and clear buffer of vt640
C
      CALL GSGRTM
D     CALL GSBFTM(ITERM,ZERO)
C
C---- Read from vt640 memory
C     5-bit chars + octal 100 in each byte. loaded to the right.
C     arranged in 128 columns of 5 dots wide. cols numbered (0:127)
C     and 480 rows of one dot high numbered (0:479)
C     origin (0,0) at bottom left of screen
C
      NRWMIN = 0
      NRWMAX = 479
      CALL GSRDTM(ITERM,NRWMIN,NRWMAX,SCREEN)
C
C---- Start writing rows
C
      DO 10 NROW = NRWMAX,NRWMIN,-4
        CALL QWRITE(IUNIT,SCREEN(0, (NROW-3)),512)
   10 CONTINUE
C
C---- Close disc file
C
      CALL QCLOSE(IUNIT)
      CALL GSTIMR(SECTOT,SSTAGE,ITPRNT)
      WRITE (LUNOUT,FMT=6000) SECTOT
C
C---- Format statements
C
 6000 FORMAT (2X,'GSSVTM: Save complete . cpu sec = ',F8.2)
 6002 FORMAT (2X,'GSSVTM: Started ')
C
      END
C
C
C
      SUBROUTINE GSSWLN(X1,Y1,X2,Y2,NCODE1,NCODE2,NSWAP)
C     ===================================================
C
C     A.D. McLachlan JUN 1984 Last updated 10 JUL 1984
C
C---- Swaps ends of lines in clipping routine
C
C     .. Scalar Arguments ..
      REAL X1,X2,Y1,Y2
      INTEGER NCODE1,NCODE2,NSWAP
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      REAL X,Y
      INTEGER N
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
      NSWAP = -NSWAP
      X = X1
      X1 = X2
      X2 = X
      Y = Y1
      Y1 = Y2
      Y2 = Y
      N = NCODE1
      NCODE1 = NCODE2
      NCODE2 = N
C
      END
C
C
C
      SUBROUTINE GSTIMR(SECTOT,SSTAGE,ITPRNT)
C     ========================================
C
C---- Plot84 version of "timer" and "time0"
C
C     A.D. McLachlan NOV 1984. Last updated 15 JAN 1986
C
C     .. Scalar Arguments ..
      REAL SECTOT,SSTAGE
      INTEGER ITPRNT
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      REAL SEC,SEC0,SEC1
      INTEGER ISEC,ITIME
C     ..
C     .. External Functions ..
C      INTEGER LIB$INIT_TIMER,LIB$STAT_TIMER
C      EXTERNAL LIB$INIT_TIMER,LIB$STAT_TIMER
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC REAL
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
      ITIME = 0 
      ISEC = 0
Cvax   ITIME = LIB$STAT_TIMER(2,ISEC)
      SEC = REAL(ISEC)/100.0 - SEC0
      SECTOT = SEC
      SSTAGE = SEC - SEC1
      SEC1 = SEC
      IF (ITPRNT.EQ.1) WRITE (LUNOUT,FMT=6000) SECTOT,SSTAGE
      RETURN
C
      ENTRY GSTIM0(ITPRNT)
C     =====================
C
      ITIME = 0
Cvax  ITIME = LIB$INIT_TIMER()
      ITIME = 0
      ISEC = 0
Cvax  ITIME = LIB$STAT_TIMER(2,ISEC)
      SEC0 = REAL(ISEC)/100.0
      SEC1 = 0.0
      IF (ITPRNT.EQ.1) WRITE (LUNOUT,FMT=6002)
C
C---- Format statements
C
 6000 FORMAT (1X,'-- TIME -- FOR JOB STEP ',F8.2,'  THIS STAGE ',F8.2)
 6002 FORMAT (1X,'-- TIME -- STARTED AT 0.0 SECS !!')
C
      END
C
C
C
      SUBROUTINE GSTMPY(TA,TB,TAB)
C     =============================
C
C---- Multiplies together two plane transformation matrices
C
C     A.D. McLachlan JUL 1984. Last updated 9 JULY 1984.
C
C---- Each transformation is of the type
C      ( X2 )   (   A   B   U  )   ( X1 )
C      ( Y2 ) * (   C   D   V  ) * ( Y1 )
C      (1.0 )   (  0.0 0.0 1.0 )   (1.0 )
C
C     .. Array Arguments ..
      REAL TA(3,3),TAB(3,3),TB(3,3)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      INTEGER I,J
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
      DO 20 J = 1,3
        DO 10 I = 1,3
          TAB(I,J) = TA(I,1)*TB(1,J) + TA(I,2)*TB(2,J) + TA(I,3)*TB(3,J)
   10   CONTINUE
   20 CONTINUE
C
      END
C
C
C
      SUBROUTINE GSTSAV(NTRSAV)
C     ==========================
C
C---- save plot transformation and postion variables
C
C     A.D. McLachlan JUL 1984. Last updated 27 JUL 1984
C
C---- definitions of common blocks
C   <<GSUTR>>      ((user transformation variables))
C       USRMAT(3,3)   =  user scaling matrix:
C           XDRW=USRMAT(1,1)*XU+USRMAT(1,2)*YU + XORIG
C           YDRW=USRMAT(2,1)*XU+USRMAT(2,2)*YU + YORIG
C                  XORIG:=: USRMAT(1,3) YORIG:=: USRMAT(2,3)
C                XORIG,YORIG = user origin on drawing board (mm)
C
C       SCALEX,SCALEY =  mm equiv of 1 user unit along local x,y
C       USANGX,USANGY =  angles of local user axes (radians)
C       CUMAT(3,3)    =  character*user scaling matrix
C                  CUORGX:=:CUMAT(1,3) CUORGY:=:CUMAT(2,3)
C                  CUORGX,CUORGY =  character*user origin value
C       LINMOD        =  .TRUE. for line mode
C       ICULNK        =  .TRUE. for user char trans linked
C       NSTRAN(24)    =  array equivalent to (usrmat...iculnk)
C       KPRINT        =  print control, copy of iprint for some
C                        routines
C
C    <<GSCHX>>     ((character position variables))
C       CHRMAT(3,3)   =  character transformation matrix
C                CHORGX:=:CHRMAT(1,3) CHORGY:=:CHRMAT(2,3)
C                CHORGX,CHORGY =  character origin in char units
C                                 relative to (xchar,ychar)
C       CHRSCX,CHRSCY =  char width and height in character units
C                        (default=3.0)
C       CHANGX,CHANGY =  angles of char x and y (radians)
C       CHRSPX,CHRSPY =  character space extra values
C       ICENTC        =  character centering control
C       UCSPAC        =  .true. for uniform spacing
C       IFONT         =  font number
C       FONTIN        =  .true. after font tables read in
C       XCHAR,YCHAR   =  character anchor point
C       XCSTRT,YCSTRT =  character cursor position 
C                        relative to anchor point
C                        in character units on character grid
C       ANGFAC        =  pi/180.0 degrees to radians
C       NCTRAN(24)    =  array equivalent to (chrmat...angfac)
C
C---- Copy common note that kprint is not saved
C
C     .. Array Arguments ..
      INTEGER NTRSAV(48)
C     ..
C     .. Scalars in Common ..
      REAL ANGFAC,CHANGX,CHANGY,CHRSCX,CHRSCY,CHRSPX,CHRSPY,SCALEX,
     +     SCALEY,USANGX,USANGY,XCHAR,XCSTRT,YCHAR,YCSTRT
      INTEGER ICENTC,IFONT,KPRINT,LUNIN,LUNOUT
      LOGICAL*4 FONTIN,ICULNK,LINMOD,UCSPAC
C     ..
C     .. Arrays in Common ..
      REAL CHRMAT,CUMAT,USRMAT
C     ..
C     .. Local Scalars ..
      REAL CHORGX,CHORGY,CUORGX,CUORGY,XORIG,YORIG
      INTEGER I
C     ..
C     .. Local Arrays ..
      INTEGER NCTRAN(24),NSTRAN(24)
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSCHX/CHRMAT(3,3),CHRSCX,CHRSCY,CHANGX,CHANGY,
     +       CHRSPX,CHRSPY,ICENTC,UCSPAC,IFONT,FONTIN,XCHAR,YCHAR,
     +       XCSTRT,YCSTRT,ANGFAC
      COMMON /GSUTR/USRMAT(3,3),SCALEX,SCALEY,USANGX,USANGY,
     +       CUMAT(3,3),LINMOD,ICULNK,KPRINT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (XORIG,USRMAT(1,3)), (YORIG,USRMAT(2,3))
      EQUIVALENCE (CUORGX,CUMAT(1,3)), (CUORGY,CUMAT(2,3))
      EQUIVALENCE (NSTRAN(1),USRMAT(1,1))
      EQUIVALENCE (CHORGX,CHRMAT(1,3)), (CHORGY,CHRMAT(2,3))
      EQUIVALENCE (NCTRAN(1),CHRMAT(1,1))
C
      DO 10 I = 1,24
        NTRSAV(I) = NSTRAN(I)
   10 CONTINUE
      DO 20 I = 1,24
        NTRSAV(24+I) = NCTRAN(I)
   20 CONTINUE
      RETURN
C
      ENTRY GSTRES(NTRSAV)
C     =====================
C
C---- Restore common
C
      DO 30 I = 1,24
        NSTRAN(I) = NTRSAV(I)
   30 CONTINUE
      DO 40 I = 1,24
        NCTRAN(I) = NTRSAV(24+I)
   40 CONTINUE
C
      END
C
C
C
      SUBROUTINE GSTXT(BUFFER)
C     =========================
C
C---- Write character string BUFFER to AED
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were LINE
C
C     .. Scalar Arguments ..
      CHARACTER BUFFER* (*)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      INTEGER I,IC,J,K
C     ..
C     .. Local Arrays ..
      CHARACTER*1 LINE(130)
C     ..
C     .. External Subroutines ..
      EXTERNAL AEDTXT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ICHAR,LEN,CHAR
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
C---- Get length of string
C
      J = LEN(BUFFER)
C
C---- Find last non-blank character
C
      DO 10 I = J,1,-1
        IF (BUFFER(I:I).NE.' ') GO TO 20
   10 CONTINUE
      RETURN
C
C---- Copy string to local array to allow room
C     to insert cr lf at end
C
   20 K = 0
      DO 30 J = 1,I
        IC = ICHAR(BUFFER(J:J))
        K = K + 1
        IF (IC.EQ.35) THEN
C
C---- Character is # - Replace character # by cr lf
C
        LINE(K)= CHAR(13)
        LINE(K+1)= CHAR(10)
          K = K + 1
        ELSE
          LINE(K) = CHAR(IC)
        END IF
   30 CONTINUE
      LINE(K+1)= CHAR(13)
      LINE(K+2)= CHAR(10)
      CALL AEDTXT(LINE,K+2)
C
      END
C
C
C
      SUBROUTINE GSTYON
C     ==================
C
C---- Change terminal to or from teletype mode in interactive plotting
C
C     A.D. McLachlan SEP 1984. Last updated 3 OCT 1984
C
C     .. Scalars in Common ..
      REAL DOTMMX,DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWBLMX,DWBLMY,
     +     DWLIMX,DWLIMY,PAPLMX,PAPLMY,UBXMAX,UBXMIN,UBYMAX,UBYMIN,
     +     V64LMX,V64LMY
      INTEGER ICOLOR,IDRLVL,IPRINT,IUNIT,IXMAX,IXMIN,IYMAX,IYMIN,LINWT,
     +        LUNIN,LUNOUT,MCNTFL,MDEVIC,MDIREC,MIXCOL,MOUT,MPIC,MSCAFL,
     +        NERROR,NPICS
      LOGICAL*4 DEVCON,INITON
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. External Subroutines ..
      EXTERNAL GSCYTM,GSGRTM,GSMYTM,GSTYTM,GSVBRK
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
C
C     .. Save Statement ..
C
      SAVE
C
      IF ((MDIREC.NE.0) .AND. (DEVCON)) THEN
C
C---- switch terminal mode to teletype
C     cursor at bottom left. erase previous writing
C
        CALL GSTYTM
        CALL GSCYTM
        CALL GSMYTM(0,23)
      END IF
      RETURN
C
      ENTRY GSTYOF()
C     =============
C
C---- Switch to vector graph mode
C
      IF ((MDIREC.NE.0) .AND. (DEVCON)) THEN
        CALL GSGRTM
C
C---- break old vector chain
C
        CALL GSVBRK
      END IF
C
      END
C
C
C
      SUBROUTINE GSUTRN
C     ==================
C
C---- set up user transformation defaults. other entries to rescale
C     adapted from d.a. agard plot82
C
C     A.D. McLachlan JUN 1984. Last updated 18 SEPT 1984
C
C     .. Scalar Arguments ..
      REAL A,B,X,XCGAP,Y,YCDOWN
      INTEGER N
C     ..
C     .. Array Arguments ..
      REAL TMAT(2,2)
C     ..
C     .. Scalars in Common ..
      REAL ANGFAC,CHANGX,CHANGY,CHRSCX,CHRSCY,CHRSPX,CHRSPY,DOTMMX,
     +     DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWBLMX,DWBLMY,DWLIMX,
     +     DWLIMY,PAPLMX,PAPLMY,SCALEX,SCALEY,UBXMAX,UBXMIN,UBYMAX,
     +     UBYMIN,USANGX,USANGY,V64LMX,V64LMY,XBNEW,XBOLD,XCHAR,XCSTRT,
     +     XNOW,YBNEW,YBOLD,YCHAR,YCSTRT,YNOW
      INTEGER ICENTC,ICOLOR,IDRLVL,IFONT,IPRINT,IUNIT,IXMAX,IXMIN,IYMAX,
     +        IYMIN,KPRINT,LINWT,LUNIN,LUNOUT,MCNTFL,MDEVIC,MDIREC,
     +        MIXCOL,MOUT,MPIC,MSCAFL,NERROR,NPICS
      LOGICAL*4 DEVCON,FONTIN,ICULNK,INITON,LINMOD,UCSPAC
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. Arrays in Common ..
      REAL CHRMAT,CUMAT,USRMAT
C     ..
C     .. Local Scalars ..
      REAL CHORGX,CHORGY,CUORGX,CUORGY,PI,XORIG,YORIG
      INTEGER I,ICLINK,ICMODE,J
C     ..
C     .. Local Arrays ..
      INTEGER NCTRAN(24),NSTRAN(24)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSLVCK,GSMUCT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ATAN2,COS,SIN,SQRT
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSCHX/CHRMAT(3,3),CHRSCX,CHRSCY,CHANGX,CHANGY,
     +       CHRSPX,CHRSPY,ICENTC,UCSPAC,IFONT,FONTIN,XCHAR,YCHAR,
     +       XCSTRT,YCSTRT,ANGFAC
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
      COMMON /GSDWX/XNOW,YNOW,XBNEW,YBNEW,XBOLD,YBOLD
      COMMON /GSUTR/USRMAT(3,3),SCALEX,SCALEY,USANGX,USANGY,
     +       CUMAT(3,3),LINMOD,ICULNK,KPRINT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (XORIG,USRMAT(1,3)), (YORIG,USRMAT(2,3))
      EQUIVALENCE (CUORGX,CUMAT(1,3)), (CUORGY,CUMAT(2,3))
      EQUIVALENCE (NSTRAN(1),USRMAT(1,1))
      EQUIVALENCE (CHORGX,CHRMAT(1,3)), (CHORGY,CHRMAT(2,3))
      EQUIVALENCE (NCTRAN(1),CHRMAT(1,1))
C
C---- check level
C
      IF (IDRLVL.EQ.0) CALL GSLVCK('GSUTRN')
      IF (IDRLVL.LT.2) IDRLVL = 2
C
C---- set user transformations
C
      PI = ATAN2(1.0,1.0)*4.0
      DO 20 J = 1,3
        DO 10 I = 1,3
          USRMAT(I,J) = 0.0
          CHRMAT(I,J) = 0.0
          CUMAT(I,J) = 0.0
   10   CONTINUE
   20 CONTINUE
      USRMAT(3,3) = 1.0
      CHRMAT(3,3) = 1.0
      CUMAT(3,3) = 1.0
      SCALEX = 1.0
      SCALEY = 1.0
      CHRSCX = 3.0
      CHRSCY = 3.0
      USRMAT(1,1) = SCALEX
      USRMAT(2,2) = SCALEY
      CHRMAT(1,1) = CHRSCX
      CHRMAT(2,2) = CHRSCY
      USANGX = 0.0
      USANGY = PI/2.0
      CHANGX = 0.0
      CHANGY = PI/2.0
      CHRSPX = 0.0
      CHRSPY = 0.0
      ICULNK = .TRUE.
      ICENTC = 0
      UCSPAC = .FALSE.
      IFONT = 1
      LINMOD = .TRUE.
C
C---- anchor point and cursor
C
      XCHAR = 0.0
      YCHAR = 0.0
      XCSTRT = 0.0
      YCSTRT = 0.0
      GO TO 60
C
      ENTRY GSSCLU(A,B)
C     ==================
C
C---- (A,B)   horizontal and vertical scales.
C             1 user unit=(a,b) mm scales are user/drawing board
C
      SCALEX = A
      SCALEY = B
      GO TO 40
C
      ENTRY GSUROT(A,B)
C     =================
C
C---- Set rotation of user coord system in radians anti-clockwise
C     with x and y axes along directions of angles (a,b)
C
      USANGX = A
      USANGY = B
      GO TO 40
C
      ENTRY GSUMAT(TMAT)
C     ==================
C
C---- Reset transformation matrix (values in mm)
C     since this call destroys the current scales we recalculate them
C
      USRMAT(1,1) = TMAT(1,1)
      USRMAT(2,1) = TMAT(2,1)
      USRMAT(1,2) = TMAT(1,2)
      USRMAT(2,2) = TMAT(2,2)
      SCALEX = SQRT(TMAT(1,1)**2+TMAT(2,1)**2)
      SCALEY = SQRT(TMAT(1,2)**2+TMAT(2,2)**2)
      IF (SCALEX.EQ.0) THEN
        USANGX = 0.0
      ELSE
        USANGX = ATAN2(TMAT(2,1),TMAT(1,1))
      END IF
C
      IF (SCALEY.EQ.0) THEN
        USANGY = 0.0
      ELSE
        USANGY = ATAN2(TMAT(2,2),TMAT(1,2))
      END IF
      GO TO 50
C
      ENTRY GSORGD(X,Y)
C     ==================
C
C---- Set up user origin on drawing board in mm
C
      XORIG = X
      YORIG = Y
      GO TO 50
C
      ENTRY GSORGU(X,Y)
C     =================
C
C---- Set up user origin in terms of current user coords
C
      XORIG = USRMAT(1,1)*X + XORIG + USRMAT(1,2)*Y
      YORIG = USRMAT(2,1)*X + YORIG + USRMAT(2,2)*Y
      GO TO 50
C
      ENTRY GSSCLC(A,B)
C     ==================
C
C---- Set values of chrscx,chrscy
C
      CHRSCX = A
      CHRSCY = B
      GO TO 30
C
      ENTRY GSCROT(A,B)
C     ==================
C
C---- Set angles for character coordinate system in radians
C     anticlockwise with x and y axes along directions of (a,b)
C
      CHANGX = A
      CHANGY = B
      GO TO 30
C
      ENTRY GSCMAT(TMAT)
C     ===================
C
C---- Reset character transformation matrix
C
      CHRMAT(1,1) = TMAT(1,1)
      CHRMAT(1,2) = TMAT(1,2)
      CHRMAT(2,1) = TMAT(2,1)
      CHRMAT(2,2) = TMAT(2,2)
      CHRSCX = SQRT(TMAT(1,1)**2+TMAT(2,1)**2)
      CHRSCY = SQRT(TMAT(1,2)**2+TMAT(2,2)**2)
      IF (CHRSCX.EQ.0) THEN
        CHANGX = 0.0
      ELSE
        CHANGX = ATAN2(TMAT(2,1),TMAT(1,1))
      END IF
C
      IF (CHRSCY.EQ.0) THEN
        CHANGY = 0.0
      ELSE
        CHANGY = ATAN2(TMAT(2,2),TMAT(1,2))
      END IF
      GO TO 50
C
      ENTRY GSORGC(X,Y)
C     ==================
C
C---- Set up origin of character transformation in character units
C
      CHORGX = X
      CHORGY = Y
      GO TO 50
C
      ENTRY GSCSPA(A,B)
C     ==================
C
C---- (A,B)=values of extra character space,in character units
C
      CHRSPX = A
      CHRSPY = B
      RETURN
C
      ENTRY GSCSPU(N)
C     ================
C
C---- (N)=iuspce iuspce=(0) variable spacing, (1)fixed
C
      IF ((N.LT.0) .OR. (N.GT.1)) THEN
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6016) N
        N = 0
      END IF
C
      IF (N.EQ.0) THEN
        UCSPAC = .FALSE.
      ELSE
        UCSPAC = .TRUE.
      END IF
      RETURN
C
      ENTRY GSCENC(N)
C     ================
C
C---- Set relative character position
C     N = 0  lower left hand corner (default)
C     N = 1  centred
C
      IF ((N.LT.0) .OR. (N.GT.1)) THEN
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6004) N
        N = 0
      END IF
      ICENTC = N
      RETURN
C
      ENTRY GSANCD(X,Y)
C     ==================
C
C---- Set up character anchor point on drawing board in mm
C
      XCHAR = X
      YCHAR = Y
      XCSTRT = 0.0
      YCSTRT = 0.0
      RETURN
C
      ENTRY GSANCU(X,Y)
C     ==================
C
C---- Set up character anchor point in user space
C
      XCHAR = USRMAT(1,1)*X + USRMAT(1,2)*Y + XORIG
      YCHAR = USRMAT(2,1)*X + USRMAT(2,2)*Y + YORIG
      XCSTRT = 0.0
      YCSTRT = 0.0
      RETURN
C
      ENTRY GSFCUR(X,Y)
C     ==================
C
C---- Report position of character cursor
C
      X = XCSTRT
      Y = YCSTRT
      RETURN
C
      ENTRY GSPCUR(X,Y)
C     ==================
C
C---- Set position of character cursor
C
      XCSTRT = X
      YCSTRT = Y
      RETURN
C
      ENTRY GSSCUR(XCGAP)
C     ====================
C
C---- Move cursor to right by xcgap
C
      XCSTRT = XCSTRT + XCGAP
      RETURN
C
      ENTRY GSLNFD(YCDOWN)
C     =====================
C
C---- Character cursor line feed
C
      XCSTRT = 0
      YCSTRT = YCSTRT - YCDOWN
      RETURN
C
      ENTRY GSTLNK(N)
C     ================
C
C---- Set linking of plot transformations
C
      IF ((N.LT.0) .OR. (N.GT.1)) THEN
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6000) N
        N = 1
      END IF
      ICLINK = N
      IF (ICLINK.EQ.1) THEN
        ICULNK = .TRUE.
      ELSE
        ICULNK = .FALSE.
      END IF
      RETURN
C
      ENTRY GSCMOD(N)
C     ===============
C
      IF ((N.LT.0) .OR. (N.GT.1)) THEN
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6002) N
        N = 0
      END IF
      ICMODE = N
      IF (ICMODE.EQ.1) THEN
        LINMOD = .FALSE.
      ELSE
        LINMOD = .TRUE.
      END IF
      RETURN
   30 CHRMAT(1,1) = COS(CHANGX)*CHRSCX
      CHRMAT(2,1) = SIN(CHANGX)*CHRSCX
      CHRMAT(1,2) = COS(CHANGY)*CHRSCY
      CHRMAT(2,2) = SIN(CHANGY)*CHRSCY
      GO TO 50
   40 USRMAT(1,1) = COS(USANGX)*SCALEX
      USRMAT(2,1) = SIN(USANGX)*SCALEX
      USRMAT(1,2) = COS(USANGY)*SCALEY
      USRMAT(2,2) = SIN(USANGY)*SCALEY
C
C---- check level
C
   50 IF (IDRLVL.NE.3) CALL GSLVCK('GS/UCTRN/')
   60 CALL GSMUCT(USRMAT,CHRMAT,CUMAT,ICULNK,IPRINT)
      IF (IPRINT.GE.2) THEN
        WRITE (LUNOUT,FMT=6006) USRMAT(1,1),USRMAT(1,2),XORIG,
     +    USRMAT(2,1),USRMAT(2,2),YORIG
        WRITE (LUNOUT,FMT=6008) SCALEX,SCALEY,USANGX/ANGFAC,
     +    USANGY/ANGFAC
        WRITE (LUNOUT,FMT=6010) CHRMAT(1,1),CHRMAT(1,2),CHORGX,
     +    CHRMAT(2,1),CHRMAT(2,2),CHORGY
        WRITE (LUNOUT,FMT=6012) CHRSCX,CHRSCY,CHANGX/ANGFAC,
     +    CHANGY/ANGFAC
      END IF
C
C---- Format statements
C
 6000 FORMAT (2X,'!!!GSTLNK ERROR: ICLNK = ',I5,' NOT (0,1) - RESET',
     +       ' TO (1)=LINKED ')
 6002 FORMAT (2X,'!!!GSCMOD ERROR: ICMODE = ',I5,' NOT (0,1) - RESET',
     +        'TO (0)=LINE-MODE ')
 6004 FORMAT (2X,'!!!GSCENC ERROR: N=',I5,' OUT OF RANGE(0-1)  RESET',
     +         'AS 0=LOWLEFT ')
 6006 FORMAT (2X,'USRMAT(1,1),(1,2) XORIG  = ',3F10.4,/2X,'USRMAT(2,1)',
     +       ',(2,2) YORIG  = ',3F10.4)
 6008 FORMAT (2X,'SCALEX SCALEY USANGX USANGY(DEG) = ',4F10.4)
 6010 FORMAT (2X,'CHRMAT(1,1),(1,2) CHORGX = ',3F10.4,/2X,'CHRMAT(2,1)',
     +       ',(2,2) CHORGY = ',3F10.4)
 6012 FORMAT (2X,'CHRSCX CHRSCY CHANGX CHANGY (DEG) = ',4F10.4)
 6016 FORMAT (2X,'!!!GSCSPU ERROR: IUSPCE=',I5,' NOT (0,1) RESET',
     +        ' AS 0 = VARIABLE SPACING ')
C
      END
C
C
C
      SUBROUTINE GSVCLN(XVEC,YVEC,NPOINT)
C     ====================================
C
C---- Plot a set of npoint points joined by lines
C
C     A.D. McLachlan JUN 1984. Last updated 25 JUL 1984
C
C     .. Scalar Arguments ..
      INTEGER NPOINT
C     ..
C     .. Array Arguments ..
      REAL XVEC(NPOINT),YVEC(NPOINT)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      INTEGER J
C     ..
C     .. External Subroutines ..
      EXTERNAL GSDWTO,GSMVTO
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
      CALL GSMVTO(XVEC(1),YVEC(1))
      IF (NPOINT.GE.2) THEN
        DO 10 J = 2,NPOINT
          CALL GSDWTO(XVEC(J),YVEC(J))
   10   CONTINUE
      END IF
C
      END
C
C
C
      SUBROUTINE GSVIEW(FILNAM,NSTART,ISCALE,SCAFAC,LINSIZ,
     +                    NOCENT,NOPRNT,NINTER,IERR)
C     =========================================================
C
C---- Runs interactive graphics on the vt640 terminal
C
C     A.D. McLachlan JUNE 1984. Last updated 13 MAR 1985
C
C     adapted from d.a. agard's "view" program from plot82
C
C     FILNAM is name of intermediate plot file to read from
C     ISCALE=1,2,3 (1) uniform to fill screen
C                  (2) chosen multiple of paper scale
C                  (3) independent x,y to fill screen
C     SCAFAC       scale factor (screenscale/paperscale)
C                  default is 0.5 (only for iscale=2)
C     NOCENT=0,1   do, do not centre picture (only for iscale=1,2)
C     LINSIZ=0,1-9 (0)use original thickness (1-9) use value set
C     NSTART       number of picture to start at in plot file
C                  if nstart.lt.1 program asks user for value
C     NINTER=1,0   interactive use or not
C                  ninter= -1 then do not read filnam or ninter
C     NOPRNT=0,1   print on,off
C     IERR=0,1     normal function or error return
C
C     vt640 machine has raster of physical dots (640*480)
C     and xerox monitor plots at 10 physical dots/3mm
C     with full size (192.0,144.0)mm or (7.55906,5.66929)inches
C     vt640 programming uses raster of memory dots (0:1023 * 0:779)
C     and then the range (1024,780) is scaled down by factors
C     1024*(5/8)=640  780*(8/13)=480.
C
C     the paper plotter used by plot82 is 13 inches across and 20 in.
C     deep with 100 paper dots per inch. so 1300 (1296 used) across
C     and 2000 down. "nbpi" is the notional number of paper dots per
C     inch used in the plot file (normally 100), and is used by
C     this program to do the scaling from paper dots to vt640 memory
C     dots
C     the paper plotter actually has 1320 dots (13.2 inches) across
C     but normally only 1296 are used.
C
C     this program leaves a margin at the edge of the screen and
C     uses screen area size (550*465) physical dots (165.0*139.5)mm
C     (6.5*5.5) inches with margins left 60=(0:59) bottom 8=(0:7)
C     physical user area is (60:609 * 8:472) dots
C       corresponding area in memory dots is (880*756) with margins
C     left 96=(0:95) bottom 13=(0:12)
C     memory user area (96:975 * 13:768)
C     with this convention default scale is that
C     1 paper mm scales to 0.5 screen mm
C     scales are across 880/13=67.692 memdots/paper inch
C                up     756/11=68.727 memdots/paper inch
C
C     .. Parameters ..
      REAL DTMMX,DTMMY
      PARAMETER (DTMMX=880.0/165.0,DTMMY=756.0/139.5)
C     ..
C     .. Scalar Arguments ..
      REAL SCAFAC
      INTEGER IERR,ISCALE,LINSIZ,NINTER,NOCENT,NOPRNT,NSTART
      CHARACTER FILNAM* (*)
C     ..
C     .. Scalars in Common ..
      REAL DOTMMX,DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWLIMX,DWLIMY
      INTEGER ICOLOR,IUNITR,IXMAX,IXMIN,IYMAX,IYMIN,LINWT,LUNIN,LUNOUT,
     +        MCNTFL,MDEVIC,MDIREC,MIXCOL,MOUT,MPIC,MSCAFL,NPICS,NREC
      CHARACTER PASWRD*8,TITLEH*80
C     ..
C     .. Arrays in Common ..
      REAL SPARE1,SPARE2
C     ..
C     .. Local Scalars ..
      REAL AXFAC,AXRNGE,AYFAC,AYRNGE,SCALX,SCALY,SCMAXP,SCMAXX,SCMAXY,
     +     SIZX,SIZX1,SIZY,SIZY1,SPAREX,SPAREY,TRUSCX,TRUSCY,XMAX,XMIN,
     +     XSCRM1,XWID,YMAX,YMIN,YSCRM1,YWID
      INTEGER IDOT,IERAS,IND,IOUT,ISIZX1,ISIZY1,ISXMAX,ISXMIN,ISYMAX,
     +        ISYMIN,IUNIT,IX,IXBIG,IXDRAW,IXEDG,IXMOVE,IXMRG1,IXOFF,
     +        IXOLD,IXSPAR,IY,IYBIG,IYDRAW,IYEDG,IYMOVE,IYMRG1,IYOFF,
     +        IYOLD,IYSPAR,J,JINIT,JPIC,JSKIP,KEOF,KKEOF,LL,MORE,NCONTR,
     +        NDOT,NSKIP
      INTEGER*2 IEND,ILWT,IPAP,IPEN
      CHARACTER CHKEY*1
C     ..
C     .. Local Arrays ..
      INTEGER LOFFX(8),LOFFY(8)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSCFIL,GSCYTM,GSDOTM,GSDWTM,GSERTM,
     +         GSFLBR,GSFLP1,GSFLRI,GSFLSR,GSGRTM,
     +         GSINTM,GSMVTM,GSMYTM,GSOFLR,GSOFTM,
     +         GSPTTM,GSRHDR,GSRVTM,GSSCTM,GSTYTM
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MIN,NINT,REAL
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSFHD/IUNITR,NREC,DOTMMX,DOTMMY,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,
     +       MCNTFL,DWLIMX,DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,NPICS,
     +       PASWRD,SPARE1(15),TITLEH,SPARE2(68)
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Data statements ..
C
C---- screen size 880 by 756 for iscale=2
C
C---- screen left and bottom margins 96,13 mem dots
C
      DATA ISIZX1/879/,ISIZY1/755/
      DATA SIZX1/880.0/,SIZY1/756.0/
      DATA IXMRG1/96/,IYMRG1/13/
      DATA XSCRM1/165.0/,YSCRM1/139.5/
      DATA ISXMIN/5/,ISXMAX/1018/,ISYMIN/5/,ISYMAX/775/
C
C---- plot file command codes
C
      DATA IEND/-1/,IDOT/-2/,ILWT/-3/,IPEN/-4/,IPAP/-5/,IERAS/-6/
      DATA LOFFX/1,0,-1,0,2,0,-2,0/
      DATA LOFFY/0,1,0,-1,0,2,0,-2/
      DATA CHKEY/' '/
C
C---- scale factors 880/165.0 and 756/139.5
C
C---- screen size in mm
C
C---- edge dot limits for marking edge of screen
C
C---- plot header contains 512 bytes or 128 words of header
C     iunitr is not part of the header, 
C     but is used by the GSfile routines
C     information in an array in this common block
C       WORD(1)     :=: NREC                INTEGER
C       WORD(2:3)   :=: DOTMMX,DOTMMY       REALS
C       WORD(4:16)  :=: (IXMIN:MCNTFL)      INTEGER
C       WORD(17:22) :=: (DWLIMX:DVYMAX)     REALS
C       WORD(23)    :=: NPICS               INTEGER
C       WORD(24:25) :=: PASWRD              'PLOT%%84'
C       WORD(26:40) :=: SPARE1(1:17)        SPARE
C       WORD(41:60) :=: TITLEH(1:80)        CHARACTERS
C       WORD(61:128):=: SPARE2(1:68)        SPARE
C
C
      JPIC = 0
      JINIT = 0
      NSKIP = NSTART - 1
      NCONTR = 0
      IF (NOPRNT.EQ.0) WRITE (LUNOUT,FMT=6052) NINTER,NOPRNT,FILNAM,
     +    NSTART,ISCALE,SCAFAC,NOCENT,LINSIZ
C
C---- get filename and set options
C
      IF (NINTER.NE.1) GO TO 10
      CALL NOCRLF ('+Type full name of plot file:-........=$')
      READ (LUNIN,FMT=6056) FILNAM
   10 CONTINUE
      IF (NINTER.EQ.-1) THEN
        NINTER = 1
      END IF
      IF ((NINTER.EQ.0) .AND. (NOPRNT.EQ.0)) WRITE (LUNOUT,
     +    FMT=6028) FILNAM
      IF (NINTER.EQ.0) GO TO 30
   20 CONTINUE
      CALL NOCRLF ('+VIEW640: Auto(0) or interactive(1)...=$')
      READ (LUNIN,FMT=6012) NCONTR
      IF ((NCONTR.EQ.0) .OR. (NCONTR.EQ.1)) GO TO 30
      WRITE (LUNOUT,FMT=6050)
      GO TO 20
   30 CONTINUE
      IF ((NCONTR.EQ.1) .OR. (NINTER.EQ.0)) GO TO 40
C
C---- set for auto options if online otherwise use options as called
C
      NSTART = 1
      ISCALE = 1
      SCAFAC = 0.5
      LINSIZ = 0
      NOCENT = 0
      NOPRNT = 1
   40 CONTINUE
      IF ((NCONTR.EQ.0) .OR. (NINTER.EQ.0)) GO TO 60
   50 CONTINUE
C
C---- read print switch
C
      CALL NOCRLF ('+Noprnt: <0>Print on (1)Print off.....=$')
      READ (LUNIN,FMT=6012) NOPRNT
C
      IF ((NINTER.EQ.0) .OR. (NOPRNT.EQ.1)) GO TO 60
      WRITE (LUNOUT,FMT=6050)
      GO TO 50
   60 CONTINUE
C
C---- output unit vt640 terminal
C
      IOUT = 7
C
C---- open plot file and read the first header
C     to see how many pictures there are
C
      CALL GSOFLR(IUNIT,FILNAM)
      KEOF = 0
      CALL GSRHDR(KEOF)
      IF (KEOF.EQ.1) GO TO 120
      IF (NOPRNT.EQ.0) WRITE (LUNOUT,FMT=6030) NPICS
   70 CONTINUE
C
C---- Start of main picture loop NOTE that header has already been read
C
C
      IF (NSTART.GT.0) GO TO 100
      IF ((NINTER.EQ.0) .OR. (NCONTR.EQ.0)) GO TO 100
      IF ((NOPRNT.EQ.0) .AND. (JPIC.GT.0)) WRITE (LUNOUT,FMT=6014) JPIC
   80 CONTINUE
      CALL NOCRLF ('+Type no of pictures to skip..........=$')
      READ (LUNIN,FMT=6012) NSKIP
      IF (NSKIP.GE.0) GO TO 90
      WRITE (LUNOUT,FMT=6050)
      GO TO 80
   90 CONTINUE
  100 CONTINUE
C
C---- Skip up to start point
C
      IF (NSKIP.LT.1) GO TO 130
      DO 110 JSKIP = 1,NSKIP
        JPIC = JPIC + 1
        IF (NREC.EQ.0) GO TO 120
        IF (NOPRNT.EQ.0) WRITE (LUNOUT,FMT=6020) JPIC,IUNIT,NREC,
     +      DOTMMX,DOTMMY,IXMIN,IXMAX,IYMIN,IYMAX,LINWT,ICOLOR,MIXCOL,
     +      MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,DWLIMY,DVXMIN,
     +      DVXMAX,DVYMIN,DVYMAX,NPICS,TITLEH
        CALL GSFLSR(4*NREC)
        KEOF = 0
        CALL GSRHDR(KEOF)
        IF (KEOF.EQ.1) GO TO 120
  110 CONTINUE
      GO TO 130
  120 CONTINUE
C
C---- End of pictures
C
      WRITE (LUNOUT,FMT=6000) (JPIC-1),NSKIP
      IERR = 1
      GO TO 440
  130 CONTINUE
C
C---- Start processing picture
C
      JPIC = JPIC + 1
      IF (NREC.EQ.0) GO TO 140
      IF (NOPRNT.EQ.0) WRITE (LUNOUT,FMT=6020) JPIC,IUNIT,NREC,DOTMMX,
     +    DOTMMY,IXMIN,IXMAX,IYMIN,IYMAX,LINWT,ICOLOR,MIXCOL,MDEVIC,
     +    MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,DWLIMY,DVXMIN,DVXMAX,
     +    DVYMIN,DVYMAX,NPICS,TITLEH
      GO TO 150
  140 CONTINUE
C
C---- No picture or no records
C
      WRITE (LUNOUT,FMT=6004) (JPIC-1)
      IERR = 1
      GO TO 440
  150 CONTINUE
C
C---- Plot dimensions in mm
C
C---- Range of plot in pixel integers dots on plot file
C     add 0.01 to avoid divide by zero
C
      AXRNGE = REAL(IXMAX-IXMIN) + 0.01
      AYRNGE = REAL(IYMAX-IYMIN) + 0.01
      XMIN = REAL(IXMIN)/DOTMMX
      XMAX = REAL(IXMAX)/DOTMMX
      YMIN = REAL(IYMIN)/DOTMMY
      YMAX = REAL(IYMAX)/DOTMMY
      XWID = AXRNGE/DOTMMX
      YWID = AYRNGE/DOTMMY
      AXFAC = XWID/XSCRM1
      AYFAC = YWID/YSCRM1
      SCMAXX = 0.999/AXFAC
      SCMAXY = 0.999/AYFAC
      SCMAXP = MIN(SCMAXX,SCMAXY)
      IF (NOPRNT.EQ.0) WRITE (LUNOUT,FMT=6032) XMIN,XMAX,YMIN,YMAX,
     +    XWID,YWID,XSCRM1,YSCRM1,AXFAC,AYFAC,SCMAXP
      IF ((NINTER.EQ.0) .OR. (NCONTR.EQ.0)) GO TO 250
  160 CONTINUE
C
C---- Select scaling options
C
      CALL NOCRLF ('+Scaling: <1>Ufill (2)Choice (3)Fill..=$')
      READ (LUNIN,FMT=6012) ISCALE
      IF (ISCALE.EQ.0) ISCALE = 1
      IF ((ISCALE.GE.1) .AND. (ISCALE.LE.3)) GO TO 170
      WRITE (LUNOUT,FMT=6050)
      GO TO 160
  170 CONTINUE
      IF (ISCALE.NE.2) GO TO 200
  180 CONTINUE
      CALL NOCRLF ('+Scale factor screen/paper <0.5>......=$')
      READ (LUNIN,FMT=6048) SCAFAC
      IF (SCAFAC.EQ.0.0) SCAFAC = 0.5
      IF (SCAFAC.GE.0.0) GO TO 190
      WRITE (LUNOUT,FMT=6050)
      GO TO 180
  190 CONTINUE
  200 CONTINUE
      NOCENT = 1
      IF (ISCALE.EQ.3) GO TO 220
  210 CONTINUE
      CALL NOCRLF ('+Nocentering <0> (1)..................=$')
      READ (LUNIN,FMT=6012) NOCENT
      IF ((NOCENT.EQ.0) .OR. (NOCENT.EQ.1)) GO TO 220
      WRITE (LUNOUT,FMT=6050)
      GO TO 210
  220 CONTINUE
  230 CONTINUE
      CALL NOCRLF ('+Lineweight <0>As in file (1-9)Fixed..=$')
      READ (LUNIN,FMT=6012) LINSIZ
      IF ((LINSIZ.GE.0) .AND. (LINSIZ.LE.9)) GO TO 240
      WRITE (LUNOUT,FMT=6050)
      GO TO 230
  240 CONTINUE
  250 CONTINUE
C
C---- Calculate scale factors
C
C---- Device size
C
      SIZX = SIZX1
      SIZY = SIZY1
C
C---- Set margin offsets
C
      IXOFF = IXMRG1
      IYOFF = IYMRG1
      IXBIG = IXOFF + ISIZX1
      IYBIG = IYOFF + ISIZY1
      GO TO (280,260,280) ISCALE
  260 CONTINUE
C
C---- Option with scale keyed to paper plot
C     (1 paper mm = scafac screen mm: default scafac=0.5)
C
      IF (SCAFAC.LE.0.0) SCAFAC = 0.5
C
C---- Dotmmx and dotmmy dots per mm assumed
C
      SCALX = (DTMMX/DOTMMX)*SCAFAC
      SCALY = (DTMMY/DOTMMY)*SCAFAC
C
C---- filling ratio for screen area 880*756 memdots
C
      AXFAC = (AXRNGE*SCALX)/SIZX
      AYFAC = (AYRNGE*SCALY)/SIZY
      IF (NOPRNT.EQ.0) WRITE (LUNOUT,FMT=6026) AXFAC,AYFAC
      IF (SCAFAC.LT.SCMAXP) GO TO 270
      IF ((AXFAC.LE.1.0) .AND. (AYFAC.LE.1.0)) GO TO 270
C
C---- plot too big for requested scale
C
      SCMAXX = SCAFAC/AXFAC
      SCMAXY = SCAFAC/AYFAC
      IF (NOPRNT.EQ.0) WRITE (LUNOUT,FMT=6006) SCAFAC,SCMAXX,SCMAXY
      IF (NOPRNT.EQ.1) CALL NOCRLF ('+RESCALED!$')
      GO TO 280
  270 CONTINUE
      IF (NOPRNT.EQ.0) WRITE (LUNOUT,FMT=6024) SCAFAC,SCAFAC
      GO TO 300
  280 CONTINUE
C
C---- maximal scalings on screen area 880*756
C
      SCMAXX = XSCRM1/XWID
      SCMAXY = YSCRM1/YWID
      AXFAC = 1.0
      AYFAC = 1.0
      IF (ISCALE.EQ.3) GO TO 290
C
C---- for uniform shape
C
      SCMAXP = MIN(SCMAXX,SCMAXY)
      AXFAC = SCMAXP/SCMAXX
      AYFAC = SCMAXP/SCMAXY
      SCMAXX = SCMAXP
      SCMAXY = SCMAXP
  290 CONTINUE
C
C---- actual scale ratios used
C
      TRUSCX = SCMAXX
      TRUSCY = SCMAXY
      IF (NOPRNT.EQ.0) WRITE (LUNOUT,FMT=6026) AXFAC,AYFAC
      IF (NOPRNT.EQ.0) WRITE (LUNOUT,FMT=6024) TRUSCX,TRUSCY
      SCALX = (DTMMX/DOTMMX)*SCMAXX
      SCALY = (DTMMY/DOTMMY)*SCMAXY
      GO TO 300
  300 CONTINUE
C
C---- prepare for actual plotting
C
C---- centre the plot on screen
C
      IXEDG = IXOFF
      IYEDG = IYOFF
      IF ((NOCENT.EQ.1) .OR. (ISCALE.EQ.3)) GO TO 310
      SPAREX = (1.0-AXFAC)*SIZX
      SPAREY = (1.0-AYFAC)*SIZY
      IXSPAR = 0.5*SPAREX
      IYSPAR = 0.5*SPAREY
      IXEDG = IXOFF + IXSPAR
      IYEDG = IYOFF + IYSPAR
  310 CONTINUE
C
C---- pause to read input information:type any integer or "return" to go on
C
      IF (NOPRNT.EQ.1) GO TO 320
      CALL NOCRLF ('+VIEW: Type "RETURN" to draw$')
      READ (LUNIN,FMT=6012) MORE
  320 CONTINUE
C
C---- start up terminal
C
      JINIT = 1
      CALL GSINTM(IOUT)
      CALL GSGRTM
C
C---- draw edge dots on screen
C
      CALL GSPTTM(ISXMIN,IYOFF)
      CALL GSPTTM(ISXMIN,IYBIG)
      CALL GSPTTM(ISXMAX,IYOFF)
      CALL GSPTTM(ISXMAX,IYBIG)
      CALL GSPTTM(IXOFF,ISYMIN)
      CALL GSPTTM(IXBIG,ISYMIN)
      CALL GSPTTM(IXOFF,ISYMAX)
      CALL GSPTTM(IXBIG,ISYMAX)
C
C---- initialise pen position and line thickness
C
      IF (LINSIZ.GT.0) LINWT = LINSIZ
      IXOLD = IXEDG
      IYOLD = IYEDG
C
C---- expects nrec plot records (ix,iy) with no "end" codes
C
      NDOT = 0
      DO 350 J = 1,NREC
        NDOT = NDOT - 1
        KKEOF = 0
        CALL GSFLRI(IX,IY,KKEOF)
        IF (KKEOF.EQ.1) GO TO 360
C
C---- check command codes
C
        IF (IX.LT.0) THEN
          IF (IX.EQ.IEND) THEN
            GO TO 370
          ELSE IF (IX.EQ.IDOT) THEN
            NDOT = 2
          ELSE IF (IX.EQ.ILWT) THEN
C
C---- line thickness code ignored if linsiz is set
C
            IF (LINSIZ.EQ.0) LINWT = IY
C
C---- skip all "pen","paper" codes
C
          ELSE IF (IX.EQ.IERAS) THEN
C
C---- erase code
C
            IF (IY.EQ.1) THEN
              CALL GSERTM
            ELSE IF (IY.EQ.0) THEN
              CALL GSDOTM
            ELSE IF (IY.EQ.2) THEN
              CALL GSRVTM
            END IF
          END IF
          GO TO 340
        END IF
        IF (IY.LT.0) THEN
          IXOLD = NINT((IX-IXMIN)*SCALX) + IXEDG
          IYOLD = NINT((-IY-IYMIN)*SCALY) + IYEDG
        ELSE
          IX = NINT((IX-IXMIN)*SCALX) + IXEDG
          IY = NINT((IY-IYMIN)*SCALY) + IYEDG
          IF (NDOT.LT.1) THEN
            CALL GSMVTM(IXOLD,IYOLD)
            CALL GSDWTM(IX,IY)
          ELSE
            CALL GSPTTM(IX,IY)
          END IF
C
C---- here for multiple wt lines
C
          IF (LINWT.GT.1) THEN
            LL = LINWT - 1
            IF (LL.LT.2) LL = 2
            DO 330 IND = 1,LL
              IXMOVE = LOFFX(IND) + IXOLD
              IYMOVE = LOFFY(IND) + IYOLD
              IF (IXMOVE.LT.IXEDG) IXMOVE = IXEDG
              IF (IYMOVE.LT.IYEDG) IYMOVE = IYEDG
              IF (IXMOVE.GT.IXBIG) IXMOVE = IXBIG
              IF (IYMOVE.GT.IYBIG) IYMOVE = IYBIG
              IF (NDOT.LT.1) CALL GSMVTM(IXMOVE,IYMOVE)
              IXDRAW = LOFFX(IND) + IX
              IYDRAW = LOFFY(IND) + IY
              IF (IXDRAW.LT.IXEDG) IXDRAW = IXEDG
              IF (IYDRAW.LT.IYEDG) IYDRAW = IYEDG
              IF (IXDRAW.GT.IXBIG) IXDRAW = IXBIG
              IF (IYDRAW.GT.IYBIG) IYDRAW = IYBIG
              IF (NDOT.LT.1) THEN
                CALL GSDWTM(IXDRAW,IYDRAW)
              ELSE
                CALL GSPTTM(IXDRAW,IYDRAW)
              END IF
  330       CONTINUE
          END IF
          IXOLD = IX
          IYOLD = IY
        END IF
  340   CONTINUE
  350 CONTINUE
C
C---- end of this picture
C
      GO TO 370
  360 CONTINUE
C
C---- error during plot file reading
C
      IERR = 1
      GO TO 370
  370 CONTINUE
C
C---- get commands from terminal
C
      CALL GSTYTM
      CALL GSMYTM(0,23)
C
C---- if non-interactive go to next picture
C
      IF ((NINTER.EQ.0) .OR. (NCONTR.EQ.0)) THEN
        CALL NOCRLF ('+$')
        READ (LUNIN,FMT=6012) MORE
        GO TO 410
      END IF
C
C---- pause to look at picture "return" or "space" to save and continue
C     "+" to save and pause. any other character to delete
C
      CALL NOCRLF ('+Type "RETURN" or "SPACE" to save "+" to pause$')
      CALL NOCRLF ('+any other character to delete$')
      READ (LUNIN,FMT=6058) CHKEY
      CALL GSCYTM
      IF (CHKEY.EQ.' ') THEN
        GO TO 390
      ELSE IF (CHKEY.EQ.'+') THEN
        CALL NOCRLF ('+$')
        READ (LUNIN,FMT=6012) MORE
        GO TO 390
      ELSE
C
C---- clear screen
C
        CALL GSINTM(IOUT)
      END IF
  390 CONTINUE
C
C---- another picture ? (0)=quit (1)=next, (-1)=repeat (100)=restart
C
      CALL GSTYTM
      CALL GSMYTM(0,23)
      CALL NOCRLF ('+(0)End (1)next (-1)again (100)begin..=$')
      READ (LUNIN,FMT=6012) MORE
      IF (MORE.EQ.0) THEN
        GO TO 450
      ELSE IF (MORE.EQ.1) THEN
        GO TO 410
      ELSE IF (MORE.EQ.-1) THEN
        GO TO 400
      ELSE IF (MORE.EQ.100) THEN
        CALL GSFLP1
        JPIC = 0
        GO TO 410
      ELSE
        WRITE (LUNOUT,FMT=6050)
        GO TO 390
      END IF
  400 CONTINUE
C
C---- when more=(-1) redraw same picture
C     to redraw current picture backspace the plot file
C
      CALL GSFLBR(4*NREC+512)
C
C---- read header again
C
      KEOF = 0
      CALL GSRHDR(KEOF)
      IF (KEOF.EQ.1) GO TO 140
      JPIC = JPIC - 1
      GO TO 130
  410 CONTINUE
C
C---- when more=(1) draw next picture read header for next picture
C
      KEOF = 0
      CALL GSRHDR(KEOF)
      IF (KEOF.EQ.1) GO TO 420
      GO TO 70
  420 CONTINUE
      WRITE (LUNOUT,FMT=6004) JPIC
  440 CONTINUE
C
C---- clear the screen and return control to alpha mode
C
      IF (JINIT.EQ.0) GO TO 460
C
C---- pause to read screen type "return" to exit
C
      IF (NINTER.EQ.0) GO TO 450
      IF (NCONTR.EQ.1) CALL NOCRLF ('+VIEW finished$')
      IF (NCONTR.EQ.0) CALL NOCRLF ('+$')
  450 CONTINUE
      CALL NOCRLF ('+Exit: type "RETURN" or "SPACE" to save picture$')
      CALL NOCRLF ('+ any other character to clear screen$')
      READ (LUNIN,FMT=6058) CHKEY
      IF (CHKEY.NE.' ') THEN
        CALL GSSCTM
      END IF
      CALL GSTYTM
      CALL GSCYTM
      CALL GSOFTM(IOUT)
  460 CONTINUE
C
C---- finish
C
      CALL GSCFIL(IUNIT)
      RETURN
C
C---- Format statements
C
 6000 FORMAT (2X,'!!!VIEW -- Too few pictures ',I5,' found: tried to s',
     +       'kip ',I5)
 6004 FORMAT (2X,'!!!VIEW -- Picture ',I5,' ENDS FILE ')
 6006 FORMAT (2X,'!!!VIEW -- Plot too big: ',/2X,'Chosen scale =',F10.4,
     +       ' max poss scales are X,Y ',2F10.4)
 6012 FORMAT (I5)
 6014 FORMAT (2X,'Series number of last picture was ',I5)
 6020 FORMAT (/1X,'--Picture number ',I5,' --   on IUNIT=',I5,/1X,I5,
     +       ' records ',2F10.4,' dots/mm: x,y ',/2X,'input pixel inde',
     +       'x ranges X: ',2I6,' Y: ',2I6,/2X,'LINWT  ',I5,' ICOLOR ',
     +       I5,' MIXCOL ',I5,/2X,'MDEVIC ',I5,' MDIREC ',I5,' MOUT   ',
     +       I5,/2X,'MPIC   ',I5,' MSCAFL ',I5,' MCNTFL ',I5,/2X,'DWLI',
     +       'MX ',F10.4,6X,'DWLIMY ',F10.4,/2X,'DVXMIN ',F10.4,6X,'DV',
     +       'XMAX ',F10.4,/2X,'DVYMIN ',F10.4,6X,'DVYMAX ',F10.4,/2X,
     +       'NPICS  ',I5,' TITLE: ',/1X,A)
 6024 FORMAT (2X,'Scale magnification screen/paper X,Y ',2F10.4)
 6026 FORMAT (2X,'User area screen filling factors X,Y ',2F10.4)
 6028 FORMAT (1X,'--Plot file-name requested is:-- ',A)
 6030 FORMAT (1X,'--File contains ',I5,' pictures--')
 6032 FORMAT (/1X,'--Original plot dimensions in mm--',/2X,'limits:   ',
     +       'X1 X2 ',2F9.2,'      Y1 Y2 ',2F9.2,/2X,'widths used X Y ',
     +       2F9.2,'; available ',2F9.2,/2X,'width filling factors X Y',
     +       ';           ',2F8.4,/2X,'max scale factor allowed;      ',
     +       '      ',F8.4,/)
 6048 FORMAT (F10.5)
 6050 FORMAT (/1X,'--ILLEGAL INPUT VALUE-- TRY AGAIN ')
 6052 FORMAT (2X,'VIEW called with ',/2X,'NINTER =',I5,' NOPRNT =',I5,
     +       /2X,'FILNAM =    ',A,/2X,'NSTART =',I5,' ISCALE =',I5,' S',
     +       'CAFAC =',F10.5,/2X,'NOCENT =',I5,' LINSIZ =',I5,/)
 6056 FORMAT (A)
 6058 FORMAT (A,A)
C
      END
C
C
C
      SUBROUTINE GSXYTM(ITERM,IX,IY,CODE)
C     ====================================
C
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were CODE
C     and the followig BYTEs to INTEGER: MSX, MSY, LSX, LSY, OLDMSY,
C                                        OLDLSY, OLDMSX, OLDLSX
C
C     .. Scalar Arguments ..
      INTEGER ITERM,IX,IY
      CHARACTER*1 CODE,GS,US
C     ..
C     .. Scalars in Common..
      INTEGER IDXOFF,IDYOFF,IXOLD,IYOLD,MODOLD
      REAL    FACX,FACY,XFOFF,YFOFF
C     ..
C     .. External Subroutines ..
      EXTERNAL PUTLINE
      INTRINSIC CHAR
C     .. 
C     .. Common Blocks ..
      COMMON /GSDVT/FACX,FACY,XFOFF,YFOFF,IDXOFF,IDYOFF,
     +                IXOLD,IYOLD,MODOLD
C
C     .. Save Statement ..
C
      SAVE /GSDVT/
C
C     .. Data Statement ..
      DATA BUFPTR/1/
C
C---- Decode coordinate  bytes
C
C   the bit pattern code here is
C
C   bit value            32 16  8  4  2  1
C   bit number      7  6  5  4  3  2  1  0
C
C       HIGH X     P  0  1  X9 X8 X7 X6 X5
C       LOW  X     P  0  2  X4 X3 X2 X1 X0
C
C       HIGH Y     P  0  1  Y9 Y8 Y7 Y6 Y5
C       LOW  Y     P  0  3  Y4 Y3 Y2 Y1 Y0
C
C     IX = 32*HIGHX + LOWX
C     IY = 32*HIGHY + LOWY
C
C---- Code.ne.0 means control character goes into buffer
C
      GS = CHAR(29)
      US = CHAR(31)
C
      IF (CODE.EQ.GS) CALL PUTLINE (IXOLD,IYOLD,IX,IY)
C
C---- Save old coords for next time
C
      IXLAST = IX
      IYLAST = IY
C
      END
C
C
C
      SUBROUTINE IROT(IX,IY,RM,JX,JY)
C     ===============================
C
C---- Rotate JX,JY by 2 x 2 matrix RM, answer in IX, IY
C
C     .. Scalar Arguments ..
      INTEGER IX,IY,JX,JY
C     ..
C     .. Array Arguments ..
      REAL RM(2,2)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
      IX = NINT(RM(1,1)*JX+RM(1,2)*JY)
      IY = NINT(RM(2,1)*JX+RM(2,2)*JY)
C
      END
C
C
C
      INTEGER FUNCTION ISHFT (INUM,IBITS)
C     ===================================
C
C---- ISHIFT perfoms a bitwise shift of INUM by IBITS bits. 
C     For IBITS = 0    ISHIFT returns INUM unchanged
C     For IBITS > 0    ISHIFT returns the Left shift of INUM 
C     For IBITS < 0    ISHIFT returns the Right shift of INUM 
C
      INTEGER INUM, IBITS
C
      IF (IBITS.EQ.0) THEN
        ISHFT = INUM
      ELSE IF (IBITS.GT.0) THEN
        ISHFT = INUM * IBITS * 2
      ELSE
        ISHFT = INUM / (IABS(IBITS) * 2)
      ENDIF
C
      END
C
      SUBROUTINE PUTCHR (IC)
C     ======================
C
      INTEGER IC, BUFPTR
C
      CHARACTER BUFFER*256
C
      INTRINSIC CHAR
C
      COMMON /CHRBUF/ BUFFER,BUFPTR
C
      SAVE
C
      BUFFER(BUFPTR:BUFPTR) = CHAR(IC)
      BUFPTR = BUFPTR + 1
      END
C
C
      SUBROUTINE PUTLIN (X1, Y1, X2, Y2)
C     ===================================
C
      INTEGER X1, X2, Y1, Y2, GS, US, BUFPTR, LUNIN, LUNOUT
C
      CHARACTER BUFFER*256
C
      EXTERNAL PUTCHR, SENDXY 
C
      COMMON /CHRBUF/ BUFFER,BUFPTR
      COMMON /PINOUT/ LUNIN,LUNOUT
C
      SAVE
C
      DATA GS/29/,US/31/
C
      BUFPTR = 1
      CALL PUTCHR (GS)
      CALL SENDXY (X1, Y1)
      CALL SENDXY (X2, Y2)
      CALL PUTCHR (US)
      WRITE (LUNOUT,100) BUFFER(1:BUFPTR-1)
 100  FORMAT (' ',A)
      END
C
C
C
      SUBROUTINE RPLOT(ILIM,IER)
C     ==========================
C
C     Concerted for PLOT84 Sept 1984. Last updated 11 SEP 1984
C
C     Read plot to get limits ILIM (= umin,umax,vmin,vmax)
C     Starts at record IREC, returns IREC pointing to next plot
C     if IER = 0
C
C     Returns IER=1 if end of file found in header, =-1 if eof
C     in data, otherwise IER=0
C
C   Header is INTEGER*4 !!!!!!!!
C
C     .. Scalar Arguments ..
      INTEGER IER
C     ..
C     .. Array Arguments ..
      INTEGER ILIM(4)
C     ..
C     .. Scalars in Common ..
      REAL DOTMMX,DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,DWLIMX,DWLIMY
      INTEGER LUNIN,LUNOUT
      INTEGER*4 ICOLOR,IUNITR,IXMAX,IXMIN,IYMAX,IYMIN,LINWT,MCNTFL,
     +          MDEVIC,MDIREC,MIXCOL,MOUT,MPIC,MSCAFL,NPICS,NREC
C     ..
C     .. Arrays in Common ..
      REAL SPARE1,SPARE2,TITLEH
C     ..
C     .. Local Scalars ..
      INTEGER IEND,IERAS,ILNW,IPAP,IPEN
C     ..
C     .. Local Arrays ..
      INTEGER*4 NHEADR(128)
C     ..
C     .. External Subroutines ..
      EXTERNAL QREAD
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSFHD/IUNITR,NREC,DOTMMX,DOTMMY,IXMIN,IXMAX,IYMIN,IYMAX,
     +       LINWT,ICOLOR,MIXCOL,MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,
     +       DWLIMX,DWLIMY,DVXMIN,DVYMIN,DVXMAX,DVYMAX,NPICS,SPARE1(17),
     +       TITLEH(20),SPARE2(68)
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (NHEADR(1),NREC)
C     ..
C     .. Data statements ..
C
      DATA IPEN/-4/,IEND/-1/,IPAP/-5/,IERAS/-6/,ILNW/-3/
C
      IER = 0
C
C---- Read header
C
      CALL QREAD(1,NHEADR,512,IER)
C
C---- End File if block length wrong
C
      IF (IER.EQ.0) THEN
C
C---- Read limits (in bits)
C
        ILIM(1) = IXMIN
        ILIM(2) = IXMAX
        ILIM(3) = IYMIN
        ILIM(4) = IYMAX
      END IF
C
C---- End of this plot Or end of file
C
      END
C
C
C
      SUBROUTINE SCALE(A,B)
C     =====================
C
C     A.D. MCLACHLAN JULY 1984. LAST UPDATED 8 SEP 1984
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were NFONTS
C
C---- multiply scales by (a,b)
C
C     .. Scalar Arguments ..
      REAL A,B,C,D,DEG,X,Y
      INTEGER ICOLR,LWT,N,NPOINT
C     ..
C     .. Array Arguments ..
      REAL XVEC(1),YVEC(1)
C     ..
C     .. Scalars in Common ..
      REAL ANGFAC,BXMAX,BXMIN,BYMAX,BYMIN,CHANGX,CHANGY,CHRSCX,CHRSCY,
     +     CHRSPX,CHRSPY,DOTMMX,DOTMMY,DVXMAX,DVXMIN,DVYMAX,DVYMIN,
     +     DWBLMX,DWBLMY,DWLIMX,DWLIMY,FACX,FACY,PAPLMX,PAPLMY,SCALEX,
     +     SCALEY,UBXMAX,UBXMIN,UBYMAX,UBYMIN,USANGX,USANGY,V64LMX,
     +     V64LMY,XBNEW,XBOLD,XCHAR,XCSTRT,XFOFF,XNOW,YBNEW,YBOLD,YCHAR,
     +     YCSTRT,YFOFF,YNOW
      INTEGER ICENTC,ICOLOR,IDRLVL,IDXOFF,IDYOFF,IFONT,IPRINT,IUNIT,
     +        IUNITR,IXMAX,IXMIN,IYMAX,IYMIN,KPRINT,LINWT,LUNIN,LUNOUT,
     +        MCNTFL,MDEVIC,MDIREC,MIXCOL,MOUT,MPIC,MSCAFL,NERROR,
     +        NPICS,IXOLD,IYOLD,MODOLD
      LOGICAL*4 DEVCON,FONTIN,ICULNK,INITON,LINMOD,UCSPAC
      CHARACTER FILNAM*80,TITLE*80
C     ..
C     .. Arrays in Common ..
      REAL CHRMAT,CUMAT,USRMAT
      INTEGER IREC
      INTEGER*2 IFHT,IFSTRT,IFWID,IFX0,IFY0,LENGF
      CHARACTER*1 NFONTS
C     ..
C     .. Local Scalars ..
      REAL AA,BB,CHORGX,CHORGY,CUORGX,CUORGY,PI,XORIG,YORIG
      INTEGER M,MODE
      CHARACTER TITLEH*80
C     ..
C     .. Local Arrays ..
      REAL*4 AREC(128)
      REAL TMAT(2,2)
      INTEGER NCTRAN(24),NSTRAN(24)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSANCU,GSCENC,GSCOLR,GSCROT,GSFONT,GSLNWT,
     +         GSORGD,GSORGU,GSPAPR,GSSCLC,GSSCLU,GSUMAT,
     +         GSUROT,GSVCLN
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ATAN2
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
      COMMON /GSCHX/CHRMAT(3,3),CHRSCX,CHRSCY,CHANGX,CHANGY,CHRSPX,
     +       CHRSPY,ICENTC,UCSPAC,IFONT,FONTIN,XCHAR,YCHAR,XCSTRT,
     +       YCSTRT,ANGFAC
      COMMON /GSCLP/BXMIN,BXMAX,BYMIN,BYMAX
      COMMON /GSDVT/FACX,FACY,XFOFF,YFOFF,IDXOFF,IDYOFF
     +       IXOLD,IYOLD,MODOLD
      COMMON /GSDVW/MDEVIC,MDIREC,MOUT,MPIC,MSCAFL,MCNTFL,DWLIMX,
     +       DWLIMY,DVXMIN,DVXMAX,DVYMIN,DVYMAX,UBXMIN,UBXMAX,UBYMIN,
     +       UBYMAX,FILNAM,DOTMMX,DOTMMY,DEVCON,INITON,NERROR,DWBLMX,
     +       DWBLMY,PAPLMX,PAPLMY,V64LMX,V64LMY,IUNIT,IXMIN,IXMAX,IYMIN,
     +       IYMAX,LINWT,ICOLOR,MIXCOL,NPICS,IPRINT,IDRLVL,TITLE
      COMMON /GSDWX/XNOW,YNOW,XBNEW,YBNEW,XBOLD,YBOLD
      COMMON /GSFHD/IUNITR,IREC(128)
      COMMON /GSFNT/IFSTRT(150,4),LENGF(150,4),IFX0(150,4),
     +       IFY0(150,4),IFWID(150,4),IFHT(150,4),NFONTS(4,3000,4)
      COMMON /GSUTR/USRMAT(3,3),SCALEX,SCALEY,USANGX,USANGY,
     +       CUMAT(3,3),LINMOD,ICULNK,KPRINT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Equivalences ..
      EQUIVALENCE (XORIG,USRMAT(1,3)), (YORIG,USRMAT(2,3))
      EQUIVALENCE (CUORGX,CUMAT(1,3)), (CUORGY,CUMAT(2,3))
      EQUIVALENCE (NSTRAN(1),USRMAT(1,1))
      EQUIVALENCE (CHORGX,CHRMAT(1,3)), (CHORGY,CHRMAT(2,3))
      EQUIVALENCE (NCTRAN(1),CHRMAT(1,1))
      EQUIVALENCE (IREC(1),AREC(1))
      EQUIVALENCE (TITLEH,IREC(41))
C
      AA = SCALEX*A
      BB = SCALEY*B
      CALL GSSCLU(AA,BB)
      RETURN
C
      ENTRY ROTATE(DEG)
C     =================
C
      PI = ATAN2(1.0,1.0)*4.0
      ANGFAC = PI/180.0
      AA = ANGFAC*DEG
      BB = (PI/2.0) + AA
      CALL GSUROT(AA,BB)
      RETURN
C
      ENTRY TRNSFM(A,B,C,D)
C     ======================
C
      TMAT(1,1) = A
      TMAT(1,2) = B
      TMAT(2,1) = C
      TMAT(2,2) = D
      CALL GSUMAT(TMAT)
      RETURN
C
      ENTRY ORIGIN(X,Y,N)
C     ===================
C
C     N = 0    absolute (in mm)
C     N = 1    relative to old origin (user units)
C
      IF ((N.LT.0) .OR. (N.GT.1)) THEN
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6000) N
        N = 0
      END IF
      MODE = N + 1
      IF (MODE.EQ.1) THEN
        CALL GSORGD(X,Y)
      ELSE IF (MODE.EQ.2) THEN
        CALL GSORGU(X,Y)
      END IF
      RETURN
C
      ENTRY ANROT(DEG)
C     ================
C
      GO TO 10
C
      ENTRY SCLCHR(A,B)
C     =================
C
C---- (A,B)   values for char width & height
C
      AA = CHRSCX*A
      BB = CHRSCY*B
      CALL GSSCLC(AA,BB)
      RETURN
C
      ENTRY CHRSPC(A)
C     ===============
C
      BB = CHRSCX*A
      AA = CHRSCY
      CALL GSSCLC(AA,BB)
      RETURN
C
      ENTRY LOCCHR(X,Y,N)
C     ===================
C
C---- Set up character origin in user units
C       N = 0   relative to current origin
C       N = 1   relative to current pen position
C
      IF ((N.LT.0) .OR. (N.GT.1)) THEN
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6002) N
        N = 1
      END IF
      M = N + 1
      IF (M.EQ.1) THEN
        CALL GSANCU(X,Y)
      ELSE IF (M.EQ.2) THEN
        CALL GSANCU(X+XNOW,Y+YNOW)
      END IF
      RETURN
C
      ENTRY IRELCH(N)
C     ===============
C
      IF ((N.NE.0) .AND. (N.NE.4)) THEN
        IF (IPRINT.GE.1) WRITE (LUNOUT,FMT=6004) N
        N = 0
      END IF
      CALL GSCENC(N)
      RETURN
C
      ENTRY FONT(N)
C     =============
C
      CALL GSFONT(N)
      RETURN
C
      ENTRY LINEWT(LWT)
C     =================
C
      CALL GSLNWT(LWT)
      RETURN
C
      ENTRY PEN(ICOLR)
C     ================
C
      CALL GSCOLR(ICOLR)
      RETURN
C
      ENTRY PAPER(X)
C     ==============
C
      CALL GSPAPR(X)
      RETURN
C
      ENTRY VECPLT(XVEC,YVEC,NPOINT)
C     =============================
C
      CALL GSVCLN(XVEC,YVEC,NPOINT)
      RETURN
C
C---- Now same as rotchr
C
      ENTRY ROTCHR(DEG)
C     =================
C
10    CONTINUE
C
      PI = ATAN2(1.0,1.0)*4.0
      ANGFAC = PI/180.0
      AA = ANGFAC*DEG
      BB = (PI/2.0) + AA
      CALL GSCROT(AA,BB)
C
C---- Format statements
C
 6000 FORMAT (2X,'!!! ORIGIN ERROR: N=',I5,' OUT OF RANGE(0-1) RESET A',
     +       'S 0=ABS ')
 6002 FORMAT (2X,'!!! LOCCHR ERROR: N=',I5,' NOT 0 OR 1  RESET AS 1=US',
     +       'ER COORD ')
 6004 FORMAT (2X,'!!!IRELCH ERROR: N=',I5,' NOT 0 OR 4  RESET AS 0=LOW',
     +       'LEFT ')
C
      END
C
C
C
      SUBROUTINE SENDXY (XI, YI)
C     ==================================
C 
      INTEGER XI, YI, XLAST, YLAST
C
      INTRINSIC MOD
      EXTERNAL PUTCHR
C
      COMMON /XYLAST/ XLAST,YLAST
C
      SAVE
C
      IF (YI/32 .NE. YLAST/32) CALL PUTCHR (32 + (YI/32))
      IF (MOD(YI,32) .NE. MOD(YLAST,32) .OR. XI/32 .NE. XLAST/32)
     +  CALL PUTCHR (96 + MOD(YI,32))
      IF (XI/32 .NE. XLAST/32) CALL PUTCHR (32 + (XI/32))
      CALL PUTCHR (64 + MOD(XI,32))
      XLAST = XI
      YLAST = YI
      END
C
C
C
      SUBROUTINE SETTBL
C     =================
C
C---- Set colour lookuptable for lower 4 bits (picture)
C     and upper 4 bits (for text)
C
C---- ITABLE    0,0,0,            ! black
C                 255,0,0,            ! red
C                 0,255,0,      ! green
C                 255,255,0,      ! yellow
C                 0,0,255,      ! blue
C                 255,0,255,      ! magenta
C                 0,255,255,      ! cyan
C                 255,255,255,      ! white
C                 255,100,128,      ! pink
C                 150,30,30,      ! brick
C                 255,128,0,      ! orange
C                 170,255,170,      ! aqua
C                 100,100,100,      ! grey
C                 150,100,50,      ! brown
C                 150,180,255,      ! sky
C                 200,100,255      ! lilac
C
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      INTEGER I,N
C     ..
C     .. Local Arrays ..
      INTEGER ITABLE(3,16)
C     ..
C     .. External Subroutines ..
      EXTERNAL AEDSCT
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Data statements ..
      DATA ITABLE/0,0,0,255,0,0,0,255,0,255,255,0,0,0,255,255,0,255,0,
     +     255,255,255,255,255,255,100,128,150,30,30,255,128,0,170,255,
     +     170,100,100,100,150,100,50,150,180,255,200,100,255/
C     ..
C
C---- Set colour table
C
      DO 10 I = 1,16
        CALL AEDSCT(I-1,1,ITABLE(1,I),ITABLE(2,I),ITABLE(3,I))
        N = (I-1)*16
        CALL AEDSCT(N,1,ITABLE(1,I),ITABLE(2,I),ITABLE(3,I))
   10 CONTINUE
C
      END
C
C
C
      SUBROUTINE SETVP(NVP)
C     =====================
C
C---- Set viewport to viewport number NVP
C
C      Screen size is 768 x 575
C      Memory is 1024 x 1024
C      DATA LVP
C         0,XS,0,YS,            ! S screen
C         0,XM,0,YM,            ! M memory
C         0,XS,YS2,YS,            ! T top
C         0,XS,0,YS2,            ! B bottom
C         0,XS2,0,YS,            ! L left
C         XS2,XS,0,YS,            ! R right
C         0,XS2,YS2,YS,            ! TL top left
C         XS2,XS,YS2,YS,      ! TR top right
C         0,XS2,0,YS2,            ! BL bottom left
C         XS2,XS,0,YS2            ! BR bottom right
C
C---- Viewport keys
C      VPS/'S','M','T','B','L','R','TL','TR','BL','BR'/
C
C     .. Parameters ..
      REAL XS,XS2,YS,YS2
      PARAMETER (XS=760,XS2=XS/2,YS=570,YS2=YS/2)
      REAL XM,YM
      PARAMETER (XM=1020,YM=1020)
C     ..
C     .. Scalar Arguments ..
      INTEGER NVP
C     ..
C     .. Scalars in Common ..
      REAL SCALX,SCALY,TX,TY
      INTEGER IPICT,KXMAX,KXMIN,KYMAX,KYMIN,LUNIN,LUNOUT,NBACK
      LOGICAL AAV,CLEAR,EOF,HTEXT,PAN,PICTURE,ROTATE,TABLE,UNIFORM
C     ..
C     .. Arrays in Common ..
      REAL RM
      INTEGER PENS,PFLAGS
C     ..
C     .. Local Arrays ..
      INTEGER LVP(4,10)
C     ..
C     .. Common blocks ..
      COMMON /FLAGS/NBACK,PENS(8),PFLAGS(8),RM(2,2),TX,TY,SCALX,SCALY,
     +       KXMIN,KXMAX,KYMIN,KYMAX,IPICT,AAV,PAN,ROTATE,CLEAR,UNIFORM,
     +       TABLE,PICTURE,HTEXT,EOF
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
C     .. Data statements ..
      DATA LVP/0,XS,0,YS,0,XM,0,YM,0,XS,YS2,YS,0,XS,0,YS2,0,XS2,0,YS,
     +     XS2,XS,0,YS,0,XS2,YS2,YS,XS2,XS,YS2,YS,0,XS2,0,YS2,XS2,XS,0,
     +     YS2/
C
      KXMIN = LVP(1,NVP)
      KXMAX = LVP(2,NVP)
      KYMIN = LVP(3,NVP)
      KYMAX = LVP(4,NVP)
C
      END
C
C
C
      SUBROUTINE STRING(BYTXT,NCHARS)
C     ===============================
C
C---- Plot a text string. lookalike for plot82
C
C     A.D. MCLACHLAN JULY 1984. LAST UPDATED 24 JUL 1984
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were BYTXT
C
C     .. Scalar Arguments ..
      INTEGER NCHARS
C     ..
C     .. Array Arguments ..
      CHARACTER*1 BYTXT(1)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      INTEGER I,N,NJUST
      CHARACTER TEXT*256
C     ..
C     .. External Subroutines ..
      EXTERNAL GSCETX,GSSTRH
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC CHAR
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
      CALL GSSTRH(BYTXT,NCHARS)
      RETURN
C
      ENTRY CSTRNG(BYTXT,NCHARS)
C     ===========================
C
      TEXT = ' '
      N = NCHARS
      IF (N.LT.1) N = 1
      IF (N.GT.256) N = 256
      DO 10 I = 1,N
        TEXT(I:I) = BYTXT(I)
   10 CONTINUE
      NJUST = 2
      CALL GSCETX(TEXT(1:N),NJUST)
C
      END
C
C
C
      SUBROUTINE SYMBOL(X,Y,HGT,BYTXT,ANGLE,NC)
C     =========================================
C
C---- lookalike for plot82 symbol and number
C
C     A.D. MCLACHLAN JUN 1984. LAST UPDATED 27 JUL 1984
C     9/2/90 - PJD: Changed definition of several BYTEs to CHARACTER*1.
C     They were BYTXT
C
C      (X,Y) = positions
C      HGT   = height in user units
C      BYTXT = character string in bytes
C      ANGLE = orientation in degrees
C      NC    = number of chars
C
C     .. Scalar Arguments ..
      REAL ANGLE,FNUM,HGT,X,Y
      INTEGER NC,NDIG
C     ..
C     .. Array Arguments ..
      CHARACTER*1 BYTXT(1)
C     ..
C     .. Scalars in Common ..
      INTEGER LUNIN,LUNOUT
C     ..
C     .. Local Scalars ..
      REAL ANGFAC,PI,THETA
      INTEGER INUM,NAFTER,NDIGIT,NENTRY,NJUST
C     ..
C     .. Local Arrays ..
      INTEGER NTRSAV(48)
C     ..
C     .. External Subroutines ..
      EXTERNAL GSANCU,GSCROT,GSFNUM,GSINUM,
     +         GSSCLC,GSSTRH,GSTRES,GSTSAV
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ATAN2
C     ..
C     .. Common blocks ..
      COMMON /PINOUT/LUNIN,LUNOUT
C
C     .. Save Statement ..
C
      SAVE
C
      NENTRY = 1
      GO TO 10
C
      ENTRY NUMBER(X,Y,HGT,FNUM,ANGLE,NDIG)
C     =====================================
C
      NENTRY = 2
C
C---- Save current character scales
C
   10 CALL GSTSAV(NTRSAV)
      PI = ATAN2(1.0,1.0)*4.0
      ANGFAC = PI/180.0
      THETA = ANGLE*ANGFAC
      CALL GSSCLC(HGT,HGT)
      CALL GSCROT(THETA,THETA+PI/2.0)
      IF ((X.NE.999.0) .OR. (Y.NE.999.0) .OR.
     +    (NENTRY.NE.1)) CALL GSANCU(X,Y)
      IF (NENTRY.EQ.2) THEN
C
C---- "NUMBER" plots a formatted number left justified
C
        NJUST = 1
        IF (NDIG.GE.0) THEN
C
C----  NDIG ge 0 use decimal format
C
          NAFTER = NDIG
          NDIGIT = NDIG
          CALL GSFNUM(FNUM,NDIGIT,NAFTER,1.0,1.0,NJUST)
        ELSE
C
C----  NDIG.lt.0 plot as integer
C
          INUM = FNUM
          NDIGIT = 1
          CALL GSINUM(INUM,NDIGIT,1.0,1.0,NJUST)
        END IF
      ELSE
C
C---- "SYMBOL" plots a string
C
        CALL GSSTRH(BYTXT,NC)
      END IF
C
C---- Restore scales
C
      CALL GSTRES(NTRSAV)
C
      END
