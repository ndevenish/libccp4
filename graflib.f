C
C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
C                  GRAPHICS LIBRARY
C                  ===============
C
C                for use with program
C
C             "MOSFLM","OSCGEN","STILLS"
C
C---- Allows graphics output to be directed to a Sigma or
C      a Tectronix (or dec terminal in tectronix emulation mode),
C      or to write a PLOT84 plotting file for hardcopy output
C
C      NGR = 1  Sigma
C          = 2  Tektronix
C          = 3  PLOT84 file
C          = 4  LNO3 postscript laserprinter,
C               code from Duilio Cascio
C          = 5  Hewlett Packard
C          = 6 Silicon Graphics
C          = 7 Calcomp
C          = 8 Postscript
C
C---- Last modified 11/5/89 to add LNO3 options. (AGWL)
C
C ****** These are NOT YET TESTED ******
C
C---- Also add a GS call in PIXINIT to put tektronix terminals into
C     graphics mode.
C
C---- Add TKEND to set microterm terminals back into normal mode
C     from 4010 (probably won't work for other terminals)
C
C---- Add PL84CLEAR to start a new picture for PLOT84 files,
C     and remove call to PLT$PICT in PL84INIT.
C
C---- This file contains the following subroutines
C
C    Routines called by the source programs
C
C  GRalpha.f   GRclear.f    GRclose.f    GRcol.f
C  GRcursor.f  GRdraw.f     GRend.f      GRfix.f
C  GRflo.f     GRflush.f    GRinit.f     GRint.f
C  GRinteg.f   GRmove.f     GRnewline.f  GRnewpage.f
C  GRorigin.f  GRpixinit.f  GRpoint.f    GRposn.f
C  GRreal.f    GRstring.f   GRxcur.f     
C
C
C     Tek routines
C
C    tkalpha.f    tkclear.f    tkcross.f    tkcursor.f
C    tkdecode.f   tkdraw.f     tkencode.f   tkend.f
C    tkexgr.f     tkflush.f    tkinit.f     tkmove.f
C    tknewpage.f  tkpixinit.f  tkpoint.f    
C
C
C     Sigma routines
C 
C    sigalpha.f    sigclear.f    sigcol.f    sigcursor.f
C    sigdraw.f     siginit.f     sigmove.f   sigpixinit.f
C    sigpoint.f
C
C
C     LNO3 routines
C
C    lno3draw.f    lno3init.f    lno3move.f    lno3point.f
C    endplt.f       factor.f       newpen.f       plot3.f
C    symbol.f
C
C
C     SG (Silicon Graphics) routines
C
C     sgalpha.f    sgclear.f    sgcross.f    sgcursor.f
C     sgdecode.f   sgdraw.f     sgencode.f   sgend.f
C     sgexgr.f     sginit.f     sgmove.f     sgnewpage.f
C     sgpixinit.f  sgpoint.f    sgstartup.f
C
C
C  TOM's routines
C
C    tomatt.f    tomcha.f    tomcur.f    tomdev.f
C    tomdot.f    tomdsh.f    tomfix.f    tomflo.f
C    tomint.f    tomlin.f    tommod.f    tommov.f
C    tompen.f    tomsiz.f    tomsym.f    
C
C
C  HP routines
C
C      hpcol.f    hpcv.f
C      hpinit.f   hpmd.f
C
C
C Miscellaneous (??)
C
C    calcominit.f     calcommd.f
C
C    drawcha.f    nval.f    plotopen.f    rval.f 
C
C    postinit.f       postmd.f   
C
C    stringlength.f
C
C    tdvend.f    tekfin.f    tekstr.f    tgiend.f
C    tgino.f     thp7475.f   tpiccle.f   tpltn.f
C    tpost.f     tt4010.f
C
C    writerxy.f    writexy.f
C
C---- Plotting limits depend on the devicea/lled
C          possibly the easiest method for scaling data
C          for different screen plotting areas is 
C
C
C     1)
C           Use windowing in the program
C     2)
C           just multiply all values of x and y in the following
C           routines by a scale factor. (this will also result in
C           plotter output files being changed of course)
C
C            screen plot area                      = 180.0  x  140.0
C            plot files produced by "PLOT" command = 300.0  x  300.0
C            plot files produced by "HP" command   = 240.0  x  185.0
C
C        Note :
C          absolute scale for tek =  1024 x 768
C          therfore xscale=  5.689
C                   yscale=  5.614
C              let scale = 5.65        
C
C
C
      SUBROUTINE GRINIT
C     =====================
C
C
C---- Opens the graphics unit and clears screen, puts graphics
C     display into alpha state
C
C---- For sigma, set default colour, clear pixel store
C
C
C
C---- Dummy set of NGX,NGY,NLI if not using a graphics device
C
C
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
      LOGICAL VMSVAX
C     ..
C     .. Local Scalars ..
      REAL DUM1,DUM2
      INTEGER IDUM3
      INTEGER SIGXOFF,SIGYOFF,SIGNGX,SIGNGY
      INTEGER TEKXOFF,TEKYOFF,TEKNGX,TEKNGY
      INTEGER HPPXOFF,HPLXOFF,HPPYOFF,HPLYOFF,HPNGX,HPNGY
      INTEGER POPXOFF,POLXOFF,POPYOFF,POLYOFF,PONGX,PONGY
      INTEGER CAPXOFF,CALXOFF,CAPYOFF,CALYOFF,CANGX,CANGY
      REAL HPLSCALE,HPPSCALE,
     +     POPSCALE,POLSCALE,
     +     CALSCALE,CAPSCALE        
C     ..
C     .. External Subroutines ..
      EXTERNAL FACTOR,LNO3INIT,PL84INIT,SIGINIT,TKINIT
      LOGICAL VAXVMS
      EXTERNAL VAXVMS
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /SITE/ VMSVAX
      COMMON /PLOTDEF/ SIGSCALE,SIGXOFF,SIGYOFF,SIGNGX,SIGNGY,
     +          TEKSCALE,TEKXOFF,TEKYOFF,TEKNGX,TEKNGY,
     +          HPPSCALE,HPLSCALE,HPPXOFF,HPLXOFF,HPPYOFF,
     +          HPLYOFF,HPNGX,HPNGY,
     +          POPSCALE,POLSCALE,POPXOFF,POLXOFF,POPYOFF,
     +          POLYOFF,PONGX,PONGY,
     +          CAPSCALE,CALSCALE,CAPXOFF,CALXOFF,CAPYOFF,
     +          CALYOFF,CANGX,CANGY
C     ..
C
C
      VMSVAX = .FALSE.
      IF (VAXVMS()) VMSVAX = .TRUE.
cc
cc- next line for allinat
cc
cc-al      vmsvax = .true.
C
C---- Dummy set of ngx,ngy,nli if not using a graphics device
C
      IF (NGR.EQ.0) THEN
        NLI = 100000
        NGX = 100000
        NGY = 100000
      ELSE IF (NGR.EQ.1) THEN
C
C---- this is sigma terminal
C
        CALL SIGINIT
cc??        NGX = 768
cc??        NGY = 512
          NGX=SIGNGX
          NGY=SIGNGY
          XSCALE=SIGSCALE
          XOFFSET=SIGXOFF
          YOFFSET=SIGYOFF                  
C
C---- Number of lines on screen (dummy for sigma)
C
        NLI = 1000
      ELSE IF (NGR.EQ.2) THEN
        CALL TKINIT
        NGX = 1024
        NGY = 768
C This is from tom !!!
	tekxscale=5.65
	tekyscale=5.65
	tekxoff=0
	tekyoff=0
        XSCALE=tekxscale
        YSCALE=tekyscale
        XOFFSET=tekxoff
        YOFFSET=tekyoff
C---- Number of lines on screen,
C     should be 24 for DEC terminal in Tektronix emulation
C
        IF (NLI.EQ.0) NLI = 34
      ELSE IF (NGR.EQ.3) THEN
        CALL PL84INIT
C
C---- Default limits for PLOT84 files, 328mm across, 508mm down
C
        NGX = 32800
        NGY = 50800
      ELSE IF (NGR.EQ.4) THEN
        CALL LNO3INIT(DUM1,DUM2,IDUM3)
C
C---- 8 inches x 300 dots/inch
C
        NGX = 2400
        NGY = 2400
        CALL FACTOR(1.44)
C
C---- !KLUDGE: scale factor to match real photo
C              change here  for different printers
C
        ELSE IF (NGR.EQ.6) THEN
          CALL SGSTARTUP
          CALL SGINIT
          NGX=1024
          NGY=768
          XSCALE=5.62
          XOFFSET=0.0
          YOFFSET=0.0
        ELSE IF (NGR.EQ.5)THEN
          CALL HPINIT
          NGX=HPNGX
          NGY=HPNGY
C
C
          IF (NDIR.EQ.1)THEN
            XOFFSET=HPLXOFF
            YOFFSET=HPLYOFF
            XSCALE=HPLSCALE
          ELSE 
            XOFFSET=HPPXOFF
            XSCALE=HPPSCALE
            YOFFSET=HPPYOFF
          ENDIF
C
C
        ELSE IF (NGR.EQ.8)THEN
          CALL POSTINIT
          NGX=PONGX
          NGY=PONGY
C
C
          IF (NDIR.EQ.1)THEN
            XOFFSET=POLXOFF
            YOFFSET=POLYOFF
            XSCALE=POLSCALE
          ELSE
            XOFFSET=POPXOFF
            YOFFSET=POPYOFF
            XSCALE=POPSCALE
          ENDIF
C
C
        ELSE IF (NGR.EQ.7)THEN
          CALL CALCOMINIT
          NGX=CANGX
          NGY=CANGY
C
C
          IF (NDIR.EQ.1)THEN
            XOFFSET=CALXOFF
            YOFFSET=CALYOFF
            XSCALE=CALSCALE
          ELSE
            XOFFSET=CAPXOFF
            YOFFSET=CAPYOFF
            XSCALE=CAPSCALE
          ENDIF
C
C
        END IF        
C
C
      NHALFX = NGX/2
      NHALFY = NGY/2
      END
C
C
C
        SUBROUTINE CALCOMINIT
C       ======================
C
C
        CHARACTER LINE*80
        INTEGER POINTER
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /BUFFER/ LINE,POINTER
C        
        CALL PLOTOPEN(3)
C
        WRITE(88,'(A)')' /CI '
        WRITE(88,'(A)')' DT 30 30 900'
        POINTER=1
        LINE=' '
C
        RETURN
        END
C
C
C
        SUBROUTINE CALCOMMD(X,Y,MOVE)
C       ===============================
C
C
        INTEGER X,Y,POINTER,LENGTH
        CHARACTER LINE*80,OUTPUT*20
        LOGICAL MOVE
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /BUFFER/ LINE,POINTER
C
C
        CALL WRITEXY(OUTPUT,X,Y,LENGTH)
C
C
        IF (MOVE)THEN
          OUTPUT(3:20)=OUTPUT(1:18)        
          OUTPUT(1:2)='MA'
        ELSE
          OUTPUT(3:20)=OUTPUT(1:18)        
          OUTPUT(1:2)='DA'
        ENDIF
C
C
        LENGTH=LENGTH+3
        OUTPUT(LENGTH:LENGTH)=';'
C        
C----  now check the buffer
C
        IF (POINTER+LENGTH.GT.78)THEN
C
C---     buffer is full
C
          WRITE(88,'(A)')LINE(1:POINTER)
          LINE=' '
          POINTER=LENGTH+1
          LINE(2:LENGTH+1)=OUTPUT(1:LENGTH)
        ELSE
C
C---     add the data to buffer
C
          LINE(POINTER+1:POINTER+LENGTH)=OUTPUT(1:LENGTH)
          POINTER=POINTER+LENGTH
        ENDIF
C
        RETURN
        END
C
C
C
        SUBROUTINE DRAWCHA(X,Y,CH)
C       ===========================
C
        INTEGER X,Y,XX,YY,POS,ACSIIVAL,SPACING,SPACE
        REAL FONTSIZE,R,FONTSCALE
        CHARACTER CH
        LOGICAL CONT
        BYTE TYPE,POSXY,NUMBER,WIDTH
        INTEGER*2 IND
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /VECTORSAVE/ CONT
      COMMON /FSCOMM/ FONTSIZE,SPACING,SPACE,FONTSCALE
      COMMON /FONTDAT/ IND(127),POSXY(2,2000),NUMBER(127),TYPE(2000),
     +                  WIDTH(127)
C
C
        CONT=.FALSE.
        ACSIIVAL=ICHAR(CH)
        POS=IND(ACSIIVAL)
        N=NUMBER(ACSIIVAL)
C
C
        IF (N.EQ.0)THEN
          R=SPACE+SPACING
          XX=NVAL(R)
          X=X+XX*FONTSIZE
          RETURN
        ENDIF
C
C
        IF (NGR.EQ.5) CALL HPCV(.TRUE.)
C
C
          DO 2 J=0,N-1
            R=POSXY(1,POS+J)
            XX=NVAL(R)
            R=POSXY(2,POS+J)
            YY=NVAL(R)
            XX=X+FLOAT(XX)*FONTSIZE
            YY=Y+FLOAT(YY)*FONTSIZE
C
C
            IF (NGR.EQ.5)THEN
C
C---- Improve the quality of the characters
C     by collecting vectors for the hpgl plotter
C
              IF (TYPE(POS+J).EQ.0)THEN
                CALL GRMOVE(XX,YY)
                CONT=.FALSE.
              ENDIF
C
C
              IF (TYPE(POS+J).EQ.1)THEN
                CALL GRDRAW(XX,YY)
                CONT=.TRUE.
              ENDIF
C
C
            ELSE
              IF (TYPE(POS+J).EQ.0)CALL GRMOVE(XX,YY)
              IF (TYPE(POS+J).EQ.1)CALL GRDRAW(XX,YY)
            ENDIF              
2          CONTINUE
C
C
        IF (NGR.EQ.5)CALL HPCV(.FALSE.)
C        
        CONT=.FALSE.
        R=WIDTH(ACSIIVAL)+SPACING
        XX=NVAL(R)
        X=X+XX*FONTSIZE
C
        RETURN
        END
C
C
C
      SUBROUTINE ENDPLT
C     =================
C
C
C     .. External Subroutines ..
      EXTERNAL PLOT3
C     ..
      CALL PLOT3(0.0,0.0,-999)
      END
C
C
C
      SUBROUTINE FACTOR(FACTR)
C     ========================
C
C
C
C     .. Scalar Arguments ..
      REAL FACTR
C     ..
C     .. Scalars in Common ..
      REAL FACT,XOFFS,YOFFS
      INTEGER IPC,IPEN
C     ..
C     .. Common blocks ..
      COMMON /LASOFFS/XOFFS,YOFFS,IPC,FACT,IPEN
C     ..
      FACT = FACTR
      END
C
C
C
      SUBROUTINE GRALPHA
C     ======================
C
C
C---- Set display in alpha mode
C
C
C
C
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
C     ..
C     .. External Subroutines ..
      EXTERNAL SIGALPHA,TKALPHA,SGALPHA
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
C     ..
      IF (NGR.EQ.1) CALL SIGALPHA
      IF (NGR.EQ.2) CALL TKALPHA
      IF (NGR.EQ.6) CALL SGALPHA
      END
C
C
C
      SUBROUTINE GRCLEAR
C     ======================
C
C
C---- Clear graphics screen, leave display in same state
C
C
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
C     ..
C     .. External Subroutines ..
      EXTERNAL PL84CLEAR,SIGCLEAR,TKCLEAR,SGCLEAR
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
C     ..
C
C
      IF (NGR.EQ.1) THEN
        CALL SIGCLEAR
      ELSE IF (NGR.EQ.2) THEN
        CALL TKCLEAR
      ELSE IF (NGR.EQ.3) THEN
        CALL PL84CLEAR
      ELSE IF (NGR.EQ.6) THEN
        CALL SGCLEAR
      END IF
C
C
      END

C
C
C
      SUBROUTINE GRCLOSE
C     =====================
C
C
C---- Close plot file
C
C
C
C
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
C     ..
C     .. External Subroutines ..
      EXTERNAL PL84CLOSE
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
C     ..
      IF (NGR.EQ.3) CALL PL84CLOSE
      END
C
C
C
      SUBROUTINE GRCOL(N)
C     =======================
C
C
C---- For sigma, set colour
C
C
* replacement for the things formerly picked up from sgi include
* files fgl.h and fdevice.h
      INTEGER BLUE, CYAN, GREEN, RED, WHITE, YELLOW
      PARAMETER (BLUE = 4, CYAN = 6, GREEN = 2, RED = 1, WHITE = 7,
     $     YELLOW = 3)
      INTEGER GETVAL, QREAD, KEYBD, MOUSEX, MOUSEY, MPROJE, MVIEWI,
     $     WINOPE
      PARAMETER (KEYBD = 513, MOUSEX = 266, MOUSEY = 267, MPROJE = 1,
     $     MVIEWI = 2)
C#include "fgl.h"
C#include "fdevice.h"
cc-al         include 'fgl.h'
cc-al         include 'fdevice.h'
C
C
C     .. Scalar Arguments ..
      INTEGER N
C     ..
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
C     ..
C     .. External Subroutines ..
      EXTERNAL SIGCOL
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
C     ..
C
C
      IF (NGR.EQ.1) CALL SIGCOL(N)
C
C
        IF (NGR.EQ.6)THEN
          IF (N.EQ.1)CALL COLOR(WHITE)
          IF (N.EQ.2)CALL COLOR(RED)
          IF (N.EQ.3)CALL COLOR(BLUE)
          IF (N.EQ.4)CALL COLOR(GREEN)
          IF (N.EQ.5)CALL COLOR(YELLOW)
          IF (N.EQ.6)CALL COLOR(CYAN)
        ENDIF
C
C
        IF (NGR.EQ.5 .OR. NGR.EQ.7)THEN
C
C
          IF (NGR.EQ.5.AND.N.GT.8)THEN
            N=N-8
          ENDIF
C
C
          IF (NGR.EQ.7.AND.N.GT.4)THEN
            N=N-4
          ENDIF
C
C
          CALL HPCOL(N)
C
C
        ENDIF
C
C
        RETURN
        END
C
C
C
      SUBROUTINE GRCURSOR(IX,IY,CHA)
C     ==================================
C
C
C---- Put display in graphics mode,put up cursor.
C     the position of cursor is returned in IX,IY after pressing
C     "HIT" key on SIGMA or any (?) key on TK
C
C     return to alpha mode
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER IX,IY
      CHARACTER CHA
C     ..
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
C     ..
C     .. External Subroutines ..
      EXTERNAL SIGCURSOR,TKCURSOR,SGCURSOR
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
C     ..
      IF (NGR.EQ.1) CALL SIGCURSOR(IX,IY)
      IF (NGR.EQ.2) CALL TKCURSOR(IX,IY,CHA)
      IF (NGR.EQ.6) CALL SGCURSOR(IX,IY,CHA)
      END

C
C
C
      SUBROUTINE GRDRAW(IX,IY)
C     ============================
C
C
C---- Draw a line from current position to ix,iy in graphics mode
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER IX,IY
C     ..
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
C     ..
C     .. External Subroutines ..
      EXTERNAL LNO3DRAW,PL84DRAW,SIGDRAW,TKDRAW
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
C     ..
C
C
C---- now check the direction
C       if ndir=1 then no offset
C       if ndir=2 then put picture at top etc
C
        IF (NGR.EQ.5.OR.NGR.EQ.7.OR.NGR.EQ.8)THEN
C
C
           IF (NDIR.EQ.6)THEN
              N23=NINT(FLOAT(NGX)*2.0/4.0)
              N=IX
              IX=IY+N23
              IY=NGY-N
           ELSE IF (NDIR.EQ.3)THEN
              N13=NINT(FLOAT(NGX)/4.0)
              N=IX
              IX=IY+N13
              IY=NGY-N
          ELSE IF (NDIR.EQ.4)THEN
              N=IX
              IX=IY
              IY=NGY-N
          ENDIF
        ENDIF
C
C
        IX=IX+XOFFSET
        IY=IY+YOFFSET
C
C
       IF ( IX.LT.0 .OR. IX.GT.(NGX-1) .OR. IY.LT.0 .OR.
     +      IY.GT.(NGY-1) ) THEN
C
C---- Debug
C
      WRITE(6,100) IX,IY
 100  FORMAT(1X,'ATTEMPT TO WRITE OUTSIDE DISPLAY AREA IX,IY',2I5)
C
C
      ELSE IF (NGR.EQ.1) THEN
        CALL SIGDRAW(IX,IY)
      ELSE IF (NGR.EQ.2) THEN
        CALL TKDRAW(IX,IY)
      ELSE IF (NGR.EQ.3) THEN
        CALL PL84DRAW(IX,IY)
      ELSE IF (NGR.EQ.4) THEN
        CALL LNO3DRAW(IX,IY)
      ELSE IF (NGR.EQ.5) THEN
        CALL HPMD(IX,IY,.FALSE.)
      ELSE IF (NGR.EQ.6) THEN
        CALL SGDRAW(IX,IY)
      ELSE IF (NGR.EQ.7) THEN
        CALL CALCOMMD(IX,IY,.FALSE.)
      ELSE IF (NGR.EQ.8) THEN
        CALL POSTMD(IX,IY,.FALSE.)
      END IF
C
C
      END



C
C
C
      SUBROUTINE GREND
C     ====================
C
C
C---- End current picture,
C     for graphics (TK) terminals where it is allowed (?)
C     send sequence to revert terminal to exit 4010/4014 mode.
C
C
C
C
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
C     ..
C     .. External Subroutines ..
      EXTERNAL PL84END,TKEND
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
C     ..
      IF (NGR.EQ.2) CALL TKEND
      IF (NGR.EQ.3) CALL PL84END
      IF (NGR.EQ.6) CALL SGEND
      END
C
C
C
        SUBROUTINE GRFIX(X,Y,R,N,M)
C       ===============================
C
C
C
C
       INTEGER X,Y,M,N,XXX,I,NPOS,SPACE,SPACING,IXLAST,IYLAST
       REAL FONTSIZE,R,FONTSCALE
       CHARACTER CHA,LINE*20,FORMT*8
C
C
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI,NDIR
      REAL GRFACT,DISPLAY,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /FSCOMM/ FONTSIZE,SPACING,SPACE,FONTSCALE
      COMMON /LAST/ IXLAST,IYLAST
C
C
        XXX=NINT(R*10**M)
        R=FLOAT(XXX)/10**M
        XXX=X
C
C----  write into the line variable the number
C      notice that since line is 12 char, the number
C      will be written right justified
C
        FORMT='(F20.  )'
C
C
        IF (M.GE.10)THEN
          WRITE(UNIT=FORMT(6:7),FMT='(I2)')M
        ELSE
          WRITE(UNIT=FORMT(6:6),FMT='(I1)')M
        ENDIF        
C
C
        WRITE(UNIT=LINE,FMT=FORMT)R
C
C---- Now find the decimal point
C     now take n characters , where the last is m after the DP
C
        DO 1 I=21-N,20
C
C----    and write each char 
C
          CALL DRAWCHA(XXX,Y,LINE(I:I))
1        CONTINUE
C
C
        IXLAST=XXX+FONTSIZE*(SPACING+SPACE)
C
        RETURN
        END
C
C
C
        SUBROUTINE GRFLO(X,Y,RR,M)
C       ==============================
C
C
C
C
      INTEGER X,Y,M,N,XXX,I,NPOS,SPACE,SPACING,IXLAST,IYLAST
      INTEGER LENGTH,FRACT
      REAL FONTSIZE,R,RR,FONTSCALE
      CHARACTER CHA,LINE*16,FORMT*9
C
C
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI,NDIR
      REAL GRFACT,DISPLAY,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /LAST/ IXLAST,IYLAST
      COMMON /FSCOMM/ FONTSIZE,SPACING,SPACE,FONTSCALE
C
C
        XXX=X
C
C----  I don't like the default format, so change 
C        0.123e 45
C        to 
C        1.23e 44
C
C---- SO first multiply by 0.1
C
        R=RR*0.1        
C
C----  write into the line variable the number
C      notice that since line is 12 char, the number
C      will be written right justified
C
        LINE=' '
        FORMT=' '
C
C
        IF (M.LT.6)THEN
           WRITE(6,*)' Format crap in GRflo'
           RETURN
        ELSE IF (M.GE.6.AND.
     +         ((M.LT.10.AND.R.LT.0).OR.(M.LT.9.AND.R.GE.0)))THEN
          FORMT='(E . )'          
C
C
          IF (R.GE.0)THEN
            WRITE(UNIT=FORMT(3:3),FMT='(I1)')M+1
            WRITE(UNIT=FORMT(5:5),FMT='(I1)')M-5
          ELSE 
            WRITE(UNIT=FORMT(3:3),FMT='(I1)')M
            WRITE(UNIT=FORMT(5:5),FMT='(I1)')M-6
          ENDIF          
C
C
        ELSE IF (((M.GE.10.AND.R.LT.0).OR.(M.GE.9.AND.R.GE.0)).AND.
     +            M.LE.14)THEN
          FORMT='(E  . )'
C
C
          IF (R.GE.0)THEN
            WRITE(UNIT=FORMT(3:4),FMT='(I2)')M+1
            WRITE(UNIT=FORMT(6:6),FMT='(I1)')M-5
          ELSE 
            WRITE(UNIT=FORMT(3:4),FMT='(I2)')M
            WRITE(UNIT=FORMT(6:6),FMT='(I1)')M-6
          ENDIF          
C
C
        ELSE IF (M.GT.14)THEN
          CALL GRALPHA
          WRITE(6,*)' oVERFLOW IN GRFLO'
          RETURN
        ENDIF
C
C
        IF (R.GE.0)THEN
          WRITE(UNIT=LINE(16-M:16),FMT=FORMT)R
        ELSE 
          WRITE(UNIT=LINE(17-M:16),FMT=FORMT)R
        ENDIF
C
C
        NPOS=INDEX(LINE,'.')
C        
C---   now swap the dp and the number on place to the right
C
        LINE(NPOS:NPOS)=LINE(NPOS+1:NPOS+1)
        LINE(NPOS+1:NPOS+1)='.'
C
C
        IF (R.LT.0)THEN
          LINE(17-M:17-M)='-'
        ENDIF
C
C---- now take n characters , where the last is m after the DP
C
        DO 1 I=17-M,16
C
C----    and write each char 
C
          CALL DRAWCHA(XXX,Y,LINE(I:I))
1        CONTINUE
C
C
        IXLAST=XXX+FONTSIZE*(SPACING+SPACE)
C
        RETURN
        END
C
C
C
      SUBROUTINE GRFLUSH
C     =====================
C
C
C---- Opens the graphics unit and clears screen, puts graphics
C     display into alpha state
C
C---- For sigma, set default colour, clear pixel store
C
C
C
C---- Dummy set of NGX,NGY,NLI if not using a graphics device
C
C
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
      LOGICAL VMSVAX
C     ..
C     .. Local Scalars ..
      REAL DUM1,DUM2
      INTEGER IDUM3
C     ..
C     .. External Subroutines ..
      EXTERNAL TKFLUSH
      LOGICAL VAXVMS
      EXTERNAL VAXVMS
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /SITE/ VMSVAX
C     ..
C
      IF (NGR.EQ.2) CALL TKFLUSH
      END
C
C
C
        SUBROUTINE GRINT(X,Y,N,M)
C       =============================
C
C
C
C
      INTEGER X,Y,M,N,XXX,YYY,I,SPACE,SPACING,IXLAST,IYLAST
      REAL FONTSIZE,FONTSCALE
      CHARACTER CHA,LINE*12,FORMT*5
C
C
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI,NDIR
      REAL GRFACT,DISPLAY,YSCALE,XSCALE,XOFFSET,YOFFSET
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /LAST/ IXLAST,IYLAST
      COMMON /FSCOMM/ FONTSIZE,SPACING,SPACE,FONTSCALE
C
C
        XXX=X
C
C----  write into the line variable the number
C      notice that since line is 12 char, the number
C      will be written right justified
C
        FORMT='(I  )'
C
C
        IF (M.GE.10)THEN
          WRITE(UNIT=FORMT(3:4),FMT='(I2)')M
        ELSE
          WRITE(UNIT=FORMT(3:3),FMT='(I1)')M
        ENDIF
C
C
        WRITE(UNIT=LINE(13-M:12),FMT=FORMT)N
C
C---- Now take the last "M" characters
C
        DO 1 I=13-M,12
C
C----    and write each char 
C
          CALL DRAWCHA(XXX,Y,LINE(I:I))
1        CONTINUE
C
C
        IXLAST=XXX+FONTSIZE*(SPACING+SPACE)
C
        RETURN
        END            
C
C
C
      SUBROUTINE GRINTEG(INUM,NDIG,SIZX,SIZY,NJUST)
C     ================================================
C
C
C---- Ouput an integer INUM in field length NDIG,
C    size SIZX by SIZY character units
C
C  NJUST = -1 left justify
C        =  0 centre
C        =  1 right justify
C
C
C
C
C     .. Scalar Arguments ..
      REAL SIZX,SIZY
      INTEGER INUM,NDIG,NJUST
C     ..
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
C     ..
C     .. External Subroutines ..
      EXTERNAL PL84INTEG
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
C     ..
      IF (NGR.EQ.3) CALL PL84INTEG(INUM,NDIG,SIZX,SIZY,NJUST)
      END
C
C
C
      SUBROUTINE GRMOVE(IX,IY)
C     ===========================
C
C
C---- Move to IX,IY in graphics mode
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER IX,IY
C     ..
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
C     ..
C     .. External Subroutines ..
      EXTERNAL LNO3MOVE,PL84MOVE,SIGMOVE,TKMOVE
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET

          save
C     ..
C
C----	now check the direction
C       if ndir=1 then no offset
C       if ndir=2 then put picture at top etc
C
	IF (NGR.EQ.5.OR.NGR.EQ.7.OR.NGR.EQ.8)THEN
C
C
	 IF (NDIR.EQ.2)THEN
	  N23=NINT(FLOAT(NGX)*2.0/4.0)
	  N=IX
	  IX=IY+N23
	  IY=NGY-N
	 ELSE IF (NDIR.EQ.3)THEN
	  N13=NINT(FLOAT(NGX)/4.0)
	  N=IX
	  IX=IY+N13
	  IY=NGY-N
	 ELSE IF (NDIR.EQ.4)THEN
	  N=IX
	  IX=IY
	  IY=NGY-N
	 ENDIF
C
C
	ENDIF
C
C
	IX=IX+XOFFSET
	IY=IY+YOFFSET
C
C
      IF ((IX.LT.0) .OR. (IX.GT.NGX-1) .OR. (IY.LT.0) .OR.
     +    (IY.GT.NGY-1)) THEN
C
C---- Debug
C      write(6,100) ix,iy
C 100  format(1x,'attempt to write outside display area ix,iy',2i7)
C
      ELSE IF (NGR.EQ.1) THEN
        CALL SIGMOVE(IX,IY)
      ELSE IF (NGR.EQ.2) THEN
        CALL TKMOVE(IX,IY)
      ELSE IF (NGR.EQ.3) THEN
        CALL PL84MOVE(IX,IY)
      ELSE IF (NGR.EQ.4) THEN
        CALL LNO3MOVE(IX,IY)
      ELSE IF (NGR.EQ.6) THEN
        CALL SGMOVE(IX,IY)
      ELSE IF (NGR.EQ.5) THEN
        CALL  HPMD(IX,IY,.TRUE.)
      ELSE IF (NGR.EQ.7) THEN
        CALL CALCOMMD(IX,IY,.TRUE.)
      ELSE IF (NGR.EQ.8) THEN
        CALL POSTMD(IX,IY,.TRUE.)
      END IF
C
C
      END

C
C
C
      SUBROUTINE GRNEWLINE(X)
C     ===========================
C
C
C---- Newline with a spacing X*charsize
C
C
C
C
C     .. Scalar Arguments ..
      REAL X
C     ..
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
C     ..
C     .. External Subroutines ..
      EXTERNAL PL84NEWLINE
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
C     ..
      IF (NGR.EQ.3) CALL PL84NEWLINE(X)
      END
C
C
C
      SUBROUTINE GRNEWPAGE
C     ========================
C
C
C---- Rings bell and waits until c/r entered, when it clears
C     the page and starts a new page. only operative on TX
C
C
C
C
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
C     ..
C     .. External Subroutines ..
      EXTERNAL TKNEWPAGE
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
C     ..
      IF (NGR.NE.1) THEN
        IF (NGR.EQ.2) CALL TKNEWPAGE
      END IF
      END
C
C
C
      SUBROUTINE GRORIGIN(X,Y)
C     ============================
C
C
C---- Define an origin (in mm) for PLOT84
C
C
C
C
C     .. Scalar Arguments ..
      REAL X,Y
C     ..
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
C     ..
C     .. External Subroutines ..
      EXTERNAL PL84ORIGIN
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
C     ..
      IF (NGR.EQ.3) CALL PL84ORIGIN(X,Y)
      END
C
C
C
      SUBROUTINE GRPIXINIT
C     ========================
C
C
C---- Initialise terminal for drawing a series of points,
C     rather than lines.
C
C
C
C
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
C     ..
C     .. External Subroutines ..
      EXTERNAL SIGPIXINIT,TKPIXINIT
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
C     ..
      IF (NGR.EQ.1) THEN
        CALL SIGPIXINIT
      ELSE
        CALL TKPIXINIT
      END IF
      END
C
C
C
      SUBROUTINE GRPOINT(IX,IY)
C     =============================
C
C
C---- Draw a point (ie one pixel) at position IX,IY.
C     graphics device must have been initialised correctly
C     by a single call to GRPIXINIT.
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER IX,IY
C     ..
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
C     ..
C     .. External Subroutines ..
      EXTERNAL LNO3POINT,SIGPOINT,TKPOINT
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
C     ..
      IF (NGR.EQ.1) THEN
        CALL SIGPOINT(IX,IY)
      ELSE IF (NGR.EQ.2) THEN
        CALL TKPOINT(IX,IY)
      ELSE IF (NGR.EQ.4) THEN
        CALL LNO3POINT(IX,IY)
      ELSE IF (NGR.EQ.6) THEN
        CALL SGPOINT(IX,IY)
      END IF
      END
C
C
C
      SUBROUTINE GRPOSN(IX,IY,IFLAG,IER)
C     =====================================
C
C
C---- Change current position to ix,iy to position following output
C     only needed for tk because the sigma scrolls its screen.
C     leave display in alpha mode
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER IER,IFLAG,IX,IY
C     ..
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
C     ..
C     .. External Subroutines ..
      EXTERNAL TKMOVE
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
C     ..
      write(*,*)
      IF (NGR.EQ.2) CALL TKMOVE(IX,IY)
      END


C
C
C
      SUBROUTINE GRREAL(X,NDIG,NAFTER,SIZX,SIZY,NJUST)
C     ===================================================
C
C
C---- Ouput an integer X in field length NDIG,
C     NAFTER characters after decimal,
C     size SIZX by SIZY character units
C
C  NJUST = -1 left justify
C        =  0 centre
C        =  1 right justify
C
C
C
C
C     .. Scalar Arguments ..
      REAL SIZX,SIZY,X
      INTEGER NAFTER,NDIG,NJUST
C     ..
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
C     ..
C     .. External Subroutines ..
      EXTERNAL PL84REAL
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
C     ..
      IF (NGR.EQ.3) CALL PL84REAL(X,NDIG,NAFTER,SIZX,SIZY,NJUST)
      END
C
C
C
      SUBROUTINE GRSTRING(STR)
C     ===========================
C
C
C---- Output a string
C
C
C
C
C     .. Scalar Arguments ..
      CHARACTER STR* (*)
C     ..
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
C     ..
C     .. External Subroutines ..
      EXTERNAL PL84STRING
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
C     ..
      IF (NGR.EQ.3) CALL PL84STRING(STR)
      END
C
C
C
      SUBROUTINE GRXCUR(X)
C     ========================
C
C
C---- Move the cursor X character positions along X
C     from current position
C
C
C
C
C     .. Scalar Arguments ..
      REAL X
C     ..
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI
C     ..
C     .. External Subroutines ..
      EXTERNAL PL84XCUR
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
C     ..
      IF (NGR.EQ.3) CALL PL84XCUR(X)
      END
C
C
C
        SUBROUTINE HPCOL(N)
C       ====================
C
C
C---- to send a pen colour command to the calcom or hp plotter
C        as both the commands are the same in each language
C
C
        CHARACTER*80 LINE,WORD*4
        INTEGER POINTER
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /BUFFER/ LINE,POINTER
C
C
        WORD(1:4)='SP ;'
        WRITE(UNIT=WORD(3:3),FMT='(I1)')N
C
C                        
        IF (POINTER.GT.74)THEN
C
C----     buffer is full
C
          WRITE(88,'(A)')LINE(1:POINTER)
          LINE=' '
          POINTER=5
          LINE(2:5)=WORD(1:4)
        ELSE
C
C----     add the data to buffer
C
          LINE(POINTER+1:POINTER+4)=WORD(1:4)
          POINTER=POINTER+4
        ENDIF
C
C
        RETURN
        END
C
C
C
        SUBROUTINE HPCV(ON)
C       ====================
C
C
        INTEGER X,Y,POINTER,LENGTH
        CHARACTER LINE*80,OUTPUT*4
        LOGICAL ON
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /BUFFER/ LINE,POINTER
C
C
        IF (ON)THEN
          OUTPUT(1:4)='cv1;'
        ELSE
          OUTPUT(1:4)='cv0;'
        ENDIF
C
C----  now check the buffer
C
        IF (POINTER.GT.74)THEN
C
C---     buffer is full
C
          WRITE(88,'(A)')LINE(1:POINTER)
          LINE=' '
          POINTER=5
          LINE(2:5)=OUTPUT(1:4)
        ELSE
C
C---     add the data to buffer
C
          LINE(POINTER+1:POINTER+4)=OUTPUT(1:4)
          POINTER=POINTER+4
        ENDIF
C
        RETURN
        END
C
C
C
        SUBROUTINE HPINIT
C       ==================
C
C
        CHARACTER LINE*80
        INTEGER POINTER
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /BUFFER/ LINE,POINTER
C        
        CALL PLOTOPEN(1)
C
        LINE=' '
        LINE(2:2)=CHAR(27)
        LINE(3:3)=CHAR(46)
        LINE(4:4)='('
        WRITE(88,'(A)')LINE(1:5)
        LINE(4:80)=
     1    'I81;;17:  N;19:  M500:IN;GM999;VS10;PU;SP1;SI1.0,1.0;'
        LINE(12:12)=CHAR(27)
        LINE(13:13)=CHAR(46)
        LINE(19:19)=CHAR(27)
        LINE(20:20)=CHAR(46)
        WRITE(88,'(A)')LINE(1:55)
        POINTER=1
        LINE=' '
C
        RETURN
        END
C
C
C
        SUBROUTINE HPMD(X,Y,MOVE)
C       ==========================
C
C
        INTEGER X,Y,POINTER,LENGTH
        CHARACTER LINE*80,OUTPUT*20
        LOGICAL MOVE,CONT
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /BUFFER/ LINE,POINTER
      COMMON /VECTORSAVE/ CONT
C
C
        CALL WRITEXY(OUTPUT,X,Y,LENGTH)
C
C
        IF (MOVE)THEN        
          OUTPUT(3:20)=OUTPUT(1:18)
          OUTPUT(1:2)='PU'
        ELSE
C
C
          IF (CONT)THEN
            OUTPUT(3:20)=OUTPUT(1:18)
            OUTPUT(1:2)='PA'
          ELSE
            OUTPUT(3:20)=OUTPUT(1:18)
            OUTPUT(1:2)='PD'
          ENDIF
        ENDIF
C
C
        LENGTH=LENGTH+3
        OUTPUT(LENGTH:LENGTH)=';'
C        
C----  now check the buffer
C
        IF (POINTER+LENGTH.GT.78)THEN
C
C---     buffer is full
C
          WRITE(88,'(A)')LINE(1:POINTER)
          LINE=' '
          POINTER=LENGTH+1
          LINE(2:LENGTH+1)=OUTPUT(1:LENGTH)
        ELSE
C
C---     add the data to buffer
C
          LINE(POINTER+1:POINTER+LENGTH)=OUTPUT(1:LENGTH)
          POINTER=POINTER+LENGTH
        ENDIF
C
        RETURN
        END
C
C
C
      SUBROUTINE LNO3DRAW(IX,IY)
C     ===========================
C
C
C     .. Scalar Arguments ..
      INTEGER IX,IY
C     ..
C     .. Local Scalars ..
      REAL X,Y
C     ..
C     .. External Subroutines ..
      EXTERNAL PLOT3
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC REAL
C     ..
      X = REAL(IX)*8/2400
      Y = REAL(IY)*8/2400
      CALL PLOT3(X,Y,2)
      END
C
C
C
C    **** LNO3 ROUTINES ****
C         =============
C
C
      SUBROUTINE LNO3INIT(DUM1,DUM2,IDUM3)
C     =====================================
C
C
C
C---- Emulates the calcomp routine "PLOTS" for a laser printer.
C
C
C
C
C     .. Scalar Arguments ..
      REAL DUM1,DUM2
      INTEGER IDUM3
C     ..
C     .. Scalars in Common ..
      REAL FACT,XOFFS,YOFFS
      INTEGER IPC,IPEN
C     ..
C     .. Local Scalars ..
      BYTE BEGIN
      CHARACTER FILNAM*100
      INTEGER ISTAT
C     ..
C     .. Common blocks ..
      COMMON /LASOFFS/XOFFS,YOFFS,IPC,FACT,IPEN
      LOGICAL ONLINE,LMB,DLAB,IMPC
      COMMON /IOO/IOUT,IUNIT,ONLINE,ITIN,ITOUT,INOD,INMO,IDU,NWRN,
     +            LMB,DLAB,IMPC,IGUNIT
C     ..
      FILNAM = ' '
      BEGIN = 4
      FACT = 1.0
      XOFFS = 0.0
      YOFFS = 0.0
      IPC = 0.0
      CALL UGTENV ('PLOT',FILNAM)
      IF(FILNAM.EQ.' ') FILNAM = 'PLOT'
      IGUNIT = 92
      ISTAT = 0
      CALL CCPDPN(92, FILNAM, 'NEW', 'F',0, ISTAT)
      IF (ISTAT.NE.0) CALL CCPERR(1,'Can''t open PLOT file')
      WRITE (92,FMT=6000) BEGIN
      WRITE (92,FMT=6000) 'initgraphics'
      WRITE (92,FMT=6000) 'newpath'
      WRITE (92,FMT=6000) '/inch {72 mul} def'
      WRITE (92,FMT=6000) '.3 setlinewidth'
      WRITE (92,FMT=6000) '2.0 inch -2.0 inch translate'
C
 6000 FORMAT (A)
      END
C
C
C
      SUBROUTINE LNO3MOVE(IX,IY)
C     ===========================
C
C
C     .. Scalar Arguments ..
      INTEGER IX,IY
C     ..
C     .. Local Scalars ..
      REAL X,Y
C     ..
C     .. External Subroutines ..
      EXTERNAL PLOT3
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC REAL
C     ..
      X = REAL(IX)*8/2400
      Y = REAL(IY)*8/2400
      CALL PLOT3(X,Y,3)
      END
C
C
C
      SUBROUTINE LNO3POINT(IX,IY)
C     ============================
C
C
C     .. Scalar Arguments ..
      INTEGER IX,IY
C     ..
C     .. Local Scalars ..
      REAL X,Y
C     ..
C     .. External Subroutines ..
      EXTERNAL PLOT3
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC REAL
C     ..
      X = REAL(IX)*8/2400
      Y = REAL(IY)*8/2400
C
C---- move  to x,y
C
      CALL PLOT3(X+0.01,Y,3)
C
C---- draw  point at x,y
C
      CALL PLOT3(X-0.01,Y,2)
      END
C
C
C
      SUBROUTINE LNSYMBOL(X,Y,TZ,LABEL,ANGLE,NCHAR)
C     ===========================================
C
C
C---- Emulates calcomp routine "SYMBOL"
C
C
C
C     .. Scalar Arguments ..
      REAL ANGLE,TZ,X,Y
      INTEGER NCHAR
      CHARACTER LABEL*160
C     ..
C     .. Scalars in Common ..
      REAL FACT,XOFFS,YOFFS
      INTEGER IPC,IPEN
C     ..
C     .. Local Scalars ..
      REAL ANG,CORR,V1,V2
      INTEGER ISF
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
C     .. Common blocks ..
      COMMON /LASOFFS/XOFFS,YOFFS,IPC,FACT,IPEN
C     ..
      ANG = ANGLE - 90.0
      IPC = IPC + 1
      V1 = (Y/2.51+YOFFS)*FACT
      V2 = 26.0/2.51 - (X/2.51+XOFFS)*FACT
      IF (NCHAR.LT.0) THEN
        CORR = (TZ/5.02)*FACT
        WRITE (92,FMT=6000) V1 - CORR,' inch',V2,' inch',' moveto'
        WRITE (92,FMT=6000) V1 + CORR,' inch',V2,' inch',' lineto'
        WRITE (92,FMT=6000) V1,' inch',V2 - CORR,' inch',' moveto'
        WRITE (92,FMT=6000) V1,' inch',V2 + CORR,' inch',' lineto'
      ELSE
        ISF = NINT(FACT*TZ*40.0)
        IF (ISF.LE.0) ISF = 1
        IF (IPEN.LE.2) THEN
          WRITE (92,FMT=6002) '/Times-Roman findfont ',ISF,
     +      ' scalefont setfont'
        ELSE
          WRITE (92,FMT=6002) '/Times-Bold findfont ',ISF,
     +      ' scalefont setfont'
        END IF
        WRITE (92,FMT=6000) V1,' inch',V2,' inch',' moveto'
        WRITE (92,FMT='(F6.1,'' rotate'')') ANG
        WRITE (92,FMT=6004) '(',LABEL(1:NCHAR),') show'
        WRITE (92,FMT='(F6.1,'' rotate'')') - ANG
      END IF
C
 6000 FORMAT (1X,F8.4,A5,F8.4,A5,A7)
 6002 FORMAT (A22,I2,A18)
 6004 FORMAT (A1,A,A6)
      END
C
C
C
      SUBROUTINE NEWPEN(JPEN)
C     =======================
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER JPEN
C     ..
C     .. Scalars in Common ..
      REAL FACT,XOFFS,YOFFS
      INTEGER IPC,IPEN
C     ..
C     .. Local Scalars ..
      REAL WIDTH
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC REAL
C     ..
C     .. Common blocks ..
      COMMON /LASOFFS/XOFFS,YOFFS,IPC,FACT,IPEN
C     ..
      IPEN = JPEN
      WRITE (92,FMT='(A)') 'stroke'
      WIDTH = REAL(IPEN)*0.2
      WRITE (92,FMT=6000) WIDTH
 6000 FORMAT (F4.1,' setlinewidth')
      END
C
C
C
        INTEGER FUNCTION NVAL(R)
C       ========================
C
C
C---- TEK scale function to convert squid units to tek absolute
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
C
C
        RR=R*XSCALE
        NVAL=NINT(RR)
C
        RETURN
        END
C
C
C
      SUBROUTINE PLOT3(X,Y,IOPT)
C     ========================
C
C
C
C---- Emulates the calcomp routine "PLOT" on a laser printer.
C
C
C
C     .. Scalar Arguments ..
      REAL X,Y
      INTEGER IOPT
C     ..
C     .. Scalars in Common ..
      REAL FACT,XOFFS,YOFFS
      INTEGER IPC,IPEN
C     ..
C     .. Local Scalars ..
      REAL V1,V2,XX
      INTEGER JOPT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     ..
C     .. Common blocks ..
      COMMON /LASOFFS/XOFFS,YOFFS,IPC,FACT,IPEN
C     ..
      JOPT = IOPT
      IPC = IPC + 1
      V1 = X/2.51 + XOFFS
      V2 = Y/2.51 + YOFFS
C
C
      IF (JOPT.LT.0) THEN
        XOFFS = V1
        YOFFS = V2
        JOPT = ABS(JOPT)
      END IF
C
C
      XX = V1
      V1 = V2*FACT
      V2 = 26.0/2.51 - XX*FACT
C
C
      IF (JOPT.EQ.2) THEN
        WRITE (92,FMT=6002) V1,' inch',V2,' inch',' lineto'
      ELSE IF (JOPT.EQ.3) THEN
C
C
        IF (IPC.GE.100) THEN
          WRITE (92,FMT=6000) 'stroke'
          IPC = 0
        END IF
C
C
        WRITE (92,FMT=6002) V1,' inch',V2,' inch',' moveto'
      ELSE IF (JOPT.EQ.999 .OR. JOPT.EQ.-999) THEN
        WRITE (92,FMT=6000) 'stroke'
        WRITE (92,FMT=6000) 'showpage'
      ELSE
        WRITE (6,FMT=6004) IOPT
      END IF
C
C
 6000 FORMAT (A)
 6002 FORMAT (1X,F8.4,A5,F8.4,A5,A7)
 6004 FORMAT ('0PS> option is not in the Postscript library.          ',
     +       '  Please add it in.',I5)
      END
C      
       SUBROUTINE PLOTOPEN (IDUM)
       INTEGER IDUM
       END
C
C
C
        SUBROUTINE POSTINIT
C       ====================
C
C
        CALL PLOTOPEN(2)
C
      WRITE(88,*)' /H { show } def'
      WRITE(88,*)' /N { newpath } def'
      WRITE(88,*)' /S { stroke } def'
      WRITE(88,*)' /M { moveto } def'
      WRITE(88,*)' /L { lineto } def'
      WRITE(88,*)' 0.4 setlinewidth '
      WRITE(88,*)' newpath'
C
C
        RETURN
        END        
C
C
C
      SUBROUTINE POSTMD(IX,IY,MOVE)
C     ==============================
C
C
        INTEGER POINTER,LENGTH,NUMBER
        CHARACTER LINE*80,OUTPUT*20
        REAL X,Y
        LOGICAL MOVE
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /DRAWINSTRUCTIONS/ NUMBER
      COMMON /BUFFER/ LINE,POINTER
C
C
        X=FLOAT(NGY-IY)/10.0
        Y=FLOAT(IX)/10.0
        OUTPUT=' '
C
        IF (NUMBER.GT.1000)THEN
          CALL WRITERXY(OUTPUT,X,Y,LENGTH)
          OUTPUT(6:20)=OUTPUT(1:15)
          OUTPUT(1:5)=' S N '
          LENGTH=LENGTH+8
          OUTPUT(LENGTH-1:LENGTH-1)='M'
C
C
          IF (POINTER+LENGTH.GT.78)THEN
C
C----       buffer is full
C
            WRITE(88,'(A)')LINE(1:POINTER)
            LINE=' '
            POINTER=LENGTH+1
            LINE(2:LENGTH+1)=OUTPUT(1:LENGTH)
          ELSE
C
C----       add the data to buffer
C
            LINE(POINTER+1:POINTER+LENGTH)=OUTPUT(1:LENGTH)
            POINTER=POINTER+LENGTH
          ENDIF
C
C
          NUMBER=0
          OUTPUT=' '
        ELSE
          NUMBER = NUMBER + 1
        ENDIF          
C
C
        CALL WRITERXY(OUTPUT,X,Y,LENGTH)
C
C
        IF (MOVE)THEN        
          NUMBER=0
          OUTPUT(6:20)=OUTPUT(1:15)
          OUTPUT(1:5)=' S N '
          LENGTH=LENGTH+5
          OUTPUT(LENGTH+2:LENGTH+2)='M'
        ELSE
          OUTPUT(LENGTH+2:LENGTH+2)='L'
        ENDIF
C
C
        LENGTH=LENGTH+3
C        
C----  now check the buffer
C
        IF (POINTER+LENGTH.GT.79)THEN
C
C---     buffer is full
C
          WRITE(88,'(A)')LINE(1:POINTER)
          LINE=' '
          POINTER=LENGTH+1
          LINE(2:LENGTH+1)=OUTPUT(1:LENGTH)
        ELSE
C
C----     add the data to buffer
C
          LINE(POINTER+1:POINTER+LENGTH)=OUTPUT(1:LENGTH)
          POINTER=POINTER+LENGTH
        ENDIF
C
C
        RETURN
        END
C
C
C
        REAL FUNCTION RVAL(N)
C       =====================
C
C
C---- tek scale function to convert tek units to squid absolute
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
C
C
        RVAL=FLOAT(N)/XSCALE
C
        RETURN
        END
C
C
C
      SUBROUTINE SGALPHA
C     ===================
C
C
C
      RETURN
      END
C
C
C
        SUBROUTINE SGCLEAR
C       ===================
C
C---- Subroutine to clear Silicon graphics window
C
        CALL RESHAP
        CALL COLOR(0)
        CALL CLEAR
        CALL COLOR(7)
        RETURN
        END
C
C
C
      SUBROUTINE SGCROSS(IX,IY)
C     ==========================
C
C
C---- Draw a cross
C
C
C     .. Scalar Arguments ..
      INTEGER IX,IY
C     ..
C     .. Local Scalars ..
      INTEGER ILX,ILY,ISIZE,ISX,ISY
C     ..
C     .. External Subroutines ..
      EXTERNAL SGDRAW,SGMOVE
C     ..
C     .. Data statements ..
      DATA ISIZE/8/
C     ..
      ISX = IX - ISIZE
      ILX = IX + ISIZE
      ISY = IY - ISIZE
      ILY = IY + ISIZE
      CALL SGMOVE(ISX,IY)
      CALL SGDRAW(ILX,IY)
      CALL SGMOVE(IX,ISY)
      CALL SGDRAW(IX,ILY)
      END
C
C
C
      SUBROUTINE SGCURSOR(X,Y,CHA)
C     =============================
C
C---- Subroutine to return position of cross hairs
C
*  replacement for the things formerly picked up from sgi include
* files fgl.h and fdevice.h
      INTEGER BLUE, CYAN, GREEN, RED, WHITE, YELLOW
      PARAMETER (BLUE = 4, CYAN = 6, GREEN = 2, RED = 1, WHITE = 7,
     $     YELLOW = 3)
      INTEGER GETVAL, QREAD, KEYBD, MOUSEX, MOUSEY, MPROJE, MVIEWI,
     $     WINOPE
      PARAMETER (KEYBD = 513, MOUSEX = 266, MOUSEY = 267, MPROJE = 1,
     $     MVIEWI = 2)
C#include "fgl.h"
C#include "fdevice.h"
cc-al         include 'fgl.h'
cc-al         include 'fdevice.h'
C
C
        INTEGER X,Y,NUM,INT,SX,SY,AX,AY
        INTEGER*2 DATA,TOP,BOTTOM,LEFT,RIGHT
        CHARACTER CHA
        COMMON/LAST/IXLAST,IYLAST
C
C
C
        WRITE(6,'(A)')' Graphics input> '
C
C            *************
        CALL QDEVIC(KEYBD)
C            *************
C
1       CONTINUE
C
C            ******
        CALL QRESET
C            ******
C        
        INT=QREAD(DATA)
        IF (DATA.LT.20.OR.DATA.GT.127)GOTO 1
        CHA=CHAR(DATA)
C
C            *************
        CALL GETORI(AX,AY)
        CALL GETSIZ(SX,SY)
C            *************
C
        X = GETVAL(MOUSEX)-AX
        Y = GETVAL(MOUSEY)-AY
C
C
        X=NINT(FLOAT(X)*1024.0/FLOAT(SX))
        Y=NINT(FLOAT(Y)*768.0/FLOAT(SY))
        IXLAST=X
        IYLAST=Y
C
C            ************
        CALL COLOR(WHITE)
        CALL TOMSYM(7)
C            ************
C
        WRITE(6,'(A)')' Text input> '
C
      RETURN
      END
C
C
C
        SUBROUTINE SGDECODE(STRING,IY,IX)
C       ===================================
C
C---- The bytes are returned in the order:
C  Character, high X, low X, high Y, low Y - 
C  and then optionally CR and EOT
C  The 'tag' bits are ignored here.
C
        CHARACTER*6 STRING
C
C
        IXHIGH=ICHAR(STRING(2:2))-32
        IXLOW=ICHAR(STRING(3:3))-32
        IYHIGH=ICHAR(STRING(4:4))-32
        IYLOW=ICHAR(STRING(5:5))-32
C
        IX=IXHIGH*32+IXLOW
        IY=IYHIGH*32+IYLOW
C
        RETURN
        END
C
      SUBROUTINE SGDRAW(IX,IY)
C     =========================
C
C
      INTEGER IX,IY
C
C          *************
      CALL DRAW2I(IX,IY)
C          *************
C
      RETURN
      END
C
C
C
        SUBROUTINE SGENCODE(IX,IY,OUTPUT)
C       ==================================
C
C---- Pack coordinates into 4 bytes for Silicon graphics window
C  Each coordinate can require up to 10 bits.  These are split into two
C  groups of five - one group in the low order bits of each byte.
C  Bits 5 and 6 are used as "tag' bits - see below.  The eighth bit (bit 7)
C  is not used.
C
C  IT is used to set the TAG bits.
C  Bytes are sent in the order:  high y, low y, high x, low x
C  with respective tag bits +32,+96,+32,+64
C
        CHARACTER OUTPUT*4
        INTEGER XLOW,YLOW
C
C
        XLOW = MOD(IX,32)
        YLOW = MOD(IY,32)
        OUTPUT(4:4)=CHAR(XLOW+64)
        OUTPUT(3:3)=CHAR((IX-XLOW)/32+32)
        OUTPUT(2:2)=CHAR(YLOW+96)
        OUTPUT(1:1)=CHAR((IY-YLOW)/32+32)
C
        RETURN
        END
C
C
C
      SUBROUTINE SGEND
C     =================
C
C
      RETURN
      END


C
C
C
	SUBROUTINE SGEXGR
C       ==================
C
	RETURN
	END
C
C
C
        SUBROUTINE SGINIT
C       ==================
C
C            ******
        CALL RESHAP
C            ******
C
        RETURN
        END
C
C
C
      SUBROUTINE SGMOVE(IX,IY)
C     =========================
C
C
      INTEGER IX,IY
      INTEGER XN,YN,FLAG
      CHARACTER GS,FS
      CHARACTER STRING*4
      LOGICAL GRMODE,POINTMODE
      COMMON /TX/ GRMODE,POINTMODE
C
C
C
      JX=IX
      JY=IY
C
C          *************
      CALL MOVE2I(IX,IY)
C          *************
C
      RETURN
      END
C
C
C
	SUBROUTINE SGNEWPAGE
C       =====================
C
C
C            *****
        CALL CLEAR
C            *****
C
	RETURN
	END
C
C
C
      SUBROUTINE SGPIXINIT
C     =====================
C
C
      RETURN
      END
C
C
C
      SUBROUTINE SGPOINT(IX,IY)
C     ==========================
C
C
C---- Draw a cross
C
C
C     .. Scalar Arguments ..
      INTEGER IX,IY
C     ..
C     .. Local Scalars ..
      INTEGER ILX,ILY,ISIZE,ISX,ISY
C     ..
C     .. External Subroutines ..
      EXTERNAL SGDRAW,SGMOVE
C     ..
C     .. Data statements ..
      DATA ISIZE/2/
C     ..
      ISX = IX - ISIZE
      ILX = IX + ISIZE
      ISY = IY - ISIZE
      ILY = IY + ISIZE
      CALL SGMOVE(ISX,IY)
      CALL SGDRAW(ILX,IY)
      CALL SGMOVE(IX,ISY)
      CALL SGDRAW(IX,ILY)
      END
C
C
C
        SUBROUTINE SGSTARTUP
C       =====================
C
C
C        this is a start up routine for graphics, nothing for gino
C        for the own graphics , the font is loaded
C
C
C
* replacement for the things formerly picked up from sgi include
* files fgl.h and fdevice.h
      INTEGER BLUE, CYAN, GREEN, RED, WHITE, YELLOW
      PARAMETER (BLUE = 4, CYAN = 6, GREEN = 2, RED = 1, WHITE = 7,
     $     YELLOW = 3)
      INTEGER GETVAL, QREAD, KEYBD, MOUSEX, MOUSEY, MPROJE, MVIEWI,
     $     WINOPE
      PARAMETER (KEYBD = 513, MOUSEX = 266, MOUSEY = 267, MPROJE = 1,
     $     MVIEWI = 2)
C#include "fgl.h"
C#include "fdevice.h"
cc-al         include 'fgl.h'
cc-al         include 'fdevice.h'
C
C
        INTEGER*2 IND
        LOGICAL HELPOPEN
        BYTE      POSXY,NUMBER,TYPE,BYTE1,BYTE2,BYTE3,WIDTH
        INTEGER K,I,INUM,J,NU,SPACE,SPACING,NN
        REAL FONTSIZE,FONTSCALE
        REAL IDMAT (4,4)
C
C
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI,NDIR
      REAL GRFACT,DISPLAY,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /FONTDAT/ IND(127),POSXY(2,2000),NUMBER(127),
     +                  TYPE(2000),WIDTH(127)
      COMMON /FSCOMM/ FONTSIZE,SPACING,SPACE,FONTSCALE
      COMMON /HELPFILE/ HELPOPEN
      LOGICAL ONLINE,LMB,DLAB,IMPC
      COMMON /IOO/IOUT,IUNIT,ONLINE,ITIN,ITOUT,INOD,INMO,IDU,NWRN,
     +            LMB,DLAB,IMPC,IGUNIT
      INTEGER ISTAT
C
CMJH
C Start up the SG graphics.
C
        DO 111 I=1,4
          DO 211 J=1,4
            IDMAT(I,J)=0.
            IF(I.EQ.J)IDMAT(I,J)=1.
 211      CONTINUE 
 111    CONTINUE 
C
C 
        CALL FOREGR
        CALL KEEPAS(1024,768)
        NN=WINOPE('SQUID',5)
        CALL GCONFI
        CALL MMODE(MPROJE)
        CALL ORTHO2(0.0,1024.0,0.0,768.0)
        CALL MMODE (MVIEWI)
        CALL LOADMA(IDMAT)
        CALL QDEVIC(528)
        CALL COLOR(0)
        CALL CLEAR
C
C
        ISTAT = 0
        CALL CCPDPN(2, 'SQUIDFONT', 'READONLY', 'U', 0, ISTAT)
        IF (ISTAT.NE.0) GOTO 2222
C
        K=1
C
C
        DO 200 I=1,127
          READ(2,END=222)BYTE1,BYTE2,BYTE3,
     1            (TYPE(J),POSXY(1,J),POSXY(2,J),J=K,K+BYTE2-1)
          INUM=BYTE1
          NU=BYTE2
          IND(INUM)=K
          NUMBER(INUM)=NU
          WIDTH(INUM)=BYTE3
          K=K+NU
C
C
          IF (K.GT.2000)THEN
            WRITE(6,*)'  No more room for font'
            STOP
          ENDIF
C
C
200        CONTINUE
222        CONTINUE
        WRITE(6,*)' Number of characters in font = ',I-1
        WRITE(6,*)' Number of points (max=2000)  = ',K
        CLOSE (2)
C
C---- Now try to find the help
C
        ISTAT = 0
        CALL CCPDPN(91, 'HLP/HELP.RAB', 'READONLY', 'F', 0, ISTAT)
        IF (ISTAT .NE. 0) GOTO 99
        HELPOPEN=.TRUE.
C
        CALL TOMSIZ(2.5,2.5)
C
        RETURN        
C
2222        CONTINUE
C
        WRITE(6,*)'  '        
        WRITE(6,*)'  No character font found !'
        WRITE(6,*)'  have you assigned a file to font '
        WRITE(6,*)'      Ugh '
        STOP
C
99        CONTINUE
         WRITE(6,*)'  Could not find main help file '
         HELPOPEN=.FALSE.         
         RETURN
        END
C
C
C
      SUBROUTINE SIGALPHA
C     ====================
C
C
C---- Sets ALPHA state
C     Set ALPHA screen on and graphic screen off
C
C---- AA - ALPHA screen ON
C     GB - Graphics screen OFF
C     DB - newline sequence to trigger transition from
C          Graphics to ALPHA state
C
C
C
C
C     .. Scalars in Common ..
      INTEGER ICOLOR
      LOGICAL GRMODE,POINTMODE
C     ..
C     .. Common blocks ..
      COMMON /SIGMA/GRMODE,ICOLOR,POINTMODE
C     ..
      IF (GRMODE) THEN
C
        WRITE (12,FMT='(''AAGBDB''/)')
        GRMODE = .FALSE.
        POINTMODE = .FALSE.
      END IF
      END
C
C
C
      SUBROUTINE SIGCLEAR
C     ====================
C
C
C---- Clears graphic screen
C     Leaves Sigma in same state as before
C
C---- DA - clear pixel store
C     DB - newline sequence to trigger transition
C          from Graphics to ALPHA state
C
C
C
C
C     .. Scalars in Common ..
      INTEGER ICOLOR
      LOGICAL GRMODE,POINTMODE
C     ..
C     .. Common blocks ..
      COMMON /SIGMA/GRMODE,ICOLOR,POINTMODE
C     ..
      IF (GRMODE) THEN
        WRITE (12,FMT='(''DA'')')
      ELSE
        WRITE (12,FMT='(''+-*/DADB''/)')
      END IF
      END
C
C
C
      SUBROUTINE SIGCOL(ICOL)
C     ========================
C
C
C---- Sets color of next vector
C     Leaves Sigma in same state as before
C     ICOL must lie in range 0 to 15 (0 is useless).
C     These are the default table setting.
C     See page 9-25 in the Sigma manual.
C
C  0  black - useful !!
C  1  green
C  2  blue
C  3  red
C  4  cyan
C  5  magenta
C  6  orange
C  7  yellow
C  8  light green
C  9  light blue
C 10  pink
C 11  dark green
C 12  dark blue
C 13  dark red
C 14  grey
C 15  white
C
C---- HIn - Select color (pixel input data) n is hex -
C           default is F = 15 = white
C     DB - newline sequence to trigger transition from
C          Graphics to ALPHA state
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER ICOL
C     ..
C     .. Scalars in Common ..
      INTEGER ICOLOR
      LOGICAL GRMODE,POINTMODE
C     ..
C     .. Common blocks ..
      COMMON /SIGMA/GRMODE,ICOLOR,POINTMODE
C     ..
      IF (ICOL.NE.ICOLOR) THEN
        IF (ICOL.GE.0 .AND. ICOL.LE.15) THEN
C
          IF (GRMODE) THEN
            WRITE (12,FMT='(''HI'',Z1)') ICOL
          ELSE
            WRITE (12,FMT='(''+-*/HI'',Z1,''DB''/)') ICOL
          END IF
          ICOLOR = ICOL
        END IF
      END IF
      END
C
C
C
C    **** SIGMA ROUTINES ****
C         ==============
C
C
      SUBROUTINE SIGCURSOR(IX,IY)
C     ===========================
C
C
C---- Puts SIGMA into graphics mode, graphics screen on,
C     ALPHA screen off
C
C---- Puts cursor up at screen center
C     Coordinates of cursor are returned in IX,IY after a 'HIT'.
C     After hit turns ALPHA screen on and cursor off
C     Leave graphics screen on
C
C
C
C---- Check that point lies within screen boundary
C
C     .. Scalar Arguments ..
      INTEGER IX,IY
C     ..
C     .. Scalars in Common ..
      INTEGER ICOLOR
      LOGICAL GRMODE,POINTMODE
C     ..
C     .. Local Scalars ..
      INTEGER IXX,IYY
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MAX,MIN
C     ..
C     .. Common blocks ..
      COMMON /SIGMA/GRMODE,ICOLOR,POINTMODE
C     ..
      IXX = MIN(IX,767)
      IXX = MAX(IXX,0)
      IYY = MIN(IY,511)
      IYY = MAX(IYY,0)
C
C---- Debug
C     if(.not.GRmode) write(12,fmt='(''+-*/GAAB'',$)')
C     write(12,fmt='(''CACEGI'',2i3,''CFHF'')') ixx+100,iyy+100
C
      IF (GRMODE) THEN
        WRITE (12,FMT='(''CACEGI'',2I3,''CFHF'')') IXX + 100,IYY + 100
      ELSE
        WRITE (12,FMT='(''+-*/GAABCACEGI'',2I3,''CFHF'')') IXX + 100,
     +    IYY + 100
      END IF
C
      READ (12,FMT=6000) IX,IY
C
      WRITE (12,FMT=6002)
      GRMODE = .FALSE.
 6000 FORMAT (1X,I3,1X,I3)
 6002 FORMAT ('+-*/AACBDB',/)
      END
C
C
C
      SUBROUTINE SIGDRAW(IX,IY)
C     ==========================
C
C
C---- Draws a line from current position to point ix,iy
C
C
C---- Check that point lies within screen boundary
C
C     .. Scalar Arguments ..
      INTEGER IX,IY
C     ..
C     .. Scalars in Common ..
      INTEGER ICOLOR
      LOGICAL GRMODE,POINTMODE
C     ..
C     .. Local Scalars ..
      INTEGER IXX,IYY
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MAX,MIN
C     ..
C     .. Common blocks ..
      COMMON /SIGMA/GRMODE,ICOLOR,POINTMODE
C     ..
      IXX = MIN(IX,767)
      IXX = MAX(IXX,0)
      IYY = MIN(IY,511)
      IYY = MAX(IYY,0)
C
      IF (GRMODE) THEN
        WRITE (12,FMT='(''GJ'',2I3)') IXX + 100,IYY + 100
      ELSE
        WRITE (12,FMT='(''+-*/DD000DCGAABGJ'',2I3)') IXX + 100,IYY + 100
        GRMODE = .TRUE.
      END IF
      END
C
C
C
      SUBROUTINE SIGINIT
C     ===================
C
C
C---- DA - clears pixel store
C     GH - graphic flag sequence recognized at any time
C     DB - newline sequence to trigger transition from Graphics
C          to ALPHA state
C     HIn - Select color (pixel input data) n is hex
C
C
C
C
C     .. Scalars in Common ..
      INTEGER ICOLOR
      LOGICAL GRMODE,POINTMODE
C     ..
      CHARACTER FILNAM*100
C     .. Common blocks ..
      LOGICAL ONLINE,LMB,DLAB,IMPC
      COMMON /IOO/IOUT,IUNIT,ONLINE,ITIN,ITOUT,INOD,INMO,IDU,NWRN,
     +            LMB,DLAB,IMPC,IGUNIT
      COMMON /SIGMA/GRMODE,ICOLOR,POINTMODE
C     ..
      INTEGER ISTAT
      FILNAM = ' '
      CALL UGTENV('GRAPHICS',FILNAM)
      IF (FILNAM.EQ.' ') FILNAM = 'GRAPHICS'
      IGUNIT = 12
      ISTAT = 0
      CALL CCPDPN(12, FILNAM, 'UNKNOWN', 'F', 0, ISTAT)
      IF (ISTAT.NE.0) CALL CCPERR(1,'Can''t open GRAPHICS')
C
C---- reset default color to white
C
      ICOLOR = 15
C
      WRITE (12,FMT='(''+-*/DAGHHI'',Z1,''DB''/)') ICOLOR
      GRMODE = .FALSE.
      POINTMODE = .FALSE.
      END
C
C
C
      SUBROUTINE SIGMOVE(IX,IY)
C     ==========================
C
C
C---- Moves to point (x,y) in graphics mode
C
C
C
C---- Check that point lies within screen boundary
C
C     .. Scalar Arguments ..
      INTEGER IX,IY
C     ..
C     .. Scalars in Common ..
      INTEGER ICOLOR
      LOGICAL GRMODE,POINTMODE
C     ..
C     .. Local Scalars ..
      INTEGER IXX,IYY
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MAX,MIN
C     ..
C     .. Common blocks ..
      COMMON /SIGMA/GRMODE,ICOLOR,POINTMODE
C     ..
      IXX = MIN(IX,767)
      IXX = MAX(IXX,0)
      IYY = MIN(IY,511)
      IYY = MAX(IYY,0)
C
      IF (GRMODE) THEN
        WRITE (12,FMT='(''GI'',2I3)') IXX + 100,IYY + 100
      ELSE
        WRITE (12,FMT='(''+-*/DD000DCGAABGI'',2I3)') IXX + 100,IYY + 100
        GRMODE = .TRUE.
      END IF
      END
C
C
C
      SUBROUTINE SIGPIXINIT
C     ======================
C
C
C---- Set sigma to graphics mode and dot/point mode
C
C
C
C
C     .. Scalars in Common ..
      REAL DISPLAY,GRFACT
      INTEGER ICOLOR,NGR,NGX,NGY,NHALFX,NHALFY,NLI
      LOGICAL GRMODE,POINTMODE
C     ..
C     .. Common blocks ..
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /SIGMA/GRMODE,ICOLOR,POINTMODE
C     ..
      WRITE (12,FMT='(''+-*/DD000DCGAABDACHGJ'')')
      GRMODE = .TRUE.
      POINTMODE = .TRUE.
      END
C
C
C
      SUBROUTINE SIGPOINT(IX,IY)
C     ==========================
C
C
C     .. Scalar Arguments ..
      INTEGER IX,IY
C     ..
      WRITE (12,FMT=6000) IX + 100,IY + 100
 6000 FORMAT (1X,2I3)
      END
C
C
C
        REAL FUNCTION STRINGLENGTH(LENGTH,SIZE)
C       ========================================
C
C
        REAL FONTSIZE,FONTSCALE,LENGTH
        INTEGER SPACING,SPACE
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /FSCOMM/ FONTSIZE,SPACING,SPACE,FONTSCALE
C
C----  this returns the length of a string of length
C      "length" in the units of program (180*140)
C      The number 0.0167 is the default font size to generate
C      characters of the correct size where 2.5 = alpha mode
C
C
        STRINGLENGTH=LENGTH*SIZE*(FONTSCALE/0.01664)
C
        RETURN
        END
C
C
C
        SUBROUTINE TDVEND
C       =================
C
C
C---- finishes device call (pltn or hp7475 or t4010)
C
      CHARACTER LINE*80
      LOGICAL WANTNEWPAGE
      INTEGER POINTER
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /NEWPAGE/ WANTNEWPAGE
      COMMON /BUFFER/ LINE,POINTER
C
C
        IF (NGR.EQ.5)THEN
          WRITE(88,'(A)')LINE(1:POINTER)
          WRITE(88,'(A)')' PU;SP;PA10984,7684;'
          IF (WANTNEWPAGE)WRITE(88,'(A)')' PG; '
C
C
           WRITE(88,'(A)')' '
        LINE=' '
        LINE(2:2)=CHAR(27)
        LINE(3:3)=CHAR(46)
        LINE(4:4)=')'
        WRITE(88,'(A)')LINE(1:5)
          CLOSE(88)
        ENDIF
C
C
        IF (NGR.EQ.5)THEN
          WRITE(88,'(A)')LINE(1:POINTER)
          CLOSE(88)
        ENDIF
C
C
        IF (NGR.EQ.2)CALL GRALPHA
C
C
        IF (NGR.EQ.4)THEN
          WRITE(88,'(A)')LINE(1:POINTER)
          WRITE(88,'(A)')' S '
          IF (WANTNEWPAGE)WRITE(88,'(A)')' SHOWPAGE'
          CLOSE(88)
        ENDIF          
C
C
        LINE=' '
        POINTER=1
C
        RETURN
        END
C
C        
C
        SUBROUTINE TEKFIN
C       ==================
C
C
        RETURN
        END
C
C
C
        SUBROUTINE TEKSTR
C       =================
C
        RETURN
        END
C
C
C
        SUBROUTINE TGIEND
C       =================
C
C
C---- finishes call to graphics
C
C
C        call ginend
C
        RETURN
        END
C
C
C
        SUBROUTINE TGINO
C       ================
C
C        call gino
C
        RETURN
        END
C
C
C
        SUBROUTINE THP7475
C       ==================
C
C
C---- calls an hpgl device for output for plots
C         
C        IMPT
C           output to a file "hp.plt"
C           device plotting limits = 240.0 x 200.0
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
C
C
        NGR=5
        CALL GRINIT
C
        RETURN
        END
C
C
C
      SUBROUTINE TKALPHA
C     ===================
C
C
C
C
C     .. Scalars in Common ..
      LOGICAL GRMODE,POINTMODE
C     ..
C     .. Local Scalars ..
      CHARACTER CR
C     ..
C     .. External Subroutines ..
      EXTERNAL TKEXGR
C     ..
C     .. Common blocks ..
      COMMON /TX/GRMODE,POINTMODE
C     ..
C     .. Data statements ..
c-vms      DATA CR/13/
C     ..
C
      CR = CHAR(13)
C
C
      IF (GRMODE) THEN
        WRITE (12,FMT=6000) CR
        CALL TKEXGR
      END IF
C
C
      CALL TKFLUSH
 6000 FORMAT (A)
      END


C
C
C
      SUBROUTINE TKCLEAR
C     ===================
C
C
C---- Subroutine to clear tektronix screen
C
C
C     .. Local Scalars ..
      CHARACTER ESC,FF
C     ..
      LOGICAL VMSVAX
      COMMON /SITE/ VMSVAX
C     .. Data statements ..
c-vms      DATA ESC/27/
c-vms      DATA FF/12/
C     ..
C
      ESC = CHAR(27)
      FF = CHAR(12)
C
C
      IF (VMSVAX) THEN
      WRITE (12,FMT=6000) ESC,FF
      ELSE
      WRITE (12,FMT=6001) ESC,FF
      END IF
      CALL TKFLUSH
C
C
 6000 FORMAT ('+',2A,$)
 6001 FORMAT (2A,$)
      END
C
C
C
      SUBROUTINE TKCROSS(IX,IY)
C     ==========================
C
C
C---- Draw a cross
C
C
C     .. Scalar Arguments ..
      INTEGER IX,IY
C     ..
C     .. Local Scalars ..
      INTEGER ILX,ILY,ISIZE,ISX,ISY
C     ..
C     .. External Subroutines ..
      EXTERNAL TKDRAW,TKMOVE
C     ..
C     .. Data statements ..
      DATA ISIZE/8/
C     ..
      ISX = IX - ISIZE
      ILX = IX + ISIZE
      ISY = IY - ISIZE
      ILY = IY + ISIZE
      CALL TKMOVE(ISX,IY)
      CALL TKDRAW(ILX,IY)
      CALL TKMOVE(IX,ISY)
      CALL TKDRAW(IX,ILY)
      END
c----------------------------------------------------------
      subroutine TKCURSOR(ix,iy,cha)
c----------------------------------------------------------
c
      character x,cha
      integer esc,sub
c      BYTE STRING(6)
      CHARACTER*6 string
      data esc /27/,sub/26/,x/'+'/
      LOGICAL VMSVAX
      COMMON /SITE/ VMSVAX
c
      write(12,10) char(esc),char(sub)
10    format(2a,$)
99      continue

c      READ (12,FMT=6002) (STRING(J),J=1,6)
c      CALL TKDECODE(STRING(2),IY,IX)
      read(5,20,end=99) string
20    format(a)
C
C---- Put up a cross at cursor position
C
      call tkdecode(string,iy,ix)
      cha=string(1:1)
      CALL TKCROSS(IX,IY)
      CALL TKALPHA
c
         

6002   FORMAT (6A)
      return
      end

c------------------------------------------------------
c**********************************************************************
c
	subroutine TKDECODE(STRING,iy,ix)
c
C  The bytes are returned in the order:
C  Character, high X, low X, high Y, low Y - and then optionally CR and EOT
C  The 'tag' bits are ignored here.
c
	CHARACTER*6 STRING

	ixhigh=ichar(string(2:2))-32
	ixlow=ichar(string(3:3))-32
	iyhigh=ichar(string(4:4))-32
	iylow=ichar(string(5:5))-32

	ix=ixhigh*32+ixlow
	iy=iyhigh*32+iylow

	return
	end


c
C
C
C
      SUBROUTINE TKDRAW(IX,IY)
C     =========================
C
C
C---- Assumes already in graphics mode
C
C
C     .. Scalar Arguments ..
      INTEGER IX,IY
C     ..
C     .. Scalars in Common ..
      LOGICAL GRMODE,POINTMODE
C     ..
C     .. Local Scalars ..
      INTEGER J,JX,JY
      CHARACTER FS,GS
C     ..
C     .. Local Arrays ..
C      BYTE STRING(4)
       character string*4     
C     ..
C     .. External Subroutines ..
      EXTERNAL TKENCODE
C     ..
C     .. Common blocks ..
      LOGICAL VMSVAX
      COMMON /SITE/ VMSVAX
      COMMON /TX/GRMODE,POINTMODE
       SAVE
C     ..
C     .. Data statements ..
c-vms      DATA GS/29/,FS/28/
C     ..
C
       GS = CHAR(29)
       FS = CHAR(28)
C
C
      JX = IX
      JY = IY
      CALL TKENCODE(JX,JY,STRING)
C
C
      IF (VMSVAX) THEN
      WRITE (12,FMT=6000) (STRING)
      ELSE
      WRITE (12,FMT=6001) (STRING)
      END IF
C
C
 6000 FORMAT ('+',A,$)
 6001 FORMAT (A,$)
      END
	subroutine TKENCODE(ix,iy,output)
c
C  Pack coordinates into 4 bytes for Tektronic 4010 terminal
C  Each coordinate can require up to 10 bits.  These are split into two
C  groups of five - one group in the low order bits of each byte.
C  Bits 5 and 6 are used as "tag' bits - see below.  The eighth bit (bit 7)
C  is not used.
C
C  IT is used to set the TAG bits.
C  Bytes are sent in the order:  high y, low y, high x, low x
C  with respective tag bits +32,+96,+32,+64
c
	character output*4
	integer xlow,ylow

	xlow = mod(ix,32)
	ylow = mod(iy,32)
	output(4:4)=char(xlow+64)
	output(3:3)=char((ix-xlow)/32+32)
	output(2:2)=char(ylow+96)
	output(1:1)=char((iy-ylow)/32+32)
c
	return
	end
c
c**********************************************************************
C
C
C
      SUBROUTINE TKEND
C     =================
C
C
C---- Subroutine to restore terminal working in tektronix emulation
C     to its original state.
C     The code given here is for Micro-term 4560 terminals and may
C     be different for other terminals.
C
C
C     .. Scalars in Common ..
      LOGICAL GRMODE,POINTMODE
C     ..
C     .. Local Scalars ..
      CHARACTER CAN
C     ..
C     .. Common blocks ..
      LOGICAL VMSVAX
      COMMON /SITE/ VMSVAX
      COMMON /TX/GRMODE,POINTMODE
C     ..
C     .. Data statements ..
c-vms      DATA CAN/24/
C     ..
C
      CAN = CHAR(24)
C
      IF (VMSVAX) THEN
      WRITE (12,FMT=6000) CAN
      ELSE
      WRITE (12,FMT=6001) CAN
      END IF
C
C
      GRMODE = .FALSE.
      POINTMODE = .FALSE.
C
C
 6000 FORMAT ('+',A,$)
 6001 FORMAT (A,$)
      END
C
C
C
      SUBROUTINE TKEXGR
C     ==================
C
C
C---- Subroutine to exit from graphics mode
C
C
C     .. Scalars in Common ..
      LOGICAL GRMODE,POINTMODE
C     ..
C     .. Local Scalars ..
      CHARACTER US,ESC,ANSI
C     ..
C     .. Common blocks ..
      COMMON /TX/GRMODE,POINTMODE
      LOGICAL VMSVAX
      COMMON /SITE/ VMSVAX
      CHARACTER*200 EMULATION
C     ..
C     .. Data statements ..
c-vms      DATA US/31/
c-vms      DATA ESC /27/
      DATA ANSI /'2'/
C     ..
C
      US = CHAR(31)
      ESC = CHAR(27)
C
      EMULATION = ' '
C
C          *****************************
      CALL UGTENV('GRAF_MODE',EMULATION)
      CALL CCPUPC(EMULATION)
C          *****************************
C
      IF (EMULATION.EQ.' ') EMULATION = 'PERICOM'
C
C
      IF(EMULATION.EQ.'MICRO_TERM' .OR. EMULATION .EQ.
     +                'PERICOM') THEN
C
C
         IF(VMSVAX) THEN
         WRITE (12,FMT=6000) US
         ELSE
         WRITE (12,FMT=6001) US
         END IF
       GO TO 200
       END IF
C
C
      IF(EMULATION.EQ.'SELENAR') THEN
C
C
      IF (VMSVAX) THEN
      WRITE(12,100)ESC,ANSI
      ELSE
      WRITE(12,101)ESC,ANSI
      END IF
C
C
      END IF
C
C
200   CONTINUE
C
C
      GRMODE = .FALSE.
      POINTMODE = .FALSE.
C
C
100     FORMAT('+',2A,$)
101     FORMAT(2A,$)
 6000   FORMAT ('+',A,$)
 6001   FORMAT (A,$)
      END

C
C
C
      SUBROUTINE TKFLUSH
C     ==================
C
C
C
C
C     .. Scalars in Common ..
      LOGICAL GRMODE,POINTMODE
C     ..
      CHARACTER FILNAM*100
C     .. Common blocks ..
      LOGICAL VMSVAX
      COMMON /SITE/ VMSVAX
      COMMON /TX/GRMODE,POINTMODE
C     ..
      CHARACTER*1 ESC,T4014,GS,BRAK,STAR
      CHARACTER*200 EMULATION
      DATA T4014 /'1'/, BRAK/'['/, STAR/'*'/
C
      GS = CHAR(29)
      ESC = CHAR(27)
C
C
      CLOSE (UNIT=12)
C
      FILNAM = ' '
      CALL UGTENV('GRAPHICS',FILNAM)
      IF(FILNAM.EQ.' ') FILNAM='GRAPHICS'
      OPEN (UNIT=12,FILE=FILNAM,STATUS='UNKNOWN')
C
C
      EMULATION = ' '
      CALL UGTENV('GRAF_MODE',EMULATION)
      CALL CCPUPC(EMULATION)
C
C---- hardwired for YORK pericoms... SITE SPECIFIC
C
      IF (EMULATION.EQ.' ') EMULATION = 'PERICOM'
C
C
       IF (EMULATION.EQ.'SELENAR') THEN
C
C
           IF (VMSVAX) THEN
           WRITE(12,10)ESC,T4014
           ELSE
           WRITE(12,6000)ESC,T4014
           END IF
C
C
       ELSE IF (EMULATION.EQ.'MICRO_TERM') THEN
C
C
           IF (VMSVAX) THEN
           WRITE(12,100)GS
           ELSE
           WRITE(12,6100)GS
           END IF
C
C
        ELSE IF (EMULATION.EQ.'PERICOM') THEN
C
C
          IF (VMSVAX) THEN
          WRITE(12,7000) ESC,BRAK,STAR
          ELSE
          WRITE(12,7001) ESC,BRAK,STAR
          END IF
C
C
       END IF
C
       POINTMODE = .FALSE.
C
C
100       FORMAT('+',A,$)
6100      FORMAT(A,$)
10        FORMAT('+',2A,$)
6000      FORMAT(2A,$)
7000      FORMAT('+',3A,$)
7001      FORMAT(3A,$)
      END
C
C
C
      SUBROUTINE TKINIT
C     ==================
C
C
C
C
C     .. Scalars in Common ..
      LOGICAL GRMODE,POINTMODE
C     ..
C     .. External Subroutines ..
      EXTERNAL TKCLEAR
C     ..
      CHARACTER FILNAM*100
C     .. Common blocks ..
      LOGICAL VMSVAX
      COMMON /SITE/ VMSVAX
      COMMON /TX/GRMODE,POINTMODE
C     ..
      CHARACTER*1 ESC,T4014,GS,BRAK,STAR
      CHARACTER*200 EMULATION
c-vms      DATA GS /29/
c-vms      DATA ESC /27/
      DATA T4014 /'1'/, BRAK/'['/, STAR/'*'/
C
      GS = CHAR(29)
      ESC = CHAR(27)
C
C
C
      FILNAM = ' '
      CALL UGTENV('GRAPHICS',FILNAM)
      IF(FILNAM.EQ.' ') FILNAM='GRAPHICS'
      OPEN (UNIT=12,FILE=FILNAM,STATUS='UNKNOWN')
C
C
      EMULATION = ' '
      CALL UGTENV('GRAF_MODE',EMULATION)
      CALL CCPUPC(EMULATION)
C
C---- hardwired for YORK pericoms... SITE SPECIFIC
C
      IF (EMULATION.EQ.' ') EMULATION = 'PERICOM'
C
C
       IF (EMULATION.EQ.'SELENAR') THEN
C
C
           IF (VMSVAX) THEN
           WRITE(12,10)ESC,T4014
           ELSE
           WRITE(12,6000)ESC,T4014
           END IF
C
C
       ELSE IF (EMULATION.EQ.'MICRO_TERM') THEN
C
C
           IF (VMSVAX) THEN
           WRITE(12,100)GS
           ELSE
           WRITE(12,6100)GS
           END IF
C
C
        ELSE IF (EMULATION.EQ.'PERICOM') THEN
C
C
          IF (VMSVAX) THEN
          WRITE(12,7000) ESC,BRAK,STAR
          ELSE
          WRITE(12,7001) ESC,BRAK,STAR
          END IF
C
C
       END IF
C
C
      CALL TKCLEAR
      POINTMODE = .FALSE.
C
C
100       FORMAT('+',A,$)
6100      FORMAT(A,$)
10        FORMAT('+',2A,$)
6000      FORMAT(2A,$)
7000      FORMAT('+',3A,$)
7001      FORMAT(3A,$)
      END
C
C
C
      SUBROUTINE TKMOVE(IX,IY)
C     ========================
C
C
C---- Puts terminal into graphics mode before move.
C
C
C     .. Scalar Arguments ..
      INTEGER IX,IY
C     ..
C     .. Scalars in Common ..
      LOGICAL GRMODE,POINTMODE
C     ..
C     .. Local Scalars ..
      INTEGER J,JX,JY
      CHARACTER FS,GS
C     ..
C     .. Local Arrays ..
      BYTE STRING(4)
C     ..
C     .. External Subroutines ..
      EXTERNAL TKENCODE
C     ..
C     .. Common blocks ..
      COMMON /TX/GRMODE,POINTMODE
      LOGICAL VMSVAX
      COMMON /SITE/ VMSVAX
C     ..
C     .. Data statements ..
c-vms      DATA GS/29/,FS/28/
C     ..
C
      GS = CHAR(29)
      FS = CHAR(28)
C
C
      JX = IX
      JY = IY
C
C
      IF (VMSVAX) THEN
      WRITE (12,FMT=6000) GS
      ELSE
      WRITE (12,FMT=6001) GS
      END IF
C
C
      CALL TKENCODE(JX,JY,STRING)
C
C
      IF (VMSVAX) THEN
      WRITE (12,FMT=6002) (STRING(J),J=1,4)
      ELSE
      WRITE (12,FMT=6003) (STRING(J),J=1,4)
      END IF
C
C
      GRMODE = .TRUE.
      RETURN
C
C
 6000 FORMAT ('+',A,$)
 6001 FORMAT (A,$)
 6002 FORMAT ('+',4A,$)
 6003 FORMAT (4A,$)
      END
C
C
C
C    **** TEKTRONIX ROUTINES ****
C         ==================
C
C
      SUBROUTINE TKNEWPAGE
C     =====================
C
C
C     .. Local Scalars ..
      CHARACTER BELL*1
C     ..
      LOGICAL VAXVMS
C     .. External Subroutines ..
      EXTERNAL TKALPHA,TKCLEAR,VAXVMS
C     ..
C     .. Data statements ..
c-vms      DATA BELL/7/
C     ..
      BELL = CHAR(7)
C
      WRITE (12,FMT='(1X,A)') BELL
cc??      IF (VAXVMS())  CALL TKALPHA
      CALL TKALPHA
      WRITE (6,FMT=6002)
      READ (5,FMT=6000,END=10)
      CALL TKCLEAR
   10 RETURN
 6000 FORMAT (1X)
 6002 FORMAT(' Enter C/R ')
      END
C
C
C
      SUBROUTINE TKPIXINIT
C     =====================
C
C
C---- Initialise graphics device to point plot mode.
C     **** not available for 4010 devices ****
C
C
C     .. Scalars in Common ..
      LOGICAL GRMODE,POINTMODE
C     ..
C     .. Local Scalars ..
      CHARACTER FS,GS
C     ..
C     .. Common blocks ..
      LOGICAL VMSVAX
      COMMON /SITE/ VMSVAX
      COMMON /TX/GRMODE,POINTMODE
C     ..
C     .. Data statements ..
c-vms      DATA GS/29/,FS/28/
C     ..
C
      GS = CHAR(29)
      FS = CHAR(28)
C
C
      IF (VMSVAX) THEN
      WRITE (12,FMT=6000) GS
      WRITE (12,FMT=6000) FS
      ELSE
      WRITE (12,FMT=6001) GS
      WRITE (12,FMT=6001) FS
      END IF
C
C
      POINTMODE = .TRUE.
C
C
 6000 FORMAT ('+',A,$)
 6001 FORMAT (A,$)
      END
C
C
C
      SUBROUTINE TKPOINT(IX,IY)
C     =========================
C
C
C
C
C     .. Scalar Arguments ..
      INTEGER IX,IY
C     ..
C     .. Scalars in Common ..
      LOGICAL GRMODE,POINTMODE
C     ..
C     .. Local Scalars ..
      INTEGER J,JX,JY
C     ..
C     .. Local Arrays ..
      BYTE STRING(4)
C     ..
C     .. External Subroutines ..
      EXTERNAL TKENCODE,TKPIXINIT
C     ..
C     .. Common blocks ..
      LOGICAL VMSVAX
      COMMON /SITE/ VMSVAX
      COMMON /TX/GRMODE,POINTMODE
C     ..
C
C
      IF (.NOT.POINTMODE) CALL TKPIXINIT
      JX = IX
      JY = IY
      CALL TKENCODE(JX,JY,STRING)
C
C
      IF (VMSVAX) THEN
      WRITE (12,FMT=6000) (STRING(J),J=1,4)
      ELSE
      WRITE (12,FMT=6001) (STRING(J),J=1,4)
      END IF
C
C
 6000 FORMAT ('+',4A,$)
 6001 FORMAT (4A,$)
      END
C
C
C
        SUBROUTINE TOMATT(N)
C       ====================
C
C
C---- sets the speed of plotting (always n=10 in squid)
C
C
        REAL X,Y
C
C----  needs thinking about
C       needed for hpgl
C
        RETURN
        END
C
C
C
        SUBROUTINE TOMCHA(TEXT)
C       =======================
C
C
C---- write the characters in text starting at the current
C     position on the screen
C
C
C
        CHARACTER*(*) TEXT
        INTEGER X,Y,LENGTH,I,IXLAST,IYLAST,SPACING,SPACE,XXX
        REAL FONTSIZE,FONTSCALE
C
      REAL GRFACT,DISPLAY,XSCALE,YSCALE,XOFFSET,YOFFSET
      INTEGER NGR,NGX,NGY,NHALFX,NHALFY,NLI,NDIR
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /LAST/ IXLAST,IYLAST
      COMMON /FSCOMM/ FONTSIZE,SPACING,SPACE,FONTSCALE
C
C
        XXX=IXLAST
        Y=IYLAST
        LENGTH=LEN(TEXT)
C
C
        DO 1 I=1,LENGTH
C
C----    and write each char 
C
          CALL DRAWCHA(XXX,Y,TEXT(I:I))
1        CONTINUE
C
C
C          
        IXLAST=XXX+FONTSIZE*(SPACING+SPACE)
C
        RETURN
        END
C
C
C
        SUBROUTINE TOMCUR(IKEY,X,Y)
C       ===========================
C
C
C          routine to turn on the cursor and 
C          return the position of the cursor when called
C          and the character pressed when called
C
C
      REAL X,Y
      INTEGER IKEY
      CHARACTER CHA
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /LAST/ IXLAST,IYLAST
C
C
        CALL GRCURSOR(IX,IY,CHA)
        IKEY=ICHAR(CHA)
        IXLAST=IX
        IYLAST=IY
        X=RVAL(IX)
        Y=RVAL(IY)
        CALL GRMOVE(IX,IY)
        CALL GRALPHA
C
        RETURN
        END
C
C
C
        SUBROUTINE TOMDEV(N,M)
C       ======================
C
C---- opens a device for writing plotter output
C
C----        blank ,  this is no longer used as the device
C                     is open in the initiallise section
C
        RETURN
        END
C
C
C
        SUBROUTINE TOMDOT(X)
C       ===================
C
C
        REAL X
C
C        call dot(x)
C
        RETURN
        END
C
C
C
        SUBROUTINE TOMDSH(N,X,Y,Z)
C       ==========================
C
C
C---- calls a dashed line 
C
C
      REAL X,Y,Z,REPEAT,SOLID,BIT,RDASH
      INTEGER N
      LOGICAL DASH,FLAG,FLAG2
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /SETDAS/ DASH,REPEAT,SOLID,BIT,GAP,RDASH,FLAG,FLAG2,NN
C
C
        IF (N.NE.0)THEN
          DASH=.TRUE.
        ELSE
          DASH=.FALSE.
        ENDIF
C
C
        BIT=Z
        SOLID=Y
        REPEAT=X
        IF (N.EQ.2.OR.N.EQ.-2)REPEAT=REPEAT+BIT
        GAP=(REPEAT-SOLID-BIT)/2.0
        RDASH=0.0
        FLAG=.TRUE.
        FLAG2=.TRUE.
        NN=N
C
C----  The first is the dash line discripter
C      1 ... Simple dash of repeat x, solid bit of length y
C      2 ... Double dash of repeat y, solid bits of length y ,z
C      < 0 then remember the current status of the line
C      0 ... Return to solid line
C
        RETURN
        END
C
C
C
        SUBROUTINE TOMFIX(R,N,M)
C       ========================
C
C
C---- to write a fix format real
C        r=number , n=total field , m=dp position
C
      REAL R
      INTEGER IXLAST,IYLAST,X,Y,N,M
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /LAST/ IXLAST,IYLAST
C
C
        X=IXLAST
        Y=IYLAST
        CALL GRFIX(X,Y,R,N,M)
C
        RETURN
        END
C
C
C
        SUBROUTINE TOMFLO(R,LENGTH)
C       ===========================
C
C
C---- to write a  floating point number
C        r=number , length=total field
C
      REAL R
      INTEGER LENGTH,X,Y
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /LAST/ IXLAST,IYLAST
C
        X=IXLAST
        Y=IYLAST
        CALL GRFLO(X,Y,R,LENGTH)
C
        RETURN
        END
C
C
C
        SUBROUTINE TOMINT(N,LENGTH)
C       ==========================
C
C
C---- writes an e/nteger at the current plotting position
C     n=number, length=field
C
      INTEGER N,LENGTH,X,IXLAST,Y,IYLAST
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /LAST/ IXLAST,IYLAST
C
C
        X=IXLAST
        Y=IYLAST
        CALL GRINT(X,Y,N,LENGTH)
C
        RETURN
        END
C
C
C
        SUBROUTINE TOMLIN(X,Y)
C       ======================
C
C
C        draw a line from the last position to (x,y)
C
      LOGICAL DASH,FLAG,FLAG2,FLAG3
      REAL X,Y,REPEAT,SOLID,BIT,LENGTH,SLOPE,XX,YY,XTEMP
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /LAST/ IXLAST,IYLAST
      COMMON /SETDAS/ DASH,REPEAT,SOLID,BIT,GAP,RDASH,FLAG,FLAG2,N
C
C
        IF (DASH)THEN
C
C
          IF (N.GT.0)THEN
           FLAG=.TRUE.
           FLAG2=.TRUE.
           RDASH=0.0
          ENDIF
C
C
          XLAST=RVAL(IXLAST)
          YLAST=RVAL(IYLAST)
          XTEMP=X-XLAST
C
C
          IF (ABS(XTEMP).LT.0.0001)THEN
            COSX=0
C
C
            IF (Y.LT.YLAST)THEN
              SINX=-1
            ELSE
              SINX=1
            ENDIF
C
C
          ELSE
            ANGLE=ATAN((Y-YLAST)/(XTEMP))
            COSX=COS(ANGLE)
            SINX=SIN(ANGLE)
          ENDIF
C
C
          LENGTH=SQRT((XTEMP)*(XTEMP)+
     +                (Y-YLAST)*(Y-YLAST))
          STEP=0.02
C
C
          IF (X.LT.XLAST)THEN
            LENGTH=-LENGTH
            STEP=-0.02
          ENDIF
C
C
          DO 1 R=0,LENGTH,STEP
            XX=COSX*R+XLAST
            II=NVAL(XX)
            YY=SINX*R+YLAST
            JJ=NVAL(YY)
            RDASH=RDASH+0.02
C
C
            IF (ABS(RDASH).GT.REPEAT)THEN
              RDASH=0.0
              FLAG2=.TRUE.
              FLAG=.TRUE.
              CALL GRMOVE(II,JJ)
            ENDIF
C
C
            IF (ABS(RDASH).GE.SOLID)THEN
C
C
              IF (FLAG)THEN
                CALL GRDRAW(II,JJ)
                  FLAG=.FALSE.
              ENDIF
C
C
              IF (N.EQ.2.OR.N.EQ.-2)THEN
C
C
                IF ((ABS(RDASH).GE.(SOLID+GAP)).AND.FLAG2)THEN
                  CALL GRMOVE(II,JJ)
                  FLAG2=.FALSE.                      
                  FLAG3=.TRUE.
                ENDIF
C
C
                IF ((ABS(RDASH).GE.(SOLID+GAP+BIT)).AND.FLAG3)THEN
                  CALL GRDRAW(II,JJ)
                  FLAG3=.FALSE.
                ENDIF
C
C
              ENDIF
            ENDIF
C
C
1          CONTINUE
C
C
          IXLAST=NVAL(X)
          IYLAST=NVAL(Y)
          IF (FLAG.AND.N.GT.0)CALL GRDRAW(IXLAST,IYLAST)
          IF (.NOT.(FLAG).AND.N.GT.0)CALL GRMOVE(IXLAST,IYLAST)
        ELSE
          IX=NVAL(X)
          IY=NVAL(Y)
          CALL GRDRAW(IX,IY)
          IXLAST=IX
          IYLAST=IY
        ENDIF
C
        RETURN
        END
C
C
C
        SUBROUTINE TOMMOD
C       =================
C
C---- flush of buffer for output
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /LAST/ IXLAST,IYLAST
C
C
        CALL GRMOVE(IXLAST,IYLAST)
        CALL GRALPHA
C
        RETURN
        END
C
C
C
        SUBROUTINE TOMMOV(X,Y)
C       ======================
C
C
C---- move to the position x,y
C
      REAL X,Y
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /LAST/ IXLAST,IYLAST
C
        IX=NVAL(X)
        IY=NVAL(Y)
        IXLAST=IX
        IYLAST=IY
        CALL GRMOVE(IX,IY)
C
        RETURN
        END
C
C
C
        SUBROUTINE TOMPEN(N,M,L)
C       ========================
C
C
C---- change the pen colour (number) defined to n
C     m and l are not used
C
C
        INTEGER N,M,L
C
        CALL GRCOL(N)
C
        RETURN
        END
C
C
C
        SUBROUTINE TOMSIZ(X,Y)
C       ======================
C
C
C---- change the text size on the graphics screen to x by y 
C
C
      INTEGER SPACE,SPACING
      REAL X,Y
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /FSCOMM/ FONTSIZE,SPACING,SPACE,FONTSCALE
C
        FONTSIZE=X*FONTSCALE
C
        RETURN
        END
C
C
C
        SUBROUTINE TOMSYM(N)
C       ====================
C
C
C---- to draw a symbol 
C     n= integer defining number of symbol
C
C
      INTEGER N,IX,IY,IXLAST,IYLAST
      CHARACTER CHA
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /LAST/ IXLAST,IYLAST
C
C
           IX=IXLAST
          IY=IYLAST
          CHA=CHAR(N)
          CALL DRAWCHA(IX,IY,CHA)
          X=RVAL(IXLAST)
          Y=RVAL(IYLAST)
          CALL TOMMOV(X,Y)
C
        RETURN
        END
C
C
C
        SUBROUTINE TPICCLE
C       ==================
C
C
C---- clear the graphics screen
C
C
        CALL GRCLEAR
C
        RETURN
        END
C
C
C
        SUBROUTINE TPLTN
C       ================
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /DRAWINSTRUCTIONS/ NUMBER
C
C
        NGR=8
        NUMBER=0
        CALL GRINIT
C
        RETURN
        END
C
C
C
        SUBROUTINE TPOST
C       ================
C
C
C---- calls a calcom output device for plotter output
C        IMPT 
C          output to a file  "data.plt"
C          device plotting limits = 300.0 x 300.0
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
C
C
        NGR=7
        CALL GRINIT
C
        RETURN
        END
C
C
C
        SUBROUTINE TT4010
C       =================
C
C
C---- call device "screen"
C          IMPT
C          output directly to screen
C        plotting limits =       180.0 x 140.0
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
      COMMON /DEVICE/ NO
C
        NGR=2
        CALL GRINIT
C
        RETURN
        END
C
C
C        
      SUBROUTINE WRITERXY(SEND,X,Y,LENGTH)
C     =====================================
C
C
C---- subroutine to write the value of x and y
C        in the format '  x/y' into send
C
      INTEGER LENGTH,LX,LY,DP
      CHARACTER SEND*(*)
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
C
C
      IF (X.LE.9.AND.X.GE.0)THEN
        WRITE(UNIT=SEND(1:3),FMT='(F3.1)')X
        LX=4
      ELSE IF ((X.GT.9.AND.X.LE.99).OR.
     *         (X.LT.0.AND.X.GE.-9))THEN
        WRITE(UNIT=SEND(1:4),FMT='(F4.1)')X
        LX=5
      ELSE IF ((X.GT.99.AND.X.LE.999).OR.
     *         (X.LT.-9.AND.X.GE.-99))THEN
        WRITE(UNIT=SEND(1:5),FMT='(F5.1)')X
        LX=6
      ELSE IF ((X.GT.999.AND.X.LE.9999).OR.
     *         (X.LT.-99.AND.X.GE.-999))THEN
        WRITE(UNIT=SEND(1:5),FMT='(F6.1)')X
        LX=7
      ELSE IF ((X.GT.9999.AND.X.LE.99999).OR.
     *         (X.LT.-999.AND.X.GE.-9999))THEN
        WRITE(UNIT=SEND(1:6),FMT='(F7.1)')X
        LX=8
      ENDIF
C
C
      IF (NGR.EQ.3)SEND(LX:LX)=','
      IF (NGR.EQ.5.OR.NGR.EQ.4)SEND(LX:LX)=' '
      LX=LX+1
C
C
      IF (Y.LE.9.AND.Y.GE.0)THEN
        WRITE(UNIT=SEND(LX:LX+2),FMT='(F3.1)')Y
        LY=LX+2
      ELSE IF ((Y.GT.9.AND.Y.LE.99).OR.
     *         (Y.LT.0.AND.Y.GT.-9))THEN
        WRITE(UNIT=SEND(LX:LX+3),FMT='(F4.1)')Y
        LY=LX+3
      ELSE IF ((Y.GT.99.AND.Y.LE.999).OR.
     *         (Y.LT.-9.AND.Y.GT.-99))THEN
        WRITE(UNIT=SEND(LX:LX+4),FMT='(F5.1)')Y
        LY=LX+4
      ELSE IF ((Y.GT.999.AND.Y.LE.9999).OR.
     *         (Y.LT.-99.AND.Y.GT.-999))THEN
        WRITE(UNIT=SEND(LX:LX+5),FMT='(F6.1)')Y
        LY=LX+5
      ELSE IF ((Y.GT.9999.AND.Y.LE.99999).OR.
     *         (Y.LT.-999.AND.Y.GT.-9999))THEN
        WRITE(UNIT=SEND(LX:LX+6),FMT='(F7.1)')Y
        LY=LX+6
      ENDIF
C
C
      LENGTH=LY
C
      RETURN        
      END
C
C
C
      SUBROUTINE WRITEXY(SEND,X,Y,LENGTH)
C     ====================================
C
C
C---- subroutine to write the value of x and y
C        in the format '  x/y' into send
C
C
      INTEGER X,Y,LENGTH,LX,LY
      CHARACTER SEND*(*)
C
C
      COMMON /GRAPHICS/NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      COMMON /TOMG/ NDIR,XSCALE,YSCALE,XOFFSET,YOFFSET
C
C
      IF (X.LE.9.AND.X.GE.0)THEN
        WRITE(UNIT=SEND(1:1),FMT='(I1)')X
        LX=2
      ELSE IF ((X.GT.9.AND.X.LE.99).OR.
     *         (X.LT.0.AND.X.GE.-9))THEN
        WRITE(UNIT=SEND(1:2),FMT='(I2)')X
        LX=3
      ELSE IF ((X.GT.99.AND.X.LE.999).OR.
     *         (X.LT.-9.AND.X.GE.-99))THEN
        WRITE(UNIT=SEND(1:3),FMT='(I3)')X
        LX=4
      ELSE IF ((X.GT.999.AND.X.LE.9999).OR.
     *         (X.LT.-99.AND.X.GE.-999))THEN
        WRITE(UNIT=SEND(1:4),FMT='(I4)')X
        LX=5
      ELSE IF ((X.GT.9999.AND.X.LE.99999).OR.
     *         (X.LT.-999.AND.X.GE.-9999))THEN
        WRITE(UNIT=SEND(1:5),FMT='(I5)')X
        LX=6
      ENDIF
C
C
      IF (NGR.EQ.3)SEND(LX:LX)=','
      IF (NGR.EQ.5.OR.NGR.EQ.4)SEND(LX:LX)=' '
      LX=LX+1
C
C
      IF (Y.LE.9.AND.Y.GE.0)THEN
        WRITE(UNIT=SEND(LX:LX),FMT='(I1)')Y
        LY=LX+0
      ELSE IF ((Y.GT.9.AND.Y.LE.99).OR.
     *         (Y.LT.0.AND.Y.GT.-9))THEN
        WRITE(UNIT=SEND(LX:LX+1),FMT='(I2)')Y
        LY=LX+1
      ELSE IF ((Y.GT.99.AND.Y.LE.999).OR.
     *         (Y.LT.-9.AND.Y.GT.-99))THEN
        WRITE(UNIT=SEND(LX:LX+2),FMT='(I3)')Y
        LY=LX+2
      ELSE IF ((Y.GT.999.AND.Y.LE.9999).OR.
     *         (Y.LT.-99.AND.Y.GT.-999))THEN
        WRITE(UNIT=SEND(LX:LX+3),FMT='(I4)')Y
        LY=LX+3
      ELSE IF ((Y.GT.9999.AND.Y.LE.99999).OR.
     *         (Y.LT.-999.AND.Y.GT.-9999))THEN
        WRITE(UNIT=SEND(LX:LX+4),FMT='(I5)')Y
        LY=LX+4
      ENDIF
C
C
      LENGTH=LY
C
      RETURN        
      END
