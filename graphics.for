C
C---- graphics library for use with program "mosflm"
C     allows graphics output to be directed to a sigma or
C     a tectronix (or dec terminal in tectronix emulation mode)
C
C
      SUBROUTINE GRAPH_INIT
C     =====================
C
C---- opens the graphics unit and clears screen, puts graphics
C     display into alpha state
C     for sigma, set default colour, clear pixel store
C
      COMMON /GRAPHICS/ NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
C
C---- dummy set of ngx,ngy,nli if not using a graphics device
C
      IF (NGR.EQ.0) THEN
        NLI=100000
        NGX=100000
        NGY=100000
      END IF
      IF (NGR.EQ.1) THEN
        CALL SIG_INIT
        NGX=768
        NGY=512
C
C---- number of lines on screen (dummy for sigma)
C
        NLI=1000      
      END IF
      IF (NGR.EQ.2) THEN
        CALL TK_INIT
        NGX=1024
        NGY=768
C
C---- number of lines on screen (must be reset to 24
C     for vt200 in tektronix emulation )
C
        IF (NLI.EQ.0) NLI=34      
      END IF                  
      NHALFX=NGX/2
      NHALFY=NGY/2
      RETURN
      END
C
C
      SUBROUTINE GRAPH_CLEAR
C     ======================
C
C---- clear graphics screen, leave display in same state
C
      COMMON /GRAPHICS/ NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      IF (NGR.EQ.1) CALL SIG_CLEAR
      IF (NGR.EQ.2) CALL TK_CLEAR
      RETURN
      END
C
C
      SUBROUTINE GRAPH_POSN(IX,IY,IFLAG,IER)
C     =====================================
C
C---- change current position to ix,iy to position following output
C     only needed for tk because the sigma scrolls its screen.
C     leave display in alpha mode
C
      COMMON /GRAPHICS/ NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      IF (NGR.EQ.2) CALL TK_MOVE(IX,IY)
      RETURN
      END
C
C
      SUBROUTINE GRAPH_MOVE(IX,IY)
C     ============================
C
C---- move to ix,iy in graphics mode
C
      COMMON /GRAPHICS/ NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      IF ((IX.LT.0).OR.(IX.GT.NGX-1).OR.(IY.LT.0).OR.
     +   (IY.GT.NGY-1)) THEN
        WRITE(6,100) IX,IY
 100    FORMAT(1X,'ATTEMPT TO WRITE OUTSIDE DISPLAY AREA IX,IY',2I5)
        RETURN
      END IF
      IF (NGR.EQ.1) CALL SIG_MOVE(IX,IY)
      IF (NGR.EQ.2) CALL TK_MOVE(IX,IY)
      RETURN
      END
C
C
      SUBROUTINE GRAPH_DRAW(IX,IY)
C     ============================
C
C---- draw a line from current position to ix,iy in graphics mode
C
      COMMON /GRAPHICS/ NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      IF ((IX.LT.0).OR.(IX.GT.NGX-1).OR.(IY.LT.0).OR.
     +   (IY.GT.NGY-1)) THEN
        WRITE(6,100) IX,IY
 100    FORMAT(1X,'ATTEMPT TO WRITE OUTSIDE DISPLAY AREA IX,IY',2I5)
        RETURN
      END IF
      IF (NGR.EQ.1) CALL SIG_DRAW(IX,IY)
      IF (NGR.EQ.2) CALL TK_DRAW(IX,IY)
      RETURN
      END
C
C
      SUBROUTINE GRAPH_ALPHA
C     ======================
C
C---- set display in alpha mode
C
      COMMON /GRAPHICS/ NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      IF (NGR.EQ.1) CALL SIG_ALPHA
      IF (NGR.EQ.2) CALL TK_ALPHA
      RETURN
      END
C
C
      SUBROUTINE GRAPH_COL(N)
C     =======================
C
C---- for sigma, set colour
C
      COMMON /GRAPHICS/ NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      IF (NGR.EQ.1) CALL SIG_COL(N)
      RETURN
      END
C
C
      SUBROUTINE GRAPH_CURSOR(IX,IY)
C     ==============================
C
C---- put display in graphics mode,put up cursor.
C     the position of cursor is returned in ix,iy after pressing
C     hit" key on sigma or any (?) key on tk.
C     return to alpha mode
C
      COMMON /GRAPHICS/ NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      IF (NGR.EQ.1) CALL SIG_CURSOR(IX,IY)
      IF (NGR.EQ.2) CALL TK_CURSOR(IX,IY)
      RETURN
      END
C
C
      SUBROUTINE GRAPH_NEWPAGE
C     ========================
C
C---- rings bell and waits until c/r entered, when it clears
C     the page and starts a new page. only operative on tx
C
      COMMON /GRAPHICS/ NGR,NGX,NGY,NHALFX,NHALFY,GRFACT,DISPLAY,NLI
      IF (NGR.EQ.1) RETURN
      IF (NGR.EQ.2) CALL TK_NEWPAGE
      RETURN
      END
C
C---- tektronix routines
C
C
      SUBROUTINE TK_NEWPAGE
C     =====================
C
      CHARACTER*1 BELL
C
      BELL = CHAR(7)
      WRITE(12,FMT='(1X,A)') BELL
      CALL TK_ALPHA
      WRITE(12,FMT='(1X,''ENTER C/R'')')
      READ(12,100)
 100  FORMAT(1X)
      CALL TK_CLEAR
      RETURN
      END
C
C
      SUBROUTINE TK_ALPHA
C     ===================
C
      COMMON /TX/ GRAPH_MODE
      LOGICAL GRAPH_MODE
      CHARACTER CR
C
      CR = CHAR(13)
      IF (.NOT.GRAPH_MODE) RETURN
      WRITE(12,100)CR
  100 FORMAT(A)
      CALL TK_EXGR
      RETURN
      END
C
C
      SUBROUTINE TK_CURSOR(IX,IY)
C     ===========================
C
C---- subroutine to return position of cross hairs
C
      CHARACTER ESC,SUB,X
      CHARACTER*1 STRING(6)
      DATA X/'+'/
C
      ESC = CHAR(27)
      SUB = CHAR(26)
      WRITE(12,10) ESC,SUB
10    FORMAT('+',2A,$)
      READ(12,20) (STRING(J),J=1,6)
20    FORMAT(6A)
      CALL TK_DECODE(STRING(2),IY,IX)
C
C---- put up a cross at cursor position
C
      CALL TK_CROSS(IX,IY)
      CALL TK_ALPHA
      RETURN
      END
C
C
      SUBROUTINE TK_CROSS(IX,IY)
C     ==========================
C
C---- draw a cross
C
      DATA ISIZE/8/
      ISX=IX-ISIZE
      ILX=IX+ISIZE
      ISY=IY-ISIZE
      ILY=IY+ISIZE
      CALL TK_MOVE(ISX,IY)
      CALL TK_DRAW(ILX,IY)
      CALL TK_MOVE(IX,ISY)
      CALL TK_DRAW(IX,ILY)
      RETURN
      END
C
C
      SUBROUTINE TK_MOVE(IX,IY)
C     =========================
C
      COMMON /TX/ GRAPH_MODE
      LOGICAL GRAPH_MODE
      CHARACTER*1 GS,FS,STRING(4)
C
      GS = CHAR(29)
      FS = CHAR(28)
      JX=IX
      JY=IY
      WRITE(12,20) GS
20    FORMAT('+',A,$)
90    CALL TK_ENCODE(JX,JY,STRING)
      WRITE(12,40) (STRING(J),J=1,4)
40    FORMAT('+',4A,$)
      GRAPH_MODE=.TRUE.
50    RETURN
      END
C
C
      SUBROUTINE TK_DRAW(IX,IY)
C     =========================
C
      COMMON /TX/ GRAPH_MODE
      LOGICAL GRAPH_MODE
      CHARACTER GS,FS
      CHARACTER*1 STRING(4)
C
      GS = CHAR(29)
      FS = CHAR(28)
      JX=IX
      JY=IY
90    CALL TK_ENCODE(JX,JY,STRING)
      WRITE(12,40) (STRING(J),J=1,4)
40    FORMAT('+',4A,$)
      RETURN
      END
C
C
      SUBROUTINE TK_INIT
C     ==================
C
      COMMON /TX/ GRAPH_MODE
      LOGICAL GRAPH_MODE
      OPEN(UNIT=12,FILE='GRAPHICS',STATUS='UNKNOWN')
      CALL TK_CLEAR
      RETURN
      END
C
C
      SUBROUTINE TK_CLEAR
C     ===================
C
C---- subroutine to clear tektronix screen
C
      CHARACTER ESC,FF
C
      ESC = CHAR(27)
      FF = CHAR(12)
      WRITE(12,10) ESC,FF
10    FORMAT('+',2A,$)
      RETURN
      END
C
C
      SUBROUTINE TK_EXGR
C     ==================
C
C---- subroutine to exit from graphics mode
C
      COMMON /TX/ GRAPH_MODE
      LOGICAL GRAPH_MODE
      CHARACTER US
C
      US = CHAR(31)
      WRITE(12,10) US
10    FORMAT('+',A,$)
      GRAPH_MODE=.FALSE.
      RETURN
      END
C
C
      SUBROUTINE TK_ENCODE(IX,IY,I4)
C     =============================
C
C---- pack coordinates into 4 bytes for tektronic 4010 terminal
C     each coordinate can require up to 10 bits.  these are split into two
C     groups of five - one group in the low order bits of each byte.
C     bits 5 and 6 are used as "tag' bits - see below.  the eighth bit (bit 7)
C     is not used.
C
C---- it is used to set the tag bits.
C     bytes are sent in the order:  high y, low y, high x, low x
C     with respective tag bits +32,+96,+32,+64
C
C     DATA IT/'40206020'X/
      DATA IT/1075863584/
C
      I4 = IT
C
C---- high y + 32  - first byte
C
      CALL MVBITS(IY,5,5,I4,0)  
C
C---- low y  + 96  - second byte
C
      CALL MVBITS(IY,0,5,I4,8)  
C
C---- high x + 32  - third byte
C
      CALL MVBITS(IX,5,5,I4,16) 
C
C---- low x  + 64  - fourth byte
C
      CALL MVBITS(IX,0,5,I4,24) 
      RETURN
      END
C
C
      SUBROUTINE TK_DECODE(I4,IY,IX)
C     =============================
C
C---- the bytes are returned in the order:
C     character, high x, low x, high y, low y - and then optionally cr and eot
C     the 'tag' bits are ignored here.
C
C---- high x  -  first byte
C
      CALL MVBITS(I4,0,5,IX,5)  
C
C---- low x   -  second byte
C
      CALL MVBITS(I4,8,5,IX,0)  
C
C---- high y  -  third byte
C
      CALL MVBITS(I4,16,5,IY,5) 
C
C---- low y   -  fouth byte
C
      CALL MVBITS(I4,24,5,IY,0) 
      RETURN
      END
C
C---- sigma routines
C
C
      SUBROUTINE SIG_CURSOR(IX,IY)
C     ============================
C
C---- puts sigma into graphics mode, graphics screen on, alpha screen off
C     puts cursor up at screen center
C     coordinates of cursor are returned in ix,iy after a 'hit'.
C     after hit turns alpha screen on and cursor off
C     leave graphics screen on
C
      COMMON /SIGMA/ GRAPH_MODE,ICOLOR
      LOGICAL GRAPH_MODE
C
C---- check that point lies within screen boundary
C
      IXX = MIN(IX,767)
      IXX = MAX(IXX,0)
      IYY = MIN(IY,511)
      IYY = MAX(IYY,0)
C
C---- if(.not.graph_mode) write(12,fmt='(''+-*/gaab'',$)')
C     write(12,fmt='(''cacegi'',2i3,''cfhf'')') ixx+100,iyy+100
C
      IF(GRAPH_MODE) THEN
      WRITE(12,FMT='(''CACEGI'',2I3,''CFHF'')') IXX+100,IYY+100
      ELSE
      WRITE(12,FMT='(''+-*/GAABCACEGI'',2I3,''CFHF'')') IXX+100,IYY+100
      ENDIF
      READ(12,200) IX,IY
  200      FORMAT(1X,I3,1X,I3)
      WRITE(12,300)
  300      FORMAT('+-*/AACBDB'/)
      GRAPH_MODE = .FALSE.
      RETURN
      END
C
C
      SUBROUTINE SIG_INIT
C     ===================
C
C---- da - clears pixel store
C     gh - graphic flag sequence recognized at any time
C     db - newline sequence to trigger transition from graphics to alpha state
C     hin - select color (pixel input data) n is hex
C
      COMMON /SIGMA/ GRAPH_MODE,ICOLOR
      LOGICAL GRAPH_MODE
      OPEN(UNIT=12,FILE='GRAPHICS',STATUS='UNKNOWN')
C
C---- reset default color to white
C
      ICOLOR = 15  
      WRITE(12,FMT='(''+-*/DAGHHI'',Z1,''DB''/)') ICOLOR
      GRAPH_MODE = .FALSE.
      RETURN
      END
C
C
      SUBROUTINE SIG_CLEAR
C     ====================
C
C---- clears graphic screen
C     leaves sigma in same state as before
C
C---- da - clear pixel store
C     db - newline sequence to trigger transition from graphics to alpha state
C      COMMON /SIGMA/ GRAPH_MODE,ICOLOR
      LOGICAL GRAPH_MODE
      IF(GRAPH_MODE) THEN
        WRITE(12,FMT='(''DA'')')
      ELSE
        WRITE(12,FMT='(''+-*/DADB''/)')
      ENDIF
      RETURN
      END
C
C
      SUBROUTINE SIG_COL(ICOL)
C     ========================
C
C---- sets color of next vector
C     leaves sigma in same state as before
C     icol must lie in range 0 to 15 (0 is useless).  these are the default
C     table setting. see page 9-25 in the sigma manual.
C
C---- 0  black - useful !!
C     1  green
C     2  blue
C     3  red
C     4  cyan
C     5  magenta
C     6  orange
C     7  yellow
C     8  light green
C     9  light blue
C     10  pink
C     11  dark green
C     12  dark blue
C     13  dark red
C     14  grey
C     15  white
C
C---- hin - select color (pixel input data) n is hex - default is f = 15 = white
C     db - newline sequence to trigger transition from graphics to alpha state
C
      COMMON /SIGMA/ GRAPH_MODE,ICOLOR
      LOGICAL GRAPH_MODE
      IF(ICOL.EQ.ICOLOR) RETURN
      IF(ICOL.LT.0.OR.ICOL.GT.15) RETURN
      IF(GRAPH_MODE) THEN
        WRITE(12,FMT='(''HI''Z1)') ICOL
      ELSE
        WRITE(12,FMT='(''+-*/HI'',Z1,''DB''/)') ICOL
      ENDIF
      ICOLOR = ICOL
      RETURN
      END
C
C
      SUBROUTINE SIG_ALPHA
C     ====================
C
C---- sets alpha state
C     set alpha screen on and graphic screen off
C
C---- aa - alpha screen on
C     gb - graphics screen off
C     db - newline sequence to trigger transition from graphics to alpha state
C
      COMMON /SIGMA/ GRAPH_MODE,ICOLOR
      LOGICAL GRAPH_MODE
      IF(.NOT.GRAPH_MODE) RETURN
      WRITE(12,FMT='(''AAGBDB''/)')
      GRAPH_MODE = .FALSE.
      RETURN
      END
C
C
      SUBROUTINE SIG_MOVE(IX,IY)
C     ==========================
C
C---- moves to point (x,y) in graphics mode
C
      COMMON /SIGMA/ GRAPH_MODE,ICOLOR
      LOGICAL GRAPH_MODE
C
C---- check that point lies within screen boundary
C
      IXX = MIN(IX,767)
      IXX = MAX(IXX,0)
      IYY = MIN(IY,511)
      IYY = MAX(IYY,0)
      IF(GRAPH_MODE) THEN
        WRITE(12,FMT='(''GI'',2I3)') IXX+100,IYY+100
      ELSE
        WRITE(12,FMT='(''+-*/DD000DCGAABGI'',2I3)') IXX+100,IYY+100
        GRAPH_MODE = .TRUE.
      ENDIF
      RETURN
      END
C
C
      SUBROUTINE SIG_DRAW(IX,IY)
C     ==========================
C
C---- draws a line from current position to point ix,iy
C
      COMMON /SIGMA/ GRAPH_MODE,ICOLOR
      LOGICAL GRAPH_MODE
C
C---- check that point lies within screen boundary
C
      IXX = MIN(IX,767)
      IXX = MAX(IXX,0)
      IYY = MIN(IY,511)
      IYY = MAX(IYY,0)
      IF (GRAPH_MODE) THEN
        WRITE(12,FMT='(''GJ'',2I3)') IXX+100,IYY+100
      ELSE
        WRITE(12,FMT='(''+-*/DD000DCGAABGJ'',2I3)') IXX+100,IYY+100
        GRAPH_MODE = .TRUE.
      ENDIF
      RETURN
      END
