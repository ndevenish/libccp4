	SUBROUTINE QTYPE (ISTAMP)
C     =========================
C
C     ..
C     .. Scalar Arguments ..
      INTEGER*2 ISTAMP
C
C     .. Parameters ..
 
C---- Added code to write out map/machine stamp to header.  D.Wild 20/8/91
C---- This will be replaced by C code in diskio.c
C --------------------------------------------------------------------------
C                              MT constants                             
C      four MT nibbles represent uchar, int, float, double                 
C ---------------------------------------------------------------------------

      parameter (MT_SUN      =  '1111'X,
     * 		 MT_ALLIANT  =  '1111'X,
     *		 MT_IRIS4    =  '1111'X,
     *		 MT_MIPSEB   =  '1111'X,
     *		 MT_APOLLO   =  '1111'X,
     *	  	 MT_UNICOS   =  '3331'X,
     *		 MT_CTSS     =  '3331'X,
     *		 MT_VAX      =  '2241'X,
     *		 MT_MIPSEL   =  '4441'X,
     *		 MT_PC       =  '4144'X,
     *		 MT_MAC      =  '1111'X,
     *		 MT_SUN386   =  '1444'X)

C --------------------------------------------------------------------------
C		Machine dependencies                                
C		Comment out as necessary - MIPS unix f77 can use c preprocessor
C               Temporary version - to be replaced with c code
C ---------------------------------------------------------------------------
C #ifdef VAX
       INTEGER    DF_MT
       parameter (DF_MT	      =  MT_VAX)
C #endif

C#ifdef MIPSEL
C#define DF_MT   MT_MIPSEL
C#endif


C#ifdef MIPSEB
C#define DF_MT   MT_MIPSEB
C#endif
 
	ISTAMP = DF_MT
	RETURN
	END
