/*****************************************************************************
* 
*			  machine.h                                           
*     Machine constants and global variables for diskio.c    
*                    D.Wild EMBL, 24th September 1991
* 
* Portions developed at the National Center for Supercomputing Applications at
* the University of Illinois at Urbana-Champaign.
* 
*****************************************************************************/
#define DFE_BADCONV     -37 /* Don't know how to convert data type */
/*--------------------------------------------------------------------------*/
/*                   definitions of file modes                              */
#define BYTE  0
#define INT16 1   
#define INT32 6
#define FLOAT32 2
#define COMP32  3
#define COMP64  4
/*--------------------------------------------------------------------------*/
/*                              MT constants                                */
/*      four MT nibbles represent uchar, int, float, double                 */
/*      for MTZLIB need to define as INTEGER*4                              */
/*--------------------------------------------------------------------------*/
#define	DFMT_SUN        0x11111111
#define	DFMT_SGI        0x11111111
#define DFMT_APOLLO	0x11111111
#define	DFMT_MIPSEB     0x11111111
#define	DFMT_UNICOS     0x33313331
#define	DFMT_CTSS       0x33313331
#define	DFMT_VAX        0x22412241
#define DFMT_MIPSEL     0x44414441
#define	DFMT_ALLIANT    0x44414441
#define	DFMT_PC         0x41444144	/* note byte swapping ??? */
				       /* check this... */
#define	DFMT_MAC        0x11111111
#define DFMT_SUN386	0x14441444
/* type info codes */
#define	DFNT_UINT       0
#define	DFNT_SINT       1
#define	DFNT_INT        2
#define	DFNT_UCHAR      3
#define	DFNT_CHAR       4
#define	DFNT_FLOAT      5
#define	DFNT_DOUBLE     6

/* class info codes for int */
#define	DFNTI_MBO       1	/* Motorola byte order 2's compl */
#define	DFNTI_VBO       2	/* Vax byte order 2's compl */
#define	DFNTI_IBO       4	/* Intel byte order 2's compl */

/* class info codes for float */
#define	DFNTF_IEEE      1	/* IEEE format */
#define	DFNTF_VAX       2	/* Vax format */
#define	DFNTF_CRAY      3	/* Cray format */
#define	DFNTF_PC        4	/* PC floats - flipped IEEE */

/* class info codes for char */
#define	DFNTC_BYTE      0	/* bitwise/numeric field */
#define	DFNTC_ASCII     1	/* ASCII */
#define	DFNTC_EBCDIC    5	/* EBCDIC */

/*--------------------------------------------------------------------------*/
/*                      Machine dependencies                                */
/*--------------------------------------------------------------------------*/
#ifdef VAX
#define DF_MT   DFMT_VAX
#endif

#ifdef MIPSEL
#define DF_MT   DFMT_MIPSEL
#endif


#ifdef MIPSEB
#define DF_MT   DFMT_MIPSEB
#endif
/*---------------------------------------------------------------------------*/
/*                      Global variables                                     */
/*---------------------------------------------------------------------------*/
typedef short int16;
typedef long int32;
typedef float float32;
FILE  *infile;                                                                              
int16 fileMT;              /* machine type stamp in file                */
int16 fileIT;              /* integer type in file                      */
int16 userIT;              /* user machine integer type                 */
int16 fileFT;              /* float type in file                        */
int16 userFT;              /* user machine float type                   */
int
    Iconvert[MAXFILES],   /* true if machine  IT = IT to be read       */
    Fconvert[MAXFILES];   /* true if machine  FT = FT to be read       */
int ConvError;              /* used by DFconvert                       */
int Mode;                   /* returned by cqmode                      */
unsigned char mtstring[3];  /*       machine stamp                     */
unsigned char fileType[MAXFILES][5];  /*       MAP/MTZ stamp           */
long  map_stamp_posn = 208; /* MAP stamp offset in header              */
long  mt_stamp_posn =  8;   /* machine stamp offset in MTZ header      */
int DFerror;                /* Error code for DF routines              */
/*---------------------------------------------------------------------------*/
/*                      External functions                                   */
/*---------------------------------------------------------------------------*/
extern int DFCVvaxF2ieeeF();
extern int DFCVieeeF2vaxF();
