/****************************************************************************
  binsort.c
  Z130891

HOW TO USE

Just run binsort without any arguments and short manual will be printed
on your screen.

WHAT IS BINSORT

binsort	was designed for sorting BINARY FIXED LENGTH records
under UNIX (and possible others) operating system.
Directives for sorting are present in command line, input is read from stdin,
output written into stdout, message written into stderr. This allows
binsort to be used as a filter.

SOMETHING ABOUT ALGORITHMS.

Program uses UNIX sort procedure qsort. Compare function "cmproutine"
interprets an array of key descriptions by subsequent calling of data
type specific functions. There are also separate functions which perform
also mask processing among them. Cruical resource of each sort algorithm
is a work space. binsort (as it does not know the file size in advance)
allocates workspace of WORKASZ bytes, which can be reset before binsort
compilation or altered through BINSORT_MEM external variable or -m switch.
Work area is rounded to 4 record boundary and used by qsort to produce
a set of sorted "runs", which are by merge distributed to 3 (of total 4)
scratch files. These are created in directory SCRPATH, which can again be
set before binsort compilation and altered by BINSORT_SCR external variable.
No modifying switch is provided. Distribution and subsequent merging uses
polyphase merge algorithm mentioned below. In this phase the work space
is devided into 4 parts and serves as I/O buffers for scratch files.

WARRANTY

As usual, absolutly none. But you can address your complains during
next few months to

zelinka@uk.ac.york,yorvic      (from the U.K.)
zelinka@yorvic.york.ac.uk      (from the rest of the world)

NOTE

Do you use the new binsortint version, which allows usage of binsort
from fortran programs?
*****************************************************************************/

#ifndef __convex__
#define __STDC__
#endif /* __convex__ */

#ifdef  POSIX
#  define __EXTENSIONS__
#endif /* POSIX */

#ifdef VAX_VMS
#    include    stdio
#    include    stdlib
#    include    unixio
#    include    processes
#    include    time
#else      /* UNIX */
#include <stdio.h>
#include <string.h>           /* string operations */

#include <fcntl.h>            /* for i/o */
#include <sys/types.h>        /* for statistics & io */
#include <sys/stat.h>         /* for i/o */
#include <errno.h>
#include <sys/param.h>        /* for statistics */
#include <sys/times.h>        /* for statistics */
#endif     /* VAX_VMS - UNIX */

#include "binsort.h"          /* key data types definition */

#ifdef POSIX
#  include <stdlib.h>           /* for qsort, getenv prototypes */
#  include <unistd.h>
#endif /* POSIX */


#ifdef SYSV
#  include <unistd.h>

    extern char       *getenv(
#ifdef FUNCPROTO
			      char *name
#endif /* FUNCPROTO */
			      );
    extern void        qsort(
#ifdef FUNCPROTO
			     char  *base,
			     size_t nel,
			     size_t size,
			     int (*compar)(void *e1, void *e2)
#endif /* FUNCPROTO */
			     );
#endif /* SYSV */


#if defined(bsd) | defined(sun) | defined(BSD) 
/*** This is somtehing really special for Kim ***/
    extern char       *getenv();
    extern void        qsort();

#include <sys/file.h>

#define SEEK_SET L_SET
#define SEEK_CUR L_INCR
#define SEEK_END L_XTND
#endif /* BSD */

#define  tolower(c)     ((c > 'A' && c < 'Z') ? c + ('a'-'A') : c)

/*** System/machine/country dependent ***/

#define CLOCKFREQ             60             /* system clock frequence */


/*** Site dependent, please modify if necessary***/

#define SCRPATH            "/usr/tmp"           /* default scratch file path */
#define	WORKASZ            4096000                 /* default work area size */

/*** Low level compare function ("prototype")                            ***
 *** Returns 1 if rec1 > rec2    0 if rec1 == rec2     -1 if rec1 < rec2 ***/

typedef int   (*CMPFUN)(
#ifdef FUNCPROTO
			char             *rec1,
			char             *rec2,
			struct key_dsc   *key
#endif /* FUNCPROTO */
			);

/*** Internal key structure ***/

struct key_dsc {                  /* internal key description */
  CMPFUN       cmp_function;      /* -> compare function */
  short        key_type,          /* data type of key */
               asc_desc_order;    /* asc/desc sort order */
  int          key_pos,           /* 1st key byte position in record
				     numbered from 0 */
               key_length;        /* key length in given data type units */
  union {                         /* mask description */
  unsigned char  m_char;          /* mask for char data types */
  unsigned short m_short;         /* mask for short data types */
  unsigned long  m_long;          /* mask for long data types */
  }            mask;              /* mask used BEFORE comparison */
};

typedef struct key_dsc *          KEYDSC;


/*** routines used int this module ***/
static void             advancerunrec(
#ifdef FUNCPROTO
			       struct scratch_tape  *ctape
#endif /* FUNCPROTO */
			       );
static int              cmp_char(
#ifdef FUNCPROTO
			       char            *e1,
                               char            *e2,
			       KEYDSC           key
#endif /* FUNCPROTO */
			       );
static int              cmp_uchar(
#ifdef FUNCPROTO
			       char            *e1,
                               char            *e2,
			       KEYDSC           key
#endif /* FUNCPROTO */
			       );
static int              cmp_uchar_mask(
#ifdef FUNCPROTO
			       char            *e1,
                               char            *e2,
			       KEYDSC           key
#endif /* FUNCPROTO */
			       );
static int              cmp_double(
#ifdef FUNCPROTO
			       char            *e1,
                               char            *e2,
			       KEYDSC           key
#endif /* FUNCPROTO */
			       );
static int              cmp_float(
#ifdef FUNCPROTO
			       char            *e1,
                               char            *e2,
			       KEYDSC           key
#endif /* FUNCPROTO */
			       );
static int              cmp_long(
#ifdef FUNCPROTO
			       char            *e1,
                               char            *e2,
			       KEYDSC           key
#endif /* FUNCPROTO*/
			       );
static int              cmp_ulong(
#ifdef FUNCPROTO
			       char            *e1,
                               char            *e2,
			       KEYDSC           key
#endif /* FUNCPROTO */
			       );
static int              cmp_ulong_mask(
#ifdef FUNCPROTO
			       char            *e1,
                               char            *e2,
			       KEYDSC           key
#endif /* FUNCPROTO */
			       );
static int              cmp_short(
#ifdef FUNCPROTO
			       char            *e1,
                               char            *e2,
			       KEYDSC           key
#endif /* FUNCPROTO */
			       );
static int              cmp_ushort(
#ifdef FUNCPROTO
			       char            *e1,
                               char            *e2,
			       KEYDSC           key
#endif /* FUNCPROTO */
			       );
static int              cmp_ushort_mask(
#ifdef FUNCPROTO
			       char            *e1,
                               char            *e2,
			       KEYDSC           key
#endif /* FUNCPROTO */
			       );
static int              cmproutine(
#ifdef FUNCPROTO
			       char            *e1,
			       char            *e2
#endif /* FUNCPROTO */
			       );
static int              fillworka(
#ifdef FUNCPROTO
			       void
#endif /* FUNCPROTO */
			       );
static void             flushrunbuf(
#ifdef FUNCPROTO
			       void
#endif /* FUNCPROTO */
			       );
static void             gettape(
#ifdef FUNCPROTO
			       int              num
#endif /* FUNCPROTO */
			       );
static void             ioerr(
#ifdef FUNCPROTO
			       void
#endif /* FUNCPROTO */
			       );
static void             linecmd(
#ifdef FUNCPROTO
			      int               argc,
                              char             *argv []
#endif /* FUNCPROTO */
			      );
int                     main(
#ifdef FUNCPROTO
			      int               argc,
                              char             *argv []
#endif /* FUNCPROTO */
			      );
static void             manual(
#ifdef FUNCPROTO
			       void
#endif /* FUNCPROTO */
			       );
static void             memoryerr(
#ifdef FUNCPROTO
			       void
#endif /* FUNCPROTO */
			       );
static void             merge(
#ifdef FUNCPROTO
			       void
#endif /* FUNCPROTO */
			       );
static void             merge1(
#ifdef FUNCPROTO
			       struct scratch_tape   *p1
#endif /* FUNCPROTO */
			       );
static void             merge2(
#ifdef FUNCPROTO
			       struct scratch_tape   *p1,
			       struct scratch_tape   *p2
#endif /* FUNCPROTO */
			       );
static void             merge3(
#ifdef FUNCPROTO
			       struct scratch_tape   *p1,
			       struct scratch_tape   *p2,
			       struct scratch_tape   *p3
#endif /* FUNCPROTO */
			       );
static void             mergeruns(
#ifdef FUNCPROTO
			       void
#endif /* FUNCPROTO */
			       );
static void             printkeys(
#ifdef FUNCPROTO
			       void
#endif /* FUNCPROTO */
			       );
static void             proterr(
#ifdef FUNCPROTO
			       void
#endif /* FUNCPROTO */
			       );
static void             prtstatist(
#ifdef FUNCPROTO
			       long             realtime
#endif /* FUNCPROTO */
			       );
void                    putrunrec(
#ifdef FUNCPROTO
			       char            *rec
#endif /* FUNCPROTO */
			       );
static int              readrun(
#ifdef FUNCPROTO
			       void
#endif /* FUNCPROTO */
			       );
static void             rewindtape(
#ifdef FUNCPROTO
			       int               num
#endif /* FUNCPROTO */
			       );
static void             sort(
#ifdef FUNCPROTO
			       int               nrecords,
			       int               recsize
#endif /* FUNCPROTO */
			       );
static void             testprint(
#ifdef FUNCPROTO
			       void
#endif /* FUNCPROTO */
			       );
static void             writerun(
#ifdef FUNCPROTO
			       int               num,
			       int               nrec
#endif /* FUNCPROTO */
			       );
static void             my_exit(
#ifdef FUNCPROTO
			       int               status
#endif /* FUNCPROTO */
			       );

/*** Memory resources ***/
static char     *scrpath = SCRPATH;

/*** Work area ***/
static char    *workarea;            /* work area */
static int      workasz = WORKASZ,   /* default work area maximal size */
                actworkasz = 0,      /* work area actual rounded size */
                workainrec = 0;      /*         - || -                in
				        records */
static int      scrbufsize;          /* scratch file buffer size (bytes) */
static int      scrbufinrec;         /* scratch file buffer size (records) */

/**** The routine manual gives you some information about the interface ***/
static void
manual()
{
fprintf(stderr,
"\n\n\
Command:\n\
binsort key key_value [key keyvalue [...]] <infile >outfile\n\
\n\
Keys:\n\
    -r n    n - length of one (FIXED LENGTH) record\n\
\n\
    -k typ:order:start[:length[:mask]] - key description.\n\
	    typ    - data type (c character) (C unsigned character)\n\
			       (s short) (S unsigned short)\n\
                               (l long) (L unsigned long)\n\
			       (f float) (d double)\n\
	    order  - (a ascending)\n\
                     (d descending)\n\
	    start  - position of the beginning of key from beginning\n\
		     of the record in bytes. 1st record byte has position 0.\n\
	    length - number of units of specified data type, default\n\
                     value 1\n\
            mask   - hexadecimal value interpreted as mask,\n\
                     which is applied to data unit BEFORE comparison.\n\
                     Mask can be used only with C, S, & L data types.\n\
                     If mask is present, length must be given explicitly.\n\
\n\
     -m n   n - number of bytes used in main work area. Default is written\n\
                below\n\
\n\
     -v     with no value, verbose mode of binsort\n\
\n\
Environment variables:\n\
     BINSORT_SCR    - scratch path\n\
     BINSORT_MEM    - work area in bytes\n\
\n\
Bugs/Features:\n\
	Data types other then short, char were not tested well.\n\
	Each record is alligned on LONG boundary. Some machines may\n\
        require double. Data fields withinin record itself must be\n\
        properly alligned, otherwise BUS ERROR is invoked.\n\
        Binsort does not check semantics of keyvalue data.\n\
\n\
Notes:\n\
	Current work area size %dB,\n\
	Current scratch file path %s.\n\
\n\
Version Z130891                            Good Luck\n\
                                              J. Zelinka\n\
", workasz, scrpath);
}

/*** Internal constants - do not change them ***/

#define INPUT                   0       /* stdin file descriptor */
#define OUTPUT                  1       /* stdout file descriptor */
#define	TRUE                    1
#define	FALSE                   0

/*** Input switches ***/

#define F_LREC                  "-r"
#define F_KEY                   "-k"
#define F_MEM                   "-m"
#define F_VERBOSE               "-v"
#define F_ASC                   'a'
#define F_DESC                  'd'
#define F_CHAR                  'c'
#define F_UCHAR                 'C'
#define F_SHORT                 's'
#define F_USHORT                'S'
#define F_LONG                  'l'
#define F_ULONG                 'L'
#define F_FLOAT                 'f'
#define F_DOUBLE                'd'

/*** internal variables ***/

static int      f_verbose = FALSE;      /* verbose flag */

/*** Record and it's attributes ***/
static int      lrecl = 0,    /* fixed record length */
                actrecl,      /* record length rounded to suitable boundary */
                nkeys = 0,    /* number of sort keys */
                nrecs = 0,    /* number of all records */
                outnrecs = 0; /*  -  ||  -        as a check count */
static struct key_dsc  *keys; /* key definitions */

/******************************* C O D E *********************************/
main(argc, argv)
int      argc;
char    *argv[];
{
  time_t        oldrealtime, newrealtime;
  char         *charvalue;       


  /*** real time init. ***/
  oldrealtime = time(&oldrealtime);

  /*** environment variables processing ***/
  if (charvalue = (char *) getenv ("BINSORT_MEM"))
    workasz = atoi(charvalue);
  if (charvalue = (char *) getenv ("BINSORT_SCR"))
    scrpath = charvalue;

  /*** Input parameters processing ***/
  linecmd(argc, argv);	 /* comands from command line */
  if (f_verbose)
    printkeys();

  /*** Work area initialization ***/
  actrecl = lrecl + ((sizeof(int)-(lrecl%sizeof(int))) % sizeof(int));
                          /* record allignment */
  actworkasz = workasz - (workasz % (actrecl * 4)); /* rounded on 4 records
						       boundary */
  scrbufsize = actworkasz / 4;                      /* scratch file buffer */
  workainrec = actworkasz / actrecl;                /* lengths in records */
  scrbufinrec = scrbufsize / actrecl;
  if (workainrec <= 20 || !(workarea = (char *)malloc(actworkasz)))
    memoryerr();


  /*** Sort processing ***/
  if (f_verbose)
    fprintf(stderr, "binsort -- sorting started\n");
  merge();

  /*** Final statistics ***/
  newrealtime = time(&newrealtime);
  if (f_verbose) {
    fprintf(stderr, "binsort -- End\n");
    prtstatist(newrealtime-oldrealtime);
  }
  my_exit(0);
}


/*=============================================================*/
static void
linecmd(argc, argv)
register int	argc;
char		*argv[];
{
  register int                  i, j;
  register struct key_dsc      *pkeys;
  int                           d1, d2, d3;
  char                          c, c1, c2;


  for (i = 1; i < argc; ++i)	/* parse line, count keys */
    if (!strcmp(argv[i], F_VERBOSE))
      f_verbose = TRUE;
    else if (!strcmp(argv[i], F_LREC)) {
      ++i;
      if (i == argc || sscanf(argv[i], "%d%1c", &lrecl, &c) != 1) {
	manual();
	fprintf(stderr, "binsort -- bad record length\n");
	my_exit(1);
      }
    }
    else if (!strcmp(argv[i], F_MEM)) {
      ++i;
      if (i == argc || sscanf(argv[i], "%d%1c", &workasz, &c) != 1) {
	manual();
	fprintf(stderr, "binsort -- bad memory specification\n");
	my_exit(1);
      }
    }
    else if (!strcmp(argv[i], F_KEY)) {
      ++i;
      if (i < argc
             &&
	  (((j = sscanf(argv[i],"%1c:%1c:%d:%d:%x%1c",
			&c1, &c2, &d1, &d2, &d3, &c)) == 5
	                    &&
               (c1 == F_UCHAR || c1 == F_USHORT || c1 == F_ULONG))
                   ||
	     (j == 4 || j == 3)
                            &&
               ((c1=tolower(c1)) == F_CHAR || c1 == F_SHORT ||
                              c1 == F_LONG || c1 == F_FLOAT ||
	                      c1 == F_DOUBLE))
              &&
           (c2 == F_ASC || c2 == F_DESC))
	++nkeys;
      else {
	manual();
	fprintf(stderr, "binsort -- bad key specification\n");
	my_exit(1);
      }
    }
    else {
      manual();
      fprintf(stderr, "binsort -- bad option\n");
      my_exit(1);
    }

  if (lrecl == 0) {
    manual();
    fprintf(stderr, "binsort -- bad record length\n");
    my_exit(1);
  }

  if (nkeys == 0) {
    manual();
    fprintf(stderr, "binsort -- no keys\n");
    my_exit(1);
  }

  pkeys = keys = (struct key_dsc *)malloc(nkeys * sizeof(struct key_dsc));
  for (i = 1; i < argc; ++i)
    if (!strcmp(argv[i], F_KEY)) {	/* key values */
      d2 = 1;                           /* default length */
      d3 = 0;                           /* default no mask */
      ++i;
      sscanf(argv[i],"%1c:%1c:%d:%d:%x%1c",&c1,&c2,&d1,&d2,&d3,&c);
      pkeys->asc_desc_order = ( c2 == F_ASC) ? ASC : DESC;
      pkeys->key_pos = d1;
      pkeys->key_length = d2;

      switch (c1) {
      case F_CHAR:   pkeys->key_type = CHAR;
		     pkeys->mask.m_char = 0;
	             pkeys->cmp_function = cmp_char;
	             break;
      case F_UCHAR:  pkeys->key_type = UCHAR;
		     pkeys->mask.m_char = d3;
	             if (d3)
		       pkeys->cmp_function = cmp_uchar_mask;
	             else
		       pkeys->cmp_function = cmp_uchar;
	             break;
      case F_SHORT:  pkeys->key_type = SHORT;
		     pkeys->mask.m_short = 0;
	             pkeys->cmp_function = cmp_short;
	             break;
      case F_USHORT: pkeys->key_type = USHORT;
		     pkeys->mask.m_short = d3;
	             if (d3)
		       pkeys->cmp_function = cmp_ushort_mask;
	             else
		       pkeys->cmp_function = cmp_ushort;
	             break;
      case F_LONG:   pkeys->key_type = LONG;
		     pkeys->mask.m_long = 0;
	             pkeys->cmp_function = cmp_long;
	             break;
      case F_ULONG:  pkeys->key_type = ULONG;
		     pkeys->mask.m_long = d3;
	             if (d3)
		       pkeys->cmp_function = cmp_ulong_mask;
	             else
		       pkeys->cmp_function = cmp_ulong;
	             break;
      case F_FLOAT:  pkeys->key_type = FLOAT;
	             pkeys->cmp_function = cmp_float;
	             break;
      case F_DOUBLE: pkeys->key_type = DOUBLE;
	             pkeys->cmp_function = cmp_double;
	             break;
      default:       manual();
	             fprintf(stderr, "binsort -- bad key type\n");
	             my_exit(1);
      }
      ++pkeys;
    }
}



/*=============================================================*/
static int
readrun()             /*** Returns number of read (and sorted) records ***/
{
    static int          firsttimecalled = TRUE;  /* just a flag */

    register char      *prec;
    register int	numrec,         /* # of records read */
                        i,              /* cycle paremater */
                        fillsize;       /* # butes occupied by data */

    fillsize = fillworka();             /* fill (read into) area */
    if (numrec = fillsize/actrecl)	/* there are some records */
	sort(numrec, actrecl);
    if (fillsize < actworkasz && firsttimecalled) { /* merge necessary ? */
	for (i = numrec, prec = workarea; i; --i, prec += actrecl)
	    if (fwrite(prec, sizeof(char), lrecl, stdout) != lrecl)
		ioerr();
	return(0);                      /* indicate end of data */
	}
    firsttimecalled = FALSE;
    return(numrec);
}


/*=====================================================================*/
static int
fillworka()	/* returns atual number of bytes occupated in workarea */
{
    register int	lng;
    register char      *pw,             /* -> work area */
                       *pwe;            /* -> byte after work area */

    pw = workarea;
    pwe = pw + actworkasz;

    while (pw < pwe && (lng = fread(pw, sizeof(char), lrecl, stdin)) == lrecl){
	++nrecs;
	pw += actrecl;
	}
    if (pw < pwe && !feof(stdin)) /* still free space and wrong input */
	proterr();
    return(pw - workarea);
}


/*================================================================*/
static void
sort(nrecords, recsize)
int              nrecords;
int              recsize;
{
  qsort(workarea, nrecords, recsize, cmproutine); /* oh, U N I X */
}


/*=================================================================*/
static int
cmproutine(e1, e2)
char       *e1, *e2;           /* basics elements - records */
{
  register struct key_dsc      *pkey;
  register int                  i, j;

  for (pkey = keys, i = nkeys; i; --i, ++pkey)
    if (j = (*pkey->cmp_function)(e1, e2, pkey))
      return(pkey->asc_desc_order == ASC ? j : -j);
  return(0);
}


static int
cmp_char(e1, e2, key)
char            *e1, *e2;
KEYDSC           key;
{
  register char      *p1, *p2;
  register char	      c1, c2;
  register int        n;

  p1 = e1 + key->key_pos;
  p2 = e2 + key->key_pos;

  for (n = key->key_length; n; --n, ++p1, ++p2)
    if ((c1 = *p1) < (c2 = *p2))
      return(-1);
    else if (c1 > c2)
      return(1);
  return(0);
}

static int
cmp_uchar(e1, e2, key)
char            *e1, *e2;
KEYDSC           key;
{
  register unsigned char      *p1, *p2;
  register unsigned char       c1, c2;
  register int                 n;

  p1 = (unsigned char *)(e1 + key->key_pos);
  p2 = (unsigned char *)(e2 + key->key_pos);

  for (n = key->key_length; n; --n, ++p1, ++p2)
    if ((c1 = *p1) < (c2 = *p2))
      return(-1);
    else if (c1 > c2)
      return(1);
  return(0);
}

static int
cmp_uchar_mask(e1, e2, key)
char            *e1, *e2;
KEYDSC           key;
{
  register unsigned char      *p1, *p2;
  register unsigned char       c1, c2;
  register unsigned char       mask;
  register int                 n;

  p1 = (unsigned char *)(e1 + key->key_pos);
  p2 = (unsigned char *)(e2 + key->key_pos);
  mask = key->mask.m_char;

  for (n = key->key_length; n; --n, ++p1, ++p2)
    if ((c1 = *p1 & mask) < (c2 = *p2 & mask))
      return(-1);
    else if (c1 > c2)
      return(1);
  return(0);
}

static int
cmp_short(e1, e2, key)
char            *e1, *e2;
KEYDSC           key;
{
  register short     *p1, *p2;
  register short      c1, c2;
  register int        n;

  p1 = (short *)(e1 + key->key_pos);
  p2 = (short *)(e2 + key->key_pos);

  for (n = key->key_length; n; --n, ++p1, ++p2)
    if ((c1 = *p1) < (c2 = *p2))
      return(-1);
    else if (c1 > c2)
      return(1);
  return(0);
}


static int
cmp_ushort(e1, e2, key)
char            *e1, *e2;
KEYDSC           key;
{
  register unsigned short     *p1, *p2;
  register unsigned short      c1, c2;
  register int                 n;

  p1 = (unsigned short *)(e1 + key->key_pos);
  p2 = (unsigned short *)(e2 + key->key_pos);

  for (n = key->key_length; n; --n, ++p1, ++p2)
    if ((c1 = *p1) < (c2 = *p2))
      return(-1);
    else if (c1 > c2)
      return(1);
  return(0);
}


static int
cmp_ushort_mask(e1, e2, key)
char            *e1, *e2;
KEYDSC           key;
{
  register unsigned short     *p1, *p2;
  register unsigned short      c1, c2;
  register unsigned short      mask;
  register int                 n;

  p1 = (unsigned short *)(e1 + key->key_pos);
  p2 = (unsigned short *)(e2 + key->key_pos);
  mask = key->mask.m_short;

  for (n = key->key_length; n; --n, ++p1, ++p2)
    if ((c1 = *p1 & mask) < (c2 = *p2 & mask))
      return(-1);
    else if (c1 > c2)
      return(1);
  return(0);
}


static int
cmp_long(e1, e2, key)
char            *e1, *e2;
KEYDSC           key;
{
  register long      *p1, *p2;
  register long       c1, c2;
  register int        n;

  p1 = (long *)(e1 + key->key_pos);
  p2 = (long *)(e2 + key->key_pos);

  for (n = key->key_length; n; --n, ++p1, ++p2)
    if ((c1 = *p1) < (c2 = *p2))
      return(-1);
    else if (c1 > c2)
      return(1);
  return(0);
}


static int
cmp_ulong(e1, e2, key)
char            *e1, *e2;
KEYDSC           key;
{
  register unsigned long      *p1, *p2;
  register unsigned long       c1, c2;
  register int                 n;

  p1 = (unsigned long *)(e1 + key->key_pos);
  p2 = (unsigned long *)(e2 + key->key_pos);

  for (n = key->key_length; n; --n, ++p1, ++p2)
    if ((c1 = *p1) < (c2 = *p2))
      return(-1);
    else if (c1 > c2)
      return(1);
  return(0);
}


static int
cmp_ulong_mask(e1, e2, key)
char            *e1, *e2;
KEYDSC           key;
{
  register unsigned long      *p1, *p2;
  register unsigned long       c1, c2;
  register unsigned long       mask;
  register int                 n;

  p1 = (unsigned long *)(e1 + key->key_pos);
  p2 = (unsigned long *)(e2 + key->key_pos);
  mask = key->mask.m_long;

  for (n = key->key_length; n; --n, ++p1, ++p2)
    if ((c1 = *p1 & mask) < (c2 = *p2 & mask))
      return(-1);
    else if (c1 > c2)
      return(1);
  return(0);
}


static int
cmp_float(e1, e2, key)
char            *e1, *e2;
KEYDSC           key;
{
  register float     *p1, *p2;
  register float      c1, c2;
  register float      n;

  p1 = (float *)(e1 + key->key_pos);
  p2 = (float *)(e2 + key->key_pos);

  for (n = key->key_length; n; --n, ++p1, ++p2)
    if ((c1 = *p1) < (c2 = *p2))
      return(-1);
    else if (c1 > c2)
      return(1);
  return(0);
}


static int
cmp_double(e1, e2, key)
char            *e1, *e2;
KEYDSC           key;
{
  register double    *p1, *p2;
  register double     c1, c2;
  register int        n;

  p1 = (double *)(e1 + key->key_pos);
  p2 = (double *)(e2 + key->key_pos);

  for (n = key->key_length; n; --n, ++p1, ++p2)
    if ((c1 = *p1) < (c2 = *p2))
      return(-1);
    else if (c1 > c2)
      return(1);
  return(0);
}



/*====================================================================*/
static void
proterr()			/* protocol error exit */
{
  fprintf(stderr, "binsort -- bad protocol, not fixed length records or\n");
  fprintf(stderr, "           bad keys\n");
  my_exit(1);
}


/*====================================================================*/
static void
ioerr()
{
  perror("binsort -- I/O error, f. computer or system");
  my_exit(1);
}


/*=====================================================================*/
static void
memoryerr()
{
  fprintf(stderr, "binsort -- Out of internal or external memory\n");
  fprintf(stderr, "           Use parameters to increase them\n");
  my_exit(1);
}

/*=====================================================================*/
static void
my_exit(status)
int     status;
{
fclose(stdin);
fclose(stdout);
fclose(stderr);
_exit(status);
}


/*=====================================================================*/
static void
printkeys()
{
  int			 i, length, mask;
  struct key_dsc	*p;
  char		        *ttype, *tasc;

  fputc('\n', stderr);
  fprintf(stderr, "binsort -- Max. internal memory  %d[B]\n", workasz);
  fprintf(stderr, "binsort -- Input record length   %d\n", lrecl);
  fprintf(stderr, "binsort -- key definitions:\n");
  for (i=0; i < nkeys;++i) {
    p = keys+i;
    mask = 0;
    switch (p->key_type) {
    case CHAR:	ttype = "char";
                break;
    case UCHAR:	ttype = "uchar";
                mask = p->mask.m_char;
                break;
    case SHORT:	ttype = "short";
                break;
    case USHORT:ttype = "ushort";
                mask = p->mask.m_short;
                break;
    case LONG:	ttype = "long";
                break;
    case ULONG:	ttype= "ulong";
                mask = p->mask.m_long;
                break;
    case FLOAT:	ttype = "float";
                break;
    case DOUBLE: ttype = "double";
                 break;
    }
    tasc = (p->asc_desc_order == ASC) ? "ascending" : "descending";
    fprintf(stderr,"           keytype %s order %s keypos %d keylng %d",
	    ttype, tasc, p->key_pos, p->key_length);
    if (mask)
      fprintf(stderr, " mask %x", mask);
    fputc('\n', stderr);
  }
  fputc('\n', stderr);
}


/*=============================================================*/
static void
prtstatist(realtime)
long		realtime;
{
  struct tms	spenttime;

  fprintf(stderr,"\n\nbinsort statistics:\n");
  fprintf(stderr,"\tSorted records:\t\t\t%d\n", nrecs);
  fprintf(stderr,"\tInternal memory [bytes]:\t%d\n", actworkasz);
  fprintf(stderr,"\tReal time [s]:\t\t\t%d\n", (int)realtime + 1);
  times(&spenttime);
  fprintf(stderr,"\tUser time [s]:\t\t\t%.2f\n",
	  (float)spenttime.tms_utime/(float)CLOCKFREQ);
  fprintf(stderr,"\tSystem time [s]:\t\t%.2f\n",
	  (float)spenttime.tms_stime/(float)CLOCKFREQ);
}











/**************************************************************************
  merge

Polyphase merge sorting with "horizontal distribution"
implemented for T = 4, P = T-1 = 3
as published in:
         Donald E. Knuth: The Art of Computer Programming Vol.3
                          (Sorting and Searching) pp. 270-271
                          Addison-Wesley Publishing Company, Inc. (1973)

***************************************************************************/


#define T                     4
#define P                     3


/*** 1st element of arrays is not used as indices start from 1 ***/

static int     j,         /* logical tape unit 1 <= j <= T */
               l,
               A [T+1],   /* The perfect Fibonacci distribution we are
			     striving for */
               D [T+1];   /* Number of dummy runs assumed to be present at
			     the beginning of logical tape unit number j */
static struct scratch_tape {
  int        fd;          /* tape file descriptor */
  int        truns;       /* actual runs written into tape */
  int        runlng;      /* current run length */
  char      *buf;         /* i/o buffer */
  char      *pbuf;        /* pointer to buffer */
}            TAPE [T+1];  /* Number of the physical tape unit corresponding
			     to logical tape unit number j */


static void
merge()
{
  register int        i, nrec, a, sizerec;
  char               *pb;
  struct scratch_tape tmpTAPE;
  register int        tmpD;


  /*** My initialize ***/
  if ((nrec = readrun()) == 0)/* No records for merge */
    return;                   /* no merge phase necessary */

 /*D1:*** initialise ***/
  if (f_verbose)
    fprintf(stderr, "binsort -- disk merging started\n");
  for (j = 1; j < T; ++j) {
    A [j] = D [j] = 1;
    gettape(j);    /* open scratch file for TAPE [j] */
  }
  A [T] = D [T] = 0;
  gettape(T);
  l = j = 1;

 D2:/*** Input to tape j ***/
  writerun(j, nrec);
  ++((TAPE [j]).truns);
  --(D [j]);
  if ((nrec = readrun()) == 0) {      /* no aditional records */
    /*** Rewind tapes ***/
    for (i = 1, pb = workarea; i <= T; ++i, pb += scrbufsize) {
      (TAPE [i]).buf = pb;            /* allocate i/o buffer */
	rewindtape(i);                /* rewind tape */
    }
    goto D5;
  }

/*D3:*** Advance j ***/
  if (D [j] < D [j+1]) {
    ++j;
    goto D2;
  }
  else if (D [j] == 0)
    goto D4;
  else {
    j = 1;
    goto D2;
  }

 D4:/*** Up a level ***/
  ++l;
  a = A [1];
  for (j = 1; j <= P; ++j) {
    D [j] = a + A [j+1] - A [j];
    A [j] = a + A [j+1];
    }
  j = 1;
  goto D2;

 D5:/*** Merge ***/
  /*** this is an original algorithm version. It was modified, what results
       in saveing of rewriting TAPE [1] to output file
  if (l == 0) {
    *** write output from TAPE [1] ***
    return;                 *** end of merging ***
  }
  else
  ***************************************************************************/
  if (f_verbose)
    fprintf(stderr, "binsort -- merge level %d started\n", l);
  while (D [P] != 0 || (TAPE [P]).truns != 0) {
    for (j = 1; j <= P; ++j)
      if (!(D [j]))              /* all D [j] > 0 ? */
	break;
    if (j > P) {                 /* yes, all are greater */
      ++(D [T]);
      for (j = 1; j <= P; ++j)
	--(D [j]);
    }
    else                         /* no, at least one is not greater */
      mergeruns();               /* merge runs from tapes where D [j] == 0 */
  }

 /*D6:*** Down a level ***/
  --l;
  /*** This is algorithm modification mentioned above ***/
  if (l == 0)
    return;               /* end of merging */
  /******************************************************/
  /*** some small correction, rewind was moved several lines below ***
       rewindtape(P);
       rewindtape(T);
   ***                                                             ***/
  memcpy(&tmpTAPE, TAPE + T, sizeof(tmpTAPE));
  tmpD = D [T];
  for (i = T; i > 1; --i) {
    memcpy(TAPE + i, TAPE + i - 1, sizeof(tmpTAPE));
    D [i] = D [i-1];
  }
  memcpy(TAPE + 1, &tmpTAPE, sizeof(tmpTAPE));
  D [1] = tmpD;
  rewindtape(1);
  rewindtape(T);
  goto D5;
}


static void
mergeruns()
{
  register int         i, j, size;
  int                  newrunlng;
  int                  nmergeruns;       /* # of actual merged runs */
  struct scratch_tape *tapes [P+1],      /* tapes taken into account */
                      *ctape;            /* current tape */

  nmergeruns = newrunlng = 0;
  for (j = 1; j <= P; ++j) {
    if (D [j])                 /* this is a dummy run, which is discarded */
      --(D [j]);
    else {                            /* this is a real run and is merged */
      ++nmergeruns;
      ctape = tapes [nmergeruns] = TAPE + j;
      --(ctape->truns);            /* we shell process onre run from here */
      if ((read(ctape->fd, &(ctape->runlng), sizeof(int))) != sizeof(int))
	ioerr();
      ctape->pbuf = ctape->buf + scrbufsize;  /* mark buffer as exhausted */
      advancerunrec(ctape);
      newrunlng += ctape->runlng;
    }
  }

  /*** This is algorithm modification mentioned above ***/
  if (l > 1) {
    if (write((TAPE [T]).fd, &newrunlng, sizeof(int)) != sizeof(int))
      ioerr();
  }

  switch (nmergeruns) {
  case 1: merge1(tapes [1]);
          break;
  case 2: merge2(tapes [1], tapes [2]);
          break;
  case 3: merge3(tapes [1], tapes [2], tapes [3]);
          break;
  }
  flushrunbuf();                                 /* flush new run */
  ++((TAPE [T]).truns);                        /* update # of runs */
}


static void
merge1(p1)
struct scratch_tape *p1;
{
  /*** Just copies records from tape p1 into outfp ***/

  while (TRUE) {
    putrunrec(p1->pbuf);
    /*** Look if current input is exhausted ***/
    if (--(p1->runlng))
      /*** NO, advance tape ***/
      advancerunrec(p1);
    else
      /*** YES, merge is finished ***/
      return;
    }
}


static void
merge2(p1, p2)
struct scratch_tape  *p1, *p2;
{
  /*** Merges TWO runs (batches) from tapes p1, p2 into outfp      ***
   *** The algorithm simply swaps between inputs, so that current  ***
   *** is lower than other and then current is written into output ***
   *** If one of the runs is exhausted, merge1 is called           ***/

  register struct scratch_tape *current, *other, *tmp;


  current = p1;
  other = p2;

  do {
    if (cmproutine(current->pbuf, other->pbuf) <= 0)
      /*** current is lower, no swap ***/;
    else {
      /*** current is greater, swap current and other ***/
      tmp = current;
      current = other;
      other = tmp;
    }

    /*** Write current, which is now lower than other ***/
    putrunrec(current->pbuf);
    /*** Look if current input is exhausted ***/
    if (--(current->runlng))
      /*** NO, advance tape ***/
      advancerunrec(current);
    else {
      /*** YES, "merge just rest of the other run ***/
      merge1(other);
      return;
    }
  } while (TRUE);
}


static void
merge3(p1, p2, p3)
struct scratch_tape *p1, *p2, *p3;
{
  /*** Merges THREE runs (batches) from tapes p1, p2, p3 into output ***
   *** Algorithm maintains current record, greater of the other two  ***
   *** and lower of other two. Necessary comparisons involve swaps,  ***
   *** so than at the end the current is LOWER than lower and that   ***
   *** is lower than greater. If input from one of the three tapes   ***
   *** is exhausted, merge2 routine is called                        ***/


  register struct scratch_tape *current, *greater, *lower, *tmp;

  current = p1;
  if (cmproutine(p2->pbuf, p3->pbuf) <= 0) {
    lower = p2;
    greater = p3;
  }
  else {
    lower = p3;
    greater = p2;
  }
  
  do {
    if (cmproutine(current->pbuf, lower->pbuf) <= 0)
      ;
    else {
      tmp = lower;
      if (cmproutine(current->pbuf, greater->pbuf) <= 0) {
	lower = current;
	current = tmp;
      }
      else {
	lower = greater;
	greater = current;
	current = tmp;
      }
    }
    putrunrec(current->pbuf);
    if (--(current->runlng))
      advancerunrec(current);
    else {
      merge2(lower, greater);
      return;
    }
  } while (TRUE);
}


static void
gettape(num)
int     num;
{
  char                   scrname [500];
  int                    fdes;
  struct scratch_tape   *ctape;
  struct stat statstr;

  ctape = TAPE + num;
  sprintf(scrname, "%s/binsort%07d%d", scrpath, getpid(), num); /* gen name */
  if ((fdes = open(scrname, O_RDWR | O_CREAT, S_IREAD|S_IWRITE)) < 0)
                            /* create scratch file */
    ioerr();
  (void)fstat(fdes, &statstr);                               /* file status */
  if (!(statstr.st_mode & S_IFREG)) {
    fprintf(stderr, "binsort -- scratch is not a regular file\n");
    my_exit(1);
  }
  unlink(scrname);                         /* delete after close !!! */
  ctape->fd = fdes;                        /* file descriptor */
  ctape->truns = ctape->runlng = 0;        /* no runs on tape */
  ctape->buf = ctape->pbuf = (char *)NULL;
}

static void
rewindtape(num)
int          num;
{
  struct scratch_tape   *ctape;
  unsigned               size;

  ctape = TAPE + num;
  if (num == T)
    ctape->pbuf = ctape->buf;               /* tape for write is empty */
  else
    ctape->pbuf = ctape->buf + scrbufsize;  /* tape for read in empty */
  (void)lseek(ctape->fd, 0L, SEEK_SET);
}


static void
writerun(num, nrec)
int          num;
int          nrec;
{
  struct scratch_tape   *ctape;
  unsigned               bytelength;

  ctape = TAPE + num;
  if (write(ctape->fd, &nrec, sizeof(nrec)) == -1)   /* run length */
    ioerr();
  bytelength = actrecl * nrec;
  if (write(ctape->fd, workarea, bytelength) == -1)  /* run */
    ioerr();
}

static void
advancerunrec(ctape)
struct scratch_tape      *ctape;
{
  register int            bread;

  ctape->pbuf += actrecl;
  if (ctape->pbuf >= ctape->buf + scrbufsize) {
    bread = (scrbufinrec < ctape->runlng) ? scrbufinrec : ctape->runlng;
    bread *= actrecl;
    if (read(ctape->fd, ctape->buf, bread) != bread)
      ioerr();
    ctape->pbuf = ctape->buf;
  }
}


void
putrunrec(rec)
char        *rec;
{
  register struct scratch_tape      *ctape;

  ctape = TAPE + T;

  /*** This is algorithm modification mentioned above ***/
  if (l == 1) {
    if (fwrite(rec, sizeof(char), lrecl, stdout) != lrecl)
      ioerr();
  }
  else {
    if (ctape->pbuf == (ctape->buf + scrbufsize))
      flushrunbuf();
    memcpy(ctape->pbuf, rec, actrecl);
    ctape->pbuf += actrecl;
    }
}


static void
flushrunbuf()
{
  register struct scratch_tape      *ctape;
  int                                size;

  ctape = TAPE + T;
  if (size = ctape->pbuf - ctape->buf) {
    if (write(ctape->fd, ctape->buf, size) != size)
      ioerr();
    ctape->pbuf = ctape->buf;
  }
}





/**********************
static void
testprint()
{
  register int      j;

  for (j = 1; j <= T; ++j) {
    fprintf(stderr,
      "j %3d l %3d A %3d D %3d TAPE %3d truns %3d runlng %3d\n",
	    j, l, A[j], D[j], (TAPE+j)->fd, (TAPE+j)->truns, (TAPE+j)->runlng);
  }
}
************************/
