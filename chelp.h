/****************************************************************************
 *                                                                          *
 * This file contains the lowest level routines for the CCP4 Program Suite. *
 * It contains code that is specific to both the VMS and Unix versions of   *
 * the software eg the set of C routines required by the disk input/output  *
 * routines in diskio.for                                                   *
 *                                                                          *
 * Routine  Arguments                      Purpose                          *
 *                                                                          *
 * ustenv   (string, result)             - set the environment variable (U) *
 * copen    (iunit,filnam,istat)         - fopen (open random access file)  *
 * qclose   (iunit)                      - fclose (shut random access file) *
 * qmode    (iunit,mode,nmcitm)          - change size of item in file ops  *
 * qread    (iunit,array,nitems,ier)     - fread from random access file    *
 * qwrite   (iunit,array,nitems)         - fwrite to random access file     *
 * qseek    (iunit,irec,iel,lrecl)       - fseek within random access file  *
 * qback    (iunit,lrecl)                - fseek within random access file  *  
 * qskip    (iunit,lrecl)                - fseek within random access file  *
 * cqinq    (iunit,lfilnm,filnam,length) - inquire file on the given stream *
 * qlocate  (iunit,locate)               - ftell within random access file  *
 * qtype   (istamp)                     - returns machine type stamp       *
 *                                                                          *
 * Comments, complaints, moans and updates to: ccp4@uk.ac.daresbury.cxa     *
 *                                                                          *
 * CCP4, 28th February 1992                                                 *
 *  $Date$
 *                                                                          *
 ****************************************************************************/

/****************************************************************************
 * Known Machines                                                           *
 ****************************************************************************/

/* Each type of system we know about should cause KNOWN_MACHINE to be
   defined and also define CALL_LIKE_<something> to be defined.  Thus
   if you know system foo has a Fortran calling convention like the
   native Sun compiler uses, define CALL_LIKE_SUN and you won't need
   to examine the definitions of the interface functions below.
   Further tests on the system type may be necessary e.g., to get the
   include files right. 

   Note it's assumed below that a Fortran INTEGER corresponds to a C
   int and a Fortran REAL corresponds to a C float.
*/

#if F2C				/* netlib f2c compiler -- not
				   currently functional */
#  define KNOWN_MACHINE
#  define CALL_LIKE_SUN 1	/* only for a single character
				   variable in the arg list! */
#else
#if defined (_AIX)                        /* IBM unix - models RS/6000 */
#  define KNOWN_MACHINE
#  define CALL_LIKE_HPUX 1
#endif
#if defined (alliant)                          /* alliant model FX28xx */
#  define KNOWN_MACHINE
#  define CALL_LIKE_SUN 1
#endif
#if defined (ardent) || defined (titan)  /* (st)ardent and titan range */
#  ifndef stardent
#    define stardent
#  endif
#endif
#if defined (__convex__)                            /* Convex C series */
#  define KNOWN_MACHINE
#  define CALL_LIKE_SUN 1
#endif
#if defined (ESV)                /* Evans & Sutherland ESV workstation */
#  define KNOWN_MACHINE
#  define CALL_LIKE_SUN 1
#endif
#if defined (__hpux)                         /* Hewlett Packard models */
#  define KNOWN_MACHINE
#  define CALL_LIKE_HPUX 1
#endif
#if defined (iris)                      /* Silicon graphics with 68xxx */
#  define KNOWN_MACHINE
#  define CALL_LIKE_IRIS 1
#endif
#ifdef __sgi			/* ansi */
#  ifndef sgi
#    define sgi
#  endif
#endif
#if defined (sgi)                     /* Silicon graphics with R3000's */
#  define KNOWN_MACHINE
#  define CALL_LIKE_SUN 1
#endif
#if defined (solbourne)                                   /* sun clone */
#  ifndef sun
#   define sun               /* don't know whether it's defined or not */
#  endif
#endif
#if defined (stardent)                                    /* as ardent */
#  define KNOWN_MACHINE
#  define CALL_LIKE_STARDENT 1
#endif
#if defined (sun)                                  /* sun workstations */
#  define KNOWN_MACHINE
#  define CALL_LIKE_SUN 1
#  define strerror(i) sys_errlist[i] /* k&r compiler doesn't have it */
#endif
#if defined (ultrix) || defined(__OSF1__) || defined(__osf__)
#  define KNOWN_MACHINE
#  define CALL_LIKE_SUN 1
#endif
#if defined (VMS)                      /* DEC VAX-VMS operating system */
#  define KNOWN_MACHINE
#endif
#endif

#if ! defined (KNOWN_MACHINE)
  #error System type is not known -- see the Installation Guide
#endif				/*  ! defined (KNOWN_MACHINE) */

/****************************************************************************
 * Include Files                                                            *
 ****************************************************************************/
#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif

#include <stdio.h>

#if defined (VMS)
#  include <descrip.h>		/* non-POSIX */
#else
#  include <sys/types.h>
#  include <sys/times.h>
#endif

#if defined(NOUNISTD)		/* ESV, old Concentrix */
#  include <sys/file.h>		/* non-POSIX */
#else
#  include <stdlib.h> 
#endif

#ifdef stardent			/* who knows if this works anyhow... */
#  include <sys/types.h>
#  include <malloc.h>		/* non-POSIX */
#else
#  include <stddef.h>
#endif

#include <string.h>		/* BSD might need strings.h and index
				   instead of strchr.  This is ANSI
				   header, not POSIX.1 */
#if !defined(NOUNISTD) && !defined(VMS)
#  include <unistd.h>
#endif

#include <errno.h>

#ifdef _AIX			/* would do no harm on others, though */
#  include <time.h>
#endif

#include <ctype.h>
#include <curses.h> 

/****************************************************************************
 * Defaults and Customisable items                                          *
 ****************************************************************************/

#define MAXFLEN       500       /* the maximum length of a filename in CCP4 */
#define MAXFILES       10    /* maximum number of files open symultaneously */
#define FILE_FLUSH     20    /* force output buffer to flush every n writes */
#define DEFSIZE         2    /* default mode access for random access files */

#ifndef SEEK_SET                   /* Alliants don't have the file stdlib.h */
#define SEEK_SET 0		   /* (now added in Concentrix3) */
#define SEEK_CUR 1
#define SEEK_END 2
#endif /* ! SEEK_SET */

#define IRRELEVANT_OP   0      /* used to show if an fseek is needed or not */
#define READ_OP         1
#define WRITE_OP        2

/****************************************************************************
 * Error Numbers                                                            *
 ****************************************************************************/

#define NO_ERR    0                                              /* success */
#define NO_OPEN   1                                   /* error opening file */
#define NO_CLOSE  2                                   /* error closing file */



struct treenode *tree = NULL;   /* pointer to root node of tree structure */
struct treenode *current = NULL;    /* used to point to current tree node */
struct treenode *previous = NULL;  /* used to point to previous tree node */

int clevel = 0;                    /* used to indicate current tree depth */
int plevel = 0;                   /* used to indicate previous tree depth */

int SCREEN_WIDTH ;
int SCREEN_DEPTH ;







/****************************************************************************
 * Machine dependent and byte swapping parts                                *
 ****************************************************************************/



#ifdef CONVERT
#  include "machine.h"
#  include "dfconvert.h"
#  include "diconvert.h"
#endif

#if defined (VMS) || defined (alliant)
#  define DELFILE remove
#else
#  define DELFILE unlink
#endif

#if defined (ardent) || defined (titan) || defined (stardent)
  struct Str_Desc {
    char *Str_pointer;
    int  Str_length;
    int id;
  };
#endif

#if defined (iris)		/* DL: i find this hard to believe... */
#  define [ %
#  define ] %
#endif

/****************************************************************************
 * Global Initialised Variables                                             *
 ****************************************************************************/

static char rcsid[] = "$Header$";
static int flushf      =  0;              /* counter to flush output buffer */
static int initialised =  0;    /* flag to initialise data and file streams */
static char *file_attribute[] = {"w+", "w+", "r+", "w+", "r"};/* file modes */

/* Note machine dependencies in here.  The assumption is that we have
   a 32-bit machine and that int == INTEGER (*4), short == INTEGER*2, float
   == REAL */

static int item_sizes[] = {                     /* table of bytes per item */
  (int) sizeof (char),                                          /* 0: bytes */
#if defined (sgi)
  (int) sizeof (short),		/* silicon graphics bodge (This is fixed in
				   IRIX4, at least) */
#else
  (int) sizeof (short int),                                /* 1: half words */
#endif
  (int) sizeof (float),                                   /* 2: reals/words */
  (int) sizeof (int),           /* 3: `short complex' (pairs of half words).
				   NB int rather than 2*short since must fit
				   into fortran integer */
  (int) 2*sizeof (float),                    /* 4: complex (pairs of words) */
#if 0				/* fixme: diskio.for says mode 5 is integer --
				   mode 6 isn't mentioned there and not used
				   (?) */
  (int) sizeof (char),                                             /* bytes */
  (int) sizeof (int)};                              /* pairs of small words */
#endif
  (int) sizeof (int),
  (int) sizeof (int)};

/****************************************************************************
 * Global Uninitialised Variables                                           *
 ****************************************************************************/

static FILE *file_stream[MAXFILES];                 /* Pointer to disk file */
static char file_name[MAXFILES][MAXFLEN];      /* Pointer to disk file name */
static int  file_bytes_per_item[MAXFILES];/* Pointer to disk file item size */
static int  file_is_scratch[MAXFILES];    /* Indicates if file is 'SCRATCH' */
static int  file_last_op [MAXFILES];    /* see man fopen rd/wr combinations */

#define MAX_LINE_LEN 512
#define DEF_OPTION     0
#define lcase(c) ((c >= 'A' && c <= 'Z') ? c - 'A' + 'a' : c)


/**************************************************************************
 * Structures                                                             *
 **************************************************************************/

struct block {                       /* definition of our data block .... */
  char *name;                       /* is header name to identify it then */
  struct block *lines;    /* a linked list of lines making up a paragraph */
};

struct list {                         /* definition of a linked list .... */
  struct treenode *child;                  /* is an item in the list then */
  struct list *next;                    /* a pointer to next item in list */
};

struct treenode {                       /* definition of a tree node .... */
  struct block *item;                    /* is a data item (a block) then */
  struct treenode *parent;          /* a pointer to the parent node and a */
  struct list *descendent;                     /* linked list of children */
};











