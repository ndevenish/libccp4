/*************************************************************************
	binsortint.c
	Z260193

IMPORTANT

This version is compatible with binsort version Z130891 and later.
Agains version Z080389 arguments of SRTBEG have been changed.
Modify and recompile your programs.

WHAT IS binsortint

binsortint is a set of routines used as an interface to
binsort mainly from FORTRAN programs.
Link this module in form of object module with your program.

CALLS:
  SRTBEG		sort initialisation
  SRTRLS		puts one record into sort
  SRTMRG		finishes sequence of input records
  SRTRET		gets one record from sort

For information about key type values see binsortkey.h

				Good Luck	J. Zelinka
***************************************************************************/

#include "binsort.h"

#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#ifndef NOUNISTD		/* ESV, for instance doesn'r have it */
#  include <unistd.h>
#  include <malloc.h>
#endif
#include <stddef.h>

static int	fildesout[2],	/* file descriptors for "r" & "w" access */
		fildesin[2];
static FILE	*filout,        /* file pointers for fildesout, fildesin */
		*filin;
static int	pid;		/* child process id */
static int	recl;		/* record length (fixed) */

/*=======================================================
SRTBEG:	Sort initialisation

	INTEGER		SRTBEG		(function) returns 0 if O.K.
                                        errno ot -1 otherwise
	INTEGER		NKYES		number of keys
	INTEGER 	KEYBUF(at least # of keys * 5)	keys description
	INTEGER		LRECL		(FIXED) length of record (BYTES)
	INTEGER		MEMSIZE		size of memory used by sort (BYTES)
						if .EQ. 0, default size

	I = SRTBEG(NKEYS,KEYBUF,LRECL,MEMSIZE)  I.EQ.0 status O.K.

KEYBUF consist of NKEYS entries, each of the form:
        INTEGER 	KEYTYP		key type (see binsortproc.h)
	INTEGER 	ORDER		sort order ( - || - )
	INTEGER 	KEYPOS		position within record (BYTES, 1st is
						position 0)
	INTEGER 	KEYLNG		length (data units chars, shorts ...)
	INTEGER         MASK            mask applied to data element before
	                                comparison (if 0 no mask applied)
=======================================================*/

#if defined (__hpux) || defined (_AIX)
  int srtbeg (nkeys, keybuf, lrecl, memsize)
#endif

#if defined (ardent) || defined (titan) || defined (stardent)
  int SRTBEG (nkeys, keybuf, lrecl, memsize)
#endif

#if defined (__convex__) || defined (ultrix) || defined (sgi) || defined (ESV)
  int srtbeg_ (nkeys, keybuf, lrecl, memsize)
#endif

#if defined (alliant) || defined (sun) || defined (solbourne)
  int srtbeg_ (nkeys, keybuf, lrecl, memsize)
#endif

#if defined (iris)
  fortran srtbeg_ (nkeys, keybuf, lrecl, memsize)
#endif

int     	*keybuf;	/* keys description */
int	        *lrecl;		/* length of record */
int	        *nkeys;		/* number of keys */
int	        *memsize;       /* size of memory (BYTES) used by sort */
{
  char         **argv, **pargv;
  int            argc;
  static char   *binsortname = "binsort";
  static char   *rclswitch = "-r";
  char           lengthbuf [10];
  static char   *memswitch = "-m";
  char           memsizbuf [10];
  int            numkeys;
  static char   *keyswitch = "-k";
  char           keydatatype;
  char           sortorder;

  pipe(fildesin);
  pipe(fildesout);

  recl = *lrecl;

  if ((pid = fork()) == 0 ) {	/* the child process */

    /* create input and output pipes */

    close(0);		/* close stdin */
    dup(fildesout[0]);
    close(1);
    dup(fildesin[1]);
    close(fildesout[1]);
    close(fildesin[0]);
    if (errno != 0) {
      perror("Binsort streams");
      _exit(255);
    }

    /* prepare binsort command line */

    argc = (*nkeys * 2) + 2 + 1 + 1;
    if (*memsize)
      argc += 2;
    argv = (char **)malloc(argc * sizeof(char *));
    if (argv == NULL) {
      fprintf(stderr,"malloc failed in SRTBEG\n");
      _exit(255);
    }
    argv [argc-1] = (char *)NULL;
    pargv = argv;
    *pargv++ = binsortname;
    *pargv++ = rclswitch;                  /* -r length */
    sprintf(lengthbuf, "%d", recl);
    *pargv++ = lengthbuf;
    if (*memsize) {
      *pargv++ = memswitch;                /* -m size */
      sprintf(memsizbuf, "%d", *memsize);
      *pargv++ = memsizbuf;
    }
    for (numkeys = *nkeys; numkeys; --numkeys, keybuf += 5) {
      *pargv++ = keyswitch;                /* -k keydescription */
      *pargv = (char *)malloc(256);
      if (*pargv == NULL) {
	fprintf(stderr,"malloc failed in SRTBEG\n");
	_exit(255);
      }
      switch (keybuf [0]) {
	case CHAR:     keydatatype = 'c';
	               break;
        case UCHAR:    keydatatype = 'C';
	               break;
        case SHORT:    keydatatype = 's';
	               break;
	case USHORT:   keydatatype = 'S';
	               break;
	case LONG:     keydatatype = 'l';
	               break;
	case ULONG:    keydatatype = 'L';
	               break;
	case FLOAT:    keydatatype = 'f';
	               break;
	case DOUBLE:   keydatatype = 'd';
	               break;
	}
      sortorder = (keybuf [1] == ASC) ? 'a' : 'd';
      switch (keybuf [0]) {
      case UCHAR:
      case USHORT:
      case ULONG:  sprintf(*pargv, "%c:%c:%d:%d:%x", keydatatype, sortorder,
			   keybuf [2], keybuf [3], keybuf [4]);
	           break;
      default:     sprintf(*pargv, "%c:%c:%d:%d", keydatatype, sortorder,
			   keybuf [2], keybuf [3]);
      }
      ++pargv;
    }

    execvp(binsortname, argv);
    perror("Trying to execute binsort");
    _exit(errno);
  }
  else if (pid == -1) {		/* fork failed */
    perror("Trying to fork for binsort");
    return(errno);
  }
  else {                         /* THE PARENT */
    close(fildesout[0]);
    close(fildesin[1]);
    if (!(filout = fdopen(fildesout[1], "w")))
      return(255);
  }
  return(0);
}

/*=======================================================
SRTRLS:	Release one record into Sort
	INTEGER		                             SRTRLS  (function)
	CHARACTER*(at least length of the record)    RECORD   pointer to record

	I = SRTRLS(RECORD)		I.EQ.0 status O.K.
                                        errno otherwise
=======================================================*/

#if defined (__hpux) || defined (_AIX)
  int srtrls (record)
#endif

#if defined (ardent) || defined (titan) || defined (stardent)
  int SRTRLS (record)
#endif

#if defined (__convex__) || defined (ultrix) || defined (sgi) || defined (ESV) 
  int srtrls_ (record)
#endif

#if defined (alliant) || defined (sun) || defined (solbourne)
  int srtrls_ (record)
#endif

#if defined (iris)
  fortran srtrls_ (record)
#endif

char		*record;
{
  register unsigned long ret;

  ret = fwrite(record, sizeof(char), recl, filout);
  return(ret == recl ? 0 : ferror(filout));
}

/*=======================================================
SRTMRG:	Merge - finish release phase
	INTEGER		SRTMRG          (function)

	I = SRTMRG()			I.EQ.0 status O.K.
                                        errno otherwise
=======================================================*/

#if defined (__hpux) || defined (_AIX)
  int srtmrg ()
#endif

#if defined (ardent) || defined (titan) || defined (stardent)
  int SRTMRG ()
#endif

#if defined (__convex__) || defined (ultrix) || defined (sgi) || defined (ESV) 
  int srtmrg_ ()
#endif

#if defined (alliant) || defined (sun) || defined (solbourne)
  int srtmrg_ ()
#endif

#if defined (iris)
  fortran srtmrg_ ()
#endif

{
    fclose(filout);
    if (!(filin = fdopen(fildesin[0], "r")))
      return(255);
    return(0);
}


/*=======================================================
SRTRET:	Return 1 record from sort
	INTEGER		                             SRTRET  (function)
	CHARACTER*(at least length of the record)    RECORD   pointer to record

	I = SRTRET(RECORD)		I.EQ.0 status O.K.
					I.EQ. -1 End of records
					errno otherwise
=======================================================*/

#if defined (__hpux) || defined (_AIX)
  int srtret (record)
#endif

#if defined (ardent) || defined (titan) || defined (stardent)
  int SRTRET (record)
#endif

#if defined (__convex__) || defined (ultrix) || defined (sgi)  || defined (ESV)
  int srtret_ (record)
#endif

#if defined (alliant) || defined (sun) || defined (solbourne)
  int srtret_ (record)
#endif

#if defined (iris)
  fortran srtret_ (record)
#endif

char		*record;
{
    register int	ret;

    if ((ret = fread(record, sizeof(char), recl, filin)) == recl)
      return(0);
    else if (feof(filin)) {
      fclose(filin);
      return(-1);
    }
    else
      return(ferror(filin));
}

