/*************************************************************************
	binsortint.c
	Z131093

     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.

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
#include <stdlib.h>
#ifndef NOUNISTD		/* ESV, for instance doesn't have it */
#  include <unistd.h>
#endif
#include <stddef.h>
#include <errno.h>
#include <sys/wait.h>
#include <sys/types.h>		/* necessary on Titan, at least, and
				   for POSIX wait */

#ifndef EIO
#  define EIO 5
#endif

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

#if defined (__convex__) || defined (ultrix) || defined (sgi) || \
    defined (ESV) || defined(__OSF1__) || defined(__osf__) || defined(F2C)
  int srtbeg_ (nkeys, keybuf, lrecl, memsize)
#endif

#if defined (alliant) || defined (sun) || defined (solbourne)
  int srtbeg_ (nkeys, keybuf, lrecl, memsize)
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

    if ((close(0) != 0) ||		/* close stdin */
	(dup(fildesout[0]) < 0) ||
	(close(1) != 0) ||
	(dup(fildesin[1]) < 0) ||
	(close(fildesout[1]) != 0) ||
	(close(fildesin[0]) != 0)) {
      perror("Binsort streams");
      _exit(1);
    }

    /* prepare binsort command line */

    argc = (*nkeys * 2) + 2 + 1 + 1;
    if (*memsize)
      argc += 2;
    argv = (char **)malloc(argc * sizeof(char *));
    if (argv == NULL) {
      fprintf(stderr,"malloc failed in SRTBEG\n");
      _exit(1);
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
	_exit(1);
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
      return(EIO);
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

#if defined (__convex__) || defined (ultrix) || defined (sgi) || \
    defined (ESV) || defined(__OSF1__) || defined(__osf__) || defined(F2C)
  int srtrls_ (record)
#endif

#if defined (alliant) || defined (sun) || defined (solbourne)
  int srtrls_ (record)
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

#if defined (__convex__) || defined (ultrix) || defined (sgi) || \
    defined (ESV) || defined(__OSF1__) || defined(__osf__) || defined(F2C)
  int srtmrg_ ()
#endif

#if defined (alliant) || defined (sun) || defined (solbourne)
  int srtmrg_ ()
#endif

{
    fclose(filout);
    if (!(filin = fdopen(fildesin[0], "r")))
      return(EIO);
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

#if defined (__convex__) || defined (ultrix) || defined (sgi) || \
    defined (ESV) || defined(__OSF1__) || defined(__osf__) || defined(F2C)
  int srtret_ (record)
#endif

#if defined (alliant) || defined (sun) || defined (solbourne)
  int srtret_ (record)
#endif

char		*record;
{
    register int	ret;
# if defined (ESV) || defined (ultrix) || defined (alliant)
    /* Ultrix guessed as BSD-ish */
    union wait *status;
#else  /* SysVile, POSIX */
    int status;
#endif

    if ((ret = fread(record, sizeof(char), recl, filin)) == recl)
      return(0);
    /* else EOF or read error */
    if ((int) wait (&status) < 0) { /* some error with sub-process */
      fclose(filin);
      return (EIO);
    }
    if (feof(filin) && status == 0
	&& ret == 0) {		/* ensure record not truncated */
      fclose(filin);
      return(-1);		/* normal EOF */
    }
    fclose(filin);
    if (status != 0)
      return (status);		/* sub-process abended */
    ret=ferror(filin);
    if (ret != 0) {
      return (ret);
    } else			/* e.g. premature EOF */
      return (EIO);
}
