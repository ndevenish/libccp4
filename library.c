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
 * cclose   (iunit)                      - fclose (shut random access file) *
 * cmode    (iunit,mode,nmcitm)          - change size of item in file ops  *
 * cread    (iunit,array,nitems,ier)     - fread from random access file    *
 * cwrite   (iunit,array,nitems)         - fwrite to random access file     *
 * cseek    (iunit,irec,iel,lrecl)       - fseek within random access file  *
 * cback    (iunit,lrecl)                - fseek within random access file  *  
 * cskip    (iunit,lrecl)                - fseek within random access file  *
 * cqinq    (iunit,lfilnm,filnam,length) - inquire file on the given stream *
 * clocate  (iunit,locate)               - ftell within random access file  *
 * cprint   (iflag,msg)                  - output debug messages            *
 * cmtype   (istamp)                     - returns machine type stamp       *
 *                                                                          *
 * Comments, complaints, moans and updates to: ccp4@uk.ac.daresbury.cxa     *
 *                                                                          *
 * CCP4, 28th February 1992                                                 *
 *  mod. DL 17/6/92                                                         *
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
   include files right. */

#if defined (_AIX)                        /* IBM unix - models RS/6000 */
				/* (surmised from GNU configures -- needs
				   confirmation) */
#  define KNOWN_MACHINE
#  define CALL_LIKE_AIX 1
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
#endif
#if defined (ultrix)                                       /* DEC unix */
#  define KNOWN_MACHINE
#  define CALL_LIKE_SUN 1
#endif
#if defined (vax)                      /* DEC VAX-VMS operating system */
#  define KNOWN_MACHINE
#endif
#if ! defined (KNOWN_MACHINE)
  #error System type is not known -- see the Installation Guide
#else

/****************************************************************************
 * Include Files                                                            *
 ****************************************************************************/

#include <stdio.h>

#ifndef vax
#  include <sys/types.h>
#  include <sys/times.h>
#  include <sys/param.h>
#endif

#if defined (alliant)
#  include <sys/file.h>
#else
#  ifndef ESV
#  include <stdlib.h> 
#  else
#    include <sys/fcntl.h>
     char *getenv();
#  endif
#endif

#ifdef stardent
#  include <sys/types.h>
#  include <malloc.h>
#else
#  include <stddef.h>
#endif

#if defined (vax)
#  include <descrip.h>
#  include <string.h>
#else
#  ifndef ESV  
#    include <strings.h>
#  else  
#    include <string.h> 
#  endif
#  ifndef alliant
#    ifndef ESV
#      include <unistd.h>
#    endif
#  endif
#endif

/****************************************************************************
 * Defaults and Customisable items                                          *
 ****************************************************************************/

#define MAXFLEN       500       /* the maximum length of a filename in CCP4 */
#define MAXFILES       10    /* maximum number of files open symultaneously */
#define FILE_FLUSH     20    /* force output buffer to flush every n writes */
#define DEFSIZE         2    /* default mode access for random access files */

#ifndef SEEK_SET                   /* Alliants don't have the file stdlib.h */
#define SEEK_SET 0
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

/****************************************************************************
 * Machine dependent and byte swapping parts                                *
 ****************************************************************************/

#ifdef CONVERT
#  include "machine.h"
#  include "dfconvert.h"
#  include "diconvert.h"
#endif

#if defined (vax)
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

#if defined (iris)
#  define [ %
#  define ] %
#endif

/****************************************************************************
 * Global Initialised Variables                                             *
 ****************************************************************************/

static int flushf      =  0;              /* counter to flush output buffer */
static int print_flag  = -1;            /* flag to output debug information */
static int initialised =  0;    /* flag to initialise data and file streams */
static char *file_attribute[] = {"w+", "w+", "r+", "w+", "r"};/* file modes */

static int item_sizes[] = {                      /* table of bytes per item */
  (int) sizeof (char),                                             /* bytes */
#if defined (sgi)
  (int) sizeof (short),                           /* silicon graphics bodge */
#else
  (int) sizeof (short int),                                  /* small words */
#endif
  (int) sizeof (int),                               /* pairs of small words */
  (int) sizeof (float),                                            /* words */
  (int) sizeof (double),                                  /* pairs of words */
  (int) sizeof (char),                                             /* bytes */
  (int) sizeof (int)};                              /* pairs of small words */

/****************************************************************************
 * Global Uninitialised Variables                                           *
 ****************************************************************************/

static FILE *file_stream[MAXFILES];                 /* Pointer to disk file */
static char file_name[MAXFILES][MAXFLEN];      /* Pointer to disk file name */
static int  file_bytes_per_item[MAXFILES];/* Pointer to disk file item size */
static int  file_is_scratch[MAXFILES];    /* Indicates if file is 'SCRATCH' */
static int  file_last_op [MAXFILES];    /* see man fopen rd/wr combinations */

/****************************************************************************
 * Routine: flength                                                         *
 ****************************************************************************/

static int flength (s, len)          /* get real length of a Fortran string */
char *s;
int len;
{
  while (s[--len] == ' ');
  return (++len);
} /* End of flength */

/****************************************************************************
 * Routine: ustenv (Unix only - vms version in vms.for)                     *
 ****************************************************************************/

#if ! defined (vax)

#if CALL_LIKE_AIX
  void ustenv (str, Lstr, result)
  char *str;
  int  Lstr;
#endif

#if CALL_LIKE_HPUX
  void ustenv (str, result, Lstr)
  char *str;
  int  Lstr;
#endif

#if CALL_LIKE_STARDENT
  void USTENV (str, result)
  struct Str_Desc *str;
#endif

#if CALL_LIKE_SUN
  void ustenv_ (str, result, Lstr)
  char *str;
  int  Lstr;
#endif

#if CALL_LIKE_IRIS
  fortran ustenv_ (Lstr, str, result)
  char *str;
  int  Lstr;
#endif

int *result;
{
  int Length;
  char name[MAXFLEN], value[MAXFLEN], *temp;

#if CALL_LIKE_STARDENT
  Length = flength (str->Str_pointer, str->Str_length);
  if (Length > MAXFLEN) Length = MAXFLEN - 1;
  (void) strncpy (name, str->Str_pointer, Length);
#else
  Length = flength (str, Lstr);
  if (Length > MAXFLEN) Length = MAXFLEN - 1;
  (void) strncpy (name, str, Length);
#endif
  name[Length] = '\0'; 

#if defined (sgi) || defined (sun) || defined (__hpux)
  temp = (char *) malloc (MAXFLEN);
  (void) strcpy (temp, name);
  *result = putenv (temp);
  /* note the necessary lack of free() */
#else
  temp = (char *) index (name, '=');
  if (temp != (char *) NULL) {
    *temp = '\0';
    temp++;
    (void) strcpy (value, temp);
  };
  *result = setenv (name, value, 1);
#endif
} /* End of ustenv */

#endif  /* end of #if ! defined (vax) */

/****************************************************************************
 * Routine: cunlink                                                         *
 ****************************************************************************/

#if CALL_LIKE_AIX || CALL_LIKE_HPUX
  void cunlink (filename, Lfilename)
  char *filename;
  int  Lfilename;
#endif

#if CALL_LIKE_STARDENT
  void CUNLINK (filename)
  struct Str_Desc *filename;
#endif

#if defined (vax)
  void CUNLINK (filename)
  struct dsc$descriptor_s *filename;
#endif

#if CALL_LIKE_SUN
  void cunlink_ (filename, Lfilename)
  char *filename;
  int  Lfilename;
#endif

#if CALL_LIKE_IRIS
  fortran cunlink_ (Lfilename, filename)
  char *filename;
  int  Lfilename;
#endif
{
  int Length;
  char tempfile[MAXFLEN];

#if CALL_LIKE_STARDENT
    Length = flength (filename->Str_pointer, filename->Str_length);
    if (Length > MAXFLEN) Length = MAXFLEN - 1;
    (void) strncpy (tempfile, filename->Str_pointer, Length);
    tempfile[Length] = '\0'; 
    (void) DELFILE (tempfile);
#else

#  if ! defined (vax)
    Length = flength (filename, Lfilename);
    if (Length > MAXFLEN) Length = MAXFLEN - 1;
    (void) strncpy (tempfile, filename, Length);
    tempfile[Length] = '\0'; 
    (void) DELFILE (tempfile);
#  else
    Length = flength (filename->dsc$a_pointer, filename->dsc$w_length);
    if (Length > MAXFLEN) Length = MAXFLEN - 1;
    (void) strncpy (tempfile, filename->dsc$a_pointer, Length);
    tempfile[Length] = '\0'; 
    (void) DELFILE (tempfile);
#  endif

#endif
} /* End of cunlink */
 
/****************************************************************************
 * Routine: getelapsed                                                      *
 ****************************************************************************/

#if ! defined (vax)

long elapsed;                                   /* used to get elapsed time */

#ifndef HZ			/* this needs attention */
#  ifdef CLK_TCK
#    define HZ CLK_TCK
#  else
#    define HZ 60
#  endif
#endif

#if CALL_LIKE_AIX || CALL_LIKE_HPUX
  void getelapsed ()
#endif

#if CALL_LIKE_STARDENT
  void GETELAPSED ()
#endif

#if CALL_LIKE_SUN
  void getelapsed_ ()
#endif

#if CALL_LIKE_IRIS
  fortran getelapsed_ ()
#endif
{
#if defined (__hpux)
  int minutes;
  float seconds;

  elapsed = (clock ()/CLOCKS_PER_SEC) - elapsed;
  minutes = (int) (elapsed / 60);
  seconds = (int) elapsed - 60.0 * (float) minutes; 
  (void) printf ("Elapsed: %02d:%04.2fs\n", minutes, seconds);
#else
  int minutes;
  float usertime, systime, seconds;
  struct tms  buffer;

  if (times (&buffer) != -1)
    {
      usertime = (float) (buffer.tms_utime + buffer.tms_cutime);
      systime  = (float) (buffer.tms_stime + buffer.tms_cstime);
      usertime /= HZ;
      systime  /= HZ;
      /* fixme: should use fortran i/o */
      printf ("User: %-.2fs System: %-.2fs ", usertime, systime);
    }

  elapsed = (long) time (0) - elapsed;
  minutes = (int) (elapsed / 60);
  seconds = (int) elapsed - 60.0 * (float) minutes; 
  printf ("Elapsed: %02d:%04.2fs\n", minutes, seconds);
#endif				/* __hpux */
}
#endif  /* end of #if ! defined (vax) */

#if ! defined (vax)

/****************************************************************************
 * Routine: initfyp (Unix only - vms version in vms.for)                    *
 ****************************************************************************/

#if CALL_LIKE_AIX || CALL_LIKE_HPUX
  void initfyp ()
#endif

#if CALL_LIKE_STARDENT
  void INITFYP ()
#endif

#if CALL_LIKE_SUN
  void initfyp_ ()
#endif

#if CALL_LIKE_IRIS
  fortran initfyp_ ()
#endif
{
#if defined (__hpux)
  elapsed = clock () / CLOCKS_PER_SEC ;
#else
  elapsed = (long) time (0) ;
#endif				/* __hpux */
}
#endif  /* end of #if ! defined (vax) */

/****************************************************************************
 * Routine: sysmsg                                                          *
 ****************************************************************************/

static void sysmsg (s1, s2, s3, num)
char *s1, *s2, *s3;
int num;
{
  /* fixme: should use fortran i/o */
  if (*s1 == '\0')
    (void) fprintf (stdout, "%s %s %s\n", s1, s2, s3);
  else
    (void) fprintf (stdout, "%s: %s %s\n", s1, s2, s3);
  if (num != NO_ERR) exit (num);
} /* End of sysmsg */

/****************************************************************************
 * Routine: copen                                                           *
 ****************************************************************************/

#if CALL_LIKE_AIX
  void copen (iunit, filename, Lfilename, istat)
  char *filename;
  int  Lfilename;
#endif

#if CALL_LIKE_HPUX
  void copen (iunit, filename, istat, Lfilename)
  char *filename;
  int  Lfilename;
#endif

#if CALL_LIKE_STARDENT
  void COPEN (iunit, filename, istat)
  struct Str_Desc *filename;
#endif

#if defined (vax)
  void COPEN (iunit, filename, istat)
  struct dsc$descriptor_s *filename;
#endif

#if CALL_LIKE_SUN
  void copen_ (iunit, filename, istat, Lfilename)
  char *filename;
  int  Lfilename;
#endif

#if CALL_LIKE_IRIS
  fortran copen_ (iunit, Lfilename, filename, istat)
  char *filename;
  int  Lfilename;
#endif

int  *iunit, *istat;
{
  int Length, i, jstat;

  jstat = *istat;

  if (! initialised) {
    for (i = 1; i < MAXFILES; i++) {
      file_stream[i]         = (FILE *) NULL;
      file_name[i][0]        = '\0';
      file_bytes_per_item[i] = item_sizes[DEFSIZE];  /* default item size */
      file_is_scratch[i]     = 0;                                /* FALSE */
      file_last_op[i]        = IRRELEVANT_OP;
    }
    initialised = 1;
  }

  for (i = 1; i < MAXFILES; i++)           /* Find next available stream */
    if (file_stream[i] == (FILE *) NULL) break;

  if (i == MAXFILES) 
    i = -1;                                 /* return no more units flag */
  else {

#if CALL_LIKE_STARDENT
    Length = flength (filename->Str_pointer, filename->Str_length);
    if (Length > MAXFLEN) Length = MAXFLEN - 1;
    (void) strncpy (file_name[i], filename->Str_pointer, Length);
#else

#if defined (vax)
    Length = flength (filename->dsc$a_pointer, filename->dsc$w_length);
    if (Length > MAXFLEN) Length = MAXFLEN - 1;
    (void) strncpy (file_name[i], filename->dsc$a_pointer, Length);
#else
    Length = flength (filename, Lfilename);
    if (Length > MAXFLEN) Length = MAXFLEN - 1;
    (void) strncpy (file_name[i], filename, Length);
#endif

#endif

    file_name[i][Length] = '\0';
    file_last_op[i] = IRRELEVANT_OP;
    file_bytes_per_item[i] = item_sizes[DEFSIZE];      /* default item size */
    file_is_scratch[i] = (jstat == 2 ? 1 : 0);
    file_stream[i] = fopen (file_name[i], file_attribute[jstat - 1]);
    if (file_stream[i] == (FILE *) NULL)        /* return open failure flag */
      i = -2;
    if (i > 0 && file_is_scratch[i]) {
      if (DELFILE (file_name[i]))
        sysmsg ("(Q)QOPEN", "error unlinking scratch file", file_name[i],NO_ERR);
    }

  }
  *iunit = i;                                   /* return the stream number */

#ifdef CONVERT
/***************************************************************************
 *    Code needed to implement integer/float conversion in heterogeneous   *
 *    machine enviroment --  D.Wild EMBL October 1991                      *
 *    if open for read, check file type(MTZ/MAP) and read machine stamp    *
 *    if scratch file no conversion                                        *
 ***************************************************************************/
  
  Iconvert[*iunit] = 0;
  Fconvert[*iunit] = 0;

  if ((*file_attribute[*istat - 1] != 'w') && (file_is_scratch[*iunit] == 0))
    {
      fread (fileType[*iunit],sizeof(char),4,file_stream[*iunit]);
      if (strncmp (fileType[*iunit],"MTZ",3) == 0)
        {
          fseek (file_stream[*iunit], mt_stamp_posn, SEEK_SET);
          fread (mtstring, sizeof(char), 2, file_stream[*iunit]);
          userIT = (DF_MT>>4) & 0x0f; /* get 2nd nibble from right for int */
          fileIT = (mtstring[0]>>4) & 0x0f;   
          Iconvert[*iunit] = (fileIT != userIT); /* int conversion needed? */
          userFT = (DF_MT>>8) & 0x0f;/* get 3rd nibble from right for float */
          fileFT = (mtstring[1]>>4) & 0x0f;   
          Fconvert[*iunit] = (fileFT != userFT); /* float conversion needed? */
          fseek (file_stream[*iunit], 0, SEEK_SET);
        }
      else
        {
          fseek (file_stream[*iunit], map_stamp_posn, SEEK_SET);
          fread (fileType[*iunit],sizeof(char), 4, file_stream[*iunit]);
          if (strncmp(fileType[*iunit],"MAP",3) == 0)
            {
              fread (mtstring, sizeof(char), 2, file_stream[*iunit]);
              userIT = (DF_MT>>4) & 0x0f; /* get 2nd nibble from right for int */
              fileIT = (mtstring[0]>>4) & 0x0f;   
              Iconvert[*iunit] = (fileIT != userIT); /* int conversion needed? */
              userFT = (DF_MT>>8) & 0x0f;/* get 3rd nibble from right for float */
              fileFT = (mtstring[1]>>4) & 0x0f;   
              Fconvert[*iunit] = (fileFT != userFT); /* float conversion needed? */
              fseek (file_stream[*iunit], 0, SEEK_SET);
            }
          else
            {
              Iconvert[*iunit] = 0;
              Fconvert[*iunit] = 0;
              fseek (file_stream[*iunit], 0, SEEK_SET);
            }
        }
    }
#endif /* CONVERT */

} /* End of copen */
 
/****************************************************************************
 * Routine: cclose                                                          *
 ****************************************************************************/

#if CALL_LIKE_AIX || CALL_LIKE_HPUX
  void cclose (iunit)
#endif

#if defined (vax) || defined (ardent) || defined (titan) || defined (stardent)
  void CCLOSE (iunit)
#endif

#if CALL_LIKE_SUN
  void cclose_ (iunit)
#endif

#if CALL_LIKE_IRIS
  fortran cclose_ (iunit)
#endif

int *iunit;
{
  if (! initialised) 
    sysmsg ("QCLOSE","qopen/qqopen not yet called", "", NO_OPEN);

  if (file_stream[*iunit] != (FILE *) NULL) {
    if (fclose (file_stream[*iunit]) == EOF) 
      sysmsg ("QCLOSE", "error closing", file_name[*iunit], NO_CLOSE);
    file_stream[*iunit] = (FILE *) NULL;
  }

  file_name[*iunit][0] = '\0';
} /* End of cclose */

/****************************************************************************
 * Routine: cmode                                                           *
 ****************************************************************************/

#if CALL_LIKE_AIX || CALL_LIKE_HPUX
  void cmode (iunit, mode, size)
#endif

#if defined (vax) || defined (ardent) || defined (titan) || defined (stardent)
  void CMODE (iunit, mode, size)
#endif

#if CALL_LIKE_SUN
  void cmode_ (iunit, mode, size)
#endif

#if CALL_LIKE_IRIS
  fortran cmode_ (iunit, mode, size)
#endif

int *iunit, *mode, *size;
{
  if (! initialised) 
    sysmsg ("QMODE", "qopen/qqopen not yet called", "", NO_OPEN);

  if (*mode >= 0 && *mode <= 6)
    file_bytes_per_item[*iunit] = item_sizes[*mode];
  else
    file_bytes_per_item[*iunit] = item_sizes[DEFSIZE];
  *size = file_bytes_per_item[*iunit];       /* return number of bytes/item */

#ifdef CONVERT
  Mode = *mode;                               /* Global variable    */
  if (Mode == 10) Mode = 0;                   /* change to modes of */
  if ((Mode == 11) || (Mode == 12)) Mode = 2; /* data types         */
#endif

} /* End of cmode */

/****************************************************************************
 * Routine: cread                                                           *
 ****************************************************************************/

#if CALL_LIKE_AIX || CALL_LIKE_AIX
  void cread (iunit, buffer, nitems, result)
#endif

#if defined (vax) || defined (ardent) || defined (titan) || defined (stardent)
  void CREAD (iunit, buffer, nitems, result)
#endif

#if CALL_LIKE_SUN
  void cread_ (iunit, buffer, nitems, result)
#endif

#if CALL_LIKE_IRIS
  fortran cread_ (iunit, buffer, nitems, result)
#endif

float *buffer;
int *iunit, *nitems, *result;
{
  int i, nbytes;
  register char *buf;

  if (! initialised) 
    sysmsg ("QREAD", "qopen/qqopen not yet called", "", NO_OPEN);

  if (file_last_op[*iunit] == WRITE_OP) 
    (void) fseek (file_stream[*iunit], 0L, SEEK_CUR);
  file_last_op[*iunit] = READ_OP;

#ifndef CONVERT
#if defined (__hpux)
  /* HP's don't like the fread below, but will take the read here - why? */
  nbytes = *nitems * file_bytes_per_item[*iunit];
  i = (int) read (fileno(file_stream[*iunit]), buffer, (size_t) nbytes);
  i /= file_bytes_per_item[*iunit];
#else
  i = fread (buffer, (size_t) file_bytes_per_item[*iunit], (size_t) *nitems, 
             file_stream[*iunit]);
#endif
#else
/*****************************************************************************
 *       Code needed to implement integer/float conversion in heterogeneous  *  
 *       machine enviroment - uses macros from NCSA                          *
 *                   D.Wild EMBL October 1991                                *
 *****************************************************************************/

  nbytes = *nitems * file_bytes_per_item[*iunit];
  buf = (char *) calloc(nbytes, sizeof(char));
  i = fread (buf, (size_t) file_bytes_per_item[*iunit], 
                (size_t) *nitems, file_stream[*iunit]);

  switch (Mode)  {
    case BYTE:  memcpy((char *) buffer, buf, nbytes);
                break;
    case INT16: if (!Iconvert[*iunit])
                  memcpy ((char *) buffer, buf, nbytes);
                else
                  DI2convert(buf, (char *) buffer, DFNT_SINT, fileIT, 
                               userIT, *nitems, ConvError);
                break;
    case INT32: if (! Iconvert[*iunit])
                  memcpy ((char *) buffer, buf, nbytes);
                else
                  DI4convert (buf, (char *) buffer, DFNT_INT, fileIT, 
                               userIT, *nitems, ConvError);
                break;
    case FLOAT32: if (! Fconvert[*iunit])
                    memcpy ((char *) buffer, buf, nbytes);
                  else
                    DFconvert (buf, (char *) buffer, DFNT_FLOAT, fileFT, userFT,
                              *nitems, ConvError);
                  break;
   }

  (void) free (buf);
#endif /* CONVERT */

  *result = (i == *nitems ? 0 : i);
  if (i == 0) *result = -1;                 /* return appropriate flag */
} /* End of cread */

/****************************************************************************
 * Routine: cwrite                                                          *
 ****************************************************************************/

#if CALL_LIKE_AIX || CALL_LIKE_HPUX
  void cwrite (iunit, buffer, nitems, result)
#endif

#if defined (vax) || defined (ardent) || defined (titan) || defined (stardent)
  void CWRITE (iunit, buffer, nitems, result)
#endif

#if CALL_LIKE_SUN
  void cwrite_ (iunit, buffer, nitems, result)
#endif

#if CALL_LIKE_IRIS
  fortran cwrite_ (iunit, buffer, nitems, result)
#endif

float *buffer;
int *iunit, *nitems, *result;
{
  int i;

  if (! initialised) 
    sysmsg ("QWRITE", "qopen/qqopen not yet called", "", NO_OPEN);

  if (file_last_op[*iunit] == READ_OP)
    (void) fseek (file_stream[*iunit], 0L, SEEK_CUR);
  file_last_op[*iunit] = WRITE_OP;

  i = (int) fwrite (buffer, (size_t) file_bytes_per_item[*iunit], (size_t) *nitems, file_stream[*iunit]);
  if ((flushf = ++flushf % FILE_FLUSH) == 0)     /* periodically flush file */
    (void) fflush (file_stream[*iunit]);
  *result = (i == *nitems ? 0 : i);              /* return appropriate flag */
} /* End of cwrite */

/****************************************************************************
 * Routine: cseek                                                           *
 ****************************************************************************/

#if CALL_LIKE_AIX || CALL_LIKE_HPUX
  void cseek (iunit, irecl, iel, lrecl)
#endif

#if defined (vax) || defined (ardent) || defined (titan) || defined (stardent)
  void CSEEK (iunit, irecl, iel, lrecl)
#endif

#if CALL_LIKE_SUN
  void cseek_ (iunit, irecl, iel, lrecl)
#endif

#if CALL_LIKE_IRIS
  fortran cseek_ (iunit, irecl, iel, lrecl)
#endif

int *iunit, *irecl, *iel, *lrecl;
{
  long int position;

  if (! initialised) 
    sysmsg ("QSEEK", "qopen/qqopen not yet called", "", NO_OPEN);

  position = (long) ((*lrecl)*(*irecl - 1) + (*iel - 1));
  position *= (long) file_bytes_per_item[*iunit];
  file_last_op[*iunit] = IRRELEVANT_OP;
  (void) fseek (file_stream[*iunit],position,SEEK_SET);
} /* End of cseek */

/****************************************************************************
 * Routine: cback                                                           *
 ****************************************************************************/

#if CALL_LIKE_AIX || CALL_LIKE_HPUX
  void cback (iunit, lrecl)
#endif

#if defined (vax) || defined (ardent) || defined (titan) || defined (stardent)
  void CBACK (iunit, lrecl)
#endif

#if CALL_LIKE_SUN
  void cback_ (iunit, lrecl)
#endif

#if CALL_LIKE_IRIS
  fortran cback_ (iunit, lrecl)
#endif

int *iunit, *lrecl;
{
  long int position;

  if (! initialised) 
    sysmsg ("QBACK", "qopen/qqopen not yet called", "", NO_OPEN);

  position = ftell (file_stream[*iunit]) - (*lrecl)*file_bytes_per_item[*iunit];
  file_last_op[*iunit] = IRRELEVANT_OP;
  (void) fseek (file_stream[*iunit], position, SEEK_SET);
} /* End of cback */

/****************************************************************************
 * Routine: cskip                                                           *
 ****************************************************************************/

#if CALL_LIKE_AIX || CALL_LIKE_HPUX
  void cskip (iunit, lrecl)
#endif

#if defined (vax) || defined (ardent) || defined (titan) || defined (stardent)
  void CSKIP (iunit, lrecl)
#endif

#if CALL_LIKE_SUN
  void cskip_ (iunit, lrecl)
#endif

#if CALL_LIKE_IRIS
  fortran cskip_ (iunit, lrecl)
#endif

int *iunit, *lrecl;
{
  long int position;

  if (! initialised) 
    sysmsg ("QSKIP", "qopen/qqopen not yet called", "", NO_OPEN);

  position = ftell (file_stream[*iunit]) + (*lrecl)*file_bytes_per_item[*iunit];
  file_last_op[*iunit] = IRRELEVANT_OP;
  (void) fseek (file_stream[*iunit],position,SEEK_SET);
} /* End of cskip */

/****************************************************************************
 * Routine: cqinq                                                           *
 ****************************************************************************/

#if CALL_LIKE_AIX
  void cqinq (iunit, filnam, len_filnam, length)
  char *filnam; 
  int len_filnam;
#endif

#if CALL_LIKE_HPUX
  void cqinq (iunit, filnam, length, len_filnam)
  char *filnam; 
  int len_filnam;
#endif

#if CALL_LIKE_STARDENT
  void CQINQ (iunit, filnam, length)
  struct Str_Desc *filnam;
#endif

#if defined (vax)
  void CQINQ (iunit, filnam, length)
  struct dsc$descriptor_s *filnam;
#endif

#if CALL_LIKE_SUN
  void cqinq_ (iunit, filnam, length, len_filnam)
  char *filnam;
  int len_filnam;
#endif

#if CALL_LIKE_IRIS
  fortran cqinq_ (iunit, len_filnam, filnam, length)
  char *filnam;
  int len_filnam;
#endif

int *iunit, *length;
{
  char real_name[MAXFLEN];
  int stream, Length, i;
  long position;

  if (! initialised) 
    sysmsg ("QQINQ", "qopen/qqopen not yet called", "", NO_OPEN);

  *length = -1;                                    /* default return value */
  stream = *iunit;

  if (file_stream[stream] == (FILE *) NULL) {/* no unit open try file name */
#if CALL_LIKE_STARDENT
    Length = flength (filnam->Str_pointer, filnam->Str_length);
    if (Length > MAXFLEN) Length = MAXFLEN - 1;
    (void) strncpy (real_name, filnam->Str_pointer, Length);
#else

#if defined (vax)
    Length = flength (filnam->dsc$a_pointer, filnam->dsc$w_length);
    if (Length > MAXFLEN) Length = MAXFLEN - 1;
    (void) strncpy (real_name, filnam->dsc$a_pointer, Length);
#else
    Length = flength (filnam, len_filnam);
    if (Length > MAXFLEN) Length = MAXFLEN - 1;
    (void) strncpy (real_name, filnam, Length);
#endif

#endif
    real_name[Length] = '\0';
    for (i = 0; i < MAXFILES; i++)
      if (! strcmp (real_name, file_name[i])) break;
    stream = i % MAXFILES;
  }

  if (file_stream[stream] != (FILE *) NULL) {
    file_last_op[stream] = IRRELEVANT_OP;
    (void) fflush (file_stream[stream]);        /* flush the output stream */
    position = ftell (file_stream[stream]);   /* remember current position */
    (void) fseek (file_stream[stream],0L,SEEK_END);            /* seek EOF */
    *length = (int) ftell (file_stream[stream]);          /* get file size */
    (void) fseek (file_stream[stream],position,SEEK_SET); /* seek position */
  }
} /* End of cqinq */

/****************************************************************************
 * Routine: clocate                                                         *
 ****************************************************************************/

#if CALL_LIKE_AIX || CALL_LIKE_HPUX
  void clocate (iunit, locate)
#endif

#if defined (vax) || defined (ardent) || defined (titan) || defined (stardent)
  void CLOCATE (iunit, locate)
#endif

#if CALL_LIKE_SUN
  void clocate_ (iunit, locate)
#endif

#if CALL_LIKE_IRIS
  fortran clocate_ (iunit, locate)
#endif

int *iunit, *locate;
{
  if (! initialised) 
    sysmsg ("QLOCATE", "qopen/qqopen not yet called", "", NO_OPEN);

  *locate = -1;
  if (file_stream[*iunit] != (FILE *) NULL)
    *locate = (int) ftell (file_stream[*iunit]) / file_bytes_per_item[*iunit];
} /* End of clocate */

/****************************************************************************
 * Routine: cprint                                                          *
 ****************************************************************************/

#if CALL_LIKE_AIX || CALL_LIKE_HPUX
  void cprint (istat, line, Lline)
  char *line;
  int  Lline;
#endif

#if CALL_LIKE_STARDENT
  void CPRINT (istat, line)
  struct Str_Desc *line;
#endif

#if defined (vax)
  void CPRINT (istat, line)
  struct dsc$descriptor_s *line;
#endif

#if CALL_LIKE_SUN
  void cprint_ (istat, line, Lline)
  char *line;
  int  Lline;
#endif

#if CALL_LIKE_IRIS
  fortran cprint_ (istat, Lline, line)
  char *line;
  int  Lline;
#endif

int *istat;
{
  int Length;
  char temp[MAXFLEN];

#if CALL_LIKE_STARDENT
  Length = flength (line->Str_pointer, line->Str_length);
  if (Length > MAXFLEN) Length = MAXFLEN - 1;
  (void) strncpy (temp, line->Str_pointer, Length);
#else

#if defined (vax)
  Length = flength (line->dsc$a_pointer, line->dsc$w_length);
  if (Length > MAXFLEN) Length = MAXFLEN - 1;
  (void) strncpy (temp, line->dsc$a_pointer, Length);
#else
  Length = flength (line, Lline);
  if (Length > MAXFLEN) Length = MAXFLEN - 1;
  (void) strncpy (temp, line, Length);
#endif

#endif

  temp[Length] = '\0';
  if (print_flag == -1) print_flag = *istat;  /* on first call set up level */
  if (*istat <= print_flag) sysmsg ("","",temp,NO_ERR);
} /* End of cprint */

/****************************************************************************
 * Routine: cmtype                                                          *
 ****************************************************************************/

#ifndef CONVERT
#define DF_MT 0
#endif

#if CALL_LIKE_AIX || CALL_LIKE_HPUX
  void cmtype (istamp)
  int *istamp;
#endif

#if defined (vax) || defined (ardent) || defined (titan) || defined (stardent)
  void CMTYPE (istamp)
  int *istamp;
#endif

#if CALL_LIKE_SUN
  void cmtype_ (istamp)
  int *istamp;
#endif

#if CALL_LIKE_IRIS
  fortran cmtype_ (istamp)
  int *istamp;
#endif
{
  *istamp = DF_MT;
} /* End of cmtype */

#ifdef CONVERT

/*****************************************************************************
* 
*			  NCSA HDF version 3.10r3
*				Dec 6, 1990
*
* NCSA HDF Version 3.10r3 source code and documentation are in the public
* domain.  Specifically, we give to the public domain all rights for future
* licensing of the source code, all resale rights, and all publishing rights.
* 
* We ask, but do not require, that the following message be included in all
* derived works:
* 
* Portions developed at the National Center for Supercomputing Applications at
* the University of Illinois at Urbana-Champaign.
* 
* THE UNIVERSITY OF ILLINOIS GIVES NO WARRANTY, EXPRESSED OR IMPLIED, FOR THE
* SOFTWARE AND/OR DOCUMENTATION PROVIDED, INCLUDING, WITHOUT LIMITATION,
* WARRANTY OF MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE
* 
*****************************************************************************/
#include <ctype.h>
/*typedef long int32;
typedef float float32;
*/
union float_uint_uchar {
    float32 f;
    int32 i;
    unsigned char c[4];
};

int DFCVvaxF2ieeeF(in, out, size)
union float_uint_uchar in[], out[];
int size;
{
    register unsigned char exp;
    int i;

    for (i=0; i<size; i++)
      {
        exp = (in[i].c[1] << 1) | (in[i].c[0] >> 7);  /* extract exponent */
        if (!exp && !in[i].c[1]) out[i].i = 0;        /* zero value */
        else if (exp>2) {                               /* normal value */
            out[i].c[0] = in[i].c[1] - 1; /* subtracts 2 from exponent */
                /* copy mantissa, LSB of exponent */
            out[i].c[1] = in[i].c[0];
            out[i].c[2] = in[i].c[3];
            out[i].c[3] = in[i].c[2];
        }
        else if (exp) {                          /* denormalized number */
            register int shft;

            out[i].c[0] = in[i].c[1] & 0x80;   /* keep sign, zero exponent */
            shft = 3 - exp;
            /* shift original mant by 1 or 2 to get denormalized mant */
            /* prefix mantissa with '1'b or '01'b as appropriate */
            out[i].c[1] = ((in[i].c[0] & 0x7f) >> shft) | (0x10 << exp);
            out[i].c[2] = (in[i].c[0] << (8-shft)) | (in[i].c[3] >> shft);
            out[i].c[3] = (in[i].c[3] << (8-shft)) | (in[i].c[2] >> shft);
        }
        else {                                  /* sign=1 -> infinity or NaN */
            out[i].c[0] = 0xff;                /* set exp to 255 */
                /* copy mantissa */
            out[i].c[1] = in[i].c[0] | 0x80;  /* LSB of exp = 1 */
            out[i].c[2] = in[i].c[3];
            out[i].c[3] = in[i].c[2];
        }
      }
    return(0);
}

            
int DFCVieeeF2vaxF(in, out, size)
union float_uint_uchar in[], out[];
int size;
{
    register unsigned char exp;
    int i;

    for (i=0; i<size; i++)
      {
         exp = (in[i].c[0] << 1) | (in[i].c[1] >> 7); /* extract exponent */
         if (exp) {                                  /* non-zero exponent */
            /* copy mantissa, last bit of exponent */
           out[i].c[0] = in[i].c[1];
           out[i].c[2] = in[i].c[3];
           out[i].c[3] = in[i].c[2];
           if (exp<254)                        /* normal value */
             out[i].c[1] = in[i].c[0] + 1;   /* actually adds two to exp */
           else {                              /* infinity or NaN */
             if (exp==254)                     /* unrepresentable - OFL */
               out[i].i = 0;                  /* set mant=0 for overflow */
            out[i].c[0] &= 0x7f;              /* set last bit of exp to 0 */
            out[i].c[1] = 0x80;               /* sign=1 exp=0 -> OFL or NaN */
          }
        }
        else if (in[i].c[1] & 0x60) {               /* denormalized value */
          register int shft;
    
          shft = (in[i].c[1] & 0x40) ? 1 : 2;  /* shift needed to normalize */
            /* shift mantissa */
            /* note last bit of exp set to 1 implicitly */
          out[i].c[0] = (in[i].c[1] << shft) & (in[i].c[2] >> (8-shft));
          out[i].c[3] = (in[i].c[2] << shft) & (in[i].c[3] >> (8-shft));
          out[i].c[2] = in[i].c[3] << shft;
          out[i].c[1] = (in[i].c[0] & 0x80);          /* sign */
          if (shft==1) {                          /* set exp to 2 */
            out[i].c[1] |= 0x01;
            out[i].c[0] &= 0x7f;                  /* set LSB of exp to 0 */
          }
        }
        else out[i].i = 0;                            /* zero */
      }
    return(0);
}
#endif /* CONVERT */


#if defined (__hpux)

/* these extra routines are used by HPUX machines */

/****************************************************************************
 * Routine: gerror                                                          *
 ****************************************************************************/

void gerror (str, Lstr)
char *str;
int  Lstr;
{
  int i;
  extern int errno;

  (void) strncpy (str, strerror (errno), Lstr);
  for (i = strlen (str); i < Lstr; i++) str[i] = ' ';   /* pad with spaces */
} /* End of gerror (str, Lstr) */

/****************************************************************************
 * Routine: getenv_                                                         *
 ****************************************************************************/

void getenv_ (name, value, Lname, Lvalue)
char *name, *value;
int Lname, Lvalue;
{
  int i;

  name[flength (name, Lname)] = (char) NULL; 

  (void) strncpy (value, getenv (name), Lvalue);
  for (i = strlen (value); i < Lvalue; i++) 
    value[i] = ' ';                                      /* pad with spaces */
} /* End of getenv_ (name, value, lname, lvalue) */

/****************************************************************************
 * Routine: rename_                                                         *
 ****************************************************************************/

int rename_ (from, to, Lfrom, Lto)
char *from, *to;
int Lfrom, Lto;
{
  from[flength (from, Lfrom)] = (char) NULL; 
  to[flength (to, Lto)] = (char) NULL; 

  return (rename (from, to));
} /* End of rename_ (from, to) */

/****************************************************************************
 * Routine: access_                                                         *
 ****************************************************************************/

int access_ (name, mode, Lname, Lmode)
char *name, *mode;
int Lname, Lmode;
{
  int imode;

  switch (*mode) {
    case 'R':
    case 'r': imode = R_OK;
              break;
    case 'W':
    case 'w': imode = W_OK;
              break;
    case 'X':
    case 'x': imode = X_OK;
              break;
    case 'E':
    case 'e':
    default:  imode = F_OK;
              break;
  };
  name[flength (name, Lname)] = (char) NULL; 

  return (access (name, imode));
} /* End of access_ (name, value) */

#endif /* defined (__hpux) */

#endif				/*  ! defined (KNOWN_MACHINE) */
