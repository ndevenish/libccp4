/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/

/** @file library_utils.c
 *  Utility functions.
 *  Charles Ballard
 */

#include <time.h>
#include <math.h>
#include "ccp4_lib.h"
#include "ccp4_sysdep.h"
#include "ccp4_errno.h"

#define CCP4_ERRNO(y) (CCP4_ERR_UTILS | (y))          
                                       
static char rcsid[] = "$Id$";

static uint16 nativeIT = NATIVEIT; /* machine integer type */
static uint16 nativeFT = NATIVEFT; /* machine float type */

int ccp4_utils_translate_mode_float(float *out, const void *buffer, int dim, int mode)
{
  unsigned char *ucp;
  unsigned short *usp;
  float *fp = out, *ufp, tmp1, tmp2;
  register int ctr;

  switch(mode) {
  case 0:
    ucp = (unsigned char *)buffer;
    for(ctr = 0; ctr < dim ; ++ctr)
      *fp++ = (float) *ucp++;
    break;
  case 1:
    usp = (unsigned short *)buffer;
    for(ctr = 0; ctr < dim ; ++ctr)
      *fp++ = (float) *usp++;
    break;
  case 3:
    /* complex short (define type ?) */
    usp = (unsigned short *)buffer;
    for(ctr = 0; ctr < dim ; ++ctr) {
      tmp1 = (float) *usp++;
      tmp2 = (float) *usp++;
      *fp++ = (float) sqrt(tmp1*tmp1 + tmp2*tmp2);
    }
    break;
  case 4:
    /* complex real (define type ?) */
    ufp = (float *)buffer;
    for(ctr = 0; ctr < dim ; ++ctr) {
      tmp1 = *ufp++;
      tmp2 = *ufp++;
      *fp++ = (float) sqrt(tmp1*tmp1 + tmp2*tmp2);
    }
    break;
  case 2:
  default:
    break;
  }
  
  return (ctr);
}

/* This gets the length of a Fortran string with trailing blanks removed. 
   Previous version fixed for empty strings  */
size_t ccp4_utils_flength (char *s, int len)
{
  while (s[--len] == ' ')
    if (len == 0) return 0;
  return (++len);
}

void ccp4_utils_print (const char *message)
{
  printf ("%s\n",message);
 }

#if ! defined (VMS)
int ccp4_utils_setenv (char *str)
{
#if defined (sgi) || defined (sun) || defined (__hpux) || \
    defined(_AIX) || defined(ultrix) || defined (__OSF1__) || \
    defined (__osf__) || defined (__FreeBSD__) || defined (linux) || \
    defined (titan) || defined (_WIN32)
  /* putenv is the POSIX.1, draft 3 proposed mechanism */
      /* ESV seems to have it in the SysVile universe */
  int putenv ();
  char *param;

  if ( (param = (char *) ccp4_utils_malloc( (strlen(str)+1)*sizeof(char) )) == NULL) {
    ccp4_errno = CCP4_ERRNO(errno);
    return -1; }
  strcpy(param,str);
  return (putenv (param));
  /* note the necessary lack of free() */
#else
  /* setenv is not POSIX, BSD might have to use `index' */
  int setenv ();
  char *param1,*param2;

  if ( (param1 = (char *) ccp4_utils_malloc( (strlen(str)+1)*sizeof(char) )) == NULL) {
    ccp4_errno = CCP4_ERRNO(errno);
    return -1; }
  strcpy(param1,str);
  if ((param2 = (char *) strchr(param1, '=')) == NULL) {
    ccp4_errno = CCP4_ERRNO(errno);
    return -1; }
  *param2++ = '\0';
  return (setenv (param1, param2, 1));
#endif
}
#endif

#if ! defined (VMS)
int ccp4_utils_outbuf(void)
{
#if defined (sgi) || defined (sun) || \
    defined(ultrix) || defined (__OSF1__) || \
    defined (__FreeBSD__)
  return setlinebuf(stdout);
#else
#  if defined (_AIX)
  return -1;
#  else
  return setvbuf(stdout, NULL, _IOLBF, 0);
#  endif
#endif
}
#endif

union float_uint_uchar ccp4_nan ()

#if NATIVEFT == DFNTF_BEIEEE || NATIVEFT == DFNTF_LEIEEE
#  define NAN 0xfffa5a5a
#endif
/* For \idx{Convex} native mode and \idx{VAX} use a \idx{Rop} value:        */
/*                                                                          */
/* <magic numbers>=                                                         */
#if NATIVEFT == DFNTF_CONVEXNATIVE
#  define NAN 0x80000000
#endif
#if NATIVEFT == DFNTF_VAX
#  define NAN 0x00008000
#endif
#ifndef NAN
#  error "NAN isn't defined (needs NATIVEFT)"
#endif
{
  union float_uint_uchar realnum;

  realnum.i = NAN;
  return (realnum);
}

int ccp4_utils_isnan (const union float_uint_uchar *realnum)
{
    switch (nativeFT) {
    case DFNTF_BEIEEE :
    case DFNTF_LEIEEE :
      return ((realnum->i & 0x7f800000) == 0x7f800000); /* exponent all 1s */
    case DFNTF_CONVEXNATIVE :
      return ((realnum->i & 0xff800000) == 0x80000000);      
    case DFNTF_VAX :
      return ((realnum->i & 0x0000ff80) == 0x00008000);
    default :
      ccp4_fatal("CCP4_UTILS_ISNAN: bad nativeFT");
      return 0;                   /* avoid compiler warning */
    }
}

#define MDFBIG -1.0E10          /* BIOMOL absence flag value */
void ccp4_utils_bml (int ncols, union float_uint_uchar cols[])
{
  int i;
  for (i=0; i<ncols; i++)
    if (cols[i].i != NAN)
      if (cols[i].f <= MDFBIG) cols[i].f = 0.0;
}

void ccp4_utils_wrg (int ncols, union float_uint_uchar cols[], float wminmax[])
{
  int i;
  for (i=0; i<ncols; i++)
    if (cols[i].i != NAN)
       if (cols[i].f > MDFBIG) {
         if (cols[i].f < wminmax[2*i]) wminmax[2*i] = cols[i].f;
         if (cols[i].f > wminmax[1+2*i]) wminmax[1+2*i] = cols[i].f; }
}

void ccp4_utils_hgetlimits (int *IValueNotDet, float *ValueNotDet)
{
  *IValueNotDet = INT_MAX;
  *ValueNotDet  = FLT_MAX;
}

int ccp4_utils_mkdir (const char *path, const char *cmode)
#if !defined (_MVS) && !defined(_WIN32)
{  
  mode_t mode = 0;
  int result; 
#if defined (__APPLE__)
  static const unsigned short TBM = 0x07;

  switch (strlen(cmode)) {
  case 4:
    mode |= (*cmode & TBM) << 9 ;
    mode |= (*(cmode+1) & TBM) << 6 ;
    mode |= (*(cmode+2) & TBM) << 3 ;
    mode |= (*(cmode+3) & TBM) ;
    break;
  case 3:
    mode |= (*cmode & TBM) << 6 ;
    mode |= (*(cmode+1) & TBM) << 3 ; 
    mode |= (*(cmode+2) & TBM) ;      
    break;
  case 2:
    mode |= (*cmode & TBM) << 3 ;
    mode |= (*(cmode+1) & TBM) ;
    break;
  case 1:
    mode |= (*cmode & TBM) ;
    break;
  default:
    mode = 0x0fff ;
  }
#else 
/* Possible modes (see stat.h)
  Currently pass 3-character string and interpret as octal.
  Try also S_IRWXU, S_IRWXG, etc. */
  sscanf(cmode,"%o",&mode);
#endif   
  result = mkdir(path,mode); 

  if (result == -1) {
    if (errno == EEXIST) {
      result = 1;
    }
  }
  return (result); 
}
#else
   {
     printf("No harvesting on NT.");
     return (-1);
   }
#endif

int ccp4_utils_chmod (const char *path, const char *cmode)
#if !defined (_MVS) || !defined(_WIN32)
{ mode_t mode = 0;
#if defined (__APPLE__)
  static const unsigned short TBM = 0x07;

  switch (strlen(cmode)) {
  case 4:
    mode |= (*cmode & TBM) << 9 ;
    mode |= (*(cmode+1) & TBM) << 6 ;
    mode |= (*(cmode+2) & TBM) << 3 ;
    mode |= (*(cmode+3) & TBM) ;
    break;
  case 3:
    mode |= (*cmode & TBM) << 6 ;
    mode |= (*(cmode+1) & TBM) << 3 ; 
    mode |= (*(cmode+2) & TBM) ;      
    break;
  case 2:
    mode |= (*cmode & TBM) << 3 ;
    mode |= (*(cmode+1) & TBM) ;
    break;
  case 1:
    mode |= (*cmode & TBM) ;
    break;
  default:
    mode = 0x0fff ;
  }
#else 
/* Possible modes (see stat.h)
  Currently pass 3-character string and interpret as octal.
  Try also S_IRWXU, S_IRWXG, etc. */
  sscanf(cmode,"%o",&mode);
#endif
  return (chmod(path,mode)); 
}
#else
   {
     printf("No harvesting on NT.");
     return (-1);
   }
#endif

/* CCP4_UTILS_MALLOC
   This is a wrapper for the malloc function, which adds some
   error trapping */

void *ccp4_utils_malloc(size_t size)
/* Wrapper for malloc function; should trap for errors */

{ void *val; 

  val = malloc (size);
  if (!val && size)
    {
      perror ("Failure in ccp4_utils_malloc");
      abort ();
    }
  return val;}

/* CCP4_UTILS_REALLOC
   This is a wrapper for the realloc function, which adds some
   error trapping */

void *ccp4_utils_realloc(void *ptr, size_t size)
{ void *val; 

  val = realloc (ptr, size);
  if (!val && size)
    {
      perror ("Failure in ccp4_utils_realloc");
      abort ();
    }
  return val;}

/* CCP4_UTILS_CALLOC
   This is a wrapper for the calloc function, which adds some
   error trapping */

void *ccp4_utils_calloc(size_t nelem , size_t elsize)
{ void *val; 

  val = calloc (nelem, elsize);
  if (!val && elsize)
    {
      perror ("Failure in ccp4_utils_calloc");
      abort ();
    }
  return val;}


/** Return the user's login name.
 * Note that getlogin only works for processes attached to
 * a terminal, and hence won't work from the GUI. 
 * @return pointer to character string containing login name.
 */
char *ccp4_utils_username(void)
#if ! defined (_MVS) && ! defined (_WIN32)
{ 
  char *userid=NULL;
  return(cuserid(userid)); 
}
#else
{
  printf("No login id under ming32\n");
  return NULL;
}
#endif

/** Extracts the basename from a full file name.
 * Separators for directories and extensions are OS-specific.
 * @param filename full file name string.
 * @return pointer to basename
 */
char *ccp4_utils_basename(char *filename)
{
  int i, indx1=-1, length;
  char *basename;

  for ( i = strlen(filename)-1; i >= 0; i-- ) {
    if (filename[i] == PATH_SEPARATOR) {
      indx1 = i; 
      break;
    }
  }
  length = strlen(filename) - indx1;
  for ( i = indx1+1; i < strlen(filename); i++ ) 
    if (filename[i] == EXT_SEPARATOR) {
      length = i - indx1; 
      break;
    }
  basename = ccp4_utils_malloc(length*sizeof(char));
  strncpy(basename,filename+indx1+1,length-1);
  basename[length-1]='\0';
  return basename;
}

/** Extracts the pathname from a full file name.
 * Separators for directories and extensions are OS-specific.
 * @param filename full file name string.
 * @return pointer to pathname with trailing separator.
 */
char *ccp4_utils_pathname(char *filename)
{
  int i, indx1=-1, length;
  char *pathname;

  for ( i = strlen(filename)-1; i >= 0; i-- ) {
    if (filename[i] == PATH_SEPARATOR) {
      indx1 = i; 
      break;
    }
  }
  length = indx1+2;
  pathname = ccp4_utils_malloc(length*sizeof(char));
  strncpy(pathname,filename,length-1);
  pathname[length-1]='\0';
  return pathname;
}

/** Extracts the extension from a full file name.
 * Separators for directories and extensions are OS-specific.
 * @param filename full file name string.
 * @return pointer to extension
 */
char *ccp4_utils_extension(char *filename)
{
  int i, indx1=-1, length=1;
  char *extension;

  for ( i = strlen(filename)-1; i >= 0; i-- ) {
    if (filename[i] == EXT_SEPARATOR) {
      indx1 = i; 
      length = strlen(filename) - indx1;
      break;
    } else if (filename[i] == PATH_SEPARATOR) {
      indx1 = i; 
      length = 1;
      break;
    }
  }
  extension = ccp4_utils_malloc(length*sizeof(char));
  strncpy(extension,filename+indx1+1,length-1);
  extension[length-1]='\0';
  return extension;
}

void ccp4_utils_idate (int iarray[3])
{
     struct tm *lt;
     time_t tim;
     tim = time(NULL);
     lt = localtime(&tim);
     iarray[0] = lt->tm_mday;
     iarray[1] = lt->tm_mon+1;  /* need range 1-12 */
     iarray[2] = lt->tm_year + 1900;
}

char *ccp4_utils_date(char *date)
{
  int iarray[3];

  ccp4_utils_idate(iarray);
  sprintf(date,"%2d/%2d/%4d",iarray[0],iarray[1],iarray[2]);
  date[10] = '\0';

  return (date);
}

/** Function to obtain current time.
 * @param iarray Array containing hours, minutes and seconds.
 * @return void.
 */
void ccp4_utils_itime (int iarray[3])
{
     struct tm *lt;
     time_t tim;
     tim = time(NULL);
     lt = localtime(&tim);
     iarray[0] = lt->tm_hour; 
     iarray[1] = lt->tm_min; 
     iarray[2] = lt->tm_sec;
}

/** Alternative to ccp4_utils_itime with time as character string.
 * @param time Character string of form HH:MM:SS
 * @return pointer to character string.
 */
char *ccp4_utils_time(char *time)
{
  int iarray[3];

  ccp4_utils_itime(iarray);
  sprintf(time,"%2.2d:%2.2d:%2.2d",iarray[0],iarray[1],iarray[2]);
  time[8] = '\0';

  return (time);
}

/** Function to obtain User and System times.
 * @param tarray Array containing User and System times.
 * @return Sum of User and System times.
 */
float ccp4_utils_etime (float tarray[2])
#if ! defined (_MVS) && ! defined (_WIN32)
{
  static long clk_tck = 0;

  struct tms buffer;
  time_t utime, stime;
  if (! clk_tck) clk_tck = sysconf(_SC_CLK_TCK);
  (void) times(&buffer);
  tarray[0] = (float) buffer.tms_utime / (float)clk_tck;
  tarray[1] = (float) buffer.tms_stime / (float)clk_tck;
  return (tarray[0]+tarray[1]);
}
#else
{
  printf("etime not implemented under this compiler.\n");
  return (0.0f);
}
#endif

#if defined (__hpux) || defined (_AIX)
/* <AIX and HPUX support>=                                                  */
void gerror (str, Lstr)
char *str;
int  Lstr;
{
  int i;

  if (errno == 0) {             /* Avoid `Error 0' or some such message */
    for (i=1; Lstr; i++)
      str[i] = ' ';
  } else {
    (void) strncpy (str, strerror (errno), Lstr);
    for (i = strlen (str); i < Lstr; i++) str[i] = ' ';  /* pad with spaces */
  }
} /* End of gerror (str, Lstr) */

int ierrno () {
  return errno;
}

#endif             /*  HPUX and AIX support */
#if defined(F2C) || defined(G77)
/* <f2c support>=                                                           */
int exit_ (status)
     int *status;
{
  f_exit ();                    /* may or may not be registered with
                                   exit, depending on the C libraries
                                   capabilities, but is idempotent */
  exit (*status);
}

int time_ ()
{
  return (int) time (NULL);
}

int getpid_ ()
{
  return (int) getpid ();
}

/* following are from libI77/fio.h */
#define MXUNIT 100
typedef struct
{       FILE *ufd;      /*0=unconnected*/
        char *ufnm;
        long uinode;
        int udev;
        int url;        /*0=sequential*/
        flag useek;     /*true=can backspace, use dir, ...*/
        flag ufmt;
        flag uprnt;
        flag ublnk;
        flag uend;
        flag uwrt;      /*last io was write*/
        flag uscrtch;
} unit;
extern unit f__units[];
#define TRUE_ (1)
#define FALSE_ (0)
#define err(f,m,s) {if(f) errno= m; else f__fatal(m,s); return(m);}
/* end of fio.h extract */

int isatty_ (lunit)
     int *lunit;
{
  if (*lunit>=MXUNIT || *lunit<0)
    err(1,101,"isatty");
  /* f__units is a table of descriptions for the unit numbers (defined
     in io.h) with file descriptors rather than streams */
  return (isatty(fileno((f__units[*lunit]).ufd)) ? TRUE_ : FALSE_);
}

int gerror_ (str, Lstr)
char *str;
int  Lstr;
{
  int i;

  if (errno == 0) {             /* Avoid `Error 0' or some such message */
    for (i=1; Lstr; i++)
      str[i] = ' ';
  } else {
    (void) strncpy (str, strerror (errno), Lstr);
    for (i = strlen (str); i < Lstr; i++) str[i] = ' ';  /* pad with spaces */
  }
  return 0;
}

int ierrno_ () {
  return errno;
}

int itime_ (array)
     int array[3];
{
     struct tm *lt;
     time_t tim;
     tim = time(NULL);
     lt = localtime(&tim);
     array[0] = lt->tm_hour; array[1] = lt->tm_min; array[2] = lt->tm_sec;
}
/* These ought to be intrinsic, but they should only be applied to          */
/* [[INTEGER]] arguments.  The types [[integer]] and [[logical]] are both   */
/* assumed to be [[int]].                                                   */
/*                                                                          */
/* <f2c support>=                                                           */
int /* integer */ ibset_ (a, b)
     int /* integer */ *a, *b;
{
  return (*a) | 1<<(*b);
}

int /* integer */ ibclr_ (a, b)
     int /* integer */ *a, *b;
{
  return (*a) & ~(1<<(*b));
}

int /* logical */ btest_ (a, b)
     int /* integer */ *a, *b;
{
  return ((((unsigned long) *a)>>(*b)))&1 ? TRUE_ : FALSE_;
}
#endif              /* F2C support  */

#if defined (_MVS)
double ccp4_erfc( double x )
{
  double t,z,ans;

  z=fabs(x);
  t=1.0/(1.0+0.5*z);
  ans=t*exp(-z*z-1.26551223+t*(1.00002368+t*(0.37409196+t*(0.09678418+
      t*(-0.18628806+t*(0.27886807+t*(-1.13520398+t*(1.48851587+
      t*(-0.82215223+t*0.17087277)))))))));
  return  x >= 0.0 ? ans : 2.0-ans;
}
#endif

#if defined (__APPLE__)
void _carbon_init(int argc, char **argv) {}
void _objcInit(void) {}
#endif

#  if (defined _WIN32) || (defined _MVS)
double rint(double x) { 
  if (x >= 0.) {
   return (double)(int)(x+.5);
  }
  return (double)(int)(x-.5);
}
#endif
