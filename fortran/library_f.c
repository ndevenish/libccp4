
/*  Include library.h                                                       */
#include "library.h"
#include "library_f.h"


/* This gets the length of a \ft{} string ([[character*]]\meta{len}         */
/* variable) \meta{s} with trailing blanks removed.  \fixme{Avoid lossage   */
/*   on null/blank string}                                                  */
/*                                                                          */
/* <internal routines>=                                                     */
static size_t flength (char *s, int len)
{
  while (s[--len] == ' ');
  return (++len);
}

#if ! defined (VMS)
/* <ustenv code>=                                                           */
FORTRAN_SUBR ( USTENV, ustenv,
         (fpstr str, int *result, int Lstr),
         (fpstr str, int *result),
         (fpstr str, int Lstr, int *result))
{
  size_t Length;
  char name[MAXFLEN];

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

  ccp4_ustenv (name, result);
}

#endif


FORTRAN_SUBR ( CUNLINK, cunlink,
      (fpstr filename, int Lfilename),
      (fpstr filename),
      (fpstr filename, int Lfilename))
{
  size_t Length;
  char tempfile[MAXFLEN];

#ifdef VMS
  return;                       /* can't do it */
#else
#  if CALL_LIKE_STARDENT
    Length = flength (filename->Str_pointer, filename->Str_length);
    if (Length > MAXFLEN) Length = MAXFLEN - 1;
    (void) strncpy (tempfile, filename->Str_pointer, Length);
#  else
    Length = flength (filename, Lfilename);
    if (Length > MAXFLEN) Length = MAXFLEN - 1;
    (void) strncpy (tempfile, filename, Length);
#  endif
  tempfile[Length] = '\0';

  ccp4_cunlink (tempfile);
#endif /* VMS */
}

#ifndef VMS                     /* we'll use the Fortran version in VMS*/
#ifndef _MVS
FORTRAN_SUBR ( CCPAL1, ccpal1,
     (void (* routne) (), int *n, int type[], int length[]),
     (void (* routne) (), int *n, int type[], int length[]),
     (void (* routne) (), int *n, int type[], int length[]))
{
  ccp4_ccpal1(routne, n, type, length);
}
#endif /* VMS */
#endif

FORTRAN_SUBR ( COPEN, copen,
    (int *iunit, fpstr filename, int *istat, int Lfilename),
    (int *iunit, fpstr filename, int *istat),
    (int *iunit, fpstr filename, int Lfilename, int *istat))
{
  size_t Length;
  char tempfile[MAXFLEN];

#if CALL_LIKE_STARDENT
  Length = flength (filename->Str_pointer, filename->Str_length);
  if (Length > MAXFLEN) Length = MAXFLEN - 1;
  (void) strncpy (tempfile, filename->Str_pointer, Length);
#else
#  if defined (VMS)
  Length = flength (filename->dsc$a_pointer, filename->dsc$w_length);
  if (Length > MAXFLEN) Length = MAXFLEN - 1;
  (void) strncpy (tempfile, filename->dsc$a_pointer, Length);
#  else
  Length = flength (filename, Lfilename);
  if (Length > MAXFLEN) Length = MAXFLEN - 1;
  (void) strncpy (tempfile, filename, Length);
#  endif
#endif
  tempfile[Length] = '\0';

  ccp4_copen (iunit, tempfile, *istat);
}

FORTRAN_SUBR ( QRARCH, qrarch,
    (int *iunit, int *ipos, int *ireslt),
    (int *iunit, int *ipos, int *ireslt),
    (int *iunit, int *ipos, int *ireslt))
{
  ccp4_qrarch (*iunit, *ipos, ireslt);
}

FORTRAN_SUBR ( QWARCH, qwarch,
    (int *iunit, int *ipos),
    (int *iunit, int *ipos),
    (int *iunit, int *ipos))
{
  ccp4_qwarch (*iunit, *ipos);
}

FORTRAN_SUBR ( QCLOSE, qclose,
    (int *iunit),
    (int *iunit),
    (int *iunit))
{
  ccp4_qclose (*iunit);
}

FORTRAN_SUBR ( QMODE, qmode,
    (int *iunit, int *mode, int *size),
    (int *iunit, int *mode, int *size),
    (int *iunit, int *mode, int *size))
{
  ccp4_qmode (*iunit, *mode, size);
}

FORTRAN_SUBR ( QREAD, qread,
    (int *iunit, uint8 * buffer, int *nitems, int *result),
    (int *iunit, uint8 * buffer, int *nitems, int *result),
    (int *iunit, uint8 * buffer, int *nitems, int *result))
{
  ccp4_qread (*iunit, buffer, *nitems, result);
}

FORTRAN_SUBR ( QREADC, qreadc,
    (int *iunit, fpstr  buffer, int *result, int Lbuffer),
    (int *iunit, fpstr  buffer, int *result),
    (int *iunit, fpstr  buffer, int Lbuffer, int *result))
{
  int n;

#if defined (VMS)
  n = buffer->dsc$w_length;
#else
#  if CALL_LIKE_STARDENT
  n = buffer->Str_length;
#  else                         /* normal */
  n = Lbuffer;
#  endif
#endif

  ccp4_qreadc (*iunit, buffer, (size_t) n, result);
}

FORTRAN_SUBR ( QWRITE, qwrite,
    (int *iunit, uint8 * buffer, int *nitems),
    (int *iunit, uint8 * buffer, int *nitems),
    (int *iunit, uint8 * buffer, int *nitems))
{
  ccp4_qwrite (*iunit, buffer, *nitems);
}

FORTRAN_SUBR ( QWRITC, qwritc,
    (int *iunit, fpstr  buffer, int Lbuffer),
    (int *iunit, fpstr  buffer),
    (int *iunit, fpstr  buffer, int Lbuffer))
{
  int n;

#if defined (VMS)
  n = buffer->dsc$w_length;
#else
#  if CALL_LIKE_STARDENT
  n = buffer->Str_length;
#  else                         /* normal */
  n = Lbuffer;
#  endif
#endif

  ccp4_qwritc (*iunit, buffer, (size_t) n);
}

FORTRAN_SUBR ( QSEEK, qseek,
    (int *iunit, int *irec, int *iel, int *lrecl),
    (int *iunit, int *irec, int *iel, int *lrecl),
    (int *iunit, int *irec, int *iel, int *lrecl))
{
  ccp4_qseek (*iunit, *irec, *iel, *lrecl);
}

FORTRAN_SUBR ( QBACK, qback,
    (int *iunit, int *lrecl),
    (int *iunit, int *lrecl),
    (int *iunit, int *lrecl))
{
  ccp4_qback (*iunit, *lrecl);
}

FORTRAN_SUBR ( QSKIP, qskip,
    (int *iunit, int *lrecl),
    (int *iunit, int *lrecl),
    (int *iunit, int *lrecl))
{
  ccp4_qskip (*iunit, *lrecl);
}

FORTRAN_SUBR ( CQINQ, cqinq,
    (int *istrm, fpstr filnam, int *length, int len_filnam),
    (int *istrm, fpstr filnam, int *length),
    (int *istrm, fpstr filnam, int len_filnam, int *length))
{
  char real_name[MAXFLEN];
  size_t Length;

#if CALL_LIKE_STARDENT
    Length = flength (filnam->Str_pointer, filnam->Str_length);
    if (Length > (size_t) MAXFLEN) Length = (size_t) MAXFLEN - 1;
    (void) strncpy (real_name, filnam->Str_pointer, Length);
#else
#  if defined (VMS)
     Length = flength (filnam->dsc$a_pointer, filnam->dsc$w_length);
     if (Length > (size_t) MAXFLEN) Length = (size_t) MAXFLEN - 1;
     (void) strncpy (real_name, filnam->dsc$a_pointer, Length);
#  else
     Length = flength (filnam, len_filnam);
     if (Length > (size_t) MAXFLEN) Length = (size_t) MAXFLEN - 1;
     (void) strncpy (real_name, filnam, Length);
#  endif
#endif
    real_name[Length] = '\0';

  ccp4_cqinq (*istrm, real_name, length);
}

FORTRAN_SUBR ( QLOCATE, qlocate,
    (int *iunit, int *locate),
    (int *iunit, int *locate),
    (int *iunit, int *locate))
{
  ccp4_qlocate (*iunit, locate);
}

FORTRAN_SUBR ( QNAN, qnan,
    (union float_uint_uchar *realnum),
    (union float_uint_uchar *realnum),
    (union float_uint_uchar *realnum))
{
  ccp4_qnan (realnum);
}

FORTRAN_SUBR ( CISNAN, cisnan,
    (union float_uint_uchar *realnum),
    (union float_uint_uchar *realnum),
    (union float_uint_uchar *realnum))
{
  ccp4_cisnan (realnum);
}

FORTRAN_SUBR ( CCPBML, ccpbml,
    (int *ncols, union float_uint_uchar cols[]),
    (int *ncols, union float_uint_uchar cols[]),
    (int *ncols, union float_uint_uchar cols[]))
{
  ccp4_ccpbml (*ncols, cols) ;
}

FORTRAN_SUBR ( CCPWRG, ccpwrg,
    (int *ncols, union float_uint_uchar cols[], float wminmax[]),
    (int *ncols, union float_uint_uchar cols[], float wminmax[]),
    (int *ncols, union float_uint_uchar cols[], float wminmax[]))
{
  ccp4_ccpwrg (*ncols, cols, wminmax) ;
}

FORTRAN_SUBR ( HGETLIMITS, hgetlimits,
    (int *IValueNotDet, float *ValueNotDet),
    (int *IValueNotDet, float *ValueNotDet),
    (int *IValueNotDet, float *ValueNotDet))
{
  ccp4_hgetlimits (IValueNotDet, ValueNotDet);
}

FORTRAN_SUBR ( CMKDIR, cmkdir,
    (const fpstr path, const fpstr cmode, int *result, int Lpath, int Lmode),
    (const fpstr path, const fpstr cmode, int *result),
    (const fpstr path, int Lpath, const fpstr cmode, int Lmode, int *result))
{ size_t Length;
  char name[MAXFLEN];

  /* truncate path to MAXFLEN - 1 characters, MAXFLEN defined in library.h */
  Length = (size_t) Lpath;
  if (Length > MAXFLEN) Length = MAXFLEN - 1; 
  (void) strncpy (name, path, Length);
  name[Length] = '\0'; 

  ccp4_cmkdir (name, cmode, result);
}

FORTRAN_SUBR ( CCHMOD, cchmod,
    (const fpstr path, const fpstr cmode, int *result, int Lpath, int Lmode),
    (const fpstr path, const fpstr cmode, int *result),
    (const fpstr path, int Lpath, const fpstr cmode, int Lmode, int *result))
{ size_t Length;
  char name[MAXFLEN];
  
  /* truncate path to MAXFLEN - 1 characters, MAXFLEN defined in library.h */
  Length = (size_t) Lpath;
  if (Length > MAXFLEN) Length = MAXFLEN - 1;
  (void) strncpy (name, path, Length);
  name[Length] = '\0'; 

  ccp4_cchmod (name, cmode, result);
}

