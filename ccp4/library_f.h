
#ifndef CCP4_LIBRARY_F
#define CCP4_LIBRARY_F

/****************************************************************************
 * Prototype subroutines                                                    *
 ****************************************************************************/

static size_t flength (char *s, int len);

FORTRAN_SUBR ( USTENV, ustenv,
         (fpstr str, int *result, int Lstr),
         (fpstr str, int *result),
         (fpstr str, int Lstr, int *result));

FORTRAN_SUBR ( CUNLINK, cunlink,
      (fpstr filename, int Lfilename),
      (fpstr filename),
      (fpstr filename, int Lfilename));

FORTRAN_SUBR ( CCPAL1, ccpal1,
     (void (* routne) (), int *n, int type[], int length[]),
     (void (* routne) (), int *n, int type[], int length[]),
     (void (* routne) (), int *n, int type[], int length[]));

FORTRAN_SUBR ( COPEN, copen,
    (int *iunit, fpstr filename, int *istat, int Lfilename),
    (int *iunit, fpstr filename, int *istat),
    (int *iunit, fpstr filename, int Lfilename, int *istat));

FORTRAN_SUBR ( QRARCH, qrarch,
    (int *iunit, int *ipos, int *ireslt),
    (int *iunit, int *ipos, int *ireslt),
    (int *iunit, int *ipos, int *ireslt));

FORTRAN_SUBR ( QWARCH, qwarch,
    (int *iunit, int *ipos),
    (int *iunit, int *ipos),
    (int *iunit, int *ipos));

FORTRAN_SUBR ( QCLOSE, qclose,
    (int *iunit),
    (int *iunit),
    (int *iunit));

FORTRAN_SUBR ( QMODE, qmode,
    (int *iunit, int *mode, int *size),
    (int *iunit, int *mode, int *size),
    (int *iunit, int *mode, int *size));

FORTRAN_SUBR ( QREAD, qread,
    (int *iunit, uint8 * buffer, int *nitems, int *result),
    (int *iunit, uint8 * buffer, int *nitems, int *result),
    (int *iunit, uint8 * buffer, int *nitems, int *result));

FORTRAN_SUBR ( QREADC, qreadc,
    (int *iunit, fpstr  buffer, int *result, int Lbuffer),
    (int *iunit, fpstr  buffer, int *result),
    (int *iunit, fpstr  buffer, int Lbuffer, int *result));

FORTRAN_SUBR ( QWRITE, qwrite,
    (int *iunit, uint8 * buffer, int *nitems),
    (int *iunit, uint8 * buffer, int *nitems),
    (int *iunit, uint8 * buffer, int *nitems));

FORTRAN_SUBR ( QWRITC, qwritc,
    (int *iunit, fpstr  buffer, int Lbuffer),
    (int *iunit, fpstr  buffer),
    (int *iunit, fpstr  buffer, int Lbuffer));

FORTRAN_SUBR ( QSEEK, qseek,
    (int *iunit, int *irec, int *iel, int *lrecl),
    (int *iunit, int *irec, int *iel, int *lrecl),
    (int *iunit, int *irec, int *iel, int *lrecl));

FORTRAN_SUBR ( QBACK, qback,
    (int *iunit, int *lrecl),
    (int *iunit, int *lrecl),
    (int *iunit, int *lrecl));

FORTRAN_SUBR ( QSKIP, qskip,
    (int *iunit, int *lrecl),
    (int *iunit, int *lrecl),
    (int *iunit, int *lrecl));

FORTRAN_SUBR ( CQINQ, cqinq,
    (int *istrm, fpstr filnam, int *length, int len_filnam),
    (int *istrm, fpstr filnam, int *length),
    (int *istrm, fpstr filnam, int len_filnam, int *length));

FORTRAN_SUBR ( QLOCATE, qlocate,
    (int *iunit, int *locate),
    (int *iunit, int *locate),
    (int *iunit, int *locate));

FORTRAN_SUBR ( QNAN, qnan,
    (union float_uint_uchar *realnum),
    (union float_uint_uchar *realnum),
    (union float_uint_uchar *realnum));

FORTRAN_SUBR ( CISNAN, cisnan,
    (union float_uint_uchar *realnum),
    (union float_uint_uchar *realnum),
    (union float_uint_uchar *realnum));

FORTRAN_SUBR ( CCPBML, ccpbml,
    (int *ncols, union float_uint_uchar cols[]),
    (int *ncols, union float_uint_uchar cols[]),
    (int *ncols, union float_uint_uchar cols[]));

FORTRAN_SUBR ( CCPWRG, ccpwrg,
    (int *ncols, union float_uint_uchar cols[], float wminmax[]),
    (int *ncols, union float_uint_uchar cols[], float wminmax[]),
    (int *ncols, union float_uint_uchar cols[], float wminmax[]));

FORTRAN_SUBR ( HGETLIMITS, hgetlimits,
    (int *IValueNotDet, float *ValueNotDet),
    (int *IValueNotDet, float *ValueNotDet),
    (int *IValueNotDet, float *ValueNotDet));

FORTRAN_SUBR ( CMKDIR, cmkdir,
    (const fpstr path, const fpstr cmode, int *result, int Lpath, int Lmode),
    (const fpstr path, const fpstr cmode, int *result),
    (const fpstr path, int Lpath, const fpstr cmode, int Lmode, int *result));

FORTRAN_SUBR ( CCHMOD, cchmod,
    (const fpstr path, const fpstr cmode, int *result, int Lpath, int Lmode),
    (const fpstr path, const fpstr cmode, int *result),
    (const fpstr path, int Lpath, const fpstr cmode, int Lmode, int *result));

#endif  /* CCP4_LIBRARY_F */

