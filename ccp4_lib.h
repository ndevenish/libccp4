/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/

#ifndef __CCP4_LIB
#define __CCP4_LIB

#include <string.h>
#include "ccp4_types.h"
#include "library_file.h"
static char rcsidh[] = "$Id$";

#ifdef __cplusplus
namespace CCP4 {
extern "C" {
#endif

/****************************************************************************
 * Function prototypes                                                      *
 ****************************************************************************/

size_t ccp4_utils_flength (char *, int);

int ccp4_utils_translate_mode_float(float *, const void *, int, int);

void ccp4_utils_fatal (const char *);

void ccp4_utils_print (const char *message);

int ccp4_utils_setenv (char *);

int ccp4_utils_outbuf (void);

union float_uint_uchar ccp4_nan ();

int ccp4_utils_isnan (const union float_uint_uchar *);

void ccp4_utils_bml (int, union float_uint_uchar *);

void ccp4_utils_wrg (int, union float_uint_uchar *, float *);

void ccp4_utils_hgetlimits (int *, float *);

int ccp4_utils_mkdir (const char *, const char *);

int ccp4_utils_chmod (const char *, const char *);

void *ccp4_utils_malloc(size_t);

void *ccp4_utils_realloc(void *, size_t);

void *ccp4_utils_calloc(size_t, size_t);

int ccp4_file_size(const char *);

char *ccp4_utils_username(void);

char *ccp4_utils_basename(char *filename);

char *ccp4_utils_pathname(char *filename);

char *ccp4_utils_extension(char *filename);

void ccp4_utils_idate (int *);

char *ccp4_utils_date(char *);

void ccp4_utils_itime (int *);

char *ccp4_utils_time(char *);

float ccp4_utils_etime (float *);

#if defined (_MVS)
double ccp4_erfc( double x );
#endif

/****************************************************************************
*  End of prototypes                                                        *
*****************************************************************************/
#ifdef __cplusplus
}
}
#endif

#endif  /* __CCP4_LIB */