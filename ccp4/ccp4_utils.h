/*
     ccp4_utils.h: headers for utility functions.
     Copyright (C) 2001  CCLRC, Charles Ballard

     This code is distributed under the terms and conditions of the
     CCP4 Program Suite Licence Agreement as a CCP4 Library.
     A copy of the CCP4 licence can be obtained by writing to the
     CCP4 Secretary, Daresbury Laboratory, Warrington WA4 4AD, UK.
*/

/** @file ccp4_utils.h
 *  @brief   Utility functions.
 *  @author  Charles Ballard
 */

#ifndef __CCP4_UTILS
#define __CCP4_UTILS

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

/* turn on line buffering for stdout */
int ccp4_utils_outbuf (void);

/* turn off any buffering on stdin */
int ccp4_utils_noinpbuf (void);

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

char *ccp4_utils_joinfilenames(char *dir, char *file);

void ccp4_utils_idate (int *);

char *ccp4_utils_date(char *);

void ccp4_utils_itime (int *);

char *ccp4_utils_time(char *);

float ccp4_utils_etime (float *);

#if defined (_MSC_VER)
double ccp4_erfc( double x );
#endif

/****************************************************************************
*  End of prototypes                                                        *
*****************************************************************************/
#ifdef __cplusplus
}
}
#endif

#endif  /* __CCP4_UTILS */
