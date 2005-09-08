/*
     ccp4_types.h: CCP4 library.c macro definitions etc
     Copyright (C) 2001  CCLRC

     This code is distributed under the terms and conditions of the
     CCP4 Program Suite Licence Agreement as a CCP4 Library.
     A copy of the CCP4 licence can be obtained by writing to the
     CCP4 Secretary, Daresbury Laboratory, Warrington WA4 4AD, UK.
*/
#ifndef __CCP4_TYPES
#define __CCP4_TYPES

#include "ccp4_sysdep.h"

typedef unsigned short uint16;
#ifdef SIXTEENBIT
typedef unsigned long uint32;
#else
typedef unsigned int uint32;
#endif
typedef float float32;
typedef unsigned char uint8;
union float_uint_uchar {
    float32 f;
    uint32 i;
    uint8 c[4];
  };

typedef   char     *        pstr;

/* CCP4 library.c macro definitions */

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

typedef struct { double r;             /* real component and */
                 double i;             /* imaginary component of */
               } COMPLEX;              /* a complex number */

typedef struct { double r;             /* radial and */
                 double phi;           /* angular component of */
               } POLAR;                /* a complex number */

/* some simple macros, which may exist anyway */
#ifndef SQR
#define SQR(x) ((x)*(x))
#endif
#ifndef DEGREE
#define DEGREE(x) ((((x < 0)?(x)+2*M_PI:(x))*360)/(2*M_PI))
#endif
#ifndef RADIAN
#define RADIAN(x) ((((x<0)?(x)+360:(x))*2*M_PI)/360)
#endif
#ifndef MAX
#define MAX(x, y) (((x)>(y))?(x):(y))
#endif
#ifndef MIN
#define MIN(x, y) (((x)<(y))?(x):(y))
#endif
#ifndef ABS
#define ABS(x) (((x)<0)?-(x):(x))
#endif
#ifndef SIGN
#define SIGN(x) (((x)<0)?-1:1)
#endif

#endif   /* __CCP4_TYPES */
