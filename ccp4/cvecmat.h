/*
     cvecmat.h: header file for cvecmat.c
     Copyright (C) 2001  CCLRC, Martyn Winn

     This code is distributed under the terms and conditions of the
     CCP4 Program Suite Licence Agreement as a CCP4 Library.
     A copy of the CCP4 licence can be obtained by writing to the
     CCP4 Secretary, Daresbury Laboratory, Warrington WA4 4AD, UK.
*/
#ifndef __CCP4_VECMAT
#define __CCP4_VECMAT

#ifdef  __cplusplus
extern "C" {
#endif
static char rcsidhv[] = "$Id$";

void ccp4_dcross(const double a[3], const double b[3], double c[3]);
void ccp4_3matmul(double c[3][3], const double a[3][3], const double b[3][3]);
void ccp4_4matmul( float c[4][4], const float  a[4][4], const float b[4][4]);
double invert3matrix(const double a[3][3], double ai[3][3]);
float invert4matrix(const float a[4][4], float ai[4][4]);

float ccp4_pow_ii(const float base, const int power);

#ifdef __cplusplus
}
#endif

#endif  /*!CCP4_VECMAT */
