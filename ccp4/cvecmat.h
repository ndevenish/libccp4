/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
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

float pow_ii(const float base, const int power);

#ifdef __cplusplus
}
#endif

#endif  /*!CCP4_VECMAT */
