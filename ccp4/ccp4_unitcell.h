/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/
#ifndef __CCP4_UNITCELL
#define __CCP4_UNITCELL

#ifdef  __cplusplus
namespace CCP4uc {
extern "C" {
#endif

#include <math.h>

/* from input cell and orthogonalisation code, find orthogonalisation
   and fractionalisation matrices. Returns cell volume. */
double ccp4uc_frac_orth_mat(const double cell[6], const int ncode, 
			   double ro[3][3], double rf[3][3]);

/* from input cell, find dimensions of reciprocal cell. 
   Returns reciprocal cell volume. */

double ccp4uc_calc_rcell(const double cell[6], double rcell[6]);

/* Convert orthogonal to fractional coordinates. Translation only if
   deliberate origin shift - does this ever happen? Leave it to the
   application. */

void ccp4uc_orth_to_frac(const double rf[3][3], const double xo[3], double xf[3]);

/* Convert fractional to orthogonal coordinates. */

void ccp4uc_frac_to_orth(const double ro[3][3], const double xf[3], double xo[3]);

/* Convert orthogonal to fractional u matrix. */

void ccp4uc_orthu_to_fracu(const double rf[3][3], const double uo[6], double uf[6]);

/* Convert fractional to orthogonal u matrix. */

void ccp4uc_fracu_to_orthu(const double ro[3][3], const double uf[6], double uo[6]);

/* Calculate cell volume from cell parameters */

double ccp4uc_calc_cell_volume(const double cell[6]);

/* Check cells agree within tolerance */

int ccp4uc_cells_differ(const double cell1[6], const double cell2[6], const double tolerance);

#ifdef __cplusplus
} }
#endif

#endif  /*!CCP4_UNITCELL */
