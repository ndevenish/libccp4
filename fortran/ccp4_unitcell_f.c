/*
     ccp4_unitcell_f.c: Fortran API to ccp4_unitcell.c
     Copyright (C) 2001  CCLRC, Martyn Winn

     This library is free software and is distributed under the terms and
     conditions of the CCP4 licence agreement as `Part 0' (Annex 2)
     software, which is version 2.1 of the GNU Lesser General Public
     Licence (LGPL) with the following additional clause:

        `You may also combine or link a "work that uses the Library" to
        produce a work containing portions of the Library, and distribute
        that work under terms of your choice, provided that you give
        prominent notice with each copy of the work that the specified
        version of the Library is used in it, and that you include or
        provide public access to the complete corresponding
        machine-readable source code for the Library including whatever
        changes were used in the work. (i.e. If you make changes to the
        Library you must distribute those, but you do not need to
        distribute source or object code to those portions of the work
        not covered by this licence.)'

     Note that this clause grants an additional right and does not impose
     any additional restriction, and so does not affect compatibility
     with the GNU General Public Licence (GPL). If you wish to negotiate
     other terms, please contact the maintainer.

     You can redistribute it and/or modify the library under the terms of
     the GNU Lesser General Public License as published by the Free Software
     Foundation; either version 2.1 of the License, or (at your option) any
     later version.

     This library is distributed in the hope that it will be useful, but
     WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     Lesser General Public License for more details.

     You should have received a copy of the CCP4 licence and/or GNU
     Lesser General Public License along with this library; if not, write
     to the CCP4 Secretary, Daresbury Laboratory, Warrington WA4 4AD, UK.
     The GNU Lesser General Public can also be obtained by writing to the
     Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
     MA 02111-1307 USA
*/

/** @file ccp4_unitcell_f.c
 *  Fortran API to ccp4_unitcell.c.
 *  Martyn Winn
 */

#include "ccp4_fortran.h"
#include "ccp4_unitcell.h"
static char rcsid[] = "$Id$";

/* from input cell and orthogonalisation code, find orthogonalisation
   and fractionalisation matrices. Returns cell volume. */

FORTRAN_SUBR ( CCP4UC_F_FRAC_ORTH_MAT, ccp4uc_f_frac_orth_mat,
          (const float cell[6], const int *ncode, 
	   float ro[3][3], float rf[3][3], float *volume),
          (const float cell[6], const int *ncode, 
	   float ro[3][3], float rf[3][3], float *volume),
          (const float cell[6], const int *ncode, 
	   float ro[3][3], float rf[3][3], float *volume))
{
  int i,j;
  double ro_cmat[3][3], rf_cmat[3][3], dcell[6];

  for (i = 0; i < 6; ++i) 
    dcell[i] = (double) cell[i];

  *volume =  (float) ccp4uc_frac_orth_mat(dcell, *ncode, ro_cmat, rf_cmat);
  for (i = 0; i < 3; ++i) {
    for (j = 0; j < 3; ++j) {
      ro[i][j] = (float) ro_cmat[j][i];
      rf[i][j] = (float) rf_cmat[j][i];
    }
  }
}

FORTRAN_SUBR ( CCP4UC_F_CALC_RCELL, ccp4uc_f_calc_rcell,
          (const float cell[6], float rcell[6], float *rvolume),
          (const float cell[6], float rcell[6], float *rvolume),
          (const float cell[6], float rcell[6], float *rvolume))
{
  int i;
  double dcell[6],drcell[6];

  for (i = 0; i < 6; ++i) 
    dcell[i] = (double) cell[i];

  *rvolume = (float) ccp4uc_calc_rcell(dcell, drcell);

  for (i = 0; i < 6; ++i) 
    rcell[i] = (float) drcell[i];

}

FORTRAN_SUBR ( CCP4UC_F_ORTH_TO_FRAC, ccp4uc_f_orth_to_frac,
          (const float rf[3][3], const float xo[3], float xf[3]),
          (const float rf[3][3], const float xo[3], float xf[3]),
	  (const float rf[3][3], const float xo[3], float xf[3]))
{
  int i,j;
  double rf_cmat[3][3], dxo[3], dxf[3];

  for (i = 0; i < 3; ++i) {
    dxo[i] = (double) xo[i];
    for (j = 0; j < 3; ++j) 
      rf_cmat[i][j] = (double) rf[j][i];
  }
  ccp4uc_orth_to_frac(rf_cmat, dxo, dxf);
  for (i = 0; i < 3; ++i) 
    xf[i] = (float) dxf[i];

}

FORTRAN_SUBR ( CCP4UC_F_FRAC_TO_ORTH, ccp4uc_f_frac_to_orth,
          (const float ro[3][3], const float xf[3], float xo[3]),
          (const float ro[3][3], const float xf[3], float xo[3]),
	  (const float ro[3][3], const float xf[3], float xo[3]))
{
  int i,j;
  double ro_cmat[3][3], dxf[3], dxo[3];

  for (i = 0; i < 3; ++i) {
    dxf[i] = (double) xf[i];
    for (j = 0; j < 3; ++j) 
      ro_cmat[i][j] = (double) ro[j][i];
  }
  ccp4uc_orth_to_frac(ro_cmat, dxf, dxo);
  for (i = 0; i < 3; ++i) 
    xo[i] = (float) dxo[i];

}

FORTRAN_SUBR ( CCP4UC_F_ORTHU_TO_FRACU, ccp4uc_f_orthu_to_fracu,
          (const float rf[3][3], const float uo[3], float uf[3]),
          (const float rf[3][3], const float uo[3], float uf[3]),
	  (const float rf[3][3], const float uo[3], float uf[3]))
{
  int i,j;
  double rf_cmat[3][3], duo[3], duf[3];

  for (i = 0; i < 3; ++i) {
    duo[i] = (double) uo[i];
    for (j = 0; j < 3; ++j) 
      rf_cmat[i][j] = (double) rf[j][i];
  }
  ccp4uc_orthu_to_fracu(rf_cmat, duo, duf);
  for (i = 0; i < 3; ++i) 
    uf[i] = (float) duf[i];

}

FORTRAN_SUBR ( CCP4UC_F_FRACU_TO_ORTHU, ccp4uc_f_fracu_to_orthu,
          (const float ro[3][3], const float uf[3], float uo[3]),
          (const float ro[3][3], const float uf[3], float uo[3]),
	  (const float ro[3][3], const float uf[3], float uo[3]))
{
  int i,j;
  double ro_cmat[3][3], duf[3], duo[3];

  for (i = 0; i < 3; ++i) {
    duf[i] = (double) uf[i];
    for (j = 0; j < 3; ++j) 
      ro_cmat[i][j] = (double) ro[j][i];
  }
  ccp4uc_orthu_to_fracu(ro_cmat, duf, duo);
  for (i = 0; i < 3; ++i) 
    uo[i] = (float) duo[i];

}

FORTRAN_SUBR ( CELLCHK, cellchk,
	       (const float cell1[6], const float cell2[6], const float *errfrc, int *ierr),
	       (const float cell1[6], const float cell2[6], const float *errfrc, int *ierr),
	       (const float cell1[6], const float cell2[6], const float *errfrc, int *ierr))
{
  int i;
  double dcell1[6], dcell2[6];

  for (i = 0; i < 6; ++i) {
    dcell1[i] = (double) cell1[i];
    dcell2[i] = (double) cell2[i];
  }

  *ierr = ccp4uc_cells_differ(dcell1, dcell2, (double) *errfrc);

}

