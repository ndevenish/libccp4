/*
     cmap_skew.c: set and fetch the skew matrix
     Copyright (C) 2001  CCLRC, Charles Ballard

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
#include <math.h>
#include "cmaplib.h"
#include "cmap_skew.h"
#include "cmap_errno.h"

/*! Set the values of the translation and rotation elements of the skew matrix.
 Note: the stored file is in FORTRAN order mat[fastest][slowest]
 \param mfile (CMMFile *)
 \param skew_mat (const float *) the skew translation vestor
 \param skew_trans (const float *) the skew rotation matrix (C ordering)
 \return 1 if either skew_trans or skew_mat is non-NULL  */
int ccp4_cmap_set_mask(CMMFile *mfile, const float *skew_mat, const float *skew_trans)
{
  int ictr, jctr;

  if (!mfile) {
      ccp4_signal( CCP4_ERRLEVEL(3) |  CMAP_ERRNO(CMERR_NoChannel),
                "ccp4_cmap_set_mask",NULL);
    return (EOF);}

  if (skew_trans)
    for(ictr = 0; ictr < 3 ; ++ictr) 
      mfile->skew.translation[ictr] = *(skew_trans + ictr);
  else
    for(ictr = 0; ictr < 3 ; ++ictr) 
      mfile->skew.translation[ictr] = 0.0F;

  if (skew_mat)
    for(ictr = 0; ictr < 3 ; ++ictr) 
      for (jctr = 0 ; jctr < 3 ; ++jctr) 
        mfile->skew.rotation[jctr][ictr] = *(skew_mat + (3*ictr) + jctr);
  else
    for(ictr = 0; ictr < 3 ; ++ictr) 
      for (jctr = 0 ; jctr < 3 ; ++jctr) 
        mfile->skew.rotation[jctr][ictr] = 0.0F;

  return (skew_trans != NULL || skew_mat != NULL );
}

/*! Get the values of the translation and rotation elements of the skew matrix.
 Note: the stored file is in FORTRAN order mat[fastest][slowest], the returned
 values are in C mat[slowest][fastest] ordering
 \param mfile (CMMFile *)
 \param skew_mat (const float *) the skew translation vestor
 \param skew_trans (const float *) the skew rotation matrix (C ordering)
 \return 1 if mask is set */
int ccp4_cmap_get_mask(const CMMFile *mfile, float *skew_mat, float *skew_trans)
{
  int ictr, jctr;
  
  if (!mfile || !skew_mat || !skew_trans) {
      ccp4_signal( CCP4_ERRLEVEL(3) |  CMAP_ERRNO(CMERR_NoChannel),
                "ccp4_cmap_get_mask",NULL);
    return (EOF);}

  for(ictr = 0; ictr < 3 ; ++ictr) 
     *(skew_trans + ictr) = mfile->skew.translation[ictr];

  for(ictr = 0; ictr < 3 ; ++ictr) 
    for (jctr = 0 ; jctr < 3 ; ++jctr) 
      *(skew_mat + (3*ictr) + jctr) = mfile->skew.rotation[jctr][ictr];

  return skew_set(&mfile->skew);
}

/*! Internal: test whether values are set in the skew matrices
  \param skew (CMMFile_Skew *)
  \return TRUE or FALSE */
int skew_set(const CMMFile_Skew *skew)
{
  return 
    skew->translation[0] != 0.0F ||
    skew->translation[1] != 0.0F ||
    skew->translation[2] != 0.0F ||
    skew->rotation[0][0] != 0.0F ||
    skew->rotation[0][1] != 0.0F ||
    skew->rotation[0][2] != 0.0F ||
    skew->rotation[1][0] != 0.0F ||
    skew->rotation[1][1] != 0.0F ||
    skew->rotation[1][2] != 0.0F ||
    skew->rotation[2][0] != 0.0F ||
    skew->rotation[2][1] != 0.0F ||
    skew->rotation[2][2] != 0.0F;
}
