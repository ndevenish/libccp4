/*
     cmap_skew.c: set and fetch the skew matrix
     Copyright (C) 2001  CCLRC, Charles Ballard

     This code is distributed under the terms and conditions of the
     CCP4 Program Suite Licence Agreement as a CCP4 Library.
     A copy of the CCP4 licence can be obtained by writing to the
     CCP4 Secretary, Daresbury Laboratory, Warrington WA4 4AD, UK.
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
