/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/

/** @file csymlib_f.c
 *  Fortran API to csymlib.c.
 *  Created Sept. 2001 by Martyn Winn
 */

/*#define FORTRAN_CALL_DEBUG 1*/

#if defined (FORTRAN_CALL_DEBUG)
#  define CSYMLIB_DEBUG(x) x
#else
#  define CSYMLIB_DEBUG(x)
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "ccp4_fortran.h"
#include "ccp4_parser.h"
#include "ccp4_spg.h"
#include "csymlib.h"
#include "cmtzlib.h"
#include "cvecmat.h"
static char rcsid[] = "$Id$";

#define MAXSYM 192

static CCP4SPG *spacegroup;          /* allow more than one spacegroup ?? */

FORTRAN_SUBR ( INVSYM, invsym,
               (const float a[4][4], float ai[4][4]),
               (const float a[4][4], float ai[4][4]),
               (const float a[4][4], float ai[4][4]))
{
  CSYMLIB_DEBUG(puts("CSYMLIB_F: INVSYM");)

  invert4matrix(a,ai);
}

/* Now same as symfr2 */
FORTRAN_SUBR ( SYMFR3, symfr3,
               (const fpstr icol, const int *i1, int *ns, float rot[MAXSYM][4][4],
                     int *eflag, int icol_len),
               (const fpstr icol, const int *i1, int *ns, float rot[MAXSYM][4][4],
                     int *eflag),
               (const fpstr icol, int icol_len, const int *i1, int *ns, 
                     float rot[MAXSYM][4][4], int *eflag))

{ 
  char *temp_name;
  int i,j,k,nsym;
  float tmp_rot[4][4];

  CSYMLIB_DEBUG(puts("CSYMLIB_F: SYMFR3");)

  temp_name = ccp4_FtoCString(FTN_STR(icol)+(*i1-1), FTN_LEN(icol)-(*i1-1));
  nsym = symop_to_mat4( temp_name, temp_name+strlen(temp_name), tmp_rot);
  for (i = 0; i < nsym; ++i)
    for (j = 0; j < 4; ++j) 
      for (k = 0; k < 4; ++k) 
        rot[*ns+i-1][j][k] = tmp_rot[k][j];
  *ns += nsym - 1;
  *eflag = 0;

  free(temp_name);
}

/* This is Charles' version */
FORTRAN_SUBR( SYMFR2, symfr2,
	      (fpstr symchs, int *icol, int *nsym, float rot[MAXSYM][4][4], int symchs_len),
	      (fpstr symchs, int *icol, int *nsym, float rot[MAXSYM][4][4]),
	      (fpstr symchs, int symchs_len, int *icol, int *nsym, float rot[MAXSYM][4][4]))
/* symfr2   ---- Read and interpret symmetry operations

   SYMFR2 recognises the following types of input:
      real space symmetry operatoions, e.g. X+1/2, Y-X, Z
      reciprocal space operations,     e.g. h,l-h,-k
      reciprocal axis vectors,         e.g. a*+c*,c*, -b*
      real space axis vectors,         e.g. a,c-a,-b

   The subroutine returns the appropriate 4x4 transformation
   matrix for each operation.  The calling program must 
   interpret the resutling matrix(ces) correctly.

   On entry, icol is the first character to look at 
             nsym is the number of the first symmetry
	     operation to be read, and returns with the last
	     one read

*/
{
  char *temp_name;
  int i,j,k,ns;
  float tmp_rot[4][4];

  CSYMLIB_DEBUG(puts("CSYMLIB_F: SYMFR2");)

  temp_name = ccp4_FtoCString(FTN_STR(symchs)+(*icol-1), FTN_LEN(symchs)-(*icol-1));
  ns = symop_to_mat4( temp_name, temp_name+strlen(temp_name), tmp_rot);
  for (i = 0; i < ns; ++i)
    for (j = 0; j < 4; ++j) 
      for (k = 0; k < 4; ++k) 
        rot[*nsym+i-1][j][k] = tmp_rot[k][j];
  *nsym += ns - 1;

  free(temp_name);
}

/** Fortran wrapper for mat4_to_symop.
 * @param nsm number of symmetry matrices passed.
 * @param rsm symmetry matrices.
 * @param symchs symmetry strings returned.
 * @param iprint print flag.
 */
FORTRAN_SUBR ( SYMTR3, symtr3,
               (const int *nsm, const float rsm[MAXSYM][4][4], 
                     fpstr symchs, const int *iprint, int symchs_len),
               (const int *nsm, const float rsm[MAXSYM][4][4], 
                     fpstr symchs, const int *iprint),
               (const int *nsm, const float rsm[MAXSYM][4][4], 
                     fpstr symchs, int symchs_len, const int *iprint))

{ char temp_symch[80];
 int i,j,k;
 float rsym[4][4];

  CSYMLIB_DEBUG(puts("CSYMLIB_F: SYMTR3");)

  for (i = 0; i < *nsm; ++i) {  
    /* need to transpose F to C */
    for (j = 0; j < 4; ++j) 
      for (k = 0; k < 4; ++k) 
        rsym[j][k] = rsm[i][k][j];
    mat4_to_symop(temp_symch,temp_symch+80,rsym);
    ccp4_CtoFString(FTN_STR(symchs+i*FTN_LEN(symchs)),FTN_LEN(symchs),temp_symch);

    if (*iprint) {
      printf("Symmetry %d %s \n",i+1,temp_symch);
    }
  }
}

/** Fortran wrapper for mat4_to_symop.
 * @param nsm number of symmetry matrices passed.
 * @param rsm symmetry matrices.
 * @param symchs symmetry strings returned.
 */
FORTRAN_SUBR ( SYMTR4, symtr4,
               (const int *nsm, const float rsm[MAXSYM][4][4], 
                     fpstr symchs, int symchs_len),
               (const int *nsm, const float rsm[MAXSYM][4][4], 
                     fpstr symchs),
               (const int *nsm, const float rsm[MAXSYM][4][4], 
                     fpstr symchs, int symchs_len))

{ char temp_symch[80];
 int i,j,k;
 float rsym[4][4];

  CSYMLIB_DEBUG(puts("CSYMLIB_F: SYMTR4");)

  for (i = 0; i < *nsm; ++i) {  
    /* need to transpose F to C */
    for (j = 0; j < 4; ++j) 
      for (k = 0; k < 4; ++k) 
        rsym[j][k] = rsm[i][k][j];
    mat4_to_symop(temp_symch,temp_symch+80,rsym);
    ccp4_CtoFString(FTN_STR(symchs+i*FTN_LEN(symchs)),FTN_LEN(symchs),temp_symch);
  }
}

FORTRAN_SUBR ( PGMDF, pgmdf,
               (int *jlass, int*jcentr, int jscrew[3]),
               (int *jlass, int*jcentr, int jscrew[3]),
               (int *jlass, int*jcentr, int jscrew[3]))
{
  static int klass, icentr, iscrew[3];

  CSYMLIB_DEBUG(puts("CSYMLIB_F: PGMDF");)

  if (*jlass==0) {
    /* need to set these variables */
    *jlass = klass;
    *jcentr = icentr;
    jscrew[0] = iscrew[0];
    jscrew[1] = iscrew[1];
    jscrew[2] = iscrew[2];
  } else {
    klass = *jlass;
    icentr = *jcentr;
    iscrew[0] = jscrew[0];
    iscrew[1] = jscrew[1];
    iscrew[2] = jscrew[2];
  }
  /* sorry, too lazy to do write statements! */
}

FORTRAN_SUBR ( PGDEFN, pgdefn,
               (fpstr nampg, int *nsymp, const int *nsym, float rsmt[192][4][4],
                const ftn_logical *lprint, int nampg_len),
               (fpstr nampg, int *nsymp, const int *nsym, float rsmt[192][4][4],
                const ftn_logical *lprint),
               (fpstr nampg, int nampg_len, int *nsymp, const int *nsym, 
                float rsmt[192][4][4], const ftn_logical *lprint))
{
  int i,k,l;
  ccp4_symop *op1;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: PGDEFN");)

  /* free any existing spacegroup and start again */
  if ( spacegroup ) ccp4spg_free(spacegroup);

  op1 = (ccp4_symop *) ccp4_utils_malloc(*nsym*sizeof(ccp4_symop));
  for (i = 0; i < *nsym; ++i) {
    for (k = 0; k < 3; ++k) {
      for (l = 0; l < 3; ++l)
	op1[i].rot[k][l] = rsmt[i][l][k];
      op1[i].trn[k] = rsmt[i][3][k];
    }
  }
  /* first, identify spacegroup from supplied symops */
  spacegroup = ccp4_spgrp_reverse_lookup(*nsym,op1);

  ccp4_CtoFString(FTN_STR(nampg),FTN_LEN(nampg),spacegroup->point_group);
  *nsymp = spacegroup->nsymop_prim;

  free(op1);
}

FORTRAN_SUBR ( PGNLAU, pgnlau,
               (const fpstr nampg, int *nlaue, fpstr launam,
                int nampg_len, int launam_len),
               (const fpstr nampg, int *nlaue, fpstr launam),
               (const fpstr nampg, int nampg_len, int *nlaue, 
                fpstr launam, int launam_len))
{
  CSYMLIB_DEBUG(puts("CSYMLIB_F: PGNLAU");)

  if (!spacegroup) {
    printf("PGNLAU: No spacegroup loaded yet! \n");
    return;
  }

  /* We should check we have the right spacegroup! However,
     nampg is typically in the format of the MTZ header record,
     which is different from that recorded in syminfo.lib */

  *nlaue = spacegroup->nlaue;
  ccp4_CtoFString(FTN_STR(launam),FTN_LEN(launam),spacegroup->laue_name);

}

FORTRAN_SUBR ( PATSGP, patsgp,
               (const fpstr spgnam, const fpstr pgname, fpstr patnam, int *lpatsg, 
                int spgnam_len, int pgname_len, int patnam_len),
               (const fpstr spgnam, const fpstr pgname, fpstr patnam, int *lpatsg),
               (const fpstr spgnam, int spgnam_len, const fpstr pgname, 
                int pgname_len, fpstr patnam, int patnam_len, int *lpatsg))
{
  CSYMLIB_DEBUG(puts("CSYMLIB_F: PATSGP");)

  if (!spacegroup) {
    printf("PATSGP: No spacegroup loaded yet! \n");
    return;
  }

  *lpatsg = spacegroup->npatt;
  ccp4_CtoFString(FTN_STR(patnam),FTN_LEN(patnam),spacegroup->patt_name);

}

FORTRAN_SUBR ( ASUSET, asuset,
	       (fpstr spgnam, int *numsgp, fpstr pgname,
                int *msym, float rrsym[192][4][4], int *msymp,
                int *mlaue, ftn_logical *lprint, int spgnam_len, int pgname_len),
	       (fpstr spgnam, int *numsgp, fpstr pgname,
                int *msym, float rrsym[192][4][4], int *msymp,
                int *mlaue, ftn_logical *lprint),
	       (fpstr spgnam, int spgnam_len, int *numsgp, 
                fpstr pgname,int pgname_len,
                int *msym, float rrsym[192][4][4], int *msymp,
                int *mlaue, ftn_logical *lprint))
{
  int i,k,l;
  ccp4_symop *op1;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: ASUSET");)

  /* free any existing spacegroup and start again */
  if ( spacegroup ) ccp4spg_free(spacegroup);

  op1 = (ccp4_symop *) ccp4_utils_malloc(*msym*sizeof(ccp4_symop));
  for (i = 0; i < *msym; ++i) {
    for (k = 0; k < 3; ++k) {
      for (l = 0; l < 3; ++l)
	op1[i].rot[k][l] = rrsym[i][l][k];
      op1[i].trn[k] = rrsym[i][3][k];
    }
  }

  /* Loading by symops ensures spacegroup as desired ordering of symops.
     This is important for ASUGET which may use ISYM stored in MTZ file. */
  spacegroup = ccp4_spgrp_reverse_lookup(*msym,op1);

  /* If we fail to find match for symops, fall back on spacegroup number. */
  if (!spacegroup ) {
    if (*numsgp > 0) {
      spacegroup = ccp4spg_load_by_ccp4_num(*numsgp);
    } else {
      printf("ASUSET: no spacegroup info! \n");
      return;
    }
  }

  ccp4_CtoFString(FTN_STR(pgname),FTN_LEN(pgname),spacegroup->point_group);
  *msymp = spacegroup->nsymop_prim;
  *mlaue = spacegroup->nlaue;

  ccp4spg_print_recip_ops(spacegroup);
}

FORTRAN_SUBR ( ASUPUT, asuput,
               (const int ihkl[3], int jhkl[3], int *isym),
               (const int ihkl[3], int jhkl[3], int *isym),
               (const int ihkl[3], int jhkl[3], int *isym))
{
  int hin,kin,lin,hout,kout,lout;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: ASUPUT");)

  hin = ihkl[0]; kin = ihkl[1]; lin = ihkl[2];

  *isym = ccp4spg_put_in_asu(spacegroup, hin, kin, lin, &hout, &kout, &lout);

  jhkl[0] = hout; jhkl[1] = kout; jhkl[2] = lout; 
}

FORTRAN_SUBR ( ASUGET, asuget,
               (const int ihkl[3], int jhkl[3], const int *isym),
               (const int ihkl[3], int jhkl[3], const int *isym),
               (const int ihkl[3], int jhkl[3], const int *isym))
{
  int hin,kin,lin,hout,kout,lout;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: ASUGET");)

  hin = ihkl[0]; kin = ihkl[1]; lin = ihkl[2];

  ccp4spg_generate_indices(spacegroup, *isym, hin, kin, lin, &hout, &kout, &lout);

  jhkl[0] = hout; jhkl[1] = kout; jhkl[2] = lout; 
}

FORTRAN_SUBR ( ASUPHP, asuphp,
               (const int jhkl[3], const int *lsym, const int *isign, 
                const float *phasin, float *phasout),
               (const int jhkl[3], const int *lsym, const int *isign, 
                const float *phasin, float *phasout),
               (const int jhkl[3], const int *lsym, const int *isign, 
                const float *phasin, float *phasout))
{
  int hin,kin,lin;
  float trans[3];

  CSYMLIB_DEBUG(puts("CSYMLIB_F: ASUPHP");)

  trans[0] = spacegroup->symop[*lsym-1].trn[0];
  trans[1] = spacegroup->symop[*lsym-1].trn[1];
  trans[2] = spacegroup->symop[*lsym-1].trn[2];

  hin = jhkl[0]; kin = jhkl[1]; lin = jhkl[2];

  *phasout = ccp4spg_phase_shift(hin, kin, lin, *phasin, trans, *isign);
}

/** Test whether reflection or it's Friedel mate is in asu.
 * Note that the argument nlaue is ignored, and the most recent
 * spacegroup information used instead.
 * @param ihkl reflection indices
 * @param nlaue (ignored)
 * @return 1 if in asu, -1 if -h -k -l is in asu, 0 otherwise
 */
FORTRAN_FUN (int, INASU, inasu,
	       (const int ihkl[3], const int *nlaue),
               (const int ihkl[3], const int *nlaue),
               (const int ihkl[3], const int *nlaue))
{
  int ih, ik, il;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: INASU");)

  if (!spacegroup) {
    printf("INASU: No spacegroup loaded yet! \n");
    return 999;
  }

  ih = ihkl[0];
  ik = ihkl[1];
  il = ihkl[2];

  return ccp4spg_is_in_pm_asu(spacegroup,ih,ik,il);
}

FORTRAN_SUBR ( PRTRSM, prtrsm,
	       (const fpstr pgname, const int *nsymp, 
                const float rsymiv[192][4][4], int pgname_len),
	       (const fpstr pgname, const int *nsymp, 
                const float rsymiv[192][4][4]),
	       (const fpstr pgname, int pgname_len, const int *nsymp, 
                const float rsymiv[192][4][4]))
{

  CSYMLIB_DEBUG(puts("CSYMLIB_F: PRTRSM");)

  ccp4spg_print_recip_ops(spacegroup);

}

void ccp4spg_register_by_ccp4_num(int numspg) {

   spacegroup = ccp4spg_load_by_ccp4_num(numspg);

}

FORTRAN_SUBR ( MSYMLB3, msymlb3,
	       (const int *ist, int *lspgrp, fpstr namspg_cif,
		fpstr namspg_cifs, fpstr nampg, int *nsymp, int *nsym, 
                float rlsymmmatrx[192][4][4], int namspg_cif_len,
                int namspg_cifs_len, int nampg_len),
	       (const int *ist, int *lspgrp, fpstr namspg_cif,
		fpstr namspg_cifs, fpstr nampg, int *nsymp, int *nsym, 
                float rlsymmmatrx[192][4][4]),
	       (const int *ist, int *lspgrp, fpstr namspg_cif, int namspg_cif_len,
		fpstr namspg_cifs, int namspg_cifs_len, fpstr nampg, int nampg_len, 
                int *nsymp, int *nsym, float rlsymmmatrx[192][4][4]))
{
  int i,j,k;
  char *temp_name;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: MSYMLB3");)

  /* search by number first
     we assume that lspgrp is in CCP4 convention */
  if (*lspgrp > 0) {

    /* free any existing spacegroup and start again */
    if ( spacegroup ) ccp4spg_free(spacegroup);

    spacegroup = ccp4spg_load_by_ccp4_num(*lspgrp);

  } else {

    /* else try to search by name */
    temp_name = ccp4_FtoCString(FTN_STR(namspg_cif), FTN_LEN(namspg_cif));
    if (strlen(temp_name)) {

      /* free any existing spacegroup and start again */
      if ( spacegroup ) ccp4spg_free(spacegroup);

      spacegroup = ccp4spg_load_by_spgname(temp_name);

    }
    free (temp_name);
  }

  if (spacegroup) {
    *lspgrp = spacegroup->spg_ccp4_num;
    ccp4_CtoFString(FTN_STR(namspg_cif),FTN_LEN(namspg_cif),spacegroup->symbol_xHM);
    ccp4_CtoFString(FTN_STR(namspg_cifs),FTN_LEN(namspg_cifs),spacegroup->symbol_old);
    ccp4_CtoFString(FTN_STR(nampg),FTN_LEN(nampg),spacegroup->point_group);
    *nsymp = spacegroup->nsymop_prim;
    *nsym = spacegroup->nsymop;
    for (i = 0; i < *nsym; ++i) {
      for (j = 0; j < 3; ++j) {
        for (k = 0; k < 3; ++k) 
          rlsymmmatrx[i][k][j] = spacegroup->symop[i].rot[j][k];
        rlsymmmatrx[i][3][j] = spacegroup->symop[i].trn[j];
        rlsymmmatrx[i][j][3] = 0.0;
      }
      rlsymmmatrx[i][3][3] = 1.0;
    }
  }
}

FORTRAN_SUBR ( MSYMLB, msymlb,
	       (const int *ist, int *lspgrp, fpstr namspg_cif,
		fpstr nampg, int *nsymp, int *nsym, 
                float rlsymmmatrx[192][4][4], int namspg_cif_len,
                int nampg_len),
	       (const int *ist, int *lspgrp, fpstr namspg_cif,
		fpstr nampg, int *nsymp, int *nsym, 
                float rlsymmmatrx[192][4][4]),
	       (const int *ist, int *lspgrp, fpstr namspg_cif, int namspg_cif_len,
		fpstr nampg, int nampg_len, 
                int *nsymp, int *nsym, float rlsymmmatrx[192][4][4]))
{
  char *namspg_cifs;
  int namspg_cifs_len=0;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: MSYMLB");)

  FORTRAN_CALL ( MSYMLB3, msymlb3,
	       (ist, lspgrp, namspg_cif, namspg_cifs, nampg, nsymp, nsym, 
                rlsymmmatrx, namspg_cif_len, namspg_cifs_len, nampg_len),
	       (ist, lspgrp, namspg_cif, namspg_cifs, nampg, nsymp, nsym, 
                rlsymmmatrx),
	       (ist, lspgrp, namspg_cif, namspg_cif_len, namspg_cifs, 
                namspg_cifs_len, nampg, nampg_len, nsymp, nsym, rlsymmmatrx));

}

FORTRAN_SUBR ( MSYMLB2, msymlb2,
	       (const int *ist, int *lspgrp, fpstr namspg_cif,
		fpstr nampg, int *nsymp, int *nsym, 
                float rlsymmmatrx[192][4][4], int namspg_cif_len,
                int nampg_len),
	       (const int *ist, int *lspgrp, fpstr namspg_cif,
		fpstr nampg, int *nsymp, int *nsym, 
                float rlsymmmatrx[192][4][4]),
	       (const int *ist, int *lspgrp, fpstr namspg_cif, int namspg_cif_len,
		fpstr nampg, int nampg_len, 
                int *nsymp, int *nsym, float rlsymmmatrx[192][4][4]))
{
  char *namspg_cifs;
  int namspg_cifs_len=0;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: MSYMLB2");)

  FORTRAN_CALL ( MSYMLB3, msymlb3,
	       (ist, lspgrp, namspg_cif, namspg_cifs, nampg, nsymp, nsym, 
                rlsymmmatrx, namspg_cif_len, namspg_cifs_len, nampg_len),
	       (ist, lspgrp, namspg_cif, namspg_cifs, nampg, nsymp, nsym, 
                rlsymmmatrx),
	       (ist, lspgrp, namspg_cif, namspg_cif_len, namspg_cifs, 
                namspg_cifs_len, nampg, nampg_len, nsymp, nsym, rlsymmmatrx));

}

FORTRAN_SUBR ( MSYGET, msyget,
	       (const int *ist, int *lspgrp, int *nsym, 
                float rlsymmmatrx[192][4][4]),
	       (const int *ist, int *lspgrp, int *nsym, 
                float rlsymmmatrx[192][4][4]),
	       (const int *ist, int *lspgrp, int *nsym, 
                float rlsymmmatrx[192][4][4]))
{
  char *namspg_cif, *namspg_cifs, *nampg;
  int namspg_cif_len=0, namspg_cifs_len=0, nampg_len=0, nsymp=0;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: MSYGET");)

  FORTRAN_CALL ( MSYMLB3, msymlb3,
	       (ist, lspgrp, namspg_cif, namspg_cifs, nampg, &nsymp, nsym, 
                rlsymmmatrx, namspg_cif_len, namspg_cifs_len, nampg_len),
	       (ist, lspgrp, namspg_cif, namspg_cifs, nampg, &nsymp, nsym, 
                rlsymmmatrx),
	       (ist, lspgrp, namspg_cif, namspg_cif_len, namspg_cifs, 
                namspg_cifs_len, nampg, nampg_len, &nsymp, nsym, rlsymmmatrx));

}

/* Epsilon zones currently set up in ccp4spg_load_spacegroup
   If these are not available, use lookup by symops */

FORTRAN_SUBR ( EPSLN, epsln,
	       (const int *nsm, const int *nsmp, const float rsm[192][4][4],
		const int *iprint),
	       (const int *nsm, const int *nsmp, const float rsm[192][4][4],
		const int *iprint),
	       (const int *nsm, const int *nsmp, const float rsm[192][4][4],
		const int *iprint))
{
  int i,k,l;
  ccp4_symop *op1;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: EPSLN");)

  if (!spacegroup) {
    /* identify spacegroup from supplied symops */
    op1 = (ccp4_symop *) ccp4_utils_malloc(*nsm*sizeof(ccp4_symop));
    for (i = 0; i < *nsm; ++i) {
      for (k = 0; k < 3; ++k) {
        for (l = 0; l < 3; ++l) {
	  op1[i].rot[k][l] = rsm[i][l][k];
	}
        op1[i].trn[k] = rsm[i][k][3];
      }
    }
    spacegroup = ccp4_spgrp_reverse_lookup(*nsm,op1);
  }
}

FORTRAN_SUBR ( EPSLON, epslon,
	       (const int ih[3], float *epsi, int *isysab),
	       (const int ih[3], float *epsi, int *isysab),
	       (const int ih[3], float *epsi, int *isysab))
{
  int h,k,l;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: EPSLON");)

  if (!spacegroup) {
    printf("EPSLON: No spacegroup loaded yet! \n");
    return;
  }

  h = ih[0]; k = ih[1]; l = ih[2]; 

  *epsi = (float) ccp4spg_get_multiplicity(spacegroup, h, k, l);
  *isysab = ccp4spg_is_sysabs(spacegroup, h, k, l);
}

FORTRAN_SUBR ( SYSAB, sysab,
	       (const int in[3], int *isysab),
	       (const int in[3], int *isysab),
	       (const int in[3], int *isysab))
{
  int h,k,l;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: SYSAB");)

  h = in[0]; k = in[1]; l = in[2]; 

  *isysab = ccp4spg_is_sysabs(spacegroup, h, k, l);
}

/** Set up centric zones based on symmetry operators.
 * Convention: translations are in rsm[isym][3][*]
 * @param nsm number of symmetry matrices passed.
 * @param rsm symmetry matrices.
 * @param iprint print flag.
 */
FORTRAN_SUBR ( CENTRIC, centric,
	       (const int *nsm, const float rsm[192][4][4],
		const int *iprint),
	       (const int *nsm, const float rsm[192][4][4],
		const int *iprint),
	       (const int *nsm, const float rsm[192][4][4],
		const int *iprint))
{
  int i,k,l;
  ccp4_symop *op1;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: CENTRIC");)

  if (!spacegroup) {
    /* identify spacegroup from supplied symops */
    op1 = (ccp4_symop *) ccp4_utils_malloc(*nsm*sizeof(ccp4_symop));
    for (i = 0; i < *nsm; ++i) {
      for (k = 0; k < 3; ++k) {
        for (l = 0; l < 3; ++l) {
	  op1[i].rot[k][l] = rsm[i][l][k];
	}
        op1[i].trn[k] = rsm[i][3][k];
      }
    }
    spacegroup = ccp4_spgrp_reverse_lookup(*nsm,op1);
  }
}

FORTRAN_SUBR ( CENTR, centr,
	       (const int ih[3], int *ic),
	       (const int ih[3], int *ic),
	       (const int ih[3], int *ic))
{
  int h,k,l;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: CENTR");)

  h = ih[0]; k = ih[1]; l = ih[2]; 

  *ic = ccp4spg_is_centric(spacegroup, h, k, l);

}

FORTRAN_SUBR ( CENTPHASE, centphase,
	       (const int ih[3], float *cenphs),
	       (const int ih[3], float *cenphs),
	       (const int ih[3], float *cenphs))
{
  int h,k,l;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: CENTPHASE");)

  h = ih[0]; k = ih[1]; l = ih[2]; 

  if (! ccp4spg_is_centric(spacegroup, h, k, l) ) {
    printf("CENTPHASE: This is not a centric reflection!\n");
    return;
  }

  *cenphs = ccp4spg_centric_phase(spacegroup, h, k, l);
}

/* Returns map limits of a.s.u. in fractional units.
   These are rounded up or down to mimic <= or < respectively.
   In fact, these limits may be larger than 1 a.s.u. but always
   have one corner at the origin */
FORTRAN_SUBR ( SETLIM, setlim,
	       (const int *lspgrp, float xyzlim[3][2]),
	       (const int *lspgrp, float xyzlim[3][2]),
	       (const int *lspgrp, float xyzlim[3][2]))
{ 
  CCP4SPG *tmp_spacegroup;      

  CSYMLIB_DEBUG(puts("CSYMLIB_F: SETLIM");)

  if (!spacegroup || spacegroup->spg_ccp4_num != *lspgrp) {
    /* load spacegroup if necessary */
    /* spacegroup only temporary, as setlim is not expected to
       interact with other calls */
    tmp_spacegroup = ccp4spg_load_by_ccp4_num(*lspgrp);
    xyzlim[0][1] = tmp_spacegroup->mapasu_ccp4[0];
    xyzlim[1][1] = tmp_spacegroup->mapasu_ccp4[1];
    xyzlim[2][1] = tmp_spacegroup->mapasu_ccp4[2];
    free(tmp_spacegroup); 
  } else {
    xyzlim[0][1] = spacegroup->mapasu_ccp4[0];
    xyzlim[1][1] = spacegroup->mapasu_ccp4[1];
    xyzlim[2][1] = spacegroup->mapasu_ccp4[2];
  }
  xyzlim[0][0] = 0.0;
  xyzlim[1][0] = 0.0;
  xyzlim[2][0] = 0.0;
}

FORTRAN_SUBR ( SETGRD, setgrd,
	       (const int *nlaue, const float *sample, const int *nxmin,
                const int *nymin, const int *nzmin, int *nx, int *ny, int *nz),
	       (const int *nlaue, const float *sample, const int *nxmin,
                const int *nymin, const int *nzmin, int *nx, int *ny, int *nz),
	       (const int *nlaue, const float *sample, const int *nxmin,
                const int *nymin, const int *nzmin, int *nx, int *ny, int *nz))
{

  set_fft_grid(spacegroup, *nxmin, *nymin, *nzmin, *sample, nx, ny, nz);

}

FORTRAN_SUBR ( CALC_ORIG_PS, calc_orig_ps,
	       (fpstr namspg_cif, int *nsym, float rsym[192][4][4], int *norig,
		float orig[96][3], ftn_logical *lpaxisx, ftn_logical *lpaxisy,
		ftn_logical *lpaxisz, int namspg_cif_len),
	       (fpstr namspg_cif, int *nsym, float rsym[192][4][4], int *norig,
		float orig[96][3], ftn_logical *lpaxisx, ftn_logical *lpaxisy,
		ftn_logical *lpaxisz, int namspg_cif_len),
	       (fpstr namspg_cif, int namspg_cif_len, int *nsym, float rsym[192][4][4], 
                int *norig, float orig[96][3], ftn_logical *lpaxisx, 
                ftn_logical *lpaxisy, ftn_logical *lpaxisz))
{
  int i,j,k;
  int polarx, polary, polarz;
  float crsym[192][4][4];

  CSYMLIB_DEBUG(puts("CSYMLIB_F: CALC_ORIG_PS");)

  for (i = 0; i < *nsym; ++i) {
    for (j = 0; j < 4; ++j) {
      for (k = 0; k < 4; ++k) {
        crsym[i][k][j] = rsym[i][j][k];
      }
    }
  }
  *norig = ccp4spg_generate_origins(namspg_cif, *nsym, crsym, orig, polarx, polary, polarz, 1);
  *lpaxisx = polarx ? FORTRAN_LOGICAL_TRUE : FORTRAN_LOGICAL_FALSE;
  *lpaxisy = polary ? FORTRAN_LOGICAL_TRUE : FORTRAN_LOGICAL_FALSE;
  *lpaxisz = polarz ? FORTRAN_LOGICAL_TRUE : FORTRAN_LOGICAL_FALSE;

}

static double coefhkl[6];

FORTRAN_SUBR ( SETRSL, setrsl,
	       (const float *a, const float *b, const float *c,
                const float *alpha, const float *beta, const float *gamma),
	       (const float *a, const float *b, const float *c,
                const float *alpha, const float *beta, const float *gamma),
	       (const float *a, const float *b, const float *c,
                const float *alpha, const float *beta, const float *gamma))
{
  float cell[6];

  CSYMLIB_DEBUG(puts("CSYMLIB_F: SETRSL");)

  cell[0] = *a;
  cell[1] = *b;
  cell[2] = *c;
  cell[3] = *alpha;
  cell[4] = *beta;
  cell[5] = *gamma;

  MtzHklcoeffs(cell, coefhkl);

}

FORTRAN_FUN (float, STHLSQ, sthlsq,
	     (const int *ih, const int *ik, const int *il),
	     (const int *ih, const int *ik, const int *il),
	     (const int *ih, const int *ik, const int *il))
{
  int in[3];

  CSYMLIB_DEBUG(puts("CSYMLIB_F: STHLSQ");)

  in[0] = *ih;
  in[1] = *ik;
  in[2] = *il;

  return (0.25*MtzInd2reso(in, coefhkl));
}

FORTRAN_FUN (float, STS3R4, sts3r4,
	     (const int *ih, const int *ik, const int *il),
	     (const int *ih, const int *ik, const int *il),
	     (const int *ih, const int *ik, const int *il))
{
  int in[3];

  CSYMLIB_DEBUG(puts("CSYMLIB_F: STS3R4");)

  in[0] = *ih;
  in[1] = *ik;
  in[2] = *il;

  return (0.25*MtzInd2reso(in, coefhkl));
}

/* straight translation, needs to be done properly, used in phistats */

FORTRAN_SUBR ( HANDCHANGE, handchange,
	       (const int *lspgrp, float *cx, float *cy, float *cz),
	       (const int *lspgrp, float *cx, float *cy, float *cz),
	       (const int *lspgrp, float *cx, float *cy, float *cz))
{
  CSYMLIB_DEBUG(puts("CSYMLIB_F: HANDCHANGE");)

  switch (*lspgrp) {
  case 80:
    *cx=0.0;
    *cy=0.5;
    *cz=0.0;
    break;
  case 98:
    *cx=0.0;
    *cy=0.5;
    *cz=0.25;
    break;
  case 210:
    *cx=0.75;
    *cy=0.25;
    *cz=0.75;
    break;
  case 214:
    *cx=0.25;
    *cy=0.25;
    *cz=0.25;
    break;
  }

}
