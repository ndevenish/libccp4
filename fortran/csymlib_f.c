/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/

/** @page csym_f_page Fortran API to CSYM 
 *
 *  @section csym_f_file_list File list

<ul>
<li>csymlib_f.c
</ul>
 *
 *  @section csym_f_overview Overview

This library consists of a set of wrappers to the CSYM library
giving the same API as the original symlib.f For details of the
API, see the original <a href="../symlib.html">documentation</a>.
This document covers some peculiarities of the C implementation.

 *   @section csym_f_multiple Multiple Spacegroups

The set of Fortran calls which mimic the original symlib.f assume
you are working within a single spacegroup. All calls access the
same spacegroup data structure, in analogy with the COMMON blocks
of symlib.f For cases where you wish to work with multiple
spacegroups (e.g. in the program <a href="../reindex.html">REINDEX</a>,
a different set of calls is provided (the names of which generally
start with "CCP4SPG_F_"). These identify the spacegroup of interest
via an index "sindx" (by analogy with the "mindx" of mtzlib).

 */
 
/** @file csymlib_f.c
 *
 *  @brief Fortran API for symmetry information.
 *
 *  @author Martyn Winn 
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
#include "csymlib.h"
#include "cmtzlib.h"
#include "cvecmat.h"
static char rcsid[] = "$Id$";

#define MSPAC 4
#define MAXSYM 192

static CCP4SPG *spacegroup = NULL;          /* allow more than one spacegroup ?? */
static CCP4SPG *spacegrp[MSPAC] = {NULL};   /* cf. Eugene's channel for rwbrook */

void ccp4spg_mem_tidy(void) {

  CSYMLIB_DEBUG(puts("CSYMLIB_F: ccp4spg_mem_tidy");)

  /* free any existing spacegroup */
  if ( spacegroup ) ccp4spg_free(&spacegroup);

}

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
  nsym = symop_to_mat4( temp_name, temp_name+strlen(temp_name), tmp_rot[0]);
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
  ns = symop_to_mat4( temp_name, temp_name+strlen(temp_name), tmp_rot[0]);
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
    mat4_to_symop(temp_symch,temp_symch+79,rsym);
    /* mat4_to_symop fills temp_symch with spaces */
    /* ccp4_CtoFString will perform strlen(temp_symch) */
    temp_symch[79] = '\0';
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
    /* mat4_to_symop will pad with spaces, but ccp4_CtoFString needs 
     * null-terminated 
     */
    temp_symch[79] = '\0';
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
  if ( spacegroup ) ccp4spg_free(&spacegroup);

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


/** Return Laue number and name for current spacegroup. 
 * @param nampg Point group name (unused in this implementation)
 * @param nlaue Laue number
 * @param launam Laue name
 */
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

/** Return Laue number and name for a spacegroup onto index "sindx". 
 * @param sindx index of this spacegroup.
 * @param nlaue Laue number
 * @param launam Laue name
 */
FORTRAN_SUBR ( CCP4SPG_F_GET_LAUE, ccp4spg_f_get_laue,
               (const int *sindx, int *nlaue, fpstr launam, int launam_len),
               (const int *sindx, int *nlaue, fpstr launam),
               (const int *sindx, int *nlaue, fpstr launam, int launam_len))
{
  CSYMLIB_DEBUG(puts("CSYMLIB_F: CCP4SPG_F_GET_LAUE");)

  if (*sindx <= 0 || *sindx > MSPAC) {
    printf("Error in CCP4SPG_F_GET_LAUE: sindx %d out of range!\n",*sindx);
    return;
  }

  if ( ! spacegrp[*sindx-1] ) {
    printf("CCP4SPG_F_GET_LAUE: No spacegroup loaded on channel %d ! \n",*sindx);
    return;
  }

  *nlaue = spacegrp[*sindx-1]->nlaue;
  ccp4_CtoFString(FTN_STR(launam),FTN_LEN(launam),spacegrp[*sindx-1]->laue_name);

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

/** Set spacegroup for subsequent calls to ASUPUT, ASUGET, ASUSYM and ASUPHP.
 * @param spgnam spacegroup name
 * @param numsgp spacegroup number
 * @param pgname On return, point group name
 * @param msym number of symmetry matrices passed.
 * @param rrsym symmetry matrices (preferred method of identifying spacegroup).
 * @param msymp On return, number of primitive symmetry operators
 * @param mlaue On return, number of Laue group.
 * @param lprint If true, print symmetry information.
 */
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
  if ( spacegroup ) ccp4spg_free(&spacegroup);

  op1 = (ccp4_symop *) ccp4_utils_malloc(*msym*sizeof(ccp4_symop));
  for (i = 0; i < *msym; ++i) {
    for (k = 0; k < 3; ++k) {
      for (l = 0; l < 3; ++l)
	op1[i].rot[k][l] = rrsym[i][l][k];
      op1[i].trn[k] = rrsym[i][3][k];
    }
  }

  /* Loading by symops ensures spacegroup has desired ordering of symops.
     This is important for ASUGET which may use ISYM stored in MTZ file. */
  spacegroup = ccp4_spgrp_reverse_lookup(*msym,op1);

  /* If we fail to find match for symops, fall back on spacegroup number. */
  if (!spacegroup ) {
    if (*numsgp > 0) {
      if ( ! (spacegroup = ccp4spg_load_by_ccp4_num(*numsgp)) ) {
        printf("ASUSET: failed to load spacegroup info from SYMINFO! \n");
        ccperror(1,"Fatal error in ASUSET.");
        return;
      }
    } else {
      printf("ASUSET: no spacegroup info! \n");
      ccperror(1,"Fatal error in ASUSET.");
      return;
    }
  }

  ccp4_CtoFString(FTN_STR(pgname),FTN_LEN(pgname),spacegroup->point_group);
  *msymp = spacegroup->nsymop_prim;
  *mlaue = spacegroup->nlaue;

  if (*lprint == FORTRAN_LOGICAL_TRUE) {
    printf("Reciprocal space symmetry: \n");
    printf("Space group: \"%s\" Point group: \"%s\" Laue group: \"%s\" \n",
       spacegroup->symbol_xHM,spacegroup->point_group,spacegroup->laue_name); 
    printf("Asymmetric unit: \"%s\" \n",spacegroup->asu_descr); 
    ccp4spg_print_recip_ops(spacegroup);
  }
}

/** Return symmetry operators and inverses, set up by ASUSET.
 * @param rassym symmetry operators.
 * @param rinsym inverse symmetry operators.
 * @param nisym number of symmetry operators returned.
 */
FORTRAN_SUBR ( ASUSYM, asusym,
	       (float rassym[384][4][4], float rinsym[384][4][4], int *nisym),
	       (float rassym[384][4][4], float rinsym[384][4][4], int *nisym),
	       (float rassym[384][4][4], float rinsym[384][4][4], int *nisym))
{
  int i,j,k,l;
  float sgn;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: ASUSYM");)

  if (spacegroup) {
    *nisym = 0;
    for (i = 0; i < spacegroup->nsymop_prim; ++i) {
      sgn = +1.0;
      for (j = 0; j < 2; ++j) {
        for (k = 0; k < 3; ++k) {
          for (l = 0; l < 3; ++l) {
            rassym[*nisym][l][k] = sgn * spacegroup->symop[i].rot[k][l];
            rinsym[*nisym][l][k] = sgn * spacegroup->invsymop[i].rot[k][l];
          }
          rassym[*nisym][3][k] = sgn * spacegroup->symop[i].trn[k];
          rinsym[*nisym][3][k] = sgn * spacegroup->invsymop[i].trn[k];
          rassym[*nisym][k][3] = 0.0;
          rinsym[*nisym][k][3] = 0.0;
        }
        rassym[*nisym][3][3] = 1.0;
        rinsym[*nisym][3][3] = 1.0;
        ++(*nisym);
        sgn = -1.0;
      }
    }
  } else {
    printf("ASUSYM: No spacegroup loaded yet! \n");
  }

}

/** Put reflection in asymmetric unit, as set up by ASUSET.
 * @param ihkl input indices.
 * @param jhkl output indices.
 * @param isym symmetry operation applied (ISYM number).
 */
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

/** Get the original indices jkhl from input indices ihkl generated
 * under symmetry operation isym.
 * @param ihkl input indices.
 * @param jhkl output indices (recovered original indices).
 * @param isym symmetry operation to be applied (ISYM number).
 */
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

/** Generate phase of symmetry equivalent JHKL from that of IHKL.
 * @param jhkl indices hkl generated in ASUPUT
 * @param lsym symmetry number for generating JHKL
 * @param isign 1   for I+ , -1   for I-
 * @param phasin phase for reflection IHKL
 * @param phasout phase for reflection JHKL
 */
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

/** Loads a spacegroup onto index "sindx". The spacegroup is
 * identified by the spacegroup name.
 * @param sindx index of this spacegroup.
 * @param namspg spacegroup name.
 */
FORTRAN_SUBR ( CCP4SPG_F_LOAD_BY_NAME, ccp4spg_f_load_by_name,
	       (const int *sindx, fpstr namspg, int namspg_len),
	       (const int *sindx, fpstr namspg),
	       (const int *sindx, fpstr namspg, int namspg_len))
{ 
  char *temp_name;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: CCP4SPG_F_LOAD_BY_NAME");)

  if (*sindx <= 0 || *sindx > MSPAC) {
    printf("Error in CCP4SPG_F_LOAD_BY_NAME: sindx %d out of range!\n",*sindx);
    return;
  }

  /* free any existing spacegroup and start again */
  if ( spacegrp[*sindx-1] ) ccp4spg_free(&spacegrp[*sindx-1]);

  temp_name = ccp4_FtoCString(FTN_STR(namspg), FTN_LEN(namspg));
  if (strlen(temp_name)) {
    spacegrp[*sindx-1] = ccp4spg_load_by_ccp4_spgname(temp_name);
  }
  free (temp_name);
}

/** Loads a spacegroup onto index "sindx". The spacegroup is
 * identified by the set of symmetry matrices.
 * @param sindx index of this spacegroup.
 * @param msym number of symmetry matrices passed.
 * @param rrsym symmetry matrices.
 */
FORTRAN_SUBR ( CCP4SPG_F_LOAD_BY_OPS, ccp4spg_f_load_by_ops,
	       (const int *sindx, int *msym, float rrsym[192][4][4]),
	       (const int *sindx, int *msym, float rrsym[192][4][4]),
	       (const int *sindx, int *msym, float rrsym[192][4][4]))
{
  int i,k,l;
  ccp4_symop *op1;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: CCP4SPG_F_LOAD_BY_OPS");)

  if (*sindx <= 0 || *sindx > MSPAC) {
    printf("Error in CCP4SPG_F_LOAD_BY_OPS: sindx %d out of range!\n",*sindx);
    return;
  }

  /* free any existing spacegroup and start again */
  if ( spacegrp[*sindx-1] ) ccp4spg_free(&spacegrp[*sindx-1]);

  op1 = (ccp4_symop *) ccp4_utils_malloc(*msym*sizeof(ccp4_symop));
  for (i = 0; i < *msym; ++i) {
    for (k = 0; k < 3; ++k) {
      for (l = 0; l < 3; ++l)
	op1[i].rot[k][l] = rrsym[i][l][k];
      op1[i].trn[k] = rrsym[i][3][k];
    }
  }

  /* Loading by symops ensures spacegroup has desired ordering of symops.
     This is important for ASUGET which may use ISYM stored in MTZ file. */
  spacegrp[*sindx-1] = ccp4_spgrp_reverse_lookup(*msym,op1);

  if (!spacegroup ) {
    printf("CCP4SPG_F_LOAD_BY_OPS: no spacegroup info! \n");
    ccperror(1,"Fatal error in CCP4SPG_F_LOAD_BY_OPS.");
    return;
  }

  printf("Reciprocal space symmetry: \n");
  printf("Space group: \"%s\" Point group: \"%s\" Laue group: \"%s\" \n",
       spacegrp[*sindx-1]->symbol_xHM,spacegrp[*sindx-1]->point_group,
       spacegrp[*sindx-1]->laue_name); 
  printf("Asymmetric unit: \"%s\" \n",spacegrp[*sindx-1]->asu_descr); 
  ccp4spg_print_recip_ops(spacegrp[*sindx-1]);
}

/** Put reflection in asymmetric unit of spacegroup on index sindx.
 * @param sindx index of this spacegroup.
 * @param ihkl input indices.
 * @param jhkl output indices.
 * @param isym symmetry operation applied (ISYM number).
 */
FORTRAN_SUBR ( CCP4SPG_F_ASUPUT, ccp4spg_f_asuput,
               (const int *sindx, const int ihkl[3], int jhkl[3], int *isym),
               (const int *sindx, const int ihkl[3], int jhkl[3], int *isym),
               (const int *sindx, const int ihkl[3], int jhkl[3], int *isym))
{
  int hin,kin,lin,hout,kout,lout;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: CCP4SPG_F_ASUPUT");)

  if (*sindx <= 0 || *sindx > MSPAC) {
    printf("Error in CCP4SPG_F_ASUPUT: sindx %d out of range!\n",*sindx);
    return;
  }

  if ( ! spacegrp[*sindx-1] ) {
    printf("CCP4SPG_F_ASUPUT: No spacegroup loaded on channel %d ! \n",*sindx);
    return;
  }

  hin = ihkl[0]; kin = ihkl[1]; lin = ihkl[2];

  *isym = ccp4spg_put_in_asu(spacegrp[*sindx-1], hin, kin, lin, &hout, &kout, &lout);

  jhkl[0] = hout; jhkl[1] = kout; jhkl[2] = lout; 
}

/** Test whether reflection or it's Friedel mate is in asu.
 * The argument nlaue is checked against the value for the current
 * spacegroup: if it differs then spacegroup->nlaue is updated temporarily.
 * @param ihkl reflection indices.
 * @param nlaue Laue group number.
 * @return 1 if in asu, -1 if -h -k -l is in asu, 0 otherwise
 */
FORTRAN_FUN (int, INASU, inasu,
	       (const int ihkl[3], const int *nlaue),
               (const int ihkl[3], const int *nlaue),
               (const int ihkl[3], const int *nlaue))
{
  int ih, ik, il, nlaue_save = -1, retval;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: INASU");)

  if (!spacegroup) {
    printf("INASU: No spacegroup loaded yet! \n");
    return 999;
  }

  if (spacegroup->nlaue != *nlaue) {
    /* The requested Laue number is different to that for the
       current spacegroup
       Save the current Laue code and load the data for the requested code */
    nlaue_save = spacegroup->nlaue;
    if (ccp4spg_load_laue(spacegroup,*nlaue)) {
      printf("INASU: unrecognised CCP4 Laue code\n");
      return 999;
    }
  }
  ih = ihkl[0];
  ik = ihkl[1];
  il = ihkl[2];
  retval = ccp4spg_is_in_pm_asu(spacegroup,ih,ik,il);
  if (nlaue_save > -1) {
    /* Restore previous settings */
    ccp4spg_load_laue(spacegroup,nlaue_save);
  }

  return retval;
}

/** Test whether reflection or it's Friedel mate is in the asymmetric
 * unit of the spacegroup on index "sindx".
 * @param sindx index of this spacegroup.
 * @param ihkl reflection indices.
 * @return 1 if in asu, -1 if -h -k -l is in asu, 0 otherwise
 */
FORTRAN_FUN (int, CCP4SPG_F_INASU, ccp4spg_f_inasu,
	       (const int *sindx, const int ihkl[3]),
               (const int *sindx, const int ihkl[3]),
               (const int *sindx, const int ihkl[3]))
{
  int ih, ik, il, retval;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: CCP4SPG_F_INASU");)

  if (*sindx <= 0 || *sindx > MSPAC) {
    printf("Error in CCP4SPG_F_INASU: sindx %d out of range!\n",*sindx);
    return 999;
  }

  if ( ! spacegrp[*sindx-1] ) {
    printf("CCP4SPG_F_INASU: No spacegroup loaded on channel %d ! \n",*sindx);
    return 999;
  }
  ih = ihkl[0];
  ik = ihkl[1];
  il = ihkl[2];
  retval = ccp4spg_is_in_pm_asu(spacegrp[*sindx-1],ih,ik,il);

  return retval;
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

  CSYMLIB_DEBUG(puts("CSYMLIB_F: ccp4spg_register_by_ccp4_num");)

   spacegroup = ccp4spg_load_by_ccp4_num(numspg);

}

/** Fortran wrapper for ccp4spg_load_by_* functions.
 * @param ist Obsolete parameter.
 * @param lspgrp Spacegroup number in CCP4 convention. If set on
 * entry, used to search for spacegroup. Returned value is that found.
 * @param namspg_cif Spacegroup name. If set on
 * entry, used to search for spacegroup. Returned value is the full
 * extended Hermann Mauguin symbol.
 * @param namspg_cifs On output, contains the spacegroup name without
 * any spaces.
 * @param nampg On output, the point group name.
 * @param nsymp On output, the number of primitive symmetry operators.
 * @param nsym On output, the total number of symmetry operators.
 * @param rlsymmmatrx On output, the symmetry operators.
 */
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
  char *temp_name, *shortname=NULL;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: MSYMLB3");)

  /* search by number first
     we assume that lspgrp is in CCP4 convention */
  if (*lspgrp > 0) {

    /* free any existing spacegroup and start again */
    if ( spacegroup ) ccp4spg_free(&spacegroup);

    spacegroup = ccp4spg_load_by_ccp4_num(*lspgrp);

  } else {

    /* else try to search by name */
    temp_name = ccp4_FtoCString(FTN_STR(namspg_cif), FTN_LEN(namspg_cif));
    if (strlen(temp_name)) {

      /* free any existing spacegroup and start again */
      if ( spacegroup ) ccp4spg_free(&spacegroup);

      spacegroup = ccp4spg_load_by_ccp4_spgname(temp_name);

    }
    free (temp_name);
  }

  if (spacegroup) {
    if (spacegroup->spg_ccp4_num > 0) {
      *lspgrp = spacegroup->spg_ccp4_num;
    } else {
      *lspgrp = spacegroup->spg_num;
    }
    ccp4_CtoFString(FTN_STR(namspg_cif),FTN_LEN(namspg_cif),spacegroup->symbol_xHM);
    if (spacegroup->symbol_old) {
     if (strlen(spacegroup->symbol_old) > 0) {
      shortname = (char *) ccp4_utils_malloc(strlen(spacegroup->symbol_old)*sizeof(char));
      ccp4spg_to_shortname(shortname,spacegroup->symbol_old);
     }
    } 
    if (!shortname && spacegroup->symbol_xHM) {
     if (strlen(spacegroup->symbol_xHM) > 0) {
      shortname = (char *) ccp4_utils_malloc(strlen(spacegroup->symbol_xHM)*sizeof(char));
      ccp4spg_to_shortname(shortname,spacegroup->symbol_xHM);
     }
    }
    ccp4_CtoFString(FTN_STR(namspg_cifs),FTN_LEN(namspg_cifs),shortname);
    free(shortname);
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
  char namspg_cifs;
  int namspg_cifs_len=0;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: MSYMLB");)

  FORTRAN_CALL ( MSYMLB3, msymlb3,
	       (ist, lspgrp, namspg_cif, &namspg_cifs, nampg, nsymp, nsym, 
                rlsymmmatrx, namspg_cif_len, namspg_cifs_len, nampg_len),
	       (ist, lspgrp, namspg_cif, &namspg_cifs, nampg, nsymp, nsym, 
                rlsymmmatrx),
	       (ist, lspgrp, namspg_cif, namspg_cif_len, &namspg_cifs, 
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
  char namspg_cifs;
  int namspg_cifs_len=0;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: MSYMLB2");)

  FORTRAN_CALL ( MSYMLB3, msymlb3,
	       (ist, lspgrp, namspg_cif, &namspg_cifs, nampg, nsymp, nsym, 
                rlsymmmatrx, namspg_cif_len, namspg_cifs_len, nampg_len),
	       (ist, lspgrp, namspg_cif, &namspg_cifs, nampg, nsymp, nsym, 
                rlsymmmatrx),
	       (ist, lspgrp, namspg_cif, namspg_cif_len, &namspg_cifs, 
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
  char namspg_cif, namspg_cifs, nampg;
  int namspg_cif_len=0, namspg_cifs_len=0, nampg_len=0, nsymp=0;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: MSYGET");)

  FORTRAN_CALL ( MSYMLB3, msymlb3,
	       (ist, lspgrp, &namspg_cif, &namspg_cifs, &nampg, &nsymp, nsym, 
                rlsymmmatrx, namspg_cif_len, namspg_cifs_len, nampg_len),
	       (ist, lspgrp, &namspg_cif, &namspg_cifs, &nampg, &nsymp, nsym, 
                rlsymmmatrx),
	       (ist, lspgrp, &namspg_cif, namspg_cif_len, &namspg_cifs, 
                namspg_cifs_len, &nampg, nampg_len, &nsymp, nsym, rlsymmmatrx));

}

/** Epsilon zones currently set up in ccp4spg_load_spacegroup
 * If these are not available, use lookup by symops.
 * @param nsm number of symmetry operators.
 * @param nsmp number of primitive symmetry operators.
 * @param rsm symmetry matrices.
 * @param iprint If iprint > 0 then a summary of epsilon zones is printed.
 */
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

  if (spacegroup && *iprint > 0) ccp4spg_print_epsilon_zones(spacegroup);
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
 * @param iprint If iprint > 0 then a summary of centric zones is printed.
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

  if (spacegroup && *iprint > 0) ccp4spg_print_centric_zones(spacegroup);
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

  if (*ic == -1) ccperror(1,"Fatal error in CENTR.");
}

FORTRAN_SUBR ( CCP4SPG_F_IS_CENTRIC, ccp4spg_f_is_centric,
	       (const int *sindx, const int ih[3], int *ic),
	       (const int *sindx, const int ih[3], int *ic),
	       (const int *sindx, const int ih[3], int *ic))
{
  int h,k,l;

  CSYMLIB_DEBUG(puts("CSYMLIB_F: CCP4SPG_F_IS_CENTRIC");)

  if (*sindx <= 0 || *sindx > MSPAC) {
    printf("Error in CCP4SPG_F_IS_CENTRIC: sindx %d out of range!\n",*sindx);
    return;
  }

  if ( ! spacegrp[*sindx-1] ) {
    printf("CCP4SPG_F_IS_CENTRIC: No spacegroup loaded on channel %d ! \n",*sindx);
    return;
  }

  h = ih[0]; k = ih[1]; l = ih[2]; 

  *ic = ccp4spg_is_centric(spacegrp[*sindx-1], h, k, l);

  if (*ic == -1) ccperror(1,"Fatal error in CCP4SPG_F_IS_CENTRIC.");
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
    if ( ! (tmp_spacegroup = ccp4spg_load_by_ccp4_num(*lspgrp)) ) {
      printf("SETLIM: failed to load spacegroup info from SYMINFO! \n");
      return;
    }
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

/* Returns map limits of a.s.u. in fractional units.
   These are rounded up or down to mimic <= or < respectively.
   In fact, these limits may be larger than 1 a.s.u. but always
   have one corner at the origin.
   This version uses mapasu_zero limits from sgtbx */
FORTRAN_SUBR ( SETLIM_ZERO, setlim_zero,
	       (const int *lspgrp, float xyzlim[3][2]),
	       (const int *lspgrp, float xyzlim[3][2]),
	       (const int *lspgrp, float xyzlim[3][2]))
{ 
  CCP4SPG *tmp_spacegroup;      

  CSYMLIB_DEBUG(puts("CSYMLIB_F: SETLIM_ZERO");)

  if (!spacegroup || spacegroup->spg_ccp4_num != *lspgrp) {
    /* load spacegroup if necessary */
    /* spacegroup only temporary, as setlim is not expected to
       interact with other calls */
    if ( ! (tmp_spacegroup = ccp4spg_load_by_ccp4_num(*lspgrp)) ) {
      printf("SETLIM_ZERO: failed to load spacegroup info from SYMINFO! \n");
      return;
    }
    xyzlim[0][1] = tmp_spacegroup->mapasu_zero[0];
    xyzlim[1][1] = tmp_spacegroup->mapasu_zero[1];
    xyzlim[2][1] = tmp_spacegroup->mapasu_zero[2];
    free(tmp_spacegroup); 
  } else {
    xyzlim[0][1] = spacegroup->mapasu_zero[0];
    xyzlim[1][1] = spacegroup->mapasu_zero[1];
    xyzlim[2][1] = spacegroup->mapasu_zero[2];
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
  int nlaue_save = -1;

  if (spacegroup->nlaue != *nlaue) {
    printf("SETGRD: supplied CCP4 Laue code is different from that currently stored\n");
    printf("NLAUE (supplied) = %d\n",*nlaue);
    printf("NLAUE (library)  = %d\n",spacegroup->nlaue);
    printf("(For program FFT and certain spacegroups, this is OK.)\n");
    /* The requested Laue number is different to that for the
       current spacegroup
       Save the current Laue code and load the data for the requested code */
    nlaue_save = spacegroup->nlaue;
    if (ccp4spg_load_laue(spacegroup,*nlaue)) {
      printf("SETGRD: unrecognised CCP4 Laue code, couldn't set FFT grid\n");
      return;
    }
  }
  set_fft_grid(spacegroup, *nxmin, *nymin, *nzmin, *sample, nx, ny, nz);
  if (nlaue_save > -1) {
    /* Restore previous settings */
    ccp4spg_load_laue(spacegroup,nlaue_save);
  }
  return;
}

FORTRAN_SUBR ( FNDSMP, fndsmp,
	       (const int *minsmp, const int *nmul, const float *sample, int *nsampl),
	       (const int *minsmp, const int *nmul, const float *sample, int *nsampl),
	       (const int *minsmp, const int *nmul, const float *sample, int *nsampl))
{

  *nsampl = get_grid_sample(*minsmp, *nmul, *sample);

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
  char *temp_namspg;
  int i,j,k;
  int polarx, polary, polarz;
  float crsym[192][4][4];

  CSYMLIB_DEBUG(puts("CSYMLIB_F: CALC_ORIG_PS");)

  temp_namspg = ccp4_FtoCString(FTN_STR(namspg_cif), FTN_LEN(namspg_cif));
  for (i = 0; i < *nsym; ++i) {
    for (j = 0; j < 4; ++j) {
      for (k = 0; k < 4; ++k) {
        crsym[i][k][j] = rsym[i][j][k];
      }
    }
  }
  *norig = ccp4spg_generate_origins(temp_namspg, *nsym, crsym, orig, &polarx, &polary, &polarz, 1);
  *lpaxisx = polarx ? FORTRAN_LOGICAL_TRUE : FORTRAN_LOGICAL_FALSE;
  *lpaxisy = polary ? FORTRAN_LOGICAL_TRUE : FORTRAN_LOGICAL_FALSE;
  *lpaxisz = polarz ? FORTRAN_LOGICAL_TRUE : FORTRAN_LOGICAL_FALSE;

  free(temp_namspg);
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
