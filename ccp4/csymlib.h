/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/

/** @file csymlib.h
 *  Functions defining the C-level API for accessing spacegroup properties.
 *  @author Martyn Winn 
 */

#ifndef __CSymLib__
#define __CSymLib__

#ifdef  __cplusplus
namespace CSym {
extern "C" {
#endif
static char rcsidhs[] = "$Id$";

/** Look up spacegroup in standard setting by number and load properties.
 * @param numspg spacegroup number
 * @return pointer to spacegroup
 */
CCP4SPG *ccp4spg_load_by_standard_num(const int numspg); 

/** Look up spacegroup by CCP4 number and load properties.
 * @param ccp4numspg CCP4 spacegroup number
 * @return pointer to spacegroup
 */
CCP4SPG *ccp4spg_load_by_ccp4_num(const int ccp4numspg); 

/** Look up spacegroup by name.
 * @param spgname Spacegroup name.
 * @return pointer to spacegroup
 */
CCP4SPG *ccp4spg_load_by_spgname(const char *spgname);

/** Look up spacegroup by symmetry operators and load properties.
 * @param nsym1 number of operators (including non-primitive)
 * @param op1 pointer to array of operators
 * @return pointer to spacegroup
 */
CCP4SPG * ccp4_spgrp_reverse_lookup(const int nsym1, const ccp4_symop *op1);

/** Look up spacegroup from SYMOP.
 *  This would not normally be called directly, but via one of
 *  the wrapping functions. 
 * @param numspg spacegroup number
 * @param ccp4numspg CCP4 spacegroup number
 * @param spgname Spacegroup name.
 * @param nsym1 number of operators (including non-primitive)
 * @param op1 pointer to array of operators
 * @return pointer to spacegroup
 */
CCP4SPG *ccp4spg_load_spacegroup(const int numspg, const int ccp4numspg,
        const char *spgname, const int nsym1, const ccp4_symop *op1); 

/** Free all memory malloc'd from static pointers.
 * To be called before program exit. The function can be
 * registered with atexit.
 */
void ccp4spg_mem_tidy(void);

/** Free memory associated with spacegroup.
 * @param sp pointer to spacegroup
 */
void ccp4spg_free(CCP4SPG *sp);

/** Look up spacegroup in standard setting by number and load into
 * static storage of csymlib_f.
 * @param numspg spacegroup number
 * @return void
 */
void ccp4spg_register_by_ccp4_num(int numspg);

/** Derive centering operators from Hall symbol (deprecated).
 * Centering operators are now read from syminfo.lib
 * @param symbol_Hall Hall symbol for spacegroup
 * @param cent_ops centering operators
 * @return number of centering operators (0 if none found)
 */
int ccp4_spg_get_centering(const char *symbol_Hall, float cent_ops[4][3]);

/** Test if reflection is in asu of Laue group 1bar.
 * @return 1 if in asu else 0
 */
int ASU_1b   (const int h, const int k, const int l);

/** Test if reflection is in asu of Laue group 2/m.
 * @return 1 if in asu else 0
 */
int ASU_2_m  (const int h, const int k, const int l);

/** Test if reflection is in asu of Laue group mmm.
 * @return 1 if in asu else 0
 */
int ASU_mmm  (const int h, const int k, const int l);

/** Test if reflection is in asu of Laue group 4/m.
 * @return 1 if in asu else 0
 */
int ASU_4_m  (const int h, const int k, const int l);

/** Test if reflection is in asu of Laue group 4/mmm.
 * @return 1 if in asu else 0
 */
int ASU_4_mmm(const int h, const int k, const int l);

/** Test if reflection is in asu of Laue group 3bar.
 * @return 1 if in asu else 0
 */
int ASU_3b   (const int h, const int k, const int l);

/** Test if reflection is in asu of Laue group 3bar1m.
 * @return 1 if in asu else 0
 */
int ASU_3bm  (const int h, const int k, const int l);

/** Test if reflection is in asu of Laue group 3barm.
 * @return 1 if in asu else 0
 */
int ASU_3bmx (const int h, const int k, const int l);

/** Test if reflection is in asu of Laue group 6/m.
 * @return 1 if in asu else 0
 */
int ASU_6_m  (const int h, const int k, const int l);

/** Test if reflection is in asu of Laue group 6/mmm.
 * @return 1 if in asu else 0
 */
int ASU_6_mmm(const int h, const int k, const int l);

/** Test if reflection is in asu of Laue group m3bar.
 * @return 1 if in asu else 0
 */
int ASU_m3b  (const int h, const int k, const int l);

/** Test if reflection is in asu of Laue group m3barm.
 * @return 1 if in asu else 0
 */
int ASU_m3bm  (const int h, const int k, const int l);

/** Function to return Hall symbol for spacegroup.
 * @param sp pointer to spacegroup
 * @return pointer to Hall symbol for spacegroup
 */
char *ccp4spg_symbol_Hall(CCP4SPG* sp);

ccp4_symop ccp4_symop_invert( const ccp4_symop op1 );

/** Compare two spacegroup names. Blanks are removed when
 * making the comparison. Strings are converted to upper
 * case before making the comparison. If one string has " 1 " and 
 * the other doesn't, then strip out " 1" to do "short" comparison.
 * @param spgname1 First spacegroup name.
 * @param spgname2 Second spacegroup name.
 * @return 1 if they are equal else 0.
*/
int ccp4spg_name_equal(const char *spgname1, const char *spgname2);

/** Function to create "short" name of spacegroup. Blanks
 * are removed, as are " 1" elements (except for the special case
 * of "P 1").
 * @param shortname String long enough to hold short name.
 * @param longname Long version of spacegroup name.
 * @return Pointer to shortname.
*/
char *ccp4spg_to_shortname(char *shortname, const char *longname);

/** Compare two symmetry operator lists.
 * Kevin's code. The lists are coded as ints, which are then sorted and compared.
 * @param nsym1 number of symmetry operators in first list
 * @param op1 first list of symmetry operators
 * @param nsym2 number of symmetry operators in second list
 * @param op2 second list of symmetry operators
 * @return 1 if they are equal else 0.
*/
int ccp4_spgrp_equal( int nsym1, const ccp4_symop *op1, int nsym2, const ccp4_symop *op2);

/** Make an integer coding of a symmetry operator.
 * The coding takes 30 bits: 18 for the rotation and 12 for the translation.
 * @param op symmetry operator
 * @return int code.
 */
int ccp4_symop_code(ccp4_symop op);

/* Comparison of symmetry operators encoded as integers.
 * In ccp4_spgrp_equal, this is passed to the stdlib qsort.
*/
int ccp4_int_compare( const void *p1, const void *p2 );

/** Test whether reflection or it's Friedel mate is in asu.
 * @param sp pointer to spacegroup
 * @param h reflection index
 * @param k reflection index
 * @param l reflection index
 * @return 1 if in asu, -1 if -h -k -l is in asu, 0 otherwise
 */
int ccp4spg_is_in_pm_asu(const CCP4SPG* sp, const int h, const int k, const int l);

/** Test whether reflection is in asu.
 * @param sp pointer to spacegroup
 * @param h reflection index
 * @param k reflection index
 * @param l reflection index
 * @return 1 if in asu, 0 otherwise
 */
int ccp4spg_is_in_asu(const CCP4SPG* sp, const int h, const int k, const int l);

/** Place reflection (hin,kin,lin) in the asymmetric unit of spacegroup "sp".
 * Resultant indices are placed in (hout,kout,lout).
 * @param sp pointer to spacegroup
 * @param hin input reflection index
 * @param kin input reflection index
 * @param lin input reflection index
 * @param hout output reflection index
 * @param kout output reflection index
 * @param lout output reflection index
 * @return "isym" if successful, 0 otherwise. "isym" = 2*isymop - 1 for 
 * reflections placed in the positive asu, i.e. I+ of a Friedel pair, and
 * "isym" = 2*isymop for reflections placed in the negative asu, i.e. I- of 
 * a Friedel pair. Here "isymop" is the number of the symmetry operator used.
 */
int ccp4spg_put_in_asu(const CCP4SPG* sp, const int hin, const int kin, const int lin,
		       int *hout, int *kout, int *lout );

/** Transform reflection (hin,kin,lin) according to spacegroup "sp" and
 * operation "isym". Resultant indices are placed in (hout,kout,lout).
 * @param sp pointer to spacegroup
 * @param isym required operation, see ccp4spg_put_in_asu
 * @param hin input reflection index
 * @param kin input reflection index
 * @param lin input reflection index
 * @param hout output reflection index
 * @param kout output reflection index
 * @param lout output reflection index
 * @return void
 */
void ccp4spg_generate_indices(const CCP4SPG* sp, const int isym,
                  const int hin, const int kin, const int lin,
			      int *hout, int *kout, int *lout );

/* shift phase value associated with hin,kin,lin according to translation 
and optional sign change. Return in range 0,360 */
float ccp4spg_phase_shift(const int hin, const int kin, const int lin,
			  const float phasin, const float trans[3], const int isign);
int ccp4spg_do_chb(const float chb[3][3]);
void ccp4spg_set_centric_zones(CCP4SPG* sp);
int ccp4spg_is_centric(const CCP4SPG* sp, const int h, const int k, const int l);
int ccp4spg_check_centric_zone(const int nzone, const int h, const int k, const int l);
float ccp4spg_centric_phase(const CCP4SPG* sp, const int h, const int k, const int l);
void ccp4spg_print_centric_zones(const CCP4SPG* sp);
char *ccp4spg_describe_centric_zone(const int nzone, char *centric_zone);
void ccp4spg_set_epsilon_zones(CCP4SPG* sp);
int ccp4spg_get_multiplicity(const CCP4SPG* sp, const int h, const int k, const int l);
int ccp4spg_check_epsilon_zone(const int nzone, const int h, const int k, const int l);
void ccp4spg_print_epsilon_zones(const CCP4SPG* sp);
char *ccp4spg_describe_epsilon_zone(const int nzone, char *epsilon_zone);

int ccp4spg_is_sysabs(const CCP4SPG* sp, const int h, const int k, const int l);

/* ccp4spg_generate_origins translated from Alexei Vagin's CALC_ORIG_PS */
int ccp4spg_generate_origins(const char *namspg, const int nsym, const float rsym[][4][4],
			     float origins[][3], int *polarx, int *polary, int *polarz,
			     const int iprint);

int range_to_limits(const char *range, float limits[2]);

void set_fft_grid(CCP4SPG* sp, const int nxmin, const int nymin, const int nzmin, 
		  const float sample, int *nx, int *ny, int *nz);
int all_factors_le_19(const int n);
int get_grid_sample(const int minsmp, const int nmul, const float sample);

#ifdef __cplusplus
} }
#endif
#endif
