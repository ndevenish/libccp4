/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/

/** @file cmtzlib.h
 *  Functions defining the C-level API for accessing MTZ files.
 *  MtzGet and MtzPut read and write MTZ files to/from a data
 *  structure defined in mtzdata.h  Other functions allow one
 *  to access data structure members, and to manipulate the structure
 *  and the values of structure members. Functions with names
 *  beginning <tt>ccp4_lr</tt> or <tt>ccp4_lw</tt> are primarily 
 *  to support the Fortran API.
 *  @author Martyn Winn 
 */

#ifndef __CMTZLib__
#define __CMTZLib__

static char rcsidhm[] = "$Id$";

/* defines CCP4::CCP4File */
#include "ccp4_lib.h"

#ifdef  __cplusplus
namespace CMtz {
extern "C" {
typedef CCP4::CCP4File CCP4File;
#endif

/* needs to be here for C++ to use CCP4File typedef */
#include "mtzdata.h"

/**** MTZ i/o ****/

/** Reads the contents of the MTZ file into an MTZ structure.
 * @param logname logical name of MTZ file
 * @param read_refs whether to read reflections into memory (non-zero) or
 *        to read later from file (zero)
 * @return pointer to MTZ struct
 */
MTZ *MtzGet(const char *logname, int read_refs);

/** Reads reflection data from MTZ file.
 * @param filein pointer to input file
 * @param ncol number of columns to read
 * @param refldata array of reflection data
 * @return void
 */
void MtzRrefl(CCP4File *filein, int ncol, float *refldata);

/** Writes an MTZ data structure to disk. If file is already open, MtzPut
 * uses file pointer in mtz struct, else uses logical name of file.
 * @param mtz pointer to MTZ struct.
 * @param logname logical name for output file or blank.
 * @return void
 */
void MtzPut(MTZ *mtz, const char *logname);

/** Opens a new MTZ file for writing.
 * @param logname logical name for output file.
 * @param hdrst position of header
 * @return pointer to file
 */
CCP4File *MtzOpenForWrite(const char *logname, int *hdrst);

void MtzWhdrLine(CCP4File *fileout, int nitems, char buffer[]);
/* write header record to fileout. Record is filled from
     nitems to MTZRECORDLENGTH by blanks */

/** Write ncol column entries to fileout from refldata.
 * @param fileout pointer to output MTZ file.
 * @param ncol number of reflection data items to write.
 * @param refldata array of reflection data items.
 * @return number of items written.
 */
int MtzWrefl(CCP4File *fileout, int ncol, float *refldata);

/**** Memory allocation ****/

/** Allocates memory for an MTZ header structure. 
 * @param nxtal Number of crystals to allocate.
 * @param nset Number of datasets for each crystal to allocate.
 * @return pointer to MTZ header struct
 */
MTZ *MtzMalloc(int nxtal, int nset[]);

/** Frees the memory reserved for the MTZ header struct.
 * @param mtz pointer to MTZ header struct. 
 * @return void
 */
void MtzFree(MTZ *mtz);

/** Allocates memory for an MTZ column. Space is allocated for
 * the reflection data if and only if mtz->refs_in_memory is set.
 * @param mtz pointer to MTZ header struct. 
 * @param nref number of reflections in column.
 * @return pointer to MTZ column.
 */
MTZCOL *MtzMallocCol(MTZ *mtz, int nref);

/** Frees the memory reserved for 'col'
 * @param col pointer to MTZ column.
 * @return void
 */
void MtzFreeCol(MTZCOL *col);

MTZBAT *MtzMallocBatch(void);
/* Allocates memory for a single batch header */

void MtzFreeBatch(MTZBAT *batch);
/* Frees the memory reserved for 'batch' */
  
char *MtzCallocHist(int nhist);
/* Allocates memory for the mtz history with 'nhist' lines */

void MtzFreeHist(char *hist);
/* Frees the memory reserved for 'hist' */

/**** Header operations ****/
  
int MtzNbat(const MTZ *mtz);
/* get the number of batches in the mtz */

int MtzNref(const MTZ *mtz);
/* get the number of reflections in the mtz */

int MtzSpacegroupNumber(const MTZ *mtz);
/* get the spacegroup number (likely CCP4 convention) */

/** Return the overall resolution limits of the MTZ structure.
 * These are the widest limits over all crystals present.
 * @param mtz pointer to MTZ struct
 * @param minres minimum resolution
 * @param maxres maximum resolution
 * @return void
 */
void MtzResLimits(const MTZ *mtz, float *minres, float *maxres);

/**** Crystal operations ****/

/** Get the total number of crystals in the MTZ structure
 * @param mtz pointer to MTZ struct
 * @return number of active crystals
 */
int MtzNxtal(const MTZ *mtz);

/** Get the number of active crystals in the MTZ structure
 * @param mtz pointer to MTZ struct
 * @return number of active crystals
 */
int MtzNumActiveXtal(const MTZ *mtz);

/** Return array of pointers to crystals.
 * @param mtz pointer to MTZ struct
 * @return array of pointers to crystals
 */
MTZXTAL **MtzXtals(MTZ *mtz);

/** Return pointer to the ixtal'th crystal. 
 * @param mtz pointer to MTZ struct
 * @param ixtal number of the particular crystal (ixtal = 0 ... MtzNxtal(xtal) -1 
 * @return pointer to the specified crystal
 */
MTZXTAL *MtzIxtal(const MTZ *mtz, const int ixtal);

/** Return the full path name of a crystal as "/xname" 
 * Memory for the path name is assigned with malloc, and can
 * be free'd by the calling function.
 * @param xtal pointer to the crystal struct
 * @return pointer to string containing path name
 */
char *MtzXtalPath(const MTZXTAL *xtal);

MTZXTAL *MtzXtalLookup(const MTZ *mtz, const char *label);
/* Returns a pointer to the crystal of mtz with the given `label`, or NULL */

MTZXTAL *MtzAddXtal(MTZ *mtz, const char *xname, const char *pname,
                  const float cell[6]);
/* Add a crystal to header mtz */

/** For a given crystal, return number of datasets in that crystal.
 * @param xtal pointer to the crystal struct
 * @return number of datasets
 */
int MtzNsetsInXtal(const MTZXTAL *xtal);

/** For a given crystal, return number of active datasets in that crystal.
 * @param xtal pointer to the crystal struct
 * @return number of datasets
 */
int MtzNumActiveSetsInXtal(const MTZ *mtz, const MTZXTAL *xtal);

/** For a given crystal, return array of pointers to datasets 
 * in that crystal.
 * @param xtal pointer to the crystal struct
 * @return array of pointers to datasets 
 */
MTZSET **MtzSetsInXtal(MTZXTAL *xtal);

/** For a given crystal, return pointer to the iset'th dataset
 * in that crystal.
 * @param xtal pointer to the crystal struct
 * @param iset number of the particular dataset (iset = 0 ... MtzNsetsInXtal(xtal) -1
 * @return pointer to specified dataset
 */
MTZSET *MtzIsetInXtal(const MTZXTAL *xtal, const int iset);

/**** Dataset operations ****/

/** Get the number of datasets in the MTZ structure
 * @param mtz pointer to MTZ struct
 * @return total number of datasets
 */
int MtzNset(const MTZ *mtz);

/** Get the number of active datasets in the MTZ structure
 * @param mtz pointer to MTZ struct
 * @return total number of datasets
 */
int MtzNumActiveSet(const MTZ *mtz);

/** Get the crystal associated with a dataset 
 * The pointer to MTZ is required to do reverse lookup of xname. 
 * @param mtz pointer to MTZ struct
 * @param set pointer to dataset
 * @return pointer to parent crystal
 */
MTZXTAL *MtzSetXtal(const MTZ *mtz, const MTZSET *set);

/** Return the full path name of a dataset as "/xname/dname" 
 * The pointer to MTZ is required to do reverse lookup of xname.
 * Memory for the path name is assigned with malloc, and can
 * be free'd by the calling function.
 * @param mtz pointer to MTZ struct
 * @param set pointer to dataset
 * @return pointer to string containing path name
 */
char *MtzSetPath(const MTZ *mtz, const MTZSET *set);

MTZSET *MtzSetLookup(const MTZ *mtz, const char *label);
/* Returns a pointer to the dataset of mtz with the given `label`, or NULL */

MTZSET *MtzAddDataset(MTZ *mtz, MTZXTAL *xtl, const char *dname,
                    const float wavelength);
/* Add a dataset to crystal xtl */

/** For a given dataset, return number of columns in that dataset.
 * This is simply set->ncol and so includes all columns irrespective
 * of col->active
 * @param set pointer to dataset
 * @return number of columns
 */
int MtzNcolsInSet(const MTZSET *set);

/** For a given dataset, return number of active columns in that dataset.
 * @param set pointer to dataset
 * @return number of active columns
 */
int MtzNumActiveColsInSet(const MTZSET *set);

/** For a given dataset, return number of batches in that dataset.
 * @param mtz pointer to MTZ struct
 * @param set pointer to dataset
 * @return number of batches
 */
int MtzNbatchesInSet(const MTZ *mtz, const MTZSET *set);

/** For a given dataset, return array of pointers to columns 
 * in that dataset.
 * @param set pointer to dataset
 * @return array of pointers to columns 
 */
MTZCOL **MtzColsInSet(MTZSET *set);

/** For a given dataset, return pointer to the icol'th column
 * in that dataset.
 * @param set pointer to dataset
 * @param icol number of 
 * the particular column (icol = 0 ... MtzNcolsInSet(set) -1
 * @return pointer to specified column
 */
MTZCOL *MtzIcolInSet(const MTZSET *set, const int icol);

/**** Column operations ****/

/* Add a column to dataset set and create + fill with NAN */
MTZCOL *MtzAddColumn(MTZ *mtz, MTZSET *set, const char *label,
                   const char *type);

/** Assigns a column to a dataset identified by crystal_name and
 * dataset_name. First, the function checks whether the
 * column already belongs to this dataset, in which case it does nothing.
 * Then it checks if the requested dataset exists. If not, it is created,
 * though it is better to explicitly create it beforehand. Finally, the column
 * is assigned to the dataset.
 * @param mtz pointer to MTZ struct
 * @param col pointer to column
 * @param crystal_name name of crystal containing dataset
 * @param dataset_name name of dataset
 */
void MtzAssignColumn(MTZ *mtz, MTZCOL *col, const char crystal_name[],  
	     const char dataset_name[]);

/* Toggle active flag of column */
int MtzToggleColumn(MTZCOL *col);

MTZSET  *MtzColSet(const MTZ *mtz, const MTZCOL *col);
/* get the dataset associated with a column */

/** Get the number of columns in the MTZ data structure.
 * @param mtz pointer to MTZ struct
 * @return number of columns
 */
int MtzNcol(const MTZ *mtz);
  
/** Get the number of active columns in the MTZ data structure.
 * @param mtz pointer to MTZ struct
 * @return number of columns
 */
int MtzNumActiveCol(const MTZ *mtz);

/** Return the full path name of a column as "/xname/dname/label" 
 * Memory for the path name is assigned with malloc, and can
 * be free'd by the calling function.
 * @param mtz pointer to MTZ struct
 * @param col pointer to MTZ column.
 * @return full path name of column
 */
char *MtzColPath(const MTZ *mtz, const MTZCOL *col);

void MtzRJustPath(char *path, const char *partial, const int njust);
/* Complete a right-justified path by prefixing with wildcards */

int MtzPathMatch(const char *path1, const char *path2);
/* test for match between two paths, including wildcards */

MTZCOL *MtzColLookup(const MTZ *mtz, const char *label);
/* Returns a pointer to the column of mtz with the given `label`, or NULL */

/** Get the MTZ column type of a particular column.
 * @param col pointer to MTZ column.
 * @return column type
 */
char *MtzColType(MTZCOL *col);

/** Print summary of current crystal/dataset/column hierarchy. This
 * is designed for debugging purposes rather than for the user.
 * @param mtz pointer to MTZ struct
 * @return void
 */
void MtzDebugHierarchy(const MTZ *mtz);

int MtzListColumn(const MTZ *mtz, char clabs[][31], char ctyps[][3], int csetid[]);
/* List of column information: label, type, dataset.
   Returns number of columns in current structure. */

/**** helper functions ****/

/** Find where indices h, k, l are in MTZ structure. Usually, they
 * will be first 3 columns of 1st dataset, but safest not to assume this.
 * @param mtz pointer to MTZ struct
 * @param ind_xtal crystal containing indices
 * @param ind_set dataset containing indices
 * @param ind_col 3 columns containing indices
 * @return void
 */
void MtzFindInd(const MTZ *mtz, int *ind_xtal, int *ind_set, int ind_col[3]);

/** Calculate resolution from indices and coefhkl.
 * coefhkl is obtained from MtzHklcoeffs.
 * @param in integer array of 3 indices
 * @param coefhkl double array of 6 coefficients
 * @return resolution
 */
float MtzInd2reso(const int in[3], const double coefhkl[6]);

/** Generate coefhkl coefficients from given cell parameters.
 * @param cell cell dimensions to be used for resolution calculation.
 * @param coefhkl double array of 6 coefficients
 * @return void
 */
void MtzHklcoeffs(const float cell[6], double coefhkl[6]);

/** Reads batch arrays into data structure.
 * @param intbuf pointer to integer batch array
 * @param fltbuf pointer to float batch array
 * @param batch pointer to batch structure
 * @return void
 */
void MtzArrayToBatch(const int *intbuf, const float *fltbuf, MTZBAT *batch);

void MtzBatchToArray(MTZBAT *batch, int *intbuf, float *fltbuf);
/* Writes batch info into the structure `batch`. */

/**** pseudo-mtzlib routines ****/

void ccp4_lrtitl(const MTZ *mtz, char ftitle[], size_t *len);

int ccp4_lrhist(const MTZ *mtz, char history[][MTZRECORDLENGTH]);

void ccp4_lrsort(const MTZ *mtz, int isort[5]);

void ccp4_lrbats(const MTZ *mtz, int *nbatx, int batchx[]);

void ccp4_lrcell(const MTZXTAL *xtl, float cell[]);

void ccp4_lrsymi(const MTZ *mtz, int *nsympx, char *ltypex, int *nspgrx, 
       char *spgrnx, char *pgnamx);

void ccp4_lrsymm(const MTZ *mtz, int *nsymx, float rsymx[192][4][4]);

int MtzParseLabin(char *labin_line, const char prog_labels[][31], 
	   const int nlprgi, char user_labels[][2][31]);
/* Uses LABIN or LABOUT line to convert program labels to user labels */
/* This is a helper function, but does not access reflection structure at all */
/* Returns the number of program labels matched */

MTZCOL **ccp4_lrassn(const MTZ *mtz, const char labels[][31], const int nlabels, 
		const char types[][3]);
/* Assigns labels in lsprgi to file mtz, and returns pointers to columns */

void ccp4_lridx(const MTZ *mtz, char crystal_name[][64], char dataset_name[][64],
	    char project_name[][64], int isets[], float datcell[][6], 
            float datwave[], int *ndatasets);

/** Returns iref'th reflection from file held in MTZ struct mtz. Returns
 * data for all columns held in input file, but in the order that they are 
 * held in the datasets rather than in strict file order. The "lookup"
 * mechanism can still be used to find particular columns.
 * In "in-memory" mode, reflection data is taken from arrays held in
 * memory. In the traditional file-based mode, a reflection record is
 * read from the input file.
 * @param mtz pointer to MTZ struct
 * @param resol resolution of reflection (output).
 * @param adata array of requested values (output).
 * @param logmss array of flags for missing data (output).
 * @param iref index of requested reflection
 * @return 1 if past last reflection, else 0
 */
int ccp4_lrrefl(const MTZ *mtz, float *resol, float adata[], int logmss[], int iref);

/** Returns iref'th reflection from file held in MTZ struct mtz. Returns
 * data for certain columns held in input file, as specified by the
 * column pointers held in lookup.
 * In "in-memory" mode, reflection data is taken from arrays held in
 * memory. In the traditional file-based mode, a reflection record is
 * read from the input file.
 * @param mtz pointer to MTZ struct
 * @param resol resolution of reflection (output).
 * @param adata array of requested values (output).
 * @param logmss array of flags for missing data (output).
 * @param lookup array of pointers to requested columns
 * @param ncols number of requested columns
 * @param iref index of requested reflection
 * @return 1 if past last reflection, else 0
 */
int ccp4_lrreff(const MTZ *mtz, float *resol, float adata[], int logmss[],
		const MTZCOL *lookup[], const int ncols, const int iref);

int ccp4_ismnf(const MTZ *mtz, const float datum);

void ccp4_lhprt(const MTZ *mtz, int iprint);

void ccp4_lhprt_adv(const MTZ *mtz, int iprint);

void ccp4_lrbat(MTZBAT *batch, float *buf, char *charbuf, int iprint);

void MtzPrintBatchHeader(MTZBAT *batch);

void ccp4_lwtitl(MTZ *mtz, char ftitle[], int flag);

/** Sets the sort order in the MTZ header. The sort order is
 * a list of columns to be used for sorting, to be applied in
 * the order they appear in the list, i.e. sort first on
 * colsort[0], then on colsort[1], etc.
 * @param mtz Pointer to MTZ struct
 * @param colsort Array of pointers to columns.
 * @return void
 */
void MtzSetSortOrder(MTZ *mtz, MTZCOL *colsort[5]);

/** Adds history lines to the MTZ header in front of existing history lines.
 * @param mtz pointer to MTZ struct
 * @param history lines to be added
 * @param nlines number of lines to be added
 * @return total number of history lines
 */
int MtzAddHistory(MTZ *mtz, const char history[][MTZRECORDLENGTH], const int nlines);

MTZCOL **ccp4_lwassn(MTZ *mtz, const char labels[][31], const int nlabels, 
             const char types[][3], const int iappnd);

void ccp4_lwidx(MTZ *mtz, const char crystal_name[],  const char dataset_name[],
	    const char project_name[], float datcell[6], float *datwave);


/** Function to output reflection values for iref'th reflection.
 * In "in-memory" mode, values are added/updated for columns for which 
 * a column-pointer is given in lookup, up to a maximum of ncol columns.
 * In the traditional file-based mode, a reflection record is written
 * to file.
 * @param mtz pointer to MTZ struct
 * @param adata array of values.
 * @param lookup array of pointers to columns.
 * @param ncol number of columns.
 * @param iref index of reflection.
 * @return void
 */
void ccp4_lwrefl(MTZ *mtz, const float adata[], MTZCOL *lookup[], 
		 const int ncol, const int iref);

void ccp4_lwbat(MTZ *mtz, MTZBAT *batch, const int batno, const float *buf, const char *charbuf);

void ccp4_lwbsetid(MTZ *mtz, MTZBAT *batch, const char xname[], const char dname[]);

/* -- Below here there are no implementations -- */

/* COMPLEX HLToSF(float hla, float hlb, float hlc, float hld, BOOLEAN centric); */
/* Returns the mean structure factor as a complex number from a structure
   factor probability distribution described by Hendrickson/Lattman
   coefficients. If `centric == TRUE`, the coefficients describe a centric
   distribution. */

/* MTZ *MtzSort(MTZ *mtz, char *ident); */
/* Sorts `mtz` using the identifiers (separated by spaces) in `ident` as
   keys. Sorting can be done on up to 200 columns. A pointer to `*mtz` is
   returned. */

/* MTZ *HLCombine (MTZ *to, float toscale, MTZ *frm, float frmscale); */
/* Combines extra phase information for common reflections between 'frm'
   and 'to' into the phase information of 'to'. The phase probabilities
   are described by Hendrickson Lattman coefficients, with the identifiers
   "HLA", "HLB", HLC", and "HLD", the indices are identified by "H", "K" 
   and "L". HL-coeffs from 'to' are scaled by 'toscale', the coeffs from
   'frm' are scaled by 'frmscale'. A pointer to `to` is returned. */

/* void MtzPhiFom(MTZ *mtz); */
/* Calculates the best phase and the figure of from Hendrickson Lattman
   coefficients. The following columns should be present in `mtz`:
   "H", "K" & "L" (indices); "PHIB" & "FOM" (best phase (degrees) and figure of
   merit); "HLA", "HLB", "HLC" & "HLD" (Hendrickson Lattman coefficients). */

/* MTZ *MtzCopy(MTZ *frm); */
/* Produces an exact copy of `frm` and returns a pointer to it. */

/* MTZ *MtzColAppend(MTZ *mtz, char *ident, char type); */
/* Appends a column to `*mtz` with identity `ident` and type `type`, provided
   no column with identity `ident` exists. */

/* MTZ *MtzColRemove(MTZ *mtz, char *ident); */
/* Removes a column from `*mtz` with identity `ident`. */

/* MTZ *MtzUpdateRanges(MTZ *mtz); */
/* Updates ranges of all columns in `mtz` and returns `mtz`. */

/* MTZCOL *MtzColNewRange(MTZCOL *col, int nref); */
/* Updates the minimum & maximum values in `col` and returns `col`. */

/* int *MtzUnique(MTZ *mtz, char *ident); */
/* Returns an array (k) of indices: k[j] returns the first occurrence of a set
   of idents, eg. MtzUnique(mtz, "H K L") returns an array from which all the 
   unique reflections can be determined by indexing with k: k[i] is the index
   of the last relection of a set which all have the same hkl indices. It is
   assumed that `mtz` is sorted, using `ident` as keys. */

/* float PhaseProb(float phase, float hla, float hlb, float hlc, float hld,
		BOOLEAN centric); */
/* Returns the probability of `phase` (expressed in radians) as determined by
   the Hendrickson-Lattmann coefficients `hla`, `hlb`, `hlc` and `hld`. If
   `centric == TRUE`, the coefficients describe a centric distribution. */

#ifdef __cplusplus
} }
#endif
#endif
