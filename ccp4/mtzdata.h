/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/

/** @file mtzdata.h
 *
 *  @brief Definition of MTZ data structure.
 *
 *  The file defines a hierarchy of structs which hold the
 *  MTZ data structure.
 *
 *  @author Martyn Winn 
 */

#ifndef __CMTZData__
#define __CMTZData__

#define MTZVERSN "MTZ:V1.1"         /**< traditional version number! */

/** defines for sizes in MTZ structure */
#define SIZE1 20                    /**< size of pre-reflection block */
#define MTZRECORDLENGTH 80          /**< length of records */

#define NBATCHWORDS 185       /**< total size of batch header buffer */
#define NBATCHINTEGERS 29     /**< size of integer section of batch header buffer */
#define NBATCHREALS 156       /**< size of float section of batch header buffer */

#define MXTALS      100      /**< maximum number of crystals (for a few arrays - to be removed!) */
#define MSETS      1000      /**< maximum number of datasets (for a few arrays - to be removed!) */
#define MCOLUMNS  10000      /**< maximum number of columns (for a few arrays - to be removed!) */

/** MTZ column struct. */
typedef struct { char label[31];       /**< column name as given by user */
		 char type[3];         /**< column type */
                 int active;           /**< whether column in active list */
                 unsigned int source;  /**< column index in input file */
 		 float min;            /**< minimum data element */
		 float max;            /**< maximum data element */
		 float *ref;           /**< data array */
	       } MTZCOL;

/** MTZ dataset struct. */
typedef struct { int setid;            /**< Dataset id */
		 char dname[65];       /**< Dataset name */
		 float wavelength;     /**< Dataset wavelength */
		 int ncol;             /**< number of columns */
		 MTZCOL **col;         /**< columns */
	       } MTZSET;

/** MTZ crystal struct. */
typedef struct { int xtalid;           /**< Crystal id */
		 char xname[65];       /**< Crystal name */
		 char pname[65];       /**< Project name */
		 float cell[6];        /**< Crystal cell */
                 float resmin;         /**< Low resolution limit */
                 float resmax;         /**< High resolution limit */
		 int nset;             /**< number of datasets */
		 MTZSET **set;         /**< datasets */
	       } MTZXTAL;

/** MTZ batch struct. */
typedef struct bathead { int num;              /**< batch number */
		 char title[71];       /**< batch title */
		 char gonlab[3][9];    /**< names of the three axes */
                 int iortyp;           /**< type of orientation block (for 
                                          possible future use, now = 0) */
		 int lbcell[6];        /**< refinement flags for cell */
		 int misflg;           /**< number of phixyz used (0, 1, or 2) */
		 int jumpax;           /**< reciprocal axis closest to rotation
					  axis */
		 int ncryst;           /**< crystal number */
		 int lcrflg;           /**< mosaicity model: 0 = isotropic, 
					  1 = anisotropic */
		 int ldtype;           /**< type of data: 2D (1), 3D (2), or 
					  Laue (3) */
		 int jsaxs;            /**< goniostat scan axis number */
		 int nbscal;           /**< number of batch scales & Bfactors
					  (0 if unset) */
		 int ngonax;           /**< number of goniostat axes */
		 int lbmflg;           /**< flag for type of beam info:
					  = 0 for alambd, delamb
					  = 1 also delcor, divhd, divvd */
		 int ndet;             /**< number of detectors (current maximum
					  2) */
                 int nbsetid;          /**< dataset id - should be pointer? */
                 float cell[6];        /**< cell dimensions */
		 float umat[9];        /**< orientation matrix U */
		 float phixyz[2][3];   /**< missetting angles at beginning and
					  end of oscillation */
		 float crydat[12];     /**< mosaicity */
		 float datum[3];       /**< datum values of goniostat axes */
		 float phistt;         /**< start of phi relative to datum */
		 float phiend;         /**< end of phi relative to datum */
		 float scanax[3];      /**< rotation axis in lab frame */
		 float time1;          /**< start time */
		 float time2;          /**< stop time */
		 float bscale;         /**< batch scale */
		 float bbfac;          /**< batch temperature factor */
		 float sdbscale;       /**< sd bscale */
		 float sdbfac;         /**< sd bbfac */
                 float phirange;       /**< phi range */
		 float e1[3];          /**< vector 1 ("Cambridge" laboratory axes)
					  defining ngonax goniostat axes */
		 float e2[3];          /**< vector 2 ("Cambridge" laboratory axes)
					  defining ngonax goniostat axes */
		 float e3[3];          /**< vector 3 ("Cambridge" laboratory axes)
					  defining ngonax goniostat axes */
		 float source[3];      /**< idealised source vector */
		 float so[3];          /**< source vector */
		 float alambd;         /**< wavelength (A) */
		 float delamb;         /**< dispersion (deltalambda / lambda) */
		 float delcor;         /**< correlated component */
		 float divhd;          /**< horizontal beam divergence */
		 float divvd;          /**< vertical beam divergence */
		 float dx[2];          /**< xtal to detector distance */
		 float theta[2];       /**< detector tilt angle */
		 float detlm[2][2][2]; /**< min & max values of detector coords
					  (pixels) */
		 struct bathead *next; /**< next batch in list */
	       } MTZBAT;

/** MTZ symmetry struct. */
typedef struct { int spcgrp;           /**< spacegroup number */
		 char spcgrpname[11];  /**< spacegroup name */
		 int nsym;             /**< number of symmetry operations */
		 float sym[192][4][4]; /**< symmetry operations */
		 int nsymp;            /**< number of primitive symmetry ops. */
		 char symtyp;          /**< lattice type (P,A,B,C,I,F,R) */
		 char pgname[11];      /**< pointgroup name */
               } SYMGRP;

typedef union { char amnf[4]; 
                float fmnf;
              } MNF;

/* Allow up to 50 crystals. Replace by linked list? */

/** Top level of MTZ struct. */
typedef struct { CCP4File *filein;     /**< file for reading */
                 CCP4File *fileout;    /**< file for writing */
		 char title[71];       /**< title of mtz structure */
		 char *hist;           /**< history of mtz file */
		 int histlines;        /**< number of lines in hist */
		 int nxtal;            /**< number of crystals */
                 int ncol_read;        /**< number of columns from file */
		 int nref;             /**< number of reflections */
                 int refs_in_memory;   /**< whether reflections are held in memory */
		 int n_orig_bat;       /**< original number of batches */
                 MNF mnf;              /**< value of missing number flag */
                 SYMGRP mtzsymm;       /**< symmetry information */
		 MTZXTAL **xtal;       /**< crystals */
		 MTZBAT *batch;        /**< first batch header */
		 MTZCOL *order[5];     /**< sort order */
	       } MTZ;

#endif
