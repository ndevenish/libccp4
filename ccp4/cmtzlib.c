/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/

/** @file cmtzlib.c
 *  C library for reading, writing and manipulating MTZ files.
 *  @date Created Sept. 2000 
 *  @author Martyn Winn 
 */

/* see cmtzlib.h for descriptions of functions */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "cmtzlib.h"
#include "ccp4_types.h"
#include "ccp4_array.h"
#include "ccp4_parser.h"
#include "ccp4_vars.h"
#include "ccp4_errno.h"
static char rcsid[] = "$Id$";

/* stuff for error reporting */
#define CMTZ_ERRNO(n) (CCP4_ERR_MTZ | (n))

/* error defs */
#define  CMTZERR_Ok                  0
#define  CMTZERR_NoChannel           1
#define  CMTZERR_NoFile              2
#define  CMTZERR_NoLogicalName       3
#define  CMTZERR_CantOpenFile        4
#define  CMTZERR_NoHeader            5
#define  CMTZERR_ReadFail            6
#define  CMTZERR_WriteFail           7
#define  CMTZERR_ParamError          8
#define  CMTZERR_Cellerr             9
#define  CMTZERR_FileStamp           10
#define  CMTZERR_SymErr              11
#define  CMTZERR_AllocFail           12
#define  CMTZERR_MaxFile             13
#define  CMTZERR_ParserFail          14
#define  CMTZERR_NotMTZ              15

MTZ *MtzGet(const char *logname, int read_refs)

{ MTZ *mtz;
  CCP4File *filein;
  int istat, newproj;
  MTZCOL *colin[MCOLUMNS];
  char *filename;
  char crysin[MXTALS][65],projin[MXTALS][65],crystal[65],project[65];
  float cellin[MXTALS][6],cell[6];
  int jxtalin[MSETS];
  char mkey[4], keyarg[76], hdrrec[MTZRECORDLENGTH], label[30], type;
  int i, j, hdrst, ntotcol, nref, ntotset=0, nbat, nhist=0, icol, icolin;
  int ixtal, jxtal, iset, iiset, icset, nxtal=0, nset[MCOLUMNS]={0}, isym=0;
  int ncol[MXTALS][MSETSPERXTAL]={{0}};
  int indhigh[3],indlow[3],isort[5],debug=0;
  float min,max,totcell[6],minres,maxres;
  float refldata[MCOLUMNS];
  double coefhkl[6];

  /* For cparser */
  CCP4PARSERARRAY *parser;
  CCP4PARSERTOKEN *token=NULL;
  char *key;
  int ntok,iprint=0;

  /* For batches */
  int ibat,nwords,nintegers,nreals;
  float buf[NBATCHWORDS];
  int *intbuf = (int *) buf;
  float *fltbuf = buf + NBATCHINTEGERS;
  MTZBAT *batch;

  if (debug) 
    printf(" Entering MtzGet \n");

  /* check input */
  if (!logname) return NULL;

  /* Open the mtz file: */
  if (getenv(logname) != NULL) {
    filename = strdup(getenv(logname));
  } else {
    filename = strdup(logname);
  }

  if (debug) 
    printf(" Opening file %s \n",filename);

  filein = ccp4_file_open(filename,O_RDONLY);
  if (! filein ) {
    ccp4_signal(CMTZ_ERRNO(CMTZERR_CantOpenFile),"MtzGet",NULL);
    return NULL;
  }

  if (debug) 
    printf(" File opened successfully \n");

  ccp4_file_setstamp(filein, 2);
  /* Read architecture */
  istat = ccp4_file_rarch (filein);
  if (!istat)
   printf(" WARNING: no architecture information in file -- assuming native. \n");

  parser = ccp4_parse_start(20);
  if (parser == NULL) {
    ccp4_signal(CMTZ_ERRNO(CMTZERR_ParserFail),"MtzGet",NULL);
    return NULL;
  }
  /* Set some convenient pointers to members of the parser array */
  key   = parser->keyword;
  token = parser->token;

  ccp4_file_seek (filein, 0, SEEK_SET);
  ccp4_file_setmode(filein,0);
  istat = ccp4_file_readchar(filein, hdrrec, 4);
  ntok = ccp4_parser(hdrrec, MTZRECORDLENGTH, parser, iprint);

  if (!ccp4_keymatch(key,"MTZ")) {
    ccp4_signal(CMTZ_ERRNO(CMTZERR_NotMTZ),"MtzGet",NULL);
    return(NULL);
  }

  if (debug) 
    printf(" MTZ file confirmed \n");

  ccp4_file_setmode(filein,6);
  istat = ccp4_file_read(filein, (uint8 *) &hdrst, 1);
  if (debug) printf(" hdrst read as %d \n",hdrst);

  /* 1st Pass: Read ntotcol, nref, nbat and dataset info.  
     nxtal and nset are used to assign memory for MTZ structure.
     Position at top of header */
  ccp4_file_seek(filein, hdrst-1, SEEK_SET);

  if (debug) 
    printf(" Start first pass \n");

  strcpy(project,"dummy");
  strcpy(crystal,"dummy");
  iiset = -1;
  ccp4_file_setmode(filein,0);
  istat = ccp4_file_readchar(filein, hdrrec, MTZRECORDLENGTH);
  ntok = ccp4_parser(hdrrec, MTZRECORDLENGTH, parser, iprint);
  while (!ccp4_keymatch(key,"END")) {

    /* read total number of columns, reflections and batches */
    if (ccp4_keymatch(key, "NCOL")) {
      ntotcol = (int) token[1].value;
      nref = (int) token[2].value;
      nbat = (int) token[3].value;
    }

    /* read total number of datasets over all projects/crystals */
    else if (ccp4_keymatch(key, "NDIF")) {
      ntotset = (int) token[1].value;
    }

    /* PROJECT line. Projects are not part of data structure, but
       may imply new crystal in hierarchy. */
    else if (ccp4_keymatch(key, "PROJ")) {
      ++iiset;
      strcpy(project,"dummy");
      if (ntok > 2) strcpy(project,token[2].fullstring);
      strcpy(crystal,project);
      jxtal = -1;
      for (ixtal = 0; ixtal < nxtal; ++ixtal) {
        if (strcmp(projin[ixtal],project) == 0) {
          jxtal = ixtal;
        }
      }
      /* New project implies new crystal */
      newproj=0;
      if (jxtal == -1) {
        ++nxtal;
        jxtalin[iiset]=nxtal-1;
        strcpy(projin[nxtal-1],project);
        strcpy(crysin[nxtal-1],crystal);
        newproj=1;
      }
    }

    /* CRYSTAL line. This will be missing in old files! 
       If the line is present but incomplete we treat it as missing. */
    else if (ccp4_keymatch(key, "CRYS")) {
      if (ntok >= 3) {
       strcpy(crystal,token[2].fullstring);
       if (newproj == 1) {
        strcpy(crysin[nxtal-1],crystal);
       } else { 
        jxtal = -1;
        for (ixtal = 0; ixtal < nxtal; ++ixtal) {
          if (strcmp(crysin[ixtal],crystal) == 0) {
            jxtal = ixtal;
          }
        }
        if (jxtal == -1) {
          ++nxtal;
          jxtalin[iiset]=nxtal-1;
          strcpy(projin[nxtal-1],project);
          strcpy(crysin[nxtal-1],crystal);
        }
       }
      }
    }

    /* DATASET line. This should present for every dataset so use to
       increment dataset count. */
    else if (ccp4_keymatch(key, "DATA")) {
      ++nset[nxtal-1];
    }

    /* DCELL line. */
    else if (ccp4_keymatch(key, "DCEL")) {
      for (i = 0; i < 6; ++i) 
        cell[i] = (float) token[i+2].value;
      /* If old crystal but cell dimensions differ, make new crystal.
         This is primarily for old files with no CRYSTAL cards. */
      if (jxtal > -1 && iiset > 0 && 
                (cellin[jxtal][0] != cell[0] || cellin[jxtal][1] != cell[1]
              || cellin[jxtal][2] != cell[2] || cellin[jxtal][3] != cell[3]
              || cellin[jxtal][4] != cell[4] || cellin[jxtal][5] != cell[5] )) {
        if (debug) {
          printf(" MtzGet: Old crystal %d but new cell dimensions. \n",jxtal);
          for (i = 0; i < 6; ++i) 
            printf(" %f %f \n",cellin[jxtal][i],cell[i]);
	}
        ++nxtal;
        jxtalin[iiset]=nxtal-1;
        strcpy(projin[nxtal-1],project);
        strcpy(crysin[nxtal-1],crystal);
	/* Try to make crystal name unique */
        sprintf(crysin[nxtal-1]+strlen(crystal),"%d",nxtal);
	/* correct DATASET increment */
        --nset[nxtal-2];
        ++nset[nxtal-1];
      }
      for (i = 0; i < 6; ++i) 
        cellin[nxtal-1][i] = cell[i];
    }

    istat = ccp4_file_readchar(filein, hdrrec, MTZRECORDLENGTH);
    ntok = ccp4_parser(hdrrec, MTZRECORDLENGTH, parser, iprint);
  }

  if (debug) {
    printf(" MtzGet: Found %d crystals \n",nxtal);
    for (i = 0; i < nxtal; ++i) 
      printf(" MtzGet: Crystal %d has %d datasets \n",i+1,nset[i]);
  }

  if (debug) 
    printf(" MtzGet: end of 1st pass \n");

  /* If no datasets found in the input MTZ file, create a single
     dataset in a single crystal */
  if (ntotset == 0) {
    nxtal = 1; nset[0] = 1;
  }

  /* Allocate memory for input MTZ file */
  if (! (mtz = MtzMalloc(nxtal, nset))) return NULL;
  mtz->filein = filein;
  mtz->nref = nref;
  mtz->ncol_read = ntotcol;
  mtz->nbat = nbat;
  mtz->batch = NULL;
  mtz->refs_in_memory = read_refs;

  /* 2nd Pass: Copy dataset information to MTZ structure.
     Position at top of header */
  ccp4_file_setmode(filein,6);
  ccp4_file_seek(filein, hdrst-1, SEEK_SET);

  /* Read dataset information */
  for (i = 0; i < nxtal; ++i) 
    nset[i] = -1;
  iiset = -1;
  ccp4_file_setmode(filein,0);
  istat = ccp4_file_readchar(filein, hdrrec, MTZRECORDLENGTH);
  ntok = ccp4_parser(hdrrec, MTZRECORDLENGTH, parser, iprint);
  while (strncmp((strncpy(mkey,hdrrec,4)),"END",3) != 0) {

    if (strncmp (mkey, "PROJ",4) == 0) {
      ++iiset;
      strcpy(mtz->xtal[jxtalin[iiset]]->pname,projin[jxtalin[iiset]]);
      strcpy(mtz->xtal[jxtalin[iiset]]->xname,crysin[jxtalin[iiset]]);
      mtz->xtal[jxtalin[iiset]]->xtalid = jxtalin[iiset] + 1;
    }

    else if (strncmp (mkey, "DATA",4) == 0) {
      iset = (int) token[1].value;
      ++nset[jxtalin[iiset]];
      mtz->xtal[jxtalin[iiset]]->set[nset[jxtalin[iiset]]]->setid = iset;
      strcpy(mtz->xtal[jxtalin[iiset]]->set[nset[jxtalin[iiset]]]->dname,"dummy");
      if (ntok > 2) strcpy(mtz->xtal[jxtalin[iiset]]->set[nset[jxtalin[iiset]]]->dname,
                              token[2].fullstring);

    }

    else if (strncmp (mkey, "DCEL",4) == 0) {
      for (i = 0; i < 6; ++i) 
        mtz->xtal[jxtalin[iiset]]->cell[i] = (float) token[i+2].value;
    }

    /* this keyword not in use yet */
    else if (strncmp (mkey, "DRES",4) == 0) {
      sscanf(hdrrec+16,"%d %d %d %d %d %d",
                 indhigh,indhigh+sizeof(int),indhigh+2*sizeof(int),
                 indlow,indlow+sizeof(int),indlow+2*sizeof(int));
      MtzHklcoeffs(mtz->xtal[jxtalin[iiset]]->cell, coefhkl);
      mtz->xtal[jxtalin[iiset]]->resmax = MtzInd2reso(indhigh, coefhkl);
      mtz->xtal[jxtalin[iiset]]->resmin = MtzInd2reso(indlow, coefhkl);
    }

    else if (strncmp (mkey, "DWAV",4) == 0) {
      mtz->xtal[jxtalin[iiset]]->set[nset[jxtalin[iiset]]]->wavelength = (float) token[2].value;
    }

    istat = ccp4_file_readchar(filein, hdrrec, MTZRECORDLENGTH);
    ntok = ccp4_parser(hdrrec, MTZRECORDLENGTH, parser, iprint);
  }

  if (debug) 
    printf(" MtzGet: end of 2nd pass \n");

  /* If no datasets found in the input MTZ file, create a single
     dataset in a single crystal */
  if (ntotset == 0) {
    mtz->xtal[0]->xtalid = 1;
    strcpy(mtz->xtal[0]->xname,"dummy");
    strcpy(mtz->xtal[0]->pname,"dummy");
    mtz->xtal[0]->cell[0] = 0.0;
    mtz->xtal[0]->set[0]->setid = 1;
    strcpy(mtz->xtal[0]->set[0]->dname,"dummy");
    mtz->xtal[0]->set[0]->wavelength = 0.0;
  }

  /* 3rd Pass: Position at top of header */
  ccp4_file_setmode(filein,6);
  ccp4_file_seek(filein, hdrst-1, SEEK_SET);

  for (i = 0; i < mtz->nxtal; ++i) {
   for (j = 0; j < mtz->xtal[i]->nset; ++j) {
     ncol[i][j] = -1;
   }
  }
  icolin = -1;
  ccp4_file_setmode(filein,0);
  istat = ccp4_file_readchar(filein, hdrrec, MTZRECORDLENGTH);
  ntok = ccp4_parser(hdrrec, MTZRECORDLENGTH, parser, iprint);
  while (strncmp((strncpy(mkey,hdrrec,4)),"END",3) != 0) {

    if (strncmp (mkey, "VERS",4) == 0) {
      if (strncmp (hdrrec+5,"MTZ:V1.1",8) != 0) {
         printf("Input MTZ file is not correct version !!\n");
         return(NULL);
         }  
       }
    else if (strncmp (mkey, "TITL",4) == 0) {
       strncpy(mtz->title,hdrrec+6,71); 
       }

    else if (strncmp (mkey, "CELL",4) == 0) {
      sscanf(hdrrec+5,"%f%f%f%f%f%f",&totcell[0],&totcell[1],&totcell[2],
                 &totcell[3],&totcell[4],&totcell[5]);
      for (i = 0; i < mtz->nxtal; ++i) {
        if (mtz->xtal[i]->cell[0] < 0.01) {
          mtz->xtal[i]->cell[0] = totcell[0];
          mtz->xtal[i]->cell[1] = totcell[1];
          mtz->xtal[i]->cell[2] = totcell[2];
          mtz->xtal[i]->cell[3] = totcell[3];
          mtz->xtal[i]->cell[4] = totcell[4];
          mtz->xtal[i]->cell[5] = totcell[5];
        }
      } 
    }
    else if (strncmp (mkey, "SORT",4) == 0) {
      sscanf(hdrrec+5,"%d%d%d%d%d",&isort[0],&isort[1],&isort[2],
                 &isort[3],&isort[4]);
    }
    else if (strncmp (mkey, "SYMI",4) == 0) {
      sscanf(hdrrec+7,"%d%d %c%d%s%s",&mtz->mtzsymm.nsym,&mtz->mtzsymm.nsymp,
	     &mtz->mtzsymm.symtyp,&mtz->mtzsymm.spcgrp,mtz->mtzsymm.spcgrpname,
             mtz->mtzsymm.pgname);
       }
    else if (strncmp (mkey, "SYMM",4) == 0) {
      symop_to_mat4(hdrrec+4,hdrrec+80,mtz->mtzsymm.sym[isym++][0]);
       }

    else if (strncmp (mkey, "COLU",4) == 0) {
      ++icolin;
      sscanf(hdrrec+7,"%s",label);
      sscanf(hdrrec+38,"%c%f%f%d",&type,&min,&max,&icset);
      /* Special trap for M/ISYM */
      if (type == 'Y' && strncmp (label,"M/ISYM",6) == 0)
        strcpy(label,"M_ISYM");
      /* Find dataset corresponding to this column */
      ixtal = 0; iset = 0;
      for (i = 0; i < mtz->nxtal; ++i) {
       for (j = 0; j < mtz->xtal[i]->nset; ++j) {
        if (mtz->xtal[i]->set[j]->setid == icset) {
          ixtal = i;
          iset = j;
          break;
        }
       }
      }

      ++ncol[ixtal][iset];
      icol = ncol[ixtal][iset];

      /* Create column. */
      mtz->xtal[ixtal]->set[iset]->col[icol] = MtzMallocCol(mtz,nref);

      strcpy(mtz->xtal[ixtal]->set[iset]->col[icol]->label,label);
      mtz->xtal[ixtal]->set[iset]->col[icol]->type[0] = type;
      mtz->xtal[ixtal]->set[iset]->col[icol]->type[1] = '\0';
      mtz->xtal[ixtal]->set[iset]->col[icol]->active = 1;
      mtz->xtal[ixtal]->set[iset]->col[icol]->source = icolin + 1;
      mtz->xtal[ixtal]->set[iset]->col[icol]->min = min;
      mtz->xtal[ixtal]->set[iset]->col[icol]->max = max;
      mtz->xtal[ixtal]->set[iset]->ncol = icol + 1;

      colin[icolin] = mtz->xtal[ixtal]->set[iset]->col[icol];
    }

    else if (strncmp (mkey, "VALM",4) == 0) {
      sscanf(hdrrec+5,"%s",keyarg);
      if (strncmp (keyarg, "NAN",3) == 0) {
        sprintf(mtz->mnf.amnf,"NAN");
      } else {
        sscanf(hdrrec+5,"%f",&mtz->mnf.fmnf);
      }
    }

    else if (strncmp (mkey, "RESO",4) == 0) {
      sscanf(hdrrec+5,"%f %f",&minres,&maxres);
      for (i = 0; i < mtz->nxtal; ++i) {
        if (mtz->xtal[i]->resmax == 0.0)
          mtz->xtal[i]->resmax = maxres;
        if (mtz->xtal[i]->resmin == 100.0)
          mtz->xtal[i]->resmin = minres;
      }
    }

    istat = ccp4_file_readchar(filein, hdrrec, MTZRECORDLENGTH);
    ntok = ccp4_parser(hdrrec, MTZRECORDLENGTH, parser, iprint);
  }

  /* copy sort order */
  for (i = 0; i < 5; ++i) {
    if (isort[i] > 0) 
      mtz->order[i] = colin[isort[i]-1];
  }

  if (debug) 
    printf(" MtzGet: end of 3rd pass \n");

  /* Now read history if any */
  istat = ccp4_file_readchar(filein, hdrrec, MTZRECORDLENGTH);
  ntok = ccp4_parser(hdrrec, MTZRECORDLENGTH, parser, iprint);
  while (!ccp4_keymatch(key,"MTZE")) {

    if (ccp4_keymatch(key, "MTZH")) {
      nhist = (int) token[1].value;
      /* allocate memory for nhist lines */
      mtz->hist = MtzCallocHist(nhist);
      mtz->histlines = nhist;

      for (i = 0; i < nhist; ++i) {
        istat = ccp4_file_readchar(filein, hdrrec, MTZRECORDLENGTH);
        strncpy(mtz->hist + MTZRECORDLENGTH*i,hdrrec,MTZRECORDLENGTH);
      }

    } else if (ccp4_keymatch(key, "MTZB")) {
      for (ibat = 0; ibat < nbat; ++ibat) {

        istat = ccp4_file_readchar(filein, hdrrec, MTZRECORDLENGTH);
        ntok = ccp4_parser(hdrrec, MTZRECORDLENGTH, parser, iprint);
        if (!ccp4_keymatch(key, "BH")) {
          printf("Batch headers corrupted !!\n");
          return(NULL);
        }

	/* allocate memory for this batch */
        if (ibat == 0) {
          mtz->batch = MtzMallocBatch();
          batch = mtz->batch;
        } else {
          batch->next = MtzMallocBatch();
          batch = batch->next;
        }
        batch->next = NULL;
        batch->num = (int) token[1].value;
        nwords = (int) token[2].value;
        nintegers = (int) token[3].value;
        nreals = (int) token[4].value;
	/* read batch title */
        istat = ccp4_file_readchar(filein, hdrrec, MTZRECORDLENGTH);
        strncpy(batch->title,hdrrec+6,70); 

        ccp4_file_setmode(filein,6);
        istat = ccp4_file_read(filein, (uint8 *) intbuf, nintegers);
        ccp4_file_setmode(filein,2);
        istat = ccp4_file_read(filein, (uint8 *) fltbuf, nreals);

        MtzArrayToBatch(intbuf, fltbuf, batch);

        ccp4_file_setmode(filein,0);
	istat = ccp4_file_readchar(filein, hdrrec, MTZRECORDLENGTH); 
        ntok = ccp4_parser(hdrrec, MTZRECORDLENGTH, parser, iprint);
        if (ntok == 4) {
          strcpy(batch->gonlab[0],token[1].fullstring); 
          strcpy(batch->gonlab[1],token[2].fullstring); 
          strcpy(batch->gonlab[2],token[3].fullstring); 
          batch->gonlab[0][9] = batch->gonlab[1][9] = batch->gonlab[2][9] = '\0';
        } else {
          batch->gonlab[0][0] = batch->gonlab[1][0] = batch->gonlab[2][0] = '\0';
	}
      }
    }

    istat = ccp4_file_readchar(filein, hdrrec, MTZRECORDLENGTH);
    ntok = ccp4_parser(hdrrec, MTZRECORDLENGTH, parser, iprint);
  }

  /* Finished with the parser array */
  ccp4_parse_end(parser);

  if (debug) 
    printf(" MtzGet: end of batch pass \n");

  /* Position at start of reflections */
  ccp4_file_setmode(filein,6);
  ccp4_file_seek(filein, SIZE1, SEEK_SET);

  if (read_refs) {

    /* Read all reflections into memory - make this optional? */
    for (i = 0; i < mtz->nref; ++i) {
      MtzRrefl(filein, ntotcol, refldata);
      for (j = 0; j < ntotcol; ++j)
        colin[j]->ref[i] = refldata[j];
    }

    /* Recalculate resolution limits */
    for (i = 0; i < mtz->nxtal; ++i) {
      MtzHklcoeffs(mtz->xtal[i]->cell, coefhkl);
      for (j = 0; j < mtz->nref; ++j) {
        indhigh[0] = (int) mtz->xtal[0]->set[0]->col[0]->ref[j];
        indhigh[1] = (int) mtz->xtal[0]->set[0]->col[1]->ref[j];
        indhigh[2] = (int) mtz->xtal[0]->set[0]->col[2]->ref[j];
        maxres = MtzInd2reso(indhigh, coefhkl);
        if (maxres > mtz->xtal[i]->resmax) mtz->xtal[i]->resmax = maxres;
        if (maxres < mtz->xtal[i]->resmin) mtz->xtal[i]->resmin = maxres;
      }
    }

    /* And close the mtz file: */
    ccp4_file_close(filein);
    mtz->filein = NULL;
  }

  free(filename); 

  return(mtz);}

void MtzArrayToBatch(const int *intbuf, const float *fltbuf, MTZBAT *batch)

{  int i;

  batch->iortyp = intbuf[3];
  for (i = 0; i < 6; ++i)
    batch->lbcell[i] = intbuf[4 + i];
  batch->misflg = intbuf[10];
  batch->jumpax = intbuf[11];
  batch->ncryst = intbuf[12];
  batch->lcrflg = intbuf[13];
  batch->ldtype = intbuf[14];
  batch->jsaxs = intbuf[15];
  batch->nbscal = intbuf[16];
  batch->ngonax = intbuf[17];
  batch->lbmflg = intbuf[18];
  batch->ndet = intbuf[19];
  batch->nbsetid = intbuf[20];

  for (i = 0; i < 6; ++i)
    batch->cell[i] = fltbuf[i];
  for (i = 0; i < 9; ++i)
    batch->umat[i] = fltbuf[6 + i];
  for (i = 0; i < 3; ++i) 
    batch->phixyz[0][i] = fltbuf[15 + i];
  for (i = 0; i < 3; ++i) 
    batch->phixyz[1][i] = fltbuf[18 + i];
  for (i = 0; i < 12; ++i) 
    batch->crydat[i] = fltbuf[21 + i];
  for (i = 0; i < 3; ++i) 
    batch->datum[i] = fltbuf[33 + i];
  batch->phistt = fltbuf[36];
  batch->phiend = fltbuf[37];
  for (i = 0; i < 3; ++i) 
    batch->scanax[i] = fltbuf[38 + i];
  batch->time1 = fltbuf[41];
  batch->time2 = fltbuf[42];
  batch->bscale = fltbuf[43];
  batch->bbfac = fltbuf[44];
  batch->sdbscale = fltbuf[45];
  batch->sdbfac = fltbuf[46];
  batch->phirange = fltbuf[47];
  for (i = 0; i < 3; ++i)
    batch->e1[i] = fltbuf[59 + i];
  for (i = 0; i < 3; ++i)
    batch->e2[i] = fltbuf[62 + i];
  for (i = 0; i < 3; ++i)
    batch->e3[i] = fltbuf[65 + i];
  for (i = 0; i < 3; ++i)
    batch->source[i] = fltbuf[80 + i];
  for (i = 0; i < 3; ++i)
    batch->so[i] = fltbuf[83 + i];
  batch->alambd = fltbuf[86];
  batch->delamb = fltbuf[87];
  batch->delcor = fltbuf[88];
  batch->divhd = fltbuf[89];
  batch->divvd = fltbuf[90];
  for (i = 0; i < batch->ndet; ++i)
  { batch->dx[i] = fltbuf[111 + (i * 6)];
    batch->theta[i] = fltbuf[112 + (i * 6)];
    batch->detlm[i][0][0] = fltbuf[113 + (i * 6)];
    batch->detlm[i][0][1] = fltbuf[114 + (i * 6)];
    batch->detlm[i][1][0] = fltbuf[115 + (i * 6)];
    batch->detlm[i][1][1] = fltbuf[116 + (i * 6)];}

}

void MtzRrefl(CCP4File *filein, int ncol, float *refldata) {

  int istat;

  ccp4_file_setmode(filein,2);
  istat = ccp4_file_read(filein, (uint8 *) refldata, ncol);

}

float MtzInd2reso(const int in[3], const double coefhkl[6]) {

  int ih,ik,il;
  float reso;

  ih = in[0];
  ik = in[1];
  il = in[2];

  reso = (float) 4.0 * (ih*ih*coefhkl[0] + ih*ik*coefhkl[1] + 
             ih*il*coefhkl[2] + ik*ik*coefhkl[3] + 
             ik*il*coefhkl[4] + il*il*coefhkl[5]);

  return reso;

}

void MtzHklcoeffs(const float cell[6], double coefhkl[6]) {

  /* generate coefhkl coefficients from given cell parameters */

  int i;
  double alpha,beta,gamma,degtorad,denom;
  double ax,bx,by,cx,cy,cz;
  double axst,ayst,azst,byst,bzst,czst;

  /* sanity clause (but there ain't no sanity clause!) */
  for (i = 0; i < 6; ++i) 
    coefhkl[i] = 0.0;
  for (i = 0; i < 6; ++i)
    if (cell[i] < 0.001) {
      ccp4_signal(CMTZ_ERRNO(CMTZERR_Cellerr),"MtzHklcoeffs",NULL);
      return;
    }

  degtorad = atan(1.0)/45.0;

  alpha = degtorad*cell[3];
  beta = degtorad*cell[4];
  gamma = degtorad*cell[5];

  /* orthogonal frame for calculation 
   a along x, b in x-y plane */

  ax = cell[0];
  bx = cell[1] * cos(gamma);
  by = cell[1] * sin(gamma);
  cx = cell[2] * cos(beta);
  cy = (cell[1]*cell[2]*cos(alpha) - bx*cx)/by;
  cz = sqrt(cell[2]*cell[2] - cx*cx - cy*cy);

  /* find reciprocal vectors in orthogonal frame */

  denom = ax*by*cz;
  axst = 1.0/ax;
  ayst = -bx*cz/denom;
  azst = (bx*cy - by*cx)/denom;
  byst = 1.0/by;
  bzst = -ax*cy/denom;
  czst = 1.0/cz;

  coefhkl[0] = 0.25*(axst*axst + ayst*ayst + azst*azst);
  coefhkl[1] = 0.5*(ayst*byst + azst*bzst);
  coefhkl[2] = 0.5*(azst*czst);
  coefhkl[3] = 0.25*(byst*byst + bzst*bzst);
  coefhkl[4] = 0.5*(bzst*czst);
  coefhkl[5] = 0.25*(czst*czst);
}

void ccp4_lrtitl(const MTZ *mtz, char ftitle[], size_t *len) {

  size_t length;

  length = strlen(strcpy(ftitle, mtz->title));
  while (ftitle[--length] == ' ');
  *len =  ++length;

}

int ccp4_lrhist(const MTZ *mtz, char history[][MTZRECORDLENGTH]) {

  int i;

  for (i = 0; i < mtz->histlines; ++i) {
    strncpy(history[i],mtz->hist + MTZRECORDLENGTH*i,MTZRECORDLENGTH);
  }

  return mtz->histlines;
}

void ccp4_lrsort(const MTZ *mtz, int isort[5]) {

  int i,j,k,l,icol;

  icol = 0;
  for (i = 0; i < 5; ++i) 
    isort[i] = 0;
  /* Loop over crystals */
  for (i = 0; i < mtz->nxtal; ++i) {
    /* Loop over datasets for each crystal */
    for (j = 0; j < mtz->xtal[i]->nset; ++j) {
      /* Loop over columns for each dataset */
      for (k = 0; k < mtz->xtal[i]->set[j]->ncol; ++k) {
        ++icol;
        for (l = 0; l < 5; ++l) {
          if (mtz->order[l] == mtz->xtal[i]->set[j]->col[k])
            isort[l] = icol;
        }
      }
    }
  }

}

void ccp4_lrbats(const MTZ *mtz, int *nbatx, int batchx[]) {

  int i=0;
  MTZBAT *batch;

  *nbatx = mtz->nbat;
  batch = mtz->batch;
  while (batch != NULL) {
    batchx[i++] = batch->num;
    batch = batch->next;
  }

}

void MtzDebugHierarchy(const MTZ *mtz) {

  int i,j,k;

  if (mtz->filein)
    printf("MtzDebugHierarchy: input file = %s \n",mtz->filein->name);
  if (mtz->fileout)
    printf("MtzDebugHierarchy: output file = %s \n",mtz->fileout->name);

  printf("MtzDebugHierarchy: nxtal = %d \n",mtz->nxtal);
  for (i = 0; i < mtz->nxtal; ++i) {
   printf("MtzDebugHierarchy: xtal = %s, nset = %d \n",mtz->xtal[i]->xname,
              mtz->xtal[i]->nset);
   for (j = 0; j < mtz->xtal[i]->nset; ++j) {
    printf("MtzDebugHierarchy: xtal = %s, set = %s, ncol = %d \n",mtz->xtal[i]->xname,
              mtz->xtal[i]->set[j]->dname,mtz->xtal[i]->set[j]->ncol);
     for (k = 0; k < mtz->xtal[i]->set[j]->ncol; ++k) {
      printf("MtzDebugHierarchy: col = %s (%d) \n",
              mtz->xtal[i]->set[j]->col[k]->label,
              mtz->xtal[i]->set[j]->col[k]->active);
     }
   }
  }

}

/* List of column information: label, type, dataset.
   Returns number of columns in current structure. */
int MtzListColumn(const MTZ *mtz, char clabs[][31], char ctyps[][3], int csetid[]) {

 int i,j,k,icol=0;

 /* Loop over crystals */
   for (i = 0; i < mtz->nxtal; ++i) {
 /* Loop over datasets for each crystal */
    for (j = 0; j < mtz->xtal[i]->nset; ++j) {
 /* Loop over columns for each dataset */
     for (k = 0; k < mtz->xtal[i]->set[j]->ncol; ++k) {
       if (strcmp(mtz->xtal[i]->set[j]->col[k]->type,"Y") == 0 && 
           strcmp(mtz->xtal[i]->set[j]->col[k]->label,"M_ISYM") == 0) {
         strcpy(clabs[icol],"M/ISYM");
       } else {
         strcpy(clabs[icol],mtz->xtal[i]->set[j]->col[k]->label);
       }
       strcpy(ctyps[icol],mtz->xtal[i]->set[j]->col[k]->type);
       csetid[icol] = mtz->xtal[i]->set[j]->setid;
       ++icol;
     }
    }
   }
   return icol;
}

void ccp4_lrcell(const MTZXTAL *xtl, float cell[]) {

  int i;

  for (i = 0; i < 6; ++i) {
    cell[i] = xtl->cell[i];
  }

}

void MtzResLimits(const MTZ *mtz, float *minres, float *maxres) {

  int i;

  *maxres = 0.0;
  *minres = 100.0;
 /* Loop over crystals */
  for (i = 0; i < mtz->nxtal; ++i) {
    if (mtz->xtal[i]->resmax > *maxres) *maxres = mtz->xtal[i]->resmax;
    if (mtz->xtal[i]->resmin < *minres) *minres = mtz->xtal[i]->resmin;
  }

}

void ccp4_lrsymi(const MTZ *mtz, int *nsympx, char *ltypex, int *nspgrx, 
       char *spgrnx, char *pgnamx) {

  *nsympx = mtz->mtzsymm.nsymp;
  *nspgrx = mtz->mtzsymm.spcgrp;
  strcpy(ltypex,&mtz->mtzsymm.symtyp);
  strcpy(spgrnx,mtz->mtzsymm.spcgrpname);
  strcpy(pgnamx,mtz->mtzsymm.pgname);

}

int MtzSpacegroupNumber(const MTZ *mtz)
/* get the spacegroup number (likely CCP4 convention) */
{
  if (!mtz) return 0;
  return mtz->mtzsymm.spcgrp;
}

void ccp4_lrsymm(const MTZ *mtz, int *nsymx, float rsymx[192][4][4]) {

  int i,j,k;

  *nsymx = mtz->mtzsymm.nsym;
  for (i = 0; i < *nsymx; ++i) {
    for (j = 0; j < 4; ++j) {
      for (k = 0; k < 4; ++k) {
        rsymx[i][k][j] = mtz->mtzsymm.sym[i][j][k];
      }
    }
  }

}

int MtzParseLabin(char *labin_line, const char prog_labels[][31], 
            const int nlprgi, char user_labels[][2][31]) 

/* Uses LABIN or LABOUT line to convert program labels to user labels */
/* This is a helper function, but does not access reflection structure at all */
/* Returns the number of program labels matched */
 
{ int i,j,imatch,nlabels=0;
  char label1[31],label2[31];

  /* For cparser */
  CCP4PARSERARRAY *parser=NULL;
  CCP4PARSERTOKEN *token=NULL;
  char *key;
  int ntok,iprint=0;

  parser = ccp4_parse_start(strlen(labin_line));
  if (parser == NULL) {
    ccp4_signal(CMTZ_ERRNO(CMTZERR_ParserFail),"MtzParseLabin",NULL);
    return 0;
  }
  /* Set some convenient pointers to members of the parser array */
  key   = parser->keyword;
  token = parser->token;

  ntok = ccp4_parser(labin_line, strlen(labin_line), parser, iprint);

  if (ccp4_keymatch(key,"LABI")) {
    if (iprint) printf("Interpreting LABIN line.\n");
  } else if (ccp4_keymatch(key,"LABO")) {
    if (iprint) printf("Interpreting LABOUT line.\n");
  } else {
    printf("Input is not LABIN or LABOUT line !!\n");
    return 0;
  }

  /* initialise user labels */
  for (j = 0; j < nlprgi; ++j) {
    strcpy(user_labels[j][0],"");
    strcpy(user_labels[j][1],"");
  }

  for (i = 1; i < ntok; i += 2) {
    strcpy(label1,token[i].fullstring);
    strcpy(label2,token[i+1].fullstring);

    /* check first label against program labels */
    imatch = 0;
    for (j = 0; j < nlprgi; ++j) {
      if (strcmp(label1,prog_labels[j]) == 0) {
        strcpy(user_labels[j][0],label1);
        strcpy(user_labels[j][1],label2);
        imatch = 1;
        ++nlabels;
        break;
      }
    }

    if (imatch == 0) {
    /* check second label against program labels */
      for (j = 0; j < nlprgi; ++j) {
        if (strcmp(label2,prog_labels[j]) == 0) {
          strcpy(user_labels[j][0],label2);
          strcpy(user_labels[j][1],label1);
          imatch = 1;
          ++nlabels;
          break;
        }
      }
    }

    if (imatch == 0) {
      /* no match */
      printf("clkyin: neither label recognised: %s %s \n",label1,label2);
    }
  }

  /* Finished with the parser array */
  ccp4_parse_end(parser);

  return (nlabels);
}

MTZCOL **ccp4_lrassn(const MTZ *mtz, const char labels[][31], const int nlabels, 
             const char types[][3]) 

/* Assigns labels in array labels to file mtz, and returns pointers to columns */
/* Note, this is different from old lrassn, in that any conversion from
   program labels to user labels should have been done. */
 
{
  int ilab;
  char label[31];
  MTZCOL *col, **lookup;

  lookup = (MTZCOL **) ccp4_utils_malloc(nlabels*sizeof(MTZCOL *));

 /* Loop over labels */
   for (ilab = 0; ilab < nlabels; ++ilab) {

     strcpy(label,labels[ilab]);
     /* column not assigned */
     if (label[0] == '\0') {
       lookup[ilab] = NULL;
     } else {
       /* Special trap for M/ISYM */
       if (strcmp(types[ilab],"Y") == 0 && strcmp(label,"M/ISYM") == 0)
         strcpy(label,"M_ISYM");
       col = MtzColLookup(mtz,label);
       if (col != NULL && strncmp(col->type,types[ilab],1) != 0) {
         printf("From ccp4_lrassn: warning: expected type %s does not match file type %s!\n", 
             types[ilab],col->type);
       }

       lookup[ilab] = col;
     }
   }

   return lookup;
}

void ccp4_lridx(const MTZ *mtz, char crystal_name[][64], char dataset_name[][64],
	    char project_name[][64], int isets[], float datcell[][6], 
            float datwave[], int *ndatasets) {

  int iset,i,j,k;

 /* Loop over crystals */
   for (iset = 0, i = 0; i < mtz->nxtal; ++i) {
 /* Loop over datasets for each crystal */
    for (j = 0; j < mtz->xtal[i]->nset; ++j ) {
      strcpy(crystal_name[iset],mtz->xtal[i]->xname);
      strcpy(dataset_name[iset],mtz->xtal[i]->set[j]->dname);
      strcpy(project_name[iset],mtz->xtal[i]->pname);
      isets[iset] = mtz->xtal[i]->set[j]->setid;
      for (k = 0; k < 6; ++k) {
        datcell[iset][k] = mtz->xtal[i]->cell[k];
      }
      datwave[iset] = mtz->xtal[i]->set[j]->wavelength;
      ++iset;
    }
   }
   *ndatasets = iset;
}

int ccp4_lrrefl(const MTZ *mtz, float *resol, float adata[], int logmss[], int iref) {

  int i,j,k,icol;
  int ind[3],ixtal=0;
  unsigned int colin;
  float refldata[MCOLUMNS];
  double coefhkl[6];

  /* If we are past the last reflection, indicate this with return value. */
  if (iref > mtz->nref) 
    return 1;

  /* If reflections not in memory, read next record from file. */
  if (!mtz->refs_in_memory) {
    MtzRrefl( mtz->filein, mtz->ncol_read, refldata);
  }

  icol = -1;
 /* Loop over all columns in the MTZ struct, and select those which
    derive from the input file. */
  for (i = 0; i < mtz->nxtal; ++i) {
    for (j = 0; j < mtz->xtal[i]->nset; ++j) {
     for (k = 0; k < mtz->xtal[i]->set[j]->ncol; ++k) {
       if (colin = mtz->xtal[i]->set[j]->col[k]->source) {
         ++icol;
         if (mtz->refs_in_memory) {
           adata[icol] = mtz->xtal[i]->set[j]->col[k]->ref[iref-1];
         } else {
           adata[icol] = refldata[colin - 1];
	 }
         logmss[icol] = ccp4_ismnf(mtz, adata[icol]);
       }
     }
    }
  }

  /* calculate resolution of this reflection, based on cell of 
     first crystal */
  ind[0] = (int) adata[0];
  ind[1] = (int) adata[1];
  ind[2] = (int) adata[2];
  MtzHklcoeffs(mtz->xtal[ixtal]->cell, coefhkl);
  *resol = MtzInd2reso(ind, coefhkl);
  /* kludge taken from mtzlib.f */
  if (*resol > mtz->xtal[ixtal]->resmax) *resol = mtz->xtal[ixtal]->resmax;
  if (*resol < mtz->xtal[ixtal]->resmin) *resol = mtz->xtal[ixtal]->resmin;

  return 0;
}

int ccp4_lrreff(const MTZ *mtz, float *resol, float adata[], int logmss[],
   const MTZCOL *lookup[], const int ncols, const int iref) {

  int icol;
  int ind[3],ixtal=0;
  unsigned int colin;
  float refldata[MCOLUMNS];
  double coefhkl[6];
  union float_uint_uchar uf;

  /* If we are past the last reflection, indicate this with return value. */
  if (iref > mtz->nref) 
    return 1;

  /* If reflections not in memory, read next record from file. */
  if (!mtz->refs_in_memory) {
    MtzRrefl( mtz->filein, mtz->ncol_read, refldata);
  }

  if (strncmp (mtz->mnf.amnf,"NAN",3) == 0) {
    uf = ccp4_nan();
  } else {
    uf.f = mtz->mnf.fmnf;
  }

  /* loop over columns requested in lookup array. */
  for (icol=0; icol < ncols; icol++) {
    logmss[icol] = 1;
    if (lookup[icol]) {
      if (mtz->refs_in_memory) {
        adata[icol] = lookup[icol]->ref[iref-1];
        logmss[icol] = ccp4_ismnf(mtz, adata[icol]);
      } else {
         if (colin = lookup[icol]->source) {
           adata[icol] = refldata[colin - 1];
           logmss[icol] = ccp4_ismnf(mtz, adata[icol]);
	 } else {
           adata[icol] = uf.f;
           logmss[icol] = 1;
	 }
      }
    }
  }

  ind[0] = (int) adata[0];
  ind[1] = (int) adata[1];
  ind[2] = (int) adata[2];
  MtzHklcoeffs(mtz->xtal[ixtal]->cell, coefhkl);
  *resol = MtzInd2reso(ind, coefhkl);
  /* kludge taken from mtzlib.f */
  if (*resol > mtz->xtal[ixtal]->resmax) *resol = mtz->xtal[ixtal]->resmax;
  if (*resol < mtz->xtal[ixtal]->resmin) *resol = mtz->xtal[ixtal]->resmin;

  return 0;
}

int ccp4_ismnf(const MTZ *mtz, const float datum) {

  if (strncmp (mtz->mnf.amnf,"NAN",3) == 0) {
    return ccp4_utils_isnan((union float_uint_uchar *) &datum);
  } else {
    if (datum == mtz->mnf.fmnf)
        return 1;
  }
  return 0;
}

void ccp4_lhprt(const MTZ *mtz, int iprint) {

  int i,j,k,isort[5];
  float maxres=0.0,minres=100.0;
  char buffer[MTZRECORDLENGTH+1],symline[81];

  if (iprint <= 0) return;

  printf(" * Title:\n\n");
  printf(" %s\n\n",mtz->title);
  printf(" * Number of Datasets = %d\n\n",MtzNumActiveSet(mtz));
  printf(" * Dataset ID, project/crystal/dataset names, cell dimensions, wavelength:\n\n");
 /* Loop over crystals */
  for (i = 0; i < mtz->nxtal; ++i) {
 /* Loop over datasets for each crystal */
    for (j = 0; j < mtz->xtal[i]->nset; ++j) {
      /* check if dataset contains any active columns */
      if ( (MtzNumActiveColsInSet(mtz->xtal[i]->set[j]) == 0) &&
           (MtzNbatchesInSet(mtz,mtz->xtal[i]->set[j]) == 0) ) continue;
      printf(" %8d %s\n",mtz->xtal[i]->set[j]->setid,mtz->xtal[i]->pname);
      printf("          %s\n",mtz->xtal[i]->xname);
      printf("          %s\n",mtz->xtal[i]->set[j]->dname);
      printf("          %10.4f%10.4f%10.4f%10.4f%10.4f%10.4f\n",
        mtz->xtal[i]->cell[0],mtz->xtal[i]->cell[1],mtz->xtal[i]->cell[2],
        mtz->xtal[i]->cell[3],mtz->xtal[i]->cell[4],mtz->xtal[i]->cell[5]);
      printf("          %10.5f\n",mtz->xtal[i]->set[j]->wavelength);
    }
  }
  printf("\n * Number of Columns = %d\n\n",MtzNumActiveCol(mtz));
  printf(" * Number of Reflections = %d\n\n",mtz->nref);
  if (strncmp (mtz->mnf.amnf,"NAN",3) == 0) {
   printf(" * Missing value set to NaN in input mtz file\n\n");
  } else {
   printf(" * Missing value set to %f in input mtz file\n\n",mtz->mnf.fmnf);
  }
  if (MtzNbat(mtz) > 0)
    printf(" * Number of Batches = %d\n\n",MtzNbat(mtz));

  if (iprint == 2 || iprint == 3) {
    printf(" * HISTORY for current MTZ file :\n\n");
    for (i = 0; i < mtz->histlines; ++i) {
      strncpy(buffer,mtz->hist + MTZRECORDLENGTH*i,MTZRECORDLENGTH);
      buffer[MTZRECORDLENGTH] = '\0';
      printf(" %s\n",buffer);
    }
    printf("\n");
  }

  if (iprint == 1 || iprint == 2 || iprint >=4 ) {

  printf(" * Column Labels :\n\n");
 /* Loop over crystals */
  for (i = 0; i < mtz->nxtal; ++i) {
 /* Loop over datasets for each crystal */
   for (j = 0; j < mtz->xtal[i]->nset; ++j) {
 /* Loop over columns for each dataset */
    for (k = 0; k < mtz->xtal[i]->set[j]->ncol; ++k) {
     if (mtz->xtal[i]->set[j]->col[k]->active) {
      if (strcmp(mtz->xtal[i]->set[j]->col[k]->type,"Y") == 0 && 
         strcmp(mtz->xtal[i]->set[j]->col[k]->label,"M_ISYM") == 0) {
       printf(" M/ISYM");
      } else {
       printf(" %s",mtz->xtal[i]->set[j]->col[k]->label);
      }
     }
    }
   }
  }
  printf("\n\n * Column Types :\n\n");
 /* Loop over crystals */
  for (i = 0; i < mtz->nxtal; ++i) {
 /* Loop over datasets for each crystal */
   for (j = 0; j < mtz->xtal[i]->nset; ++j) {
 /* Loop over columns for each dataset */
    for (k = 0; k < mtz->xtal[i]->set[j]->ncol; ++k) {
     if (mtz->xtal[i]->set[j]->col[k]->active) 
      printf(" %s",mtz->xtal[i]->set[j]->col[k]->type);
    }
   }
  }
  printf("\n\n * Associated datasets :\n\n");
 /* Loop over crystals */
  for (i = 0; i < mtz->nxtal; ++i) {
 /* Loop over datasets for each crystal */
   for (j = 0; j < mtz->xtal[i]->nset; ++j) {
 /* Loop over columns for each dataset */
    for (k = 0; k < mtz->xtal[i]->set[j]->ncol; ++k) {
     if (mtz->xtal[i]->set[j]->col[k]->active) 
      printf(" %d",mtz->xtal[i]->set[j]->setid);
    }
   }
  }

  } else if ( iprint == 3 ) {

  printf(" * Column Labels, Types, Ranges [and Dataset IDs] :\n\n");

  /* Loop over crystals/datasets/columns */
  for (i = 0; i < mtz->nxtal; ++i) {
   for (j = 0; j < mtz->xtal[i]->nset; ++j) {
    for (k = 0; k < mtz->xtal[i]->set[j]->ncol; ++k) {
     if (mtz->xtal[i]->set[j]->col[k]->active) 
      if (strcmp(mtz->xtal[i]->set[j]->col[k]->type,"Y") == 0 && 
         strcmp(mtz->xtal[i]->set[j]->col[k]->label,"M_ISYM") == 0) {
       printf(" M/ISYM                         %2s %19.4f %19.4f %8d \n",
         mtz->xtal[i]->set[j]->col[k]->type,
         mtz->xtal[i]->set[j]->col[k]->min,mtz->xtal[i]->set[j]->col[k]->max,
         mtz->xtal[i]->set[j]->setid);
      } else {
       printf(" %-30s %2s %19.4f %19.4f %8d \n",
         mtz->xtal[i]->set[j]->col[k]->label,mtz->xtal[i]->set[j]->col[k]->type,
         mtz->xtal[i]->set[j]->col[k]->min,mtz->xtal[i]->set[j]->col[k]->max,
         mtz->xtal[i]->set[j]->setid);
      }
    }
   }
  }

  }

  /* write overall cell - just for scripts which grep for this */
  printf("\n\n * Cell Dimensions (obsolete - use crystal cells):\n\n");
  printf(" %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f \n\n",
        mtz->xtal[0]->cell[0],mtz->xtal[0]->cell[1],mtz->xtal[0]->cell[2],
        mtz->xtal[0]->cell[3],mtz->xtal[0]->cell[4],mtz->xtal[0]->cell[5]);

  /* Calculate overall  resolution limits */
  for (i = 0; i < mtz->nxtal; ++i) {
      if (mtz->xtal[i]->resmax > maxres) maxres = mtz->xtal[i]->resmax;
      if (mtz->xtal[i]->resmin < minres) minres = mtz->xtal[i]->resmin;
  }
  printf(" * Resolution Range :\n\n");
  printf(" %10.5f %10.5f     ( %10.3f - %10.3f A )\n\n",
       minres,maxres,1.0/sqrt(minres),1.0/sqrt(maxres));
  ccp4_lrsort(mtz, isort);
  printf(" * Sort Order : %d %d %d %d %d\n\n",isort[0],isort[1],isort[2],
       isort[3],isort[4]);

  if (iprint == 3 || iprint == 4 ) {

    printf(" * Number of Symmetry Operations = %d \n",mtz->mtzsymm.nsym);
    printf(" * Number of Primitive Operations = %d \n",mtz->mtzsymm.nsymp);
    printf(" * Space Group = %d %s \n",mtz->mtzsymm.spcgrp,mtz->mtzsymm.spcgrpname);
    printf(" * Lattice Type = %c \n",mtz->mtzsymm.symtyp);
    printf(" * Point Group Name = %s \n",mtz->mtzsymm.pgname);

    printf("\n * Symmetry Operations : \n\n");
    for (i = 0; i < mtz->mtzsymm.nsym; ++i) {
      mat4_to_symop(symline,symline+80,mtz->mtzsymm.sym[i]);
      symline[60] = '\0';
      printf(" Symmetry %d %s\n",i+1,symline);
      for (j = 0; j < 4; ++j) 
        printf(" %5.2f %5.2f %5.2f %5.2f \n",mtz->mtzsymm.sym[i][j][0],
           mtz->mtzsymm.sym[i][j][1],mtz->mtzsymm.sym[i][j][2],
	       mtz->mtzsymm.sym[i][j][3]);
    }

  } else {
    printf(" * Space group = %s (number     %d)\n\n",mtz->mtzsymm.spcgrpname,
       mtz->mtzsymm.spcgrp);
  }
}

void ccp4_lhprt_adv(const MTZ *mtz, int iprint) {

  int i,j,k;
  char buffer[MTZRECORDLENGTH+1];

  printf(" HEADER INFORMATION FROM MTZ FILE \n\n");

  printf(" * File information :\n\n");

  printf("%s       %s\n",MTZTITLE,mtz->title);
  printf("%s       %d\n",MTZSPACEGROUP,mtz->mtzsymm.spcgrp);
  printf("%s       %d\n",MTZNUMREFLS,mtz->nref);
  if (strncmp (mtz->mnf.amnf,"NAN",3) == 0) {
   printf("%s       %s\n",MTZMNF,"NaN");
  } else {
   printf("%s       %f\n",MTZMNF,mtz->mnf.fmnf);
  }
  printf("%s       %s\n",MTZSORTORDER,"(not implemented)");

  printf("\n * Crystals, datasets :\n");
 /* Loop over crystals */
  for (i = 0; i < mtz->nxtal; ++i) {

    printf("\n%s       %s\n",CRYSTALXTALNAME,mtz->xtal[i]->xname);
    printf("%s       %s\n",CRYSTALPNAME,mtz->xtal[i]->pname);
    printf("%s       %10.4f%10.4f%10.4f%10.4f%10.4f%10.4f\n",CRYSTALCELL,
        mtz->xtal[i]->cell[0],mtz->xtal[i]->cell[1],mtz->xtal[i]->cell[2],
        mtz->xtal[i]->cell[3],mtz->xtal[i]->cell[4],mtz->xtal[i]->cell[5]);

 /* Loop over datasets for each crystal */
    for (j = 0; j < mtz->xtal[i]->nset; ++j) {
      printf("\n    %s       %s\n",DATASETDNAME,mtz->xtal[i]->set[j]->dname);
      printf("    %s       %10.5f\n",DATASETWAVELENGTH,mtz->xtal[i]->set[j]->wavelength);
      if (mtz->xtal[i]->set[j]->ncol > 0) {
        printf("\n        %s %s\n",COLUMNLABEL,COLUMNTYPE);
 /* Loop over columns for each dataset */
        for (k = 0; k < mtz->xtal[i]->set[j]->ncol; ++k) {
          printf("                     %-31s %-3s\n",mtz->xtal[i]->set[j]->col[k]->label,
                                       mtz->xtal[i]->set[j]->col[k]->type);
        }
      }
    }
  }

  printf("\n * HISTORY for current MTZ file :\n\n");
  for (i = 0; i < mtz->histlines; ++i) {
    strncpy(buffer,mtz->hist + MTZRECORDLENGTH*i,MTZRECORDLENGTH);
    buffer[MTZRECORDLENGTH] = '\0';
    printf(" %s\n",buffer);
  }

}

void ccp4_lrbat(MTZBAT *batch, float *buf, char *charbuf, int iprint)

{ int nwords=NBATCHWORDS,nintegers=NBATCHINTEGERS,nreals=NBATCHREALS;
  int *intbuf = (int *) buf;
  float *fltbuf = buf + NBATCHINTEGERS;

  intbuf[0] = nwords;
  intbuf[1] = nintegers;
  intbuf[2] = nreals;

  MtzBatchToArray(batch,intbuf,fltbuf);

  strncpy(charbuf,batch->title,70); 
  strncpy(charbuf+70,batch->gonlab[0],8); 
  strncpy(charbuf+78,batch->gonlab[1],8); 
  strncpy(charbuf+86,batch->gonlab[2],8); 

  if (iprint > 0) {
    MtzPrintBatchHeader(batch);
  }
}

void MtzPrintBatchHeader(MTZBAT *batch) {

  int i;
  char labtype[20],axes[5],string1[40],string2[40];

  switch (batch->ldtype) {
  case 1:
    strcpy(labtype,"oscillation data");
    break;
  case 2:
    strcpy(labtype,"area detector data");
    break;
  case 3:
    strcpy(labtype,"Laue data");
    break;
  default:
    strcpy(labtype,"*** unknown data type ***");
  }

  switch (batch->jumpax) {
  case 1:
    strcpy(axes,"a*");
    break;
  case 2:
    strcpy(axes,"b*");
    break;
  case 3:
    strcpy(axes,"c*");
    break;
  default:
    strcpy(axes,"none");
  }

  printf(" Batch number: \n");
  printf(" %6d    %s\n",batch->num,batch->title);
  printf("\n %s \n\n %s %7d     %s  \n\n %s %7d\n %s %7d\n %s %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f\n %s %7d %7d %7d %7d %7d %7d \n",
	 "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
	 "Orientation data for batch",batch->num,labtype,
	 "  Crystal number ...................",batch->ncryst,
         "  Associated dataset ID ............",batch->nbsetid,
         "  Cell dimensions ..................",
         batch->cell[0],batch->cell[1],batch->cell[2],
         batch->cell[3],batch->cell[4],batch->cell[5],
         "  Cell fix flags ...................",
         batch->lbcell[0],batch->lbcell[1],batch->lbcell[2],
         batch->lbcell[3],batch->lbcell[4],batch->lbcell[5]);
  if (!batch->misflg) {
    strcpy(string1,"Orientation matrix U .............");
    strcpy(string2,"    (including setting angles)    ");
  } else {
    strcpy(string1,"Standard orientation matrix U ....");
    strcpy(string2,"                                  ");
  }    
  printf("   %s %9.4f %9.4f %9.4f \n   %s %9.4f %9.4f %9.4f \n   %s %9.4f %9.4f %9.4f \n",
         string1,batch->umat[0],batch->umat[1],batch->umat[2],
         string2,batch->umat[3],batch->umat[4],batch->umat[5],
         "                                  ",batch->umat[6],batch->umat[7],batch->umat[8]);
  if (batch->misflg) {
  printf("   %s %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f\n",
         "Missetting angles PhiX PhiY PhiZ..",
         batch->phixyz[0][0],batch->phixyz[0][1],batch->phixyz[0][2],
         batch->phixyz[1][0],batch->phixyz[1][1],batch->phixyz[1][2]);
  }
  printf("   %s%s%s   %s\n",
         "Reciprocal axis nearest ",batch->gonlab[0],"..",axes);
  if (!batch->lcrflg) {
    printf("   %s %6.3f \n",
	   "Mosaicity ........................",batch->crydat[0]);
  } else {
    printf("   %s %6.3f %6.3f \n",
           "Mosaicity (horizontal, vertical)..",batch->crydat[0],batch->crydat[1]);
  }
  printf("   %s %7.3f %8.3f %8.3f \n",
	 "Datum goniostat angles (degrees)..",batch->datum[0],batch->datum[1],batch->datum[2]);
  if (batch->jsaxs > 0 && batch->jsaxs <= batch->ngonax) 
    printf("   %s  %s \n",
	 "Scan axis ........................",batch->gonlab[batch->jsaxs-1]);
  printf("   %s %8.3f %8.3f \n   %s %8.3f \n   %s %8.2f %8.2f \n",
	 "Start & stop Phi angles (degrees).",batch->phistt,batch->phiend,
	 "Range of Phi angles (degrees).....",batch->phirange,
         "Start & stop time (minutes).......",batch->time1,batch->time2);

  if (batch->nbscal == 4) {
    printf("   %s %9.4f %9.4f \n   %s %9.4f %9.4f \n",
	   "   Batch scale & SD .................",batch->bscale,batch->sdbscale,
	   "   Batch B-factor & SD ..............",batch->bbfac,batch->sdbfac);
  }

  printf("   %s  \n   %s %7d \n   %s %s %s %9.4f %9.4f %9.4f \n   %s %s %s %9.4f %9.4f %9.4f \n   %s %s %s %9.4f %9.4f %9.4f \n",
         " Crystal goniostat information :-",
	 "   Number of goniostat axes..........",batch->ngonax,
	 "   Goniostat vectors.....",batch->gonlab[0],"....",batch->e1[0],batch->e1[1],batch->e1[2],
	 "                    .....",batch->gonlab[1],"....",batch->e2[0],batch->e2[1],batch->e2[2],
	 "                    .....",batch->gonlab[2],"....",batch->e3[0],batch->e3[1],batch->e3[2]);

  printf("   %s \n   %s  %9.4f %9.4f %9.4f \n   %s %9.4f %9.4f %9.4f \n",
	 " Beam information :-",
	 "   Idealized X-ray beam vector.......",batch->source[0],batch->source[1],batch->source[2],
	 "   X-ray beam vector with tilts......",batch->so[0],batch->so[1],batch->so[2]);

  if (batch->lbmflg == 0) {
    printf("   %s %9.5f %9.5f \n",
           "   Wavelength and dispersion ........",batch->alambd,batch->delamb);
  } else if (batch->lbmflg == 1) {
    printf("   %s %9.5f %9.5f %9.5f \n   %s %7.3f %7.3f \n",
	   "   Wavelength and dispersion ........",batch->alambd,batch->delamb,batch->delcor,
	   "   Divergence .......................",batch->divhd,batch->divvd);
  }

  /* sorry, bored now ... */

}

void ccp4_lwtitl(MTZ *mtz, char ftitle[], int flag) {

  size_t length;

  if (flag == 0) {

    strncpy(mtz->title,ftitle,71);

  } else {

    length = strlen(mtz->title);
    while (mtz->title[--length] == ' ');
    mtz->title[length+1] = ' ';
    strncpy(mtz->title+length+2,ftitle,69-length);

  }

}

void MtzSetSortOrder(MTZ *mtz, MTZCOL *colsort[5]) {

  int i;

  for (i = 0; i < 5; ++i) 
    if (colsort[i]) mtz->order[i] = colsort[i];

}

int MtzAddHistory(MTZ *mtz, const char history[][MTZRECORDLENGTH], const int nlines) {

  int i;
  char *newhist;

  newhist = MtzCallocHist(mtz->histlines + nlines);
  /* write new history lines */
  for (i = 0; i < nlines; ++i) {
    strncpy(newhist + MTZRECORDLENGTH*i,history[i],MTZRECORDLENGTH);
  }
  /* copy old history lines */
  for (i = 0; i < mtz->histlines; ++i) {
    strncpy(newhist + MTZRECORDLENGTH*nlines + MTZRECORDLENGTH*i,
        mtz->hist + MTZRECORDLENGTH*i,MTZRECORDLENGTH);
  }
  MtzFreeHist(mtz->hist);
  mtz->hist = newhist;
  mtz->histlines += nlines;

  return mtz->histlines;
}

void ccp4_lwidx(MTZ *mtz, const char crystal_name[],  const char dataset_name[],
	    const char project_name[], float datcell[6], float *datwave) {

  MTZXTAL *xtl;
  MTZSET *set;
  int i;
  char path1[200];

  /* Is it a new crystal? */
  if ((xtl = MtzXtalLookup(mtz,crystal_name)) == NULL) {
    xtl = MtzAddXtal(mtz,crystal_name,project_name,datcell);
    MtzAddDataset(mtz,xtl,dataset_name,*datwave);
  } else {
    /* Existing crystal - update parameters */
    strcpy(xtl->pname,project_name);
    if (datcell[0] > 0.0)
      for (i = 0; i < 6; ++i) 
        xtl->cell[i] = datcell[i];
    strcpy( path1, "/" );
    strcat( path1, xtl->xname );
    strcat( path1, "/" );
    strcat( path1, dataset_name );
    /* Is it a new dataset? */
    if ((set = MtzSetLookup(mtz,path1)) == NULL) {
      MtzAddDataset(mtz,xtl,dataset_name,*datwave);
    } else {
      if (*datwave > 0.0)
        set->wavelength = *datwave;
    }
  }
}

void MtzAssignColumn(MTZ *mtz, MTZCOL *col, const char crystal_name[],  
     const char dataset_name[]) 
{

  MTZXTAL *xtl;
  MTZSET *set, *oldset;
  int i,j;
  float datcell[6] = {0.0}, datwave = 0.0;
  char path1[200], *path2;

  if ( !mtz || !col || !crystal_name || !dataset_name || 
       !strcmp(crystal_name,"") || !strcmp(dataset_name,"") )
      ccp4_signal(CMTZ_ERRNO(CMTZERR_ParamError),"MtzAssignColumn",NULL);

  /* if column already belongs in this dataset, do nothing and return */
  oldset = MtzColSet(mtz, col);
  path2 = MtzSetPath(mtz, oldset);
  strcpy( path1, "/" );
  strcat( path1, crystal_name );
  strcat( path1, "/" );
  strcat( path1, dataset_name );
  if ( MtzPathMatch( path1, path2 ) ) {
    free (path2);
    return;
  }
  free (path2);

  /* remove column from existing set */
  for (i = 0; i < oldset->ncol; ++i) {
    if ( oldset->col[i] == col ) {
      for (j = i; j < oldset->ncol - 1; ++j) 
	oldset->col[j] = oldset->col[j+1];
      oldset->col[oldset->ncol--] = NULL;
      break;
    }
  }

  /* Does the requested new dataset exist? If not, create it. */
  if ( !(set = MtzSetLookup(mtz,path1)) ) {
    if ( !(xtl = MtzXtalLookup(mtz,crystal_name)) ) 
      xtl = MtzAddXtal(mtz,crystal_name,crystal_name,datcell);
    set = MtzAddDataset(mtz,xtl,dataset_name,datwave);
  }

  /* Add column to new dataset */
  set->col[set->ncol++] = col;

}

void ccp4_lwsymm(MTZ *mtz, int *nsymx, int *nsympx, float rsymx[192][4][4], 
   char ltypex[], int *nspgrx, char spgrnx[], char pgnamx[])
{
  int i,j,k;

  mtz->mtzsymm.nsym = *nsymx;
  mtz->mtzsymm.nsymp = *nsympx;
  for (i = 0; i < *nsymx; ++i) {
    for (j = 0; j < 4; ++j) {
      for (k = 0; k < 4; ++k) {
        mtz->mtzsymm.sym[i][k][j] = rsymx[i][j][k];
      }
    }
  }
  strcpy(&mtz->mtzsymm.symtyp,ltypex);
  mtz->mtzsymm.spcgrp = *nspgrx;
  ccp4spg_to_shortname(mtz->mtzsymm.spcgrpname,spgrnx);
  strcpy(mtz->mtzsymm.pgname,pgnamx);

}

MTZCOL **ccp4_lwassn(MTZ *mtz, const char labels[][31], const int nlabels, 
             const char types[][3], const int iappnd) 

/* Assign columns for writing. Check to see if columns already exist,
   else create them. If iappnd = 0, then deactivate columns which are
   not selected (allows one to write out a subset of columns) */

{
  int i,j,k,ilab;
  MTZCOL *col, **lookup;

  lookup = (MTZCOL **) ccp4_utils_malloc(nlabels*sizeof(MTZCOL *));

  /* if iappnd = 0, deactivate existing columns */
  if (iappnd == 0) {
 /* Loop over crystals */
   for (i = 0; i < mtz->nxtal; ++i) {
 /* Loop over datasets for each crystal */
    for (j = 0; j < mtz->xtal[i]->nset; ++j) {
 /* Loop over columns for each dataset */
     for (k = 0; k < mtz->xtal[i]->set[j]->ncol; ++k) {
       mtz->xtal[i]->set[j]->col[k]->active = 0;
     }
    }
   }
  } 

  /* Loop over labels */
  for (ilab = 0; ilab < nlabels; ++ilab) {
    if (strcmp(types[ilab],"Y") == 0 && strcmp(labels[ilab],"M/ISYM") == 0) {
      col = MtzColLookup(mtz,"M_ISYM");
    } else {
      col = MtzColLookup(mtz,labels[ilab]);
    }
    if (col) {
      col->active = 1;
      lookup[ilab] = col;
    } else {
      /* add new column to first dataset - MtzAssignColumn corrects this */
      if (strcmp(types[ilab],"Y") == 0 && strcmp(labels[ilab],"M/ISYM") == 0) {
        lookup[ilab] = MtzAddColumn(mtz, mtz->xtal[0]->set[0], 
                      "M/ISYM", types[ilab]);
      } else {
        lookup[ilab] = MtzAddColumn(mtz, mtz->xtal[0]->set[0], 
                      labels[ilab], types[ilab]);
      }
    }
  }

  return lookup;
}

void ccp4_lwbat(MTZ *mtz, MTZBAT *batch, const int batno, const float *buf, const char *charbuf)

/* write new batch information to 'batch' or if 'batch' is NULL create   */
/* new batch header with batch number 'batno'                           */
/* don't update mtz->nbat - input no. of batches - use MtzNbat instead */

{  int i;

  int *intbuf = (int *) buf;
  const float *fltbuf = buf + NBATCHINTEGERS;
  char cbatch[95]=" ";
  int cbatch_len;
  MTZBAT *otherbat;

  if (batch == NULL) {
    /* add new batch at end of list */
    batch = mtz->batch;
    if (batch == NULL) {
      mtz->batch = MtzMallocBatch();
      batch = mtz->batch;
      batch->num = batno;
      batch->next = NULL;
    } else {
      if (batch->num == batno) {
        printf("From ccp4_lwbat: warning: attempt to add new batch with existing batch number %d!\n",batno);
        return;
      }
      while (batch->next != NULL) {
        batch = batch->next;
        if (batch->num == batno) {
          printf("From ccp4_lwbat: warning: attempt to add new batch with existing batch number %d!\n",batno);
          return;
        }
      }
      batch->next = MtzMallocBatch();
      batch = batch->next;
      batch->num = batno;
      batch->next = NULL;
    }
  } else {
    if (batch->num != batno) {
      /* renumbering - check unique */
      otherbat = mtz->batch;
      while (otherbat != NULL) {
        if (otherbat->num == batno && otherbat != batch) {
          printf("From ccp4_lwbat: warning: attempt to change batch number to existing batch number %d!\n",batno);
          return;
        }
        otherbat = otherbat->next;
      }
      batch->num = batno;
    }
  }

  MtzArrayToBatch(intbuf,fltbuf,batch);

  cbatch_len = ( strlen(charbuf) < 94 ) ? strlen(charbuf) : 94;
  strncpy(cbatch,charbuf,cbatch_len);

  strncpy(batch->title,cbatch,70); 
  strncpy(batch->gonlab[0],cbatch+70,8); 
  strncpy(batch->gonlab[1],cbatch+78,8); 
  strncpy(batch->gonlab[2],cbatch+86,8); 
  batch->gonlab[0][9] = batch->gonlab[1][9] = batch->gonlab[2][9] = '\0';

}

void ccp4_lwbsetid(MTZ *mtz, MTZBAT *batch, const char xname[], const char dname[])

{
  MTZXTAL *xtl;
  MTZSET *set;
  char path1[200];

  if ((xtl = MtzXtalLookup(mtz,xname)) != NULL) {
    strcpy( path1, "/" );
    strcat( path1, xtl->xname );
    strcat( path1, "/" );
    strcat( path1, dname );
    if ((set = MtzSetLookup(mtz,path1)) != NULL) {
      batch->nbsetid = set->setid;
      return;
    }
  }
  printf("From ccp4_lwbsetid: warning: dataset id not found!\n");

}

void ccp4_lwrefl(MTZ *mtz, const float adata[], MTZCOL *lookup[], 
           const int ncol, const int iref)

{ int i,j,k,l,icol,ind[3];
  float refldata[MCOLUMNS],res;
  double coefhkl[6];

  /* if this is extra reflection, check memory */
  if (mtz->refs_in_memory && iref > mtz->nref) {
    if (iref > ccp4array_size(lookup[0]->ref)) {
     /* Loop over crystals */
      for (i = 0; i < mtz->nxtal; ++i) {
     /* Loop over datasets for each crystal */
       for (j = 0; j < mtz->xtal[i]->nset; ++j) {
      /* Loop over columns for each dataset */
        for (k = 0; k < mtz->xtal[i]->set[j]->ncol; ++k) {
         ccp4array_resize(mtz->xtal[i]->set[j]->col[k]->ref, iref);
        }
       }
      }
    }
  }

  icol = -1;
  for (i = 0; i < ncol; ++i) {
    if (lookup[i]) {
      /* update reflection in memory or add to refldata array. */
      if (mtz->refs_in_memory) {
        lookup[i]->ref[iref-1] = adata[i];
      } 
      /* update column ranges */
      if (!ccp4_ismnf(mtz, adata[i])) {
        if (adata[i] < lookup[i]->min) lookup[i]->min = adata[i];
        if (adata[i] > lookup[i]->max) lookup[i]->max = adata[i];
      }
    }
  }

  if (!mtz->refs_in_memory) {

    icol = -1;
    /* Loop over all active columns */
    for (i = 0; i < mtz->nxtal; ++i) 
     for (j = 0; j < mtz->xtal[i]->nset; ++j) 
      for (k = 0; k < mtz->xtal[i]->set[j]->ncol; ++k) 
        if (mtz->xtal[i]->set[j]->col[k]->active) {
          ++icol;
          /* for each active column, see if value to write */
          for (l = 0; l < ncol; ++l) 
            if (lookup[l] == mtz->xtal[i]->set[j]->col[k]) {
              refldata[icol] = adata[l];
	      break;
	    }
	}

     MtzWrefl(mtz->fileout, icol+1, refldata);

     /* Update resolution limits. For in-memory mode, this is done in MtzPut. */
     for (i = 0; i < mtz->nxtal; ++i) {
       MtzHklcoeffs(mtz->xtal[i]->cell, coefhkl);
       ind[0] = (int) refldata[0];
       ind[1] = (int) refldata[1];
       ind[2] = (int) refldata[2];
       res = MtzInd2reso(ind, coefhkl);
       if (res > 0.0) {
         if (res > mtz->xtal[i]->resmax) mtz->xtal[i]->resmax = res;
         if (res < mtz->xtal[i]->resmin) mtz->xtal[i]->resmin = res;
       }
     }
  }

  /* increment nref if we are adding new reflections */
  if (iref > mtz->nref)
    mtz->nref = iref;
}

void MtzPut(MTZ *mtz, const char *logname)

{ char hdrrec[81],symline[81];
 CCP4File *fileout;
 int i, j, k, l, hdrst, icol, numbat, isort[5], debug=0;
 int ind[3],ind_xtal,ind_set,ind_h_col,ind_k_col,ind_l_col;
 double coefhkl[6];
 float maxres=0.0,minres=100.0,res,refldata[200];
 int nwords=NBATCHWORDS,nintegers=NBATCHINTEGERS,nreals=NBATCHREALS;
 float buf[NBATCHWORDS];
 int *intbuf = (int *) buf;
 float *fltbuf = buf + NBATCHINTEGERS;
 MTZBAT *batch;

 if (debug) 
   printf(" MtzPut: entering \n");

 if (!mtz->fileout) {

   hdrst = mtz->nref * MtzNumActiveCol(mtz) + SIZE1 + 1;
   fileout = MtzOpenForWrite(logname, &hdrst);

   if (debug) 
     printf(" MtzPut: file opened \n");

 } else {
   fileout = mtz->fileout;
 }

 if (mtz->refs_in_memory) {

   /* Write all reflections from memory - make this optional? */
   for (l = 0; l < mtz->nref; ++l) {
     icol = 0;
   /* Loop over crystals */
     for (i = 0; i < mtz->nxtal; ++i) {
   /* Loop over datasets for each crystal */
      for (j = 0; j < mtz->xtal[i]->nset; ++j) {
   /* Loop over columns for each dataset */
       for (k = 0; k < mtz->xtal[i]->set[j]->ncol; ++k) {
         if (mtz->xtal[i]->set[j]->col[k]->active) {
           refldata[icol++] = mtz->xtal[i]->set[j]->col[k]->ref[l];
         }
       }
      }
     }
     MtzWrefl(fileout, icol, refldata);
   }

   if (debug) 
     printf(" MtzPut: reflections written \n");

 }

 ccp4_file_setmode(fileout,0);
 /* Write header */
 sprintf(hdrrec,"VERS %8s",MTZVERSN);
 MtzWhdrLine(fileout,13,hdrrec);
 strcpy(hdrrec,"TITLE ");
 strncpy(hdrrec+6,mtz->title,70);
 MtzWhdrLine(fileout,76,hdrrec);
 /* if new batch headers have been written, lose the old ones */
 /* mtz->nbat is original number of batches, MtzNbat(mtz) the current */
 if (MtzNbat(mtz) == mtz->nbat) {
   numbat = mtz->nbat;
 } else {
   numbat = MtzNbat(mtz) - mtz->nbat;
 }
 sprintf(hdrrec,"NCOL %8d %12d %8d",MtzNumActiveCol(mtz),mtz->nref,numbat);
 MtzWhdrLine(fileout,35,hdrrec);
 if (debug) printf(" MtzPut: NCOL just written \n");

 /* Purely for backwards compatibility */
 if (mtz->xtal[0] != NULL) {
   sprintf(hdrrec,"CELL  %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f",mtz->xtal[0]->cell[0],
           mtz->xtal[0]->cell[1],mtz->xtal[0]->cell[2],mtz->xtal[0]->cell[3],
           mtz->xtal[0]->cell[4],mtz->xtal[0]->cell[5]);
   MtzWhdrLine(fileout,65,hdrrec);
 }
 if (debug) printf(" MtzPut: CELL just written \n");

 ccp4_lrsort(mtz, isort);
 sprintf(hdrrec,"SORT  %3d %3d %3d %3d %3d",isort[0],isort[1],isort[2],
       isort[3],isort[4]);
 MtzWhdrLine(fileout,25,hdrrec);
 if (debug) printf(" MtzPut: SORT just written \n");

 sprintf(hdrrec,"SYMINF %3d %2d %c %5d %7s %5s",mtz->mtzsymm.nsym,mtz->mtzsymm.nsymp,
	     mtz->mtzsymm.symtyp,mtz->mtzsymm.spcgrp,mtz->mtzsymm.spcgrpname,
             mtz->mtzsymm.pgname);
 MtzWhdrLine(fileout,35,hdrrec);
 if (debug) printf(" MtzPut: SYMINF just written \n");

 for (i = 0; i < mtz->mtzsymm.nsym; ++i) {
     mat4_to_symop(symline,symline+74,mtz->mtzsymm.sym[i]);
     symline[74] = '\0';
     sprintf(hdrrec,"SYMM %74s",symline);
     MtzWhdrLine(fileout,79,hdrrec); 
 }
 if (debug) printf(" MtzPut: symmetry just written \n");

 if (mtz->refs_in_memory) {
  /* Find dataset of indices */
  for (i = 0; i < mtz->nxtal; ++i) 
   for (j = 0; j < mtz->xtal[i]->nset; ++j) 
    for (k = 0; k < mtz->xtal[i]->set[j]->ncol; ++k) {
     if (mtz->xtal[i]->set[j]->col[k]->label[0] == 'H' &&
         mtz->xtal[i]->set[j]->col[k]->type[0] == 'H') {
       ind_xtal = i;
       ind_set = j;
       ind_h_col = k;
     }
     if (mtz->xtal[i]->set[j]->col[k]->label[0] == 'K' &&
         mtz->xtal[i]->set[j]->col[k]->type[0] == 'H') 
       ind_k_col = k;
     if (mtz->xtal[i]->set[j]->col[k]->label[0] == 'L' &&
         mtz->xtal[i]->set[j]->col[k]->type[0] == 'H') 
       ind_l_col = k;
    }

  /* Recalculate crystal resolution limits */
  for (i = 0; i < mtz->nxtal; ++i) {
   mtz->xtal[i]->resmax = 0.0;
   mtz->xtal[i]->resmin = 100.0;
   MtzHklcoeffs(mtz->xtal[i]->cell, coefhkl);
   for (j = 0; j < mtz->nref; ++j) {
      ind[0] = (int) mtz->xtal[ind_xtal]->set[ind_set]->col[ind_h_col]->ref[j];
      ind[1] = (int) mtz->xtal[ind_xtal]->set[ind_set]->col[ind_k_col]->ref[j];
      ind[2] = (int) mtz->xtal[ind_xtal]->set[ind_set]->col[ind_l_col]->ref[j];
      res = MtzInd2reso(ind, coefhkl);
      /* crystal limits */
      if (res > 0.0) {
        if (res > mtz->xtal[i]->resmax) mtz->xtal[i]->resmax = res;
        if (res < mtz->xtal[i]->resmin) mtz->xtal[i]->resmin = res;
      }
   }
  }
 }
 /* Calculate overall  resolution limits */
 for (i = 0; i < mtz->nxtal; ++i) {
      if (mtz->xtal[i]->resmax > maxres) maxres = mtz->xtal[i]->resmax;
      if (mtz->xtal[i]->resmin < minres) minres = mtz->xtal[i]->resmin;
 }
 sprintf(hdrrec,"RESO %-20f %-20f",minres,maxres);
 MtzWhdrLine(fileout,46,hdrrec);

 if (debug) 
   printf(" MtzPut: resolution limts just written \n");

 if (strncmp (mtz->mnf.amnf,"NAN",3) == 0) {
   sprintf(hdrrec,"VALM NAN");
   MtzWhdrLine(fileout,8,hdrrec);
 } else {
   sprintf(hdrrec,"VALM %-20f",mtz->mnf.fmnf);
   MtzWhdrLine(fileout,25,hdrrec);
 }

 if (debug) 
   printf(" MtzPut: VALM just written \n");

 /* Loop over crystals */
 for (i = 0; i < mtz->nxtal; ++i) {
 /* Loop over datasets for each crystal */
  for (j = 0; j < mtz->xtal[i]->nset; ++j) {
 /* Loop over columns for each dataset */
   for (k = 0; k < mtz->xtal[i]->set[j]->ncol; ++k) {
     if (mtz->xtal[i]->set[j]->col[k]->active) {
       if (strcmp(mtz->xtal[i]->set[j]->col[k]->type,"Y") == 0 && 
           strcmp(mtz->xtal[i]->set[j]->col[k]->label,"M_ISYM") == 0) {
         sprintf(hdrrec,"COLUMN %-30s ","M/ISYM");
       } else {
         sprintf(hdrrec,"COLUMN %-30s ",mtz->xtal[i]->set[j]->col[k]->label);
       }
       sprintf(hdrrec+38,"%c %17.4f %17.4f %4d",
                   mtz->xtal[i]->set[j]->col[k]->type[0],
                   mtz->xtal[i]->set[j]->col[k]->min,
                   mtz->xtal[i]->set[j]->col[k]->max,
                   mtz->xtal[i]->set[j]->setid);
       MtzWhdrLine(fileout,MTZRECORDLENGTH,hdrrec);
     }
   }
  }
 }

 if (debug) 
   printf(" MtzPut: column info just written \n");

 sprintf(hdrrec,"NDIF %8d",MtzNumActiveSet(mtz));
 MtzWhdrLine(fileout,13,hdrrec);

 if (debug) 
   printf(" MtzPut: about to write dataset info \n");

 /* Loop over crystals */
 for (i = 0; i < mtz->nxtal; ++i) {
 /* Loop over datasets for each crystal */
  for (j = 0; j < mtz->xtal[i]->nset; ++j) {
   /* check if dataset contains any active columns or batches */
   if ( (MtzNumActiveColsInSet(mtz->xtal[i]->set[j]) == 0) &&
        (MtzNbatchesInSet(mtz,mtz->xtal[i]->set[j]) == 0) ) continue;
   sprintf(hdrrec,"PROJECT %7d %-64s",mtz->xtal[i]->set[j]->setid,
                                      mtz->xtal[i]->pname);
   MtzWhdrLine(fileout,MTZRECORDLENGTH,hdrrec);
   sprintf(hdrrec,"CRYSTAL %7d %-64s",mtz->xtal[i]->set[j]->setid,
                                      mtz->xtal[i]->xname);
   MtzWhdrLine(fileout,MTZRECORDLENGTH,hdrrec);
   sprintf(hdrrec,"DATASET %7d %-64s",mtz->xtal[i]->set[j]->setid,
                                      mtz->xtal[i]->set[j]->dname);
   MtzWhdrLine(fileout,MTZRECORDLENGTH,hdrrec);
   sprintf(hdrrec,"DCELL   %7d %10.4f%10.4f%10.4f%10.4f%10.4f%10.4f",
        mtz->xtal[i]->set[j]->setid,mtz->xtal[i]->cell[0],
        mtz->xtal[i]->cell[1],mtz->xtal[i]->cell[2],
        mtz->xtal[i]->cell[3],mtz->xtal[i]->cell[4], 
        mtz->xtal[i]->cell[5]);
   MtzWhdrLine(fileout,76,hdrrec);
   sprintf(hdrrec,"DWAVEL  %7d %10.5f",mtz->xtal[i]->set[j]->setid,
                                       mtz->xtal[i]->set[j]->wavelength);
   MtzWhdrLine(fileout,26,hdrrec);
  }
 }

 if (MtzNbat(mtz) > 0) {
   batch = mtz->batch;
   /* if new batch headers have been written, lose the old ones */
   if (MtzNbat(mtz) > mtz->nbat) {
     for (i=0; i < mtz->nbat; ++i)
       batch = batch->next;
     numbat = MtzNbat(mtz) - mtz->nbat;
   } else {
     numbat = mtz->nbat;
   }
   for (i = 0; i < numbat; i += 12) {
     sprintf(hdrrec,"BATCH ");
     l = 6;
     for (j = 0; j < 12 && 12*i+j < numbat; ++j) {
       sprintf(hdrrec+6+6*j,"%6d",batch->num);
       l += 6;
       batch = batch->next;
     }
     MtzWhdrLine(fileout,l,hdrrec);
   }
 }

 sprintf(hdrrec,"END ");
 MtzWhdrLine(fileout,4,hdrrec);

 if (debug) 
   printf(" MtzPut: main header written \n");

 if (mtz->histlines > 0) {
   sprintf(hdrrec,"MTZHIST %3d",mtz->histlines);
   MtzWhdrLine(fileout,11,hdrrec);
   for (i = 0; i < mtz->histlines; ++i) {
     strncpy(hdrrec,mtz->hist + MTZRECORDLENGTH*i,MTZRECORDLENGTH);
     MtzWhdrLine(fileout,MTZRECORDLENGTH,hdrrec);
   }
 }

 if (MtzNbat(mtz) > 0) {
   batch = mtz->batch;
   /* if new batch headers have been written, lose the old ones */
   if (MtzNbat(mtz) > mtz->nbat)
     for (i=0; i < mtz->nbat; ++i)
       batch = batch->next;
   sprintf(hdrrec,"MTZBATS");
   MtzWhdrLine(fileout,7,hdrrec);
   while (batch != NULL) {
     sprintf(hdrrec,"BH %8d%8d%8d%8d",batch->num,nwords,nintegers,nreals);
     MtzWhdrLine(fileout,35,hdrrec);
     strcpy(hdrrec,"TITLE ");
     strncpy(hdrrec+6,batch->title,70);
     MtzWhdrLine(fileout,76,hdrrec);
     MtzBatchToArray(batch,intbuf,fltbuf);
     intbuf[0] = nwords;
     intbuf[1] = nintegers;
     intbuf[2] = nreals;
     ccp4_file_setmode(fileout,2);
     ccp4_file_write(fileout, (uint8 *) buf, nwords);
     ccp4_file_setmode(fileout,0);
     if (batch->gonlab[0] != "") {
       sprintf(hdrrec,"BHCH %8s%8s%8s",batch->gonlab,batch->gonlab+9,batch->gonlab+17);
     } else {
       sprintf(hdrrec,"BHCH                         ");
     }
     MtzWhdrLine(fileout,29,hdrrec);
     batch = batch->next;
   }
 }

 if (debug) 
   printf(" MtzPut: batch headers written \n");

 sprintf(hdrrec,"MTZENDOFHEADERS ");
 MtzWhdrLine(fileout,16,hdrrec);

 /* go back and correct hdrst */
 if (!mtz->refs_in_memory) {
   ccp4_file_setmode(fileout,0);
   ccp4_file_seek(fileout, 4, SEEK_SET); 
   hdrst = mtz->nref * MtzNumActiveCol(mtz) + SIZE1 + 1;
   ccp4_file_setmode(fileout,2);
   ccp4_file_write(fileout,(uint8 *) &hdrst,1);
 }

 /* And close the mtz file: */
 if (!mtz->fileout) 
   ccp4_file_close(fileout);

 if (debug) 
   printf(" MtzPut: bye bye \n");

}

CCP4File *MtzOpenForWrite(const char *logname, int *hdrst)

{ CCP4File *fileout;
 int debug=0;
 char *filename;

 if (debug) printf(" MtzOpenForWrite: entering \n");
 /* Open the mtz file: */
 if (getenv(logname) != NULL) {
   filename = strdup(getenv(logname));
 } else {
   filename = strdup(logname);
 }
 fileout = ccp4_file_open(filename,O_RDWR);
 if (debug) printf(" MtzOpenForWrite: file opened \n");

 /* Write initial info */
 ccp4_file_setmode(fileout,0);
 ccp4_file_writechar(fileout,"MTZ ",4);
 ccp4_file_setmode(fileout,2);
 ccp4_file_write(fileout,(uint8 *) hdrst,1);

 ccp4_file_setstamp(fileout,2);
/* Write architecture */
 ccp4_file_warch(fileout);
 if (debug) printf(" MtzOpenForWrite: stamp written \n");

 /* Position at start of reflections - intervening gap should be filled
    with zeros */
 ccp4_file_seek(fileout, SIZE1, SEEK_SET); 

 free(filename); 

 if (debug) printf(" MtzOpenForWrite: bye bye \n");
 return fileout;
}

void MtzBatchToArray(MTZBAT *batch, int *intbuf, float *fltbuf)

/* Writes batch info into the structure `batch`. */

{  int i;

  intbuf[3] = batch->iortyp;
  for (i = 0; i < 6; ++i)
    intbuf[4+i] = batch->lbcell[i];
  intbuf[10] = batch->misflg;
  intbuf[11] = batch->jumpax;
  intbuf[12] = batch->ncryst;
  intbuf[13] = batch->lcrflg;
  intbuf[14] = batch->ldtype;
  intbuf[15] = batch->jsaxs;
  intbuf[16] = batch->nbscal;
  intbuf[17] = batch->ngonax;
  intbuf[18] = batch->lbmflg;
  intbuf[19] = batch->ndet;
  intbuf[20] = batch->nbsetid;

  for (i = 0; i < 6; ++i)
    fltbuf[i] = batch->cell[i];
  for (i = 0; i < 9; ++i)
    fltbuf[6 + i] = batch->umat[i];
  for (i = 0; i < 3; ++i) 
    fltbuf[15 + i] = batch->phixyz[0][i];
  for (i = 0; i < 3; ++i) 
    fltbuf[18 + i] = batch->phixyz[1][i];
  for (i = 0; i < 12; ++i) 
    fltbuf[21 + i] = batch->crydat[i];
  for (i = 0; i < 3; ++i) 
    fltbuf[33 + i] = batch->datum[i];
  fltbuf[36] = batch->phistt;
  fltbuf[37] = batch->phiend;
  for (i = 0; i < 3; ++i) 
    fltbuf[38 + i] = batch->scanax[i];
  fltbuf[41] = batch->time1;
  fltbuf[42] = batch->time2;
  fltbuf[43] = batch->bscale;
  fltbuf[44] = batch->bbfac;
  fltbuf[45] = batch->sdbscale;
  fltbuf[46] = batch->sdbfac;
  fltbuf[47] = batch->phirange;
  for (i = 0; i < 3; ++i)
    fltbuf[59 + i] = batch->e1[i];
  for (i = 0; i < 3; ++i)
    fltbuf[62 + i] = batch->e2[i];
  for (i = 0; i < 3; ++i)
    fltbuf[65 + i] = batch->e3[i];
  for (i = 0; i < 3; ++i)
    fltbuf[80 + i] = batch->source[i];
  for (i = 0; i < 3; ++i)
    fltbuf[83 + i] = batch->so[i];
  fltbuf[86] = batch->alambd;
  fltbuf[87] = batch->delamb;
  fltbuf[88] = batch->delcor;
  fltbuf[89] = batch->divhd;
  fltbuf[90] = batch->divvd;
  for (i = 0; i < batch->ndet; ++i)
  { fltbuf[111 + (i * 6)] = batch->dx[i];
    fltbuf[112 + (i * 6)] = batch->theta[i];
    fltbuf[113 + (i * 6)] = batch->detlm[i][0][0];
    fltbuf[114 + (i * 6)] = batch->detlm[i][0][1];
    fltbuf[115 + (i * 6)] = batch->detlm[i][1][0];
    fltbuf[116 + (i * 6)] = batch->detlm[i][1][1];}

}

void MtzWhdrLine(CCP4File *fileout, int nitems, char buffer[]) {

  /* write header record to fileout. Record is filled from
     nitems to MTZRECORDLENGTH by blanks */

 char hdrrec[MTZRECORDLENGTH];
 int i;

 for (i = 0; i < nitems; ++i)
   hdrrec[i] = buffer[i];
 for (i = nitems; i < MTZRECORDLENGTH; ++i)
   hdrrec[i] = ' ';

 ccp4_file_writechar(fileout,hdrrec,MTZRECORDLENGTH);

}

int MtzWrefl(CCP4File *fileout, int ncol, float *refldata) {

  if (!fileout)  {
    ccp4_signal(CMTZ_ERRNO(CMTZERR_NoFile),"MtzWrefl",NULL);
    return 0;
  }
  return (ccp4_file_write(fileout, (uint8 *) refldata, ncol));

}

MTZ *MtzMalloc(int nxtal, int nset[])

{ MTZ *mtz;
  int i,j;
  float zerocell[6]={0.0};

  /* Allocate main header and symmetry */
  mtz = (MTZ *) ccp4_utils_malloc(sizeof(MTZ));
  if (mtz == NULL) {
    ccp4_signal(CMTZ_ERRNO(CMTZERR_AllocFail),"MtzMalloc",NULL);
    return NULL;
  }

  mtz->nxtal=0;
  /* Allocate crystals and datasets. */
  if (nxtal == 0) {
    mtz->xtal[0] = NULL;
  } else {
    for (i = 0; i < nxtal; ++i) {
      /* This adds mtz->xtal[i] */
      if ( ! MtzAddXtal(mtz,"NULL_xname","NULL_pname",zerocell) ) return NULL;
      mtz->xtal[i]->nset = 0;
      for (j = 0; j < nset[i]; ++j) {
        /* This adds mtz->xtal[i]->set[j] */
        if ( ! MtzAddDataset(mtz,mtz->xtal[i],"NULL_dname",0.0) ) return NULL;
      }
    }
  }

  /* initialise main header */
  mtz->filein = NULL;
  mtz->fileout = NULL;
  mtz->title[0] = '\0';
  mtz->hist = NULL;
  mtz->histlines = 0;
  mtz->nxtal = nxtal;
  mtz->nref = 0;
  mtz->refs_in_memory = 1;
  mtz->nbat = 0;
  sprintf(mtz->mnf.amnf,"NAN");
  mtz->batch = NULL;
  for (i = 0; i < 5; ++i) {
    mtz->order[i] = NULL;
  }

  return(mtz);
}

void MtzFree(MTZ *mtz)

/* Frees the memory reserved for 'mtz' */

{ int i,j,k;

 /* Close attached mtz files */
 if (mtz->filein) {
    ccp4_file_close(mtz->filein);
    mtz->filein = NULL;
 }
 if (mtz->fileout) {
    ccp4_file_close(mtz->fileout);
    mtz->fileout = NULL;
 }

  /* Loop over crystals */
  for (i = 0; i < mtz->nxtal; ++i) {
  /* Loop over datasets for each crystal */
   for (j = 0; j < mtz->xtal[i]->nset; ++j) {
  /* Loop over columns for each dataset */
    for (k = 0; k < mtz->xtal[i]->set[j]->ncol; ++k) {
      MtzFreeCol(mtz->xtal[i]->set[j]->col[k]);
    }
    free((void *) mtz->xtal[i]->set[j]);
   }
   free((void *) mtz->xtal[i]);
  }

  if (mtz->nbat > 0) 
    MtzFreeBatch(mtz->batch);

  if (mtz->hist != NULL) 
    MtzFreeHist(mtz->hist);

  free((void *) mtz);
}

MTZBAT *MtzMallocBatch()

/* Allocates memory for a single batch header */

{ MTZBAT *batch;

  batch = (MTZBAT *) ccp4_utils_malloc(sizeof(MTZBAT));
  if (batch == NULL)
  { printf("MtzMallocBatch: not enough memory for operation!!\n");
    exit(1);}

  return(batch);
}

void MtzFreeBatch(MTZBAT *batch) 

/* Frees the memory reserved for 'batch' */

{
  if (batch != NULL) {
    MtzFreeBatch(batch->next);
    free(batch);
  }
}

MTZCOL *MtzMallocCol(MTZ *mtz, int nref)

{ MTZCOL *col;

  col = (MTZCOL *) ccp4_utils_malloc(sizeof(MTZCOL));
  if (col == NULL)
  { printf("MtzMallocCol: not enough memory for operation!!\n");
    exit(1);}

  col->ref = NULL;
  if (mtz->refs_in_memory) {
    ccp4array_new_size(col->ref,nref);
    if (col->ref == NULL)
    { printf("MtzMallocCol: not enough memory for operation!!\n");
      exit(1);}
  }

  return(col);
}

void MtzFreeCol(MTZCOL *col)

{ if (col->ref) ccp4array_free(col->ref);
  free((void *) col);
}

char *MtzCallocHist(int nhist)

/* Allocates memory for the mtz history with 'nhist' lines */

{ char *hist;

 hist = (char *) ccp4_utils_calloc(nhist, sizeof(char)*MTZRECORDLENGTH);
 return(hist);
}

void MtzFreeHist(char *hist)

/* Frees the memory reserved for 'hist' */

{ 
  free((void *) hist);
}

MTZXTAL *MtzAddXtal(MTZ *mtz, const char *xname, const char *pname,
              const float cell[6])
{
  /* add a new crystal to the mtz */
  int i,x;
  MTZXTAL *xtal;

  if (mtz->nxtal == MXTALS) { 
    printf("No more xtals! \n"); 
    return NULL;
  }
  xtal = (MTZXTAL *) ccp4_utils_malloc( sizeof(MTZXTAL) );
  if (! xtal ) { 
    ccp4_signal(CMTZ_ERRNO(CMTZERR_AllocFail),"MtzAddXtal",NULL);
    return NULL;
  }
  /* fill out the data */
  strncpy( xtal->xname, xname, 65 );
  strncpy( xtal->pname, pname, 65 );
  xtal->resmin = 100.0;
  xtal->resmax = 0.0;
  for (i = 0; i < 6; i++) xtal->cell[i] = cell[i];
  /* make new xtalid */
  for (i = x = 0; x < mtz->nxtal; x++)
    if (mtz->xtal[x]->xtalid > i) i = mtz->xtal[x]->xtalid;
  xtal->xtalid = ++i;
  xtal->nset = 0;
  /* add pointer to mtz */
  mtz->xtal[ mtz->nxtal++ ] = xtal;

  return xtal;
}

MTZSET *MtzAddDataset(MTZ *mtz, MTZXTAL *xtl, const char *dname,
                 const float wavelength)
{
  /* add a new dataset to the xtal */
  int i,x,s;
  MTZSET *set;

  if (xtl->nset == MSETSPERXTAL) { 
    printf("No more datasets! \n"); 
    return NULL;
  }
  set = (MTZSET *) ccp4_utils_malloc( sizeof(MTZSET) );
  if ( ! set ) { 
    ccp4_signal(CMTZ_ERRNO(CMTZERR_AllocFail),"MtzAddDataset",NULL);
    return NULL;
  }
  /* fill out the data */
  strncpy( set->dname, dname, 65 );
  set->wavelength = wavelength;
  /* make new setid */
  for (i = x = 0; x < mtz->nxtal; x++)
    for (s = 0; s < mtz->xtal[x]->nset; s++)
      if (mtz->xtal[x]->set[s]->setid > i) i = mtz->xtal[x]->set[s]->setid;
  set->setid = ++i;
  set->ncol = 0;
  /* add pointer to xtal */
  xtl->set[ xtl->nset++ ] = set;

  return set;
}

MTZCOL *MtzAddColumn(MTZ *mtz, MTZSET *set, const char *label,
                const char *type)
{
  /* add a new column to the dataset */
  int i,nref;
  union float_uint_uchar uf;
  MTZCOL *col;

  if (set->ncol == 200) { printf("No more columns!"); exit(1); }

  /* allocate some memory for first column */
  if (!mtz->refs_in_memory) {
    nref = 0;
  } else if (mtz->nref == 0) {
    nref = 2000;
  } else {
    nref = mtz->nref;
  }
  col = MtzMallocCol(mtz, nref);
  if (col == NULL) { printf("Not enough memory for column!"); exit(1); }

  /* fill out the data */
  strncpy( col->label, label, 30 );
  strncpy( col->type, type, 2);
  col->active = 1;
  col->source = 0;
  col->min = 1.e06;
  col->max = -1.e06;
  /* add pointer to set, and backpointer */
  set->col[ set->ncol++ ] = col;
  /* initialise column to MNF */
  if (strncmp (mtz->mnf.amnf,"NAN",3) == 0) {
   uf = ccp4_nan();
  } else {
   uf.f = mtz->mnf.fmnf;
  }
  for (i=0; i < nref; i++) col->ref[i] = uf.f;

  return col;
}

int MtzToggleColumn(MTZCOL *col)
{
  /* Toggle active flag of column */
  if (col->active) {
    col->active = 0;
  } else {
    col->active = 1;
  }
  return col->active;
}

MTZSET *MtzColSet(const MTZ *mtz, const MTZCOL *col)
{
  /* get the dataset associated with a column */
  int x,s,c;
  for (x=0; x < mtz->nxtal; x++)
    for (s=0; s < mtz->xtal[x]->nset; s++)
      for (c=0; c < mtz->xtal[x]->set[s]->ncol; c++)
      if (mtz->xtal[x]->set[s]->col[c] == col)
        return mtz->xtal[x]->set[s];
  printf ("MtzColSet: no such column"); exit(1);
}

MTZXTAL *MtzSetXtal(const MTZ *mtz, const MTZSET *set)
{
  /* get the crystal associated with a dataset */
  int x,s;
  for (x=0; x < mtz->nxtal; x++)
    for (s=0; s < mtz->xtal[x]->nset; s++)
      if (mtz->xtal[x]->set[s] == set)
        return mtz->xtal[x];
  printf ("MtzSetXtal: no such dataset"); exit(1);
}

int MtzNxtal(const MTZ *mtz)
{
  return mtz->nxtal;
}

int MtzNumActiveXtal(const MTZ *mtz)
{
  int k,ixtal=0;

  for (k=0; k < mtz->nxtal; k++)
     if (MtzNumActiveSetsInXtal(mtz,mtz->xtal[k])) ++ixtal;
  return ixtal;
}

MTZXTAL **MtzXtals(MTZ *mtz)
{
  return mtz->xtal;
}

MTZXTAL *MtzIxtal(const MTZ *mtz, const int ixtal)
{
  return mtz->xtal[ixtal];
}

int MtzNsetsInXtal(const MTZXTAL *xtal)
{
  return xtal->nset;
}

int MtzNumActiveSetsInXtal(const MTZ *mtz, const MTZXTAL *xtal)
{
  int k,iset=0;

  for (k=0; k < xtal->nset; k++)
     if (MtzNumActiveColsInSet(xtal->set[k]) ||
         MtzNbatchesInSet(mtz, xtal->set[k])) ++iset;
  return iset;
}

MTZSET **MtzSetsInXtal(MTZXTAL *xtal)
{
  return xtal->set;
}

MTZSET *MtzIsetInXtal(const MTZXTAL *xtal, const int iset)
{
  return xtal->set[iset];
}

int MtzNcolsInSet(const MTZSET *set)
{
  return set->ncol;
}

int MtzNumActiveColsInSet(const MTZSET *set)
{
  int k,icol=0;

  for (k=0; k < set->ncol; k++)
     icol += set->col[k]->active;
  return icol;
}

int MtzNbatchesInSet(const MTZ *mtz, const MTZSET *set)
{
  int ibatch=0;
  MTZBAT *batch;

  for (batch = mtz->batch ; batch != NULL ; batch = batch->next ) 
    if (batch->nbsetid == set->setid) ++ibatch;

  return ibatch;
}

MTZCOL **MtzColsInSet(MTZSET *set)
{
  return set->col;
}

MTZCOL *MtzIcolInSet(const MTZSET *set, const int icol)
{
  return set->col[icol];
}

char *MtzColType(MTZCOL *col)
{
  return col->type;
}

int MtzNset(const MTZ *mtz)
{
  int x,iset=0;
  for (x=0; x < mtz->nxtal; x++)
    iset += MtzNsetsInXtal(mtz->xtal[x]);
  return iset;
}

int MtzNumActiveSet(const MTZ *mtz)
{
  int x,iset=0;
  for (x=0; x < mtz->nxtal; x++)
    iset += MtzNumActiveSetsInXtal(mtz,mtz->xtal[x]);
  return iset;
}
  
int MtzNcol(const MTZ *mtz)
{
  int x,s,icol=0;
  for (x=0; x < mtz->nxtal; x++)
    for (s=0; s < mtz->xtal[x]->nset; s++)
      icol += MtzNcolsInSet(mtz->xtal[x]->set[s]);
  return icol;
}
  
int MtzNumActiveCol(const MTZ *mtz)
{
  int x,s,icol=0;
  for (x=0; x < mtz->nxtal; x++)
    for (s=0; s < mtz->xtal[x]->nset; s++)
      icol += MtzNumActiveColsInSet(mtz->xtal[x]->set[s]);
  return icol;
}
   
int MtzNref(const MTZ *mtz)
{
  /* get the number of reflections in the mtz */

  return mtz->nref;
}
 
int MtzNbat(const MTZ *mtz)
{
  /* get the number of batches in the mtz */
  int cnt=0;
  MTZBAT *batch;

  for (batch = mtz->batch ; batch != NULL ; batch = batch->next ) 
    ++cnt;

  return cnt;
}
  
char *MtzXtalPath(const MTZXTAL *xtal)
{
  /* Return the full path name of a crystal */
  char *path;
  size_t length;

  length = strlen(xtal->xname)+2;
  path = (char *) ccp4_utils_malloc(length*sizeof(char));
  strcpy( path, "/" );
  strcat( path, xtal->xname );
  path[length-1] = '\0';
  return ( path );
}

char *MtzSetPath(const MTZ *mtz, const MTZSET *set)
{
  /* Return the full path name of a dataset */
  char *path, *path1;
  size_t length;

  path1 = MtzXtalPath( MtzSetXtal( mtz, set ) );
  length = strlen(path1)+strlen(set->dname)+2;
  path = ccp4_utils_malloc(length*sizeof(char));
  strcpy( path, path1 );
  free (path1);  
  strcat( path, "/" );
  strcat( path, set->dname );
  path[length-1] = '\0';
  return ( path );
}

char *MtzColPath(const MTZ *mtz, const MTZCOL *col)
{
  /* Return the full path name of a column */
  char *path, *path1;
  size_t length;

  path1 = MtzSetPath( mtz, MtzColSet( mtz, col ) );
  length = strlen(path1)+strlen(col->label)+2;
  path = (char *) ccp4_utils_malloc(length*sizeof(char));
  strcpy( path, path1 );
  free (path1);  
  strcat( path, "/" );
  strcat( path, col->label );
  path[length-1] = '\0';
  return ( path );
}

void MtzRJustPath(char *path, const char *partial, const int njust)
{
  /* Complete a right-justified path by prefixing with wildcards */
  int i, j;
  /* count the slashes */
  for ( i = j = 0; i < strlen(partial); i++ ) if ( partial[i] == '/' ) j++;

  strcpy( path, "");
  if ( j++ < njust ) strcat( path, "/" );
  while ( j++ < njust ) strcat( path, "*/" );
  strcat( path, partial );
}

int MtzPathMatch(const char *path1, const char *path2)
{
  /* test for match between two paths, including wildcards */
  /* this version only handles wildcards at the end of name components */
  int p1 = 0, p2 = 0;
  while ( path1[p1] != '\0' && path2[p2] != '\0' ) {    /* search both paths */
    if ( path1[p1] != path2[p2] ) {
      if ( path1[p1] != '*' && path2[p2] != '*' )
      return FALSE;                       /* non-wild mismatch is terminal */
      while ( path1[p1] != '/' && path1[p1] != '\0' ) p1++;   /* skip compnt */
      while ( path2[p2] != '/' && path2[p2] != '\0' ) p2++;   /* skip compnt */
    } else {
      p1++; p2++;
    }
  }
  return (path1[p1] == path2[p2]);          /* true only if both paths ended */
}


MTZCOL *MtzColLookup(const MTZ *mtz, const char *label)
{
  /* Returns a pointer to the column of mtz with the given `label`, or NULL */
  int x,s,c;
  char *path1, path2[200];

  /* complete the right-justified path */
  MtzRJustPath( path2, label, 3 );
  /* now find the matching column */
  for (x=0; x < mtz->nxtal; x++)  /* not much point in optimising this */
    for (s=0; s < mtz->xtal[x]->nset; s++)
      for (c=0; c < mtz->xtal[x]->set[s]->ncol; c++) {
        path1 = MtzColPath(mtz, mtz->xtal[x]->set[s]->col[c]);
        if ( MtzPathMatch( path1, path2 ) ) {
          free (path1);
          return mtz->xtal[x]->set[s]->col[c];
	}
        free (path1);
      }
  return NULL;
}

MTZSET *MtzSetLookup(const MTZ *mtz, const char *label)
{
  /* Returns a pointer to the dataset of mtz with the given `label`, or NULL */
  int x,s;
  char *path1, path2[200];

  /* complete the right-justified path */
  MtzRJustPath( path2, label, 2 );
  /* now find the matching column */
  for (x=0; x < mtz->nxtal; x++)  
    for (s=0; s < mtz->xtal[x]->nset; s++) {
      path1 = MtzSetPath(mtz, mtz->xtal[x]->set[s]);
      if ( MtzPathMatch( path1, path2 ) ) {
        free (path1);
        return mtz->xtal[x]->set[s];
      }
    free (path1);
    }
  return NULL;
}

MTZXTAL *MtzXtalLookup(const MTZ *mtz, const char *label)
{
  /* Returns a pointer to the crystal of mtz with the given `label`, or NULL */
  int x;
  char *path1, path2[200];

  /* complete the right-justified path */
  MtzRJustPath( path2, label, 1 );
  /* now find the matching column */
  for (x=0; x < mtz->nxtal; x++) { 
    path1 = MtzXtalPath(mtz->xtal[x]);
    if ( MtzPathMatch( path1, path2 ) ) {
      free (path1);
      return mtz->xtal[x];
    }
    free (path1);
  }
  return NULL;
}