/*
     cmap_labels.h: header for cmap_labels.c
     Copyright (C) 2001  CCLRC, Charles Ballard

     This code is distributed under the terms and conditions of the
     CCP4 Program Suite Licence Agreement as a CCP4 Library.
     A copy of the CCP4 licence can be obtained by writing to the
     CCP4 Secretary, Daresbury Laboratory, Warrington WA4 4AD, UK.
*/
#ifndef __GUARD_MAPLIB_LABEL
#define __GUARD_MAPLIB_LABEL

#ifdef __cplusplus
extern "C" {
#endif

int parse_maplabels(CMMFile *mfile);
int write_maplabels(const CMMFile *mfile);


#ifdef __cplusplus
}
#endif

#endif    /* __GUARD_MAPLIB_LABEL */
