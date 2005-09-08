/*
     cmaplib_f.h: header files for cmaplib_f.c
     Copyright (C) 2001  CCLRC, Charles Ballard

     This code is distributed under the terms and conditions of the
     CCP4 Program Suite Licence Agreement as a CCP4 Library.
     A copy of the CCP4 licence can be obtained by writing to the
     CCP4 Secretary, Daresbury Laboratory, Warrington WA4 4AD, UK.
*/
#ifndef __GUARD_MAPLIB_FORTRAN
#define __GUARD_MAPLIB_FORTRAN

#include "cmaplib.h"

#define MAXMAP MAXFILES

typedef struct _IOConvMap IOConvMap;

struct _IOConvMap {
  int ipc;
  char *logname;
  CMMFile *mapfile;
};

#endif  /*  __GUARD_MAPLIB_FORTRAN */
