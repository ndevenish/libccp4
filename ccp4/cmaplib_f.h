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
