/*
     w32mvs.c: functions required by MVS
     Copyright (C) 2003  Alun Ashton

     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part 1' (Annex 2) software.
     A copy of the CCP4 licence can be obtained by writing to the
     CCP4 Secretary, Daresbury Laboratory, Warrington WA4 4AD, UK.
*/

#ifdef _MSC_VER

#include "w32mvs.h"

char *ccp4_utils_username(void)
{
  char userSystemInfo[BUFSIZE];
  unsigned long bufsize = BUFSIZE;
  if (GetUserName(userSystemInfo,&bufsize)) return userSystemInfo;
  printf("No login id found\n");
  return NULL;
}

//float __stdcall ETIME(float tarray[2]); 

float ccp4_utils_etime (float tarray[2])
{
  //return ETIME(tarray);
  tarray[0] = 0;
  tarray[1] = 0;
  return 0;
}
#endif
