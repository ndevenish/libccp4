
#ifdef _MVS

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
