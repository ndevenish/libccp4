/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/

/*   ccp4program.h

     Header file for functions accessing CCP4 program info
*/

/*------------------------------------------------------------------*/

/* Macro definitions */

/*------------------------------------------------------------------*/

#ifndef __CCP4Program__
#define __CCP4Program__

#ifdef  __cplusplus
namespace CCP4 {
extern "C" {
#endif
static char rcsidhp[] = "$Id$";

#define CCP4_VERSION_NO "4.2"
#define CCP4_PATCH_LEVEL "4.2.0"

/*------------------------------------------------------------------*/

/* Function Prototypes */

/*------------------------------------------------------------------*/

char *ccp4_prog_vers(char *progvers);

char *ccp4ProgramName(char *progname);

void ccp4ProgramTime(int init);

int ccp4VerbosityLevel(int level);

/* check existence of licence agreement */

int ccp4_licence_exists(const char *name);

#ifdef __cplusplus
} 
} 
#endif

#endif   /* __CCP4Program__ */