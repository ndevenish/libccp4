/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/

/*   ccplib.c

     Header file for CCP4 library clones
     Peter Briggs CCP4 April 2001

     NB. These are placeholder functions to be replace by proper
     functions in the future.
*/

/*------------------------------------------------------------------*/

/* Macro definitions */

/*------------------------------------------------------------------*/

#ifndef __CCPLib__
#define __CCPLib__

static char rcsidhl[] = "$Id$";

#ifdef  __cplusplus
namespace CCP4 {
extern "C" {
#endif

/* MAXLINE = maximum number of characters in lines read
   from environ.def and default.def files (ccp4fyp)
   MAXTOKS = maximum number of tokens in lines read from
   environ.def and default.def files (ccp4fyp)
   MAXNAMES = maximum number of logical names that can be
   read and stored from environ.def (ccp4fyp)
*/
#define CCP4_MAXLINE  200
#define CCP4_MAXTOKS  3
#define CCP4_MAXNAMES 150
#define CCP4_MODULO   100000

/*------------------------------------------------------------------*/

/* Structures and typedefs */

/*------------------------------------------------------------------*/

/* <None declared> */

/*------------------------------------------------------------------*/

/* Function Prototypes */

/*------------------------------------------------------------------*/

int ccperror(int ierr, char* message);

int ccp4printf(int level, char *format, ...);

int ccp4fyp(int argc, char **argv);

void *ccp4malloc(size_t size, char *message);

int ccp4setenv(char *logical_name, char* value, char **envname,
		       char **envtype, char **envext, int ienv, int no_overwrt);

int ccpexists(char *filename);

int ccpputenv(char *logical_name, char *file_name);

void ccp4_banner(void);

#ifdef __cplusplus
}
}
#endif

#endif   /* __CCPLib__ */
