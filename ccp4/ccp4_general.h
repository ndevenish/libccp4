/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/

/*   ccp4_general.c

     Header file for CCP4 library clones
     Peter Briggs et al CCP4 April 2001

     NB. These are placeholder functions to be replace by proper
     functions in the future.
*/

/*------------------------------------------------------------------*/

/* Macro definitions */

/*------------------------------------------------------------------*/

#ifndef __CCPGeneral__
#define __CCPGeneral__

static char rcsidhl[] = "$Id$";

/* note order: this must be outside CCP4 namespace */
#include "ccp4_parser.h"

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

/* stuff for error reporting */
#define CGEN_ERRNO(n) (CCP4_ERR_GEN | (n))

/* error defs */
#define  CGENERR_Ok                  0
#define  CGENERR_AllocFail           1
#define  CGENERR_CantSetEnvironment  2
#define  CGENERR_MaxNamesExceeded    3
#define  CGENERR_EOptionUseError     4
#define  CGENERR_DOptionUseError     5
#define  CGENERR_LogicalNameUseError 6
#define  CGENERR_CantOpenEnvFile     7
#define  CGENERR_CantOpenDefFile     8
#define  CGENERR_ParseEnvFail        9
#define  CGENERR_ParseDefFail        10
#define  CGENERR_CantFindInFile      11
#define  CGENERR_EnvPathFail         12
#define  CGENERR_DefPathFail         13
#define  CGENERR_CantGetClibd        14
#define  CGENERR_CantGetCcp4Scr      15

/*------------------------------------------------------------------*/

/* Structures and typedefs */

/*------------------------------------------------------------------*/

/* <None declared> */

/*------------------------------------------------------------------*/

/* Function Prototypes */

/*------------------------------------------------------------------*/

void ccp4_mem_tidy(void);

int ccperror(int ierr, char* message);

int ccp4printf(int level, char *format, ...);

int ccp4fyp(int argc, char **argv);

int ccp4fyp_cleanup(int ienv, char **envname, char **envtype, char **envext,
		    char *logical_name, char *file_name, char *file_type,
		    char *file_ext, char *env_file, char *def_file,
		    char *dir, CCP4PARSERARRAY *parser);

int ccp4setenv(char *logical_name, char* value, char **envname,
	       char **envtype, char **envext, int *ienv, int no_overwrt);

int ccp4setenv_cleanup(char *file_ext, char *file_root, char *file_path,
		       char *file_name);

int ccpexists(char *filename);

int ccpputenv(char *logical_name, char *file_name);

void ccp4_banner(void);

#ifdef __cplusplus
}
}
#endif

#endif   /* __CCPGeneral__ */