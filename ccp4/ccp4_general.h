/*
     ccp4_general.h: header for general library functions and utilities.
     Copyright (C) 2001  CCLRC, Peter Briggs et al

     This library is free software and is distributed under the terms and
     conditions of the CCP4 licence agreement as `Part 0' (Annex 2)
     software, which is version 2.1 of the GNU Lesser General Public
     Licence (LGPL) with the following additional clause:

        `You may also combine or link a "work that uses the Library" to
        produce a work containing portions of the Library, and distribute
        that work under terms of your choice, provided that you give
        prominent notice with each copy of the work that the specified
        version of the Library is used in it, and that you include or
        provide public access to the complete corresponding
        machine-readable source code for the Library including whatever
        changes were used in the work. (i.e. If you make changes to the
        Library you must distribute those, but you do not need to
        distribute source or object code to those portions of the work
        not covered by this licence.)'

     Note that this clause grants an additional right and does not impose
     any additional restriction, and so does not affect compatibility
     with the GNU General Public Licence (GPL). If you wish to negotiate
     other terms, please contact the maintainer.

     You can redistribute it and/or modify the library under the terms of
     the GNU Lesser General Public License as published by the Free Software
     Foundation; either version 2.1 of the License, or (at your option) any
     later version.

     This library is distributed in the hope that it will be useful, but
     WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     Lesser General Public License for more details.

     You should have received a copy of the CCP4 licence and/or GNU
     Lesser General Public License along with this library; if not, write
     to the CCP4 Secretary, Daresbury Laboratory, Warrington WA4 4AD, UK.
     The GNU Lesser General Public can also be obtained by writing to the
     Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
     MA 02111-1307 USA
*/

/*   ccp4_general.c

     Header file for CCP4 library clones
     Peter Briggs et al CCP4 April 2001
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

void ccp4f_mem_tidy(void);

int ccperror(int ierr, const char *message);

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
