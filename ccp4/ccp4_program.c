/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part ii)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/

/** @file ccp4_program.c
 *  Utilies to set and fetch program information.
 *  Peter Briggs CCP4 May 2001
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ccp4_program.h"
#include "ccp4_parser.h"
#include "ccp4_lib.h"
#include "ccplib.h"
static char rcsid[] = "$Id$";

char *ccp4_prog_vers(char *progvers) 
{
  static char *programversion=NULL;
  int         lname;

  if (progvers) {
    lname = strlen(progvers)+1;
    programversion = (char *) realloc(programversion,lname);
    strncpy(programversion,progvers,lname);
  }
  return programversion;
}

/*------------------------------------------------------------------*/

/* ccp4programname

   Set or return program name

   Always returns a pointer to the program name
   If progname is not NULL then set the program name to
   progname.
*/
char *ccp4ProgramName(char *progname)
{
  static char *programname=NULL;
  int         lname;

  if (progname) {
    lname = strlen(progname)+1;
    programname = (char *) realloc(programname,lname);
    strncpy(programname,progname,lname);
  }
  return programname;
}

/* ccp4ProgramTime

   Set or print program time information
*/
void ccp4ProgramTime(int init)
{
  static int elaps0;
  static float tarray0[2];
  int elaps;
  float tarray[2];

  if (init) {
    elaps0 = time(NULL);
    ccp4_utils_etime(tarray0);
  } else {
    elaps = time(NULL) - elaps0;
    ccp4_utils_etime(tarray);

    printf("Times: User: %9.1fs System: %6.1fs Elapsed: %5d:%2.2d  \n",
	   tarray[0]-tarray0[0],tarray[1]-tarray0[1],elaps/60,elaps%60);
  }

}

/* ccp4VerbosityLevel

   Set or return the reference verbosity level

   Always return the verbosity level - if verboselevel is
   between 0 and 9 then reset the verbosity level to
   verboselevel
*/
int ccp4VerbosityLevel(int level)
{
  /* The default level is 1 */
  static int verbositylevel=1;

  if (level > -1 && level < 10)
    verbositylevel = level;
  return verbositylevel;
}

/* check existence of licence agreement */

int ccp4_licence_exists(const char *name)
{
  int sue=1,lpath;
  char *filepath,*filename,tmp_string[20];

  strtoupper(tmp_string,name);
  if (strmatch(tmp_string,"CCP4")) {
    filepath = (char *) getenv("CCP4");
    if (filepath) {
      lpath = strlen(filepath);
      filename = (char *) ccp4_utils_malloc(sizeof(char)*(lpath+12));
      strcpy(filename,filepath);
      strcpy(filename+lpath,"/.agree2ccp4");
      if (ccpexists(filename)) sue = 0;
    }
    if (sue == 1) {
      filepath = (char *) getenv("HOME");
      if (filepath) {
        lpath = strlen(filepath);
        filename = (char *) ccp4_utils_malloc(sizeof(char)*(lpath+12));
        strcpy(filename,filepath);
        strcpy(filename+lpath,"/.agree2ccp4");
        if (ccpexists(filename)) sue = 0;
      }
    }
    if (sue == 1) {
      ccperror(1,"Cannot find required license agreements!");
      return 0;
    }
  }
  return 1;
}
 