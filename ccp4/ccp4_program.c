/*
     ccp4_program.c: Utilies to set and fetch program information.
     Copyright (C) 2001  CCLRC, Peter Briggs

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

/** @file ccp4_program.c
 *  Utilies to set and fetch program information.
 *  Peter Briggs CCP4 May 2001
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ccp4_program.h"
#include "ccp4_parser.h"
#include "ccp4_utils.h"
#include "ccp4_general.h"
static char rcsid[] = "$Id$";

char *ccp4_prog_vers(char *progvers) 
{
  static char programversion[MAXLEN_PROGVERSION]="";
  int         i;

  if (progvers) {
    i = 0;
    while (progvers[i] != '\0' && i < MAXLEN_PROGVERSION) {
      programversion[i] = progvers[i];
      ++i;
    }
    if (i == MAXLEN_PROGVERSION) {
      programversion[MAXLEN_PROGVERSION-1] = '\0';
    } else {
      programversion[i] = '\0';
    }
  }
  return programversion;
}

/*------------------------------------------------------------------*/

/* ccp4ProgramName

   Set or return program name

   Always returns a pointer to the program name
   If progname is not NULL then set the program name to
   progname.

   NB Default program name will be returned as "CCP4",
   until reset by the calling subprogram.
*/
char *ccp4ProgramName(const char *progname)
{
  static char programname[MAXLEN_PROGNAME]="CCP4";
  int         i;

  if (progname) {
    i = 0;
    while (progname[i] != '\0' && i < MAXLEN_PROGNAME) {
      programname[i] = progname[i];
      ++i;
    }
    if (i == MAXLEN_PROGNAME) {
      programname[MAXLEN_PROGNAME-1] = '\0';
    } else {
      programname[i] = '\0';
    }
  }
  return programname;
}

/* ccp4RCSDate

   Set or return program RCS date

   If the input string is not a NULL pointer then
   it is assumed to be an RCS string
   This is processed to extract a date string in
   the form "DD/MM/YY" (day/month/year), which is
   then stored.

   ccp4RCSDate always returns the currently
   stored date string.
*/
char *ccp4RCSDate(const char *rcs_string)
{
  static char RCSDate[MAXLEN_RCSDATE]="";
  char        tmpstr1[8],tmpstr2[3];
  int         i;

  /* Deconstruct the RCS string passed to this
     function */
  if (rcs_string) {
    /* Extract useful data from RCS string for examination */
    strncpy(tmpstr1,rcs_string,7);
    tmpstr1[7] = '\0';
    strncpy(tmpstr2,rcs_string,2);
    tmpstr2[2] = '\0';
    if (strncmp(tmpstr1,"$Date: ",7) == 0) {
      /* Raw form of RCS string (not exported) i.e.:
	 "$Date$"
      */
      /* Build the date string in the form DD/MM/YY */
      strncpy(RCSDate,rcs_string+15,2);
      strncat(RCSDate,"/",1);
      strncat(RCSDate,rcs_string+12,2);
      strncat(RCSDate,"/",1);
      strncat(RCSDate,rcs_string+9,2);
    } else if (strlen(rcs_string) > 10 &&
	       (strncmp(tmpstr2,"19",2) == 0 || strncmp(tmpstr2,"20",2)) ) {
      /* RCS string after export i.e.:
	 "2003/05/14 11:45:13 ..." */
      /* Build the date string in the form DD/MM/YY */
      strncpy(RCSDate,rcs_string+8,2);
      strncat(RCSDate,"/",1);
      strncat(RCSDate,rcs_string+5,2);
      strncat(RCSDate,"/",1);
      strncat(RCSDate,rcs_string+2,2);
    } else {
      /* Fallback */
      strncpy(RCSDate,"",1);
    }
  }
  /* Always return the stored date */
  return RCSDate;
}

/* ccp4ProgramTime

   Set or print program time information
*/
void ccp4ProgramTime(int init)
{
  static int elaps0=0;
  static float tarray0[2];
  int elaps;
  float tarray[2];

  if (init || !elaps0 ) {
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
  char *filepath=NULL,*filename=NULL,tmp_string[20];

  strtoupper(tmp_string,name);
  if (strmatch(tmp_string,"CCP4")) {
    filepath = (char *) getenv("CCP4");
    if (filepath) {
      lpath = strlen(filepath);
      filename = (char *) ccp4_utils_malloc(sizeof(char)*(lpath+12));
      strcpy(filename,filepath);
      strcpy(filename+lpath,"/.agree2ccp4");
      if (ccpexists(filename)) sue = 0;
      /* Make sure that we clean up */
      if (filename) free(filename);
    }
    if (sue == 1) {
      filepath = (char *) getenv("HOME");
      if (filepath) {
        lpath = strlen(filepath);
        filename = (char *) ccp4_utils_malloc(sizeof(char)*(lpath+12));
        strcpy(filename,filepath);
        strcpy(filename+lpath,"/.agree2ccp4");
        if (ccpexists(filename)) sue = 0;
	/* Make sure that we clean up */
	if (filename) free(filename);
      }
    }
    if (sue == 1) {
      ccperror(1,"Cannot find required license agreements!");
      return 0;
    }
  }
  return 1;
}

/* html_log_output and summary_output currently only used by ccperror to
   tidy up Fortran program output. Defaults are 0 for C programs. */
int html_log_output(int ihtml_in) {
  static int ihtml=0;

  if (ihtml_in >= 0)
    ihtml = ihtml_in;
  return ihtml;
}

int summary_output(int isumm_in) {
  static int isumm=0;

  if (isumm_in >= 0)
    isumm = isumm_in;
  return isumm;
}
