/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/

/** @file ccp4_general_f.c
 *  Fortran API to ccp4_general.c.
 *  Created Oct. 2001 by Martyn Winn
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "ccp4_errno.h"
#include "ccp4_fortran.h"
#include "ccp4_parser.h"
#include "ccp4_program.h"
#include "ccp4_lib.h"
#include "ccplib.h"
static char rcsid[] = "$Id$";

FORTRAN_SUBR ( CCPFYP, ccpfyp,
               (),
               (),
               ())
{ int argc, i, ierr, arg_len=500, debug=0;
  char **argv=NULL, arg[500];

  /* turn on line buffering for stdout from C (don't think this affects
     Fortran side). This ensures we get library messages, but will slow
     things down. Is this what we want? */
  if(ccp4_utils_outbuf())
    ccp4_utils_print("OUTBUF:Can't turn off output buffering");
  /* Turn off any buffering on input. This allows mized C and Fortran
     reading of stdin */
  if(ccp4_utils_noinpbuf())
    ccp4_utils_print("NOINPBUF:Can't turn off input buffering");

  if (debug) 
    printf(" Entering CCPFYP \n");

/* couldn't find a C equivalent to this. In any case, since
   these functions are for Fortran programs, this may be
   the only way?? */

  /* IARGC doesn't include argv[0] */
  argc = FORTRAN_CALL (IARGC, iargc, (), (), ()) + 1;
  argv = (char **) ccp4_utils_malloc(argc*sizeof(char *));
  if (debug) 
    printf("Allocating memory for %d command line arguments \n",argc);
  for (i = 0; i < argc; ++i) {
    FORTRAN_CALL (GETARG, getarg, (&i,arg,arg_len), (&i,arg), (&i,arg,arg_len));
    argv[i] = ccp4_FtoCString(arg,arg_len);
    if (debug) 
      printf("CCPFYP: command line argument %d %s\n",i,argv[i]);
  }

  /* Do the preprocessing and return the error status */
  ierr = ccp4fyp(argc, argv);

  /* Calls to ccp4_FtoCString allocate memory for argv[..]
     which needs to be explicitly freed before leaving this
     function */
  for (i = 0; i < argc; i++) {
    if (argv[i]) {
      free(argv[i]);
    }
  }
  /* Also need to free argv itself */
  if (argv) free(argv);

  /* Now act on any errors from ccp4fyp */
  if (ierr) {
    /* Pass the error status and last error message to ccperror */
    ccperror(ierr,(char*) ccp4_strerror(ccp4_errno));
  }

  if (debug) 
    printf(" Leaving CCPFYP \n");
  return;
}

FORTRAN_SUBR ( CCPUPC, ccpupc,
               (fpstr string, int string_len),
               (fpstr string),
               (fpstr string, int string_len))
{
  char *string2, *string3;

  string2 = ccp4_FtoCString(FTN_STR(string), FTN_LEN(string));
  if (!string2) return;
  string3 = (char *) ccp4_utils_malloc((strlen(string2)+1)*sizeof(char));
  strtoupper(string3, string2);
  string3[strlen(string3)] = '\0';

  ccp4_CtoFString(FTN_STR(string), FTN_LEN(string), string3);

  free((char *) string2);
  free((char *) string3);
}

FORTRAN_SUBR ( CCPLWC, ccplwc,
               (fpstr string, int string_len),
               (fpstr string),
               (fpstr string, int string_len))
{
  char *string2, *string3;

  string2 = ccp4_FtoCString(FTN_STR(string), FTN_LEN(string));
  string3 = (char *) ccp4_utils_malloc(strlen(string2)*sizeof(char));

  strncpy(string, strtolower(string3, string2), strlen(string2)); 

  free((char *) string2);
  free((char *) string3);
}

FORTRAN_SUBR ( CCPERR, ccperr,
               (const int *istat, const fpstr errstr, int errstr_len),
               (const int *istat, const fpstr errstr),
               (const int *istat, const fpstr errstr, int errstr_len))
{ 
  char *tmp_errstr;

  tmp_errstr = ccp4_FtoCString(FTN_STR(errstr), FTN_LEN(errstr));
  ccperror(*istat, tmp_errstr);

  free((char *) tmp_errstr);
}

FORTRAN_SUBR ( QPRINT, qprint,
               (const int *iflag, const fpstr msg, int msg_len),
               (const int *iflag, const fpstr msg),
               (const int *iflag, const fpstr msg, int msg_len))
{ 
  char *tmp_msg;

  tmp_msg = ccp4_FtoCString(FTN_STR(msg), FTN_LEN(msg));
  ccp4printf(*iflag,"%s\n",tmp_msg); 

  free((char *) tmp_msg);
}

FORTRAN_FUN ( int, LENSTR, lenstr,
               (fpstr string, int string_len),
               (fpstr string),
               (fpstr string, int string_len))
{
  return ( (int) ccp4_utils_flength (FTN_STR(string), FTN_LEN(string)) );
}

/** Fortran wrapper to integer data function.
 * @param imonth Month (1-12).
 * @param iday Day (1-31).
 * @param iyear Year (4 digit).
 */
FORTRAN_SUBR ( UIDATE, uidate,
               (int *imonth, int *iday, int *iyear),
               (int *imonth, int *iday, int *iyear),
               (int *imonth, int *iday, int *iyear))
{ 
  int iarray[3];

  ccp4_utils_idate (iarray);
  *imonth = iarray[1];
  *iday = iarray[0];
  *iyear = iarray[2];
}

/** Fortran wrapper to string data function.
 * @param caldat Date string in format dd/mm/yy.
 */
FORTRAN_SUBR ( CCPDAT, ccpdat,
               (fpstr caldat, int caldat_len),
               (fpstr caldat),
               (fpstr caldat, int caldat_len))
{ 
  char date[11];

  ccp4_utils_date(date);
  /* convert 4-digit year to old-style 2-digit year */
  date[6] = date[8];
  date[7] = date[9];
  date[8] = '\0';
  ccp4_CtoFString(FTN_STR(caldat),FTN_LEN(caldat),date);
}

FORTRAN_SUBR ( CCPTIM, ccptim,
               (int *iflag, float *cpu, float *elaps),
               (int *iflag, float *cpu, float *elaps),
               (int *iflag, float *cpu, float *elaps))
{ 
  static int tim0;
  static float tlast;
  float tarray[2];

  if (*iflag == 0) {
    *elaps = 0.0;
    tim0 = time(NULL);
    *cpu = tlast = ccp4_utils_etime(tarray);
  } else {
    *elaps = time(NULL) - tim0;
    *cpu = ccp4_utils_etime(tarray) - tlast;
  }

}

FORTRAN_SUBR ( UTIME, utime,
               (fpstr ctime, int ctime_len),
               (fpstr ctime),
               (fpstr ctime, int ctime_len))
{ 
  char time[9];

  ccp4_CtoFString(FTN_STR(ctime),FTN_LEN(ctime),ccp4_utils_time(time));

}

FORTRAN_SUBR ( UCPUTM, ucputm,
               (float *sec), (float *sec), (float *sec))
{
  static float tlast;
  float tarray[2];

  if (*sec == 0.0) {
    *sec = tlast = ccp4_utils_etime(tarray);
  } else {
    *sec = ccp4_utils_etime(tarray) - tlast;
  }

}

FORTRAN_SUBR ( CCP4_VERSION, ccp4_version,
               (const fpstr version, int version_len),
               (const fpstr version),
               (const fpstr version, int version_len))
{ 
  ccp4_CtoFString(FTN_STR(version), FTN_LEN(version), CCP4_VERSION_NO); 
}

FORTRAN_SUBR ( CCP4_PROG_VERSION, ccp4_prog_version,
               (const fpstr version, int *iflag, int version_len),
               (const fpstr version, int *iflag),
               (const fpstr version, int version_len, int *iflag))
{ 
  char *tmp_vers;

  if (*iflag) {
    ccp4_CtoFString(FTN_STR(version), FTN_LEN(version), ccp4_prog_vers(NULL)); 
  } else {
    tmp_vers = ccp4_FtoCString(FTN_STR(version), FTN_LEN(version));   
    ccp4_prog_vers(tmp_vers);
    free((char *) tmp_vers);
  }
}

FORTRAN_SUBR ( CCPVRS, ccpvrs,
               (const int *ilp, const fpstr prog, const fpstr vdate,
                                int prog_len, int vdate_len),
               (const int *ilp, const fpstr prog, const fpstr vdate),
               (const int *ilp, const fpstr prog, int prog_len,
		                const fpstr vdate, int vdate_len))

{ 
  char *tmp_prog;

  tmp_prog = ccp4_FtoCString(FTN_STR(prog), FTN_LEN(prog));
  ccp4ProgramName(tmp_prog);
  ccp4_banner();

  free((char *) tmp_prog);
}

FORTRAN_SUBR ( CCPRCS, ccprcs,
               (const int *ilp, const fpstr prog, const fpstr rcsdat,
                                int prog_len, int rcsdat_len),
               (const int *ilp, const fpstr prog, const fpstr rcsdat),
               (const int *ilp, const fpstr prog, int prog_len,
		                const fpstr rcsdat, int rcsdat_len))

{ 
  char *tmp_prog;

  tmp_prog = ccp4_FtoCString(FTN_STR(prog), FTN_LEN(prog));
  ccp4ProgramName(tmp_prog);
  ccp4_banner();

  free((char *) tmp_prog);
}

FORTRAN_SUBR ( CCPPNM, ccppnm,
               (const fpstr pnm, int pnm_len),
               (const fpstr pnm, int pnm_len),
               (const fpstr pnm, int pnm_len))

{ 
  ccp4_CtoFString(FTN_STR(pnm), FTN_LEN(pnm), ccp4ProgramName(NULL)); 
}

FORTRAN_FUN ( ftn_logical, CCPEXS, ccpexs,
               (const fpstr name, int name_len),
               (const fpstr name),
               (const fpstr name, int name_len))
{
  char *string, *string1;

  string = ccp4_FtoCString(FTN_STR(name), FTN_LEN(name));
  string1 = (char *) getenv(string);
  if (string1) strcpy(string,string1);

  return ( ccpexists(string) ? FORTRAN_LOGICAL_TRUE : FORTRAN_LOGICAL_FALSE );
}

FORTRAN_FUN ( ftn_logical, CCPLIC, ccplic,
               (const fpstr name, int name_len),
               (const fpstr name),
               (const fpstr name, int name_len))
{
  char *string;

  string = ccp4_FtoCString(FTN_STR(name), FTN_LEN(name));

  return ( ccp4_licence_exists(string) ? FORTRAN_LOGICAL_TRUE : FORTRAN_LOGICAL_FALSE );
}
