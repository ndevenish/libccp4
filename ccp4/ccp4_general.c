/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/

/** @file ccp4_general.c
 *  General library functions and utilities.
 *  Peter Briggs
 */

/*   ccplib.c
     Peter Briggs CCP4 May 2001

     General library functions and utilities

     ccperror(level,message)
     Error reporting and program termination

     ccp4printf(level,format,printargs)
     A wrapper for vprintf - acts like printf with an additional
     argument which is checked against the reference verbosity
     level.

     ccp4fyp(argc,argv)
     Initialise environment for CCP4 programs and parse the
     command line arguments
     FIXME doesn't obey the CCP4FYP search logic for environ.def
     and default.def

     ccp4setenv(logical_name,value,envname,envtype,envext,ienv,
     no_overwrt)
     Set up file names and associate with logical_name in
     environment
     FIXME doesn't properly handle unrecognised input

     ccpexists(filename)
     Check if file exists and can be opened for reading

     ccpputenv(logical_name,file_name)
     Set environment variable logical_name to have value file_name
*/

/*------------------------------------------------------------------*/

/* CCP4 library clones */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>

/* Library header file */
#include "ccplib.h"
#include "ccp4_lib.h"
#include "ccp4_parser.h"
#include "ccp4_program.h"
static char rcsid[] = "$Id$";

/*------------------------------------------------------------------*/

/* ccperror

   Error reporting and program termination

   ccperror is called with a message level ierr and a message string
   The exact behaviour is determined by the value of ierr:

   0 : normal termination and stop
   1 : fatal error and stop
   2 : report severe warning
   3 : report information
   4 : report from library

   ierr=-1 also reports last system error and terminates.

   The text in message is sent to standard output, and to standard
   error for ierr=1 (or -1).

*/
int ccperror(int ierr, char* message)
{
  char *prog_name=NULL;

  /* Get the program name */
  prog_name = ccp4ProgramName(NULL);
  if (!prog_name) prog_name = strdup("CCP4");

  if (ierr==0) {
    /* Level 0 : normal termination */
    ccp4printf(0," %s:  %s\n",prog_name,message);
    /* Get the amount of time elapsed since start of
       program. Initialised by ccp4fyp */
    ccp4ProgramTime(0);
    exit(0);

  } else if (ierr==1 || ierr==-1) {
    /* Level 1 (-1) : fatal error */
    /* If ierr=-1 then print last system error
       N.B. Use of perror in this context is untested by me */
    if (ierr < 0) perror("Last system error message");
    /* Need to get the amount of time elapsed since start of
       program
       This would be initialised by ccp4fyp */
    /* Send the message to the standard error */
    fprintf(stderr," %s:  %s\n",prog_name,message);
    /* Also to the standard out */
    ccp4printf(0," %s:  %s\n",prog_name,message);
    exit(1);

  } else if (ierr==2) {
    /* Level 2 : severe warning */
    ccp4printf(0," \n $TEXT:Warning: $$ comment $$ \n WARNING: %s\n $$\n",
	   message);

  } else if (ierr>2)  {
    /* Levels higher than 2 : report information */
    ccp4printf(0,"%s\n",message);
  }
  return 0;
}

/*------------------------------------------------------------------*/

/* ccp4printf

   This is a wrapper for vprintf

   If the supplied message is less than or equal to the reference
   verbosity level then the format string and remaining arguments
   are passed to vprintf to be printed on stdout.
   Otherwise nothing is printed.

   The format string has the same format as the format strings
   passed to printf, with the remaining arguments being the values
   (if any) to be inserted into placeholders in the format string.

   Returns the number of bytes printed, or zero if no printing was
   performed, or a negative number for an error from vprintf.
*/
int ccp4printf(int level, char *format, ...)
{
  int     nbytes=0;
  va_list args;

  /* Check the level against the refence verbosity level */
  if (level <= ccp4VerbosityLevel(-1)) { 
    /* Use the vprint function to print */
    va_start(args,format);
    nbytes = vprintf(format,args);
    va_end(args);
  }
  /* Return to calling function */
  return nbytes;
}

/*------------------------------------------------------------------*/

/* ccp4fyp

   Initialise environment for CCP4 programs and parse the
   command line arguments
*/
int ccp4fyp(int argc, char **argv)
{
  int  i,iarg=1,arg_end=0,ienv=0,diag=0;
  int  ihelp,def_init=1,env_init=1;
  char *testarg=NULL,line[CCP4_MAXLINE];
  char *envname[CCP4_MAXNAMES],*envtype[CCP4_MAXNAMES],*envext[CCP4_MAXNAMES];
  char *def_file=NULL,*env_file=NULL,*cinclude=NULL;
  FILE *envfp,*deffp;
  char *logical_name=NULL,*file_name=NULL,*file_type=NULL,*file_ext=NULL;
  int  lfilename;
  char *envfilename,*deffilename;
  CCP4PARSERARRAY *parser;

  /* Begin */
  if (diag) printf("CCP4FYP: starting\n");

  if (diag) 
   for (i = 0; i < argc; ++i) 
      printf("ccp4fyp: comand line argument %d %s\n",i,argv[i]);

  /* Initialise timing information */
  ccp4ProgramTime(1);

  ccp4ProgramName(ccp4_utils_basename(argv[0])); 

  /* Process command line option switches first
     Ignore iarg=0 because this is the executable name */

  while (iarg < argc && !arg_end) {
    if (diag) printf("CCP4FYP: command line argument %d = \"%s\"\n",iarg,argv[iarg]);
    if (argv[iarg][0] == '-') {
      if (diag) printf("--> This is an option switch\n");
      testarg = (char *) ccp4_utils_malloc((strlen(argv[iarg])+1)*sizeof(char));
      strtoupper(testarg,&(argv[iarg][1]));
      if (diag) printf("--> Truncated and uppercased it is now \"%s\"\n",testarg);

      /* Test for each possible option:
	 -v(erbose) 0-9  verbose output level
	 -h(elp) 0-9     (alias for -v)
	 -n              don't read environ.def and default.def
	 -d <filename>   use <filename> instead of default.def
	 -e <filename>   use <filename> instead of environ.def
	 -i              print CCP4 library version, program name and
	                 program version to standard output, and exit. 
	 -nohtml         suppress printing of html tags
	 -nosummary      suppress printing of summary tags
      */

      /* Verbose option
	 -v <0-9>
	 -h <0-9>
      */
      /* The message levels need to be store somewhere globally
	 accessible
	 Level 0 switches off all output (silent mode)
	 Level 9 prints everything
	 Level 1 is normal I guess
	 It is not clear from documentation precisely what is/is not
	 output for the levels between 0 and 9
      */
      if (testarg[0] == 'V' || testarg[0] == 'H') {
	iarg++;
	if (diag) printf("--> Identified -v option:");
	if (iarg < argc) {
	  if (strlen(argv[iarg]) == 1 && isdigit(argv[iarg][0])) {
	    ihelp = atoi(argv[iarg]);
	  } else {
	    ihelp = 1;
	  }
	  if (diag) printf(" level %d\n",ihelp);
	} else {
	  ihelp = 1;
	  if (diag) printf(" no help level - defaulting to 1\n");
	}

	/* Don't read environ.def and default.def
	   -n
	*/
      } else if (testarg[0] == 'N' && strlen(testarg) == 1) {
	if (diag) printf("--> Identified -n option\n");
	def_init = 0;
	env_init = 0;

	/* Use non-default default.def file
	   -d <filename>
	*/
      } else if (testarg[0] == 'D') {
	iarg++;
	if (diag) printf("--> Identified -d option:");
	if (iarg < argc) {
	  def_file = (char *) ccp4_utils_malloc(sizeof(char)*(strlen(argv[iarg])+1));
	  if (!def_file) ccperror(1,"Can't fetch filename for -d option");
	  strcpy(def_file,argv[iarg]);
	  def_init = 1;
	  if (diag) printf(" default.def file is \"%s\"\n",def_file);
	} else {
	  if (diag) printf(" no filename found\n");
	  ccperror(1,"Use: -d filename");
	}

	/* Use non-default environ.def file
	   -e <filename>
	*/
      } else if (testarg[0] == 'E') {
	iarg++;
	if (diag) printf("--> Identified -e option:");
	if (iarg < argc) {
	  env_file = (char *) ccp4_utils_malloc(sizeof(char)*(strlen(argv[iarg])+1));
	  if (!env_file) ccperror(1,"Can't fetch filename for -d option");
	  strcpy(env_file,argv[iarg]);
	  def_init = 1;
	  if (diag) printf(" environ.def file is \"%s\"\n",env_file);
	} else {
	  if (diag) printf(" no filename found\n");
	  ccperror(1,"Use: -e filename");
        }

	/* Info option
	   -i(nfo)
	*/
      } else if (testarg[0] == 'I') {
	if (diag) printf("--> Identified -i(nfo) option\n");
	/* Print program information and stop */
	printf("CCP4 software suite: library version %s\n",CCP4_VERSION_NO);
	printf("CCP4 software suite: patch level     %s\n",CCP4_PATCH_LEVEL);
	printf("Program:             %s",ccp4ProgramName(NULL));
	if (ccp4_prog_vers(NULL)) printf("; version %s",ccp4_prog_vers(NULL));
	printf("\n");
	exit(0);

	/* Unrecognised switch */
      } else {
	ccp4printf(1,"Ignoring unrecognised switch \"%s\"\n",argv[iarg]);
      }

      /* Next argument */
      iarg++;

    } else {
      /* Found an argument which doesn't start with a "-"
	 This is the end of the command line switches */
      if (diag) printf("CCP4FYP: end of command line switches\n");
      arg_end = 1;
    }
  }

  /* Initialise debug (verbosity) level */
  ccp4VerbosityLevel(ihelp);

  /* Initialise html, summary tags */
  /* NOT IMPLEMENTED */

  /* Read in environ.def */
  if (env_init) {
    if (diag) printf("CCP4FYP: reading environ.def file\n");
    /* Non-standard file? */
    if (env_file) {
      if (diag) printf("--> environ.def file was supplied on the command line\n");
    } else {
      /* Use the standard environ.def in $CINCL */
      if (diag) printf("--> use standard environ.def file\n");
      /* Get value of CINCL variable */
      cinclude = (char *) getenv("CINCL");
      if (!cinclude) {
	if (diag) printf("--> CINCL env var has no value assigned\n");
      } else {
	if (diag) printf("--> CINCL is \"%s\"\n",cinclude);
	/* Set the full path for the environ.def file */
	lfilename = strlen(cinclude) + strlen("environ.def") + 2;
	envfilename = (char *) ccp4_utils_malloc(sizeof(char)*lfilename);
	if (!envfilename)
	  ccperror(-1,"CCP4FYP: cannot set environ.def pathname");
	strcpy(envfilename,cinclude);
        envfilename[strlen(cinclude)] = PATH_SEPARATOR;
        envfilename[strlen(cinclude)+1] = '\0';
	strcat(envfilename,"environ.def");
	if (diag) printf("--> Full path for environ.def is \"%s\"\n",envfilename);
	/* Open the environ.def file as read-only*/
	ccp4printf(2,"Opening file \"%s\"\n",envfilename);
	envfp = fopen(envfilename,"r");
	if (!envfp)
	  ccperror(1,"CCP4FYP: failed to open environ.def");

        /* Set a ccp4_parser array to deal with input from
	   environ.def */
	parser = (CCP4PARSERARRAY *) ccp4_parse_start(CCP4_MAXTOKS);

	/* Set the delimiters to whitespace, = and .
	   This should split lines into the three components */
	ccp4_parse_delimiters(parser," \t=.",NULL);

	/* Read from the file until EOF*/
	while (fgets(line,CCP4_MAXLINE,envfp)) {

	  /* Remove the trailing newline from fgets */
	  line[strlen(line)-1] = '\0';

	  /* Use ccp4_parse to get the tokens on each line */
	  ccp4_parse_reset(parser);
	  if (ccp4_parse(line,parser) == 3) {
	    logical_name = parser->token[0].fullstring;
	    file_type    = parser->token[1].fullstring;
	    file_ext     = parser->token[2].fullstring;
	    if (!logical_name || !file_type || !file_ext)
	      ccperror(-1,"CCP4FYP: couldn't parse line from environ.def");

	    /* environ.def contains lines of the form
	       LOGICALNAME=type.ext # comments
	       where type is "in", "out" or "inout"
	       and ext is the default extension, e.g. "mtz" or "scr"
	    */

	    /* Store in arrays for use when decoding default.def
	       and logical names on command line */
	    if (ienv+1 == CCP4_MAXNAMES)
	      ccperror(1,"CCP4FYP: too many logical names in environ.def file");

	    /* Logical name in envname */
	    envname[ienv] = (char *)
	      ccp4_utils_malloc(sizeof(char)*(strlen(logical_name)+1));
	    strcpy(envname[ienv],logical_name);

	    /* File type in envtype */
	    envtype[ienv] = (char *)
	      ccp4_utils_malloc(sizeof(char)*(strlen(file_type)+1));
	    strcpy(envtype[ienv],file_type);

	    /* File extension in envext */
	    envext[ienv] = (char *)
	      ccp4_utils_malloc(sizeof(char)*(strlen(file_ext)+1));
	    strcpy(envext[ienv],file_ext);
	    
	    if (diag) printf("Decoded line: %s = %s.%s\n",envname[ienv],
		   envtype[ienv],envext[ienv]);

	    /* Increment ienv counter for number of name-pairs
	       read in */
	    ienv++;

	    /* Reset number of tokens before reading next line */
	    ccp4_parse_reset(parser);

	  } 
	}
	/* Close the environ.def file */
	fclose(envfp);

	/* Finished with the parser array */
	ccp4_parse_end(parser);
      }
    }
  }

  /* Read in default.def */
  if (def_init) {
    if (diag) printf("CCP4FYP: reading default.def file\n");
    /* Non-standard file? */
    if (def_file) {
      if (diag) printf("--> default.def file was supplied on the command line\n");
    } else {
      /* Use the standard environ.def in $CINCL */
      if (diag) printf("--> use standard default.def file\n");
      /* Get value of CINCL variable */
      cinclude = (char *) getenv("CINCL");
      if (!cinclude) {
	if (diag) printf("--> CINCL env var has no value assigned\n");
      } else {
	if (diag) printf("--> CINCL is \"%s\"\n",cinclude);
	/* Set the full path for the default.def file */
	lfilename = strlen(cinclude) + strlen("default.def") + 2;
	deffilename = (char *) ccp4_utils_malloc(sizeof(char)*lfilename);
	if (!deffilename)
	  ccperror(-1,"CCP4FYP: cannot set default.def pathname");
	strcpy(deffilename,cinclude);
        deffilename[strlen(cinclude)] = PATH_SEPARATOR;
        deffilename[strlen(cinclude)+1] = '\0';
	strcat(deffilename,"default.def");
	if (diag) printf("--> Full path for default.def is \"%s\"\n",deffilename);
	/* Open the default.def file as read-only*/
	ccp4printf(2,"Opening file \"%s\"\n",deffilename);
	deffp = fopen(deffilename,"r");
	if (!deffp)
	  ccperror(1,"CCP4FYP: failed to open default.def");

        /* Set a ccp4_parser array to deal with input from
	   default.def */
	parser = (CCP4PARSERARRAY *) ccp4_parse_start(CCP4_MAXTOKS);

	/* Set the delimiters to whitespace and =
	   This should split lines into the two components */
	ccp4_parse_delimiters(parser," \t=",NULL);

	/* Read from the file until EOF*/
	while (fgets(line,CCP4_MAXLINE,deffp)) {

	  /* Remove the trailing newline from fgets */
	  line[strlen(line)-1] = '\0';

	  /* Use ccp4_parse to get the tokens on each line */
	  ccp4_parse_reset(parser);
	  if (ccp4_parse(line,parser) == 2) {
	    logical_name = parser->token[0].fullstring;
	    file_name    = parser->token[1].fullstring;
	    if (!logical_name || !file_name)
	      ccperror(-1,"CCP4FYP: couldn't parse line from default.def");

	    /* default.def contains lines of the form
	       LOGICALNAME=FILENAME # comments
	    */
	    if (diag) printf("Decoded line: %s = %s\n",logical_name,file_name);

	    /* Set up the environment for this pair
	       Don't overwrite any existing logical name */
	    ccp4setenv(logical_name,file_name,envname,envtype,envext,ienv,1);

      	    /* Reset number of tokens before reading next line */
            ccp4_parse_reset(parser);
	  }  
	}
	/* Close the default.def file */
	fclose(deffp);

	/* Finished with the parser array */
	ccp4_parse_end(parser);
      }
    }
  }

  /* Read in the rest of the command line arguments
     These should consist of pairs of arguments i.e.
     <logical name> <file name>
  */

  ccp4printf(2,"Processing Command Line Arguments\n");
  while (iarg < argc) {
    /* Get logical name and uppercase it */
    logical_name = (char *) ccp4_utils_malloc((strlen(argv[iarg])+1)*sizeof(char));
    if (diag) printf("--> Raw logical name: \"%s\"\n",argv[iarg]);
    strtoupper(logical_name,argv[iarg]);
    logical_name[strlen(argv[iarg])] = '\0';
    if (diag) printf("--> Logical name: \"%s\"",logical_name);
    iarg++;
    /* Get associated filename */
    if (iarg < argc) {
      file_name = (char *) ccp4_utils_malloc((strlen(argv[iarg])+1)*sizeof(char));
      strcpy(file_name,argv[iarg]);
      if (diag) printf("   file name: \"%s\"\n",file_name);
      /* Set up the environment for this pair
	 Do overwrite any existing logical name */
      ccp4setenv(logical_name,file_name,envname,envtype,envext,ienv,0);
      iarg++;
    } else {
      /* No associated filename - exit with error */
      if (diag) printf("   no associated logical name\n");
      ccperror(1,"Use: <logical name> <filename> ...");
    }
    /* Next pair of arguments */
  }
  ccp4printf(2,"End of pre-processing stage\n");

  /* End of CCP4FYP */
  if (diag) printf("CCP4FYP: ending\n");
  return 0;
}

/*------------------------------------------------------------------*/

/* ccp4setenv

   Set environment variables

   Associates logical_name with value. It is passed arrays of name,
   type and extension for ienv number of name lines read from
   environ.def, which may be used to modify the value before
   assignment.

   If no_overwrt is true then any existing logical_name are not
   redefined.
*/
int ccp4setenv(char *logical_name, char* value, char **envname,
	      char **envtype, char **envext, int ienv, int no_overwrt)
{
  int  diag=0;
  int  icount,lext=0,lroot=0,lpath=0,lname,procid;
  char tmp_name[]="programX",*clibd,*cscr;
  char *file_ext=NULL,*file_root=NULL,*file_path=NULL,*file_name=NULL;
  char *prog_name,*tmpstr1;
  char errstr[81];

  /* Begin */
  if (diag) printf("CCP4SETENV: started, ienv = %d\n",ienv);

  /* Exit if the logical name already exists and we are in
     no-overwrite mode */
  if (getenv(logical_name) && no_overwrt) return 0;
  
  /* Get the program (executable) name */
  /* This is actually the first argument from the argument list
     argv[0] which will need to be passed to ccp4setenv, or accessed
     from some global variable */
  prog_name = tmp_name;
  if (diag) printf("CCP4SETENV: program name = \"%s\"\n",prog_name);

  /* Look for a match between logical_name and the names from
     environ.def */
  if (diag) printf("CCP4SETENV: looking for a match to logical name \"%s\"\n",
		   logical_name);
  /* compare on strlen(envname[icount]) characters, so that e.g. HKLIN1 will match */
  icount = 0;
  while (icount<ienv && strncmp(logical_name,envname[icount],strlen(envname[icount]))) {
    icount++;
  }
  if (icount == ienv) {
    /* Not in the list - non-standard logical name */
    if (diag) printf("%s is non-standard logical name\n",logical_name);
    /* Add it to the list */
    /* FIXME: This means altering ienv etc ... not doing it now */
  } else {
    if (diag) printf("CCP4SETENV: Matched logical name for number %d\n",icount);
  }

  /* Split the supplied value up into components to extract get the
     file extension, the file root (i.e. name less extension) and
     the file path
     It is possible there is no path and/or no extension */
  if (diag) printf("CCP4SETENV: supplied file = \"%s\"\n",value); 

  /* Get file path */
  file_path = ccp4_utils_pathname(value);
  lpath = strlen(file_path)-1;
  if (diag) printf("CCP4SETENV: path = \"%s\"\n",file_path); 

  /* Get file extension */
  file_ext = ccp4_utils_extension(value);
  lext = strlen(file_ext);
  if (diag) printf("CCP4SETENV: extension = \"%s\"\n",file_ext); 

  /* Get file root */
  file_root = ccp4_utils_basename(value);
  lroot = strlen(file_root);
  if (diag) printf("CCP4SETENV: root = \"%s\"\n",file_root); 

  /* Add the appropriate file extension if none was supplied
     The exception is for /dev/null or NL: */
  if (!strmatch(value,"/dev/null") && !strmatch(value,"NL:")) {
    if (lext <= 0) {
      /* Add extension */
      if (icount < ienv) {
	lext = strlen(envext[icount]);
	file_ext = (char *) ccp4_utils_malloc(sizeof(char)*(lext+1));
	strncpy(file_ext,envext[icount],(lext+1));
	if (diag) printf("CCP4SETENV: added extension \"%s\"\n",file_ext);
      } else {
	if (diag) printf("CCP4SETENV: not adding extension for non-standard file name\n");
      }
    }
    /* If no path supplied then get appropriate path depending
       on the extension
       .dic, .lib, .bes, .prt = $CLIBD
       .scr = $CCP4_SCR
    */
    if (lpath < 0) {
      /* Fetch the appropriate path name from the environment */
      
      if (strmatch(file_ext,"lib") || strmatch(file_ext,"dic")
	  || strmatch(file_ext,"bes") || strmatch(file_ext,"prt")) {
	/* Fetch CLIBD */
	clibd = (char *) getenv("CLIBD");
	if (clibd) {
	  if (diag) printf("CCP4SETENV: CLIBD = \"%s\"\n",clibd);
	  /* Store in file_path */
	  lpath = strlen(clibd);
	  file_path = (char *) ccp4_utils_malloc(sizeof(char)*(lpath+1));
	  strncpy(file_path,clibd,(lpath+1));
	  if (diag) printf("CCP4SETENV: set file path to CLIBD = \"%s\"\n",file_path);
	} else {
	  /* Couldn't get CLIBD */
	  ccperror(1,"CCP4SETENV: couldn't get CLIBD from environment - check setup");
	}

      } else if (strmatch(file_ext,"scr")) {
	/* Fetch CCP4_SCR */
	cscr = (char *) getenv("CCP4_SCR");
	if (cscr) {
	  if (diag) printf("CCP4SETENV: CCP4_SCR = \"%s\"\n",cscr);
	  /* Store in file_path */
	  lpath = strlen(cscr);
	  file_path = (char *) ccp4_utils_malloc(sizeof(char)*(lpath+1));
	  strncpy(file_path,cscr,(lpath+1));
	  if (diag) printf("CCP4SETENV: set file path to CCP4_SCR = \"%s\"\n",file_path);
	} else {
	  /* Couldn't get CCP4_SCR */
	  ccperror(1,"CCP4SETENV: couldn't get CCP4_SCR from environment - check setup");
	}
	/* Replace scr extension with the process id
	 In fact to guarantee that it is always 5 characters,
	 take the id number modulo 100,000
	 FIXME there maybe rounding problems doing it this
	 way - should be fixed up */
	procid = (int) getpid();
	if (diag) printf("CCP4SETENV: initial procid = %d\n",procid);
        procid = procid % CCP4_MODULO;
	if (diag) printf("CCP4SETENV: procid = %d",procid);
	if (file_ext) free(file_ext);
	file_ext = (char*) ccp4_utils_malloc(sizeof(char)*6);
	sprintf(file_ext,"%05d",procid);
        lext = 5;
	if (diag) printf(" giving file extension \"%s\"\n",file_ext);
      }
      /* No special path for this particular extension */
    }
  } else {
    if (diag) printf("CCP4SETENV: detected dev-null\n");
  }

  /* Build the filename */
  lname = lpath + 1;
  file_name = (char *) ccp4_utils_realloc(file_name,sizeof(char)*(lname + 1));
  if (lpath < 0) {
    file_name[0] = '\0';
  } else if (lpath == 0) {
    file_name[0] = PATH_SEPARATOR;
    file_name[1] = '\0';
  } else {
    strncpy(file_name,file_path,lname);
    file_name[lpath] = PATH_SEPARATOR;
    file_name[lpath+1] = '\0';
  }
  if (diag) printf("CCP4SETENV: building filename = \"%s\"\n",file_name);
  lname = lname + lroot;
  file_name = (char *) ccp4_utils_realloc(file_name,sizeof(char)*(lname + 1));
  if (lroot) {
    strcat(file_name,file_root);
  }
  if (diag) printf("CCP4SETENV: building filename = \"%s\"\n",file_name);
  if (lext > 0) {
    lname = lname + lext + 1;
    file_name = (char *) ccp4_utils_realloc(file_name,sizeof(char)*(lname + 1));
    strcat(file_name,".");
    if (lext) {
      strcat(file_name,file_ext);
    }
    file_name[lname] = '\0';
  }
  if (diag) printf("CCP4SETENV: building filename = \"%s\"\n",file_name);

  /* Test that (non-default) input files exist */
  if (icount < ienv) {
    if (strmatch(envtype[icount],"in") && !no_overwrt) {
      /* Does file exist? */
      if (diag) printf("CCP4SETENV: checking for existence of input file\n");
      if (ccpexists(file_name)) {
	if (diag) printf("CCP4SETENV: \"%s\" can be opened for reading\n",file_name);
      } else {
	if (diag) printf("CCP4SETENV: \"%s\" cannot be opened for reading\n",file_name);
        sprintf(errstr,"Cannot find file: \"%s\" ",file_name);
	ccperror(-1,errstr);
      }
    }
  } else {
    if (diag) printf("CCP4SETENV: cannot determine file type (input or output)\n");
  }

  /* Set the environment variable */
  if (ccpputenv(logical_name,file_name)) {
    if (diag) printf("CCP4SETENV: ccpputenv returned okay\n");
  } else {
    ccperror(-1,"Cannot create environment variable");
  }

  if (diag) {
    tmpstr1 = (char *) getenv(logical_name);
    if (tmpstr1) {
      printf("CCP4SETENV: logical name %s has value \"%s\"\n",logical_name,tmpstr1);
    } else {
      printf("CCP4SETENV: failed to fetch value for logical_name \"%s\"\n",logical_name);
    }
  }

  /* Free dynamically allocated memory before returning */
  if (file_ext) free(file_ext);
  if (file_root) free(file_root);
  if (file_path) free(file_path);
  if (file_name) free(file_name);

  return 1;
}

/*------------------------------------------------------------------*/

/* ccpexists

   Check if named file exists
   The check is performed by attempting to fopen the file for
   read only, then immediately closing the file. If this method
   proves to be unsatisfactory then it may be necessary to investigate
   using access or stat instead.

   Returns 1 if the file can be opened for read (=exists), 0 otherwise.
*/
int ccpexists(char *filename)
{
  FILE *fp;

  if (filename) {
    fp = fopen(filename,"r");
    if (fp) {
      fclose(fp);
      return 1;
    }
  }
  return 0;
}

/*------------------------------------------------------------------*/

/* ccpputenv

   This is a wrapper for the C putenv command. It must be supplied
   with a logical name (the name of a variable which will set in the
   environment) and a file name (which will be assigned to that
   variable).
   FIXME For some systems might also need to use setenv

   Returns 1 if successful and 0 otherwise.
*/
int ccpputenv(char *logical_name, char *file_name)
{
  int ltmpstr,diag=0;
  char *tmpstr;

  if (logical_name && file_name) {
    /* Allocate memory for temporary string */
    ltmpstr = strlen(logical_name) + strlen(file_name) + 1;
    tmpstr = (char *) ccp4_utils_malloc(sizeof(char)*(ltmpstr+1));
    /* putenv requires a string of the form "logical_name=file_name" */
    if (tmpstr) {
      strcpy(tmpstr,logical_name);
      strcat(tmpstr,"=");
      strcat(tmpstr,file_name);
      tmpstr[ltmpstr] = '\0';
      if (diag) printf("CCPPUTENV: string going into putenv is \"%s\"\n",tmpstr);
      /* putenv returns 0 on success */
      if (ccp4_utils_setenv(tmpstr) == 0) {
        /* free tmpstr here as ccp4_utils_setenv does separate malloc */
        free (tmpstr);
        return 1;
      }
    }
  }
  return 0;
}

void ccp4_banner(void) {

  int diag=0;
  char date[11],time[9],prog_vers_str[19];

  if (diag) printf("Entering ccp4_banner \n");

  if (ccp4_prog_vers(NULL)) {
    strcpy(prog_vers_str,ccp4_prog_vers(NULL));
  } else {
    strcpy(prog_vers_str," ");
  }

  printf(" \n");
  printf(" ###############################################################\n");
  printf(" ###############################################################\n");
  printf(" ###############################################################\n");
  printf(" ### CCP4 %3s: %-18s %-18s  ##########\n", 
       CCP4_VERSION_NO,ccp4ProgramName(NULL),prog_vers_str);
  printf(" ###############################################################\n");
  printf(" User: %s  Run date: %s Run time: %s \n\n\n",
       ccp4_utils_username(),ccp4_utils_date(date),ccp4_utils_time(time)); 
  printf(" Please reference: Collaborative Computational Project, Number 4. 1994.\n");
  printf(" \"The CCP4 Suite: Programs for Protein Crystallography\". Acta Cryst. D50, 760-763.\n");
  printf(" as well as any specific reference in the program write-up.\n\n");

  if (diag) printf("Leaving ccp4_banner \n");

}
