/* FORTRAN API for library.c                                                */
/* also include missing routines and wrappers for C commands                */
/*                                                                          */
/* revisions:                                                               */
/*           (4/5/01) C.Ballard                                             */

/*                                                                          */
/* % Copyright Daresbury Laboratory 1992--1995                              */
/* % This is a CCP4 `part (i)' file for the purposes of copyright.          */
/* % See the CCP4 distribution conditions for explanation.                  */
/*                                                                          */
/* % \documentstyle[a4wide,times,noweb,makeidx]{article}                    */
/*                                                                          */
/* % \newcommand{\ac}[1]{{\rm\normalshape\sc #1}}   % acronym               */
/*                                                                          */
/* \documentclass{article}                                                  */
/* \usepackage{a4wide,times,noweb,makeidx}                                  */
/* \newcommand{\ac}[1]{\textsc{#1}}   % acronym                             */
/* \newcommand{\meta}[1]{\mbox{$\langle$\sl #1\/$\rangle$}}                 */
/* \newcommand{\ft}{\idx{Fortran}}                                          */
/* \newcommand{\idx}[1]{#1\index{#1}}                                       */
/* \newcommand{\fixme}[1]{\index{Fixme!}[{\bf Fixme!:} #1\@.]}              */
/*                                                                          */
/* \title{FORTRAN wrapper library routines}                                 */
/* \date{$ $Date$ $}                                  */
/* \author{This version: Dave Love, Daresbury}                              */
/*                                                                          */
/* \makeindex                                                               */
/*                                                                          */
/* \noweboptions{longchunks,smallcode}                                      */
/*                                                                          */
/* \begin{document}                                                         */
/*                                                                          */
/* \maketitle                                                               */
/*                                                                          */
/* \noindent                                                                */
/* This file contains the lowest level routines for the CCP4 Program        */
/* Suite, mainly for i/o (as required by the {\tt diskio} routines) and     */
/* bit-twiddling.  It's been partly re-engineered from a non-literate C     */
/* file and isn't properly documented yet.  \fixme{It should generate user  */
/*   documentation eventually}                                              */
/* \bigskip                                                                 */
/*                                                                          */
/* \tableofcontents                                                         */
/*                                                                          */
/*                                                                          */
/* \section{Summary}                                                        */
/*                                                                          */
/* The following routines are defined:                                      */
/* \bigskip                                                                 */
/*                                                                          */
/* \noindent                                                                */
/* \begin{tabular}{ll}                                                      */
/*                                                                          */
/* Routine and  arguments &                      Purpose \\                 */
/* \hline                                                                   */
/* [[ustenv(string, result)]]         & set an environment variable  \\     */
/* [[copen(iunit,filnam,istat)]]      & open random access file using [[fopen]] \\ */
/* [[qclose(iunit)]]                  & shut random access file using [[fclose]] \\ */
/* [[qmode(iunit,mode,nmcitm)]]       & change size of item in file ops. \\ */
/* [[qread(iunit,array,nitems,ier)]]  & [[fread]] from random access file \\ */
/* [[qwrite(iunit,array,nitems)]]     & [[fwrite]] to random access file \\ */
/* [[qrarch(iunit,ipos,ireslt)]] & set up diskio number translation \\      */
/* [[qwarch(iunit,ipos)]] & write `machine stamp' to diskio file \\         */
/* [[qseek(iunit,irec,iel,lrecl)]]    & [[fseek]] within random access file \\ */
/* [[qback(iunit,lrecl)]]             & backspace within random access file \\ */
/* [[qskip(iunit,lrecl)]]             & skip forward within random access file \\ */
/* [[cqinq(iunit,lfilnm,filnam,length)]] & inquire file status on the given stream \\ */
/* [[qlocate(iunit,locate)]]          & current position within random access file  */
/* \end{tabular}                                                            */
/*                                                                          */
/*                                                                          */
/* \section{Portability}                                                    */
/*                                                                          */
/* We aim for compatibility with K\&R\index{K&R@K\&R C} and                 */
/* \index{ANSI C@\ac{ansi} C} \ac{ansi}~C\@ as well as \idx{VAX             */
/*   C}\index{C!VAX}.  One particularly annoying                            */
/* consequence is that we can't rely on [[#elif]] preprocessor              */
/* directives.  I don't know whether anything needs to be changed for the   */
/* new \idx{DEC C}, which is apparently \ac{ansi}\dots{}                    */
/*                                                                          */
/*                                                                          */
/* \section{Code}                                                           */
/*                                                                          */
/* These are the components of the code.  The \LA{}guarded code\RA{} is     */
/* executed when we've identified the platform.                             */
/*                                                                          */
/* A literate program like this is designed for human consumption.  It      */
/* comprises `chunks' of code interspersed in commentary.  Chunks,          */
/* indicated by \LA{}\dots\RA{} in code, are macro-substituted by their     */
/* definitions (starting with \LA{}\dots\endmoddef) to produce              */
/* compilable code.  The definitions of chunks may be added to later, as    */
/* inidcated by \LA{}\dots\plusendmoddef.  Chunks are cross-referenced      */
/* by their trailing tag.                                                   */
/*                                                                          */
/* <*>=                                                                     */

#include "library.h"

/* This creates a null-terminated C string from an input
   string obtained from a Fortran call. Memory assigned
   by malloc, so can be freed. */

char *ccp4_FtoCString(fpstr str1, int str1_len)
{
  char *str2;

  size_t length = ccp4_flength(FTN_STR(str1),FTN_LEN(str1));
  if(!length)
    return NULL;
  str2 = (char *) ccp4malloc((length+1)*sizeof(char));
  strncpy(str2, FTN_STR(str1), length); 
  str2[length] = '\0';

  return str2;
}

/* Transfer C string to Fortran string for passing back to Fortran call.
   Characters after null-terminator may be junk, so pad with spaces. */

void ccp4_CtoFString(fpstr str1, int str1_len, const char *cstring)
{
  int i;

  if (str1_len > strlen(cstring)) {
    strcpy(FTN_STR(str1),cstring);
    for (i = strlen(cstring); i < FTN_LEN(str1); ++i) 
      str1[i] = ' ';
  } else {
    strncpy(FTN_STR(str1),cstring,str1_len);
  }
}

#if ! defined (VMS)
/* \section{Miscellaneous routines}                                         */
/* \subsection{{\tt subroutine ustenv(\meta{string}, \meta{result})}}       */
/*                                                                          */
/* This sets an environment variable \meta{var} to \meta{val}, where the    */
/* argument \meta{string}[[==']]\meta{var}[['//'='//']]\meta{val}[[']].     */
/* This is for use by the `\idx{logical name}' mechanism for specifying     */
/* file connexions.  Note that a \idx{VMS} varsion is supplied in {\tt      */
/*   vms.for} and that there is no standard way of setting and              */
/* environment variable.  In a minimal \ac{posix} system it might be        */
/* necessary to twiddle the environment strings explicitly.                 */
/*                                                                          */
/*                                                                          */
/* <miscellaneous routines>=                                                */
/* <ustenv code>=                                                           */
FORTRAN_SUBR ( USTENV, ustenv,
         (fpstr str, int *result, int str_len),
         (fpstr str, int *result),
         (fpstr str, int str_len, int *result))
{
  size_t Length;
  char *temp_name;

  Length = ccp4_flength (FTN_STR(str), FTN_LEN(str));
  temp_name = (char *) malloc(Length+1);
  strncpy (temp_name, FTN_STR(str), Length);
  temp_name[Length] = '\0'; 

  *result = ccp4_ustenv (temp_name);
  free(temp_name);
}
#endif

/* \subsection{{\tt outbuf()}}                                              */
/*                                                                          */
/* This sets stdout to line buffering                                       */
/*                                                                          */
/* <miscellaneous routines>=                                                */
/* <outbuf code>=                                                           */
FORTRAN_SUBR ( OUTBUF, outbuf, (), (), ())
{
  ccp4_outbuf();
}

/* \subsection{{\tt subroutine cunlink (\meta{filename})}}                  */
/* This unlinks \meta{filename} from the directory.  It's intended for      */
/* use with scratch files, so that they can be hidden when opened but       */
/* still be available as long as they remain connected (see [[CCPOPN]]).    */
/* This functionality doesn't seem to exist in \idx{VMS}\@.  Failure to     */
/* unlink isn't fatal (it's been observed, apparently spuriously).          */
/*                                                                          */
/* <miscellaneous routines>=                                                */
FORTRAN_SUBR ( CUNLINK, cunlink,
      (fpstr filename, int filename_len),
      (fpstr filename),
      (fpstr filename, int filename_len))
{
  size_t Length;
  char *tempfile;

#ifdef VMS
  return;                       /* can't do it */
#else
  Length = ccp4_flength (FTN_STR(filename), FTN_LEN(filename));
  tempfile = (char *) malloc(Length+1);
  strncpy (tempfile, FTN_STR(filename), Length);
  tempfile[Length] = '\0';

  if( unlink(tempfile) != 0)
    qprint("CUNLINK: Can't unlink");
  free(tempfile);
#endif /* VMS */
}
/* \section{Dynamic memory allocation}                                      */
/* It's nice to be able to determine array sizes at run time to avoid       */
/* messy recompilation.  The only way effectively to get dynamic            */
/* allocation in Fortran77 reasonably portably is to do the allocation,     */
/* e.g.\ in C, and invoke the Fortran routine passed as a parameter with    */
/* pointers to the allocated memory which it will treat as arrays.  If we   */
/* want to allow more than one array, it's more tricky.                     */
/*                                                                          */
/* \subsection{{\tt subroutine ccpal1 (\meta{routne}, \meta{n}.             */
/*     \meta{type}, \meta{length})}}                                        */
/* Arranges to call subroutine \meta{routne} with \meta{n} array            */
/* arguments.  Each has a type indicated by \meta{type}$(i)$ and a length   */
/* given by \meta{length}($i$).  \meta{type} is an integer array with       */
/* values 1, 2, 3, 4 inidcating {\tt                                        */
/*   INTEGER}, {\tt REAL}, {\tt DOUBLE PRECISION} and {\tt COMPLEX}         */
/* respectively.                                                            */
/* It's not immediately clear what all the Fortran/C                        */
/* conventions are for passing [[CHARACTER]] arrays, so we'll arrange a     */
/* higher-level interface and have [[types]] here just numeric.  The        */
/* Fortran ([[CCPALC]]) will also do argument validation.  Also the rules   */
/* for passing external routines as arguments aren't clear---assume         */
/* the obvious way.                                                         */
/*                                                                          */
/* There's a \idx{VMS} Fortran version of this, although the code here      */
/* does work fine in VMS\@.                                                 */
/*                                                                          */
/* NB: there's a possibility of a hook here to use memory-mapped files on   */
/* systems with the capability and insufficient VM\@.                       */
/*                                                                          */
/* Under protest, this now allocates zeroed storage for where programs      */
/* make bad assumptions.                                                    */
/*                                                                          */
/* <miscellaneous routines>=                                                */
#ifndef VMS                     /* we'll use the Fortran version in VMS*/
#ifndef _MVS
FORTRAN_SUBR ( CCPAL1, ccpal1,
     (void (* routne) (), int *n, int type[], int length[]),
     (void (* routne) (), int *n, int type[], int length[]),
     (void (* routne) (), int *n, int type[], int length[]))
{
  static int item_sizes[] = {
    (int) sizeof (char),           /* 0: bytes */
    (int) sizeof (short int),      /* 1: (integer) half words */
    (int) sizeof (float),          /* 2: reals/words */
    (int) sizeof (int),            /* 3: `short complex' (pairs of half words).
                                         NB int rather than 2*short since must fit
                                          into fortran integer */
    (int) 2*sizeof (float),        /* 4: complex (pairs of words) */
    (int) sizeof (int),            /* 5: not used */
    (int) sizeof (int)             /* 6: integers */
  };
  int i, size, *leng[13];
  void *pointer[13];

  for (i=0; i<*n; i++) {
    switch (type[i]) {
    case 1:
      size = item_sizes[6]; break; /* integer */
    case 2:
      size = item_sizes[2]; break; /* real */
    case 3:
      size = 2*item_sizes[2]; break; /* double */
    case 4:
      size = 2*item_sizes[2]; break; /* complex */
    case 5:
      size = item_sizes[1]; break; /* bytes (logical or integer *1) */
    }
    pointer[i+1] = calloc ((size_t) length[i], (size_t) size);
    if (pointer[i+1] == NULL) ccp4_fatal ("CCPALC: can't allocate memory");
    leng[i+1] = &(length[i]);   /* convenience */
  }
  switch (*n) {
  case 1:
    (* routne) (leng[1], pointer[1]);
    break;
  case 2:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2]);
    break;
  case 3:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3]);
    break;
  case 4:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3], leng[4], pointer[4]);
    break;
  case 5:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3], leng[4], pointer[4],
                leng[5], pointer[5]);
    break;
  case 6:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3], leng[4], pointer[4],
                leng[5], pointer[5], leng[6], pointer[6]);
    break;
  case 7:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3], leng[4], pointer[4],
                leng[5], pointer[5], leng[6], pointer[6],
                leng[7], pointer[7]);
    break;
  case 8:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3], leng[4], pointer[4],
                leng[5], pointer[5], leng[6], pointer[6],
                leng[7], pointer[7], leng[8], pointer[8]);
    break;
  case 9:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3], leng[4], pointer[4],
                leng[5], pointer[5], leng[6], pointer[6],
                leng[7], pointer[7], leng[8], pointer[8],
                leng[9], pointer[9]);
    break;
  case 10:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3], leng[4], pointer[4],
                leng[5], pointer[5], leng[6], pointer[6],
                leng[7], pointer[7], leng[8], pointer[8],
                leng[9], pointer[9], leng[10], pointer[10]);
    break;
  case 11:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3], leng[4], pointer[4],
                leng[5], pointer[5], leng[6], pointer[6],
                leng[7], pointer[7], leng[8], pointer[8],
                leng[9], pointer[9], leng[10], pointer[10],
                leng[11], pointer[11]);
    break;
  case 12:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3], leng[4], pointer[4],
                leng[5], pointer[5], leng[6], pointer[6],
                leng[7], pointer[7], leng[8], pointer[8],
                leng[9], pointer[9], leng[10], pointer[10],
                leng[11], pointer[11], leng[12], pointer[12]);
    break;
  }
  for (i=0; i<*n; i++)
    free (pointer[i+1]);
}
#endif /* VMS */
#endif

/* \section{Diskio routines}                                                */
/*                                                                          */
/* \subsection{{\tt subroutine copen(\meta{iunit}, \meta{filename},         */
/*     \meta{istat})}}                                                      */
/* Opens \meta{filename} on diskio stream \meta{iunit}.  \meta{istat}       */
/* corresponds to the open mode given to [[qopen]], from which [[copen]]    */
/* is always called---see diskio documentation.                             */
/*                                                                          */
/* <diskio routines>=                                                       */ 
FORTRAN_SUBR ( COPEN, copen,
    (int *iunit, fpstr filename, int *istat, int filename_len),
    (int *iunit, fpstr filename, int *istat),
    (int *iunit, fpstr filename, int filename_len, int *istat))
{
  size_t Length;
  char *tempfile;

  Length = ccp4_flength (FTN_STR(filename), FTN_LEN(filename));
  tempfile = (char *) malloc(Length+1);
  strncpy (tempfile, FTN_STR(filename), Length);
  tempfile[Length] = '\0';

  *iunit = ccp4_qopen (tempfile, *istat);
  free(tempfile);
}

/* \subsection{{\tt subroutine qrarch (\meta{iunit},                        */
/*     \meta{ipos}, \meta{ireslt})}}                                        */
/*                                                                          */
/* For binary files with a well-determined structure in terms of            */
/* [[float]]s and [[int]]s we may want to set up the connected stream to    */
/* do transparent reading of files written on a machine with a different    */
/* architecture.  This is currently the case for map\index{map files} and   */
/* \idx{MTZ files} and this routine is called from \idx{mtzlib} and         */
/* \idx{maplib}.                                                            */
/*                                                                          */
/*                                                                          */
/* [[qrarch]] reads the \idx{machine stamp} at {\em word\/} \meta{ipos}     */
/* for the diskio file on stream \meta{iunit} and sets up the appropriate   */
/* bit-twiddling for subsequent [[qread]]s on that stream.  The             */
/* information read from the file is returned in \meta{ireslt} in the       */
/* form $\mbox{[[fileFT]]}+16\mbox{[[fileIT]]}$.  If the stamp is zero      */
/* (as it would be for files written with a previous version of the         */
/* library) we assume the file is in native format and needs no             */
/* conversion in [[qread]]; in this case \meta{ireslt} will be zero and     */
/* the caller can issue a warning.  [[Iconvert]] and [[Fconvert]] are       */
/* used by [[qread]] to determine the type of conversion (if any) to be     */
/* applied to integers and reals.                                           */
/*                                                                          */
/* Fudge:\index{fudge} Ian Tickle reports old VAX files which have a machine */
/* stamp which is byte-flipped from the correct VAX value,although it should */
/* always have been zero as far as I can see.  To accommodate this, set the */
/* logical \idx{NATIVEMTZ} and the machine stamp won't be read for any      */
/* input files for which [[qrarch]] is called.                              */
/*                                                                          */
/* Extra feature: logical/environment variable [[CONVERT_FROM]] may be set  */ 
/* to one of [[BEIEEE]], [[LEIEEE]], [[VAX]] or [[CONVEXNATIVE]] to avoid   */
/* reading the machine stamp and assume the file is from the stipulated     */
/* archictecture for all input MTZ and map files for which [[qrarch]] is    */
/* called.                                                                  */
/*                                                                          */
/* N.B.: leaves the stream positioned just after the machine stamp.         */
/*                                                                          */
FORTRAN_SUBR ( QRARCH, qrarch,
    (int *iunit, int *ipos, int *ireslt),
    (int *iunit, int *ipos, int *ireslt),
    (int *iunit, int *ipos, int *ireslt))
{
  *ireslt = ccp4_qrarch (*iunit, *ipos);
}
/* \subsection{{\tt subroutine qwarch(\meta{iunit}, \meta{ipos})}}          */
/* This is the complement of [[qrarch]], writing the native machine         */
/* architecture information (`\idx{machine stamp}') to diskio stream        */
/* \meta{iunit} at {\em word\/} \meta{ipos}.  Currently called              */
/* from \idx{mtzlib} and \idx{maplib}.                                      */
/*                                                                          */
/* The machine stamp in [[mtstring]] is four nibbles in order, indicating   */
/* complex and real format (must both be the same), integer format and      */
/* character format (currently irrelevant).  The last two bytes of          */
/* [[mtstring]] are currently unused and always zero.                       */
/*                                                                          */
/* N.B.: leaves the stream positioned just after the machine stamp.         */
/*                                                                          */
/* <diskio routines>=                                                       */
FORTRAN_SUBR ( QWARCH, qwarch,
    (int *iunit, int *ipos),
    (int *iunit, int *ipos),
    (int *iunit, int *ipos))
{
  ccp4_qwarch (*iunit, *ipos);
}

/* \subsection{{\tt subroutine qclose (\meta{iunit})}}                      */
/* Closes the file open on \idx{diskio} stream \meta{iunit}.                */
/*                                                                          */
/* <diskio routines>=                                                       */
FORTRAN_SUBR ( QCLOSE, qclose,
    (int *iunit),
    (int *iunit),
    (int *iunit))
{
  ccp4_qclose (*iunit);
}

/* \subsection{{\tt subroutine qmode (\meta{iunit}, \meta{mode},            */
/*     \meta{size})}}                                                       */
/* Changes the \idx{diskio} \idx{access mode} for stream \meta{iunit} to    */
/* \meta{mode}.  The resulting size in bytes of items for transfer is       */
/* returned as \meta{size}.                                                 */
/*                                                                          */
/* <diskio routines>=                                                       */
FORTRAN_SUBR ( QMODE, qmode,
    (int *iunit, int *mode, int *size),
    (int *iunit, int *mode, int *size),
    (int *iunit, int *mode, int *size))
{
  *size = ccp4_qmode (*iunit, *mode);
}

/* \subsection{{\tt subroutine qread(\meta{iunit}, \meta{buffer},           */
/*     \meta{nitems}, \meta{result})}}                                      */
/*                                                                          */
/* Reads \meta{nitems} in the current mode (set by [[qmode]]) from diskio stream */
/* \meta{iunit} previously opened by [[qopen]](/[[copen]]) and returns      */
/* \meta{result} which is [[0]] on success or [[-1]] at EOF\@.              */
/* It aborts on an i/o error.                                               */
/* Numbers written in a foreign format will be translated if necessary if   */
/* the stream is connected to an MTZ or map file.                           */
/*                                                                          */
/* <diskio routines>=                                                       */
FORTRAN_SUBR ( QREAD, qread,
    (int *iunit, uint8 *buffer, int *nitems, int *result),
    (int *iunit, uint8 *buffer, int *nitems, int *result),
    (int *iunit, uint8 *buffer, int *nitems, int *result))
{
  *result = ccp4_qread (*iunit, buffer, *nitems);
  if (*result == *nitems)
    *result = 0;
}

/* \subsection{{\tt subroutine qreadc(\meta{iunit}, \meta{buffer},          */
/*     \meta{result})}}                                                     */
/*                                                                          */
/* Fills [[CHARACTER]] buffer in byte mode from diskio stream               */
/* \meta{iunit} previously opened by [[qopen]](/[[copen]]) and returns      */
/* \meta{result} which is the number of items read or [[0]] on failure.     */
/* Call it with a character substring if necessary to control the number    */
/* of bytes read.                                                           */
/*                                                                          */
/* <diskio routines>=                                                       */
FORTRAN_SUBR ( QREADC, qreadc,
    (int *iunit, fpstr buffer, int *result, int buffer_len),
    (int *iunit, fpstr buffer, int *result),
    (int *iunit, fpstr buffer, int buffer_len, int *result))
{
  int n;

  n = FTN_LEN(buffer);

  *result = ccp4_qreadc (*iunit, FTN_STR(buffer), (size_t) n);
  if (*result == n)
    *result = 0;
}

/* \subsection{{\tt subroutine qwrite (\meta{iunit}, \meta{buffer},         */
/*     \meta{nitems})}}                                                     */
/* This write \meta{nitems} items from \meta{buffer} to [[qopen]]ed         */
/* stream \meta{iunit} using the current mode.                              */
/*                                                                          */
/* <diskio routines>=                                                       */
FORTRAN_SUBR ( QWRITE, qwrite,
    (int *iunit, uint8 * buffer, int *nitems),
    (int *iunit, uint8 * buffer, int *nitems),
    (int *iunit, uint8 * buffer, int *nitems))
{
  ccp4_qwrite (*iunit, buffer, *nitems);
}

/* \subsection{{\tt subroutine qwritc (\meta{iunit}, \meta{buffer})}}       */
/*                                                                          */
/* Writes [[CHARACTER*(*)]] \meta{buffer} to [[qopen]]ed                    */
/* stream \meta{iunit} in byte mode.                                        */
/*                                                                          */
/* <diskio routines>=                                                       */
FORTRAN_SUBR ( QWRITC, qwritc,
    (int *iunit, fpstr buffer, int buffer_len),
    (int *iunit, fpstr buffer),
    (int *iunit, fpstr buffer, int buffer_len))
{
  int n;

  n = FTN_LEN(buffer);

  ccp4_qwritc (*iunit, FTN_STR(buffer), (size_t) n);
}

/* \subsection{{\tt subroutine qseek (\meta{iunit}, \meta{irec},            */
/*     \meta{iel}, \meta{lrecl})}}                                          */
/* Seeks to element \meta{iel} in record \meta{irec} in diskio stream       */
/* \meta{iunit} whose record length is \meta{lrecl}.                        */
/*                                                                          */
/* <diskio routines>=                                                       */
FORTRAN_SUBR ( QSEEK, qseek,
    (int *iunit, int *irec, int *iel, int *lrecl),
    (int *iunit, int *irec, int *iel, int *lrecl),
    (int *iunit, int *irec, int *iel, int *lrecl))
{
  /*switch from FORTRAN offset to C offset */
  ccp4_qseek (*iunit, *irec-1, *iel-1, *lrecl);
}

/* \subsection{{\tt subroutine qback (\meta{iunit}, \meta{lrecl})}}         */
/* Backspaces one record, of length \meta{lrecl} on diskio stream \meta{iunit}. */
/*                                                                          */
/* <diskio routines>=                                                       */
FORTRAN_SUBR ( QBACK, qback,
    (int *iunit, int *lrecl),
    (int *iunit, int *lrecl),
    (int *iunit, int *lrecl))
{
  ccp4_qback (*iunit, *lrecl);
}

/* \subsection{{\tt subroutine qskip (\meta{iunit}, \meta{lrecl})}}         */
/* Skip forward 1 record of length \meta{lrecl} on diskio stream \meta{iunit}.*/
/*                                                                          */
/* <diskio routines>=                                                       */
FORTRAN_SUBR ( QSKIP, qskip,
    (int *iunit, int *lrecl),
    (int *iunit, int *lrecl),
    (int *iunit, int *lrecl))
{
  ccp4_qskip (*iunit, *lrecl);
}

/* \subsection{{\tt subroutine cqinq (\meta{istrm}, \meta{filnam},          */
/*     \meta{length})}}                                                     */
/* Returns the name \meta{filnam} and \meta{length} of the file (if any)    */
/* open on diskio stream \meta{istrm}.                                      */
/*                                                                          */
/* <diskio routines>=                                                       */
FORTRAN_SUBR ( CQINQ, cqinq,
    (int *istrm, fpstr filename, int *length, int filename_len),
    (int *istrm, fpstr filename, int *length),
    (int *istrm, fpstr filename, int filename_len, int *length))
{
  char *real_name;
  size_t Length;

  Length = ccp4_flength (FTN_STR(filename), FTN_LEN(filename));
  real_name = (char *) malloc(Length+1);
  strncpy (real_name, FTN_STR(filename), Length);
  real_name[Length] = '\0';

  *length = (int) ccp4_qinq (*istrm, real_name);
  free(real_name);
}

/* \subsection{{\tt subroutine qlocate (\meta{iunit}, \meta{locate})}}      */
/* Returns the current position \meta{locate} in the diskio stream \meta{iunit}. */
/*                                                                          */
/* <diskio routines>=                                                       */
FORTRAN_SUBR ( QLOCATE, qlocate,
    (int *iunit, int *locate),
    (int *iunit, int *locate),
    (int *iunit, int *locate))
{
  *locate = (int) ccp4_qlocate (*iunit);
}
/* \section{`Magic' numbers}                                                */
/*                                                                          */
/* When, for instance, an $F$ is unobserved in a derivative, we might       */
/* want to give it a special value---a `\idx{magic number}'---possibly in   */
/* addition to a special value of the $\sigma$, like a negative one.        */
/* Using such a number in a calculation (by mistake, through ignoring the   */
/* value of $\sigma$, say) should not allow one to get half-sensible        */
/* results as one might if this number was $-9999$ or some such.  (There    */
/* is non-enforced connexion between the $F$ and its $\sigma$ in the MTZ    */
/* file, although one could think of adding extra columns to the file       */
/* with bit-encoded flags telling whether the $F$ in a given column was     */
/* observed.)                                                               */
/*                                                                          */
/* The obvious tactic with \ac{ieee} arithmetic is to use a \idx{NaN}       */
/* value in such situations.  Things may be set up so that we either get    */
/* an exception on using it in arithmetic or it silently propagates to all  */
/* values using it and its presence is indicated by a NaN in the output.    */
/* On a \idx{VAX} architecture we can't use NaN, but there is the           */
/* possibility of using a                                                   */
/* `reserved operand'\index{reserved operand|see{Rop}}                      */
/* (`\idx{Rop}') value,                                                     */
/* which will cause an exception (by experiment: when used for              */
/* floating-point arithmetic {\em or\/} printed, but not when assigned).    */
/* The \idx{Convex} native mode is similar, except that the Rop may be      */
/* printed (in the form {\tt Rop0x}\meta{fraction part}).                   */
/*                                                                          */
/* On, say, the \idx{IBM 370 architecture}---which we don't currently       */
/* support---anything's a valid floating point number, and the best ploy    */
/* is probably to use the largest representable number as the `magic'       */
/* value.  This would stand a good chance of raising an overflow            */
/* exception if used.  Anyhow, if such bad use of an undefined value is     */
/* made in a program due to insufficient checking by the code, it should    */
/* be spotted on the \ac{ieee} systems and the bug fixed---it's not         */
/* strictly necessary that it should cause a fatal error on all             */
/* architectures.                                                           */
/*                                                                          */
/* We need to provide a means of setting the magic number and checking      */
/* whether a given value is such.  These are architecture-dependent         */
/* bit-level operations, hence their presence in the C code.                */
/*                                                                          */
/* The suite doesn't currently use these routines, but should do soon.      */
/* \subsection{Setting a value: {\tt subroutine qnan(value)}}               */
/*                                                                          */
/* [[qnan]] was originally a \ft{} [[real function]] returning the value    */
/* (and actually done in 2 stages) with a subroutine implementation like    */
/* this called by the \ft{} function to avoid problems under \idx{VMS}      */
/* and native \idx{Convex}.  However, the \idx{f2c} calling convention      */
/* for a function loses in that case since it assumes a [[double]] value    */
/* returned which is cast to [[float]] with a SIGFPE, sigh.                 */
/*                                                                          */
/* <magic numbers>=                                                         */
FORTRAN_SUBR ( QNAN, qnan,
    (union float_uint_uchar *realnum),
    (union float_uint_uchar *realnum),
    (union float_uint_uchar *realnum))
{
  *realnum = ccp4_nan ();
}
/* \subsection{Testing a value: {\tt int cisnan(\meta{real})}}              */
/*                                                                          */
/* We want a \ft{} logical function [[qisnan]] to test whether its argument */
/* is a \idx{NaN} or \idx{Rop}.  We have to do this by writing a C          */
/* [[int]]-valued procedure and testing the returned value in the \ft{}     */
/* so that we don't have to assume how it represents logical values.  The   */
/* {\tt diskio}\index{diskio} library module provides the                   */
/* trivial interface [[QISNAN]].                                            */
/*                                                                          */
/* <magic numbers>=                                                         */
FORTRAN_FUN (int, CISNAN, cisnan,
	     (union float_uint_uchar *realnum),
	     (union float_uint_uchar *realnum),
	     (union float_uint_uchar *realnum))
{
  return (ccp4_isnan (realnum));
}

/* \subsection{Absent data test for {\tt mtzlib}: {\tt subroutine           */
/*     ccpbml (\meta{ncols}, \meta{cols})}}                                 */
/* In {\tt mtzlib} there's a fudge for \idx{BIOMOL}-convention absence      */
/* flags, which are re-written to zeroes.  To do the real number            */
/* comparison, though, it's necessary to do a [[qnan]]-type test first.     */
/* We don't want to call [[qnan]] (which calls [[cisnan]]) on every         */
/* number in the data file, so the tests are amortised in this routine      */
/* which deals with a whole array \meta{cols} of length \meta{ncols}.       */
/*                                                                          */
/* <magic numbers>=                                                         */
FORTRAN_SUBR ( CCPBML, ccpbml,
    (int *ncols, union float_uint_uchar cols[]),
    (int *ncols, union float_uint_uchar cols[]),
    (int *ncols, union float_uint_uchar cols[]))
{
  ccp4_bml (*ncols, cols) ;
}

/* \subsection{Updating MTZ column ranges: {\tt subroutine ccpwrg           */
/*     (\meta{ncols}, \meta{rcols}, \meta{wmin}, \meta{wmax})}}             */
/* This is a similar fudge to [[ccpbml]] to avoid [[QISNAN]] calls in       */
/* updating the MTZ column ranges in {\tt mtzlib}.  Note that [[wminmax]]   */
/* actually indexes a 3-D Fortran array with the first                      */
/* dimension range of 2, indicating minimum and maximum values respectively. */
/*                                                                          */
/* <magic numbers>=                                                         */
FORTRAN_SUBR ( CCPWRG, ccpwrg,
    (int *ncols, union float_uint_uchar cols[], float wminmax[]),
    (int *ncols, union float_uint_uchar cols[], float wminmax[]),
    (int *ncols, union float_uint_uchar cols[], float wminmax[]))
{
  ccp4_wrg (*ncols, cols, wminmax) ;
}

/* \subsection{Routines for Data Harvesting: {\tt subroutine hgetlimits}}    */
/* Returns largest int and largest float as defined in <limits.h> and       */
/* <float.h>                                                                 */
FORTRAN_SUBR ( HGETLIMITS, hgetlimits,
    (int *IValueNotDet, float *ValueNotDet),
    (int *IValueNotDet, float *ValueNotDet),
    (int *IValueNotDet, float *ValueNotDet))
{
  ccp4_hgetlimits (IValueNotDet, ValueNotDet);
}

/* Wrap-around for mkdir function. Returns 0 if successful, 1 if directory  */
/* already exists, and -1 if other error.                                   */
FORTRAN_SUBR ( CMKDIR, cmkdir,
    (const fpstr path, const fpstr cmode, int *result, int path_len, int cmode_len),
    (const fpstr path, const fpstr cmode, int *result),
    (const fpstr path, int path_len, const fpstr cmode, int cmode_len, int *result))
{ 
  char *temp_path, *temp_cmode;

  temp_path = ccp4_FtoCString(FTN_STR(path), FTN_LEN(path));
  temp_cmode = ccp4_FtoCString(FTN_STR(cmode), FTN_LEN(cmode));

  *result = ccp4_mkdir (temp_path, temp_cmode);
  free(temp_path);
  free(temp_cmode);
}

/* Wrap-around for mkdir function. Returns 0 if successful, 1 if directory     */
/* already exists, and -1 if other error.                                      */
FORTRAN_SUBR ( CCHMOD, cchmod,
    (const fpstr path, const fpstr cmode, int *result, int path_len, int cmode_len),
    (const fpstr path, const fpstr cmode, int *result),
    (const fpstr path, int path_len, const fpstr cmode, int cmode_len, int *result))
{ 
  char *temp_path, *temp_cmode;

  temp_path = ccp4_FtoCString(FTN_STR(path), FTN_LEN(path));
  temp_cmode = ccp4_FtoCString(FTN_STR(cmode), FTN_LEN(cmode));

  *result = ccp4_chmod (temp_path, temp_cmode);
  free(temp_path);
  free(temp_cmode);
}

#ifdef _AIX
/* \section{Missing system support}                                         */
/*                                                                          */
/* Routines often found in {\tt \idx{libU77}.a} or somesuch are missing     */
/* on some systems.\index{HPUX}\index{AIX}                                  */
/*                                                                          */
/* <AIX support>=                                                           */
void idate (int iarray[3])
{
     struct tm *lt;
     time_t tim;
     tim = time(NULL);
     lt = localtime(&tim);
     iarray[0] = lt->tm_mday;
     iarray[1] = lt->tm_mon+1;  /* need range 1-12 */
     iarray[2] = lt->tm_year + 1900;
}
#endif
#if defined (__hpux) || defined (_AIX)
/* <AIX and HPUX support>=                                                  */
void gerror (str, Lstr)
char *str;
int  Lstr;
{
  int i;

  if (errno == 0) {             /* Avoid `Error 0' or some such message */
    for (i=1; Lstr; i++)
      str[i] = ' ';
  } else {
    (void) strncpy (str, strerror (errno), Lstr);
    for (i = strlen (str); i < Lstr; i++) str[i] = ' ';  /* pad with spaces */
  }
} /* End of gerror (str, Lstr) */

int ierrno () {
  return errno;
}

void itime (array)
     int array[3];
{
     struct tm *lt;
     time_t tim;
     tim = time(NULL);
     lt = localtime(&tim);
     array[0] = lt->tm_hour; array[1] = lt->tm_min; array[2] = lt->tm_sec;
}

static long clk_tck = 0;

#if 0                           /* dtime isn't used at present */
float dtime (tarray)
     float tarray[2];
{
  struct tms buffer;
  time_t utime, stime;
  static time_t old_utime = 0, old_stime = 0;
  if (! clk_tck) clk_tck = sysconf(_SC_CLK_TCK);
  (void) times(&buffer);
  utime = buffer.tms_utime; stime = buffer.tms_stime;
  tarray[0] = ((float)(utime - old_utime)) / (float)clk_tck;
  tarray[1] = ((float)(stime - old_stime)) / (float)clk_tck;
  old_utime = utime; old_stime = stime;
  return (tarray[0]+tarray[1]);
}
#endif                          /* dtime */

float etime (tarray)
     float tarray[2];
{
  struct tms buffer;
  time_t utime, stime;
  if (! clk_tck) clk_tck = sysconf(_SC_CLK_TCK);
  (void) times(&buffer);
  tarray[0] = (float) buffer.tms_utime / (float)clk_tck;
  tarray[1] = (float) buffer.tms_stime / (float)clk_tck;
  return (tarray[0]+tarray[1]);
}

#endif             /*  HPUX and AIX support */

/** Lahey Express LF95 6.0 **/
#ifdef LF95
int gerror_ (str, Lstr)
char *str;
int  Lstr;
{
  int i;

  if (errno == 0) {             /* Avoid `Error 0' or some such message */
    for (i=1; Lstr; i++)
      str[i] = ' ';
  } else {
    (void) strncpy (str, strerror (errno), Lstr);
    for (i = strlen (str); i < Lstr; i++) str[i] = ' ';  /* pad with spaces */
  }
  return 0;
}
#endif

#if defined(ABSOFT_F77) || defined(LAHEY)
/*  int exit_ (status) */
/*       int *status; */
/*  { */
/*    f_exit (); */
/*    exit (*status); */
/*  } */

/*  int time_ () */
/*  { */
/*    return (int) time (NULL); */
/*  } */

/*  int time_ () */
/*  { */
/*    return (int) time (NULL); */
/*  } */

int getpid_ ()
{
  return (int) getpid ();
}

int idate_ (iarray)
int iarray[3];
{
  struct tm *lt;
  time_t tim;
  tim = time(NULL);
  lt = localtime(&tim);
  iarray[0] = lt->tm_mday;
  iarray[1] = lt->tm_mon+1;
  iarray[2] = lt->tm_year + 1900;
  return 0;
}

int gerror_ (str, Lstr)
char *str;
int  Lstr;
{
  int i;

  if (errno == 0) {             /* Avoid `Error 0' or some such message */
    for (i=1; Lstr; i++)
      str[i] = ' ';
  } else {
    (void) strncpy (str, strerror (errno), Lstr);
    for (i = strlen (str); i < Lstr; i++) str[i] = ' ';  /* pad with spaces */
  }
  return 0;
}

int ierrno_ () {
  return errno;
}

/*  void itime (array) */
/*       int array[3]; */
/*  { */
/*       struct tm *lt; */
/*       time_t tim; */
/*       tim = time(NULL); */
/*       lt = localtime(&tim); */
/*       array[0] = lt->tm_hour; array[1] = lt->tm_min; array[2] = lt->tm_sec; */
/*  } */

int itime_ (array)
int array[3];
{
  struct tm *lt;
  time_t tim;
  tim = time(NULL);
  lt = localtime(&tim);
  array[0] = lt->tm_hour; array[1] = lt->tm_min; array[2] = lt->tm_sec;
}

static long clk_tck = 0;

float etime_ (tarray)      /* NB `doublereal' return for f2c. */
     float tarray[2];
{
  struct tms buffer;
  time_t utime, stime;
  if (! clk_tck) clk_tck = sysconf(_SC_CLK_TCK);
  (void) times(&buffer);
  tarray[0] = (float) buffer.tms_utime / (float)clk_tck;
  tarray[1] = (float) buffer.tms_stime / (float)clk_tck;
  return (tarray[0]+tarray[1]);
}

#endif              /* ABSOFT_F77 support  */


#if defined(F2C) || defined(G77)
/* <f2c support>=                                                           */
int exit_ (status)
     int *status;
{
  f_exit ();                    /* may or may not be registered with
                                   exit, depending on the C libraries
                                   capabilities, but is idempotent */
  exit (*status);
}

int time_ ()
{
  return (int) time (NULL);
}

int getpid_ ()
{
  return (int) getpid ();
}

/* following are from libI77/fio.h */
#define MXUNIT 100
typedef struct
{       FILE *ufd;      /*0=unconnected*/
        char *ufnm;
        long uinode;
        int udev;
        int url;        /*0=sequential*/
        flag useek;     /*true=can backspace, use dir, ...*/
        flag ufmt;
        flag uprnt;
        flag ublnk;
        flag uend;
        flag uwrt;      /*last io was write*/
        flag uscrtch;
} unit;
extern unit f__units[];
#define TRUE_ (1)
#define FALSE_ (0)
#define err(f,m,s) {if(f) errno= m; else f__fatal(m,s); return(m);}
/* end of fio.h extract */

int isatty_ (lunit)
     int *lunit;
{
  if (*lunit>=MXUNIT || *lunit<0)
    err(1,101,"isatty");
  /* f__units is a table of descriptions for the unit numbers (defined
     in io.h) with file descriptors rather than streams */
  return (isatty(fileno((f__units[*lunit]).ufd)) ? TRUE_ : FALSE_);
}

int idate_ (iarray)
     int iarray[3];
{
     struct tm *lt;
     time_t tim;
     tim = time(NULL);
     lt = localtime(&tim);
     iarray[0] = lt->tm_mday;
     iarray[1] = lt->tm_mon+1;  /* need range 1-12 */
     iarray[2] = lt->tm_year + 1900;
     return 0;
}

int gerror_ (str, Lstr)
char *str;
int  Lstr;
{
  int i;

  if (errno == 0) {             /* Avoid `Error 0' or some such message */
    for (i=1; Lstr; i++)
      str[i] = ' ';
  } else {
    (void) strncpy (str, strerror (errno), Lstr);
    for (i = strlen (str); i < Lstr; i++) str[i] = ' ';  /* pad with spaces */
  }
  return 0;
}

int ierrno_ () {
  return errno;
}

int itime_ (array)
     int array[3];
{
     struct tm *lt;
     time_t tim;
     tim = time(NULL);
     lt = localtime(&tim);
     array[0] = lt->tm_hour; array[1] = lt->tm_min; array[2] = lt->tm_sec;
}

static long clk_tck = 0;

doublereal etime_ (tarray)      /* NB `doublereal' return for f2c. */
     float tarray[2];
{
  struct tms buffer;
  time_t utime, stime;
  if (! clk_tck) clk_tck = sysconf(_SC_CLK_TCK);
  (void) times(&buffer);
  tarray[0] = (float) buffer.tms_utime / (float)clk_tck;
  tarray[1] = (float) buffer.tms_stime / (float)clk_tck;
  return (tarray[0]+tarray[1]);
}
/* These ought to be intrinsic, but they should only be applied to          */
/* [[INTEGER]] arguments.  The types [[integer]] and [[logical]] are both   */
/* assumed to be [[int]].                                                   */
/*                                                                          */
/* <f2c support>=                                                           */
int /* integer */ ibset_ (a, b)
     int /* integer */ *a, *b;
{
  return (*a) | 1<<(*b);
}

int /* integer */ ibclr_ (a, b)
     int /* integer */ *a, *b;
{
  return (*a) & ~(1<<(*b));
}

int /* logical */ btest_ (a, b)
     int /* integer */ *a, *b;
{
  return ((((unsigned long) *a)>>(*b)))&1 ? TRUE_ : FALSE_;
}
#endif              /* F2C support  */

/* isatty doesnt seem to be in Mircrosoft Visual Studdio so this is a fudge */
#if CALL_LIKE_MVS
int __stdcall ISATTY (int *lunit)
{
  lunit = 0 ;
  return lunit;
}

/* erfc doesnt seem to be in Mircrosoft Visual Studdio so this is a fudge */
float __stdcall ERFC(float *value)
{
  return (float) ccp4_erfc( (double) *value);
}
#endif

/* erfc isn't available for Intel compiler ? */
#ifdef IFC
float erfc_(float *value)
{
  return (float) ccp4_erfc( (double) *value);
}
#endif
 
