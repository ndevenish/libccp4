/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/

/** @file cmap_open.c
 *
 *  @brief Opening CCP4-format map files.
 *
 *  @author Charles Ballard
 */

#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <fcntl.h>
#include "cmaplib.h"
#include "cmap_header.h"
#include "cmap_labels.h"
#include "cmap_errno.h"

/*! Internal: malloc CMMFile struct for reading into
 \return CMMFile */ 
CMMFile *init_cmap_read(void)
{
  CMMFile *mfile = (CMMFile *) malloc(sizeof(CMMFile));
  if (mfile)
    memset(mfile,'\0',sizeof(CMMFile));
  return mfile;
}

/*! Internal: malloc CMMFile struct for writing
 \return CMMFile */ 
CMMFile *init_cmap_write(void)
{
  CMMFile *mfile = (CMMFile *) malloc(sizeof(CMMFile));
  if (mfile) {
    memset(mfile,'\0',sizeof(CMMFile));
    mfile->data_mode = DEFMODE;
    mfile->symop.offset = 1024U;
    mfile->data.offset = 1024U; }
  return mfile;
}

/*! Internal: Identify file as a ccp4 format map
 \param file The (CCP4File *) struct representing the file. 
 \return non-zero on true, 0 on false */
int is_cmap(CCP4File *file)
{
  char buffer[4];
  const unsigned int map_offset = 208U;
  if (file == NULL)
    return 0;
  if ( ccp4_file_raw_seek(file,map_offset,SEEK_SET) == EOF)
    return 0;
  if (ccp4_file_readchar(file,buffer,4U) != 4U)
    return 0;
  ccp4_file_rewind(file);
  return !strncmp(buffer,"MAP ",4);
}

/*! The file is opened.
 \param filename (char *) the filename
 \param mode (int) the i/o mode , possible values are O_RDONLY, O_WRONLY, 
       O_RDWR, O_APPEND, O_TMP, O_CREAT, O_TRUNC - see ccp4_sysdep.h
 \return (void *) CMMFile structure */
void *ccp4_cmap_open(const char *filename, int mode)
{
  CMMFile *mfile;
  CCP4File *cfile;
  const size_t stamp_offset = 212U;
  char type[4];
  
  if ((cfile = ccp4_file_open(filename, mode)) == NULL) {
    ccp4_signal( CCP4_ERRLEVEL(3) | CMAP_ERRNO(CMERR_CantOpenFile),
		 "ccp4_cmap_open",NULL);
    return (NULL); } 
  ccp4_file_raw_setstamp(cfile, stamp_offset);
  /* read or write only */
  if (cfile->read) {
    if (!is_cmap(cfile) || cfile->length < 1025) {
      ccp4_signal( CCP4_ERRLEVEL(3) | CMAP_ERRNO(CMERR_NoHeader),
		 "ccp4_cmap_open",NULL);
      ccp4_file_close(cfile);
      return NULL; }
    ccp4_file_rarch(cfile);
    mfile = init_cmap_read();
    mfile->stream = cfile;
    mfile->file_name = cfile->name;
    parse_mapheader(mfile);
    parse_maplabels(mfile);
  } else if (cfile->write) {
    mfile = init_cmap_write();
    mfile->stream = cfile;
    mfile->file_name = cfile->name;
    write_mapheader(mfile);
    write_maplabels(mfile);
  } else {
    ccp4_signal( CCP4_ERRLEVEL(3) | CMAP_ERRNO(CMERR_CantOpenFile),
		 "ccp4_cmap_open",NULL);
    return (NULL); }
  return (mfile);
}

