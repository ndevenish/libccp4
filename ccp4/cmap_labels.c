/*
     cmap_labels.c: read and write map header labels
     Copyright (C) 2001  CCLRC, Charles Ballard

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
#include <string.h>
#include "cmaplib.h"
#include "cmap_labels.h"
#include "cmap_errno.h"

/*! Internal: read the labels from file header and copy into char * array
  Called when the file is opened in read mode.
  \param mfile (CMMFile *)
  \return 1 on succes */
int parse_maplabels(CMMFile *mfile)
{
  char buffer[81], *cptr;
  const unsigned int n_byt_label = 80U, max_label = 10U;
/*  const unsigned int labels_offset = 224U; */
  int i;
  
/*  ccp4_file_seek(mfile->stream,labels_offset,SEEK_SET); */
  for (i=0 ; i!=mfile->labels.number ; i++) {
    ccp4_file_readchar(mfile->stream,(uint8 *) buffer,n_byt_label);
    cptr = buffer+n_byt_label;
    while (*--cptr == ' ');
    *(++cptr) = '\0';
    mfile->labels.labels[i] = strdup(buffer);
  }
  ccp4_file_raw_seek(mfile->stream,(max_label-mfile->labels.number)
		     *n_byt_label,
		     SEEK_CUR);
  return 1;
}

/*! Internal: dump the labels char * array to file, offset at 224 bytes.
  Called when the file is opened or closed in write mode, immediately after the
  header is written.
 \param mfile (const CMMFile *)
 \return 1 on success, 0 on failure */
int write_maplabels(const CMMFile *mfile)
{
  char buffer[80];
/*  const unsigned int labels_offset = 224U; */
  int i, result = 0;

/*  ccp4_file_seek(mfile->stream,labels_offset,SEEK_SET);  */
  for (i=0 ; i != mfile->labels.number ; i++) {
    memset(buffer,' ',80U);
    strncpy(buffer,mfile->labels.labels[i],strlen(mfile->labels.labels[i]));
    result += ccp4_file_writechar(mfile->stream,(uint8 *) buffer,80U);
  }
  memset(buffer,' ',80U);
  while(i != 10) {
    result += ccp4_file_writechar(mfile->stream,(uint8 *) buffer,80U);
    i++;
  }
  return (result == 800) ? 1 : 0 ;
}

/*! Set the label in the map header.  Headers are 80 characters long.
  The labels are written to the file when it is closed. Therefore,
  the file must be in write mode.
  If label == NULL the element corresponding to posn is removed.
  The number of labels is recalculated on each call.
 \param mfile (CMMFile *)
 \param label (const char *) the C-style character array
 \param posn (int) the label number (C-style, 0 -> 9) 
 \return number of label effected, or EOF */
int ccp4_cmap_set_label(CMMFile *mfile, const char *label, int posn)
{  
  int i,j;
  
  if (mfile == NULL) {
    ccp4_signal( CCP4_ERRLEVEL(3) | CMAP_ERRNO(CMERR_NoChannel),
                "ccp4_cmap_set_label",NULL);
    return (EOF);}

  if (ccp4_file_is_write(mfile->stream) == 0) {
    ccp4_signal( CCP4_ERRLEVEL(3) | CMAP_ERRNO(CMERR_WriteFail),
                "ccp4_cmap_label_set",NULL);
    return (EOF);}    
  
/*posn must be between 0 and 9 */
  if (posn < 0) {
    posn = 0;
  } else if (posn > mfile->labels.number) {
    posn = mfile->labels.number;
  }

  if (mfile->labels.labels[posn] != NULL) 
    free(mfile->labels.labels[posn]);

/* if label == NULL reset the value and compress set */    
  if (label == NULL) {
    mfile->labels.labels[posn] = NULL;
    for ( i=posn ; i!=10 ; i++)
      if (mfile->labels.labels[i] == NULL)
        for ( j=i+1 ; j!=10; j++)
          if (mfile->labels.labels[j] != NULL) {
            mfile->labels.labels[i] = mfile->labels.labels[j];
            mfile->labels.labels[j] = NULL;
            break;
          }
    }
  else
    mfile->labels.labels[posn] = strdup(label);

/* recalculate number */
  for ( i=0 ; i!=10 ; i++)
    if (mfile->labels.labels[i] == NULL)
      break;
  mfile->labels.number = i;
  
  return posn;
}

/*! Get the label corresponding to position posn
  \param mfile (const CMMFile *)
  \param posn (int) desired label number
  \return pointer to label posn */
char *ccp4_cmap_get_label(const CMMFile *mfile, int posn)
{
  char *label;
  if (mfile == NULL) {
    ccp4_signal( CCP4_ERRLEVEL(3) | CMAP_ERRNO(CMERR_NoChannel),
                "ccp4_cmap_get_label",NULL);
    return (NULL);}

  if (posn < 0 || posn >= mfile->labels.number) 
    label = NULL;
  else 
    label = mfile->labels.labels[posn];

  return label;
}

/*! Return the number of labels.
 \param mfile (CMMFile *)
 \return the number of labels */
int ccp4_cmap_number_label(const CMMFile *mfile)
{
  return mfile->labels.number;
}

/*! Get the label corresponding to the title
    wrapping ccp4_cmap_get_label.
 \param mfile (const CMMFile *)
 \return pointer to label 0, or NULL */
char *ccp4_cmap_get_title(const CMMFile *mfile)
{
  return ccp4_cmap_get_label(mfile, 0);
}

/*! Set the label corresponding to the title,
    wrapping ccp4_cmap_set_label
 \param mfile (CMMFile *)
 \param label
 \return 0 or EOF on failure */
int ccp4_cmap_set_title(CMMFile *mfile, const char *title)
{
  return ccp4_cmap_set_label(mfile, title, 0);
}
