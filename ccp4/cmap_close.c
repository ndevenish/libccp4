/*
     cmap_close.c: close map file
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
#include <math.h>
#include <stdarg.h>
#include "cmaplib.h"
#include "cmap_header.h"
#include "cmap_labels.h"
#include "cmap_errno.h"

/*! Close the file.
 In write mode the header is output, along with the machine
 stamp.  In read mode the file is just closed. 
 Write mode supports ways of updating the map statistics ( 
 only active for FLOAT32).
 /param mfile (CMMFile *)
 /return void */
void ccp4_cmap_close(CMMFile *mfile)
{
  int i;
  
  if ( mfile == NULL) 
    return;

  if (ccp4_file_is_write(mfile->stream) ) {
    if ( mfile->data_mode == FLOAT32) {
      switch (mfile->close_mode) {
      case 1:
        break;
      case 2:
	mfile->stats.offset = 0.0f;
      case 0:
      default:
        mfile->stats.mean /= mfile->stats.total;
        mfile->stats.rms /= mfile->stats.total;
        mfile->stats.rms -= mfile->stats.mean*mfile->stats.mean;
        mfile->stats.rms = (mfile->stats.rms > 0) ? sqrt(mfile->stats.rms) : 0;
        mfile->stats.mean += (double) mfile->stats.offset;
        break;
      }
    }
    write_mapheader(mfile);
    write_maplabels(mfile);
    ccp4_file_warch(mfile->stream);
  }
  ccp4_file_close(mfile->stream);
  for (i=0 ; i != mfile->labels.number ; i++)
    if (mfile->labels.labels[i] != NULL)
      free(mfile->labels.labels[i]);
  free(mfile);
}

/*! Set the close mode:
    0: calculate based on stored values (default)
    1: just dump the current values
 /param mfile (CMMFile *)
 /param mask (unsigned int) close mode
 /return void */
void ccp4_cmap_closemode(CMMFile *mfile, unsigned int mask)
{
  mfile->close_mode = mask;
}
