/*
     cmap_stats.c: deal with map statistics.
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
#include <math.h>
#include "cmaplib.h"
#include "cmap_stats.h"
#include "cmap_errno.h"

/*! Internal: use floats in range section_begin to section_end
  to update the map statistics.
  \param stats (CMMFile_Stats *)
  \param section_begin (void *) start of section
  \param section_end (void *) one past end-of-section
  \return total of map elements so far */
int stats_update(CMMFile_Stats *stats, void *section_begin,
                         void *section_end)
{        
  float *ufp = (float *) section_begin;
  double val;
      
  if (stats->total == 0 && *ufp < -1.0e10 ) {         
    stats->offset = *ufp;
  } 
  while (ufp < (float *) section_end) {
    val = (double) (*ufp - stats->offset);
    stats->mean += val;
    stats->rms += val * val;
    stats->min = MIN( stats->min, *ufp);
    stats->max = MAX( stats->max, *ufp);
    
    ufp++;
  }
  
  stats->total += (float *)section_end - (float *)section_begin;
                
  return (stats->total);
}

