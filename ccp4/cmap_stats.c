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

