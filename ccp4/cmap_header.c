#include <string.h>
#include <math.h>
#include <stdarg.h>
#include "cmaplib.h"
#include "cmap_errno.h"

/*! Internal: read header from file and fill CMMFile struct.
  Called after file is opened for read.
  \param mfile (CMMFile *)
  \return 1 on success, EOF on failure*/
int parse_mapheader(CMMFile *mfile)
{
  const int read_total = 74;
  const size_t header_size = 1024U, n_byt_symop = 80U;
  unsigned char buffer[224];
  int result;
  float fmean,frms;
  
   ccp4_file_rewind(mfile->stream);

   memset(buffer,'\0',224);
   result = ccp4_file_readint(mfile->stream, &buffer[0], 10) ;
   result += ccp4_file_readfloat(mfile->stream, &buffer[40], 6);
   result += ccp4_file_readint(mfile->stream, &buffer[64], 3);
   result += ccp4_file_readfloat(mfile->stream, &buffer[76], 3);
   result += ccp4_file_readint(mfile->stream, &buffer[88], 3);
   result += ccp4_file_readfloat(mfile->stream, &buffer[100], 12);
   result += ccp4_file_readint(mfile->stream, &buffer[148], 8);
   result += ccp4_file_readchar(mfile->stream, &buffer[180], 24);
   result += ccp4_file_readint(mfile->stream, &buffer[204], 3);
   result += ccp4_file_readfloat(mfile->stream, &buffer[216], 1);
   result += ccp4_file_readint(mfile->stream, &buffer[220], 1);
   
   if (result != read_total) {
    ccp4_signal( CCP4_ERRLEVEL(3) | CMAP_ERRNO(CMERR_ReadFail),
                 "parse_header",
                 NULL);
    return EOF; }
  
  memcpy(&mfile->map_dim[0],&buffer[0],sizeof(mfile->map_dim));
  memcpy(&mfile->data_mode,&buffer[12],sizeof(int));
  memcpy(&mfile->origin[0],&buffer[16],sizeof(mfile->origin));
  memcpy(&mfile->cell_grid[0],&buffer[28],sizeof(mfile->cell_grid));
  memcpy(&mfile->cell[0],&buffer[40],sizeof(mfile->cell));
  memcpy(&mfile->axes_order[0],&buffer[64],sizeof(mfile->axes_order));
  memcpy(&mfile->stats.min,&buffer[76],sizeof(float));
  memcpy(&mfile->stats.max,&buffer[80],sizeof(float));
  memcpy(&fmean,&buffer[84],sizeof(float));
  mfile->stats.mean = (double) fmean;
  memcpy(&mfile->spacegroup,&buffer[88],sizeof(int));
  memcpy(&mfile->symop.size,&buffer[92],sizeof(int));
  memcpy(&mfile->user_access,&buffer[180],sizeof(mfile->user_access));
  memcpy(&mfile->data.header_size,&buffer[204],sizeof(int));
  memcpy(&frms,&buffer[216],sizeof(float));
  mfile->stats.rms = (double) frms;
  memcpy(&mfile->labels.number,&buffer[220],sizeof(int));
  
  memcpy(&result,&buffer[96],sizeof(int));
  if (result !=0) {
    memcpy(&mfile->skew.rotation[0][0],&buffer[100],sizeof(mfile->skew.rotation));
    memcpy(&mfile->skew.translation[0],&buffer[148],sizeof(mfile->skew.translation));
  }
  
  ccp4_file_setmode(mfile->stream, mfile->data_mode);
  /* may go to seperate function */
  mfile->symop.offset = header_size;
  mfile->data.offset = mfile->symop.offset + mfile->symop.size;
  mfile->data.section_size = mfile->map_dim[0]*mfile->map_dim[1]
                             *ccp4_file_itemsize(mfile->stream);
  mfile->data.block_size = mfile->data.section_size + mfile->data.header_size;
  mfile->data.number = mfile->map_dim[2];
  mfile->symop.number = mfile->symop.size / n_byt_symop;
  
  return 1;
}

/*! Internal: write summary of current CMMFile struct to file.
  Called when file is opened write, and closed write.
  \param mfile (CMMFile *)
  \return 1 on success, EOF on failure */
int write_mapheader(CMMFile *mfile)
{
  const int write_total = 74;
  unsigned char buffer[224];
  int result;
  float fmean,frms;
    
  memset(buffer,'\0',224);
  memcpy(&buffer[0],&mfile->map_dim[0],sizeof(mfile->map_dim));
  memcpy(&buffer[12],&mfile->data_mode,sizeof(int));
  memcpy(&buffer[16],&mfile->origin[0],sizeof(mfile->origin));
  memcpy(&buffer[28],&mfile->cell_grid[0],sizeof(mfile->cell_grid));
  memcpy(&buffer[40],&mfile->cell[0],sizeof(mfile->cell));
  memcpy(&buffer[64],&mfile->axes_order[0],sizeof(mfile->axes_order));
  memcpy(&buffer[76],&mfile->stats.min,sizeof(float));
  memcpy(&buffer[80],&mfile->stats.max,sizeof(float));
  fmean = (float) mfile->stats.mean;
  memcpy(&buffer[84],&fmean,sizeof(float));
  memcpy(&buffer[88],&mfile->spacegroup,sizeof(int));
  memcpy(&buffer[92],&mfile->symop.size,sizeof(int));
  memcpy(&buffer[180],&mfile->user_access,sizeof(mfile->user_access));
  memcpy(&buffer[204],&mfile->data.header_size,sizeof(int));
  memcpy(&buffer[208],"MAP ",4U);
  frms = (float) mfile->stats.rms;
  memcpy(&buffer[216],&frms,sizeof(float));
  memcpy(&buffer[220],&mfile->labels.number,sizeof(int));
  
  if (skew_set(&mfile->skew) == TRUE) {
    result = 1;
    memcpy(&buffer[96],&result, sizeof(int));
    memcpy(&buffer[100],&mfile->skew.rotation[0][0],sizeof(mfile->skew.rotation));
    memcpy(&buffer[148],&mfile->skew.translation[0],sizeof(mfile->skew.translation));
  }

   ccp4_file_seek(mfile->stream, 0L, SEEK_SET);
    
   result = ccp4_file_writeint(mfile->stream, &buffer[0], 10);
   result += ccp4_file_writefloat(mfile->stream, &buffer[40], 6);
   result += ccp4_file_writeint(mfile->stream, &buffer[64], 3);
   result += ccp4_file_writefloat(mfile->stream, &buffer[76], 3);
   result += ccp4_file_writeint(mfile->stream, &buffer[88], 3);
   result += ccp4_file_writefloat(mfile->stream, &buffer[100], 12);
   result += ccp4_file_writeint(mfile->stream, &buffer[148], 8);
   result += ccp4_file_writechar(mfile->stream, &buffer[180], 24);
   result += ccp4_file_writeint(mfile->stream, &buffer[204], 3);
   result += ccp4_file_writefloat(mfile->stream, &buffer[216], 1);
   result += ccp4_file_writeint(mfile->stream, &buffer[220], 1);
   
   if (result != write_total)
    ccp4_signal( CCP4_ERRLEVEL(3) | CMAP_ERRNO(CMERR_WriteFail),
                 "write_header",
                 NULL);

  return ( (result == write_total) ? 1 : EOF);
}

