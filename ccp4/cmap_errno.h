#ifndef __GUARD_MAPLIB_ERR
#define __GUARD_MAPLIB_ERR

#include "ccp4_errno.h"

#ifdef __cplusplus
extern "C" {
#endif

#define CMAP_ERRNO(n) (CCP4_ERR_MAP | (n))

/* error defs */
#define  CMERR_Ok                  0
#define  CMERR_NoChannel           1
#define  CMERR_NoFile              2
#define  CMERR_NoLogicalName       3
#define  CMERR_CantOpenFile        4
#define  CMERR_NoHeader            5
#define  CMERR_ReadFail            6
#define  CMERR_WriteFail           7
#define  CMERR_ParamError          8
#define  CMERR_UnrecognK           9
#define  CMERR_FileStamp           10
#define  CMERR_SymErr              11
#define  CMERR_AllocFail           12
#define  CMERR_MaxFile             13
#define  CMERR_SeekFail            14

#ifdef __cplusplus
}
#endif

#endif     /* __GUARD_MAPLIB_ERR */

