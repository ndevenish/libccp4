/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/

/*  base error codes on system errors. */
#ifndef __CCP4_ERROR_GUARD
#define __CCP4_ERROR_GUARD

#include <errno.h>
/* initially 
   || 12 bits (system) | 4 bits (level) | 16 bits (code) ||
*/
static char rcsidhe[] = "$Id$";

#ifndef CCP4_ERRSYSTEM
#define CCP4_ERRSYSTEM(x) (((x)&0xfff)<<24)
#endif
#ifndef CCP4_ERRLEVEL
#define CCP4_ERRLEVEL(x)  (((x)&0xf)<<16)
#endif
#ifndef CCP4_ERRSETLEVEL
#define CCP4_ERRSETLEVEL(y,x) ((y) & (~CCP4_ERRLEVEL(0xf)) | CCP4_ERRLEVEL(x)))
#endif
#ifndef CCP4_ERRGETSYS   
#define CCP4_ERRGETSYS(x)   (((x)>>24)&0xfff)
#endif
#ifndef CCP4_ERRGETLEVEL
#define CCP4_ERRGETLEVEL(x) (((x)>>16)&0xf)
#endif
#ifndef CCP4_ERRGETCODE
#define CCP4_ERRGETCODE(x)  ((x)&0xffff)
#endif

#define CCP4_ERR_SYS CCP4_ERRSYSTEM(0x0)
#define CCP4_ERR_FILE CCP4_ERRSYSTEM(0x1)
#define CCP4_ERR_COORD CCP4_ERRSYSTEM(0x2)
#define CCP4_ERR_MTZ CCP4_ERRSYSTEM(0x3)
#define CCP4_ERR_MAP CCP4_ERRSYSTEM(0x4)
#define CCP4_ERR_UTILS CCP4_ERRSYSTEM(0x5)
#define CCP4_ERR_PARS CCP4_ERRSYSTEM(0x6)
#define CCP4_ERR_SYM CCP4_ERRSYSTEM(0x7)

#define CCP4_COUNT(x) sizeof(x)/sizeof(x[0])

extern int ccp4_errno;

#ifdef __cplusplus
namespace CCP4 {
extern "C" {
#endif

void ccp4_error( const char *);

const char *ccp4_strerror( int);

void ccp4_fatal(const char *);

int cfile_perror(const char *);

void ccp4_signal(int, const char * const, void (*)());

#ifdef __cplusplus
}
}
#endif

#endif  /*!CCP4_ERROR_GUARD */
