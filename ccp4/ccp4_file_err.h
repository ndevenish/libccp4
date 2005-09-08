/*
     ccp4_file_err.h: header file with file handling error codes
     Copyright (C) 2001  CCLRC, Charles Ballard

     This code is distributed under the terms and conditions of the
     CCP4 Program Suite Licence Agreement as a CCP4 Library.
     A copy of the CCP4 licence can be obtained by writing to the
     CCP4 Secretary, Daresbury Laboratory, Warrington WA4 4AD, UK.
*/
#ifndef _GUARD_FILE_ERR
#define _GUARD_FILE_ERR

#define CCP4_ERRNO(y) (CCP4_ERR_FILE | (y))
#define CIO_Ok 0
#define CIO_BadMode 1
#define CIO_CantOpenFile 2
#define CIO_MaxFile 3
#define CIO_ReadFail 4
#define CIO_WriteFail 5
#define CIO_CloseFail 6
#define CIO_SeekFail 7
#define CIO_NullPtr 8
#define CIO_EOF 9
#define CIO_NoFile 10
#define CIO_NotOpen 11
#define CIO_UnlinkFail 12

#endif

