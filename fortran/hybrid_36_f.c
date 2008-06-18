/*
     hybrid_36_f.c: Fortran API to hybrid_36.c
     Copyright (C) 2008  STFC, Martyn Winn

     This library is free software: you can redistribute it and/or
     modify it under the terms of the GNU Lesser General Public License
     version 3, modified in accordance with the provisions of the 
     license to address the requirements of UK law.
 
     You should have received a copy of the modified GNU Lesser General 
     Public License along with this library.  If not, copies may be 
     downloaded from http://www.ccp4.ac.uk/ccp4license.php
 
     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU Lesser General Public License for more details.
*/

/** @file hybrid_36_f.c
 *  Fortran API to hybrid_36.c.
 *  Martyn Winn
 */

#include "ccp4_fortran.h"
#include "mmdb/hybrid_36.h"
/* rcsid[] = "$Id$" */

/* I wanted to do these as Fortran functions, but not sure I could handle
   the string conversion properly for CHARACTER function.
*/


/* hybrid-36 encoder: converts integer value to string result

      iwidth: must be 4 (e.g. for residue sequence numbers)
                  or 5 (e.g. for atom serial numbers)

      value: integer value to be converted

      strval: char array containing string result
*/

FORTRAN_SUBR ( HY36ENCODE_F, hy36encode_f,
               (const int *iwidth, int *value, fpstr strval, int strval_len),
               (const int *iwidth, int *value, fpstr strval),
               (const int *iwidth, int *value, fpstr strval, int strval_len))
{
  unsigned width;
  char result[6];

  width = (unsigned) *iwidth;

  if (hy36encode(width, *value, result)) {
    printf("problem in hy36encode_f! \n");
  }
  ccp4_CtoFString(FTN_STR(strval),FTN_LEN(strval),result);

}

/*  hybrid-36 decoder: converts string s to integer result

      iwidth: must be 4 (e.g. for residue sequence numbers)
                  or 5 (e.g. for atom serial numbers)

      strval: string to be converted

      value: integer holding the conversion result
*/


FORTRAN_SUBR ( HY36DECODE_F, hy36decode_f,
               (const int *iwidth, fpstr strval, int *value, int strval_len),
               (const int *iwidth, fpstr strval, int *value),
               (const int *iwidth, fpstr strval, int strval_len, int *value))

{
  unsigned width;
  char* s;

  width = (unsigned) *iwidth;
  s = ccp4_FtoCString(FTN_STR(strval), FTN_LEN(strval));

  if (hy36decode(width, s, width, value)) {
    printf("problem in hy36decode_f! \n");
  }

} 
