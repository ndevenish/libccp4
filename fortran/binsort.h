/*
     binsort.h: header for binary sorting functions

     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part 1' (Annex 2) software.
     A copy of the CCP4 licence can be obtained by writing to the
     CCP4 Secretary, Daresbury Laboratory, Warrington WA4 4AD, UK.
*/
/****************************************************************************
  binsort.h
  Z130891

binsort - key data types definition.
****************************************************************************/

/*** Data types definition name begining with U means unsigned... ***/

#define CHAR        (int)1
#define UCHAR       (int)2
#define SHORT       (int)3
#define USHORT      (int)4
#define LONG        (int)5
#define ULONG       (int)6
#define FLOAT       (int)7
#define DOUBLE      (int)8

/*** Sorting order ***/
#define ASC         (int)0      /* ascending */
#define DESC        (int)1      /* descending */
