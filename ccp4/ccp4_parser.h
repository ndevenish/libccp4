/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/

/*   ccp4_parser.h

     Header file for ccp4_parser routines
     Peter Briggs CCP4 April 2001
*/

/*------------------------------------------------------------------*/

/* Macro definitions */

/*------------------------------------------------------------------*/

#ifndef __CCP4_Parser__
#define __CCP4_Parser__

static char rcsidhhh[] = "$Id$";

/* note order: these must be outside CCP4 namespace */
#include <stdio.h>
#include"ccp4_utils.h"
#include"ccp4_spg.h"

/* Macro to make C functions callable from C++ */
#ifdef  __cplusplus
namespace CCP4 {
extern "C" {
typedef CSym::ccp4_symop ccp4_symop;
#endif

/*------------------------------------------------------------------*/

/* Parser String Sizes */

/*------------------------------------------------------------------*/

#define CCP4_PARSER_MAXLINELENGTH     200
#define CCP4_PARSER_MAXFILENAMELENGTH 200

/*------------------------------------------------------------------*/

/* Structures and typedefs */

/*------------------------------------------------------------------*/

/* CCP4 Parser token
   Construct to hold the information about a single token */

typedef struct {
  char   *fullstring;   /* Full string containing all of token */
  char   word[5];       /* First four characters of token */
  double value;         /* Equivalent numerical value */
  int    isstring;      /* Flag: true if token is character string */
  int    strlen;        /* Number of characters in whole token (strings only) */
  int    isnumber;      /* Flag: true if token is number */
  int    intdigits;     /* Number of 'digits' preceeding the decimal point
			   (numbers only) */
  int    frcdigits;     /* Number of 'digits' after the decimal point (numbers
			   only) */
  int    isquoted;      /* Flag: true if token is contained in quotes */
  int    isnull;        /* Flag: true if token is null field */
  int    ibeg,iend;     /* Begin and end character positions of token
			   in input line */
} CCP4PARSERTOKEN;

/* CCP4 Parser array
   Construct to hold the information about a parsed line */

typedef struct {
  /* "Public" members */
  char   keyword[5];      /* Keyword (=token[1].token, uppercased) */
  int    ntokens;         /* Number of tokens */
  CCP4PARSERTOKEN *token; /* Array of tokens */
  /* "Private" members */
  FILE   *fp;             /* Pointer to an external command file */
  int    maxtokens;       /* Maximum number of tokens allowed */
  char   *delim;          /* List of delimiter characters */
  char   *nulldelim;      /* List of null delimiter characters */
  char   *comment;        /* List of comment characters */
  double max_exponent;    /* Largest allowed exponent for numerical tokens */
  double min_exponent;    /* Smallest allowed exponent for numerical tokens */
} CCP4PARSERARRAY;     

/*------------------------------------------------------------------*/

/* Function Prototypes */

/*------------------------------------------------------------------*/

/* Core cparser functions */

CCP4PARSERARRAY* ccp4_parse_start(const int maxtokens);

int ccp4_parse_end(CCP4PARSERARRAY *parsePtr);

int ccp4_parse_init_token(const CCP4PARSERARRAY *parsePtr, const int itok);

int ccp4_parse_delimiters(CCP4PARSERARRAY *parsePtr, const char *delim,
				  const char *nulldelim);

int ccp4_parse_comments(CCP4PARSERARRAY *parsePtr, const char *comment_chars);

int ccp4_parse_maxmin(CCP4PARSERARRAY *parsePtr, const double max_exponent,
			      const double min_exponent);

int ccp4_parse_reset(CCP4PARSERARRAY *parsePtr);

int ccp4_parse(char *line, CCP4PARSERARRAY *parser);

int ccp4_parser(char *line, const int n, CCP4PARSERARRAY *parser,
			const int print);

/* External utility functions */

int ccp4_keymatch(const char *keyin1, const char *keyin2);

/* Internal utility functions */

char *strtoupper (char *str1, const char *str2);

char *strtolower (char *str1, const char *str2);

int strmatch (const char *str1, const char *str2);

int charmatch(const char character, const char *charlist);

int doublefromstr(const char *str, const double max_exp, const double min_exp,
			  double *valuePtr, double *intvaluePtr, int *intdigitsPtr,
			  double *frcvaluePtr, int *frcdigitsPtr,
			  double *expvaluePtr, int *expdigitsPtr);

/* This is Charles' version of symfr */
ccp4_symop symop_to_rotandtrn(const char *symchs_begin, const char *symchs_end);
int symop_to_mat4(const char *symchs_begin, const char *symchs_end, float *rot);
ccp4_symop mat4_to_rotandtrn(const float rsm[4][4]);
/* This is Charles' version of symtr */
char *rotandtrn_to_symop(char *symchs_begin, char *symchs_end, const ccp4_symop symop);
void rotandtrn_to_mat4(float rsm[4][4], const ccp4_symop symop);
char *mat4_to_symop(char *symchs_begin, char *symchs_end, const float rsm[4][4]);
char *mat4_to_recip_symop(char *symchs_begin, char *symchs_end, const float rsm[4][4]);

#ifdef __cplusplus
}
}
#endif

#endif  /* __CCP4_Parser__ */
