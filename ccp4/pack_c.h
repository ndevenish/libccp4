#define WORD short int
/* WORD is a two-byte integer. */

#define LONG int
/* LONG is a four byte integer. */
/* Dave Love 5/7/94: using `int' gets you 4 bytes on the 32-bit Unix
   (and VAX) systems I know of and also on (64-bit) OSF/1 Alphas which
   have 64-bit longs.  (This definition previously used `long'.) */





/* Functions required for packing: */


void v2pack_wordimage_c(WORD *img, int x, int y, char *filename);
/* Pack image 'img', containing 'x * y' WORD-sized pixels into 'filename'.
   This function generates Version 2 images! */

void v2pack_longimage_c(LONG *img, int x, int y, char *filename);
/* Pack image 'img', containing 'x * y' LONG-sized pixels into 'filename'. 
   This function generates Version 2 images! */


/* Functions required for unpacking: */


void readpack_word_c(WORD *img, char *filename);
/* Unpacks packed image from 'filename' into the WORD-array 'img'. Scans the
   file defined by 'filename' until the PACKIDENTIFIER is found, then unpacks
   starting from there. */

void readpack_long_c(LONG *img, char *filename);
/* Unpacks packed image from 'filename' into the LONG-array 'img'. Scans the
   file defined by 'filename' until the PACKIDENTIFIER is found, then unpacks
   starting from there. */

void imsiz_c(char *filename, LONG *x, LONG *y);
/* Determines the size of the the packed image "filename" after unpacking. The
   dimensions are returned in x and y. */



