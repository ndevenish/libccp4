#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pack_c.h"


/* pack_c.c, version 2 (backwards compatible with earlier versions)...
                                                    JPA, 26 June 1995
						    jpa@mrc-lmb.cam.ac.uk

   This file contains functions capable of compressing and decompressing 
   images. It is especially suited for X-ray diffraction patterns, or other 
   image formats in which orthogonal pixels contain "grey-levels" and
   vary smoothly accross the image. Clean images measured by a MAR-research
   image plate scanner containing two bytes per pixel can be compressed by 
   a factor of 3.5 to 4.5 .

   Since the images are encoded in a byte-stream, there should be no problem
   concerening big- or little ended machines: both will produce an identical
   packed image.

   Compression is achieved by first calculating the differences between every
   pixel and the truncated value of four of its neighbours. For example:
   the difference for a pixel at img[x, y] is:
   
     img[x, y] - (int) (img[x-1, y-1] + 
                        img[x-1, y] + 
			img[x-1, y+1] + 
			img[x, y-1]) / 4

   After calculating the differences, they are encoded in a packed array. A 
   packed array consists of consequitive chunks which have the following format:
   - Three bits containing the logarithm base 2 of the number of pixels encoded
     in the chunk.
   - Three bits defining the number of bits used to encode one element of the
     chunk. The value of these three bits is used as index in a lookup table 
     to get the actual number of bits of the elements of the chunk.
        Note: in version 2, there are four bits in this position!! This allows
              more efficient packing of synchrotron data! The routines in this
	      sourcefile are backwards compatible.
	                                             JPA, 26 June 1995
   - The truncated pixel differences.

   To compress an image, call pack_wordimage_c() or pack_longimage_c(). These
   will append the packed image to any header information already written to
   disk (take care that the file containing this information is closed before
   calling). To decompress an image, call readpack_word_c() or
   readpack_long_c(). These functions will find the start of the packed image
   themselves, irrespective of the header format.

   In order to provide an interface to fortran programs, the functions 
   pack_wordimage_f(), pack_longimage_f(), read_wordimage_f() and 
   read_long_image_f() are provided. They are called by the fortran subroutines
   PACK_WORDIMAGE, PACK_LONGIMAGE, READPACK_WORD, and READPACK_LONG, which
   can be found in the accompanying sourcefile "pack_f.f".


                                            Jan Pieter Abrahams, 6 Jan 1993   */




/******************************************************************************/

/* Some general defines: */


#define PACKIDENTIFIER "\nCCP4 packed image, X: %04d, Y: %04d\n"
/* This string defines the start of a packed image. An image file is scanned
   until this string is encountered, the size of the unpacked image is 
   determined from the values of X and Y (which are written out as formatted
   ascii numbers), and the packed image is expected to start immediately after
   the null-character ending the string. */

#define V2IDENTIFIER "\nCCP4 packed image V2, X: %04d, Y: %04d\n"
/* This string defines the start of a packed image. An image file is scanned
   until this string is encountered, the size of the unpacked image is 
   determined from the values of X and Y (which are written out as formatted
   ascii numbers), and the packed image is expected to start immediately after
   the null-character ending the string. */

#define PACKBUFSIZ BUFSIZ
/* Size of internal buffer in which the packed array is stored during transit
   form an unpacked image to a packed image on disk. It is set to the size
   used by the buffered io-routines given in <stdio.h>, but it could be 
   anything. */

#define DIFFBUFSIZ 16384L
/* Size of the internal buffer in which the differences between neighbouring 
   pixels are stored prior to compression. The image is therefore compressed 
   in DIFFBUFSIZ chunks. Decompression does not need to know what DIFFBUFSIZ
   was when the image was compressed. By increasing this value, the image
   can be compressed into a packed image which is a few bytes smaller. Do
   not decrease the value of DIFFBUFSIZ below 128L. */
 
#define BYTE char
/* BYTE is a one byte integer. */

#define WORD short int
/* WORD is a two-byte integer. */

#define LONG int
/* LONG is a four byte integer. */
/* Dave Love 5/7/94: using `int' gets you 4 bytes on the 32-bit Unix
   (and VAX) systems I know of and also on (64-bit) OSF/1 Alphas which
   have 64-bit longs.  (This definition previously used `long'.) */



/******************************************************************************/

/* Some usefull macros used in the code of this sourcefile: */


#define max(x, y) (((x) > (y)) ? (x) : (y)) 
/* Returns maximum of x and y. */

#define min(x, y) (((x) < (y)) ? (x) : (y)) 
/* Returns minimum of x and y. */

#undef abs			/* avoid complaint from DEC C, at least */
#define abs(x) (((x) < 0) ? (-(x)) : (x))
/* Returns the absolute value of x. */

static const LONG setbits[33] =
                         {0x00000000L, 0x00000001L, 0x00000003L, 0x00000007L,
			  0x0000000FL, 0x0000001FL, 0x0000003FL, 0x0000007FL,
			  0x000000FFL, 0x000001FFL, 0x000003FFL, 0x000007FFL,
			  0x00000FFFL, 0x00001FFFL, 0x00003FFFL, 0x00007FFFL,
			  0x0000FFFFL, 0x0001FFFFL, 0x0003FFFFL, 0x0007FFFFL,
			  0x000FFFFFL, 0x001FFFFFL, 0x003FFFFFL, 0x007FFFFFL,
			  0x00FFFFFFL, 0x01FFFFFFL, 0x03FFFFFFL, 0x07FFFFFFL,
			  0x0FFFFFFFL, 0x1FFFFFFFL, 0x3FFFFFFFL, 0x7FFFFFFFL,
                          0xFFFFFFFFL};
/* This is not a macro really, but I've included it here anyway. Upon indexing,
   it returns a LONG with the lower (index) number of bits set. It is equivalent
   to the following macro:
     #define setbits(n) (((n) == 32) : ((1L << (n)) - 1) : (-1L)) 
   Indexing the const array should usually be slightly faster. */

#define shift_left(x, n)  (((x) & setbits[32 - (n)]) << (n))
/* This macro is included because the C standard does not properly define a 
   left shift: on some machines the bits which are pushed out at the left are
   popped back in at the right. By masking, the macro prevents this behaviour.
   If you are sure that your machine does not pops bits back in, you can speed
   up the code insignificantly by taking out the masking. */

#define shift_right(x, n) (((x) >> (n)) & setbits[32 - (n)])
/* See comment on left shift. */



/******************************************************************************/

/* Some fortran compilers require c-functions to end with an underscore. */

#if defined(_AIX) || defined(__hpux)
/* no underscore by default */
#else
#  if defined (VMS) || defined (vms) || defined (__vms) || defined (__VMS)\
      || defined (ardent) || defined (titan) || defined (stardent)
#    define pack_wordimage_f PACK_WORDIMAGE_F
#    define v2pack_wordimage_f V2PACK_WORDIMAGE_F
#    define pack_longimage_f PACK_LONGIMAGE_F
#    define v2pack_longimage_f V2PACK_LONGIMAGE_F
#    define readpack_word_f READPACK_WORD_F
#    define readpack_long_f READPACK_LONG_F
#    define mirror_wordimage MIRROR_WORDIMAGE
#    define mirror_longimage MIRROR_LONGIMAGE
#    define imsiz_f IMSIZ_F
#  else
#    define pack_wordimage_f pack_wordimage_f_
#    define v2pack_wordimage_f v2pack_wordimage_f_
#    define pack_longimage_f pack_longimage_f_
#    define v2pack_longimage_f v2pack_longimage_f_
#    define readpack_word_f readpack_word_f_
#    define readpack_long_f readpack_long_f_
#    define mirror_wordimage mirror_wordimage_
#    define mirror_longimage mirror_longimage_
#    define imsiz_f imsiz_f_
#  endif
#endif

/******************************************************************************/

/* Prototypes of the functions in this sourcefile, as required by the ANSI 
   standard. The pack_c.h file contains the functions other routines might
   call. Here are functions which will are not really usefull for image
   processing programmes, and which are used locally in this file. Also 
   front-end fortran callable C-functions are not included in the pack_c.h
   file. */


/* Functions required for packing: */


void pack_wordimage_f(WORD *img, LONG *x, LONG *y, LONG *filename);
/* Fortran frontend of pack_wordimage_c. Because the way in which fortran
   passes strings is not defined, it passes the filename in which the
   packed image should be stored as an array of LONGs. */

void v2pack_wordimage_f(WORD *img, LONG *x, LONG *y, LONG *filename);
/* Fortran frontend of pack_wordimage_c. Because the way in which fortran
   passes strings is not defined, it passes the filename in which the
   packed image should be stored as an array of LONGs. This function generates
   Version 2 images! */

void pack_longimage_f(LONG *img, LONG *x, LONG *y, LONG *filename);
/* Fortran frontend of pack_longimage_c. Because the way in which fortran
   passes strings is not defined, it passes the filename in which the
   packed image should be stored as an array of LONGs. */

void v2pack_longimage_f(LONG *img, LONG *x, LONG *y, LONG *filename);
/* Fortran frontend of pack_longimage_c. Because the way in which fortran
   passes strings is not defined, it passes the filename in which the
   packed image should be stored as an array of LONGs. This function generates
   Version 2 images! */

void pack_wordimage_c(WORD *img, int x, int y, char *filename);
/* Pack image 'img', containing 'x * y' WORD-sized pixels into 'filename'. */

void pack_longimage_c(LONG *img, int x, int y, char *filename);
/* Pack image 'img', containing 'x * y' LONG-sized pixels into 'filename'. */

LONG *diff_words(WORD *img, int x, int y, LONG *diffs, LONG done);
/* Calculates the difference of WORD-sized pixels of an image with the 
   truncated mean value of four of its neighbours. 'x' is the number of fast
   coordinates of the image 'img', 'y' is the number of slow coordinates,
   'diffs' will contain the differences, 'done' defines the index of the pixel 
   where calculating the differences should start. A pointer to the last
   difference is returned. Maximally DIFFBUFSIZ differences are returned in 
   'diffs'.*/

LONG *diff_longs(LONG *img, int x, int y, LONG *diffs, LONG done);
/* Calculates the difference of LONG-sized pixels of an image with the 
   truncated mean value of four of its neighbours. 'x' is the number of fast
   coordinates of the image 'img', 'y' is the number of slow coordinates,
   'diffs' will contain the differences, 'done' defines the index of the pixel 
   where calculating the differences should start. A pointer to the last
   difference is returned. Maximally DIFFBUFSIZ differences are returned in 
   'diffs'.*/

int bits(LONG *chunk, int n);
/* Returns the number of bits neccesary to encode the longword-array 'chunk'
   of size 'n' The size in bits of one encoded element can be 0, 4, 5, 6, 7, 
   8, 16 or 32. */

int v2bits(LONG *chunk, int n);
/* Returns the number of bits neccesary to encode the longword-array 'chunk'
   of size 'n' The size in bits of one encoded element can be 0, 3, 4, 5, 6, 7, 
   8, 9, 10, 11, 12, 13, 14, 15, 16 or 32. */

void pack_chunk(LONG *lng, int nmbr, int bitsize, FILE *file);
/* Packs 'nmbr' LONGs starting at 'lng[0]' into a packed array of 'bitsize'
   sized elements. If the internal buffer in which the array is packed is full,
   it is flushed to 'file', making room for more of the packed array. If 
   ('lng == NULL'), the buffer is flushed aswell. */

void v2pack_chunk(LONG *lng, int nmbr, int bitsize, FILE *file);
/* Packs 'nmbr' LONGs starting at 'lng[0]' into a packed array of 'bitsize'
   sized elements. If the internal buffer in which the array is packed is full,
   it is flushed to 'file', making room for more of the packed array. If 
   ('lng == NULL'), the buffer is flushed aswell. This is a new function
   included in version 2, but not existing in version 1! */

void pack_longs(LONG *lng, int n, BYTE **target, int *bit, int size);
/* Pack 'n' WORDS, starting with 'lng[0]' into the packed array 'target'. The 
   elements of such a packed array do not obey BYTE-boundaries, but are put one 
   behind the other without any spacing. Only the 'bitsiz' number of least 
   significant bits are used, the rest is truncated. The starting bit of
   'target' is 'bit' (bits range from 0 to 7). After completion of 
   'pack_words()', both '**target' and '*bit' are updated and define the next 
   position in 'target' from where packing could continue. */



/* Functions required for unpacking: */


void readpack_word_f(WORD *img, LONG *filename);
/* Fortran frontend of readpack_word_c. Because the way in which fortran
   passes strings is not defined, it passes the filename in which the
   packed image should be stored as an array of LONGs. */

void readpack_long_f(LONG *img, LONG *filename);
/* Fortran frontend of readpack_long_c. Because the way in which fortran
   passes strings is not defined, it passes the filename in which the
   packed image should be stored as an array of LONGs. */

void unpack_word(FILE *packfile, int x, int y, WORD *img);
/* Unpacks a packed image into the WORD-array 'img'. The image is stored
   in 'packfile'. The file should be properly positioned: the first BYTE 
   read is assumed to be the first BYTE of the packed image. */

void v2unpack_word(FILE *packfile, int x, int y, WORD *img);
/* Unpacks a packed image into the WORD-array 'img'. The image is stored
   in 'packfile'. The file should be properly positioned: the first BYTE
   read is assumed to be the first BYTE of the packed image. This function 
   reads Version 2 packed images! */

void unpack_long(FILE *packfile, int x, int y, LONG *img);
/* Unpacks a packed image into the LONG-array 'img'. The image is stored
   in 'packfile'. The file should be properly positioned: the first BYTE 
   read is assumed to be the first BYTE of the packed image. */

void v2unpack_long(FILE *packfile, int x, int y, LONG *img);
/* Unpacks a packed image into the LONG-array 'img'. The image is stored
   in 'packfile'. The file should be properly positioned: the first BYTE
   read is assumed to be the first BYTE of the packed image. This function 
   reads Version 2 packed images! */




/* Function required to convert a WORD-array into a char array, required for
   both compression and decompression if called from fortran. */

char *long_to_char(LONG *lng, char *string);
/* Shrinks an array of LONGs into an array of chars, used in order to translate 
   an encoded string array passed by fortran into a c-type string. Returns
   'string'. */

void imsiz_f(LONG *filename, LONG *x, LONG *y);
/* Fortran frontend of imsiz_c. Because the way in which fortran
   passes strings is not defined, it passes the filename in which the
   packed image should be stored as an array of LONGs. */



/* Some other usefull functions for manipulating images. */

void mirror_wordimg(WORD *img, LONG *x, LONG *y);
/* Replaces img with its mirror by interchanging rows. '*x' is the fast index,
   '*y' is the slow index. */

void mirror_longimg(LONG *img, LONG *x, LONG *y);
/* Replaces img with its mirror by interchanging rows. '*x' is the fast index,
   '*y' is the slow index. */




/******************************************************************************/

void pack_wordimage_f(WORD *img, LONG *x, LONG *y, LONG *filename)
/* Fortran frontend of pack_wordimage_c. Because the way in which fortran
   passes strings is not defined, it passes the filename in which the
   packed image should be stored as an array of LONGs. */

{ char c_filename[1024];

  pack_wordimage_c(img, (LONG) *x, (LONG) *y,
		                           long_to_char(filename, c_filename));}



/******************************************************************************/

void v2pack_wordimage_f(WORD *img, LONG *x, LONG *y, LONG *filename)
/* Fortran frontend of pack_wordimage_c. Because the way in which fortran
   passes strings is not defined, it passes the filename in which the
   packed image should be stored as an array of LONGs. This function generates
   Version 2 images!*/

{ char c_filename[1024];

  v2pack_wordimage_c(img, (LONG) *x, (LONG) *y,
		                           long_to_char(filename, c_filename));}



/******************************************************************************/

void pack_longimage_f(LONG *img, LONG *x, LONG *y, LONG *filename)
/* Fortran frontend of pack_longimage_c. Because the way in which fortran
   passes strings is not defined, it passes the filename in which the
   packed image should be stored as an array of LONGs. */

{ char c_filename[1024];

  pack_longimage_c(img, (LONG) *x, (LONG) *y,
		                           long_to_char(filename, c_filename));}



/******************************************************************************/

void v2pack_longimage_f(LONG *img, LONG *x, LONG *y, LONG *filename)
/* Fortran frontend of pack_longimage_c. Because the way in which fortran
   passes strings is not defined, it passes the filename in which the
   packed image should be stored as an array of LONGs. */

{ char c_filename[1024];

  v2pack_longimage_c(img, (LONG) *x, (LONG) *y,
		                           long_to_char(filename, c_filename));}



/******************************************************************************/

void pack_wordimage_c(WORD *img, int x, int y, char *filename)

/* Pack image 'img', containing 'x * y' WORD-sized pixels into 'filename'. */

{ int chunksiz, packsiz, nbits, next_nbits, tot_nbits;
  LONG buffer[DIFFBUFSIZ];
  LONG *diffs = buffer;
  LONG *end = diffs - 1;
  LONG done = 0;
  FILE *packfile;

  packfile = fopen(filename, "a");
  if (packfile == NULL)
  { fprintf(stderr,"The file %s cannot be created!\n   ...giving up...\n", 
	    filename);
    exit(1);}
  else
  { fprintf(packfile, PACKIDENTIFIER, x, y);
    while (done < (x * y))
    { end = diff_words(img, x, y, buffer, done);
      done += (end - buffer) + 1;
      diffs = buffer;
      while (diffs <= end)
      { packsiz = 0;
        chunksiz = 1;
        nbits = bits(diffs, 1);
        while (packsiz == 0)
        { if (end <= (diffs + chunksiz * 2))
	    packsiz = chunksiz;
          else
	  { next_nbits = bits(diffs + chunksiz, chunksiz); 
	    tot_nbits = 2 * max(nbits, next_nbits);
	    if (tot_nbits >= (nbits + next_nbits + 6))
	      packsiz = chunksiz;
	    else
            { nbits = tot_nbits;
	      if (chunksiz == 64)
	        packsiz = 128;
	      else
	        chunksiz *= 2;}}}
        pack_chunk(diffs, packsiz, nbits / packsiz, packfile);
        diffs += packsiz;}}
    pack_chunk(NULL, 0, 0, packfile);
    fclose(packfile);}}



/******************************************************************************/

void v2pack_wordimage_c(WORD *img, int x, int y, char *filename)

/* Pack image 'img', containing 'x * y' WORD-sized pixels into 'filename'. */

{ int chunksiz, packsiz, nbits, next_nbits, tot_nbits;
  LONG buffer[DIFFBUFSIZ];
  LONG *diffs = buffer;
  LONG *end = diffs - 1;
  LONG done = 0;
  FILE *packfile;

  packfile = fopen(filename, "a");
  if (packfile == NULL)
  { fprintf(stderr,"The file %s cannot be created!\n   ...giving up...\n", 
	    filename);
    exit(1);}
  else
  { fprintf(packfile, V2IDENTIFIER, x, y);
    while (done < (x * y))
    { end = diff_words(img, x, y, buffer, done);
      done += (end - buffer) + 1;
      diffs = buffer;
      while (diffs <= end)
      { packsiz = 0;
        chunksiz = 1;
        nbits = v2bits(diffs, 1);
        while (packsiz == 0)
        { if (end <= (diffs + chunksiz * 2))
	    packsiz = chunksiz;
          else
	  { next_nbits = v2bits(diffs + chunksiz, chunksiz); 
	    tot_nbits = 2 * max(nbits, next_nbits);
	    if (tot_nbits >= (nbits + next_nbits + 7))
	      packsiz = chunksiz;
	    else
            { nbits = tot_nbits;
	      if (chunksiz == 64)
	        packsiz = 128;
	      else
	        chunksiz *= 2;}}}
        v2pack_chunk(diffs, packsiz, nbits / packsiz, packfile);
        diffs += packsiz;}}
    v2pack_chunk(NULL, 0, 0, packfile);
    fclose(packfile);}}



/******************************************************************************/

void pack_longimage_c(LONG *img, int x, int y, char *filename)

/* Pack image 'img', containing 'x * y' LONG-sized pixels into 'filename'. */

{ int chunksiz, packsiz, nbits, next_nbits, tot_nbits;
  LONG buffer[DIFFBUFSIZ];
  LONG *diffs = buffer;
  LONG *end = diffs - 1;
  LONG done = 0;
  FILE *packfile;

  packfile = fopen(filename, "a");
  if (packfile == NULL)
  { fprintf(stderr,"The file %s cannot be created!\n   ...giving up...\n", 
	    filename);
    exit(1);}
  else
  { fprintf(packfile, PACKIDENTIFIER, x, y);
    while (done < (x * y))
    { end = diff_longs(img, x, y, buffer, done);
      done += (end - buffer) + 1;
      diffs = buffer;
      while (diffs <= end)
      { packsiz = 0;
        chunksiz = 1;
        nbits = bits(diffs, 1);
        while (packsiz == 0)
        { if (end <= (diffs + chunksiz * 2))
	    packsiz = chunksiz;
          else
	  { next_nbits = bits(diffs + chunksiz, chunksiz); 
	    tot_nbits = 2 * max(nbits, next_nbits);
	    if (tot_nbits >= (nbits + next_nbits + 6))
	      packsiz = chunksiz;
	    else
            { nbits = tot_nbits;
	      if (chunksiz == 64)
	        packsiz = chunksiz * 2;
	      else
	        chunksiz *= 2;}}}
        pack_chunk(diffs, packsiz, nbits / packsiz, packfile);
        diffs += packsiz;}}
    pack_chunk(NULL, 0, 0, packfile);
    fclose(packfile);}}



/******************************************************************************/

void v2pack_longimage_c(LONG *img, int x, int y, char *filename)

/* Pack image 'img', containing 'x * y' LONG-sized pixels into 'filename'. */

{ int chunksiz, packsiz, nbits, next_nbits, tot_nbits;
  LONG buffer[DIFFBUFSIZ];
  LONG *diffs = buffer;
  LONG *end = diffs - 1;
  LONG done = 0;
  FILE *packfile;

  packfile = fopen(filename, "a");
  if (packfile == NULL)
  { fprintf(stderr,"The file %s cannot be created!\n   ...giving up...\n", 
	    filename);
    exit(1);}
  else
  { fprintf(packfile, V2IDENTIFIER, x, y);
    while (done < (x * y))
    { end = diff_longs(img, x, y, buffer, done);
      done += (end - buffer) + 1;
      diffs = buffer;
      while (diffs <= end)
      { packsiz = 0;
        chunksiz = 1;
        nbits = v2bits(diffs, 1);
        while (packsiz == 0)
        { if (end <= (diffs + chunksiz * 2))
	    packsiz = chunksiz;
          else
	  { next_nbits = v2bits(diffs + chunksiz, chunksiz); 
	    tot_nbits = 2 * max(nbits, next_nbits);
	    if (tot_nbits >= (nbits + next_nbits + 7))
	      packsiz = chunksiz;
	    else
            { nbits = tot_nbits;
	      if (chunksiz == 64)
	        packsiz = chunksiz * 2;
	      else
	        chunksiz *= 2;}}}
        v2pack_chunk(diffs, packsiz, nbits / packsiz, packfile);
        diffs += packsiz;}}
    v2pack_chunk(NULL, 0, 0, packfile);
    fclose(packfile);}}



/******************************************************************************/

LONG *diff_words(WORD *word, int x, int y, LONG *diffs, LONG done)

/* Calculates the difference of WORD-sized pixels of an image with the
   truncated mean value of four of its neighbours. 'x' is the number of fast
   coordinates of the image 'img', 'y' is the number of slow coordinates,
   'diffs' will contain the differences, 'done' defines the index of the pixel
   where calculating the differences should start. A pointer to the last
   difference is returned. Maximally DIFFBUFSIZ differences are returned in
   'diffs'.*/

{ LONG i = 0;
  LONG tot = x * y;

  if (done == 0)
  { *diffs = word[0];
    ++diffs;
    ++done;
    ++i;}
  while ((done <= x) && (i < DIFFBUFSIZ))
  { *diffs = word[done] - word[done - 1];
    ++diffs;
    ++done;
    ++i;}
  while ((done < tot) && (i < DIFFBUFSIZ))
  { *diffs = word[done] - (word[done - 1] + word[done - x + 1] +
                           word[done - x] + word[done - x - 1] + 2) / 4;
    ++diffs;
    ++done;
    ++i;}
  return(--diffs);}



/******************************************************************************/

LONG *diff_longs(LONG *lng, int x, int y, LONG *diffs, LONG done)

/* Calculates the difference of LONG-sized pixels of an image with the
   truncated mean value of four of its neighbours. 'x' is the number of fast
   coordinates of the image 'img', 'y' is the number of slow coordinates,
   'diffs' will contain the differences, 'done' defines the index of the pixel
   where calculating the differences should start. A pointer to the last
   difference is returned. Maximally DIFFBUFSIZ differences are returned in
   'diffs'.*/

{ LONG i = 0, d;
  LONG tot = x * y;
  LONG huge = shift_left(1, 30);

  if (done == 0)
  { *diffs = min(max(-huge, lng[0]), huge);
    ++diffs;
    ++done;
    ++i;}
  while ((done <= x) && (i < DIFFBUFSIZ))
  { d = lng[done] - lng[done - 1];
    *diffs = min(max(-huge, d), huge);
    ++diffs;
    ++done;
    ++i;}
  while ((done < tot) && (i < DIFFBUFSIZ))
  { d = lng[done] - (lng[done - 1] + lng[done - x + 1] +
		     lng[done - x] + lng[done - x - 1] + 2) / 4;
    *diffs = min(max(-huge, d), huge);
    ++diffs;
    ++done;
    ++i;}
  return(--diffs);}



/******************************************************************************/

int bits(LONG *chunk, int n)

/* Returns the number of bits neccesary to encode the longword-array 'chunk'
   of size 'n' The size in bits of one encoded element can be 0, 4, 5, 6, 7,
   8, 16 or 32. */

{ int size, maxsize, i;

  for (i = 1, maxsize = abs(chunk[0]); i < n; ++i)
    maxsize = max(maxsize, abs(chunk[i]));
  if (maxsize == 0)
    size = 0;
  else if (maxsize < 8)
    size = 4 * n;
  else if (maxsize < 16)
    size = 5 * n;
  else if (maxsize < 32)
    size = 6 * n;
  else if (maxsize < 64)
    size = 7 * n;
  else if (maxsize < 128)
    size = 8 * n;
  else if (maxsize < 32768)
    size = 16 * n;
  else
    size = 32 * n;
  return(size);}



/******************************************************************************/

int v2bits(LONG *chunk, int n)

/* Returns the number of bits neccesary to encode the longword-array 'chunk'
   of size 'n' The size in bits of one encoded element can be 0, 3, 4, 5, 6, 7,
   8, 9, 10, 11, 12, 13, 14, 15, 16 or 32. */

{ int size, maxsize, i;

  for (i = 1, maxsize = abs(chunk[0]); i < n; ++i)
    maxsize = max(maxsize, abs(chunk[i]));
  if (maxsize == 0)
    size = 0;
  else if (maxsize < 4)
    size = 3 * n;
  else if (maxsize < 8)
    size = 4 * n;
  else if (maxsize < 16)
    size = 5 * n;
  else if (maxsize < 32)
    size = 6 * n;
  else if (maxsize < 64)
    size = 7 * n;
  else if (maxsize < 128)
    size = 8 * n;
  else if (maxsize < 256)
    size = 9 * n;
  else if (maxsize < 512)
    size = 10 * n;
  else if (maxsize < 1024)
    size = 11 * n;
  else if (maxsize < 2048)
    size = 12 * n;
  else if (maxsize < 4096)
    size = 13 * n;
  else if (maxsize < 8192)
    size = 14 * n;
  else if (maxsize < 16384)
    size = 15 * n;
  else if (maxsize < 32768)
    size = 16 * n;
  else
    size = 32 * n;
  return(size);}



/******************************************************************************/

void pack_chunk(LONG *lng, int nmbr, int bitsize, FILE *packfile)

/* Packs 'nmbr' LONGs starting at 'lng[0]' into a packed array of 'bitsize'
   sized elements. If the internal buffer in which the array is packed is full,
   it is flushed to 'file', making room for more of the packed array. If 
   ('lng == NULL'), the buffer is flushed aswell. */

{ static LONG bitsize_encode[33] = {0, 0, 0, 0, 1, 2, 3, 4, 5, 0, 0,
                                    0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0,
                                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7};
  LONG descriptor[2], i, j;
  static BYTE *buffer = NULL;
  static BYTE *buffree = NULL;
  static int bitmark;

  if (buffer == NULL)
  { buffree = buffer = (BYTE *) malloc(PACKBUFSIZ);
    bitmark = 0;}
  if (lng != NULL)
  { for (i = nmbr, j = 0; i > 1; i /= 2, ++j);
    descriptor[0] = j;
    descriptor[1] = bitsize_encode[bitsize];
    if ((buffree - buffer) > (PACKBUFSIZ - (130 * 4)))
    { fwrite(buffer, sizeof(BYTE), buffree - buffer, packfile);
      buffer[0] = buffree[0];
      buffree = buffer;}
    pack_longs(descriptor, 2, &buffree, &bitmark, 3);
    pack_longs(lng, nmbr, &buffree, &bitmark, bitsize);}
  else
  { fwrite(buffer, sizeof(BYTE), (buffree - buffer) + 1, packfile);
    free((void *) buffer);
    buffer = NULL;}}



/******************************************************************************/

void v2pack_chunk(LONG *lng, int nmbr, int bitsize, FILE *packfile)

/* Packs 'nmbr' LONGs starting at 'lng[0]' into a packed array of 'bitsize'
   sized elements. If the internal buffer in which the array is packed is full,
   it is flushed to 'file', making room for more of the packed array. If 
   ('lng == NULL'), the buffer is flushed aswell. This is a new function
   included in version 2, but not existing in version 1! */

{ static LONG bitsize_encode[33] = {0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8,
                                    9, 10, 11, 12, 13, 14, 0, 0, 0, 0, 0,
                                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15};
  LONG descriptor[2], i, j;
  static BYTE *buffer = NULL;
  static BYTE *buffree = NULL;
  static int bitmark;

  if (buffer == NULL)
  { buffree = buffer = (BYTE *) malloc(PACKBUFSIZ);
    bitmark = 0;}
  if (lng != NULL)
  { for (i = nmbr, j = 0; i > 1; i /= 2, ++j);
    descriptor[0] = j;
    descriptor[1] = bitsize_encode[bitsize];
    if ((buffree - buffer) > (PACKBUFSIZ - (130 * 4)))
    { fwrite(buffer, sizeof(BYTE), buffree - buffer, packfile);
      buffer[0] = buffree[0];
      buffree = buffer;}
    pack_longs(descriptor, 1, &buffree, &bitmark, 3);
    pack_longs(descriptor + 1, 1, &buffree, &bitmark, 4);
    pack_longs(lng, nmbr, &buffree, &bitmark, bitsize);}
  else
  { fwrite(buffer, sizeof(BYTE), (buffree - buffer) + 1, packfile);
    free((void *) buffer);
    buffer = NULL;}}



/******************************************************************************/

void pack_longs(LONG *lng, int n, BYTE **target, int *bit, int size)

/* Pack 'n' WORDS, starting with 'lng[0]' into the packed array 'target'. The 
   elements of such a packed array do not obey BYTE-boundaries, but are put one 
   behind the other without any spacing. Only the 'bitsiz' number of least 
   significant bits are used. The starting bit of 'target' is 'bit' (bits range
   from 0 to 7). After completion of 'pack_words()', both '**target' and '*bit'
   are updated and define the next position in 'target' from which packing
   could continue. */

{ LONG mask, window;
  int valids, i, temp;
  int temp_bit = *bit;
  BYTE *temp_target = *target;

  if (size > 0)
  { mask = setbits[size];
    for (i = 0; i < n; ++i)
    { window = lng[i] & mask;
      valids = size;
      if (temp_bit == 0)
        *temp_target = (BYTE) window;
      else
      { temp = shift_left(window, temp_bit);
        *temp_target |= temp;}
      window = shift_right(window, 8 - temp_bit);
      valids = valids - (8 - temp_bit);
      if (valids < 0)
        temp_bit += size;
      else
      { while (valids > 0)
        { *++temp_target = (BYTE) window;
          window = shift_right(window, 8);
          valids -= 8;}
        temp_bit = 8 + valids;}
      if (valids == 0)
      { temp_bit = 0;
        ++temp_target;}}
  *target = temp_target;
  *bit = (*bit + (size * n)) % 8;}}



/******************************************************************************/

void readpack_word_f(WORD *img, LONG *filename)
/* Fortran frontend of readpack_word_c. Because the way in which fortran
   passes strings is not defined, it passes the filename in which the
   packed image should be stored as an array of LONGs. */

{ char c_filename[1024];

  readpack_word_c(img, long_to_char(filename, c_filename));}



/******************************************************************************/

void readpack_long_f(LONG *img, LONG *filename)
/* Fortran frontend of readpack_long_c. Because the way in which fortran
   passes strings is not defined, it passes the filename in which the
   packed image should be stored as an array of LONGs. */

{ char c_filename[1024];

  readpack_long_c(img, long_to_char(filename, c_filename));}



/******************************************************************************/

void readpack_word_c(WORD *img, char *filename)

/* Unpacks packed image from 'filename' into the WORD-array 'img'. Scans the
   file defined by 'filename' until the PACKIDENTIFIER is found, then unpacks
   starting from there. */

{ FILE *packfile;
  int x = 0, y = 0, i = 0, c = 0, version = 0;
  char header[BUFSIZ];

  packfile = fopen(filename, "r");
  if (packfile == NULL)
    printf("%s does not exist.\n", filename);
  else
  { header[0] = '\n';
    header[1] = 0;
    while ((c != EOF) && ((x == 0) || (y == 0)))
    { c = i = x = y = 0;
      while ((++i < BUFSIZ) && (c != EOF) && (c != '\n') && (x==0) && (y==0)) 
	if ((header[i] = c = getc(packfile)) == '\n')
	{ if (sscanf(header, PACKIDENTIFIER, &x, &y) == 2)
	    version = 1;
	  else if (sscanf(header, V2IDENTIFIER, &x, &y) == 2)
	    version = 2;}}
    if (version == 1)
      unpack_word(packfile, x, y, img);
    else if (version == 2)
      v2unpack_word(packfile, x, y, img);
    fclose(packfile);}}



/******************************************************************************/

void readpack_long_c(LONG *img, char *filename)

/* Unpacks packed image from 'filename' into the LONG-array 'img'. Scans the
   file defined by 'filename' until the PACKIDENTIFIER is found, then unpacks
   starting from there. */

{ FILE *packfile;
  int x = 0, y = 0, i = 0, c = 0, version = 0;
  char header[BUFSIZ];

  packfile = fopen(filename, "r");
  if (packfile == NULL)
    printf("%s does not exist.", filename);
  else
  { header[0] = '\n';
    header[1] = 0;
    while ((c != EOF) && ((x == 0) || (y == 0)))
    { c = i = x = y = 0;
      while ((++i < BUFSIZ) && (c != EOF) && (c != '\n') && (x==0) && (y==0)) 
	if ((header[i] = c = getc(packfile)) == '\n')
	{ if (sscanf(header, PACKIDENTIFIER, &x, &y) == 2)
	    version = 1;
	  else if (sscanf(header, V2IDENTIFIER, &x, &y) == 2)
	    version = 2;}}
    if (version == 1)
      unpack_long(packfile, x, y, img);
    else if (version == 2)
      v2unpack_long(packfile, x, y, img);
    fclose(packfile);}}



/******************************************************************************/

void unpack_word(FILE *packfile, int x, int y, WORD *img)

/* Unpacks a packed image into the WORD-array 'img'. The image is stored
   in 'packfile'. The file should be properly positioned: the first BYTE
   read is assumed to be the first BYTE of the packed image. */

{ int valids = 0, spillbits = 0, usedbits, total = x * y;
  LONG window = 0L, spill, pixel = 0, nextint, bitnum, pixnum;
  static int bitdecode[8] = {0, 4, 5, 6, 7, 8, 16, 32};

  while (pixel < total)
  { if (valids < 6) 
    { if (spillbits > 0)
      { window |= shift_left(spill, valids);
	valids += spillbits;
	spillbits = 0;}
      else
      { spill = (LONG) getc(packfile);
	spillbits = 8;}}
    else
    { pixnum = 1 << (window & setbits[3]);
      window = shift_right(window, 3);
      bitnum = bitdecode[window & setbits[3]];
      window = shift_right(window, 3);
      valids -= 6;
      while ((pixnum > 0) && (pixel < total))
      { if (valids < bitnum)
	{ if (spillbits > 0)
	  { window |= shift_left(spill, valids);
	    if ((32 - valids) > spillbits)
	    { valids += spillbits;
	      spillbits = 0;}
	    else
	    { usedbits = 32 - valids;
	      spill = shift_right(spill, usedbits);
	      spillbits -= usedbits;
	      valids = 32;}}
	  else
	  { spill = (LONG) getc(packfile);
	    spillbits = 8;}}
        else
	{ --pixnum;
	  if (bitnum == 0)
            nextint = 0;
	  else
	  { nextint = window & setbits[bitnum];
	    valids -= bitnum;
	    window = shift_right(window, bitnum);
	    if ((nextint & (1 << (bitnum - 1))) != 0)
	      nextint |= ~setbits[bitnum];}
	  if (pixel > x)
	  { img[pixel] = (WORD) (nextint + 
				      (img[pixel-1] + img[pixel-x+1] + 
                                       img[pixel-x] + img[pixel-x-1] + 2) / 4);
	    ++pixel;}
	  else if (pixel != 0)
	  { img[pixel] = (WORD) (img[pixel - 1] + nextint);
	    ++pixel;}
	  else
	    img[pixel++] = (WORD) nextint;}}}}}




/******************************************************************************/

void v2unpack_word(FILE *packfile, int x, int y, WORD *img)

/* Unpacks a packed image into the WORD-array 'img'. The image is stored
   in 'packfile'. The file should be properly positioned: the first BYTE
   read is assumed to be the first BYTE of the packed image. */

{ int valids = 0, spillbits = 0, usedbits, total = x * y;
  LONG window = 0L, spill, pixel = 0, nextint, bitnum, pixnum;
  static int bitdecode[16] = {0, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 
                              16, 32};

  while (pixel < total)
  { if (valids < 7) 
    { if (spillbits > 0)
      { window |= shift_left(spill, valids);
	valids += spillbits;
	spillbits = 0;}
      else
      { spill = (LONG) getc(packfile);
	spillbits = 8;}}
    else
    { pixnum = 1 << (window & setbits[3]);
      window = shift_right(window, 3);
      bitnum = bitdecode[window & setbits[4]];
      window = shift_right(window, 4);
      valids -= 7;
      while ((pixnum > 0) && (pixel < total))
      { if (valids < bitnum)
	{ if (spillbits > 0)
	  { window |= shift_left(spill, valids);
	    if ((32 - valids) > spillbits)
	    { valids += spillbits;
	      spillbits = 0;}
	    else
	    { usedbits = 32 - valids;
	      spill = shift_right(spill, usedbits);
	      spillbits -= usedbits;
	      valids = 32;}}
	  else
	  { spill = (LONG) getc(packfile);
	    spillbits = 8;}}
        else
	{ --pixnum;
	  if (bitnum == 0)
            nextint = 0;
	  else
	  { nextint = window & setbits[bitnum];
	    valids -= bitnum;
	    window = shift_right(window, bitnum);
	    if ((nextint & (1 << (bitnum - 1))) != 0)
	      nextint |= ~setbits[bitnum];}
 	  if (pixel > x)
	  { img[pixel] = (WORD) (nextint + 
				      (img[pixel-1] + img[pixel-x+1] + 
                                       img[pixel-x] + img[pixel-x-1] + 2) / 4);
	    ++pixel;}
	  else if (pixel != 0)
	  { img[pixel] = (WORD) (img[pixel - 1] + nextint);
	    ++pixel;}
	  else
	    img[pixel++] = (WORD) nextint;}}}}}




/******************************************************************************/

void unpack_long(FILE *packfile, int x, int y, LONG *img)

/* Unpacks a packed image into the LONG-array 'img'. The image is stored
   in 'packfile'. The file should be properly positioned: the first BYTE
   read is assumed to be the first BYTE of the packed image. */

{ int valids = 0, spillbits = 0, usedbits, total = x * y;
  LONG window = 0L, spill, pixel = 0, nextint, bitnum, pixnum;
  static int bitdecode[8] = {0, 4, 5, 6, 7, 8, 16, 32};

  while (pixel < total)
  { if (valids < 6) 
    { if (spillbits > 0)
      { window |= shift_left(spill, valids);
	valids += spillbits;
	spillbits = 0;}
      else
      { spill = (LONG) getc(packfile);
	spillbits = 8;}}
    else
    { pixnum = 1 << (window & setbits[3]);
      window = shift_right(window, 3);
      bitnum = bitdecode[window & setbits[3]];
      window = shift_right(window, 3);
      valids -= 6;
      while ((pixnum > 0) && (pixel < total))
      { if (valids < bitnum)
	{ if (spillbits > 0)
	  { window |= shift_left(spill, valids);
	    if ((32 - valids) > spillbits)
	    { valids += spillbits;
	      spillbits = 0;}
	    else
	    { usedbits = 32 - valids;
	      spill = shift_right(spill, usedbits);
	      spillbits -= usedbits;
	      valids = 32;}}
	  else
	  { spill = (LONG) getc(packfile);
	    spillbits = 8;}}
        else
	{ --pixnum;
	  if (bitnum == 0)
            nextint = 0;
	  else
	  { nextint = window & setbits[bitnum];
	    valids -= bitnum;
	    window = shift_right(window, bitnum);
	    if ((nextint & (1 << (bitnum - 1))) != 0)
	      nextint |= ~setbits[bitnum];}
	  if (pixel > x)
	  { img[pixel] = (LONG) (nextint + 
				      (img[pixel-1] + img[pixel-x+1] + 
                                       img[pixel-x] + img[pixel-x-1] + 2) / 4);
	    ++pixel;}
	  else if (pixel != 0)
	  { img[pixel] = (LONG) (img[pixel - 1] + nextint);
	    ++pixel;}
	  else
	    img[pixel++] = (LONG) nextint;}}}}}



/******************************************************************************/

void v2unpack_long(FILE *packfile, int x, int y, LONG *img)

/* Unpacks a packed image into the LONG-array 'img'. The image is stored
   in 'packfile'. The file should be properly positioned: the first BYTE
   read is assumed to be the first BYTE of the packed image. */

{ int valids = 0, spillbits = 0, usedbits, total = x * y;
  LONG window = 0L, spill, pixel = 0, nextint, bitnum, pixnum;
  static int bitdecode[16] = {0, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 
                              16, 32};

  while (pixel < total)
  { if (valids < 7) 
    { if (spillbits > 0)
      { window |= shift_left(spill, valids);
	valids += spillbits;
	spillbits = 0;}
      else
      { spill = (LONG) getc(packfile);
	spillbits = 8;}}
    else
    { pixnum = 1 << (window & setbits[3]);
      window = shift_right(window, 3);
      bitnum = bitdecode[window & setbits[4]];
      window = shift_right(window, 4);
      valids -= 7;
      while ((pixnum > 0) && (pixel < total))
      { if (valids < bitnum)
	{ if (spillbits > 0)
	  { window |= shift_left(spill, valids);
	    if ((32 - valids) > spillbits)
	    { valids += spillbits;
	      spillbits = 0;}
	    else
	    { usedbits = 32 - valids;
	      spill = shift_right(spill, usedbits);
	      spillbits -= usedbits;
	      valids = 32;}}
	  else
	  { spill = (LONG) getc(packfile);
	    spillbits = 8;}}
        else
	{ --pixnum;
	  if (bitnum == 0)
            nextint = 0;
	  else
	  { nextint = window & setbits[bitnum];
	    valids -= bitnum;
	    window = shift_right(window, bitnum);
	    if ((nextint & (1 << (bitnum - 1))) != 0)
	      nextint |= ~setbits[bitnum];}
	  if (pixel > x)
	  { img[pixel] = (LONG) (nextint + 
				      (img[pixel-1] + img[pixel-x+1] + 
                                       img[pixel-x] + img[pixel-x-1] + 2) / 4);
	    ++pixel;}
	  else if (pixel != 0)
	  { img[pixel] = (LONG) (img[pixel - 1] + nextint);
	    ++pixel;}
	  else
	    img[pixel++] = (LONG) nextint;}}}}}



/******************************************************************************/

char *long_to_char(LONG *lng, char *string)
/* Shrinks an array of LONGs into an array of chars, used in order to translate 
   an encoded string array passed by fortran into a c-type string. Returns
   'string'. */

{ char *s = string;
  int i;

  do
    *(s++) = (char) *lng;
  while (*(lng++) != 0);
  return(string);} 



/******************************************************************************/

void imsiz_c(char *filename, LONG *x, LONG *y)

/* Determines the size of the the packed image "filename" after unpacking. The
   dimensions are returned in x and y. */

{ FILE *packfile;
  int i = 0, c = 0, version = 0;
  char header[BUFSIZ];

  packfile = fopen(filename, "r");
  header[0] = '\n';
  header[1] = 0;
  *x = *y = 0;
  if (packfile != NULL)
  { while ((c != EOF) && ((*x == 0) || (*y == 0)))
    { c = i = *x = *y = 0;
      while ((++i < BUFSIZ) && (c != EOF) && (c != '\n') && (*x==0) && (*y==0)) 
        if ((header[i] = c = getc(packfile)) == '\n')
        { if (sscanf(header, PACKIDENTIFIER, x, y) == 2)
            version = 1;
          else if (sscanf(header, V2IDENTIFIER, x, y) == 2)
            version = 2;}}}
  fclose(packfile);}



/******************************************************************************/

void imsiz_f(LONG *filename, LONG *x, LONG *y)

/* Fortran frontend of imsiz_c. Because the way in which fortran
   passes strings is not defined, it passes the filename in which the
   packed image should be stored as an array of LONGs. */

{ char c_filename[1024];

  imsiz_c(long_to_char(filename, c_filename), x, y);}




/******************************************************************************/

void mirror_wordimg(WORD *img, LONG *x, LONG  *y)

/* Replaces img with its mirror by interchanging rows. 'x' is the fast index,
   'y' is the slow index. */

{ WORD *buff;
  int i, j;

  buff = (WORD *)malloc(sizeof(WORD) * *x);
  for (i = 0, j = *y - 1; i < j; ++i, --j)
  { memcpy(buff, img + (i * *x), sizeof(WORD) * *x);
    memcpy(img + (i * *x), img + (j * *x), sizeof(WORD) * *x);
    memcpy(img + (j * *x), buff, sizeof(WORD) * *x);}
  free((void *) buff);}



/******************************************************************************/

void mirror_longimg(LONG *img, LONG *x, LONG  *y)

/* Replaces img with its mirror by interchanging rows. 'x' is the fast index,
   'y' is the slow index. */

{ LONG *buff;
  int i, j;

  buff = (LONG *)malloc(sizeof(LONG) * *x);
  for (i = 0, j = *y - 1; i < j; ++i, --j)
  { memcpy(buff, img + (i * *x), sizeof(LONG) * *x);
    memcpy(img + (i * *x), img + (j * *x), sizeof(LONG) * *x);
    memcpy(img + (j * *x), buff, sizeof(LONG) * *x);}
  free((void *) buff);}



/******************************************************************************/



