/****************************************************************************
 *                                                                          *
 * This file contains the lowest level routines for the CCP4 Program Suite. *
 * It contains code that is specific to both the VMS and Unix versions of   *
 * the software eg the set of C routines required by the disk input/output  *
 * routines in diskio.for                                                   *
 *                                                                          *
 * Routine  Arguments                      Purpose                          *
 *                                                                          *
 * ustenv   (string, result)             - set the environment variable (U) *
 * copen    (iunit,filnam,istat)         - fopen (open random access file)  *
 * qclose   (iunit)                      - fclose (shut random access file) *
 * qmode    (iunit,mode,nmcitm)          - change size of item in file ops  *
 * qread    (iunit,array,nitems,ier)     - fread from random access file    *
 * qwrite   (iunit,array,nitems)         - fwrite to random access file     *
 * qseek    (iunit,irec,iel,lrecl)       - fseek within random access file  *
 * qback    (iunit,lrecl)                - fseek within random access file  *  
 * qskip    (iunit,lrecl)                - fseek within random access file  *
 * cqinq    (iunit,lfilnm,filnam,length) - inquire file on the given stream *
 * qlocate  (iunit,locate)               - ftell within random access file  *
 * qtype    (istamp)                     - returns machine type stamp       *
 *                                                                          *
 * Comments, complaints, moans and updates to: ccp4@dl.ac.uk                *
 *                                                                          *
 * CCP4, 28th February 1992                                                 *
 *  $Date$
 *                                                                          *
 ****************************************************************************/

/* Library.h contains a lot of the machine dependent info. */
#include "library.h"

struct treenode *tree = NULL;   /* pointer to root node of tree structure */
struct treenode *current = NULL;    /* used to point to current tree node */
struct treenode *previous = NULL;  /* used to point to previous tree node */

int clevel = 0;                    /* used to indicate current tree depth */
int plevel = 0;                   /* used to indicate previous tree depth */

int SCREEN_WIDTH ;
int SCREEN_DEPTH ;



/****************************************************************************
 * Machine dependent and byte swapping parts; specific to chelp.            *
 ****************************************************************************/

#include <curses.h>

#ifdef CONVERT
#  include "machine.h"
#  include "dfconvert.h"
#  include "diconvert.h"
#endif

#if defined (VMS) || defined (alliant)
#  define DELFILE remove
#else
#  define DELFILE unlink
#endif

#if defined (iris)		/* DL: i find this hard to believe... */
#  define [ %
#  define ] %
#endif


#define MAX_LINE_LEN 512
#define DEF_OPTION     0
#define lcase(c) ((c >= 'A' && c <= 'Z') ? c - 'A' + 'a' : c)


/**************************************************************************
 * Structures                                                             *
 **************************************************************************/

struct block {                       /* definition of our data block .... */
  char *name;                       /* is header name to identify it then */
  struct block *lines;    /* a linked list of lines making up a paragraph */
};

struct list {                         /* definition of a linked list .... */
  struct treenode *child;                  /* is an item in the list then */
  struct list *next;                    /* a pointer to next item in list */
};

struct treenode {                       /* definition of a tree node .... */
  struct block *item;                    /* is a data item (a block) then */
  struct treenode *parent;          /* a pointer to the parent node and a */
  struct list *descendent;                     /* linked list of children */
};


/*************************************************************************
 * Prototype Internal Routines                                           *
 *************************************************************************/

void read_input_file(FILE *in);

void show_tree(FILE *in);

void process_file(FILE *in);

void manipulate_tree(struct treenode *tree);

void read_input_file (FILE *fptr);

void show_tree (FILE *fptr);

void process_file (FILE *fptr);

void insert_into_tree (int level, struct block *line);

int show_children (struct list *node);

void manipulate_tree (struct treenode *ptr);

void list_topics (struct list *ptr);

int get_response (struct list *ptr);

struct treenode *create_node (struct block *line);

struct list *join_node (struct treenode *ptr);




