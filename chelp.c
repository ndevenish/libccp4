/******************
 * Routine: chelp  
 ******************/

#include "chelp.h"



#if CALL_LIKE_HPUX
  void chelp ()
#endif

#if CALL_LIKE_STARDENT
  void CHELP ()
#endif

#if defined (VMS)
  void CHELP ()
#endif

#if CALL_LIKE_SUN
  void chelp_ ()
#endif

#if CALL_LIKE_IRIS
  fortran chelp_ ()
#endif


{
  char *filename ;
  int opt = DEF_OPTION;
#if defined (sgi) || defined (__OSF1__) || defined (__osf__)
  int setlinebuf(FILE *);
#endif
  FILE *in;

          filename = getenv ("CHELPFILE");

  initscr();  /* initialise terminal settings and <curses.h> 
                            data structures and variables */
  SCREEN_WIDTH = COLS;
  SCREEN_DEPTH = LINES - 3;
  tree=NULL;

   refresh();
   endwin();

      if ((in = fopen (filename,"r")) == NULL)
  {(void) fprintf (stderr,"cannot open %s\n",filename);
          exit (2);
   }

#if defined (sgi) || defined (__OSF1__) || defined (__osf__)
  setlinebuf(stdout);
#endif

  switch (opt) {
   case 0: read_input_file (in);
           break;
   case 1: show_tree (in);
           exit (0);
           break;
   case 2: process_file (in);
           exit (0);
           break;
                    }

  (void) fclose (in);

  manipulate_tree (tree);

   } /* end of main */


/**************************************************************************/

void read_input_file (fptr)     /* read the input file into the tree structure */
FILE *fptr;
{
  char line[MAX_LINE_LEN];
  struct block *item, *clist, *flist;
  int level, c;

  while (!feof (fptr))
    {
      c = getc (fptr);
      while (c == ' ' || c == '\t' || c == '\n') c = getc (fptr);
      (void) ungetc (c,fptr);
      if (c >= '0' && c <= '9')
        {
          (void) fscanf (fptr,"%[^\n]s",line);
          item = (struct block *) malloc ((unsigned) sizeof (struct block));
          item->name = malloc ((unsigned) (1 + strlen (line)));
          (void) sscanf (line,"%d %[^\n]s",&level,item->name);
          item->lines = NULL;
          clist = item;
          flist = item;
          while (!feof (fptr))
            {
              c = getc (fptr);
              while (c == '\n') c = getc (fptr);
              (void) ungetc (c,fptr);
              if (c >= '0' && c <= '9') break;
              (void) fscanf (fptr,"%[^\n]s",line);
              item = (struct block *) malloc ((unsigned) sizeof (struct block));
              item->name = malloc ((unsigned) (1 + strlen (line)));
              (void) strcpy (item->name,line);
              item->lines = NULL;
              clist->lines = item;
              clist = item;
            }
          insert_into_tree (level,flist);
        }
      else
        {
          (void) printf ("lines out of order\n");
          exit (3);
        }
    } /* end while */
} /* end of read_input_file */

#define XLIN      10

void show_tree (fptr)
FILE *fptr;
{
  int c, xlins = 1, sp;

  while ((c = getc (fptr)) != EOF) {
    if (c == XLIN)
      xlins = 1;
    else if (c >= '0' && c <= '9')
      {  
       if (xlins) 
         {
           xlins = 0;
           for (sp = 1; sp <= c-'0'; sp++)
            {
#if defined (sgi)
   putc (' ', stderr); 
   putc (' ', stderr); 
#else
   putc (' ', stdout); 
   putc (' ', stdout); 
#endif
             }
           while (c != XLIN) {
#if defined (sgi)
   putc (c, stderr); 
#else
   putc (c, stdout); 
#endif
             c = getc (fptr);
           }
#if defined (sgi)
   putc (XLIN, stderr); 
#else
   putc (XLIN, stdout); 
#endif
           ungetc (c, fptr);
         }
      }
     else 
      {
       while (c != XLIN)
         c = getc (fptr);
       ungetc (c, fptr);
      }
   }
} /* End of show_tree */

/**************************************************************************/

void process_file (fptr)
FILE *fptr;
{
  int c, xlins = 1;

  while ((c = getc (fptr)) != EOF) {
    if (c == XLIN)
      xlins = 1;
    else if (c >= '1' && c <= '9')
      {  
       if (xlins) xlins = 0;
      }
    else 
     { 
      if (xlins) 
        {
#if defined (sgi)
   putc (' ', stderr); 
#else
   putc (' ', stdout); 
#endif
         xlins = 0;
        }
     }
#if defined (sgi)
   putc (c, stderr); 
#else
   putc (c, stdout); 
#endif
   }
} /* End of process_file */

/**************************************************************************
 * Level 2                                                                *
 **************************************************************************/

void insert_into_tree (level,line)            /* insert the item into the tree */
int level;                 /* level is used to indicate depth in tree */
struct block *line;
{
  struct treenode *temp, *tree;

  if (tree == NULL && level == 0) 
    {
      tree = create_node (line); 
      tree->parent = tree;
      previous = tree;
      current = tree;
    }
  else if (tree != NULL && level == 0)
    {
      (void) printf ("only one level zero allowed\n");
      exit (4);
    }
  else if (tree == NULL && level != 0)
    {
      (void) printf ("level zero not implemented yet\n");
      exit (5);
    }
  else
    {
      previous = current;
      plevel = clevel;
      if (level == clevel)              /* node at same level as last one */
        current = current->parent;
      else if (level <= clevel)                    /* node above last one */
        {
          clevel = plevel - level + 1;
          while (clevel--) current = current->parent;
        }
      temp = create_node (line);          /* now at correct point in tree */
      current->descendent = join_node (temp);
      current = temp;
      clevel = level;
    }
} /* end of insert_into_tree */

/**************************************************************************/

int show_children (node)
struct list *node;
{
  if (node == NULL) return (0);
  while (node != 0) 
    {

      (void) printf ("\t%s",node->child->item->name);

      node = node->next;
    }
  return (0);
} /* end of show_children */

/**************************************************************************/

struct treenode *create_node (line)           /* create new node for tree */
struct block *line;
{
  struct treenode *ptr;

  ptr = (struct treenode *) malloc ((unsigned) sizeof (struct treenode));
  ptr->item = line;
  ptr->parent = current;
  ptr->descendent = NULL;

  return (ptr);
} /* end of create_node */

/**************************************************************************/

struct list *join_node (ptr)    /* join the tree node to its parents list */
struct treenode *ptr;
{ 
  struct list *newnode;

  newnode = (struct list *) malloc (sizeof (struct list));
  newnode->next = current->descendent;
  newnode->child = ptr;

  return (newnode);
} /* end of join_node */

/**************************************************************************/

void manipulate_tree (ptr)
struct treenode *ptr;
{
  struct block *info;
  int pause;

  do {
#if ! defined (VMS)
    (void) system ("clear");
#endif
    info = ptr->item;

    (void) printf ("%s\n\n",info->name);

    pause = 0;
    while (info->lines != NULL)
      {
        info = info->lines;

   (void) printf ("%s\n",info->name);

        if ((++pause) % SCREEN_DEPTH == 0) 
          {

   (void) printf ("Press RETURN to continue ... ");

            do {
            } while (getc (stdin) != '\n');
          }
      }
    list_topics (ptr->descendent);
  } while  (get_response (ptr->descendent) != 0);

} /* end of manipulate_tree */

/**************************************************************************
 * Level 3                                                                *
 **************************************************************************/

void list_topics (ptr)
struct list *ptr;

{
  struct list *current;
  int linelen, columns, rows, maxline = 0, numlines = 0, result;

  current = ptr;
  while (current != NULL)
    {
      linelen = strlen (current->child->item->name);
      if (linelen > maxline) maxline = linelen;
      numlines++;
      current = current->next;
    }

  result = numlines;
  maxline += 1;                   /* put 1 spaces between each option */


  columns = (SCREEN_WIDTH / maxline) - 1;  
  rows = numlines / columns;

  if (result) 
{
 (void) printf ("\nFurther Information on:\n\n");
}

  if ((current = ptr) != NULL) rows++;
  for (numlines = 1; numlines <= rows; numlines++)
   {
     for (linelen = 1; linelen <= columns; linelen++) 
       {
         if (current == NULL) break;

  (void) printf ("%-*s  ",maxline,current->child->item->name);

         current = current->next;
       }

 (void) printf ("\n"); 

   }

} /* end of list_topics */

/**************************************************************************/

int get_response (ptr)
struct list *ptr;
{
  char reply[MAX_LINE_LEN];
  int i;
  char *temp, *start;

  (void) printf ("\n\nEnter Choice: ");

  i =  scanf ("%[^\n]s",reply);
  (void) getc (stdin);                 /* skip over the newline character */
  if (i == 0) return (0);     /* no response so return to calling routine */
  for (i = 0; i < strlen (reply); i++) 
    reply[i] = lcase (reply[i]);

  while (ptr != NULL)
    {
      temp = malloc ((unsigned) (1 + strlen (reply)));
      (void) strncpy (temp,ptr->child->item->name,strlen (reply));
      for (start = temp; *temp != '\0'; temp++) *temp = lcase (*temp);
      i = strncmp (reply,start,strlen (reply));
      if (i == 0) 
        {
          (void) manipulate_tree (ptr->child);
          return (1);
        }
      free (start);
      ptr = ptr->next;
    }

  return (0);
} /* end of get_response */
