#include <curses.h> 

#if defined (hewlett_packard) || defined (aix)
  void ucurse (nlines, ncols)
#endif

#if defined (ardent) || defined (titan) || defined (stardent)
  void UCURSE (nlines, ncols)
#endif

#if defined (__convex__) || defined (ultrix)  || defined (sgi) || \
    defined (ESV)        || defined (alliant) || defined (sun) || \
    defined (solbourne)
  void ucurse_ (nlines, ncols)
#endif

#if defined (iris)
  fortran ucurse_ (nlines, ncols)
#endif

int *nlines, *ncols;
{
  initscr();  /* initialise terminal settings and <curses.h> 
                 data structures and variables */
  *nlines = LINES;
  *ncols = COLS;

   refresh();
   endwin();
} /* End of ucurse */
