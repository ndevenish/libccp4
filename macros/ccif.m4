
# Note that ccif as distributed by Eugene does not install and does
# conform to normal unix standard notions of the behaviour of include 
# and libraries.
#
# So we will depend on you doing something by hand, and that is to link
# (or copy) ccif.a to libccif.a (and that it is in the top directory of 
# ccif).
#

 
# AM_PATH_CCIF([ACTION-IF-FOUND [,ACTION-IF-NOT-FOUND]])
# ----------------------------------------------------------
# set up for CCIF
#
AC_DEFUN([AM_PATH_CCIF],
[
AC_PROVIDE([AM_PATH_CCIF])

AC_ARG_WITH(ccif,
  AC_HELP_STRING( [--with-ccif=PFX], [use ccif library (default NO) and set prefix] ),
  [
    test "$withval" = no || with_ccif=yes 
    test "$withval" = yes || ccif_prefix="$withval" ],
  [ with_ccif="$enable_ccif" 
  ] ) 

if test x$with_ccif = xyes ; then
#user override
AS_IF([test "x$CCIF_LIBS" != x && test "x$CCIF_CXXFLAGS" != x ],
[
  have_ccif=yes
],
[
saved_LIBS="$LIBS"
saved_CXXFLAGS="$CXXFLAGS"
CCIF_CXXFLAGS=""
CCIF_LIBS=""

if test x$ccif_prefix != x; then
	# very likely the majority of cases, we will try to configure with:
	# --with-ccif=/some/thing
	#


 # should ideally be CCIF_CXXFLAGS="-I$ccif_prefix/include", and the like
 # when CCIF and dependencies get installed
 #  
ac_ccif_dirs='
.
include
lib
src
lib/ccif'
for ac_dir in $ac_ccif_dirs; do
  if test -r "$ccif_prefix/$ac_dir/ccif/sym.h"; then
    ac_CCIF_CXXFLAGS="-I$ccif_prefix/$ac_dir"
    break
    fi
  done
 #
 # SGI compiler CC (CXX=CC) needs -lm to link maths library, but 
 # GCC c++ does not.
 #
for ac_dir in $ac_ccif_dirs; do
  for ac_extension in a so sl dylib; do
  if test -r "$ccif_prefix/$ac_dir/libccif.$ac_extension"; then
    ac_CCIF_LDOPTS="-L$ccif_prefix/$ac_dir -lccif"
    break 2
    fi
  done
  done

else
 # the compiler looks in the "standard" places for CCIF.  In real life,
 # it would be quite unlikely that CCIF would be installed in /usr/include, 
 # /usr/lib etc. so this code will not usually find the right dependencies.
 ac_CCIF_CXXFLAGS=""
 ac_CCIF_LDOPTS=""
fi

AC_MSG_CHECKING([for CCCIFManager in CCIF])
	LIBS="$ac_CCIF_LDOPTS $saved_LIBS"
	CXXFLAGS="$ac_CCIF_CXXFLAGS $saved_CXXFLAGS"
	#
	# AC_TRY_LINK uses the c compiler (set by AC_LANG), so we will
	# temporarily reassign $CC to the c++ compiler.
 	#
	AC_LANG_PUSH(C)
	AC_TRY_LINK([#include "ccif/sym.h"] ,[ zzs_scope();  ], have_ccif=yes, have_ccif=no)
	AC_LANG_POP(C)  # the language we have just quit

 LIBS="$saved_LIBS"
 CXXFLAGS="$saved_CXXFLAGS"

]) # user override

AS_IF([test "x$have_ccif" = xyes],
  [  test "x$CCIF_CXXFLAGS" = x && CCIF_CXXFLAGS=$ac_CCIF_CXXFLAGS
     test "x$CCIF_LIBS" = x && CCIF_LIBS=$ac_CCIF_LDOPTS
     AC_MSG_RESULT($have_ccif)
     ifelse([$1], , :, [$1])],
  [  AC_MSG_RESULT($have_ccif)
     ifelse([$2], , :, [$2])] )

fi #dnl --with-ccif 

AC_SUBST(CCIF_CXXFLAGS)
AC_SUBST(CCIF_LIBS)
 
])
