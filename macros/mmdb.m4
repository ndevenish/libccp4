
# Note that mmdb as distributed by Eugene does not install and does
# conform to normal unix standard notions of the behaviour of include 
# and libraries.
#
# So we will depend on you doing something by hand, and that is to link
# (or copy) mmdb.a to libmmdb.a (and that it is in the top directory of 
# mmdb).
#

 
# AM_PATH_MMDB([ACTION-IF-FOUND [,ACTION-IF-NOT-FOUND]])
# ----------------------------------------------------------
# set up for MMDB
#
AC_DEFUN([AM_PATH_MMDB],
[
AC_PROVIDE([AM_PATH_MMDB])

AC_ARG_WITH(mmdb,
  AC_HELP_STRING( [--with-mmdb=PFX], [use mmdb library and set prefix] ),
  [
    test "$withval" = no || mmdb_prefix="" 
    test "$withval" = yes || mmdb_prefix="$withval" ],
  [ mmdb_prefix="" ] ) 

#user override
AS_IF([test "x$MMDB_LIBS" != x && test "x$MMDB_CXXFLAGS" != x ],
[
  ac_MMDB_LDOPTS=$MMDB_LIBS
  ac_MMDB_CXXFLAGS=$MMDB_CXXFLAGS
  have_mmdb=yes
],
[
saved_LIBS="$LIBS"
saved_CXXFLAGS="$CXXFLAGS"
MMDB_CXXFLAGS=""
MMDB_LIBS=""

if test x$mmdb_prefix != x; then
	# very likely the majority of cases, we will try to configure with:
	# --with-mmdb=/some/thing
	#


 # should ideally be MMDB_CXXFLAGS="-I$MMDB_prefix/include", and the like
 # when MMDB and dependencies get installed
 #  
ac_mmdb_dirs='
.
include
lib
src
lib/src
lib/src/mmdb'
for ac_dir in $ac_mmdb_dirs; do
  if test -r "$mmdb_prefix/$ac_dir/mmdb/mmdb_manager.h"; then
    ac_MMDB_CXXFLAGS="-I$mmdb_prefix/$ac_dir"
    break
    fi
  done
 #
 # SGI compiler CC (CXX=CC) needs -lm to link maths library, but 
 # GCC c++ does not.
 #
for ac_dir in $ac_mmdb_dirs; do
  for ac_extension in a so sl dylib; do
  if test -r "$mmdb_prefix/$ac_dir/libmmdb.$ac_extension"; then
    ac_MMDB_LDOPTS="-L$mmdb_prefix/$ac_dir -lmmdb"
    break 2
    fi
  done
  done

else
 # the compiler looks in the "standard" places for MMDB.  In real life,
 # it would be quite unlikely that MMDB would be installed in /usr/include, 
 # /usr/lib etc. so this code will not usually find the right dependencies.
 ac_MMDB_CXXFLAGS=""
 ac_MMDB_LDOPTS=""
fi

AC_MSG_CHECKING([for CMMDBManager in MMDB])
	LIBS="$ac_MMDB_LDOPTS $saved_LIBS"
	CXXFLAGS="$ac_MMDB_CXXFLAGS $saved_CXXFLAGS"
	#
	# AC_TRY_LINK uses the c compiler (set by AC_LANG), so we will
	# temporarily reassign $CC to the c++ compiler.
 	#
        AC_LANG_PUSH(C++)
	AC_TRY_LINK([#include "mmdb/mmdb_manager.h"] ,[ CMMDBManager a;  ], have_mmdb=yes, have_mmdb=no)
        AC_LANG_POP(C++)

 LIBS="$saved_LIBS"
 CXXFLAGS="$saved_CXXFLAGS"

]) # user override

AS_IF([test "x$have_mmdb" = xyes],
  [  test "x$MMDB_CXXFLAGS" = x && MMDB_CXXFLAGS=$ac_MMDB_CXXFLAGS
     test "x$MMDB_LIBS" = x && MMDB_LIBS=$ac_MMDB_LDOPTS
     AC_MSG_RESULT($have_mmdb)
     ifelse([$1], , :, [$1])],
  [  AC_MSG_RESULT($have_mmdb)
     ifelse([$2], , :, [$2])] )

AC_SUBST(MMDB_CXXFLAGS)
AC_SUBST(MMDB_LIBS)
 
])
