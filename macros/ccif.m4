
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

AC_MSG_CHECKING([libccif])

#user override
AS_IF([test "x$CCIF_LIBS" != x && test "x$CCIF_CXXFLAGS" != x ],
[
  have_ccif=yes
],
[
saved_LIBS="$LIBS"
saved_CFLAGS="$CFLAGS"
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
lib'
for ac_dir in $ac_ccif_dirs; do
  if test -r "$ccif_prefix/$ac_dir/ccif/sym.h"; then
    ac_CCIF_CXXFLAGS="-I$ccif_prefix/$ac_dir -I$ccif_prefix/$ac_dir/ccif"
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
  
  test x"$ac_CCIF_CXXFLAGS" != x && have_ccif=yes
  test x"$ac_CCIF_LDOPTS" != x && have_ccif=yes
else
 # the compiler looks in the "standard" places for CCIF.  In real life,
 # it would be quite unlikely that CCIF would be installed in /usr/include, 
 # /usr/lib etc. so this code will not usually find the right dependencies.
 ac_CCIF_CXXFLAGS=""
 ac_CCIF_LDOPTS=""
fi

]) # user override

AS_IF([test "x$have_ccif" = xyes],
  [  test "x$CCIF_CXXFLAGS" = x && CCIF_CXXFLAGS=$ac_CCIF_CXXFLAGS
     test "x$CCIF_LIBS" = x && CCIF_LIBS=$ac_CCIF_LDOPTS
     AC_MSG_RESULT($have_ccif)
     ifelse([$1], , :, [$1])],
  [  AC_MSG_RESULT($have_ccif)
     ifelse([$2], , :, [$2])] )

AC_SUBST(CCIF_CXXFLAGS)
AC_SUBST(CCIF_LIBS)
 
])
