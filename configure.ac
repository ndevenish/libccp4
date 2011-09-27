# Process this with autoconf to produce a configure script

dnl package-name version-number bugs-to
AC_INIT(ccp4, 6.1, ccp4@ccp4.ac.uk)


AC_CONFIG_SRCDIR([ccp4/cmtzlib.c])
AM_CONFIG_HEADER(config.h)
AC_CONFIG_MACRO_DIR([macros])
AM_INIT_AUTOMAKE

AC_PROG_LIBTOOL

AC_PROG_CC
AC_PROG_CXX


AC_ARG_ENABLE(fortran, AS_HELP_STRING([--enable-fortran],[compile fortran API]),
[fortran_api=$enableval], [fortran_api=yes])

AC_ARG_ENABLE(ccif, AS_HELP_STRING([--enable-ccif],[compile ccif API for fortran]),
[ccif_api=$enableval], [ccif_api=yes])

AC_ARG_WITH(ccif,
  AC_HELP_STRING( [--with-ccif=PFX], [use ccif library (default NO) and set prefix] ),
  [
    ccif_api=yes
    test "$withval" = yes || ccif_prefix="$withval" ],
  [ with_ccif="$ccif_api" ] )

AM_CONDITIONAL([FORTRAN_LIB], [ test "x$fortran_api" = xyes ])
AM_CONDITIONAL([CCIF_API], [ test "x$ccif_api" = xyes ])

if test "x$fortran_api" = xyes; then
  AC_PROG_F77(_AC_F95_FC)
  cc_basename=`$ECHO "X$compiler_F77" | $Xsed -e 's%.*/%%' -e "s%^$host_alias-%%"`
  case $host_os in
    linux* )
      case $cc_basename,$host_cpu in    
        ifort*)
          M4=${M4:-"m4 -D_ifc=8"}
          AC_DEFINE([IFC],[8],[using intel fortran compiler])
          ;;
        ifc*)
          M4=${M4:-"m4 -D_efc=1"}
          AC_DEFINE([IFC],[1],[using intel fortran compiler])
          ;;
        g77*)
          M4=${M4:-"m4 -D_g77=1"}
          AC_DEFINE([G77],[1],[using g77 compiler])
          ;;
        gfortran*)
          M4=${M4-"m4 -D_gfort=1"} 
          AC_DEFINE([GFORTRAN],[1],[using gfortran compiler])
          ;;
        xlf*)
          M4=${M4-"m4 -D_xlf=1"} ;;
        pgf*) ;;
        esac
    ;;
    darwin* )
      case $cc_basename,$host_cpu in   
        ifort*)
          M4=${M4:-"m4 -D_ifc=8"}
          AC_DEFINE([IFC],[8],[using intel fortran compiler])
          ;;
        ifc*)
          M4=${M4:-"m4 -D_ifc=1"}
          AC_DEFINE([IFC],[1],[using intel fortran compiler])
          ;;
        g77*)
          M4=${M4:-"m4 -D_g77=1"}
          AC_DEFINE([G77],[1],[using g77 compiler])
          ;;
        gfortran*)
          M4=${M4-"m4 -D_gfort=1"}
          AC_DEFINE([GFORTRAN],[1],[using gfortran compiler])
          ;;
        xlf*)
          M4=${M4-"m4 -D_xlf=1"} ;;
        esac
    ;;
    irix* )
      case $cc_basename,$host_cpu in
        f77* )
          M4=${M4:-"m4 -D_sgi=1"}
          ;;
        f90*)
          M4=${M4:-"m4 -D_sgi=1 -D_f90=1"}
          ;;
        esac 
  esac
  
  AM_PATH_MMDB(,
    [ if test "x$mmdb_prefix" != x ; then
        if test "x$ac_MMDB_CXXFLAGS" != x ; then
          case "$ac_MMDB_CXXFLAGS" in
          *src/mmdb )
          ac_MMDB_LDOPTS=`echo "$ac_MMDB_CXXFLAGS" | sed s#src/mmdb#src#g`
          ;;
          *include/mmdb )
          ac_MMDB_LDOPTS=`echo "$ac_MMDB_CXXFLAGS" | sed s#include/mmdb#lib#g`
          ;;
          *)
          esac
          MMDB_CXXFLAGS="$ac_MMDB_CXXFLAGS"
          MMDB_LIBS=`echo "$ac_MMDB_LDOPTS -lmmdb" | sed s#-I#-L#`
          AC_MSG_WARN([assuming library $MMDB_LIBS headers $MMDB_CXXFLAGS])
          have_mmdb=yes
        else
          MMDB_CXXFLAGS="-I$mmdb_prefix/src -I$mmdb_prefix/include"
          MMDB_LIBS="-L$mmdb_prefix/src -L$mmdb_prefix/lib -lmmdb"
          AC_MSG_WARN([assuming library $MMDB_LIBS headers $MMDB_CXXFLAGS])
          have_mmdb=yes
        fi
      else
         AC_MSG_ERROR([mmdb not found])
      fi
    ]
  )
  
  if test x"$ccif_api" = xyes ; then
  AM_PATH_CCIF(,
    [ if test "x$ccif_prefix" != x ; then
        if test "x$ac_CCIF_CXXFLAGS" != x ; then
          case "$ac_CCIF_CXXFLAGS" in
          *include/ccif )
          ac_CCIF_LDOPTS=`echo "$ac_CCIF_CXXFLAGS" | sed s#include/ccif#lib#g`
          ;;
          *)
          esac
          CCIF_CXXFLAGS="$ac_CCIF_CXXFLAGS"
          CCIF_LIBS=`echo "$ac_CCIF_LDOPTS -lccif" | sed s#-I#-L#`
          AC_MSG_WARN([assuming library $CCIF_LIBS headers $CCIF_CXXFLAGS])
          have_ccif=yes
        else
          CCIF_CXXFLAGS="-I$ccif_prefix/src -I$ccif_prefix/include"
          CCIF_LIBS="-L$ccif_prefix/ccif -L$ccif_prefix/lib -lccif"
          AC_MSG_WARN([assuming library $CCIF_LIBS headers $CCIF_CXXFLAGS])
          have_ccif=yes
        fi
      else
       AC_MSG_ERROR([ccif not found])
      fi
    ]
  )
  fi
  
  fi #dnl fortran API elements

PACKAGE_ROOT=$prefix
test "x$PACKAGE_ROOT" = xNONE && PACKAGE_ROOT=$ac_default_prefix
AC_DEFINE_UNQUOTED([PACKAGE_ROOT],["$PACKAGE_ROOT"],[packge install directory])

AC_SUBST(M4)

AC_OUTPUT(libccp4c.pc libccp4f.pc Makefile ccp4/Makefile fortran/Makefile data/Makefile)
