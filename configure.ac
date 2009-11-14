# Process this with autoconf to produce a configure script

dnl package-name version-number bugs-to
AC_INIT(ccp4, 6.1, ccp4@ccp4.ac.uk)

dnl package-name version-number, the gzip'd tar file is named
dnl package-version.tar.gz
AM_INIT_AUTOMAKE(ccp4, 6.1)

AC_CONFIG_SRCDIR([ccp4/cmtzlib.c])
AM_CONFIG_HEADER(config.h)

AC_PROG_LIBTOOL

AC_PROG_CC

AC_ARG_ENABLE(fortran, AS_HELP_STRING([--enable-fortran],[compile fortran API]),
[fortran_api=$enableval], [fortran_api=yes])

AM_CONDITIONAL([FORTRAN_LIB], [ test "x$fortran_api" = xyes ])

AS_IF([test "x$fortran_api" = xyes],[
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
])

AC_SUBST(M4)

AC_OUTPUT(libccp4c.pc libccp4f.pc Makefile ccp4/Makefile fortran/Makefile data/Makefile)
