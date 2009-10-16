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
AC_PROG_F77(_AC_F95_FC)

cc_basename=`$ECHO "X$compiler_F77" | $Xsed -e 's%.*/%%' -e "s%^$host_alias-%%"`
case $host_os in
  linux* )
    case $cc_basename,$host_cpu in    
      ifort*)
        M4=${M4:-"m4 -D_ifc=8"} ;;
      ifc*)
        M4=${M4:-"m4 -D_efc=1"} ;;
      g77*)
        M4=${M4:-"m4 -D_g77=1"} ;;
      gfortran*)
        M4=${M4-"m4 -D_gfort=1"} ;;
      xlf*)
        M4=${M4-"m4 -D_xlf=1"} ;;
      pgf*) ;;
      esac
  ;;
  darwin* )
    case $cc_basename,$host_cpu in   
      ifort*)
        M4=${M4:-"m4 -D_ifc=8"} ;;
      ifc*)
        M4=${M4:-"m4 -D_efc=1"} ;;
      g77*)
        M4=${M4:-"m4 -D_g77=1"} ;;
      gfortran*)
        M4=${M4-"m4 -D_gfort=1"} ;;
      xlf*)
        M4=${M4-"m4 -D_xlf=1"} ;;
      esac
esac

AC_SUBST(M4)

AC_OUTPUT(libccp4c.pc libccp4f.pc Makefile ccp4/Makefile data/Makefile)
