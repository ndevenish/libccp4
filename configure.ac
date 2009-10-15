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

AC_OUTPUT(libccp4c.pc libccp4f.pc Makefile ccp4/Makefile data/Makefile)
