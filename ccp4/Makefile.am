INCLUDES = $(MMDB_CXXFLAGS) $(CCIF_CXXFLAGS)

AM_LDFLAGS = $(MMDB_LIBS) $(CCIF_LIBS)

libccp4c_la_SOURCES = \
ccp4_array.c cmap_data.c cmtzlib.c ccp4_general.c   \
cmap_header.c csymlib.c ccp4_parser.c cmap_labels.c cvecmat.c            \
ccp4_program.c cmap_open.c library_err.c ccp4_unitcell.c cmap_skew.c     \
library_file.c cmap_accessor.c cmap_stats.c library_utils.c cmap_close.c \
cmap_symop.c pack_c.c

library_includedir=$(pkgincludedir)

library_include_HEADERS = \
ccp4_file_err.h  ccp4_program.h   ccp4_unitcell.h  cmap_errno.h \
cmap_stats.h     csymlib.h        library_file.h   w32mvs.h \
ccp4_fortran.h   ccp4_spg.h       ccp4_utils.h     cmap_header.h \
cmaplib.h        cvecmat.h        mtzdata.h        ccp4_array.h \
ccp4_general.h   ccp4_sysdep.h    ccp4_vars.h      cmap_labels.h \
cmaplib_f.h      overview.h       ccp4_errno.h     ccp4_parser.h \
ccp4_types.h     cmap_data.h      cmap_skew.h      cmtzlib.h \
library_f.h

lib_LTLIBRARIES = libccp4c.la

libccp4c_la_LDFLAGS = -version-info 6:0:1
