ACLOCAL_AMFLAGS = -I m4

lib_LTLIBRARIES = ccp4/libccp4c.la
if FORTRAN_LIB
lib_LTLIBRARIES += fortran/libccp4f.la
endif

if GNU_FORTRAN
AM_FFLAGS = -fno-second-underscore
endif

ccp4datadir = $(datadir)/ccp4
ccp4includedir = $(includedir)/ccp4

ccp4_libccp4c_la_SOURCES = \
ccp4/ccp4_array.c ccp4/cmap_data.c ccp4/cmtzlib.c ccp4/ccp4_general.c \
ccp4/cmap_header.c ccp4/csymlib.c ccp4/ccp4_parser.c ccp4/cmap_labels.c \
ccp4/cvecmat.c ccp4/ccp4_program.c ccp4/cmap_open.c ccp4/library_err.c \
ccp4/ccp4_unitcell.c ccp4/cmap_skew.c ccp4/library_file.c \
ccp4/cmap_accessor.c ccp4/cmap_stats.c ccp4/library_utils.c ccp4/cmap_close.c \
ccp4/cmap_symop.c ccp4/pack_c.c

# FIXME: is ccp4/w32mvs.h needed here?
#        is cmaplib_f.h a public header?
ccp4include_HEADERS = \
ccp4/ccp4_file_err.h  ccp4/ccp4_program.h   ccp4/ccp4_unitcell.h  \
ccp4/cmap_errno.h     ccp4/cmap_stats.h     ccp4/csymlib.h        \
ccp4/library_file.h   ccp4/w32mvs.h         ccp4/ccp4_fortran.h   \
ccp4/ccp4_spg.h       ccp4/ccp4_utils.h     ccp4/cmap_header.h    \
ccp4/cmaplib.h        ccp4/cvecmat.h        ccp4/mtzdata.h        \
ccp4/ccp4_array.h     ccp4/ccp4_general.h   ccp4/ccp4_sysdep.h    \
ccp4/ccp4_vars.h      ccp4/cmap_labels.h    ccp4/cmaplib_f.h      \
ccp4/overview.h       ccp4/ccp4_errno.h     ccp4/ccp4_parser.h    \
ccp4/ccp4_types.h     ccp4/cmap_data.h      ccp4/cmap_skew.h      \
ccp4/cmtzlib.h        ccp4/pack_c.h


fortran_libccp4f_la_SOURCES = \
fortran/ccp4_diskio_f.c fortran/ccp4_parser_f.c fortran/cmaplib_f.c \
fortran/csymlib_f.c fortran/ccp4_general_f.c fortran/ccp4_unitcell_f.c \
fortran/cmtzlib_f.c fortran/library_f.c \
 \
fortran/ccplib.f fortran/fftlib.f fortran/modlib.f fortran/parser.f \
fortran/rwbrook.f fortran/libhtml.f fortran/keyparse.f \
fortran/dna_output.f fortran/pack_f.f fortran/pxxml.f fortran/lgglib.f \
fortran/plot84lib.f fortran/plotsubs.f \
fortran/cciflib.fh  fortran/dna_header.fh \
fortran/harderiv.inc  fortran/harsymm.inc \
fortran/harshell.inc  fortran/harvest.inc

# there are two implementations of SRTBEG/SRTRLS/SRTMRG/SRTRET functions
# the sorting_main.f is probably better, but needs to be reviewed and tested
# before we switch to it on all platforms.
if HAVE_FORK
fortran_libccp4f_la_SOURCES += fortran/binsortint.c fortran/binsort.h
else
fortran_libccp4f_la_SOURCES += fortran/sorting_main.f
endif

nodist_fortran_libccp4f_la_SOURCES = fortran/unix.f

fortran_libccp4f_la_CPPFLAGS = -I$(srcdir)/ccp4
fortran_libccp4f_la_LIBADD = ccp4/libccp4c.la -lmmdb
if CCIF_API
  fortran_libccp4f_la_SOURCES += fortran/harvlib.f fortran/cciflib.f
  fortran_libccp4f_la_LIBADD += -lccif
else
  fortran_libccp4f_la_SOURCES += fortran/harv_dummy.f
endif

BUILT_SOURCES = fortran/unix.f
CLEANFILES = fortran/unix.f
fortran/unix.f: fortran/unix.m4 Makefile
	m4 $(M4_ARG) $< > $@

ccp4_libccp4c_la_LDFLAGS = -no-undefined
fortran_libccp4f_la_LDFLAGS = -no-undefined

dist_data_DATA = data/atomsf_neutron.lib data/atomsf.lib \
                     data/syminfo.lib data/symop.lib
# Files default.def and environ.def are not part of libccp4.
# They are included in CCP4 and may be useful for running programs
# compiled with libccp4 (see http://www.ccp4.ac.uk/dist/html/ccp4.html
# for details)

if FORTRAN_LIB
  ccp4data_DATA = data/font84.dat
  noinst_PROGRAMS = data/fontpack
  data_fontpack_SOURCES = data/fontpack.f
  data_fontpack_LDADD = fortran/libccp4f.la
  dist_noinst_DATA = data/font84.ascii
  CLEANFILES += data/font84.dat
endif

data/font84.dat: data/font84.ascii data/fontpack$(EXEEXT) 
	rm -f $@
	[ -e data/font84.ascii ] || copy_font84_ascii=true; \
	$${copy_font84_ascii:=false} && cp $< data/; \
	( cd data/ && ./fontpack$(EXEEXT) ) && \
	( $$copy_font84_ascii && rm -f data/font84.ascii ||: )

# FIXME: all files but the first are unused now, should they be distributed?
EXTRA_DIST = fortran/unix.m4 \
ccp4/w32mvs.c ccp4/vmslibrary.c \
fortran/vmsdiskio.for fortran/vms.for \
fortran/testlib.f fortran/mthlib.for fortran/w32mvs.f

