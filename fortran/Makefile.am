lib_LTLIBRARIES = libccp4f.la

libccp4f_c_files = \
ccp4_diskio_f.c ccp4_parser_f.c cmaplib_f.c csymlib_f.c \
ccp4_general_f.c ccp4_unitcell_f.c cmtzlib_f.c library_f.c \
binsortint.c

libccp4f_f_files = \
ccplib.f fftlib.f modlib.f parser.f rwbrook.f libhtml.f \
keyparse.f unix.f dna_output.f pack_f.f pxxml.f lgglib.f \
plot84lib.f plotsubs.f harvlib.f cciflib.f

libccp4f_la_SOURCES = $(libccp4f_c_files) $(libccp4f_f_files)

libccp4f_la_LDFLAGS = -version-info 6:0:1

AM_CFLAGS = -I$(srcdir)/../ccp4 

unix.f : $(srcdir)/unix.m4
	$(M4) unix.m4 >unix.f
