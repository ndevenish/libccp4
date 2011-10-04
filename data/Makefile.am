if FORTRAN_LIB
  FONT84 = font84.dat
  FONTPACK = fontpack
endif

INCLUDES = $(MMDB_CXXFLAGS) $(CCIF_CXXFLAGS)

AM_LDFLAGS = $(MMDB_LIBS) $(CCIF_LIBS)

noinst_PROGRAMS = $(FONTPACK)

fontpack_SOURCES = fontpack.f

fontpack_LDADD = ../fortran/libccp4f.la ../ccp4/libccp4c.la

pkgdatadir = $(datadir)/ccp4

dist_pkgdata_DATA = \
atomsf_neutron.lib	atomsf.lib \
syminfo.lib   $(FONT84)	symop.lib

font84.dat: fontpack$(EXEEXT) font84.ascii
	rm -f $@
	[ -e font84.ascii ] || ln -s $(srcdir)/font84.ascii .
	./fontpack$(EXEEXT)
