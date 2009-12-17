if FORTRAN_LIB
  FONT84 = font84.dat
endif

noinst_PROGRAMS = fontpack

fontpack_SOURCES = fontpack.f

fontpack_LDADD = ../fortran/libccp4f.la ../ccp4/libccp4c.la

pkgdatadir = $(datadir)/ccp4

dist_pkgdata_DATA = \
atomsf_neutron.lib	atomsf.lib \
syminfo.lib   $(FONT84)	symop.lib

font84.dat: fontpack font84.ascii
	./fontpack
