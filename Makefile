

##########################################################################
# Makefile for CCP4 Library                                              #
##########################################################################
#
# Usage: make                     - to make the library
#        make routine             - to remake that particular routine
#
# The options below are for our Silicon Graphics (use -Nc40 to increase
# number of nested loops). On system V change RANLIB to "echo" since it's 
# not used.
#
CCP4_MASTER = /nfs/dlpx1/ccpdisk/xtal
CCP4_LIB    = /ccpdisk/ccp4/lib
CCP4_BIN    = /pxbin
FCOMP       = f77
FFLAGS      = -Nc40 -L/ccpdisk/ccp4/lib
OPTIM       = -g3
CCOMP       = cc
CFLAGS      = -I/nfs/dlpx1/ccpdisk/xtal/ccp4/include
RANLIB      = echo "ranlib"
SHELL       = /bin/sh

FTARGETS   = ccplib fftlib maplib parser rwatom rwbrook modlib \
             symlib lcflib mtzlib diskio unix frodo_maplib hlplib \
             gdummy

GRAPHICS   = plot84lib graflib graphics

CTARGETS   = binsortint library ucurse

help:
	@echo "Use: make libs    - to make library libccp4.a "
	@echo "     make binsort - to make binsort "
	@echo "     make clean   - to clean directory "

libs:	${CCP4_LIB}/libccp4.a

${CCP4_LIB}/libccp4.a: ${CTARGETS} ${FTARGETS} ${GRAPHICS}
	${RANLIB} ${CCP4_LIB}/libccp4.a

${FTARGETS} ${GRAPHICS}:
	-/bin/rm -rf $@.dir
	mkdir $@.dir
	cd $@.dir ; fsplit $?         ; \
	${FCOMP} ${FFLAGS} -c ${OPTIM} *.f  ; \
	ar r ${CCP4_LIB}/libccp4.a *.o
	-/bin/rm -rf $@.dir
	touch $@

${CTARGETS}:
	/bin/rm -f *.o
	${CCOMP} -c ${CFLAGS} $?
	ar r ${CCP4_LIB}/libccp4.a *.o
	/bin/rm -f *.o
	touch $@

binsort:
	${CCOMP} ${CFLAGS} $@.c -o ${CCP4_BIN}/$@

testlib:
	ln testlib.for testlib.f
	$(FCOMP) $(FFLAGS) $(OPTIM) -o testlib testlib.f -lccp4
	rm testlib.f

clean:
	-rm -f *.f *.o ${FTARGETS} ${CTARGETS} ${GRAPHICS} testlib


# Make dependencies:

ccplib:		${CCP4_MASTER}/ccp4/lib/src/ccplib.for
fftlib:		${CCP4_MASTER}/ccp4/lib/src/fftlib.for
hlplib:         ${CCP4_MASTER}/ccp4/lib/src/hlplib.for
maplib:		${CCP4_MASTER}/ccp4/lib/src/maplib.for
parser:		${CCP4_MASTER}/ccp4/lib/src/parser.for
rwatom:		${CCP4_MASTER}/ccp4/lib/src/rwatom.for
rwbrook: 	${CCP4_MASTER}/ccp4/lib/src/rwbrook.for
modlib:		${CCP4_MASTER}/ccp4/lib/src/modlib.for
symlib:		${CCP4_MASTER}/ccp4/lib/src/symlib.for
lcflib:		${CCP4_MASTER}/ccp4/lib/src/lcflib.for
mtzlib:		${CCP4_MASTER}/ccp4/lib/src/mtzlib.for
diskio:		${CCP4_MASTER}/ccp4/lib/src/diskio.for
unix:		${CCP4_MASTER}/ccp4/lib/src/unix.for
plot84lib:	${CCP4_MASTER}/ccp4/lib/src/plot84lib.for
graflib:	${CCP4_MASTER}/ccp4/lib/src/graflib.for
graphics:	${CCP4_MASTER}/ccp4/lib/src/graphics.for
binsortint:	${CCP4_MASTER}/ccp4/lib/src/binsortint.c
library:	${CCP4_MASTER}/ccp4/lib/src/library.c
ucurse:		${CCP4_MASTER}/ccp4/lib/src/ucurse.c
frodo_maplib:	${CCP4_MASTER}/ccp4/lib/src/frodo_maplib.for
testlib:	${CCP4_MASTER}/ccp4/lib/src/testlib.for
gdummy:		${CCP4_MASTER}/ccp4/lib/src/gdummy.for
