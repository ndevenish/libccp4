$!
$ SET NOVERIFY
$ CCP4_DEF
$ CLIBS
$!
$ type sys$input
This compiles and link modules to make CCPLIB.OLB on VAX/VMS.

Use "@make_ccplib" to get the version using the old VAX assembler disk I/O
routines.  This requires VAXDISKIO.FOR, BLOCKIO.MAR and BLOCKMOVE.MAR. 

Use "@make_ccplib  ccplib  C" to get the version using the new C disk I/O
routines.  This requires DISKIO.C, though the low-level routines are actually
in LIBRARY.C.  It is slower than the assembler, the SCRATCH file open does not
work properly (the file is not deleted), but the C code is easier to follow in
case of problems, so it is probably better for debugging. 

Both versions also require CCPLIB.FOR, VMS.FOR and LIBRARY.C.

First compile CCPLIB.FOR, VMS.FOR and LIBRARY.C ...
$!
$ FORTRAN CLIBS:FFTLIB
$ FORTRAN CLIBS:MAPLIB
$ FORTRAN CLIBS:PARSER
$ FORTRAN CLIBS:RWATOM
$ FORTRAN CLIBS:RWBROOK
$ FORTRAN CLIBS:MODLIB
$ FORTRAN CLIBS:MTHLIB
$ FORTRAN CLIBS:FRODO_MAPLIB
$ FORTRAN CLIBS:SYMLIB
$ FORTRAN CLIBS:LCFLIB
$ FORTRAN CLIBS:MTZLIB
$ FORTRAN CLIBS:PLOT84LIB
$ FORTRAN CLIBS:GRAFLIB
$ FORTRAN CLIBS:CCPLIB
$ FORTRAN CLIBS:VMS
$ CC /DEF=VAX  CLIBS:LIBRARY
$!
$ if p1.eqs."" then p1:=CLIB:libccp4
$!
$ if p2.nes."C"
$ then
$!
$   type sys$input
Now compile VAXDISKIO.FOR, BLOCKIO.MAR and BLOCKMOVE.MAR ...
$   FORTRAN CLIBS:VAXDISKIO
$   MACRO CLIBS:BLOCKIO
$   MACRO CLIBS:BLOCKMOVE
$!
$   write sys$output "Now create the library ''p1'.OLB ..."
$!
$   LIBRARY/CREATE 'p1'  - 
CLIBS:CCPLIB,            -
CLIBS:VMS,               -
CLIBS:LIBRARY,           -
CLIBS:VAXDISKIO,         -
CLIBS:BLOCKIO,           -
CLIBS:BLOCKMOVE,         -
  CLIBS:FFTLIB,          -
  CLIBS:MAPLIB,          -
  CLIBS:PARSER,          -
  CLIBS:RWATOM,          -
  CLIBS:RWBROOK,         -
  CLIBS:MODLIB,          -
  CLIBS:MTHLIB,          -
  CLIBS:FRODO_MAPLIB,    -
  CLIBS:SYMLIB,          -
  CLIBS:LCFLIB,          -
  CLIBS:MTZLIB,          -
  CLIBS:PLOT84LIB,       -
  CLIBS:GRAFLIB,         -
  CLIBS:VMS
$!            
$   write sys$output "Last clean up the object modules ..."
$!
$DELETE/LOG               -
CLIBS:CCPLIB.OBJ;,        -
CLIBS:VMS.OBJ;,           -
CLIBS:LIBRARY.OBJ;,       -
CLIBS:VAXDISKIO.OBJ;,     -
CLIBS:BLOCKIO.OBJ;,       -
CLIBS:BLOCKMOVE.OBJ;,     -
CLIBS:FFTLIB.OBJ;,        -
CLIBS:MAPLIB.OBJ;,        -
CLIBS:PARSER.OBJ;,        -
CLIBS:RWATOM.OBJ;,        -
CLIBS:RWBROOK.OBJ;,       -
CLIBS:MODLIB.OBJ;,        -
CLIBS:MTHLIB.OBJ;,        -
CLIBS:FRODO_MAPLIB.OBJ;,  -
CLIBS:SYMLIB.OBJ;,        -
CLIBS:LCFLIB.OBJ;,        -
CLIBS:MTZLIB.OBJ;,        -
CLIBS:PLOT84LIB.OBJ;,     -
CLIBS:GRAFLIB.OBJ;
$!
$ else
$!
$   type sys$input
Now compile DISKIO.FOR ...
$   FORTRAN CLIBS:DISKIO
$!
$   write sys$output "Now create the library ''p1'.OLB ..."
$   LIBRARY/CREATE 'p1' -
CLIBS:CCPLIB,            -
CLIBS:VMS,               -
CLIBS:LIBRARY,           -
CLIBS:DISKIO,            -
  CLIBS:FFTLIB,          -
  CLIBS:MAPLIB,          -
  CLIBS:PARSER,          -
  CLIBS:RWATOM,          -
  CLIBS:RWBROOK,         -
  CLIBS:MODLIB,          -
  CLIBS:MTHLIB,          -
  CLIBS:FRODO_MAPLIB,    -
  CLIBS:SYMLIB,          -
  CLIBS:LCFLIB,          -
  CLIBS:MTZLIB,          -
  CLIBS:PLOT84LIB,       -
  CLIBS:GRAFLIB,         -
  CLIBS:VMS
$!
$   write sys$output "Last clean up the object modules ..."
$ DELETE/LOG              -
CLIBS:CCPLIB.OBJ;,        -
CLIBS:VMS.OBJ;,           -
CLIBS:LIBRARY.OBJ;,       -
CLIBS:DISKIO.OBJ;,        -
CLIBS:FFTLIB.OBJ;,        -
CLIBS:MAPLIB.OBJ;,        -
CLIBS:PARSER.OBJ;,        -
CLIBS:RWATOM.OBJ;,        -
CLIBS:RWBROOK.OBJ;,       -
CLIBS:MODLIB.OBJ;,        -
CLIBS:MTHLIB.OBJ;,        -
CLIBS:FRODO_MAPLIB.OBJ;,  -
CLIBS:SYMLIB.OBJ;,        -
CLIBS:LCFLIB.OBJ;,        -
CLIBS:MTZLIB.OBJ;,        -
CLIBS:PLOT84LIB.OBJ;,     -
CLIBS:GRAFLIB.OBJ;       
$!
$ endif
