$!
$ SET NOVERIFY
$ CCP4_DEF
$ CLIBS
$!
$! 
$ IF (P1.EQS."").OR.(P1.EQS."M").OR.(P1.EQS."C").OR. -
 (P1.EQS."F").OR.(P1.EQS."H")
$ THEN
$ GOTO SKIP
$ ELSE
$ WRITE SYS$OUTPUT "    use:"
$ WRITE SYS$OUTPUT "    @MAKE_CCPLIB <H,M,C,F, ""> <FILNAME>"
$ WRITE SYS$OUTPUT "    type: @MAKE_CCPLIB H to get help"
$ EXIT
$!
$ ENDIF
$!
$ SKIP:
$!
$ IF P1 .EQS. "H"
$ THEN
$ TYPE SYS$INPUT

This procedure compiles and links the CCP4 library modules
on a VAX VMS platform.

If you have C compiler on your machine you can use new
set of the new C disk input/output routines. The same as being used
in the Unix version of the CCP4 program suite.
These are slower then the old Vax assembler routines, the scratch
file open does not work properly (the file is not deleted),
but the C code is easier to follow in case of problems, so
it is probably better for debugging.Use:
    @make_ccplib C <object file name>

If you want to use the old Vax assembler input/output routines
with few C routines from the library.c which are still needed.
Use:
    @make_ccplib M <object file name>

If you don't have C compiler there is still possible to compile
the CCP4 library with assembler input/output routines and some
dummy fortran routines which are used instead of the C stuff from
the library.c. Use:
    @make_ccplib F <object file name>

By default the library object file is called ccplib.olb and is placed in
the directory defined by the logical name CLIB.
$ EXIT
$!
$ ENDIF
$!
$ WRITE SYS$OUTPUT "First compile common fortran routines"
$ WRITE SYS$OUTPUT ""
$!
$ WRITE SYS$OUTPUT "Compiling FFTLIB.FOR"
$ FORTRAN CLIBS:FFTLIB
$ WRITE SYS$OUTPUT "Compiling MAPLIB.FOR"
$ FORTRAN CLIBS:MAPLIB
$ WRITE SYS$OUTPUT "Compiling PARSER.FOR"
$ FORTRAN CLIBS:PARSER
$ WRITE SYS$OUTPUT "Compiling RWATOM.FOR"
$ FORTRAN CLIBS:RWATOM
$ WRITE SYS$OUTPUT "Compiling RWBROOK.FOR"
$ FORTRAN CLIBS:RWBROOK
$ WRITE SYS$OUTPUT "Compiling MODLIB.FOR"
$ FORTRAN CLIBS:MODLIB
$ WRITE SYS$OUTPUT "Compiling MTHLIB.FOR"
$ FORTRAN CLIBS:MTHLIB
$ WRITE SYS$OUTPUT "Compiling FRODO_MAPLIB.FOR"
$ FORTRAN CLIBS:FRODO_MAPLIB
$ WRITE SYS$OUTPUT "Compiling SYMLIB.FOR"
$ FORTRAN CLIBS:SYMLIB
$ WRITE SYS$OUTPUT "Compiling LCFLIB.FOR"
$ FORTRAN CLIBS:LCFLIB
$ WRITE SYS$OUTPUT "Compiling MTZLIB.FOR"
$ FORTRAN CLIBS:MTZLIB
$ WRITE SYS$OUTPUT "Compiling PLOT84LIB.FOR"
$ FORTRAN CLIBS:PLOT84LIB
$ WRITE SYS$OUTPUT "Compiling GRAFLIB.FOR"
$ FORTRAN CLIBS:GRAFLIB
$ WRITE SYS$OUTPUT "Compiling CCPLIB.FOR"
$ FORTRAN CLIBS:CCPLIB
$ WRITE SYS$OUTPUT "Compiling VMS.FOR"
$ FORTRAN CLIBS:VMS
$!
$ if p2 .eqs. "" 
$ then 
$ p2 := CLIB:libccp4 
$!
$ WRITE SYS$OUTPUT "By default object library file name"
$ WRITE SYS$OUTPUT "assumed to be CLIB:libccp4.obj"
$ else
$ WRITE SYS$OUTPUT "Object library file name is ''P2' "
$ endif
$!
$ if p1 .eqs. "M"
$ then
$!
$   type sys$input
Now compile LIBRARY.C VAXDISKIO.FOR, BLOCKIO.MAR and BLOCKMOVE.MAR ...
$   CC /DEF=VAX  CLIBS:LIBRARY
$   FORTRAN CLIBS:VAXDISKIO
$   MACRO CLIBS:BLOCKIO
$   MACRO CLIBS:BLOCKMOVE
$!
$   write sys$output "Now create the library ''p2'.OLB ..."
$!
$   LIBRARY/CREATE 'p2'  - 
CLIBS:CCPLIB,            -
CLIBS:VMS,               -
CLIBS:LIBRARY,           -
CLIBS:VAXDISKIO,         -
CLIBS:BLOCKIO,           -
CLIBS:BLOCKMOVE,         -
CLIBS:FFTLIB,            -
CLIBS:MAPLIB,            -
CLIBS:PARSER,            -
CLIBS:RWATOM,            -
CLIBS:RWBROOK,           -
CLIBS:MODLIB,            -
CLIBS:MTHLIB,            -
CLIBS:FRODO_MAPLIB,      -
CLIBS:SYMLIB,            -
CLIBS:LCFLIB,            -
CLIBS:MTZLIB,            -
CLIBS:PLOT84LIB,         -
CLIBS:GRAFLIB
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
$ endif
$!
$ if p1 .eqs. "C" 
$ then
$!
$   type sys$input
Now compile LIBRARY.C and DISKIO.FOR ...
$!
$   CC/DEF=VAX CLIBS:LIBRARY
$   FORTRAN CLIBS:DISKIO
$!
$  write sys$output "Now create the library ''p2'.OLB ..."
$  LIBRARY/CREATE 'p2' -
CLIBS:CCPLIB,          -
CLIBS:VMS,             -
CLIBS:LIBRARY,         -
CLIBS:DISKIO,          -
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
$ DELETE/LOG             -
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
$!
$ if (p1 .eqs. "F") .or. (p1 .eqs. "")
$ then
$!
$   type sys$input
Now compile VMS_DUM_C.FOR, VAXDISKIO.FOR, BLOCKIO.MAR and BLOCKMOVE.MAR ...
$   FORTRAN CLIBS:VMS_DUM_C
$   FORTRAN CLIBS:VAXDISKIO
$   MACRO CLIBS:BLOCKIO
$   MACRO CLIBS:BLOCKMOVE
$!
$   write sys$output "Now create the library ''p2'.OLB ..."
$!
$   LIBRARY/CREATE 'p2'  - 
CLIBS:CCPLIB,            -
CLIBS:VMS,               -
CLIBS:VMS_DUM_C,         -
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
CLIBS:GRAFLIB         
$!            
$   write sys$output "Last clean up the object modules ..."
$!
$ DELETE/LOG              -
CLIBS:CCPLIB.OBJ;,        -
CLIBS:VMS.OBJ;,           -
CLIBS:VMS_DUM_C.OBJ;,     -
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
$ endif
$!
