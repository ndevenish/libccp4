$! Add the MOSFLM stuff to the library
$!
$ fort/list sys$input
$ ass/nolog nl: sys$output
$ search .lis "DEC Fortran"
$ if $severity .eq. 1
$   then
$   FORTRAN := fortran/separate/noopt/deb
$   ! otherwise all routines in a source file are put in a single module.
$   deass sys$output
$   write sys$output "Assuming DEC Fortran"
$ else
$   deass sys$output
$ endif
$ del .lis;,.obj;
$ FORTRAN clibs:graflib.f,clibs:pack_f.f
$ CC clibs:pack_c.c,chelp.c
$ lib/rep ccp4_lib:libccp4.olb graflib.obj,pack_f.obj,pack_c.obj,chelp.obj 
$ DEL graflib.obj;*,pack_f.obj;*,pack_c.obj;*,chelp.obj;*
$!
