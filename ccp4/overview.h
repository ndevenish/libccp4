/** @mainpage CINCH - Crystallography IN C Headers
 *
 * @note I'm fed up of the uninspiring "new library" and here's
 *       my first attempt at a funky name. Alternatives welcome!
 *
 * @section aims Aims

The CCP4 software suite is based around a library of routines which
cover common tasks, such as file opening, parsing keyworded input,
reading and writing of standard data formats, applying symmetry
operations, etc.  Programs in the suite call these routines which, as
well as saving the programmer some effort, ensure that the varied
programs in the suite have a similar look-and-feel. 
<p>
Since 2002, there has been a major effort to re-write
much of the CCP4 library into C/C++. The aims are:

<ul>
<li>To implement a better representation of the underlying data model.
For example, Eugene Krissinel's MMDB library acts on a data structure
which represents the various levels of structure of a protein model.
The new MTZ library encapsulates the crystal/dataset hierarchy that
is increasingly being used by programs.
<li>To maintain support for existing programs. In particular, the 
existing Fortran APIs will be maintained, although they will now often
be only wrappers to functions in the new library. It is hoped that many
existing programs will be migrated to using the new library directly.
<li>To provide support for scripting. It is possible to generate APIs 
for Python, Tcl and Perl automatically from the core C code. Thus, much
of the standard CCP4 functionality wil be available to scripts used
e.g. in ccp4i or the molecular graphics project.
</ul>

This incremental approach, maintaining the existing suite while
improving the underlying code, puts constraints on what is possible, but
is considered more appropriate for a collaborative project like CCP4.

 * @section start This documentation

<p>
This documentation is generated automatically by 
<a href="http://www.doxygen.org/">Doxygen</a> from
comment sections in the code. It is therefore detailed and extensive. 
The library divides roughly into the following sections:
<dl>
<dt>CMTZ library
<dd>See the @ref cmtz_page page for C/C++ programmers, and the
@ref cmtz_f_page page for Fortran programmers.
<dt>CMAP library
<dd>Pending ...
<dt>MMDB library
<dd>See Eugene's <a href="http://www.ebi.ac.uk/~keb/cldoc">documentation</a>.
<dt>CSYM library
<dd>See the @ref csym_page page for C/C++ programmers, and the
@ref csym_f_page page for Fortran programmers.
<dt>CCP4 utility library
<dd>See the @ref utilities_page page for C/C++ programmers.
<dt>Low level functionality
<dd>Pending ...
</dl>

 */
