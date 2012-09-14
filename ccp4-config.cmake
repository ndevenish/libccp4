# - Find CCP4 libraries
# Find one or more of CCP4 libraries: ccp4c, ccp4f, mmdb, ccif,
# clipper-core, clipper-ccp4, clipper-contrib, clipper-minimol, clipper-mmdb.
#
# Once done this will define
#  CCP4_INCLUDE_DIRS - all include directories
#  <name>_LIBRARY - library, name is one of CCP4C, CCP4F, MMDB, CCIF,
#                            CLIPPER-CORE, CLIPPER-CCP4, CLIPPER-CONTRIB,
#                            CLIPPER-MINIMOL, CLIPPER-MMDB
#  CCIF_LIBRARIES - CCIF_LIBRARY with (if needed) regex library
#  CLIPPER-CORE_LIBRARIES - CLIPPER-CORE_LIBRARY with (if needed)
#                           fftw2 and thread library
#  CCP4_LIBRARIES - all requested libraries with necessary dependencies
#  <name>_FOUND - TRUE if library and header files are found.
#
# You need to name libraries that you will use as components:
# FIND_PACKAGE(CCP4 COMPONENTS mmdb ccp4c)
# or
# FIND_PACKAGE(CCP4 REQUIRED mmdb ccp4c)
#
# Sample usage:
#   FIND_PACKAGE(CCP4 REQUIRED mmdb ccp4)
#   INCLUDE_DIRECTORIES(${CCP4_INCLUDE_DIRS})
#   TARGET_LINK_LIBRARIES(<YourTarget> ${CCP4_LIBRARIES})

if(NOT CCP4_FIND_COMPONENTS)
    message(FATAL_ERROR "Please specify CCP4 libraries/components.")
endif()

include(FindPackageHandleStandardArgs)
include(CheckFunctionExists)
include(CheckCSourceCompiles)
include(CheckCXXSourceCompiles)

foreach(_component ${CCP4_FIND_COMPONENTS})
    string(TOUPPER ${_component} _upper)
    string(TOLOWER ${_component} _lower)
    set(_lib_var "${_upper}_LIBRARY")
    find_library(${_lib_var} NAMES ${_lower}
                 HINTS ${LIB_INSTALL_DIR}
                 PATHS $ENV{CLIB}
                       $ENV{CCP4}/lib
                       $ENV{CCP4}/lib64)
    #message("DEBUG: ${_lib_var} (${_lower}) = ${${_upper}_LIBRARY}")

    if (${_upper} STREQUAL "MMDB")
        set(_header "mmdb/mmdb_defs.h")
    elseif (${_upper} STREQUAL "CCP4C")
        set(_header "ccp4/ccp4_general.h")
    elseif (${_upper} STREQUAL "CCP4F")
        # fortran library, no headers
        set(_header)
    elseif (${_upper} STREQUAL "CCIF")
        set(_header "ccif/ccif_defines.h")
    elseif (${_upper} STREQUAL "CLIPPER-CORE")
        find_package(Threads)
        set(_header "clipper/clipper.h")
    elseif (${_upper} STREQUAL "CLIPPER-CCP4")
        set(_header "clipper/clipper-ccp4.h")
    elseif (${_upper} STREQUAL "CLIPPER-CONTRIB")
        set(_header "clipper/clipper-contrib.h")
    elseif (${_upper} STREQUAL "CLIPPER-MINIMOL")
        set(_header "clipper/clipper-minimol.h")
    elseif (${_upper} STREQUAL "CLIPPER-MMDB")
        set(_header "clipper/clipper-mmdb.h")
    else()
        message(FATAL_ERROR "'${_component}' is not a valid CCP4 library.")
    endif()

    if (_header)
        set(_incl_var "${_upper}_INCLUDE_DIR")
        find_path(${_incl_var} ${_header}
                  PATHS $ENV{CINCL} $ENV{CCP4}/include)
        message(STATUS "Looking for ${_header} - ${${_incl_var}}")
        find_package_handle_standard_args(CCP4 DEFAULT_MSG
                                          ${_lib_var} ${_incl_var})
        set(CCP4_INCLUDE_DIRS ${CCP4_INCLUDE_DIRS} ${${_incl_var}})
    else()
        find_package_handle_standard_args(CCP4 DEFAULT_MSG
                                          ${_lib_var})
    endif()

    # check if libccif needs -lregex
    if (${_upper} STREQUAL "CCIF")
        set(_CCIF_SRC "int main() {zzs_undump(0,0,0);}")
        foreach (_REGEX_LIB "" regex)
            message (STATUS "Trying _REGEX_LIB=${_REGEX_LIB}")
            set(CMAKE_REQUIRED_LIBRARIES ${CCIF_LIBRARY} ${_REGEX_LIB} m)
            set(_VAR _LINKING_WITH_CCIF_${_REGEX_LIB})
            check_c_source_compiles("${_CCIF_SRC}" ${_VAR})
            if (${_VAR})
                set(CCIF_LIBRARIES ${CCIF_LIBRARY} ${_REGEX_LIB} m)
                break()
            endif()
        endforeach()
        if (NOT ${_VAR})
            message(FATAL_ERROR "Linking with libccif failed.")
        endif()
    endif()

    # check if clipper-core needs rfftw fftw
    if (${_upper} STREQUAL "CLIPPER-CORE")
        set(_SAVE ${CMAKE_REQUIRED_INCLUDES})
        set(CMAKE_REQUIRED_INCLUDES "${_SAVE};${CLIPPER-CORE_INCLUDE_DIR}")
        set(_CLIP_SRC "#include <clipper/clipper.h>\n"
                      "int main() {clipper::FFTmap().fft_h_to_x();}")
            set(CMAKE_REQUIRED_LIBRARIES ${CLIPPER-CORE_LIBRARY})
            set(_VAR _LINKING_WITH_CLIPPER_CORE)
            check_cxx_source_compiles("${_CLIP_SRC}" ${_VAR})
        if (NOT ${_VAR})
            set(_ADD_LIBS ${CMAKE_THREAD_LIBS_INIT})
            set(CMAKE_REQUIRED_LIBRARIES ${CLIPPER-CORE_LIBRARY} ${_ADD_LIBS})
            set(_VAR _LINKING_WITH_CLIPPER_CORE_AND_THREADS)
            check_cxx_source_compiles("${_CLIP_SRC}" ${_VAR})
        endif()
        if (NOT ${_VAR})
            set(_ADD_LIBS srfftw sfftw ${CMAKE_THREAD_LIBS_INIT})
            set(CMAKE_REQUIRED_LIBRARIES ${CLIPPER-CORE_LIBRARY} ${_ADD_LIBS})
            set(_VAR _LINKING_WITH_CLIPPER_CORE_AND_SFFTW)
            check_cxx_source_compiles("${_CLIP_SRC}" ${_VAR})
        endif()
        if (NOT ${_VAR})
            set(_ADD_LIBS rfftw fftw ${CMAKE_THREAD_LIBS_INIT})
            set(CMAKE_REQUIRED_LIBRARIES ${CLIPPER-CORE_LIBRARY} ${_ADD_LIBS})
            set(_VAR _LINKING_WITH_CLIPPER_CORE_AND_FFTW)
            check_cxx_source_compiles("${_CLIP_SRC}" ${_VAR})
        endif()
        if (NOT ${_VAR})
            message(FATAL_ERROR "Linking with clipper-core failed.")
        endif()
        set(CLIPPER-CORE_LIBRARIES ${CLIPPER-CORE_LIBRARY} ${_ADD_LIBS})
        SET(CMAKE_REQUIRED_INCLUDES ${_SAVE})
    endif()
endforeach()


set(CCP4_LIBRARIES ${CLIPPER-CCP4_LIBRARY}
                   ${CLIPPER-CONTRIB_LIBRARY}
                   ${CLIPPER-MINIMOL_LIBRARY}
                   ${CLIPPER-MMDB_LIBRARY}
                   ${CLIPPER-CORE_LIBRARIES}
                   ${CCIF_LIBRARIES}
                   ${CCP4F_LIBRARY}
                   ${CCP4C_LIBRARY}
                   ${MMDB_LIBRARY})

if(CCP4_INCLUDE_DIRS)
    list(REMOVE_DUPLICATES CCP4_INCLUDE_DIRS)
endif()
message(STATUS "CCP4 include directory: ${CCP4_INCLUDE_DIRS}")

mark_as_advanced(CCP4C_INCLUDE_DIR MMDB_INCLUDE_DIR CCIF_INCLUDE_DIR
                 CLIPPER-CORE_INCLUDE_DIR CLIPPER-CCP4_INCLUDE_DIR
                 CLIPPER-CONTRIB_INCLUDE_DIR CLIPPER-MINIMOL_INCLUDE_DIR
                 CLIPPER-MMDB_INCLUDE_DIR)

