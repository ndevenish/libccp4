cmake_minimum_required(VERSION 3.4)
project(libccp4 C Fortran)

include(GNUInstallDirs)

option(BUILD_SHARED_LIBS "build shared libs (true)" TRUE)
option(FORTRAN_LIB "build fortran library (true)" TRUE)

include(${CMAKE_SOURCE_DIR}/cmake/source.cmake)

set(PACKAGE "libccp4")
set(VERSION "8.0")

add_library(ccp4c ${ccp4c_SOURCES} ${ccp4c_HEADERS})
add_library(ccp4::ccp4c ALIAS ccp4c)

target_include_directories(ccp4c PRIVATE .
    ${CMAKE_SOURCE_DIR}
    ${CMAKE_BINARY_DIR}
)
set_target_properties(ccp4c PROPERTIES SOVERSION ${VERSION})
set_target_properties(ccp4c PROPERTIES PUBLIC_HEADER "${ccp4c_HEADERS}")
if(MSVC)
    set_target_properties(ccp4c PROPERTIES DEFINE_SYMBOL "DLL_EXPORT")
endif()

install(TARGETS ccp4c 
      ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
      LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
      RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
      PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/ccp4
)

if(FORTRAN_LIB)
message(STATUS "Check for M4 macro processor")
find_program(M4
  NAMES "m4" "m4${CMAKE_EXECUTABLE_SUFFIX}"
  DOC "Location of the M4 macro processor"
  )
if(NOT M4)
  message(FATAL_ERROR
    "The `m4' macro processor was not found.  "
    "You must install `m4' before you can to compile libccp4.")
endif(NOT M4)
message(STATUS "Check for M4 macro processor: found ${M4}")

message(STATUS "Check for mmdb2 library")
find_library(MMDB2 mmdb2)
if(NOT MMDB2)
  message(FATAL_ERROR
    "The mmdb2 library was not found.  "
    "You must install mmdb2 before you can to compile libccp4f.")
endif(NOT MMDB2)
message(STATUS "Check for mmdb2 library: found ${MMDB2}")

if(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  set(M4_ARGS -D_ifc=8)
  add_compile_options(-DIFC)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  # Gfortran
  set(M4_ARGS -D_gfort=1)
  add_compile_options(-DGFORTRAN)
endif(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)

if(WIN32)
  set(M4_ARGS ${M4_ARGS} -Dwindows=1)
endif(WIN32)

add_custom_command(
    OUTPUT  unix.f
    COMMAND m4 ${M4_ARGS} ${CMAKE_SOURCE_DIR}/fortran/unix.m4 > ${CMAKE_CURRENT_BINARY_DIR}/unix.f
    DEPENDS ${CMAKE_SOURCE_DIR}/fortran/unix.m4
)
add_library(ccp4f ${ccp4f_SOURCES} ${CMAKE_CURRENT_BINARY_DIR}/unix.f)

target_link_libraries(ccp4f PRIVATE 
                      ccp4::ccp4c
                      ${MMDB2}
)

target_include_directories(ccp4f PRIVATE .
    ${CMAKE_SOURCE_DIR}/ccp4
    ${CMAKE_BINARY_DIR}
)

set_target_properties(ccp4f PROPERTIES SOVERSION ${VERSION})

install(TARGETS ccp4f
      ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
      LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
      RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
endif(FORTRAN_LIB)

if(BUILD_SHARED_LIBS)
set(prefix      ${CMAKE_INSTALL_PREFIX})
set(exec_prefix ${CMAKE_INSTALL_PREFIX})
set(libdir      ${CMAKE_INSTALL_PREFIX}/lib)
set(includedir  ${CMAKE_INSTALL_PREFIX}/include)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/ccp4c.pc.cmakein
   ${CMAKE_CURRENT_BINARY_DIR}/ccp4c.pc.in @ONLY)
file(GENERATE OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/ccp4c.pc
   INPUT ${CMAKE_CURRENT_BINARY_DIR}/ccp4c.pc.in)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/ccp4c.pc DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)
if(FORTRAN_LIB)
set(CCP4F_REQUIRED  mmdb2)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/ccp4f.pc.cmakein
   ${CMAKE_CURRENT_BINARY_DIR}/ccp4f.pc.in @ONLY)
file(GENERATE OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/ccp4f.pc
   INPUT ${CMAKE_CURRENT_BINARY_DIR}/ccp4f.pc.in)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/ccp4f.pc DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)
endif(FORTRAN_LIB)
endif(BUILD_SHARED_LIBS)

install(FILES ${CMAKE_SOURCE_DIR}/ccp4-config.cmake DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/ccp4 )
