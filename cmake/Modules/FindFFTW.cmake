# Distributed under the OSI-approved BSD 3-Clause License.
# Copyright Stefano Sinigardi
#
#.rst:
# FindFFTW
# --------
#
# Result Variables
# ^^^^^^^^^^^^^^^^
#
# This module will set the following variables in your project::
#
#  ``FFTW_FOUND``
#    True if FFTW found on the local system
#
#  ``FFTW_INCLUDE_DIRS``
#    Location of FFTW header files.
#
#  ``FFTW_LIBRARIES``
#    The FFTW libraries.
#
# Hints
# ^^^^^
#
#  ``FFTW_ROOT_DIR``
#    Set this variable to a directory that contains a FFTW installation.
#

include(FindPackageHandleStandardArgs)

set( FFTW_USE_PKGCONFIG OFF )

if( EXISTS "${FFTW_ROOT_DIR}" )
  file( TO_CMAKE_PATH "${FFTW_ROOT_DIR}" FFTW_ROOT_DIR_FIXED_PATH )
endif()

if( EXISTS "$ENV{FFTW_ROOT_DIR}" )
  file( TO_CMAKE_PATH "$ENV{FFTW_ROOT_DIR}" FFTW_ROOT_DIR )
  set( FFTW_ROOT_DIR "${FFTW_ROOT_DIR}" CACHE PATH "Prefix for FFTW installation." )
endif()

if( NOT EXISTS "${FFTW_ROOT_DIR}" AND NOT EXISTS "${FFTW_ROOT_DIR_FIXED_PATH}")
  set( FFTW_USE_PKGCONFIG ON )
endif()

set(ENABLE_FFTW_LONG_DOUBLE FALSE CACHE BOOL "Enabling FFTW3L")

#=============================================================================
# As a first try, use the PkgConfig module.  This will work on many
# *NIX systems.  See :module:`findpkgconfig`
# This will return ``FFTW_INCLUDEDIR`` and ``FFTW_LIBDIR`` used below.
if( FFTW_USE_PKGCONFIG )
  find_package(PkgConfig)
  pkg_check_modules( FFTW QUIET fftw3 )

  if( EXISTS "${FFTW_INCLUDEDIR}" )
    get_filename_component( FFTW_ROOT_DIR "${FFTW_INCLUDEDIR}" DIRECTORY CACHE)
  endif()
endif()

#=============================================================================
# Set FFTW_INCLUDE_DIRS and FFTW_LIBRARIES. If we skipped the PkgConfig step, try
# to find the libraries at $FFTW_ROOT_DIR (if provided) or in standard system
# locations.  These find_library and find_path calls will prefer custom
# locations over standard locations (HINTS).  If the requested file is not found
# at the HINTS location, standard system locations will be still be searched
# (/usr/lib64 (Redhat), lib/i386-linux-gnu (Debian)).

find_path( FFTW_INCLUDE_DIR
  NAMES fftw3.h
  HINTS ${FFTW_ROOT_DIR}/include ${FFTW_ROOT_DIR} ${FFTW_ROOT_DIR_FIXED_PATH} ${FFTW_ROOT_DIR_FIXED_PATH}/include ${FFTW_INCLUDEDIR}
)

find_library( FFTW_LIBRARY
  NAMES fftw3 fftw3-3 libfftw3-3
  HINTS ${FFTW_ROOT_DIR}/lib ${FFTW_ROOT_DIR} ${FFTW_ROOT_DIR_FIXED_PATH} ${FFTW_ROOT_DIR_FIXED_PATH}/lib ${FFTW_LIBDIR}
  PATH_SUFFIXES Release Debug
)

find_library( FFTWF_LIBRARY
  NAMES fftw3f fftw3f-3 libfftw3f-3
  HINTS ${FFTW_ROOT_DIR}/lib ${FFTW_ROOT_DIR} ${FFTW_ROOT_DIR_FIXED_PATH} ${FFTW_ROOT_DIR_FIXED_PATH}/lib ${FFTW_LIBDIR}
  PATH_SUFFIXES Release Debug
)

if (ENABLE_FFTW_LONG_DOUBLE)
  find_library( FFTWL_LIBRARY
    NAMES fftw3l fftw3l-3 libfftw3l-3
    HINTS ${FFTW_ROOT_DIR}/lib ${FFTW_ROOT_DIR} ${FFTW_ROOT_DIR_FIXED_PATH} ${FFTW_ROOT_DIR_FIXED_PATH}/lib ${FFTW_LIBDIR}
    PATH_SUFFIXES Release Debug
  )
endif()

# Do we also have debug versions?
find_library( FFTW_LIBRARY_DEBUG
  NAMES fftw3d
  HINTS ${FFTW_ROOT_DIR}/lib ${FFTW_ROOT_DIR}  ${FFTW_ROOT_DIR_FIXED_PATH} ${FFTW_ROOT_DIR_FIXED_PATH}/lib ${FFTW_LIBDIR}
  PATH_SUFFIXES Debug
)

find_library( FFTWF_LIBRARY_DEBUG
  NAMES fftw3fd
  HINTS ${FFTW_ROOT_DIR}/lib ${FFTW_ROOT_DIR}  ${FFTW_ROOT_DIR_FIXED_PATH} ${FFTW_ROOT_DIR_FIXED_PATH}/lib ${FFTW_LIBDIR}
  PATH_SUFFIXES Debug
)

if (ENABLE_FFTW_LONG_DOUBLE)
  find_library( FFTWL_LIBRARY_DEBUG
    NAMES fftw3ld
    HINTS ${FFTW_ROOT_DIR}/lib ${FFTW_ROOT_DIR}  ${FFTW_ROOT_DIR_FIXED_PATH} ${FFTW_ROOT_DIR_FIXED_PATH}/lib ${FFTW_LIBDIR}
    PATH_SUFFIXES Debug
  )
endif()

set( FFTW_INCLUDE_DIRS ${FFTW_INCLUDE_DIR} )
set( FFTW_LIBRARIES ${FFTW_LIBRARY} ${FFTWF_LIBRARY} ${FFTWL_LIBRARY} )

#=============================================================================
# handle the QUIETLY and REQUIRED arguments and set FFTW_FOUND to TRUE if all
# listed variables are TRUE
find_package_handle_standard_args( FFTW
  FOUND_VAR
    FFTW_FOUND
  REQUIRED_VARS
    FFTW_INCLUDE_DIR
    FFTW_LIBRARY
    FFTWF_LIBRARY
  )

mark_as_advanced( FFTW_ROOT_DIR FFTW_LIBRARY FFTW_INCLUDE_DIR
  FFTWF_LIBRARY FFTWL_LIBRARY FFTW_LIBRARY_DEBUG FFTWF_LIBRARY_DEBUG FFTWL_LIBRARY_DEBUG
  FFTW_USE_PKGCONFIG FFTW_CONFIG )


#=============================================================================
# Register imported libraries:
# 1. If we can find a Windows .dll file (or if we can find both Debug and
#    Release libraries), we will set appropriate target properties for these.
# 2. However, for most systems, we will only register the import location and
#    include directory.

# Look for dlls, or Release and Debug libraries.
if(WIN32)
  string( REPLACE ".lib" ".dll" FFTW_LIBRARY_DLL        "${FFTW_LIBRARY}" )
  string( REPLACE ".lib" ".dll" FFTWF_LIBRARY_DLL       "${FFTWF_LIBRARY}" )
  string( REPLACE ".lib" ".dll" FFTWL_LIBRARY_DLL       "${FFTWL_LIBRARY}" )
  string( REPLACE ".lib" ".dll" FFTW_LIBRARY_DEBUG_DLL  "${FFTW_LIBRARY_DEBUG}" )
  string( REPLACE ".lib" ".dll" FFTWF_LIBRARY_DEBUG_DLL "${FFTWF_LIBRARY_DEBUG}" )
  string( REPLACE ".lib" ".dll" FFTWL_LIBRARY_DEBUG_DLL "${FFTWL_LIBRARY_DEBUG}" )
endif()

if( FFTW_FOUND AND NOT TARGET FFTW::fftw3 )
  if( EXISTS "${FFTW_LIBRARY_DLL}" AND EXISTS "${FFTWF_LIBRARY_DLL}" AND EXISTS "${FFTWL_LIBRARY_DLL}" )

    # Windows systems with dll libraries.
    add_library( FFTW::fftw3      SHARED IMPORTED )
    if (ENABLE_FFTW_LONG_DOUBLE)
      add_library( FFTW::fftw3l     SHARED IMPORTED )
    endif()
    add_library( FFTW::fftw3f     SHARED IMPORTED )

    # Windows with dlls, but only Release libraries.
    set_target_properties( FFTW::fftw3f PROPERTIES
      IMPORTED_LOCATION_RELEASE         "${FFTWF_LIBRARY_DLL}"
      IMPORTED_IMPLIB                   "${FFTWF_LIBRARY}"
      INTERFACE_INCLUDE_DIRECTORIES     "${FFTW_INCLUDE_DIRS}"
      IMPORTED_CONFIGURATIONS           Release
      IMPORTED_LINK_INTERFACE_LANGUAGES "C" )
    if (ENABLE_FFTW_LONG_DOUBLE)
      set_target_properties( FFTW::fftw3l PROPERTIES
        IMPORTED_LOCATION_RELEASE         "${FFTWL_LIBRARY_DLL}"
        IMPORTED_IMPLIB                   "${FFTWL_LIBRARY}"
        INTERFACE_INCLUDE_DIRECTORIES     "${FFTW_INCLUDE_DIRS}"
        IMPORTED_CONFIGURATIONS           Release
        IMPORTED_LINK_INTERFACE_LANGUAGES "C" )
    endif()
    set_target_properties( FFTW::fftw3 PROPERTIES
      IMPORTED_LOCATION_RELEASE         "${FFTW_LIBRARY_DLL}"
      IMPORTED_IMPLIB                   "${FFTW_LIBRARY}"
      INTERFACE_INCLUDE_DIRECTORIES     "${FFTW_INCLUDE_DIRS}"
      IMPORTED_CONFIGURATIONS           Release
      IMPORTED_LINK_INTERFACE_LANGUAGES "C" )

    # If we have both Debug and Release libraries
    if( EXISTS "${FFTW_LIBRARY_DEBUG_DLL}" AND EXISTS "${FFTWF_LIBRARY_DEBUG_DLL}" AND EXISTS "${FFTWL_LIBRARY_DEBUG_DLL}" )
      set_property( TARGET FFTW::fftw3f APPEND PROPERTY IMPORTED_CONFIGURATIONS Debug )
      set_target_properties( FFTW::fftw3f PROPERTIES
        IMPORTED_LOCATION_DEBUG           "${FFTWF_LIBRARY_DEBUG_DLL}"
        IMPORTED_IMPLIB_DEBUG             "${FFTWF_LIBRARY_DEBUG}" )
      if (ENABLE_FFTW_LONG_DOUBLE)
        set_property( TARGET FFTW::fftw3l APPEND PROPERTY IMPORTED_CONFIGURATIONS Debug )
        set_target_properties( FFTW::fftw3l PROPERTIES
          IMPORTED_LOCATION_DEBUG           "${FFTWL_LIBRARY_DEBUG_DLL}"
          IMPORTED_IMPLIB_DEBUG             "${FFTWL_LIBRARY_DEBUG}" )
      endif()
      set_property( TARGET FFTW::fftw3 APPEND PROPERTY IMPORTED_CONFIGURATIONS Debug )
      set_target_properties( FFTW::fftw3 PROPERTIES
        IMPORTED_LOCATION_DEBUG           "${FFTW_LIBRARY_DEBUG_DLL}"
        IMPORTED_IMPLIB_DEBUG             "${FFTW_LIBRARY_DEBUG}" )
    endif()

  else()

    # For all other environments (ones without dll libraries), create
    # the imported library targets.
    add_library( FFTW::fftw3      UNKNOWN IMPORTED )
    add_library( FFTW::fftw3f     UNKNOWN IMPORTED )
    add_library( FFTW::fftw3l     UNKNOWN IMPORTED )
    set_target_properties( FFTW::fftw3f PROPERTIES
      IMPORTED_LOCATION                 "${FFTWF_LIBRARY}"
      INTERFACE_INCLUDE_DIRECTORIES     "${FFTW_INCLUDE_DIRS}"
      IMPORTED_LINK_INTERFACE_LANGUAGES "C" )
    if (ENABLE_FFTW_LONG_DOUBLE)
      set_target_properties( FFTW::fftw3l PROPERTIES
        IMPORTED_LOCATION                 "${FFTWL_LIBRARY}"
        INTERFACE_INCLUDE_DIRECTORIES     "${FFTW_INCLUDE_DIRS}"
        IMPORTED_LINK_INTERFACE_LANGUAGES "C" )
    endif()
    set_target_properties( FFTW::fftw3 PROPERTIES
      IMPORTED_LOCATION                 "${FFTW_LIBRARY}"
      INTERFACE_INCLUDE_DIRECTORIES     "${FFTW_INCLUDE_DIRS}"
      IMPORTED_LINK_INTERFACE_LANGUAGES "C" )
  endif()
endif()
