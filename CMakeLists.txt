cmake_minimum_required(VERSION 3.15)

if(DEFINED ENV{VCPKG_DEFAULT_TRIPLET})
  message(STATUS "Setting default vcpkg target triplet to $ENV{VCPKG_DEFAULT_TRIPLET}")
  set(VCPKG_TARGET_TRIPLET $ENV{VCPKG_DEFAULT_TRIPLET})
endif()

if(CMAKE_SYSTEM_PROCESSOR MATCHES "^x86" OR CMAKE_SYSTEM_PROCESSOR MATCHES "^AMD64")
  set(IS_X86 TRUE)
else()
  set(IS_X86 FALSE)
endif()

if(ENABLE_VCPKG_INTEGRATION AND DEFINED ENV{VCPKG_ROOT} AND NOT DEFINED CMAKE_TOOLCHAIN_FILE)
  set(CMAKE_POLICY_DEFAULT_CMP0077 NEW)
  set(X_VCPKG_APPLOCAL_DEPS_INSTALL ON)
  set(CMAKE_TOOLCHAIN_FILE "$ENV{VCPKG_ROOT}/scripts/buildsystems/vcpkg.cmake" CACHE STRING "")
  #set(_VCPKG_INSTALLED_DIR ${CMAKE_CURRENT_LIST_DIR}/vcpkg CACHE STRING "")  #folder for manifest-installed dependencies
  message(STATUS "VCPKG found: $ENV{VCPKG_ROOT}")
  message(STATUS "Using VCPKG integration")
  if(VCPKG_MANIFEST_FEATURES)
    message(STATUS "VCPKG_MANIFEST_FEATURES: ${VCPKG_MANIFEST_FEATURES}")
  endif()
elseif(DEFINED CMAKE_TOOLCHAIN_FILE)
  message(STATUS "Using toolchain: ${CMAKE_TOOLCHAIN_FILE}")
  if(CMAKE_TOOLCHAIN_FILE MATCHES "vcpkg.cmake")
    message(STATUS "Toolchain uses VCPKG integration")
    if(VCPKG_MANIFEST_FEATURES)
      message(STATUS "VCPKG_MANIFEST_FEATURES: ${VCPKG_MANIFEST_FEATURES}")
    endif()
  endif()
else()
  message(WARNING "vcpkg not found, toolchain not defined, build might fail")
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/vcpkg.json)
  file(READ ${CMAKE_CURRENT_SOURCE_DIR}/vcpkg.json VCPKG_JSON_STRING)
  string(JSON VERSION_STRING GET ${VCPKG_JSON_STRING} version)
else()
  set(VERSION_STRING "0.0.0.1")
endif()

string(REPLACE "." ";" VERSION_LIST ${VERSION_STRING})
list(LENGTH VERSION_LIST VERSION_LIST_LENGTH)
if(VERSION_LIST_LENGTH LESS 3)
  message(FATAL_ERROR "ALaDyn needs at least major.minor.patch version numbers to properly configure")
endif()
list(GET VERSION_LIST 0 ALaDyn_MAJOR_VERSION)
list(GET VERSION_LIST 1 ALaDyn_MINOR_VERSION)
list(GET VERSION_LIST 2 ALaDyn_PATCH_VERSION)
if(VERSION_LIST_LENGTH GREATER 3)
  list(GET VERSION_LIST 3 ALaDyn_TWEAK_VERSION)
else()
  set(ALaDyn_TWEAK_VERSION 0)
endif()

set(ALaDyn_VERSION ${ALaDyn_MAJOR_VERSION}.${ALaDyn_MINOR_VERSION}.${ALaDyn_PATCH_VERSION}.${ALaDyn_TWEAK_VERSION})
message("ALaDyn_VERSION: ${ALaDyn_VERSION}")

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
if(MSYS)
    set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/msys2/" ${CMAKE_MODULE_PATH})
endif()

if(APPLE)
    set(MACOSX_RPATH TRUE)
endif(APPLE)

# you can force FFTW if necessary, avoiding MKL (which is preferred on Intel systems)
option(FORCE_FFTW "Forcing FFTW libraries" OFF)

# forcing old mpif.h if the correct version of the mpi module is not available
set(FORCE_OLD_MPI OFF CACHE BOOL "Forcing old mpif.h linking")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

project(ALaDyn
    VERSION ${ALaDyn_VERSION}
    LANGUAGES Fortran CXX
)

add_executable(ALaDyn src/ALaDyn.F90)

if(UNIX
   AND CMAKE_CXX_COMPILER_ID STREQUAL "PGI"
   AND NOT APPLE)
    find_package(Numa REQUIRED)
    target_link_libraries(ALaDyn PRIVATE ${NUMA_LIBRARY})
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "Intel" AND NOT FORCE_FFTW)
    find_package(MKL REQUIRED)
    set(MKL_FFTW_INCLUDE_DIR "${MKL_INCLUDE_DIR}/fftw/")
    target_include_directories(ALaDyn PRIVATE ${MKL_INCLUDE_DIR}
                                              ${MKL_FFTW_INCLUDE_DIR})
    target_link_libraries(ALaDyn PRIVATE ${MKL_LIBRARIES})
else()
    find_package(FFTW REQUIRED OPTIONAL_COMPONENTS MPI)
    target_include_directories(ALaDyn PRIVATE ${FFTW_INCLUDE_DIR})
    target_link_libraries(ALaDyn PRIVATE ${FFTW_LIBRARY})
    if(FFTW_MPI_FOUND)
        target_include_directories(ALaDyn PRIVATE ${FFTW_MPI_INCLUDE_DIR})
        target_link_libraries(ALaDyn PRIVATE ${FFTW_MPI_LIBRARY})
    endif()
endif()

# Add the source files
add_subdirectory(src)

message(STATUS "ALaDyn: Using ${CMAKE_Fortran_COMPILER_ID} Compiler")
if(CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
    string(REGEX REPLACE "-fPIE" "" CMAKE_Fortran_FLAGS_RELEASE
                         ${CMAKE_Fortran_FLAGS_RELEASE})
    string(REGEX REPLACE "-fPIE" "" CMAKE_Fortran_FLAGS_DEBUG
                         ${CMAKE_Fortran_FLAGS_DEBUG})
    string(REGEX REPLACE "-fPIE" "" CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS})
    string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -r8 ")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -r8 -g ")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -fdefault-real-8")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -fdefault-real-8 -Wall -Wextra")
    string(APPEND CMAKE_Fortran_FLAGS_PROFILING
           " -fdefault-real-8 -g -pg -Wall -fcheck='all' ")

    if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10.0)
        message(
            STATUS
                "Adding argument mismatch tolerance flag and invalid boz, for MPICH compatibility"
        )
        string(APPEND CMAKE_Fortran_FLAGS_RELEASE
               " -fallow-argument-mismatch -fallow-invalid-boz")
        string(APPEND CMAKE_Fortran_FLAGS_DEBUG
               " -fallow-argument-mismatch -fallow-invalid-boz")
        string(APPEND CMAKE_Fortran_FLAGS_PROFILING
               " -fallow-argument-mismatch -fallow-invalid-boz")
    else()
        message(
            STATUS
                "No need to add argument mismatch tolerance flag and invalid boz"
        )
    endif()

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -real-size 64")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -real-size 64 -diag-enable warn")
    string(
        APPEND
        CMAKE_Fortran_FLAGS_PROFILING
        "-g -O2 -qopt-report=5 -qopt-report-file=comp_report_out -qopt-report-per-object -real-size 64"
    )
    if(FORCE_FFTW)
        message(STATUS "ALaDyn: Forcing FFTW, avoid MKL even if found")
    else()
        message(STATUS "ALaDyn: MKL is preferred to FFTW, if found")
        string(APPEND CMAKE_Fortran_LINK_FLAGS " -mkl")
    endif()
else()
    message(
        STATUS "ALaDyn: Compiler ${CMAKE_Fortran_COMPILER} found, unsupported")
    message(STATUS "ALaDyn: Let's try using it...")
endif()

# Check CXX compiler
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    string(APPEND CMAKE_CXX_COMPILE_FLAGS " -Wall")
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
    string(APPEND CMAKE_CXX_COMPILE_FLAGS " -diag-enable warn")
endif()

set_target_properties(ALaDyn PROPERTIES LINKER_LANGUAGE Fortran)

install(TARGETS ALaDyn DESTINATION bin)
