# Distributed under the OSI-approved BSD 3-Clause License.
# Copyright Stefano Sinigardi
#
#.rst:
# FindMKL
# --------
#
# Result Variables
# ^^^^^^^^^^^^^^^^
#
#  ``MKL_FOUND``
#    True if mkl is found
#
#  ``MKL_INCLUDE_DIR``
#    Location of MKL headers
#
#  ``MKL_LIBRARIES``
#    List of MKL libraries found, only if explicitly requested
#

include(FindPackageHandleStandardArgs)

set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)
find_package(OpenMP)
if (OPENMP_FOUND)
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
endif()

if($ENV{MKLROOT})
  file(TO_CMAKE_PATH $ENV{MKLROOT} MKLROOT_FIXED_PATH)
endif()
message(STATUS "ENV{MKLROOT}: $ENV{MKLROOT}")
message(STATUS "MKLROOT_FIXED_PATH: ${MKLROOT_FIXED_PATH}")
find_path(MKL_ROOT include/mkl.h PATHS $ENV{MKLROOT} ${MKLROOT_FIXED_PATH} DOC "Folder contains MKL")
find_path(MKL_INCLUDE_DIR mkl.h PATHS ${MKL_ROOT} PATH_SUFFIXES include)

set(MKL_FIND_LIBRARIES_REQUIRED OFF CACHE BOOL "Set to true to manually find MKL library files")

if (MKL_FIND_LIBRARIES_REQUIRED)
  #set(DLL_SUFFIX "_dll")
  #set(CMAKE_FIND_LIBRARY_SUFFIXES .a)

  #find_library(MKL_BLAS_ILP     NAMES mkl_blas95_ilp64               HINTS ${MKL_ROOT}/lib/intel64 NO_DEFAULT_PATH)
  #find_library(MKL_BLAS_LP      NAMES mkl_blas95_lp64                HINTS ${MKL_ROOT}/lib/intel64 NO_DEFAULT_PATH)
  find_library(MKL_CORE         NAMES mkl_core${DLL_SUFFIX}          HINTS ${MKL_ROOT}/lib/intel64 NO_DEFAULT_PATH)
  find_library(MKL_GNU_THREAD   NAMES mkl_gnu_thread                 HINTS ${MKL_ROOT}/lib/intel64 NO_DEFAULT_PATH)
  #find_library(MKL_INTEL_ILP    NAMES mkl_intel_ilp64${DLL_SUFFIX}   HINTS ${MKL_ROOT}/lib/intel64 NO_DEFAULT_PATH)
  find_library(MKL_INTEL_LP     NAMES mkl_intel_lp64${DLL_SUFFIX}    HINTS ${MKL_ROOT}/lib/intel64 NO_DEFAULT_PATH)
  #find_library(MKL_INTEL_THREAD NAMES mkl_intel_thread${DLL_SUFFIX}  HINTS ${MKL_ROOT}/lib/intel64 NO_DEFAULT_PATH)
  #find_library(MKL_LAPACK_ILP   NAMES mkl_lapack95_ilp64             HINTS ${MKL_ROOT}/lib/intel64 NO_DEFAULT_PATH)
  #find_library(MKL_LAPACK_LP    NAMES mkl_lapack95_lp64              HINTS ${MKL_ROOT}/lib/intel64 NO_DEFAULT_PATH)
  #find_library(MKL_PGI_THREAD   NAMES mkl_pgi_thread                 HINTS ${MKL_ROOT}/lib/intel64 NO_DEFAULT_PATH)
  #find_library(MKL_RT           NAMES mkl_rt                         HINTS ${MKL_ROOT}/lib/intel64 NO_DEFAULT_PATH)
  #find_library(MKL_SEQUENTIAL   NAMES mkl_sequential${DLL_SUFFIX}    HINTS ${MKL_ROOT}/lib/intel64 NO_DEFAULT_PATH)
  #find_library(MKL_TBB_THREAD   NAMES mkl_tbb_thread${DLL_SUFFIX}    HINTS ${MKL_ROOT}/lib/intel64 NO_DEFAULT_PATH)
  set(MKL_LIBRARIES ${MKL_BLAS_ILP} ${MKL_BLAS_LP} ${MKL_INTEL_ILP} ${MKL_INTEL_LP} ${MKL_LAPACK_ILP} ${MKL_LAPACK_LP} ${MKL_RT} ${MKL_SEQUENTIAL} ${MKL_TBB_THREAD} ${MKL_INTEL_THREAD} ${MKL_GNU_THREAD} ${MKL_PGI_THREAD} ${MKL_CORE} Threads::Threads ${CMAKE_DL_LIBS})

  #message(STATUS "MKL_LIBRARIES: ${MKL_LIBRARIES}")
endif()

find_package_handle_standard_args( MKL
  FOUND_VAR
    MKL_FOUND
  REQUIRED_VARS
    MKL_INCLUDE_DIR
    #MKL_BLAS_ILP
    #MKL_BLAS_LP
    #MKL_CORE
    #MKL_GNU_THREAD
    #MKL_INTEL_ILP
    #MKL_INTEL_LP
    #MKL_INTEL_THREAD
    #MKL_LAPACK_ILP
    #MKL_LAPACK_LP
    #MKL_PGI_THREAD
    #MKL_RT
    #MKL_SEQUENTIAL
    #MKL_TBB_THREAD
  )
