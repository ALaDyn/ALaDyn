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

include(FindPackageHandleStandardArgs)

find_path(MKL_ROOT include/mkl.h PATHS $ENV{MKLROOT} ${INTEL_ROOT}/mkl  DOC "Folder contains MKL")
find_path(MKL_INCLUDE_DIR mkl.h PATHS ${MKL_ROOT} PATH_SUFFIXES include)

find_package_handle_standard_args( MKL
  FOUND_VAR
    MKL_FOUND
  REQUIRED_VARS
    MKL_INCLUDE_DIR
  )
