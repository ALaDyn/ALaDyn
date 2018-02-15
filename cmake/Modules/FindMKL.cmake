# Find the MKL includes
#
#   MKL_FOUND            : True if mkl is found
#   MKL_INCLUDE_DIR      : include directory


if(DEFINED $ENV{INTELROOT})
 set(INTELROOT $ENV{INTELROOT})
else()
 set(INTELROOT /opt/intel)
endif()

set(INTEL_ROOT $INTELROOT CACHE PATH "Folder contains intel libs")
find_path(MKL_ROOT include/mkl.h PATHS $ENV{MKLROOT} ${INTEL_ROOT}/mkl  DOC "Folder contains MKL")

find_path(MKL_INCLUDE_DIR mkl.h PATHS ${MKL_ROOT} PATH_SUFFIXES include)
set(__looked_for MKL_INCLUDE_DIR)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(MKL DEFAULT_MSG ${__looked_for})
