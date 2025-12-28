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

# Old species (provisional)
# add_definitions(-DOLD_SPECIES)

message(STATUS "ALaDyn: Building using the generator ${CMAKE_GENERATOR}")
# Look for JSON-FORTRAN
set(JSON_FORTRAN_SUPPORT TRUE CACHE BOOL "ALaDyn: Building ALaDyn with JSON-Fortran support")
if( JSON_FORTRAN_SUPPORT )
  message(STATUS "ALaDyn: Building ALaDyn with JSON-Fortran support")
  set(JFORTRAN_BASE_PATH ${CMAKE_SOURCE_DIR}/third_party/json-fortran)
  set(JFORTRAN_PATH ${JFORTRAN_BASE_PATH}/json-fortran_source)
  set(JFORTRAN_LOCALS_PATH ${JFORTRAN_BASE_PATH}/json-fortran_locals)
  set(JFORTRAN_SOURCE_PATH ${JFORTRAN_PATH}/src)
  set(JFORTRAN_BINARY_PATH ${JFORTRAN_PATH}/build)
  find_package(Git QUIET)
  if(GIT_FOUND AND EXISTS "${PROJECT_SOURCE_DIR}/.git")
  # Update submodules as needed
      option(GIT_SUBMODULE "Check submodules during build" ON)
      if(GIT_SUBMODULE)
          message(STATUS "ALaDyn: Submodule update")
          execute_process(COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive
                          WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                          RESULT_VARIABLE GIT_SUBMOD_RESULT)
          if(NOT GIT_SUBMOD_RESULT EQUAL "0")
              message(FATAL_ERROR "git submodule update --init failed with ${GIT_SUBMOD_RESULT}, please checkout submodules")
          endif()
      endif()
  endif()

  if(NOT EXISTS "${JFORTRAN_PATH}/CMakeLists.txt")
      message(FATAL_ERROR "The submodules were not downloaded! GIT_SUBMODULE was turned off or failed. Please update submodules and try again.")
  endif()

  if( EXISTS ${JFORTRAN_PATH} AND NOT EXISTS ${JFORTRAN_LOCALS_PATH})
    message(STATUS "JSON-Fortran: Now building the JSON-Fortran library")
    file(MAKE_DIRECTORY ${JFORTRAN_BINARY_PATH})
    set(CMAKE_INSTALL_MESSAGE LAZY)
    # Configure subproject into ${JFORTRAN_BINARY_PATH}
    execute_process(COMMAND ${CMAKE_COMMAND}
    -DCMAKE_INSTALL_MESSAGE=LAZY
    -DCMAKE_INSTALL_PREFIX=${JFORTRAN_LOCALS_PATH}
    -DSKIP_DOC_GEN=TRUE
    -G "${CMAKE_GENERATOR}"
    ${JFORTRAN_PATH}
    WORKING_DIRECTORY ${JFORTRAN_BINARY_PATH})
    # Building and installing
    execute_process(COMMAND ${CMAKE_COMMAND}
    --build ${JFORTRAN_BINARY_PATH}
    --target install)
    message(STATUS "JSON-Fortran: Built")
  endif()
endif()

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

get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
if(CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
  message(STATUS "ALaDyn: Using PGI Compiler")
  set(USING_PGI_COMPILER TRUE)
  add_definitions(-DUSING_PGI_COMPILER)
  string(REGEX REPLACE "-fPIE" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
  string(REGEX REPLACE "-fPIE" "" CMAKE_Fortran_FLAGS_DEBUG ${CMAKE_Fortran_FLAGS_DEBUG})
  string(REGEX REPLACE "-fPIE" "" CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS})
  string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -r8 ")
  string(APPEND CMAKE_Fortran_FLAGS_DEBUG   " -r8 -g ")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  message(STATUS "ALaDyn: Using GCC Compiler")
  set(USING_GNU_COMPILER TRUE)
  add_definitions(-DUSING_GNU_COMPILER)
  string(APPEND CMAKE_CXX_COMPILE_FLAGS " -Wall")
  string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -fdefault-real-8 -funroll-all-loops -O3")
  string(APPEND CMAKE_Fortran_FLAGS_DEBUG   " -fdefault-real-8 -Wall -Wextra -fbacktrace -fcheck=bounds -O0 -g")
  string(APPEND CMAKE_Fortran_FLAGS_PROFILING " -fdefault-real-8 -g -O2 -pg -Wall")
  if(MARCONI_KNL OR CORI_KNL)
    string(APPEND CMAKE_CXX_COMPILE_FLAGS " -march=knl -mavx512f -mavx512cd -mavx512er -mavx512pf")
    string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -march=knl -mavx512f -mavx512cd -mavx512er -mavx512pf")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG   " -march=knl -mavx512f -mavx512cd -mavx512er -mavx512pf")
  elseif(CORI_HASWELL)
    string(APPEND CMAKE_CXX_COMPILE_FLAGS " -march=core-avx2 -mavx2")
    string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -march=core-avx2 -mavx2")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG   " -march=core-avx2 -mavx2")
  endif()
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10.0)
    message(STATUS "Adding argument mismatch tolerance flag and invalid boz, for MPICH compatibility")
    string(APPEND CMAKE_Fortran_FLAGS_RELEASE   " -fallow-argument-mismatch -fallow-invalid-boz")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG     " -fallow-argument-mismatch -fallow-invalid-boz")
    string(APPEND CMAKE_Fortran_FLAGS_PROFILING " -fallow-argument-mismatch -fallow-invalid-boz")
  else()
    message(STATUS "No need to add argument mismatch tolerance flag and invalid boz")
  endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  message(STATUS "ALaDyn: Using Intel Compiler")
  set(USING_INTEL_COMPILER TRUE)
  add_definitions(-DUSING_INTEL_COMPILER)
  string(APPEND CMAKE_CXX_COMPILE_FLAGS " -diag-enable warn")
  string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -real-size 64")
  string(APPEND CMAKE_Fortran_FLAGS_DEBUG   " -real-size 64 -diag-enable warn -g")
  string(APPEND CMAKE_Fortran_FLAGS_PROFILING "-g -qopt-report=5 -qopt-report-file=comp_report_out -qopt-report-per-object -real-size 64")
  string(APPEND CMAKE_Fortran_FLAGS_REPORTS " -qopt-report=2 -qopt-report-phase=vec,loop -real-size 64")
  if(MARCONI_KNL OR CORI_KNL)
    string(APPEND CMAKE_CXX_COMPILE_FLAGS " -xMIC-AVX512")
    string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -xMIC-AVX512 -O3")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG   " -xMIC-AVX512 -check all -fpe0 -warn -traceback -debug extended -Og")
    string(APPEND CMAKE_Fortran_FLAGS_PROFILING " -xMIC-AVX512 -O2")
    string(APPEND CMAKE_Fortran_FLAGS_REPORTS " -xMIC-AVX512 -O2")
    string(APPEND CMAKE_Fortran_LINK_FLAGS " -lstdc++")
  elseif(CORI_HASWELL)
    string(APPEND CMAKE_CXX_COMPILE_FLAGS " -xCORE-AVX2")
    string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -xCORE-AVX2 -O3")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG   " -xCORE-AVX2 -check all -fpe0 -warn -traceback -debug all -O0")
    string(APPEND CMAKE_Fortran_FLAGS_PROFILING " -xCORE-AVX2 -O2")
    string(APPEND CMAKE_Fortran_FLAGS_REPORTS " -xCORE-AVX2 -O2")
    string(APPEND CMAKE_Fortran_LINK_FLAGS " -lstdc++")
  else()
    string(APPEND CMAKE_CXX_COMPILE_FLAGS " -axcore-avx-i,SSE4.2")
    string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -axcore-avx-i,SSE4.2 -O3")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG   " -check all -fpe0 -warn -traceback -debug extended -Og")
    string(APPEND CMAKE_Fortran_LINK_FLAGS " -lstdc++")
  endif()
  if(FORCE_FFTW)
    message(STATUS "ALaDyn: Forcing FFTW, avoid MKL even if found")
  else()
    message(STATUS "ALaDyn: MKL is preferred to FFTW, if found")
    string(APPEND CMAKE_Fortran_LINK_FLAGS " -mkl")
  endif()
else()
  if(MARCONI_KNL OR CORI_HASWELL OR CORI_KNL)
    error("ALaDyn: Unsupported compiler for KNL")
  else()
    message(STATUS "ALaDyn: Compiler ${CMAKE_Fortran_COMPILER} found, unsupported")
    message(STATUS "ALaDyn: Let's try using it...")
  endif()
endif()

if(WIN32 AND USING_PGI_COMPILER)
  find_package(MPI COMPONENTS C)
  set(FORCE_OLD_MPI ON CACHE BOOL "Forcing old mpif.h linking" FORCE)
else()
  find_package(MPI COMPONENTS Fortran)
endif()

if(FORCE_OLD_MPI)
  message(STATUS "ALaDyn: MPI libraries are linked against mpif.h")
  add_definitions(-DFORCE_OLD_MPI)
endif()
if(FORCE_STATIC_LINK)
  string(APPEND CMAKE_Fortran_LINK_FLAGS " -static")
endif()

if(UNIX AND USING_PGI_COMPILER AND NOT APPLE)
  find_package(Numa REQUIRED)
endif()

add_definitions(-DUSE_FILESYSTEM)
set(CMAKE_CXX_STANDARD 17)

if(USING_INTEL_COMPILER AND NOT FORCE_FFTW)
  find_package(MKL REQUIRED)
  set(MKL_FFTW_INCLUDE_DIR "${MKL_INCLUDE_DIR}/fftw/")
else()
  find_package(FFTW REQUIRED OPTIONAL_COMPONENTS MPI)
endif()

set(jsonfortran-GNU_DIR ${JFORTRAN_LOCALS_PATH}/jsonfortran-gnu-8.1.0/cmake)
set(jsonfortran-Intel_DIR ${JFORTRAN_LOCALS_PATH}/jsonfortran-intel-8.1.0/cmake)
if(JSON_FORTRAN_SUPPORT)
  find_package ( jsonfortran-${CMAKE_Fortran_COMPILER_ID} 8.1.0 REQUIRED)
endif()

string(APPEND CMAKE_CXX_COMPILE_FLAGS " ${MPI_COMPILE_FLAGS}")
string(APPEND CMAKE_CXX_LINK_FLAGS " ${MPI_LINK_FLAGS}")
string(APPEND CMAKE_Fortran_COMPILE_FLAGS " ${MPI_COMPILE_FLAGS} ${MPI_Fortran_COMPILE_FLAGS}")
string(APPEND CMAKE_Fortran_LINK_FLAGS " ${MPI_LINK_FLAGS} ${MPI_Fortran_LINK_FLAGS}")

include_directories(
  ${OTHER_INCLUDE_DIR}
  ${MPI_INCLUDE_PATH}
  ${MPI_Fortran_INCLUDE_PATH}
  ${FFTW_INCLUDE_DIR}
  ${MKL_INCLUDE_DIR}
  ${MKL_FFTW_INCLUDE_DIR}
  ${Boost_INCLUDE_DIR}
)

if(FFTW_MPI_FOUND)
  include_directories(
   ${FFTW_MPI_INCLUDE_DIR}
  )
endif()

if(MKL_FOUND)
  add_definitions(-DUSE_MKL)
endif()

if(jsonfortran-${CMAKE_Fortran_COMPILER_ID}_FOUND)
  include_directories (
    ${jsonfortran_INCLUDE_DIRS}
  )
  message(STATUS "ALaDyn: JSON-Fortran has been found")
  add_definitions(-DJSON_FORTRAN)
endif()

# Add the source files from subdirectories
add_subdirectory(src)

# Add MKL sources if found
if(MKL_FOUND)
  target_sources(ALaDyn PRIVATE src/util/mkl_random_generator.f90 ${MKL_INCLUDE_DIR}/mkl_vsl.f90)
endif()

if(FFTW_MPI_LIBRARY)
  target_link_libraries(ALaDyn PRIVATE
    ${FFTW_MPI_LIBRARY}
  )
endif()

if(FFTW_LIBRARY)
  target_link_libraries(ALaDyn PRIVATE
    ${FFTW_LIBRARY}
  )
endif()

if(MKL_LIBRARIES)
  target_link_libraries(ALaDyn PRIVATE
    ${MKL_LIBRARIES}
  )
endif()

if(MPI_Fortran_LIBRARIES)
  target_link_libraries(ALaDyn PRIVATE
    ${MPI_Fortran_LIBRARIES}
  )
endif()

if(MPI_LIBRARIES)
  target_link_libraries(ALaDyn PRIVATE
    ${MPI_LIBRARIES}
  )
endif()

if(Boost_LIBRARIES)
  target_link_libraries(ALaDyn PRIVATE
    ${Boost_LIBRARIES}
  )
endif()

if(NUMA_LIBRARY)
  target_link_libraries(ALaDyn PRIVATE
    ${NUMA_LIBRARY}
  )
endif()

if(jsonfortran-${CMAKE_Fortran_COMPILER_ID}_FOUND)
  target_link_libraries(ALaDyn PRIVATE
    jsonfortran-static
  )
endif()

set_target_properties(ALaDyn PROPERTIES LINKER_LANGUAGE Fortran)
string(TOLOWER ${CMAKE_Fortran_COMPILER_ID} EXE_SUFFIX)
set_target_properties(ALaDyn PROPERTIES OUTPUT_NAME ALaDyn_${EXE_SUFFIX})

install(TARGETS ALaDyn DESTINATION bin)

# Testing support
# Support both BUILD_TESTING (CMake standard) and ENABLE_TEST (build.ps1 flag)
option(BUILD_TESTING "Build test suite" ON)
option(ENABLE_TEST "Build test suite (alias for BUILD_TESTING)" OFF)
if(ENABLE_TEST)
    set(BUILD_TESTING ON CACHE BOOL "Build test suite" FORCE)
endif()
if(BUILD_TESTING)
    message(STATUS "ALaDyn: Building test suite")
    # Enable CTest in root directory so tests are discoverable
    enable_testing()
    add_subdirectory(tests)
endif()
