cmake_minimum_required(VERSION 3.10)

if(DEFINED ENV{VCPKG_DEFAULT_TRIPLET})
  message(STATUS "Setting default vcpkg target triplet to $ENV{VCPKG_DEFAULT_TRIPLET}")
  set(VCPKG_TARGET_TRIPLET $ENV{VCPKG_DEFAULT_TRIPLET})
endif()

if(DEFINED ENV{VCPKG_ROOT} AND NOT DEFINED CMAKE_TOOLCHAIN_FILE)
  set(CMAKE_TOOLCHAIN_FILE "$ENV{VCPKG_ROOT}/scripts/buildsystems/vcpkg.cmake" CACHE STRING "")
  message(STATUS "VCPKG found: $ENV{VCPKG_ROOT}")
  message(STATUS "Using VCPKG integration")
  message(STATUS "VCPKG_MANIFEST_FEATURES: ${VCPKG_MANIFEST_FEATURES}")
elseif(DEFINED CMAKE_TOOLCHAIN_FILE)
  message(STATUS "Using toolchain: ${CMAKE_TOOLCHAIN_FILE}")
  if(CMAKE_TOOLCHAIN_FILE MATCHES "vcpkg.cmake")
    message(STATUS "Toolchain uses VCPKG integration")
    message(STATUS "VCPKG_MANIFEST_FEATURES: ${VCPKG_MANIFEST_FEATURES}")
  endif()
else()
  message(WARNING "vcpkg not found, toolchain not defined, build might fail")
endif()

project(ALaDyn)
enable_language(Fortran)
enable_language(CXX)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
if(MSYS)
  set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/msys2/" ${CMAKE_MODULE_PATH})
endif()

# uncomment the following definition to enable GDB attach to debug while running
# add_definitions(-DENABLE_GDB_ATTACH)
if(NOT WIN32)
  add_definitions(-DUSE_GDB_ATTACH)
endif()

# you can force FFTW if necessary, avoiding MKL (which is preferred on Intel systems)
set(FORCE_FFTW OFF CACHE BOOL "Forcing FFTW libraries")

# set this variable to true to force static linking (if supported by the compiler)
set(FORCE_STATIC_LINK FALSE CACHE BOOL "Forcing static linking to libraries")

# forcing old mpif.h if the correct version of the mpi module is not available
set(FORCE_OLD_MPI OFF CACHE BOOL "Forcing old mpif.h linking")

# make sure that the default is a RELEASE
set(default_build_type "Release")
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "ALaDyn: Setting build type to '${default_build_type}' as none was specified.")
  set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE
      STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
    "Debug" "Release" "MinSizeRel" "RelWithDebInfo" "Profiling")
endif()

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

list(APPEND ALaDyn_Fortran_sources
  src/ALaDyn.F90
  src/cpp_lib/cpp_folder_tree.cpp
  src/cpp_lib/find_last_addr.cpp
  src/diagnostics/diag_part_and_fields.f90
  src/diagnostics/run_data_info.f90
  src/dynamics/boris_push.f90
  src/dynamics/curr_and_fields_util.f90
  src/dynamics/env_evolve_in_time.f90
  src/dynamics/fluid_density_momenta.f90
  src/dynamics/pic_evolve_in_time.f90
  src/dynamics/window.f90
  src/fft/legacy_fft_lib.F90
  src/fft/modern_fft_lib.F90
  src/fft/prl_fft.f90
  src/fields/grid_field_param.f90
  src/fields/grid_fields.f90
  src/fields/init_grid_fields.f90
  src/grid/grid_param.f90
  src/grid/set_grid_param.f90
  src/grid/stretched_grid.f90
  src/ionization/ionize.f90
  src/ionization/ionz_data.f90
  src/IO/pic_in.f90
  src/IO/init_laser_field.f90
  src/IO/init_part_distrib.f90
  src/IO/init_beam_part_distrib.f90
  src/IO/pic_out.f90
  src/IO/pic_out_util.f90
  src/IO/psolve.f90
  src/particles/grid_part_lib.f90
  src/particles/grid_part_connect.f90
  src/particles/grid_part_util.f90
  src/parallel/mpi_var.f90
  src/parallel/mpi_part_interface.f90
  src/parallel/mpi_field_interface.f90
  src/parallel/mpi_curr_interface.f90
  src/parallel/parallel.F90
  src/start/pic_dump.f90
  src/start/read_input.f90
  src/start/set_init_param.f90
  src/start/start_all.F90
  src/start/system_utilities.f90
  src/work/array_alloc.f90
  src/work/code_util.f90
  src/work/common_param.f90
  src/work/control_bunch_input.f90
  src/work/fstruct_data.f90
  src/work/phys_param.f90
  src/work/precision_def.F90
  src/work/pstruct_data.f90
  src/work/struct_def.f90
  src/work/util.f90
)

set(USING_PGI_COMPILER FALSE)
set(USING_GNU_COMPILER FALSE)
set(USING_INTEL_COMPILER FALSE)

get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
if(Fortran_COMPILER_NAME MATCHES "pgf*")
  message(STATUS "ALaDyn: Using PGI Compiler")
  set(USING_PGI_COMPILER TRUE)
  string(REGEX REPLACE "-fPIE" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
  string(REGEX REPLACE "-fPIE" "" CMAKE_Fortran_FLAGS_DEBUG ${CMAKE_Fortran_FLAGS_DEBUG})
  string(REGEX REPLACE "-fPIE" "" CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS})
  string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -r8 ")
  string(APPEND CMAKE_Fortran_FLAGS_DEBUG   " -r8 -g ")
elseif(Fortran_COMPILER_NAME MATCHES "gfor*")
  message(STATUS "ALaDyn: Using GCC Compiler")
  set(USING_GNU_COMPILER TRUE)
  string(APPEND CMAKE_CXX_COMPILE_FLAGS       " -Wall")
  string(APPEND CMAKE_Fortran_FLAGS_RELEASE   " -fdefault-real-8")
  string(APPEND CMAKE_Fortran_FLAGS_DEBUG     " -fdefault-real-8 -Wall -Wextra")
  string(APPEND CMAKE_Fortran_FLAGS_PROFILING " -fdefault-real-8 -g -pg -Wall -fcheck='all' ")
  if(MARCONI_KNL)
    string(APPEND CMAKE_CXX_COMPILE_FLAGS       " -mavx512f -mavx512cd -mavx512er -mavx512pf")
    string(APPEND CMAKE_Fortran_FLAGS_RELEASE   " -funroll-all-loops  -mavx512f -mavx512cd -mavx512er -mavx512pf -O3")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG     " -fbacktrace -fbounds-check -mavx512f -mavx512cd -mavx512er -mavx512pf -Og -g")
    string(APPEND CMAKE_Fortran_FLAGS_PROFILING " -funroll-all-loops  -mavx512f -mavx512cd -mavx512er -mavx512pf -O3")
  else()
    string(APPEND CMAKE_Fortran_FLAGS_RELEASE   " -funroll-all-loops -O3")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG     " -fbacktrace -fbounds-check -O0 -g")
    string(APPEND CMAKE_Fortran_FLAGS_PROFILING " -funroll-all-loops -O3 ")
  endif()
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10.0)
    message(STATUS "Adding argument mismatch tolerance flag and invalid boz, for MPICH compatibility")
    string(APPEND CMAKE_Fortran_FLAGS_RELEASE   " -fallow-argument-mismatch -fallow-invalid-boz")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG     " -fallow-argument-mismatch -fallow-invalid-boz")
    string(APPEND CMAKE_Fortran_FLAGS_PROFILING " -fallow-argument-mismatch -fallow-invalid-boz")
  else()
    message(STATUS "No need to add argument mismatch tolerance flag and invalid boz")
  endif()

elseif(Fortran_COMPILER_NAME MATCHES "ifor*" OR Fortran_COMPILER_NAME MATCHES "ftn*")
  message(STATUS "ALaDyn: Using Intel Compiler")
  set(USING_INTEL_COMPILER TRUE)
  string(APPEND CMAKE_CXX_COMPILE_FLAGS " -diag-enable warn")
  string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -real-size 64")
  string(APPEND CMAKE_Fortran_FLAGS_DEBUG   " -real-size 64 -diag-enable warn")
  string(APPEND CMAKE_Fortran_FLAGS_PROFILING "-g -O2 -qopt-report=5 -qopt-report-file=comp_report_out -qopt-report-per-object -real-size 64")
  if(MARCONI_KNL)
    string(APPEND CMAKE_CXX_COMPILE_FLAGS " -xMIC-AVX512")
    string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -xMIC-AVX512 -O3")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG   " -xMIC-AVX512 -check all -fpe0 -warn -traceback -debug extended -O0 -g")
    string(APPEND CMAKE_Fortran_LINK_FLAGS " -lstdc++")
  else()
    string(APPEND CMAKE_CXX_COMPILE_FLAGS " -axcore-avx-i,SSE4.2")
    string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -axcore-avx-i,SSE4.2 -O3")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG   " -check all -fpe0 -warn -traceback -debug extended -O0 -g")
    string(APPEND CMAKE_Fortran_LINK_FLAGS " -lstdc++")
  endif()
  if(FORCE_FFTW)
    message(STATUS "ALaDyn: Forcing FFTW, avoid MKL even if found")
  else()
    message(STATUS "ALaDyn: MKL is preferred to FFTW, if found")
    string(APPEND CMAKE_Fortran_LINK_FLAGS " -mkl")
  endif()
else()
  if(MARCONI_KNL)
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

if(NOT WIN32)
  find_package(Boost COMPONENTS system filesystem)
  if(Boost_FOUND)
    add_definitions(-DUSE_BOOST)
  else()
    add_definitions(-DUSE_FILESYSTEM)
    set(CMAKE_CXX_STANDARD 17)
  endif()
endif()

if(USING_INTEL_COMPILER AND NOT FORCE_FFTW)
  find_package(MKL REQUIRED)
  set(MKL_FFTW_INCLUDE_DIR "${MKL_INCLUDE_DIR}/fftw/")
else()
  find_package(FFTW REQUIRED OPTIONAL_COMPONENTS MPI)
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
add_executable(ALaDyn
  ${ALaDyn_Fortran_sources}
)

add_library(cpp_folder_tree STATIC src/cpp_lib/cpp_folder_tree.cpp)
add_library(find_last_addr STATIC src/cpp_lib/find_last_addr.cpp)
add_library(enable_gdb_attach STATIC src/cpp_lib/enable_gdb_attach.cpp)

target_link_libraries(ALaDyn
  cpp_folder_tree
  find_last_addr
  enable_gdb_attach
)

if(FFTW_MPI_LIBRARY)
  target_link_libraries(ALaDyn
    ${FFTW_MPI_LIBRARY}
  )
endif()

if(FFTW_LIBRARY)
  target_link_libraries(ALaDyn
    ${FFTW_LIBRARY}
  )
endif()

if(MKL_LIBRARIES)
  target_link_libraries(ALaDyn
    ${MKL_LIBRARIES}
  )
endif()

if(MPI_Fortran_LIBRARIES)
  target_link_libraries(ALaDyn
    ${MPI_Fortran_LIBRARIES}
  )
endif()

if(MPI_LIBRARIES)
  target_link_libraries(ALaDyn
    ${MPI_LIBRARIES}
  )
endif()

if(Boost_LIBRARIES)
  target_link_libraries(ALaDyn
    ${Boost_LIBRARIES}
  )
endif()

if(NUMA_LIBRARY)
  target_link_libraries(ALaDyn
    ${NUMA_LIBRARY}
  )
endif()

set_target_properties(ALaDyn PROPERTIES LINKER_LANGUAGE Fortran)

install(TARGETS ALaDyn DESTINATION ${CMAKE_SOURCE_DIR}/bin/)
