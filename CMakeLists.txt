cmake_minimum_required(VERSION 3.4)
project(ALaDyn)
enable_language(Fortran)
enable_language(CXX)
set(CMAKE_CXX_STANDARD 11)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules")
cmake_policy(SET CMP0074 NEW)
if(MSYS)
  list(INSERT CMAKE_MODULE_PATH 0 "${CMAKE_SOURCE_DIR}/cmake/msys2/")
endif()

# uncomment the following definition to enable GDB attach to debug while running
# add_definitions(-DENABLE_GDB_ATTACH)
if(NOT WIN32)
  add_definitions(-DUSE_GDB_ATTACH)
endif()

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
set(FORCE_FFTW OFF CACHE BOOL "Forcing FFTW libraries")

# set this variable to true to force static linking (if supported by the compiler)
set(FORCE_STATIC_LINK FALSE CACHE BOOL "Forcing static linking to libraries")

# make sure that the default is a RELEASE
set(default_build_type "Release")
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "ALaDyn: Setting build type to '${default_build_type}' as none was specified.")
  set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE
      STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
    "Debug" "Release" "MinSizeRel" "RelWithDebInfo" "Profiling" "Reports")
endif()

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

list(APPEND ALaDyn_Fortran_sources
  src/ALaDyn.F90
  src/cpp_lib/cpp_folder_tree.cpp
  src/cpp_lib/find_last_addr.cpp
  src/diagnostics/diag_part_and_fields.f90
  src/diagnostics/run_data_info.F90
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
  src/IO/pic_in.F90
  src/IO/init_laser_field.f90
  src/IO/init_part_distrib.f90
  src/IO/init_beam_part_distrib.f90
  src/IO/read_json.f90
  src/IO/read_namelist.f90
  src/IO/pic_out.f90
  src/IO/pic_out_util.f90
  src/IO/psolve.f90
  src/IO/tracking.f90
  src/particles/grid_part_lib.f90
  src/particles/grid_part_connect.F90
  src/particles/grid_part_util.f90
  src/particles/grid_tracking_part_connect.f90
  src/particles/interpolation_lib.f90
  src/parallel/mpi_var.f90
  src/parallel/mpi_part_interface.f90
  src/parallel/mpi_field_interface.f90
  src/parallel/mpi_curr_interface.f90
  src/parallel/parallel.F90
  src/start/pic_dump.f90
  src/start/read_input.F90
  src/start/set_init_param.f90
  src/start/start_all.F90
  src/start/system_utilities.f90
  src/structs/base_species.f90
  src/structs/grid_def.f90
  src/structs/index_array.f90
  src/structs/particles_aux_def.f90
  src/structs/particles_def.f90
  src/structs/struct_def.f90
  src/util/array_util.f90
  src/util/code_util.f90
  src/util/random_generator.f90
  src/util/util.F90
  src/util/warning.f90
  src/work/array_alloc.f90
  src/work/common_param.f90
  src/work/control_bunch_input.f90
  src/work/fstruct_data.f90
  src/work/memory_pool.f90
  src/work/phys_param.f90
  src/work/precision_def.F90
  src/work/pstruct_data.F90
  src/work/sim_params_types.f90
)

set(USING_PGI_COMPILER FALSE)
set(USING_GNU_COMPILER FALSE)
set(USING_INTEL_COMPILER FALSE)

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

set(jsonfortran-GNU_DIR ${JFORTRAN_BINARY_PATH})
set(jsonfortran-Intel_DIR ${JFORTRAN_BINARY_PATH})
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
  list(APPEND ALaDyn_Fortran_sources src/util/mkl_random_generator.f90)
  list(APPEND ALaDyn_Fortran_sources ${MKL_INCLUDE_DIR}/mkl_vsl.f90)
endif()

if(jsonfortran-${CMAKE_Fortran_COMPILER_ID}_FOUND)
  include_directories (
    ${jsonfortran_INCLUDE_DIRS}
  )
  message(STATUS "ALaDyn: JSON-Fortran has been found")
  add_definitions(-DJSON_FORTRAN)
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

if(jsonfortran-${CMAKE_Fortran_COMPILER_ID}_FOUND)
  target_link_libraries(ALaDyn
    jsonfortran-static
  )
endif()

set_target_properties(ALaDyn PROPERTIES LINKER_LANGUAGE Fortran)
string(TOLOWER ${CMAKE_Fortran_COMPILER_ID} EXE_SUFFIX)
set_target_properties(ALaDyn PROPERTIES OUTPUT_NAME ALaDyn_${EXE_SUFFIX})

install(TARGETS ALaDyn DESTINATION ${CMAKE_SOURCE_DIR}/bin/)
