cmake_minimum_required(VERSION 3.4)
project(ALaDyn)
enable_language(Fortran)
enable_language(CXX)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# uncomment the following definition to enable GDB attach to debug while running
# add_definitions(-DENABLE_GDB_ATTACH)
if(NOT WIN32)
  add_definitions(-DUSE_GDB_ATTACH)
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
    "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

list(APPEND ALaDyn_Fortran_sources
  src/ALaDyn.F90
  src/precision_def.F90
  src/mpi_var.f90
  src/phys_param.f90
  src/grid_and_particles.f90
  src/code_util.f90
  src/system_utilities.f90
  src/util.f90
  src/fft_lib.F90
  src/struct_def.f90
  src/grid_param.f90
  src/control_bunch_input.f90
  src/ionz_data.f90
  src/ionize.f90
  src/pstruct_data.F90
  src/fstruct_data.f90
  src/psolv.f90
  src/all_param.f90
  src/particles.F90
  src/parallel.F90
  src/pic_rutil.f90
  src/der_lib.f90
  src/grid_fields.f90
  src/pdf_moments.f90
  src/pic_in.f90
  src/pic_out.f90
  src/pic_out_util.f90
  src/pic_dump.f90
  src/pic_evolve_in_time.f90
  src/pwfa_bunch_field_calculation.F90
  src/read_input.f90
  src/pwfa_output_addons.f90
)

set(USING_PGI_COMPILER FALSE)
set(USING_GNU_COMPILER FALSE)
set(USING_INTEL_COMPILER FALSE)

get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
if(Fortran_COMPILER_NAME MATCHES "pgf*")
  message(STATUS "ALaDyn: Using PGI Compiler (experimental)")
  set(USING_PGI_COMPILER TRUE)
  string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -r8 ")
  string(APPEND CMAKE_Fortran_FLAGS_DEBUG   " -r8 -g ")
elseif(Fortran_COMPILER_NAME MATCHES "gfor*")
  message(STATUS "ALaDyn: Using GCC Compiler")
  set(USING_GNU_COMPILER TRUE)
  string(APPEND CMAKE_CXX_COMPILE_FLAGS " -Wall")
  string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -fdefault-real-8")
  string(APPEND CMAKE_Fortran_FLAGS_DEBUG   " -fdefault-real-8 -Wall -Wextra")
  if(MARCONI_KNL)
    string(APPEND CMAKE_CXX_COMPILE_FLAGS " -mavx512f -mavx512cd -mavx512er -mavx512pf")
    string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -funroll-all-loops  -mavx512f -mavx512cd -mavx512er -mavx512pf -O3")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG   " -fbacktrace -fbounds-check -mavx512f -mavx512cd -mavx512er -mavx512pf -Og -g")
  else()
    string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -funroll-all-loops -O3")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG   " -fbacktrace -fbounds-check -Og -g")
  endif()
elseif(Fortran_COMPILER_NAME MATCHES "ifor*" OR Fortran_COMPILER_NAME MATCHES "ftn*")
  message(STATUS "ALaDyn: Using Intel Compiler")
  set(USING_INTEL_COMPILER TRUE)
  string(APPEND CMAKE_CXX_COMPILE_FLAGS " -diag-enable warn")
  string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -real-size 64")
  string(APPEND CMAKE_Fortran_FLAGS_DEBUG   " -real-size 64 -diag-enable warn")
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

if(FORCE_STATIC_LINK)
  string(APPEND CMAKE_Fortran_LINK_FLAGS " -static")
endif()

if(WIN32 AND USING_PGI_COMPILER)
  find_package(MPI COMPONENTS C REQUIRED)
  list(APPEND ALaDyn_Fortran_sources ${MPI_INCLUDE_PATH}/mpi.f90)
  add_definitions(-DUSE_MPI_MODULE)
  include_directories("${MPI_INCLUDE_PATH}/amd64")
  set(MISSING_MPI_LIBRARY "${MPI_INCLUDE_PATH}/../Lib/amd64/msmpifec.lib")
  list(APPEND MPI_LIBRARIES ${MISSING_MPI_LIBRARY})
else()
  find_package(MPI REQUIRED)
endif()

if(NOT WIN32)
  find_package(Boost COMPONENTS system filesystem REQUIRED)
  if(Boost_FOUND)
    add_definitions(-DUSE_BOOST)
  endif()
endif()

if(USING_INTEL_COMPILER AND NOT FORCE_FFTW)
  find_package(MKL REQUIRED)
  set(MKL_FFTW_INCLUDE_DIR "${MKL_INCLUDE_DIR}/fftw/")
else()
  find_package(FFTW REQUIRED)
endif()

string(APPEND CMAKE_CXX_COMPILE_FLAGS " ${MPI_COMPILE_FLAGS}")
string(APPEND CMAKE_CXX_LINK_FLAGS " ${MPI_LINK_FLAGS}")
string(APPEND CMAKE_Fortran_COMPILE_FLAGS " ${MPI_COMPILE_FLAGS} ${MPI_Fortran_COMPILE_FLAGS}")
string(APPEND CMAKE_Fortran_LINK_FLAGS " ${MPI_LINK_FLAGS} ${MPI_Fortran_LINK_FLAGS}")

include_directories(${OTHER_INCLUDE_DIR} ${MPI_INCLUDE_PATH} ${MPI_Fortran_INCLUDE_PATH} ${FFTW_INCLUDE_DIR} ${MKL_INCLUDE_DIR} ${MKL_FFTW_INCLUDE_DIR} ${Boost_INCLUDE_DIR})
link_directories(${OTHER_LINK_DIR})

add_executable(ALaDyn
  ${ALaDyn_Fortran_sources}
)

add_library(cpp_folder_tree STATIC src/cpp_folder_tree.cpp)
add_library(find_last_addr STATIC src/find_last_addr.cpp)
add_library(enable_gdb_attach STATIC src/enable_gdb_attach.cpp)

target_link_libraries(ALaDyn
  cpp_folder_tree
  find_last_addr
  enable_gdb_attach
  ${MPI_Fortran_LIBRARIES}
  ${MPI_LIBRARIES}
  ${Boost_LIBRARIES}
  ${MKL_LIBRARIES}
  ${FFTW_LIBRARY}
)

set_target_properties(ALaDyn PROPERTIES LINKER_LANGUAGE Fortran)

install(TARGETS ALaDyn DESTINATION ${CMAKE_SOURCE_DIR}/bin/)
