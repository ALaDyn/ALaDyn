cmake_minimum_required (VERSION 2.8.10)
project(ALaDyn)
enable_language(Fortran)
enable_language(CXX)
set (CMAKE_CXX_STANDARD 11)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

if (MSVC)
  set(Boost_USE_STATIC_LIBS ON)
  set(Boost_USE_STATIC_RUNTIME ON)
endif()

if(APPLE)
  set(CMAKE_CXX_FLAGS "-std=c++11 -stdlib=libc++")
endif() 

find_package(Boost COMPONENTS system filesystem REQUIRED)
find_package(MPI REQUIRED)
#find_package(MKL)
find_package(FFTW REQUIRED)

# make sure that the default is a RELEASE
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE RELEASE)
endif()

set (CMAKE_C_FLAGS_RELEASE "-O3")
set (CMAKE_CXX_FLAGS_RELEASE "-O3")
set (CMAKE_C_FLAGS_DEBUG "-Og -g")
set (CMAKE_CXX_FLAGS_DEBUG "-Og -g")

get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
if (Fortran_COMPILER_NAME MATCHES "gfortran*")
  set (CMAKE_Fortran_FLAGS_RELEASE "-fdefault-real-8 -funroll-all-loops -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fdefault-real-8 -Wall -Wextra -fbacktrace -fbounds-check -Og -g")
elseif (Fortran_COMPILER_NAME MATCHES "ifort*")
  set (CMAKE_Fortran_FLAGS_RELEASE "-real-size 64 -axcore-avx-i,SSE4.2 -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-real-size 64 -O0 -g -real-size 64 -g -check all -fpe0 -warn -traceback -debug extended")
else ()
  message ("CMAKE_Fortran_COMPILER found but unsupported")
  message ("Let's try using it...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif ()

set(CMAKE_CXX_COMPILE_FLAGS "${CMAKE_CXX_COMPILE_FLAGS} ${MPI_COMPILE_FLAGS}")
set(CMAKE_CXX_LINK_FLAGS "${CMAKE_CXX_LINK_FLAGS} ${MPI_LINK_FLAGS}")
set(CMAKE_Fortran_COMPILE_FLAGS "${CMAKE_Fortran_COMPILE_FLAGS} ${MPI_COMPILE_FLAGS} ${MPI_Fortran_COMPILE_FLAGS}")
set(CMAKE_Fortran_LINK_FLAGS "${CMAKE_CXX_LINK_FLAGS} ${MPI_LINK_FLAGS} ${MPI_Fortran_LINK_FLAGS}")
#if(NOT FFTW_FOUND AND NOT MKL_FOUND)
#message("FFTW and MKL not found, unable to build")
#endif()

include_directories(${MPI_INCLUDE_PATH} ${MPI_Fortran_INCLUDE_PATH} ${FFTW_INCLUDES} ${MKL_INCLUDE_DIRS} ${Boost_INCLUDE_DIR})

add_executable (ALaDyn 
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
	src/ionize.f90
	src/pstruct_data.F90
	src/fstruct_data.f90
	src/all_param.f90
	src/particles.F90
	src/parallel.F90
	src/pic_rutil.f90
	src/der_lib.f90
	src/grid_fields.f90
	src/pdf_moments.f90
	src/pic_in.f90
	src/pic_out.f90
	src/pic_dump.f90
	src/pic_evolve_in_time.f90
	src/pwfa_bunch_field_calculation.F90
	src/read_input.f90
	src/pwfa_output_addons.f90
)


target_link_libraries(ALaDyn 
	cpp_folder_tree 
	find_last_addr 
	${MPI_Fortran_LIBRARIES} 
	${MPI_LIBRARIES} 
	${FFTW_LIBRARIES} 
	${Boost_LIBRARIES}
) 

set_target_properties(ALaDyn PROPERTIES HAS_CXX TRUE LINKER_LANGUAGE Fortran)

install (TARGETS ALaDyn DESTINATION ${CMAKE_SOURCE_DIR}/bin/)

add_library(cpp_folder_tree STATIC src/cpp_folder_tree.cpp)
set_target_properties(cpp_folder_tree PROPERTIES HAS_CXX TRUE)
set_property(TARGET ${cpp_folder_tree} PROPERTY LINKER_LANGUAGE CXX)

add_library(find_last_addr STATIC src/find_last_addr.cpp)
set_target_properties(find_last_addr PROPERTIES HAS_CXX TRUE)
set_property(TARGET ${find_last_addr} PROPERTY LINKER_LANGUAGE CXX)
