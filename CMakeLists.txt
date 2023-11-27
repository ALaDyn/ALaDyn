cmake_minimum_required(VERSION 3.15)

#Add cmake project
project(
    ALaDyn
    VERSION 3.0.2
    LANGUAGES Fortran CXX)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

#Add cmake executable
add_executable(ALaDyn src/ALaDyn.F90)

if(DEFINED ENV{VCPKG_DEFAULT_TRIPLET})
    message(
        STATUS
            "Setting default vcpkg target triplet to $ENV{VCPKG_DEFAULT_TRIPLET}"
    )
    set(VCPKG_TARGET_TRIPLET $ENV{VCPKG_DEFAULT_TRIPLET})
endif()

if(DEFINED ENV{VCPKG_ROOT} AND NOT DEFINED CMAKE_TOOLCHAIN_FILE)
    set(CMAKE_TOOLCHAIN_FILE
        "$ENV{VCPKG_ROOT}/scripts/buildsystems/vcpkg.cmake"
        CACHE STRING "")
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

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
if(MSYS)
    set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/msys2/"
                          ${CMAKE_MODULE_PATH})
endif()

if(APPLE)
    set(MACOSX_RPATH TRUE)
endif(APPLE)

# you can force FFTW if necessary, avoiding MKL (which is preferred on Intel systems)
option(FORCE_FFTW "Forcing FFTW libraries" OFF)

# set this variable to true to force static linking (if supported by the compiler)
option(FORCE_STATIC_LINK "Forcing static linking to libraries" OFF)

# forcing old mpif.h if the correct version of the mpi module is not available
set(FORCE_OLD_MPI
    OFF
    CACHE BOOL "Forcing old mpif.h linking")

# make sure that the default is a RELEASE
set(DEFAULT_BUILD_TYPE "Release")
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(
        STATUS
            "ALaDyn: Setting build type to '${DEFAULT_BUILD_TYPE}' as none was specified."
    )
    set(CMAKE_BUILD_TYPE
        "${DEFAULT_BUILD_TYPE}"
        CACHE STRING "Choose the type of build." FORCE)
    set_property(
        CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel"
                                        "RelWithDebInfo" "Profiling")
endif()

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(NOT Boost_FOUND)
    set(CMAKE_CXX_STANDARD 17)
    set(CMAKE_CXX_STANDARD_REQUIRED ON)
endif()

if(FORCE_STATIC_LINK)
    string(APPEND CMAKE_Fortran_LINK_FLAGS " -static")
endif()

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
