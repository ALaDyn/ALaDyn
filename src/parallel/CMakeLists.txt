target_sources(ALaDyn PRIVATE mpi_var.F90)

if(WIN32 AND CMAKE_CXX_COMPILER_ID STREQUAL "PGI")
    find_package(MPI REQUIRED COMPONENTS C)
    if(MPI_FOUND)
        set(FORCE_OLD_MPI
            ON
            CACHE BOOL "Forcing old mpif.h linking" FORCE)
        message(STATUS "ALaDyn: MPI libraries are linked against mpif.h")
        target_compile_definitions(ALaDyn PRIVATE FORCE_OLD_MPI)
        target_include_directories(ALaDyn PRIVATE ${MPI_INCLUDE_PATH})
        target_link_libraries(ALaDyn PRIVATE ${MPI_LIBRARIES})
    endif()
else()
    find_package(MPI REQUIRED COMPONENTS Fortran)
    if(MPI_FOUND)
        target_include_directories(ALaDyn PRIVATE ${MPI_INCLUDE_PATH}
                                                  ${MPI_Fortran_INCLUDE_PATH})
        target_link_libraries(ALaDyn PRIVATE ${MPI_LIBRARIES})
    endif()
endif()

if(MPI_FOUND)
    target_sources(
        ALaDyn PRIVATE mpi_curr_interface.f90 mpi_field_interface.f90
                       mpi_part_interface.f90 parallel.F90)
    target_compile_definitions(ALaDyn PRIVATE ALaDyn_USE_MPI)
endif()
