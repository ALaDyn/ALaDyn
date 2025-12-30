################################################################################
# ALaDyn Test Suite
# This file configures the testing framework for the ALaDyn project
################################################################################

# Enable CTest for test discovery
enable_testing()

# Find MPI for parallel tests
find_package(MPI REQUIRED COMPONENTS Fortran)

# No additional test-specific flags needed - tests inherit from parent project's
# CMAKE_Fortran_FLAGS which already include -fdefault-real-8 and other necessary flags

# Set the module output directory for test framework modules
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

################################################################################
# Test Utility Library - Provides test assertions and utilities
################################################################################
add_library(aladyn_test_utils STATIC
    test_framework/test_assertions.f90
    test_framework/test_runner.f90
)
target_include_directories(aladyn_test_utils PUBLIC
    ${CMAKE_BINARY_DIR}  # For .mod files from main build (precision_def, etc.)
    ${CMAKE_CURRENT_BINARY_DIR}  # For test_assertions.mod, test_runner.mod
)
# Ensure ALaDyn modules are built before test utilities
add_dependencies(aladyn_test_utils ALaDyn)

################################################################################
# Unit Tests for precision_def module
# This test uses the precision_def module from ALaDyn
################################################################################
add_executable(test_precision_def
    unit/test_precision_def.f90
)
target_link_libraries(test_precision_def PRIVATE aladyn_test_utils)
target_include_directories(test_precision_def PRIVATE ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR})
add_dependencies(test_precision_def ALaDyn)
add_test(NAME precision_def_tests COMMAND test_precision_def)

################################################################################
# Unit Tests for phys_param module
# This test uses the phys_param module from ALaDyn
################################################################################
add_executable(test_phys_param
    unit/test_phys_param.f90
)
target_link_libraries(test_phys_param PRIVATE aladyn_test_utils)
target_include_directories(test_phys_param PRIVATE ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR})
add_dependencies(test_phys_param ALaDyn)
add_test(NAME phys_param_tests COMMAND test_phys_param)

################################################################################
# Unit Tests for util module (math utilities)
################################################################################
add_executable(test_util
    unit/test_util.f90
)
target_link_libraries(test_util PRIVATE aladyn_test_utils)
target_include_directories(test_util PRIVATE ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR})
add_test(NAME util_tests COMMAND test_util)

################################################################################
# Unit Tests for stretched_grid module
################################################################################
add_executable(test_stretched_grid
    unit/test_stretched_grid.f90
)
target_link_libraries(test_stretched_grid PRIVATE aladyn_test_utils MPI::MPI_Fortran)
target_include_directories(test_stretched_grid PRIVATE ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR})
add_test(NAME stretched_grid_tests COMMAND test_stretched_grid)

################################################################################
# Unit Tests for grid_param module
################################################################################
add_executable(test_grid_param
    unit/test_grid_param.f90
)
target_link_libraries(test_grid_param PRIVATE aladyn_test_utils MPI::MPI_Fortran)
target_include_directories(test_grid_param PRIVATE ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR})
add_test(NAME grid_param_tests COMMAND test_grid_param)

################################################################################
# Unit Tests for boris_push module (core particle pusher)
################################################################################
add_executable(test_boris_push
    unit/test_boris_push.f90
)
target_link_libraries(test_boris_push PRIVATE aladyn_test_utils MPI::MPI_Fortran)
target_include_directories(test_boris_push PRIVATE ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR})
add_test(NAME boris_push_tests COMMAND test_boris_push)

################################################################################
# Unit Tests for array_alloc module
################################################################################
add_executable(test_array_alloc
    unit/test_array_alloc.f90
)
target_link_libraries(test_array_alloc PRIVATE aladyn_test_utils)
target_include_directories(test_array_alloc PRIVATE ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR})
add_test(NAME array_alloc_tests COMMAND test_array_alloc)

################################################################################
# Integration Test - Smoke test for basic functionality
################################################################################
add_executable(test_integration_smoke
    integration/test_smoke.f90
)
target_link_libraries(test_integration_smoke PRIVATE aladyn_test_utils MPI::MPI_Fortran)
target_include_directories(test_integration_smoke PRIVATE ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR})
add_test(NAME integration_smoke_test COMMAND test_integration_smoke)

################################################################################
# Integration Test - LWFA basic scenario validation
################################################################################
add_executable(test_integration_lwfa
    integration/test_lwfa_scenario.f90
)
target_link_libraries(test_integration_lwfa PRIVATE aladyn_test_utils MPI::MPI_Fortran)
target_include_directories(test_integration_lwfa PRIVATE ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR})
add_test(NAME integration_lwfa_test COMMAND test_integration_lwfa)

################################################################################
# Test Suite Summary
################################################################################
message(STATUS "ALaDyn Test Suite configured:")
message(STATUS "  - Unit tests: precision_def, phys_param, util, stretched_grid, grid_param, boris_push, array_alloc")
message(STATUS "  - Integration tests: smoke, lwfa_scenario")
message(STATUS "  - Run tests with: ctest --output-on-failure")
