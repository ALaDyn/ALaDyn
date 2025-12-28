# ALaDyn Test Suite

This directory contains the test suite for the ALaDyn Particle-in-Cell code.

## Directory Structure

```
tests/
├── CMakeLists.txt          # Test build configuration
├── README.md               # This file
├── test_framework/         # Test utilities
│   ├── test_assertions.f90 # Assertion macros
│   └── test_runner.f90     # Test suite utilities
├── unit/                   # Unit tests
│   ├── test_precision_def.f90
│   ├── test_phys_param.f90
│   ├── test_util.f90
│   ├── test_stretched_grid.f90
│   ├── test_grid_param.f90
│   ├── test_boris_push.f90
│   └── test_array_alloc.f90
└── integration/            # Integration tests
    ├── test_smoke.f90
    └── test_lwfa_scenario.f90
```

## Running Tests

### Using CTest (Recommended)

After building ALaDyn with CMake:

```bash
cd build
ctest --output-on-failure
```

### Run Specific Tests

```bash
# Run tests matching a pattern
ctest -R precision

# Run with verbose output
ctest -V

# Run tests in parallel
ctest -j4
```

### Building Without Tests

To disable tests:

```bash
cmake .. -DBUILD_TESTING=OFF
```

## Test Categories

### Unit Tests (`tests/unit/`)

Test individual modules in isolation:

| Test | Module | Description |
|------|--------|-------------|
| `test_precision_def` | `precision_def` | Precision kinds and utility functions |
| `test_phys_param` | `phys_param` | Physical constants and parameters |
| `test_util` | `util` | Mathematical utilities (RNG, sorting) |
| `test_stretched_grid` | `stretched_grid` | Grid stretching transformations |
| `test_grid_param` | `grid_param` | Grid dimension calculations |
| `test_boris_push` | `boris_push` | Boris particle pusher algorithm |
| `test_array_alloc` | `array_alloc` | Array allocation utilities |

### Integration Tests (`tests/integration/`)

Test interactions between modules and physics validation:

| Test | Description |
|------|-------------|
| `test_smoke` | Basic functionality smoke test |
| `test_lwfa_scenario` | LWFA physics parameter validation |

## Writing New Tests

### 1. Create a Test File

```fortran
program test_my_module
 use test_assertions
 use test_runner
 
 implicit none
 
 call start_test_suite('my_module')
 
 call test_feature_one()
 call test_feature_two()
 
 call end_test_suite('my_module')
 
 if (.not. test_suite_passed()) then
  error stop 1
 end if

contains

 subroutine test_feature_one()
  real(dp) :: result, expected
  
  call run_test('feature_one')
  
  ! Test code here
  expected = 1.0_dp
  result = 1.0_dp
  
  call assert_near_dp(expected, result, 1.0e-14_dp, 'feature one works')
 end subroutine

end program
```

### 2. Add to CMakeLists.txt

```cmake
add_executable(test_my_module
    unit/test_my_module.f90
)
target_compile_options(test_my_module PRIVATE ${TEST_Fortran_FLAGS})
target_link_libraries(test_my_module PRIVATE aladyn_test_utils)
target_include_directories(test_my_module PRIVATE ${CMAKE_BINARY_DIR})
add_test(NAME my_module_tests COMMAND test_my_module)
```

## Test Framework API

### Assertions

```fortran
! Boolean assertions
call assert_true(condition, 'test name')
call assert_false(condition, 'test name')

! Integer comparison
call assert_equal_int(expected, actual, 'test name')

! Floating point comparison (with default tolerance)
call assert_equal_dp(expected, actual, 'test name')
call assert_equal_sp(expected, actual, 'test name')

! Floating point with custom tolerance
call assert_near_dp(expected, actual, tolerance, 'test name')
call assert_near_sp(expected, actual, tolerance, 'test name')

! Array comparison
call assert_array_equal_dp(expected, actual, n, 'test name')
call assert_array_near_dp(expected, actual, n, tolerance, 'test name')
```

### Test Organization

```fortran
! Start a test suite
call start_test_suite('suite_name')

! Mark a test as running
call run_test('test_name')

! Skip a test with reason
call skip_test('test_name', 'reason for skipping')

! End suite and print summary
call end_test_suite('suite_name')

! Check if all tests passed
if (.not. test_suite_passed()) error stop 1
```

## Test Output

Example output:

```
============================================
TEST SUITE: precision_def
============================================

--- Running: precision_kinds ---
[PASS] sp kind is defined
[PASS] sp has at least 6 digits of precision
[PASS] dp kind is defined
[PASS] dp has at least 15 digits of precision

--- Running: precision_constants ---
[PASS] zero_dp equals 0.0
[PASS] one_dp equals 1.0

End of test suite: precision_def
============================================
TEST SUMMARY
============================================
Total tests:  10
Passed:       10
Failed:       0
============================================
RESULT: PASSED
============================================
```

## Contributing Tests

When contributing to ALaDyn:

1. **New features** must include corresponding unit tests
2. **Bug fixes** should include a test that demonstrates the fix
3. **Physics changes** need validation tests against known results
4. Tests must pass before PRs can be merged

## CI Integration

Tests run automatically in GitHub Actions on:
- Every push
- Every pull request
- Daily scheduled builds

See `.github/workflows/ccpp.yml` for CI configuration.
