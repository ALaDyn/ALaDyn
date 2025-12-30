# AGENTS.md - AI Agent Guidelines for ALaDyn

This document provides guidelines for AI coding agents working with the ALaDyn codebase.

## Project Overview

**ALaDyn** (Acceleration by Laser and Dynamics of charged particles) is a Particle-in-Cell (PIC) code for plasma physics simulations. It is primarily written in **Fortran** with some **C++** helper utilities.

### Main Use Cases

- Laser Wakefield Acceleration (LWFA)
- Plasma Wakefield Acceleration (PWFA)
- Target Normal Sheath Acceleration (TNSA)

## Repository Structure

```shell
ALaDyn/
├── src/                    # Main source code
│   ├── ALaDyn.F90          # Main program entry point
│   ├── cpp_lib/            # C++ utility functions (filesystem, debugging)
│   ├── depot/              # Legacy/deprecated bunch utilities
│   ├── diagnostics/        # Output diagnostics and run info
│   ├── dynamics/           # Particle dynamics (Boris push, evolution)
│   ├── fft/                # FFT implementations (modern and legacy)
│   ├── fields/             # Grid field operations
│   ├── grid/               # Grid parameters and stretched grids
│   ├── IO/                 # Input/output routines
│   ├── ionization/         # Ionization physics
│   ├── parallel/           # MPI parallelization
│   ├── particles/          # Particle data structures and utilities
│   ├── start/              # Initialization and startup routines
│   └── work/               # Common parameters, precision, utilities
├── cmake/                  # CMake modules and build scripts (GIT SUBMODULE)
├── docs/                   # Documentation
├── examples/               # Example input files
├── scripts/                # Build and utility scripts
└── deprecated/             # Deprecated code
```

### Important: Git Submodules

**The `cmake/` directory is a Git submodule** pointing to https://github.com/cenit/ccm

To initialize submodules after cloning:

```bash
git submodule update --init --recursive
```

The cmake submodule contains:

- `build.ps1` - PowerShell build script with vcpkg integration
- `build-doc.ps1` - Documentation build script
- `utils.psm1` - PowerShell utility module
- Various helper scripts for deployment and configuration

**Always ensure submodules are initialized before running CI builds or using cmake/build.ps1**

## Build System

- **CMake** (minimum version 3.19) is used for building
- **vcpkg** is the preferred package manager for dependencies
- Supported compilers: GNU (gfortran), Intel (ifort), PGI

### Building the Code (Powershell required, do not bypass)

```powershell
.\cmake\build.ps1 -UseVCPKG -DisableInteractive -DoNotUpdateVCPKG -DoNotUpdateTOOL -DoNotDeleteBuildFolder -EnableTEST
```

#### Important build.ps1 Flags

| Flag | Description |
|------|-------------|
| `-UseVCPKG` | Use vcpkg for dependency management |
| `-DisableInteractive` | Disable interactive prompts (required for CI) |
| `-DoNotUpdateVCPKG` | Skip vcpkg updates |
| `-DoNotUpdateTOOL` | Skip tool self-updates |
| `-DoNotDeleteBuildFolder` | Keep build folder after completion |
| `-EnableTEST` | **Enable building and running tests** |
| `-BuildDebug` | Build debug version (in addition to release) |
| `-DoNotUseNinja` | Use Makefile generator instead of Ninja (see note below) |

**Note**: Without `-BuildDebug`, only the release build is produced. The build output goes to `build_release/` for release builds.

#### Ninja vs Makefile Generator

By default, `build.ps1` uses the **Ninja** generator for faster builds. However, **Ninja can cause issues with Fortran code**, particularly:

1. **Preprocessing corruption**: Ninja's handling of Fortran preprocessing (`.F90` files with preprocessor directives) can corrupt source files, causing cryptic errors like `Invalid character in name` pointing to valid Fortran kind specifiers (e.g., `1.0_dp`).

2. **Module dependency ordering**: Fortran modules must be compiled in dependency order. While CMake handles this, Ninja's parallel execution can sometimes cause race conditions.

**Recommendation**: If you encounter unexplained compilation errors in Fortran files (especially test files or files using `use` statements with kind parameters), try adding `-DoNotUseNinja` to switch to the Makefile generator:

```powershell
.\cmake\build.ps1 -UseVCPKG -DisableInteractive -DoNotUpdateVCPKG -DoNotUpdateTOOL -DoNotDeleteBuildFolder -EnableTEST -DoNotUseNinja
```

The Makefile generator is slower but more reliable for Fortran projects.

#### Manual CMake (if failing do not trigger code modifications in reaction)

```bash
mkdir build && cd build
cmake ..
cmake --build . --target install
```

### Dependencies

- MPI (OpenMPI or MPICH)
- FFTW3 (or Intel MKL)

### Running on HPC Systems

```bash
# Generate and submit job script
./scripts/run.sh -n 2 -t 136 -a MyProject

# Dry run (see generated script)
./scripts/run.sh -p marconi-knl --dry-run

# Local execution
./scripts/run.sh --local -t 4
```

## Code Style Guidelines

### Fortran Style

1. **File Extensions**:
   - `.F90` for preprocessed Fortran (capital F)
   - `.f90` for standard Fortran (lowercase f)

2. **License Header**: All source files must include the standard copyright header:
   
   ```fortran
   !*****************************************************************************************************!
   !                            Copyright 2008-2020  The ALaDyn Collaboration                            !
   !*****************************************************************************************************!
   ```

3. **Module Structure**:

   ```fortran
   module module_name

    use dependency_module
    
    implicit none
    
    ! Module-level declarations
    
   contains
   
    subroutine/function definitions
    
   end module
   ```

4. **Indentation**: Use 1 space for module/program body indentation, standard indentation for nested constructs

5. **Naming Conventions**:
   - Module names: `snake_case` (e.g., `precision_def`, `boris_push`, `grid_param`)
   - Subroutine/function names: `snake_case` (e.g., `lpf_momenta_and_positions`)
   - Variables: `snake_case` for multi-word, lowercase for single word
   - Constants/parameters: lowercase (e.g., `dp`, `sp`, `zero_dp`)

6. **Precision**: Use precision kinds from `precision_def` module:
   - `dp` for double precision reals
   - `sp` for single precision reals
   - `dp_int` for 64-bit integers
   - `hp_int` for 16-bit integers

7. **Comments**: Use `!` for inline comments, `!!` for documentation comments (FORD-compatible)

### C++ Style

1. **File Extension**: `.cpp`

2. **License Header**: Use C-style block comments with asterisks:
   
   ```cpp
   /*******************************************************************************************************
    *                            Copyright 2008-2020  The ALaDyn Collaboration                            *
    ******************************************************************************************************/
   ```

3. **Standard**: C++11 minimum (C++17 preferred for `std::filesystem`)

4. **Fortran Interop**: Use `extern "C"` blocks with trailing underscore naming:

   ```cpp
   extern "C" {
   void function_name_(char* arg, size_t len) { ... }
   }
   ```

## Testing and CI

- GitHub Actions workflow in `.github/workflows/ccpp.yml`
- CI runs on Ubuntu and macOS
- Tests are run automatically via `ctest` after each build
- **All code modifications must include corresponding tests**

### Test Suite Structure

```shell
tests/
├── CMakeLists.txt          # Test build configuration
├── test_framework/         # Test utilities and assertions
│   ├── test_assertions.f90 # Assertion macros for testing
│   └── test_runner.f90     # Test suite runner utilities
├── unit/                   # Unit tests for individual modules
│   ├── test_precision_def.f90
│   ├── test_phys_param.f90
│   ├── test_util.f90
│   ├── test_stretched_grid.f90
│   ├── test_grid_param.f90
│   ├── test_boris_push.f90
│   └── test_array_alloc.f90
└── integration/            # Integration tests
    ├── test_smoke.f90      # Basic functionality smoke test
    └── test_lwfa_scenario.f90  # LWFA physics validation
```

### Running Tests

```bash
# After building with CMake
cd build
ctest --output-on-failure

# Run specific test
ctest -R precision_def

# Verbose output
ctest -V

# Run tests in parallel
ctest -j4
```

### Building with Tests

```bash
# Enable tests (enabled by default)
cmake .. -DBUILD_TESTING=ON
cmake --build .
ctest
```

### Writing Tests

When adding new functionality, follow these guidelines:

1. **Create a test file** in `tests/unit/` for unit tests or `tests/integration/` for integration tests

2. **Use the test framework**:
   ```fortran
   program test_my_module
    use test_assertions
    use test_runner
    
    implicit none
    
    call start_test_suite('my_module')
    
    call test_feature_1()
    call test_feature_2()
    
    call end_test_suite('my_module')
    
    if (.not. test_suite_passed()) then
     error stop 1
    end if
   
   contains
   
    subroutine test_feature_1()
     call run_test('feature_1')
     call assert_near_dp(expected, actual, tolerance, 'description')
     call assert_true(condition, 'description')
    end subroutine
   
   end program
   ```

3. **Available assertions**:
   - `assert_true(condition, name)` - Assert condition is true
   - `assert_false(condition, name)` - Assert condition is false
   - `assert_equal_int(expected, actual, name)` - Compare integers
   - `assert_equal_dp(expected, actual, name)` - Compare double precision
   - `assert_near_dp(expected, actual, tol, name)` - Compare with tolerance
   - `assert_array_near_dp(expected, actual, n, tol, name)` - Compare arrays

4. **Add the test to CMakeLists.txt**:
   ```cmake
   add_executable(test_my_module unit/test_my_module.f90)
   target_compile_options(test_my_module PRIVATE ${TEST_Fortran_FLAGS})
   target_link_libraries(test_my_module PRIVATE aladyn_test_utils)
   target_include_directories(test_my_module PRIVATE ${CMAKE_BINARY_DIR})
   add_test(NAME my_module_tests COMMAND test_my_module)
   ```

### Test Categories

| Category | Description | Location |
|----------|-------------|----------|
| **Unit Tests** | Test individual functions/modules in isolation | `tests/unit/` |
| **Integration Tests** | Test interactions between modules | `tests/integration/` |
| **Physics Tests** | Validate physics correctness (LWFA, PWFA, etc.) | `tests/integration/` |

### Test Coverage Requirements

When making changes, ensure:

1. **New features**: Add unit tests for all new public functions/subroutines
2. **Bug fixes**: Add a test that would have caught the bug
3. **Physics changes**: Add validation tests for physics accuracy
4. **Performance changes**: Document expected behavior, add regression tests

### Test Naming Conventions

- Test files: `test_<module_name>.f90`
- Test programs: `test_<module_name>`
- Test subroutines: `test_<feature_name>`
- Test names (in assertions): descriptive, lowercase with spaces

## Making Changes

### Before Modifying Code

1. Understand the module dependency hierarchy (start from `ALaDyn.F90`)
2. Check if similar patterns exist elsewhere in the codebase
3. Verify compilation with at least GNU Fortran

### When Adding New Features

1. Place code in the appropriate subdirectory under `src/`
2. Update the relevant `CMakeLists.txt` to include new source files
3. Follow existing module structure and naming conventions
4. Add the standard license header to new files

### When Fixing Bugs

1. Make minimal, surgical changes
2. Preserve existing code structure and style
3. Do not refactor unrelated code

## Key Files to Understand

| File | Purpose |
|------|---------|
| `src/ALaDyn.F90` | Main program, entry point |
| `src/work/precision_def.F90` | Precision definitions (dp, sp, etc.) |
| `src/work/common_param.f90` | Common simulation parameters |
| `src/dynamics/boris_push.f90` | Core particle pusher algorithms |
| `src/start/read_input.f90` | Input file parsing |
| `CMakeLists.txt` | Main build configuration |

## Common Pitfalls

1. **Real Precision**: The code uses `-fdefault-real-8` (GNU) or equivalent flags. Do not assume default real is single precision.

2. **MPI Compatibility**: Some MPI implementations require special handling. Check `FORCE_OLD_MPI` option if using legacy `mpif.h`.

3. **Array Indexing**: Fortran uses 1-based indexing by default; some arrays may use custom bounds.

4. **Module Dependencies**: Fortran modules must be compiled in dependency order. CMake handles this, but be aware when adding new dependencies.

5. **Preprocessor Directives**: `.F90` files may contain preprocessor macros; `.f90` files should not.

## Documentation

- In-code documentation uses FORD format (`!!` comments)
- User documentation is in `docs/pages/`
- Input guide: `docs/pages/NAMELIST_GUIDE.md`
- Build guide: `docs/pages/BUILD.md`

## Contributing

See `docs/pages/CONTRIBUTING.md` for the full contribution workflow:

1. Fork the repository
2. Create a feature branch (`dev/<yourname>/<feature>`)
3. Make changes and ensure compilation
4. Squash commits to meaningful units
5. Open a Pull Request
