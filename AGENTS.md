# AGENTS.md - AI Agent Guidelines for ALaDyn

This document provides guidelines for AI coding agents working with the ALaDyn codebase.

## Project Overview

**ALaDyn** (Acceleration by Laser and Dynamics of charged particles) is a Particle-in-Cell (PIC) code for plasma physics simulations. It is primarily written in **Fortran** with some **C++** helper utilities.

### Main Use Cases
- Laser Wakefield Acceleration (LWFA)
- Plasma Wakefield Acceleration (PWFA)
- Target Normal Sheath Acceleration (TNSA)

## Repository Structure

```
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
├── cmake/                  # CMake modules and build scripts
├── docs/                   # Documentation
├── examples/               # Example input files
├── scripts/                # Build and utility scripts
└── deprecated/             # Deprecated code
```

## Build System

- **CMake** (minimum version 3.15) is used for building
- **vcpkg** is the preferred package manager for dependencies
- Supported compilers: GNU (gfortran), Intel (ifort), PGI

### Building the Code

#### Unix/Linux/macOS
```bash
# Unified build script with auto-detection
./scripts/build.sh

# Specific options
./scripts/build.sh -c intel -t Debug -p marconi-knl

# Show all options
./scripts/build.sh --help
```

#### Windows (PowerShell)
```powershell
.\cmake\build.ps1 -UseVCPKG -DisableInteractive
```

#### Manual CMake
```bash
mkdir build && cd build
cmake ..
cmake --build . --target install
```

### Dependencies
- MPI (OpenMPI or MPICH)
- FFTW3 (or Intel MKL)
- Boost (filesystem, system) - optional, C++17 std::filesystem can be used

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
- Ensure code compiles before submitting PRs
- No formal test suite exists; validate compilation is the minimum requirement

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
