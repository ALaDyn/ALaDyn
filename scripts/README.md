# ALaDyn Scripts

This directory contains unified build and run scripts for ALaDyn.

## Quick Start

### Building

```bash
# Basic build with auto-detection
./build.sh

# Debug build
./build.sh -t Debug

# Specific platform and compiler
./build.sh -p marconi-knl -c intel

# Clean build with 8 parallel jobs
./build.sh --clean -j 8
```

### Running

```bash
# Generate and submit job (auto-detect platform)
./run.sh -n 2 -t 136 -a MyProject

# Dry run (generate script only)
./run.sh -p marconi-knl --dry-run

# Run locally without batch system
./run.sh --local -t 4
```

## Build Script (`build.sh`)

The unified build script replaces all platform-specific build scripts with a single, intelligent script.

### Features

- **Auto-detection**: Automatically detects OS, HPC platform, and available compilers
- **Module loading**: Handles HPC module loading for supported platforms
- **vcpkg support**: Optional vcpkg integration for dependency management
- **Multiple compilers**: GNU, Intel, and PGI compiler support
- **Build types**: Release, Debug, and Profiling configurations

### Supported Platforms

| Platform | Description |
|----------|-------------|
| `generic` | Standard Linux/Unix with no HPC modules |
| `mac` | macOS with Homebrew |
| `marconi` | CINECA Marconi (Skylake partition) |
| `marconi-knl` | CINECA Marconi (KNL partition) |
| `cnaf` | INFN CNAF |
| `edison` | NERSC Edison/Cori (Cray systems) |

### Options

```
-h, --help              Show help message
-c, --compiler TYPE     Compiler: gnu, intel, pgi (default: auto)
-t, --build-type TYPE   Build type: Release, Debug, Profiling
-j, --jobs N            Parallel jobs (default: auto)
-p, --platform NAME     HPC platform (default: auto)
--clean                 Clean build directory first
--fftw                  Force FFTW instead of MKL
--old-mpi               Use legacy mpif.h interface
--scorep                Enable ScoreP profiling
--vcpkg                 Use vcpkg for dependencies
--install-prefix PATH   Installation prefix
```

## Run Script (`run.sh`)

The unified run script generates job scripts for various batch systems and can submit them.

### Features

- **Multiple schedulers**: SLURM, LSF (bsub), direct mpirun
- **Auto-detection**: Detects batch system and platform
- **Module loading**: Generates correct module load commands
- **Flexible configuration**: Nodes, tasks, walltime, queue

### Supported Batch Systems

| System | Platforms |
|--------|-----------|
| SLURM | Marconi, generic HPC |
| LSF | CNAF |
| Local | Any (direct mpirun) |

### Options

```
-h, --help              Show help message
-n, --nodes N           Number of nodes (default: 1)
-t, --tasks N           Total MPI tasks (default: auto)
-c, --tasks-per-node N  Tasks per node
-w, --walltime TIME     Wall time (default: 01:00:00)
-p, --platform NAME     HPC platform
-q, --queue NAME        Queue/partition name
-a, --account NAME      Account for billing
-j, --job-name NAME     Job name (default: aladyn)
-e, --executable PATH   Path to ALaDyn
--compiler TYPE         Compiler used (for modules)
--fftw                  Load FFTW modules
--scorep                Enable ScoreP profiling
--dry-run               Generate without submitting
--local                 Run directly with mpirun
```

## Windows Users

For Windows builds, use the PowerShell script in `cmake/build.ps1`:

```powershell
# From repository root
.\cmake\build.ps1 -UseVCPKG -DisableInteractive
```

See the [cmake/README.md](../cmake/README.md) for detailed Windows build instructions.

## Legacy Scripts

Old platform-specific scripts have been moved to:
- `build/deprecated/` - Legacy build scripts
- `run/deprecated/` - Legacy run scripts

See the README files in those directories for migration guides.
