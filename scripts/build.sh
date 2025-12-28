#!/usr/bin/env bash

#*****************************************************************************************************
#                            Copyright 2008-2025  The ALaDyn Collaboration
#*****************************************************************************************************
#
# ALaDyn Unified Build Script
#
# This script consolidates all platform-specific build configurations into a single,
# maintainable script with automatic environment detection and HPC module support.
#
# Usage:
#   ./scripts/build.sh [options]
#
# Options:
#   -h, --help              Show this help message
#   -c, --compiler TYPE     Compiler type: gnu, intel, pgi (default: auto-detect)
#   -t, --build-type TYPE   Build type: Release, Debug, Profiling (default: Release)
#   -j, --jobs N            Number of parallel jobs (default: auto-detect)
#   -p, --platform NAME     HPC platform: generic, marconi, marconi-knl, cnaf, edison, mac
#   --clean                 Clean build directory before building
#   --fftw                  Force FFTW (avoid MKL even on Intel)
#   --old-mpi               Use legacy mpif.h interface
#   --scorep                Enable ScoreP profiling instrumentation
#   --vcpkg                 Use vcpkg for dependency management
#   --install-prefix PATH   Installation prefix (default: repository root)
#
# Environment Variables:
#   CC, CXX, FC             Override C, C++, Fortran compilers
#   FFTW_ROOT_DIR           FFTW installation path
#   VCPKG_ROOT              vcpkg installation path
#
# Examples:
#   ./scripts/build.sh                           # Auto-detect and build Release
#   ./scripts/build.sh -c intel -t Debug         # Intel compiler, Debug build
#   ./scripts/build.sh -p marconi-knl --fftw     # Marconi KNL with FFTW
#   ./scripts/build.sh --clean -j 8              # Clean build with 8 jobs

set -euo pipefail

# Script version
VERSION="2.0.0"

# Default values
BUILD_TYPE="Release"
COMPILER_TYPE=""
PLATFORM=""
JOBS=""
CLEAN_BUILD=false
FORCE_FFTW=false
FORCE_OLD_MPI=false
ENABLE_SCOREP=false
USE_VCPKG=false
INSTALL_PREFIX=""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_step() {
    echo -e "${BLUE}[STEP]${NC} $1"
}

# Print usage
usage() {
    grep '^#' "$0" | grep -v '#!/' | sed 's/^# \?//'
    exit 0
}

# Parse command line arguments
parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                usage
                ;;
            -c|--compiler)
                COMPILER_TYPE="$2"
                shift 2
                ;;
            -t|--build-type)
                BUILD_TYPE="$2"
                shift 2
                ;;
            -j|--jobs)
                JOBS="$2"
                shift 2
                ;;
            -p|--platform)
                PLATFORM="$2"
                shift 2
                ;;
            --clean)
                CLEAN_BUILD=true
                shift
                ;;
            --fftw)
                FORCE_FFTW=true
                shift
                ;;
            --old-mpi)
                FORCE_OLD_MPI=true
                shift
                ;;
            --scorep)
                ENABLE_SCOREP=true
                shift
                ;;
            --vcpkg)
                USE_VCPKG=true
                shift
                ;;
            --install-prefix)
                INSTALL_PREFIX="$2"
                shift 2
                ;;
            *)
                log_error "Unknown option: $1"
                usage
                ;;
        esac
    done
}

# Detect the operating system
detect_os() {
    case "$(uname -s)" in
        Linux*)     OS="linux" ;;
        Darwin*)    OS="macos" ;;
        CYGWIN*|MINGW*|MSYS*) OS="windows" ;;
        *)          OS="unknown" ;;
    esac
    log_info "Detected OS: $OS"
}

# Detect HPC platform based on hostname or environment
detect_platform() {
    if [[ -n "$PLATFORM" ]]; then
        log_info "Using specified platform: $PLATFORM"
        return
    fi

    local hostname
    hostname=$(hostname 2>/dev/null || echo "unknown")

    # Detect based on hostname patterns or environment
    if [[ "$hostname" == *"marconi"* ]] || [[ -d "/cineca" ]]; then
        if [[ -n "${SLURM_JOB_PARTITION:-}" ]] && [[ "$SLURM_JOB_PARTITION" == *"knl"* ]]; then
            PLATFORM="marconi-knl"
        else
            PLATFORM="marconi"
        fi
    elif [[ "$hostname" == *"cnaf"* ]] || [[ -d "/shared/software" ]]; then
        PLATFORM="cnaf"
    elif [[ "$hostname" == *"edison"* ]] || [[ "$hostname" == *"cori"* ]] || [[ -d "/opt/cray" ]]; then
        PLATFORM="edison"
    elif [[ "$OS" == "macos" ]]; then
        PLATFORM="mac"
    else
        PLATFORM="generic"
    fi

    log_info "Auto-detected platform: $PLATFORM"
}

# Detect number of available CPU cores
detect_jobs() {
    if [[ -n "$JOBS" ]]; then
        return
    fi

    if [[ "$OS" == "macos" ]]; then
        JOBS=$(sysctl -n hw.ncpu 2>/dev/null || echo 4)
    elif [[ "$OS" == "linux" ]]; then
        JOBS=$(nproc 2>/dev/null || grep -c ^processor /proc/cpuinfo 2>/dev/null || echo 4)
    else
        JOBS=4
    fi

    log_info "Using $JOBS parallel jobs"
}

# Load HPC modules based on platform and compiler
load_modules() {
    # Check if module command is available
    if ! command -v module &> /dev/null; then
        log_info "Module system not available, skipping module loading"
        return
    fi

    log_step "Loading modules for platform: $PLATFORM, compiler: $COMPILER_TYPE"

    # Purge existing modules
    module purge 2>/dev/null || true

    case "$PLATFORM" in
        marconi)
            case "$COMPILER_TYPE" in
                intel)
                    module load intel/pe-xe-2018--binary 2>/dev/null || module load intel 2>/dev/null || true
                    module load intelmpi/2018--binary 2>/dev/null || module load intelmpi 2>/dev/null || true
                    module load boost/1.66.0--intelmpi--2018--binary 2>/dev/null || module load boost 2>/dev/null || true
                    module load mkl/2018--binary 2>/dev/null || module load mkl 2>/dev/null || true
                    module load cmake 2>/dev/null || true
                    ;;
                gnu)
                    module load profile/advanced 2>/dev/null || true
                    module load gnu/6.1.0 2>/dev/null || module load gnu 2>/dev/null || true
                    module load openmpi/1-10.3--gnu--6.1.0 2>/dev/null || module load openmpi 2>/dev/null || true
                    module load fftw/3.3.4--openmpi--1-10.3--gnu--6.1.0 2>/dev/null || module load fftw 2>/dev/null || true
                    module load boost/1.61.0--gnu--6.1.0 2>/dev/null || module load boost 2>/dev/null || true
                    module load cmake 2>/dev/null || true
                    ;;
            esac
            ;;
        marconi-knl)
            module load env-knl 2>/dev/null || true
            case "$COMPILER_TYPE" in
                intel)
                    module load profile/base 2>/dev/null || true
                    module load profile/knl 2>/dev/null || true
                    module load intel/pe-xe-2018--binary 2>/dev/null || module load intel 2>/dev/null || true
                    module load intelmpi/2018--binary 2>/dev/null || module load intelmpi 2>/dev/null || true
                    module load boost/1.66.0--intelmpi--2018--binary 2>/dev/null || module load boost 2>/dev/null || true
                    if [[ "$FORCE_FFTW" == true ]]; then
                        module load fftw/3.3.7_knl--intelmpi--2018--binary 2>/dev/null || module load fftw 2>/dev/null || true
                    else
                        module load mkl/2018--binary 2>/dev/null || module load mkl 2>/dev/null || true
                    fi
                    module load cmake 2>/dev/null || true
                    ;;
                gnu)
                    module load profile/global 2>/dev/null || true
                    module load gnu/6.1.0 2>/dev/null || module load gnu 2>/dev/null || true
                    module load openmpi/1-10.3--gnu--6.1.0 2>/dev/null || module load openmpi 2>/dev/null || true
                    module load fftw/3.3.4--openmpi--1-10.3--gnu--6.1.0 2>/dev/null || module load fftw 2>/dev/null || true
                    module load boost/1.61.0--gnu--6.1.0 2>/dev/null || module load boost 2>/dev/null || true
                    module load cmake 2>/dev/null || true
                    ;;
            esac
            ;;
        cnaf)
            case "$COMPILER_TYPE" in
                intel)
                    module load compilers/gcc-4.9.2 2>/dev/null || true
                    module load compilers/intel-parallel-studio-2017 2>/dev/null || true
                    module load boost_1_56_0_gcc4_9_0 2>/dev/null || true
                    ;;
                gnu)
                    module load compilers/gcc-7.1.0 2>/dev/null || module load compilers/gcc 2>/dev/null || true
                    module load compilers/openmpi-2.1.1_gcc-7.1.0 2>/dev/null || module load openmpi 2>/dev/null || true
                    module load boost_1_64_0_gcc7_1_0 2>/dev/null || module load boost 2>/dev/null || true
                    ;;
            esac
            ;;
        edison)
            module load PrgEnv-intel 2>/dev/null || true
            if [[ "$FORCE_FFTW" == true ]]; then
                module load cray-fftw 2>/dev/null || true
            else
                module load intel-mkl 2>/dev/null || true
            fi
            module load cmake 2>/dev/null || true
            module load boost 2>/dev/null || true
            ;;
        mac)
            # macOS uses Homebrew, no modules needed
            log_info "macOS detected, expecting Homebrew-installed dependencies"
            ;;
        generic)
            log_info "Generic platform, no modules loaded"
            ;;
    esac

    # Show loaded modules
    module list 2>/dev/null || true
}

# Detect or set compiler based on platform and user choice
setup_compilers() {
    log_step "Setting up compilers"

    # Auto-detect compiler if not specified
    if [[ -z "$COMPILER_TYPE" ]]; then
        if command -v ifort &> /dev/null && [[ "$PLATFORM" != "generic" ]]; then
            COMPILER_TYPE="intel"
        elif command -v gfortran &> /dev/null; then
            COMPILER_TYPE="gnu"
        else
            log_error "No Fortran compiler found"
            exit 1
        fi
        log_info "Auto-detected compiler: $COMPILER_TYPE"
    fi

    # ScoreP instrumentation
    if [[ "$ENABLE_SCOREP" == true ]]; then
        export CC="scorep-gcc"
        export CXX="scorep-g++"
        export FC="scorep-gfortran"
        export SCOREP_WRAPPER_OFF=true
        log_info "ScoreP instrumentation enabled"
        return
    fi

    # Set compilers if not already set in environment
    case "$PLATFORM" in
        marconi|marconi-knl)
            if [[ "$COMPILER_TYPE" == "intel" ]]; then
                export CC="${CC:-/cineca/prod/opt/compilers/intel/pe-xe-2018/binary/bin/icc}"
                export CXX="${CXX:-/cineca/prod/opt/compilers/intel/pe-xe-2018/binary/bin/icpc}"
                export FC="${FC:-/cineca/prod/opt/compilers/intel/pe-xe-2018/binary/bin/ifort}"
                export CLINKER="${CXX}"
            fi
            ;;
        cnaf)
            if [[ "$COMPILER_TYPE" == "intel" ]]; then
                export CC="${CC:-/shared/software/compilers/intel_2017/compilers_and_libraries_2017.0.098/linux/bin/intel64/icc}"
                export CXX="${CXX:-/shared/software/compilers/intel_2017/compilers_and_libraries_2017.0.098/linux/bin/intel64/icpc}"
                export FC="${FC:-/shared/software/compilers/intel_2017/compilers_and_libraries_2017.0.098/linux/bin/intel64/ifort}"
            elif [[ "$COMPILER_TYPE" == "gnu" ]]; then
                export CC="${CC:-/shared/software/compilers/gcc-7.1.0/bin/gcc}"
                export CXX="${CXX:-/shared/software/compilers/gcc-7.1.0/bin/g++}"
                export FC="${FC:-/shared/software/compilers/gcc-7.1.0/bin/gfortran}"
            fi
            ;;
        mac)
            # Find gfortran from Homebrew
            if [[ -z "${FC:-}" ]]; then
                local gfortran_path
                gfortran_path=$(ls -1 /usr/local/bin/gfortran-* /opt/homebrew/bin/gfortran-* 2>/dev/null | sort -V | tail -1 || true)
                if [[ -n "$gfortran_path" ]]; then
                    export FC="$gfortran_path"
                    log_info "Using Fortran compiler: $FC"
                fi
            fi
            ;;
        generic)
            # Use system defaults
            export CC="${CC:-gcc}"
            export CXX="${CXX:-g++}"
            export FC="${FC:-gfortran}"
            ;;
    esac

    log_info "CC=$CC, CXX=$CXX, FC=$FC"
}

# Configure CMake options
configure_cmake_options() {
    log_step "Configuring CMake options"

    CMAKE_OPTS=()
    CMAKE_OPTS+=("-DCMAKE_BUILD_TYPE=$BUILD_TYPE")

    if [[ -n "$INSTALL_PREFIX" ]]; then
        CMAKE_OPTS+=("-DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX")
    fi

    if [[ -n "${FC:-}" ]]; then
        CMAKE_OPTS+=("-DCMAKE_Fortran_COMPILER=$FC")
    fi

    if [[ -n "${CC:-}" ]]; then
        CMAKE_OPTS+=("-DCMAKE_C_COMPILER=$CC")
    fi

    if [[ -n "${CXX:-}" ]]; then
        CMAKE_OPTS+=("-DCMAKE_CXX_COMPILER=$CXX")
    fi

    if [[ -n "${CLINKER:-}" ]]; then
        CMAKE_OPTS+=("-DCMAKE_LINKER=$CLINKER")
    fi

    # Platform-specific options
    case "$PLATFORM" in
        marconi-knl)
            CMAKE_OPTS+=("-DMARCONI_KNL:BOOL=TRUE")
            ;;
    esac

    # FFTW options
    if [[ "$FORCE_FFTW" == true ]]; then
        CMAKE_OPTS+=("-DFORCE_FFTW:BOOL=ON")
        case "$PLATFORM" in
            cnaf)
                CMAKE_OPTS+=("-DFFTW_ROOT_DIR=/shared/software/project/aladyn/fftw")
                ;;
            marconi-knl)
                CMAKE_OPTS+=("-DFFTW_ROOT_DIR=/cineca/prod/opt/libraries/fftw/3.3.7_knl/intelmpi--2018--binary/")
                ;;
            edison)
                CMAKE_OPTS+=("-DFFTW_USE_STATIC_LIBS:BOOL=ON")
                CMAKE_OPTS+=("-DFFTW_ROOT_DIR=/opt/cray/pe/fftw/3.3.6.2/x86_64/")
                ;;
        esac
    fi

    # MPI options
    if [[ "$FORCE_OLD_MPI" == true ]]; then
        CMAKE_OPTS+=("-DFORCE_OLD_MPI:BOOL=ON")
    fi

    # Boost options (avoid CMake's built-in FindBoost issues)
    CMAKE_OPTS+=("-DBoost_NO_BOOST_CMAKE=ON")

    # vcpkg integration
    if [[ "$USE_VCPKG" == true ]]; then
        if [[ -n "${VCPKG_ROOT:-}" ]]; then
            CMAKE_OPTS+=("-DCMAKE_TOOLCHAIN_FILE=${VCPKG_ROOT}/scripts/buildsystems/vcpkg.cmake")
        else
            log_warn "vcpkg requested but VCPKG_ROOT not set"
        fi
    fi

    log_info "CMake options: ${CMAKE_OPTS[*]}"
}

# Main build function
build() {
    local script_dir
    script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    local repo_root
    repo_root="$(cd "$script_dir/.." && pwd)"

    log_step "Building ALaDyn from: $repo_root"

    cd "$repo_root"

    # Set default install prefix
    if [[ -z "$INSTALL_PREFIX" ]]; then
        INSTALL_PREFIX="$repo_root"
    fi

    local build_dir="$repo_root/build"

    # Clean if requested
    if [[ "$CLEAN_BUILD" == true ]] && [[ -d "$build_dir" ]]; then
        log_info "Cleaning build directory"
        rm -rf "$build_dir"
    fi

    # Create build directory
    mkdir -p "$build_dir"
    cd "$build_dir"

    # Configure
    log_step "Configuring with CMake"
    cmake "${CMAKE_OPTS[@]}" ..

    # Build
    log_step "Building with $JOBS jobs"
    cmake --build . --target install -- -j"$JOBS"

    log_info "Build complete! Binary installed to: $INSTALL_PREFIX/bin/ALaDyn"
}

# Main entry point
main() {
    echo "=============================================="
    echo "  ALaDyn Unified Build Script v${VERSION}"
    echo "=============================================="

    parse_args "$@"
    detect_os
    detect_platform
    detect_jobs
    load_modules
    setup_compilers
    configure_cmake_options
    build

    echo ""
    log_info "Build completed successfully!"
    echo "=============================================="
}

main "$@"
