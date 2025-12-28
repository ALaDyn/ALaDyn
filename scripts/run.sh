#!/usr/bin/env bash

#*****************************************************************************************************
#                            Copyright 2008-2025  The ALaDyn Collaboration
#*****************************************************************************************************
#
# ALaDyn Unified Run Script
#
# This script generates and optionally submits job scripts for various HPC batch systems.
# Supports SLURM, LSF (bsub), and direct mpirun execution.
#
# Usage:
#   ./scripts/run.sh [options]
#
# Options:
#   -h, --help              Show this help message
#   -n, --nodes N           Number of nodes (default: 1)
#   -t, --tasks N           Total number of MPI tasks (default: auto-detect per platform)
#   -c, --tasks-per-node N  Tasks per node (alternative to -t)
#   -w, --walltime TIME     Wall time limit (default: 01:00:00)
#   -p, --platform NAME     HPC platform: marconi-knl, cnaf, generic (default: auto-detect)
#   -q, --queue NAME        Queue/partition name (default: platform-specific)
#   -a, --account NAME      Account/project name for billing
#   -j, --job-name NAME     Job name (default: aladyn)
#   -e, --executable PATH   Path to ALaDyn executable (default: ./ALaDyn)
#   -i, --input PATH        Path to input.nml (default: current directory)
#   --compiler TYPE         Compiler used: gnu, intel (for module loading)
#   --fftw                  Load FFTW modules instead of MKL
#   --scorep                Enable ScoreP profiling
#   --dry-run               Generate job script without submitting
#   --local                 Run directly with mpirun (no batch submission)
#
# Environment Variables:
#   SLURM_JOB_ACCOUNT       Default account for SLURM
#   ALADYN_EXECUTABLE       Default executable path
#
# Examples:
#   ./scripts/run.sh -n 2 -t 136 -w 02:00:00 -a MyProject
#   ./scripts/run.sh --local -t 4                    # Run locally with 4 MPI tasks
#   ./scripts/run.sh -p marconi-knl --dry-run        # Generate script only
#   ./scripts/run.sh -p cnaf -t 144 --compiler gnu   # CNAF with GNU compiler

set -euo pipefail

# Script version
VERSION="2.0.0"

# Default values
NODES=1
TASKS=""
TASKS_PER_NODE=""
WALLTIME="01:00:00"
PLATFORM=""
QUEUE=""
ACCOUNT=""
JOB_NAME="aladyn"
EXECUTABLE="./ALaDyn"
INPUT_DIR="."
COMPILER_TYPE="intel"
USE_FFTW=false
ENABLE_SCOREP=false
DRY_RUN=false
LOCAL_RUN=false

# Output files
STDOUT_FILE="opic.txt"
STDERR_FILE="epic.txt"
JOB_SCRIPT="job_aladyn.sh"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_step() { echo -e "${BLUE}[STEP]${NC} $1"; }

usage() {
    grep '^#' "$0" | grep -v '#!/' | sed 's/^# \?//'
    exit 0
}

parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help) usage ;;
            -n|--nodes) NODES="$2"; shift 2 ;;
            -t|--tasks) TASKS="$2"; shift 2 ;;
            -c|--tasks-per-node) TASKS_PER_NODE="$2"; shift 2 ;;
            -w|--walltime) WALLTIME="$2"; shift 2 ;;
            -p|--platform) PLATFORM="$2"; shift 2 ;;
            -q|--queue) QUEUE="$2"; shift 2 ;;
            -a|--account) ACCOUNT="$2"; shift 2 ;;
            -j|--job-name) JOB_NAME="$2"; shift 2 ;;
            -e|--executable) EXECUTABLE="$2"; shift 2 ;;
            -i|--input) INPUT_DIR="$2"; shift 2 ;;
            --compiler) COMPILER_TYPE="$2"; shift 2 ;;
            --fftw) USE_FFTW=true; shift ;;
            --scorep) ENABLE_SCOREP=true; shift ;;
            --dry-run) DRY_RUN=true; shift ;;
            --local) LOCAL_RUN=true; shift ;;
            *) log_error "Unknown option: $1"; usage ;;
        esac
    done
}

detect_platform() {
    if [[ -n "$PLATFORM" ]]; then
        log_info "Using specified platform: $PLATFORM"
        return
    fi

    local hostname
    hostname=$(hostname 2>/dev/null || echo "unknown")

    if [[ "$hostname" == *"marconi"* ]] || [[ -d "/cineca" ]]; then
        if [[ -n "${SLURM_JOB_PARTITION:-}" ]] && [[ "$SLURM_JOB_PARTITION" == *"knl"* ]]; then
            PLATFORM="marconi-knl"
        else
            PLATFORM="marconi"
        fi
    elif [[ "$hostname" == *"cnaf"* ]] || [[ -d "/shared/software" ]]; then
        PLATFORM="cnaf"
    else
        PLATFORM="generic"
    fi

    log_info "Auto-detected platform: $PLATFORM"
}

detect_batch_system() {
    if [[ "$LOCAL_RUN" == true ]]; then
        BATCH_SYSTEM="local"
        return
    fi

    if command -v sbatch &> /dev/null; then
        BATCH_SYSTEM="slurm"
    elif command -v bsub &> /dev/null; then
        BATCH_SYSTEM="lsf"
    else
        BATCH_SYSTEM="local"
        log_warn "No batch system detected, will use local mpirun"
    fi

    log_info "Batch system: $BATCH_SYSTEM"
}

set_platform_defaults() {
    case "$PLATFORM" in
        marconi-knl)
            TASKS_PER_NODE="${TASKS_PER_NODE:-68}"
            QUEUE="${QUEUE:-knl_usr_prod}"
            ;;
        marconi)
            TASKS_PER_NODE="${TASKS_PER_NODE:-48}"
            QUEUE="${QUEUE:-skl_usr_prod}"
            ;;
        cnaf)
            TASKS_PER_NODE="${TASKS_PER_NODE:-24}"
            QUEUE="${QUEUE:-hpc_inf}"
            ;;
        generic)
            TASKS_PER_NODE="${TASKS_PER_NODE:-4}"
            ;;
    esac

    # Calculate total tasks if not specified
    if [[ -z "$TASKS" ]]; then
        TASKS=$((NODES * TASKS_PER_NODE))
    fi

    log_info "Configuration: $NODES node(s), $TASKS total tasks, $TASKS_PER_NODE tasks/node"
}

generate_module_commands() {
    local modules=""

    case "$PLATFORM" in
        marconi-knl)
            modules+="module purge\n"
            modules+="module load env-knl\n"
            if [[ "$COMPILER_TYPE" == "intel" ]]; then
                modules+="module load profile/base\n"
                modules+="module load profile/knl\n"
                modules+="module load intel/pe-xe-2018--binary\n"
                modules+="module load intelmpi/2018--binary\n"
                modules+="module load boost/1.66.0--intelmpi--2018--binary\n"
                if [[ "$USE_FFTW" == true ]]; then
                    modules+="module load fftw/3.3.7_knl--intelmpi--2018--binary\n"
                else
                    modules+="module load mkl/2018--binary\n"
                fi
            elif [[ "$COMPILER_TYPE" == "gnu" ]]; then
                modules+="module load profile/global\n"
                modules+="module load gnu/6.1.0\n"
                modules+="module load openmpi/1-10.3--gnu--6.1.0\n"
                modules+="module load fftw/3.3.4--openmpi--1-10.3--gnu--6.1.0\n"
                modules+="module load boost/1.61.0--gnu--6.1.0\n"
            fi
            ;;
        marconi)
            modules+="module purge\n"
            if [[ "$COMPILER_TYPE" == "intel" ]]; then
                modules+="module load intel/pe-xe-2018--binary\n"
                modules+="module load intelmpi/2018--binary\n"
                modules+="module load boost/1.66.0--intelmpi--2018--binary\n"
                modules+="module load mkl/2018--binary\n"
            fi
            ;;
        cnaf)
            modules+="module purge\n"
            if [[ "$COMPILER_TYPE" == "intel" ]]; then
                modules+="module load compilers/gcc-4.9.2\n"
                modules+="module load compilers/intel-parallel-studio-2017\n"
                modules+="module load boost_1_56_0_gcc4_9_0\n"
            elif [[ "$COMPILER_TYPE" == "gnu" ]]; then
                modules+="module load compilers/gcc-4.9.2\n"
                modules+="module load compilers/openmpi-1.8.1_gcc-4.9.0_with_cuda6.5\n"
                modules+="module load boost_1_56_0_gcc4_9_0\n"
            fi
            ;;
    esac

    echo -e "$modules"
}

generate_mpirun_command() {
    local mpi_cmd=""

    case "$PLATFORM" in
        marconi*|generic)
            mpi_cmd="mpirun ${EXECUTABLE}"
            ;;
        cnaf)
            if [[ "$COMPILER_TYPE" == "intel" ]]; then
                mpi_cmd="export TMI_CONFIG=/shared/software/compilers/impi/intel64/etc/tmi.conf\n"
                mpi_cmd+="/shared/software/compilers/impi/intel64/bin/mpirun -np ${TASKS} -genv PSM_SHAREDCONTEXTS_MAX 8 -genv I_MPI_FABRICS shm:tmi ${EXECUTABLE}"
            else
                mpi_cmd="/usr/share/lsf/9.1/linux2.6-glibc2.3-x86_64/bin/mpirun.lsf env PSM_SHAREDCONTEXTS_MAX=8 ${EXECUTABLE}"
            fi
            ;;
    esac

    echo -e "$mpi_cmd"
}

generate_slurm_script() {
    log_step "Generating SLURM job script: $JOB_SCRIPT"

    local modules
    modules=$(generate_module_commands)
    local mpi_cmd
    mpi_cmd=$(generate_mpirun_command)

    cat > "$JOB_SCRIPT" << EOF
#!/bin/bash
#SBATCH --job-name=${JOB_NAME}
#SBATCH --nodes=${NODES}
#SBATCH --ntasks=${TASKS}
#SBATCH --ntasks-per-node=${TASKS_PER_NODE}
#SBATCH --time=${WALLTIME}
#SBATCH --output=job_%j.out
#SBATCH --error=job_%j.err
EOF

    if [[ -n "$QUEUE" ]]; then
        echo "#SBATCH --partition=${QUEUE}" >> "$JOB_SCRIPT"
    fi

    if [[ -n "$ACCOUNT" ]]; then
        echo "#SBATCH --account=${ACCOUNT}" >> "$JOB_SCRIPT"
    fi

    cat >> "$JOB_SCRIPT" << EOF

# Load modules
${modules}
# ScoreP settings
EOF

    if [[ "$ENABLE_SCOREP" == true ]]; then
        cat >> "$JOB_SCRIPT" << EOF
export SCOREP_ENABLE_PROFILING=true
export SCOREP_ENABLE_TRACING=false
export SCOREP_EXPERIMENT_DIRECTORY=profile
EOF
    fi

    cat >> "$JOB_SCRIPT" << EOF

# Change to input directory
cd ${INPUT_DIR}

# Run ALaDyn
echo "Starting ALaDyn on \${SLURM_NNODES} nodes with \${SLURM_NTASKS} tasks"
echo "Start time: \$(date)"

${mpi_cmd} >> ${STDOUT_FILE} 2>> ${STDERR_FILE}

echo "End time: \$(date)"
EOF

    chmod +x "$JOB_SCRIPT"
    log_info "Generated: $JOB_SCRIPT"
}

generate_lsf_script() {
    log_step "Generating LSF job script: $JOB_SCRIPT"

    local modules
    modules=$(generate_module_commands)
    local mpi_cmd
    mpi_cmd=$(generate_mpirun_command)

    cat > "$JOB_SCRIPT" << EOF
#!/bin/bash
#BSUB -J ${JOB_NAME}
#BSUB -o %J.out
#BSUB -e %J.err
#BSUB -q ${QUEUE}
#BSUB -n ${TASKS}
EOF

    if [[ "$COMPILER_TYPE" == "gnu" ]]; then
        echo "#BSUB -a openmpi" >> "$JOB_SCRIPT"
    fi

    cat >> "$JOB_SCRIPT" << EOF

# Load modules
${modules}
EOF

    if [[ "$ENABLE_SCOREP" == true ]]; then
        cat >> "$JOB_SCRIPT" << EOF
# ScoreP settings
export SCOREP_ENABLE_PROFILING=true
export SCOREP_ENABLE_TRACING=false
export SCOREP_EXPERIMENT_DIRECTORY=profile
EOF
    fi

    cat >> "$JOB_SCRIPT" << EOF

# Change to input directory
cd ${INPUT_DIR}

# Run ALaDyn
echo "Starting ALaDyn with ${TASKS} tasks"
${mpi_cmd} >> ${STDOUT_FILE} 2>> ${STDERR_FILE}
EOF

    chmod +x "$JOB_SCRIPT"
    log_info "Generated: $JOB_SCRIPT"
}

run_local() {
    log_step "Running ALaDyn locally with $TASKS MPI tasks"

    cd "$INPUT_DIR"

    # Create output files
    touch "$STDOUT_FILE" "$STDERR_FILE"

    echo "Starting ALaDyn with $TASKS tasks"
    echo "Start time: $(date)"

    if command -v mpirun &> /dev/null; then
        mpirun -np "$TASKS" "$EXECUTABLE" >> "$STDOUT_FILE" 2>> "$STDERR_FILE"
    elif command -v mpiexec &> /dev/null; then
        mpiexec -n "$TASKS" "$EXECUTABLE" >> "$STDOUT_FILE" 2>> "$STDERR_FILE"
    else
        log_error "No MPI launcher found (mpirun or mpiexec)"
        exit 1
    fi

    echo "End time: $(date)"
    log_info "Run completed. Output in $STDOUT_FILE, errors in $STDERR_FILE"
}

submit_job() {
    if [[ "$DRY_RUN" == true ]]; then
        log_info "Dry run - job script generated but not submitted"
        echo ""
        echo "To submit manually:"
        case "$BATCH_SYSTEM" in
            slurm) echo "  sbatch $JOB_SCRIPT" ;;
            lsf)   echo "  bsub < $JOB_SCRIPT" ;;
        esac
        return
    fi

    log_step "Submitting job"

    case "$BATCH_SYSTEM" in
        slurm)
            sbatch "$JOB_SCRIPT"
            ;;
        lsf)
            bsub < "$JOB_SCRIPT"
            ;;
        local)
            run_local
            ;;
    esac
}

main() {
    echo "=============================================="
    echo "  ALaDyn Unified Run Script v${VERSION}"
    echo "=============================================="

    parse_args "$@"
    detect_platform
    detect_batch_system
    set_platform_defaults

    # Use environment variable for account if not specified
    if [[ -z "$ACCOUNT" ]] && [[ -n "${SLURM_JOB_ACCOUNT:-}" ]]; then
        ACCOUNT="$SLURM_JOB_ACCOUNT"
    fi

    # Use environment variable for executable if not specified
    if [[ "$EXECUTABLE" == "./ALaDyn" ]] && [[ -n "${ALADYN_EXECUTABLE:-}" ]]; then
        EXECUTABLE="$ALADYN_EXECUTABLE"
    fi

    # Verify executable exists
    if [[ ! -f "$EXECUTABLE" ]] && [[ "$DRY_RUN" == false ]]; then
        log_warn "Executable not found: $EXECUTABLE"
    fi

    case "$BATCH_SYSTEM" in
        slurm)
            generate_slurm_script
            ;;
        lsf)
            generate_lsf_script
            ;;
        local)
            # No script generation for local runs
            ;;
    esac

    submit_job

    echo ""
    log_info "Done!"
    echo "=============================================="
}

main "$@"
