#!/bin/bash
set -e

# Determine the repository root directory
# Works whether called as ./ford.sh from scripts/ or ./scripts/ford.sh from root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Find FORD project file, name is given by $1
ROOT=$(find "${REPO_ROOT}" -name $1 -exec dirname {} \;)
echo "FORD project file located in ${ROOT}"
# Get output directory, where documentation is written
# This can later be used for publishing
OUTPUT_DIR=$(grep 'output_dir' ${ROOT}/$1| sed 's/output_dir: \(.*\)/\1/')
OUTPUT_DIR="${ROOT}/${OUTPUT_DIR}"
echo "Documentation written to ${OUTPUT_DIR}"
# Set output directory variable (GitHub Actions)
echo "output_dir=${OUTPUT_DIR}" >> $GITHUB_OUTPUT 2>/dev/null || echo ::set-output name=output_dir::$OUTPUT_DIR
# Build documentation
echo "Building documentation" 
ford ${ROOT}/$1
