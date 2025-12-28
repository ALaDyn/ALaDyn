title: How to build

# How to build the code

ALaDyn is built using **CMake** (minimum version 3.15) and can optionally use **vcpkg** for dependency management.

## Quick Start

1) Clone ALaDyn with submodules:

```bash
git clone https://github.com/ALaDyn/ALaDyn.git
cd ALaDyn
git submodule update --init --recursive
```

2) Follow your system prerequisites (below)

3) Build using one of the supported methods

## Supported Platforms

We support running ALaDyn only on **x86-64 CPUs** with **64-bit operating systems**.
Supported compilers: **GNU (gfortran)**, **Intel (ifort)**

## Dependencies

- **MPI** (OpenMPI or MPICH)
- **FFTW3** (or Intel MKL)
- **CMake** (>= 3.15)
- **PowerShell** (for the recommended build script)

## Building with vcpkg (Recommended)

The recommended way to build ALaDyn is using the PowerShell build script with vcpkg integration:

```powershell
./cmake/build.ps1 -UseVCPKG -DisableInteractive -DoNotUpdateVCPKG -DoNotUpdateTOOL -DoNotDeleteBuildFolder
```

This script handles:
- vcpkg setup and dependency installation
- CMake configuration
- Building and installing the executable

## Building with Plain CMake

If you have system-installed dependencies, you can build directly with CMake:

```bash
mkdir -p build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
cmake --build . --parallel
cmake --install .
```

## Prerequisites

### Ubuntu

Open a Bash terminal and install the required dependencies:

```bash
sudo apt-get update
sudo apt-get install -y gfortran cmake make git ninja-build openmpi-bin libopenmpi-dev libfftw3-dev libfftw3-mpi-dev pkg-config
```

**For vcpkg build (optional additional packages):**

```bash
sudo apt-get install -y yasm nasm mono-devel
```

**PowerShell installation (required for the recommended build script):**

Follow the [official Microsoft guide](https://learn.microsoft.com/en-us/powershell/scripting/install/install-ubuntu) or use these commands:

```bash
# Install pre-requisite packages
sudo apt-get install -y wget apt-transport-https software-properties-common

# Get the version of Ubuntu
source /etc/os-release

# Download the Microsoft repository keys
wget -q https://packages.microsoft.com/config/ubuntu/$VERSION_ID/packages-microsoft-prod.deb

# Register the Microsoft repository keys
sudo dpkg -i packages-microsoft-prod.deb

# Delete the Microsoft repository keys file
rm packages-microsoft-prod.deb

# Update the list of packages after we added packages.microsoft.com
sudo apt-get update

# Install PowerShell
sudo apt-get install -y powershell
```

After installation, you can start PowerShell with `pwsh`.

### macOS

1) Install the XCode Command Line Tools (if not already installed):

```bash
xcode-select --install
```

2) Install Homebrew following the [official guide](https://brew.sh/).

3) Install dependencies:

```bash
brew update
brew upgrade
brew install gcc cmake make git ninja open-mpi fftw pkg-config
```

**For vcpkg build (optional additional packages):**

```bash
brew install libomp yasm nasm automake autoconf-archive mono
```

**Note:** You may need to set up the gfortran symlink for CMake:

```bash
# Find gfortran version
GFORTRAN_PATH=$(brew --prefix gcc)/bin/gfortran-$(brew list --versions gcc | awk '{print $2}' | cut -d. -f1)
sudo ln -sf "$GFORTRAN_PATH" /usr/local/bin/gfortran
```

**PowerShell installation (required for the recommended build script):**

Follow the [official Microsoft guide](https://learn.microsoft.com/en-us/powershell/scripting/install/install-powershell-on-macos) or install via Homebrew:

```bash
brew install --cask powershell
```

After installation, you can start PowerShell with `pwsh`.

### Windows (via WSL)

Native Windows builds are **not supported** due to the complexity of Fortran compiler installation on Windows. Instead, we recommend using **Windows Subsystem for Linux (WSL)**, which provides a full Linux environment.

1) Install WSL following the [official Microsoft guide](https://learn.microsoft.com/en-us/windows/wsl/install):

```powershell
wsl --install
```

2) After WSL is installed and you have an Ubuntu distribution running, follow the **Ubuntu** instructions above to install dependencies and build ALaDyn.

3) Your Windows files are accessible from WSL at `/mnt/c/` (for the C: drive), and you can run the built executable directly within the WSL environment.

## Running ALaDyn

### On HPC Systems

Use the unified run script to generate and submit job scripts:

```bash
# Generate and submit job script
./scripts/run.sh -n 2 -t 136 -a MyProject

# Dry run (see generated script without submitting)
./scripts/run.sh -p marconi-knl --dry-run

# Specify queue and walltime
./scripts/run.sh -n 4 -t 272 -w 02:00:00 -q <queue_name> -a <account>
```

### Local Execution

```bash
# Run directly with mpirun
./scripts/run.sh --local -t 4
```

### Manual Execution

```bash
mpirun -np <num_tasks> ./ALaDyn
```

## Build Script Options

The `cmake/build.ps1` script supports many options:

| Option | Description |
|--------|-------------|
| `-UseVCPKG` | Use vcpkg for dependencies |
| `-DisableInteractive` | Disable interactive prompts (for CI) |
| `-DoNotUpdateVCPKG` | Skip vcpkg update |
| `-DoNotUpdateTOOL` | Skip tool repository update |
| `-DoNotDeleteBuildFolder` | Keep build folder after completion |
| `-BuildDebug` | Build with debug configuration |

Run `Get-Help ./cmake/build.ps1` for the full list of options.

## Troubleshooting

### Git Submodules

If the `cmake/` directory is empty, initialize the submodules:

```bash
git submodule update --init --recursive
```

### MPI Issues

Some MPI implementations require special handling. Check the `FORCE_OLD_MPI` CMake option if using legacy `mpif.h`.

### Compiler Not Found

Ensure your Fortran compiler is in the PATH. On macOS, you may need to explicitly set `FC`:

```bash
export FC=/usr/local/bin/gfortran
```

## Deprecated Build Scripts

Legacy build scripts for specific HPC systems are available in `scripts/build/deprecated/` for reference but are no longer actively maintained
