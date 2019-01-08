# How to build the code

1) Follow your system prerequisites (below)
2) Clone ALaDyn from this repository, or download a stable release 

```bash
git clone https://github.com/ALaDyn/ALaDyn.git
cd ALaDyn
```

3) Use the best script depending to your configuration. There are many examples inside `scripts/build`. For example:

```bash
./scripts/build/cmake.generic
```

Note that some scripts only work in a specific environment or shell.  
We support running `ALaDyn` only on x86-64 CPUs, with 64 bit operating systems.

## Prerequisites

### Ubuntu

1) Open a Bash terminal and type the following commands

```bash
sudo apt-get update
sudo apt-get dist-upgrade
sudo apt-get install -y g++ gfortran cmake make git ninja-build libboost-all-dev libopenmpi-dev pkgconf libfftw3-dev pkg-config
```

### macOS

1) If not already installed, install the XCode Command Line Tools, typing this command in a terminal:

```bash
xcode-select --install
```

2) If not already installed, install Homebrew following the [official guide](https://brew.sh/index_it.html).
3) Open the terminal and type these commands

```bash
brew update
brew upgrade
brew install gcc cmake make git ninja boost open-mpi fftw pkg-config
```

### Windows (7+) - PGI Compiler

1) Install Visual Studio 2017 from the [official website](https://www.visualstudio.com/)
2) Install MS-MPI 9.0 from the [official website](https://www.microsoft.com/en-us/download/details.aspx?id=56511)
3) Open your Powershell with Administrator privileges, type the following command and confirm it:

```PowerShell
PS \>                 Set-ExecutionPolicy unrestricted
```

4) If not already installed, please install chocolatey using the [official guide](http://chocolatey.org)
5) If you are not sure about having them updated, or even installed, please install `git`, `javaruntime`, `cmake` and an updated `Powershell`. To do so, open your Powershell with Administrator privileges and type

```PowerShell
PS \>                 cinst -y git cmake powershell javaruntime
```

6) Restart the PC if required by chocolatey after the latest step
7) Install PGI 18.10 Community Edition from the [official website](https://www.pgroup.com/products/community.htm) (it is better to avoid installing MS-MPI, JRE and Cygwin from the PGI installer)
8) Activate license for PGI 18.10 Community Edition (run as Administrator the link for the License Manager in your start menu)
9) Define a work folder, which we will call WORKSPACE in this tutorial: this could be a "Code" folder in our home, a "cpp" folder on our desktop, whatever you want. Create it if you don't already have, using your favourite method (mkdir in Powershell, or from the graphical interface in explorer). We will now define an environment variable to tell the system where our folder is. Please note down its full path. Open a Powershell (as a standard user) and type

```PowerShell
PS \>                 rundll32 sysdm.cpl,EditEnvironmentVariables
```

10) In the upper part of the window that pops-up, we have to create two new environment variables: one with name `WORKSPACE` and value the full path noted down before and one with name `VCPKG_ROOT` and value `%WORKSPACE%\vcpkg`.
If it not already in the `PATH` (this is possible only if you did it before), we also need to modify the "Path" variable adding the following string (on Windows 10 you need to add a new line to insert it, on Windows Windows 7/8 it is necessary to append it using a `;` as a separator between other records):

```cmd
                      %PROGRAMFILES%\CMake\bin
```

11) If `vcpkg` is not installed, please follow the next procedure, otherwise please jump to #13

```PowerShell
PS \>                 cd $env:WORKSPACE
PS Code>              git clone https://github.com/Microsoft/vcpkg.git
PS Code>              cd vcpkg
PS Code\vcpkg>        .\bootstrap-vcpkg.bat
```

12) Open a Powershell with Administrator privileges and type

```PowerShell
PS \>                 cd $env:WORKSPACE
PS Code>              cd vcpkg
PS Code\vcpkg>        .\vcpkg integrate install
```

13) Open a Powershell (as a standard user) and type (the last command requires a confirmation and is used to clean up unnecessary files)

```PowerShell
PS \>                 cd $env:WORKSPACE
PS Code>              cd vcpkg
PS Code\vcpkg>        .\vcpkg install boost:x64-windows-static fftw3:x64-windows-static msmpi:x64-windows-static
PS Code\vcpkg>        rmdir .\buildtrees\
PS Code\vcpkg>        cd $env:WORKSPACE
PS Code>              git clone https://github.com/ALaDyn/ALaDyn
```

14) Open a `cmd` shell and build `ALaDyn` using the `scripts\build\cmake.win.bat` script or open a `powershell` shell and build `ALaDyn` using the `scripts\build\cmake.win.ps1` script

```cmd
X:\>                  cd %WORKSPACE%
X:\Code>              cd ALaDyn
X:\Code\ALaDyn>       .\scripts\build\cmake.win.bat
```

```PowerShell
PS \>                 cd $env:WORKSPACE
PS Code>              cd ALaDyn
PS Code\ALaDyn>       .\scripts\build\cmake.win.ps1
```

#### Upgrade software

1) To update software installed with Chocolatey, open a Powershell with Administrator privileges and type

```PowerShell
PS \>                 cup all -y
```

2) To update libraries installed with vcpkg, open a Powershell (as a standard user), type these commands and follow on-screen instructions

```PowerShell
PS \>                 cd $env:WORKSPACE
PS Code>              cd vcpkg
PS Code>              git pull
PS Code>              .\bootstrap-vcpkg.bat
PS Code>              .\vcpkg update
PS Code>              .\vcpkg upgrade --no-dry-run
```

### Cygwin

1) If not already installed, please install chocolatey using the [official guide](http://chocolatey.org)
2) Open a Powershell with Administrator privileges and type

```PowerShell
PS \>                 cinst -y cygwin
```

3) Open a Powershell (as a standard user) and type

```PowerShell
PS \>                 Invoke-WebRequest https://cygwin.com/setup-x86_64.exe -OutFile $env:WORKSPACE\cygwin-setup.exe
PS \>                 cd $env:WORKSPACE
PS Code>              .\cygwin-setup --quiet-mode --no-shortcuts --no-startmenu --no-desktop --upgrade-also --packages gcc-g++,libopenmpi-devel,gcc-fortran,cmake,fftw3,libfftw3-devel,libboost-devel,zlib-devel,pkg-config
```
