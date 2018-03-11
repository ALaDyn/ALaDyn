# How to build the code

1) Follow your system prerequisites (below)
2) Open your terminal and type these commands (they should be completely system independent)

```bash
git clone https://github.com/ALaDyn/ALaDyn.git
cd ALaDyn
mkdir build
cd build
cmake ..
cmake --build . --target install
```

On Windows (*not* Cygwin) the second to last line should read `cmake -G "Ninja" "-DCMAKE_TOOLCHAIN_FILE=$env:WORKSPACE\vcpkg\scripts\buildsystems\vcpkg.cmake" ..`

# Prerequisites to build the code

## Ubuntu

1) Open a Bash terminal and type the following commands

```bash
sudo apt-get update
sudo apt-get dist-upgrade
sudo apt-get install -y g++ gfortran cmake make git ninja-build libboost-all-dev libopenmpi-dev pkgconf libfftw3-dev
```

## macOS

1) If not already installed, install the XCode Command Line Tools, typing this command in a terminal:

```bash
xcode-select --install
```

2) If not already installed, install Homebrew following the [official guide](https://brew.sh/index_it.html).
3) Open the terminal and type these commands

```bash
brew update
brew upgrade
brew install gcc cmake make git ninja boost open-mpi fftw
```

## Windows (7+)

1) Install Visual Studio 2015 Community (no PGI compiler is still compatible with VS 2017)
2) Install PGI 17.10 Community Edition
3) If not already installed, please install chocolatey using the [official guide](http://chocolatey.org)
4) If you are not sure about having them updated, or even installed, please install `git`, `cmake` and an updated `Powershell`. To do so, open your Powershell with Administrator privileges and type

```PowerShell
PS \>             cinst -y git cmake powershell ninja
```

5) Restart the PC if required by chocolatey after the latest step
6) Open your Powershell with Administrator privileges, type the following command and confirm it:

```PowerShell
PS \>             Set-ExecutionPolicy unrestricted
```

7) Define a work folder, which we will call WORKSPACE in this tutorial: this could be a "Code" folder in our home, a "cpp" folder on our desktop, whatever you want. Create it if you don't already have, using your favourite method (mkdir in Powershell, or from the graphical interface in explorer). We will now define an environment variable to tell the system where our folder is. Please note down its full path. Open a Powershell (as a standard user) and type

```PowerShell
PS \>             rundll32 sysdm.cpl,EditEnvironmentVariables
```

8) In the upper part of the window that pops-up, we have to create three new environment variables: one with name `WORKSPACE` and value the full path noted down before and one with name `VCPKG_ROOT` and value `%WORKSPACE%\vcpkg`.
If it not already in the `PATH` (this is possible only if you did it before), we also need to modify the "Path" variable adding the following string (on Windows 10 you need to add a new line to insert it, on Windows Windows 7/8 it is necessary to append it using a `;` as a separator between other records):

```cmd
%PROGRAMFILES%\CMake\bin
```

9) If `vcpkg` is not installed, please follow the next procedure, otherwise please jump to #10

```PowerShell
PS \>             cd $env:WORKSPACE
PS Code>          git clone https://github.com/Microsoft/vcpkg.git
PS Code>          cd vcpkg
PS Code\vcpkg>    .\bootstrap-vcpkg.bat
```

10) Open a Powershell with Administrator privileges and type

```PowerShell
PS \>             cd $env:WORKSPACE
PS Code>          cd vcpkg
PS Code\vcpkg>    .\vcpkg integrate install
```

11) Open a Powershell (as a standard user) and type (the last command requires a confirmation and is used to clean up unnecessary files)

```PowerShell
PS \>             cd $env:WORKSPACE
PS Code>          cd vcpkg
PS Code\vcpkg>    .\vcpkg install boost:x64-windows-static fftw:x64-windows-static msmpi:x64-windows-static
PS Code\vcpkg>    rmdir .\buildtrees\
```

Pay attention to possible failures due to missing MS-MPI SDK installation. A message should be prompted to the shell describing how to fix it.

### Upgrade software

1) To update software installed with Chocolatey, open a Powershell with Administrator privileges and type

```PowerShell
PS \>             cup all -y
```

2) To update libraries installed with vcpkg, open a Powershell (as a standard user), type these commands and follow on-screen instructions

```PowerShell
PS \>             cd $env:WORKSPACE
PS Code>          cd vcpkg
PS Code>          git pull
PS Code>          .\bootstrap-vcpkg.bat
PS Code>          .\vcpkg update
PS Code>          .\vcpkg upgrade --no-dry-run
```

## Cygwin

1) If not already installed, please install chocolatey using the [official guide](http://chocolatey.org)
2) Open a Powershell with Administrator privileges and type

```PowerShell
PS \>             cinst -y cygwin
```

3) Open a Powershell (as a standard user) and type (remove the `_x64` suffix in the second line if your operating system is 32 bit)

```PowerShell
PS \>             Invoke-WebRequest https://cygwin.com/setup-x86_64.exe -OutFile $env:WORKSPACE\cygwin-setup.exe
PS \>             .\cygwin-setup --quiet-mode --no-shortcuts --no-startmenu --no-desktop --upgrade-also --packages gcc-g++,gcc-gfortran,cmake,git,libboost-devel
```
