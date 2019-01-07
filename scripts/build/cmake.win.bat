@Echo Off
if exist "%PROGRAMFILES(x86)%\Microsoft Visual Studio\2017\Professional\VC\Auxiliary\Build\vcvarsall.bat" (
  call "%PROGRAMFILES(x86)%\Microsoft Visual Studio\2017\Professional\VC\Auxiliary\Build\vcvarsall.bat" x64
) else (
  if exist "%PROGRAMFILES(x86)%\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat" (
    call "%PROGRAMFILES(x86)%\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
  ) else (
    Echo Unable to find Visual Studio 2017
    Exit /B 1
  )
)

set PGI=%PROGRAMFILES%\PGI
set PATH=%PROGRAMFILES%\PGI\flexlm;%PATH%
set PATH=%PGI%\win64\18.10\bin;%PATH%
set PATH=%PATH%;.
set FLEXLM_BATCH=1
title PGI 18.10
echo PGI 18.10 Enabled

rmdir /S /Q build
mkdir build
cd build
cmake -G "NMake Makefiles" "-DCMAKE_TOOLCHAIN_FILE=%WORKSPACE%\vcpkg\scripts\buildsystems\vcpkg.cmake" -DCMAKE_BUILD_TYPE="Release" -DVCPKG_TARGET_TRIPLET="x64-windows-static" ..
cmake --build . --target install
REM nmake VERBOSE=1
cd ..
