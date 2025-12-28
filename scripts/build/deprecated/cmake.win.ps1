#!/usr/bin/env pwsh

$env:PGI = "${env:PROGRAMFILES}/PGI"
$env:Path = "${env:PROGRAMFILES}/PGI/flexlm;$env:Path"
$env:Path = "${env:PROGRAMFILES}/PGI/win64/19.10/bin;$env:Path"
$env:Path = "${env:Path};."
$env:FLEXLM_BATCH = 1
Write-Host "PGI 19.10 Enabled"

#Remove-Item .\build_win -Force -Recurse -ErrorAction SilentlyContinue
New-Item -Path .\build_win -ItemType directory -Force
Set-Location build_win

cmake -G "NMake Makefiles" "-DCMAKE_TOOLCHAIN_FILE=${env:WORKSPACE}\vcpkg\scripts\buildsystems\vcpkg.cmake" "-DVCPKG_TARGET_TRIPLET=x64-windows" "-DCMAKE_BUILD_TYPE=Release" ..
cmake --build . --target install

Set-Location ..
