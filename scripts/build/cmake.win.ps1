#!/usr/bin/env pwsh

$env:PGI = "$env:PROGRAMFILES/PGI"
$env:Path = "$env:PROGRAMFILES/PGI/flexlm;$env:Path"
$env:Path = "$env:PGI/win64/18.10/bin;$env:Path"
$env:Path = "$env:Path;."
$env:FLEXLM_BATCH = 1
Write-Host "PGI 18.10 Enabled"

Remove-Item .\build -Force -Recurse -ErrorAction SilentlyContinue
New-Item -Path .\build -ItemType directory -Force
Set-Location build

cmake -G "NMake Makefiles" "-DCMAKE_TOOLCHAIN_FILE=$env:WORKSPACE\vcpkg\scripts\buildsystems\vcpkg.cmake" "-DVCPKG_TARGET_TRIPLET=x64-windows" "-DCMAKE_BUILD_TYPE=Release" ..
cmake --build . --target install

Set-Location ..
