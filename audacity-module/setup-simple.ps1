# Audacity Cloud AI - Simple Setup Script for Windows

Write-Host "Audacity Cloud AI Module Setup" -ForegroundColor Cyan
Write-Host "==============================" -ForegroundColor Cyan
Write-Host ""

$ErrorActionPreference = "Stop"

# Check CMake
Write-Host "[1/6] Checking CMake..." -ForegroundColor Yellow
try {
    $null = Get-Command cmake
    Write-Host "  OK: CMake found" -ForegroundColor Green
}
catch {
    Write-Host "  ERROR: CMake not found!" -ForegroundColor Red
    Write-Host "  Install from: https://cmake.org/download/" -ForegroundColor Yellow
    exit 1
}

# Check Git
Write-Host "[2/6] Checking Git..." -ForegroundColor Yellow
try {
    $null = Get-Command git
    Write-Host "  OK: Git found" -ForegroundColor Green
}
catch {
    Write-Host "  ERROR: Git not found!" -ForegroundColor Red
    Write-Host "  Install from: https://git-scm.com/download/win" -ForegroundColor Yellow
    exit 1
}

# Clone or find Audacity
Write-Host "[3/6] Getting Audacity source..." -ForegroundColor Yellow
$audacityPath = "C:\Coding Projects\audacity"

if (Test-Path $audacityPath) {
    Write-Host "  OK: Audacity source found" -ForegroundColor Green
}
else {
    Write-Host "  Cloning Audacity (this may take a while)..." -ForegroundColor Yellow
    Set-Location "C:\Coding Projects"
    git clone https://github.com/audacity/audacity.git
    Set-Location audacity
    git checkout Audacity-3.4.2
    Write-Host "  OK: Audacity cloned" -ForegroundColor Green
}

# Copy module
Write-Host "[4/6] Copying module..." -ForegroundColor Yellow
$sourcePath = $PSScriptRoot
$targetPath = Join-Path $audacityPath "modules\mod-cloud-ai"

if (Test-Path $targetPath) {
    Remove-Item $targetPath -Recurse -Force
}

Copy-Item $sourcePath $targetPath -Recurse
Write-Host "  OK: Module copied" -ForegroundColor Green

# Update CMakeLists
Write-Host "[5/6] Updating CMakeLists..." -ForegroundColor Yellow
$cmakePath = Join-Path $audacityPath "modules\CMakeLists.txt"
$content = Get-Content $cmakePath -Raw

if ($content -notlike "*mod-cloud-ai*") {
    Add-Content $cmakePath "`nadd_subdirectory(mod-cloud-ai)"
    Write-Host "  OK: CMakeLists updated" -ForegroundColor Green
}
else {
    Write-Host "  OK: Already configured" -ForegroundColor Green
}

# Build
Write-Host "[6/6] Building (this will take 10-20 minutes)..." -ForegroundColor Yellow
$buildPath = Join-Path $audacityPath "build"

if (-not (Test-Path $buildPath)) {
    New-Item $buildPath -ItemType Directory | Out-Null
}

Set-Location $buildPath

# Configure
Write-Host "  Configuring CMake..." -ForegroundColor Gray
cmake -G "Visual Studio 17 2022" -A x64 .. 2>&1 | Out-Null

if ($LASTEXITCODE -ne 0) {
    Write-Host "  Trying VS 2019..." -ForegroundColor Gray
    cmake -G "Visual Studio 16 2019" -A x64 .. 2>&1 | Out-Null
}

if ($LASTEXITCODE -ne 0) {
    Write-Host "  ERROR: CMake configuration failed" -ForegroundColor Red
    Write-Host "  Install Visual Studio with C++ tools" -ForegroundColor Yellow
    exit 1
}

Write-Host "  OK: Project configured" -ForegroundColor Green

# Build
Write-Host "  Building module (please wait)..." -ForegroundColor Gray
cmake --build . --config Release --target mod-cloud-ai

if ($LASTEXITCODE -eq 0) {
    Write-Host ""
    Write-Host "SUCCESS! Module built!" -ForegroundColor Green -BackgroundColor Black
    Write-Host ""
    
    $dll = Get-ChildItem -Path . -Filter "mod-cloud-ai.dll" -Recurse | Select-Object -First 1
    
    if ($dll) {
        Write-Host "Module location:" -ForegroundColor Cyan
        Write-Host "  $($dll.FullName)" -ForegroundColor White
        Write-Host ""
        
        $response = Read-Host "Install to Audacity? (Y/N)"
        
        if ($response -eq 'Y' -or $response -eq 'y') {
            $audacityModules = "C:\Program Files\Audacity\modules"
            
            if (Test-Path $audacityModules) {
                Copy-Item $dll.FullName $audacityModules -Force
                Write-Host "Installed! Launch Audacity and check Generate menu" -ForegroundColor Green
            }
            else {
                Write-Host "Audacity not found at default location" -ForegroundColor Yellow
                Write-Host "Copy DLL manually to your Audacity modules folder" -ForegroundColor Yellow
            }
        }
    }
}
else {
    Write-Host ""
    Write-Host "BUILD FAILED - Check errors above" -ForegroundColor Red
    exit 1
}
