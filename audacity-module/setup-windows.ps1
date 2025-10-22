# Audacity Cloud AI Module - Windows Setup Script
# Run this from the audacity-module directory

Write-Host @"
╔════════════════════════════════════════════════╗
║  Audacity Cloud AI Module - Windows Setup     ║
╔════════════════════════════════════════════════╝
"@ -ForegroundColor Cyan

$audacitySource = "C:\Coding Projects\audacity"
$moduleSource = $PSScriptRoot

Write-Host "`n[1/6] Checking prerequisites..." -ForegroundColor Yellow

# Check CMake
if (-not (Get-Command cmake -ErrorAction SilentlyContinue)) {
    Write-Host "✗ CMake not found!" -ForegroundColor Red
    Write-Host "  Please install from: https://cmake.org/download/" -ForegroundColor Yellow
    Write-Host "  Make sure to check 'Add CMake to system PATH'" -ForegroundColor Yellow
    exit 1
}
Write-Host "✓ CMake found" -ForegroundColor Green

# Check Git
if (-not (Get-Command git -ErrorAction SilentlyContinue)) {
    Write-Host "✗ Git not found!" -ForegroundColor Red
    Write-Host "  Please install from: https://git-scm.com/download/win" -ForegroundColor Yellow
    exit 1
}
Write-Host "✓ Git found" -ForegroundColor Green

# Check Visual Studio
$vsWhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
if (Test-Path $vsWhere) {
    $vsPath = & $vsWhere -latest -property installationPath
    Write-Host "✓ Visual Studio found: $vsPath" -ForegroundColor Green
} else {
    Write-Host "⚠ Visual Studio not detected" -ForegroundColor Yellow
    Write-Host "  Install from: https://visualstudio.microsoft.com/downloads/" -ForegroundColor Yellow
    Write-Host "  Required: Desktop development with C++" -ForegroundColor Yellow
}

Write-Host "`n[2/6] Checking Audacity source..." -ForegroundColor Yellow

if (-not (Test-Path $audacitySource)) {
    Write-Host "Audacity source not found. Cloning..." -ForegroundColor Yellow
    
    Push-Location "C:\Coding Projects"
    git clone https://github.com/audacity/audacity.git
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✓ Audacity cloned" -ForegroundColor Green
        Set-Location audacity
        git checkout Audacity-3.4.2
    } else {
        Write-Host "✗ Failed to clone Audacity" -ForegroundColor Red
        Pop-Location
        exit 1
    }
    Pop-Location
} else {
    Write-Host "✓ Audacity source found" -ForegroundColor Green
}

Write-Host "`n[3/6] Copying module to Audacity..." -ForegroundColor Yellow

$targetDir = Join-Path $audacitySource "modules\mod-cloud-ai"

if (Test-Path $targetDir) {
    Write-Host "Module directory exists, removing old version..." -ForegroundColor Yellow
    Remove-Item $targetDir -Recurse -Force
}

Copy-Item $moduleSource $targetDir -Recurse
Write-Host "✓ Module copied" -ForegroundColor Green

Write-Host "`n[4/6] Updating Audacity CMakeLists.txt..." -ForegroundColor Yellow

$cmakeListsPath = Join-Path $audacitySource "modules\CMakeLists.txt"
$cmakeContent = Get-Content $cmakeListsPath -Raw

if ($cmakeContent -notmatch "mod-cloud-ai") {
    Add-Content $cmakeListsPath "`nadd_subdirectory(mod-cloud-ai)"
    Write-Host "✓ CMakeLists.txt updated" -ForegroundColor Green
} else {
    Write-Host "✓ CMakeLists.txt already configured" -ForegroundColor Green
}

Write-Host "`n[5/6] Generating Visual Studio project..." -ForegroundColor Yellow

$buildDir = Join-Path $audacitySource "build"

if (-not (Test-Path $buildDir)) {
    mkdir $buildDir | Out-Null
}

Push-Location $buildDir

# Try VS 2022 first, then 2019
$generators = @(
    "Visual Studio 17 2022",
    "Visual Studio 16 2019"
)

$configured = $false
foreach ($gen in $generators) {
    Write-Host "Trying $gen..." -ForegroundColor Gray
    cmake -G $gen -A x64 .. 2>$null
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✓ Project generated with $gen" -ForegroundColor Green
        $configured = $true
        break
    }
}

if (-not $configured) {
    Write-Host "✗ Failed to configure CMake" -ForegroundColor Red
    Write-Host "  Please ensure Visual Studio is installed with C++ tools" -ForegroundColor Yellow
    Pop-Location
    exit 1
}

Write-Host "`n[6/6] Building module..." -ForegroundColor Yellow

cmake --build . --config Release --target mod-cloud-ai

if ($LASTEXITCODE -eq 0) {
    Write-Host "`n✓ BUILD SUCCESSFUL!" -ForegroundColor Green -BackgroundColor Black
    
    # Find the DLL
    $dll = Get-ChildItem -Path . -Filter "mod-cloud-ai.dll" -Recurse | Select-Object -First 1
    
    if ($dll) {
        Write-Host "`n📦 Module built: $($dll.FullName)" -ForegroundColor Cyan
        
        # Offer to copy to Audacity
        Write-Host "`nWould you like to install to Audacity? (Y/N)" -ForegroundColor Yellow
        $response = Read-Host
        
        if ($response -eq 'Y' -or $response -eq 'y') {
            $audacityModules = "C:\Program Files\Audacity\modules"
            
            if (Test-Path $audacityModules) {
                try {
                    Copy-Item $dll.FullName $audacityModules -Force
                    Write-Host "✓ Module installed to Audacity!" -ForegroundColor Green
                    Write-Host "`nLaunch Audacity and check:" -ForegroundColor Cyan
                    Write-Host "  Generate → AI Text-to-Speech" -ForegroundColor White
                } catch {
                    Write-Host "✗ Failed to copy (may need admin rights)" -ForegroundColor Red
                    Write-Host "  Manually copy from: $($dll.FullName)" -ForegroundColor Yellow
                    Write-Host "  To: $audacityModules" -ForegroundColor Yellow
                }
            } else {
                Write-Host "Audacity installation not found at default location" -ForegroundColor Yellow
                Write-Host "  Module is at: $($dll.FullName)" -ForegroundColor Cyan
            }
        }
    }
} else {
    Write-Host "`n✗ BUILD FAILED" -ForegroundColor Red
    Write-Host "Check error messages above for details" -ForegroundColor Yellow
}

Pop-Location

Write-Host "`n" -NoNewline
Write-Host "════════════════════════════════════════════════" -ForegroundColor Cyan
Write-Host "Setup complete! Check messages above for status." -ForegroundColor Cyan
Write-Host "════════════════════════════════════════════════" -ForegroundColor Cyan
