# Building on Windows - Complete Guide

## Prerequisites

### 1. Install Visual Studio 2019 or 2022
Download from: https://visualstudio.microsoft.com/downloads/

**Required Components:**
- Desktop development with C++
- Windows 10 SDK
- CMake tools for Windows

### 2. Install CMake
Download from: https://cmake.org/download/

**During installation:**
- ✓ Check "Add CMake to system PATH"

### 3. Install Git
Download from: https://git-scm.com/download/win

## Build Steps

### Step 1: Get Audacity Source
```powershell
# Open PowerShell in a workspace directory
cd "C:\Coding Projects"

# Clone Audacity
git clone https://github.com/audacity/audacity.git
cd audacity

# Checkout stable version
git checkout Audacity-3.4.2
```

### Step 2: Copy Module to Audacity
```powershell
# Copy your module
Copy-Item -Recurse "C:\Coding Projects\Audacity-CloudAI\audacity-module" ".\modules\mod-cloud-ai"

# Verify it copied
dir modules\mod-cloud-ai
```

### Step 3: Update Audacity's CMakeLists.txt
```powershell
# Add module to Audacity's build
Add-Content ".\modules\CMakeLists.txt" "`nadd_subdirectory(mod-cloud-ai)"
```

### Step 4: Generate Visual Studio Project
```powershell
# Create build directory
mkdir build
cd build

# Generate VS project (choose your VS version)
# For VS 2022:
cmake -G "Visual Studio 17 2022" -A x64 ..

# For VS 2019:
cmake -G "Visual Studio 16 2019" -A x64 ..
```

### Step 5: Build the Module
```powershell
# Option A: Build from command line
cmake --build . --config Release --target mod-cloud-ai

# Option B: Open in Visual Studio
start Audacity.sln
# Then in VS: Build → Build Solution
```

### Step 6: Find Your Module
```powershell
# Module will be at:
dir .\Release\modules\mod-cloud-ai.dll

# Or:
dir .\modules\mod-cloud-ai\Release\mod-cloud-ai.dll
```

### Step 7: Install Module
```powershell
# Copy to Audacity installation
Copy-Item ".\Release\modules\mod-cloud-ai.dll" "C:\Program Files\Audacity\modules\"

# Or if Audacity is installed elsewhere, adjust path
```

### Step 8: Test
```powershell
# Launch Audacity
& "C:\Program Files\Audacity\Audacity.exe"

# Check menu:
# Generate → AI Text-to-Speech (should appear!)
```

## Quick Build Script

Save this as `build-module.ps1`:

```powershell
# Build Cloud AI Module for Audacity
# Run from Audacity source root

Write-Host "Building mod-cloud-ai..." -ForegroundColor Cyan

# Create build dir if needed
if (-not (Test-Path "build")) {
    mkdir build
}

cd build

# Configure if needed
if (-not (Test-Path "Audacity.sln")) {
    Write-Host "Configuring CMake..." -ForegroundColor Yellow
    cmake -G "Visual Studio 17 2022" -A x64 ..
}

# Build just the module
Write-Host "Building module..." -ForegroundColor Yellow
cmake --build . --config Release --target mod-cloud-ai

if ($LASTEXITCODE -eq 0) {
    Write-Host "✓ Build successful!" -ForegroundColor Green
    Write-Host "Module location: .\Release\modules\mod-cloud-ai.dll" -ForegroundColor Green
} else {
    Write-Host "✗ Build failed!" -ForegroundColor Red
}
```

## Troubleshooting

### "CMake not found"
- Reinstall CMake and check "Add to PATH"
- Restart PowerShell after install

### "Visual Studio generator not found"
- Install Visual Studio with C++ tools
- Use correct generator name for your version

### "wxWidgets not found"
- Audacity includes wxWidgets, no separate install needed
- Make sure you're building from Audacity source tree

### Module doesn't appear in Audacity
- Check module is in: `C:\Program Files\Audacity\modules\`
- Check Audacity log: Help → Show Log
- Look for "Cloud AI Module: Initializing..."

### Build errors in module code
- Check all .cpp/.h files are in `audacity-module/`
- Verify CMakeLists.txt includes all sources
- Check for missing semicolons or syntax errors

## Alternative: Standalone Build

If you just want to test the module without building all of Audacity:

1. Install Audacity from official installer
2. Get Audacity headers (from source)
3. Build just the module against installed Audacity

This is more complex and not recommended for first build.

## Next Steps

After successful build:
1. Test each effect (TTS, Music, SFX, Isolation, Transcription)
2. Try voice browser
3. Configure settings
4. Test audio preview
5. Report any issues

## Getting Help

If you encounter issues:
1. Check Audacity build docs: https://github.com/audacity/audacity/blob/master/BUILDING.md
2. Check module logs in Audacity
3. Verify Python backend is installed
4. Check API key is configured
