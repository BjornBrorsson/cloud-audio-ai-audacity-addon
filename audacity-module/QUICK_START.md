# Quick Start - Windows Build

## Easiest Method: Run the Simple Script

```powershell
cd "C:\Coding Projects\Audacity-CloudAI\audacity-module"
.\setup-simple.ps1
```

This will automatically build everything for you!

---

## Manual Build Steps

### 1. Install Prerequisites
- Visual Studio 2022 (with C++ tools)
- CMake
- Git

### 2. Clone Audacity
```powershell
cd "C:\Coding Projects"
git clone https://github.com/audacity/audacity.git
cd audacity
```

### 3. Copy Module
```powershell
Copy-Item -Recurse "..\Audacity-CloudAI\audacity-module" ".\modules\mod-cloud-ai"
```

### 4. Configure
```powershell
Add-Content ".\modules\CMakeLists.txt" "`nadd_subdirectory(mod-cloud-ai)"
```

### 5. Build
```powershell
mkdir build
cd build
cmake -G "Visual Studio 17 2022" -A x64 ..
cmake --build . --config Release --target mod-cloud-ai
```

### 6. Install
```powershell
Copy-Item "path\to\mod-cloud-ai.dll" "C:\Program Files\Audacity\modules\"
```

---

## Test Without Building

```powershell
cd "C:\Coding Projects\Audacity-CloudAI"
python audacity_cloudai.py tts --text "Hello" --output test.wav
```

Works immediately!
