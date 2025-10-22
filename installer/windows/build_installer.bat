@echo off
REM Build Windows installer using Inno Setup

echo Building Audacity Cloud AI Windows Installer...

REM Check if Inno Setup is installed
where /q iscc
if errorlevel 1 (
    echo ERROR: Inno Setup compiler not found.
    echo.
    echo Please install Inno Setup from: https://jrsoftware.org/isdl.php
    echo Or use the portable version and add it to PATH
    pause
    exit /b 1
)

REM Build the installer
echo Compiling installer...
iscc installer.iss

if errorlevel 1 (
    echo ERROR: Installer compilation failed
    pause
    exit /b 1
)

echo.
echo SUCCESS! Installer created in dist/ folder
echo.
dir ..\..\dist\AudacityCloudAI-Setup-*.exe
pause
