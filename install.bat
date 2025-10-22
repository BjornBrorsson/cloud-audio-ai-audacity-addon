@echo off
REM Audacity Cloud AI - Installation Script for Windows
REM This script installs all dependencies and sets up the plugin

echo.
echo ============================================
echo   Audacity Cloud AI - Installation
echo ============================================
echo.

REM Check Python
python --version >nul 2>&1
if errorlevel 1 (
    echo ERROR: Python is not installed or not in PATH
    echo.
    echo Please install Python 3.8+ from https://python.org
    echo Make sure to check "Add Python to PATH" during installation
    echo.
    pause
    exit /b 1
)

echo [1/4] Checking Python version...
python --version

echo.
echo [2/4] Upgrading pip...
python -m pip install --upgrade pip

echo.
echo [3/4] Installing dependencies...
pip install -r requirements.txt

if errorlevel 1 (
    echo.
    echo ERROR: Failed to install dependencies
    echo Please check your internet connection and try again
    pause
    exit /b 1
)

echo.
echo [4/4] Setting up configuration...
if not exist .env (
    copy .env.example .env
    echo Created .env file - Please add your ElevenLabs API key
) else (
    echo .env file already exists
)

echo.
echo ============================================
echo   Installation Complete!
echo ============================================
echo.
echo Next steps:
echo   1. Edit .env and add your ElevenLabs API key
echo   2. Double-click "Start GUI.bat" to launch
echo   3. Or use CLI: python audacity_cloudai.py --help
echo.
echo Documentation: README.md
echo GUI Guide: GUI_README.md
echo.
pause
