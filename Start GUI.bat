@echo off
REM Audacity Cloud AI - GUI Launcher
REM Double-click this file to start the GUI

echo.
echo ====================================
echo   Audacity Cloud AI - Starting GUI
echo ====================================
echo.

REM Check if Python is installed
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

REM Launch the GUI
echo Launching GUI...
echo.
python gui_launcher.py

REM If the script exits with error, show message
if errorlevel 1 (
    echo.
    echo ====================================
    echo   GUI closed with an error
    echo ====================================
    echo.
    echo You can also use the command-line version:
    echo   python audacity_cloudai.py --help
    echo.
)

pause
