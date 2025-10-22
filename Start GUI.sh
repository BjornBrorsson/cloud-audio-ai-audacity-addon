#!/bin/bash
# Audacity Cloud AI - GUI Launcher
# Run this script to start the GUI

echo ""
echo "===================================="
echo "  Audacity Cloud AI - Starting GUI"
echo "===================================="
echo ""

# Check if Python is installed
if ! command -v python3 &> /dev/null; then
    echo "ERROR: Python 3 is not installed"
    echo ""
    echo "Please install Python 3.8+ from your package manager"
    echo "  - Ubuntu/Debian: sudo apt-get install python3"
    echo "  - macOS: brew install python3"
    echo ""
    exit 1
fi

# Launch the GUI
echo "Launching GUI..."
echo ""
python3 gui_launcher.py

# If the script exits with error, show message
if [ $? -ne 0 ]; then
    echo ""
    echo "===================================="
    echo "  GUI closed with an error"
    echo "===================================="
    echo ""
    echo "You can also use the command-line version:"
    echo "  python3 audacity_cloudai.py --help"
    echo ""
fi

read -p "Press Enter to close..."
