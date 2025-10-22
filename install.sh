#!/bin/bash
# Audacity Cloud AI - Installation Script for Linux/macOS

echo ""
echo "============================================"
echo "  Audacity Cloud AI - Installation"
echo "============================================"
echo ""

# Check Python
if ! command -v python3 &> /dev/null; then
    echo "ERROR: Python 3 is not installed"
    echo ""
    echo "Please install Python 3.8+ from your package manager:"
    echo "  Ubuntu/Debian: sudo apt-get install python3 python3-pip python3-tk"
    echo "  Fedora: sudo dnf install python3 python3-pip python3-tkinter"
    echo "  macOS: brew install python3 python-tk"
    echo ""
    exit 1
fi

echo "[1/4] Checking Python version..."
python3 --version

echo ""
echo "[2/4] Upgrading pip..."
python3 -m pip install --upgrade pip

echo ""
echo "[3/4] Installing dependencies..."
pip3 install -r requirements.txt

if [ $? -ne 0 ]; then
    echo ""
    echo "ERROR: Failed to install dependencies"
    echo "Please check your internet connection and try again"
    exit 1
fi

echo ""
echo "[4/4] Setting up configuration..."
if [ ! -f .env ]; then
    cp .env.example .env
    echo "Created .env file - Please add your ElevenLabs API key"
else
    echo ".env file already exists"
fi

# Make shell scripts executable
chmod +x "Start GUI.sh"
chmod +x install.sh

echo ""
echo "============================================"
echo "  Installation Complete!"
echo "============================================"
echo ""
echo "Next steps:"
echo "  1. Edit .env and add your ElevenLabs API key"
echo "  2. Run: ./Start GUI.sh"
echo "  3. Or use CLI: python3 audacity_cloudai.py --help"
echo ""
echo "Documentation: README.md"
echo "GUI Guide: GUI_README.md"
echo ""
