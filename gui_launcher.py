#!/usr/bin/env python3
"""
Audacity Cloud AI - GUI Launcher
Starts the graphical user interface
"""

import sys
from pathlib import Path

# Fix for Python 3.13+ where audioop was removed
try:
    import audioop
except ModuleNotFoundError:
    # Provide a stub for pydub compatibility
    import sys
    from types import ModuleType
    audioop = ModuleType('audioop')
    sys.modules['audioop'] = audioop
    sys.modules['pyaudioop'] = audioop

# Add src to path
sys.path.insert(0, str(Path(__file__).parent / 'src'))

def main():
    """Launch the GUI application."""
    try:
        # Check if tkinter is available
        import tkinter as tk
        from tkinter import messagebox
        
    except ImportError:
        print("ERROR: tkinter is not installed.")
        print("Please install tkinter:")
        print("  - Ubuntu/Debian: sudo apt-get install python3-tk")
        print("  - Fedora: sudo dnf install python3-tkinter")
        print("  - macOS: brew install python-tk")
        print("  - Windows: Reinstall Python with tkinter enabled")
        sys.exit(1)
    
    try:
        # Run setup wizard if needed
        from ui.setup_wizard import check_and_run_setup
        
        setup_complete = check_and_run_setup()
        
        # Launch main application
        from ui.main_window import MainWindow
        
        app = MainWindow()
        app.run()
        
    except KeyboardInterrupt:
        print("\nApplication closed by user")
        sys.exit(0)
        
    except Exception as e:
        print(f"ERROR: Failed to launch GUI: {e}", file=sys.stderr)
        print("\nYou can still use the command-line interface:")
        print("  python audacity_cloudai.py --help")
        sys.exit(1)


if __name__ == '__main__':
    main()
