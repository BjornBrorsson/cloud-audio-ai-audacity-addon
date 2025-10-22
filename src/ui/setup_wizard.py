"""Setup wizard for first-time configuration."""

import tkinter as tk
from tkinter import ttk, messagebox
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).parent.parent))

from utils import Config


class SetupWizard:
    """First-time setup wizard for API key configuration."""
    
    def __init__(self, parent=None):
        """Initialize the setup wizard."""
        self.root = parent or tk.Tk()
        self.root.title("Audacity Cloud AI - Setup Wizard")
        self.root.geometry("600x400")
        self.root.resizable(False, False)
        
        self.api_key = tk.StringVar()
        self.setup_complete = False
        
        self._create_widgets()
        self._center_window()
    
    def _center_window(self):
        """Center the window on screen."""
        self.root.update_idletasks()
        width = self.root.winfo_width()
        height = self.root.winfo_height()
        x = (self.root.winfo_screenwidth() // 2) - (width // 2)
        y = (self.root.winfo_screenheight() // 2) - (height // 2)
        self.root.geometry(f'{width}x{height}+{x}+{y}')
    
    def _create_widgets(self):
        """Create wizard widgets."""
        # Header
        header_frame = ttk.Frame(self.root, padding=20)
        header_frame.pack(fill=tk.X)
        
        title_label = ttk.Label(
            header_frame,
            text="Welcome to Audacity Cloud AI",
            font=('Arial', 16, 'bold')
        )
        title_label.pack()
        
        subtitle_label = ttk.Label(
            header_frame,
            text="Let's set up your ElevenLabs API key",
            font=('Arial', 10)
        )
        subtitle_label.pack(pady=5)
        
        # Content
        content_frame = ttk.Frame(self.root, padding=20)
        content_frame.pack(fill=tk.BOTH, expand=True)
        
        # Instructions
        instructions = ttk.Label(
            content_frame,
            text=(
                "To use this plugin, you need an ElevenLabs API key.\n\n"
                "1. Go to https://elevenlabs.io\n"
                "2. Sign up or log in\n"
                "3. Navigate to Profile → API Keys\n"
                "4. Copy your API key and paste it below"
            ),
            justify=tk.LEFT
        )
        instructions.pack(pady=10)
        
        # API Key input
        key_frame = ttk.Frame(content_frame)
        key_frame.pack(fill=tk.X, pady=20)
        
        ttk.Label(key_frame, text="API Key:", font=('Arial', 10, 'bold')).pack(anchor=tk.W)
        
        key_entry = ttk.Entry(key_frame, textvariable=self.api_key, width=50, show='*')
        key_entry.pack(fill=tk.X, pady=5)
        
        show_key_var = tk.BooleanVar()
        
        def toggle_key_visibility():
            key_entry.config(show='' if show_key_var.get() else '*')
        
        show_check = ttk.Checkbutton(
            key_frame,
            text="Show API key",
            variable=show_key_var,
            command=toggle_key_visibility
        )
        show_check.pack(anchor=tk.W)
        
        # Buttons
        button_frame = ttk.Frame(self.root, padding=20)
        button_frame.pack(side=tk.BOTTOM, fill=tk.X)
        
        ttk.Button(
            button_frame,
            text="Test & Save",
            command=self._test_and_save,
            style='Accent.TButton'
        ).pack(side=tk.RIGHT, padx=5)
        
        ttk.Button(
            button_frame,
            text="Skip (I'll configure later)",
            command=self._skip_setup
        ).pack(side=tk.RIGHT, padx=5)
    
    def _test_and_save(self):
        """Test the API key and save if valid."""
        api_key = self.api_key.get().strip()
        
        if not api_key:
            messagebox.showerror("Error", "Please enter an API key")
            return
        
        # Show progress
        progress_window = tk.Toplevel(self.root)
        progress_window.title("Testing...")
        progress_window.geometry("300x100")
        progress_window.transient(self.root)
        progress_window.grab_set()
        
        ttk.Label(progress_window, text="Testing API key...").pack(pady=20)
        progress_bar = ttk.Progressbar(progress_window, mode='indeterminate')
        progress_bar.pack(pady=10, padx=20, fill=tk.X)
        progress_bar.start()
        
        # Test the API key
        def test_key():
            try:
                from utils import ElevenLabsAPI
                api = ElevenLabsAPI(api_key)
                # Try to get user info to validate key
                api.get_user_info()
                return True
            except Exception as e:
                return str(e)
        
        # Run test in background
        import threading
        
        def test_thread():
            result = test_key()
            progress_window.after(0, lambda: self._handle_test_result(result, progress_window, api_key))
        
        thread = threading.Thread(target=test_thread, daemon=True)
        thread.start()
    
    def _handle_test_result(self, result, progress_window, api_key):
        """Handle the test result."""
        progress_window.destroy()
        
        if result is True:
            # Save the API key
            env_file = Path(__file__).parent.parent.parent / '.env'
            
            try:
                with open(env_file, 'w') as f:
                    f.write(f"ELEVENLABS_API_KEY={api_key}\n")
                
                messagebox.showinfo(
                    "Success",
                    "API key validated and saved successfully!\n\n"
                    "You can now use all features of the plugin."
                )
                self.setup_complete = True
                self.root.destroy()
                
            except Exception as e:
                messagebox.showerror(
                    "Error",
                    f"Failed to save API key:\n{e}\n\n"
                    "Please create a .env file manually."
                )
        else:
            messagebox.showerror(
                "Invalid API Key",
                f"The API key is invalid or there was an error:\n\n{result}\n\n"
                "Please check your key and try again."
            )
    
    def _skip_setup(self):
        """Skip setup wizard."""
        result = messagebox.askyesno(
            "Skip Setup",
            "You can configure your API key later in Settings.\n\n"
            "Continue without setup?"
        )
        if result:
            self.root.destroy()
    
    def run(self):
        """Run the wizard."""
        self.root.mainloop()
        return self.setup_complete


def check_and_run_setup():
    """Check if setup is needed and run wizard if necessary."""
    env_file = Path(__file__).parent.parent.parent / '.env'
    
    # Check if .env exists and has API key
    if env_file.exists():
        with open(env_file, 'r') as f:
            content = f.read()
            if 'ELEVENLABS_API_KEY=' in content and len(content.split('ELEVENLABS_API_KEY=')[1].split('\n')[0].strip()) > 10:
                return True  # Already configured
    
    # Run setup wizard
    wizard = SetupWizard()
    return wizard.run()


if __name__ == '__main__':
    wizard = SetupWizard()
    wizard.run()
