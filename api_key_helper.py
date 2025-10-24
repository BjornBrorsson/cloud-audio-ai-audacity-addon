#!/usr/bin/env python3
"""
API Key Helper - Shows GUI dialog for API key setup
Called by Nyquist plugins before executing main commands
"""

import os
import sys
import tkinter as tk
from tkinter import messagebox, simpledialog
from pathlib import Path

def get_env_path():
    """Get path to .env file in project root"""
    # Script directory
    script_dir = Path(__file__).parent.absolute()
    return script_dir / ".env"

def check_api_key():
    """Check if API key exists and is valid"""
    env_path = get_env_path()
    
    # Check .env file
    if env_path.exists():
        with open(env_path, 'r') as f:
            content = f.read()
            if 'ELEVENLABS_API_KEY=' in content:
                key = content.split('ELEVENLABS_API_KEY=')[1].split('\n')[0].strip()
                if key and len(key) > 10:
                    return True, key
    
    # Check environment variable
    env_key = os.getenv('ELEVENLABS_API_KEY')
    if env_key and len(env_key) > 10:
        return True, env_key
    
    return False, None

def show_api_key_dialog(error_message=None):
    """Show dialog asking for API key"""
    root = tk.Tk()
    root.withdraw()  # Hide main window
    
    # Determine dialog title and message
    if error_message:
        title = "API Key Error"
        message = f"{error_message}\n\nPlease enter a valid ElevenLabs API key:"
    else:
        title = "API Key Required"
        message = "You need an ElevenLabs API key to use this plugin.\n\nGet a free key at: https://elevenlabs.io\n\nPlease enter your API key:"
    
    # Show custom dialog
    dialog = APIKeyDialog(root, title, message)
    root.mainloop()
    
    return dialog.result

class APIKeyDialog:
    def __init__(self, parent, title, message):
        self.result = None
        
        self.top = tk.Toplevel(parent)
        self.top.title(title)
        self.top.geometry("500x300")
        self.top.resizable(False, False)
        
        # Center window
        self.top.update_idletasks()
        width = self.top.winfo_width()
        height = self.top.winfo_height()
        x = (self.top.winfo_screenwidth() // 2) - (width // 2)
        y = (self.top.winfo_screenheight() // 2) - (height // 2)
        self.top.geometry(f'{width}x{height}+{x}+{y}')
        
        # Make modal
        self.top.transient(parent)
        self.top.grab_set()
        
        # Message label
        tk.Label(
            self.top, 
            text=message, 
            wraplength=450, 
            justify=tk.LEFT,
            font=("Arial", 10)
        ).pack(pady=20, padx=20)
        
        # Link button
        link_frame = tk.Frame(self.top)
        link_frame.pack(pady=5)
        
        tk.Button(
            link_frame,
            text="🌐 Get API Key at elevenlabs.io",
            command=self.open_website,
            fg="blue",
            cursor="hand2",
            relief=tk.FLAT,
            font=("Arial", 9, "underline")
        ).pack()
        
        # API key entry
        tk.Label(self.top, text="API Key:", font=("Arial", 10, "bold")).pack(pady=(10, 5))
        
        self.entry = tk.Entry(self.top, width=50, show="*")
        self.entry.pack(pady=5, padx=20)
        self.entry.focus()
        
        # Show/hide button
        self.show_key = tk.BooleanVar(value=False)
        tk.Checkbutton(
            self.top,
            text="Show key",
            variable=self.show_key,
            command=self.toggle_show_key
        ).pack()
        
        # Buttons
        button_frame = tk.Frame(self.top)
        button_frame.pack(pady=20)
        
        tk.Button(
            button_frame,
            text="Save",
            command=self.save,
            width=10,
            bg="#4CAF50",
            fg="white",
            font=("Arial", 10, "bold")
        ).pack(side=tk.LEFT, padx=5)
        
        tk.Button(
            button_frame,
            text="Cancel",
            command=self.cancel,
            width=10
        ).pack(side=tk.LEFT, padx=5)
        
        # Bind Enter key
        self.entry.bind('<Return>', lambda e: self.save())
        
    def toggle_show_key(self):
        if self.show_key.get():
            self.entry.config(show="")
        else:
            self.entry.config(show="*")
    
    def open_website(self):
        import webbrowser
        webbrowser.open("https://elevenlabs.io")
    
    def save(self):
        key = self.entry.get().strip()
        if not key:
            messagebox.showwarning("Invalid Key", "Please enter an API key.")
            return
        
        if len(key) < 10:
            messagebox.showwarning("Invalid Key", "API key seems too short. Please check and try again.")
            return
        
        # Save to .env file
        try:
            env_path = get_env_path()
            with open(env_path, 'w') as f:
                f.write(f"ELEVENLABS_API_KEY={key}\n")
            
            messagebox.showinfo("Success", "API key saved successfully!\n\nYou can now use the plugin.")
            self.result = key
            self.top.destroy()
        except Exception as e:
            messagebox.showerror("Error", f"Could not save API key:\n{e}")
    
    def cancel(self):
        self.result = None
        self.top.destroy()

def main():
    """Main entry point - check API key and prompt if needed"""
    # Check if API key exists
    has_key, key = check_api_key()
    
    if has_key:
        # Key exists, exit success
        sys.exit(0)
    
    # Check if we should show error message (passed as argument)
    error_message = None
    if len(sys.argv) > 1:
        error_message = sys.argv[1]
    
    # Show dialog to get API key
    result = show_api_key_dialog(error_message)
    
    if result:
        # Key was entered and saved
        sys.exit(0)
    else:
        # User cancelled
        sys.exit(1)

if __name__ == "__main__":
    main()
