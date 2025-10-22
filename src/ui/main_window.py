"""Main GUI window for Audacity Cloud AI."""

import tkinter as tk
from tkinter import ttk, filedialog, messagebox, scrolledtext
from pathlib import Path
import sys
import threading

sys.path.insert(0, str(Path(__file__).parent.parent))

from generators import TextToSpeechGenerator, MusicGenerator, SoundEffectsGenerator
from effects import VoiceIsolator
from analyzers import AudioTranscriber
from utils import Config, VoiceLibraryBrowser


class MainWindow:
    """Main application window."""
    
    def __init__(self):
        """Initialize the main window."""
        self.root = tk.Tk()
        self.root.title("Audacity Cloud AI")
        self.root.geometry("900x700")
        
        # Style configuration
        self._configure_styles()
        
        # Create menu bar
        self._create_menu()
        
        # Create main content
        self._create_widgets()
        
        # Status bar
        self._create_status_bar()
        
        # Center window
        self._center_window()
    
    def _configure_styles(self):
        """Configure ttk styles."""
        style = ttk.Style()
        style.theme_use('clam')
        
        # Define custom colors
        style.configure('Accent.TButton', foreground='white', background='#0066cc')
        style.configure('Success.TLabel', foreground='#28a745')
        style.configure('Error.TLabel', foreground='#dc3545')
    
    def _center_window(self):
        """Center the window on screen."""
        self.root.update_idletasks()
        width = self.root.winfo_width()
        height = self.root.winfo_height()
        x = (self.root.winfo_screenwidth() // 2) - (width // 2)
        y = (self.root.winfo_screenheight() // 2) - (height // 2)
        self.root.geometry(f'{width}x{height}+{x}+{y}')
    
    def _create_menu(self):
        """Create menu bar."""
        menubar = tk.Menu(self.root)
        self.root.config(menu=menubar)
        
        # File menu
        file_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="File", menu=file_menu)
        file_menu.add_command(label="Settings", command=self._show_settings)
        file_menu.add_separator()
        file_menu.add_command(label="Exit", command=self.root.quit)
        
        # Tools menu
        tools_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="Tools", menu=tools_menu)
        tools_menu.add_command(label="Voice Library", command=self._show_voice_library)
        tools_menu.add_command(label="List My Voices", command=self._list_voices)
        tools_menu.add_command(label="Check Configuration", command=self._check_config)
        
        # Help menu
        help_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="Help", menu=help_menu)
        help_menu.add_command(label="Documentation", command=self._show_docs)
        help_menu.add_command(label="Examples", command=self._show_examples)
        help_menu.add_separator()
        help_menu.add_command(label="About", command=self._show_about)
    
    def _create_widgets(self):
        """Create main widgets."""
        # Create notebook (tabs)
        self.notebook = ttk.Notebook(self.root)
        self.notebook.pack(fill=tk.BOTH, expand=True, padx=5, pady=5)
        
        # Import tab modules
        from ui.tabs import (
            TextToSpeechTab,
            MusicGenerationTab,
            SoundEffectsTab,
            VoiceIsolatorTab,
            TranscriptionTab
        )
        
        # Create tabs
        self.tts_tab = TextToSpeechTab(self.notebook, self._update_status)
        self.music_tab = MusicGenerationTab(self.notebook, self._update_status)
        self.sfx_tab = SoundEffectsTab(self.notebook, self._update_status)
        self.isolator_tab = VoiceIsolatorTab(self.notebook, self._update_status)
        self.transcription_tab = TranscriptionTab(self.notebook, self._update_status)
        
        # Add tabs to notebook
        self.notebook.add(self.tts_tab.frame, text="🎤 Text-to-Speech")
        self.notebook.add(self.music_tab.frame, text="🎶 Music")
        self.notebook.add(self.sfx_tab.frame, text="🔊 Sound Effects")
        self.notebook.add(self.isolator_tab.frame, text="🎧 Voice Isolator")
        self.notebook.add(self.transcription_tab.frame, text="📝 Transcription")
    
    def _create_status_bar(self):
        """Create status bar."""
        status_frame = ttk.Frame(self.root)
        status_frame.pack(side=tk.BOTTOM, fill=tk.X)
        
        self.status_label = ttk.Label(status_frame, text="Ready", relief=tk.SUNKEN, anchor=tk.W)
        self.status_label.pack(side=tk.LEFT, fill=tk.X, expand=True)
        
        # Progress bar (hidden by default)
        self.progress_bar = ttk.Progressbar(status_frame, mode='indeterminate', length=200)
    
    def _update_status(self, message, show_progress=False):
        """Update status bar."""
        self.status_label.config(text=message)
        
        if show_progress:
            self.progress_bar.pack(side=tk.RIGHT, padx=5)
            self.progress_bar.start()
        else:
            self.progress_bar.stop()
            self.progress_bar.pack_forget()
    
    def _show_settings(self):
        """Show settings dialog."""
        from ui.dialogs import SettingsDialog
        SettingsDialog(self.root)
    
    def _show_voice_library(self):
        """Show voice library browser."""
        from ui.dialogs import VoiceLibraryDialog
        VoiceLibraryDialog(self.root)
    
    def _list_voices(self):
        """List available voices."""
        try:
            gen = TextToSpeechGenerator()
            voices = gen.get_available_voices()
            
            # Create dialog to display voices
            dialog = tk.Toplevel(self.root)
            dialog.title("My Voices")
            dialog.geometry("600x400")
            
            # Create text widget
            text = scrolledtext.ScrolledText(dialog, wrap=tk.WORD)
            text.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
            
            # Display voices
            for voice in voices:
                text.insert(tk.END, f"Voice: {voice.get('name', 'Unknown')}\n")
                text.insert(tk.END, f"ID: {voice.get('voice_id', 'N/A')}\n")
                if 'labels' in voice:
                    labels = ', '.join([f"{k}: {v}" for k, v in voice['labels'].items()])
                    text.insert(tk.END, f"Labels: {labels}\n")
                text.insert(tk.END, "\n" + "-"*50 + "\n\n")
            
            text.config(state=tk.DISABLED)
            
        except Exception as e:
            messagebox.showerror("Error", f"Failed to fetch voices:\n{e}")
    
    def _check_config(self):
        """Check configuration."""
        try:
            Config.validate()
            
            from utils import ElevenLabsAPI
            api = ElevenLabsAPI()
            user_info = api.get_user_info()
            
            subscription = user_info.get('subscription', {})
            tier = subscription.get('tier', 'N/A')
            char_count = subscription.get('character_count', 0)
            char_limit = subscription.get('character_limit', 0)
            
            messagebox.showinfo(
                "Configuration Check",
                f"✓ API Key: Valid\n"
                f"✓ Subscription: {tier}\n"
                f"✓ Characters used: {char_count:,} / {char_limit:,}\n\n"
                "Everything looks good!"
            )
            
        except Exception as e:
            messagebox.showerror(
                "Configuration Error",
                f"Configuration check failed:\n\n{e}\n\n"
                "Please check your API key in Settings."
            )
    
    def _show_docs(self):
        """Show documentation."""
        import webbrowser
        webbrowser.open("https://github.com/yourusername/audacity-cloudai")
    
    def _show_examples(self):
        """Show examples dialog."""
        from ui.dialogs import ExamplesDialog
        ExamplesDialog(self.root)
    
    def _show_about(self):
        """Show about dialog."""
        messagebox.showinfo(
            "About",
            "Audacity Cloud AI Plugin\n"
            "Version 1.0.0\n\n"
            "AI-powered audio generation for Audacity\n"
            "using ElevenLabs Cloud APIs\n\n"
            "© 2025 - MIT License\n"
            "Open Source Project"
        )
    
    def run(self):
        """Run the application."""
        self.root.mainloop()


if __name__ == '__main__':
    app = MainWindow()
    app.run()
