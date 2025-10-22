"""Dialog windows for the GUI."""

import tkinter as tk
from tkinter import ttk, messagebox, filedialog, scrolledtext
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).parent.parent))

from utils import Config, VoiceLibraryBrowser


class SettingsDialog:
    """Settings dialog window."""
    
    def __init__(self, parent):
        """Initialize settings dialog."""
        self.dialog = tk.Toplevel(parent)
        self.dialog.title("Settings")
        self.dialog.geometry("500x400")
        self.dialog.transient(parent)
        self.dialog.grab_set()
        
        self._create_widgets()
        self._center_window()
    
    def _center_window(self):
        """Center the dialog."""
        self.dialog.update_idletasks()
        width = self.dialog.winfo_width()
        height = self.dialog.winfo_height()
        x = (self.dialog.winfo_screenwidth() // 2) - (width // 2)
        y = (self.dialog.winfo_screenheight() // 2) - (height // 2)
        self.dialog.geometry(f'{width}x{height}+{x}+{y}')
    
    def _create_widgets(self):
        """Create dialog widgets."""
        # API Key section
        api_frame = ttk.LabelFrame(self.dialog, text="API Configuration", padding=15)
        api_frame.pack(fill=tk.X, padx=10, pady=10)
        
        ttk.Label(api_frame, text="ElevenLabs API Key:").pack(anchor=tk.W)
        
        self.api_key_var = tk.StringVar()
        
        # Load current API key
        try:
            env_file = Path(__file__).parent.parent.parent / '.env'
            if env_file.exists():
                with open(env_file, 'r') as f:
                    for line in f:
                        if line.startswith('ELEVENLABS_API_KEY='):
                            self.api_key_var.set(line.split('=', 1)[1].strip())
        except:
            pass
        
        key_entry = ttk.Entry(api_frame, textvariable=self.api_key_var, width=50, show='*')
        key_entry.pack(fill=tk.X, pady=5)
        
        show_var = tk.BooleanVar()
        ttk.Checkbutton(
            api_frame,
            text="Show API key",
            variable=show_var,
            command=lambda: key_entry.config(show='' if show_var.get() else '*')
        ).pack(anchor=tk.W)
        
        # Defaults section
        defaults_frame = ttk.LabelFrame(self.dialog, text="Defaults", padding=15)
        defaults_frame.pack(fill=tk.X, padx=10, pady=10)
        
        ttk.Label(defaults_frame, text="Default Voice:").pack(anchor=tk.W)
        self.voice_var = tk.StringVar(value=Config.DEFAULT_VOICE_ID)
        ttk.Entry(defaults_frame, textvariable=self.voice_var, width=50).pack(fill=tk.X, pady=5)
        
        ttk.Label(defaults_frame, text="Default Model:").pack(anchor=tk.W, pady=(10, 0))
        self.model_var = tk.StringVar(value=Config.DEFAULT_MODEL)
        model_combo = ttk.Combobox(defaults_frame, textvariable=self.model_var, state='readonly')
        model_combo['values'] = list(Config.get_available_models().keys())
        model_combo.pack(fill=tk.X, pady=5)
        
        ttk.Label(defaults_frame, text="Output Directory:").pack(anchor=tk.W, pady=(10, 0))
        output_frame = ttk.Frame(defaults_frame)
        output_frame.pack(fill=tk.X, pady=5)
        
        self.output_var = tk.StringVar(value=str(Config.TEMP_DIR))
        ttk.Entry(output_frame, textvariable=self.output_var).pack(side=tk.LEFT, fill=tk.X, expand=True)
        ttk.Button(output_frame, text="Browse...", command=self._browse_output).pack(side=tk.RIGHT, padx=(5, 0))
        
        # Buttons
        button_frame = ttk.Frame(self.dialog)
        button_frame.pack(side=tk.BOTTOM, fill=tk.X, padx=10, pady=10)
        
        ttk.Button(button_frame, text="Save", command=self._save).pack(side=tk.RIGHT, padx=5)
        ttk.Button(button_frame, text="Cancel", command=self.dialog.destroy).pack(side=tk.RIGHT)
    
    def _browse_output(self):
        """Browse for output directory."""
        directory = filedialog.askdirectory(initialdir=self.output_var.get())
        if directory:
            self.output_var.set(directory)
    
    def _save(self):
        """Save settings."""
        try:
            env_file = Path(__file__).parent.parent.parent / '.env'
            
            with open(env_file, 'w') as f:
                f.write(f"ELEVENLABS_API_KEY={self.api_key_var.get()}\n")
                f.write(f"DEFAULT_VOICE_ID={self.voice_var.get()}\n")
                f.write(f"DEFAULT_MODEL={self.model_var.get()}\n")
                # Note: Output directory would need to be stored elsewhere
            
            messagebox.showinfo("Success", "Settings saved successfully!")
            self.dialog.destroy()
            
        except Exception as e:
            messagebox.showerror("Error", f"Failed to save settings:\n{e}")


class VoiceLibraryDialog:
    """Voice Library browser dialog."""
    
    def __init__(self, parent):
        """Initialize voice library dialog."""
        self.dialog = tk.Toplevel(parent)
        self.dialog.title("Voice Library")
        self.dialog.geometry("800x600")
        self.dialog.transient(parent)
        
        self.browser = VoiceLibraryBrowser()
        self.voices = []
        
        self._create_widgets()
        self._center_window()
        self._load_featured()
    
    def _center_window(self):
        """Center the dialog."""
        self.dialog.update_idletasks()
        width = self.dialog.winfo_width()
        height = self.dialog.winfo_height()
        x = (self.dialog.winfo_screenwidth() // 2) - (width // 2)
        y = (self.dialog.winfo_screenheight() // 2) - (height // 2)
        self.dialog.geometry(f'{width}x{height}+{x}+{y}')
    
    def _create_widgets(self):
        """Create dialog widgets."""
        # Search frame
        search_frame = ttk.Frame(self.dialog, padding=10)
        search_frame.pack(fill=tk.X)
        
        ttk.Label(search_frame, text="Search:").pack(side=tk.LEFT)
        self.search_var = tk.StringVar()
        search_entry = ttk.Entry(search_frame, textvariable=self.search_var, width=40)
        search_entry.pack(side=tk.LEFT, padx=10, fill=tk.X, expand=True)
        ttk.Button(search_frame, text="Search", command=self._search).pack(side=tk.LEFT, padx=5)
        ttk.Button(search_frame, text="Featured", command=self._load_featured).pack(side=tk.LEFT)
        
        # Voices list
        list_frame = ttk.Frame(self.dialog, padding=10)
        list_frame.pack(fill=tk.BOTH, expand=True)
        
        # Create treeview
        columns = ('Name', 'Gender', 'Age', 'Accent', 'Creator')
        self.tree = ttk.Treeview(list_frame, columns=columns, show='headings', selectmode='browse')
        
        for col in columns:
            self.tree.heading(col, text=col)
            self.tree.column(col, width=150)
        
        # Scrollbar
        scrollbar = ttk.Scrollbar(list_frame, orient=tk.VERTICAL, command=self.tree.yview)
        self.tree.configure(yscroll=scrollbar.set)
        
        self.tree.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        
        # Details frame
        details_frame = ttk.LabelFrame(self.dialog, text="Voice Details", padding=10)
        details_frame.pack(fill=tk.X, padx=10, pady=(0, 10))
        
        self.details_text = tk.Text(details_frame, height=5, wrap=tk.WORD, state=tk.DISABLED)
        self.details_text.pack(fill=tk.X)
        
        # Bind selection
        self.tree.bind('<<TreeviewSelect>>', self._on_select)
        
        # Buttons
        button_frame = ttk.Frame(self.dialog, padding=10)
        button_frame.pack(fill=tk.X)
        
        self.add_button = ttk.Button(button_frame, text="Add to My Voices", command=self._add_voice, state=tk.DISABLED)
        self.add_button.pack(side=tk.RIGHT, padx=5)
        ttk.Button(button_frame, text="Close", command=self.dialog.destroy).pack(side=tk.RIGHT)
    
    def _search(self):
        """Search voices."""
        query = self.search_var.get()
        if query:
            try:
                result = self.browser.search_voices(query=query)
                self._display_voices(result.get('voices', []))
            except Exception as e:
                messagebox.showerror("Error", f"Search failed:\n{e}")
    
    def _load_featured(self):
        """Load featured voices."""
        try:
            result = self.browser.get_featured_voices()
            self._display_voices(result.get('voices', []))
        except Exception as e:
            messagebox.showerror("Error", f"Failed to load voices:\n{e}")
    
    def _display_voices(self, voices):
        """Display voices in the tree."""
        # Clear existing
        for item in self.tree.get_children():
            self.tree.delete(item)
        
        self.voices = voices
        
        # Add voices
        for voice in voices:
            labels = voice.get('labels', {})
            self.tree.insert('', tk.END, values=(
                voice.get('name', 'Unknown'),
                labels.get('gender', 'N/A'),
                labels.get('age', 'N/A'),
                labels.get('accent', 'N/A'),
                voice.get('creator_name', 'N/A')
            ))
    
    def _on_select(self, event):
        """Handle voice selection."""
        selection = self.tree.selection()
        if selection:
            index = self.tree.index(selection[0])
            voice = self.voices[index]
            
            # Update details
            self.details_text.config(state=tk.NORMAL)
            self.details_text.delete('1.0', tk.END)
            self.details_text.insert('1.0', f"Description: {voice.get('description', 'N/A')}\n")
            self.details_text.insert(tk.END, f"Voice ID: {voice.get('voice_id', 'N/A')}\n")
            if 'use_cases' in voice:
                self.details_text.insert(tk.END, f"Use Cases: {', '.join(voice['use_cases'])}\n")
            self.details_text.config(state=tk.DISABLED)
            
            self.add_button.config(state=tk.NORMAL)
    
    def _add_voice(self):
        """Add selected voice to account."""
        selection = self.tree.selection()
        if not selection:
            return
        
        index = self.tree.index(selection[0])
        voice = self.voices[index]
        
        # Ask for name
        name = tk.simpledialog.askstring("Voice Name", "Enter a name for this voice:")
        if name:
            try:
                self.browser.add_voice_to_account(
                    public_user_id=voice.get('public_owner_id'),
                    voice_id=voice.get('voice_id'),
                    new_name=name
                )
                messagebox.showinfo("Success", f"Voice '{name}' added to your account!")
            except Exception as e:
                messagebox.showerror("Error", f"Failed to add voice:\n{e}")


class ExamplesDialog:
    """Examples dialog."""
    
    def __init__(self, parent):
        """Initialize examples dialog."""
        self.dialog = tk.Toplevel(parent)
        self.dialog.title("Examples")
        self.dialog.geometry("700x500")
        self.dialog.transient(parent)
        
        self._create_widgets()
        self._center_window()
    
    def _center_window(self):
        """Center the dialog."""
        self.dialog.update_idletasks()
        width = self.dialog.winfo_width()
        height = self.dialog.winfo_height()
        x = (self.dialog.winfo_screenwidth() // 2) - (width // 2)
        y = (self.dialog.winfo_screenheight() // 2) - (height // 2)
        self.dialog.geometry(f'{width}x{height}+{x}+{y}')
    
    def _create_widgets(self):
        """Create widgets."""
        notebook = ttk.Notebook(self.dialog)
        notebook.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # Music examples
        music_frame = ttk.Frame(notebook, padding=10)
        notebook.add(music_frame, text="Music")
        
        music_text = scrolledtext.ScrolledText(music_frame, wrap=tk.WORD)
        music_text.pack(fill=tk.BOTH, expand=True)
        
        from generators import MusicGenerator
        gen = MusicGenerator()
        examples = gen.get_example_prompts()
        
        for category, prompt in examples.items():
            music_text.insert(tk.END, f"{category}:\n  {prompt}\n\n")
        
        music_text.config(state=tk.DISABLED)
        
        # Sound Effects examples
        sfx_frame = ttk.Frame(notebook, padding=10)
        notebook.add(sfx_frame, text="Sound Effects")
        
        sfx_text = scrolledtext.ScrolledText(sfx_frame, wrap=tk.WORD)
        sfx_text.pack(fill=tk.BOTH, expand=True)
        
        from generators import SoundEffectsGenerator
        sfx_gen = SoundEffectsGenerator()
        sfx_examples = sfx_gen.get_example_prompts()
        
        for category, prompt in sfx_examples.items():
            sfx_text.insert(tk.END, f"{category}:\n  {prompt}\n\n")
        
        sfx_text.config(state=tk.DISABLED)
        
        # Close button
        ttk.Button(self.dialog, text="Close", command=self.dialog.destroy).pack(pady=10)
