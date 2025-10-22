"""Music Generation tab (simplified implementation)."""

import tkinter as tk
from tkinter import ttk, filedialog, messagebox, scrolledtext
from pathlib import Path
import sys
import threading

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from generators import MusicGenerator
from utils import Config


class MusicGenerationTab:
    """Music generation tab."""
    
    def __init__(self, parent, status_callback):
        """Initialize the music tab."""
        self.parent = parent
        self.status_callback = status_callback
        self.generator = None
        
        self.frame = ttk.Frame(parent, padding=10)
        self._create_widgets()
    
    def _create_widgets(self):
        """Create tab widgets."""
        # Prompt input
        prompt_frame = ttk.LabelFrame(self.frame, text="Music Description", padding=10)
        prompt_frame.pack(fill=tk.BOTH, expand=True, pady=(0, 10))
        
        self.prompt_widget = scrolledtext.ScrolledText(prompt_frame, height=6, wrap=tk.WORD)
        self.prompt_widget.pack(fill=tk.BOTH, expand=True)
        self.prompt_widget.insert('1.0', 'Describe the music you want to generate...')
        
        # Settings
        settings_frame = ttk.LabelFrame(self.frame, text="Settings", padding=10)
        settings_frame.pack(fill=tk.X, pady=(0, 10))
        
        # Duration
        ttk.Label(settings_frame, text="Duration (seconds):").grid(row=0, column=0, sticky=tk.W, padx=(0, 10))
        self.duration_var = tk.IntVar(value=30)
        ttk.Spinbox(settings_frame, from_=10, to=120, textvariable=self.duration_var, width=10).grid(row=0, column=1, sticky=tk.W)
        
        # Use composition plan
        self.use_plan_var = tk.BooleanVar()
        ttk.Checkbutton(settings_frame, text="Use composition plan (more structured)", variable=self.use_plan_var).grid(row=1, column=0, columnspan=2, sticky=tk.W, pady=5)
        
        # Output
        output_frame = ttk.LabelFrame(self.frame, text="Output", padding=10)
        output_frame.pack(fill=tk.X, pady=(0, 10))
        
        self.output_path = tk.StringVar(value=str(Config.TEMP_DIR / "music_output.wav"))
        
        ttk.Label(output_frame, text="Save to:").grid(row=0, column=0, sticky=tk.W, padx=(0, 10))
        ttk.Entry(output_frame, textvariable=self.output_path, width=50).grid(row=0, column=1, sticky=tk.EW)
        ttk.Button(output_frame, text="Browse...", command=self._browse_output).grid(row=0, column=2, padx=(10, 0))
        
        output_frame.columnconfigure(1, weight=1)
        
        # Generate button
        self.generate_button = ttk.Button(
            self.frame,
            text="Generate Music",
            command=self._generate,
            style='Accent.TButton'
        )
        self.generate_button.pack(side=tk.RIGHT)
    
    def _browse_output(self):
        """Browse for output file."""
        filename = filedialog.asksaveasfilename(
            defaultextension=".wav",
            filetypes=[("WAV files", "*.wav"), ("All files", "*.*")],
            initialdir=Config.TEMP_DIR
        )
        if filename:
            self.output_path.set(filename)
    
    def _generate(self):
        """Generate music."""
        prompt = self.prompt_widget.get('1.0', tk.END).strip()
        
        if not prompt or prompt == 'Describe the music you want to generate...':
            messagebox.showerror("Error", "Please enter a music description")
            return
        
        self.generate_button.config(state=tk.DISABLED)
        self.status_callback("Generating music...", show_progress=True)
        
        def generate_thread():
            try:
                if not self.generator:
                    self.generator = MusicGenerator()
                
                audio = self.generator.generate(
                    prompt=prompt,
                    duration=self.duration_var.get(),
                    output_file=Path(self.output_path.get()),
                    use_composition_plan=self.use_plan_var.get()
                )
                
                self.frame.after(0, lambda: self._generation_complete(True, None))
                
            except Exception as e:
                self.frame.after(0, lambda: self._generation_complete(False, str(e)))
        
        threading.Thread(target=generate_thread, daemon=True).start()
    
    def _generation_complete(self, success, error):
        """Handle generation completion."""
        self.generate_button.config(state=tk.NORMAL)
        self.status_callback("Ready", show_progress=False)
        
        if success:
            messagebox.showinfo("Success", f"Music generated!\n\nSaved to: {self.output_path.get()}")
        else:
            messagebox.showerror("Error", f"Generation failed:\n\n{error}")
