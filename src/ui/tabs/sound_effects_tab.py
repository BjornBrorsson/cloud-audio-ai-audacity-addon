"""Sound Effects tab (simplified)."""

import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from pathlib import Path
import sys
import threading

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from generators import SoundEffectsGenerator
from utils import Config


class SoundEffectsTab:
    """Sound effects generation tab."""
    
    def __init__(self, parent, status_callback):
        self.parent = parent
        self.status_callback = status_callback
        self.generator = None
        
        self.frame = ttk.Frame(parent, padding=10)
        self._create_widgets()
    
    def _create_widgets(self):
        """Create widgets."""
        # Description input
        desc_frame = ttk.LabelFrame(self.frame, text="Sound Effect Description", padding=10)
        desc_frame.pack(fill=tk.X, pady=(0, 10))
        
        self.description_var = tk.StringVar()
        ttk.Entry(desc_frame, textvariable=self.description_var, font=('Arial', 11)).pack(fill=tk.X)
        
        # Settings
        settings_frame = ttk.LabelFrame(self.frame, text="Settings", padding=10)
        settings_frame.pack(fill=tk.X, pady=(0, 10))
        
        ttk.Label(settings_frame, text="Duration (seconds, optional):").grid(row=0, column=0, sticky=tk.W)
        self.duration_var = tk.DoubleVar(value=0)
        ttk.Spinbox(settings_frame, from_=0, to=30, textvariable=self.duration_var, width=10).grid(row=0, column=1, sticky=tk.W, padx=10)
        ttk.Label(settings_frame, text="(0 = auto)").grid(row=0, column=2, sticky=tk.W)
        
        # Output
        output_frame = ttk.LabelFrame(self.frame, text="Output", padding=10)
        output_frame.pack(fill=tk.X, pady=(0, 10))
        
        self.output_path = tk.StringVar(value=str(Config.TEMP_DIR / "sfx_output.wav"))
        
        ttk.Label(output_frame, text="Save to:").grid(row=0, column=0, sticky=tk.W)
        ttk.Entry(output_frame, textvariable=self.output_path, width=50).grid(row=0, column=1, sticky=tk.EW)
        ttk.Button(output_frame, text="Browse...", command=self._browse).grid(row=0, column=2, padx=(10, 0))
        
        output_frame.columnconfigure(1, weight=1)
        
        # Generate button
        self.generate_button = ttk.Button(
            self.frame,
            text="Generate Sound Effect",
            command=self._generate,
            style='Accent.TButton'
        )
        self.generate_button.pack(side=tk.RIGHT)
    
    def _browse(self):
        filename = filedialog.asksaveasfilename(
            defaultextension=".wav",
            filetypes=[("WAV files", "*.wav"), ("All files", "*.*")],
            initialdir=Config.TEMP_DIR
        )
        if filename:
            self.output_path.set(filename)
    
    def _generate(self):
        description = self.description_var.get().strip()
        if not description:
            messagebox.showerror("Error", "Please enter a description")
            return
        
        self.generate_button.config(state=tk.DISABLED)
        self.status_callback("Generating sound effect...", show_progress=True)
        
        def generate_thread():
            try:
                if not self.generator:
                    self.generator = SoundEffectsGenerator()
                
                duration = self.duration_var.get() if self.duration_var.get() > 0 else None
                
                audio = self.generator.generate(
                    description=description,
                    duration_seconds=duration,
                    output_file=Path(self.output_path.get())
                )
                
                self.frame.after(0, lambda: self._complete(True, None))
                
            except Exception as e:
                self.frame.after(0, lambda: self._complete(False, str(e)))
        
        threading.Thread(target=generate_thread, daemon=True).start()
    
    def _complete(self, success, error):
        self.generate_button.config(state=tk.NORMAL)
        self.status_callback("Ready", show_progress=False)
        
        if success:
            messagebox.showinfo("Success", f"Sound effect generated!\n\nSaved to: {self.output_path.get()}")
        else:
            messagebox.showerror("Error", f"Failed:\n\n{error}")
