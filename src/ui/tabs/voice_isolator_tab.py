"""Voice Isolator tab (simplified)."""

import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from pathlib import Path
import sys
import threading

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from effects import VoiceIsolator
from utils import Config


class VoiceIsolatorTab:
    """Voice isolation tab."""
    
    def __init__(self, parent, status_callback):
        self.parent = parent
        self.status_callback = status_callback
        self.isolator = None
        
        self.frame = ttk.Frame(parent, padding=10)
        self._create_widgets()
    
    def _create_widgets(self):
        """Create widgets."""
        # Input file
        input_frame = ttk.LabelFrame(self.frame, text="Input Audio", padding=10)
        input_frame.pack(fill=tk.X, pady=(0, 10))
        
        self.input_path = tk.StringVar()
        
        ttk.Label(input_frame, text="Audio file:").grid(row=0, column=0, sticky=tk.W)
        ttk.Entry(input_frame, textvariable=self.input_path, width=50).grid(row=0, column=1, sticky=tk.EW)
        ttk.Button(input_frame, text="Browse...", command=self._browse_input).grid(row=0, column=2, padx=(10, 0))
        
        input_frame.columnconfigure(1, weight=1)
        
        # Info
        info_frame = ttk.Frame(self.frame)
        info_frame.pack(fill=tk.X, pady=10)
        
        ttk.Label(
            info_frame,
            text="Voice Isolator removes background noise and isolates voice from audio.",
            wraplength=600,
            justify=tk.LEFT
        ).pack(anchor=tk.W)
        
        # Output file
        output_frame = ttk.LabelFrame(self.frame, text="Output", padding=10)
        output_frame.pack(fill=tk.X, pady=(0, 10))
        
        self.output_path = tk.StringVar(value=str(Config.TEMP_DIR / "isolated_output.wav"))
        
        ttk.Label(output_frame, text="Save to:").grid(row=0, column=0, sticky=tk.W)
        ttk.Entry(output_frame, textvariable=self.output_path, width=50).grid(row=0, column=1, sticky=tk.EW)
        ttk.Button(output_frame, text="Browse...", command=self._browse_output).grid(row=0, column=2, padx=(10, 0))
        
        output_frame.columnconfigure(1, weight=1)
        
        # Process button
        self.process_button = ttk.Button(
            self.frame,
            text="Isolate Voice",
            command=self._process,
            style='Accent.TButton'
        )
        self.process_button.pack(side=tk.RIGHT)
    
    def _browse_input(self):
        filename = filedialog.askopenfilename(
            filetypes=[("Audio files", "*.wav *.mp3 *.flac"), ("All files", "*.*")]
        )
        if filename:
            self.input_path.set(filename)
    
    def _browse_output(self):
        filename = filedialog.asksaveasfilename(
            defaultextension=".wav",
            filetypes=[("WAV files", "*.wav"), ("All files", "*.*")],
            initialdir=Config.TEMP_DIR
        )
        if filename:
            self.output_path.set(filename)
    
    def _process(self):
        if not self.input_path.get():
            messagebox.showerror("Error", "Please select an input file")
            return
        
        self.process_button.config(state=tk.DISABLED)
        self.status_callback("Isolating voice...", show_progress=True)
        
        def process_thread():
            try:
                if not self.isolator:
                    self.isolator = VoiceIsolator()
                
                audio = self.isolator.isolate(
                    input_file=Path(self.input_path.get()),
                    output_file=Path(self.output_path.get())
                )
                
                self.frame.after(0, lambda: self._complete(True, None))
                
            except Exception as e:
                self.frame.after(0, lambda: self._complete(False, str(e)))
        
        threading.Thread(target=process_thread, daemon=True).start()
    
    def _complete(self, success, error):
        self.process_button.config(state=tk.NORMAL)
        self.status_callback("Ready", show_progress=False)
        
        if success:
            messagebox.showinfo("Success", f"Voice isolated!\n\nSaved to: {self.output_path.get()}")
        else:
            messagebox.showerror("Error", f"Failed:\n\n{error}")
