"""Transcription tab (simplified)."""

import tkinter as tk
from tkinter import ttk, filedialog, messagebox, scrolledtext
from pathlib import Path
import sys
import threading

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from analyzers import AudioTranscriber
from utils import Config


class TranscriptionTab:
    """Audio transcription tab."""
    
    def __init__(self, parent, status_callback):
        self.parent = parent
        self.status_callback = status_callback
        self.transcriber = None
        
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
        
        # Options
        options_frame = ttk.LabelFrame(self.frame, text="Options", padding=10)
        options_frame.pack(fill=tk.X, pady=(0, 10))
        
        self.speakers_var = tk.BooleanVar()
        ttk.Checkbutton(options_frame, text="Enable speaker diarization", variable=self.speakers_var).pack(anchor=tk.W)
        
        self.labels_var = tk.BooleanVar()
        ttk.Checkbutton(options_frame, text="Create Audacity labels", variable=self.labels_var).pack(anchor=tk.W)
        
        # Output
        output_frame = ttk.LabelFrame(self.frame, text="Output", padding=10)
        output_frame.pack(fill=tk.X, pady=(0, 10))
        
        self.output_path = tk.StringVar(value=str(Config.TEMP_DIR / "transcript.txt"))
        
        ttk.Label(output_frame, text="Save to:").grid(row=0, column=0, sticky=tk.W)
        ttk.Entry(output_frame, textvariable=self.output_path, width=50).grid(row=0, column=1, sticky=tk.EW)
        ttk.Button(output_frame, text="Browse...", command=self._browse_output).grid(row=0, column=2, padx=(10, 0))
        
        output_frame.columnconfigure(1, weight=1)
        
        # Result display
        result_frame = ttk.LabelFrame(self.frame, text="Transcript", padding=10)
        result_frame.pack(fill=tk.BOTH, expand=True, pady=(0, 10))
        
        self.result_text = scrolledtext.ScrolledText(result_frame, height=10, wrap=tk.WORD, state=tk.DISABLED)
        self.result_text.pack(fill=tk.BOTH, expand=True)
        
        # Transcribe button
        self.transcribe_button = ttk.Button(
            self.frame,
            text="Transcribe Audio",
            command=self._transcribe,
            style='Accent.TButton'
        )
        self.transcribe_button.pack(side=tk.RIGHT)
    
    def _browse_input(self):
        filename = filedialog.askopenfilename(
            filetypes=[("Audio files", "*.wav *.mp3 *.flac"), ("All files", "*.*")]
        )
        if filename:
            self.input_path.set(filename)
    
    def _browse_output(self):
        filename = filedialog.asksaveasfilename(
            defaultextension=".txt",
            filetypes=[("Text files", "*.txt"), ("JSON files", "*.json"), ("SRT files", "*.srt"), ("All files", "*.*")],
            initialdir=Config.TEMP_DIR
        )
        if filename:
            self.output_path.set(filename)
    
    def _transcribe(self):
        if not self.input_path.get():
            messagebox.showerror("Error", "Please select an input file")
            return
        
        self.transcribe_button.config(state=tk.DISABLED)
        self.status_callback("Transcribing audio...", show_progress=True)
        
        def transcribe_thread():
            try:
                if not self.transcriber:
                    self.transcriber = AudioTranscriber()
                
                result = self.transcriber.transcribe(
                    input_file=Path(self.input_path.get()),
                    enable_speaker_diarization=self.speakers_var.get(),
                    output_file=Path(self.output_path.get()),
                    create_audacity_labels=self.labels_var.get()
                )
                
                self.frame.after(0, lambda: self._complete(True, result.get('text', ''), None))
                
            except Exception as e:
                self.frame.after(0, lambda: self._complete(False, '', str(e)))
        
        threading.Thread(target=transcribe_thread, daemon=True).start()
    
    def _complete(self, success, transcript, error):
        self.transcribe_button.config(state=tk.NORMAL)
        self.status_callback("Ready", show_progress=False)
        
        if success:
            # Display transcript
            self.result_text.config(state=tk.NORMAL)
            self.result_text.delete('1.0', tk.END)
            self.result_text.insert('1.0', transcript)
            self.result_text.config(state=tk.DISABLED)
            
            messagebox.showinfo("Success", f"Transcription complete!\n\nSaved to: {self.output_path.get()}")
        else:
            messagebox.showerror("Error", f"Transcription failed:\n\n{error}")
