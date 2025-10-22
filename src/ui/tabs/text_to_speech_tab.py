"""Text-to-Speech tab for the GUI."""

import tkinter as tk
from tkinter import ttk, filedialog, messagebox, scrolledtext
from pathlib import Path
import sys
import threading

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from generators import TextToSpeechGenerator
from utils import Config


class TextToSpeechTab:
    """Text-to-Speech generation tab."""
    
    def __init__(self, parent, status_callback):
        """Initialize the TTS tab."""
        self.parent = parent
        self.status_callback = status_callback
        self.generator = None
        
        # Create frame
        self.frame = ttk.Frame(parent, padding=10)
        self._create_widgets()
    
    def _create_widgets(self):
        """Create tab widgets."""
        # Text input
        text_frame = ttk.LabelFrame(self.frame, text="Text to Convert", padding=10)
        text_frame.pack(fill=tk.BOTH, expand=True, pady=(0, 10))
        
        self.text_widget = scrolledtext.ScrolledText(text_frame, height=10, wrap=tk.WORD)
        self.text_widget.pack(fill=tk.BOTH, expand=True)
        self.text_widget.insert('1.0', 'Enter your text here...')
        
        # Settings
        settings_frame = ttk.LabelFrame(self.frame, text="Settings", padding=10)
        settings_frame.pack(fill=tk.X, pady=(0, 10))
        
        # Voice selection
        voice_frame = ttk.Frame(settings_frame)
        voice_frame.pack(fill=tk.X, pady=5)
        
        ttk.Label(voice_frame, text="Voice:").grid(row=0, column=0, sticky=tk.W, padx=(0, 10))
        self.voice_var = tk.StringVar(value=Config.DEFAULT_VOICE_ID)
        self.voice_combo = ttk.Combobox(voice_frame, textvariable=self.voice_var, width=40)
        self.voice_combo.grid(row=0, column=1, sticky=tk.W)
        self.voice_combo['values'] = list(Config.get_popular_voices().values())
        self.voice_combo.current(0)
        
        ttk.Button(voice_frame, text="Browse Library", command=self._browse_voices).grid(row=0, column=2, padx=(10, 0))
        
        # Model selection
        model_frame = ttk.Frame(settings_frame)
        model_frame.pack(fill=tk.X, pady=5)
        
        ttk.Label(model_frame, text="Model:").grid(row=0, column=0, sticky=tk.W, padx=(0, 10))
        self.model_var = tk.StringVar(value=Config.DEFAULT_MODEL)
        models = [(m['name'], mid) for mid, m in Config.get_available_models().items() if 'eleven_' in mid and 'music' not in mid]
        model_combo = ttk.Combobox(model_frame, textvariable=self.model_var, width=30, state='readonly')
        model_combo.grid(row=0, column=1, sticky=tk.W)
        model_combo['values'] = [m[0] for m in models]
        model_combo.current(0)
        
        # Voice settings
        sliders_frame = ttk.Frame(settings_frame)
        sliders_frame.pack(fill=tk.X, pady=10)
        
        # Stability
        ttk.Label(sliders_frame, text="Stability:").grid(row=0, column=0, sticky=tk.W)
        self.stability_var = tk.DoubleVar(value=Config.DEFAULT_STABILITY)
        stability_scale = ttk.Scale(sliders_frame, from_=0.0, to=1.0, variable=self.stability_var, orient=tk.HORIZONTAL)
        stability_scale.grid(row=0, column=1, sticky=tk.EW, padx=10)
        self.stability_label = ttk.Label(sliders_frame, text=f"{Config.DEFAULT_STABILITY:.2f}")
        self.stability_label.grid(row=0, column=2)
        stability_scale.config(command=lambda v: self.stability_label.config(text=f"{float(v):.2f}"))
        
        # Similarity
        ttk.Label(sliders_frame, text="Similarity:").grid(row=1, column=0, sticky=tk.W)
        self.similarity_var = tk.DoubleVar(value=Config.DEFAULT_SIMILARITY_BOOST)
        similarity_scale = ttk.Scale(sliders_frame, from_=0.0, to=1.0, variable=self.similarity_var, orient=tk.HORIZONTAL)
        similarity_scale.grid(row=1, column=1, sticky=tk.EW, padx=10)
        self.similarity_label = ttk.Label(sliders_frame, text=f"{Config.DEFAULT_SIMILARITY_BOOST:.2f}")
        self.similarity_label.grid(row=1, column=2)
        similarity_scale.config(command=lambda v: self.similarity_label.config(text=f"{float(v):.2f}"))
        
        # Style
        ttk.Label(sliders_frame, text="Style:").grid(row=2, column=0, sticky=tk.W)
        self.style_var = tk.DoubleVar(value=Config.DEFAULT_STYLE)
        style_scale = ttk.Scale(sliders_frame, from_=0.0, to=1.0, variable=self.style_var, orient=tk.HORIZONTAL)
        style_scale.grid(row=2, column=1, sticky=tk.EW, padx=10)
        self.style_label = ttk.Label(sliders_frame, text=f"{Config.DEFAULT_STYLE:.2f}")
        self.style_label.grid(row=2, column=2)
        style_scale.config(command=lambda v: self.style_label.config(text=f"{float(v):.2f}"))
        
        sliders_frame.columnconfigure(1, weight=1)
        
        # Output
        output_frame = ttk.LabelFrame(self.frame, text="Output", padding=10)
        output_frame.pack(fill=tk.X, pady=(0, 10))
        
        self.output_path = tk.StringVar(value=str(Config.TEMP_DIR / "tts_output.wav"))
        
        ttk.Label(output_frame, text="Save to:").grid(row=0, column=0, sticky=tk.W, padx=(0, 10))
        ttk.Entry(output_frame, textvariable=self.output_path, width=50).grid(row=0, column=1, sticky=tk.EW)
        ttk.Button(output_frame, text="Browse...", command=self._browse_output).grid(row=0, column=2, padx=(10, 0))
        
        output_frame.columnconfigure(1, weight=1)
        
        # Generate button
        button_frame = ttk.Frame(self.frame)
        button_frame.pack(fill=tk.X)
        
        self.generate_button = ttk.Button(
            button_frame,
            text="Generate Speech",
            command=self._generate,
            style='Accent.TButton'
        )
        self.generate_button.pack(side=tk.RIGHT)
    
    def _browse_voices(self):
        """Open voice library browser."""
        from ui.dialogs import VoiceLibraryDialog
        dialog = VoiceLibraryDialog(self.frame)
        # TODO: Update voice_var with selected voice
    
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
        """Generate speech."""
        text = self.text_widget.get('1.0', tk.END).strip()
        
        if not text or text == 'Enter your text here...':
            messagebox.showerror("Error", "Please enter some text to convert")
            return
        
        # Disable button
        self.generate_button.config(state=tk.DISABLED)
        self.status_callback("Generating speech...", show_progress=True)
        
        # Run in background thread
        def generate_thread():
            try:
                if not self.generator:
                    self.generator = TextToSpeechGenerator()
                
                # Get voice ID from selection
                voice_id = Config.DEFAULT_VOICE_ID  # TODO: Map from combo selection
                model_id = self.model_var.get()
                
                # Map model name to ID
                models = Config.get_available_models()
                for mid, m in models.items():
                    if m['name'] == model_id:
                        model_id = mid
                        break
                
                audio = self.generator.generate(
                    text=text,
                    voice_id=voice_id,
                    model_id=model_id,
                    stability=self.stability_var.get(),
                    similarity_boost=self.similarity_var.get(),
                    style=self.style_var.get(),
                    output_file=Path(self.output_path.get())
                )
                
                # Success
                self.frame.after(0, lambda: self._generation_complete(True, None))
                
            except Exception as e:
                self.frame.after(0, lambda: self._generation_complete(False, str(e)))
        
        thread = threading.Thread(target=generate_thread, daemon=True)
        thread.start()
    
    def _generation_complete(self, success, error):
        """Handle generation completion."""
        self.generate_button.config(state=tk.NORMAL)
        self.status_callback("Ready", show_progress=False)
        
        if success:
            messagebox.showinfo(
                "Success",
                f"Speech generated successfully!\n\n"
                f"Saved to: {self.output_path.get()}\n\n"
                "You can now import this file into Audacity."
            )
        else:
            messagebox.showerror("Error", f"Generation failed:\n\n{error}")
